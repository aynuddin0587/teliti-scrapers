#!/usr/bin/env python3
"""
Resumable Huawei/Qingyue historical surface-water API executor.

Purpose
-------
Execute a pre-approved station x timestamp request plan safely and reproducibly.
The script is intentionally serial: no parallel requests are used, because API calls
are metered and the primary objective is auditability rather than maximum speed.

Expected input plan
-------------------
The CSV produced by qingyue_build_historical_plan.py, including at least:
  request_id, plan_order, priority, plan_component, station_identity_id,
  target_datetime, city_param, province_param, staname_param, valley_param,
  version_param, query_string, endpoint.

Key safety properties
---------------------
* Requires a non-empty station name for every request.
* Normalizes form-style + spaces to %20 before signing.
* Sends the exact SDK-canonical URI when the signer provides it.
* Durable SQLite checkpoint after every HTTP attempt.
* Never repeats requests already classified as ok or no_record.
* 404 + errCode=0 + data=[] is treated as no_record, not as a fatal error.
* 200 + errCode=0 + data=[] is also treated as no_record.
* 401/403 and unexpected client/application errors stop the run immediately.
* Transient network/429/5xx errors are retried conservatively.
* A hard executor call-budget prevents accidental overrun.
* Every HTTP response body is preserved byte-for-byte under source/YYYY/MM/.
* Authorization headers and API secrets are NEVER written to disk.
* Original approved CSV and SQLite plan hash are unchanged.
* Runs stages in scientific priority order; May-Oct 2020 is deferred by default.
* Additional diurnal hours require a successful noon observation for that date.
* No-noon conditional skips are recorded separately, with ZERO HTTP attempts.
* Already-attempted diurnal records, successful or otherwise, are never rewritten.
* All steps run with the same original plan SHA256 and response archive paths.

Credentials
-----------
Set environment variables before running:
  HUAWEI_APP_KEY
  HUAWEI_APP_SECRET

Windows PowerShell example for the current session:
  $env:HUAWEI_APP_KEY="..."
  $env:HUAWEI_APP_SECRET="..."

Typical commands
----------------
Validate without any API calls:
  python qingyue_historical_executor.py validate --plan "...csv"

Five-call smoke test:
  python qingyue_historical_executor.py run --plan "...csv" --max-new-calls 5

Run only a curated subset of request IDs from the approved plan:
  python qingyue_historical_executor.py run --request-list "...probe.csv"

Show status for that subset without API calls:
  python qingyue_historical_executor.py status --request-list "...probe.csv"

Continue/resume stage 1 (default):
  python qingyue_historical_executor.py run --stage 1

Advance after reviewing stage 1:
  python qingyue_historical_executor.py run --stage 2
  python qingyue_historical_executor.py run --stage 3
  python qingyue_historical_executor.py run --stage 4

Opt-in to the low-coverage 2020 period ONLY after reviewing remaining quota:
  python qingyue_historical_executor.py run --stage deferred-2020

Preview stages without API calls:
  python qingyue_historical_executor.py preview

Retry a previously logged 401/403 after fixing authentication/signing:
  python qingyue_historical_executor.py run --retry-auth-failures --max-new-calls 5

Status only:
  python qingyue_historical_executor.py status --plan "...csv"

Rebuild processed CSV exports from archived responses:
  python qingyue_historical_executor.py export --plan "...csv"
"""

from __future__ import annotations

import argparse
import collections
import csv
import datetime as dt
import gzip
import hashlib
import importlib.util
import json
import os
import random
import sqlite3
import sys
import tempfile
import time
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Tuple

try:
    import requests
except ImportError as exc:  # pragma: no cover
    raise SystemExit(
        "Missing Python package 'requests'. Install it with: pip install requests"
    ) from exc


EXECUTOR_VERSION = "1.1.0"
DEFAULT_ROOT = Path(
    "D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical"
)
DEFAULT_PLAN = DEFAULT_ROOT / "plan" / "qingyue_request_plan_recommended.csv"
DEFAULT_SIGNER = Path(
    "D:/# R Project/penelitian/nmemc/script/qingyue/apig_sdk/signer.py"
)

# 45,000 package ceiling minus a deliberate ~3,000-call reserve. This counter
# covers calls made by THIS executor database only; prior manual tests are not
# known to the script. The user's existing manual usage is small relative to the
# reserve, but the limit remains configurable from the command line.
DEFAULT_EXECUTOR_CALL_BUDGET = 42000
DEFAULT_TIMEOUT_SECONDS = 60.0
DEFAULT_DELAY_SECONDS = 0.25
DEFAULT_MAX_ATTEMPTS_PER_REQUEST = 2

COMPLETED_STATUSES = {"ok", "no_record", "skipped_noon_missing"}
RETRYABLE_STATUSES = {"pending", "retryable_error"}

# These WHERE clauses operate on the existing, unmodified approved plan. They
# are mutually exclusive and collectively cover all 39,626 planned request IDs.
# The original plan SHA256 remains exactly the same as v1.0.3, so the existing
# SQLite ledger can be resumed without migration or deleting any saved responses.
STAGE_LABELS = {
    "1": "Monthly backbone, 2021-2025",
    "2": "Monthly backbone, Nov-Dec 2020",
    "3": "Monthly backbone, 2026",
    "4": "Conditional semiannual diurnal, 2021-2026",
    "deferred-2020": "Deferred May-Oct 2020 (low coverage)",
}
STAGE_SQL = {
    "1": "(plan_component='monthly_noon_backbone' AND substr(target_datetime,1,4) BETWEEN '2021' AND '2025')",
    "2": "(plan_component='monthly_noon_backbone' AND substr(target_datetime,1,7) IN ('2020-11','2020-12'))",
    "3": "(plan_component='monthly_noon_backbone' AND substr(target_datetime,1,4)='2026')",
    "4": "(plan_component='semiannual_full_day_extra' AND substr(target_datetime,1,4) BETWEEN '2021' AND '2026')",
    "deferred-2020": "(substr(target_datetime,1,7) BETWEEN '2020-05' AND '2020-10')",
}
ACTIVE_STAGES = ("1", "2", "3", "4")
ALL_STAGES = ACTIVE_STAGES + ("deferred-2020",)


def stage_for_row(row) -> str:
    target = str(row["target_datetime"] or "")
    component = str(row["plan_component"] or "")
    year, month = target[:4], target[5:7]
    if component == "monthly_noon_backbone":
        if "2021" <= year <= "2025":
            return "1"
        if year == "2020" and month in ("11", "12"):
            return "2"
        if year == "2026":
            return "3"
    if component == "semiannual_full_day_extra" and "2021" <= year <= "2026":
        return "4"
    if year == "2020" and "05" <= month <= "10":
        return "deferred-2020"
    raise RuntimeError(
        "Plan row does not belong to a recognized stage: "
        f"request_id={row['request_id']!r} datetime={target!r} component={component!r}"
    )


def stage_filter_sql(stage: str) -> str:
    if stage == "all":
        return "(" + " OR ".join(STAGE_SQL[s] for s in ACTIVE_STAGES) + ")"
    return STAGE_SQL[stage]


def stage_order_sql() -> str:
    return "CASE " + " ".join(
        f"WHEN {STAGE_SQL[s]} THEN {i}" for i, s in enumerate(ALL_STAGES, 1)
    ) + " ELSE 99 END"


PLAN_REQUIRED_COLUMNS = {
    "request_id",
    "plan_order",
    "priority",
    "plan_component",
    "station_identity_id",
    "target_datetime",
    "city_param",
    "province_param",
    "staname_param",
    "valley_param",
    "version_param",
    "query_string",
    "endpoint",
}

# Source-native API fields currently observed. Export keeps unknown future fields
# too, but this ordering makes the core columns stable and readable.
KNOWN_API_FIELDS = [
    "province",
    "city",
    "river",
    "basin",
    "section",
    "monitor_time",
    "qulity",
    "water_temp",
    "ph",
    "ph_l",
    "dissolvedoxygen",
    "dissolvedoxygen_l",
    "codmn",
    "codmn_l",
    "nh3-n",
    "nh3-n_l",
    "tp",
    "tn",
    "conductivity",
    "turbidity",
    "chlorophyll",
    "algal_density",
    "section_status",
]

PLAN_EXPORT_FIELDS = [
    "request_id",
    "plan_order",
    "priority",
    "plan_component",
    "study_site_code",
    "study_site_name",
    "station_identity_id",
    "cnemc_station_name",
    "cnemc_area",
    "cnemc_river_basin",
    "qingyue_station_name",
    "qingyue_province",
    "qingyue_city",
    "qingyue_valley",
    "longitude",
    "latitude",
    "qingyue_first_time",
    "qingyue_last_time",
    "direct_cnemc_first_time",
    "effective_history_end",
    "target_datetime",
    "target_date",
    "target_hour",
]


def now_iso() -> str:
    return dt.datetime.now(dt.timezone.utc).astimezone().isoformat(timespec="seconds")


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def parse_target_datetime(value: str) -> dt.datetime:
    value = (value or "").strip()
    for fmt in ("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S"):
        try:
            return dt.datetime.strptime(value, fmt)
        except ValueError:
            pass
    raise ValueError(f"Unsupported target_datetime: {value!r}")


def atomic_write_bytes(path: Path, data: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, tmp_name = tempfile.mkstemp(prefix=".tmp_", dir=str(path.parent))
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data)
            f.flush()
            os.fsync(f.fileno())
        os.replace(tmp_name, path)
    finally:
        if os.path.exists(tmp_name):
            os.unlink(tmp_name)


def atomic_write_text(path: Path, text: str, encoding: str = "utf-8") -> None:
    atomic_write_bytes(path, text.encode(encoding))


def safe_int(value: Any, default: int = 0) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def load_plan(path: Path) -> Tuple[List[Dict[str, str]], List[str]]:
    if not path.exists():
        raise FileNotFoundError(f"Plan not found: {path}")
    with path.open("r", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        if reader.fieldnames is None:
            raise RuntimeError("Plan CSV has no header")
        fields = list(reader.fieldnames)
        missing = sorted(PLAN_REQUIRED_COLUMNS.difference(fields))
        if missing:
            raise RuntimeError("Plan is missing required column(s): " + ", ".join(missing))
        rows = [dict(r) for r in reader]
    return rows, fields


def validate_plan(rows: List[Dict[str, str]]) -> Dict[str, Any]:
    if not rows:
        raise RuntimeError("Plan contains zero requests")

    seen_request_ids = set()
    seen_station_time = set()
    duplicate_request_ids = 0
    duplicate_station_times = 0
    empty_staname = 0
    bad_query = 0
    bad_datetime = 0
    endpoint_count = set()
    components: Dict[str, int] = {}
    sites: Dict[str, int] = {}

    for row in rows:
        rid = (row.get("request_id") or "").strip()
        if rid in seen_request_ids:
            duplicate_request_ids += 1
        seen_request_ids.add(rid)

        staname = (row.get("staname_param") or "").strip()
        if not staname:
            empty_staname += 1

        target = (row.get("target_datetime") or "").strip()
        try:
            parse_target_datetime(target)
        except ValueError:
            bad_datetime += 1

        st_key = ((row.get("station_identity_id") or "").strip(), target)
        if st_key in seen_station_time:
            duplicate_station_times += 1
        seen_station_time.add(st_key)

        qs = row.get("query_string") or ""
        if "staname=" not in qs or "sta_time=" not in qs:
            bad_query += 1

        endpoint_count.add((row.get("endpoint") or "").strip())
        comp = (row.get("plan_component") or "").strip()
        components[comp] = components.get(comp, 0) + 1
        site = (row.get("study_site_code") or "").strip()
        sites[site] = sites.get(site, 0) + 1

    problems = {
        "duplicate_request_ids": duplicate_request_ids,
        "duplicate_station_times": duplicate_station_times,
        "empty_staname": empty_staname,
        "bad_query": bad_query,
        "bad_datetime": bad_datetime,
    }
    if any(problems.values()):
        raise RuntimeError("Plan validation failed: " + json.dumps(problems, ensure_ascii=False))

    # Every planned extra time must map to exactly one noon request, not merely
    # to a station in the catalogue. A missing link could cause silent skips.
    noon_keys = {
        (r["station_identity_id"], r["target_datetime"])
        for r in rows if r["plan_component"] == "monthly_noon_backbone"
    }
    missing_noon_dependencies = 0
    for r in rows:
        if r["plan_component"] != "semiannual_full_day_extra":
            continue
        target = parse_target_datetime(r["target_datetime"])
        noon_key = (
            r["station_identity_id"],
            target.replace(hour=12, minute=0, second=0).strftime("%Y-%m-%d %H:%M:%S"),
        )
        if noon_key not in noon_keys:
            missing_noon_dependencies += 1
    if missing_noon_dependencies:
        raise RuntimeError(
            f"Plan has {missing_noon_dependencies} extra-hour request(s) without a noon backbone"
        )

    return {
        "noon_dependencies_missing": missing_noon_dependencies,
        "rows": len(rows),
        "unique_request_ids": len(seen_request_ids),
        "unique_station_times": len(seen_station_time),
        "endpoint_count": len(endpoint_count),
        "components": components,
        "study_sites": sites,
    }


def load_signer_module(signer_path: Path):
    """Load Huawei APIG signer.py with package context intact.

    Huawei's SDK signer.py imports sibling modules with relative imports, e.g.
    ``from . import sm3_hash``. Loading signer.py as an isolated file therefore
    fails with ``attempted relative import with no known parent package``.

    We register the SDK directory as a small synthetic package and then load
    signer.py as a submodule of that package. This works with the existing
    ``apig_sdk`` folder without modifying Huawei's SDK files.
    """
    signer_path = signer_path.expanduser().resolve()
    if not signer_path.exists():
        raise FileNotFoundError(f"Huawei signer.py not found: {signer_path}")

    package_dir = signer_path.parent
    package_init = package_dir / "__init__.py"
    if not package_init.exists():
        raise FileNotFoundError(
            "Huawei APIG SDK package is incomplete: missing "
            f"{package_init}. Keep signer.py, __init__.py, sm3_hash.py and the "
            "other SDK files together in the apig_sdk directory."
        )

    package_name = "teliti_huawei_apig_sdk"
    signer_module_name = f"{package_name}.signer"

    # Clear a prior synthetic load in case this function is called again in the
    # same Python process with a different SDK path.
    for name in list(sys.modules):
        if name == package_name or name.startswith(package_name + "."):
            del sys.modules[name]

    package_spec = importlib.util.spec_from_file_location(
        package_name,
        package_init,
        submodule_search_locations=[str(package_dir)],
    )
    if package_spec is None or package_spec.loader is None:
        raise RuntimeError(f"Could not load Huawei SDK package: {package_dir}")

    package_module = importlib.util.module_from_spec(package_spec)
    sys.modules[package_name] = package_module
    package_spec.loader.exec_module(package_module)

    signer_spec = importlib.util.spec_from_file_location(
        signer_module_name,
        signer_path,
        submodule_search_locations=None,
    )
    if signer_spec is None or signer_spec.loader is None:
        raise RuntimeError(f"Could not load signer module: {signer_path}")

    module = importlib.util.module_from_spec(signer_spec)
    sys.modules[signer_module_name] = module
    signer_spec.loader.exec_module(module)

    if not hasattr(module, "Signer") or not hasattr(module, "HttpRequest"):
        raise RuntimeError("signer.py does not expose Signer and HttpRequest")
    return module


def get_credentials() -> Tuple[str, str]:
    key = os.getenv("HUAWEI_APP_KEY", "").strip()
    secret = os.getenv("HUAWEI_APP_SECRET", "").strip()
    if not key or not secret:
        raise RuntimeError(
            "Huawei credentials are missing. Set HUAWEI_APP_KEY and HUAWEI_APP_SECRET "
            "in the environment before running."
        )
    return key, secret


def connect_db(db_path: Path) -> sqlite3.Connection:
    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(str(db_path), timeout=60)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA synchronous=FULL")
    conn.execute("PRAGMA foreign_keys=ON")
    return conn


def init_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE IF NOT EXISTS meta (
            key TEXT PRIMARY KEY,
            value TEXT NOT NULL
        );

        CREATE TABLE IF NOT EXISTS requests (
            request_id TEXT PRIMARY KEY,
            plan_order INTEGER NOT NULL,
            priority INTEGER,
            plan_component TEXT,
            study_site_code TEXT,
            study_site_name TEXT,
            station_identity_id TEXT,
            cnemc_station_name TEXT,
            cnemc_area TEXT,
            cnemc_river_basin TEXT,
            qingyue_station_name TEXT,
            qingyue_province TEXT,
            qingyue_city TEXT,
            qingyue_valley TEXT,
            longitude TEXT,
            latitude TEXT,
            qingyue_first_time TEXT,
            qingyue_last_time TEXT,
            direct_cnemc_first_time TEXT,
            effective_history_end TEXT,
            target_datetime TEXT NOT NULL,
            target_date TEXT,
            target_hour TEXT,
            city_param TEXT,
            province_param TEXT,
            staname_param TEXT NOT NULL,
            valley_param TEXT,
            version_param TEXT,
            query_string TEXT NOT NULL,
            endpoint TEXT NOT NULL,
            status TEXT NOT NULL DEFAULT 'pending',
            attempts INTEGER NOT NULL DEFAULT 0,
            first_attempt_at TEXT,
            last_attempt_at TEXT,
            completed_at TEXT,
            http_status INTEGER,
            api_err_code TEXT,
            api_err_msg TEXT,
            api_request_id TEXT,
            response_row_count INTEGER,
            response_path TEXT,
            response_sha256 TEXT,
            response_headers_json TEXT,
            last_error TEXT
        );

        CREATE INDEX IF NOT EXISTS idx_requests_status_order
            ON requests(status, plan_order);
        CREATE INDEX IF NOT EXISTS idx_requests_station_time
            ON requests(station_identity_id, target_datetime);

        CREATE TABLE IF NOT EXISTS attempt_log (
            attempt_id INTEGER PRIMARY KEY AUTOINCREMENT,
            request_id TEXT NOT NULL,
            attempted_at TEXT NOT NULL,
            attempt_number INTEGER NOT NULL,
            http_status INTEGER,
            outcome TEXT NOT NULL,
            elapsed_ms REAL,
            response_path TEXT,
            response_sha256 TEXT,
            response_headers_json TEXT,
            error_message TEXT,
            FOREIGN KEY(request_id) REFERENCES requests(request_id)
        );
        """
    )
    conn.commit()


def get_meta(conn: sqlite3.Connection, key: str) -> Optional[str]:
    row = conn.execute("SELECT value FROM meta WHERE key=?", (key,)).fetchone()
    return None if row is None else str(row["value"])


def set_meta(conn: sqlite3.Connection, key: str, value: str) -> None:
    conn.execute(
        "INSERT INTO meta(key,value) VALUES(?,?) "
        "ON CONFLICT(key) DO UPDATE SET value=excluded.value",
        (key, value),
    )


def import_plan_into_db(
    conn: sqlite3.Connection,
    plan_path: Path,
    rows: List[Dict[str, str]],
    plan_fields: List[str],
) -> None:
    plan_hash = sha256_file(plan_path)
    existing_hash = get_meta(conn, "plan_sha256")

    if existing_hash is not None and existing_hash != plan_hash:
        raise RuntimeError(
            "This state database belongs to a different plan CSV.\n"
            f"Database plan SHA256: {existing_hash}\n"
            f"Current plan SHA256:  {plan_hash}\n"
            "Use the original plan or start a new state directory/database."
        )

    existing_rows = safe_int(get_meta(conn, "plan_rows"), -1)
    if existing_rows not in (-1, len(rows)):
        raise RuntimeError(
            f"State database expects {existing_rows} plan rows but current plan has {len(rows)}"
        )

    insert_sql = """
        INSERT OR IGNORE INTO requests(
            request_id, plan_order, priority, plan_component,
            study_site_code, study_site_name, station_identity_id,
            cnemc_station_name, cnemc_area, cnemc_river_basin,
            qingyue_station_name, qingyue_province, qingyue_city, qingyue_valley,
            longitude, latitude, qingyue_first_time, qingyue_last_time,
            direct_cnemc_first_time, effective_history_end,
            target_datetime, target_date, target_hour,
            city_param, province_param, staname_param, valley_param, version_param,
            query_string, endpoint
        ) VALUES (
            ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?
        )
    """

    values = []
    for r in rows:
        values.append(
            (
                r.get("request_id", ""),
                safe_int(r.get("plan_order"), 0),
                safe_int(r.get("priority"), 0),
                r.get("plan_component", ""),
                r.get("study_site_code", ""),
                r.get("study_site_name", ""),
                r.get("station_identity_id", ""),
                r.get("cnemc_station_name", ""),
                r.get("cnemc_area", ""),
                r.get("cnemc_river_basin", ""),
                r.get("qingyue_station_name", ""),
                r.get("qingyue_province", ""),
                r.get("qingyue_city", ""),
                r.get("qingyue_valley", ""),
                r.get("longitude", ""),
                r.get("latitude", ""),
                r.get("qingyue_first_time", ""),
                r.get("qingyue_last_time", ""),
                r.get("direct_cnemc_first_time", ""),
                r.get("effective_history_end", ""),
                r.get("target_datetime", ""),
                r.get("target_date", ""),
                r.get("target_hour", ""),
                r.get("city_param", ""),
                r.get("province_param", ""),
                r.get("staname_param", ""),
                r.get("valley_param", ""),
                r.get("version_param", ""),
                r.get("query_string", ""),
                r.get("endpoint", ""),
            )
        )

    with conn:
        conn.executemany(insert_sql, values)
        set_meta(conn, "executor_version", EXECUTOR_VERSION)
        set_meta(conn, "plan_sha256", plan_hash)
        set_meta(conn, "plan_rows", str(len(rows)))
        set_meta(conn, "plan_path_original", str(plan_path))
        if get_meta(conn, "state_created_at") is None:
            set_meta(conn, "state_created_at", now_iso())
        set_meta(conn, "plan_fields_json", json.dumps(plan_fields, ensure_ascii=False))

    db_count = conn.execute("SELECT COUNT(*) AS n FROM requests").fetchone()["n"]
    if db_count != len(rows):
        raise RuntimeError(
            f"State database contains {db_count} request rows; expected {len(rows)}. "
            "Do not merge different plans into one state database."
        )


def attempt_count(conn: sqlite3.Connection) -> int:
    return int(conn.execute("SELECT COUNT(*) AS n FROM attempt_log").fetchone()["n"])


def status_counts(conn: sqlite3.Connection) -> Dict[str, int]:
    rows = conn.execute("SELECT status, COUNT(*) AS n FROM requests GROUP BY status").fetchall()
    return {str(r["status"]): int(r["n"]) for r in rows}


def print_status(conn: sqlite3.Connection, call_budget: int) -> None:
    counts = status_counts(conn)
    total = sum(counts.values())
    completed = sum(counts.get(s, 0) for s in COMPLETED_STATUSES)
    attempts = attempt_count(conn)
    print("Qingyue historical executor status")
    print("=" * 36)
    print(f"Plan requests:             {total:,}")
    print(f"Resolved plan requests:    {completed:,}")
    print(f"  ok:                      {counts.get('ok', 0):,}")
    print(f"  no_record:               {counts.get('no_record', 0):,}")
    print(f"  skipped_noon_missing:    {counts.get('skipped_noon_missing', 0):,} (no API call)")
    print(f"Pending:                   {counts.get('pending', 0):,}")
    print(f"Retryable error:           {counts.get('retryable_error', 0):,}")
    print(f"Transient exhausted:       {counts.get('transient_exhausted', 0):,}")
    print(f"Fatal/application error:   {counts.get('fatal_error', 0):,}")
    print(f"Response mismatch:         {counts.get('response_mismatch', 0):,}")
    print(f"API HTTP attempts logged:  {attempts:,}")
    print(f"Executor call budget:      {call_budget:,}")
    print(f"Executor attempts left:    {max(call_budget - attempts, 0):,}")


def print_subset_status(conn: sqlite3.Connection, request_ids: set[str], call_budget: int) -> None:
    placeholders = ",".join("?" for _ in request_ids)
    rows = conn.execute(
        f"SELECT status, COUNT(*) AS n FROM requests WHERE request_id IN ({placeholders}) GROUP BY status",
        list(request_ids),
    ).fetchall()
    counts = {str(r["status"]): int(r["n"]) for r in rows}
    total = sum(counts.values())
    completed = sum(counts.get(s, 0) for s in COMPLETED_STATUSES)
    attempts = conn.execute(
        f"SELECT COUNT(*) AS n FROM attempt_log WHERE request_id IN ({placeholders})",
        list(request_ids),
    ).fetchone()["n"]
    ok_by_year = conn.execute(
        f"""
        SELECT substr(target_datetime,1,4) AS yr,
               SUM(CASE WHEN status='ok' THEN 1 ELSE 0 END) AS ok_n,
               SUM(CASE WHEN status='no_record' THEN 1 ELSE 0 END) AS no_record_n,
               COUNT(*) AS total_n
        FROM requests
        WHERE request_id IN ({placeholders})
        GROUP BY substr(target_datetime,1,4)
        ORDER BY yr
        """,
        list(request_ids),
    ).fetchall()
    site_rows = conn.execute(
        f"""
        SELECT study_site_code,
               SUM(CASE WHEN status='ok' THEN 1 ELSE 0 END) AS ok_n,
               SUM(CASE WHEN status='no_record' THEN 1 ELSE 0 END) AS no_record_n,
               COUNT(*) AS total_n
        FROM requests
        WHERE request_id IN ({placeholders})
        GROUP BY study_site_code
        ORDER BY study_site_code
        """,
        list(request_ids),
    ).fetchall()

    print("Qingyue request-subset status")
    print("=" * 31)
    print(f"Subset requests:            {total:,}")
    print(f"Resolved:                   {completed:,}")
    print(f"  ok:                       {counts.get('ok', 0):,}")
    print(f"  no_record:                {counts.get('no_record', 0):,}")
    print(f"  skipped_noon_missing:     {counts.get('skipped_noon_missing', 0):,} (no API call)")
    print(f"Pending:                    {counts.get('pending', 0):,}")
    print(f"Subset HTTP attempts:       {int(attempts):,}")
    print(f"Global HTTP attempts:       {attempt_count(conn):,}")
    print(f"Global executor attempts left: {max(call_budget - attempt_count(conn), 0):,}")
    print("By year (ok / no_record / total):")
    for r in ok_by_year:
        print(f"  {r['yr']}: {int(r['ok_n'] or 0):,} / {int(r['no_record_n'] or 0):,} / {int(r['total_n']):,}")
    print("By study site (ok / no_record / total):")
    for r in site_rows:
        print(f"  {r['study_site_code'] or '<blank>'}: {int(r['ok_n'] or 0):,} / {int(r['no_record_n'] or 0):,} / {int(r['total_n']):,}")


def print_staged_status(conn: sqlite3.Connection) -> None:
    """Print progress from the existing ledger without accessing the API."""
    print("Staged acquisition (plan / ok / no_record / skipped / pending-or-other):")
    for stage in ALL_STAGES:
        data = conn.execute(
            "SELECT status, COUNT(*) AS n FROM requests WHERE "
            + STAGE_SQL[stage] + " GROUP BY status"
        ).fetchall()
        counts = {str(r["status"]): int(r["n"]) for r in data}
        total = sum(counts.values())
        unresolved = total - sum(counts.get(k, 0) for k in COMPLETED_STATUSES)
        print(
            f"  {stage:>13}  {total:>6,} / {counts.get('ok', 0):>6,} / "
            f"{counts.get('no_record', 0):>6,} / "
            f"{counts.get('skipped_noon_missing', 0):>6,} / {unresolved:>6,}"
        )


def validate_stage_partition(rows: List[Dict[str, str]]) -> Dict[str, int]:
    counts = collections.Counter(stage_for_row(r) for r in rows)
    if sum(counts.values()) != len(rows):
        raise RuntimeError("Staged partition is incomplete")
    return dict(counts)


def find_noon_backbone(conn: sqlite3.Connection, row: sqlite3.Row) -> Optional[sqlite3.Row]:
    """Resolve an extra-hour record to its exact same-station/same-day noon row."""
    target = parse_target_datetime(str(row["target_datetime"]))
    noon = target.replace(hour=12, minute=0, second=0).strftime("%Y-%m-%d %H:%M:%S")
    matches = conn.execute(
        """SELECT request_id,status,attempts FROM requests
           WHERE station_identity_id=? AND target_datetime=?
             AND plan_component='monthly_noon_backbone'""",
        (row["station_identity_id"], noon),
    ).fetchall()
    if len(matches) > 1:
        raise RuntimeError(
            "Multiple noon backbone rows for "
            f"station={row['station_identity_id']} date={noon[:10]}"
        )
    return matches[0] if matches else None


def mark_conditional_skip(
    conn: sqlite3.Connection, row: sqlite3.Row, noon_request_id: str
) -> bool:
    """Mark an unattempted extra-hour request without creating an HTTP attempt."""
    if int(row["attempts"]) != 0 or str(row["status"]) != "pending":
        return False
    with conn:
        cur = conn.execute(
            """UPDATE requests
               SET status='skipped_noon_missing',
                   completed_at=?,
                   last_error=?
               WHERE request_id=? AND status='pending' AND attempts=0""",
            (
                now_iso(),
                "Conditional skip: same-station same-day noon backbone returned "
                f"no_record (noon_request_id={noon_request_id}); ZERO HTTP attempts",
                row["request_id"],
            ),
        )
    return cur.rowcount == 1


def response_archive_path(root: Path, row: sqlite3.Row, attempt_number: int) -> Path:
    target = parse_target_datetime(str(row["target_datetime"]))
    return (
        root
        / "source"
        / f"{target.year:04d}"
        / f"{target.month:02d}"
        / f"{row['request_id']}_attempt{attempt_number:02d}.json"
    )


def build_full_url(endpoint: str, query_string: str) -> str:
    endpoint = endpoint.strip()
    query_string = query_string.strip().lstrip("?")
    if not endpoint or not query_string:
        raise RuntimeError("Endpoint or query string is empty")

    # urllib.parse.urlencode() uses '+' for spaces. Huawei's classic APIG
    # signer parses query values with urllib.parse.unquote(), not
    # unquote_plus(), so a '+' can be signed as a literal plus (%2B) while an
    # HTTP server interprets the transmitted '+' as a space. This causes an
    # otherwise valid request to fail signature verification with HTTP 401.
    # Convert form-style spaces to RFC3986 %20 before constructing/signing the
    # request. A genuine plus character is encoded as %2B and is unaffected.
    query_string = query_string.replace("+", "%20")
    return endpoint + ("&" if "?" in endpoint else "?") + query_string


def sign_request(
    signer_module, app_key: str, app_secret: str, full_url: str
) -> Tuple[str, Dict[str, str], Any]:
    """Sign a request and return the exact URL/headers/body to transmit.

    Only x-stage is present before signing. User-Agent and Accept are added
    afterward so they are not part of SignedHeaders and cannot be modified by
    the HTTP client in a way that invalidates the signature.

    Huawei's classic APIG SDK canonicalizes the query during Sign() and, in
    common versions, appends that canonical query to req.uri. When available,
    transmit that exact SDK-produced URI. If a different SDK version leaves
    req.uri as path-only, fall back to the already-normalized full URL.
    """
    headers = {"x-stage": "RELEASE"}
    body = ""

    sig = signer_module.Signer()
    sig.Key = app_key
    sig.Secret = app_secret
    req = signer_module.HttpRequest("GET", full_url, headers, body)
    sig.Sign(req)

    req_headers = dict(req.headers)
    req_headers.setdefault("User-Agent", "Teliti-Qingyue-Historical/1.0.2")
    req_headers.setdefault("Accept", "application/json")

    signed_url = full_url
    req_uri = str(getattr(req, "uri", "") or "")
    req_scheme = str(getattr(req, "scheme", "") or "")
    req_host = str(getattr(req, "host", "") or "")
    if req_uri and "?" in req_uri and req_scheme and req_host:
        signed_url = f"{req_scheme}://{req_host}{req_uri}"

    req_body = getattr(req, "body", b"")
    return signed_url, req_headers, req_body


def parse_json_bytes(raw: bytes) -> Tuple[Optional[Dict[str, Any]], Optional[str]]:
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError:
        text = raw.decode("utf-8", errors="replace")
    try:
        obj = json.loads(text)
    except json.JSONDecodeError as exc:
        return None, f"JSON parse error: {exc}"
    if not isinstance(obj, dict):
        return None, f"Expected JSON object, got {type(obj).__name__}"
    return obj, None


def classify_response(
    row: sqlite3.Row,
    http_status: int,
    obj: Optional[Dict[str, Any]],
    parse_error: Optional[str],
) -> Tuple[str, bool, bool, str]:
    """Return (status, completed, retryable, message)."""
    if parse_error is not None or obj is None:
        if http_status == 429 or 500 <= http_status <= 599:
            return "retryable_error", False, True, parse_error or "transient non-JSON response"
        return "fatal_error", False, False, parse_error or "non-JSON response"

    err_code = obj.get("errCode")
    err_msg = obj.get("errMsg")

    # Authentication failures often use a gateway-level JSON schema rather
    # than Qingyue's normal {data, errCode, errMsg} schema. Classify by HTTP
    # status first so the diagnostic is not obscured by "data is not a list".
    if http_status in (401, 403):
        gateway_msg = (
            obj.get("message")
            or obj.get("error_msg")
            or obj.get("errorMessage")
            or obj.get("error")
            or err_msg
            or "authentication/signature rejected by Huawei APIG"
        )
        return (
            "fatal_error",
            False,
            False,
            f"HTTP {http_status} authentication failure: {gateway_msg}",
        )

    data = obj.get("data")
    if not isinstance(data, list):
        return "fatal_error", False, False, "JSON field 'data' is not a list"

    # Provider behavior verified during manual testing: HTTP 404 may carry
    # errCode=0, errMsg=success, data=[] when no station-time record exists.
    if http_status in (200, 404) and str(err_code) in ("0", "0.0") and len(data) == 0:
        return "no_record", True, False, "valid station-time lookup with no record"

    if http_status == 200 and str(err_code) in ("0", "0.0"):
        if len(data) != 1:
            return (
                "fatal_error",
                False,
                False,
                f"Expected exactly one record but API returned {len(data)}",
            )

        rec = data[0]
        if not isinstance(rec, dict):
            return "fatal_error", False, False, "API data[0] is not an object"

        requested_station = str(row["staname_param"] or "").strip()
        returned_station = str(rec.get("section") or "").strip()
        requested_time = str(row["target_datetime"] or "").strip()
        returned_time = str(rec.get("monitor_time") or "").strip()

        mismatch = []
        if returned_station != requested_station:
            mismatch.append(
                f"section requested={requested_station!r} returned={returned_station!r}"
            )
        if returned_time != requested_time:
            mismatch.append(
                f"monitor_time requested={requested_time!r} returned={returned_time!r}"
            )
        if mismatch:
            return "response_mismatch", False, False, "; ".join(mismatch)

        return "ok", True, False, "one exact station-time record"

    if http_status in (401, 403):
        return "fatal_error", False, False, f"authentication/authorization HTTP {http_status}"

    if http_status == 429 or 500 <= http_status <= 599:
        return "retryable_error", False, True, f"transient HTTP {http_status}"

    return (
        "fatal_error",
        False,
        False,
        f"unexpected response HTTP {http_status}; errCode={err_code!r}; errMsg={err_msg!r}",
    )


def log_attempt_and_update(
    conn: sqlite3.Connection,
    row: sqlite3.Row,
    outcome: str,
    http_status: Optional[int],
    elapsed_ms: Optional[float],
    response_path: Optional[Path],
    response_sha256: Optional[str],
    response_headers: Optional[Dict[str, str]],
    obj: Optional[Dict[str, Any]],
    error_message: Optional[str],
    completed: bool,
) -> None:
    attempt_no = int(row["attempts"]) + 1
    attempted_at = now_iso()
    headers_json = json.dumps(response_headers or {}, ensure_ascii=False, sort_keys=True)

    api_err_code = None
    api_err_msg = None
    api_request_id = None
    response_row_count = None
    if isinstance(obj, dict):
        api_err_code = None if obj.get("errCode") is None else str(obj.get("errCode"))
        api_err_msg = None if obj.get("errMsg") is None else str(obj.get("errMsg"))
        api_request_id = None if obj.get("requestId") is None else str(obj.get("requestId"))
        data = obj.get("data")
        if isinstance(data, list):
            response_row_count = len(data)

    with conn:
        conn.execute(
            """
            INSERT INTO attempt_log(
                request_id, attempted_at, attempt_number, http_status, outcome,
                elapsed_ms, response_path, response_sha256,
                response_headers_json, error_message
            ) VALUES (?,?,?,?,?,?,?,?,?,?)
            """,
            (
                row["request_id"],
                attempted_at,
                attempt_no,
                http_status,
                outcome,
                elapsed_ms,
                None if response_path is None else str(response_path),
                response_sha256,
                headers_json,
                error_message,
            ),
        )
        conn.execute(
            """
            UPDATE requests
            SET status=?,
                attempts=attempts+1,
                first_attempt_at=COALESCE(first_attempt_at, ?),
                last_attempt_at=?,
                completed_at=CASE WHEN ? THEN ? ELSE completed_at END,
                http_status=?,
                api_err_code=?,
                api_err_msg=?,
                api_request_id=?,
                response_row_count=?,
                response_path=?,
                response_sha256=?,
                response_headers_json=?,
                last_error=?
            WHERE request_id=?
            """,
            (
                outcome,
                attempted_at,
                attempted_at,
                1 if completed else 0,
                attempted_at,
                http_status,
                api_err_code,
                api_err_msg,
                api_request_id,
                response_row_count,
                None if response_path is None else str(response_path),
                response_sha256,
                headers_json,
                error_message,
                row["request_id"],
            ),
        )


def set_status(conn: sqlite3.Connection, request_id: str, status: str, last_error: str) -> None:
    with conn:
        conn.execute(
            "UPDATE requests SET status=?, last_error=? WHERE request_id=?",
            (status, last_error, request_id),
        )


def load_request_id_filter(path_value: Optional[str]) -> Optional[set[str]]:
    if not path_value:
        return None
    path = Path(path_value)
    if not path.exists():
        raise FileNotFoundError(f"Request-list file not found: {path}")

    request_ids: set[str] = set()
    if path.suffix.lower() == ".csv":
        with path.open("r", encoding="utf-8-sig", newline="") as f:
            reader = csv.DictReader(f)
            if reader.fieldnames is None or "request_id" not in reader.fieldnames:
                raise RuntimeError("Request-list CSV must contain a request_id column")
            for row in reader:
                rid = (row.get("request_id") or "").strip()
                if rid:
                    request_ids.add(rid)
    else:
        for line in path.read_text(encoding="utf-8-sig").splitlines():
            rid = line.strip()
            if rid and not rid.startswith("#"):
                request_ids.add(rid)

    if not request_ids:
        raise RuntimeError(f"Request-list contains no request IDs: {path}")
    return request_ids


def request_candidates(
    conn: sqlite3.Connection,
    max_attempts_per_request: int,
    component: Optional[str],
    study_site: Optional[str],
    start_order: Optional[int],
    retry_exhausted: bool,
    retry_auth_failures: bool,
    request_ids: Optional[set[str]] = None,
    stage: str = "1",
) -> Iterable[sqlite3.Row]:
    statuses = ["pending", "retryable_error"]
    if retry_exhausted:
        statuses.append("transient_exhausted")
    if retry_auth_failures:
        statuses.append("fatal_error")
    placeholders = ",".join("?" for _ in statuses)
    sql = f"SELECT * FROM requests WHERE status IN ({placeholders})"
    params: List[Any] = list(statuses)

    if retry_auth_failures:
        # Never revive arbitrary fatal application errors. Only explicitly
        # retry rows whose prior fatal status was an HTTP 401/403.
        sql += " AND (status != 'fatal_error' OR http_status IN (401,403))"

    if not retry_exhausted:
        sql += " AND attempts < ?"
        params.append(max_attempts_per_request)
    if component:
        sql += " AND plan_component = ?"
        params.append(component)
    if study_site:
        sql += " AND study_site_code = ?"
        params.append(study_site)
    if start_order is not None:
        sql += " AND plan_order >= ?"
        params.append(start_order)

    sql += " AND " + stage_filter_sql(stage)
    sql += " ORDER BY " + stage_order_sql() + ", plan_order"
    for row in conn.execute(sql, params):
        if request_ids is not None and str(row["request_id"]) not in request_ids:
            continue
        yield row


def run_executor(args: argparse.Namespace) -> None:
    plan_path = Path(args.plan)
    root = Path(args.root)
    db_path = root / "state" / "qingyue_historical.sqlite"
    log_path = root / "log" / f"qingyue_historical_{dt.date.today():%Y%m%d}.log"
    log_path.parent.mkdir(parents=True, exist_ok=True)

    rows, fields = load_plan(plan_path)
    summary = validate_plan(rows)

    conn = connect_db(db_path)
    init_schema(conn)
    import_plan_into_db(conn, plan_path, rows, fields)

    signer_module = load_signer_module(Path(args.signer_path))
    app_key, app_secret = get_credentials()

    attempts_before = attempt_count(conn)
    if attempts_before >= args.call_budget:
        raise RuntimeError(
            f"Executor call budget already exhausted: {attempts_before:,} / {args.call_budget:,}"
        )

    request_id_filter = load_request_id_filter(getattr(args, "request_list", None))
    if request_id_filter is not None:
        known = {str(r["request_id"]) for r in conn.execute("SELECT request_id FROM requests")}
        unknown = sorted(request_id_filter.difference(known))
        if unknown:
            preview = ", ".join(unknown[:5])
            raise RuntimeError(
                f"Request-list contains {len(unknown)} request_id value(s) not in the approved plan; "
                f"first: {preview}"
            )

    session = requests.Session()
    new_calls = 0
    conditional_skipped = 0
    conditional_blocked = 0
    conditional_preserved = 0
    run_started = now_iso()

    def log(msg: str) -> None:
        line = f"{now_iso()} | {msg}"
        print(line, flush=True)
        with log_path.open("a", encoding="utf-8") as f:
            f.write(line + "\n")

    log(
        f"START executor v{EXECUTOR_VERSION}; plan_rows={summary['rows']}; "
        f"attempts_already_logged={attempts_before}; call_budget={args.call_budget}; "
        f"stage={args.stage}"
    )

    for initial_row in request_candidates(
        conn,
        max_attempts_per_request=args.max_attempts_per_request,
        component=args.component,
        study_site=args.study_site,
        start_order=args.start_order,
        retry_exhausted=args.retry_exhausted,
        retry_auth_failures=args.retry_auth_failures,
        request_ids=request_id_filter,
        stage=args.stage,
    ):
        if args.max_new_calls is not None and new_calls >= args.max_new_calls:
            log(f"Stopping at --max-new-calls={args.max_new_calls}")
            break
        if attempt_count(conn) >= args.call_budget:
            log("Stopping because executor call budget has been reached")
            break

        # Reload the current row because it may have been updated by a retry.
        row = conn.execute(
            "SELECT * FROM requests WHERE request_id=?", (initial_row["request_id"],)
        ).fetchone()
        if row is None or row["status"] in COMPLETED_STATUSES:
            continue

        if str(row["plan_component"]) == "semiannual_full_day_extra":
            noon = find_noon_backbone(conn, row)
            if noon is None:
                raise RuntimeError(
                    "Missing required noon backbone for extra-hour request "
                    f"{row['request_id']}; refusing an unverified API call"
                )
            noon_status = str(noon["status"])
            if noon_status == "no_record":
                if mark_conditional_skip(conn, row, str(noon["request_id"])):
                    conditional_skipped += 1
                else:
                    conditional_preserved += 1
                continue
            if noon_status != "ok":
                conditional_blocked += 1
                continue

        full_url = build_full_url(str(row["endpoint"]), str(row["query_string"]))
        while True:
            current_attempts = int(row["attempts"])
            if current_attempts >= args.max_attempts_per_request and not args.retry_exhausted:
                set_status(
                    conn,
                    str(row["request_id"]),
                    "transient_exhausted",
                    f"max attempts reached ({args.max_attempts_per_request})",
                )
                log(
                    f"EXHAUSTED order={row['plan_order']} request={row['request_id']} "
                    f"station={row['staname_param']} time={row['target_datetime']}"
                )
                break

            if args.max_new_calls is not None and new_calls >= args.max_new_calls:
                break
            if attempt_count(conn) >= args.call_budget:
                break

            attempt_number = current_attempts + 1
            archive_path = response_archive_path(root, row, attempt_number)
            try:
                signed_url, signed_headers, signed_body = sign_request(
                    signer_module, app_key, app_secret, full_url
                )
                started = time.perf_counter()
                response = session.request(
                    "GET",
                    signed_url,
                    headers=signed_headers,
                    data=signed_body,
                    timeout=args.timeout,
                    allow_redirects=False,
                )
                elapsed_ms = (time.perf_counter() - started) * 1000.0
                new_calls += 1

                raw = response.content
                atomic_write_bytes(archive_path, raw)
                digest = sha256_bytes(raw)
                response_headers = {str(k): str(v) for k, v in response.headers.items()}
                obj, parse_error = parse_json_bytes(raw)
                outcome, completed, retryable, message = classify_response(
                    row, response.status_code, obj, parse_error
                )

                log_attempt_and_update(
                    conn=conn,
                    row=row,
                    outcome=outcome,
                    http_status=response.status_code,
                    elapsed_ms=elapsed_ms,
                    response_path=archive_path,
                    response_sha256=digest,
                    response_headers=response_headers,
                    obj=obj,
                    error_message=None if completed else message,
                    completed=completed,
                )

                log(
                    f"{outcome.upper()} order={row['plan_order']} "
                    f"attempt={attempt_number} http={response.status_code} "
                    f"station={row['staname_param']} time={row['target_datetime']} "
                    f"site={row['study_site_code']} rows={0 if obj is None or not isinstance(obj.get('data'), list) else len(obj.get('data'))} "
                    f"elapsed_ms={elapsed_ms:.0f}"
                )

                if completed:
                    break

                if outcome in {"fatal_error", "response_mismatch"}:
                    log(
                        "STOPPING immediately to protect metered quota: " + message
                    )
                    print_status(conn, args.call_budget)
                    raise SystemExit(2)

                if retryable:
                    row = conn.execute(
                        "SELECT * FROM requests WHERE request_id=?",
                        (row["request_id"],),
                    ).fetchone()
                    if row is None:
                        raise RuntimeError("Request disappeared from state database")
                    if int(row["attempts"]) >= args.max_attempts_per_request:
                        set_status(
                            conn,
                            str(row["request_id"]),
                            "transient_exhausted",
                            message,
                        )
                        break
                    sleep_s = args.retry_backoff * (2 ** (int(row["attempts"]) - 1))
                    sleep_s += random.uniform(0, min(0.25, sleep_s * 0.1))
                    log(f"Retrying after {sleep_s:.2f}s: {message}")
                    time.sleep(sleep_s)
                    continue

                break

            except (requests.RequestException, OSError) as exc:
                # A transport exception may occur before the provider receives the
                # request; conservatively record it as an attempt because we cannot
                # prove whether the metered gateway counted it.
                new_calls += 1
                err = f"{type(exc).__name__}: {exc}"
                log_attempt_and_update(
                    conn=conn,
                    row=row,
                    outcome="retryable_error",
                    http_status=None,
                    elapsed_ms=None,
                    response_path=None,
                    response_sha256=None,
                    response_headers=None,
                    obj=None,
                    error_message=err,
                    completed=False,
                )
                log(
                    f"NETWORK_ERROR order={row['plan_order']} attempt={attempt_number} "
                    f"station={row['staname_param']} time={row['target_datetime']} error={err}"
                )
                row = conn.execute(
                    "SELECT * FROM requests WHERE request_id=?", (row["request_id"],)
                ).fetchone()
                if row is None:
                    raise RuntimeError("Request disappeared from state database")
                if int(row["attempts"]) >= args.max_attempts_per_request:
                    set_status(conn, str(row["request_id"]), "transient_exhausted", err)
                    break
                sleep_s = args.retry_backoff * (2 ** (int(row["attempts"]) - 1))
                time.sleep(sleep_s)
                continue

        if args.delay > 0:
            time.sleep(args.delay)

    log(
        f"END run; new_attempts={new_calls}; conditional_skipped={conditional_skipped}; "
        f"conditional_blocked={conditional_blocked}; "
        f"conditional_preserved={conditional_preserved}; "
        f"started={run_started}; finished={now_iso()}"
    )
    print_status(conn, args.call_budget)
    print_staged_status(conn)
    conn.close()


def iter_ok_records(conn: sqlite3.Connection) -> Iterable[Tuple[sqlite3.Row, Dict[str, Any]]]:
    for row in conn.execute(
        "SELECT * FROM requests WHERE status='ok' ORDER BY plan_order"
    ):
        path_value = row["response_path"]
        if not path_value:
            raise RuntimeError(f"OK request has no response_path: {row['request_id']}")
        path = Path(str(path_value))
        if not path.exists():
            raise FileNotFoundError(f"Archived response missing: {path}")
        raw = path.read_bytes()
        digest = sha256_bytes(raw)
        if row["response_sha256"] and digest != row["response_sha256"]:
            raise RuntimeError(f"Response checksum mismatch: {path}")
        obj, err = parse_json_bytes(raw)
        if err or obj is None:
            raise RuntimeError(f"Could not parse archived OK response {path}: {err}")
        data = obj.get("data")
        if not isinstance(data, list) or len(data) != 1 or not isinstance(data[0], dict):
            raise RuntimeError(f"Archived OK response no longer validates: {path}")
        yield row, data[0]


def export_processed(args: argparse.Namespace) -> None:
    plan_path = Path(args.plan)
    root = Path(args.root)
    db_path = root / "state" / "qingyue_historical.sqlite"
    if not db_path.exists():
        raise FileNotFoundError(f"State database not found: {db_path}")

    rows, fields = load_plan(plan_path)
    validate_plan(rows)
    conn = connect_db(db_path)
    init_schema(conn)
    import_plan_into_db(conn, plan_path, rows, fields)

    processed_dir = root / "processed"
    processed_dir.mkdir(parents=True, exist_ok=True)
    manifest_path = processed_dir / "qingyue_historical_manifest.csv.gz"
    observations_path = processed_dir / "qingyue_historical_observations.csv.gz"
    summary_path = processed_dir / "qingyue_historical_summary.json"

    # Manifest export: one row per planned request.
    manifest_cols = [
        "request_id", "plan_order", "priority", "plan_component",
        "study_site_code", "study_site_name", "station_identity_id",
        "staname_param", "target_datetime", "status", "attempts",
        "http_status", "api_err_code", "api_err_msg", "api_request_id",
        "response_row_count", "response_path", "response_sha256",
        "first_attempt_at", "last_attempt_at", "completed_at", "last_error",
    ]
    with gzip.open(manifest_path, "wt", encoding="utf-8-sig", newline="") as f:
        w = csv.DictWriter(f, fieldnames=manifest_cols)
        w.writeheader()
        for r in conn.execute("SELECT * FROM requests ORDER BY plan_order"):
            w.writerow({c: r[c] for c in manifest_cols})

    # First pass over OK responses to discover any future API fields not yet known.
    ok_materialized: List[Tuple[sqlite3.Row, Dict[str, Any]]] = []
    unknown_fields = set()
    for r, rec in iter_ok_records(conn):
        ok_materialized.append((r, rec))
        unknown_fields.update(set(rec.keys()).difference(KNOWN_API_FIELDS))
    api_fields = KNOWN_API_FIELDS + sorted(unknown_fields)
    obs_fields = PLAN_EXPORT_FIELDS + api_fields + [
        "api_request_id",
        "response_sha256",
        "response_path",
    ]

    with gzip.open(observations_path, "wt", encoding="utf-8-sig", newline="") as f:
        w = csv.DictWriter(f, fieldnames=obs_fields, extrasaction="ignore")
        w.writeheader()
        for r, rec in ok_materialized:
            out: Dict[str, Any] = {c: r[c] if c in r.keys() else "" for c in PLAN_EXPORT_FIELDS}
            for c in api_fields:
                v = rec.get(c, "")
                if isinstance(v, (dict, list)):
                    v = json.dumps(v, ensure_ascii=False, separators=(",", ":"))
                out[c] = v
            out["api_request_id"] = r["api_request_id"]
            out["response_sha256"] = r["response_sha256"]
            out["response_path"] = r["response_path"]
            w.writerow(out)

    counts = status_counts(conn)
    summary = {
        "executor_version": EXECUTOR_VERSION,
        "exported_at": now_iso(),
        "plan_sha256": get_meta(conn, "plan_sha256"),
        "plan_rows": sum(counts.values()),
        "api_attempts_logged": attempt_count(conn),
        "status_counts": counts,
        "observation_rows_exported": len(ok_materialized),
        "manifest": str(manifest_path),
        "observations": str(observations_path),
    }
    atomic_write_text(
        summary_path,
        json.dumps(summary, ensure_ascii=False, indent=2) + "\n",
    )

    print(f"Manifest:     {manifest_path}")
    print(f"Observations: {observations_path}")
    print(f"Summary:      {summary_path}")
    print(f"Observation rows exported: {len(ok_materialized):,}")
    conn.close()


def status_command(args: argparse.Namespace) -> None:
    plan_path = Path(args.plan)
    root = Path(args.root)
    db_path = root / "state" / "qingyue_historical.sqlite"
    rows, fields = load_plan(plan_path)
    summary = validate_plan(rows)
    print(f"Plan validation PASS: {summary['rows']:,} requests")
    if not db_path.exists():
        print(f"No state database yet: {db_path}")
        return
    conn = connect_db(db_path)
    init_schema(conn)
    import_plan_into_db(conn, plan_path, rows, fields)
    request_id_filter = load_request_id_filter(getattr(args, "request_list", None))
    if request_id_filter is not None:
        known = {str(r["request_id"]) for r in conn.execute("SELECT request_id FROM requests")}
        unknown = sorted(request_id_filter.difference(known))
        if unknown:
            raise RuntimeError(f"Request-list contains {len(unknown)} request_id value(s) not in the approved plan")
        print_subset_status(conn, request_id_filter, args.call_budget)
    else:
        print_status(conn, args.call_budget)
        print_staged_status(conn)
    conn.close()


def validate_command(args: argparse.Namespace) -> None:
    path = Path(args.plan)
    rows, _ = load_plan(path)
    summary = validate_plan(rows)
    print("Plan validation PASS")
    print(f"Rows: {summary['rows']:,}")
    print(f"Unique request IDs: {summary['unique_request_ids']:,}")
    print(f"Unique station-time pairs: {summary['unique_station_times']:,}")
    print(f"Extra-hour rows lacking noon dependency: {summary['noon_dependencies_missing']:,}")
    print(f"Endpoints: {summary['endpoint_count']}")
    print("Components:")
    for k, v in sorted(summary["components"].items()):
        print(f"  {k}: {v:,}")
    print("Study-site codes:")
    for k, v in sorted(summary["study_sites"].items()):
        print(f"  {k or '<blank>'}: {v:,}")
    print(f"SHA256: {sha256_file(path)}")
    print("Stage partition:")
    for stage, count in validate_stage_partition(rows).items():
        print(f"  {stage}: {count:,}")
    print("No API calls were made.")


def backup_state_command(args: argparse.Namespace) -> None:
    """Create a consistent SQLite snapshot, including any outstanding WAL pages."""
    root = Path(args.root)
    db_path = root / "state" / "qingyue_historical.sqlite"
    if not db_path.is_file():
        raise FileNotFoundError(f"No existing SQLite state to back up: {db_path}")
    backups_dir = root / "state" / "backups"
    backups_dir.mkdir(parents=True, exist_ok=True)
    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%d_%H%M%S_%f_UTC")
    target = backups_dir / f"qingyue_historical_before_v{EXECUTOR_VERSION}_{stamp}.sqlite"
    with sqlite3.connect(str(db_path), timeout=60) as source:
        with sqlite3.connect(str(target), timeout=60) as dest:
            source.backup(dest)
    with sqlite3.connect(str(target)) as verified:
        integrity = verified.execute("PRAGMA integrity_check").fetchone()[0]
        if integrity != "ok":
            raise RuntimeError(f"Backup failed SQLite integrity check: {integrity}")
        rows = verified.execute("SELECT COUNT(*) FROM requests").fetchone()[0]
        attempts = verified.execute("SELECT COUNT(*) FROM attempt_log").fetchone()[0]
    print(f"SQLite backup written: {target}")
    print(f"Integrity check: {integrity}")
    print(f"Preserved plan rows: {rows:,}; attempts: {attempts:,}")
    print("No API calls were made.")


def preview_command(args: argparse.Namespace) -> None:
    rows, fields = load_plan(Path(args.plan))
    summary = validate_plan(rows)
    counts = validate_stage_partition(rows)
    print("Qingyue staged acquisition preview (ZERO API calls)")
    print("=" * 52)
    print(f"Approved plan requests: {summary['rows']:,}")
    print(f"Original plan SHA256:   {sha256_file(Path(args.plan))}")
    for stage in ALL_STAGES:
        print(f"  {stage:>13}: {counts.get(stage, 0):>6,}  {STAGE_LABELS[stage]}")
    print(f"  Active stages 1-4: {sum(counts.get(s,0) for s in ACTIVE_STAGES):,}")
    print("Run without --stage defaults to stage 1; 'all' EXCLUDES deferred-2020.")
    db_path = Path(args.root) / "state" / "qingyue_historical.sqlite"
    if db_path.exists():
        conn = connect_db(db_path)
        init_schema(conn)
        import_plan_into_db(conn, Path(args.plan), rows, fields)
        print(f"Existing logged HTTP attempts: {attempt_count(conn):,}")
        print_staged_status(conn)
        conn.close()
    else:
        print("No existing SQLite state at:", db_path)


def add_common_args(p: argparse.ArgumentParser) -> None:
    p.add_argument("--plan", default=str(DEFAULT_PLAN), help="Approved request-plan CSV")
    p.add_argument("--root", default=str(DEFAULT_ROOT), help="Historical archive root directory")
    p.add_argument(
        "--call-budget",
        type=int,
        default=DEFAULT_EXECUTOR_CALL_BUDGET,
        help="Hard ceiling for HTTP attempts logged by this executor state database",
    )


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Execute Qingyue historical API request plan safely")
    sub = p.add_subparsers(dest="command", required=True)

    pv = sub.add_parser("validate", help="Validate request plan; makes zero API calls")
    add_common_args(pv)

    pp = sub.add_parser("preview", help="Show stage allocation and ledger status; zero API calls")
    add_common_args(pp)

    pb = sub.add_parser("backup-state", help="Consistent backup of existing SQLite ledger; zero API calls")
    add_common_args(pb)

    ps = sub.add_parser("status", help="Show state/progress; makes zero API calls")
    add_common_args(ps)
    ps.add_argument(
        "--request-list",
        default=None,
        help="Optional CSV (request_id column) or text file; show status only for this approved-plan subset",
    )

    pe = sub.add_parser("export", help="Rebuild processed CSV.GZ exports; makes zero API calls")
    add_common_args(pe)

    pr = sub.add_parser("run", help="Execute/resume the approved API request plan by stage")
    add_common_args(pr)
    pr.add_argument(
        "--stage",
        choices=("1", "2", "3", "4", "all", "deferred-2020"),
        default="1",
        help="Default 1. Use 1,2,3,4 sequentially; all excludes low-coverage early 2020. "
             "deferred-2020 is explicit opt-in only.",
    )
    pr.add_argument("--signer-path", default=str(DEFAULT_SIGNER))
    pr.add_argument(
        "--request-list",
        default=None,
        help="Optional CSV (request_id column) or text file; execute only these request IDs from the approved plan",
    )
    pr.add_argument("--timeout", type=float, default=DEFAULT_TIMEOUT_SECONDS)
    pr.add_argument("--delay", type=float, default=DEFAULT_DELAY_SECONDS)
    pr.add_argument("--retry-backoff", type=float, default=2.0)
    pr.add_argument(
        "--max-attempts-per-request",
        type=int,
        default=DEFAULT_MAX_ATTEMPTS_PER_REQUEST,
        help="Total attempts allowed for one request before it is quarantined",
    )
    pr.add_argument(
        "--max-new-calls",
        type=int,
        default=None,
        help="Stop after this many new HTTP attempts in the current run (good for smoke tests)",
    )
    pr.add_argument("--component", default=None, help="Run only one plan_component")
    pr.add_argument("--study-site", default=None, help="Run only one study_site_code")
    pr.add_argument("--start-order", type=int, default=None)
    pr.add_argument(
        "--retry-exhausted",
        action="store_true",
        help="Include requests previously marked transient_exhausted",
    )
    pr.add_argument(
        "--retry-auth-failures",
        action="store_true",
        help="Retry only previously logged fatal_error rows whose HTTP status was 401/403",
    )
    return p


def main() -> None:
    args = build_parser().parse_args()
    if args.call_budget <= 0:
        raise SystemExit("--call-budget must be positive")
    if args.command == "validate":
        validate_command(args)
    elif args.command == "preview":
        preview_command(args)
    elif args.command == "backup-state":
        backup_state_command(args)
    elif args.command == "status":
        status_command(args)
    elif args.command == "export":
        export_processed(args)
    elif args.command == "run":
        if args.max_attempts_per_request < 1:
            raise SystemExit("--max-attempts-per-request must be >= 1")
        if args.max_new_calls is not None and args.max_new_calls < 1:
            raise SystemExit("--max-new-calls must be >= 1")
        if args.delay < 0 or args.retry_backoff < 0:
            raise SystemExit("--delay and --retry-backoff must be >= 0")
        run_executor(args)
    else:  # pragma: no cover
        raise SystemExit(f"Unknown command: {args.command}")


if __name__ == "__main__":
    main()

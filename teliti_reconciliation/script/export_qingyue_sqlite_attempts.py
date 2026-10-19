#!/usr/bin/env python3
"""Read all Huawei SQLite attempt logs without modifying any database.

Outputs a single gzipped CSV containing one row per actual executor attempt.
This is separate from the per-request manifest and the successful observations.
"""
from __future__ import annotations

import argparse
import csv
import gzip
import os
import sqlite3
import tempfile
from pathlib import Path
from urllib.parse import quote

COLS = (
    "analysis_huawei_archive", "request_id", "station_identity_id",
    "target_datetime", "plan_component", "study_site_code", "attempt_id",
    "attempted_at", "attempt_number", "http_status", "outcome", "elapsed_ms",
    "response_path", "response_sha256", "response_headers_json", "error_message"
)


def roots(hist_root: Path) -> dict[str, Path]:
    base = {
        "historical_main": hist_root,
        "probe_noon_52": hist_root / "probes" / "fixed2_2020older_v1",
        "probe_nonnoon_180": hist_root / "probes" / "fixed2_nonnoon_2015_2020_v1",
    }
    more = hist_root / "probes"
    if more.is_dir():
        for p in more.iterdir():
            if p.is_dir() and p not in base.values() and (p / "processed" / "qingyue_historical_manifest.csv.gz").is_file():
                base[f"probe_other_{p.name}"] = p
    return base


def readonly_connect(path: Path) -> sqlite3.Connection:
    # URI quoting protects Windows directory names containing spaces and '#'.
    uri = "file:" + quote(path.resolve().as_posix(), safe="/:\\") + "?mode=ro"
    return sqlite3.connect(uri, uri=True)


def export(hist_root: Path, out: Path) -> None:
    input_roots = roots(hist_root)
    if not all((root / "state" / "qingyue_historical.sqlite").is_file() for root in input_roots.values()):
        missing = [str(root / "state" / "qingyue_historical.sqlite") for root in input_roots.values()
                   if not (root / "state" / "qingyue_historical.sqlite").is_file()]
        raise SystemExit("Missing SQLite source(s):\n" + "\n".join(missing))

    out.parent.mkdir(parents=True, exist_ok=True)
    fd, temp = tempfile.mkstemp(prefix=".qingyue_attempts_", suffix=".csv.gz", dir=out.parent)
    os.close(fd)
    count = 0
    try:
        with gzip.open(temp, "wt", encoding="utf-8-sig", newline="") as dst:
            writer = csv.DictWriter(dst, fieldnames=COLS)
            writer.writeheader()
            for name, root in input_roots.items():
                path = root / "state" / "qingyue_historical.sqlite"
                with readonly_connect(path) as conn:
                    conn.row_factory = sqlite3.Row
                    expected = conn.execute("SELECT COUNT(*) FROM attempt_log").fetchone()[0]
                    sql = """
                    SELECT a.attempt_id,a.request_id,a.attempted_at,a.attempt_number,
                           a.http_status,a.outcome,a.elapsed_ms,a.response_path,
                           a.response_sha256,a.response_headers_json,a.error_message,
                           r.station_identity_id,r.target_datetime,r.plan_component,
                           r.study_site_code
                    FROM attempt_log AS a
                    INNER JOIN requests AS r ON a.request_id = r.request_id
                    ORDER BY a.attempt_id
                    """
                    n = 0
                    for row in conn.execute(sql):
                        item = dict(row)
                        item["analysis_huawei_archive"] = name
                        writer.writerow(item)
                        n += 1
                    if n != expected:
                        raise RuntimeError(f"Attempt log contains orphan request IDs in {name}: {n}/{expected}")
                    count += n
                    print(f"{name}: {n:,} attempts (read-only)")
        os.replace(temp, out)
    finally:
        if os.path.exists(temp):
            os.unlink(temp)
    print(f"Total attempt rows: {count:,}")
    print("Saved:", out)


if __name__ == "__main__":
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--historical-root", required=True)
    p.add_argument("--output", required=True)
    a = p.parse_args()
    export(Path(a.historical_root), Path(a.output))

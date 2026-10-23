#!/usr/bin/env python3
"""Build a no-API-call Qingyue request plan matched to Landsat scenes.

Inputs:
  * Landsat all-radii annual CSV exports (2020 onward)
  * Metadata-only GEE scene inventory with exact UTC ISO timestamps
  * Existing approved Qingyue request plan and optional isolated probe plans

Default temporal rule:
  Select the three latest 4-hourly China Standard Time slots strictly before
  each Landsat acquisition. For a typical ~10:30 China-local overpass, these
  are 08:00, 04:00, and 00:00. Slots are calculated from the actual scene time.

This script makes NO API calls and never modifies existing plans or SQLite DBs.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from datetime import date, datetime, time, timedelta, timezone
from pathlib import Path
from urllib.parse import urlencode
from zoneinfo import ZoneInfo

import pandas as pd

CHINA_TZ = ZoneInfo("Asia/Shanghai")
DEFAULT_ENDPOINT = "https://nawaterstation.apistore.huaweicloud.com/api/surface_water/data"
SATELLITE_COMPONENT = "landsat_preceding_time"
FILE_RE = re.compile(r"^landsat_wq_acquisition_all_radii_(\d{4})_v4_corrected\.csv$", re.I)

PLAN_REQUIRED = [
    "request_id", "plan_order", "priority", "plan_component", "study_site_code",
    "study_site_name", "station_identity_id", "cnemc_station_name", "cnemc_area",
    "cnemc_river_basin", "qingyue_station_name", "qingyue_province", "qingyue_city",
    "qingyue_valley", "longitude", "latitude", "qingyue_first_time", "qingyue_last_time",
    "direct_cnemc_first_time", "effective_history_end", "target_datetime", "target_date",
    "target_hour", "city_param", "province_param", "staname_param", "valley_param",
    "version_param", "query_string", "endpoint", "expected_api_calls"
]


def clean(x) -> str:
    if x is None:
        return ""
    s = str(x).strip()
    if s.lower() in {"nan", "none", "null", "<na>"}:
        return ""
    return s


def read_csv_text(path: Path, usecols=None) -> pd.DataFrame:
    return pd.read_csv(path, dtype=str, keep_default_na=False, usecols=usecols, low_memory=False)


def parse_iso(value: str, default_tz=timezone.utc) -> datetime:
    s = clean(value)
    if not s:
        raise ValueError("empty timestamp")
    if s.endswith("Z"):
        s = s[:-1] + "+00:00"
    dtv = datetime.fromisoformat(s)
    if dtv.tzinfo is None:
        dtv = dtv.replace(tzinfo=default_tz)
    return dtv


def parse_bound(value: str, end_of_day=False):
    s = clean(value)
    if not s:
        return None
    # Parse date-only bounds explicitly so end_of_day is respected.
    if re.fullmatch(r"\d{4}-\d{2}-\d{2}", s):
        try:
            d = date.fromisoformat(s)
            t = time(23, 59, 59) if end_of_day else time(0, 0, 0)
            return datetime.combine(d, t, tzinfo=CHINA_TZ)
        except ValueError:
            return None
    try:
        return parse_iso(s, default_tz=CHINA_TZ)
    except ValueError:
        return None


def prior_slots(acq_local: datetime, n_slots: int, hours: list[int]) -> list[datetime]:
    candidates = []
    # Search a few preceding local dates; 3 days is ample for the default 3 slots.
    for offset in range(0, 4):
        day = acq_local.date() - timedelta(days=offset)
        for hour in hours:
            candidate = datetime.combine(day, time(hour=hour), tzinfo=CHINA_TZ)
            if candidate < acq_local:
                candidates.append(candidate)
    candidates.sort(reverse=True)
    return candidates[:n_slots]


def sha_request_id(station_id: str, target: str) -> str:
    payload = f"{SATELLITE_COMPONENT}|{station_id}|{target}".encode("utf-8")
    return hashlib.sha256(payload).hexdigest()[:24]


def get_plan_root(plan_path: Path) -> Path:
    # A plan is expected at <archive-root>/plan/<file>.csv.
    return plan_path.parent.parent


def load_manifest_status(plan_path: Path) -> dict[str, dict[str, str]]:
    root = get_plan_root(plan_path)
    manifest_path = root / "processed" / "qingyue_historical_manifest.csv.gz"
    if not manifest_path.exists():
        return {}
    try:
        m = read_csv_text(manifest_path)
    except Exception as exc:
        print(f"WARNING: could not read manifest {manifest_path}: {exc}", file=sys.stderr)
        return {}
    if "request_id" not in m.columns:
        return {}
    out = {}
    for _, r in m.iterrows():
        rid = clean(r.get("request_id", ""))
        if not rid:
            continue
        out[rid] = {k: clean(r.get(k, "")) for k in [
            "status", "attempts", "http_status", "api_err_code", "api_err_msg",
            "api_request_id", "response_row_count", "response_path", "response_sha256",
            "first_attempt_at", "last_attempt_at", "completed_at", "last_error"
        ]}
    return out


def load_plan(path: Path) -> pd.DataFrame:
    if not path.exists():
        raise FileNotFoundError(f"Plan not found: {path}")
    df = read_csv_text(path)
    missing = [c for c in ["request_id", "station_identity_id", "target_datetime", "study_site_code"] if c not in df.columns]
    if missing:
        raise ValueError(f"Plan {path} missing required columns: {missing}")
    for c in ["station_identity_id", "target_datetime", "study_site_code", "request_id"]:
        df[c] = df[c].map(clean)
    return df


def read_satellite_site_scenes(gee_dir: Path, min_year: int) -> tuple[pd.DataFrame, pd.DataFrame, list[Path]]:
    files = []
    for p in sorted(gee_dir.glob("landsat_wq_acquisition_all_radii_*_v4_corrected.csv")):
        m = FILE_RE.match(p.name)
        if m and int(m.group(1)) >= min_year:
            files.append(p)
    if not files:
        raise FileNotFoundError(f"No annual Landsat files >= {min_year} found in {gee_dir}")

    pieces = []
    needed = {"site_code", "country", "landsat_product_id", "landsat_scene_id"}
    for p in files:
        m = FILE_RE.match(p.name)
        file_year = int(m.group(1))
        header = read_csv_text(p, usecols=lambda c: c in needed).head(0)
        missing = sorted(needed - set(header.columns))
        if missing:
            raise ValueError(f"{p.name} missing required columns: {missing}")
        x = read_csv_text(p, usecols=lambda c: c in needed)
        x["source_file_year"] = str(file_year)
        x["source_file"] = p.name
        for c in ["site_code", "country", "landsat_product_id", "landsat_scene_id"]:
            x[c] = x[c].map(clean)
        x["site_code"] = x["site_code"].str.upper()
        pieces.append(x)

    all_rows = pd.concat(pieces, ignore_index=True)
    all_rows = all_rows[(all_rows.site_code != "") & (all_rows.landsat_product_id != "")]
    all_rows["country_norm"] = all_rows["country"].str.strip().str.casefold()
    china_names = {"china", "cn", "chn", "中国", "people's republic of china", "pr china"}
    china_rows = all_rows[all_rows.country_norm.isin(china_names)].copy()
    non_china_summary = (all_rows[~all_rows.country_norm.isin(china_names)]
        .groupby(["country", "site_code"], dropna=False)
        .agg(source_rows=("landsat_product_id", "size"), products=("landsat_product_id", "nunique"))
        .reset_index())

    site_scene = china_rows[["site_code", "country", "landsat_product_id", "landsat_scene_id", "source_file_year", "source_file"]].drop_duplicates(
        subset=["site_code", "landsat_product_id"], keep="first"
    )
    # One product/site should have a single stable country and scene ID.
    conflict = site_scene.groupby(["site_code", "landsat_product_id"])["landsat_scene_id"].nunique()
    if (conflict > 1).any():
        raise ValueError("Conflicting scene IDs for the same site_code + product ID")
    return site_scene.reset_index(drop=True), non_china_summary, files


def load_scene_inventory(path: Path) -> pd.DataFrame:
    inv = read_csv_text(path)
    required = ["landsat_product_id", "acquisition_time_utc_iso"]
    missing = [c for c in required if c not in inv.columns]
    if missing:
        raise ValueError(f"Scene inventory missing columns: {missing}")
    inv["landsat_product_id"] = inv["landsat_product_id"].map(clean)
    inv["acquisition_time_utc_iso"] = inv["acquisition_time_utc_iso"].map(clean)
    inv = inv[(inv.landsat_product_id != "") & (inv.acquisition_time_utc_iso != "")].copy()
    inv["_parsed_utc"] = inv["acquisition_time_utc_iso"].map(lambda x: parse_iso(x, timezone.utc).astimezone(timezone.utc))
    ntime = inv.groupby("landsat_product_id")["_parsed_utc"].nunique()
    if (ntime > 1).any():
        raise ValueError("Scene inventory has conflicting times for one landsat_product_id")
    inv = inv.sort_values("acquisition_time_utc_iso").drop_duplicates("landsat_product_id", keep="first")
    return inv


def station_catalog_from_plan(plan: pd.DataFrame) -> pd.DataFrame:
    needed = [
        "study_site_code", "study_site_name", "station_identity_id", "cnemc_station_name",
        "cnemc_area", "cnemc_river_basin", "qingyue_station_name", "qingyue_province",
        "qingyue_city", "qingyue_valley", "longitude", "latitude", "qingyue_first_time",
        "qingyue_last_time", "direct_cnemc_first_time", "effective_history_end", "city_param",
        "province_param", "staname_param", "valley_param", "version_param", "endpoint"
    ]
    for c in needed:
        if c not in plan.columns:
            plan[c] = ""
    p = plan[needed].copy()
    p["study_site_code"] = p["study_site_code"].map(clean).str.upper()
    p["station_identity_id"] = p["station_identity_id"].map(clean)
    p = p[(p.study_site_code != "") & (p.station_identity_id != "")]
    # A station identity must belong to one study-site code.
    site_n = p.groupby("station_identity_id")["study_site_code"].nunique()
    if (site_n > 1).any():
        bad = site_n[site_n > 1].index.tolist()[:10]
        raise ValueError(f"Station identities map to multiple study_site_code values: {bad}")
    p = p.sort_values(["study_site_code", "station_identity_id"]).drop_duplicates("station_identity_id", keep="first")
    return p


def manifest_lookup(plan_paths: list[Path]) -> dict[str, dict[str, str]]:
    out = {}
    for p in plan_paths:
        out.update(load_manifest_status(p))
    return out


def make_unified_ledger(plan_paths: list[Path], new_plan: pd.DataFrame, output_path: Path) -> pd.DataFrame:
    rows = []
    for plan_path in plan_paths:
        if not plan_path.exists():
            continue
        df = load_plan(plan_path)
        statuses = load_manifest_status(plan_path)
        root = str(get_plan_root(plan_path))
        for _, r in df.iterrows():
            d = r.to_dict()
            rid = clean(d.get("request_id", ""))
            status = statuses.get(rid, {})
            d["ledger_source_plan"] = str(plan_path)
            d["ledger_source_root"] = root
            d["status"] = status.get("status", "manifest_missing")
            for col in ["attempts", "http_status", "api_err_code", "api_err_msg", "api_request_id", "response_row_count", "response_path", "response_sha256", "first_attempt_at", "last_attempt_at", "completed_at", "last_error"]:
                d[col] = status.get(col, "")
            rows.append(d)
    if len(new_plan):
        for _, r in new_plan.iterrows():
            d = r.to_dict()
            d["ledger_source_plan"] = str(output_path.parent / "plan" / "qingyue_landsat_preceding_PLAN.csv")
            d["ledger_source_root"] = str(output_path.parent)
            d["status"] = "pending"
            d["attempts"] = "0"
            for col in ["http_status", "api_err_code", "api_err_msg", "api_request_id", "response_row_count", "response_path", "response_sha256", "first_attempt_at", "last_attempt_at", "completed_at", "last_error"]:
                d[col] = ""
            rows.append(d)
    if not rows:
        ledger = pd.DataFrame()
    else:
        ledger = pd.DataFrame(rows)
        if "request_id" in ledger:
            dup = ledger[ledger.request_id.duplicated(keep=False)]
            if len(dup):
                dup.to_csv(output_path.parent / "duplicate_request_ids_across_plans.csv", index=False)
                raise ValueError("Duplicate request_id across source plans; see duplicate_request_ids_across_plans.csv")
        ledger = ledger.sort_values(["target_datetime", "study_site_code", "station_identity_id"], kind="stable")
    output_path.parent.mkdir(parents=True, exist_ok=True)
    ledger.to_csv(output_path, index=False, compression="gzip", encoding="utf-8")
    return ledger


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--gee-dir", required=True, type=Path, help="Folder containing annual all-radii Landsat CSVs")
    ap.add_argument("--scene-inventory", required=True, type=Path, help="Metadata-only GEE scene inventory CSV")
    ap.add_argument("--approved-plan", required=True, type=Path, help="Original approved Qingyue request plan")
    ap.add_argument("--existing-plan", action="append", default=[], type=Path, help="Additional existing probe plan; repeat this option")
    ap.add_argument("--output-dir", required=True, type=Path, help="New output directory; no existing plans are overwritten")
    ap.add_argument("--min-year", type=int, default=2020)
    ap.add_argument("--previous-slots", type=int, default=3)
    ap.add_argument("--schedule-hours", default="0,4,8,12,16,20")
    args = ap.parse_args()

    if args.previous_slots < 1:
        raise SystemExit("--previous-slots must be >= 1")
    try:
        hours = sorted(set(int(x.strip()) for x in args.schedule_hours.split(",") if x.strip() != ""))
    except ValueError:
        raise SystemExit("--schedule-hours must be comma-separated integers")
    if not hours or any(h < 0 or h > 23 for h in hours):
        raise SystemExit("Invalid --schedule-hours")

    out_dir = args.output_dir.resolve()
    plan_dir = out_dir / "plan"
    req_dir = out_dir / "requests"
    plan_dir.mkdir(parents=True, exist_ok=True)
    req_dir.mkdir(parents=True, exist_ok=True)

    approved_path = args.approved_plan.resolve()
    existing_paths = [p.resolve() for p in args.existing_plan]
    all_plan_paths = [approved_path] + existing_paths
    for p in all_plan_paths:
        if not p.exists():
            raise SystemExit(f"Existing plan not found: {p}")

    site_scene, non_china_summary, satellite_files = read_satellite_site_scenes(args.gee_dir.resolve(), args.min_year)
    inv = load_scene_inventory(args.scene_inventory.resolve())
    joined = site_scene.merge(inv, on="landsat_product_id", how="left", suffixes=("", "_inventory"), validate="many_to_one")
    missing_times = joined[joined["acquisition_time_utc_iso"].map(clean) == ""].copy()
    if len(missing_times):
        missing_times.to_csv(out_dir / "landsat_scenes_missing_exact_times.csv", index=False, encoding="utf-8")
        raise SystemExit(
            f"{len(missing_times)} site-scene rows lack exact UTC times. "
            f"See {out_dir / 'landsat_scenes_missing_exact_times.csv'}; no plan was generated."
        )

    # Exact scene time is interpreted as UTC, then converted to China Standard Time.
    joined["_acq_utc"] = joined["acquisition_time_utc_iso"].map(lambda x: parse_iso(x, timezone.utc).astimezone(timezone.utc))
    joined["acq_china"] = joined["_acq_utc"].map(lambda x: x.astimezone(CHINA_TZ))
    joined["acquisition_time_china_local_calc"] = joined["acq_china"].map(lambda x: x.strftime("%Y-%m-%d %H:%M:%S"))
    joined["acquisition_time_utc_iso"] = joined["_acq_utc"].map(lambda x: x.strftime("%Y-%m-%dT%H:%M:%SZ"))

    approved_df = load_plan(approved_path)
    station_catalog = station_catalog_from_plan(approved_df)
    station_by_site = {site: g.copy() for site, g in station_catalog.groupby("study_site_code", sort=True)}

    all_existing_keys = {}
    all_existing_rows = []
    existing_conflicts = []
    statuses = manifest_lookup(all_plan_paths)
    for plan_path in all_plan_paths:
        p_df = load_plan(plan_path)
        for _, r in p_df.iterrows():
            key = (clean(r.get("station_identity_id", "")), clean(r.get("target_datetime", "")))
            if not key[0] or not key[1]:
                continue
            rec = {
                "request_id": clean(r.get("request_id", "")),
                "plan_component": clean(r.get("plan_component", "")),
                "source_plan": str(plan_path),
                "status": statuses.get(clean(r.get("request_id", "")), {}).get("status", "manifest_missing"),
            }
            if key in all_existing_keys:
                if all_existing_keys[key]["request_id"] != rec["request_id"]:
                    existing_conflicts.append({"station_identity_id": key[0], "target_datetime": key[1], "first_request_id": all_existing_keys[key]["request_id"], "second_request_id": rec["request_id"], "first_plan": all_existing_keys[key]["source_plan"], "second_plan": rec["source_plan"]})
                # Prefer the approved original plan over isolated probe plans because it is the main source of truth.
                continue
            all_existing_keys[key] = rec
    if existing_conflicts:
        pd.DataFrame(existing_conflicts).to_csv(out_dir / "existing_station_time_key_conflicts.csv", index=False)
        print(f"WARNING: {len(existing_conflicts)} duplicate station-time keys exist across input plans; first plan takes precedence.", file=sys.stderr)

    # Site-code map diagnostic, including Chinese codes not represented in the approved plan.
    site_summary = (joined.groupby(["country", "site_code"], dropna=False)
        .agg(scene_site_pairs=("landsat_product_id", "nunique"), products=("landsat_product_id", "nunique"))
        .reset_index())
    site_summary["has_qingyue_site_match"] = site_summary["site_code"].isin(station_by_site)
    site_summary.to_csv(out_dir / "landsat_china_site_code_mapping.csv", index=False, encoding="utf-8")
    unmatched_sites = site_summary[~site_summary["has_qingyue_site_match"]].copy()
    unmatched_sites.to_csv(out_dir / "landsat_china_site_codes_unmatched.csv", index=False, encoding="utf-8")
    non_china_summary.to_csv(out_dir / "landsat_non_china_sites_excluded.csv", index=False, encoding="utf-8")

    crosswalk_rows = []
    new_by_key = {}
    excluded_rows = []
    scene_slots_rows = []

    for _, scene in joined.sort_values(["site_code", "_acq_utc", "landsat_product_id"]).iterrows():
        site = clean(scene["site_code"]).upper()
        product = clean(scene["landsat_product_id"])
        scene_id = clean(scene.get("landsat_scene_id", ""))
        acq_utc = scene["_acq_utc"]
        acq_local = scene["acq_china"]
        slots = prior_slots(acq_local, args.previous_slots, hours)
        if site not in station_by_site:
            scene_slots_rows.append({
                "site_code": site, "landsat_product_id": product, "landsat_scene_id": scene_id,
                "acquisition_time_utc_iso": scene["acquisition_time_utc_iso"],
                "acquisition_time_china_local": scene["acquisition_time_china_local_calc"],
                "site_match_status": "unmatched_site_code", "station_count": 0,
                "candidate_slots": ";".join(x.strftime("%Y-%m-%d %H:%M:%S") for x in slots)
            })
            continue
        stations = station_by_site[site]
        scene_slots_rows.append({
            "site_code": site, "landsat_product_id": product, "landsat_scene_id": scene_id,
            "acquisition_time_utc_iso": scene["acquisition_time_utc_iso"],
            "acquisition_time_china_local": scene["acquisition_time_china_local_calc"],
            "site_match_status": "matched", "station_count": len(stations),
            "candidate_slots": ";".join(x.strftime("%Y-%m-%d %H:%M:%S") for x in slots)
        })

        for slot_rank, slot_local in enumerate(slots, start=1):
            target = slot_local.strftime("%Y-%m-%d %H:%M:%S")
            delta_minutes = int((acq_local - slot_local).total_seconds() // 60)
            for _, st in stations.iterrows():
                sid = clean(st["station_identity_id"])
                first_dt = parse_bound(st.get("qingyue_first_time", ""), end_of_day=False)
                end_dt = parse_bound(st.get("effective_history_end", ""), end_of_day=True)
                action = "new_request"
                req_id = ""
                existing_status = ""
                existing_source = ""
                if first_dt is not None and slot_local < first_dt:
                    action = "excluded_before_station_history_start"
                elif end_dt is not None and slot_local > end_dt:
                    action = "excluded_after_station_history_end"
                else:
                    key = (sid, target)
                    if key in all_existing_keys:
                        action = "reuse_existing_request"
                        ex = all_existing_keys[key]
                        req_id = ex["request_id"]
                        existing_status = ex["status"]
                        existing_source = ex["source_plan"]
                    else:
                        req_id = sha_request_id(sid, target)
                        action = "new_request"
                        key = (sid, target)
                        if key not in new_by_key:
                            row = {c: clean(st.get(c, "")) for c in PLAN_REQUIRED}
                            row.update({
                                "request_id": req_id,
                                "plan_order": "",
                                "priority": "1",
                                "plan_component": SATELLITE_COMPONENT,
                                "study_site_code": site,
                                "study_site_name": clean(st.get("study_site_name", "")),
                                "station_identity_id": sid,
                                "target_datetime": target,
                                "target_date": slot_local.strftime("%Y-%m-%d"),
                                "target_hour": slot_local.strftime("%H"),
                                "city_param": clean(st.get("city_param", "")),
                                "province_param": clean(st.get("province_param", "")),
                                "staname_param": clean(st.get("staname_param", "")),
                                "valley_param": clean(st.get("valley_param", "")),
                                "version_param": clean(st.get("version_param", "v2")) or "v2",
                                "endpoint": clean(st.get("endpoint", "")) or DEFAULT_ENDPOINT,
                                "expected_api_calls": "1"
                            })
                            params = {
                                "city": row["city_param"],
                                "province": row["province_param"],
                                "sta_time": target,
                                "staname": row["staname_param"],
                                "valley": row["valley_param"],
                                "version": row["version_param"]
                            }
                            row["query_string"] = urlencode(params)
                            if not row["staname_param"]:
                                raise ValueError(f"Empty station name for station_identity_id={sid}")
                            new_by_key[key] = row

                crosswalk_rows.append({
                    "site_code": site,
                    "landsat_product_id": product,
                    "landsat_scene_id": scene_id,
                    "acquisition_time_utc_iso": scene["acquisition_time_utc_iso"],
                    "acquisition_time_china_local": scene["acquisition_time_china_local_calc"],
                    "station_identity_id": sid,
                    "qingyue_station_name": clean(st.get("qingyue_station_name", "")),
                    "target_datetime": target,
                    "preceding_slot_rank": slot_rank,
                    "minutes_before_acquisition": delta_minutes,
                    "match_action": action,
                    "request_id": req_id,
                    "existing_status": existing_status,
                    "existing_source_plan": existing_source
                })
                if action.startswith("excluded_"):
                    excluded_rows.append({
                        "site_code": site, "landsat_product_id": product, "landsat_scene_id": scene_id,
                        "acquisition_time_utc_iso": scene["acquisition_time_utc_iso"],
                        "station_identity_id": sid, "qingyue_station_name": clean(st.get("qingyue_station_name", "")),
                        "target_datetime": target, "preceding_slot_rank": slot_rank,
                        "minutes_before_acquisition": delta_minutes, "exclusion_reason": action,
                        "qingyue_first_time": clean(st.get("qingyue_first_time", "")),
                        "effective_history_end": clean(st.get("effective_history_end", ""))
                    })

    new_plan = pd.DataFrame(list(new_by_key.values()))
    if len(new_plan):
        new_plan = new_plan.sort_values(["target_datetime", "study_site_code", "station_identity_id"], kind="stable").reset_index(drop=True)
        new_plan["plan_order"] = range(1, len(new_plan) + 1)
        # Stable row order and request IDs make plan reruns deterministic.
        new_plan = new_plan[[c for c in PLAN_REQUIRED if c in new_plan.columns] + [c for c in new_plan.columns if c not in PLAN_REQUIRED]]
    else:
        new_plan = pd.DataFrame(columns=PLAN_REQUIRED)

    crosswalk = pd.DataFrame(crosswalk_rows)
    if len(crosswalk):
        crosswalk.to_csv(out_dir / "landsat_qingyue_scene_station_time_crosswalk.csv.gz", index=False, compression="gzip", encoding="utf-8")
    else:
        pd.DataFrame(columns=["site_code", "landsat_product_id", "station_identity_id", "target_datetime", "request_id", "match_action"]).to_csv(out_dir / "landsat_qingyue_scene_station_time_crosswalk.csv.gz", index=False, compression="gzip", encoding="utf-8")

    pd.DataFrame(scene_slots_rows).to_csv(out_dir / "landsat_qingyue_scene_slots.csv", index=False, encoding="utf-8")
    pd.DataFrame(excluded_rows).to_csv(out_dir / "landsat_qingyue_outside_station_history.csv.gz", index=False, compression="gzip", encoding="utf-8")

    plan_path = plan_dir / "qingyue_landsat_preceding_PLAN.csv"
    new_plan.to_csv(plan_path, index=False, encoding="utf-8")
    new_plan["request_id"].to_frame().to_csv(req_dir / "landsat_preceding_all_request_ids.csv", index=False, encoding="utf-8")

    # Emit request lists of <=12 IDs, compatible with the executor's safe batch pattern.
    ids = new_plan["request_id"].tolist() if len(new_plan) else []
    batch_index = 1
    for start in range(0, len(ids), 12):
        batch_ids = ids[start:start + 12]
        batch_name = req_dir / f"SAT{batch_index:04d}_request_ids.csv"
        pd.DataFrame({"request_id": batch_ids}).to_csv(batch_name, index=False, encoding="utf-8")
        batch_index += 1

    # Unified request-level ledger: existing plan/manifest rows plus new pending rows.
    # Execution SQLite DBs remain separate because each executor binds to an immutable plan hash.
    unified_path = out_dir / "qingyue_unified_request_ledger.csv.gz"
    unified = make_unified_ledger(all_plan_paths, new_plan, unified_path)

    summary = {
        "built_at_local": datetime.now().astimezone().isoformat(timespec="seconds"),
        "min_year": args.min_year,
        "previous_slots": args.previous_slots,
        "schedule_hours_china_local": hours,
        "timezone_for_api_targets": "Asia/Shanghai (China Standard Time, UTC+08:00)",
        "landsat_files_used": [str(p) for p in satellite_files],
        "landsat_file_count": len(satellite_files),
        "unique_china_site_scene_pairs": int(len(site_scene)),
        "unique_china_site_codes": sorted(site_scene.site_code.unique().tolist()),
        "unmatched_china_site_codes": sorted(unmatched_sites.site_code.tolist()),
        "non_china_site_codes_excluded": sorted(non_china_summary.site_code.astype(str).unique().tolist()) if len(non_china_summary) else [],
        "scene_inventory_rows": int(len(inv)),
        "new_request_plan_rows": int(len(new_plan)),
        "unique_existing_request_keys_reused": int(len({
            x["request_id"] for x in crosswalk_rows
            if x["match_action"] == "reuse_existing_request" and x["request_id"]
        })),
        "existing_request_crosswalk_links": int(sum(1 for x in crosswalk_rows if x["match_action"] == "reuse_existing_request")),
        "crosswalk_rows": int(len(crosswalk_rows)),
        "excluded_history_window_rows": int(len(excluded_rows)),
        "unified_request_ledger_rows": int(len(unified)),
        "new_plan_sha256": hashlib.sha256(plan_path.read_bytes()).hexdigest() if plan_path.exists() else None,
        "api_calls_made": 0,
        "plan_path": str(plan_path),
        "request_id_lists_dir": str(req_dir),
        "unified_ledger_path": str(unified_path)
    }
    (out_dir / "landsat_qingyue_preceding_plan_summary.json").write_text(json.dumps(summary, indent=2, ensure_ascii=False), encoding="utf-8")

    print("Landsat-Qingyue plan preparation complete; NO API calls were made.")
    print(f"Landsat annual files used: {len(satellite_files)}")
    print(f"Unique China site-scene pairs: {len(site_scene):,}")
    print(f"New API requests: {len(new_plan):,}")
    print(f"Existing station-time requests reused: {summary['unique_existing_request_keys_reused']:,} crosswalk links")
    print(f"Excluded outside station history: {len(excluded_rows):,} links")
    print(f"Unmatched China site codes: {', '.join(summary['unmatched_china_site_codes']) or 'none'}")
    print(f"Unified request ledger rows: {len(unified):,}")
    print(f"Plan: {plan_path}")
    print(f"Summary: {out_dir / 'landsat_qingyue_preceding_plan_summary.json'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

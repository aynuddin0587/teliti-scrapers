#!/usr/bin/env python3
"""Read-only observation QA and approved-plan preflight generation.

Dependencies: Python 3.10+, pandas, numpy.
Does not call a network service, alter original data, or access/write SQLite.
"""
import argparse
import json
from pathlib import Path
import numpy as np
import pandas as pd

CORE = ["water_temp", "ph", "dissolvedoxygen", "codmn", "nh3-n", "tp", "tn"]
OTHERS = ["conductivity", "turbidity", "chlorophyll", "algal_density"]
PARAMS = CORE + OTHERS
QUOTAS = {"BBG": 2, "BHB": 5, "DSB": 1, "DYB": 1, "JZB": 1,
          "LZB": 5, "MJE": 2, "QZB": 1, "SMB": 1, "XMB": 2}
MONTHS_STAGE2 = ["2020-11", "2020-12"]
MONTHS_STAGE3 = [f"2026-{i:02d}" for i in range(1, 8)]
VALID_GRADES = {"\u2160", "\u2161", "\u2162", "\u2163", "\u2164", "\u52a3\u2164"}


def read_csv(path):
    return pd.read_csv(path, dtype={"request_id": str, "station_identity_id": str}, low_memory=False, encoding="utf-8-sig")


def write_csv(df, path):
    df.to_csv(path, index=False, encoding="utf-8-sig")


def audit(ob, manifest, plan, out):
    ob = ob.copy()
    ob["year"] = ob.target_datetime.str[:4]
    ob["year_month"] = ob.target_datetime.str[:7]
    ob["actual_matches_target"] = (pd.to_datetime(ob.monitor_time, errors="coerce") ==
                                   pd.to_datetime(ob.target_datetime, errors="coerce"))
    ob["normal_status"] = ob.section_status.eq("\u6b63\u5e38")
    ob["valid_roman_grade"] = ob.qulity.isin(VALID_GRADES)
    ob["grade_numeric"] = ob.qulity.astype("string").str.fullmatch("[1-6]").fillna(False)
    for key in PARAMS:
        raw = pd.to_numeric(ob[key], errors="coerce")
        ob[key + "__valid"] = raw.mask(raw < 0)
    ob["core_complete"] = ob[[k + "__valid" for k in CORE]].notna().all(axis=1)
    ob["strict_eligible"] = ob.core_complete & ob.normal_status & ob.valid_roman_grade

    data = []
    for key in PARAMS:
        raw = pd.to_numeric(ob[key], errors="coerce")
        clean = ob[key + "__valid"]
        data.append(dict(parameter=key, observations=len(ob), raw_nonmissing=int(raw.notna().sum()),
                         negative_sentinels=int((raw < 0).sum()), valid_nonnegative=int(clean.notna().sum()),
                         percent_valid=round(100 * clean.notna().mean(), 2),
                         p01=round(float(clean.quantile(.01)), 5) if clean.notna().any() else None,
                         median=round(float(clean.median()), 5) if clean.notna().any() else None,
                         p99=round(float(clean.quantile(.99)), 5) if clean.notna().any() else None,
                         max=round(float(clean.max()), 5) if clean.notna().any() else None))
    write_csv(pd.DataFrame(data), out / "parameter_coverage.csv")

    byyear = ob.groupby("year", as_index=False).agg(
        observations=("request_id", "size"), core_complete=("core_complete", "sum"),
        normal_status=("normal_status", "sum"), roman_grade=("valid_roman_grade", "sum"),
        numeric_grade=("grade_numeric", "sum"), strict_eligible=("strict_eligible", "sum"),
        timestamp_match=("actual_matches_target", "sum"))
    write_csv(byyear, out / "year_quality_flags.csv")
    bysite = ob.groupby("study_site_code", as_index=False).agg(
        observations=("request_id", "size"), core_complete=("core_complete", "sum"),
        normal_status=("normal_status", "sum"), strict_eligible=("strict_eligible", "sum"))
    write_csv(bysite, out / "area_quality_flags.csv")

    floors = []
    for key, target in [("nh3-n", .025), ("codmn", .25), ("tp", .005)]:
        x = ob[key + "__valid"]
        floors.append(dict(parameter=key, exact_value=target,
                           count=int(x.eq(target).sum()),
                           percent_all=round(100 * x.eq(target).mean(), 2),
                           percent_nonmissing=round(100 * x.eq(target).sum() / x.notna().sum(), 2)))
    write_csv(pd.DataFrame(floors), out / "possible_reporting_floors.csv")

    obs_noon = ob[ob.target_datetime.str.endswith("12:00:00")].copy()
    obs_noon["day"] = obs_noon.target_datetime.str[:10]
    extra = ob[ob.plan_component.eq("semiannual_full_day_extra")].copy()
    extra["day"] = extra.target_datetime.str[:10]
    joined = extra.merge(obs_noon, on=["station_identity_id", "day"], suffixes=("_extra", "_noon"), validate="many_to_one")
    diurnal = []
    for key in CORE:
        a = joined[key + "__valid_extra"]
        b = joined[key + "__valid_noon"]
        valid = a.notna() & b.notna()
        diurnal.append(dict(parameter=key, pairs=int(valid.sum()), exact_equals=int((a[valid] == b[valid]).sum()),
                            median_abs_diff=float((a[valid] - b[valid]).abs().median()) if valid.any() else None))
    write_csv(pd.DataFrame(diurnal), out / "diurnal_variation_check.csv")

    # Classification of stage-4 pending requests by their respective noon's status.
    noon_man = manifest[manifest.plan_component.eq("monthly_noon_backbone")].copy()
    noon_man["day"] = noon_man.target_datetime.str[:10]
    noon_index = noon_man.set_index(["station_identity_id", "day"])["status"]
    stage4 = manifest[(manifest.plan_component == "semiannual_full_day_extra") &
                      (manifest.target_datetime.str[:4] >= "2021")].copy()
    stage4["day"] = stage4.target_datetime.str[:10]
    stage4["noon_status"] = [noon_index.get((row.station_identity_id, row.day), "NOON_NOT_PLANNED")
                              for row in stage4.itertuples()]
    dep = stage4.groupby(["status", "noon_status"], dropna=False).size().reset_index(name="requests")
    write_csv(dep, out / "stage4_dependency.csv")

    # The sentinel panel is drawn ONLY from original, approved station/time request IDs.
    selected = choose_sentinels(ob, plan)
    write_csv(selected, out / "preflight_station_roster.csv")
    sids = set(selected.station_identity_id)
    required = MONTHS_STAGE2 + MONTHS_STAGE3
    pre = plan[(plan.station_identity_id.isin(sids)) &
               (plan.target_datetime.str[:7].isin(required)) &
               (plan.plan_component == "monthly_noon_backbone")].copy()
    pre["ym"] = pre.target_datetime.str[:7]
    assert len(pre) == sum(QUOTAS.values()) * len(required), (len(pre),sum(QUOTAS.values())*len(required))
    assert pre.groupby("ym").size().eq(sum(QUOTAS.values())).all()
    assert pre.request_id.is_unique
    for stage, months in [(2,MONTHS_STAGE2),(3,MONTHS_STAGE3)]:
        pp = pre[pre.ym.isin(months)].sort_values(["ym", "study_site_code", "station_identity_id"])
        (out / f"stage{stage}_preflight_request_ids.txt").write_text("\n".join(pp.request_id) + "\n", encoding="utf-8")
        write_csv(pp[["request_id","target_datetime","study_site_code","cnemc_station_name","station_identity_id"]],
                  out / f"stage{stage}_preflight_inventory.csv")

    flag_counts = dict(
        observations=int(len(ob)),
        retrieved_stage1=int(((ob.plan_component == "monthly_noon_backbone") & (ob.year.between("2021", "2025"))).sum()),
        timestamp_exact_match=int(ob.actual_matches_target.sum()),
        duplicate_request_ids=int(ob.request_id.duplicated().sum()),
        duplicate_station_monitor_time=int(ob.duplicated(["station_identity_id","monitor_time"]).sum()),
        normal_status=int(ob.normal_status.sum()),
        maintenance_status=int(ob.section_status.eq("\u7ef4\u62a4").sum()),
        missing_status=int(ob.section_status.isna().sum()),
        valid_roman_grade=int(ob.valid_roman_grade.sum()),
        numeric_grade=int(ob.grade_numeric.sum()),
        invalid_negative_grade=int(ob.qulity.isin(["-3", "-1"]).sum()),
        complete_core=int(ob.core_complete.sum()),
        strict_eligible=int(ob.strict_eligible.sum()),
        extra_hour_rows=int(len(extra)),
        extra_hour_noon_pairs=int(len(joined)),
        six_point_days=int(joined[["station_identity_id", "day"]].drop_duplicates().shape[0]),
        stage2_preflight_ids=42,
        stage3_preflight_ids=147,
        already_eligible_stage4_noon_no_record=int(stage4[(stage4.status == "pending") & (stage4.noon_status == "no_record")].shape[0]))
    (out / "summary_metrics.json").write_text(json.dumps(flag_counts,indent=2,ensure_ascii=False)+"\n",encoding="utf-8")
    assert flag_counts["observations"] == int((manifest.status == "ok").sum())
    assert flag_counts["timestamp_exact_match"] == len(ob)
    assert flag_counts["duplicate_request_ids"] == 0
    assert flag_counts["duplicate_station_monitor_time"] == 0
    assert manifest.request_id.is_unique
    return flag_counts, selected, pre, stage4


def choose_sentinels(ob, plan):
    needed = MONTHS_STAGE2 + MONTHS_STAGE3
    q = plan[(plan.plan_component == "monthly_noon_backbone") &
             (plan.target_datetime.str[:7].isin(needed))].copy()
    q["ym"] = q.target_datetime.str[:7]
    eligibility = q.groupby("station_identity_id").ym.nunique()
    allowed = set(eligibility[eligibility.eq(len(needed))].index)
    stage1 = ob[(ob.plan_component == "monthly_noon_backbone") &
                (ob.target_datetime.str[:4].between("2021", "2025"))]
    hist = stage1.groupby("station_identity_id").size().rename("historical_ok").to_dict()
    candidates = (q[q.station_identity_id.isin(allowed)]
                   .drop_duplicates("station_identity_id")
                   .assign(historical_ok=lambda z: z.station_identity_id.map(hist).fillna(0).astype(int)))
    chosen = []
    for site, quota in QUOTAS.items():
        pool = candidates[candidates.study_site_code.eq(site)].copy()
        if site == "LZB":
            # Retain plausible Yellow River and Shandong coastal catchments;
            # do not use Northwest internal drainage or Haihe-assigned entries.
            pool = pool[(pool.cnemc_river_basin.eq("\u9ec4\u6cb3\u6d41\u57df")) |
                        (pool.qingyue_province.eq("\u5c71\u4e1c\u7701"))]
        if site == "BHB":
            pool = pool[pool.cnemc_river_basin.eq("\u6d77\u6cb3\u6d41\u57df")]
        assert len(pool) >= quota, (site, len(pool), quota)
        seen_province, seen_city = set(), set()
        if site == "LZB":
            # Include two Shandong stations representing different cities;
            # this is a sampling heuristic, not confirmation of basin topology.
            sd = pool[pool.qingyue_province.eq("\u5c71\u4e1c\u7701")].copy()
            coast = sd[(pd.to_numeric(sd.longitude, errors="coerce") >= 118.2) &
                       (pd.to_numeric(sd.latitude, errors="coerce") >= 37.0)]
            assert not coast.empty
            first = coast.sort_values(["historical_ok", "station_identity_id"],ascending=[False,True]).iloc[0]
            for seed in [first]:
                chosen.append(seed.to_dict())
                seen_province.add(seed.qingyue_province)
                seen_city.add(seed.qingyue_city)
                pool = pool[pool.station_identity_id.ne(seed.station_identity_id)].copy()
            sd2 = pool[pool.qingyue_province.eq("\u5c71\u4e1c\u7701") &
                       ~pool.qingyue_city.isin(seen_city)]
            assert not sd2.empty
            second = sd2.sort_values(["historical_ok", "station_identity_id"],ascending=[False,True]).iloc[0]
            chosen.append(second.to_dict())
            seen_city.add(second.qingyue_city)
            pool = pool[pool.station_identity_id.ne(second.station_identity_id)].copy()
            quota -= 2
        for _ in range(quota):
            pool["score"] = (pool.historical_ok.astype(float)
                             + np.where(~pool.qingyue_province.isin(seen_province), 12, 0)
                             + np.where(~pool.qingyue_city.isin(seen_city), 7, 0))
            top = pool.sort_values(["score", "historical_ok", "cnemc_station_name", "station_identity_id"],
                                    ascending=[False,False,True,True]).iloc[0]
            chosen.append(top.drop(labels=["score"]).to_dict())
            seen_province.add(top.qingyue_province)
            seen_city.add(top.qingyue_city)
            pool = pool[pool.station_identity_id.ne(top.station_identity_id)].copy()
    sel = pd.DataFrame(chosen)
    return sel[["study_site_code", "station_identity_id", "cnemc_station_name", "qingyue_province",
                "qingyue_city", "cnemc_river_basin", "historical_ok", "longitude", "latitude"]]


def evaluate(manifest, out):
    root = out
    sent = pd.read_csv(root / "preflight_station_roster.csv", dtype={"station_identity_id": str},encoding="utf-8-sig")
    allowed = sent[["study_site_code","station_identity_id"]]
    x = manifest.merge(allowed,on=["station_identity_id","study_site_code"],how="inner")
    x = x[x.target_datetime.str[:7].isin(MONTHS_STAGE2+MONTHS_STAGE3) &
          x.plan_component.eq("monthly_noon_backbone")].copy()
    x["ym"] = x.target_datetime.str[:7]
    rows=[]
    for month in MONTHS_STAGE2 + MONTHS_STAGE3:
        t = x[x.ym.eq(month)]
        # Note: manifest is the FULL planned ledger, not only previously called rows.
        n_ok = int((t.status == "ok").sum()); n_not = int((t.status=="no_record").sum())
        n_pending = int((t.status == "pending").sum())
        bhb = int(((t.study_site_code == "BHB") & (t.status == "ok")).sum())
        lzb = int(((t.study_site_code == "LZB") & (t.status == "ok")).sum())
        if len(t)!=sum(QUOTAS.values()) or n_pending or (n_ok+n_not)!=len(t):
            verdict = "INCOMPLETE"
        elif n_ok >= 12 and bhb >= 2 and lzb >= 2:
            verdict = "PASS_REVIEW"
        else:
            verdict = "HOLD_REVIEW"
        rows.append(dict(month=month,planned_sentinals=len(t),ok=n_ok,no_record=n_not,
                         pending=n_pending,bohai_ok_of5=bhb,laizhou_ok_of5=lzb,decision=verdict))
    res=pd.DataFrame(rows)
    write_csv(res, out / "preflight_gate_report.csv")
    return res


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument("--observations", help="Exported observations CSV.GZ (needed for full audit)")
    p.add_argument("--manifest", required=True)
    p.add_argument("--plan", help="Original approved plan CSV (needed for full audit)")
    p.add_argument("--output",required=True)
    p.add_argument("--gate-only",action="store_true")
    args=p.parse_args(); out=Path(args.output); out.mkdir(exist_ok=True,parents=True)
    man=read_csv(args.manifest)
    if args.gate_only:
        r=evaluate(man,out); print(r.to_string(index=False)); return
    if not args.observations or not args.plan:
        p.error("--observations and --plan are required except with --gate-only")
    obs=read_csv(args.observations); plan=read_csv(args.plan)
    m,sel,pre,stage4=audit(obs,man,plan,out)
    print(json.dumps(m,indent=2))
    r=evaluate(man,out)
    print("PREFLIGHT INVENTORY:",len(sel),"stations",len(pre),"original-plan request IDs")
    print("GATE INITIAL STATUS:")
    print(r.to_string(index=False))

if __name__=="__main__": main()

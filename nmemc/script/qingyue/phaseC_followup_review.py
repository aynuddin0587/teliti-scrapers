#!/usr/bin/env python3
"""Read-only Phase C follow-up QC (stdlib only, no network/SQLite/API calls).

Read all original 400 approved extra-hour request IDs and report their latest
manifest status, strict measurement quality, complete six-point days, and
January-July station-year pairs. Supports partial or completed acquisitions.
Only writes to --output; does not perform acquisition or authorise calls.
"""
import argparse,csv,gzip,json,math,collections
from pathlib import Path

CORE=('water_temp','ph','dissolvedoxygen','codmn','nh3-n','tp','tn')
HOURS=(0,4,8,12,16,20)

def read_csv(p):
    opener=gzip.open if str(p).endswith('.gz') else open
    with opener(p,'rt',encoding='utf-8-sig',newline='') as inp:
        return list(csv.DictReader(inp))

def write_csv(p,rows,headers):
    with p.open('w',encoding='utf-8-sig',newline='') as out:
        w=csv.DictWriter(out,fieldnames=headers,extrasaction='ignore');w.writeheader();w.writerows(rows)

def num(v):
    try:
        z=float(v)
        return z if math.isfinite(z) else None
    except (TypeError,ValueError):return None

def strict(r):
    if not r or r.get('section_status')!='正常' or r.get('monitor_time')!=r.get('target_datetime'):return False
    z=[num(r.get(k)) for k in CORE]
    return all(v is not None and v>=0 for v in z) and z[1]>0

def inspect(args):
    ids=[x.strip() for x in Path(args.request_list).read_text(encoding='utf-8-sig').splitlines() if x.strip() and not x.lstrip().startswith('#')]
    if len(ids)!=400 or len(set(ids))!=400:raise SystemExit('STOP: expected exactly 400 unique Phase C follow-up IDs')
    idset=set(ids)
    plan={r['request_id']:r for r in read_csv(args.plan) if r['request_id'] in idset}
    if set(plan)!=idset or any(r.get('plan_component')!='semiannual_full_day_extra' for r in plan.values()):raise SystemExit('STOP: mismatch with original approved Stage 4 plan')
    if any(not r['target_datetime'].startswith(('2021-','2022-','2023-','2024-')) for r in plan.values()):raise SystemExit('STOP: unexpected target year')
    manifest={r['request_id']:r for r in read_csv(args.manifest) if r['request_id'] in idset}
    if set(manifest)!=idset:raise SystemExit('STOP: missing IDs in exported manifest')
    obsrows=read_csv(args.observations)
    obs={r['request_id']:r for r in obsrows}
    stationtime={(r['station_identity_id'],r['target_datetime']):r for r in obsrows}
    counts=collections.Counter(r['status'] for r in manifest.values())
    if any(status not in ('pending','ok','no_record','skipped_noon_missing') for status in counts):
        raise SystemExit('STOP: unexpected request errors; inspect executor status')
    issues=[]; days=collections.defaultdict(list)
    for rid in ids:
        p=plan[rid];m=manifest[rid];r=obs.get(rid);s=m['status']
        days[(p['station_identity_id'],p['target_datetime'][:10])].append(rid)
        if s=='ok' and r is None:raise SystemExit('STOP: missing exported observation for '+rid)
        if s=='ok' and strict(r):continue
        why=s.upper() if s!='ok' else ('MAINTENANCE' if r.get('section_status')=='维护' else 'NON_NORMAL_OR_CORE_INVALID')
        issues.append(dict(request_id=rid,site=p['study_site_code'],station=p['cnemc_station_name'],datetime=p['target_datetime'],status=s,section_status=r.get('section_status','') if r else '',reason=why))
    if len(days)!=80 or any(len(rs)!=5 for rs in days.values()):raise SystemExit('STOP: expected 80 unique station-days with five hours each')
    output=Path(args.output);output.mkdir(parents=True,exist_ok=True)
    drows=[]
    for (sid,date), rids in sorted(days.items()):
        p=plan[rids[0]]
        vals=[stationtime.get((sid,f'{date} {hour:02}:00:00')) for hour in HOURS]
        available=sum(manifest[rid]['status']=='ok' for rid in rids)
        strict_extra=sum(manifest[rid]['status']=='ok' and strict(obs.get(rid)) for rid in rids)
        complete=all(strict(v) for v in vals)
        rec=dict(site=p['study_site_code'],station=p['cnemc_station_name'],station_identity_id=sid,date=date,year=date[:4],month=date[5:7],extra_ok=available,extra_strict=strict_extra,noon_valid=strict(vals[3]),strict_sixpoint=complete)
        if complete:
            ds=[num(v['dissolvedoxygen']) for v in vals]; rec.update(DO_min=min(ds),DO_noon=ds[3],DO_max=max(ds),DO_range=max(ds)-min(ds),DO_noon_minus_sixmean=ds[3]-(sum(ds)/6),DO_noon_minus_sampled_min=ds[3]-min(ds))
        drows.append(rec)
    write_csv(output/'phaseC_followup_day_results.csv',drows,['site','station','station_identity_id','date','year','month','extra_ok','extra_strict','noon_valid','strict_sixpoint','DO_min','DO_noon','DO_max','DO_range','DO_noon_minus_sixmean','DO_noon_minus_sampled_min'])
    write_csv(output/'phaseC_followup_issues.csv',issues,['site','station','datetime','request_id','status','section_status','reason'])
    grouped=collections.defaultdict(dict)
    for r in drows:grouped[(r['station_identity_id'],r['year'])][r['month']]=r
    pairs=sum(x.get('01',{}).get('strict_sixpoint',False) and x.get('07',{}).get('strict_sixpoint',False) for x in grouped.values())
    strict_count=sum(manifest[rid]['status']=='ok' and strict(obs.get(rid)) for rid in ids)
    valid_days=sum(r['strict_sixpoint'] for r in drows)
    bysite={}
    for code in sorted(set(p['study_site_code'] for p in plan.values())):
        rr=[rid for rid in ids if plan[rid]['study_site_code']==code]
        bysite[code]={'requests':len(rr),'ok':sum(manifest[rid]['status']=='ok' for rid in rr),'strict_valid':sum(manifest[rid]['status']=='ok' and strict(obs.get(rid)) for rid in rr),'no_record':sum(manifest[rid]['status']=='no_record' for rid in rr),'pending':sum(manifest[rid]['status']=='pending' for rid in rr)}
    result=dict(decision='NOT_STARTED' if counts['pending']==400 else ('PARTIAL_REVIEW' if counts['pending'] else 'COMPLETED_REVIEW'),
                note='Review-only. No automatic authorisation of new calls. Pending input is never treated as no-record.',
                requests=400,status_counts=dict(counts),strict_valid_extra_hours=strict_count,
                strict_sixpoint_days=valid_days,total_days=80,strict_Jan_Jul_pairs=pairs,total_pairs=40,by_site=bysite)
    (output/'PHASEC_FOLLOWUP_SUMMARY.json').write_text(json.dumps(result,ensure_ascii=False,indent=2),encoding='utf-8')
    lines=['# Stage 4 Phase C follow-up: read-only QC','','**Status: '+result['decision']+'**','',f"Requests: {sum(counts.values())}/400; OK={counts['ok']}, no_record={counts['no_record']}, pending={counts['pending']}.",f'Strict normal+seven-core extra-hour records: **{strict_count}/400**.',f'Strict six-point days: **{valid_days}/80**.',f'Valid paired January–July station-years: **{pairs}/40**.','','| Study area | OK | Strict valid | No record | Pending |','|---|---:|---:|---:|---:|']
    for site,v in bysite.items():lines.append(f"| {site} | {v['ok']} | {v['strict_valid']} | {v['no_record']} | {v['pending']} |")
    lines+=['','The results are descriptive and require manual review before further acquisition. Strict means normal status, matching time and complete nonnegative seven-core measurements. This does not independently validate units, calibration, outliers or hydrological connectivity.','','No Huawei calls; original plan/ledger unchanged.']
    (output/'PHASEC_FOLLOWUP_REPORT.md').write_text('\n'.join(lines)+'\n',encoding='utf-8')
    print('\n'.join(lines))

if __name__=='__main__':
    ap=argparse.ArgumentParser(description=__doc__)
    for k in ('plan','manifest','observations','request_list','output'):ap.add_argument('--'+k.replace('_','-'),required=True)
    inspect(ap.parse_args())

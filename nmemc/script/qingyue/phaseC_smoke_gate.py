#!/usr/bin/env python3
"""Read-only Stage 4 Phase C (100-call) quality/retrieval gate.

This utility NEVER performs Huawei requests or edits SQLite. It reports a
REVIEW recommendation, not approval or automatic launch of the other 400 calls.
"""
import argparse,csv,gzip,json,math
from pathlib import Path
from collections import Counter,defaultdict
HOURS=(0,4,8,12,16,20)
CORE=('water_temp','ph','dissolvedoxygen','codmn','nh3-n','tp','tn')

def read(path):
    with (gzip.open(path,'rt',encoding='utf-8-sig',newline='') if str(path).endswith('.gz')
          else open(path,encoding='utf-8-sig',newline='')) as f:return list(csv.DictReader(f))

def val(x):
    try:
        n=float(x)
        return n if math.isfinite(n) else None
    except (ValueError,TypeError):return None

def strict(r):
    vals=[val(r.get(k)) for k in CORE]
    return r.get('section_status')=='正常' and r.get('monitor_time')==r.get('target_datetime') and all(v is not None and v>=0 for v in vals) and vals[1]>0

def main():
    ap=argparse.ArgumentParser(description=__doc__)
    for key in ['plan','manifest','observations','request_list','output']:
        ap.add_argument('--'+key.replace('_','-'),required=True)
    a=ap.parse_args()
    ids=[l.strip() for l in Path(a.request_list).read_text(encoding='utf-8-sig').splitlines() if l.strip() and not l.startswith('#')]
    if len(ids)!=100 or len(ids)!=len(set(ids)):raise SystemExit('STOP: smoke request list should be exactly 100 unique approved IDs')
    idset=set(ids)
    plan={r['request_id']:r for r in read(a.plan) if r['request_id'] in idset}
    if len(plan)!=100 or any(r['plan_component']!='semiannual_full_day_extra' for r in plan.values()):raise SystemExit('STOP: smoke list does not match plan')
    statuses={r['request_id']:r['status'] for r in read(a.manifest) if r['request_id'] in idset}
    if len(statuses)!=100:raise SystemExit('STOP: Manifest does not include every smoke request')
    obsrows=read(a.observations)
    obs={r['request_id']:r for r in obsrows}
    idx={(r['station_identity_id'],r['target_datetime']):r for r in obsrows}
    summary=dict(Counter(statuses.values()))
    ok=summary.get('ok',0);bad=summary.get('no_record',0);pending=summary.get('pending',0)
    qc=0;byday=defaultdict(list)
    for rid in ids:
        r=plan[rid]
        date=r['target_datetime'][:10]
        byday[(r['station_identity_id'],date)].append(rid)
        if statuses[rid]=='ok':
            if rid not in obs:raise SystemExit('STOP: Exported observation missing for OK request '+rid)
            if strict(obs[rid]):qc+=1
    if len(byday)!=20 or any(len(rids)!=5 for rids in byday.values()):raise SystemExit('STOP: 20 days x five extra-hour requests required')
    outrows=[]
    for (sid,date),rids in sorted(byday.items()):
        extra=[(statuses[rid],obs.get(rid)) for rid in rids]
        noon=idx.get((sid,date+' 12:00:00'))
        strictday=noon is not None and strict(noon) and all(st=='ok' and row is not None and strict(row) for st,row in extra)
        meta=plan[rids[0]]
        result={'station_identity_id':sid,'study_site_code':meta['study_site_code'],
                'station_name':meta['cnemc_station_name'],'date':date,
                'supplementary_ok':sum(x[0]=='ok' for x in extra),
                'supplementary_no_record':sum(x[0]=='no_record' for x in extra),
                'supplementary_valid_normal_core':sum(x[0]=='ok' and x[1] is not None and strict(x[1]) for x in extra),
                'strict_sixpoint':strictday}
        if strictday:
            v=[val(idx[(sid,f'{date} {hour:02}:00:00')]['dissolvedoxygen']) for hour in HOURS]
            noon_do=v[3]
            result.update({'six_point_DO_range':max(v)-min(v),
                           'noon_minus_mean_DO':noon_do-(sum(v)/6),
                           'noon_minus_observed_min_DO':noon_do-min(v)})
        outrows.append(result)
    if pending or any(k not in ('ok','no_record') for k in summary):decision='INCOMPLETE_OR_ERROR'
    elif ok>=85 and qc>=75 and sum(r['strict_sixpoint'] for r in outrows)>=10:decision='REVIEW_GO'
    else:decision='HOLD_REVIEW'
    info={
      'decision':decision,'note':'Review-only decision. Does not authorize or execute additional calls.',
      'expected_requests':100,'resolved':100-pending,'http_200_ok':ok,'no_record':bad,'pending':pending,
      'strict_normal_complete_core_extra_observations':qc,
      'strict_complete_sixpoint_station_days':sum(r['strict_sixpoint'] for r in outrows),
      'expected_station_days':20,
      'criteria_review_go':'all resolved; OK>=85; strict extra observations>=75; strictly complete station-days>=10'
    }
    output=Path(a.output);output.mkdir(parents=True,exist_ok=True)
    (output/'PHASEC_SMOKE_RESULT.json').write_text(json.dumps(info,ensure_ascii=False,indent=2),encoding='utf-8')
    cols=list(dict.fromkeys(k for r in outrows for k in r))
    with (output/'phaseC_smoke_day_details.csv').open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=cols);w.writeheader();w.writerows(outrows)
    report='\n'.join([
      '# Stage 4 Phase C: 100-call smoke assessment','',
      f'**Decision: {decision}** (review-only; no automatic Stage 4 expansion)','',
      f'- Resolved: {100-pending}/100; HTTP 200: {ok}; no record: {bad}.',
      f'- Valid core+normal extra-hour records: {qc}/100.',
      f'- Strictly complete six-point station-days: {info["strict_complete_sixpoint_station_days"]}/20.',
      '- Review thresholds: all resolved; at least 85 HTTP successes, 75 strict extra-hour records, and 10 complete station-days.',
      '',
      'Run Phase C follow-up requests only after reviewing this report, the individual measurements, geographical assignments and remaining API budget.',''
    ])
    (output/'PHASEC_SMOKE_REPORT.md').write_text(report,encoding='utf-8')
    print(report)

if __name__=='__main__':main()

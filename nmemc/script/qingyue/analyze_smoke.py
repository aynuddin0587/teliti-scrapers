#!/usr/bin/env python3
"""Read-only reproducible scientific review of Stage 4 Phase C smoke responses."""
from pathlib import Path
from collections import Counter
import hashlib,json
import numpy as np
import pandas as pd

ROOT=Path('/mnt/data')
PACKAGE=ROOT/'qingyue_stage4_phaseC'/'output'
OUT=ROOT/'qingyue_stage4_phaseC_review'
PLAN=ROOT/'qingyue_staged_release'/'qingyue_request_plan_recommended.csv'
MF=ROOT/'qingyue_historical_manifest.csv.gz'
OB=ROOT/'qingyue_historical_observations.csv.gz'
CORE=['water_temp','ph','dissolvedoxygen','codmn','nh3-n','tp','tn']
HOURS=[0,4,8,12,16,20]

m=pd.read_csv(MF,dtype=str,keep_default_na=False,encoding='utf-8-sig')
o=pd.read_csv(OB,dtype=str,keep_default_na=False,encoding='utf-8-sig')
smoke=[x.strip() for x in (PACKAGE/'phaseC_smoke_100.txt').read_text('utf-8-sig').splitlines() if x.strip() and not x.startswith('#')]
follow=[x.strip() for x in (PACKAGE/'phaseC_followup_400.txt').read_text('utf-8-sig').splitlines() if x.strip() and not x.startswith('#')]
assert len(smoke)==100 and len(set(smoke))==100
assert len(follow)==400 and len(set(follow))==400 and not(set(follow)&set(smoke))
assert hashlib.sha256(PLAN.read_bytes()).hexdigest()=='a8e0c166f199971f63bdb209d4e5c13bbf5dae21eec9ebd45447ad6c453c6587'
p=pd.read_csv(PLAN,dtype=str,keep_default_na=False,encoding='utf-8-sig')
p=p[p.request_id.isin(smoke+follow)]
assert len(p)==500
p['phase']=np.where(p.request_id.isin(smoke),'smoke','follow')
assert set(p.plan_component)=={'semiannual_full_day_extra'}
assert not p.target_datetime.str.startswith('2025').any()
assert not p.target_datetime.str.startswith('2026').any()
sel=m[m.request_id.isin(smoke)].copy()
print('INPUT manifest',len(m),m.status.value_counts().to_dict(), 'observations',len(o))
print('SMOKE',sel.status.value_counts().to_dict())
assert len(sel)==100 and Counter(sel.status)==Counter({'ok':95,'no_record':5})
assert not set(follow) & set(m[m.status!='pending'].request_id)
assert set(follow) <= set(m[m.status=='pending'].request_id)
assert not set(follow) & set(o.request_id)

for x in CORE:
    o[x+'_num']=pd.to_numeric(o[x],errors='coerce')
o['strict']=o.section_status.eq('正常') & o.monitor_time.eq(o.target_datetime) & o[[v+'_num' for v in CORE]].ge(0).all(axis=1) & o['ph_num'].gt(0)
idx={(r.station_identity_id,r.target_datetime):r for _,r in o.iterrows()}
sm_p=p[p.phase=='smoke'].merge(sel[['request_id','status','http_status']],on='request_id',how='left',validate='one_to_one')
extra=sm_p.merge(o[['request_id','section_status','monitor_time','strict']+[v for v in CORE]+[v+'_num' for v in CORE]],on='request_id',how='left',validate='one_to_one')
extra['strict']=extra.strict.fillna(False).astype(bool)
extra['valid_ok']=extra.status.eq('ok') & extra.strict
assert extra.valid_ok.sum()==90
print('smoke by site:',extra.groupby('study_site_code',sort=False).agg(ok=('status',lambda x:(x=='ok').sum()),no_record=('status',lambda x:(x=='no_record').sum()),valid=('valid_ok','sum'),total=('request_id','size')).to_string())

qc_issues=extra[~extra.valid_ok].copy()
qc_issues['qc_reason']=qc_issues.apply(lambda r: 'NO_RECORD' if r.status=='no_record' else ('MAINTENANCE' if r.section_status=='维护' else ('OTHER_STATUS' if r.section_status!='正常' else ('TIMESTAMP_MISMATCH' if r.monitor_time!=r.target_datetime else ('CORE_MISSING_OR_INVALID' if not bool(r.strict) else 'OTHER')))),axis=1)
qc_cols=['study_site_code','cnemc_station_name','station_identity_id','target_datetime','request_id','status','section_status','qc_reason']+CORE
qc_issues[qc_cols].sort_values(['study_site_code','cnemc_station_name','target_datetime']).to_csv(OUT/'smoke_hour_qc_issues.csv',index=False,encoding='utf-8-sig')
print('QC issues:',qc_issues.qc_reason.value_counts().to_dict())
print(qc_issues[qc_cols].to_string(index=False,max_colwidth=22))

out=[]
for (sid,date),g in extra.groupby(['station_identity_id',extra.target_datetime.str.slice(0,10)],sort=True):
    r=g.iloc[0]
    noon=idx.get((sid,date+' 12:00:00'))
    rows=[]
    for hh in HOURS:
        q=idx.get((sid,f'{date} {hh:02d}:00:00'))
        rows.append(q)
    valid=bool(all(q is not None and bool(q.strict) for q in rows))
    rec=dict(site=r.study_site_code,station=r.cnemc_station_name,station_identity_id=sid,date=date,year=int(date[:4]),month=int(date[5:7]),longitude=float(r.longitude),latitude=float(r.latitude),province=r.qingyue_province,river=r.qingyue_valley,
        http_ok=int(g.status.eq('ok').sum()),http_no_record=int(g.status.eq('no_record').sum()),extra_valid=int(g.valid_ok.sum()),noon_strict=bool(noon is not None and noon.strict),strict_sixpoint=valid)
    if valid:
        do=np.array([float(q.dissolvedoxygen_num) for q in rows]); temp=np.array([float(q.water_temp_num) for q in rows]);
        rec.update(DO_min=do.min(),DO_noon=do[3],DO_max=do.max(),DO_mean6=do.mean(),DO_range=np.ptp(do),DO_noon_minus_mean6=do[3]-do.mean(),DO_abs_noon_minus_mean6=abs(do[3]-do.mean()),DO_noon_minus_min=do[3]-do.min(),DO_peak_hour=HOURS[int(do.argmax())],DO_low_hour=HOURS[int(do.argmin())],Temp_range=np.ptp(temp),Temp_noon=float(temp[3]),DO_gt20=bool((do>20).any()),DO_range_gt5=bool(np.ptp(do)>5))
    out.append(rec)
d=pd.DataFrame(out).sort_values(['site','station','date']).reset_index(drop=True)
d.to_csv(OUT/'smoke_station_day_diagnostics.csv',index=False,encoding='utf-8-sig')
v=d[d.strict_sixpoint].copy()
assert len(v)==13
print('Strict profiles',len(v),'/',len(d),'DO stats:',v[['DO_range','DO_abs_noon_minus_mean6','DO_noon_minus_mean6','DO_noon_minus_min']].median().round(4).to_dict())
print('\nALL STATION DAYS:')
print(d[['site','station','date','http_ok','extra_valid','strict_sixpoint','DO_min','DO_noon','DO_max','DO_range','DO_noon_minus_mean6','DO_low_hour','DO_peak_hour']].to_string(index=False))
print('\nDO statistics by month:')
print(v.groupby('month').agg(n=('DO_range','size'),median_range=('DO_range','median'),median_abs_noon_mean=('DO_abs_noon_minus_mean6','median'),median_noon_above_min=('DO_noon_minus_min','median')).round(4).to_string())
print('Extremes > DO 20',v.DO_gt20.sum(),'range >5',v.DO_range_gt5.sum())
print(v[v.DO_gt20|v.DO_range_gt5][['site','station','date','DO_min','DO_noon','DO_max','DO_range','DO_low_hour','DO_peak_hour']].to_string(index=False))

pairs=[]
for sid,g in d.groupby('station_identity_id'):
    jan=g[g.month==1];jul=g[g.month==7]
    assert len(jan)==1 and len(jul)==1
    rec=dict(site=g.site.iloc[0],station=g.station.iloc[0],year=int(g.year.iloc[0]),jan_valid=bool(jan.strict_sixpoint.iloc[0]),jul_valid=bool(jul.strict_sixpoint.iloc[0]),pair_valid=bool(jan.strict_sixpoint.iloc[0] and jul.strict_sixpoint.iloc[0]))
    if rec['pair_valid']:
        rec['jan_range']=float(jan.DO_range.iloc[0]);rec['jul_range']=float(jul.DO_range.iloc[0]);rec['jul_minus_jan_range']=rec['jul_range']-rec['jan_range']
    pairs.append(rec)
pairs=pd.DataFrame(pairs).sort_values(['site','station'])
pairs.to_csv(OUT/'smoke_pair_comparison.csv',index=False,encoding='utf-8-sig')
print('smoke strict Jan-Jul pairs',pairs.pair_valid.sum(),'/',len(pairs))
print(pairs.to_string(index=False))

# Live updated noon strict valid for follow-up 400 requests
followp=p[p.phase=='follow'].copy();followp['noon_datetime']=followp.target_datetime.str.slice(0,11)+'12:00:00'
followp['noon_strict']=followp.apply(lambda r: (r.station_identity_id,r.noon_datetime) in idx and bool(idx[(r.station_identity_id,r.noon_datetime)].strict),axis=1)
assert followp.noon_strict.all()
assert len(followp.groupby(['station_identity_id',followp.target_datetime.str[:10]]))==80
followp[['request_id','study_site_code','cnemc_station_name','target_datetime','noon_strict']].sort_values(['study_site_code','cnemc_station_name','target_datetime']).to_csv(OUT/'followup_400_live_precheck.csv',index=False,encoding='utf-8-sig')
print('FOLLOWUP all 400 pending; 80 station-days strict noon; by-site',followp.groupby('study_site_code').size().to_dict())
print('FOLLOWUP station-year pairs',len(followp.groupby(['station_identity_id',followp.target_datetime.str[:4]])), 'unique stations',followp.station_identity_id.nunique())

summ={
 'source_files':{'manifest':str(MF),'observations':str(OB)},
 'manifest_status_counts':m.status.value_counts().to_dict(),
 'observations_count':len(o),
 'smoke_requests':len(smoke),'smoke_ok':int(extra.status.eq('ok').sum()),'smoke_no_record':int(extra.status.eq('no_record').sum()),
 'smoke_strict_valid_extra':int(extra.valid_ok.sum()),'smoke_qc_reason_counts':qc_issues.qc_reason.value_counts().to_dict(),
 'smoke_strict_days':len(v),'smoke_days_total':len(d),'smoke_valid_pairs':int(pairs.pair_valid.sum()),'smoke_pairs_total':len(pairs),
 'median_abs_noon_mean_DO':float(v.DO_abs_noon_minus_mean6.median()),'median_signed_noon_mean_DO':float(v.DO_noon_minus_mean6.median()),
 'median_noon_minus_observed_min_DO':float(v.DO_noon_minus_min.median()),
 'smoke_DO_extreme_days_gt20':int(v.DO_gt20.sum()),'smoke_DO_range_days_gt5':int(v.DO_range_gt5.sum()),
 'followup_pending_all':bool(set(follow)<=set(m[m.status=='pending'].request_id)),
 'followup_strict_noon_all':bool(followp.noon_strict.all()),
 'remaining_executor_attempts_before_followup':22506-220 if False else 22246,
 'attempts_after_400_single_call_success':21846
}
(OUT/'SMOKE_AUDIT_SUMMARY.json').write_text(json.dumps(summ,ensure_ascii=False,indent=2),encoding='utf-8')

#!/usr/bin/env python3
"""Offline, reproducible audit and Stage 4 Phase C request-ID selection.

Reads existing approved plan, exported observations, manifest, and pilot-hour table.
NEVER invokes the Huawei API and NEVER writes to SQLite or edits input files.
Requires Python >=3.9; only standard-library dependencies.

IMPORTANT: The manifest in the shared input snapshot predates Phase B;
pilot_hour_results.csv and observation IDs are also used to exclude resolved IDs.
Run only for a fresh planning exercise. Preserve the initially issued allowlists
unchanged once acquisition starts.
"""
import argparse, csv, gzip, hashlib, json, math, statistics
from collections import Counter, defaultdict
from pathlib import Path

EXPECTED_SHA = 'a8e0c166f199971f63bdb209d4e5c13bbf5dae21eec9ebd45447ad6c453c6587'
HOURS = (0,4,8,12,16,20)
EXTRA_HOURS = (0,4,8,16,20)
CORE = ('water_temp','ph','dissolvedoxygen','codmn','nh3-n','tp','tn')
QUOTAS = {'XMB':15,'MJE':15,'BHB':10,'LZB':10}
SMOKE = {'XMB':3,'MJE':3,'BHB':2,'LZB':2}
CAPS = {'XMB':3,'MJE':1,'BHB':1,'LZB':2}

def read_csv(path):
    with (gzip.open(path, 'rt', encoding='utf-8-sig', newline='') if str(path).lower().endswith('.gz')
          else open(path, 'r', encoding='utf-8-sig', newline='')) as h:
        return list(csv.DictReader(h))

def write_csv(path, rows, fields):
    with open(path,'w',encoding='utf-8-sig',newline='') as h:
        w=csv.DictWriter(h,fieldnames=fields,extrasaction='ignore');w.writeheader();w.writerows(rows)

def digest(path):
    hh=hashlib.sha256()
    with open(path,'rb') as f:
        for part in iter(lambda:f.read(1024*1024),b''):hh.update(part)
    return hh.hexdigest()

def val(obj):
    if obj is None or str(obj).strip() in ('','-','NA','nan','None','NULL','-1','-2','-3'):
        return None
    try:
        x=float(obj)
        return x if math.isfinite(x) else None
    except (ValueError,TypeError):return None

def is_core_normal(row):
    if row.get('section_status')!='正常' or row.get('monitor_time')!=row.get('target_datetime'):
        return False
    nums=[val(row.get(c)) for c in CORE]
    return all(x is not None and x>=0 for x in nums) and nums[1]>0

def median_or_none(xs):
    return round(statistics.median(xs),6) if xs else None

def mean_or_none(xs):
    return round(statistics.mean(xs),6) if xs else None

def geoscreen(row):
    site=row['study_site_code']; province=row.get('qingyue_province','')
    lon=val(row.get('longitude')); lat=val(row.get('latitude'))
    if lon is None or lat is None:return False
    if site in ('XMB','MJE'):
        return province=='福建省'
    if site=='BHB':
        return province=='天津市' and 116.8<=lon<=118.1 and 38.3<=lat<=39.6
    if site=='LZB':
        return province=='山东省' and 118.0<=lon<=120.5 and 36.0<=lat<=38.5
    return False

def main():
    ap=argparse.ArgumentParser(description=__doc__)
    for a in ('plan','observations','manifest','pilot_hours','output'):ap.add_argument('--'+a.replace('_','-'),required=True)
    ap.add_argument('--force',action='store_true',help='Allow overwriting output files; never use after acquiring any Phase C IDs.')
    args=ap.parse_args()
    paths={k:Path(getattr(args,k)) for k in ('plan','observations','manifest','pilot_hours')}
    out=Path(args.output)
    if out.exists() and any(out.iterdir()) and not args.force:
        raise SystemExit('Output folder is not empty. Use a NEW folder to preserve issued request lists, or --force before any calls.')
    out.mkdir(parents=True,exist_ok=True)
    sha=digest(paths['plan'])
    if sha!=EXPECTED_SHA:raise SystemExit('STOP: original approved plan SHA256 differs from expected: '+sha)
    plan=read_csv(paths['plan']); obs=read_csv(paths['observations'])
    manifest=read_csv(paths['manifest']); pilot=read_csv(paths['pilot_hours'])
    assert len(plan)==39626, f'Unexpected plan row count: {len(plan)}'
    assert len(set(p['request_id'] for p in plan))==len(plan)
    assert len(set(r['request_id'] for r in obs))==len(obs)
    assert len(set(r['request_id'] for r in pilot))==len(pilot)
    assert len(pilot)==160
    byid={r['request_id']:r for r in obs}
    pbyid={r['request_id']:r for r in plan}
    for r in obs:
        if r['request_id'] not in pbyid:raise ValueError('Observation ID missing from approved plan: '+r['request_id'])

    # Confirm pilot request-level reconciliation.
    flags=[]; day_groups=defaultdict(list)
    for r in pilot:
        rid=r['request_id']
        pl=pbyid[rid]
        assert pl['plan_component']=='semiannual_full_day_extra'
        assert pl['target_datetime']==r['target_datetime']
        row=byid.get(rid)
        status=r['status']
        if status=='ok' and row is None:raise ValueError('Pilot OK response absent from observations: '+rid)
        if status!='ok' and row is not None:raise ValueError('Pilot non-OK has observation: '+rid)
        good=is_core_normal(row) if row else False
        report_good=str(r['qc_core_normal']).strip().lower() in ('true','1')
        if good!=report_good:raise ValueError('Quality classification disagreement for '+rid)
        if status=='ok' and not good:
            why='maintenance' if row['section_status']=='维护' else (
                'all_seven_core_missing' if all(val(row.get(c)) is None for c in CORE) else 'other_incomplete_core')
            flags.append({'site':r['study_site_code'],'station':r['station_name'],'target_datetime':r['target_datetime'],
                          'request_id':rid,'reason':why,'section_status':row.get('section_status')})
        day_groups[(r['station_identity_id'],r['target_datetime'][:10])].append(r)
    assert len(day_groups)==32
    days=[]
    for (sid,date),hour_rows in sorted(day_groups.items(),key=lambda v:(v[0][1],v[0][0])):
        one=hour_rows[0]
        all_rows=[]
        for hour in HOURS:
            key=f'{date} {hour:02}:00:00'
            matching=[r for r in obs if r['station_identity_id']==sid and r['target_datetime']==key]
            if len(matching)>1:raise ValueError(f'Duplicate station/time observation: {sid} {key}')
            all_rows.extend(matching)
        strict=len(all_rows)==6 and len({int(x['target_hour']) for x in all_rows})==6 and all(is_core_normal(x) for x in all_rows)
        result={'phase':one['phase'],'study_site_code':one['study_site_code'],'station_identity_id':sid,
                'station_name':one['station_name'],'target_date':date,'year':int(date[:4]),'month':int(date[5:7]),
                'supplementary_ok':sum(r['status']=='ok' for r in hour_rows),
                'supplementary_no_record':sum(r['status']=='no_record' for r in hour_rows),
                'supplementary_strict_valid':sum(str(r['qc_core_normal']).lower()=='true' for r in hour_rows),
                'strict_sixpoint':strict}
        if strict:
            do={int(r['target_hour']):val(r['dissolvedoxygen']) for r in all_rows}
            temps={int(r['target_hour']):val(r['water_temp']) for r in all_rows}
            assert None not in do.values()
            v=list(do.values());mu=statistics.mean(v)
            result.update({'noon_DO':do[12], 'six_point_DO_mean':mu,'six_point_DO_min':min(v),
                           'six_point_DO_max':max(v),'six_point_DO_range':max(v)-min(v),
                           'noon_minus_six_mean_DO':do[12]-mu,'abs_noon_minus_six_mean_DO':abs(do[12]-mu),
                           'noon_minus_observed_min_DO':do[12]-min(v),
                           'observed_min_hour':min(do,key=lambda k:do[k]),
                           'observed_max_hour':max(do,key=lambda k:do[k]),
                           'water_temperature_range':max(temps.values())-min(temps.values())})
        days.append(result)
    assert sum(x['strict_sixpoint'] for x in days)==25
    valid=[d for d in days if d['strict_sixpoint']]
    pairs=defaultdict(dict)
    for x in valid:pairs[(x['station_identity_id'],x['year'])][x['month']]=x
    same_pairs=[v for v in pairs.values() if 1 in v and 7 in v]
    assert len(same_pairs)==9
    jul_larger=sum(v[7]['six_point_DO_range']>v[1]['six_point_DO_range'] for v in same_pairs)
    assert jul_larger==8
    jan=[v for v in valid if v['month']==1];jul=[v for v in valid if v['month']==7]
    summary={
        'pilot_extra_requests':len(pilot),
        'pilot_extra_ok':sum(r['status']=='ok' for r in pilot),
        'pilot_extra_no_record':sum(r['status']=='no_record' for r in pilot),
        'pilot_extra_strict_quality':sum(str(r['qc_core_normal']).lower()=='true' for r in pilot),
        'strict_daily_profiles':len(valid), 'total_daily_profiles':len(days),
        'valid_january_profiles':len(jan),'valid_july_profiles':len(jul),
        'strict_paired_station_years':len(same_pairs),'paired_july_DO_range_larger':jul_larger,
        'median_abs_noon_vs_sixpoint_mean_DO':median_or_none([x['abs_noon_minus_six_mean_DO'] for x in valid]),
        'median_noon_minus_observed_min_DO':median_or_none([x['noon_minus_observed_min_DO'] for x in valid]),
        'days_noon_not_min':sum(x['noon_minus_observed_min_DO']>0 for x in valid),
        'days_noon_above_min_by_at_least_1':sum(x['noon_minus_observed_min_DO']>=1 for x in valid),
        'january_median_DO_range':median_or_none([x['six_point_DO_range'] for x in jan]),
        'july_median_DO_range':median_or_none([x['six_point_DO_range'] for x in jul]),
        'january_median_abs_noon_vs_mean':median_or_none([x['abs_noon_minus_six_mean_DO'] for x in jan]),
        'july_median_abs_noon_vs_mean':median_or_none([x['abs_noon_minus_six_mean_DO'] for x in jul]),
        'january_median_noon_minus_min':median_or_none([x['noon_minus_observed_min_DO'] for x in jan]),
        'july_median_noon_minus_min':median_or_none([x['noon_minus_observed_min_DO'] for x in jul]),
        'hourly_failed_qc_reasons':dict(Counter(f['reason'] for f in flags))
    }
    write_csv(out/'pilot_noon_vs_sixpoint.csv',days, list(days[0].keys()) if not valid else list(dict.fromkeys(k for row in days for k in row)))
    write_csv(out/'pilot_quality_flags.csv',flags,['site','station','target_datetime','request_id','reason','section_status'])
    with open(out/'pilot_quantitative_summary.json','w',encoding='utf-8') as f:json.dump(summary,f,indent=2,ensure_ascii=False)

    # Do not infer current ledger state from a possibly stale export alone.
    excluded=set(r['request_id'] for r in manifest if r['status']!='pending')
    excluded.update(r['request_id'] for r in pilot)
    excluded.update(r['request_id'] for r in obs)
    paired_noons=defaultdict(set)
    for r in obs:
        if r['target_hour']=='12' and r['target_date'][5:7] in ('01','07') and 2021<=int(r['target_date'][:4])<=2024 and is_core_normal(r):
            paired_noons[(r['station_identity_id'],int(r['target_date'][:4]))].add(r['target_date'][5:7])
    qualified={k for k,v in paired_noons.items() if v=={'01','07'}}
    rawpairs=defaultdict(list);station_meta={}
    for r in plan:
        if r['plan_component']!='semiannual_full_day_extra' or r['request_id'] in excluded:continue
        year=int(r['target_datetime'][:4]);month=r['target_datetime'][5:7];hour=int(r['target_hour'])
        if not (2021<=year<=2024 and month in ('01','07') and hour in EXTRA_HOURS):continue
        key=(r['station_identity_id'],year)
        if key not in qualified or not geoscreen(r):continue
        rawpairs[key].append(r)
        station_meta[r['station_identity_id']]=r
    pools=defaultdict(list)
    for key,rows in rawpairs.items():
        byhour={(r['target_datetime'][5:7],int(r['target_hour'])):r for r in rows}
        if len(rows)!=10 or len(byhour)!=10 or set(byhour)!={(mo,hr) for mo in ('01','07') for hr in EXTRA_HOURS}:continue
        sid,year=key;site=station_meta[sid]['study_site_code']
        if site in QUOTAS:pools[site].append({'station_identity_id':sid,'year':year,'site':site,'rows':rows,'meta':station_meta[sid]})
    selections=[];available_counts={}
    for site,limit in QUOTAS.items():
        pool=pools[site];available_counts[site]=len(pool)
        station_counts=Counter();year_counts=Counter();selected=[]
        while len(selected)<limit:
            remaining=[v for v in pool if (v['station_identity_id'],v['year']) not in {(x['station_identity_id'],x['year']) for x in selected}
                       and station_counts[v['station_identity_id']]<CAPS[site]]
            if not remaining:raise RuntimeError(f'Insufficient eligible {site}: {len(selected)} of {limit}')
            # Prefer unseen stations, underrepresented years, then immutable ID for reproducible ties.
            v=min(remaining,key=lambda v:(station_counts[v['station_identity_id']],year_counts[v['year']],v['station_identity_id'],v['year']))
            selected.append(v);station_counts[v['station_identity_id']]+=1;year_counts[v['year']]+=1
        selections.extend(selected)
    assert len(selections)==50
    smoke=[]
    for site,n in SMOKE.items():
        relevant=[p for p in selections if p['site']==site]
        sc=Counter();yc=Counter()
        for i in range(n):
            candidate=min([p for p in relevant if p not in smoke],key=lambda p:(sc[p['station_identity_id']],yc[p['year']],p['station_identity_id'],p['year']))
            smoke.append(candidate);sc[candidate['station_identity_id']]+=1;yc[candidate['year']]+=1
    assert len(smoke)==10
    smoke_keys={(r['station_identity_id'],r['year']) for r in smoke}
    rosters=[];all_ids=[];smoke_ids=[];later_ids=[]
    for rec in sorted(selections,key=lambda r:(r['site'],r['year'],r['station_identity_id'])):
        si=rec['station_identity_id'];yr=rec['year'];site=rec['site'];md=rec['meta'];is_smoke=(si,yr) in smoke_keys
        rowset=sorted(rec['rows'],key=lambda r:r['plan_order'] and int(r['plan_order']))
        ids=[v['request_id'] for v in rowset]
        jan=next(r for r in obs if r['station_identity_id']==si and r['target_datetime']==f'{yr}-01-15 12:00:00')
        jul=next(r for r in obs if r['station_identity_id']==si and r['target_datetime']==f'{yr}-07-15 12:00:00')
        rosters.append({'phase':'smoke_100' if is_smoke else 'followup_400','study_site_code':site,
                        'station_identity_id':si,'station_name':md['cnemc_station_name'],'year':yr,
                        'province':md['qingyue_province'],'longitude':md['longitude'],'latitude':md['latitude'],
                        'jan_noon_DO':jan['dissolvedoxygen'],'jul_noon_DO':jul['dissolvedoxygen'],
                        'jan_noon_request_id':jan['request_id'],'jul_noon_request_id':jul['request_id'],
                        'supplementary_request_count':len(ids),
                        'spatial_screen':'provisional_location_only_catchment_unverified'})
        all_ids.extend(ids)
        if is_smoke:smoke_ids.extend(ids)
        else:later_ids.extend(ids)
    assert len(all_ids)==500 and len(set(all_ids))==500
    assert len(smoke_ids)==100 and len(set(smoke_ids))==100
    assert len(later_ids)==400 and len(set(later_ids))==400
    assert set(all_ids)==set(smoke_ids)|set(later_ids)
    assert set(all_ids).isdisjoint(excluded)
    assert Counter(pbyid[x]['plan_component'] for x in all_ids)=={'semiannual_full_day_extra':500}
    assert all(int(pbyid[x]['target_datetime'][:4])<=2024 for x in all_ids)
    assert not any(pbyid[x]['target_datetime'].startswith('2026-01-') for x in all_ids)
    # All lines are unmodified approved request_id values.
    for name,ids in [('phaseC_approved_all_500.txt',all_ids),('phaseC_smoke_100.txt',smoke_ids),('phaseC_followup_400.txt',later_ids)]:
        (out/name).write_text('\n'.join(ids)+'\n',encoding='utf-8')
    write_csv(out/'phaseC_selected_station_years.csv',rosters,list(rosters[0]))
    audit={
      'approved_original_plan_sha256':sha,
      'input_sha256':{k:digest(v) for k,v in paths.items()},
      'plan_rows':len(plan), 'observations_rows':len(obs),
      'manifest_status_counts':dict(Counter(r['status'] for r in manifest)),
      'manifest_note':'Manifest may predate Phase A/B; excludes all 160 pilot-hour request IDs and all observation IDs from selection',
      'pool_pairs_after_all_filters_by_site':available_counts,
      'selected_station_year_pairs_by_site':dict(Counter(r['site'] for r in selections)),
      'selected_unique_stations_by_site':{site:len(set(r['station_identity_id'] for r in selections if r['site']==site)) for site in QUOTAS},
      'selected_years_by_site':{site:dict(sorted(Counter(r['year'] for r in selections if r['site']==site).items())) for site in QUOTAS},
      'smoke_station_year_pairs_by_site':dict(Counter(r['site'] for r in smoke)),
      'all_request_ids':500, 'smoke_request_ids':100, 'followup_request_ids':400,
      'zero_new_timestamps':True,'zero_api_calls':True,
      'location_screening_not_watershed_validation':True,
      'excluded_year':2025
    }
    (out/'request_selection_checks.json').write_text(json.dumps(audit,indent=2,ensure_ascii=False),encoding='utf-8')
    print('OFFLINE AUDIT PASS')
    print('Pilot:',summary)
    print('Original plan SHA256:',sha)
    print('Eligible candidate pools:',available_counts)
    print('Selected 50 station-years, 500 original request IDs, 100 smoke + 400 follow-up')
    print('Selected study-site stations:',audit['selected_unique_stations_by_site'])
    print('Selected years:',audit['selected_years_by_site'])
    print('No API calls; no changes to SQLite or the original plan.')

if __name__=='__main__':main()

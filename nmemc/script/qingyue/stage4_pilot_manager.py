#!/usr/bin/env python3
"""Read-only Qingyue Stage 4 pilot builder and post-run evaluator (stdlib only).

Never uses the network, never opens SQLite, and never edits source input files.
The existing approved plan's SHA256 and original request IDs are preserved.
"""
import argparse
import collections
import csv
import datetime
import gzip
import hashlib
import json
import math
from pathlib import Path

PLAN_SHA256 = 'a8e0c166f199971f63bdb209d4e5c13bbf5dae21eec9ebd45447ad6c453c6587'
EXPECTED_PLAN_ROWS = 39626
COMP_EXTRA = 'semiannual_full_day_extra'
COMP_NOON = 'monthly_noon_backbone'
EXTRA_HOURS = {0, 4, 8, 16, 20}
CORE = ('water_temp', 'ph', 'dissolvedoxygen', 'codmn', 'nh3-n', 'tp', 'tn')
NORMAL = '\u6b63\u5e38'
# site, year, station, phase. This roster is fixed in advance of seeing extra-hour values.
# A = proof-of-feasibility; B = expansion, only after reviewing phase A.
PILOT_PAIRS = (
    ('XMB', '2022', '\u9698\u5934\u6f6d', 'A'),
    ('MJE', '2024', '\u95fd\u5b89', 'A'),
    ('BHB', '2023', '\u6d77\u6cb3\u5927\u95f8', 'A'),
    ('LZB', '2022', '\u7f8a\u53e3', 'A'),
    ('XMB', '2023', '\u6d66\u5357\u6c34\u6587\u7ad9', 'B'),
    ('XMB', '2025', '\u534e\u5b89\u897f\u9642', 'B'),
    ('MJE', '2022', '\u95fd\u6e05\u96c4\u6c5f', 'B'),
    ('BHB', '2025', '\u5317\u6d0b\u6865', 'B'),
    ('LZB', '2021', '\u5f20\u5efa\u6865', 'B'),
    ('LZB', '2024', '\u897f\u95f8', 'B'),
    ('BBG', '2024', '\u4e09\u6ee9', 'B'),
    ('QZB', '2021', '\u5357\u5b89\u971e\u4e1c\u6865', 'B'),
    ('DSB', '2023', '\u4e91\u9704\u9ad8\u5858\u6e21\u53e3', 'B'),
    ('DYB', '2021', '\u864e\u722a\u65ad\u6865', 'B'),
    ('JZB', '2025', '\u58a8\u6c34\u6cb3\u5165\u6d77\u53e3', 'B'),
    ('SMB', '2022', '\u6c34\u8f66', 'B'),
)

def load_csv(path):
    p = Path(path)
    fn = gzip.open if p.suffix.lower() == '.gz' else open
    with fn(p, 'rt', encoding='utf-8-sig', newline='') as f:
        return list(csv.DictReader(f))

def sha256(path):
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b''):
            h.update(chunk)
    return h.hexdigest()

def write_csv(path, rows, fields):
    with open(path, 'w', encoding='utf-8-sig', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields, extrasaction='ignore')
        w.writeheader()
        w.writerows(rows)

def write_ids(path, ids, description):
    with open(path, 'w', encoding='utf-8') as f:
        f.write('# Qingyue Stage 4 approved-plan request IDs - ' + description + '\n')
        for request_id in ids:
            f.write(request_id + '\n')

def read_ids(path):
    p = Path(path)
    if p.suffix.lower() == '.csv':
        rows = load_csv(path)
        return {r['request_id'] for r in rows}
    return {line.strip() for line in p.read_text(encoding='utf-8-sig').splitlines()
            if line.strip() and not line.lstrip().startswith('#')}

def valid_core(o):
    if not o or o.get('section_status') != NORMAL:
        return False
    try:
        values = [float(o[k]) for k in CORE]
    except (ValueError, TypeError, KeyError):
        return False
    return all(math.isfinite(x) and 0 <= x < 1e6 for x in values)

def valid_obs_at_time(o, target):
    return valid_core(o) and o.get('monitor_time') == target

def validate_plan(plan):
    if len(plan) != EXPECTED_PLAN_ROWS:
        raise ValueError('Unexpected approved plan length: ' + str(len(plan)))
    ids = [r['request_id'] for r in plan]
    if len(ids) != len(set(ids)):
        raise ValueError('Duplicate approved plan request ID')

def validate_manifest(plan, manifest):
    if len(manifest) != len(plan):
        raise ValueError('Manifest and approved plan have different row counts')
    p = {r['request_id']: r for r in plan}
    m = {r['request_id']: r for r in manifest}
    if len(m) != len(manifest) or set(p) != set(m):
        raise ValueError('Manifest IDs differ from approved plan')
    for rid, r in m.items():
        original = p[rid]
        for k in ('target_datetime', 'station_identity_id', 'plan_component', 'study_site_code'):
            if r[k] != original[k]:
                raise ValueError('Manifest mismatch for request ID ' + rid + ' field ' + k)
    return m

def readiness(plan, m):
    noons = {(r['station_identity_id'], r['target_datetime']): m[r['request_id']]['status']
             for r in plan if r['plan_component'] == COMP_NOON}
    total = collections.Counter()
    areas = collections.defaultdict(collections.Counter)
    dates = collections.defaultdict(collections.Counter)
    zero_cost_ids = []
    for r in plan:
        if r['plan_component'] != COMP_EXTRA or not ('2021' <= r['target_datetime'][:4] <= '2026'):
            continue
        t = r['target_datetime']
        key = (r['station_identity_id'], t[:11] + '12:00:00')
        noon_status = noons.get(key)
        if noon_status is None:
            raise ValueError('Missing noon dependency for ' + r['request_id'])
        st = m[r['request_id']]['status']
        category = ('extra_' + st) if st != 'pending' else ('pending_noon_' + noon_status)
        total[category] += 1
        areas[r['study_site_code']][category] += 1
        dates[t[:10]][category] += 1
        if category == 'pending_noon_no_record':
            zero_cost_ids.append(r['request_id'])
    return total, areas, dates, zero_cost_ids

def build(args):
    plan_hash = sha256(args.plan)
    if plan_hash != PLAN_SHA256:
        raise ValueError('Approved plan SHA256 mismatch; refusing to create a runnable request list')
    plan = load_csv(args.plan)
    validate_plan(plan)
    manifest = load_csv(args.manifest)
    m = validate_manifest(plan, manifest)
    obs = {r['request_id']: r for r in load_csv(args.observations)}
    if len(obs) != len(load_csv(args.observations)):
        raise ValueError('Duplicate observations request IDs')
    cross = {r['station_identity_id']:r for r in load_csv(args.crosswalk)}
    geo = {}
    if args.geography_review:
        geo = {r['station_identity_id']:r for r in load_csv(args.geography_review)}
    by_full = collections.defaultdict(list)
    by_noon = {}
    for r in plan:
        t = r['target_datetime']
        if r['plan_component'] == COMP_EXTRA and t[:4] in ('2021','2022','2023','2024','2025'):
            by_full[(r['study_site_code'], t[:4], r['qingyue_station_name'])].append(r)
        if r['plan_component'] == COMP_NOON:
            by_noon[(r['station_identity_id'], t)] = r
    pairs = []
    days = []
    chosen = []
    for site, year, station, phase in PILOT_PAIRS:
        group = by_full.get((site, year, station), [])
        if len(group) != 10:
            raise ValueError(f'Expected exactly 10 Stage 4 rows for {site}/{year}/{station}, got {len(group)}')
        sids = {r['station_identity_id'] for r in group}
        if len(sids) != 1:
            raise ValueError('Station identity ambiguous for ' + site + ' ' + station)
        sid = next(iter(sids))
        if sid not in cross:
            raise ValueError('Missing crosswalk identity: ' + sid)
        flagged = geo.get(sid, {})
        for month in ('01', '07'):
            yearmonth = year + '-' + month + '-15'
            extra = [r for r in group if r['target_datetime'].startswith(yearmonth + ' ')]
            if len(extra) != 5 or {int(r['target_hour']) for r in extra} != EXTRA_HOURS:
                raise ValueError(f'Incomplete five-hour plan for {site} {yearmonth} {station}')
            if any(m[r['request_id']]['status'] != 'pending' for r in extra):
                raise ValueError(f'Chosen extra hour was not pending in original snapshot: {station} {yearmonth}')
            noon_plan = by_noon[(sid, yearmonth + ' 12:00:00')]
            nr = m[noon_plan['request_id']]
            noon_obs = obs.get(noon_plan['request_id'])
            if nr['status'] != 'ok' or not valid_obs_at_time(noon_obs, yearmonth + ' 12:00:00'):
                raise ValueError(f'Noon measurement not eligible for {station} {yearmonth}')
            if any(r['longitude'] != noon_plan['longitude'] or r['latitude'] != noon_plan['latitude'] for r in extra):
                raise ValueError('Coordinate mismatch between noon and extra rows')
            for r in extra:
                chosen.append((r, phase))
            days.append(dict(phase=phase, site=site, station_identity_id=sid, station_name=station,
                year=year, month=month, target_date=yearmonth, noon_request_id=noon_plan['request_id'],
                noon_grade=noon_obs.get('qulity',''), longitude=noon_plan['longitude'],
                latitude=noon_plan['latitude'], province=noon_plan['qingyue_province'],
                city=noon_plan['qingyue_city'], noon_status=nr['status'], extra_requests=5,
                geography_review_flag=flagged.get('geography_review_flag',''),
                geography_review_reason=flagged.get('geography_review_reason',''),
                hydrology_status='PROVISIONAL_UNVERIFIED'))
        p = [x for x in days if x['site']==site and x['station_identity_id']==sid and x['year']==year]
        if len(p) != 2:
            raise ValueError('Pair design duplicate or missing')
        pairs.append(dict(phase=phase, site=site, year=year, station_name=station,
            station_identity_id=sid, province=p[0]['province'], city=p[0]['city'],
            longitude=p[0]['longitude'], latitude=p[0]['latitude'], jan_noon_grade=p[0]['noon_grade'],
            jul_noon_grade=p[1]['noon_grade'], extra_calls=10, hydrology_status='PROVISIONAL_UNVERIFIED',
            geography_review_flag=p[0]['geography_review_flag'], geography_review_reason=p[0]['geography_review_reason']))
    if len(pairs) != 16 or len(days) != 32 or len(chosen)!=160:
        raise ValueError('Pilot size differs from preregistered 16 pairs / 32 days / 160 calls')
    if len({r['request_id'] for r,_ in chosen}) != 160:
        raise ValueError('Pilot request-ID duplication')
    selected = sorted(chosen, key=lambda kv:int(kv[0]['plan_order']))
    selected_a = [r['request_id'] for r,ph in selected if ph=='A']
    selected_b = [r['request_id'] for r,ph in selected if ph=='B']
    if (len(selected_a),len(selected_b)) != (40,120):
        raise ValueError('Phase sizes do not match 40 and 120')
    out = Path(args.output)
    out.mkdir(parents=True, exist_ok=True)
    write_ids(out/'stage4_pilot_phaseA_40.txt',selected_a,'Phase A: 40 requests; four station-year Jan/Jul pairs')
    write_ids(out/'stage4_pilot_phaseB_120.txt',selected_b,'Phase B: 120 requests; run only after Phase A review')
    write_ids(out/'stage4_pilot_all_160.txt',[r['request_id'] for r,ph in selected], 'Combined phases, reference only')
    write_csv(out/'pilot_station_year_pairs.csv',pairs,list(pairs[0]))
    write_csv(out/'pilot_station_days.csv',days,list(days[0]))
    total,areas,dates,zero_cost_ids = readiness(plan,m)
    write_csv(out/'stage4_readiness_by_site.csv',[
        {'study_site_code':site,'pending_ready_noon_ok':ct['pending_noon_ok'],
         'pending_skip_noon_no_record':ct['pending_noon_no_record'],
         'pending_blocked_noon_pending':ct['pending_noon_pending'],
         'previous_ok':ct['extra_ok'], 'previous_no_record':ct['extra_no_record'],
         'stage4_total':sum(ct.values())}
        for site,ct in sorted(areas.items())],
        ('study_site_code','pending_ready_noon_ok','pending_skip_noon_no_record',
         'pending_blocked_noon_pending','previous_ok','previous_no_record','stage4_total'))
    write_csv(out/'stage4_readiness_by_date.csv',[
        {'sampling_date':day,'pending_ready_noon_ok':ct['pending_noon_ok'],
         'pending_skip_noon_no_record':ct['pending_noon_no_record'],
         'pending_blocked_noon_pending':ct['pending_noon_pending'],
         'previous_ok':ct['extra_ok'], 'previous_no_record':ct['extra_no_record'],
         'stage4_total':sum(ct.values())}
        for day,ct in sorted(dates.items())],
        ('sampling_date','pending_ready_noon_ok','pending_skip_noon_no_record',
         'pending_blocked_noon_pending','previous_ok','previous_no_record','stage4_total'))
    write_ids(out/'stage4_optional_zero_cost_skips_2990.txt',zero_cost_ids,
              'OPTIONAL later only; these had no_record at noon in the supplied snapshot')
    expected={'pending_noon_ok':12680,'pending_noon_no_record':2990,
              'pending_noon_pending':1315,'extra_ok':85,'extra_no_record':15}
    if any(total[k] != v for k,v in expected.items()) or sum(total.values())!=17085:
        raise ValueError('Snapshot reconciliation mismatch. Rebuild after reviewing new ledger state. '+str(total))
    years=collections.Counter(x['year'] for x in pairs)
    area_counts=collections.Counter(x['site'] for x in pairs)
    meta=dict(plan_sha256=plan_hash,source_manifest_sha256=sha256(args.manifest),
              source_observations_sha256=sha256(args.observations),
              generated_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
              stage4_counts=dict(total),pilot_pairs=16,pilot_station_days=32,
              phaseA_new_calls=40,phaseB_new_calls=120,pilot_by_year=dict(years),
              pilot_by_study_site=dict(area_counts))
    (out/'stage4_snapshot_metadata.json').write_text(json.dumps(meta,indent=2,ensure_ascii=False)+'\n',encoding='utf-8')
    s = [
        '# Qingyue Stage 4: readiness and controlled 160-request pilot',
        '',
        f'Approved plan SHA256: `{plan_hash}`. This is a read-only snapshot; zero API calls.',
        '',
        '## Status after completion of the approved February-July 2026 monthly backbone',
        '',
        f'- Stage 4 total: **{sum(total.values()):,}** (including {total["extra_ok"]} previously successful and {total["extra_no_record"]} previously unsuccessful requests).',
        f'- Pending with noon OK: **{total["pending_noon_ok"]:,}**, potentially actionable.',
        f'- Pending with noon `no_record`: **{total["pending_noon_no_record"]:,}**, conditionally skippable at zero API cost.',
        f'- Pending with noon still pending: **{total["pending_noon_pending"]:,}**, all attributable to 15 January 2026.',
        '',
        'The pilot uses **16 within-station, within-year January/July pairs** across all ten study areas:',
        '**32 station-days x 5 extra hours = 160 approved request IDs.** Both noon',
        'records already exist and are marked normal, with the seven core measurements',
        'present and nonnegative. All selected extra requests were pending when built.',
        '',
        '| Study area | Paired station-years | New calls |',
        '|---|---:|---:|',
    ]
    for site,n in sorted(area_counts.items()):
        s.append(f'| {site} | {n} | {10*n} |')
    s += ['', '## Geographic and methodological caveats', '',
        '- Site labels in the original plan are **provisional study-area groupings**;',
        '  geographical proximity and exact station identity do not prove watershed connectivity.',
        '- All selected LZB representatives were restricted to Shandong provincial coordinates',
        '  but remain flagged for independent drainage verification because of basin-code concerns.',
        '- Other selected station coordinates and watershed paths also require GIS validation.',
        '- Success metrics describe point-in-time archival retrieval, not monthly means.',
        '- Hourly data are on selected January/July days; never treat 6 observations as',
        '  independent station-days. Compare hours within station-day and seasons within station-year.',
        '- Raw measurement units and apparent reporting floors still require source confirmation.',
        '- The pilot is a feasibility/design test, not a random sample for ten-area population inference.',
        '', '## Recommended execution', '',
        '1. Back up SQLite using executor `backup-state`.',
        '2. Run only `stage4_pilot_phaseA_40.txt` with `--stage 4 --max-new-calls 40`.',
        '3. Export, then run the offline `report` command from this tool.',
        '4. Review Phase A and run `stage4_pilot_phaseB_120.txt` only if justified.',
        '5. Do not run unrestricted Stage 4 or January 2026 as part of this pilot.',
        '', '## Phase A decision heuristic (review-only)', '',
        'Proceed to Phase B only after 40 requests resolve, >=28 return OK (70%),',
        'and >=24/40 extra records are NORMAL with seven valid core measurements.',
        'At least three of four selected station-years must achieve >=4/5 extra-hour',
        'retrieval AND >=4/5 QC-valid extra-hour observations on BOTH dates.',
        'At least three of eight six-point days must show some measurable variability',
        '(>0.01 in raw temperature or DO values); otherwise inspect data duplication.',
        'A failed heuristic is not an API fault; it triggers investigation.',
        '', 'Details: `pilot_station_year_pairs.csv`, `pilot_station_days.csv`,',
        '`stage4_readiness_by_site.csv`, `stage4_readiness_by_date.csv`.',
    ]
    (out/'STAGE4_READINESS_AND_PILOT.md').write_text('\n'.join(s)+'\n',encoding='utf-8')
    print('Build PASS | approved-plan SHA256:',plan_hash)
    print('Readiness:',dict(total))
    print('Pilot: 16 station-years, 32 days, 160 pending approved IDs, A=40, B=120')
    print('Areas:',dict(sorted(area_counts.items())))
    print('Years:',dict(sorted(years.items())))
    print('Files written to:',out)

def report(args):
    if sha256(args.plan) != PLAN_SHA256:
        raise ValueError('Plan hash mismatch')
    plan=load_csv(args.plan)
    validate_plan(plan)
    m=validate_manifest(plan,load_csv(args.manifest))
    obs={r['request_id']:r for r in load_csv(args.observations)}
    p={r['request_id']:r for r in plan}
    full=read_ids(args.pilot_list)
    phase_a=read_ids(args.phase_a_list)
    if len(full)!=160 or len(phase_a)!=40 or not phase_a.issubset(full):
        raise ValueError('Pilot identity/phase lists incorrect')
    if not full.issubset(p):
        raise ValueError('Unknown request ID in pilot')
    pair=collections.defaultdict(dict)
    hour_rows=[]
    for rid in sorted(full,key=lambda i:int(p[i]['plan_order'])):
        r=p[rid]
        status=m[rid]['status']
        if r['plan_component']!=COMP_EXTRA or r['target_datetime'][:4]=='2026':
            raise ValueError('Pilot ID not an eligible 2021-2025 extra-hour request')
        o=obs.get(rid)
        day=r['target_datetime'][:10]
        key=(r['study_site_code'],r['station_identity_id'],r['target_datetime'][:4])
        month=day[5:7]
        phase='A' if rid in phase_a else 'B'
        pair[key].setdefault(month,[]).append((r,status,o,phase))
        hour_rows.append(dict(phase=phase,study_site_code=r['study_site_code'],
            station_identity_id=r['station_identity_id'],station_name=r['qingyue_station_name'],
            target_datetime=r['target_datetime'],request_id=rid,status=status,
            qc_core_normal='true' if valid_obs_at_time(o,r['target_datetime']) else 'false',
            section_status=o.get('section_status','') if o else '',
            monitor_time=o.get('monitor_time','') if o else '',
            water_temp=o.get('water_temp','') if o else '',
            dissolvedoxygen=o.get('dissolvedoxygen','') if o else '',
            ph=o.get('ph','') if o else '',codmn=o.get('codmn','') if o else '',
            nh3_n=o.get('nh3-n','') if o else '',tp=o.get('tp','') if o else '',tn=o.get('tn','') if o else ''))
    day_rows=[]
    pair_rows=[]
    for (site,sid,year), bymonth in sorted(pair.items()):
        if sorted(bymonth) != ['01','07']:
            raise ValueError('Not a Jan/Jul pair: '+sid)
        phase= bymonth['01'][0][3]
        per=[]
        for month in ('01','07'):
            rr=bymonth[month]
            if len(rr)!=5 or len({int(x[0]['target_hour']) for x in rr})!=5:
                raise ValueError('Incomplete station-day for '+sid)
            stat=collections.Counter(status for _,status,_,_ in rr)
            noontime=year+'-'+month+'-15 12:00:00'
            noon_matches=[r for r in plan if r['plan_component']==COMP_NOON
                          and r['station_identity_id']==sid and r['target_datetime']==noontime]
            if len(noon_matches)!=1:
                raise ValueError('Missing noon for '+sid)
            noo=obs.get(noon_matches[0]['request_id'])
            hours=sorted((int(r['target_hour']),float(o['dissolvedoxygen']),float(o['water_temp']))
                         for r,status,o,_ in rr if status=='ok' and valid_core(o))
            if valid_obs_at_time(noo,noontime):
                hours.append((12,float(noo['dissolvedoxygen']),float(noo['water_temp'])))
            complete= len(hours)==6 and stat['ok']==5
            day_rows.append(dict(phase=phase,site=site,station_identity_id=sid,
                station_name=rr[0][0]['qingyue_station_name'],date=year+'-'+month+'-15',
                ok=stat['ok'],no_record=stat['no_record'],pending=stat['pending'],
                other=5-sum(stat[k] for k in ('ok','no_record','pending')),
                core_normal_ok=sum(valid_obs_at_time(o,r['target_datetime']) for r,status,o,_ in rr if status=='ok'),
                complete_6point=complete,
                do_range_if_complete=round(max(x[1] for x in hours)-min(x[1] for x in hours),5) if complete else '',
                temp_range_if_complete=round(max(x[2] for x in hours)-min(x[2] for x in hours),5) if complete else ''))
            per.append(stat)
        pair_rows.append(dict(phase=phase,study_site_code=site,station_identity_id=sid,
            station_name=bymonth['01'][0][0]['qingyue_station_name'],year=year,
            jan_ok=per[0]['ok'],jul_ok=per[1]['ok'],
            meets_4of5_each=per[0]['ok']>=4 and per[1]['ok']>=4,
            fully_complete=per[0]['ok']==5 and per[1]['ok']==5))
    out=Path(args.output)
    out.mkdir(parents=True,exist_ok=True)
    write_csv(out/'pilot_hour_results.csv',hour_rows,list(hour_rows[0]))
    write_csv(out/'pilot_day_results.csv',day_rows,list(day_rows[0]))
    write_csv(out/'pilot_pair_results.csv',pair_rows,list(pair_rows[0]))
    ac=[x for x in hour_rows if x['phase']=='A']
    c=collections.Counter(r['status'] for r in ac)
    ready=len(ac)==40 and sum(c[k] for k in ('ok','no_record','skipped_noon_missing'))==40
    p4=sum(r['phase']=='A' and r['meets_4of5_each'] for r in pair_rows)
    qc_good=sum(x['phase']=='A' and x['qc_core_normal']=='true' for x in hour_rows)
    qa_days={(x['site'], x['station_identity_id'], x['date'][:4], x['date'][5:7]): x['core_normal_ok'] for x in day_rows if x['phase']=='A'}
    qa_pairs=sum(r['phase']=='A' and qa_days.get((r['study_site_code'],r['station_identity_id'],r['year'],'01'),0)>=4 and qa_days.get((r['study_site_code'],r['station_identity_id'],r['year'],'07'),0)>=4 for r in pair_rows)
    variable_days=sum(x['phase']=='A' and x['complete_6point'] and ((x['do_range_if_complete'] or 0)>0.01 or (x['temp_range_if_complete'] or 0)>0.01) for x in day_rows)
    gate='INCOMPLETE' if not ready else 'REVIEW_GO' if c['ok']>=28 and qc_good>=24 and p4>=3 and qa_pairs>=3 and variable_days>=3 else 'REVIEW_HOLD'
    doc=['# Qingyue Stage 4 diurnal pilot: post-run assessment','',
         'All diagnostics are derived from exported files; zero API calls or SQLite edits.',
         f'Phase A: {c["ok"]}/40 OK; {c["no_record"]}/40 no_record; {c["pending"]}/40 pending.',
         f'Phase A normal-status complete-core observations: {qc_good}/40.',
         f'Phase A station-year pairs with >=4/5 extra-hour responses in both Jan and July: {p4}/4.',
         f'Phase A station-year pairs with >=4/5 core-valid NORMAL extra-hour responses in both Jan and July: {qa_pairs}/4.',
         f'Phase A fully complete 6-point station-days showing >0.01 change in temperature or DO: {variable_days}/8.',
         f'**Phase A decision: {gate}** (review-only; never automatically launches Phase B).',
         '', '| Phase | Site | Station | Year | Jan OK/5 | Jul OK/5 | 4/5 both? |',
         '|---|---|---|---:|---:|---:|---|']
    for r in sorted(pair_rows,key=lambda x:(x['phase'],x['study_site_code'],x['year'])):
        doc.append(f'| {r["phase"]} | {r["study_site_code"]} | {r["station_name"]} | {r["year"]} | {r["jan_ok"]} | {r["jul_ok"]} | {"Yes" if r["meets_4of5_each"] else "No"} |')
    doc+=['','A complete 6-point profile requires the noon observation plus all five',
          'supplementary hours; only complete, normal, seven-core profiles qualify for',
          'the strict within-day temperature and dissolved-oxygen range estimates.',
          'The gate does not validate units, source calibration or watershed assignment.',
          'If Phase A is incomplete, resolve pending rows before interpreting its gate.']
    (out/'PILOT_POSTRUN_REPORT.md').write_text('\n'.join(doc)+'\n',encoding='utf-8')
    print('Pilot report PASS; phase A gate:',gate)
    print('Phase A responses',dict(c),'four-of-five pairs',p4,'QC pairs',qa_pairs,'variable complete days',variable_days,'quality-complete extra hours',qc_good)
    print('Reports:',out)

def main():
    q=argparse.ArgumentParser(description=__doc__)
    sub=q.add_subparsers(dest='command',required=True)
    b=sub.add_parser('build',help='Generate fixed approved-plan pilot ID lists and readiness audit')
    b.add_argument('--plan',required=True)
    b.add_argument('--manifest',required=True)
    b.add_argument('--observations',required=True)
    b.add_argument('--crosswalk',required=True)
    b.add_argument('--geography-review',default=None)
    b.add_argument('--output',required=True)
    e=sub.add_parser('report',help='Offline post-pilot retrieval/QC report and review-only gate')
    e.add_argument('--plan',required=True)
    e.add_argument('--manifest',required=True)
    e.add_argument('--observations',required=True)
    e.add_argument('--pilot-list',required=True)
    e.add_argument('--phase-a-list',required=True)
    e.add_argument('--output',required=True)
    a=q.parse_args()
    (build if a.command=='build' else report)(a)
if __name__=='__main__':
    main()

#!/usr/bin/env python3
"""Read-only acquisition completion inventory for Qingyue v1.1.0.

Inputs: original immutable approved plan + SQLite-derived exports.
Outputs: currently-pending request-ID allowlists, eligibility accounting,
         stage-state summary, and a human-readable verification report.

No HTTP calls, SQLite reads/writes, credential use, or raw-response edits.
Lists are regenerated from the *latest* export and may be reused safely by
v1.1.0 because the executor skips already-completed request identities.
"""
from __future__ import annotations

import argparse
import collections
import csv
import gzip
import hashlib
import json
import math
from pathlib import Path

PLAN_HASH = 'a8e0c166f199971f63bdb209d4e5c13bbf5dae21eec9ebd45447ad6c453c6587'
N_PLAN = 39626
CORE = ('water_temp', 'ph', 'dissolvedoxygen', 'codmn', 'nh3-n', 'tp', 'tn')
COMPLETED = {'ok', 'no_record', 'skipped_noon_missing'}
RETRIEVABLE = {'pending', 'retryable_error'}

LIST_NAMES = [
    'stage3_january2026_pending',
    'stage3_january2026_probe20',
    'stage4_2021_2025_noon_strict_all',
    'stage4_2021_2025_noon_strict_BHB_LZB',
    'stage4_2021_2025_noon_strict_other',
    'stage4_2021_2025_noon_qc_review',
    'stage4_2026_july_noon_ok',
    'stage4_2026_january_noon_ok',
    'stage4_noon_no_record_skip',
    'stage4_noon_unresolved_block',
    'stage4_all_pending',
    'stage4_noon_ok_all',
    'deferred_2020_noon_pending',
    'deferred_2020_noon_probe25',
    'deferred_2020_extra_pending',
    'deferred_2020_all_pending',
    'unresolved_error_statuses',
]


def read_csv(path: Path):
    opener = gzip.open if str(path).endswith('.gz') else open
    with opener(path, 'rt', encoding='utf-8-sig', newline='') as f:
        return list(csv.DictReader(f))


def stage(r):
    y = r['target_datetime'][:4]
    ym = r['target_datetime'][:7]
    component = r['plan_component']
    if '2020-05' <= ym <= '2020-10':
        return 'deferred-2020'
    if component == 'monthly_noon_backbone':
        if '2021' <= y <= '2025':
            return '1'
        if ym in ('2020-11', '2020-12'):
            return '2'
        if y == '2026':
            return '3'
    if component == 'semiannual_full_day_extra' and '2021' <= y <= '2026':
        return '4'
    raise ValueError('Unclassified approved-plan request_id: ' + r['request_id'])


def good_noon(row, observations):
    o = observations.get(row['request_id'])
    if not o or o.get('section_status') != '\u6b63\u5e38':
        return False
    if o.get('monitor_time') != row['target_datetime']:
        return False
    for key in CORE:
        try:
            v = float(o.get(key, ''))
        except (TypeError, ValueError):
            return False
        if not math.isfinite(v) or v < 0:
            return False
    return True


def round_robin_probe(rows, size, attribute):
    """Deterministic, geographically or temporally stratified *pending* probe."""
    by_key = collections.defaultdict(list)
    for row in sorted(rows, key=lambda x: int(x['plan_order'])):
        by_key[attribute(row)].append(row)
    selection = []
    keys = sorted(by_key)
    while len(selection) < size and any(by_key.values()):
        for key in keys:
            if by_key[key]:
                selection.append(by_key[key].pop(0))
                if len(selection) == size:
                    break
    return sorted(selection, key=lambda r: int(r['plan_order']))


def main():
    a = argparse.ArgumentParser(description=__doc__)
    a.add_argument('--plan', required=True)
    a.add_argument('--manifest', required=True)
    a.add_argument('--observations', required=True)
    a.add_argument('--output', required=True)
    a.add_argument('--expect-initial-snapshot', action='store_true',
                   help='Require the October 14 2026 initial unresolved counts')
    args = a.parse_args()
    out = Path(args.output)
    plan_path = Path(args.plan)
    digest = hashlib.sha256(plan_path.read_bytes()).hexdigest()
    if digest != PLAN_HASH:
        raise SystemExit('STOP: original approved-plan SHA256 mismatch: ' + digest)
    P0, M0, O0 = read_csv(plan_path), read_csv(Path(args.manifest)), read_csv(Path(args.observations))
    P = {r['request_id']:r for r in P0}
    M = {r['request_id']:r for r in M0}
    O = {r['request_id']:r for r in O0}
    if len(P0) != N_PLAN or len(M0) != N_PLAN or len(P) != N_PLAN or len(M) != N_PLAN or set(P) != set(M):
        raise SystemExit('STOP: plan and manifest identities or lengths invalid')
    if len(O) != len(O0) or set(O) != {rid for rid,r in M.items() if r['status']=='ok'}:
        raise SystemExit('STOP: observations are not a unique, current export of manifest OK rows')
    for rid, p in P.items():
        m = M[rid]
        for col in ('plan_order','plan_component','station_identity_id','target_datetime','study_site_code'):
            if p[col] != m[col]:
                raise SystemExit('STOP: plan/manifest discrepancy in '+col+' for '+rid)
    NOON = {}
    for p in P0:
        if p['plan_component'] == 'monthly_noon_backbone':
            key=(p['station_identity_id'], p['target_datetime'])
            if key in NOON:
                raise SystemExit('STOP: duplicate noon identity '+str(key))
            NOON[key]=p

    groups = collections.defaultdict(list)
    stage_counts = collections.defaultdict(collections.Counter)
    year_site = collections.Counter()
    dep_counts = collections.Counter()
    for p in P0:
        rid=p['request_id']
        st=stage(p)
        m=M[rid]
        status=m['status']
        stage_counts[st][status]+=1
        if status in COMPLETED:
            continue
        if status not in RETRIEVABLE:
            groups['unresolved_error_statuses'].append(p)
            continue
        if st == '3':
            if p['target_datetime'][:7] != '2026-01':
                raise SystemExit('STOP: unexpected pending 2026 noon month: '+rid)
            groups['stage3_january2026_pending'].append(p)
        elif st == '4':
            groups['stage4_all_pending'].append(p)
        elif st == 'deferred-2020':
            groups['deferred_2020_all_pending'].append(p)
            if p['plan_component'] == 'monthly_noon_backbone':
                groups['deferred_2020_noon_pending'].append(p)
            else:
                groups['deferred_2020_extra_pending'].append(p)
        elif st in ('1','2'):
            raise SystemExit('STOP: Stage 1/2 unexpectedly has unresolved requests')
        else:
            raise SystemExit('STOP: unknown stage for unresolved request')

        if p['plan_component'] == 'semiannual_full_day_extra' and st in ('4', 'deferred-2020'):
            target_noon=p['target_datetime'][:11]+'12:00:00'
            dep=NOON.get((p['station_identity_id'], target_noon))
            if dep is None:
                raise SystemExit('STOP: missing noon dependency '+rid)
            dep_status=M[dep['request_id']]['status']
            dep_counts[(st,dep_status)]+=1
            if st == '4':
                year=p['target_datetime'][:4]
                site=p['study_site_code']
                if dep_status=='no_record':
                    cat='stage4_noon_no_record_skip'
                elif dep_status not in COMPLETED:
                    cat='stage4_noon_unresolved_block'
                elif dep_status=='ok':
                    groups['stage4_noon_ok_all'].append(p)
                    if year=='2026':
                        if p['target_datetime'][:7]=='2026-07':
                            cat='stage4_2026_july_noon_ok'
                        elif p['target_datetime'][:7]=='2026-01':
                            cat='stage4_2026_january_noon_ok'
                        else:
                            raise SystemExit('STOP: unexpected Stage4 2026 date '+rid)
                    elif '2021'<=year<='2025':
                        if good_noon(dep, O):
                            cat='stage4_2021_2025_noon_strict_all'
                            if site in ('BHB','LZB'):
                                groups['stage4_2021_2025_noon_strict_BHB_LZB'].append(p)
                            else:
                                groups['stage4_2021_2025_noon_strict_other'].append(p)
                        else:
                            cat='stage4_2021_2025_noon_qc_review'
                    else:
                        raise SystemExit('STOP: unexpected Stage4 year '+rid)
                else:
                    raise SystemExit('STOP: completed noon has unsupported status: '+dep_status)
                groups[cat].append(p)
                year_site[(year,site,cat)]+=1

    groups['stage3_january2026_probe20'] = round_robin_probe(groups['stage3_january2026_pending'],20,lambda r:r['study_site_code'])
    groups['deferred_2020_noon_probe25'] = round_robin_probe(groups['deferred_2020_noon_pending'],25,lambda r:r['target_datetime'][:7])
    # The 'all' and 'probe' lists intentionally overlap their disjoint execution cohorts.
    partition_names=[
        'stage4_2021_2025_noon_strict_all',
        'stage4_2021_2025_noon_qc_review',
        'stage4_2026_july_noon_ok',
        'stage4_2026_january_noon_ok',
        'stage4_noon_no_record_skip',
        'stage4_noon_unresolved_block',
    ]
    assert set(r['request_id'] for r in groups['stage4_all_pending']) == set(
        r['request_id'] for name in partition_names for r in groups[name]
    ), 'Stage4 pending partition mismatch'
    assert set(r['request_id'] for r in groups['stage4_2021_2025_noon_strict_all']) == set(
        r['request_id'] for name in ('stage4_2021_2025_noon_strict_BHB_LZB','stage4_2021_2025_noon_strict_other')
        for r in groups[name]
    ), 'Strict ready site partition mismatch'
    assert not groups['unresolved_error_statuses'], 'STOP: manual review of non-retryable errors required'
    pending_active=sum(stage_counts[s]['pending'] for s in ('1','2','3','4'))
    if args.expect_initial_snapshot:
        checks={
            'stage3_january2026_pending':263,
            'stage4_all_pending':15035,
            'stage4_2021_2025_noon_strict_all':8935,
            'stage4_2021_2025_noon_strict_BHB_LZB':8140,
            'stage4_2021_2025_noon_strict_other':795,
            'stage4_2021_2025_noon_qc_review':730,
            'stage4_2026_july_noon_ok':1065,
            'stage4_2026_january_noon_ok':0,
            'stage4_noon_no_record_skip':2990,
            'stage4_noon_unresolved_block':1315,
            'deferred_2020_all_pending':2885,
            'deferred_2020_noon_pending':1560,
            'deferred_2020_extra_pending':1325,
        }
        for k,n in checks.items():
            if len(groups[k])!=n:
                raise SystemExit(f'STOP: reference count changed for {k}: {len(groups[k])} (expected {n})')
        if len(O)!=17660 or pending_active!=15298:
            raise SystemExit('STOP: initial snapshot exported observation count or active pending changed')

    out.mkdir(parents=True, exist_ok=True)
    rows_for_index=[]
    for name in LIST_NAMES:
        rows=sorted(groups[name],key=lambda r:int(r['plan_order']))
        ids=[r['request_id'] for r in rows]
        if len(ids)!=len(set(ids)):
            raise SystemExit('STOP: duplicate IDs in '+name)
        path=out/(name+'.txt')
        if ids:
            path.write_text('\n'.join(ids)+'\n',encoding='utf-8')
            present=path.name
        else:
            if path.exists():path.unlink()  # delete only our known stale list
            present='(zero; no TXT file)'
        rows_for_index.append((name,len(ids),present))
    with (out/'list_index.csv').open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.writer(f)
        w.writerow(('list_name','current_pending_requests','file_name'))
        w.writerows(rows_for_index)
    with (out/'stage4_by_year_site.csv').open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.writer(f)
        w.writerow(('year','site','readiness_class','pending_request_count'))
        for row,n in sorted(year_site.items()):w.writerow((*row,n))
    state={
        'approved_plan_SHA256':digest,
        'plan_rows':len(P),
        'manifest_rows':len(M),
        'observations_ok_rows':len(O),
        'stage_counts':{s:dict(stage_counts[s]) for s in ('1','2','3','4','deferred-2020')},
        'active_stages_pending':sum(v for s in ('1','2','3','4') for k,v in stage_counts[s].items() if k not in COMPLETED),
        'all_plan_pending':sum(v for c in stage_counts.values() for k,v in c.items() if k not in COMPLETED),
        'stage4_pending_noon_dependency':{f'{s}/{d}':v for (s,d),v in sorted(dep_counts.items())},
        'request_list_counts':{name:len(groups[name]) for name in LIST_NAMES},
        'usage_note':'Offline manifest snapshot; executor SQLite status is authoritative until a new export is performed.',
        'status_semantics':'ok = data; no_record = actual attempted no-result; skipped_noon_missing = unattempted conditional skip',
    }
    (out/'completion_state.json').write_text(json.dumps(state,indent=2,ensure_ascii=False)+'\n',encoding='utf-8')
    report=[
        '# Qingyue completion inventory — read only',
        '',
        'Plan SHA256: `'+digest+'`',
        '',
        '| Stage | Planned | OK | No record | Conditional skip | Pending/other |',
        '|---|---:|---:|---:|---:|---:|',
    ]
    for st in ('1','2','3','4','deferred-2020'):
        sc=stage_counts[st]
        total=sum(sc.values())
        unresolved=sum(v for k,v in sc.items() if k not in COMPLETED)
        report.append(f'| {st} | {total:,} | {sc["ok"]:,} | {sc["no_record"]:,} | {sc["skipped_noon_missing"]:,} | {unresolved:,} |')
    report.extend(['','**Stage 1–4 complete:** '+('YES' if state['active_stages_pending']==0 else 'NO'),
                   '', '**All 39,626 rows complete:** '+('YES' if state['all_plan_pending']==0 else 'NO'),
                   '', 'Completeness means all requests have a terminal status, **not** that every request produced data.',
                   'The separately deferred May–Oct 2020 cohort is outside Stages 1–4.',
                   '', '## Current request cohorts', '', '| List | Pending rows |', '|---|---:|'])
    for name in LIST_NAMES:report.append(f'| {name} | {len(groups[name]):,} |')
    (out/'COMPLETION_CHECK.md').write_text('\n'.join(report)+'\n',encoding='utf-8')
    print('PASS: original plan SHA256 and all 39,626 request identities verified')
    print('PASS: current OK observation rows',len(O))
    print('Stage 1–4 unresolved:',state['active_stages_pending'])
    print('Stage 4 pending:',len(groups['stage4_all_pending']))
    print('Stage 4 ready (noon OK):',len(groups['stage4_noon_ok_all']))
    print('Stage 4 conditional skips (noon no_record):',len(groups['stage4_noon_no_record_skip']))
    print('Stage 4 blocked (noon unresolved):',len(groups['stage4_noon_unresolved_block']))
    print('Deferred 2020 pending:',len(groups['deferred_2020_all_pending']))
    print('All output read-only with respect to raw archive/SQLite; zero Huawei requests')
    print('Lists and report:',out)


if __name__=='__main__':
    main()

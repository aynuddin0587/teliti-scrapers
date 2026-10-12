#!/usr/bin/env python3
"""Read-only Qingyue Stage 3/4 readiness classifier and request-ID list generator.

Inputs: approved plan, exported manifest, exported observations.
Outputs: TXT allowlists, CSV summary, JSON metadata; never touches SQLite or APIs.

Classification is an acquisition triage, NOT a validated scientific QC protocol.
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

PLAN_SHA = 'a8e0c166f199971f63bdb209d4e5c13bbf5dae21eec9ebd45447ad6c453c6587'
CORE = ('water_temp', 'ph', 'dissolvedoxygen', 'codmn', 'nh3-n', 'tp', 'tn')
FUJIAN = ('XMB', 'MJE', 'QZB', 'DSB', 'SMB')
COMPARATORS = ('BHB', 'LZB')
OTHER = ('BBG', 'DYB', 'JZB')


def read_csv(path):
    opener = gzip.open if str(path).endswith('.gz') else open
    with opener(path, 'rt', newline='', encoding='utf-8-sig') as fh:
        return list(csv.DictReader(fh))


def noon_core_normal(n, obs):
    """Acquisition triage: source reports normal status and seven numeric values."""
    if n is None:
        return False
    r = obs.get(n['request_id'])
    if not r or r['section_status'] != '\u6b63\u5e38':
        return False
    if r['monitor_time'] != r['target_datetime'] or r['target_datetime'] != n['target_datetime']:
        return False
    try:
        return all(math.isfinite(float(r[c])) and float(r[c]) >= 0 for c in CORE)
    except (ValueError, TypeError, KeyError):
        return False


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--plan', required=True)
    ap.add_argument('--manifest', required=True)
    ap.add_argument('--observations', required=True)
    ap.add_argument('--output', required=True)
    ap.add_argument('--expect-snapshot', action='store_true', help='Assert the October 12 2026 reference counts')
    args = ap.parse_args()
    out = Path(args.output)
    out.mkdir(parents=True, exist_ok=True)
    with open(args.plan, 'rb') as fh:
        plan_sha = hashlib.file_digest(fh, 'sha256').hexdigest()
    if plan_sha != PLAN_SHA:
        raise SystemExit('STOP: approved plan SHA256 mismatch: ' + plan_sha)
    plan = read_csv(args.plan)
    manifest_rows = read_csv(args.manifest)
    observations = read_csv(args.observations)
    if len(plan) != 39626 or len(manifest_rows) != 39626:
        raise SystemExit('STOP: expected 39,626 approved requests in plan and manifest')
    P = {r['request_id']: r for r in plan}
    M = {r['request_id']: r for r in manifest_rows}
    O = {r['request_id']: r for r in observations}
    if len(P) != 39626 or set(P) != set(M):
        raise SystemExit('STOP: plan/manifest IDs differ, or duplicates detected')
    noon = {(r['station_identity_id'], r['target_date']):r
            for r in plan if r['plan_component'] == 'monthly_noon_backbone'}
    if len(noon) != sum(r['plan_component'] == 'monthly_noon_backbone' for r in plan):
        raise SystemExit('STOP: nonunique noon station-date identities')

    groups = collections.defaultdict(list)
    by_site_year = collections.Counter()
    eligibility = collections.Counter()
    for r in plan:
        rid = r['request_id']
        mr = M[rid]
        if mr['status'] != 'pending':
            continue
        year = r['target_date'][:4]
        site = r['study_site_code']
        if r['plan_component'] == 'monthly_noon_backbone':
            if r['target_date'] == '2026-01-15':
                groups['stage3_HOLD_january2026_noon'].append(r)
            continue
        if r['plan_component'] != 'semiannual_full_day_extra' or not ('2021' <= year <= '2026'):
            continue
        dep = noon.get((r['station_identity_id'], r['target_date']))
        if dep is None:
            raise SystemExit('STOP: missing noon dependency for ' + rid)
        ns = M[dep['request_id']]['status']
        if ns == 'no_record':
            cat = 'skip_noon_missing'
            groups['stage4_SKIP_noon_no_record'].append(r)
        elif ns != 'ok':
            cat = 'blocked_noon_pending'
            groups['stage4_HOLD_noon_pending'].append(r)
        elif year == '2026':
            cat = 'hold_2026_status_unknown'
            groups['stage4_HOLD_july2026_status_unknown'].append(r)
        elif noon_core_normal(dep, O):
            cat = 'strict_noon_ready'
            groups['stage4_READY_all_2021_2025_strict'].append(r)
            if site == 'XMB':
                groups['stage4_P1_XMB_strict'].append(r)
            elif site == 'MJE':
                groups['stage4_P1_MJE_strict'].append(r)
            elif site in ('DSB', 'QZB', 'SMB'):
                groups['stage4_P1_otherFujian_strict'].append(r)
            elif site in COMPARATORS:
                groups['stage4_P2_BHB_LZB_strict'].append(r)
            elif site in OTHER:
                groups['stage4_P3_otherSites_strict'].append(r)
            else:
                raise SystemExit('STOP: unclassified site ' + site)
            if site in FUJIAN:
                groups['stage4_P1_allFujian_strict'].append(r)
        else:
            cat = 'hold_2021_2025_noon_qc'
            groups['stage4_HOLD_2021_2025_noon_qc'].append(r)
        by_site_year[(year, site, cat)] += 1
        eligibility[cat] += 1

    expected = {
        'strict_noon_ready': 10225,
        'skip_noon_missing': 2990,
        'blocked_noon_pending': 1315,
        'hold_2026_status_unknown': 1065,
        'hold_2021_2025_noon_qc': 730,
    }
    if args.expect_snapshot and dict(eligibility) != expected:
        raise SystemExit('STOP: reference eligibility counts changed: ' + str(dict(eligibility)))
    expected_lists = {
        'stage4_P1_XMB_strict':120,
        'stage4_P1_MJE_strict':865,
        'stage4_P1_otherFujian_strict':305,
        'stage4_P1_allFujian_strict':1290,
        'stage4_P2_BHB_LZB_strict':8140,
        'stage4_P3_otherSites_strict':795,
        'stage4_READY_all_2021_2025_strict':10225,
        'stage4_HOLD_2021_2025_noon_qc':730,
        'stage4_HOLD_july2026_status_unknown':1065,
        'stage4_SKIP_noon_no_record':2990,
        'stage4_HOLD_noon_pending':1315,
        'stage3_HOLD_january2026_noon':263,
    }
    for name, num in expected_lists.items():
        rows = sorted(groups[name], key=lambda z:int(z['plan_order']))
        ids = [r['request_id'] for r in rows]
        if len(ids) != len(set(ids)):
            raise SystemExit(f'STOP: {name} contains duplicate IDs')
        if args.expect_snapshot and len(ids) != num:
            raise SystemExit(f'STOP: {name} had {len(ids)} rows, expected {num}')
        (out / (name+'.txt')).write_text('\n'.join(ids)+'\n', encoding='utf-8')
    if set(r['request_id'] for r in groups['stage4_P1_allFujian_strict']) != set(
          r['request_id'] for group in ['stage4_P1_XMB_strict', 'stage4_P1_MJE_strict', 'stage4_P1_otherFujian_strict']
          for r in groups[group]):
        raise SystemExit('STOP: Fujian partition mismatch')
    if set(r['request_id'] for r in groups['stage4_READY_all_2021_2025_strict']) != set(
          r['request_id'] for group in ['stage4_P1_allFujian_strict', 'stage4_P2_BHB_LZB_strict', 'stage4_P3_otherSites_strict']
          for r in groups[group]):
        raise SystemExit('STOP: total strict partition mismatch')
    with open(out/'stage4_readiness_by_year_site.csv', 'w', newline='', encoding='utf-8-sig') as fh:
        w=csv.writer(fh)
        w.writerow(['year', 'study_site_code', 'readiness', 'request_count'])
        for k,v in sorted(by_site_year.items()): w.writerow([*k,v])
    meta = {'approved_plan_sha256':plan_sha,
            'plan_rows':len(plan),
            'manifest_rows':len(manifest_rows),
            'observation_rows':len(observations),
            'stage4_pending_2021_2026':sum(eligibility.values()),
            'categories':dict(eligibility),
            'request_lists': {name:len(groups[name]) for name in expected_lists},
            'reference_request_lists':expected_lists,
            'classification_notes': 'Strict: noon HTTP ok, monitoring status NORMAL, exact timestamp and 7 nonnegative core numbers. Screening only; instrument validation outstanding. 2026 status is blank and held separately.'}
    if args.expect_snapshot and meta['stage4_pending_2021_2026'] != 16325:
        raise SystemExit('STOP: Stage 4 pending count changed')
    (out/'readiness_metadata.json').write_text(json.dumps(meta,indent=2,ensure_ascii=False)+'\n',encoding='utf-8')
    print('PASS: Plan and manifest reconciled:', len(plan), 'requests; SHA', plan_sha)
    print('PASS: Stage 4 pending partition:', dict(eligibility))
    for name in expected_lists:print(f'  {name}: {len(groups[name]):,}')
    print('ZERO Huawei calls; ZERO SQLite reads/writes')


if __name__=='__main__': main()

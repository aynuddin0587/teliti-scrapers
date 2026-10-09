#!/usr/bin/env python3
"""Create an auditable 20-request August 2022 adjacent-date probe.

This is OFFLINE only. It never calls Huawei and never modifies the
original approved CSV or the original acquisition SQLite database.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import io
from pathlib import Path
from urllib.parse import parse_qsl, urlencode, quote

APPROVED_PLAN_HASH = 'a8e0c166f199971f63bdb209d4e5c13bbf5dae21eec9ebd45447ad6c453c6587'
DEFAULT_ORIGINAL_PLAN = Path('D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical/plan/qingyue_request_plan_recommended.csv')
DEFAULT_ROOT = Path('D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical/date_recovery/august_2022')
PROBE_FILENAME = 'qingyue_august_2022_probe.csv'
ORIGINAL_TIME = '2022-08-15 12:00:00'
RECOVERY_DATES = ('2022-08-14', '2022-08-16')
# Ten reference identities are taken from the 2023-2025 date preflight inventory.
# Each must map uniquely to an August 15, 2022 noon row in the original plan.
REFERENCE_STATION_IDS = {
    'BBG': 'QYREC_ab556beab6c2',
    'BHB': 'QYREC_620e6a284957',
    'DSB': 'QYREC_455b1717c87a',
    'DYB': 'QYREC_3ee56759a0fd',
    'JZB': 'QYREC_2265e204e169',
    'LZB': 'QYREC_44095d5a6d02',
    'MJE': 'QYREC_043a7e9b9d1a',
    'QZB': 'QYREC_045811929263',
    'SMB': 'QYREC_1d8b10918f32',
    'XMB': 'QYREC_6e76d14cee63',
}
EXTRA_FIELDS = [
    'recovery_original_request_id',
    'recovery_original_target_datetime',
    'sampling_offset_days',
    'recovery_reason',
    'approved_plan_sha256',
]


def build_bytes(original_plan: Path) -> tuple[bytes, list[dict], list[dict]]:
    if not original_plan.is_file():
        raise FileNotFoundError(f'Original plan missing: {original_plan}')
    checksum = hashlib.sha256(original_plan.read_bytes()).hexdigest()
    if checksum != APPROVED_PLAN_HASH:
        raise ValueError('Original approved plan SHA256 mismatch; refusing to create new requests. '
                         f'Expected {APPROVED_PLAN_HASH}, found {checksum}')

    with original_plan.open('r', encoding='utf-8-sig', newline='') as f:
        reader = csv.DictReader(f)
        fields = list(reader.fieldnames or ())
        candidates: dict[str, list[dict]] = {site: [] for site in REFERENCE_STATION_IDS}
        existing_ids: set[str] = set()
        for row in reader:
            existing_ids.add(row['request_id'])
            if row['target_datetime'] != ORIGINAL_TIME or row['plan_component'] != 'monthly_noon_backbone':
                continue
            site = row['study_site_code']
            if site in REFERENCE_STATION_IDS and row['station_identity_id'] == REFERENCE_STATION_IDS[site]:
                candidates[site].append(row)

    originals: list[dict] = []
    for site in sorted(REFERENCE_STATION_IDS):
        found = candidates[site]
        if len(found) != 1:
            raise ValueError(f'Expected exactly one original row for {site}, found {len(found)}')
        originals.append(found[0])

    rows: list[dict] = []
    for date in RECOVERY_DATES:
        target = date + ' 12:00:00'
        offset = -1 if date.endswith('-14') else 1
        for base in originals:
            row = dict(base)
            pairs = parse_qsl(base['query_string'], keep_blank_values=True, strict_parsing=True)
            param_dict = dict(pairs)
            required = {
                'city': base['city_param'],
                'province': base['province_param'],
                'staname': base['staname_param'],
                'valley': base['valley_param'],
                'version': base['version_param'],
                'sta_time': ORIGINAL_TIME,
            }
            for key, value in required.items():
                if param_dict.get(key) != value:
                    raise ValueError(f'Original query/column mismatch {base["study_site_code"]}: {key}')
            if len(pairs) != 6 or len(param_dict) != 6:
                raise ValueError(f'Unexpected query parameter set for {base["study_site_code"]}')
            updated_pairs = [(key, target if key == 'sta_time' else val) for key, val in pairs]
            row['query_string'] = urlencode(updated_pairs, quote_via=quote, safe='')
            row['target_datetime'] = target
            row['target_date'] = date
            row['target_hour'] = '12'
            row['plan_order'] = str(len(rows) + 1)
            row['priority'] = '1'
            row['request_id'] = 'aug22_' + hashlib.sha256(
                f'teliti|aug2022|{base["station_identity_id"]}|{target}'.encode('utf-8')
            ).hexdigest()[:26]
            row['recovery_original_request_id'] = base['request_id']
            row['recovery_original_target_datetime'] = ORIGINAL_TIME
            row['sampling_offset_days'] = str(offset)
            row['recovery_reason'] = 'network_wide_2022_08_15_no_record'
            row['approved_plan_sha256'] = checksum
            rows.append(row)

    if len(rows) != 20 or len({r['request_id'] for r in rows}) != 20:
        raise AssertionError('Probe must contain exactly 20 unique request IDs')
    if any(r['request_id'] in existing_ids for r in rows):
        raise AssertionError('Recovery request ID collides with approved plan')
    if len({(r['station_identity_id'], r['target_datetime']) for r in rows}) != 20:
        raise AssertionError('Duplicate station/timestamp request')
    if {r['study_site_code'] for r in rows[:10]} != set(REFERENCE_STATION_IDS):
        raise AssertionError('Probe has insufficient geographic coverage')
    if {r['study_site_code'] for r in rows[10:]} != set(REFERENCE_STATION_IDS):
        raise AssertionError('Probe has insufficient geographic coverage')
    if any('sta_time=2022-08-' not in r['query_string'] or '%20' not in r['query_string'] for r in rows):
        raise AssertionError('Invalid timestamp or percent encoding')

    output = io.StringIO(newline='')
    writer = csv.DictWriter(output, fieldnames=fields + EXTRA_FIELDS, lineterminator='\n')
    writer.writeheader()
    writer.writerows(rows)
    return output.getvalue().encode('utf-8'), rows, originals


def build_probe(original_plan: Path, out: Path, recovery_root: Path) -> None:
    if original_plan.resolve() == out.resolve():
        raise ValueError('Refusing to overwrite the approved original plan')
    raw, rows, originals = build_bytes(original_plan)
    existing_state = recovery_root / 'state' / 'qingyue_historical.sqlite'
    if out.exists():
        if out.read_bytes() != raw:
            if existing_state.exists():
                raise RuntimeError('The recovery SQLite ledger already exists but the new plan bytes differ. '
                                   'Do NOT overwrite its plan; restore the original recovery CSV.')
            raise RuntimeError('Recovery plan differs from the deterministic plan. '
                               'Remove or rename the conflicting file before generating.')
        print('Existing deterministic probe plan already matches; no changes.')
    else:
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_bytes(raw)

    summary_path = out.with_name('qingyue_august_2022_station_crosswalk.csv')
    summary_fields = [
        'study_site_code', 'station_identity_id', 'station_name',
        'province', 'longitude', 'latitude', 'original_2022_08_15_request_id',
        'august_14_request_id', 'august_16_request_id',
    ]
    buf = io.StringIO(newline='')
    writer = csv.DictWriter(buf, fieldnames=summary_fields, lineterminator='\n')
    writer.writeheader()
    for i, base in enumerate(originals):
        writer.writerow({
            'study_site_code': base['study_site_code'],
            'station_identity_id': base['station_identity_id'],
            'station_name': base['qingyue_station_name'],
            'province': base['qingyue_province'],
            'longitude': base['longitude'],
            'latitude': base['latitude'],
            'original_2022_08_15_request_id': base['request_id'],
            'august_14_request_id': rows[i]['request_id'],
            'august_16_request_id': rows[i+10]['request_id'],
        })
    summary_raw = buf.getvalue().encode('utf-8')
    if summary_path.exists() and summary_path.read_bytes() != summary_raw:
        raise RuntimeError(f'Existing crosswalk differs: {summary_path}')
    if not summary_path.exists():
        summary_path.write_bytes(summary_raw)

    print('OFFLINE probe generation PASS; no API calls.')
    print('Approved plan SHA256:', APPROVED_PLAN_HASH)
    print('Selected stations: 10, one per study site')
    print('New date-specific requests: 20 (14th: 10; 16th: 10)')
    print('Recovery plan SHA256:', hashlib.sha256(raw).hexdigest())
    print('Probe CSV:', out)
    print('Crosswalk:', summary_path)


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--original-plan', type=Path, default=DEFAULT_ORIGINAL_PLAN)
    ap.add_argument('--recovery-root', type=Path, default=DEFAULT_ROOT)
    ap.add_argument('--out', type=Path, default=None)
    a = ap.parse_args()
    out = a.out or (a.recovery_root / 'plan' / PROBE_FILENAME)
    build_probe(a.original_plan, out, a.recovery_root)


if __name__ == '__main__':
    main()

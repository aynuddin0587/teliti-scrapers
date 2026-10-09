#!/usr/bin/env python3
"""Report August 2022 date-recovery pilot without any Huawei API calls.

Reads the SEPARATE 20-row recovery SQLite ledger and the ORIGINAL historical
SQLite ledger in read-only mode; writes reports into the recovery root only.
No database modification, API access, or request replay is performed.
"""
from __future__ import annotations

import argparse
import collections
import csv
import datetime as dt
import hashlib
import json
import sqlite3
from pathlib import Path

ORIGINAL_SHA = 'a8e0c166f199971f63bdb209d4e5c13bbf5dae21eec9ebd45447ad6c453c6587'
ORIGINAL_TIME = '2022-08-15 12:00:00'
RECOVERY_DATES = ('2022-08-14', '2022-08-16')
DEFAULT_ORIGINAL_ROOT = Path('D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical')
DEFAULT_RECOVERY_ROOT = DEFAULT_ORIGINAL_ROOT / 'date_recovery' / 'august_2022'
PLAN_FILENAME = 'qingyue_august_2022_probe.csv'
RECOVERY_STATUS_DONE = {'ok', 'no_record'}
MIN_SITE_SUCCESSES = 7


def connect_read_only(db_path: Path) -> sqlite3.Connection:
    if not db_path.is_file():
        raise FileNotFoundError(f'SQLite state database not found: {db_path}')
    con = sqlite3.connect(db_path.resolve().as_uri() + '?mode=ro', uri=True)
    con.row_factory = sqlite3.Row
    return con


def check_db_plan_hash(con: sqlite3.Connection, expected: str, label: str) -> None:
    row = con.execute("SELECT value FROM meta WHERE key='plan_sha256'").fetchone()
    actual = None if row is None else row['value']
    if actual != expected:
        raise RuntimeError(f'{label} ledger plan checksum mismatch: expected {expected}, got {actual}')


def read_probe_csv(plan: Path) -> tuple[list[dict], str]:
    with plan.open('r', encoding='utf-8-sig', newline='') as f:
        rows = list(csv.DictReader(f))
    if len(rows) != 20:
        raise ValueError(f'Expected exactly 20 recovery rows, found {len(rows)}')
    if len({r['request_id'] for r in rows}) != 20:
        raise ValueError('Recovery plan has duplicate IDs')
    for date in RECOVERY_DATES:
        group = [r for r in rows if r['target_datetime'] == date + ' 12:00:00']
        if len(group) != 10 or len({r['study_site_code'] for r in group}) != 10:
            raise ValueError(f'Expected ten unique-area noon requests on {date}')
    if any(r.get('approved_plan_sha256') != ORIGINAL_SHA or
           r.get('recovery_original_target_datetime') != ORIGINAL_TIME for r in rows):
        raise ValueError('Recovery plan provenance metadata does not match approved original')
    return rows, hashlib.sha256(plan.read_bytes()).hexdigest()


def assess(recovery_root: Path, original_root: Path, probe_csv: Path | None = None,
           min_success: int = MIN_SITE_SUCCESSES) -> dict:
    if not 1 <= min_success <= 10:
        raise ValueError('Threshold must be 1 to 10')
    plan = probe_csv or recovery_root / 'plan' / PLAN_FILENAME
    if not plan.is_file():
        raise FileNotFoundError('Missing recovery probe CSV: ' + str(plan))
    rows, probe_sha = read_probe_csv(plan)

    # Read main acquisition state, never write to it.
    old_db = original_root / 'state' / 'qingyue_historical.sqlite'
    with connect_read_only(old_db) as old:
        check_db_plan_hash(old, ORIGINAL_SHA, 'Original')
        old_counts = dict(old.execute(
            "SELECT status, COUNT(*) AS n FROM requests WHERE "
            "target_datetime=? AND plan_component='monthly_noon_backbone' GROUP BY status",
            (ORIGINAL_TIME,),
        ).fetchall())
        base_ids = {r['recovery_original_request_id'] for r in rows}
        if len(base_ids) != 10:
            raise ValueError('Expected ten unique original Aug 15 request IDs')
        existing_base = {}
        for old_req_id in base_ids:
            row = old.execute(
                'SELECT request_id, status, target_datetime, station_identity_id FROM requests '
                'WHERE request_id=?', (old_req_id,),
            ).fetchone()
            if row is None or row['target_datetime'] != ORIGINAL_TIME:
                raise ValueError(f'Original request ID absent or wrong timestamp: {old_req_id}')
            existing_base[old_req_id] = dict(row)

    recover_db = recovery_root / 'state' / 'qingyue_historical.sqlite'
    recovery_attempts = 0
    found: dict[str, dict] = {}
    if recover_db.exists():
        with connect_read_only(recover_db) as rec:
            check_db_plan_hash(rec, probe_sha, 'Recovery')
            recovery_attempts = rec.execute('SELECT COUNT(*) FROM attempt_log').fetchone()[0]
            for row in rec.execute('SELECT request_id, status, attempts, http_status, target_datetime, '
                                   'study_site_code, station_identity_id FROM requests'):
                found[row['request_id']] = dict(row)
            if len(found) != 20:
                raise RuntimeError('Recovery ledger must contain precisely the 20 approved probe IDs')
            for item in rows:
                ledger_row = found.get(item['request_id'])
                if (ledger_row is None or ledger_row['target_datetime'] != item['target_datetime'] or
                    ledger_row['station_identity_id'] != item['station_identity_id'] or
                    ledger_row['study_site_code'] != item['study_site_code']):
                    raise RuntimeError('Recovery ledger/CSV mismatch for request ' + item['request_id'])

    matrix: dict[tuple[str, str], dict] = {}
    for row in rows:
        ident = row['study_site_code']
        date = row['target_date']
        baseline = existing_base[row['recovery_original_request_id']]
        if baseline['station_identity_id'] != row['station_identity_id']:
            raise RuntimeError('Original/recovery station identity mismatch')
        result = found.get(row['request_id'])
        matrix[(ident, date)] = {
            'study_site_code': ident,
            'station_name': row['qingyue_station_name'],
            'station_identity_id': row['station_identity_id'],
            'requested_date': date,
            'target_datetime': row['target_datetime'],
            'sampling_offset_days': int(row['sampling_offset_days']),
            'recovery_request_id': row['request_id'],
            'original_request_id': row['recovery_original_request_id'],
            'original_aug15_status': baseline['status'],
            'recovery_status': 'pending' if result is None else result['status'],
            'http_status': '' if result is None else (result['http_status'] or ''),
            'attempts': 0 if result is None else result['attempts'],
        }

    day_stats: dict[str, dict] = {}
    for date in RECOVERY_DATES:
        statuses = collections.Counter(matrix[(site, date)]['recovery_status']
                                       for site in sorted({r['study_site_code'] for r in rows}))
        day_stats[date] = {
            'ok': statuses['ok'],
            'no_record': statuses['no_record'],
            'other': sum(n for stat, n in statuses.items() if stat not in RECOVERY_STATUS_DONE),
            'completed': statuses['ok'] + statuses['no_record'] == 10,
        }

    if not all(day_stats[date]['completed'] for date in RECOVERY_DATES):
        verdict, preferred = 'INCOMPLETE', None
    elif max(day_stats[day]['ok'] for day in RECOVERY_DATES) < min_success:
        verdict, preferred = 'HOLD_LOW_COVERAGE', None
    else:
        preferred = sorted(RECOVERY_DATES, key=lambda date: (-day_stats[date]['ok'], date))[0]
        verdict = 'RECOMMEND_' + preferred

    results = {
        'original_plan_sha256': ORIGINAL_SHA,
        'recovery_probe_sha256': probe_sha,
        'original_target_datetime': ORIGINAL_TIME,
        'original_aug15_status_counts': old_counts,
        'recovery_attempts_logged': recovery_attempts,
        'day_counts': day_stats,
        'minimum_area_successes': min_success,
        'verdict': verdict,
        'preferred_recovery_date': preferred,
        'method': 'Maximize distinct-study-area successes; tie goes to the 14th; require both 10-station panels resolved and >=7/10 success.',
        'original_plan_or_ledger_modified': False,
        'note': 'Recommendation only. No bulk recovery, backfill, or original-plan alteration is automatic.',
    }

    out_dir = recovery_root / 'processed'
    out_dir.mkdir(parents=True, exist_ok=True)
    with (out_dir / 'august_2022_recovery_probe_result.json').open('w', encoding='utf-8') as f:
        json.dump(results, f, ensure_ascii=False, indent=2)
        f.write('\n')
    fields = ['study_site_code', 'station_name', 'station_identity_id',
              'requested_date', 'target_datetime', 'sampling_offset_days',
              'original_request_id', 'original_aug15_status',
              'recovery_request_id', 'recovery_status', 'http_status', 'attempts']
    with (out_dir / 'august_2022_recovery_probe_by_station.csv').open('w', encoding='utf-8', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=fields, lineterminator='\n')
        writer.writeheader()
        writer.writerows(matrix[k] for k in sorted(matrix))

    lines = [
        '# Qingyue August 2022 adjacent-date recovery probe',
        '',
        'Offline audit. Neither original plan/ledger nor Huawei API has been changed by this report.',
        '',
        f'**Decision:** `{verdict}`',
        '',
        '| Date | Successful areas | No record | Unresolved |',
        '|---|---:|---:|---:|',
    ]
    for date in RECOVERY_DATES:
        s = day_stats[date]
        lines.append(f'| {date} | {s["ok"]} | {s["no_record"]} | {s["other"]} |')
    lines.extend([
        '',
        f'Original 2022-08-15 status counts (all stations): `{old_counts}`',
        f'Recovery API attempts recorded in separate ledger: **{recovery_attempts}**',
        '',
        '| Area | Reference station | Aug 15 original | Aug 14 | Aug 16 |',
        '|---|---|---|---|---|',
    ])
    for site in sorted({r['study_site_code'] for r in rows}):
        a = matrix[(site, RECOVERY_DATES[0])]
        b = matrix[(site, RECOVERY_DATES[1])]
        lines.append(f'| {site} | {a["station_name"]} | {a["original_aug15_status"]} | '
                     f'{a["recovery_status"]} | {b["recovery_status"]} |')
    lines.extend([
        '',
        '### Decision rule',
        f'Compare both complete ten-site panels; require at least {min_success}/10 successful areas. '
        'Choose the day with more successful areas; in a tie, prefer the 14th.',
        'If either panel is incomplete, no date is recommended. '
        'A successful pilot does not guarantee full coverage at all stations.',
        '',
        '### Next step',
        'If a date is recommended, review this report before creating a separately versioned '
        'full-station recovery plan. Do not change the target dates or request IDs in the approved '
        '39,626-request backbone, and never silently substitute both alternative dates into a monthly series.',
        '',
    ])
    (out_dir / 'august_2022_recovery_probe_report.md').write_text('\n'.join(lines), encoding='utf-8')
    print('Recovery date comparison (READ ONLY; zero Huawei calls)')
    print('Original 15 Aug status counts:', old_counts)
    for date in RECOVERY_DATES:
        x = day_stats[date]
        print(f'{date}: {x["ok"]}/10 OK, {x["no_record"]} no_record, {x["other"]} unresolved')
    print('Recovery HTTP attempts already logged:', recovery_attempts)
    print('Decision:', verdict)
    print('Reports:', out_dir)
    return results


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--original-root', type=Path, default=DEFAULT_ORIGINAL_ROOT)
    ap.add_argument('--recovery-root', type=Path, default=DEFAULT_RECOVERY_ROOT)
    ap.add_argument('--probe-plan', type=Path, default=None)
    ap.add_argument('--minimum-ok', type=int, default=MIN_SITE_SUCCESSES)
    a = ap.parse_args()
    assess(a.recovery_root, a.original_root, a.probe_plan, a.minimum_ok)


if __name__ == '__main__':
    main()

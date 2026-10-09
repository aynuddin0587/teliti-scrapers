#!/usr/bin/env python3
"""Offline month/site coverage report for the Qingyue historical executor.

Reads an existing checkpoint SQLite database and an approved request-ID subset.
NO HTTP requests, NO API credentials, and NO database writes.
"""
from __future__ import annotations

import argparse
import csv
import sqlite3
from collections import Counter, defaultdict
from pathlib import Path

DEFAULT_DB = Path(
    'D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/'
    'historical/state/qingyue_historical.sqlite'
)


def load_subset(path: Path) -> list[dict[str, str]]:
    if not path.is_file():
        raise FileNotFoundError(f'Request-list file not found: {path}')
    with path.open('r', encoding='utf-8-sig', newline='') as file:
        reader = csv.DictReader(file)
        if not reader.fieldnames or 'request_id' not in reader.fieldnames:
            raise ValueError('Request-list CSV must have a request_id column')
        out = [r for r in reader if (r.get('request_id') or '').strip()]
    if len(set(r['request_id'] for r in out)) != len(out):
        raise ValueError('Request-list contains duplicated request IDs')
    return out


def fetch_requests(db: Path, ids: list[str]) -> dict[str, dict]:
    if not db.is_file():
        raise FileNotFoundError(f'Executor checkpoint database not found: {db}')
    with sqlite3.connect(db.resolve().as_uri() + '?mode=ro', uri=True) as conn:
        conn.row_factory = sqlite3.Row
        result = {}
        for i in range(0, len(ids), 500):
            block = ids[i:i+500]
            placeholders = ','.join('?' for _ in block)
            stmt = (
                'SELECT request_id, status, attempts, http_status, '
                'response_row_count, study_site_code, station_identity_id, '
                'staname_param, target_datetime, plan_component, last_error '
                f'FROM requests WHERE request_id IN ({placeholders})'
            )
            for row in conn.execute(stmt, block):
                result[row['request_id']] = dict(row)
    missing = set(ids) - set(result)
    if missing:
        raise ValueError(
            f'{len(missing)} request IDs missing from the executor database; '
            f'first: {next(iter(sorted(missing)))}. '
            'Do not modify the approved plan or reset SQLite.'
        )
    return result


def show_table(title: str, grouped: dict[str, list[dict]]) -> None:
    print('\n' + title)
    print('-' * len(title))
    print(f'{"Group":<22}{"OK":>7}{"No record":>11}{"Pending/other":>16}{"Success":>11}')
    for key, group in sorted(grouped.items()):
        st = Counter((r.get('status') or 'missing') for r in group)
        ok = st['ok']
        no_record = st['no_record']
        other = len(group) - ok - no_record
        completed = ok + no_record
        pct = f'{100*ok/completed:.1f}%' if completed else 'n/a'
        print(f'{key:<22}{ok:>7}{no_record:>11}{other:>16}{pct:>11}')


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--request-list', required=True, type=Path)
    parser.add_argument('--db', type=Path, default=DEFAULT_DB)
    parser.add_argument('--output', type=Path, default=None,
                        help='Optional detailed CSV path for QA (no extra requests)')
    args = parser.parse_args()
    subset = load_subset(args.request_list)
    matching = fetch_requests(args.db, [r['request_id'] for r in subset])
    merged = []
    for row in subset:
        rec = {**row, **matching[row['request_id']]}
        rec['year_month'] = str(rec.get('target_datetime') or '')[:7]
        merged.append(rec)

    states = Counter((r.get('status') or 'missing') for r in merged)
    print('Qingyue probe coverage (OFFLINE; 0 API calls)')
    print('===========================================')
    print(f'Requests: {len(merged):,}')
    print('Status:', ', '.join(f'{k}={v}' for k,v in sorted(states.items())))
    print('Attempts logged for subset:', sum(int(r.get('attempts') or 0) for r in merged))

    for title, key_func in (
        ('Coverage by year-month', lambda r: r['year_month']),
        ('Coverage by probe purpose', lambda r: r.get('probe_role') or r.get('plan_component') or '<unknown>'),
        ('Coverage by observation hour', lambda r: str(r.get('target_datetime') or '')[11:13]),
        ('Coverage by year-month and hour', lambda r: str(r.get('target_datetime') or '')[:7]+' '+str(r.get('target_datetime') or '')[11:13]),
        ('Coverage by study area', lambda r: r.get('study_site_code') or '<unknown>'),
    ):
        g = defaultdict(list)
        for r in merged:
            g[str(key_func(r))].append(r)
        show_table(title, g)

    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        fields = list(dict.fromkeys(k for r in merged for k in r))
        with args.output.open('w', encoding='utf-8-sig', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=fields, extrasaction='ignore')
            writer.writeheader()
            writer.writerows(merged)
        print('\nDetailed QA CSV:', args.output)


if __name__ == '__main__':
    main()

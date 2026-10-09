#!/usr/bin/env python3
"""READ-ONLY evaluation of Qingyue approved-plan month preflight probes.
Never contacts Huawei, updates SQLite, changes plan, or marks stations missing.
"""
import argparse
import collections
import pathlib
import sqlite3

EXPECTED_PLAN_SHA256 = 'a8e0c166f199971f63bdb209d4e5c13bbf5dae21eec9ebd45447ad6c453c6587'
DEFAULT_DB=pathlib.Path('D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical/state/qingyue_historical.sqlite')
DEFAULT_PREFLIGHT=pathlib.Path('D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical/plan/qingyue_preflight_2023_2025.txt')
DEFAULT_OUTPUT=pathlib.Path('D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical/plan')


def assess(db_path, preflight_path, outdir, min_ok=3):
    if not db_path.exists() or not preflight_path.exists():
        raise FileNotFoundError('Missing SQLite ledger or preflight list: check --db and --preflight')
    ids=[line.strip() for line in preflight_path.read_text(encoding='utf8').splitlines() if line.strip() and not line.lstrip().startswith('#')]
    if len(ids)!=len(set(ids)): raise ValueError('Duplicate preflight request IDs')
    if not ids: raise ValueError('Empty preflight list')
    uri=db_path.resolve().as_uri() + '?mode=ro'
    conn=sqlite3.connect(uri,uri=True)
    conn.row_factory=sqlite3.Row
    try:
        meta=conn.execute("SELECT value FROM meta WHERE key='plan_sha256'").fetchone()
        if not meta or meta['value']!=EXPECTED_PLAN_SHA256:
            raise ValueError('SQLite ledger plan hash differs from the approved original. Stop.')
        found={}
        for start in range(0,len(ids),800):
            subset=ids[start:start+800]
            placeholders=','.join('?' for _ in subset)
            sql=('SELECT request_id, study_site_code, target_datetime, plan_component, status FROM requests '
                 f'WHERE request_id IN ({placeholders})')
            for row in conn.execute(sql,subset): found[row['request_id']]=row
        if len(found)!=len(ids):
            raise ValueError(f'{len(ids)-len(found)} preflight IDs absent from SQLite plan')
        months=collections.defaultdict(list)
        for rid in ids:
            row=found[rid]
            if row['plan_component']!='monthly_noon_backbone':
                raise ValueError('Preflight contains a non-noon-backbone ID')
            if not row['target_datetime'].endswith('12:00:00'):
                raise ValueError('Preflight contains a non-noon timestamp')
            months[row['target_datetime'][:7]].append(row)
        results=[]; accepted_months=set()
        for month,rs in sorted(months.items()):
            sites=[r['study_site_code'] for r in rs]
            if len(rs)!=10 or len(set(sites))!=10:
                raise ValueError(f'Month {month} must have 10 distinct-site sentinels, found {sites}')
            cnt=collections.Counter(r['status'] for r in rs)
            ok=cnt['ok']; miss=cnt['no_record']
            other=len(rs)-ok-miss
            if other>0: verdict='INCOMPLETE'
            elif ok==0: verdict='HOLD_BLACKOUT'
            elif ok<min_ok: verdict='HOLD_LOW_COVERAGE'
            else:
                verdict='PASS'; accepted_months.add(month)
            results.append((month,ok,miss,other,verdict))
        # Only produce executor-eligible list of original approved monthly request IDs
        # for months that passed full cross-site sentinel testing.
        whitelist=[]
        if accepted_months:
            for row in conn.execute("""SELECT request_id, target_datetime FROM requests
                WHERE plan_component='monthly_noon_backbone'
                  AND substr(target_datetime,1,4) BETWEEN '2023' AND '2025'
                ORDER BY plan_order"""):
                if row['target_datetime'][:7] in accepted_months:
                    whitelist.append(row['request_id'])
        outdir.mkdir(parents=True,exist_ok=True)
        report_path=outdir/'qingyue_month_gate_report.md'
        with report_path.open('w',encoding='utf8') as f:
            f.write('# Qingyue date-wide preflight gate\n\n')
            f.write('This is an offline, read-only evaluation of ten geographically distributed monthly sentinels. '
                    'A failed preflight indicates a suspicious requested timestamp, not proof the entire month lacks data.\n\n')
            f.write(f'Original plan SHA256: `{EXPECTED_PLAN_SHA256}`. Minimum successful study areas to PASS: **{min_ok}**.\n\n')
            f.write('| Target month | Success | No record | Pending/other | Decision |\n|---|---:|---:|---:|---|\n')
            for m,ok,missing,other,v in results:
                f.write(f'| {m} | {ok} | {missing} | {other} | {v} |\n')
            f.write(f'\nPassed months: **{len(accepted_months)}/{len(results)}**. '
                    f'Approved-plan monthly request IDs whitelisted: **{len(whitelist)}**.\n\n')
            f.write('Never run an unrestricted `--stage 1` command until held months have been reviewed. '
                    'The gate does not change the original ledger or assign any no-record status to unqueried stations.\n')
        list_path=outdir/'qingyue_stage1_cleared_months.txt'
        if whitelist:
            with list_path.open('w',encoding='utf8') as f:
                f.write('# APPROVED-plan requests; only months passing the preflight gate\n')
                for rid in whitelist:f.write(rid+'\n')
        elif list_path.exists():
            # Prevent an old allowlist from being mistaken for the current report.
            list_path.unlink()
        for month,ok,miss,other,verdict in results:
            print(f'{month}: OK={ok:2}  no_record={miss:2}  pending={other:2}  {verdict}')
        print(f'SUMMARY: {len(accepted_months)}/{len(results)} months passed; {len(whitelist)} plan IDs eligible.')
        print(f'Report: {report_path}')
        if whitelist: print(f'Approved-plan request list: {list_path}')
        else: print('NO allowlist generated because no month has yet passed.')
        print('ZERO API calls; SQLite opened read-only; original ledger unchanged.')
        return results, whitelist
    finally:
        conn.close()


def main():
    p=argparse.ArgumentParser()
    p.add_argument('--db',type=pathlib.Path,default=DEFAULT_DB)
    p.add_argument('--preflight',type=pathlib.Path,default=DEFAULT_PREFLIGHT)
    p.add_argument('--outdir',type=pathlib.Path,default=DEFAULT_OUTPUT)
    p.add_argument('--min-ok',type=int,default=3)
    a=p.parse_args()
    if not 1<=a.min_ok<=10:p.error('--min-ok must be 1..10')
    assess(a.db,a.preflight,a.outdir,a.min_ok)

if __name__=='__main__':main()

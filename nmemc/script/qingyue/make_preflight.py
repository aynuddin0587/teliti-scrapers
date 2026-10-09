#!/usr/bin/env python3
"""Generate a request-ID-only monthly preflight subset from an APPROVED plan.
Does not perform API calls or modify the existing plan or SQLite ledger.
"""
import argparse
import collections
import csv
import datetime
import hashlib
import pathlib
import re

EXPECTED_HASH = 'a8e0c166f199971f63bdb209d4e5c13bbf5dae21eec9ebd45447ad6c453c6587'
DEFAULT_PLAN = pathlib.Path('D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical/plan/qingyue_request_plan_recommended.csv')
DEFAULT_LOG = pathlib.Path('D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical/log/qingyue_historical_20261008.log')
LOG_PATTERN = re.compile(r'\| (OK|NO_RECORD) order=(\d+) attempt=(\d+) http=(\d+) station=(.*?) time=(\d{4}-\d\d-\d\d \d\d:\d\d:\d\d) site=([A-Z]+)')


def make_plan(plan_path, log_path, dest, start='2023-01', end='2025-12'):
    actual_hash = hashlib.sha256(plan_path.read_bytes()).hexdigest()
    if actual_hash != EXPECTED_HASH:
        raise ValueError(f'Approved plan checksum mismatch: {actual_hash}. Refusing to generate IDs.')
    with plan_path.open(encoding='utf-8-sig', newline='') as f:
        plan = list(csv.DictReader(f))
    noon = [r for r in plan if r['plan_component']=='monthly_noon_backbone']
    by_order = {int(r['plan_order']):r for r in noon}
    event_by_order = {}
    if log_path.exists():
        for line in log_path.read_text(encoding='utf-8').splitlines():
            m=LOG_PATTERN.search(line)
            if m:
                st,order,attempt,http,name,time,site=m.groups()
                event_by_order[int(order)]=st
    base = collections.defaultdict(list)
    station = {}
    for r in noon:
        sid=r['station_identity_id']; site=r['study_site_code']
        station[(site,sid)]=r
        if '2021-01' <= r['target_datetime'][:7] <= '2022-12' and r['target_datetime'][:10]!='2022-08-15':
            status=event_by_order.get(int(r['plan_order']))
            if status:
                base[(site,sid)].append(status)
    sites=sorted({r['study_site_code'] for r in noon})
    chosen={}
    for site in sites:
        candidates=[]
        for (s,sid),r in station.items():
            if s!=site: continue
            valid=base[(s,sid)]
            rate=valid.count('OK')/len(valid) if valid else 0
            lon=float(r['longitude']); lat=float(r['latitude'])
            # Prefer nearshore/eastern representatives in broad drainage-area groups.
            if site=='LZB' and not (118<=lon<=120 and 37<=lat<=38.5 and r['qingyue_province']=='\u5c71\u4e1c\u7701'):
                continue
            if site=='BHB' and not (117<=lon<=120 and 37<=lat<=40):
                continue
            candidates.append((-rate,-len(valid),-valid.count('OK'),-lon,sid,r))
        if not candidates: raise ValueError(f'No candidate sentinel for site {site}')
        candidates.sort(key=lambda x:x[:5])
        chosen[site]=candidates[0][-1]['station_identity_id']
    month_range=[]
    yr,mo=map(int,start.split('-'))
    while f'{yr:04d}-{mo:02d}'<=end:
        month_range.append(f'{yr:04d}-{mo:02d}')
        mo+=1
        if mo>12:mo=1;yr+=1
    look={(r['study_site_code'],r['station_identity_id'],r['target_datetime'][:7]):r for r in noon}
    selected=[]
    for month in month_range:
        for site in sites:
            row=look.get((site,chosen[site],month))
            if row is None: raise ValueError(f'Missing original-plan noon request site={site}, month={month}')
            selected.append(row)
    assert len(selected)==len(month_range)*len(sites)
    dest.parent.mkdir(parents=True,exist_ok=True)
    with dest.open('w',encoding='utf8') as f:
        f.write('# Existing approved-plan request IDs only; zero new plan requests\n')
        f.write(f'# Plan SHA256: {actual_hash}\n')
        f.write(f'# Months: {start} to {end}; one selected station per {len(sites)} study sites\n')
        for row in selected: f.write(row['request_id']+'\n')
    station_md = dest.with_name('preflight_station_inventory.md')
    with station_md.open('w',encoding='utf8') as f:
        f.write('# Historical-date preflight representatives\n\n')
        f.write(f'Plan SHA256: `{actual_hash}`. No original request IDs have been changed.\n\n')
        f.write('| Study area | Station | Successful non-blackout historical dates | Province | Longitude | Latitude |\n|---|---|---:|---|---:|---:|\n')
        for site in sites:
            r=station[(site,chosen[site])]
            observed=base[(site,chosen[site])]
            f.write(f'| {site} | {r["qingyue_station_name"]} | {observed.count("OK")}/{len(observed)} | {r["qingyue_province"]} | {r["longitude"]} | {r["latitude"]} |\n')
        f.write(f'\nPreflight comprises {len(selected)} unique approved-plan request IDs in {len(month_range)} calendar months.\n')
    print(f'PASS: {len(selected)} unique preflight IDs over {len(month_range)} months; SHA256 unchanged')
    print(f'Wrote {dest} and {station_md}')
    return len(selected)


def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--plan',type=pathlib.Path,default=DEFAULT_PLAN)
    ap.add_argument('--log',type=pathlib.Path,default=DEFAULT_LOG)
    ap.add_argument('--out',type=pathlib.Path,default=pathlib.Path('qingyue_preflight_2023_2025.txt'))
    args=ap.parse_args()
    make_plan(args.plan,args.log,args.out)

if __name__=='__main__':main()

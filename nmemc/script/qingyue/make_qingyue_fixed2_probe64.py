"""Create a conservative Qingyue historical API feasibility probe.

Inputs: previously exported approved-plan manifest CSV.GZ and the IDs from the
completed 25-request deferred-2020 preflight. Read-only; makes NO API calls.

Output A: a 64-row DESIGN CANDIDATE file. Most rows are deliberately NOT
executable because dates are absent from the approved original plan.
Output B: a 12-ID CSV safe for status/run with the existing original plan.
"""
import argparse
import csv
import datetime as dt
import gzip
import random
from pathlib import Path

# Hold station choice constant throughout the test.
STATIONS = [
    ("QYREC_585531e0150e", "most_complete", "高速公路西桥", "BBG"),
    ("QYREC_343e50d2d1a0", "least_nonzero_complete", "沙阳", "BHB"),
]
SEED = 20201015
# Six original-plan 2020 noon months; two earlier 2020 boundary months;
# eight earlier year-months, chosen to sample backward to 2015.
MONTHS = [
    (2020, 3), (2020, 4),
    (2020, 5), (2020, 6), (2020, 7),
    (2020, 8), (2020, 9), (2020, 10),
    (2019, 1), (2019, 4), (2019, 7), (2019, 10),
    (2018, 1), (2018, 7), (2017, 7), (2015, 7),
]


def read_manifest(path):
    with gzip.open(path, "rt", newline="", encoding="utf-8-sig") if str(path).endswith('.gz') else open(path, "r", newline="", encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))


def read_ids(path):
    with open(path, "r", newline="", encoding="utf-8-sig") as f:
        return {x['request_id'] for x in csv.DictReader(f)}


def generate(manifest_path, preflight_path, outdir):
    manifest = read_manifest(manifest_path)
    previous_preflight = read_ids(preflight_path)
    ids = {r['request_id'] for r in manifest}
    if len(ids) != 39626 or len(manifest) != 39626:
        raise RuntimeError('Unexpected original manifest size or duplicate IDs; stopping')
    original = {(r['station_identity_id'], r['target_datetime']): r
                for r in manifest if r['plan_component'] == 'monthly_noon_backbone'}
    rng = random.Random(SEED)
    rows = []
    for y, m in MONTHS:
        # 2020 April 30 is a deliberate boundary check against the provider's
        # station metadata, otherwise use the normal monthly-noon day 15.
        structured_day = 30 if (y, m) == (2020, 4) else 15
        # Random day within month, always at noon. April 2020 is restricted to
        # day 18-29: after the earlier of the two metadata first dates (Apr 17).
        eligible_days = (range(18, 30) if (y, m) == (2020, 4) else range(1, 29))
        eligible_days = [day for day in eligible_days if day != structured_day]
        random_day = rng.choice(eligible_days)
        for probe_type, day in [('structured', structured_day), ('random', random_day)]:
            target = f'{y:04d}-{m:02d}-{day:02d} 12:00:00'
            for station_id, completeness, name, site in STATIONS:
                r = original.get((station_id, target))
                approved = bool(r)
                if approved and r['request_id'] in previous_preflight:
                    raise RuntimeError('A previously run preflight request was selected; stopping')
                if approved and (r['status'] != 'pending' or r['attempts'] != '0'):
                    raise RuntimeError('Approved-plan candidate already attempted; stopping: ' + r['request_id'])
                if approved and (y, m) not in {(2020, i) for i in range(5, 11)}:
                    raise RuntimeError('Unexpected approval for a new-date probe; review plan')
                if approved and probe_type != 'structured':
                    raise RuntimeError('Random probe unexpectedly in original plan; review')
                if approved:
                    gate = 'A_original_plan_12'
                elif (y, m) in [(2020, 4), (2019, 10)]:
                    gate = 'B_boundary_8_new_plan'
                elif y == 2020 and m in range(5, 11):
                    gate = 'C_random_2020_12_new_plan'
                else:
                    gate = 'D_remaining_32_new_plan'
                rows.append({
                    'gate': gate,
                    'sampling': probe_type,
                    'year_month': f'{y:04d}-{m:02d}',
                    'station_role': completeness,
                    'station_identity_id': station_id,
                    'staname_param': name,
                    'study_site_code': site,
                    'target_datetime': target,
                    'original_plan_request_id': r['request_id'] if approved else '',
                    'original_plan_approved': 'yes' if approved else 'no',
                    'original_plan_status_at_export': r['status'] if approved else 'outside_original_plan',
                    'required_action': 'existing_request_id_ready_for_status' if approved else 'prepare_separate_approved_request_plan',
                })
    order = {'A_original_plan_12': 0, 'B_boundary_8_new_plan': 1,
             'C_random_2020_12_new_plan': 2, 'D_remaining_32_new_plan': 3}
    rows.sort(key=lambda r:(order[r['gate']], r['year_month'],
                            r['target_datetime'], r['station_role']))
    for i, row in enumerate(rows, 1):
        row['probe_row'] = i
    assert len(rows) == 64
    assert len({(r['station_identity_id'], r['target_datetime']) for r in rows}) == 64
    assert sum(r['sampling'] == 'structured' for r in rows) == 32
    assert sum(r['sampling'] == 'random' for r in rows) == 32
    assert sum(r['original_plan_approved'] == 'yes' for r in rows) == 12
    assert {r['station_identity_id'] for r in rows} == {s[0] for s in STATIONS}
    assert {r['gate'] for r in rows} == set(order)
    gates = {k:sum(r['gate']==k for r in rows) for k in order}
    assert list(gates.values()) == [12,8,12,32], gates
    assert all(r['original_plan_request_id'] not in previous_preflight for r in rows)

    outdir.mkdir(parents=True, exist_ok=True)
    designfile = outdir / 'qingyue_fixed2_2020_older_probe64_DESIGN_ONLY.csv'
    with designfile.open('w', newline='', encoding='utf-8-sig') as f:
        writer = csv.DictWriter(f, fieldnames=['probe_row'] + [x for x in rows[0] if x != 'probe_row'])
        writer.writeheader(); writer.writerows(rows)
    requestfile = outdir / 'qingyue_deferred2020_fixed2_noon12_REQUEST_LIST.csv'
    with requestfile.open('w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=['request_id'])
        writer.writeheader()
        writer.writerows({'request_id':r['original_plan_request_id']} for r in rows if r['original_plan_approved']=='yes')
    print('Design:',designfile)
    print('Ready subset:',requestfile)
    print('Gate counts:', gates)
    print('Approved request count:',sum(r['original_plan_approved']=='yes' for r in rows))
    print('Remaining required new-plan rows:',sum(r['original_plan_approved']=='no' for r in rows))
    print('Reproducible seed:',SEED)


if __name__ == '__main__':
    p=argparse.ArgumentParser()
    p.add_argument('--manifest',default='/mnt/data/qingyue_historical_manifest.csv.gz')
    p.add_argument('--previous-preflight',default='/mnt/data/qingyue_deferred2020_preflight25.csv')
    p.add_argument('--output-dir',default='/mnt/data')
    a=p.parse_args()
    generate(Path(a.manifest),Path(a.previous_preflight),Path(a.output_dir))

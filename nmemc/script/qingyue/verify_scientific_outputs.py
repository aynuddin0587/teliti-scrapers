#!/usr/bin/env python3
"""No external calls; check derived data, documentation and publication figures."""
import csv, json, os, subprocess, sys, zipfile
from pathlib import Path
import pandas as pd
import xml.etree.ElementTree as ET
from PIL import Image
B=Path(__file__).resolve().parent
D=B/'output'
h=pd.read_csv(D/'phaseC_hourly_qc.csv')
d=pd.read_csv(D/'phaseC_station_days.csv')
p=pd.read_csv(D/'phaseC_valid_january_july_pairs.csv')
issues=pd.read_csv(D/'phaseC_extreme_profiles_for_review.csv')
assert len(h)==600 and h.duplicated(['station_identity_id','target_datetime']).sum()==0
assert len(d)==100 and d.strict_sixpoint.sum()==80 and len(p)==34
assert h[h.hour!=12].shape[0]==500
assert h[h.hour!=12].qc_reason.value_counts().to_dict()=={'STRICT_PASS':469,'NON_NORMAL_STATUS':20,'NO_RECORD':11}
assert p.station_identity_id.nunique()==29
assert len(issues)==9 and issues.flag_ph_range_gt2.sum()==1
ids={s.strip() for s in (Path('/mnt/data/qingyue_stage4_phaseC/output/phaseC_approved_all_500.txt').read_text(encoding='utf-8-sig')).splitlines() if s.strip() and not s.startswith('#')}
assert len(ids)==500 and ids==set(h.loc[h.hour!=12,'manifest_request_id'])
for image in D.glob('figure_*.png'):
    with Image.open(image) as im:
        assert im.width>1500 and im.height>900
for img in D.glob('figure_*.svg'):
    ET.parse(img)
r=(B/'phaseC_diurnal_representativeness.R').read_text(encoding='utf-8')
# Lightweight curly-brace balance sanity check, NOT a substitute for R parser.
assert r.count('{')==r.count('}') and 'quit(' not in r
report=(B/'PHASEC_DIURNAL_SCIENTIFIC_REPORT.md').read_text(encoding='utf-8')
assert '34/50' in report and '80/100' in report and '469' in report
print('PASS: 500 exact plan-approved Phase C ID matches; 600 design rows; QC, summaries, figures and report verified.')
print('R execution unavailable in this environment (no Rscript); Python scientific analysis executed and reconciliation verified.')

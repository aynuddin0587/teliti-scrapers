#!/usr/bin/env python3
"""Make two standalone exploratory pilot charts (uses pandas, matplotlib)."""
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
root=Path(__file__).resolve().parent/'output'
df=pd.read_csv(root/'pilot_noon_vs_sixpoint.csv')
g=df[df['strict_sixpoint'].astype(str).str.lower().eq('true')].copy()
fig,ax=plt.subplots(figsize=(7.5,5.2))
for mo,lab in [(1,'January'),(7,'July')]:
 part=g[g.month==mo]
 ax.scatter(part.six_point_DO_mean,part.noon_DO,s=52,label=f'{lab} (n={len(part)})',alpha=.8)
mn=min(g.noon_DO.min(),g.six_point_DO_mean.min())-.8
mx=max(g.noon_DO.max(),g.six_point_DO_mean.max())+.8
ax.plot([mn,mx],[mn,mx],linestyle='--',linewidth=1,label='Exact agreement')
ax.set_xlim(mn,mx);ax.set_ylim(mn,mx)
ax.set_xlabel('Six-point mean dissolved oxygen (source units)')
ax.set_ylabel('Noon dissolved oxygen (source units)')
ax.set_title('Noon versus six-point mean dissolved oxygen')
ax.grid(alpha=.2);ax.legend(frameon=False)
fig.tight_layout();fig.savefig(root/'pilot_noon_vs_sixpoint_scatter.png',dpi=180);plt.close(fig)
fig,ax=plt.subplots(figsize=(7.5,5.2))
for mo,lab,j in [(1,'January',1),(7,'July',2)]:
 part=g[g.month==mo]
 x=np.full(len(part),j)+np.linspace(-.14,.14,len(part))
 ax.scatter(x,part.six_point_DO_range,s=42,alpha=.85,label=f'{lab} (n={len(part)})')
 ax.plot([j-.28,j+.28],[part.six_point_DO_range.median()]*2,linewidth=2)
ax.set_xlim(.5,2.5);ax.set_xticks([1,2],['January','July'])
ax.set_ylabel('Six-point dissolved-oxygen range (source units)')
ax.set_title('Within-day dissolved-oxygen variation in the pilot')
ax.grid(axis='y',alpha=.2)
ax.text(2.06,13.562,'SMB 2022',fontsize=9,va='center')
ax.text(2.06,10.274,'JZB 2025',fontsize=9,va='center')
ax.text(2.06,5.523,'LZB 2024',fontsize=9,va='center')
fig.tight_layout();fig.savefig(root/'pilot_january_july_DO_ranges.png',dpi=180);plt.close(fig)
print('Two figures written:', root)

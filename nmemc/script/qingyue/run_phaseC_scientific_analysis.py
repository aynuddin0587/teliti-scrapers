#!/usr/bin/env python3
"""Reproducible Phase C diurnal analysis (read-only, zero API calls).
Uses the immutable Phase C selection roster, raw observations and manifest.
Kept separately from the independent R translation included in this package.
"""
from __future__ import annotations
import argparse, hashlib, json, math, warnings
from pathlib import Path
import numpy as np
import pandas as pd
from scipy import stats
import statsmodels.formula.api as smf
from scipy.stats import chi2
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

HOURS=[0,4,8,12,16,20]
CORE=['water_temp','ph','dissolvedoxygen','codmn','nh3-n','tp','tn']
SEED=20261012
B=4000
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':10,'axes.spines.top':False,'axes.spines.right':False,'savefig.dpi':300})


def sha(p):
    h=hashlib.sha256()
    with open(p,'rb') as f:
        for b in iter(lambda: f.read(2**20), b''): h.update(b)
    return h.hexdigest()


def num(series):
    return pd.to_numeric(series,errors='coerce').mask(lambda x: x<0)


def mean_or_nan(x):
    return float(np.mean(x)) if len(x) else float('nan')


def med_or_nan(x):
    x=np.asarray(x,dtype=float); x=x[np.isfinite(x)]
    return float(np.median(x)) if len(x) else float('nan')


def bootstrap_median_ci(df, val, group='station_identity_id', b=B, seed=SEED):
    work=df[[group,val]].dropna(); rng=np.random.default_rng(seed)
    clusters=[g[val].to_numpy() for _,g in work.groupby(group)]
    n=len(clusters)
    if n < 3: return (np.nan,np.nan,n)
    out=np.empty(b)
    for k in range(b):
        draw=rng.integers(0,n,n)
        out[k]=np.median(np.concatenate([clusters[j] for j in draw]))
    return (*map(float,np.percentile(out,[2.5,97.5])),n)


def cluster_station_season_test(pairs,val,seed=SEED,b=20000):
    """Unit of resampling and random sign reversal is unique monitoring station, NOT station-year."""
    t=pairs[['station_identity_id',val]].dropna().groupby('station_identity_id')[val].mean()
    arr=t.to_numpy(dtype=float); n=len(arr)
    if n<3: return dict(unique_stations=n,mean_station_delta=np.nan,ci_low=np.nan,ci_high=np.nan,signflip_p=np.nan)
    rng=np.random.default_rng(seed)
    boots=np.mean(arr[rng.integers(n,size=(5000,n))],axis=1)
    reps=np.mean(arr[None,:] * rng.choice([-1,1],size=(b,n)),axis=1)
    p=(1+int(np.sum(np.abs(reps)>=abs(np.mean(arr)))))/(b+1)
    lo,hi=np.percentile(boots,[2.5,97.5])
    return dict(unique_stations=n,mean_station_delta=float(arr.mean()),ci_low=float(lo),ci_high=float(hi),signflip_p=float(p))


def cluster_regression(days, outcome):
    # Cluster robust SE handles repeated station-years. Group & season are associative, not causal.
    d=days.loc[days.strict_sixpoint].copy()
    d['log_y']=np.log1p(d[outcome]); d['season']=pd.Categorical(d.season,categories=['January','July'])
    try:
        fit=smf.ols('log_y ~ C(season) + C(site) + C(year)',data=d).fit(cov_type='cluster',cov_kwds={'groups':d.station_identity_id,'use_correction':True})
        coeffs=fit.params; cov=fit.cov_params(); term='C(season)[T.July]'
        low,high=fit.conf_int().loc[term]
        keys=[x for x in coeffs.index if x.startswith('C(site)')]
        sub=coeffs.loc[keys].to_numpy(); subcov=cov.loc[keys,keys].to_numpy()
        W=float(sub @ np.linalg.pinv(subcov) @ sub); pp=float(chi2.sf(W,len(keys)))
        return dict(outcome=outcome,n_days=len(d),n_station_ids=d.station_identity_id.nunique(),
                    july_log_coefficient=float(coeffs[term]),july_ci_low=float(low),july_ci_high=float(high),
                    july_p=float(fit.pvalues[term]),site_joint_wald=float(W),site_joint_df=len(keys),site_joint_p=pp,
                    description='OLS log(1+y) with site + season + year fixed terms; cluster-robust covariance by station identity')
    except Exception as e:
        return dict(outcome=outcome,error=str(e))


def save_fig(fig, path):
    fig.savefig(path.with_suffix('.png'),dpi=320,bbox_inches='tight')
    fig.savefig(path.with_suffix('.svg'),bbox_inches='tight')
    plt.close(fig)


def plot_all(days,pairs,output):
    x=days[days.strict_sixpoint].copy(); paired=pairs.copy()
    sites=['BHB','LZB','MJE','XMB']
    colors=plt.rcParams['axes.prop_cycle'].by_key()['color']
    colors={site: colors[i] for i,site in enumerate(sites)}
    # Fig 1: noon vs six-sample mean, geometry indicates measurement disagreement.
    fig,ax=plt.subplots(figsize=(6.4,5.5))
    for site in sites:
        sub=x[x.site==site]
        ax.scatter(sub.do_six_mean,sub.do_noon,s=35,alpha=.77,label=f'{site} (n={len(sub)})',color=colors[site],zorder=3)
    maxval=max(x[['do_six_mean','do_noon']].max())*1.05
    ax.plot([0,maxval],[0,maxval],linestyle='--',color='.35',lw=1.1,label='Identity (noon = six-point mean)')
    ax.set(xlabel='Six-point mean dissolved oxygen (source units)',ylabel='Noon dissolved oxygen (source units)',
           title='Noon observation versus six-point daily mean',xlim=(0,maxval),ylim=(0,maxval))
    ax.legend(fontsize=8,loc='upper left',frameon=False)
    ax.text(.98,.02,f'n={len(x)} valid station-days',ha='right',va='bottom',transform=ax.transAxes,fontsize=9)
    ax.set_aspect('equal','box'); fig.tight_layout()
    save_fig(fig,output/'figure_01_noon_vs_sixpoint_mean')
    # Fig 2: Paired Jan vs Jul DO excursions; connecting lines denote station-year pairs.
    fig,ax=plt.subplots(figsize=(6.4,5.1))
    for _,r in paired.iterrows():
        ax.plot([0,1],[r.jan_do_range,r.jul_do_range],color=colors[r.site],alpha=.48,lw=1.0)
        ax.scatter([0,1],[r.jan_do_range,r.jul_do_range],color=colors[r.site],alpha=.66,s=15)
    med=[paired.jan_do_range.median(),paired.jul_do_range.median()]
    ax.scatter([0,1],med,marker='D',s=110,facecolors='none',edgecolors='black',linewidth=1.7,label='Paired-sample median',zorder=6)
    ax.set_xticks([0,1],['January','July'])
    ax.set(ylabel='Six-point DO range (source units)',title='Matched January–July sampling-day comparison',xlim=(-.15,1.15))
    ax.legend(frameon=False,loc='upper left')
    from matplotlib.lines import Line2D
    leg=[Line2D([0],[0],color=colors[si],lw=2,label=si) for si in sites]
    ax.legend(handles=leg,loc='upper right',ncol=2,frameon=False,fontsize=8)
    fig.tight_layout(); save_fig(fig,output/'figure_02_paired_january_july_DO_range')
    # Fig 3: per-site noon-mean abs differences by month.
    fig,ax=plt.subplots(figsize=(7.5,5.0)); rng=np.random.default_rng(SEED)
    for j,site in enumerate(sites):
        for shift,season,marker in [(-.16,'January','o'),(.16,'July','^')]:
            sub=x[(x.site==site)&(x.season==season)]
            pos=j+shift+rng.uniform(-.07,.07,len(sub))
            ax.scatter(pos,sub.abs_noon_mean_diff,facecolors=colors[site] if season=='July' else 'white',edgecolors=colors[site],marker=marker,s=36,alpha=.87)
            if len(sub):
                ax.plot([j+shift-.1,j+shift+.1],[sub.abs_noon_mean_diff.median()]*2,color='black',lw=2.0)
    ax.set_xticks(range(len(sites)),sites)
    ax.set(xlabel='Provisional study-area group',ylabel='Absolute noon–six-point mean DO discrepancy (source units)',
           title='Noon representativeness varies within and among groups')
    ax.text(.99,.99,'Open circles: January  |  Filled triangles: July\nBlack ticks: group-season medians',ha='right',va='top',transform=ax.transAxes,fontsize=8.5)
    fig.tight_layout(); save_fig(fig,output/'figure_03_site_season_absolute_disagreement')
    # Fig 4: Extreme DO vs pH at a preidentified station-day, observational (not causally validated).
    suspect=x[(x.station=='生产圈闸')&(x.date=='2021-07-15')]
    if len(suspect):
        hs=pd.read_csv(output/'phaseC_hourly_qc.csv')
        t=hs[(hs.station=='生产圈闸')&(hs.date=='2021-07-15')].sort_values('hour')
        fig,ax=plt.subplots(figsize=(6.3,4.3))
        ax.plot(t.hour,t.dissolvedoxygen,marker='o',lw=2.0,label='Dissolved oxygen')
        ax.set_xticks(HOURS)
        ax.set(xlabel='Sampling hour (local time)',ylabel='Reported dissolved oxygen (source units)',
               title='Extreme within-day profile: BHB, 15 July 2021')
        ax.text(.02,.97,'Independent sensor/source validation required',transform=ax.transAxes,va='top',fontsize=9)
        fig.tight_layout(); save_fig(fig,output/'figure_04_extreme_DO_profile')


def main():
    pa=argparse.ArgumentParser()
    pa.add_argument('--observations',default='/mnt/data/qingyue_historical_observations.csv.gz')
    pa.add_argument('--manifest',default='/mnt/data/qingyue_historical_manifest.csv.gz')
    pa.add_argument('--roster',default='/mnt/data/qingyue_stage4_phaseC/output/phaseC_selected_station_years.csv')
    pa.add_argument('--followup-day-table',default='/mnt/data/phaseC_followup_day_results.csv')
    pa.add_argument('--followup-issues',default='/mnt/data/phaseC_followup_issues.csv')
    pa.add_argument('--out',default='/mnt/data/qingyue_phaseC_science/output')
    a=pa.parse_args(); output=Path(a.out); output.mkdir(parents=True,exist_ok=True)
    obs=pd.read_csv(a.observations,low_memory=False,dtype={'station_identity_id':'string'})
    manifest=pd.read_csv(a.manifest,low_memory=False,dtype={'station_identity_id':'string'})
    roster=pd.read_csv(a.roster,encoding='utf-8-sig',dtype={'station_identity_id':'string'})
    follow=pd.read_csv(a.followup_day_table,dtype={'station_identity_id':'string'})
    issues=pd.read_csv(a.followup_issues)
    assert len(roster)==50 and roster.station_identity_id.nunique()==37
    assert len(obs.request_id)==len(set(obs.request_id))
    assert (manifest.status=='ok').sum()==len(obs)
    assert set(obs.request_id)==set(manifest.loc[manifest.status=='ok','request_id'])
    assert manifest.request_id.nunique()==39626
    expected=[]
    for row in roster.itertuples(index=False):
        for m in (1,7):
            date=f'{row.year:04d}-{m:02d}-15'
            for h in HOURS:
                expected.append(dict(phase=row.phase,site=row.study_site_code,
                                     station_identity_id=row.station_identity_id,station=row.station_name,
                                     year=int(row.year),month=m,season='January' if m==1 else 'July',
                                     date=date,hour=h,target_datetime=f'{date} {h:02d}:00:00'))
    design=pd.DataFrame(expected)
    assert len(design)==600
    obs2=obs.copy()
    obs2['target_datetime']=obs2.target_datetime.astype(str)
    assert not obs2.duplicated(['station_identity_id','target_datetime']).any()
    mkey=manifest[['station_identity_id','target_datetime','status','request_id']].rename(columns={'status':'api_status','request_id':'manifest_request_id'})
    t=design.merge(mkey,on=['station_identity_id','target_datetime'],how='left',validate='one_to_one')
    assert t.api_status.notna().all() and t.manifest_request_id.notna().all()
    assert (t.api_status.isin(['ok','no_record'])).all()
    ocols=['station_identity_id','target_datetime','request_id','section_status','monitor_time']+CORE
    dat=obs2[ocols].copy().rename(columns={'request_id':'observed_request_id'})
    t=t.merge(dat,on=['station_identity_id','target_datetime'],how='left',validate='one_to_one',indicator=True)
    assert ((t.api_status=='ok')==(t._merge=='both')).all()
    assert (t.loc[t.api_status=='ok','manifest_request_id'].astype(str).values==t.loc[t.api_status=='ok','observed_request_id'].astype(str).values).all()
    for c in CORE: t[c]=num(t[c])
    t['normal']=t.section_status.eq('正常')
    t['time_matches']=t.monitor_time.eq(t.target_datetime)
    t['valid_core']=t[CORE].notna().all(axis=1)
    t['strict']=t.api_status.eq('ok') & t.normal & t.time_matches & t.valid_core
    t['qc_reason']=np.select([t.api_status.eq('no_record'),~t.normal,t.api_status.eq('ok')&~t.time_matches,t.api_status.eq('ok')&~t.valid_core],
                            ['NO_RECORD','NON_NORMAL_STATUS','TIMESTAMP_MISMATCH','INCOMPLETE_SEVEN_CORE'],default='STRICT_PASS')
    # Distinct category flags for sensitivity, not exclusion by default.
    t['review_high_do_gt20']=t.dissolvedoxygen.gt(20)
    t['review_ph_above_9_5']=t.ph.gt(9.5)
    t['review_ph_above_10']=t.ph.gt(10)
    t=t.drop(columns='_merge')
    t.to_csv(output/'phaseC_hourly_qc.csv',index=False,encoding='utf-8-sig')
    assert t.site.value_counts().to_dict()=={'XMB':180,'MJE':180,'BHB':120,'LZB':120}
    extras=t[t.hour!=12]
    assert (extras.api_status=='ok').sum()==489
    assert (extras.api_status=='no_record').sum()==11
    assert extras.strict.sum()==469
    assert (t[t.hour==12].strict).all()
    metrics=[]
    for (sid,year,m),g in t.groupby(['station_identity_id','year','month'],sort=False):
        g=g.sort_values('hour'); valid=bool(g.strict.all())
        assert len(g)==6 and list(g.hour)==HOURS
        no=g.loc[g.hour==12].iloc[0]
        extra=g[g.hour!=12]
        rec={k:no[k] for k in ['phase','site','station','station_identity_id','year','season','month','date']}
        rec.update(extra_ok=int((extra.api_status=='ok').sum()),extra_strict=int(extra.strict.sum()),
                   noon_valid=bool(no.strict),strict_sixpoint=valid)
        vars_vals={key:np.nan for key in ['do_noon','do_six_mean','do_min','do_max','do_range','signed_noon_mean_diff','abs_noon_mean_diff',
                                           'noon_minus_sampled_min','temp_range','ph_range','do_min_hour','do_max_hour','DO_ph_pearson']}
        if valid:
            do=g.dissolvedoxygen.to_numpy(float); ph=g.ph.to_numpy(float); wt=g.water_temp.to_numpy(float)
            vars_vals.update(do_noon=float(do[3]),do_six_mean=float(do.mean()),do_min=float(do.min()),do_max=float(do.max()),
                             do_range=float(np.ptp(do)),signed_noon_mean_diff=float(do[3]-do.mean()),
                             abs_noon_mean_diff=float(abs(do[3]-do.mean())),noon_minus_sampled_min=float(do[3]-do.min()),
                             temp_range=float(np.ptp(wt)),ph_range=float(np.ptp(ph)),do_min_hour=int(g.hour.iloc[np.argmin(do)]),
                             do_max_hour=int(g.hour.iloc[np.argmax(do)]),
                             DO_ph_pearson=float(np.corrcoef(do,ph)[0,1]) if np.std(ph)>0 and np.std(do)>0 else np.nan)
        rec.update(vars_vals)
        rec['flag_large_do_range_gt5']=bool(valid and rec['do_range']>5)
        rec['flag_high_do_gt20']=bool(valid and rec['do_max']>20)
        rec['flag_temp_range_gt10']=bool(valid and rec['temp_range']>10)
        rec['flag_ph_range_gt2']=bool(valid and rec['ph_range']>2)
        metrics.append(rec)
    days=pd.DataFrame(metrics)
    days.to_csv(output/'phaseC_station_days.csv',index=False,encoding='utf-8-sig')
    assert len(days)==100 and days.strict_sixpoint.sum()==80
    # Crosscheck published day summary, incl all 80 followup station-days.
    fu=days[days.phase=='followup_400'].copy()
    cmp=fu.merge(follow,how='outer',left_on=['station_identity_id','date'],right_on=['station_identity_id','date'],suffixes=('_recomputed','_provided'),indicator=True,validate='one_to_one')
    assert (cmp._merge=='both').all() and len(cmp)==80
    assert (cmp.strict_sixpoint_recomputed==cmp.strict_sixpoint_provided).all()
    assert (cmp.extra_ok_recomputed==cmp.extra_ok_provided).all()
    assert (cmp.extra_strict_recomputed==cmp.extra_strict_provided).all()
    assert cmp.strict_sixpoint_recomputed.sum()==67
    dfc=cmp[cmp.strict_sixpoint_recomputed]
    for col1,col2 in [('do_range','DO_range'),('signed_noon_mean_diff','DO_noon_minus_sixmean'),('noon_minus_sampled_min','DO_noon_minus_sampled_min')]:
        a1=dfc[col1].to_numpy(float); b1=dfc[col2].to_numpy(float)
        assert np.allclose(a1,b1,rtol=1e-9,atol=1e-8), f'input verification mismatch {col1}'
    # QA: compare reported issues by exact IDs to recomputed strict assessment
    extra_issues=extras[~extras.strict]
    assert len(extra_issues)==31 and len(issues)==21
    assert set(issues.request_id).issubset(set(extra_issues.manifest_request_id))
    assert len(extra_issues[extra_issues.phase=='followup_400'])==21
    dayjan=days[(days.month==1)&days.strict_sixpoint]
    dayjul=days[(days.month==7)&days.strict_sixpoint]
    j=days[(days.month==1)&days.strict_sixpoint].drop(columns=['month','season','date','phase','site','station','extra_ok','extra_strict','noon_valid','strict_sixpoint']).rename(columns=lambda x: 'jan_'+x if x not in ['station_identity_id','year'] else x)
    u=days[(days.month==7)&days.strict_sixpoint].drop(columns=['month','season','date','phase','site','station','extra_ok','extra_strict','noon_valid','strict_sixpoint']).rename(columns=lambda x: 'jul_'+x if x not in ['station_identity_id','year'] else x)
    paired=j.merge(u,on=['station_identity_id','year'],validate='one_to_one')
    paired=paired.merge(roster[['station_identity_id','year','station_name','study_site_code']],on=['station_identity_id','year'],validate='one_to_one')
    paired=paired.rename(columns={'station_name':'station','study_site_code':'site'})
    paired['delta_do_range']=paired.jul_do_range-paired.jan_do_range
    paired['delta_abs_noon_mean']=paired.jul_abs_noon_mean_diff-paired.jan_abs_noon_mean_diff
    paired['delta_temp_range']=paired.jul_temp_range-paired.jan_temp_range
    paired['delta_signed_noon_mean']=paired.jul_signed_noon_mean_diff-paired.jan_signed_noon_mean_diff
    paired.to_csv(output/'phaseC_valid_january_july_pairs.csv',index=False,encoding='utf-8-sig')
    assert len(paired)==34
    desc=[]
    for name,su in [('ALL',days[days.strict_sixpoint]),('January',dayjan),('July',dayjul)]:
        desc.append(dict(group=name,n=len(su),n_stations=su.station_identity_id.nunique(),
                         median_do_range=med_or_nan(su.do_range),median_abs_noon_diff=med_or_nan(su.abs_noon_mean_diff),
                         median_signed_noon_diff=med_or_nan(su.signed_noon_mean_diff),
                         median_noon_minus_min=med_or_nan(su.noon_minus_sampled_min),
                         positive_noon_minus_min=int((su.noon_minus_sampled_min>1e-8).sum()),
                         noon_minus_min_gte1=int((su.noon_minus_sampled_min>=1.0).sum()),
                         flag_range_gt5=int(su.flag_large_do_range_gt5.sum()),flag_max_gt20=int(su.flag_high_do_gt20.sum())))
    desc=pd.DataFrame(desc); desc.to_csv(output/'phaseC_summary_by_season.csv',index=False)
    grouped=days.groupby(['site','season'],sort=True).agg(
        planned_days=('strict_sixpoint','size'),valid_days=('strict_sixpoint','sum'),station_ids=('station_identity_id','nunique')).reset_index()
    grouped=grouped.merge(days[days.strict_sixpoint].groupby(['site','season']).agg(
        median_do_range=('do_range','median'),median_abs_noon_diff=('abs_noon_mean_diff','median'),
        median_signed_noon_diff=('signed_noon_mean_diff','median'),
        median_noon_minus_min=('noon_minus_sampled_min','median')).reset_index(),on=['site','season'],how='left')
    grouped.to_csv(output/'phaseC_summary_by_area_season.csv',index=False)
    compared=[]
    for name,sub in [('ALL',paired),*[(site,paired[paired.site==site]) for site in ['BHB','LZB','MJE','XMB']]]:
        comp=dict(site=name,pairs=len(sub),station_ids=sub.station_identity_id.nunique(),
                  jan_med_range=med_or_nan(sub.jan_do_range),jul_med_range=med_or_nan(sub.jul_do_range),
                  med_paired_range_diff=med_or_nan(sub.delta_do_range),
                  med_abs_noon_jan=med_or_nan(sub.jan_abs_noon_mean_diff),
                  med_abs_noon_jul=med_or_nan(sub.jul_abs_noon_mean_diff),
                  med_paired_abs_diff=med_or_nan(sub.delta_abs_noon_mean),
                  number_july_range_higher=int((sub.delta_do_range>0).sum()))
        compared.append(comp)
    pd.DataFrame(compared).to_csv(output/'phaseC_paired_site_results.csv',index=False)
    tests=[]
    for var in ['delta_do_range','delta_abs_noon_mean','delta_temp_range']:
        tests.append(dict(outcome=var,**cluster_station_season_test(paired,var)))
    pd.DataFrame(tests).to_csv(output/'phaseC_station_cluster_paired_tests.csv',index=False)
    pd.DataFrame([dict(outcome='signed_noon_minus_sixpoint_mean',**cluster_station_season_test(days[days.strict_sixpoint],'signed_noon_mean_diff'))]).to_csv(output/'phaseC_station_cluster_signed_noon_test.csv',index=False)
    mods=[cluster_regression(days,'do_range'),cluster_regression(days,'abs_noon_mean_diff')]
    pd.DataFrame(mods).to_csv(output/'phaseC_cluster_robust_regressions.csv',index=False)
    # Outlier/sensitivity: flagged by reported DO >20 and/or DO range >5; conservative maxDO ceiling not verified physically.
    sd=days[days.strict_sixpoint]
    sensitivity=[]
    subsets=[('all strict',sd),('exclude days with DO range >5',sd[~sd.flag_large_do_range_gt5]),
             ('exclude days with reported DO max >20',sd[~sd.flag_high_do_gt20]),
             ('exclude either extreme review flag',sd[~(sd.flag_large_do_range_gt5|sd.flag_high_do_gt20)]),
             ('exclude DO range >5 or pH range >2',sd[~(sd.flag_large_do_range_gt5|sd.flag_ph_range_gt2)])]
    for name,sub in subsets:
        sensitivity.append(dict(scenario=name,valid_days=len(sub),jan_days=(sub.month==1).sum(),jul_days=(sub.month==7).sum(),
                                median_abs_noon_mean=med_or_nan(sub.abs_noon_mean_diff),
                                med_jan_range=med_or_nan(sub.loc[sub.month==1,'do_range']),
                                med_jul_range=med_or_nan(sub.loc[sub.month==7,'do_range'])))
    # also paired only when both valid days survive sensitivity
    for name,sub in subsets:
        key=sub[['station_identity_id','year','month']].copy()
        uu=key.pivot_table(index=['station_identity_id','year'],columns='month',values='station_identity_id' if False else 'year',aggfunc='size',fill_value=0)
        if len(uu) and 1 in uu.columns and 7 in uu.columns:
            good=uu[(uu[1]>0)&(uu[7]>0)].reset_index()[['station_identity_id','year']]
            p1=paired.merge(good,on=['station_identity_id','year'])
        else: p1=paired.iloc[0:0]
        x=sensitivity[[v['scenario'] for v in sensitivity].index(name)]
        x['valid_pairs']=len(p1)
        x['median_paired_range_delta']=med_or_nan(p1.delta_do_range)
        x['positive_paired_range_delta']=int((p1.delta_do_range>0).sum())
    pd.DataFrame(sensitivity).to_csv(output/'phaseC_outlier_sensitivity.csv',index=False)
    sensitivity_tests=[]
    for title, pp in [('all_strict',paired),
                      ('omit_range_gt5_both_seasons',paired[(paired.jan_do_range<=5)&(paired.jul_do_range<=5)]),
                      ('omit_DO_max_gt20_both_seasons',paired[(paired.jan_do_max<=20)&(paired.jul_do_max<=20)]),
                      ('omit_DO_range_gt5_or_pH_range_gt2',paired[(paired.jan_do_range<=5)&(paired.jul_do_range<=5)&(paired.jan_ph_range<=2)&(paired.jul_ph_range<=2)])]:
        sensitivity_tests.append(dict(scenario=title,n_pairs=len(pp),
                                n_stations=pp.station_identity_id.nunique(),
                                median_pair_delta=med_or_nan(pp.delta_do_range),
                                n_positive=int((pp.delta_do_range>0).sum()),
                                **cluster_station_season_test(pp,'delta_do_range')))
    pd.DataFrame(sensitivity_tests).to_csv(output/'phaseC_seasonal_inference_sensitivity.csv',index=False)
    extreme=sd[(sd.flag_large_do_range_gt5)|(sd.flag_high_do_gt20)|(sd.flag_ph_range_gt2)].sort_values('do_range',ascending=False)
    extreme.to_csv(output/'phaseC_extreme_profiles_for_review.csv',index=False,encoding='utf-8-sig')
    ci=[]
    for col in ['do_range','signed_noon_mean_diff','abs_noon_mean_diff','noon_minus_sampled_min']:
        lo,hi,ns=bootstrap_median_ci(sd,col)
        ci.append(dict(outcome=col,median=med_or_nan(sd[col]),ci_low=lo,ci_high=hi,n_days=len(sd),n_station_clusters=ns,
                       uncertainty='cluster bootstrap across station IDs; descriptive selected cohort'))
    pd.DataFrame(ci).to_csv(output/'phaseC_cluster_bootstrap_CI.csv',index=False)
    plot_all(days,paired,output)
    manifest_counts=manifest.status.value_counts().to_dict()
    counts=dict(observation_rows=len(obs),manifest_rows=len(manifest),selected_station_years=len(roster),
                selected_stations=int(roster.station_identity_id.nunique()),design_hours=len(t),
                extra_requests=int(len(extras)),extra_http_ok=int((extras.api_status=='ok').sum()),
                extra_missing=int((extras.api_status=='no_record').sum()),extra_strict=int(extras.strict.sum()),
                days=int(len(days)),valid_days=int(days.strict_sixpoint.sum()),valid_pairs=int(len(paired)),
                valid_pair_station_identities=int(paired.station_identity_id.nunique()),
                manifest_status=manifest_counts,QC_reasons=extras.qc_reason.value_counts().to_dict())
    (output/'analysis_summary.json').write_text(json.dumps(dict(counts=counts,source_sha256={str(Path(p).name):sha(p) for p in [a.observations,a.manifest,a.roster,a.followup_day_table,a.followup_issues]}),indent=2,ensure_ascii=False),encoding='utf-8')
    print(json.dumps(counts,indent=2,ensure_ascii=False))
    print('SUMMARY',desc.to_string(index=False))
    print('PAIRED',pd.DataFrame(compared).to_string(index=False))
    print('TESTS',pd.DataFrame(tests).to_string(index=False))
    print('MODELS',pd.DataFrame(mods).to_string(index=False))
    print('SENSITIVITY',pd.DataFrame(sensitivity).to_string(index=False))
    print('EXTREMES',extreme[['site','station','date','do_range','do_min','do_noon','do_max','flag_high_do_gt20']].to_string(index=False))
    print('QC',extras.qc_reason.value_counts().to_string())

if __name__=='__main__': main()

#!/usr/bin/env python3
"""Read-only analysis of Fujian-labelled Qingyue semiannual six-point profiles.

Uses only original exported observations and manifest. Never contacts API or edits SQLite.
Python 3 dependencies: pandas, numpy, scipy, matplotlib.

python fujian_stage4_analysis.py --observations observations.csv.gz \
  --manifest manifest.csv.gz --output output_dir
"""
import argparse
from pathlib import Path
import math
import numpy as np
import pandas as pd
from scipy import stats
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

SITES = ('XMB', 'MJE', 'QZB', 'DSB')
CORE = ('water_temp', 'ph', 'dissolvedoxygen', 'codmn', 'nh3-n', 'tp', 'tn')
HOURS = (0, 4, 8, 12, 16, 20)
NORMAL = '\u6b63\u5e38'
MAINTENANCE = '\u7ef4\u62a4'
RNG = np.random.default_rng(20261012)


def write(df, path):
    df.to_csv(path, index=False, encoding='utf-8-sig', float_format='%.9g')


def describe(d):
    if not len(d):
        return {'n_days': 0, 'n_stations': 0, 'median_do_range': np.nan,
                'median_abs_noon_mean': np.nan, 'median_signed_noon_mean': np.nan,
                'median_noon_minus_min': np.nan, 'share_noon_not_min': np.nan,
                'share_noon_minus_min_ge_1': np.nan}
    return {'n_days': len(d), 'n_stations': d.station_identity_id.nunique(),
            'median_do_range': d.do_range.median(),
            'median_abs_noon_mean': d.abs_noon_sixmean.median(),
            'median_signed_noon_mean': d.signed_noon_sixmean.median(),
            'median_noon_minus_min': d.noon_minus_sampled_min.median(),
            'share_noon_not_min': (d.noon_minus_sampled_min>1e-8).mean(),
            'share_noon_minus_min_ge_1': (d.noon_minus_sampled_min>=1).mean()}


def infer_station_cluster(pairs, field, n_perm=20000, n_boot=5000):
    if not len(pairs):
        return dict(n_station_year_pairs=0, n_station_clusters=0)
    vals=pairs.groupby('station_identity_id')[field].mean().to_numpy(float)
    n=len(vals)
    flips=RNG.choice((-1,1),size=(n_perm,n))
    simulated=(flips*vals).mean(axis=1)
    observed=vals.mean()
    idx=RNG.integers(0,n,size=(n_boot,n))
    boot=vals[idx].mean(axis=1)
    return dict(n_station_year_pairs=len(pairs), n_station_clusters=n,
        mean_station_contrast=observed,
        bootstrap_stationmean_95_low=np.quantile(boot,.025),
        bootstrap_stationmean_95_high=np.quantile(boot,.975),
        signflip_stationmean_p=(1+np.sum(abs(simulated)>=abs(observed)))/(1+n_perm),
        paired_median_contrast=pairs[field].median(),
        paired_positive_count=(pairs[field]>0).sum())


def cluster_bootstrap_median(d, field, n_boot=3500):
    groups=[g[field].to_numpy(float) for _,g in d.groupby('station_identity_id')]
    n=len(groups)
    if not n: return (np.nan,np.nan)
    ans=np.empty(n_boot)
    for i in range(n_boot):
        selected=RNG.integers(0,n,n)
        ans[i]=np.median(np.concatenate([groups[j] for j in selected]))
    return np.quantile(ans,[.025,.975])


def run(obs_path, manifest_path, out_dir, priority_path=None):
    out=Path(out_dir)
    out.mkdir(parents=True,exist_ok=True)
    m=pd.read_csv(manifest_path, dtype={'request_id':'string'},low_memory=False)
    o=pd.read_csv(obs_path, dtype={'request_id':'string'},low_memory=False)
    assert len(m)==39626 and not m.request_id.duplicated().any(), 'Unexpected master plan or duplicate manifest ID'
    assert not o.request_id.duplicated().any(), 'Duplicate observation request ID'
    assert set(m.loc[m.status.eq('ok'),'request_id'])==set(o.request_id), 'Manifest/export mismatch'
    assert not m[['station_identity_id','target_datetime']].duplicated().any(), 'Duplicate station-time in manifest'
    assert not o[['station_identity_id','target_datetime']].duplicated().any(), 'Duplicate station-time in observations'

    design=m[m.plan_component.eq('semiannual_full_day_extra') & m.study_site_code.isin(SITES) &
              m.target_datetime.str[:4].between('2021','2025')].copy()
    assert len(design)==1850, f'Unexpected number of design extra-hour rows: {len(design)}'
    design['date']=design.target_datetime.str.slice(0,10)
    design['year']=design.target_datetime.str.slice(0,4).astype(int)
    design['month']=design.target_datetime.str.slice(5,7).astype(int)
    design['hour']=design.target_datetime.str.slice(11,13).astype(int)
    assert set(design.hour.unique())=={0,4,8,16,20}
    assert design.groupby(['station_identity_id','date']).size().eq(5).all()
    days_base=design[['station_identity_id','study_site_code','study_site_name','date','year','month']].drop_duplicates()
    assert len(days_base)==370
    grid=days_base.loc[days_base.index.repeat(len(HOURS))].reset_index(drop=True)
    grid['hour']=np.tile(HOURS,len(days_base))
    grid['target_datetime']=grid['date']+' '+grid.hour.map(lambda h:f'{h:02d}:00:00')
    grid['season']=np.where(grid.month.eq(1),'January','July')

    grid=grid.merge(m[['request_id','station_identity_id','target_datetime','plan_component','status']],
        on=['station_identity_id','target_datetime'],how='left',validate='1:1')
    assert len(grid)==2220 and grid.request_id.notna().all(), 'Unmatched expected 6-point plan times'
    assert grid.loc[grid.hour.eq(12),'plan_component'].eq('monthly_noon_backbone').all()
    assert grid.loc[grid.hour.ne(12),'plan_component'].eq('semiannual_full_day_extra').all()
    assert grid.status.isin(['ok','no_record','pending']).all()
    required=['request_id','monitor_time','section_status','cnemc_station_name','qingyue_station_name',
        'qingyue_province','qingyue_city','river','basin','longitude','latitude']+list(CORE)
    subset=o[required].copy()
    grid=grid.merge(subset,on='request_id',how='left',validate='1:1')
    assert grid.status.eq('ok').equals(grid.monitor_time.notna()), 'OK response has absent observation or vice versa'
    for col in CORE:
        grid[col]=pd.to_numeric(grid[col],errors='coerce')
    grid['normal']=grid.section_status.eq(NORMAL)
    grid['maintenance']=grid.section_status.eq(MAINTENANCE)
    grid['timestamp_matches']=grid.monitor_time.eq(grid.target_datetime)
    grid['seven_core_present']=np.isfinite(grid[list(CORE)].to_numpy(dtype=float)).all(axis=1)
    grid['seven_core_nonnegative']=grid[list(CORE)].ge(0).all(axis=1)
    grid['seven_core_valid']=grid.seven_core_present & grid.seven_core_nonnegative
    grid['strict']=grid.status.eq('ok') & grid.normal & grid.timestamp_matches & grid.seven_core_valid
    grid['do_only_strict']=grid.status.eq('ok') & grid.normal & grid.timestamp_matches & np.isfinite(grid.dissolvedoxygen) & grid.dissolvedoxygen.ge(0)
    grid['quality_reason']=np.select([
        grid.status.eq('pending'),grid.status.eq('no_record'),grid.status.eq('ok') & ~grid.normal,
        grid.status.eq('ok') & grid.normal & ~grid.timestamp_matches,
        grid.status.eq('ok') & grid.normal & grid.timestamp_matches & ~grid.seven_core_valid],
        ['PENDING','NO_RECORD','NON_NORMAL_STATUS','TIMESTAMP_MISMATCH','INCOMPLETE_OR_NEGATIVE_CORE'],
        default='STRICT_PASS')
    assert grid.loc[grid.strict,'status'].eq('ok').all()
    assert grid.status.eq('ok').sum()==len(grid[grid.monitor_time.notna()])
    grid['flag_high_do_gt20']=grid.dissolvedoxygen.gt(20)
    grid['flag_ph_outside_5_10']=grid.ph.lt(5)|grid.ph.gt(10)
    grid['flag_do_gt15']=grid.dissolvedoxygen.gt(15)
    grid['flag_temp_gt40']=grid.water_temp.gt(40)
    write(grid,out/'fujian_expected_hourly_QC.csv')

    day_rows=[]
    for (site,id,date),g in grid.groupby(['study_site_code','station_identity_id','date'],sort=True):
        g=g.sort_values('hour')
        assert tuple(g.hour.tolist())==HOURS
        noon=g[g.hour.eq(12)].iloc[0]
        extra=g[g.hour.ne(12)]
        strict=bool(g.strict.all())
        rec=dict(site=site,station_identity_id=id,station=noon.cnemc_station_name,
            qingyue_province=noon.qingyue_province,qingyue_city=noon.qingyue_city,
            river=noon.river,longitude=noon.longitude,latitude=noon.latitude,
            date=date,year=int(noon.year),month=int(noon.month),season=noon.season,
            noon_status=noon.status,noon_strict=bool(noon.strict),
            extras_ok=int(extra.status.eq('ok').sum()),extras_no_record=int(extra.status.eq('no_record').sum()),
            extras_pending=int(extra.status.eq('pending').sum()),
            extras_strict=int(extra.strict.sum()),
            all_extra_ok=bool(extra.status.eq('ok').all()),strict_sixpoint=strict,
            do_only_sixpoint=bool(g.do_only_strict.all()),
            invalid_hours=';'.join(f'{int(r.hour):02d}:{r.quality_reason}' for r in g.itertuples() if not r.strict))
        for col in ['do_noon','do_sixmean','do_min','do_max','do_range','signed_noon_sixmean',
             'abs_noon_sixmean','noon_minus_sampled_min','temp_range','ph_range','do_min_hour','do_max_hour']:
            rec[col]=np.nan
        if strict:
            do=g.dissolvedoxygen.to_numpy(float)
            rec.update(do_noon=float(noon.dissolvedoxygen),do_sixmean=float(do.mean()),
                do_min=float(do.min()),do_max=float(do.max()),do_range=float(np.ptp(do)),
                signed_noon_sixmean=float(noon.dissolvedoxygen-do.mean()),
                abs_noon_sixmean=float(abs(noon.dissolvedoxygen-do.mean())),
                noon_minus_sampled_min=float(noon.dissolvedoxygen-do.min()),
                temp_range=float(np.ptp(g.water_temp.to_numpy(float))),
                ph_range=float(np.ptp(g.ph.to_numpy(float))),
                do_min_hour=int(g.loc[g.dissolvedoxygen.idxmin(),'hour']),
                do_max_hour=int(g.loc[g.dissolvedoxygen.idxmax(),'hour']))
        if rec['do_only_sixpoint']:
            doval=g.dissolvedoxygen.to_numpy(float)
            rec['do_only_range']=float(np.ptp(doval))
            rec['do_only_signed_noon_sixmean']=float(noon.dissolvedoxygen-doval.mean())
        else:
            rec['do_only_range']=np.nan
            rec['do_only_signed_noon_sixmean']=np.nan
        rec['flag_do_range_gt5']=bool(strict and rec['do_range']>5)
        rec['flag_do_range_gt10']=bool(strict and rec['do_range']>10)
        rec['flag_max_do_gt20']=bool(strict and rec['do_max']>20)
        rec['flag_ph_range_gt2']=bool(strict and rec['ph_range']>2)
        day_rows.append(rec)
    days=pd.DataFrame(day_rows).sort_values(['site','station_identity_id','year','month']).reset_index(drop=True)
    assert len(days)==370
    write(days,out/'fujian_station_days.csv')

    day_summary=[]
    for key,g in days.groupby(['site','year','season'],sort=True):
        day_summary.append(dict(site=key[0],year=key[1],season=key[2],
            n_planned_days=len(g),noon_ok=int(g.noon_status.eq('ok').sum()),
            noon_strict=int(g.noon_strict.sum()),all_extra_ok=int(g.all_extra_ok.sum()),
            n_strict_profiles=int(g.strict_sixpoint.sum()),total_extra_ok=int(g.extras_ok.sum()),
            total_extra_strict=int(g.extras_strict.sum()),n_stations=g.station_identity_id.nunique()))
    coverage=pd.DataFrame(day_summary)
    write(coverage,out/'fujian_profile_coverage_by_site_year_season.csv')

    coverage_site=days.groupby('site').agg(planned_station_days=('date','size'),
        station_identities=('station_identity_id','nunique'),noon_ok=('noon_status',lambda x:int(x.eq('ok').sum())),
        noon_strict=('noon_strict','sum'),all_extra_ok=('all_extra_ok','sum'),
        valid_sixpoint_days=('strict_sixpoint','sum'),
        extra_ok=('extras_ok','sum'),extra_no_record=('extras_no_record','sum'),
        extra_pending=('extras_pending','sum'),extra_strict=('extras_strict','sum')).reset_index()
    write(coverage_site,out/'fujian_coverage_by_site.csv')
    pending_class=days.loc[days.extras_pending.gt(0)].groupby(['site','noon_status','noon_strict'],dropna=False).agg(
        n_station_days=('date','size'),pending_extra_requests=('extras_pending','sum')).reset_index()
    write(pending_class,out/'fujian_remaining_190_requests_by_noon_dependency.csv')

    valid=days[days.strict_sixpoint].copy()
    jan=valid[valid.month.eq(1)]
    jul=valid[valid.month.eq(7)]
    pairs=jan.merge(jul,on=['site','station_identity_id','year'],how='inner',suffixes=('_jan','_jul'),validate='1:1')
    pairs['delta_do_range']=pairs.do_range_jul-pairs.do_range_jan
    pairs['delta_abs_noon_sixmean']=pairs.abs_noon_sixmean_jul-pairs.abs_noon_sixmean_jan
    pairs['delta_temp_range']=pairs.temp_range_jul-pairs.temp_range_jan
    pairs['delta_ph_range']=pairs.ph_range_jul-pairs.ph_range_jan
    write(pairs,out/'fujian_valid_january_july_pairs.csv')

    # DO-only sensitivity: allow missing nutrient/core variables when DO itself is valid.
    dov=days[days.do_only_sixpoint].copy()
    dj=dov[dov.month.eq(1)]
    du=dov[dov.month.eq(7)]
    dopairs=dj.merge(du,on=['site','station_identity_id','year'],how='inner',suffixes=('_jan','_jul'),validate='1:1')
    dopairs['delta_do_only_range']=dopairs.do_only_range_jul-dopairs.do_only_range_jan
    dotests=[]
    for name,grp in [('All DO-only',dopairs),('DO-only both ranges <= 5',dopairs[(dopairs.do_only_range_jan.le(5)) & (dopairs.do_only_range_jul.le(5))])]:
        dotests.append({'sample':name,**infer_station_cluster(grp,'delta_do_only_range')})
    write(pd.DataFrame(dotests),out/'fujian_DO_only_sensitivity_tests.csv')
    write(dopairs,out/'fujian_DO_only_paired_data.csv')

    sums=[]
    for label,d in [('All',valid),('January',jan),('July',jul)]:
        sums.append({'season':label,**describe(d)})
    write(pd.DataFrame(sums),out/'fujian_summary_by_season.csv')
    site_s=[]
    for (site,season),d in valid.groupby(['site','season']):
        site_s.append({'site':site,'season':season,**describe(d)})
    write(pd.DataFrame(site_s),out/'fujian_summary_by_site_season.csv')

    pair_s=[]
    for site,g in pairs.groupby('site'):
        pair_s.append(dict(site=site,n_stationyear_pairs=len(g),n_unique_stations=g.station_identity_id.nunique(),
            median_july_minus_jan_do_range=g.delta_do_range.median(),
            positive_july_minus_jan=(g.delta_do_range>0).sum(),
            median_july_minus_jan_abs_bias=g.delta_abs_noon_sixmean.median()))
    write(pd.DataFrame(pair_s),out/'fujian_paired_by_site.csv')

    tests=[]
    for label,g in [('All years',pairs),('2021-2024',pairs[pairs.year.le(2024)]),
                    ('All years DO range <=5 both seasons',pairs[(pairs.do_range_jan.le(5))&(pairs.do_range_jul.le(5))]),
                    ('2021-2024 DO range <=5 both seasons',pairs[pairs.year.le(2024) & pairs.do_range_jan.le(5) & pairs.do_range_jul.le(5)])]:
        for field in ['delta_do_range','delta_abs_noon_sixmean','delta_temp_range']:
            tests.append({'sample':label,'outcome':field,**infer_station_cluster(g,field)})
    write(pd.DataFrame(tests),out/'fujian_station_clustered_paired_inference.csv')
    bias=[]
    for nm,g in [('All strict',valid),('2021-2024 only',valid[valid.year.le(2024)]),
                 ('Exclude DO range >5',valid[valid.do_range.le(5)]),
                 ('Exclude large DO and pH swings',valid[valid.do_range.le(5) & valid.ph_range.le(2)]),
                 ('Only days with valid seasonal pair',valid.merge(pairs[['station_identity_id','year']],on=['station_identity_id','year'],how='inner'))]:
        bias.append({'sample':nm,'outcome':'signed_noon_minus_sixpoint_mean',**infer_station_cluster(g,'signed_noon_sixmean')})
    write(pd.DataFrame(bias), out/'fujian_signed_noon_station_cluster_test.csv')

    cibs=[]
    for season,d in [('All',valid),('January',jan),('July',jul)]:
        for f in ['do_range','abs_noon_sixmean','signed_noon_sixmean','noon_minus_sampled_min']:
            lo,hi=cluster_bootstrap_median(d,f)
            cibs.append(dict(season=season,metric=f,n_days=len(d),n_stations=d.station_identity_id.nunique(),
                              median=d[f].median(),clusterbootstrap_low=lo,clusterbootstrap_high=hi))
    write(pd.DataFrame(cibs),out/'fujian_station_cluster_bootstrap_medians.csv')

    qc=grid.groupby(['study_site_code','season','hour','quality_reason'],dropna=False).size().reset_index(name='n')
    write(qc,out/'fujian_quality_reasons_by_site_season_hour.csv')
    qc2025=grid[grid.year.eq(2025)].groupby(['study_site_code','season','hour','quality_reason']).size().reset_index(name='n')
    write(qc2025,out/'fujian_2025_hourly_quality_reasons.csv')

    flags=valid.loc[valid[['flag_do_range_gt5','flag_max_do_gt20','flag_ph_range_gt2']].any(axis=1)]
    write(flags,out/'fujian_extreme_profiles_for_review.csv')
    # Review larger pH shifts using all individual usable pH values, even if another core analyte is missing.
    ph_rows=grid[grid.status.eq('ok') & grid.normal & grid.timestamp_matches & grid.ph.notna() & grid.ph.ge(0)].copy()
    ph_review=ph_rows.groupby(['study_site_code','station_identity_id','date']).agg(
        observed_ph_hours=('hour','size'),ph_min=('ph','min'),ph_max=('ph','max'),
        station=('cnemc_station_name','first')).reset_index()
    ph_review['ph_range']=ph_review.ph_max-ph_review.ph_min
    write(ph_review.loc[ph_review.ph_range.gt(2)].sort_values('ph_range',ascending=False),out/'fujian_all_usable_pH_excursions_for_review.csv')
    timing=valid.groupby(['season','do_min_hour']).size().reset_index(name='n_profiles')
    write(timing,out/'fujian_observed_DO_minimum_sampled_hour.csv')
    # Station metadata are SOURCE labels. Connected watershed is unverified.
    roster=(o[o.study_site_code.isin(SITES)][['study_site_code','station_identity_id','cnemc_station_name',
         'qingyue_station_name','qingyue_province','qingyue_city','river','basin','longitude','latitude']]
         .drop_duplicates('station_identity_id').sort_values(['study_site_code','qingyue_city','cnemc_station_name']).copy())
    roster['watershed_connection_verified']=False
    roster['geography_review_note']='Site assignment provisional; delineate upstream watershed and outlet'
    roster.loc[(roster.study_site_code.eq('MJE')) & roster.river.eq('\u6556\u6c5f'),'geography_review_note']=(
        'Different named river (Aojiang) within MJE grouping; verify outlet and intended scope')
    roster.loc[(roster.study_site_code.eq('QZB')) & roster.river.eq('\u6728\u5170\u6eaa'),'geography_review_note']=(
        'Different named river (Mulanxi) within QZB grouping; verify outlet and intended scope')
    write(roster,out/'fujian_station_geography_review.csv')

    # HTML-free plots suitable for manuscripts, no interpretation hard-coded.
    labels={'XMB':'Xiamen Bay group','MJE':'Minjiang Estuary group',
            'QZB':'Quanzhou Bay group','DSB':'Dongshan Bay group'}
    order=['XMB','MJE','QZB','DSB']
    fig,ax=plt.subplots(figsize=(8.8,4.8))
    x=np.arange(len(order)); width=.36
    jan_n=np.array([int(((days.site==s)&(days.month==1)&days.strict_sixpoint).sum()) for s in order])
    jul_n=np.array([int(((days.site==s)&(days.month==7)&days.strict_sixpoint).sum()) for s in order])
    maxplan=np.array([int(((days.site==s)&(days.month==1)).sum()) for s in order])
    b1=ax.bar(x-width/2,jan_n,width,label='January')
    b2=ax.bar(x+width/2,jul_n,width,label='July')
    for b in (b1,b2): ax.bar_label(b,padding=3,fontsize=9)
    ax.set_xticks(x,[labels[s] for s in order],rotation=9,ha='right')
    ax.set_ylabel('Strictly complete six-point station-days')
    ax.set_title('Fujian-labelled network: strict profile coverage, 2021-2025')
    ax.legend(frameon=False)
    ax.set_ylim(0,max(jan_n.max(),jul_n.max())*1.28)
    fig.tight_layout();fig.savefig(out/'figure_01_valid_profiles_by_site_season.png',dpi=190);fig.savefig(out/'figure_01_valid_profiles_by_site_season.svg');plt.close(fig)

    fig,ax=plt.subplots(figsize=(6.4,5.2))
    xp=pairs.do_range_jan.to_numpy(float);yp=pairs.do_range_jul.to_numpy(float)
    for site,g in pairs.groupby('site'):
        ax.scatter(g.do_range_jan,g.do_range_jul,s=45,alpha=.76,label=f'{site} (n={len(g)})')
    hi=max(np.nanmax(xp),np.nanmax(yp),1)*1.05
    ax.plot([0,hi],[0,hi],linestyle='--',color='gray',linewidth=1)
    ax.set_xlim(0,hi);ax.set_ylim(0,hi)
    ax.set_xlabel('January: observed six-point DO range')
    ax.set_ylabel('July: observed six-point DO range')
    ax.set_title('Paired winter-summer DO variability')
    ax.legend(frameon=False,fontsize=9)
    fig.tight_layout();fig.savefig(out/'figure_02_paired_january_july_DO_range.png',dpi=190);fig.savefig(out/'figure_02_paired_january_july_DO_range.svg');plt.close(fig)

    fig,ax=plt.subplots(figsize=(7,5.1))
    for site,g in valid.groupby('site'):
        ax.scatter(g.do_sixmean,g.do_noon,s=35,alpha=.7,label=f'{site} (n={len(g)})')
    lo=min(valid.do_sixmean.min(),valid.do_noon.min())
    hi=max(valid.do_sixmean.max(),valid.do_noon.max())
    ax.plot([lo,hi],[lo,hi],linestyle='--',color='gray',linewidth=1)
    ax.set(xlabel='Six-point arithmetic mean DO',ylabel='Noon DO',title='Noon vs six-point dissolved oxygen, 2021-2025')
    ax.legend(frameon=False,fontsize=9)
    fig.tight_layout();fig.savefig(out/'figure_03_noon_vs_sixpoint_DO.png',dpi=190);fig.savefig(out/'figure_03_noon_vs_sixpoint_DO.svg');plt.close(fig)

    # Heatmap for QC missingness concentration by time and season-year.
    hm=(grid.assign(period=grid.year.astype(str)+'-'+grid.season.str[:3])
            .groupby(['period','hour']).strict.agg(['sum','count']).reset_index())
    hm['rate']=hm['sum']/hm['count']
    order_period=[f'{y}-{mo}' for y in range(2021,2026) for mo in ['Jan','Jul']]
    mat=hm.pivot(index='period',columns='hour',values='rate').reindex(index=order_period,columns=list(HOURS))
    fig,ax=plt.subplots(figsize=(7.1,4.7))
    heat=ax.imshow(mat.values,aspect='auto',interpolation='nearest',vmin=0,vmax=1,cmap='cividis')
    ax.set_yticks(range(len(mat)),mat.index)
    ax.set_xticks(range(len(HOURS)),[f'{h:02d}:00' for h in HOURS])
    for i in range(len(mat)):
        for j in range(len(HOURS)):
            val=mat.iloc[i,j]
            ax.text(j,i,f'{val:.0%}',ha='center',va='center',fontsize=9,
                color='white' if val < .53 else 'black')
    ax.set_title('Strict core-measurement validity by sampled hour')
    cb=fig.colorbar(heat,ax=ax,fraction=.04,pad=.03)
    cb.set_label('Proportion strictly valid')
    fig.tight_layout();fig.savefig(out/'figure_04_hourly_strict_validity_heatmap.png',dpi=190);fig.savefig(out/'figure_04_hourly_strict_validity_heatmap.svg');plt.close(fig)

    summary={
        'manifest_rows':len(m),'observations_rows':len(o),
        'four_study_areas':list(SITES), 'n_station_ids':roster.station_identity_id.nunique(),
        'planned_days':len(days),'planned_timepoints':len(grid),
        'extra_requested':int((grid.hour!=12).sum()),
        'extra_ok':int((grid.hour.ne(12)&grid.status.eq('ok')).sum()),
        'extra_no_record':int((grid.hour.ne(12)&grid.status.eq('no_record')).sum()),
        'extra_pending':int((grid.hour.ne(12)&grid.status.eq('pending')).sum()),
        'extra_strict':int((grid.hour.ne(12)&grid.strict).sum()),
        'noon_strict':int(days.noon_strict.sum()),
        'pending_extras_with_noon_no_record':int(days.loc[days.noon_status.eq('no_record'),'extras_pending'].sum()),
        'pending_extras_with_noon_ok_but_strict_false':int(days.loc[days.noon_status.eq('ok') & ~days.noon_strict,'extras_pending'].sum()),
        'strict_days':len(valid),'jan_valid_days':len(jan),'jul_valid_days':len(jul),
        'strict_pairs':len(pairs),'strict_pair_station_ids':pairs.station_identity_id.nunique(),
        'do_only_valid_days':len(dov),'do_only_jan_days':len(dj),'do_only_jul_days':len(du),
        'do_only_paired_stationyears':len(dopairs),
        'median_abs_noon_sixmean':valid.abs_noon_sixmean.median(),
        'median_signed_noon_sixmean':valid.signed_noon_sixmean.median(),
        'median_noon_minus_min':valid.noon_minus_sampled_min.median(),
        'jan_median_do_range':jan.do_range.median(),
        'jul_median_do_range':jul.do_range.median(),
        'median_paired_jul_minus_jan_do_range':pairs.delta_do_range.median(),
        'count_paired_larger_jul':int(pairs.delta_do_range.gt(0).sum()),
        'max_do_range':valid.do_range.max(),
        'large_range_gt5_profiles':int(valid.flag_do_range_gt5.sum()),
        'large_range_gt10_profiles':int(valid.flag_do_range_gt10.sum()),
        'max_do_gt20_profiles':int(valid.flag_max_do_gt20.sum()),
        'ph_range_gt2_profiles':int(valid.flag_ph_range_gt2.sum()),
        '2025_january_extras_ok':int(grid[(grid.year==2025)&(grid.month==1)&(grid.hour!=12)].status.eq('ok').sum()),
        '2025_january_extras_strict':int(grid[(grid.year==2025)&(grid.month==1)&(grid.hour!=12)].strict.sum())
    }
    import json
    with open(out/'analysis_summary.json','w',encoding='utf-8') as f:
        json.dump(summary,f,ensure_ascii=False,indent=2,default=lambda x:float(x) if isinstance(x,np.floating) else int(x))
    if priority_path:
        ids=[line.strip() for line in Path(priority_path).read_text(encoding='utf-8-sig').splitlines() if line.strip() and not line.startswith('#')]
        assert len(ids)==1290 and len(set(ids))==1290
        t=m[m.request_id.isin(ids)].copy()
        assert len(t)==1290 and t.status.isin(['ok','no_record']).all()
        obs_sub=o[o.request_id.isin(ids)].copy()
        for c in CORE:
            obs_sub[c]=pd.to_numeric(obs_sub[c],errors='coerce')
        obs_sub['seven_core_valid']=np.isfinite(obs_sub[list(CORE)].to_numpy(dtype=float)).all(axis=1) & obs_sub[list(CORE)].ge(0).all(axis=1)
        obs_sub['strict']=obs_sub.section_status.eq(NORMAL) & obs_sub.monitor_time.eq(obs_sub.target_datetime) & obs_sub.seven_core_valid
        t=t.merge(obs_sub[['request_id','strict','qingyue_province']],on='request_id',how='left',validate='1:1')
        summary['priority_resolved_requests']=len(t)
        summary['priority_ok']=int(t.status.eq('ok').sum())
        summary['priority_no_record']=int(t.status.eq('no_record').sum())
        summary['priority_four_sites_requests']=int(t.study_site_code.isin(SITES).sum())
        summary['priority_sanmen_bay_requests']=int(t.study_site_code.eq('SMB').sum())
        cohort=[]
        for site,g in t.groupby('study_site_code'):
            cohort.append(dict(site=site,site_name=g.study_site_name.iloc[0],
                n_approved_requests=len(g),ok=int(g.status.eq('ok').sum()),no_record=int(g.status.eq('no_record').sum()),
                strict_valid=int(g.strict.eq(True).sum()),
                province_values=';'.join(sorted(g.qingyue_province.dropna().unique())),
                in_fujian_sample=site in SITES))
        write(pd.DataFrame(cohort),out/'original_priority_1290_acquisition_and_measurement_QC.csv')
    with open(out/'analysis_summary.json','w',encoding='utf-8') as f:
        json.dump(summary,f,ensure_ascii=False,indent=2,default=lambda x:float(x) if isinstance(x,np.floating) else int(x))
    print(json.dumps(summary,indent=2,ensure_ascii=False))
    return summary


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--observations',required=True)
    p.add_argument('--manifest',required=True)
    p.add_argument('--output',required=True)
    p.add_argument('--priority-list',default=None,help='Optional earlier approved 1290-ID continuation subset')
    a=p.parse_args()
    run(a.observations,a.manifest,a.output,a.priority_list)

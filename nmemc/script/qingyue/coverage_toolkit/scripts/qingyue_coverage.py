#!/usr/bin/env python3
"""Qingyue acquisition and scientific-coverage dashboard; read-only inputs.

Requires pandas, numpy, matplotlib. Optionally geopandas, shapely for watershed polygons.
Does not access the network, SQLite, raw API responses, or provider credentials.

Usage:
 python qingyue_coverage.py --manifest manifest.csv.gz --observations observations.csv.gz \
   --plan qingyue_request_plan_recommended.csv --output coverage_output \
   [--boundaries-dir /path/to/18_shapefiles] [--focus-sites XMB,MJE,QZB,DSB]
"""
from __future__ import annotations
import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import html
import json
import math
import re
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
from matplotlib.colors import ListedColormap

CORE = ('water_temp', 'ph', 'dissolvedoxygen', 'codmn', 'nh3-n', 'tp', 'tn')
OTHER = ('conductivity', 'turbidity', 'chlorophyll', 'algal_density')
PARAMS = CORE + OTHER
LABELS = {
    'water_temp':'Temperature', 'ph':'pH', 'dissolvedoxygen':'Dissolved oxygen',
    'codmn':'CODMn', 'nh3-n':'NH3-N', 'tp':'Total phosphorus', 'tn':'Total nitrogen',
    'conductivity':'Conductivity','turbidity':'Turbidity',
    'chlorophyll':'Chlorophyll','algal_density':'Algal density'
}
STATUSES = ('ok', 'no_record', 'skipped_noon_missing', 'pending',
            'retryable_error', 'transient_exhausted', 'fatal_error',
            'application_error', 'response_mismatch')
STAGE_ORDER = ('1','2','3','4','deferred-2020','extension')
SITE_COLORS = dict(zip(('XMB','MJE','QZB','DSB','SMB','BHB','LZB','BBG','DYB','JZB'),
                       ('#1261A0','#238B8D','#D98A25','#A8465C','#9F77A3',
                        '#436B39','#8A5B47','#4160B0','#DAA141','#B56F90')))
STATUS_COLORS = {
    'ok':'#16758A','no_record':'#E3A142','skipped_noon_missing':'#9EA8AF',
    'pending':'#DEE5E8','retryable_error':'#D9685C','transient_exhausted':'#D9685C',
    'fatal_error':'#D9685C','application_error':'#D9685C',
    'response_mismatch':'#D9685C'
}
BG='#FAFCFD'; NAVY='#16324F'; SLATE='#536675'

def stage_of(df):
    dt = df['target_datetime'].astype('string')
    month=dt.str.slice(0,7)
    year=pd.to_numeric(dt.str.slice(0,4),errors='coerce')
    component=df['plan_component'].astype('string')
    stage=np.full(len(df),'extension',dtype=object)
    stage[(month>='2020-05')&(month<='2020-10')]='deferred-2020'
    noon=(component=='monthly_noon_backbone').fillna(False).to_numpy()
    extra=(component=='semiannual_full_day_extra').fillna(False).to_numpy()
    yy=year.to_numpy()
    stage[noon&(yy>=2021)&(yy<=2025)]='1'
    stage[noon&(month.isin(['2020-11','2020-12']).to_numpy())]='2'
    stage[noon&(yy==2026)]='3'
    stage[extra&(yy>=2021)&(yy<=2026)]='4'
    return stage

def pct(a,b):
    return float(a)/float(b)*100 if b else float('nan')

def strval(v):
    return '' if pd.isna(v) else str(v)

def safe_status(s):
    return s if s in STATUSES else ('other_' + re.sub(r'[^a-z0-9_]','_',str(s).lower()))

def longest_run(indices):
    seq=sorted(set(indices))
    if not seq: return 0
    prev=seq[0]; cur=best=1
    for i in seq[1:]:
        cur=cur+1 if i==prev+1 else 1
        prev=i;best=max(best,cur)
    return best

def save_table(df, out, basename):
    df.to_csv(out/(basename+'.csv'),index=False,encoding='utf-8-sig')
    return df

def read_data(path, name):
    if not path.is_file(): raise FileNotFoundError(f'{name}: {path}')
    return pd.read_csv(path,dtype={'request_id':'string','station_identity_id':'string'},low_memory=False)

def theme(ax, title=None, subtitle=None):
    ax.set_facecolor(BG)
    if title: ax.set_title(title,loc='left',color=NAVY,weight='bold',fontsize=12,pad=17)
    ax.spines[['top','right']].set_visible(False)
    ax.spines[['left','bottom']].set_color('#D7E1E7')
    ax.tick_params(colors=SLATE,labelsize=8)
    ax.grid(axis='y',color='#DFE7EB',linewidth=.7,alpha=.6)
    ax.set_axisbelow(True)

def finish(fig,out,path):
    fig.patch.set_facecolor('white')
    fig.savefig(out/path,dpi=180,bbox_inches='tight',facecolor='white')
    fig.savefig(out/(Path(path).stem+'.pdf'),bbox_inches='tight',facecolor='white')
    plt.close(fig)

def stage_plot(df,out):
    grouped=df.groupby(['stage','status'],dropna=False).size().unstack(fill_value=0)
    statuses=[s for s in STATUSES if s in grouped.columns]+[s for s in grouped.columns if s not in STATUSES]
    order=[x for x in STAGE_ORDER if x in grouped.index]+[x for x in grouped.index if x not in STAGE_ORDER]
    grouped=grouped.loc[order]
    vals=grouped.sum(axis=1).astype(float)
    fig,ax=plt.subplots(figsize=(11,4.6))
    theme(ax,'Acquisition outcomes by plan stage')
    y=np.arange(len(order));left=np.zeros(len(order))
    for st in statuses:
        v=(grouped.get(st,pd.Series(0,index=order)).to_numpy()/vals.to_numpy())*100
        ax.barh(y,v,left=left,color=STATUS_COLORS.get(st,'#A66D7C'),height=.6,label=st.replace('_',' '))
        left+=v
    ax.set_yticks(y,[f'{s}  (n={int(vals.loc[s]):,})' for s in order]);ax.invert_yaxis()
    ax.set_xlim(0,100);ax.set_xlabel('Share of planned request identities (%)')
    ax.grid(axis='x',color='#DFE7EB',alpha=.55);ax.grid(axis='y',visible=False)
    ax.legend(loc='lower center',bbox_to_anchor=(.5,-.36),ncol=4,frameon=False,fontsize=8)
    fig.subplots_adjust(left=.23,bottom=.28)
    finish(fig,out,'figure_01_stage_outcomes.png')

def monthly_plots(monthly,out):
    allmonths=monthly.groupby('month',as_index=False)[['planned','ok','numeric_core','strict_core']].sum().sort_values('month')
    fig,ax=plt.subplots(figsize=(15,4.5));theme(ax,'Monthly noon observation coverage')
    t=np.arange(len(allmonths))
    for col,c,lab in [('planned','#C4CBD3','Planned station-months'),('ok','#1B7692','Retrieved'),('numeric_core','#60AB98','Seven-core numeric'),('strict_core','#289877','Strict seven-core, normal')]:
        ax.plot(t,allmonths[col],label=lab,color=c,lw=2.15 if col!='planned' else 1.6)
    months=allmonths.month.tolist()
    ticks=[i for i,m in enumerate(months) if m.endswith('-01') or m.endswith('-07')]
    if not ticks: ticks=list(range(0,len(months),max(1,len(months)//12)))
    ax.set_xticks(ticks,[months[i] for i in ticks],rotation=40,ha='right')
    ax.set_ylabel('Scheduled station-month observations');ax.legend(frameon=False,ncol=4,loc='upper left',fontsize=8)
    finish(fig,out,'figure_02_monthly_noon_count.png')
    # site x calendar month: rates based on planned station-months
    df=monthly.copy();df['rate']=df['strict_core']/df['planned']
    sites=sorted(df.site.unique());mts=sorted(df.month.unique())
    mat=df.pivot(index='site',columns='month',values='rate').reindex(index=sites,columns=mts)
    fig,ax=plt.subplots(figsize=(max(13,len(mts)*.19),max(4,len(sites)*.5+2)))
    cm=plt.get_cmap('YlGnBu').copy();cm.set_bad('#E7ECEF')
    im=ax.imshow(np.ma.masked_invalid(mat.to_numpy(float)),aspect='auto',cmap=cm,vmin=0,vmax=1,interpolation='nearest')
    theme(ax,'Spatial–temporal completeness: strict valid monthly noon')
    xticks=[i for i,m in enumerate(mts) if m.endswith('-01') or m.endswith('-07')]
    ax.set_xticks(xticks,[mts[i] for i in xticks],rotation=60,ha='right');ax.set_yticks(np.arange(len(sites)),sites)
    ax.grid(False)
    cb=fig.colorbar(im,ax=ax,shrink=.83,pad=.012);cb.set_label('Valid core / planned station-months',fontsize=8)
    fig.tight_layout();finish(fig,out,'figure_03_site_month_coverage_heatmap.png')
    # Separate core-numeric coverage reveals data hidden by blank source QC-status fields (notably 2026).
    numeric=monthly.copy(); numeric['rate']=numeric['numeric_core']/numeric['planned']
    mat=numeric.pivot(index='site',columns='month',values='rate').reindex(index=sites,columns=mts)
    fig,ax=plt.subplots(figsize=(max(13,len(mts)*.19),max(4,len(sites)*.5+2)))
    cm=plt.get_cmap('YlGnBu').copy();cm.set_bad('#E7ECEF')
    im=ax.imshow(np.ma.masked_invalid(mat.to_numpy(float)),aspect='auto',cmap=cm,vmin=0,vmax=1,interpolation='nearest')
    theme(ax,'Spatial–temporal coverage: numeric seven-core (all QC statuses)')
    ax.set_xticks(xticks,[mts[i] for i in xticks],rotation=60,ha='right');ax.set_yticks(np.arange(len(sites)),sites);ax.grid(False)
    fig.colorbar(im,ax=ax,shrink=.83,pad=.012).set_label('Numeric core / planned station-months')
    fig.tight_layout();finish(fig,out,'figure_11_site_month_numeric_core.png')
    return allmonths

def parameter_plot(byyear,out):
    # chart is conditional numerical validity among retrieved observations, not expected requests
    years=sorted(byyear.year.unique())
    fig,axes=plt.subplots(1,2,figsize=(14,6.4),sharey=True)
    for ax,comp,title in zip(axes,['monthly_noon_backbone','semiannual_full_day_extra'],['Monthly noon','Supplementary hours']):
        d=byyear[byyear.component==comp]
        mat=d.pivot_table(index='parameter',columns='year',values='numeric_rate',aggfunc='first').reindex(index=PARAMS,columns=years)
        cm=plt.get_cmap('PuBuGn').copy();cm.set_bad('#E7ECEF')
        img=ax.imshow(np.ma.masked_invalid(mat.to_numpy(float)),cmap=cm,vmin=0,vmax=100,aspect='auto')
        ax.set_xticks(np.arange(len(years)),years,rotation=35);ax.set_yticks(np.arange(len(PARAMS)),[LABELS[p] for p in PARAMS]);ax.grid(False)
        ax.set_title(title,color=NAVY,weight='bold',pad=12)
        for i in range(len(PARAMS)):
            for j in range(len(years)):
                x=mat.iloc[i,j]
                if pd.notna(x): ax.text(j,i,f'{x:.0f}',ha='center',va='center',fontsize=7,color='white' if x>75 else NAVY)
    fig.suptitle('Parameter availability among retrieved observations (%)',color=NAVY,weight='bold',fontsize=13)
    fig.subplots_adjust(left=.15,right=.90,top=.87,bottom=.15,wspace=.06)
    cax=fig.add_axes([.921,.24,.016,.59])
    fig.colorbar(img,cax=cax).set_label('Nonnegative numeric values (%)')
    finish(fig,out,'figure_04_parameter_availability.png')

def daily_plot(daily,out):
    if daily.empty:return
    d=daily.groupby(['year','season'],as_index=False).agg(planned=('date','size'),strict=('strict_six','sum'),all_retrieved=('all_six_ok','sum'))
    d['label']=d.year.astype(str)+' '+d.season
    x=np.arange(len(d))
    fig,ax=plt.subplots(figsize=(max(11,len(d)*.9),4.7));theme(ax,'Six-point daily profiles: retrieved versus strictly valid')
    ax.bar(x-.19,d.all_retrieved/d.planned*100,width=.38,color='#87B8CF',label='All six retrieved')
    ax.bar(x+.19,d.strict/d.planned*100,width=.38,color='#278B7F',label='Six strict-valid readings')
    ax.set_ylim(0,106);ax.set_ylabel('Share of planned six-point station-days (%)')
    ax.set_xticks(x,d.label,rotation=45,ha='right');ax.legend(frameon=False,ncol=2,loc='upper right')
    for i,r in d.iterrows(): ax.text(i+.19,r.strict/r.planned*100+2,f'{int(r.strict)}/{int(r.planned)}',fontsize=7,ha='center',color=NAVY)
    fig.tight_layout();finish(fig,out,'figure_05_diurnal_profile_completeness.png')



def polygon_month_plot(boundary_month,out):
    if boundary_month.empty:return
    bt=boundary_month.copy()
    bt['share']=bt.strict_core/bt.planned
    polys=sorted(bt.polygon.unique());months=sorted(bt.month.unique())
    mat=bt.pivot(index='polygon',columns='month',values='share').reindex(index=polys,columns=months)
    fig,ax=plt.subplots(figsize=(max(13,len(months)*.19),max(5,len(polys)*.4+1)))
    cm=plt.get_cmap('YlGnBu').copy();cm.set_bad('#E7ECEF')
    im=ax.imshow(np.ma.masked_invalid(mat.to_numpy(float)),aspect='auto',cmap=cm,vmin=0,vmax=1)
    theme(ax,'Study-polygon point coverage by month')
    ticks=[i for i,m in enumerate(months) if m.endswith('-01') or m.endswith('-07')]
    ax.set_xticks(ticks,[months[i] for i in ticks],rotation=55,ha='right')
    ax.set_yticks(np.arange(len(polys)),[x[:35] for x in polys]);ax.grid(False)
    fig.colorbar(im,ax=ax,shrink=.78,pad=.008).set_label('Strict noon / planned noon requests')
    fig.tight_layout();finish(fig,out,'figure_09_polygon_month_coverage.png')




def parameter_trend_plot(stats,out,focus_sites):
    """Descriptive medians and interquartile ranges; not a within-station trend model."""
    keep=stats[(stats.component=='monthly_noon_backbone') &
               (stats.parameter.isin(['dissolvedoxygen','nh3-n','tp']))].copy()
    if focus_sites:keep=keep[keep.site.isin(focus_sites)]
    keep=keep[keep.n>=3]  # Avoid drawing a site-year trend point from fewer than three snapshots.
    if keep.empty:return
    params=['dissolvedoxygen','nh3-n','tp']
    fig,axes=plt.subplots(3,1,figsize=(11,10.5),sharex=True)
    for ax,var in zip(axes,params):
        theme(ax,LABELS[var]+' — observed median (monthly noon)')
        d=keep[keep.parameter==var]
        for site,x in d.groupby('site'):
            x=x.sort_values('year')
            ax.plot(x.year,x['median'],marker='o',lw=1.7,markersize=4,
                    label=site,color=SITE_COLORS.get(site,'#78909c'))
            ax.fill_between(x.year.to_numpy(dtype=float),x.q25.to_numpy(dtype=float),
                            x.q75.to_numpy(dtype=float),color=SITE_COLORS.get(site,'#78909c'),alpha=.10)
        ax.set_ylabel('Source units (verify)')
    axes[0].legend(frameon=False,ncol=max(1,min(6,len(keep.site.unique()))),loc='upper right')
    axes[-1].set_xlabel('Scheduled sampling year')
    axes[-1].xaxis.set_major_locator(MaxNLocator(integer=True))
    fig.suptitle('Exploratory trends from valid noon snapshots (n>=3/year); not annual means',
                 fontsize=12,color=NAVY,weight='bold',y=.997)
    fig.tight_layout();finish(fig,out,'figure_12_selected_parameter_descriptives.png')

def station_year_plot(station_year,stationmeta,out,focus):
    d=station_year.copy()
    if focus:d=d[d.site.isin(focus)]
    score=d.groupby('station_identity_id').strict_months.sum().sort_values(ascending=False)
    ids=score.index.tolist()[:65]
    if not ids:return
    d=d[d.station_identity_id.isin(ids)].copy()
    d['label']=d.site+' | '+d.station_identity_id.str[-12:]
    d['ratio']=d.strict_months/d.planned_months
    mat=d.pivot(index='label',columns='year',values='ratio')
    order=[f"{stationmeta.set_index('station_identity_id').loc[x,'site']} | {x[-12:]}" for x in ids]
    mat=mat.reindex(index=order,columns=sorted(mat.columns))
    fig,ax=plt.subplots(figsize=(9,max(6,len(ids)*.20+2)))
    cm=plt.get_cmap('YlGnBu').copy();cm.set_bad('#E7ECEF')
    im=ax.imshow(np.ma.masked_invalid(mat.to_numpy(float)),aspect='auto',cmap=cm,vmin=0,vmax=1)
    theme(ax,'Station × year: strict monthly noon continuity')
    ax.set_xticks(np.arange(len(mat.columns)),mat.columns)
    ax.set_yticks(np.arange(len(mat.index)),mat.index,fontsize=6)
    ax.grid(False);fig.colorbar(im,ax=ax,shrink=.75).set_label('Valid core / planned months')
    fig.tight_layout();finish(fig,out,'figure_10_station_year_coverage.png')

def continuity_plot(stations,out,focus_sites):
    ss=stations.copy()
    if focus_sites: ss=ss[ss.site.isin(focus_sites)]
    ss=ss.sort_values(['strict_noon_months','site','station'],ascending=[False,True,True]).head(45)
    if ss.empty:return
    fig,ax=plt.subplots(figsize=(12,max(5,len(ss)*.22+2)))
    theme(ax,'Station continuity: usable monthly noon snapshots')
    yy=np.arange(len(ss))
    lab=[f'{r.site} | {str(r.station_identity_id)[-12:]}' for r in ss.itertuples()]
    ax.barh(yy,ss.strict_noon_months.to_numpy(),color=[SITE_COLORS.get(s,'#728A98') for s in ss.site],height=.7)
    ax.scatter(ss.planned_noon_months,yy,s=12,color=NAVY,label='Number of planned months',zorder=3)
    ax.set_yticks(yy,lab,fontsize=7);ax.invert_yaxis();ax.set_xlabel('Number of planned/retrieved months (not continuous averages)')
    ax.legend(frameon=False,fontsize=8)
    fig.tight_layout();finish(fig,out,'figure_06_station_monthly_continuity.png')

def shape_membership(stations,boundaries,out):
    """Point-in-polygon: spatial assignment only, NOT hydrological connectivity."""
    import geopandas as gpd
    from shapely.geometry import Point
    shapes=sorted(boundaries.rglob('*.shp'))
    if not shapes:
        raise ValueError('No .shp files found under '+str(boundaries))
    shape_geoms=[];notes=[]
    for f in shapes:
        try:
            g=gpd.read_file(f)
            if g.empty: raise ValueError('No features')
            if g.crs is None: raise ValueError('missing CRS: refusing to assume a coordinate system')
            g=g.to_crs('EPSG:4326')
            try: g.geometry=g.geometry.make_valid()
            except Exception: g.geometry=g.geometry.buffer(0)
            g=g[g.geometry.notnull() & ~g.geometry.is_empty]
            poly=g.geometry.union_all() if hasattr(g.geometry,'union_all') else g.geometry.unary_union
            name=f.stem
            shape_geoms.append((name,poly))
            notes.append({'shapefile':str(f),'status':'loaded','n_features':len(g),'crs_read':str(g.crs)})
        except Exception as e:
            notes.append({'shapefile':str(f),'status':'FAILED: '+str(e),'n_features':0,'crs_read':''})
    if not shape_geoms: raise ValueError('No valid polygon layers could be loaded')
    rows=[]
    for r in stations.itertuples():
        matches=[]
        if pd.notna(r.longitude) and pd.notna(r.latitude):
            pt=Point(float(r.longitude),float(r.latitude))
            matches=[name for name,p in shape_geoms if p.intersects(pt)]
        for name in matches: rows.append({'station_identity_id':r.station_identity_id,'site':r.site,'polygon':name})
    mem=pd.DataFrame(rows,columns=['station_identity_id','site','polygon'])
    save_table(mem,out,'spatial_polygon_station_membership')
    save_table(pd.DataFrame(notes),out,'spatial_shapefile_load_log')
    names=mem.groupby('station_identity_id')['polygon'].agg(lambda a:'; '.join(sorted(a))).to_dict() if not mem.empty else {}
    cnt=mem.groupby('station_identity_id').size().to_dict() if not mem.empty else {}
    stations=stations.copy()
    stations['polygon_membership']=stations.station_identity_id.map(names).fillna('')
    stations['n_polygon_matches']=stations.station_identity_id.map(cnt).fillna(0).astype(int)
    stations['inside_any_polygon']=stations.n_polygon_matches>0
    stations['multiple_polygon_matches']=stations.n_polygon_matches>1
    return stations,shape_geoms,mem

def spatial_plot(stations,out,shape_geoms=None):
    ss=stations.dropna(subset=['longitude','latitude']).copy()
    if ss.empty:return
    fig,ax=plt.subplots(figsize=(12,8));theme(ax,'Monitoring station locations and valid monthly-noon coverage')
    if shape_geoms:
        from shapely.geometry import Polygon,MultiPolygon,GeometryCollection
        def iter_poly(g):
            if g.geom_type=='Polygon': yield g
            elif hasattr(g,'geoms'):
                for sub in g.geoms: yield from iter_poly(sub)
        for name,poly in shape_geoms:
            for pg in iter_poly(poly):
                xx,yy=pg.exterior.xy
                ax.fill(xx,yy,color='#E7F2F0',alpha=.45,zorder=0)
                ax.plot(xx,yy,color='#7BAE9F',lw=.8,zorder=1)
    for site,g in ss.groupby('site'):
        size=(20+2.7*g.strict_noon_months.to_numpy())
        ax.scatter(g.longitude,g.latitude,s=size,edgecolor='white',linewidth=.5,
                   color=SITE_COLORS.get(site,'#798F9A'),alpha=.85,label=site,zorder=3)
    ax.set_xlabel('Longitude (WGS84)');ax.set_ylabel('Latitude (WGS84)')
    ax.legend(loc='upper left',ncol=2,fontsize=8,frameon=True,facecolor='white',edgecolor='#E7EDF0')
    ax.text(.01,-.11,'Point-in-polygon does not establish watershed connectivity; source coordinates require verification.',
            transform=ax.transAxes,fontsize=8,color=SLATE)
    fig.tight_layout();finish(fig,out,'figure_07_spatial_station_coverage.png')

def history_update(out, totals, m):
    path=out/'coverage_refresh_history.csv'
    fingerprint=hashlib.sha256('\n'.join((m[['request_id','status']].sort_values('request_id').fillna('').astype(str).agg(':'.join,axis=1)).tolist()).encode()).hexdigest()
    cols=['refreshed_at_utc','fingerprint','plan','ok','no_record','skipped','pending','other','strict_core_observations']
    if path.exists():
        past=pd.read_csv(path)
        for c in cols:
            if c not in past: past[c]=None
    else: past=pd.DataFrame(columns=cols)
    if fingerprint not in past['fingerprint'].astype(str).tolist():
        past.loc[len(past)]=[datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ'),fingerprint,
            totals['planned'],totals['ok'],totals['no_record'],totals['skipped'],totals['pending'],
            totals['other'],totals['strict_core_observations']]
        past.to_csv(path,index=False)
    if len(past)>1:
        p=past.sort_values('refreshed_at_utc')
        fig,ax=plt.subplots(figsize=(12,4.8));theme(ax,'Acquisition progress across saved coverage snapshots')
        t=np.arange(len(p))
        for col,lab,color in [('ok','Retrieved','#16758A'),('no_record','No record','#DCA044'),('skipped','Conditional skip','#A7B0B7'),('pending','Pending','#D2DBE0')]:
            ax.plot(t,p[col],label=lab,color=color,lw=2,marker='o',markersize=3)
        ax.set_xticks(t,p.refreshed_at_utc.str.slice(0,10),rotation=25,ha='right');ax.set_ylabel('Request identities')
        ax.legend(frameon=False,ncol=4);fig.tight_layout();finish(fig,out,'figure_08_acquisition_history.png')
    return past

def markdown_report(out, totals, stage_counts, month, stations, params_year, daily, spatial_done, n_shapes, focus):
    t=totals
    def iv(k):return f'{int(t[k]):,}'
    stage_display=stage_counts.copy()
    stage_display['terminal']=stage_display.get('ok',0)+stage_display.get('no_record',0)+stage_display.get('skipped_noon_missing',0)
    stage_display['completion_%']=(stage_display.terminal/stage_display.total*100).round(1)
    cols=[c for c in ['stage','total','ok','no_record','skipped_noon_missing','pending','terminal','completion_%'] if c in stage_display]
    try:
        vals=stage_display[cols].to_markdown(index=False)
    except ImportError:
        rows=[' | '.join(cols),' | '.join(['---']*len(cols))]
        rows += [' | '.join(map(str, r)) for r in stage_display[cols].itertuples(index=False,name=None)]
        vals='\n'.join(rows)
    dt=month.sort_values('month')
    din=daily[daily.strict_six==True]
    lines=[
      '# Qingyue acquisition and scientific coverage report',
      f'\nGenerated (UTC): {datetime.now(timezone.utc).isoformat(timespec="seconds")}',
      '\n## Scope and denominators',
      f'\n- Planned request identities: **{iv("planned")}**.',
      f'- `ok`: **{iv("ok")}**; `no_record`: **{iv("no_record")}**; `skipped_noon_missing`: **{iv("skipped")}**; pending: **{iv("pending")}**.',
      f'- Observation rows matched to `ok`: **{iv("observations")}**, with **{iv("strict_core_observations")}** meeting strict seven-core/normal/timestamp QC.',
      f'- Station identities in the approved request plan: **{len(stations):,}**.',
      '\nThe plan measures *request opportunities*, not independent time series or ecological samples. Noon requests are scheduled point measurements (typically the 15th at 12:00), not monthly means.',
      '\n## Stage outcomes', '\n'+vals,
      '\n![Stage outcomes](figure_01_stage_outcomes.png)',
      '\n## Spatial–temporal monthly-noon coverage',
      f'\n- Earliest scheduled month: **{dt.month.min()}**; latest: **{dt.month.max()}**.',
      '- Heatmap denominator = planned monthly-noon station-month request identities; unplanned area-month cells are grey. Monthly-only and supplementary-hour records are not mixed.',
      '\n![Monthly counts](figure_02_monthly_noon_count.png)',
      '\n![Area-month heatmap](figure_03_site_month_coverage_heatmap.png)',
      '- Caution: 2026 records may have blank `section_status` despite numeric values. Compare the following numeric-only plot with the strict-normal-status plot; do not label all blank-status measurements absent.',
      '\n![Numeric-core availability](figure_11_site_month_numeric_core.png)',
      '\n## Parameter coverage',
      '- The parameter heatmap denominator is *successfully retrieved observations*, not planned requests. Nonnegative numeric values are distinguished from strict normal-status observations in CSV outputs.',
      '- Negative source placeholders (e.g., -1, -2) and blanks are not counted as numeric-valid. Zero is retained. Detection-limit flags are NOT interpreted as independently verified analytical limits.',
      '\n![Parameter coverage](figure_04_parameter_availability.png)',
      '- The `13_spacetime_parameter_coverage_cube.csv` file provides the joint site × year-month × monitoring-design × parameter denominators needed for future modelling.',
      '- Files `15_parameter_descriptives_numeric.csv` and `16_parameter_descriptives_strict_normal.csv` provide n, mean, SD, median, quartiles, 5th and 95th percentiles by site, year and monitoring design. These are descriptive summaries of sampled snapshots.',
      '\n![Descriptive parameter trends](figure_12_selected_parameter_descriptives.png)',
      '\n## Six-point profiles',
      f'\n- Planned daily profiles (noon + five supplementary hours): **{len(daily):,}**.',
      f'- Six HTTP-successful records: **{int(daily.all_six_ok.sum()):,}**; six strict core-valid records: **{int(daily.strict_six.sum()):,}**.',
      '- These are six *samples* across one day, not a continuous 24-hour time series or daily mean.',
      '\n![Six-point coverage](figure_05_diurnal_profile_completeness.png)',
      '\n## Spatial locations and watershed provenance',
      f'\n- Shape-file layers successfully loaded: **{n_shapes}**. Point-in-polygon overlay enabled: **{"yes" if spatial_done else "no"}**.',
      '- Study-site labels are archive metadata, not certified hydrological assignments. Polygon membership also does not establish that a station drains to the mapped coastal area.',
      '- When polygon files are supplied, `14_polygon_spacetime_parameter_cube.csv` reports polygon × year-month × parameter, based on station-specific membership. Overlapping boundaries can legitimately count the same station in more than one polygon.',
      '- Stations with no returned observations remain on the map when their approved plan has coordinates.',
      '\n![Station map](figure_07_spatial_station_coverage.png)',
      '\n![Station-year continuity](figure_10_station_year_coverage.png)',
      '\n## Data interpretation and next acquisition',
      '- Prioritise effective *station × month × parameter* temporal continuity within the verified 18 study polygons. Do not infer increased spatial representativeness from API successes.',
      '- Distinguish `no_record` from absent planned measurements and from missing parameter values in a returned observation.',
      '- The January 2026 noon gap and January 2025 hourly QC issue require source/provenance review rather than interpolation.',
      '- Outside-catchment controls should be chosen only with an explicit matching design (climate, river type, baseline quality and management history).',
      '\n## Refresh procedure',
      '`python qingyue_historical_executor.py export`, then rerun `qingyue_coverage.py` using the refreshed manifest and observations into this same output directory.',
      'The coverage_refresh_history.csv file stores one row per distinct status snapshot, and the progress figure is produced from the second snapshot onward.',
    ]
    if spatial_done:lines += ['\n![Polygon-by-month coverage](figure_09_polygon_month_coverage.png)']
    if not din.empty:
      lines.insert(next(i for i,v in enumerate(lines) if '## Data interpretation and next acquisition' in v),
                   f'- Among strictly valid profiles, the longest observed DO range is {pd.to_numeric(din.DO_range,errors="coerce").max():.3f} source units (outliers require manual review).')
    (out/'COVERAGE_REPORT.md').write_text('\n'.join(lines)+'\n',encoding='utf-8')


def html_dashboard(out, totals, stage_counts, spatial_done):
    """Self-contained offline HTML index, regenerated on each run."""
    def esc(value): return html.escape(str(value))
    s=stage_counts.copy()
    s['terminal']=s.get('ok',0)+s.get('no_record',0)+s.get('skipped_noon_missing',0)
    s['terminal_pct']=(s.terminal/s.total*100).round(1)
    cols=['stage','total','ok','no_record','skipped_noon_missing','pending','terminal_pct']
    rows=[]
    for r in s.to_dict('records'):
        cells=[]
        for c in cols:
            v=r.get(c,0)
            label=f'{v:.1f}%' if c=='terminal_pct' else (f'{v:,}' if isinstance(v,(int,np.integer)) else str(v))
            cells.append('<td>'+esc(label)+'</td>')
        rows.append('<tr>'+''.join(cells)+'</tr>')
    imgs=[('Acquisition outcomes','figure_01_stage_outcomes.png'),
          ('Monthly noon station counts','figure_02_monthly_noon_count.png'),
          ('Monthly site strict coverage','figure_03_site_month_coverage_heatmap.png'),
          ('Monthly site numeric coverage','figure_11_site_month_numeric_core.png'),
          ('Parameter availability','figure_04_parameter_availability.png'),
          ('Six-point completeness','figure_05_diurnal_profile_completeness.png'),
          ('Station-year continuity','figure_10_station_year_coverage.png'),
          ('Station location map','figure_07_spatial_station_coverage.png'),
          ('Selected water-quality parameter summaries','figure_12_selected_parameter_descriptives.png')]
    if spatial_done:imgs.append(('Polygon-by-month coverage','figure_09_polygon_month_coverage.png'))
    if (out/'figure_08_acquisition_history.png').exists():imgs.append(('Acquisition history','figure_08_acquisition_history.png'))
    cards=[]
    for name,file in imgs:
        cards.append(f'<article class="viz"><h3>{esc(name)}</h3><a href="{file}"><img loading="lazy" src="{file}" alt="{esc(name)}"></a><p><a href="{Path(file).stem}.pdf">Vector PDF</a> · <a href="{file}">PNG</a></p></article>')
    metrics=[('Planned requests',totals['planned']),('Retrieved observations',totals['ok']),
             ('No record',totals['no_record']),('Zero-call skips',totals['skipped']),
             ('Pending requests',totals['pending']),('Core-strict records',totals['strict_core_observations']),
             ('Station identities',totals['station_identities']),('Strict six-point profiles',totals['sixpoint_strict_days'])]
    metrics_html=''.join(f'<div class="kpi"><small>{esc(k)}</small><strong>{v:,}</strong></div>' for k,v in metrics)
    files=[('Stage accounting','01_acquisition_by_stage.csv'),('Site accounting','02_acquisition_by_study_site.csv'),
        ('Station-month statuses','04_noon_station_month_matrix.csv'),('Station locations and continuity','05_station_spatial_temporal_coverage.csv'),
        ('Parameters by year','06_parameter_coverage_by_year.csv'),('Parameters by site','07_parameter_coverage_by_site.csv'),
        ('Measurement QC','08_measurement_qc_by_site_year.csv'),('Six-point daily coverage','09_sixpoint_station_day_coverage.csv'),
        ('Station-year coverage','12_station_year_temporal_coverage.csv'),('Space–time–parameter cube','13_spacetime_parameter_coverage_cube.csv'),
        ('Strict measurement summaries','16_parameter_descriptives_strict_normal.csv'),('Water-class availability','17_water_quality_class_availability.csv')]
    if spatial_done:
        files += [('Polygon station membership','spatial_polygon_station_membership.csv'),('Polygon-month coverage','11_polygon_month_coverage.csv'),
                  ('Polygon space–time–parameter cube','14_polygon_spacetime_parameter_cube.csv')]
    links=''.join(f'<li><a href="{file}">{esc(name)}</a></li>' for name,file in files)
    template_path=Path(__file__).with_name('dashboard_template.html')
    if not template_path.exists():
        raise FileNotFoundError('HTML template is missing alongside qingyue_coverage.py: '+str(template_path))
    text=template_path.read_text(encoding='utf-8')
    for key,value in {
        '{{GENERATED}}': datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M'),
        '{{METRICS}}':metrics_html,'{{HEADERS}}':''.join('<th>'+esc(x.replace('_',' '))+'</th>' for x in cols),
        '{{STAGES}}':''.join(rows),'{{FIGCARDS}}':''.join(cards),'{{FILELINKS}}':links
    }.items():text=text.replace(key,value)
    (out/'COVERAGE_DASHBOARD.html').write_text(text,encoding='utf-8')

def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--manifest',type=Path,required=True)
    ap.add_argument('--observations',type=Path,required=True)
    ap.add_argument('--plan',type=Path,default=None,help='Recommended: immutable approved plan with coordinates')
    ap.add_argument('--output',type=Path,required=True)
    ap.add_argument('--boundaries-dir',type=Path,default=None,help='Optional directory containing 18 shapefiles; requires geopandas')
    ap.add_argument('--focus-sites',default='XMB,MJE,QZB,DSB',help='Comma-separated site codes for top-station plot')
    args=ap.parse_args();out=args.output;out.mkdir(parents=True,exist_ok=True)
    focus=[s.strip() for s in args.focus_sites.split(',') if s.strip()]
    m=read_data(args.manifest,'manifest');o=read_data(args.observations,'observations')
    if not m.request_id.is_unique or not o.request_id.is_unique:raise SystemExit('STOP: duplicate request_id in supplied export')
    if args.plan:
        p=read_data(args.plan,'plan')
        if not p.request_id.is_unique or len(p)!=len(m) or set(p.request_id)!=set(m.request_id):
            raise SystemExit('STOP: plan and manifest request identity sets mismatch')
        for c in ['plan_component','station_identity_id','target_datetime','study_site_code']:
            check=p[['request_id',c]].merge(m[['request_id',c]],on='request_id',suffixes=('_p','_m'))
            if not (check[c+'_p'].astype(str)==check[c+'_m'].astype(str)).all():
                raise SystemExit(f'STOP: plan/manifest mismatch for {c}')
    else:
        p=m.copy()
        print('WARNING: --plan omitted. All-station map may omit never-retrieved station coordinates.')
    expected=set(m.loc[m.status=='ok','request_id'])
    if set(o.request_id)!=expected:
        raise SystemExit(f'STOP: observations inconsistent with manifest OK: missing={len(expected-set(o.request_id))}, extra={len(set(o.request_id)-expected)}')
    m['status']=m.status.fillna('unknown').astype(str).apply(safe_status)
    m['stage']=stage_of(m);m['year']=pd.to_numeric(m.target_datetime.astype(str).str[:4],errors='coerce')
    m['month']=m.target_datetime.astype(str).str[:7]
    m['date']=m.target_datetime.astype(str).str[:10]
    m['hour']=pd.to_numeric(m.target_datetime.astype(str).str[11:13],errors='coerce')
    m['site']=m.study_site_code.astype(str)
    p=p.copy();p['site']=p.study_site_code.astype(str)
    o=o.copy();o['year']=pd.to_numeric(o.target_datetime.astype(str).str[:4],errors='coerce')
    o['month']=o.target_datetime.astype(str).str[:7];o['site']=o.study_site_code.astype(str)
    o['date']=o.target_datetime.astype(str).str[:10];o['hour']=pd.to_numeric(o.target_datetime.astype(str).str[11:13],errors='coerce')
    o['time_match']=o.target_datetime.astype(str)==o.monitor_time.astype(str)
    o['status_normal']=o.section_status.fillna('').astype(str).str.strip().eq('正常')
    o['core_valid']=True
    for v in PARAMS:
        if v not in o.columns:o[v]=np.nan
        o[v]=pd.to_numeric(o[v],errors='coerce')
        o[v+'_valid']=np.isfinite(o[v])&(o[v]>=0)
        if v=='ph': o[v+'_valid'] &= (o[v]<=14)
        if v in CORE:o['core_valid'] &= o[v+'_valid']
    o['core_valid'] &= (o['ph']<=14)
    o['core_strict']=o.core_valid & o.time_match & o.status_normal
    o['qc_status_group']=np.select([o.status_normal,o.section_status.isna()|o.section_status.astype(str).str.strip().eq('')],['normal','blank'],default='other/maintenance')
    # Plan/manifest order preserved for a join; extra observation columns are attached by ID.
    attached=['request_id','core_valid','core_strict','time_match','status_normal','qc_status_group','dissolvedoxygen','ph','water_temp']
    m=m.merge(o[attached],on='request_id',how='left',validate='one_to_one')
    m['core_strict']=m.core_strict.eq(True).astype(bool)
    m['core_valid']=m.core_valid.eq(True).astype(bool)
    # Stage and network accounting
    stage_counts=(m.groupby(['stage','status']).size().unstack(fill_value=0)
                 .reindex(columns=list(STATUSES)+sorted(set(m.status)-set(STATUSES)),fill_value=0))
    stage_counts['total']=stage_counts.sum(axis=1)
    stage_counts=stage_counts.reset_index()
    stage_counts['stage']=pd.Categorical(stage_counts.stage,STAGE_ORDER,ordered=True)
    stage_counts=stage_counts.sort_values('stage');stage_counts['stage']=stage_counts.stage.astype(str)
    save_table(stage_counts,out,'01_acquisition_by_stage')
    site_status=(m.groupby(['site','status']).size().unstack(fill_value=0).reset_index())
    save_table(site_status,out,'02_acquisition_by_study_site')
    monthly_m=m[m.plan_component=='monthly_noon_backbone'].copy()
    monthly_m['strict_core']=monthly_m.core_strict.astype(int)
    monthly_m['numeric_core']=monthly_m.core_valid.astype(int)
    grouped=monthly_m.groupby(['site','month']).agg(
        planned=('request_id','size'),ok=('status',lambda x:(x=='ok').sum()),
        no_record=('status',lambda x:(x=='no_record').sum()),
        pending=('status',lambda x:(x=='pending').sum()), strict_core=('strict_core','sum'), numeric_core=('numeric_core','sum')
    ).reset_index()
    for c in ['ok','strict_core','numeric_core']:
        grouped[c+'_rate']=grouped[c]/grouped.planned
    save_table(grouped,out,'03_noon_coverage_by_site_month')
    station_month=monthly_m[['station_identity_id','site','month','year','status','core_strict','request_id']].copy()
    save_table(station_month,out,'04_noon_station_month_matrix')
    station_year=station_month.groupby(['station_identity_id','site','year'],as_index=False).agg(planned_months=('request_id','size'),ok_months=('status',lambda x:(x=='ok').sum()),strict_months=('core_strict','sum'))
    save_table(station_year,out,'12_station_year_temporal_coverage')
    # coordinate provenance from approved plan, *not* from observations only
    station_fields=['station_identity_id','site']
    for col in ['cnemc_station_name','qingyue_province','qingyue_city','qingyue_valley','longitude','latitude']:
        if col in p.columns: station_fields.append(col)
    stationmeta=p[station_fields].drop_duplicates('station_identity_id').copy()
    stationmeta=stationmeta.rename(columns={'cnemc_station_name':'station','qingyue_province':'province',
                  'qingyue_city':'city','qingyue_valley':'river_basin'})
    for col in ['station','province','city','river_basin','longitude','latitude']:
        if col not in stationmeta:stationmeta[col]=np.nan
    stationmeta['longitude']=pd.to_numeric(stationmeta.longitude,errors='coerce')
    stationmeta['latitude']=pd.to_numeric(stationmeta.latitude,errors='coerce')
    coords=p.groupby('station_identity_id').agg(n_longitude=('longitude','nunique'),n_latitude=('latitude','nunique')) if 'longitude' in p else None
    if coords is not None:
        stationmeta=stationmeta.merge(coords.reset_index(),on='station_identity_id',how='left')
    stationmeta['coordinate_conflict']=(stationmeta.get('n_longitude',1)>1)|(stationmeta.get('n_latitude',1)>1)
    stat=station_month.groupby('station_identity_id').agg(
        planned_noon_months=('request_id','size'),ok_noon_months=('status',lambda x:(x=='ok').sum()),
        strict_noon_months=('core_strict','sum'),no_record_months=('status',lambda x:(x=='no_record').sum()),
        pending_months=('status',lambda x:(x=='pending').sum())).reset_index()
    stat['strict_noon_months']=stat.strict_noon_months.astype(int)
    stationmeta=stationmeta.merge(stat,on='station_identity_id',how='left').fillna({'planned_noon_months':0,'ok_noon_months':0,'strict_noon_months':0,'no_record_months':0,'pending_months':0})
    validmonths=station_month.loc[station_month.core_strict].copy()
    validmonths['month_idx']=pd.to_datetime(validmonths.month+'-01').dt.year*12+pd.to_datetime(validmonths.month+'-01').dt.month
    run=validmonths.groupby('station_identity_id').month_idx.agg(lambda x:longest_run(x.to_list())).to_dict()
    stationmeta['longest_valid_month_streak']=stationmeta.station_identity_id.map(run).fillna(0).astype(int)
    firstlast=validmonths.groupby('station_identity_id').agg(first_valid_month=('month','min'),last_valid_month=('month','max')).reset_index()
    stationmeta=stationmeta.merge(firstlast,on='station_identity_id',how='left')
    stationmeta['monthly_core_coverage_rate']=np.where(stationmeta.planned_noon_months>0,stationmeta.strict_noon_months/stationmeta.planned_noon_months,np.nan)
    # mark suspect site labels only by uncertainty, never assign catchments from a code
    shape_geoms=None;spatial_done=False;n_shapes=0
    if args.boundaries_dir:
        if not args.boundaries_dir.is_dir():raise FileNotFoundError('Boundary directory: '+str(args.boundaries_dir))
        stationmeta,shape_geoms,member=shape_membership(stationmeta,args.boundaries_dir,out)
        spatial_done=True;n_shapes=len(shape_geoms)
    save_table(stationmeta,out,'05_station_spatial_temporal_coverage')
    # Parameter numerical completeness, with denominator across successfully retrieved rows
    o_long=o.melt(id_vars=['request_id','station_identity_id','site','year','month','plan_component','time_match','status_normal'],
                  value_vars=list(PARAMS),var_name='parameter',value_name='value')
    o_long['numeric_valid']=np.isfinite(o_long.value)&(o_long.value>=0)
    o_long.loc[o_long.parameter.eq('ph'),'numeric_valid'] &= o_long.loc[o_long.parameter.eq('ph'),'value']<=14
    o_long['strict_parameter']=o_long.numeric_valid&o_long.time_match&o_long.status_normal
    byyear=o_long.groupby(['plan_component','year','parameter'],as_index=False).agg(
        retrieved=('request_id','size'),numeric_valid=('numeric_valid','sum'),
        normal_status_valid=('strict_parameter','sum'))
    byyear=byyear.rename(columns={'plan_component':'component'})
    byyear['numeric_rate']=np.where(byyear.retrieved>0,byyear.numeric_valid/byyear.retrieved*100,np.nan)
    byyear['normal_rate']=np.where(byyear.retrieved>0,byyear.normal_status_valid/byyear.retrieved*100,np.nan)
    save_table(byyear,out,'06_parameter_coverage_by_year')
    bysite=o_long.groupby(['site','plan_component','parameter'],as_index=False).agg(
        retrieved=('request_id','size'),numeric_valid=('numeric_valid','sum'),
        normal_status_valid=('strict_parameter','sum')).rename(columns={'plan_component':'component'})
    bysite['numeric_rate']=np.where(bysite.retrieved>0,bysite.numeric_valid/bysite.retrieved*100,np.nan)
    bysite['normal_rate']=np.where(bysite.retrieved>0,bysite.normal_status_valid/bysite.retrieved*100,np.nan)
    save_table(bysite,out,'07_parameter_coverage_by_site')
    # Descriptive values with explicit observation-level QC filters.
    def grouped_descriptives(d, fname):
        keys=['site','year','plan_component','parameter']
        if d.empty:
            empty=pd.DataFrame(columns=keys+['n','mean','sd','min','q05','q25','median','q75','q95','max'])
            return save_table(empty,out,fname)
        g=d.groupby(keys).value.agg(n='size',mean='mean',sd='std',min='min',max='max').reset_index()
        q=d.groupby(keys).value.quantile([.05,.25,.50,.75,.95]).unstack(-1).reset_index()
        q.columns=keys+['q05','q25','median','q75','q95']
        g=g.merge(q,on=keys).rename(columns={'plan_component':'component'})
        return save_table(g,out,fname)
    descriptive_numeric=grouped_descriptives(o_long[o_long.numeric_valid & o_long.time_match],
                             '15_parameter_descriptives_numeric')
    descriptive_strict=grouped_descriptives(o_long[o_long.strict_parameter],
                             '16_parameter_descriptives_strict_normal')
    # Class indicators are categorical; 2026 coding differs from earlier Roman numeral labels.
    cl=o.copy()
    label=cl['qulity'].fillna('').astype(str).str.strip() if 'qulity' in cl else pd.Series('',index=cl.index)
    cl['class_present']=~label.isin(['','-','-1','-2','nan','NA'])
    class_summary=cl.groupby(['site','year','plan_component'],as_index=False).agg(
       retrieved=('request_id','size'),class_present=('class_present','sum'),
       class_with_normal_status=('status_normal',lambda x:int(x.sum())))
    class_summary['class_coverage']=class_summary.class_present/class_summary.retrieved
    save_table(class_summary,out,'17_water_quality_class_availability')
    # Multi-dimensional coverage cube: site × time × parameter × sample design.
    cube_planned=m.groupby(['site','year','month','plan_component'],as_index=False).agg(planned_requests=('request_id','size'),retrieved_requests=('status',lambda x:(x=='ok').sum()))
    cube_planned['join_key']=1
    parm=pd.DataFrame({'parameter':PARAMS,'join_key':1})
    cube_planned=cube_planned.merge(parm,on='join_key').drop(columns='join_key')
    cube_obs=o_long.groupby(['site','year','month','plan_component','parameter'],as_index=False).agg(numeric_valid=('numeric_valid','sum'),status_normal_valid=('strict_parameter','sum'))
    cube=cube_planned.merge(cube_obs,on=['site','year','month','plan_component','parameter'],how='left')
    cube[['numeric_valid','status_normal_valid']]=cube[['numeric_valid','status_normal_valid']].fillna(0).astype(int)
    cube['numeric_per_retrieved']=np.where(cube.retrieved_requests>0,cube.numeric_valid/cube.retrieved_requests,np.nan)
    cube['strict_per_planned']=cube.status_normal_valid/cube.planned_requests
    save_table(cube,out,'13_spacetime_parameter_coverage_cube')
    if spatial_done:
        # Membership is station-specific. Join before aggregation to avoid assigning whole site codes to polygons.
        poly_plan=m[['request_id','station_identity_id','year','month','plan_component','status']].merge(
            member,on='station_identity_id',how='inner')
        poly_base=poly_plan.groupby(['polygon','year','month','plan_component'],as_index=False).agg(
            planned_requests=('request_id','size'),retrieved_requests=('status',lambda x:(x=='ok').sum()))
        poly_base['join_key']=1
        poly_base=poly_base.merge(parm,on='join_key').drop(columns='join_key')
        poly_obs=o_long.merge(member[['station_identity_id','polygon']],on='station_identity_id',how='inner')
        poly_obs=poly_obs.groupby(['polygon','year','month','plan_component','parameter'],as_index=False).agg(
            numeric_valid=('numeric_valid','sum'),status_normal_valid=('strict_parameter','sum'))
        poly_cube=poly_base.merge(poly_obs,on=['polygon','year','month','plan_component','parameter'],how='left')
        poly_cube[['numeric_valid','status_normal_valid']]=poly_cube[['numeric_valid','status_normal_valid']].fillna(0).astype(int)
        poly_cube['strict_per_planned']=poly_cube.status_normal_valid/poly_cube.planned_requests
        save_table(poly_cube,out,'14_polygon_spacetime_parameter_cube')
    quality=o.groupby(['year','site','plan_component','qc_status_group'],as_index=False).agg(
        retrieved=('request_id','size'),numeric_seven_core=('core_valid','sum'),strict_seven_core=('core_strict','sum'),
        time_mismatch=('time_match',lambda x:(~x).sum()))
    save_table(quality,out,'08_measurement_qc_by_site_year')
    # Full six-point profile enumeration from five planned extra hours, including 2020 deferred cohort
    extras=m[m.plan_component=='semiannual_full_day_extra'].copy()
    noon=m[m.plan_component=='monthly_noon_backbone'][['station_identity_id','date','status','core_strict','dissolvedoxygen']].copy()
    noon=noon.rename(columns={'status':'noon_status','core_strict':'noon_strict','dissolvedoxygen':'noon_DO'})
    extras=extras.merge(noon,on=['station_identity_id','date'],how='left',validate='many_to_one')
    if extras.noon_status.isna().any():raise SystemExit('STOP: extra hour has no approved noon dependency')
    groupcols=['station_identity_id','site','date']
    days=extras.groupby(groupcols).agg(
        expected_extra=('request_id','size'),extra_ok=('status',lambda x:(x=='ok').sum()),
        extra_no_record=('status',lambda x:(x=='no_record').sum()),
        extra_skipped=('status',lambda x:(x=='skipped_noon_missing').sum()),
        extra_pending=('status',lambda x:(x=='pending').sum()),
        extra_strict=('core_strict','sum'),noon_status=('noon_status','first'),
        noon_strict=('noon_strict','first'),noon_DO=('noon_DO','first')).reset_index()
    days['noon_strict']=days.noon_strict.fillna(False).astype(bool)
    days['all_six_ok']=days.noon_status.eq('ok')&days.extra_ok.eq(days.expected_extra)&days.expected_extra.eq(5)
    days['strict_six']=days.noon_strict&days.extra_strict.eq(5)&days.expected_extra.eq(5)
    days['year']=days.date.str[:4].astype(int)
    days['season']=np.where(days.date.str[5:7].eq('01'),'January',np.where(days.date.str[5:7].eq('07'),'July','Other'))
    strict_extra=o[o.core_strict&(o.plan_component=='semiannual_full_day_extra')][['station_identity_id','date','dissolvedoxygen']]
    do_range=strict_extra.groupby(['station_identity_id','date']).dissolvedoxygen.agg(min_do='min',max_do='max').reset_index()
    days=days.merge(do_range,on=['station_identity_id','date'],how='left')
    days['min_do']=days[['min_do','noon_DO']].min(axis=1)
    days['max_do']=days[['max_do','noon_DO']].max(axis=1)
    days['DO_range']=np.where(days.strict_six,days.max_do-days.min_do,np.nan)
    save_table(days,out,'09_sixpoint_station_day_coverage')
    darea=days.groupby(['site','year','season'],as_index=False).agg(planned=('date','size'),
               all_retrieved=('all_six_ok','sum'),strict_six=('strict_six','sum'))
    save_table(darea,out,'10_sixpoint_coverage_by_site_season')
    # Polygon x month join: overlapping polygons counted separately, never sum polygon totals into a unique-station total
    if spatial_done:
        base=station_month.merge(member[['station_identity_id','polygon']],on='station_identity_id',how='inner')
        byboundary=base.groupby(['polygon','month'],as_index=False).agg(
           planned=('request_id','size'),ok=('status',lambda x:(x=='ok').sum()),strict_core=('core_strict','sum'))
        byboundary['strict_rate']=byboundary.strict_core/byboundary.planned
        save_table(byboundary,out,'11_polygon_month_coverage')
        polygon_month_plot(byboundary,out)
    # summary
    status=m.status.value_counts()
    totals=dict(planned=len(m),ok=int(status.get('ok',0)),no_record=int(status.get('no_record',0)),
               skipped=int(status.get('skipped_noon_missing',0)),pending=int(status.get('pending',0)),
               other=int(len(m)-sum(int(status.get(s,0)) for s in ['ok','no_record','skipped_noon_missing','pending'])),
               observations=len(o),strict_core_observations=int(o.core_strict.sum()),
               station_identities=len(stationmeta),sixpoint_strict_days=int(days.strict_six.sum()),
               year_min=int(m.year.min()),year_max=int(m.year.max()),
               source_snapshot_note='Computed from provided export files; reload latest before interpretation.')
    (out/'00_coverage_summary.json').write_text(json.dumps(totals,indent=2,ensure_ascii=False),encoding='utf-8')
    stage_plot(m,out);allmonth=monthly_plots(grouped,out);parameter_plot(byyear,out)
    daily_plot(days,out);continuity_plot(stationmeta,out,focus)
    station_year_plot(station_year,stationmeta,out,focus)
    parameter_trend_plot(descriptive_strict,out,focus)
    spatial_plot(stationmeta,out,shape_geoms)
    history_update(out,totals,m)
    markdown_report(out,totals,stage_counts,allmonth,stationmeta,byyear,days,spatial_done,n_shapes,focus)
    html_dashboard(out,totals,stage_counts,spatial_done)
    print('PASS: coverage report generated (read-only source inputs).')
    print('Snapshot:',json.dumps({k:totals[k] for k in ('planned','ok','no_record','skipped','pending','observations','strict_core_observations','station_identities','sixpoint_strict_days')}))
    print('Output:',out.resolve())
    if args.boundaries_dir:
        print('Polygon layers:',n_shapes,'; geometry membership is not hydrological validation.')
    else:print('Optional spatial overlay not run: add --boundaries-dir containing the 18 .shp files.')

if __name__=='__main__':main()

"""Figure-only companion, with source-derived notes and comparable radius scales."""
from pathlib import Path
import os, json, hashlib, math
import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from reportlab.pdfgen import canvas
from reportlab.lib.styles import ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.colors import HexColor
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from PIL import Image
import pymupdf as fitz

ROOT = Path(os.environ['TALENT_DETS_DATA_DIR'])
OUT = ROOT/'results/land_grants/companions/amws_training_radius_companion_20261014'
OUT.mkdir(parents=True, exist_ok=True)
FIG = OUT/'figures'; FIG.mkdir(exist_ok=True)
PDF = OUT/'college_allocation_training_50km_100km_companion_20261014.pdf'
W,H = 841.89,595.28
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':11,'axes.spines.top':False,'axes.spines.right':False})
fonts=Path(matplotlib.get_data_path())/'fonts/ttf'
pdfmetrics.registerFont(TTFont('DejaVu',str(fonts/'DejaVuSans.ttf')))
pdfmetrics.registerFont(TTFont('DejaVu-Bold',str(fonts/'DejaVuSans-Bold.ttf')))
pdfmetrics.registerFontFamily('DejaVu',normal='DejaVu',bold='DejaVu-Bold')
colors = {'ever_nearby':'#176B8A','never_nearby':'#B35B2C','unknown':'#665894'}
labels = {'ever_nearby':'Ever nearby','never_nearby':'Never nearby','unknown':'Unknown training'}
timing_labels = {'standard_decade':'Standard decade timing','alternative_decade':'Alternative decade timing'}
balance_labels = {'relative':'Relative-time balancing','calendar':'Calendar-decade balancing'}
sources = {}; data = {}; coverage = {}; statuses = {}; warnings = {}; support = {}; pretests = {}
def sha(p): return hashlib.sha256(p.read_bytes()).hexdigest()
for b in ('relative','calendar'):
    for r in (50,100):
        stem=f'amws_training_{r}km_{"catchments" if b=="relative" else "calendar"}_no_reuse_20261014'
        res=ROOT/'results/land_grants/event_studies'/stem
        prep=ROOT/'output/land_grants'/stem
        for folder in (res,prep):
            manifest=json.loads((folder/'completion_manifest.json').read_text())
            assert manifest['validation']['passed']
            for rel,h in manifest['output_checksums'].items():
                assert sha(folder/rel)==h, str(folder/rel)
            sources[str(folder/'completion_manifest.json')]=sha(folder/'completion_manifest.json')
        key=(b,r)
        data[key]=pd.read_csv(res/'dynamic_att.csv')
        coverage[key]=pd.read_csv(prep/'analysis_coverage_summary.csv')
        statuses[key]=pd.read_csv(res/'model_status.csv')
        warnings[key]=pd.read_csv(res/'warnings.csv')
        assert len(statuses[key])==8 and statuses[key]['ok'].all()
        assert not data[key].duplicated(['timing','group','event_time']).any()
        if (res/'event_time_support.csv').exists(): support[key]=pd.read_csv(res/'event_time_support.csv')
        if (res/'pretrend_test_diagnostics.csv').exists(): pretests[key]=pd.read_csv(res/'pretrend_test_diagnostics.csv')

c=canvas.Canvas(str(PDF),pagesize=(W,H))
c.setTitle('College allocation and training: 50 km and 100 km')
c.setAuthor('GTL Talent Determinants')
page_audit=[]
note_style=ParagraphStyle('notes',fontName='DejaVu',fontSize=8.8,leading=11.3,textColor=HexColor('#30343b'))
def row(b,r,t):
    a=coverage[b,r]; return a[a.timing==t].iloc[0]
def sample_text(b,r,t):
    a=row(b,r,t)
    return f'{r} km: {int(a.events)} events, {int(a.sites)} site units, {int(a.rows)} observations and {int(a.analysis_counties)} member counties'
def warning_text(b,r,t):
    s=statuses[b,r]; s=s[s.timing==t]
    txt='Small-treatment-cohort warnings apply.'
    if (b,r) in pretests:
        q=pretests[b,r];q=q[q.timing==t]
        bad=q[~q.joint_test_available.astype(bool)]
        if len(bad):
            lo=int(bad.covariance_rank.min());hi=int(bad.covariance_rank.max())
            ranks=str(lo) if lo==hi else f'{lo}–{hi}'
            txt+=f' Joint pre-trend tests are unavailable for {len(bad)} of four outcomes (singular covariance; ranks {ranks}).'
        else: txt+=' Joint pre-trend tests are available for all four outcomes.'
    else:
        q=warnings[b,r];q=q[q.model.str.startswith(t)]
        bad=q[q.message.str.contains('singular',case=False,na=False)]
        if len(bad): txt+=' Joint pre-trend testing is unavailable for the nearby outcome because its covariance is singular.'
        else: txt+=' Joint pre-trend tests are available for all four outcomes.'
    return txt
def support_text(b,r,t):
    if b=='relative': return 'The −20 to +70 event-time window is balanced; effects subtract the common e = 0 estimate and influence function.'
    q=support[b,r]; q=q[(q.timing==t)&(q.group=='total')].sort_values('event_time')
    cohorts='/'.join(str(int(x)) for x in q.n_cohorts)
    events='/'.join(str(int(x)) for x in q.n_events)
    sites='/'.join(str(int(x)) for x in q.n_treated_sites)
    return ('Calendar decades 1830–1950 are complete; relative-time support varies. '
        f'At e = −50, −40, …, +70, native ATT cohorts: {cohorts}; events/treated sites: {events}'
        + ('' if events==sites else f' / {sites}')
        + '. Every displayed effect subtracts the common e = 0 estimate and influence function; its reference uses all 14 treated sites.')
def draw_page(section,subsection,img,note,meta):
    c.setFillColor(HexColor('#132b3a'));c.setFont('DejaVu-Bold',19)
    c.drawString(38,H-34,section)
    c.setFont('DejaVu-Bold',12);c.drawString(38,H-56,subsection)
    c.drawImage(str(img),30,177,width=W-60,height=H-252,preserveAspectRatio=True,anchor='c',mask='auto')
    p=Paragraph('<b>Notes.</b> '+note,note_style)
    pw,ph=p.wrap(W-76,152)
    assert ph<=145, (subsection,ph)
    p.drawOn(c,38,160-ph)
    page_audit.append({'page':len(page_audit)+1,'section':section,'subsection':subsection,'notes':note,'notes_height':ph,**meta})
    c.showPage()

for b in ('relative','calendar'):
    for t in timing_labels:
        dd=pd.concat([data[b,r].query('timing==@t and group!="total"') for r in (50,100)])
        limit=max(abs(dd.ci_low).max(),abs(dd.ci_high).max())*1.09
        for r in (50,100):
            fig,axs=plt.subplots(1,3,figsize=(12.8,5.25),sharex=True,sharey=True)
            dd=data[b,r];dd=dd[dd.timing==t]
            for ax,g in zip(axs,labels):
                d=dd[dd.group==g].sort_values('event_time')
                ax.axhline(0,color='#aaaaaa',lw=.8,ls='--');ax.axvline(0,color='#cccccc',lw=.7)
                ax.errorbar(d.event_time,d.estimate,yerr=[d.estimate-d.ci_low,d.ci_high-d.estimate],fmt='o-',ms=4,lw=1.1,capsize=2,color=colors[g])
                ax.set_title(labels[g],fontweight='bold',pad=12)
                ax.set_ylim(-limit,limit);ax.set_xlim(-23 if b=='relative' else -53,73)
                ax.set_xticks(np.arange(-20 if b=='relative' else -50,71,10));ax.tick_params(axis='x',labelsize=9)
                ax.set_xlabel('Years relative to allocation')
            axs[0].set_ylabel('Effect on scientists per 1,000 births')
            fig.subplots_adjust(left=.073,right=.99,bottom=.15,top=.88,wspace=.16)
            img=FIG/f'{b}_{r}km_{t}.png';fig.savefig(img,dpi=220);plt.close(fig)
            other=100 if r==50 else 50
            n_ex=7 if b=='relative' and r==50 else 99 if b=='relative' else 11 if r==50 else 39
            note=(sample_text(b,r,t)+f'; {n_ex} shared counties excluded everywhere, with no reassignment. '
                +f'Both county catchments and training proximity use {r} km inclusive; training distance is measured from each scientist’s birth-county centroid. '
                +'Ever nearby needs one assigned nearby institution; never nearby requires every reported degree to have complete, conflict-free locations; remaining cases are unknown. '
                +'BA/BS, MA, MS and PhD counts are pooled over counties and divided by pooled estimated births (×1,000). '
                +('Alternative timing shifts birth years ending in 7–9 forward one decade. ' if t=='alternative_decade' else 'Standard timing groups birth years in their calendar decade. ')
                +'CSDID regression adjustment uses g−10 pooled births and never-treated controls; bars are pointwise 95% influence-function intervals. '
                +support_text(b,r,t)+' '+warning_text(b,r,t)+' '
                +('At 100 km the Macon County, Illinois runner-up is empty and dropped; all allocation events remain. ' if b=='relative' else 'The current 14-event calendar sample is retained at both radii. ')
                +f'Comparison sample: {sample_text(b,other,t)}. Scales match across radii. '
                +'OpenAlex reference locations and provisional identities do not verify historical teaching campuses.')
            draw_page(balance_labels[b],f'{r} km · {timing_labels[t]}',img,note,{'balance':b,'radius':r,'timing':t,'kind':'categories','symmetric_y_limit':limit})
        # Separate benchmark comparison: neither confidence intervals nor standard errors are added.
        fig,ax=plt.subplots(figsize=(12.8,5.25))
        for r,color,offset in ((50,'#176B8A',-.8),(100,'#B35B2C',.8)):
            d=data[b,r];d=d[(d.timing==t)&(d.group=='total')].sort_values('event_time')
            ax.errorbar(d.event_time+offset,d.estimate,yerr=[d.estimate-d.ci_low,d.ci_high-d.estimate],fmt='o-',ms=4,lw=1.2,capsize=2,color=color,label=f'{r} km')
        ax.axhline(0,color='#aaaaaa',ls='--',lw=.8);ax.axvline(0,color='#cccccc',lw=.7)
        ax.set_xticks(np.arange(-20 if b=='relative' else -50,71,10));ax.set_xlabel('Years relative to allocation')
        ax.set_ylabel('Effect on all scientists per 1,000 births');ax.legend(frameon=False)
        fig.subplots_adjust(left=.073,right=.985,bottom=.15,top=.95)
        img=FIG/f'{b}_benchmark_{t}.png';fig.savefig(img,dpi=220);plt.close(fig)
        note=(sample_text(b,50,t)+'. '+sample_text(b,100,t)+'. '
            +'Each total is the sum of the three mutually exclusive training-category counts, divided by pooled estimated births (×1,000). '
            +'Both county inclusion and training proximity change with radius. CSDID regression adjustment, never-treated controls, g−10 pooled-birth adjustment and e = 0 influence-function normalization match the category figures. '
            +'Bars show separately estimated pointwise 95% intervals; standard errors and interval endpoints are never summed. The category effects reconcile numerically with each total. '
            +support_text(b,100,t)+' 50 km: '+warning_text(b,50,t)+' 100 km: '+warning_text(b,100,t)+' '
            +('The 100 km version removes an empty Macon County runner-up; shared-county exclusions rise from seven to 99. ' if b=='relative' else 'Shared-county exclusions rise from eleven to 39; common county membership is fixed across both timings. ')
            +'Alternative timing shifts birth years ending in 7–9 forward one decade; the calendar version excludes decade 1960. '
            +'OpenAlex reference locations do not establish historical campuses. The radius comparison also reflects different county composition.')
        draw_page(balance_labels[b],f'Total benchmark · {timing_labels[t]}',img,note,{'balance':b,'timing':t,'kind':'benchmark'})
c.save()
doc=fitz.open(PDF)
assert len(doc)==12
render=OUT/'rendered';render.mkdir(exist_ok=True)
for i,p in enumerate(doc):
    text=p.get_text()
    assert 'Notes.' in text and page_audit[i]['section'] in text
    # User requested only titles and figure notes as document text.
    assert text.index('Notes.')>text.index(page_audit[i]['section'])
    p.get_pixmap(matrix=fitz.Matrix(1.3,1.3)).save(render/f'page_{i+1:02d}.png')
thumbs=[]
for i in range(12):
    im=Image.open(render/f'page_{i+1:02d}.png').convert('RGB');im.thumbnail((600,425));thumbs.append(im)
montage=Image.new('RGB',(1800,1700),'#d9dde0')
for i,im in enumerate(thumbs):montage.paste(im,((i%3)*600,(i//3)*425))
montage.save(render/'all_pages.png')
(OUT/'page_notes_and_validation.json').write_text(json.dumps({'passed':True,'pages':page_audit,'source_checksums':sources,'pdf_sha256':sha(PDF),'page_count':len(doc),'eight_category_pages':sum(p['kind']=='categories' for p in page_audit),'four_benchmark_pages':sum(p['kind']=='benchmark' for p in page_audit),'text_policy':'Titles and notes below figures only; chart labels rendered inside figures'},indent=2,ensure_ascii=False),encoding='utf-8')
print(PDF)

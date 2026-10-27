"""Corrected county-level companion: headings, figures and figure notes only."""
from pathlib import Path
import os,json,hashlib
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
import pymupdf
from PIL import Image

ROOT=Path(os.environ['TALENT_DETS_DATA_DIR'])
STAMP='amws_county_training_location_20261014'
PREP=ROOT/'output/land_grants'/STAMP
RES=ROOT/'results/land_grants/event_studies'/STAMP
OUT=ROOT/'results/land_grants/companions'/STAMP
OUT.mkdir(parents=True,exist_ok=True)
FIG=OUT/'figures';FIG.mkdir(exist_ok=True)
PDF=OUT/'college_allocation_county_training_location_50km_100km_20261014.pdf'
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
source_manifests={}
for folder in (PREP,RES):
    manifest=json.loads((folder/'completion_manifest.json').read_text())
    assert manifest['validation']['passed']
    for rel,h in manifest['output_checksums'].items():assert sha(folder/rel)==h,rel
    source_manifests[str(folder/'completion_manifest.json')]=sha(folder/'completion_manifest.json')
dynamic=pd.read_csv(RES/'dynamic_att.csv')
status=pd.read_csv(RES/'model_status.csv')
sample=pd.read_csv(RES/'analysis_sample_summary.csv')
support=pd.read_csv(RES/'event_time_support.csv')
diag=pd.read_csv(RES/'pretrend_test_diagnostics.csv')
warning_records=pd.read_csv(RES/'warnings.csv')
assert len(status)==32 and status.ok.all()
assert pd.read_csv(RES/'effect_decomposition_check.csv').passed.all()
assert pd.read_csv(RES/'total_radius_equivalence.csv').passed.all()
fonts=Path(matplotlib.get_data_path())/'fonts/ttf'
pdfmetrics.registerFont(TTFont('DejaVu',str(fonts/'DejaVuSans.ttf')))
pdfmetrics.registerFont(TTFont('DejaVu-Bold',str(fonts/'DejaVuSans-Bold.ttf')))
pdfmetrics.registerFontFamily('DejaVu',normal='DejaVu',bold='DejaVu-Bold')
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':11,'axes.spines.top':False,'axes.spines.right':False,'axes.formatter.useoffset':False})
groups={'ever_nearby':'Ever nearby','never_nearby':'Never nearby','unknown':'Unknown training'}
colors={'ever_nearby':'#176B8A','never_nearby':'#B35B2C','unknown':'#665894'}
balance_labels={'relative':'Relative-time balancing','calendar':'Calendar-decade balancing'}
timing_labels={'standard_decade':'Standard decade timing','alternative_decade':'Alternative decade timing'}
W,H=841.89,595.28
c=canvas.Canvas(str(PDF),pagesize=(W,H))
c.setTitle('County-level training location: 50 km and 100 km')
c.setAuthor('GTL Talent Determinants')
notes_style=ParagraphStyle('notes',fontName='DejaVu',fontSize=8.8,leading=11.3,textColor=HexColor('#30343b'))
pages=[]
def filt(frame,b,r,t):return frame[(frame.balance==b)&(frame.radius_km==r)&(frame.timing==t)]
def sample_note(b,r,t):
    z=filt(sample,b,r,t).iloc[0]
    return f'{int(z.events)} allocation events, {int(z.units)} individual county units and {int(z.observations)} observations; identical samples and county birth denominators at both radii.'
def inference_note(b,r,t):
    d=filt(diag,b,r,t).set_index('group')
    ps=[];bad=[]
    for g in (*groups,'total'):
        z=d.loc[g]
        if not bool(z.joint_test_available):ps.append('unavailable');bad.append(z)
        else:ps.append('<0.0001' if z.p_value<.0001 else f'{z.p_value:.3g}')
    txt='Small-treatment-cohort warnings apply. Native joint pre-trend p-values (ever/never/unknown/total): '+', '.join(ps)+'.'
    if bad:
        ranks=[int(z.covariance_rank) for z in bad];restr=[int(z.pre_coefficients) for z in bad]
        txt+=f' Unavailable tests have singular covariance (rank {min(ranks)}–{max(ranks)}; {min(restr)}–{max(restr)} restrictions).'
    prefix=f'{b}__{r}km__{t}__'
    extra=warning_records[warning_records.model.str.startswith(prefix)]
    if extra.message.str.contains('Simultaneous critical value',case=False,na=False).any():
        txt+=' The nearby model also warns of an unreliable simultaneous critical value; the plotted intervals are pointwise.'
    return txt
def support_note(b,r,t):
    if b=='relative':return 'The −20 to +70 relative-time window is balanced.'
    z=filt(support,b,r,t)
    total=z[z.group=='total'].sort_values('event_time')
    for g in groups:
        q=z[z.group==g].sort_values('event_time')
        assert np.array_equal(q[['n_cohorts','n_events','n_treated_sites']].values,total[['n_cohorts','n_events','n_treated_sites']].values)
    cohorts='/'.join(str(int(x)) for x in total.n_cohorts)
    events='/'.join(str(int(x)) for x in total.n_events)
    sites='/'.join(str(int(x)) for x in total.n_treated_sites)
    return ('Complete calendar decades 1830–1950; East Carolina is restored and relative-time support varies. '
      f'At e = −50, −40, …, +70, native ATT cohorts: {cohorts}; events/treated counties: {events}'
      +('' if events==sites else f' / {sites}')+'. The common e = 0 reference uses all 15 treated counties.')
def draw(section,sub,img,note,meta):
    c.setFillColor(HexColor('#132b3a'));c.setFont('DejaVu-Bold',19);c.drawString(38,H-34,section)
    c.setFont('DejaVu-Bold',12);c.drawString(38,H-56,sub)
    c.drawImage(str(img),30,177,width=W-60,height=H-252,preserveAspectRatio=True,anchor='c',mask='auto')
    p=Paragraph('<b>Notes.</b> '+note,notes_style);_,height=p.wrap(W-76,150)
    assert height<=145,(sub,height)
    p.drawOn(c,38,160-height)
    pages.append({'page':len(pages)+1,'section':section,'subsection':sub,'notes':note,'notes_height':height,**meta})
    c.showPage()
for b in balance_labels:
    for t in timing_labels:
        all_radius=dynamic[(dynamic.balance==b)&(dynamic.timing==t)]
        limits={g:1.08*max(abs(all_radius[all_radius.group==g].ci_low).max(),abs(all_radius[all_radius.group==g].ci_high).max(),1e-8) for g in groups}
        for r in (50,100):
            d=filt(dynamic,b,r,t)
            fig,axes=plt.subplots(1,3,figsize=(12.8,5.25))
            for ax,g in zip(axes,groups):
                x=d[d.group==g].sort_values('event_time')
                assert len(x)==(10 if b=='relative' else 13)
                ax.axhline(0,color='#aaaaaa',ls='--',lw=.8);ax.axvline(0,color='#cccccc',lw=.7)
                ax.errorbar(x.event_time,x.estimate,yerr=[x.estimate-x.ci_low,x.ci_high-x.estimate],fmt='o-',ms=4,lw=1.1,capsize=2,color=colors[g])
                ax.set_ylim(-limits[g],limits[g]);ax.set_xlim(-23 if b=='relative' else -53,73)
                ax.set_xticks(np.arange(-20 if b=='relative' else -50,71,10));ax.tick_params(axis='x',labelsize=9)
                ax.set_title(groups[g],fontweight='bold',pad=12);ax.set_xlabel('Years relative to allocation')
                ax.ticklabel_format(axis='y',style='plain')
            axes[0].set_ylabel('Effect on scientists per 1,000 county births')
            fig.subplots_adjust(left=.073,right=.99,bottom=.15,top=.88,wspace=.3)
            img=FIG/f'{b}_{r}km_{t}.png';fig.savefig(img,dpi=240);plt.close(fig)
            note=(sample_note(b,r,t)+' '
              +f'For each county c and birth cohort, use the full eligible US-born AMWS pool, regardless of birth county. Ever nearby: any assigned BA/BS, MA, MS or PhD institution ≤{r} km from c’s centroid. '
              +'Never nearby: at least one award, all reported awards fully located without conflicts and all institutions outside the radius. Remaining cases are unknown; known nearby overrides incomplete matching. '
              +'Each entry counts once per county; rates = 1,000 × category count / estimated births in c. No pooled catchments or shared-county exclusions. '
              +('Alternative timing moves birth years ending in 7–9 forward one decade. ' if t=='alternative_decade' else 'Standard timing uses the birth calendar decade. ')
              +'CSDID regression adjustment uses county births at g−10 and never-treated controls. Effects and influence functions subtract e = 0; bars are pointwise 95% intervals. '
              +support_note(b,r,t)+' '+inference_note(b,r,t)+' '
              +'Each category has its own vertical scale, held fixed across radii. Counts partition the same full cohort in every county; scientists may recur across counties. OpenAlex reference locations do not verify historical campuses.')
            draw(balance_labels[b],f'{r} km · {timing_labels[t]}',img,note,{'balance':b,'radius_km':r,'timing':t,'kind':'categories','category_y_limits':limits})
        a=filt(dynamic,b,50,t);a=a[a.group=='total'].sort_values('event_time')
        z=filt(dynamic,b,100,t);z=z[z.group=='total'].sort_values('event_time')
        assert np.array_equal(a.estimate.values,z.estimate.values) and np.array_equal(a.se.values,z.se.values)
        fig,ax=plt.subplots(figsize=(12.8,5.25))
        ax.errorbar(a.event_time,a.estimate,yerr=[a.estimate-a.ci_low,a.ci_high-a.estimate],fmt='o-',ms=4,lw=1.2,capsize=2,color='#176B8A',label='50 km = 100 km')
        ax.axhline(0,color='#aaaaaa',ls='--',lw=.8);ax.axvline(0,color='#cccccc',lw=.7)
        ax.set_xticks(np.arange(-20 if b=='relative' else -50,71,10));ax.set_xlabel('Years relative to allocation')
        ax.set_ylabel('Effect on all scientists per 1,000 county births');ax.legend(frameon=False)
        fig.subplots_adjust(left=.09,right=.985,bottom=.15,top=.95)
        img=FIG/f'{b}_benchmark_{t}.png';fig.savefig(img,dpi=240);plt.close(fig)
        note=(sample_note(b,50,t)+' '
          +'The total numerator is the full eligible US-born AMWS count in each birth cohort, regardless of birth county or training information. Divide by county c’s estimated births and multiply by 1,000. '
          +'Thus the total is not local-born scientist production: its numerator is common across counties, and its local variation comes from the birth denominator. '
          +'The 50 km and 100 km totals, estimates and intervals are identical and shown by one curve; only category assignments change with radius. '
          +'The three category effect series sum to this benchmark; standard errors and confidence limits are estimated separately and never added. '
          +'CSDID regression adjustment uses county births at g−10 and never-treated controls. Estimates and influence functions subtract e = 0; bars are pointwise 95% intervals. '
          +support_note(b,50,t)+' '+inference_note(b,50,t)+' '
          +('Alternative timing shifts years ending in 7–9 forward; the calendar model excludes decade 1960. ' if t=='alternative_decade' else '')
          +'There are no pooled catchments or shared-county exclusions. Scientist entries recur across county outcomes by construction; original inference conventions are retained. OpenAlex reference locations do not establish historical campuses.')
        draw(balance_labels[b],f'Total benchmark · {timing_labels[t]}',img,note,{'balance':b,'timing':t,'kind':'benchmark'})
c.save()
doc=pymupdf.open(PDF);assert len(doc)==12
render=OUT/'rendered';render.mkdir(exist_ok=True)
for i,page in enumerate(doc):
    txt=page.get_text();assert 'Notes.' in txt and pages[i]['section'] in txt
    page.get_pixmap(matrix=pymupdf.Matrix(1.5,1.5)).save(render/f'page_{i+1:02d}.png')
thumbs=[]
for i in range(12):
    im=Image.open(render/f'page_{i+1:02d}.png').convert('RGB');im.thumbnail((600,425));thumbs.append(im)
montage=Image.new('RGB',(1800,1700),'#d9dde0')
for i,im in enumerate(thumbs):montage.paste(im,((i%3)*600,(i//3)*425))
montage.save(render/'all_pages.png')
(OUT/'page_notes_and_validation.json').write_text(json.dumps({'passed':True,'page_count':12,'pages':pages,'source_manifests':source_manifests,'pdf_sha256':sha(PDF),'category_pages':8,'benchmark_pages':4,'text_policy':'Section/subsection titles and notes below figures only; essential labels inside figures'},indent=2,ensure_ascii=False),encoding='utf-8')
print(PDF)

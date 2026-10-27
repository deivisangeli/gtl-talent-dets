import csv
import hashlib
import json
import math
import os
import shutil
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
DATA_ROOT = Path(os.environ.get('TALENT_DETS_DATA_DIR', 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'))
ROOT = DATA_ROOT / 'results'
REFRESH_TAG = os.environ.get('AMWS_REFRESH_TAG', 'amws_ocr_20261011')
OUT = REPO / 'project/report_companion.tex'
SOURCES = {}
CHECKS = []
PARTS = []
publication = DATA_ROOT / 'output/amws/ocr_supplement_20261011/publication.json'
release = json.loads(publication.read_text())
assert release['status'] == 'published'
SOURCES[str(publication)] = hashlib.sha256(publication.read_bytes()).hexdigest()
for entry in release['installed']:
    if entry['path'].endswith('.csv'):
        path = DATA_ROOT / entry['path']
        value = hashlib.sha256(path.read_bytes()).hexdigest()
        assert value == entry['sha256']
        SOURCES[str(path)] = value


def read(path):
    path = Path(path)
    raw = path.read_bytes()
    SOURCES[str(path)] = hashlib.sha256(raw).hexdigest()
    return list(csv.DictReader(raw.decode('utf-8-sig').splitlines()))


def select(rows, **filters):
    return [r for r in rows if all(r[k] == str(v) for k, v in filters.items())]


def one(rows, **filters):
    found = select(rows, **filters)
    assert len(found) == 1, (filters, len(found))
    return found[0]


def num(v, digits=3):
    return f'{float(v):,.{digits}f}'


def integer(v):
    return f'{int(float(v)):,}'


def pval(v):
    return r'$<0.0001$' if float(v) < .0001 else num(v, 4)


def coefficient(value, p_value, digits):
    p = float(p_value)
    stars = '***' if p < .01 else '**' if p < .05 else '*' if p < .10 else ''
    return num(value, digits) + (r'$^{' + stars + '}$' if stars else '')


def add(text):
    PARTS.append(text)


def note(text):
    return r'\par\smallskip{\footnotesize\raggedright\textit{Notes:} ' + text + r'\par}'


def table(caption, headers, rows, notes, align=None):
    if align is None:
        align = 'l' + 'r' * (len(headers) - 1)
    body = '\n'.join(' & '.join(row) + r' \\' for row in rows)
    return '\n'.join([r'\begin{table}[H]\centering\small',
                      r'\caption{' + caption + '}', r'\setlength{\tabcolsep}{5pt}',
                      r'\begin{tabular}{' + align + r'}\toprule',
                      ' & '.join(headers) + r' \\\midrule', body,
                      r'\bottomrule\end{tabular}', note(notes), r'\end{table}'])


def regression_table(caption, header_lines, rows, notes, columns, font=r'\small', padding='5pt'):
    body = '\n'.join(row if isinstance(row, str) else ' & '.join(row) + r' \\' for row in rows)
    return '\n'.join([r'\begin{table}[H]\centering' + font,
                      r'\caption{' + caption + '}', r'\setlength{\tabcolsep}{' + padding + '}',
                      r'\begin{tabular}{l' + 'c' * columns + r'}\toprule',
                      '\n'.join(header_lines), r'\midrule', body,
                      r'\bottomrule\end{tabular}', note(notes), r'\end{table}'])


def coords(rows, x, y, scale=1):
    vals = [(float(r[x]), float(r[y]) / scale) for r in rows]
    assert all(math.isfinite(a) and math.isfinite(b) for a, b in vals)
    text = ' '.join(f'({a:.12g},{b:.12g})' for a, b in vals)
    CHECKS.append({'kind': 'coordinates', 'x': x, 'y': y, 'count': len(vals),
                   'max_rounding_error': max([abs(b-float(f'{b:.12g}')) for a,b in vals], default=0)})
    return text


def event_axis(rows, title, ylabel, x='event_time', y='estimate', low='ci_low', high='ci_high',
               ref=-10, fair=None, width=r'0.47\textwidth', height='6.7cm', limits=None,
               ribbon=False, rank=False):
    rows = sorted(rows, key=lambda r: float(r[x]))
    assert len({float(r[x]) for r in rows}) == len(rows)
    times = [float(r[x]) for r in rows]
    opts = [f'width={width}', f'height={height}', 'scale only axis=false',
            'grid=major', 'grid style={gray!15}', 'tick label style={font=\\scriptsize}',
            'label style={font=\\small}', 'title style={font=\\small}',
            f'ylabel={{{ylabel}}}', 'xlabel={Years from treatment}',
            'xtick={' + ','.join(f'{t:g}' for t in times) + '}',
            f'xmin={min(times)-4:g}', f'xmax={max(times)+4:g}',
            'scaled y ticks=false', '/pgf/number format/fixed', '/pgf/number format/precision=3',
            'unbounded coords=jump']
    if title:
        opts.insert(8, f'title={{{title}}}')
    if ylabel == 'Estimated county births':
        opts[opts.index('scaled y ticks=false')] = 'scaled y ticks=true'
    if rank:
        opts += ['ymin=0', 'ymax=1']
    elif limits:
        opts += [f'ymin={limits[0]:.12g}', f'ymax={limits[1]:.12g}']
    start = '\n'.join([r'\begin{tikzpicture}', r'\begin{axis}[' + ','.join(opts) + ']'])
    lines = [start]
    reference_y = .5 if rank else 0
    lines += [r'\addplot[gray,densely dashed,forget plot] coordinates {' +
              f'({min(times)-4:g},{reference_y:g}) ({max(times)+4:g},{reference_y:g})' + '};',
              r'\draw[gray,densely dashed] (axis cs:' + str(ref) +
              r',\pgfkeysvalueof{/pgfplots/ymin}) -- (axis cs:' + str(ref) +
              r',\pgfkeysvalueof{/pgfplots/ymax});']
    lines += [r'\draw[orange!80!black,dashed] (axis cs:0,\pgfkeysvalueof{/pgfplots/ymin}) -- (axis cs:0,\pgfkeysvalueof{/pgfplots/ymax});']
    if fair is not None:
        lines += [r'\draw[blue!65!black,dotted] (axis cs:' + str(fair) +
                  r',\pgfkeysvalueof{/pgfplots/ymin}) -- (axis cs:' + str(fair) +
                  r',\pgfkeysvalueof{/pgfplots/ymax});']
    valid = [r for r in rows if r.get(low) not in ('', 'NA', None) and r.get(high) not in ('', 'NA', None)
             and float(r[x]) != ref]
    if ribbon:
        # Split ribbons at the omitted reference rather than manufacturing its uncertainty.
        for j, group in enumerate([[r for r in valid if float(r[x]) < ref],
                                   [r for r in valid if float(r[x]) > ref]]):
            if len(group) > 1:
                lines += [r'\addplot[name path=upper' + str(j) + r',draw=none,forget plot] coordinates {' + coords(group,x,high) + '};',
                          r'\addplot[name path=lower' + str(j) + r',draw=none,forget plot] coordinates {' + coords(group,x,low) + '};',
                          r'\addplot[gray!25,forget plot] fill between[of=upper' + str(j) + ' and lower' + str(j) + '];']
        plotrows = [r for r in rows if r.get(y) not in ('', 'NA', None)]
        paths = [plotrows] if len(plotrows) == len(rows) else [
            [r for r in plotrows if float(r[x]) < ref], [r for r in plotrows if float(r[x]) > ref]]
        for path in paths:
            if path:
                lines += [r'\addplot[blue!65!black,thick,mark=*,mark size=1.7pt] coordinates {' + coords(path,x,y) + '};']
    else:
        vals = []
        for r in valid:
            v = float(r[y]); lo = float(r[low]); hi = float(r[high])
            assert lo <= v <= hi
            vals.append(f'({float(r[x]):.12g},{v:.12g}) += (0,{hi-v:.12g}) -= (0,{v-lo:.12g})')
        lines += [r'\addplot[blue!65!black,only marks,mark=*,mark size=1.7pt,error bars/.cd,y dir=both,y explicit,error bar style={line width=0.6pt}] coordinates {' + ' '.join(vals) + '};']
        refrows = [r for r in rows if float(r[x]) == ref]
        if refrows:
            lines += [r'\addplot[gray,only marks,mark=*,mark size=1.7pt] coordinates {' + coords(refrows,x,y) + '};']
    lines += [r'\end{axis}', r'\end{tikzpicture}']
    CHECKS.append({'kind':'event_axis','title':title,'count':len(rows),'reference':ref,'interval_count':len(valid)})
    return '\n'.join(lines)


def common_limits(groups, y='estimate', low='ci_low', high='ci_high'):
    vals = [0.]
    for group in groups:
        for r in group:
            for key in (y, low, high):
                if r.get(key) not in ('', 'NA', None):
                    vals.append(float(r[key]))
    span = max(vals) - min(vals)
    return min(vals) - span*.07, max(vals) + span*.07


def figure(caption, panels, notes):
    return '\n'.join([r'\begin{figure}[H]\centering', r'\hfill'.join(panels),
                      r'\caption{' + caption + '}', note(notes), r'\end{figure}'])


add(r'''% Standalone companion to Reports - Joao Francisco, meeting update labelled Oct 5, 2026.
% AMWS estimates refreshed using the published October 11, 2026 OCR supplement.
% Census-only and synthetic DiD results retain their original vintage.
% Tables and plotted values are drawn exclusively from saved outputs; this generator does not estimate models.
% Appendix results were separately estimated by analysis/report_companion_calendar_appendices.R.
% All tables and PGFPlots coordinates are embedded; no external data or bibliography required.
% IMAGE DEPENDENCY: figures/wwii_veteran_share_by_birth_quarter_1910_1940.png (unchanged source PNG).
% Report: https://docs.google.com/document/d/1mfO8Zq5Wq4jEmUOF_8h4lfH5GEWl5eK4y96WPO4HQoY/edit
% SELECT WWII Census: spec=primary; model=iv_2sls; first_stage for diagnostics.
% SELECT AMWS: window=baseline; support=window_specific; resolution=quarterly; outcome=male; weighting=births; seasonal=TRUE.
% SELECT CLS: sample=linked_1940_grade; model in {ols,iv_2sls}; cluster in {state,state_cohort}; AR uses state.
% SELECT College: spec=births_level; balanced_event_time_max70; standard_decade; two report outcomes.
% SELECT US fairs: hosted single-fair counties; robust_m30_pop_m10_balanced_oldest_treatment_m20; standard decades.
% SELECT UK fairs: lead20_standard; pooled_dz_rematched_placebos; e=-10 reference.
% SELECT Facilities main: outcome=amws_per_1000_births; balance=unbalanced; standard_decade; nevertreated; post_max=70.
% SELECT Appendices: full calendar coverage; event-time-unbalanced; e=-50 through supported +70; nevertreated.
% SELECT College appendix: calendar labels 1830-1950, audited calendar-complete counties, standard_decade, births_level, ref=0.
% SELECT Facilities appendix: complete annual blocks 1860-1949, standard_decade, ref=-10, post_max=70.
\documentclass[11pt]{article}
\usepackage[a4paper,margin=18mm]{geometry}
\usepackage[T1]{fontenc}
\usepackage{lmodern,amsmath,booktabs,float,caption,microtype,pgfplots,graphicx}
\usepgfplotslibrary{fillbetween}
\pgfplotsset{compat=1.18}
\captionsetup{font=small,labelfont=bf,skip=5pt}
\setlength{\parindent}{0pt}
\setlength{\parskip}{3pt}
\setlength{\intextsep}{10pt}
\setlength{\textfloatsep}{10pt}
\begin{document}
\section{WWII GI Bill}
''')
gi = ROOT / 'mandatory_schooling/wwii_service_scientist_1960_iv'
veteran_image = gi/'figures/wwii_veteran_share_by_birth_quarter_1910_1940.png'
veteran_asset = REPO/'project/figures'/veteran_image.name
veteran_hash = hashlib.sha256(veteran_image.read_bytes()).hexdigest()
SOURCES[str(veteran_image)] = veteran_hash
if not veteran_asset.exists() or hashlib.sha256(veteran_asset.read_bytes()).hexdigest() != veteran_hash:
    shutil.copyfile(veteran_image, veteran_asset)
assert hashlib.sha256(veteran_asset.read_bytes()).hexdigest() == veteran_hash
add(figure('WWII-veteran share by birth quarter, 1910--1940',
           [r'\includegraphics[width=\textwidth,keepaspectratio]{figures/wwii_veteran_share_by_birth_quarter_1910_1940.png}'],
           r'This descriptive sample retains Korean-only veterans; the primary regression sample below excludes them.'))
e = read(gi/'estimates.csv'); d = read(gi/'model_diagnostics.csv')
ar = read(gi/'anderson_rubin_intervals.csv')
primary = one(e, spec='primary', model='iv_2sls')
first = one(e, spec='primary', model='first_stage')
assert len(ar) == 1
rows = [['WWII service', coefficient(primary['estimate_pp'], primary['p_value'], 3)],
        ['', '(' + num(primary['std_error_pp']) + ')'], r'\midrule',
        ['Separate linear cohort trends', 'Yes'], ['PERWT weights', 'Yes'],
        ['Observations', integer(primary['observations'])],
        ['Scientist occupations', integer(primary['scientist_count'])],
        ['Birth-quarter clusters', integer(primary['cohort_clusters'])],
        ['First-stage jump (pp)', num(first['estimate_pp'])],
        ['First-stage $F$', num(one(d, spec='primary')['first_stage_F'], 2)],
        ['2SLS $p$-value', pval(primary['p_value'])],
        ['95\\% confidence interval (pp)', f'[{num(primary["lower_pp"])}, {num(primary["upper_pp"])}]'],
        ['95\\% Anderson--Rubin interval (pp)', f'[{num(ar[0]["lower_pp"])}, {num(ar[0]["upper_pp"])}]']]
add(regression_table('WWII service and scientist occupation in the 1960 Census',
          [r'Dependent variable: & \shortstack{Scientist occupation\\(percentage points)} \\',
           r' & (1) \\', r'Estimator & IV/2SLS \\'], rows,
          r'US-born civilian men born in 1925--1930, from the 5\% Census sample. Outcome: scientist occupation in 1960. '
          r'The instrument is $z=\mathbf{1}\{x\leq0\}$, with $x=4(\mathrm{BIRTHYR}-1927)+\mathrm{BIRTHQTR}-4$; '
          r'both stages include separate linear cohort trends. Estimates use PERWT and standard errors clustered by 24 birth quarters '
          r'($t_{23}$ inference); the clustered standard error appears in parentheses below the coefficient. '
          r'The primary sample excludes Korean-only veterans. Exclusion based on subsequent service changes the comparison sample. '
          r'The service effect bundles WWII service and veteran benefits; it does not isolate the GI Bill education subsidy. '
          r'Coefficients and standard errors are in percentage points. '
          r'Stars use birth-quarter-clustered $p$ values: $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.', columns=1))
assert abs(float(primary['estimate_pp'])-.752461606575638)<1e-10
amws = REPO/'project/figures/wwii_gi_bill_amws_reduced_form_baseline_quarterly_male.tex'
amws_estimates = read(ROOT/'mandatory_schooling/wwii_gi_bill_amws_cutoff/estimates.csv')
amws_main = {resolution: one(amws_estimates,window='baseline',support='window_specific',resolution=resolution,
          outcome='male',weighting='births',seasonal='TRUE') for resolution in ('monthly','quarterly')}
q, m = amws_main['quarterly'], amws_main['monthly']
def significance(row):
    return 'significant at 10\\%' if float(row['p_value']) < .10 else 'not significant at 10\\%'
raw = amws.read_text(encoding='utf-8'); SOURCES[str(amws)] = hashlib.sha256(amws.read_bytes()).hexdigest()
plot = raw[raw.index(r'\begin{tikzpicture}'):raw.index(r'\end{tikzpicture}')+len(r'\end{tikzpicture}')]
plot = plot.replace('height=6cm','height=6.3cm').replace('title={Male -- Quarterly}', 'title={Male AMWS scientists: quarterly birth cohorts, 1925--1930}')
add(figure('Quarterly male AMWS birth-cohort discontinuity', [plot],
           r'County- and quarter-of-year-adjusted cohort means, with separate fitted trends; birth-weighted models use '
           + integer(q['counties']) + r' counties. '
           r'Outcome: inferred-male scientists per 1,000 \emph{total} estimated births, using saved 0.8 gender flags. '
           r'The dashed boundary separates 1927Q4 ($x=0$) from 1928Q1 ($x=1$); the dotted later-born fit extrapolates to $x=0$. '
           + r'The reported contrast is earlier minus later at $x=0$: $' + num(q['estimate'],4)
           + r'$ (95\% CI $[' + num(q['ci_lower'],4) + ',' + num(q['ci_upper'],4)
           + r']$; $p=' + num(q['p_value'],3) + r'$). The monthly estimate is $'
           + num(m['estimate'],4) + r'$ ($p=' + num(m['p_value'],3) + r'$), ' + significance(m)
           + '; the quarterly estimate is ' + significance(q) + '. '
           r'Inference clusters separately by county and national birth cohort. This AMWS reduced form has no service first stage '
           r'and is distinct from the Census IV above.'))

add(r'\clearpage\section{Compulsory Schooling Laws}')
school = ROOT/'mandatory_schooling/linked_1940_schooling_1950_scientist_iv'
ba = ROOT/'mandatory_schooling/linked_1940_ba_1950_scientist_iv'
ee = [read(p/'estimates.csv') for p in (school,ba)]
dd = [read(p/'diagnostics.csv') for p in (school,ba)]
aa = [read(p/'anderson_rubin_intervals.csv') for p in (school,ba)]
models = [(0, 'ols'), (0, 'iv_2sls'), (1, 'ols'), (1, 'iv_2sls')]
state = [one(ee[i], sample='linked_1940_grade', model=model, cluster='state') for i, model in models]
state_cohort = [one(ee[i], sample='linked_1940_grade', model=model, cluster='state_cohort') for i, model in models]
rows = []
for i, label in [(0, 'Completed schooling (years)'), (1, 'BA-equivalent completion')]:
    rows.append([label] + [coefficient(float(r['estimate']) * 100000, r['p_value'], 1)
                          if models[j][0] == i else r'---' for j, r in enumerate(state)])
    rows.append([''] + ['(' + num(float(r['se']) * 100000, 1) + ')'
                       if models[j][0] == i else r'---' for j, r in enumerate(state)])
    rows.append([''] + ['[' + num(float(r['se']) * 100000, 1) + ']'
                       if models[j][0] == i else r'---' for j, r in enumerate(state_cohort)])
    if i == 0:
        rows.append(r'\addlinespace[4pt]')
rows += [r'\midrule', ['Birth-state fixed effects'] + ['Yes'] * 4,
         ['Region $\\times$ cohort fixed effects'] + ['Yes'] * 4,
         ['Observations'] + [integer(one(dd[i], sample='linked_1940_grade', cluster='state')['observations']) for i, model in models],
         ['Birth-state clusters'] + [integer(one(dd[i], sample='linked_1940_grade', cluster='state')['states']) for i, model in models]]
for cluster, label in [('state', 'First-stage $F$: state'), ('state_cohort', 'First-stage $F$: state $\\times$ cohort')]:
    rows.append([label] + [num(one(dd[i], sample='linked_1940_grade', cluster=cluster)['first_stage_joint_F'], 2)
                          if model == 'iv_2sls' else r'---' for i, model in models])
rows.append(['95\\% AR interval: state'] + [f'[{num(aa[i][0]["lower_per_100k"], 1)}, {num(aa[i][0]["upper_per_100k"], 1)}]'
                                          if model == 'iv_2sls' else r'---' for i, model in models])
rows.append(['Coefficient $p$: state'] + [pval(r['p_value']) for r in state])
rows.append(['Coefficient $p$: state $\\times$ cohort'] + [pval(r['p_value']) for r in state_cohort])
add(regression_table('Schooling and BA-equivalent completion: main linked-sample regressions',
          [r'Dependent variable: & \multicolumn{4}{c}{Scientist occupation in 1950 (per 100,000 men)} \\',
           r' & \multicolumn{2}{c}{Schooling (+1 year)} & \multicolumn{2}{c}{BA-equivalent ($0\to1$)} \\',
           r'\cmidrule(lr){2-3}\cmidrule(lr){4-5}',
           r' & (1) & (2) & (3) & (4) \\', r'Estimator & OLS & 2SLS & OLS & 2SLS \\'], rows,
          r'Native-born white men aged 27--54 in 1940, uniquely linked to the 1950 Census; $N=12{,}285{,}411$, with 22,743 scientist occupations. '
          r'Schooling is measured in 1940 and scientist occupation in 1950. BA-equivalent completion means at least 16 completed schooling years, '
          r'not an observed awarded degree. The five instruments indicate 1--5, 6, 7, 8, and 9+ legally required years, relative to zero. '
          r'Both models include birth-state and CLS-region $\times$ birth-cohort fixed effects and use identical person-count-weighted cells. '
          r'Effects and SEs are scientist occupations per 100,000 men for the stated treatment change; magnitudes are different estimands. '
          r'Parentheses below coefficients contain state-clustered standard errors; brackets contain state $\times$ cohort standard errors. '
          r'Coefficient $p$ values for both clustering conventions appear below the coefficient block. '
          r'First-stage statistics are joint clustered Wald $F$ tests and apply to 2SLS columns. '
          r'Anderson--Rubin (AR) intervals use state clustering; the BA interval is the accepted set detected on the saved grid. '
          r'Stars use state-clustered $p$ values: $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.',
          columns=4, font=r'\footnotesize', padding='3.5pt'))

add(r'\clearpage\section{College Allocation}')
college = ROOT/'land_grants/event_studies/amws_county_pairs_temporal_support_1830_1950_baseline_pop_balanced_event_time_max70'
cd = read(college/'baseline_pop_dynamic_att.csv'); cs=read(college/'baseline_pop_model_status.csv')
college_status = one(cs,outcome='amws_per_1000_births',spec='births_level',timing='standard_decade')
assert college_status['ok'] == 'TRUE'
college_notes = (r'Andrews selected counties versus runner-up controls; Callaway--Sant\textquotesingle Anna with baseline estimated births at $e=-10$ '
                 r'as an outcome-regression covariate. The fully balanced sample covers $e=-20$ through $+70$; effects are renormalized to $e=0$. '
                 r'Bars show saved pointwise 95\% influence-function intervals; the reference period has no uncertainty bar. '
                 r'Treatment, outcomes, and denominators use standard calendar decades. '
                 r'The sample comprises ' + integer(college_status['n_events']) + ' allocation events and '
                 + integer(college_status['n_units']) + ' stack units.')
for outcome,caption,ylabel in [('amws_per_1000_births','College allocation: AMWS scientists per 1,000 estimated births','Scientists / 1,000 births'),
                               ('county_births_estimate','College allocation: estimated county births','Estimated county births')]:
    groups = [select(cd,outcome=outcome,spec='births_level',timing='standard_decade')]
    assert all(len(g)==10 for g in groups)
    limits=common_limits(groups)
    panels=[event_axis(groups[0],None,ylabel,ref=0,limits=limits,width=r'0.97\textwidth')]
    notes=college_notes
    if outcome=='amws_per_1000_births':
        val=one(groups[0],event_time='50')
        notes += r' The standard-decade estimate at $+50$ is ' + num(val['estimate']) + r' (95\% CI [' + num(val['ci_low']) + ', ' + num(val['ci_high']) + ']).'
    else:
        notes += r' Estimated births use NHGIS-interpolated county population and Gapminder birth rates; this figure checks changes in the outcome denominator.'
    add(figure(caption,panels,notes))

add(r'\clearpage\section{World\textquotesingle s Fairs}')
us_paths = [ROOT/'worlds_fair'/
    'worlds_fairs_us_amws_event_studies_fair_cohorts_1840_1900_treatment_m20_e_m30_p50_balanced_oldest_logpop_m10_single_fair']
usd=[select(read(p/'all_dynamic_att.csv'),treatment_spec='hosted',outcome='amws_per_1000_births') for p in us_paths]
uss=[one(read(p/'all_simple_att.csv'),treatment_spec='hosted',outcome='amws_per_1000_births') for p in us_paths]
us_status=one(read(us_paths[0]/'all_model_status.csv'),treatment_spec='hosted',outcome='amws_per_1000_births')
assert us_status['ok']=='TRUE'
us_count=integer(us_status['n_treated_units'])
uk=ROOT/'worlds_fair/synthdid/ukds_1911_lgd_wikipedia_donor_pools/match_v4_balanced_e0/pooled_dz_rematched_placebos'
uka=read(uk/'dz_pooled_att.csv'); ukd=read(uk/'dz_pooled_dynamic_effects.csv'); ukc=read(uk/'dz_baseline_comparison.csv')
rows=[]
for i,label in enumerate(['Standard']):
    r=uss[i]; rows.append(['US CS-DID',label,num(r['estimate']),f'[{num(r["ci_low"])}, {num(r["ci_high"])}]',pval(r['p_value'])])
for model,label in [('lead20_standard','Standard')]:
    r=one(uka,model_id=model);rows.append(['UK synthetic DiD',label,num(r['pooled_att']),r'---',pval(r['randomization_p_two_sided'])])
add(table(r'World\textquotesingle s fairs: pooled post-treatment estimates',
          ['Estimator','Decades','Effect','95\\% interval','$p$'],rows,
          r'Treatment begins 20 years before each fair. US: ' + us_count + r' treated counties, AMWS scientists per 1,000 estimated births, '
          r'Callaway--Sant\textquotesingle Anna; $p$ values and confidence intervals use the saved CS-DID inference. '
          r'UK: four treated local-government districts, annualized Wikipedia Discovery/Science births per 100,000 population; '
          r'effects are equal-event means of post gaps minus the last untreated gap. UK $p$ values are two-sided joint rank randomization tests '
          r'with 1,000 draws and freshly matched 30-LGD placebo donor pools. No pooled UK confidence interval is supplied in these outputs. '
          r'For standard decades, the fixed-pool rank $p$ value is 0.008, versus 0.774 after rematching; '
          r'the point estimate is unchanged. The rematched two-sided effect-test $p$ value is 0.973. US and UK effect scales are different.'))
limits=common_limits(usd)
add(figure('US fairs: lead-20 AMWS event studies',
           [event_axis(usd[0],None,'Scientists / 1,000 births',fair=20,limits=limits,width=r'0.97\textwidth')],
           r'Callaway--Sant\textquotesingle Anna estimates for ' + us_count + r' single-fair host counties, using standard calendar decades. '
           r'Treatment begins at $e=0$ (orange dashed line); the fair occurs at $e=+20$ (blue dotted line). '
           r'The last untreated decade, $e=-10$ (grey dashed line), is the reference. '
           r'Bars are saved pointwise 95\% confidence intervals. The balanced oldest-cohort specification controls for log population at $e=-10$; '
           r'scientist counts, estimated-birth denominators, and treatment use standard calendar decades.'))
add(r'\clearpage')
groups=[select(ukd,model_id='lead20_standard')]
limits=common_limits(groups,'pooled_effect','effect_ci_lower','effect_ci_upper')
panels=[event_axis(groups[0],'Effect',r'Annualized births / 100,000',y='pooled_effect',low='effect_ci_lower',high='effect_ci_upper',fair=20,limits=limits,ribbon=True,height='7cm'),
        event_axis(groups[0],'Placebo percentile rank','Mean placebo percentile rank',y='mean_percentile_rank',low='rank_null_lower',high='rank_null_upper',fair=20,ribbon=True,rank=True,height='7cm')]
add(figure('UK fairs: pooled synthetic-DiD event studies with rematched placebos', panels,
           r'Standard calendar decades. The left panel shows equal-event mean gaps relative to $e=-10$; grey bands are saved pointwise 95\% confidence intervals '
           r'from inversion of the joint randomization test. The right panel shows mean event-specific placebo percentile ranks; '
           r'bands are 2.5th--97.5th percentile randomization-null envelopes, not confidence intervals. '
           r'No uncertainty is assigned to the reference period. Treatment begins at $e=0$ and the fair occurs at $e=+20$. '
           r'Four treated events are retained; each placebo receives a fresh 30-LGD donor pool using five matching variables and a 50 km exclusion. '
           r'The 1,000 joint draws prohibit duplicate pseudo-treated LGDs; numerical solver failures are excluded from the assignment universe.'))

add(r'\clearpage\section{Scientific Facilities}')
sf=ROOT/'scientific_facilities/event_studies/amws_scientific_facilities_decennial_pre1944_end1949'/REFRESH_TAG
sfd=select(read(sf/'dynamic_att.csv'),outcome='amws_per_1000_births',balance='unbalanced',control_group='nevertreated',timing='standard_decade')
sfs=select(read(sf/'post_att_summary.csv'),outcome='amws_per_1000_births',balance='unbalanced',post_max='70',control_group='nevertreated',timing='standard_decade')
assert len(sfs)==1
rows=[]
for timing,label in [('standard_decade','Standard')]:
    r=one(sfs,timing=timing)
    rows.append([label,'Never-treated',num(r['estimate']),f'[{num(r["ci_low"])}, {num(r["ci_high"])}]',pval(r['p_value']),integer(r['cohort_counties'])])
add(table('Scientific facilities: full-horizon post-treatment summaries',
          ['Timing','Controls','Effect','95\\% interval','$p$','Treated'],rows,
          r'AMWS scientists per 1,000 estimated births; unbalanced decennial Callaway--Sant\textquotesingle Anna. '
          r'Post-treatment summaries use saved cohort-size-weighted group-time effects over supported $e=0$ through $+70$. '
          r'Treatment is earliest county exposure to the existing 50 km facility buffers, with openings through 1943 and outcomes through 1949. '
          r'Treatment and outcomes use standard calendar decades, with ' + integer(sfs[0]['cohort_counties']) + ' distinct contributing treated counties. '
          r'Pointwise inference uses joint county influence functions and 1,000 multiplier draws.'))
sfnotes=(r'Unbalanced decennial AMWS rates, openings through 1943 and outcomes through 1949. '
         r'County exposure is the earliest facility opening within the existing 50 km buffers. '
         r'The grey dashed line marks the preceding decade ($e=-10$); treatment begins at $e=0$ (orange dashed line). '
         r'Bars are saved pointwise 95\% multiplier-bootstrap intervals. Treatment, scientist counts, and birth denominators use standard calendar decades. '
         r'Contributing cohorts vary across event times; longer horizons rely on fewer early cohorts. '
         r'Never-treated counties remain unexposed through 1949. '
         r'Missing observations are not imputed as zero.')
timings=('standard_decade',)
groups=[select(sfd,timing=t) for t in timings]
assert all(len(g)==11 for g in groups)
limits=common_limits(groups)
add(figure('Scientific facilities: never-treated controls',
           [event_axis(groups[0],None,'Scientists / 1,000 births',limits=limits,height='8cm',width=r'0.97\textwidth')],sfnotes))

def support_notes(groups, county_key):
    """Compact per-point cohort/county counts from the saved support exports."""
    grid=list(range(-50,71,10))
    for group in groups:
        assert sorted(int(float(r['event_time'])) for r in group)==grid
    rows=[r'Event year & ' + ' & '.join('$'+f'{e:+d}'+'$' if e else '$0$' for e in grid) + r' \\']
    assert len(groups)==1
    for group,label in zip(groups,['Standard']):
        bytime={int(float(r['event_time'])):r for r in group}
        rows.append(label+' & '+' & '.join(
            integer(bytime[e]['n_cohorts'])+'/'+integer(bytime[e][county_key]) for e in grid)+r' \\')
    return (r'\par\smallskip\textit{Support: contributing treatment cohorts / distinct treated counties.}'
            r'\par\smallskip{\setlength{\tabcolsep}{2.8pt}\begin{tabular}{l*{13}{c}}\toprule'
            +'\n'+'\n'.join(rows[:1])+'\n'+r'\midrule'+'\n'+'\n'.join(rows[1:])+'\n'+r'\bottomrule\end{tabular}}')


appendix_tag=REFRESH_TAG
college_a=ROOT/'land_grants/event_studies/amws_county_pairs_calendar_balanced_m50_p70'/appendix_tag
facility_a=ROOT/'scientific_facilities/event_studies/amws_facilities_calendar_balanced_m50_p70'/appendix_tag
for folder in (college_a,facility_a):
    assert (folder/'status.txt').read_text().strip()=='complete'
    read(folder/'input_manifest.csv')
runner=REPO/'analysis/report_companion_calendar_appendices.R'
SOURCES[str(runner)]=hashlib.sha256(runner.read_bytes()).hexdigest()

add(r'\clearpage\appendix\section{College Allocation: calendar-balanced appendix}')
cad=select(read(college_a/'baseline_pop_dynamic_att.csv'),spec='births_level',timing='standard_decade')
cas=select(read(college_a/'event_time_support.csv'),timing='standard_decade')
cam=select(read(college_a/'baseline_pop_model_status.csv'),timing='standard_decade')
caa=read(college_a/'calendar_sample_audit.csv')
read(college_a/'event_sample_audit.csv')
assert all(r['ok']=='TRUE' for r in cam)
calendar_pool={r['GEOID'] for r in caa if r['timing']=='standard_decade' and r['included']=='TRUE'}
# The original saved pool is identical to the standard-only calendar-complete pool.
assert calendar_pool=={r['GEOID'] for r in caa if r['retained_in_both_timings']=='TRUE'}
calendar_count=len(calendar_pool)
caevents=[one(cam,timing=t,outcome='amws_per_1000_births')['n_events'] for t in timings]
canotes=(r'Calendar balance requires usable AMWS rates and estimated births in every decade labelled 1830--1950 '
         r'using standard calendar decades; ' + integer(calendar_count) + ' physical counties pass, and '
         + integer(caevents[0]) + ' selected-county events retain their own runner-up controls. '
         r'No full event-time coverage is required. '
         r'Callaway--Sant\textquotesingle Anna regression estimates control for baseline births at $g-10$; '
         r'pointwise 95\% intervals and normalization to $e=0$ use the original influence-function procedure. '
         r'The window is $e=-50$ through supported $+70$; contributing cohorts vary across points. '
         r'Counts below describe the underlying dynamic ATT before subtraction of the common $e=0$ estimate. '
         r'No missing observations are imputed. ')
for i,(outcome,caption,ylabel) in enumerate([
    ('amws_per_1000_births','College allocation: calendar-balanced AMWS rates','Scientists / 1,000 births'),
    ('county_births_estimate','College allocation: calendar-balanced estimated births','Estimated county births')]):
    if i: add(r'\clearpage')
    groups=[select(cad,outcome=outcome,timing=t) for t in timings]
    supports=[select(cas,outcome=outcome,timing=t) for t in timings]
    assert all(len(g)==13 for g in groups)
    limits=common_limits(groups)
    add(figure(caption,[event_axis(groups[0],None,ylabel,ref=0,limits=limits,height='8cm',width=r'0.97\textwidth')],
               canotes+support_notes(supports,'n_treated_counties')))

add(r'\clearpage\section{Scientific Facilities: calendar-balanced appendix}')
fad=select(read(facility_a/'dynamic_att.csv'),outcome='amws_per_1000_births',
           control_group='nevertreated',balance='calendar_balanced_event_unbalanced',timing='standard_decade')
fas=select(read(facility_a/'post_att_summary.csv'),post_max='70',timing='standard_decade')
faa=read(facility_a/'calendar_sample_audit.csv')
fav=select(read(facility_a/'aggregation_validation.csv'),timing='standard_decade')
read(facility_a/'support_estimation.csv')
assert len(fas)==1 and all(abs(float(r['difference']))<1e-10 for r in fav)
assert all(r['control_group']=='nevertreated' for r in fas)
facount=sum(r['retained']=='TRUE' for r in faa)
faassignment=read(facility_a/'treatment_assignment.csv')
facontrols=sum(r['role']=='unexposed_through_1949' for r in faassignment)
rows=[]
for timing,label in zip(timings,['Standard']):
    r=one(fas,timing=timing)
    rows.append([label,num(r['estimate']),f'[{num(r["ci_low"])}, {num(r["ci_high"])}]',
                 pval(r['p_value']),integer(r['cohort_counties'])])
fanotes=(r'AMWS scientists per 1,000 estimated births; never-treated controls only. Calendar balance identifies '
         + integer(facount) + r' counties with complete annual scientist counts, positive births and population '
         r'in every ten-year block from 1860--1869 through 1940--1949. '
         r'Estimates use ' + integer(fas[0]['cohort_counties']) + ' treated counties and ' + integer(facontrols)
         + ' never-treated counties; donor-only exposed counties are excluded. '
         r'Treatment, scientist counts, and birth denominators use standard calendar decades. '
         r'Exposure is the original earliest opening within 50 km, with openings through 1943; controls remain unexposed through 1949. '
         r'No full event-time coverage is required. ')
add(table('Scientific facilities: calendar-balanced post-treatment summaries',
          ['Timing','Effect','95\\% interval','$p$','Treated'],rows,
          fanotes+r'Summaries aggregate supported $e=0$ through $+70$ group-time effects with cohort-size weights; '
          + integer(fas[0]['cohort_counties']) + r' distinct treated counties contribute. Inference uses joint county influence functions and 1,000 multiplier draws.'))
groups=[select(fad,timing=t) for t in timings]
assert all(len(g)==13 for g in groups)
limits=common_limits(groups)
add(figure('Scientific facilities: calendar-balanced, event-time-unbalanced estimates',
           [event_axis(groups[0],None,'Scientists / 1,000 births',limits=limits,height='8cm',width=r'0.97\textwidth')],
           fanotes+r'Original two-period doubly robust Callaway--Sant\textquotesingle Anna comparisons. '
           r'The reference is $e=-10$ (grey dashed line); treatment starts at $e=0$ (orange dashed line). '
           r'Bars show saved pointwise 95\% intervals from 1,000 common county multiplier draws. '
           r'Earlier leads use later-treated cohorts, while later lags use earlier-treated cohorts. '
           r'Unsupported comparisons are omitted; missing observations are not replaced by zeros. '
           +support_notes(groups,'cohort_counties')))

add(r'\end{document}')
provenance = '\n'.join('% SOURCE: ' + p + '\n% SHA256: ' + h for p,h in SOURCES.items())
text = '\n'.join(PARTS)
text = text.replace(r'\begin{document}', provenance+'\n'+r'\begin{document}',1)
assert text.count(r'\includegraphics')==1 and '\\input{' not in text
assert text.count(r'\section{')==7 and text.count(r'\begin{table}')==5 and text.count(r'\begin{figure}')==10
assert text.count(r'\begin{axis}')==10
assert all(word not in text.lower() for word in ('alternative', 'shifted', 'both timing', 'both decade'))
OUT.write_text(text,encoding='utf-8')
validation_dir = ROOT/'amws_refresh'/REFRESH_TAG
validation_dir.mkdir(parents=True, exist_ok=True)
(validation_dir/'companion_validation.json').write_text(json.dumps({'sources':SOURCES,'checks':CHECKS,'sections':7,'tables':5,'figures':10,'axes':10,'decade_definition':'standard_only','external_images':[str(veteran_asset)]},indent=2),encoding='utf-8')
print(json.dumps({'output':str(OUT),'bytes':OUT.stat().st_size,'sources':len(SOURCES),'sections':7,'tables':5,'figures':10,'axes':10}))

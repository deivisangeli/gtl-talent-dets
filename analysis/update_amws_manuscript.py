"""Export refreshed manuscript assets and prose from saved model outputs."""
import csv
import hashlib
import json
from pathlib import Path
import re
import shutil

from amws_publication_refresh import REPO, ROOT, RUN, TAG, digest

PROJECT = REPO / 'project'
FROZEN = RUN / 'backup/repo/project'
SOURCES = {}
SELECTED_ROWS = []


def read(path):
    path = Path(path)
    SOURCES[str(path)] = digest(path)
    with path.open(encoding='utf-8-sig', newline='') as stream:
        result = list(csv.DictReader(stream))
    for row in result:
        row['_source'] = str(path)
    return result


def one(rows, **filters):
    rows = [r for r in rows if all(r[k] == str(v) for k, v in filters.items())]
    assert len(rows) == 1, filters
    SELECTED_ROWS.append({'filters': filters, 'row': rows[0]})
    return rows[0]


def number(value, digits=3):
    return f'{float(value):.{digits}f}'


def integer(value):
    return f'{int(float(value)):,}'.replace(',', '{,}')


def paragraph(text, start, replacement):
    pattern = re.escape(start) + r'[^\n]*(?:\\par)?'
    result, count = re.subn(pattern, lambda _: replacement, text, count=1)
    assert count == 1, start
    return result


def assets():
    backup = json.loads((RUN / 'backup_complete.json').read_text())
    index = {}
    for record in backup['files']:
        old = Path(record['path'])
        if old.suffix.lower() not in ('.png', '.pdf') or 'results' not in old.parts:
            continue
        new = Path(str(old).replace('countyfix_amws1986_20260719_decennial_1800_1960', TAG))
        index.setdefault(record['sha256'], []).append(new)
    initial = backup['timestamp']
    manifest = []
    referenced = set(re.findall(r'\\includegraphics(?:\[[^\]]*\])?\{figures/([^}]+)\}',
                                (FROZEN / 'main.tex').read_text(encoding='utf-8-sig')))
    for original in sorted((FROZEN / 'figures').glob('*.png')):
        name = original.name
        if name not in referenced or not (name.startswith('ES_') or name.startswith('WF_ES_')):
            continue
        if name.startswith('WF_ES_'):
            # These paired plots are rendered directly into project/figures
            # from validated saved outputs, rather than copied from result PNGs.
            timing = 'g_shift' if 'alternative_decade' in name else 'fair_cohorts_1840_1900'
            horizon = 'm50' if 'lead20_m50' in name else 'm30'
            result = ROOT / 'results/worlds_fair' / (
                f'worlds_fairs_us_amws_event_studies_{timing}_treatment_m20_e_{horizon}_p50_balanced_oldest_logpop_m10_single_fair')
            inputs = [result / 'all_dynamic_att.csv', result / 'run_validation.csv']
            if 'percohort' in name:
                inputs += sorted((result / 'per_cohort_figure5').glob('*.csv'))
            validation = read(result / 'run_validation.csv')
            assert validation and all(r['passed'] == 'TRUE' for r in validation)
            assert all(p.exists() and p.stat().st_mtime >= initial for p in inputs)
            target = PROJECT / 'figures' / name
            assert target.exists() and target.stat().st_mtime >= initial
            manifest.append({'destination': str(target),
                'renderer': str(REPO / 'analysis/world_fairs/render_worlds_fairs_us_amws_main_text_figures.R'),
                'sources': {str(p): digest(p) for p in inputs},
                'sha256_before': digest(original), 'sha256_after': digest(target)})
            continue
        options = index.get(digest(original), [])
        candidates = [p for p in options if p.exists() and p.stat().st_mtime >= initial]
        assert candidates, f'No refreshed source for {name}: {options}'
        hashes = {digest(p) for p in candidates}
        assert len(hashes) == 1, f'Ambiguous refreshed source for {name}'
        source = candidates[0]
        target = PROJECT / 'figures' / name
        shutil.copy2(source, target)
        manifest.append({'destination': str(target), 'source': str(source),
                         'sha256_before': digest(original), 'sha256_after': digest(target)})
    (RUN / 'manuscript_asset_manifest.json').write_text(json.dumps(manifest, indent=2))
    print('Exported', len(manifest), 'manuscript figures')


def update_main():
    text = (FROZEN / 'main.tex').read_text(encoding='utf-8-sig')
    summary = {r['metric']: r['value'] for r in read(ROOT / 'output/amws/amws_consolidated_county_year_summary.csv')}
    total = integer(summary['total_amws_births_in_panel'])
    newer = integer(summary['amws_1986_births_in_panel'])
    text = text.replace('112{,}291', total).replace('62{,}704', newer)
    text = text.replace('newly digitised 1986 edition', 'expanded 1986 edition, including the October 11, 2026 reviewed OCR supplement,')
    text = text.replace(r'\date{June 2026}', r'\date{October 2026}')
    decades = {r['metric']:r['value'] for r in read(ROOT/'output/amws/amws_consolidated_county_decade_summary.csv')}
    data_text = ('The annual source panel covers 1800--1960 with ' + integer(summary['panel_counties'])
        + ' counties and ' + integer(summary['panel_rows']) + ' county-year observations. The aggregated decennial input contains '
        + integer(decades['panel_counties']) + ' counties and ' + integer(decades['panel_rows'])
        + ' county-decade observations. AMWS outcomes combine ' + integer(summary['amws_1906_1955_dedup_births_in_panel'])
        + ' records from the deduplicated 1906, 1938 and 1955 editions with ' + newer
        + ' records from the expanded 1986 edition, including the October 11, 2026 reviewed OCR supplement, for '
        + total + ' source records in the annual county panel. The 1986 edition is not deduplicated against the earlier editions, '
        'so these are record counts rather than verified unique people. Wikipedia STEM outcomes retain the hard-STEM definition '
        r'and both sources use the existing NHGIS population and Gapminder birth denominators.\par')
    text = paragraph(text,'The outcome panel is a county-decade panel covering',data_text)
    text = text.replace('editions (' + total + ' scientists)', 'editions (' + total + ' source records in the annual panel)')
    college_root = ROOT / 'results/land_grants/event_studies/amws_county_pairs_temporal_support_1830_1950_baseline_pop_balanced_event_time_max70'
    college = read(college_root / 'baseline_pop_dynamic_att.csv')
    cs_status = read(college_root / 'baseline_pop_model_status.csv')
    def land(outcome, timing, event):
        return one(college,outcome=outcome,spec='births_level',timing=timing,event_time=event)
    sr, ar = land('amws_per_1000_births','standard_decade',50), land('amws_per_1000_births','alternative_decade',50)
    intro = ('In this section, I investigate whether quasi-random college allocation increased local scientist production. '
        'I combine four AMWS editions, including the reviewed October 11, 2026 OCR supplement, with the selected and runner-up '
        'counties in Andrews (2023). Under the existing fully balanced specification, the estimated effect fifty years after '
        'allocation is ' + number(sr['estimate']) + ' AMWS scientists per 1,000 births under standard timing and '
        + number(ar['estimate']) + ' under alternative timing. These are local birth-cohort outcomes; they do not directly measure attendance at the allocated college.')
    text = paragraph(text,'In this section, I investigated', intro)
    def interval(r):
        return '$[' + number(r['ci_low']) + ', ' + number(r['ci_high']) + ']$'
    result = (r'Figures 1--3 retain the fully balanced window $e\in[-20,+70]$, baseline estimated births as the covariate, '
        r'and normalization to $e=0$. The count estimates at $e=+50$ are '
        + number(land('n_amws','standard_decade',50)['estimate']) + ' under standard timing and '
        + number(land('n_amws','alternative_decade',50)['estimate']) + r' under alternative timing.\par')
    text = paragraph(text,'Figures 1-3 present',result)
    result = (r'Figure 2 reports AMWS scientists per 1,000 estimated births. At $e=+50$, the standard-decade estimate is '
        + number(sr['estimate']) + r' (95\% CI ' + interval(sr) + ') and the alternative-decade estimate is '
        + number(ar['estimate']) + r' (95\% CI ' + interval(ar) + '). '
        + ('Both intervals exclude zero.' if float(sr['ci_low']) > 0 and float(ar['ci_low']) > 0 else 'The evidence varies across timing conventions; the plotted intervals determine which horizons are distinguishable from zero.')
        + r' Figure 3 retains estimated county births as the denominator check. Its results allow for changes in county births and should not be treated as proof that college allocation affected only the scientist numerator.\par')
    text = paragraph(text,'Figure 2 uses',result)
    old_caption = r'Post-treatment ATTs near zero indicate that college openings do not materially shift county births, so the effects in Figure~\ref{fig:es_per_1000_births_max70} are driven by the numerator (AMWS scientists) rather than by the denominator.'
    assert old_caption in text
    text = text.replace(old_caption,
        r'Post-treatment ATTs summarize differences in estimated county births and do not by themselves establish that college allocation affected only scientist production. The rate estimates in Figure~\ref{fig:es_per_1000_births_max70} should be read alongside this denominator check.')
    # Refresh all existing sample counts from the same saved model-status records.
    for timing, old in [('standard_decade','16'), ('alternative_decade','20')]:
        n = one(cs_status,outcome='amws_per_1000_births',spec='births_level',timing=timing)['n_events']
        assert n == old, 'Event eligibility changed; update all related captions before publication'
    elite = read(ROOT / 'results/elite_schools/event_study_decennial_1800_1960' / TAG / 'callaway_santanna_estimates.csv')
    def school(event, geo='boroughs_sep', timing='exposure14', outcome='amws_per_1000_births'):
        return one(elite,e=event,outcome=outcome,timing_mode=timing,geography=geo)
    e0, e30, e50 = school(0), school(30), school(50)
    text = text.replace('roughly 0.08 additional AMWS', 'roughly ' + number(e30['estimate'],2) + ' additional AMWS')
    text = text.replace('about 0.22 after five decades', 'about ' + number(e50['estimate'],2) + ' after five decades')
    post = [school(e) for e in (0,10,20,30,40,50)]
    significant = sum(float(r['ci_low']) > 0 or float(r['ci_high']) < 0 for r in post)
    result = (r'Figure~\ref{fig:elite_es_per_1000_births} reports the refreshed AMWS rate estimates. Under exposure-14 timing, '
        r'the estimates at $e=-30$ and $e=-20$ are ' + number(school(-30)['estimate']) + ' and '
        + number(school(-20)['estimate']) + r'. The post-treatment estimates are ' + number(e0['estimate'])
        + r' at $e=0$, ' + number(e30['estimate']) + r' at $e=+30$, and ' + number(e50['estimate'])
        + r' at $e=+50$, in AMWS scientists per 1,000 births. The corresponding standard errors at $e=0$ and $e=+50$ are '
        + number(e0['se']) + ' and ' + number(e50['se']) + '. '
        + (r'No post-treatment pointwise 95\% interval excludes zero.' if significant == 0 else
           f'{significant} of the six post-treatment pointwise 95\\% intervals exclude zero.')
        + r' The seven treated counties provide limited precision, and the divergent population paths remain a limitation on causal interpretation. Opening timing is retained as the original timing diagnostic.\par')
    text = paragraph(text,r'Figure~\ref{fig:elite_es_per_1000_births} reports', result)
    rates=[]
    for geo in ('boroughs_sep','merge_nyc','drop_nyc'):
        rows=[school(e,geo=geo) for e in (0,10,20,30,40,50)]
        rates.append(sum(float(r['estimate']) for r in rows)/len(rows))
    pre=[max(abs(float(school(e,geo=geo)['estimate'])) for e in (-30,-20,-10)) for geo in ('boroughs_sep','merge_nyc','drop_nyc')]
    result = ('Five of the thirteen high-access schools are in New York City. Holding the existing estimator, timing and comparison rules fixed, '
        r'the simple mean of supported post-treatment AMWS rate estimates ($e=0$ through $+50$, exposure-14 timing) is '
        + number(rates[0]) + ' with separate boroughs, ' + number(rates[1]) + ' with merged New York City, and '
        + number(rates[2]) + ' with New York City excluded. These are descriptive averages of the plotted coefficients, '
        'rather than separately estimated pooled ATTs. The largest absolute estimates between '
        r'$e=-30$ and $e=-10$ are ' + ', '.join(number(x) for x in pre)
        + ', respectively. Geography sensitivity and the population-trend concern remain relevant; '
        r'neither the sign of an average nor flat selected leads establishes a causal school effect.\par')
    text = paragraph(text,'Five of the thirteen high-access schools',result)
    continuous = {timing:read(ROOT / 'results/elite_schools/continuous_treatment_panel' / ('timing_' + timing) / 'continuous_treatment_estimates.csv')
                  for timing in ('opening','exposure14')}
    def continuous_values(outcome):
        return [float(one(continuous[t],outcome=outcome)['beta_per_1000_seats']) for t in ('opening','exposure14')]
    values=continuous_values('amws_per_1000_births')
    result = (r'Table~\ref{tab:elite_continuous_treatment} reports the refreshed descriptive capacity regressions. Per 1,000 seats, '
        'the estimated AMWS rate changes are ' + number(values[0]) + ' under opening timing and '
        + number(values[1]) + ' under exposure-14 timing. The corresponding count changes are '
        + ' and '.join(number(x,2) for x in continuous_values('n_amws'))
        + '. The table reports county-clustered standard errors and significance from the saved models. '
        r'The unchanged Wikipedia and population outcomes provide the same comparison and selection checks as before.\par')
    text = paragraph(text,'Every AMWS outcome loads',result)
    fairs={}
    for timing,folder in [('standard','fair_cohorts_1840_1900'),('alternative','g_shift')]:
        path=ROOT / 'results/worlds_fair' / ('worlds_fairs_us_amws_event_studies_' + folder + '_treatment_m20_e_m30_p50_balanced_oldest_logpop_m10_single_fair')
        fairs[timing]=read(path/'all_simple_att.csv')
    result = (r'Figure~\ref{fig:wf_es_n_amws} reports scientist counts and Figure~\ref{fig:wf_es_per_1000_births} '
        'reports AMWS scientists per 1,000 estimated births. The refreshed overall post-treatment count ATTs are ')
    for i,timing in enumerate(('standard','alternative')):
        r=one(fairs[timing],treatment_spec='hosted',outcome='n_amws')
        result += (' and ' if i else '') + number(r['estimate'],1) + r' (95\% CI ' + interval(r) + ') under ' + timing + ' timing'
    result += '. The rate ATTs are '
    rate_rows=[]
    for i,timing in enumerate(('standard','alternative')):
        r=one(fairs[timing],treatment_spec='hosted',outcome='amws_per_1000_births');rate_rows.append(r)
        result += (' and ' if i else '') + number(r['estimate']) + r' (95\% CI ' + interval(r) + ')'
    result += '. '
    result += ('Both rate intervals include zero; the lead-20 specification provides no robust evidence of an increase in scientist production per birth.'
               if all(float(r['ci_low']) <= 0 <= float(r['ci_high']) for r in rate_rows) else
               'The timing-specific rate estimates and their intervals should be read alongside the pre-treatment and denominator checks.')
    result += r' Figure~\ref{fig:wf_es_county_births} retains the original estimated-births check.'
    text = paragraph(text,r'Figure~\ref{fig:wf_es_n_amws} reports effects',result)
    expanded = []
    for timing, folder in [('standard','fair_cohorts_1840_1900'),('alternative','g_shift')]:
        path = ROOT / 'results/worlds_fair' / ('worlds_fairs_us_amws_event_studies_' + folder
            + '_treatment_m20_e_m50_p50_balanced_oldest_logpop_m10_single_fair')
        status = one(read(path / 'all_model_status.csv'), treatment_spec='hosted', outcome='amws_per_1000_births')
        assert status['ok'] == 'TRUE' and status['n_treated_units'] == '4'
        expanded.append(one(read(path / 'all_simple_att.csv'), treatment_spec='hosted', outcome='amws_per_1000_births'))
    result = (r'As a support robustness check, Figure~\ref{fig:wf_es_per_1000_births_m50} adds two pre-treatment periods, '
        r'$e=-50$ and $e=-40$, while retaining the same post-treatment horizon. Requiring a fully balanced eleven-period '
        'window reduces the hosted sample from six to four events under both timing conventions: the Southern Exposition '
        'in Jefferson County, the Cincinnati Centennial Exposition in Hamilton County, the Tennessee Centennial and '
        'International Exposition in Davidson County, and the Hudson--Fulton Celebration in Kings County. The World Cotton '
        'Centennial in Orleans Parish and the International Industrial Fair in Erie County are lost because their counties '
        'lack the added early support. The refreshed overall post-treatment rate effects are '
        + number(expanded[0]['estimate']) + r' scientists per 1{,}000 births under standard timing (95\% CI '
        + interval(expanded[0]) + ') and ' + number(expanded[1]['estimate'])
        + r' under alternative timing (95\% CI ' + interval(expanded[1])
        + '). Both intervals include zero. The added leads and smaller sample retain the original support diagnostic.')
    assert all(float(r['ci_low']) <= 0 <= float(r['ci_high']) for r in expanded)
    text = paragraph(text,'As a support robustness check, Figure',result)
    text = text.replace('The US county event studies are suggestive of higher scientist production only at long horizons, forty to fifty years after a fair.',
        'The US county count estimates rise at long horizons, while effects per birth remain imprecise.')
    # Document the supplement without relabeling the historical 1,000-entry audit.
    note = (r'The AMWS estimates refreshed in this document use the published October 11, 2026 reviewed OCR supplement. '
        r'Synthetic DiD specifications retain their original data vintage. '
        r'The expanded 1986 final distribution contains 99{,}308 records across countries. After the existing US geography, '
        'year and panel eligibility rules, it contributes ' + newer + ' records to the combined county panel. '
        r'The cleaning diagnostics and 1,000-entry audit below describe the earlier DOCX extraction; they are historical checks, '
        r'not an accuracy estimate for the appended supplement. The supplement has its own source, disposition and scan-review ledgers.\par' + '\n\n')
    needle=r'\subsection{American Men and Women of Science }'
    assert needle in text
    text=text.replace(needle,needle+'\n'+note,1)
    (PROJECT / 'main.tex').write_text(text,encoding='utf-8')
    table=(FROZEN/'tables/elite_continuous_treatment.tex').read_text(encoding='utf-8')
    labels={'n_amws':'AMWS scientists born (count)', 'amws_per_1000_pop':r'AMWS per 1{,}000 population',
            'amws_per_1000_births':r'AMWS per 1{,}000 births'}
    for outcome,label in labels.items():
        rows=[one(continuous[t],outcome=outcome) for t in ('opening','exposure14')]
        assert all(int(r['n']) == 165351 for r in rows)
        cells=[]
        precision = 5 if outcome == 'amws_per_1000_pop' else 3
        for r in rows:
            p=float(r['Pr(>|t|)']);stars='***' if p<.01 else '**' if p<.05 else '*' if p<.1 else ''
            cells += [number(r['beta_per_1000_seats'],precision) + ('$^{' + stars + '}$' if stars else ''),
                      '(' + number(float(r['Std. Error'])*1000,precision) + ')']
        cells.append(number(rows[0]['mean_y'],precision))
        line=label+' & '+' & '.join(cells)+r' \\'
        table,count=re.subn(re.escape(label)+r' & [^\n]*',lambda _:line,table,count=1)
        assert count==1
    (PROJECT/'tables/elite_continuous_treatment.tex').write_text(table,encoding='utf-8')
    (RUN / 'manuscript_values.json').write_text(json.dumps({'sources': SOURCES, 'selected_rows': SELECTED_ROWS}, indent=2))
    print('Updated manuscript prose and continuous-treatment table')


if __name__ == '__main__':
    assets()
    update_main()

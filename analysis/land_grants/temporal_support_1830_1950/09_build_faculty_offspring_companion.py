"""Build faculty_offspring_channel.pdf, the report companion for the
faculty-offspring channel of the college-allocation result.

Three figures in three versions: Figures 1-3 with faculty children scaled to
total college faculty (the census coverage factor), Figures 4-6 with the same
OCC1950 010-029 count unscaled, Figures 7-9 scaled by each county's faculty
headcount (build_professor_county_coverage.R). Each version has two standard-decade figures
and the break-even p* figure (both timings), with descriptive notes, a section
title and a subsection title per version, nothing else. Every number in the
notes is read from the saved outputs of 08_run_professor_channel.R,
build_professor_coverage.R, build_professor_county_coverage.R and the
children panel's build manifest; this
script estimates nothing. The layout follows project/report_companion.tex.

Run from <repo>:
    python analysis/land_grants/temporal_support_1830_1950/09_build_faculty_offspring_companion.py
Output: <data root>/results/land_grants/professor_channel/companion/faculty_offspring_channel.pdf
"""
import csv
import hashlib
import json
import os
import shutil
import subprocess
from pathlib import Path

DATA_ROOT = Path(os.environ.get('TALENT_DETS_DATA_DIR')
                 or os.environ.get('DET_DIR')
                 or 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets')
RESULTS = DATA_ROOT / 'results/land_grants/professor_channel'
COVERAGE = DATA_ROOT / 'output/occupational_fertility/professor_coverage_factors.csv'
COUNTY_COVERAGE = DATA_ROOT / 'output/occupational_fertility/professor_county_coverage_factors_summary.csv'
CHANNEL_MANIFEST = DATA_ROOT / 'output/occupational_fertility/college_channel/build_manifest.json'
OUT_DIR = RESULTS / 'companion'
NAME = 'faculty_offspring_channel'
SPEC, TIMING = 'balanced_max70', 'standard_decade'
# The two versions: 08's channel name, its figure suffix and its per-1,000 outcome.
VERSIONS = {
    'faculty': {'suffix': '', 'label': 'scaled', 'outcome': 'faculty_births_per_1000_births',
                'title': 'Scaled to total college faculty'},
    'raw': {'suffix': '_unscaled', 'label': 'unscaled', 'outcome': 'prof_births_plus_deans_per_1000_births',
            'title': 'Unscaled census counts'},
    'county_faculty': {'suffix': '_county_scaled', 'label': 'county-scaled',
                       'outcome': 'county_faculty_births_per_1000_births',
                       'title': 'Scaled by county faculty headcounts'},
}
HORIZONS = (40, 50, 60, 70)
TIMING_NAMES = {'standard_decade': 'standard', 'alternative_decade': 'alternative'}
SOURCES = {}


def read(path):
    raw = Path(path).read_bytes()
    SOURCES[str(path)] = hashlib.sha256(raw).hexdigest()
    return list(csv.DictReader(raw.decode('utf-8-sig').splitlines()))


def one(rows, **filters):
    found = [r for r in rows if all(r[k] == str(v) for k, v in filters.items())]
    assert len(found) == 1, (filters, len(found))
    return found[0]


def num(value, digits=3):
    text = f'{float(value):.{digits}f}'
    # A typeset minus, not a hyphen, for negative numbers.
    return f'$-${text[1:]}' if text.startswith('-') else text


def pct(value, digits=1):
    return f'{100 * float(value):.{digits}f}\\%'


def ci(row, lo='ci_low', hi='ci_high', digits=3):
    return f'[{num(row[lo], digits)}, {num(row[hi], digits)}]'


def join_and(parts):
    return ', '.join(parts[:-1]) + ' and ' + parts[-1] if len(parts) > 1 else parts[0]


def census_span(years):
    """1850, 1860, 1870, 1880, 1900, 1910, 1940 -> '1850--1880, 1900--1910 and 1940'."""
    runs, run = [], [years[0]]
    for y in years[1:]:
        if y == run[-1] + 10:
            run.append(y)
        else:
            runs.append(run)
            run = [y]
    runs.append(run)
    return join_and([f'{r[0]}--{r[-1]}' if len(r) > 1 else f'{r[0]}' for r in runs])


manifest_raw = CHANNEL_MANIFEST.read_bytes()
SOURCES[str(CHANNEL_MANIFEST)] = hashlib.sha256(manifest_raw).hexdigest()
channel_manifest = json.loads(manifest_raw)
last_birth_year = channel_manifest['birth_years'][1]
interpolated_years = channel_manifest.get('interpolated_years') or []
census_text = census_span(sorted(channel_manifest['censuses'])) + ' full counts'
if interpolated_years:
    census_text += f'; {min(interpolated_years)}--{max(interpolated_years)} interpolated'

es = read(RESULTS / 'channel_event_studies.csv')
grid = read(RESULTS / 'adjusted_att_grid.csv')
breakeven = read(RESULTS / 'breakeven_p.csv')
p_zero = read(RESULTS / 'p_where_e50_ci_reaches_zero.csv')
counts = read(RESULTS / 'event_counts_by_horizon.csv')
factors = read(COVERAGE)
fvals = [float(r['faculty_factor']) for r in factors]
county_summary = read(COUNTY_COVERAGE)
county_medians = [float(r['median_factor_scaled']) for r in county_summary if r['median_factor_scaled']]
county_max = max(float(r['max_factor']) for r in county_summary if r['max_factor'])

amws50 = one(es, spec=SPEC, timing=TIMING, outcome='amws_per_1000_births', sample='original',
             event_time=50)
events = {e: one(counts, timing=TIMING, event_time=e) for e in (50, 60, 70)}
n_events = events[50]['events_original']
assert events[50]['events_common'] == n_events, 'e = +50 should rest on every event'
fewer = [e for e in (60, 70) if events[e]['events_common'] != n_events]
counts_by = {(t, e): one(counts, timing=t, event_time=e) for t in TIMING_NAMES for e in HORIZONS}
# The last birth decade complete under both binnings (standard 1940 = 1940-1949,
# alternative 1940 = 1937-1946); an event allocated in decade g lacks faculty
# data at horizon e when g + e is later than that.
last_decade = (last_birth_year + 1) // 10 * 10 - 10


def dropped_cohort(e):
    """'the 1880s or later': allocations whose cohort at horizon e has no census reading."""
    return f'the {last_decade - e + 10}s or later'


def amws_at(e, sample, timing=TIMING):
    return one(es, spec=SPEC, timing=timing, outcome='amws_per_1000_births', sample=sample,
               event_time=e)['estimate']


def fewer_explained():
    """Why the hollow points differ, with the AMWS effect on both event sets."""
    if not fewer:
        return ''
    parts = [f'At $e = +{e}$ only {events[e]["events_common"]} of the {n_events} events have '
             f'faculty-birth data, because for colleges allocated in {dropped_cohort(e)} the children '
             f'born {e} years on fall after {last_birth_year}, the last birth year any census in hand '
             'observes.' for e in fewer]
    effects = '; '.join(f'at $e = +{e}$ the AMWS effect is {num(amws_at(e, "common"))} on those '
                        f'{events[e]["events_common"]} events against {num(amws_at(e, "original"))} on all '
                        f'{n_events} in Panel A' for e in fewer)
    return (' Hollow, lighter points mark horizons estimated on fewer events. ' + ' '.join(parts) +
            ' Those points therefore compare a different set of colleges and are not directly '
            f'comparable with the rest of the line ({effects}).')


def notes_for(channel):
    """Caption notes for the three figures of one version."""
    v = VERSIONS[channel]
    fac50 = one(es, spec=SPEC, timing=TIMING, outcome=v['outcome'], sample='common', event_time=50)
    adj = {p: one(grid, spec=SPEC, timing=TIMING, channel=channel, measure='per_1000_births',
                  p=p, event_time=50) for p in ('0', '0.02', '0.05')}
    assert abs(float(adj['0']['att_adjusted']) - float(amws50['estimate'])) < 1e-9, \
        'p = 0 must equal Figure 2 at e = +50 (same events there)'
    be = one(breakeven, spec=SPEC, timing=TIMING, channel=channel, measure='per_1000_births',
             event_time=50)
    pz = one(p_zero, spec=SPEC, timing=TIMING, channel=channel, measure='per_1000_births')

    scaling = {
        'faculty': 'rescaled in each census by its coverage of NCES male professional staff '
                   f'(factors {min(fvals):.2f}--{max(fvals):.2f})',
        'raw': 'counted as enumerated, without coverage rescaling',
        'county_faculty': 'rescaled in each county and census by the faculty headcount in the Education by '
                          'the Numbers panel over the coded professors (both sexes) where the headcount is '
                          'larger, and not scaled otherwise; among scaled event counties the median factor '
                          f'runs {min(county_medians):.1f}--{max(county_medians):.1f} by census, up to '
                          f'{county_max:.0f}. The largest factors fall on college counties the census coded '
                          'with only a handful of professors, so a few families weigh heavily and the '
                          'intervals are wide'}[channel]
    fewer_sentence = fewer_explained()
    channel_note = (
        'Notes: Panel A is the main-text Figure 2 series: AMWS scientists born per 1,000 estimated '
        f'county births, fully balanced sample $e \\in [-20, +70]$, {n_events} events, standard decades. '
        'Estimation follows Callaway and Sant\'Anna (2021) with runner-up counties as never-treated '
        'controls and baseline estimated births as covariate; effects are normalised to $e = 0$ and '
        f'bars are 95\\% confidence intervals. At $e = +50$ the effect is {num(amws50["estimate"])} '
        f'{ci(amws50)}. Panel B applies the same estimator to children born to college faculty per '
        '1,000 estimated births: surviving co-resident children of men coded OCC1950 010--029, each '
        f'birth year read from the census that observes it youngest ({census_text}), {scaling}. '
        f'At $e = +50$ it is {num(fac50["estimate"], 2)} {ci(fac50, digits=2)}.' + fewer_sentence)

    adjusted_note = (
        'Notes: AMWS scientists per 1,000 births minus $p$ times faculty children per 1,000 births '
        f'({v["label"]}), where $p$ is the share of faculty children assumed to become AMWS '
        'scientists. Estimates are exact on the sample with faculty-birth data, by linearity of the '
        'estimator, with 95\\% confidence intervals from the joint influence functions. At $e = +50$ '
        f'the effect is {num(adj["0"]["att_adjusted"])} for $p = 0$, {num(adj["0.02"]["att_adjusted"])} '
        f'{ci(adj["0.02"])} for $p = 2\\%$ and {num(adj["0.05"]["att_adjusted"])} {ci(adj["0.05"])} '
        f'for $p = 5\\%$; it remains significant at 5\\% up to $p \\approx$ {pct(pz["p_ci_reaches_zero"])}. '
        'The break-even rate at which faculty children account for the whole effect is '
        f'{pct(be["p_star"])} (Fieller 95\\% interval {pct(be["p_star_fieller_lo"])}--'
        f'{pct(be["p_star_fieller_hi"])}).' + (
            ' Hollow, lighter points mark ' + ' and '.join(f'$e = +{e}$' for e in fewer) +
            ', where only the events with faculty-birth data enter (' +
            '; '.join(f'{events[e]["events_common"]} of {n_events} at $+{e}$' for e in fewer) +
            '; see the channel figure), so the $p = 0$ line there is the AMWS effect on those events (' +
            '; '.join(f'{num(amws_at(e, "common"))} at $+{e}$' for e in fewer) +
            ') rather than the all-event Figure 2 (' +
            '; '.join(f'{num(amws_at(e, "original"))}' for e in fewer) + ').'
            if fewer else ''))

    # Break-even figure: p* by horizon and timing, as plotted by 08 (e >= +40, bounded Fieller only).
    be_rows = {(t, e): one(breakeven, spec=SPEC, timing=t, channel=channel, measure='per_1000_births',
                           event_time=e) for t in TIMING_NAMES for e in HORIZONS}
    plotted = {k: r for k, r in be_rows.items() if r['fieller_bounded'] == 'TRUE'}
    omitted = sorted(set(be_rows) - set(plotted))
    assert all(be_rows[k]['fieller_bounded'] == 'FALSE' for k in omitted)

    def be_series(timing):
        parts = []
        for e in HORIZONS:
            if (timing, e) not in plotted:
                continue
            r = plotted[(timing, e)]
            text = f'{pct(r["p_star"])} at $+{e}$'
            if e == 50:
                text = (f'{pct(r["p_star"])} ({pct(r["p_star_fieller_lo"])}--'
                        f'{pct(r["p_star_fieller_hi"])}) at $+{e}$')
            parts.append(text)
        return join_and(parts)

    def fewer_series(timing):
        """'standard +60 and +70 rest on 10 and 6 of 16 events', or '' if no plotted point loses events."""
        rows = [(e, counts_by[(timing, e)]) for e in HORIZONS if (timing, e) in plotted
                and counts_by[(timing, e)]['events_common'] != counts_by[(timing, e)]['events_original']]
        if not rows:
            return ''
        total = rows[0][1]['events_original']
        assert all(r['events_original'] == total for _, r in rows)
        return f'{TIMING_NAMES[timing]} ' + join_and([
            f'$+{e}$ rests on {r["events_common"]} of {total} events (colleges allocated in '
            f'{dropped_cohort(e)} drop out)' for e, r in rows])

    omitted_sentence = ''
    if omitted:
        names = ' and '.join(f'{TIMING_NAMES[t]} $e = +{e}$' for t, e in omitted)
        verb = 'is' if len(omitted) == 1 else 'are'
        omitted_sentence = (f' {names[0].upper() + names[1:]} {verb} omitted because the interval is '
                            'unbounded (the faculty-children effect is not significantly positive there).')
    fewer_parts = [s for s in (fewer_series(t) for t in TIMING_NAMES) if s]
    fewer_breakeven = (f' Because no census in hand observes births after {last_birth_year}, '
                       f'{"; ".join(fewer_parts)}. Both effects in the ratio are estimated on those '
                       'events only, so these points describe a different set of colleges from the '
                       'earlier horizons.' if fewer_parts else '')
    # 08 caps the axis at 60% when an interval is very wide; say which bars it cuts.
    cut = [k for k, r in sorted(plotted.items()) if float(r['p_star_fieller_hi']) > 0.6]
    cut_sentence = (' The axis stops at 60\\%, which cuts the upper end of the interval at ' +
                    ' and '.join(f'{TIMING_NAMES[t]} $+{e}$' for t, e in cut) + '.' if cut else '')
    multiple = float(be['p_star']) / float(be['base_rate'])
    breakeven_note = (
        'Notes: Break-even transmission rate $p^* = \\text{ATT(AMWS per 1,000 births)} / '
        f'\\text{{ATT(faculty children per 1,000 births, {v["label"]})}}$: the share of faculty '
        'children who would have to become AMWS scientists for them to account for the whole effect. '
        'Both effects are estimated on the sample with faculty-birth data; bars are Fieller 95\\% '
        'intervals from the joint influence functions. Shown from $e = +40$, where faculty children '
        f'rise clearly. Standard decades: {be_series("standard_decade")}. Alternative decades: '
        f'{be_series("alternative_decade")}.' + cut_sentence + omitted_sentence + fewer_breakeven +
        f' At standard $e = +50$, $p^*$ is about {multiple:.0f} times the AMWS base rate per birth in '
        f'the runner-up counties, birth decades 1880--1930 ({pct(be["base_rate"], 2)}).')
    return channel_note, adjusted_note, breakeven_note


OUT_DIR.mkdir(parents=True, exist_ok=True)
body = []
figure_number = 0
for channel, v in VERSIONS.items():
    files = {'channel': f'fig_channel_paths_per1000_standard_decade{v["suffix"]}_pdf.pdf',
             'adjusted': f'fig_figure2_adjusted_standard_decade{v["suffix"]}_pdf.pdf',
             'breakeven': f'fig_breakeven_p{v["suffix"]}_pdf.pdf'}
    for fig in files.values():
        src = RESULTS / fig
        SOURCES[str(src)] = hashlib.sha256(src.read_bytes()).hexdigest()
        shutil.copy2(src, OUT_DIR / fig)
    notes = notes_for(channel)
    captions = [
        'College allocation: AMWS scientists per 1,000 births and children born to college faculty '
        f'(standard decades, {v["label"]})',
        f'AMWS scientists per 1,000 births after removing faculty children (standard decades, {v["label"]})',
        f'Break-even share of faculty children who would have to become AMWS scientists ({v["label"]})']
    for i, (key, caption, note) in enumerate(zip(('channel', 'adjusted', 'breakeven'), captions, notes)):
        figure_number += 1
        if figure_number > 1:
            body.append(r'\clearpage')
        if i == 0:
            body.append(r'\subsection*{' + v['title'] + '}')
        body.append(r'''\begin{figure}[H]
\centering
\caption{''' + caption + r'''}
\includegraphics[width=\linewidth]{''' + files[key] + r'''}
\caption*{\footnotesize ''' + note + r'''}
\end{figure}
''')

header = ['% Companion to Reports - Joao Francisco: faculty offspring channel of the college-allocation result.',
          '% Built by analysis/land_grants/temporal_support_1830_1950/09_build_faculty_offspring_companion.py',
          '% from saved outputs only; no models are estimated here.']
header += [f'% SOURCE: {path}\n% SHA256: {digest}' for path, digest in SOURCES.items()]
tex = '\n'.join(header) + r'''
\documentclass[11pt]{article}
\usepackage[a4paper,margin=18mm]{geometry}
\usepackage[T1]{fontenc}
\usepackage{lmodern,amsmath,float,caption,microtype,graphicx}
\captionsetup{font=small,labelfont=bf,skip=5pt}
\setlength{\parindent}{0pt}
\setlength{\parskip}{3pt}
\pagestyle{empty}
\begin{document}
\section*{Faculty offspring channel}
''' + '\n'.join(body) + r'''
\end{document}
'''
tex_path = OUT_DIR / f'{NAME}.tex'
tex_path.write_text(tex, encoding='utf-8')
for _ in range(2):
    run = subprocess.run(['pdflatex', '-interaction=nonstopmode', '-halt-on-error', tex_path.name],
                         cwd=OUT_DIR, capture_output=True, text=True)
    assert run.returncode == 0, run.stdout[-3000:]
log = (OUT_DIR / f'{NAME}.log').read_text(encoding='latin-1')
assert 'undefined' not in log.lower() or 'Reference' not in log, 'Undefined references in the log'
for ext in ('aux', 'log', 'out'):
    (OUT_DIR / f'{NAME}.{ext}').unlink(missing_ok=True)
print(f'Wrote {OUT_DIR / (NAME + ".pdf")} ({(OUT_DIR / (NAME + ".pdf")).stat().st_size:,} bytes)')

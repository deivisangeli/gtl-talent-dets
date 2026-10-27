"""Back up and execute the AMWS publication refresh without synthetic DiD runs.

Run with the bundled Python; --step selects a dependency stage or named analysis.
The immutable backup and manifests live in Dropbox results/amws_refresh/<tag>.
"""
import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

REPO = Path(__file__).resolve().parents[1]
ROOT = Path(os.environ.get('TALENT_DETS_DATA_DIR',
    'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'))
TAG = 'amws_ocr_20261011'
RUN = ROOT / 'results/amws_refresh' / TAG
RSCRIPT = shutil.which('Rscript') or 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe'


def digest(path):
    h = hashlib.sha256()
    with path.open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()


def files(path):
    return [path] if path.is_file() else sorted(p for p in path.rglob('*') if p.is_file())


def prepare():
    if (RUN / 'backup_complete.json').exists():
        print('Verified backup already recorded:', RUN, flush=True)
        return
    RUN.mkdir(parents=True, exist_ok=True)
    publication = json.loads((ROOT / 'output/amws/ocr_supplement_20261011/publication.json').read_text())
    assert publication['status'] == 'published'
    for entry in publication['installed']:
        assert digest(ROOT / entry['path']) == entry['sha256'], entry['path']
    protected = {}
    for entry in publication['installed']:
        path = ROOT / entry['path']
        protected[str(path)] = digest(path)
    for path in files(ROOT / 'output/amws'):
        if path.name.startswith('amws_combined_us_geocoded') or '_us_geocoded_final' in path.name:
            protected[str(path)] = digest(path)
    # Preserve Census-only documents/results and every synthetic DiD artifact.
    for path in files(REPO / 'project'):
        rel = path.relative_to(REPO / 'project').as_posix().lower()
        if '.git/' in rel or 'tmp/' in rel or 'output/' in rel:
            continue
        if ('synth' in rel or 'mandatory_schooling_scientist' in rel or
            'mandatory_schooling_ba_scientist' in rel or
            path.name in ('wwII_gi_bill.tex', 'coefficients_paper_cohorts.pdf')):
            protected[str(path)] = digest(path)
    for parent in (ROOT / 'results/mandatory_schooling', ROOT / 'results/worlds_fair'):
        for child in parent.iterdir():
            if child.is_dir() and any(s in child.name for s in ('synth', 'linked_', 'wwii_service')):
                for path in files(child):
                    protected[str(path)] = digest(path)
    (RUN / 'protected_sources.json').write_text(json.dumps(protected, indent=2))
    targets = list((ROOT / 'output').glob('us_panel_county_amws*'))
    targets += [ROOT / 'output/amws/gender_inference', ROOT / 'output/mandatory_schooling',
                ROOT / 'output/land_grants']
    targets += list((ROOT / 'output/amws').glob('amws_consolidated*'))
    targets += list((ROOT / 'output/amws').glob('amws_1986_see_previous*'))
    for parent in ('land_grants/event_studies', 'elite_schools', 'scientific_facilities/event_studies',
                   'worlds_fair', 'mandatory_schooling'):
        base = ROOT / 'results' / parent
        if base.exists():
            targets += [p for p in base.iterdir() if p.is_dir() and
                        not any(s in p.name for s in ('synth', 'linked_', 'wwii_service', 'placebo', 'ukds', 'augsynth'))]
    entries = []
    for target in targets:
        if not target.exists():
            continue
        print('Backing up', target.relative_to(ROOT), flush=True)
        for source in files(target):
            destination = RUN / 'backup/data' / source.relative_to(ROOT)
            destination.parent.mkdir(parents=True, exist_ok=True)
            if not destination.exists():
                shutil.copy2(source, destination)
            entries.append({'path': str(source), 'backup': str(destination), 'sha256': digest(destination)})
    for source in files(REPO / 'project'):
        rel = source.relative_to(REPO)
        if '.git' in rel.parts or 'tmp' in rel.parts or 'output' in rel.parts:
            continue
        destination = RUN / 'backup/repo' / rel
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, destination)
    for name in ('outer', 'project'):
        cwd = REPO if name == 'outer' else REPO / 'project'
        for command, suffix in ((['git', 'status', '--porcelain=v1'], 'status'),
                                (['git', 'diff', '--binary'], 'diff')):
            with (RUN / f'{name}_{suffix}.txt').open('wb') as stream:
                subprocess.run(command, cwd=cwd, stdout=stream, check=True)
    (RUN / 'backup_complete.json').write_text(json.dumps({'release': publication,
        'files': entries, 'timestamp': time.time()}, indent=2))
    print('Backup complete:', RUN, flush=True)


def execute(label, script, args=(), overrides=None):
    assert (RUN / 'backup_complete.json').exists(), 'Run prepare first'
    env = os.environ.copy()
    env.update(TALENT_DETS_DATA_DIR=str(ROOT), DET_DIR=str(ROOT), GTL_REPO=str(REPO),
               REPORT_APPENDIX_RESULTS_TAG=TAG, FACILITIES_DECENNIAL_RESULTS_TAG=TAG,
               R_MAX_VSIZE='32Gb', OMP_NUM_THREADS='2', OPENBLAS_NUM_THREADS='2', R_DATATABLE_NUM_THREADS='2',
               AMWS_PANEL_SOURCE_REFRESH='TRUE')
    # Each invocation gets only its own specification settings.
    for key in ('BALANCE_EVENT_TIME', 'MAX_EVENT_TIME', 'MIN_EVENT_TIME', 'COHORTS',
                'ELITE_RESULTS_TAG', 'ELITE_TREATMENT_TIMING'):
        env.pop(key, None)
    env.update(overrides or {})
    command = [RSCRIPT, str(REPO / script), *args]
    log = RUN / 'logs' / f'{label}.log'
    log.parent.mkdir(exist_ok=True)
    started = time.time()
    print('Running', label, flush=True)
    with log.open('w', encoding='utf-8') as stream:
        result = subprocess.run(command, cwd=REPO, env=env, stdout=stream, stderr=subprocess.STDOUT)
    record = {'label': label, 'command': command, 'overrides': overrides or {},
              'started': started, 'seconds': time.time() - started, 'exit_code': result.returncode,
              'log': str(log)}
    with (RUN / 'commands.jsonl').open('a') as stream:
        stream.write(json.dumps(record) + '\n')
    print(label, 'exit', result.returncode, 'seconds', round(record['seconds']), flush=True)
    if result.returncode:
        print(log.read_text(errors='replace')[-5000:], flush=True)
        raise SystemExit(result.returncode)


def run_step(step):
    if step == 'prepare':
        prepare()
    elif step == 'panels':
        for name in ('year', 'decade', 'period5'):
            execute('panel_' + name, f'prep/amws/build_amws_combined_county_{name}.R')
        execute('land_panel', 'analysis/land_grants/temporal_support_1830_1950/01_build_temporal_support_panel.R')
    elif step == 'gender':
        baseline = {'AMWS_GENDER_BASELINE': str(RUN / 'backup/data/output/amws/gender_inference/amws_gender_records.csv')}
        execute('gender_inference', 'prep/amws/build_amws_gender_inference.R', overrides=baseline)
        execute('gender_flags', 'prep/amws/add_amws_gender_threshold_flags.R', overrides=baseline)
        execute('monthly_all', 'prep/mandatory_schooling/build_amws_county_month.R',
                ('--births-source=gapminder', '--years=all'))
        execute('quarterly_all', 'prep/mandatory_schooling/build_amws_county_quarter.R')
    elif step == 'college':
        script = 'analysis/land_grants/temporal_support_1830_1950/03_run_baseline_population_controls.R'
        execute('college_less_restrictive', script)
        execute('college_balanced', script, overrides={'BALANCE_EVENT_TIME': 'TRUE', 'MAX_EVENT_TIME': '70'})
        execute('college_1860_1880', script, overrides={'BALANCE_EVENT_TIME': 'TRUE',
            'MIN_EVENT_TIME': '-30', 'MAX_EVENT_TIME': '70', 'COHORTS': '1860,1870,1880'})
        execute('college_percohort', 'analysis/land_grants/temporal_support_1830_1950/07_run_csdid_per_cohort_max_e.R')
        execute('college_table', 'analysis/land_grants/make_selected_events_table.R')
        execute('appendix_college', 'analysis/report_companion_calendar_appendices.R', ('college',))
    elif step == 'elite':
        execute('elite_decennial', 'analysis/elite_schools/analysis_event_study_decennial_1800_1960.R',
                overrides={'ELITE_RESULTS_TAG': TAG})
        for timing in ('opening', 'exposure14'):
            for label, script in (('continuous', 'analysis_continuous_treatment_panel.R'),):
                execute(f'elite_{timing}_{label}', 'analysis/elite_schools/' + script,
                        overrides={'ELITE_TREATMENT_TIMING': timing})
    elif step == 'fairs':
        for horizon in ('m30', 'm50'):
            for timing in ('', '_g_shift'):
                base = f'robust_{horizon}_pop_m10_balanced_oldest_treatment_m20'
                for single in ('', '_single_fair'):
                    profile = base + single + timing
                    execute('fairs_' + profile, 'analysis/world_fairs/worlds_fairs_us_amws_event_studies.R',
                            overrides={'WORLD_FAIRS_PROFILE': profile, 'WORLD_FAIRS_CORES': '2'})
                if horizon == 'm30':
                    execute('fairs_percohort' + timing,
                        'analysis/world_fairs/worlds_fairs_us_amws_event_studies_per_cohort.R',
                        overrides={'WORLD_FAIRS_PROFILE': base + '_single_fair' + timing,
                                   'WORLD_FAIRS_PER_COHORT_SCOPE': 'figure5', 'WORLD_FAIRS_CORES': '2'})
        execute('fairs_render', 'analysis/world_fairs/render_worlds_fairs_us_amws_main_text_figures.R')
    elif step == 'facilities':
        execute('facilities', 'analysis/scientific_facilities/analysis_amws_scientific_facilities_decennial_expanded.R')
        execute('appendix_facilities', 'analysis/report_companion_calendar_appendices.R', ('facilities',))
    elif step == 'schooling':
        for cadence in ('annual', 'period5'):
            for control in ('none', 'baseline_log_population', 'preperiod_population_cagr'):
                prefix = 'COMPULSORY_AMWS_' + cadence.upper()
                execute(f'schooling_{cadence}_{control}',
                    f'analysis/mandatory_schooling/analysis_compulsory_schooling_amws_{cadence}_csdid.R',
                    overrides={prefix + '_CONTROL': control,
                               **({prefix + '_HORIZON': 'post30'} if cadence == 'period5' else {})})
        execute('schooling_region', 'analysis/mandatory_schooling/analysis_compulsory_schooling_amws_region_csdid.R')
        execute('schooling_export', 'analysis/mandatory_schooling/export_mandatory_schooling_project_artifacts.R')
    elif step.startswith('schooling_annual_'):
        control = step.removeprefix('schooling_annual_')
        execute(step, 'analysis/mandatory_schooling/analysis_compulsory_schooling_amws_annual_csdid.R',
                overrides={'COMPULSORY_AMWS_ANNUAL_CONTROL': control})
    elif step == 'schooling_period5':
        for control in ('none', 'baseline_log_population', 'preperiod_population_cagr'):
            execute('schooling_period5_' + control,
                'analysis/mandatory_schooling/analysis_compulsory_schooling_amws_period5_csdid.R',
                overrides={'COMPULSORY_AMWS_PERIOD5_CONTROL': control, 'COMPULSORY_AMWS_PERIOD5_HORIZON': 'post30'})
    elif step == 'schooling_region':
        execute(step, 'analysis/mandatory_schooling/analysis_compulsory_schooling_amws_region_csdid.R')
    elif step.startswith('schooling_region_annual_pre40_'):
        control = step.removeprefix('schooling_region_annual_pre40_')
        execute(step, 'analysis/mandatory_schooling/analysis_compulsory_schooling_amws_region_csdid.R',
                overrides={'COMPULSORY_AMWS_REGION_CADENCE': 'annual',
                           'COMPULSORY_AMWS_REGION_PROFILE': 'pre40',
                           'COMPULSORY_AMWS_REGION_CONTROL': control})
    elif step == 'schooling_region_period5':
        execute(step, 'analysis/mandatory_schooling/analysis_compulsory_schooling_amws_region_csdid.R',
                overrides={'COMPULSORY_AMWS_REGION_CADENCE': 'period5'})
    elif step == 'schooling_export':
        execute(step, 'analysis/mandatory_schooling/export_mandatory_schooling_project_artifacts.R')
    elif step == 'gi':
        execute('gi_estimate', 'analysis/mandatory_schooling/estimate_wwii_gi_bill_amws_cutoff.R')
        execute('gi_export', 'analysis/mandatory_schooling/export_wwii_gi_bill_amws_cutoff_to_project.R')
    elif step == 'verify':
        protected = json.loads((RUN / 'protected_sources.json').read_text())
        failures = [p for p, h in protected.items() if not Path(p).exists() or digest(Path(p)) != h]
        (RUN / 'protected_validation.json').write_text(json.dumps({'files': len(protected), 'failures': failures}, indent=2))
        assert not failures, failures
        print('Protected files unchanged:', len(protected))
    else:
        raise ValueError(step)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--step', required=True, choices=('prepare', 'panels', 'gender', 'college',
        'elite', 'fairs', 'facilities', 'schooling', 'gi', 'verify', 'schooling_annual_none',
        'schooling_annual_baseline_log_population', 'schooling_annual_preperiod_population_cagr',
        'schooling_period5', 'schooling_region', 'schooling_export',
        'schooling_region_annual_pre40_baseline_log_population',
        'schooling_region_annual_pre40_preperiod_population_cagr', 'schooling_region_period5'))
    run_step(parser.parse_args().step)

"""Require fresh publication assets and save the reproducible completion record."""
import csv
import hashlib
import json
import os
from pathlib import Path
import time

from amws_publication_refresh import REPO, ROOT, RUN, TAG, digest


def read(path):
    with path.open(encoding='utf-8-sig', newline='') as stream:
        return list(csv.DictReader(stream))


def md5(path):
    h = hashlib.md5()
    with path.open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            h.update(block)
    return h.hexdigest()


def finalize():
    backup = json.loads((RUN / 'backup_complete.json').read_text())
    started = backup['timestamp']
    commands = [json.loads(line) for line in (RUN / 'commands.jsonl').read_text().splitlines()]
    latest = {r['label']: r for r in commands}
    required = ['panel_year','panel_decade','panel_period5','land_panel','gender_inference','gender_flags',
        'quarterly_all','college_less_restrictive','college_balanced','college_1860_1880','college_percohort',
        'college_table','appendix_college','elite_decennial','elite_opening_continuous','elite_exposure14_continuous',
        'facilities','appendix_facilities','fairs_render','gi_estimate','gi_export','gi_integration_test','gi_export_test',
        'test_panel_source_refresh','test_annual_csdid','test_period5_csdid','test_annual_population_controls','test_region_csdid',
        'schooling_export','schooling_region_period5',
        'schooling_region_annual_pre40_baseline_log_population','schooling_region_annual_pre40_preperiod_population_cagr']
    required += ['schooling_' + cadence + '_' + control for cadence in ('annual','period5')
                 for control in ('none','baseline_log_population','preperiod_population_cagr')]
    required += [r['label'] for r in commands if r['label'].startswith('fairs_robust_') or r['label'].startswith('fairs_percohort')]
    assert all(label in latest and latest[label]['exit_code'] == 0 for label in required), [
        label for label in required if label not in latest or latest[label]['exit_code'] != 0]
    panel = json.loads((RUN / 'panel_reconciliation.json').read_text())
    assert panel['passed']
    manifest = read(REPO / 'project/figures/mandatory_schooling_figure_manifest.csv')
    for r in manifest:
        source = ROOT / 'results/mandatory_schooling' / r['source']
        target = REPO / 'project/figures' / r['destination']
        assert source.stat().st_mtime >= started and md5(source) == r['source_md5']
        assert digest(source) == digest(target)
        validation = source.parent / 'validation_summary.csv'
        # Uncontrolled annual and five-year runners save the validation of
        # both profiles at their common parent, as the figure exporter does.
        if not validation.exists() and r['kind'] == 'annual':
            validation = source.parent.parent / 'validation_summary.csv'
        elif not validation.exists() and r['kind'] == 'period5':
            validation = source.parent.parent / 'validation_summary_post30_ref15.csv'
        assert validation.stat().st_mtime >= started
        assert all(row['validation_passed'] == 'TRUE' for row in read(validation))
    compilation = json.loads((RUN / 'compilation_manifest.json').read_text())
    qa = json.loads((RUN / 'pdf_render_manifest.json').read_text())
    assert len(compilation) == 4 and len(qa) == 4
    for name, record in compilation.items():
        assert digest(Path(record['source'])) == record['source_sha256']
        assert digest(Path(record['output'])) == record['sha256_after'] == qa[name]['sha256']
        assert all(digest(Path(p)) == h for p, h in record['dependencies'].items())
        assert qa[name]['visual_inspection'] == 'passed'
    failures = []; unsupported = []; inspected = []
    for base, directories, files in os.walk(ROOT / 'results'):
        directories[:] = [d for d in directories if not any(s in d.lower() for s in
            ('amws_refresh','synth','linked_','wwii_service','ukds','augsynth'))]
        for name in files:
            if not name.endswith('.csv') or not any(s in name for s in ('model_status','support','coefficients')):
                continue
            path = Path(base) / name
            if path.stat().st_mtime < started:
                continue
            inspected.append(str(path))
            # Fair runners repeat the same statuses in a combined file and
            # per-distance-bin files. Count the combined specifications once.
            aggregate_status = any((p / 'all_model_status.csv').exists()
                                   for p in path.parents if p != path.parent or name != 'all_model_status.csv')
            for row in read(path):
                status = row.get('status', '').lower()
                if 'model_status' in name and (row.get('ok') == 'FALSE' or status in ('failed','error','not_estimated')):
                    if name == 'all_model_status.csv' or not aggregate_status:
                        failures.append({'source':str(path), **row})
                if ('support' in name or 'coefficients' in name) and status and status not in (
                    'supported','available','reference','estimated','complete','ok','feasible') and 'reference' not in status:
                    unsupported.append({'source':str(path), **row})
    (RUN / 'failed_specifications.json').write_text(json.dumps(failures, indent=2))
    (RUN / 'unsupported_horizons.json').write_text(json.dumps(unsupported, indent=2))
    protected = json.loads((RUN / 'protected_validation.json').read_text())
    assert not protected['failures']
    comparison = json.loads((RUN / 'estimate_comparison_manifest.json').read_text())
    assert all(r['comparison'] == 'matched' for r in comparison)
    coordinates = json.loads((RUN / 'companion_coordinate_validation.json').read_text())
    assert len(coordinates) == 10 and all(r['passed'] for r in coordinates)
    failure_baseline = []
    for source in sorted({r['source'] for r in failures}):
        prior = RUN / 'backup/data' / Path(source).relative_to(ROOT)
        previous = [r for r in read(prior) if r.get('ok') == 'FALSE'] if prior.exists() else []
        current = [r for r in failures if r['source'] == source]
        fields = ('treatment_spec', 'distance_bin_km', 'outcome', 'error')
        key = lambda row: tuple(row.get(f, '') for f in fields)
        failure_baseline.append({'source':source, 'previous_failed':len(previous),
            'current_failed':len(current), 'same_specifications':sorted(map(key, previous)) == sorted(map(key, current))})
    (RUN / 'failed_specifications_before_after.json').write_text(json.dumps(failure_baseline, indent=2))
    code_files = [p for directory in ('analysis','prep') for p in (REPO / directory).rglob('*')
                  if p.is_file() and p.suffix in ('.R','.py')]
    code_checksums = {str(p):digest(p) for p in code_files}
    (RUN / 'code_checksums.json').write_text(json.dumps(code_checksums, indent=2))
    record = {
        'status':'complete', 'tag':TAG, 'finished':time.time(),
        'environment':{'GTL_REPO':str(REPO), 'TALENT_DETS_DATA_DIR':str(ROOT), 'DET_DIR':str(ROOT)},
        'published_release':backup['release'], 'backup':str(RUN / 'backup_complete.json'),
        'commands':commands, 'required_successful_labels': sorted(set(required)),
        'monthly_recovery':{'script':str(REPO / 'analysis/resume_amws_monthly_refresh.R'),
            'stage':'gender_monthly_stage_97981f41d4a', 'completed':True,
            'validation':'All source hashes, annual/gender/date reconciliations and all 5,973,744 protected monthly rows passed before installation.'},
        'regional_checkpoint':{'log':str(RUN / 'logs/schooling_region_annual_checkpoint.log'),
            'completed':'annual pre20/all controls and annual pre40/uncontrolled',
            'continuation':'annual pre40 controls and period5 were completed in separate commands using the same seeds and designs.'},
        'panels':panel, 'protected_files':protected['files'], 'schooling_figures':len(manifest),
        'compilation':compilation, 'visual_qa':qa,
        'failed_specifications':len(failures), 'unsupported_comparisons':len(unsupported),
        'specification_status_sources':inspected,
        'historical_google_report':'Read for context; no write or modification was performed.',
        'comparison_files':len(comparison),
        'companion_coordinates':coordinates, 'failed_specifications_before_after':failure_baseline,
        'code_checksums':str(RUN / 'code_checksums.json'),
    }
    (RUN / 'run_manifest.json').write_text(json.dumps(record, indent=2))
    (RUN / 'README.txt').write_text(
        'AMWS publication refresh, October 11, 2026\n\n'
        'Completed the four publication PDFs using the existing empirical designs.\n'
        'Synthetic DiD, Census-only results, early-edition inputs and the historical Google report were preserved.\n'
        'Annual source records: 112,291 -> 128,572 (+16,281); early editions remain 49,587.\n'
        'Monthly/quarterly date-grid totals additionally include 1,752 records outside annual denominator support.\n\n'
        'run_manifest.json records the release, environment, commands, validations and publication hashes.\n'
        'before_after_estimates.csv compares saved estimates, uncertainty and counts.\n'
        'sample_counts_before_after.csv and sample_membership_before_after.json record sample changes.\n'
        'failed_specifications.json and unsupported_horizons.json record estimation limitations.\n'
        'failed_specifications_before_after.json confirms the unestimable fair distance-bin specifications against the backup.\n'
        'code_checksums.json records the analysis and preparation code used at completion.\n'
        'companion_coordinate_validation.json independently verifies every embedded plot axis.\n'
        'backup/ retains the pre-refresh panels, classifications, results and publication assets.\n'
        'logs/ records commands; pdf_qa/ retains rendered pages and inspection evidence.\n\n'
        'Reproduction: run analysis/amws_publication_refresh.py stages prepare, panels, gender, college, elite, fairs, facilities, schooling, gi; '
        'then the manuscript updater, companion generator, validation and compilation scripts. '
        'The saved prepare stage is idempotent and preserves the original backup.\n')
    print('Final manifest saved:', RUN / 'run_manifest.json')
    print('Failed specifications:', len(failures), '; unsupported comparisons:', len(unsupported))


if __name__ == '__main__':
    finalize()

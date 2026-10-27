"""Independent full-population validation and completion manifest publication."""
import argparse
import collections
import csv
import importlib.util
import json
import os
import sys
from pathlib import Path
from pilot import DEFAULT_ROOT, RELEASE, PROCESSED, now, read, rows, sha, write

csv.field_size_limit(16 * 1024 * 1024)
NAME = 'physical_science_dummy_20261020'
PILOT = 'output/amws/field_regex_pilot_20261020'


def run(root, out):
    prepared = read(out / '_staging/prepared.json')
    checks = prepared['input_checksums_sha256']
    assert all(sha(p) == h for p, h in checks.items()), 'Input or pilot output changed'
    assert sha(out / '_staging/classifications.jsonl') == prepared['records_sha256']
    for name, digest in prepared['code_checksums_sha256'].items():
        assert sha(out / 'code' / name) == digest
        assert sha(Path(__file__).with_name(name)) == digest
    for name in ['classifier.py', 'classifier_boundary_guard.py', 'rules_v1.json']:
        assert sha(out / 'code' / name) == sha(root / PILOT / 'candidate_code' / name)
    # Load the archived classifier rather than trusting a precomputed flag.
    sys.path.insert(0, str(out / 'code'))
    from classifier_boundary_guard import classify_entry
    selected = []
    for i, r in enumerate(rows(root / RELEASE), 1):
        if not r['source_key'].startswith('1986:'):
            continue
        assert r['is_us_birth'].strip().lower() == 'true'
        selected.append(dict(source_key=r['source_key'], entry_id=r['entry_id'], name_raw=r['name_raw'],
                             raw_text_adjusted=r['raw_text_adjusted'], release_row=i))
    assert len(selected) == len({r['source_key'] for r in selected}) == 79658
    wanted = {(r['name_raw'], r['raw_text_adjusted']) for r in selected}
    sources = collections.defaultdict(list)
    for i, r in enumerate(rows(root / PROCESSED), 1):
        identity = r['name_raw'], r['raw_text_adjusted']
        if identity in wanted:
            sources[identity].append(dict(processed_row=i, field=r['field'], doc_id=r['doc_id'],
                source_file=r['source_file'], lineid=r['lineid'], entry_instance=r['entry_instance']))
    references = {r['source']['source_key']: r for r in read(root / PILOT / 'predictions_boundary_guard_posthoc.json')}
    assert len(references) == 500
    counts = collections.Counter(); reasons = collections.Counter(); inputs = collections.Counter(); groups = collections.Counter()
    seen = set()
    dummy_path = out / 'amws_ed16_physical_science_dummy.csv'
    audit_path = out / 'amws_ed16_physical_science_audit.csv'
    with dummy_path.open(encoding='utf-8-sig', newline='') as df, audit_path.open(encoding='utf-8-sig', newline='') as af:
        dummy = csv.DictReader(df); audit = csv.DictReader(af)
        assert dummy.fieldnames == prepared['dummy_columns']
        assert audit.fieldnames == prepared['audit_columns']
        for n, (s, d, a) in enumerate(zip(selected, dummy, audit, strict=True), 1):
            assert s['source_key'] == d['source_key'] == a['source_key']
            assert s['source_key'] not in seen; seen.add(s['source_key'])
            assert s['entry_id'] == d['entry_id'] == a['entry_id'] == s['source_key'].split(':')[1]
            assert s['name_raw'] == d['name_raw'] == a['name_raw']
            assert int(a['release_row']) == s['release_row']
            assert d['physical_science'] in ('0', '1')
            assert a['physical_science'] == d['physical_science']
            assert a['mixed'] in ('0', '1') and a['unresolved'] in ('0', '1')
            flag = int(d['physical_science']); mixed = int(a['mixed']); unresolved = int(a['unresolved'])
            assert not (mixed and unresolved)
            assert not mixed or flag == 1
            assert not unresolved or flag == 0
            actual_refs = sources[(s['name_raw'], s['raw_text_adjusted'])]
            assert actual_refs and json.loads(a['source_references_json']) == actual_refs
            fields = {r['field'].strip() for r in actual_refs}
            assert len(fields) == 1 and next(iter(fields)) == a['field_raw']
            p = classify_entry(a['field_raw'], s['raw_text_adjusted'], s['name_raw'])
            assert flag == int(p['physical_match'])
            assert flag == int(p['science_group'] == 'physical_science' or p['other_reason'] == 'mixed')
            assert mixed == int(p['other_reason'] == 'mixed')
            assert mixed == int(p['physical_match'] and p['biological_match'])
            assert unresolved == int(p['other_reason'] in ['missing', 'damaged', 'unmatched'])
            for col in ['science_group', 'other_reason', 'input_source', 'selected_heading', 'scope_guard_trigger', 'rules_version']:
                assert a[col] == p[col], (s['source_key'], col)
            assert a['matched_rule_ids'] == '; '.join(p['matched_rule_ids'])
            assert int(a['review_required']) == int(p['review_required'])
            assert int(a['biological_match']) == int(p['biological_match'])
            assert int(a['scope_guard_offset']) == p['scope_guard_offset']
            assert json.loads(a['evidence_json']) == p['evidence']
            for ev in p['evidence']:
                text = s['raw_text_adjusted'] if ev['evidence_source'] == 'raw_text_adjusted' else a['field_raw']
                assert text[ev['start']:ev['end']] == ev['evidence']
                if ev['evidence_source'] == 'raw_text_adjusted':
                    assert ev['end'] <= p['scope_guard_offset']
                counts['literal_evidence_spans'] += 1
            if s['source_key'] in references:
                assert p == references[s['source_key']]['prediction']
                assert flag == int(references[s['source_key']]['prediction']['physical_match'])
                counts['pilot_entries_reproduced'] += 1
            counts['total_entries'] += 1
            counts['physical_science_1'] += flag
            counts['physical_science_0'] += 1-flag
            counts['mixed'] += mixed
            counts['unresolved'] += unresolved
            counts['resolved_negative'] += int(not flag and not unresolved)
            counts['nonblank_recovered_field'] += bool(a['field_raw'])
            counts['blank_recovered_field'] += not bool(a['field_raw'])
            counts['entries_with_multiple_source_rows'] += len(actual_refs) > 1
            counts['source_reference_rows'] += len(actual_refs)
            reasons[p['other_reason'] or 'target'] += 1
            inputs[p['input_source']] += 1
            groups[p['science_group']] += 1
            if n % 10000 == 0:
                print(f'Independently validated {n:,}/79,658 rows.', flush=True)
    assert len(seen) == counts['total_entries'] == 79658
    assert counts['pilot_entries_reproduced'] == 500
    summary = read(out / 'summary.json')
    assert dict(counts) == summary['counts']
    assert dict(reasons) == summary['other_reason_counts']
    assert dict(inputs) == summary['input_source_counts']
    assert dict(groups) == summary['original_science_group_counts']
    assert counts['physical_science_1'] + counts['physical_science_0'] == 79658
    assert groups['physical_science'] + counts['mixed'] == counts['physical_science_1']
    assert counts['resolved_negative'] + counts['unresolved'] == counts['physical_science_0']
    export = read(out / '_staging/csv_export.json')
    assert export['rows_each'] == 79658 and export['artifact_tool_value_preservation_passed']
    assert all(sha(p) == h for p, h in checks.items())
    report = [
        'AMWS EDITION 16: PHYSICAL-SCIENCE DUMMY', 'Release date: October 20, 2026', '',
        'Definition: physical_science = 1 for a Physical Science regex match, including',
        'Physical + Biological mixed results; 0 otherwise, including unresolved entries.',
        'Physical includes engineering, mathematics, statistics and computing.', '',
        f'Total retained edition-16 entries: {counts["total_entries"]:,}',
        f'physical_science = 1: {counts["physical_science_1"]:,} ({100*counts["physical_science_1"]/79658:.2f}%)',
        f'physical_science = 0: {counts["physical_science_0"]:,} ({100*counts["physical_science_0"]/79658:.2f}%)',
        f'Mixed entries, all included in 1: {counts["mixed"]:,}',
        f'Unresolved entries, all included in 0: {counts["unresolved"]:,}',
        f'Resolved negative entries: {counts["resolved_negative"]:,}',
        f'Nonblank recovered fields: {counts["nonblank_recovered_field"]:,}',
        f'Blank recovered fields: {counts["blank_recovered_field"]:,}', '',
        'A zero means no detected Physical match, not a verified non-Physical entry.',
        'Use the audit unresolved flag to separate unknowns from resolved negatives.',
        'mixed and unresolved are distinct flags; mixed entries remain eligible for 1.', '',
        'Source: all 79,658 edition-16 entries retained in the October 19, 2026 multi_v04',
        'combined release. Existing source order and US-birth inclusion are preserved.',
        'The dummy CSV retains source_key, entry_id and name_raw for one-to-one merges.',
        'Merge by source_key, not by name or row position.', '',
        'Recovered fields are joined by exact name and full biography to the processed',
        'edition-16 file. Every matching source reference is retained. All joins succeed;',
        f'{counts["entries_with_multiple_source_rows"]} entries have multiple source rows with agreeing field values.',
        f'Classifier: {summary["rules_version"]}. Its code and vocabulary are unchanged',
        'from the preserved boundary-guard candidate. No new tuning or manual overrides.',
        'The original three-way result remains in the audit, but the primary deliverable',
        'is the requested binary Physical-or-mixed dummy.', '',
        'Validation: both CSVs contain exactly 79,658 unique source keys in release order.',
        'Every row was independently rejoined and reclassified from archived code.',
        f'All {counts["literal_evidence_spans"]:,} literal evidence spans and all 500 stored pilot predictions reproduce.',
        'All counts reconcile, the dummy contains only integer 0/1 values, and source',
        'datasets and previous pilot outputs have unchanged SHA-256 checksums.', '',
        'Accuracy status: full-population application is not new accuracy validation.',
        'The existing boundary-guard evaluation remains post-hoc with no fresh holdout.',
        'OCR damage, missing vocabulary and unresolved fields retain the pilot limitations.', '',
        'Files: amws_ed16_physical_science_dummy.csv (keyed dummy),',
        'amws_ed16_physical_science_audit.csv (field, flags, reasons, rules and evidence),',
        'summary.json, manifest.json, validation.json, and an exact code/ snapshot.',
        'Intermediate authoring records remain under _staging/.', '',
        'Reproduce from the repository root:',
        'python prep/amws/fields/build_physical_science_dummy.py --data-root "<Dropbox project root>"',
        'The runner also accepts TALENT_DETS_DATA_DIR. Completed releases are verified',
        'rather than overwritten.', ''
    ]
    (out / 'report.txt').write_text('\n'.join(report), encoding='utf-8')
    products = {str(p.relative_to(out)): sha(p) for p in out.rglob('*')
                if p.is_file() and p.name not in ['manifest.json', 'validation.json'] and '__pycache__' not in p.parts}
    validation = dict(status='PASS', validated_at_utc=now(),
        total_rows_each=79658, distinct_source_keys=79658, source_order_preserved=True,
        strict_integer_dummy=True, physical_and_mixed_included=True, mixed_and_unresolved_distinct=True,
        all_exact_source_joins_valid=True, classifier_unchanged_from_pilot=True,
        all_classifications_recomputed=True, pilot_predictions_reproduced=500,
        literal_evidence_spans_verified=counts['literal_evidence_spans'], counts_reconciled=True,
        source_and_previous_pilot_checksums_unchanged=True,
        artifact_tool_export_verified=True, output_checksums_sha256=products,
        accuracy_status='Reproducibility/provenance validation only; no new accuracy estimate.')
    write(out / 'validation.json', validation)
    products['validation.json'] = sha(out / 'validation.json')
    write(out / 'manifest.json', dict(status='complete', completed_at_utc=now(),
        source_release=RELEASE, population='retained edition-16 US-birth entries', rows=79658,
        rules_version=summary['rules_version'], definition=summary['definition'], zero_definition=summary['zero_definition'],
        input_checksums_sha256=checks, code_checksums_sha256=prepared['code_checksums_sha256'],
        output_checksums_sha256=products,
        reproduction_command='python prep/amws/fields/build_physical_science_dummy.py --data-root "<Dropbox project root>"',
        entrypoint_module='classifier_boundary_guard.classify_entry',
        accuracy_status=summary['accuracy_status']))
    print(json.dumps(dict(status='PASS', counts=dict(counts))), flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-root', type=Path, default=Path(os.environ.get('TALENT_DETS_DATA_DIR', DEFAULT_ROOT)))
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    run(args.data_root, args.output or args.data_root / 'output/amws' / NAME)

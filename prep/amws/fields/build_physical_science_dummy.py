"""Publish a binary Physical-or-mixed flag for retained edition-16 entries.

The existing boundary-guarded classifier is imported unchanged. Default `all`
prepares records, authors the two CSVs, and independently validates the release.
"""
import argparse
import collections
import csv
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
from classifier_boundary_guard import classify_entry, VERSION
from pilot import DEFAULT_ROOT, RELEASE, PROCESSED, now, read, rows, sha, write

csv.field_size_limit(16 * 1024 * 1024)
NAME = 'physical_science_dummy_20261020'
PILOT = 'output/amws/field_regex_pilot_20261020'
DUMMY_COLUMNS = ['source_key', 'entry_id', 'name_raw', 'physical_science']
AUDIT_COLUMNS = ['source_key', 'entry_id', 'name_raw', 'release_row', 'field_raw',
    'physical_science', 'mixed', 'unresolved', 'science_group', 'other_reason',
    'review_required', 'input_source', 'selected_heading', 'biological_match',
    'matched_rule_ids', 'evidence_json', 'scope_guard_offset', 'scope_guard_trigger',
    'source_references_json', 'rules_version']
CLASSIFIER_FILES = ['classifier.py', 'classifier_boundary_guard.py', 'rules_v1.json']
CODE_FILES = CLASSIFIER_FILES + ['pilot.py', 'build_physical_science_dummy.py',
    'export_physical_science_dummy.mjs', 'validate_physical_science_dummy.py', 'PHYSICAL_DUMMY.md']


def source_paths(root):
    return [root / RELEASE, root / PROCESSED,
            root / PILOT / 'predictions_boundary_guard_posthoc.json',
            root / PILOT / 'validation.json', root / PILOT / 'evaluation.json',
            root / PILOT / 'evaluation_boundary_guard_posthoc.json']


def verify_completed(out):
    manifest = read(out / 'manifest.json')
    assert manifest['status'] == 'complete'
    assert all(sha(p) == h for p, h in manifest['input_checksums_sha256'].items())
    assert all(sha(out / p) == h for p, h in manifest['output_checksums_sha256'].items())
    print('Completed release already exists; checksums verified. No files replaced.', flush=True)


def prepare(root, out):
    if (out / 'manifest.json').exists():
        verify_completed(out)
        return
    staging = out / '_staging'
    before = {str(p): sha(p) for p in source_paths(root)}
    code = {name: sha(Path(__file__).with_name(name)) for name in CODE_FILES}
    for name in CLASSIFIER_FILES:
        assert code[name] == sha(root / PILOT / 'candidate_code' / name), f'Pilot classifier changed: {name}'
    if (staging / 'prepared.json').exists():
        prepared = read(staging / 'prepared.json')
        assert prepared['input_checksums_sha256'] == before
        assert prepared['code_checksums_sha256'] == code
        assert sha(staging / 'classifications.jsonl') == prepared['records_sha256']
        print('Prepared records already exist; hashes match.', flush=True)
        return
    selected = []
    for i, r in enumerate(rows(root / RELEASE), 1):
        if r['source_key'].startswith('1986:'):
            assert r['is_us_birth'].strip().lower() == 'true'
            assert r['entry_id'] == r['source_key'].split(':', 1)[1]
            selected.append(dict(source_key=r['source_key'], entry_id=r['entry_id'],
                name_raw=r['name_raw'], raw_text_adjusted=r['raw_text_adjusted'], release_row=i))
    assert len(selected) == len({r['source_key'] for r in selected}) == 79658
    wanted = {(r['name_raw'], r['raw_text_adjusted']) for r in selected}
    lookup = collections.defaultdict(list)
    for i, r in enumerate(rows(root / PROCESSED), 1):
        identity = r['name_raw'], r['raw_text_adjusted']
        if identity in wanted:
            lookup[identity].append(dict(processed_row=i, field=r['field'], doc_id=r['doc_id'],
                source_file=r['source_file'], lineid=r['lineid'], entry_instance=r['entry_instance']))
    print('Selected 79,658 entries; exact name-and-biography source lookup loaded.', flush=True)
    reference = read(root / PILOT / 'predictions_boundary_guard_posthoc.json')
    pilot = {r['source']['source_key']: r for r in reference}
    assert len(pilot) == 500
    counts = collections.Counter()
    reasons = collections.Counter()
    sources = collections.Counter()
    groups = collections.Counter()
    mismatches = []
    staging.mkdir(parents=True, exist_ok=True)
    temp = staging / 'classifications.jsonl.part'
    with temp.open('w', encoding='utf-8', newline='\n') as stream:
        for n, r in enumerate(selected, 1):
            refs = lookup.get((r['name_raw'], r['raw_text_adjusted']), [])
            assert refs, f'No exact source join: {r["source_key"]}'
            fields = {ref['field'].strip() for ref in refs}
            assert len(fields) == 1, f'Conflicting fields: {r["source_key"]}'
            field = next(iter(fields))
            p = classify_entry(field, r['raw_text_adjusted'], r['name_raw'])
            flag = int(p['physical_match'])
            mixed = int(p['other_reason'] == 'mixed')
            unresolved = int(p['other_reason'] in ('missing', 'damaged', 'unmatched'))
            assert (flag == 1) == (p['science_group'] == 'physical_science' or bool(mixed))
            assert not (mixed and unresolved)
            assert not mixed or flag == 1
            assert not unresolved or flag == 0
            for ev in p['evidence']:
                text = field if ev['evidence_source'] == 'field_raw' else r['raw_text_adjusted']
                assert text[ev['start']:ev['end']] == ev['evidence']
                if ev['evidence_source'] == 'raw_text_adjusted':
                    assert ev['end'] <= p['scope_guard_offset']
                counts['literal_evidence_spans'] += 1
            if r['source_key'] in pilot:
                previous = pilot[r['source_key']]
                assert field == previous['source']['field_raw']
                if p != previous['prediction']:
                    mismatches.append(r['source_key'])
                counts['pilot_entries_reproduced'] += 1
            dummy = dict(source_key=r['source_key'], entry_id=r['entry_id'], name_raw=r['name_raw'], physical_science=flag)
            audit = dict(source_key=r['source_key'], entry_id=r['entry_id'], name_raw=r['name_raw'],
                release_row=r['release_row'], field_raw=field, physical_science=flag, mixed=mixed,
                unresolved=unresolved, science_group=p['science_group'], other_reason=p['other_reason'],
                review_required=int(p['review_required']), input_source=p['input_source'],
                selected_heading=p['selected_heading'], biological_match=int(p['biological_match']),
                matched_rule_ids='; '.join(p['matched_rule_ids']),
                evidence_json=json.dumps(p['evidence'], ensure_ascii=False),
                scope_guard_offset=p['scope_guard_offset'], scope_guard_trigger=p['scope_guard_trigger'],
                source_references_json=json.dumps(refs, ensure_ascii=False), rules_version=p['rules_version'])
            assert list(dummy) == DUMMY_COLUMNS and list(audit) == AUDIT_COLUMNS
            stream.write(json.dumps(dict(dummy=list(dummy.values()), audit=list(audit.values())), ensure_ascii=False) + '\n')
            counts['total_entries'] += 1
            counts['physical_science_1'] += flag
            counts['physical_science_0'] += 1-flag
            counts['mixed'] += mixed
            counts['unresolved'] += unresolved
            counts['resolved_negative'] += int(not flag and not unresolved)
            counts['nonblank_recovered_field'] += bool(field)
            counts['blank_recovered_field'] += not bool(field)
            counts['entries_with_multiple_source_rows'] += len(refs) > 1
            counts['source_reference_rows'] += len(refs)
            groups[p['science_group']] += 1
            reasons[p['other_reason'] or 'target'] += 1
            sources[p['input_source']] += 1
            if n % 10000 == 0:
                print(f'Classified {n:,}/79,658 entries.', flush=True)
    assert counts['pilot_entries_reproduced'] == 500 and not mismatches
    assert all(sha(p) == h for p, h in before.items())
    assert code == {name: sha(Path(__file__).with_name(name)) for name in CODE_FILES}
    os.replace(temp, staging / 'classifications.jsonl')
    summary = dict(created_at_utc=now(), rules_version=VERSION,
        definition='physical_science = int(physical_match); includes Physical-only and Physical+Biological mixed entries',
        zero_definition='No detected Physical match, including unresolved entries; not a verified non-Physical label.',
        population='79,658 retained edition-16 US-birth entries in multi_v04 combined release',
        counts=dict(counts), other_reason_counts=dict(reasons), input_source_counts=dict(sources),
        original_science_group_counts=dict(groups),
        accuracy_status='Full-population application only; existing classifier evaluation remains posthoc_no_fresh_holdout.')
    write(out / 'summary.json', summary)
    snapshot = out / 'code'; snapshot.mkdir(exist_ok=True)
    for name in CODE_FILES:
        shutil.copyfile(Path(__file__).with_name(name), snapshot / name)
    write(staging / 'prepared.json', dict(prepared_at_utc=now(), dummy_columns=DUMMY_COLUMNS,
        audit_columns=AUDIT_COLUMNS, input_checksums_sha256=before, code_checksums_sha256=code,
        records_sha256=sha(staging / 'classifications.jsonl'), total_entries=79658,
        rules_version=VERSION, classifier_unchanged_from_pilot=True))
    print(json.dumps(summary['counts']), flush=True)


def export(out):
    if (out / 'manifest.json').exists():
        verify_completed(out)
        return
    node = Path(os.environ.get('AMWS_NODE', Path.home() / '.cache/codex-runtimes/codex-primary-runtime/dependencies/node/bin/node.exe'))
    subprocess.run([str(node), '--expose-gc', str(Path(__file__).with_name('export_physical_science_dummy.mjs')),
                    '--output', str(out)], check=True)


def validate(root, out):
    subprocess.run([sys.executable, str(Path(__file__).with_name('validate_physical_science_dummy.py')),
                    '--data-root', str(root), '--output', str(out)], check=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', nargs='?', choices=['all', 'prepare', 'export', 'validate'], default='all')
    parser.add_argument('--data-root', type=Path, default=Path(os.environ.get('TALENT_DETS_DATA_DIR', DEFAULT_ROOT)))
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    target = args.output or args.data_root / 'output/amws' / NAME
    if args.command in ('all', 'prepare'):
        prepare(args.data_root, target)
    if args.command in ('all', 'export'):
        export(target)
    if args.command in ('all', 'validate'):
        validate(args.data_root, target)

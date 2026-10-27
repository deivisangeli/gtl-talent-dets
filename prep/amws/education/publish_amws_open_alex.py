"""Publish the canonical AMWS input, restricting only 1986 to US-born records.

The immutable full coordinate release is always the source. Citizenship is not
observed or inferred. Every retained cell and relative row order are verified.
"""
import argparse
import collections
import csv
import itertools
import json
import os
from pathlib import Path
import re
import unittest

import export_amws_openalex as export

VERSION = 'us_born_1986_v1'
SOURCE_RELEASE = 'amws_education_openalex_coordinates_20261014'
SOURCE_FILE = 'amws_all_editions_education_openalex.csv'
KEY = re.compile(r'^(1906|1938|1955|1986):[0-9]+$')
ID_COLUMN = re.compile(r'^(?:ba_bs|ma|ms|phd)_award_[0-9]+_institution_[0-9]+_oa_id$')
EXPECTED_BEFORE = {'1906': 3557, '1938': 19082, '1955': 34192, '1986': 99308}
EXPECTED_AFTER = dict(EXPECTED_BEFORE, **{'1986': 79658})


def require(condition, message):
    if not condition:
        raise ValueError(message)


def decision(row):
    match = KEY.fullmatch(row['source_key'])
    require(match is not None, 'Invalid edition-specific source_key: ' + row['source_key'])
    edition = match.group(1)
    if edition != '1986':
        return edition, True, 'earlier_edition_preserved'
    flag = str(row.get('is_us_birth') or '').strip().upper()
    if flag == 'TRUE':
        return edition, True, 'true'
    return edition, False, 'false' if flag == 'FALSE' else ('missing' if not flag else 'unrecognized')


def write_filtered(source, target):
    before = collections.Counter()
    after = collections.Counter()
    exclusions = collections.Counter()
    keys = set()
    with source.open(encoding='utf-8-sig', newline='') as f, target.open('w', encoding='utf-8-sig', newline='') as out:
        reader = csv.DictReader(f)
        fields = reader.fieldnames
        require(len(fields) == len(set(fields)), 'Duplicate source headers')
        require('source_key' in fields and 'is_us_birth' in fields, 'Required filter fields absent')
        writer = csv.DictWriter(out, fieldnames=fields)
        writer.writeheader()
        for row in reader:
            require(None not in row and None not in row.values(), 'Ragged source CSV row')
            key = row['source_key']
            require(key not in keys, 'Duplicate source key: ' + key)
            keys.add(key)
            edition, keep, reason = decision(row)
            before[edition] += 1
            if keep:
                writer.writerow(row)
                after[edition] += 1
            else:
                exclusions[reason] += 1
    return dict(columns=len(fields), before_by_edition=dict(before), after_by_edition=dict(after),
                exclusions_1986={k: exclusions[k] for k in ('false', 'missing', 'unrecognized')},
                before_rows=sum(before.values()), after_rows=sum(after.values()),
                removed_rows=sum(exclusions.values()))


def validate_filtered(source, target):
    keys = set()
    all_ids = set()
    by_edition = collections.defaultdict(collections.Counter)
    totals = collections.Counter()
    with source.open(encoding='utf-8-sig', newline='') as f, target.open(encoding='utf-8-sig', newline='') as out:
        original, actual = csv.DictReader(f), csv.DictReader(out)
        require(original.fieldnames == actual.fieldnames, 'Filtered columns differ')
        columns = len(actual.fieldnames)
        id_fields = [k for k in actual.fieldnames if ID_COLUMN.fullmatch(k)]
        retained = (row for row in original if decision(row)[1])
        for expected, row in itertools.zip_longest(retained, actual):
            require(expected is not None and row is not None, 'Filtered row count differs')
            require(row == expected, 'Retained fields or relative row order changed')
            edition, keep, _ = decision(row)
            require(keep, 'Non-US-born 1986 row retained')
            key = row['source_key']
            require(key not in keys, 'Duplicate retained key')
            keys.add(key)
            ids = json.loads(row['oa_ids'])
            require(len(ids) == len(set(ids)), 'Duplicate IDs in person summary')
            per_award = collections.Counter()
            slot_ids = set()
            for field in id_fields:
                if row[field]:
                    per_award[field.rsplit('_institution_', 1)[0]] += 1
                    slot_ids.add(row[field])
            require(slot_ids == set(ids), 'Award IDs and person ID union differ')
            require(len(ids) == int(row['oa_id_count']), 'Person ID count differs')
            require(len(per_award) == int(row['oa_matched_award_count']), 'Matched award summary differs')
            multiple = sum(n > 1 for n in per_award.values())
            require(multiple == int(row['oa_multi_institution_award_count']), 'Multiple-ID award summary differs')
            local = dict(entries=1, entries_with_id=bool(ids), earned_awards=int(row['oa_award_count']),
                         matched_awards=len(per_award), multi_id_awards=multiple,
                         institution_assignments=sum(per_award.values()))
            totals.update(local)
            by_edition[edition].update(local)
            all_ids.update(ids)
    return dict(passed=True, rows=len(keys), columns=columns,
                every_retained_cell_and_relative_row_order_verified=True,
                unique_source_keys=True, all_retained_1986_flags_true=True,
                coverage=dict(totals, entries_without_id=totals['entries'] - totals['entries_with_id'], distinct_ids=len(all_ids)),
                coverage_by_edition={k: dict(v) for k, v in sorted(by_edition.items())})


def run(data_root, local_root):
    root = Path(data_root)
    release = root / 'output/amws' / SOURCE_RELEASE
    source = release / SOURCE_FILE
    target = root / 'Data/processed/amws/amws_open_alex.csv'
    manifest_path = target.with_name('amws_open_alex_manifest.json')
    completion_path = release / 'completion_manifest.json'
    completion = export.read_json(completion_path)
    require(completion['status'] == 'complete', 'Full coordinate release is incomplete')
    protected = {release / name: checksum for name, checksum in completion['output_checksums'].items()}
    protected[completion_path] = export.digest(completion_path)
    for path, checksum in protected.items():
        require(export.digest(path) == checksum, 'Immutable source changed: ' + str(path))
    print('Verified immutable full coordinate release; filtering the canonical analysis input.', flush=True)
    target.parent.mkdir(parents=True, exist_ok=True)
    temporary = target.with_name(target.name + '.us_born_1986.tmp')
    counts = write_filtered(source, temporary)
    require(counts['before_by_edition'] == EXPECTED_BEFORE, 'Unexpected source edition counts')
    require(counts['after_by_edition'] == EXPECTED_AFTER, 'Unexpected retained edition counts')
    require(counts['before_rows'] == 156139 and counts['after_rows'] == 136489, 'Unexpected total rows')
    require(counts['columns'] == 444 and counts['removed_rows'] == 19650, 'Unexpected columns or exclusions')
    validation = validate_filtered(source, temporary)
    require(validation['rows'] == counts['after_rows'], 'Filtering and validation counts disagree')
    require(validation['columns'] == counts['columns'], 'Filtering and validation columns disagree')
    test_result = unittest.TextTestRunner(verbosity=1).run(unittest.defaultTestLoader.discover(
        str(Path(__file__).parent), pattern='test_publish_amws_open_alex.py'))
    require(test_result.wasSuccessful(), 'Publication tests failed')
    filtered_checksum = export.digest(temporary)
    for path, checksum in protected.items():
        require(export.digest(path) == checksum, 'Immutable source changed during filtering: ' + str(path))
    previous = export.read_json(manifest_path) if manifest_path.exists() else None
    current_checksum = export.digest(target) if target.exists() else None
    if current_checksum is not None and current_checksum != filtered_checksum:
        require(previous is not None and previous['sha256'] == current_checksum,
                'Canonical input differs from its manifest; refusing to overwrite untracked changes')
    local = Path(local_root) / 'amws_open_alex_us_born_20261014'
    local.mkdir(parents=True, exist_ok=True)
    if previous:
        previous_digest = export.digest(manifest_path)
        archived = local / ('publication_manifest_' + previous_digest + '.json')
        if not archived.exists():
            archived.write_bytes(manifest_path.read_bytes())
        require(export.digest(archived) == previous_digest, 'Previous manifest backup differs')
    if (current_checksum == filtered_checksum and previous
            and previous.get('filter_version') == VERSION and previous.get('source_sha256') == protected[source]):
        require(previous['rows'] == validation['rows'] and previous['sha256'] == filtered_checksum, 'Existing filtered manifest differs')
        temporary.unlink()
        print(export.dumps(dict(already_complete=True, path=str(target), rows=validation['rows'], coverage=validation['coverage'])), flush=True)
        return
    metadata = dict(
        dataset='amws_open_alex', format='csv', path=str(target), source_release=str(release), source_file=str(source),
        source_completion_manifest_sha256=protected[completion_path], checksum_algorithm='sha256',
        sha256=filtered_checksum, source_sha256=protected[source],
        published_at_utc=export.now(), rows=counts['after_rows'], columns=counts['columns'],
        source_rows=counts['before_rows'], size_bytes=temporary.stat().st_size,
        transformation='Restrict the 1986 edition to the existing US-birth classification; preserve earlier editions',
        filter_version=VERSION,
        filter_rule=dict(edition_field='source_key prefix', earlier_editions_keep_all=['1906', '1938', '1955'],
                         edition_1986_field='is_us_birth', required_value='TRUE', normalization='strip whitespace and uppercase',
                         false_missing_or_unrecognized='exclude', citizenship_recorded_or_inferred=False),
        filter_counts=counts, validation=validation,
        test_report=dict(tests_run=test_result.testsRun, failures=len(test_result.failures), errors=len(test_result.errors)),
        coordinate_kind='openalex_reference_location', historical_campus_accuracy_verified=False,
        assignment_qualification='Existing provisional and conditional assignments preserved',
        counting='Distinct source_key within edition; no cross-edition person deduplication',
        source_release_preserved=True, source_release_files_verified=len(protected),
        temporary_filtered_file_validated_before_replace=True, byte_identical=False,
        retained_rows_cell_identical=True, shared_path_variable='AMWS_OPEN_ALEX',
        shared_path_configuration='paths.R', publication_script=str(Path(__file__).resolve()),
        publication_script_sha256=export.digest(Path(__file__)),
    )
    temporary_manifest = manifest_path.with_name(manifest_path.name + '.us_born_1986.tmp')
    export.write_json(temporary_manifest, metadata)
    require(export.read_json(temporary_manifest) == metadata, 'Prepared manifest readback differs')
    print('All retained rows, coverage, and tests passed; replacing the canonical file.', flush=True)
    os.replace(temporary, target)
    require(export.digest(target) == filtered_checksum, 'Published filtered checksum differs')
    os.replace(temporary_manifest, manifest_path)
    require(export.read_json(manifest_path) == metadata, 'Published manifest differs')
    export.write_json(local / 'validation_report.json', validation)
    export.write_json(local / 'filter_counts.json', counts)
    for path, checksum in protected.items():
        require(export.digest(path) == checksum, 'Immutable source changed during publication: ' + str(path))
    print(export.dumps(dict(path=str(target), rows=validation['rows'], columns=validation['columns'],
                           removed_rows=counts['removed_rows'], exclusions=counts['exclusions_1986'],
                           coverage=validation['coverage'], coverage_by_edition=validation['coverage_by_edition'])), flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-root', default=os.environ.get('TALENT_DETS_DATA_DIR', str(Path.home() / 'Globtalent Dropbox/gtl_talent_dets')))
    parser.add_argument('--local-root', default=os.environ.get('AMWS_CHECKPOINT_ROOT', str(Path.home() / '.codex/amws_checkpoints')))
    args = parser.parse_args()
    run(args.data_root, args.local_root)

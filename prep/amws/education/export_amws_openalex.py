"""Export existing AMWS award assignments without changing identity decisions.

Streaming CSV and a local SQLite work database keep original string values intact.
No network access, parsing, identity inference, or geolocation is performed.
"""
import argparse
import collections
import csv
import datetime
import functools
import hashlib
import itertools
import json
import os
from pathlib import Path
import re
import shutil
import sqlite3
import time
import unittest

NAME = 'amws_education_openalex_20261012'
VERSION = 'award_export_v1'
FAMILIES = ('ba_bs', 'ma', 'ms', 'phd')
PERSON_FILES = (
    'amws_combined_us_geocoded_education.csv',
    'amws_1906_us_unmatched_final_education.csv',
    'amws_1938_us_unmatched_final_education.csv',
    'amws_1955_us_still_unmatched_education.csv',
    'amws_ed86_final_education.csv',
)
AWARD_SUFFIXES = ('award_index', 'raw_institution', 'degree_label', 'year', 'year_raw',
                  'assignment_class', 'conditional_ids', 'flags', 'matching_flags',
                  'condition_conflict', 'fully_matched', 'oa_id_count')
META_FIELDS = ('source_key', 'oa_export_version', 'oa_ids', 'oa_id_count',
               'oa_has_id', 'oa_award_count', 'oa_matched_award_count',
               'oa_multi_institution_award_count')
EXPECTED = dict(unique_entries=156139, original_rows=156607,
                entries_with_id=132555, matched_awards=273958,
                distinct_ids=1627, multi_id_awards=67, ledger_awards=330777)
csv.field_size_limit(16 * 1024 * 1024)


def now():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def dumps(value):
    return json.dumps(value, ensure_ascii=False, separators=(',', ':'))


def read_json(path):
    return json.loads(Path(path).read_text(encoding='utf-8-sig'))


def write_json(path, value):
    Path(path).write_text(json.dumps(value, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')


def digest(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as f:
        for block in iter(lambda: f.read(1048576), b''):
            h.update(block)
    return h.hexdigest()


def rows(path):
    with Path(path).open(encoding='utf-8-sig', newline='') as f:
        yield from csv.DictReader(f)


def integer(value):
    text = str(value).strip()
    if not re.fullmatch(r'\d+(?:\.0+)?', text):
        raise ValueError('Invalid integer identifier: ' + text)
    return int(text.split('.')[0])


def source_key(row, filename):
    if filename == 'amws_ed86_final_education.csv':
        return '1986:' + str(integer(row['entry_id']))
    edition = integer(row['edition']) if filename == PERSON_FILES[0] else int(filename.split('_')[1])
    if edition not in (1906, 1938, 1955):
        raise ValueError('Unsupported edition')
    return f'{edition}:{integer(row["lineid"])}'


def eligible_award(row):
    return (row['requested_family'] in FAMILIES and bool(row['family_award_index'])
            and row['honorary'].lower() == 'false')


def assigned_ids(ledger, snapshot):
    values = json.loads(ledger['IDs_after']) if ledger else []
    if len(values) != len(set(values)):
        raise ValueError('Duplicate assigned IDs in award ledger')
    for entity in values:
        if not re.fullmatch(r'I\d+', entity) or entity not in snapshot:
            raise ValueError('Unsupported snapshot ID: ' + entity)
    return sorted(values)


def award_values(award):
    prefix = f'{award["requested_family"]}_award_{award["family_award_index"]}_'
    values = {
        'award_index': award['award_index'], 'raw_institution': award['institution_as_written'],
        'degree_label': award['label_as_written'], 'year': award['year'],
        'year_raw': award['year_raw'], 'assignment_class': award['assignment_class'],
        'conditional_ids': award['conditional_IDs'], 'flags': award['flags'],
        'matching_flags': award['matching_flags'], 'condition_conflict': award['condition_conflict'],
        'fully_matched': award['fully_matched'], 'oa_id_count': str(len(award['oa_ids'])),
    }
    out = {prefix + k: v for k, v in values.items()}
    for i, (entity, name) in enumerate(zip(award['oa_ids'], award['oa_names'], strict=True), 1):
        out[prefix + f'institution_{i}_name'] = name
        out[prefix + f'institution_{i}_oa_id'] = entity
    return out


def differing_fields(original, preferred):
    # Absent columns and present-but-empty values are distinct in provenance.
    return sorted(k for k in original.keys() | preferred.keys()
                  if (k in original) != (k in preferred) or original.get(k) != preferred.get(k))


def person_output_name(filename):
    return filename[:-4] + '_openalex.csv'


def run(data_root, local_root):
    timer = time.perf_counter()
    root = Path(data_root) / 'output/amws'
    enrichment = root / 'education_enriched_20261011_updated_final'
    resolution = root / 'openalex_id_resolution_20261012'
    local = Path(local_root) / NAME
    stage = local / 'release'
    destination = root / NAME
    if destination.exists():
        marker = destination / 'completion_manifest.json'
        if not marker.exists():
            raise ValueError('Existing destination is incomplete; refusing to overwrite')
        done = read_json(marker)
        for rel, checksum in done['output_checksums'].items():
            if digest(destination / rel) != checksum:
                raise ValueError('Published output checksum differs: ' + rel)
        print(dumps(dict(already_complete=True, release=str(destination), coverage=done['coverage'])), flush=True)
        return
    local.mkdir(parents=True, exist_ok=True)
    stage.mkdir(exist_ok=True)
    snapshot_path = Path(local_root) / 'openalex_id_resolution_20261012/snapshot_records.json'
    inputs = [enrichment / f for f in PERSON_FILES] + [
        enrichment / 'education_awards.csv', enrichment / 'source_biographies.csv',
        enrichment / 'manifest.json', resolution / 'award_id_ledger.csv',
        resolution / 'person_id_coverage.csv', resolution / 'raw_entry_crosswalk.jsonl',
        resolution / 'coverage_report.json', resolution / 'completion_manifest.json', snapshot_path,
    ]
    hashes = {str(p): digest(p) for p in inputs}
    previous = read_json(resolution / 'completion_manifest.json')
    for filename in ('award_id_ledger.csv', 'person_id_coverage.csv', 'raw_entry_crosswalk.jsonl', 'coverage_report.json'):
        if hashes[str(resolution / filename)] != previous['output_checksums'][filename]:
            raise ValueError('Published matching input changed: ' + filename)
    expected_awards = previous['source_checksums'][str(enrichment / 'education_awards.csv').replace('/', '\\')]
    if hashes[str(enrichment / 'education_awards.csv')] != expected_awards:
        raise ValueError('Published extraction input changed')
    manifest_path = local / 'input_manifest.json'
    if manifest_path.exists() and read_json(manifest_path)['input_checksums'] != hashes:
        raise ValueError('Inputs differ from this export checkpoint')
    manifest = dict(version=VERSION, started_at_utc=now(), input_checksums=hashes,
                    combined_source_precedence=list(PERSON_FILES), institution_order='Lexicographic OA ID',
                    duplicate_policy='First row of highest-priority source file',
                    no_new_identity_assignments=True)
    write_json(manifest_path, manifest)
    snapshot = read_json(snapshot_path)
    print('Verified input checksums; preparing local award joins.', flush=True)

    con = sqlite3.connect(local / 'export.sqlite')
    con.executescript('''
        DROP TABLE IF EXISTS ledger; DROP TABLE IF EXISTS awards;
        DROP TABLE IF EXISTS persons; DROP TABLE IF EXISTS variants;
        CREATE TABLE ledger (source_key TEXT, award_index INTEGER, data TEXT,
                             PRIMARY KEY(source_key,award_index));
        CREATE TABLE awards (source_key TEXT, award_index INTEGER, family TEXT,
                             family_index INTEGER, data TEXT,
                             PRIMARY KEY(source_key,award_index), UNIQUE(source_key,family,family_index));
        CREATE TABLE persons (source_key TEXT PRIMARY KEY, edition INTEGER, entry_id INTEGER,
                              source_file TEXT, source_row INTEGER, original TEXT);
        CREATE TABLE variants (source_file TEXT, source_row INTEGER, source_key TEXT, original TEXT,
                               PRIMARY KEY(source_file,source_row));
    ''')
    ledger_count = 0
    for r in rows(resolution / 'award_id_ledger.csv'):
        con.execute('INSERT INTO ledger VALUES (?,?,?)', (r['source_key'], int(r['award_index']), dumps(r)))
        ledger_count += 1
    assert ledger_count == EXPECTED['ledger_awards']
    # Retain existing matching flags and exact provenance links, without interpreting decisions.
    raw_meta = {}
    with (resolution / 'raw_entry_crosswalk.jsonl').open(encoding='utf-8') as f:
        for line in f:
            r = json.loads(line)
            raw_meta[r['record_id']] = dict(origin=r['origin'], status=r['status'], flags=r.get('flags', []))

    with (enrichment / 'education_awards.csv').open(encoding='utf-8-sig', newline='') as f:
        reader = csv.DictReader(f)
        original_award_fields = reader.fieldnames
        max_awards = collections.Counter()
        max_institutions = collections.Counter()
        counts = collections.Counter()
        matched_ids = set()
        for r in reader:
            if not eligible_award(r):
                continue
            match = con.execute('SELECT data FROM ledger WHERE source_key=? AND award_index=?',
                                (r['source_key'], int(r['award_index']))).fetchone()
            ledger = json.loads(match[0]) if match else None
            if bool(r['institution_as_written']) != bool(ledger):
                raise ValueError('Award/ledger coverage mismatch: ' + r['source_key'])
            if ledger:
                for field in ('requested_family', 'institution_as_written', 'year', 'flags'):
                    assert r[field] == ledger[field], (r['source_key'], field)
            ids = assigned_ids(ledger, snapshot)
            conditional = json.loads(ledger['conditional_IDs']) if ledger else []
            if not set(conditional) <= set(ids):
                raise ValueError('Conditional IDs are not a subset of applied IDs')
            meta = raw_meta[ledger['record_id']] if ledger else {}
            a = dict(r, oa_ids=ids, oa_names=[snapshot[i]['canonical_name'] for i in ids],
                     record_id=ledger['record_id'] if ledger else '',
                     assignment_class=ledger['assignment_class'] if ledger else '[]',
                     conditional_IDs=ledger['conditional_IDs'] if ledger else '[]',
                     condition_conflict=ledger['condition_conflict'] if ledger else 'False',
                     fully_matched=ledger['fully_matched'] if ledger else 'False',
                     matched_components=ledger['matched_components'] if ledger else '[]',
                     component_count=ledger['component_count'] if ledger else '',
                     IDs_before=ledger['IDs_before'] if ledger else '[]',
                     IDs_after=ledger['IDs_after'] if ledger else '[]',
                     additional_IDs=ledger['additional_IDs'] if ledger else '[]',
                     matching_flags=dumps(meta.get('flags', [])),
                     mapping_origin=meta.get('origin', ''), raw_mapping_status=meta.get('status', ''),
                     education_source=str(enrichment / 'education_awards.csv'),
                     matching_source=str(resolution / 'award_id_ledger.csv'),
                     decision_source=str(resolution / 'raw_entry_crosswalk.jsonl'),
                     oa_export_version=VERSION)
            con.execute('INSERT INTO awards VALUES (?,?,?,?,?)',
                        (r['source_key'], int(r['award_index']), r['requested_family'], int(r['family_award_index']), dumps(a)))
            family = r['requested_family']
            max_awards[family] = max(max_awards[family], int(r['family_award_index']))
            max_institutions[family] = max(max_institutions[family], len(ids), 1)
            counts['earned_awards'] += 1
            counts['matched_awards'] += bool(ids)
            counts['multi_id_awards'] += len(ids) > 1
            counts['awards_with_missing_year'] += not bool(r['year'])
            counts['conditional_awards'] += bool(conditional)
            matched_ids.update(ids)
    con.commit()
    assert counts['matched_awards'] == EXPECTED['matched_awards']
    assert counts['multi_id_awards'] == EXPECTED['multi_id_awards']
    assert len(matched_ids) == EXPECTED['distinct_ids']
    assert con.execute('SELECT COUNT(*) FROM ledger l LEFT JOIN awards a USING(source_key,award_index) WHERE a.source_key IS NULL').fetchone()[0] == 0
    del snapshot, raw_meta
    wide_fields = list(META_FIELDS)
    for family in FAMILIES:
        for award_no in range(1, max_awards[family] + 1):
            prefix = f'{family}_award_{award_no}_'
            wide_fields.extend(prefix + suffix for suffix in AWARD_SUFFIXES)
            for institution_no in range(1, max_institutions[family] + 1):
                wide_fields.extend((prefix + f'institution_{institution_no}_name', prefix + f'institution_{institution_no}_oa_id'))

    @functools.lru_cache(maxsize=1024)
    def wide(key):
        result = dict(source_key=key, oa_export_version=VERSION)
        all_ids = set()
        n = matched = multiple = 0
        for (data,) in con.execute('SELECT data FROM awards WHERE source_key=? ORDER BY award_index', (key,)):
            a = json.loads(data)
            result.update(award_values(a))
            all_ids.update(a['oa_ids'])
            n += 1
            matched += bool(a['oa_ids'])
            multiple += len(a['oa_ids']) > 1
        result.update(oa_ids=dumps(sorted(all_ids)), oa_id_count=str(len(all_ids)),
                      oa_has_id=str(bool(all_ids)), oa_award_count=str(n),
                      oa_matched_award_count=str(matched), oa_multi_institution_award_count=str(multiple))
        return result

    original_columns = []
    file_reports = []
    for filename in PERSON_FILES:
        with (enrichment / filename).open(encoding='utf-8-sig', newline='') as f, \
                (stage / person_output_name(filename)).open('w', encoding='utf-8-sig', newline='') as out:
            reader = csv.DictReader(f)
            fields = reader.fieldnames
            if set(fields) & set(wide_fields):
                raise ValueError('Export columns collide with original fields')
            original_columns.extend(field for field in fields if field not in original_columns)
            writer = csv.DictWriter(out, fieldnames=fields + wide_fields)
            writer.writeheader()
            n = 0
            for n, r in enumerate(reader, 1):
                key = source_key(r, filename)
                edition, entry_id = map(int, key.split(':'))
                con.execute('INSERT INTO variants VALUES (?,?,?,?)', (filename, n, key, dumps(r)))
                con.execute('INSERT OR IGNORE INTO persons VALUES (?,?,?,?,?,?)',
                            (key, edition, entry_id, filename, n, dumps(r)))
                writer.writerow(dict(r, **wide(key)))
            file_reports.append(dict(source_file=filename, output_file=person_output_name(filename), rows=n))
        con.commit()
        print(f'Exported {filename}: {n:,} original rows.', flush=True)
    assert sum(r['rows'] for r in file_reports) == EXPECTED['original_rows']
    assert con.execute('SELECT COUNT(*) FROM persons').fetchone()[0] == EXPECTED['unique_entries']
    assert con.execute('SELECT COUNT(*) FROM awards a LEFT JOIN persons p USING(source_key) WHERE p.source_key IS NULL').fetchone()[0] == 0
    combined_name = 'amws_all_editions_education_openalex.csv'
    combined_fields = original_columns + wide_fields
    edition_coverage = collections.defaultdict(collections.Counter)
    with (stage / combined_name).open('w', encoding='utf-8-sig', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=combined_fields)
        writer.writeheader()
        for key, edition, entry_id, filename, row_no, original in con.execute('SELECT * FROM persons ORDER BY edition,entry_id'):
            r = json.loads(original)
            enriched = wide(key)
            # edition is an original field only when it existed in the selected source.
            writer.writerow(dict(r, **enriched))
            edition_coverage[str(edition)]['entries'] += 1
            edition_coverage[str(edition)]['entries_with_id'] += enriched['oa_has_id'] == 'True'
    provenance_fields = ('source_key', 'edition', 'source_file', 'source_row', 'selected_combined_row',
                         'selected_source_file', 'selected_source_row', 'differing_fields', 'original_row_json')
    variant_counts = collections.Counter()
    with (stage / 'original_row_provenance.csv').open('w', encoding='utf-8-sig', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=provenance_fields)
        writer.writeheader()
        for filename, row_no, key, original, preferred_file, preferred_row, preferred in con.execute('''
                SELECT v.*,p.source_file,p.source_row,p.original FROM variants v JOIN persons p USING(source_key)
                ORDER BY v.rowid'''):
            selected = filename == preferred_file and row_no == preferred_row
            diff = differing_fields(json.loads(original), json.loads(preferred))
            writer.writerow(dict(source_key=key, edition=key.split(':')[0], source_file=filename, source_row=row_no,
                                 selected_combined_row=str(selected), selected_source_file=preferred_file,
                                 selected_source_row=preferred_row, differing_fields=dumps(diff), original_row_json=original))
            variant_counts['original_rows'] += 1
            variant_counts['selected_rows'] += selected
            variant_counts['rows_with_differences'] += bool(diff)
    first_award = json.loads(con.execute('SELECT data FROM awards LIMIT 1').fetchone()[0])
    award_fields = list(first_award)
    with (stage / 'education_awards_openalex.csv').open('w', encoding='utf-8-sig', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=award_fields)
        writer.writeheader()
        for (data,) in con.execute('SELECT data FROM awards ORDER BY CAST(substr(source_key,1,4) AS INTEGER),CAST(substr(source_key,6) AS INTEGER),award_index'):
            a = json.loads(data)
            a['oa_ids'] = dumps(a['oa_ids'])
            a['oa_names'] = dumps(a['oa_names'])
            writer.writerow(a)
    print('Exported combined entries, provenance, and earned-degree award table; validating every row.', flush=True)

    validation = validate_outputs(stage, enrichment, resolution, con, wide, wide_fields, original_columns, original_award_fields)
    covered = sum(c['entries_with_id'] for c in edition_coverage.values())
    assert covered == EXPECTED['entries_with_id']
    coverage = dict(total_entries=EXPECTED['unique_entries'], original_rows=EXPECTED['original_rows'],
                    entries_with_id=covered, entries_without_id=EXPECTED['unique_entries'] - covered,
                    distinct_ids=len(matched_ids), **dict(counts), by_edition=dict(edition_coverage),
                    provenance=dict(variant_counts), person_files=file_reports,
                    maximum_awards_per_family=dict(max_awards), maximum_ids_per_award_by_family=dict(max_institutions),
                    combined_columns=len(combined_fields), new_columns=len(wide_fields),
                    counting='Distinct source_key within edition; no cross-edition person deduplication',
                    assignments='Existing published assignments only; provisional and conditional labels preserved')
    write_json(stage / 'coverage_report.json', coverage)
    write_json(stage / 'validation_report.json', validation)
    write_json(stage / 'input_manifest.json', manifest)
    dictionary = '''# AMWS education and OpenAlex export

The combined CSV contains one row per edition-specific source_key. It includes all
entries, including those without education or matched IDs. Source-specific copies
retain every original row and column in original order. IDs are existing published
assignments, including provisional matches; this export does not verify identities.

## Keys and original fields
source_key = edition:original entry identifier (lineid for 1906/1938/1955; entry_id
for 1986). The selected original row supplies original fields unchanged. Fields
absent from that source remain empty in the combined union schema. In particular,
edition can be blank in original columns; derive edition from source_key instead.
The provenance table stores every original row as lossless JSON, selected-row
indicators, preferred source references and differing fields. Source row numbers
are one-based data rows, excluding the header. Repeated records are not linked
across editions. The existing geocoded file takes precedence, then unmatched
1906, unmatched 1938, unmatched 1955, and the separate 1986 file. Within each file
the first row is selected. This is an export choice, not a geocoding reassessment.

## Award columns
{family}_award_{n}_... uses existing earned-degree family_award_index in biography
order. Families: ba_bs, ma, ms, phd. Honorary awards and nonqualifying degree labels
are excluded from new columns. Existing education lists remain unchanged.
award_index is the original extraction ledger index; gaps are possible.
raw_institution, degree_label, year and year_raw preserve extracted string values.
An empty year remains empty even when the literal year token is present.
institution_{k}_name and institution_{k}_oa_id contain a snapshot canonical name
and its assigned short OpenAlex ID. Distinct IDs are sorted lexicographically;
slot order implies neither chronology nor primary institution. Each award's full
raw string is retained; no extraction boundary corrections are performed.
Unmatched awards retain raw fields with blank institution name/ID slots.
assignment_class and conditional_ids are JSON lists copied from the award ledger.
flags contains original extraction flags; matching_flags copies raw mapping flags.
condition_conflict and fully_matched preserve existing ledger indicators. The latter
does not imply verified accuracy. oa_id_count counts IDs applied to that award.

## Person summaries
oa_ids is the JSON union of applied award IDs. oa_id_count counts distinct IDs;
oa_has_id is True or False. oa_award_count counts qualifying earned awards,
oa_matched_award_count counts awards with at least one ID, and
oa_multi_institution_award_count counts awards with more than one applied ID.
Blank award slots mean no such award; oa_id_count=0 within a populated award means
the award has no assigned ID. JSON [] denotes an empty list, not a missing field.

## Award-level table and provenance
education_awards_openalex.csv retains every qualifying earned award, including
awards without extracted institution text. Original evidence and offsets are
unchanged. oa_ids and oa_names are aligned JSON lists. IDs_after, IDs_before,
additional_IDs, conditional_IDs, assignment_class, condition_conflict,
fully_matched, matched_components and component_count preserve ledger values;
awards absent from that ledger have no institution and empty assignment lists.
record_id joins decision_source by record_id; education_source and matching_source
join on source_key/award_index. mapping_origin and raw_mapping_status retain prior
decision categories. These references point to immutable previous releases.
Institution identity does not establish historical teaching campus or location.
'''
    (stage / 'DATA_DICTIONARY.md').write_text(dictionary, encoding='utf-8')
    source_paths = [Path(__file__), Path(__file__).with_name('test_export_amws_openalex.py')]
    code_dir = stage / 'code_snapshot'
    code_dir.mkdir(exist_ok=True)
    for source in source_paths:
        shutil.copy2(source, code_dir / source.name)
    test_result = unittest.TextTestRunner(verbosity=1).run(unittest.defaultTestLoader.discover(str(Path(__file__).parent), pattern='test_export_amws_openalex.py'))
    if not test_result.wasSuccessful():
        raise ValueError('Export tests failed')
    write_json(stage / 'test_report.json', dict(tests_run=test_result.testsRun, failures=len(test_result.failures), errors=len(test_result.errors)))
    for p, checksum in hashes.items():
        if digest(p) != checksum:
            raise ValueError('Protected input changed during export: ' + p)
    write_json(stage / 'timing_report.json', dict(started_at_utc=manifest['started_at_utc'], validated_at_utc=now(),
                                               seconds_before_publication=time.perf_counter() - timer))
    outputs = {p.relative_to(stage).as_posix(): digest(p) for p in sorted(stage.rglob('*')) if p.is_file()}
    write_json(stage / 'checksums.json', dict(algorithm='sha256', input_checksums=hashes, output_checksums=outputs))
    outputs['checksums.json'] = digest(stage / 'checksums.json')
    print('All structural and coverage checks passed; copying validated release to Dropbox.', flush=True)
    destination.mkdir(exist_ok=False)
    for rel, checksum in outputs.items():
        target = destination / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        for attempt in range(5):
            try:
                shutil.copy2(stage / rel, target)
                if digest(target) != checksum:
                    raise ValueError('Copied output checksum differs: ' + rel)
                break
            except PermissionError:
                if attempt == 4:
                    raise
                time.sleep(2 ** attempt)
    completion = dict(status='complete', version=VERSION, published_at_utc=now(),
                      input_checksums=hashes, output_checksums=outputs, coverage=coverage,
                      validation_passed=True, copied_checksums_verified=True,
                      elapsed_seconds=time.perf_counter() - timer)
    write_json(destination / 'completion_manifest.json', completion)
    assert read_json(destination / 'completion_manifest.json') == completion
    con.close()
    print(dumps(dict(release=str(destination), coverage=coverage, elapsed_seconds=completion['elapsed_seconds'])), flush=True)


def validate_outputs(stage, enrichment, resolution, con, wide, wide_fields, original_columns, original_award_fields):
    checks = collections.Counter()
    # Round-trip every original column and every new field in all source-specific exports.
    for filename in PERSON_FILES:
        with (enrichment / filename).open(encoding='utf-8-sig', newline='') as a, \
                (stage / person_output_name(filename)).open(encoding='utf-8-sig', newline='') as b:
            original = csv.DictReader(a)
            exported = csv.DictReader(b)
            assert exported.fieldnames == original.fieldnames + wide_fields
            for r, e in itertools.zip_longest(original, exported):
                assert r is not None and e is not None
                assert all(e[k] == v for k, v in r.items())
                key = source_key(r, filename)
                assert all(e[k] == str(wide(key).get(k, '')) for k in wide_fields)
                checks['preserved_original_rows'] += 1
    assert checks['preserved_original_rows'] == EXPECTED['original_rows']
    keys = set()
    reconstructed = {}
    with (stage / 'amws_all_editions_education_openalex.csv').open(encoding='utf-8-sig', newline='') as f:
        reader = csv.DictReader(f)
        assert reader.fieldnames == original_columns + wide_fields
        previous_key = (0, 0)
        for r in reader:
            key = r['source_key']
            assert key not in keys
            keys.add(key)
            numeric = tuple(map(int, key.split(':')))
            assert numeric > previous_key
            previous_key = numeric
            preferred = json.loads(con.execute('SELECT original FROM persons WHERE source_key=?', (key,)).fetchone()[0])
            assert all(r[k] == preferred.get(k, '') for k in original_columns)
            person_ids = set()
            for (data,) in con.execute('SELECT data FROM awards WHERE source_key=? ORDER BY award_index', (key,)):
                a = json.loads(data)
                prefix = f'{a["requested_family"]}_award_{a["family_award_index"]}_'
                expected_values = award_values(a)
                assert all(r[k] == str(v) for k, v in expected_values.items())
                column_ids = [r[prefix + f'institution_{i}_oa_id'] for i in range(1, 3)
                              if prefix + f'institution_{i}_oa_id' in r and r[prefix + f'institution_{i}_oa_id']]
                assert column_ids == a['oa_ids']
                assert len(column_ids) == len(set(column_ids))
                person_ids.update(column_ids)
                checks['round_tripped_awards'] += 1
                checks['round_tripped_multi_id_awards'] += len(column_ids) > 1
            assert sorted(person_ids) == json.loads(r['oa_ids'])
            reconstructed[key] = person_ids
    assert len(keys) == EXPECTED['unique_entries']
    for r in rows(resolution / 'person_id_coverage.csv'):
        assert reconstructed[r['source_key']] == set(json.loads(r['IDs_after']))
        checks['person_union_matches'] += 1
    assert checks['person_union_matches'] == len(keys)
    seen_awards = set()
    for r in rows(stage / 'education_awards_openalex.csv'):
        key = (r['source_key'], int(r['award_index']))
        assert key not in seen_awards
        seen_awards.add(key)
        expected = json.loads(con.execute('SELECT data FROM awards WHERE source_key=? AND award_index=?', key).fetchone()[0])
        assert all(r[k] == expected[k] for k in original_award_fields)
        assert json.loads(r['oa_ids']) == expected['oa_ids']
        assert json.loads(r['oa_names']) == expected['oa_names']
        ledger = con.execute('SELECT data FROM ledger WHERE source_key=? AND award_index=?', key).fetchone()
        if ledger:
            ledger = json.loads(ledger[0])
            assert set(json.loads(r['oa_ids'])) == set(json.loads(ledger['IDs_after']))
            for k, v in ledger.items():
                assert r[k] == v, (key, k)
            checks['award_ledger_matches'] += 1
    assert checks['award_ledger_matches'] == EXPECTED['ledger_awards']
    selected = set()
    seen_variants = set()
    for r in rows(stage / 'original_row_provenance.csv'):
        ref = (r['source_file'], int(r['source_row']))
        assert ref not in seen_variants
        seen_variants.add(ref)
        original = con.execute('SELECT original FROM variants WHERE source_file=? AND source_row=?', ref).fetchone()[0]
        assert r['original_row_json'] == original
        preferred = con.execute('SELECT source_file,source_row,original FROM persons WHERE source_key=?', (r['source_key'],)).fetchone()
        assert r['selected_source_file'] == preferred[0] and int(r['selected_source_row']) == preferred[1]
        assert json.loads(r['differing_fields']) == differing_fields(json.loads(original), json.loads(preferred[2]))
        if r['selected_combined_row'] == 'True':
            assert ref == preferred[:2]
            assert r['source_key'] not in selected
            selected.add(r['source_key'])
        checks['provenance_rows_verified'] += 1
    assert len(seen_variants) == EXPECTED['original_rows'] and selected == keys
    return dict(passed=True, validated_at_utc=now(), checks=dict(checks),
                original_columns_and_row_order_preserved=True, no_prior_assignments_changed=True,
                snapshot_membership_verified=True, duplicate_source_variants_preserved=True,
                missing_values_preserved=True, institution_slot_order='Lexicographic OA ID',
                semantic_review_performed=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-root', default=os.environ.get('TALENT_DETS_DATA_DIR', str(Path.home() / 'Globtalent Dropbox/gtl_talent_dets')))
    parser.add_argument('--local-root', default=os.environ.get('AMWS_CHECKPOINT_ROOT', str(Path.home() / '.codex/amws_checkpoints')))
    args = parser.parse_args()
    run(args.data_root, args.local_root)

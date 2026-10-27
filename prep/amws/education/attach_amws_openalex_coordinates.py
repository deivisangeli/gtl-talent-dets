"""Attach local OpenAlex reference locations to the immutable AMWS ID export.

All original CSV fields remain strings and are round-trip checked. This runner
does not research identities, infer historical campuses, or calculate distances.
"""
import argparse
import collections
import csv
import itertools
import json
import math
import os
from pathlib import Path
import re
import shutil
import time
import unittest

import export_amws_openalex as export

NAME = 'amws_education_openalex_coordinates_20261014'
VERSION = 'reference_coordinate_attachment_v1'
KIND = 'openalex_reference_location'
COMBINED = 'amws_all_editions_education_openalex.csv'
AWARDS = 'education_awards_openalex.csv'
PROVENANCE = 'original_row_provenance.csv'
LOOKUP = 'institution_reference_coordinates.csv'
GEO_FIELDS = ('latitude', 'longitude', 'city', 'country')
ARRAY_FIELDS = ('oa_latitudes', 'oa_longitudes', 'oa_cities', 'oa_countries')
ID_COLUMN = re.compile(r'^(?:ba_bs|ma|ms|phd)_award_[1-9]\d*_institution_[1-9]\d*_oa_id$')


def require(condition, message):
    if not condition:
        raise ValueError(message)


def coordinate_schema(fields):
    require(len(fields) == len(set(fields)), 'Duplicate CSV headers')
    prefixes = [field[:-len('oa_id')] for field in fields if ID_COLUMN.fullmatch(field)]
    require(bool(prefixes), 'No numbered institution ID columns')
    added = [prefix + field for prefix in prefixes for field in GEO_FIELDS]
    require(not set(added) & set(fields), 'Coordinates already exist in source schema')
    return prefixes, added


def load_reference(path, required_ids):
    """Reject duplicate keys globally and invalid/missing required coordinates."""
    seen = set()
    result = {}
    for row in export.rows(path):
        entity = row['id']
        require(entity not in seen, 'Duplicate reference ID: ' + entity)
        seen.add(entity)
        if entity not in required_ids:
            continue
        try:
            latitude, longitude = float(row['latitude']), float(row['longitude'])
        except (ValueError, TypeError) as exc:
            raise ValueError('Missing or nonnumeric coordinates: ' + entity) from exc
        valid = (math.isfinite(latitude) and math.isfinite(longitude)
                 and -90 <= latitude <= 90 and -180 <= longitude <= 180
                 and row['valid_coordinates'].lower() == 'true')
        require(valid, 'Invalid reference coordinates: ' + entity)
        result[entity] = row
    missing = required_ids - result.keys()
    require(not missing, 'Assigned IDs missing from reference: ' + ','.join(sorted(missing)))
    return result, len(seen)


def wide_coordinates(row, prefixes, reference):
    added = {}
    for prefix in prefixes:
        entity = row[prefix + 'oa_id']
        if entity:
            require(entity in reference, 'Unreferenced assigned ID: ' + entity)
            record = reference[entity]
            added.update({prefix + field: record[field] for field in GEO_FIELDS})
        else:
            added.update({prefix + field: '' for field in GEO_FIELDS})
    return added


def award_coordinates(row, reference):
    entities = json.loads(row['oa_ids'])
    require(len(entities) == len(set(entities)), 'Duplicate IDs within award')
    require(len(entities) == len(json.loads(row['oa_names'])), 'Unaligned source institution names')
    require(all(i in reference for i in entities), 'Unreferenced award institution')
    return dict(oa_latitudes=export.dumps([float(reference[i]['latitude']) for i in entities]),
                oa_longitudes=export.dumps([float(reference[i]['longitude']) for i in entities]),
                oa_cities=export.dumps([reference[i]['city'] for i in entities]),
                oa_countries=export.dumps([reference[i]['country'] for i in entities]))


def preserved_rows(source, target, added_fields):
    """Independent file reread: preserve every old cell and physical CSV row order."""
    with source.open(encoding='utf-8-sig', newline='') as a, target.open(encoding='utf-8-sig', newline='') as b:
        before, after = csv.DictReader(a), csv.DictReader(b)
        require(after.fieldnames == before.fieldnames + list(added_fields), 'Export header differs: ' + target.name)
        for row_no, (old, new) in enumerate(itertools.zip_longest(before, after), 1):
            require(old is not None and new is not None, 'Row count differs: ' + target.name)
            require(all(new[k] == v for k, v in old.items()), f'Original field changed: {target.name}:{row_no}')
            yield old, new


def transform_wide(source, target, reference):
    with source.open(encoding='utf-8-sig', newline='') as f, target.open('w', encoding='utf-8-sig', newline='') as out:
        reader = csv.DictReader(f)
        prefixes, added_fields = coordinate_schema(reader.fieldnames)
        writer = csv.DictWriter(out, fieldnames=reader.fieldnames + added_fields)
        writer.writeheader()
        for row in reader:
            writer.writerow(dict(row, **wide_coordinates(row, prefixes, reference)))
    return prefixes, added_fields


def validate_wide(source, target, prefixes, added_fields, reference, combined=False):
    counts = collections.Counter()
    ids = set()
    keys = set()
    previous = (0, 0)
    for old, row in preserved_rows(source, target, added_fields):
        counts['rows'] += 1
        entry_ids = set()
        per_award = collections.Counter()
        for prefix in prefixes:
            entity = row[prefix + 'oa_id']
            if not entity:
                require(all(row[prefix + k] == '' for k in GEO_FIELDS), 'Unassigned slot received coordinates')
                counts['blank_institution_slots'] += 1
                continue
            r = reference[entity]
            # Compare directly to the reference rather than the transformation helper.
            require(all(row[prefix + field] == r[field] for field in GEO_FIELDS), 'Incorrect slot coordinate join')
            counts['institution_slots_with_coordinates'] += 1
            per_award[prefix.rsplit('_institution_', 1)[0]] += 1
            entry_ids.add(entity)
            ids.add(entity)
        require(entry_ids == set(json.loads(row['oa_ids'])), 'Entry ID union differs from institution slots')
        counts['entries_with_coordinates'] += bool(entry_ids)
        counts['awards_with_coordinates'] += len(per_award)
        counts['multi_id_awards_with_coordinates'] += sum(n > 1 for n in per_award.values())
        if combined:
            key = row['source_key']
            require(key not in keys, 'Duplicate combined source_key')
            keys.add(key)
            numeric = tuple(map(int, key.split(':')))
            require(numeric > previous, 'Combined entry order changed')
            previous = numeric
    return dict(counts, distinct_ids_with_coordinates=len(ids), added_columns=len(added_fields))


def copy_verified(source, target, expected=None):
    expected = expected or export.digest(source)
    target.parent.mkdir(parents=True, exist_ok=True)
    for attempt in range(5):
        try:
            shutil.copy2(source, target)
            require(export.digest(target) == expected, 'Copied checksum differs: ' + str(target))
            return
        except PermissionError:
            if attempt == 4:
                raise
            time.sleep(2 ** attempt)


def run(data_root, local_root):
    timer = time.perf_counter()
    started = export.now()
    root = Path(data_root) / 'output/amws'
    parent = root / export.NAME
    destination = root / NAME
    local = Path(local_root) / NAME
    stage = local / 'release'
    marker = destination / 'completion_manifest.json'
    if marker.exists():
        done = export.read_json(marker)
        for rel, checksum in done['output_checksums'].items():
            require(export.digest(destination / rel) == checksum, 'Published file changed: ' + rel)
        for source, checksum in done['input_checksums'].items():
            require(export.digest(source) == checksum, 'Published coordinate input changed: ' + source)
        print(export.dumps(dict(already_complete=True, release=str(destination), coverage=done['coverage'])), flush=True)
        return
    require(not destination.exists(), 'Incomplete destination exists; refusing to overwrite')
    parent_completion = export.read_json(parent / 'completion_manifest.json')
    require(parent_completion['status'] == 'complete', 'Parent ID export is incomplete')
    input_hashes = {}
    for rel, expected in parent_completion['output_checksums'].items():
        source = parent / rel
        require(export.digest(source) == expected, 'Parent export checksum differs: ' + rel)
        input_hashes[str(source)] = expected
    for source in (parent / 'completion_manifest.json', root / 'education_geolocated/reference_institutions.csv',
                   Path(__file__), Path(__file__).with_name('test_attach_amws_openalex_coordinates.py'), Path(export.__file__)):
        input_hashes[str(source)] = export.digest(source)
    local.mkdir(parents=True, exist_ok=True)
    stage.mkdir(exist_ok=True)
    checkpoint = local / 'input_manifest.json'
    if checkpoint.exists():
        require(export.read_json(checkpoint)['input_checksums'] == input_hashes, 'Coordinate inputs differ from checkpoint')
    source_coverage = export.read_json(parent / 'coverage_report.json')
    canonical_names = {}
    award_count = 0
    for row in export.rows(parent / AWARDS):
        entities, names = json.loads(row['oa_ids']), json.loads(row['oa_names'])
        require(len(entities) == len(names), 'Unaligned original award ID/name arrays')
        for entity, name in zip(entities, names, strict=True):
            require(re.fullmatch(r'I\d+', entity) is not None, 'Invalid assigned ID')
            require(entity not in canonical_names or canonical_names[entity] == name, 'Conflicting original canonical names')
            canonical_names[entity] = name
        award_count += 1
    reference_path = root / 'education_geolocated/reference_institutions.csv'
    reference, reference_rows = load_reference(reference_path, set(canonical_names))
    require(len(reference) == source_coverage['distinct_ids'] == 1627, 'Unexpected assigned-ID coverage')
    require(award_count == source_coverage['earned_awards'] == 337692, 'Unexpected earned-award count')
    dates = collections.Counter(r['updated_date'][:10] for r in reference.values())
    reference_manifest = dict(reference_table=str(reference_path), reference_sha256=input_hashes[str(reference_path)],
                              reference_table_rows=reference_rows, assigned_institution_rows=len(reference),
                              coordinate_kind=KIND, historical_campus_accuracy_verified=False,
                              reference_record_updated_dates=dict(sorted(dates.items())),
                              source_snapshot_files=sorted({r['source_file'] for r in reference.values()}),
                              update_date_meaning='OpenAlex reference record update date; not a teaching or graduation date',
                              may_represent_city_coordinates=True)
    manifest = dict(version=VERSION, started_at_utc=started, parent_release=str(parent),
                    input_checksums=input_hashes, reference=reference_manifest,
                    matching_policy='Join assigned OA IDs only; preserve all prior identity decisions',
                    no_network_requests=True, no_historical_location_inference=True)
    export.write_json(checkpoint, manifest)
    export.write_json(stage / 'input_manifest.json', manifest)
    print(f'Parent release verified; all {len(reference):,} assigned institutions have valid local coordinates.', flush=True)

    lookup_fields = ['openalex_id', 'oa_canonical_name', 'reference_name', *GEO_FIELDS,
                     'region', 'geonames_city_id', 'coordinate_kind', 'historical_campus_accuracy_verified',
                     'reference_updated_date', 'reference_source_file', 'reference_table', 'reference_sha256']
    with (stage / LOOKUP).open('w', encoding='utf-8-sig', newline='') as out:
        writer = csv.DictWriter(out, fieldnames=lookup_fields)
        writer.writeheader()
        for entity in sorted(reference):
            r = reference[entity]
            writer.writerow(dict(openalex_id=entity, oa_canonical_name=canonical_names[entity], reference_name=r['name'],
                                 **{field: r[field] for field in GEO_FIELDS}, region=r['region'],
                                 geonames_city_id=r['geonames_city_id'], coordinate_kind=KIND,
                                 historical_campus_accuracy_verified='False', reference_updated_date=r['updated_date'],
                                 reference_source_file=r['source_file'], reference_table=str(reference_path),
                                 reference_sha256=input_hashes[str(reference_path)]))
    schemas = {}
    filenames = [COMBINED] + [r['output_file'] for r in source_coverage['person_files']]
    for filename in filenames:
        schemas[filename] = transform_wide(parent / filename, stage / filename, reference)
        print('Attached institution coordinates: ' + filename, flush=True)
    with (parent / AWARDS).open(encoding='utf-8-sig', newline='') as source, \
            (stage / AWARDS).open('w', encoding='utf-8-sig', newline='') as out:
        reader = csv.DictReader(source)
        require(not set(ARRAY_FIELDS) & set(reader.fieldnames), 'Award coordinate arrays already exist')
        writer = csv.DictWriter(out, fieldnames=reader.fieldnames + list(ARRAY_FIELDS))
        writer.writeheader()
        for row in reader:
            writer.writerow(dict(row, **award_coordinates(row, reference)))
    copy_verified(parent / PROVENANCE, stage / PROVENANCE, input_hashes[str(parent / PROVENANCE)])
    print('Attached award arrays and copied duplicate provenance; validating every original field and coordinate join.', flush=True)

    validation_files = {}
    for filename in filenames:
        prefixes, added = schemas[filename]
        result = validate_wide(parent / filename, stage / filename, prefixes, added, reference, combined=filename == COMBINED)
        expected_rows = source_coverage['total_entries'] if filename == COMBINED else next(
            r['rows'] for r in source_coverage['person_files'] if r['output_file'] == filename)
        require(result['rows'] == expected_rows, 'Entry count differs: ' + filename)
        validation_files[filename] = result
        print('Validated original fields and institution coordinates: ' + filename, flush=True)
    combined = validation_files[COMBINED]
    for actual, expected in (('rows', 'total_entries'), ('entries_with_coordinates', 'entries_with_id'),
                             ('awards_with_coordinates', 'matched_awards'), ('multi_id_awards_with_coordinates', 'multi_id_awards'),
                             ('distinct_ids_with_coordinates', 'distinct_ids')):
        require(combined[actual] == source_coverage[expected], 'Combined coordinate coverage differs: ' + actual)
    counts = collections.Counter()
    keys = set()
    for old, row in preserved_rows(parent / AWARDS, stage / AWARDS, ARRAY_FIELDS):
        key = (row['source_key'], row['award_index'])
        require(key not in keys, 'Duplicate award key')
        keys.add(key)
        entities = json.loads(row['oa_ids'])
        latitude, longitude, city, country = [json.loads(row[k]) for k in ARRAY_FIELDS]
        require(all(len(values) == len(entities) for values in (latitude, longitude, city, country)), 'Unaligned coordinate arrays')
        for i, entity in enumerate(entities):
            r = reference[entity]
            require(latitude[i] == float(r['latitude']) and longitude[i] == float(r['longitude']), 'Incorrect award coordinate array')
            require(city[i] == r['city'] and country[i] == r['country'], 'Incorrect award locality array')
        counts['awards'] += 1
        counts['matched_awards'] += bool(entities)
        counts['multi_id_awards'] += len(entities) > 1
        counts['institution_assignments_with_coordinates'] += len(entities)
    require(counts['awards'] == source_coverage['earned_awards'], 'Award row count differs')
    require(counts['matched_awards'] == combined['awards_with_coordinates'], 'Award/entry coordinate coverage differs')
    require(counts['multi_id_awards'] == combined['multi_id_awards_with_coordinates'], 'Two-ID award coverage differs')
    require(counts['institution_assignments_with_coordinates'] == combined['institution_slots_with_coordinates'], 'Institution assignment count differs')
    lookup_keys = set()
    for row in export.rows(stage / LOOKUP):
        entity = row['openalex_id']
        require(entity not in lookup_keys, 'Duplicate lookup ID')
        lookup_keys.add(entity)
        require(row['oa_canonical_name'] == canonical_names[entity], 'Lookup canonical name changed')
        require(row['reference_name'] == reference[entity]['name'], 'Lookup reference name changed')
        require(all(row[k] == reference[entity][k] for k in GEO_FIELDS), 'Lookup coordinates differ')
        require(row['reference_updated_date'] == reference[entity]['updated_date'], 'Lookup date differs')
        require(row['reference_source_file'] == reference[entity]['source_file'], 'Lookup source differs')
        require(row['coordinate_kind'] == KIND and row['historical_campus_accuracy_verified'] == 'False', 'Missing coordinate qualification')
        require(row['reference_sha256'] == input_hashes[str(reference_path)], 'Lookup reference checksum differs')
    require(lookup_keys == set(reference), 'Lookup coverage differs')
    require(export.digest(stage / PROVENANCE) == input_hashes[str(parent / PROVENANCE)], 'Duplicate provenance changed')
    test_result = unittest.TextTestRunner(verbosity=1).run(unittest.defaultTestLoader.discover(
        str(Path(__file__).parent), pattern='test_attach_amws_openalex_coordinates.py'))
    require(test_result.wasSuccessful(), 'Coordinate attachment tests failed')
    export.write_json(stage / 'test_report.json', dict(tests_run=test_result.testsRun, failures=len(test_result.failures), errors=len(test_result.errors)))
    validation = dict(passed=True, validated_at_utc=export.now(), per_file=validation_files, award_table=dict(counts),
                      lookup_rows=len(lookup_keys), all_existing_cells_and_row_order_preserved=True,
                      original_birthplace_fields_preserved=True, duplicate_provenance_byte_identical=True,
                      existing_ids_names_and_assignment_classes_preserved=True, all_assigned_ids_have_valid_coordinates=True,
                      coordinates_only_for_assigned_ids=True, historical_campus_accuracy_verified=False)
    coverage = dict(source_coverage)
    coverage.update(parent_release=str(parent), coordinate_release_version=VERSION,
                    source_combined_columns=source_coverage['combined_columns'],
                    combined_columns=source_coverage['combined_columns'] + combined['added_columns'],
                    coordinate_columns_added=combined['added_columns'],
                    coordinate_coverage=dict(distinct_ids=len(reference), entries=combined['entries_with_coordinates'],
                                             matched_awards=counts['matched_awards'], multi_id_awards=counts['multi_id_awards'],
                                             institution_assignments=counts['institution_assignments_with_coordinates'],
                                             missing_or_invalid_assigned_ids=0),
                    coordinate_kind=KIND, historical_campus_accuracy_verified=False)
    export.write_json(stage / 'validation_report.json', validation)
    export.write_json(stage / 'coverage_report.json', coverage)
    export.write_json(stage / 'reference_manifest.json', reference_manifest)
    export.write_json(stage / 'source_coverage_report.json', source_coverage)
    original_dictionary = (parent / 'DATA_DICTIONARY.md').read_text(encoding='utf-8')
    dictionary = '''

## University reference coordinates (2026-10-14)

This release appends local reference coordinates to the completed AMWS ID export.
All pre-existing fields, row order, names, IDs, assignment classes, conditions,
birthplace fields and duplicate provenance remain unchanged. Files retain their
previous filenames within the new release directory.

Each numbered institution prefix gains latitude, longitude, city and country;
for example ba_bs_award_1_institution_1_latitude. The existing institution OA ID
is the only join key. Empty institution slots keep four empty cells. Latitude
and longitude are decimal degrees copied without rounding; country retains the
reference country code. These columns describe universities, not birthplaces.

In education_awards_openalex.csv, oa_latitudes and oa_longitudes are JSON numeric
arrays; oa_cities and oa_countries are JSON string arrays. Every position follows
the existing oa_ids array, including two-institution awards. Unmatched awards
have four empty arrays []. No alternative candidate receives coordinates.

institution_reference_coordinates.csv contains one row per assigned ID with the
existing OA canonical name, separate reference name, coordinates, city, country,
region, GeoNames city ID, source file, record update date and reference checksum.
coordinate_kind=openalex_reference_location; historical_campus_accuracy_verified
is False, meaning historical campus accuracy has not been verified. OpenAlex
reference locations may be shared city coordinates, as for Harvard and MIT.
Reference update dates are source record dates, not graduation or teaching dates.
reference_manifest.json records the full set of snapshot source files and dates.

The coordinate attachment does not validate institution identities or historical
campuses. Existing provisional and conditional labels remain applicable. No
distances, county outcomes, API queries or historical location inferences are added.
'''
    (stage / 'DATA_DICTIONARY.md').write_text(original_dictionary + dictionary, encoding='utf-8')
    code = stage / 'code_snapshot'
    code.mkdir(exist_ok=True)
    for source in (Path(__file__), Path(__file__).with_name('test_attach_amws_openalex_coordinates.py'), Path(export.__file__)):
        copy_verified(source, code / source.name, input_hashes[str(source)])
    for source, checksum in input_hashes.items():
        require(export.digest(source) == checksum, 'Input changed during coordinate attachment: ' + source)
    export.write_json(stage / 'timing_report.json', dict(started_at_utc=started, validated_at_utc=export.now(),
                                                       seconds_before_publication=time.perf_counter() - timer))
    outputs = {p.relative_to(stage).as_posix(): export.digest(p) for p in sorted(stage.rglob('*')) if p.is_file()}
    export.write_json(stage / 'checksums.json', dict(algorithm='sha256', input_checksums=input_hashes, output_checksums=outputs))
    outputs['checksums.json'] = export.digest(stage / 'checksums.json')
    print('All coordinate and preservation checks passed; publishing the new release.', flush=True)
    destination.mkdir(exist_ok=False)
    for rel, checksum in outputs.items():
        copy_verified(stage / rel, destination / rel, checksum)
    completion = dict(status='complete', version=VERSION, published_at_utc=export.now(),
                      input_checksums=input_hashes, output_checksums=outputs, coverage=coverage,
                      validation_passed=True, copied_checksums_verified=True,
                      elapsed_seconds=time.perf_counter() - timer)
    export.write_json(marker, completion)
    require(export.read_json(marker) == completion, 'Completion marker differs')
    print(export.dumps(dict(release=str(destination), coordinate_coverage=coverage['coordinate_coverage'],
                           combined_columns=coverage['combined_columns'], elapsed_seconds=completion['elapsed_seconds'])), flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-root', default=os.environ.get('TALENT_DETS_DATA_DIR', str(Path.home() / 'Globtalent Dropbox/gtl_talent_dets')))
    parser.add_argument('--local-root', default=os.environ.get('AMWS_CHECKPOINT_ROOT', str(Path.home() / '.codex/amws_checkpoints')))
    args = parser.parse_args()
    run(args.data_root, args.local_root)

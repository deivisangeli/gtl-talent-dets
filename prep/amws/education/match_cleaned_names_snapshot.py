"""Offline, provisional identity matching; no web, APIs, or training geography."""
import argparse
import collections
import csv
import datetime
import gzip
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time
import unicodedata
import checkpoint_storage as storage

HERE = Path(__file__).resolve().parent
VERSION = 'snapshot_name_v1'
STOP = set('university college institute institution school the of and for at state national central medical science sciences education polytechnic technical agricultural international saint st hospital'.split())
OFFICIAL = {'canonical', 'alternative'}


def now():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def read(path):
    return json.loads(storage.resolve(path).read_text(encoding='utf-8-sig'))


def jsonl(path):
    with storage.resolve(path).open(encoding='utf-8-sig') as source:
        return [json.loads(line) for line in source if line.strip()]


def digest(path):
    value = hashlib.sha256()
    with storage.resolve(path).open('rb') as source:
        for block in iter(lambda: source.read(1048576), b''):
            value.update(block)
    return value.hexdigest()


def write(path, obj):
    path = storage.resolve(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(obj, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
    for attempt in range(7):
        try:
            temporary.replace(path)
            return
        except PermissionError:
            if attempt == 6:
                raise
            time.sleep(.1 * 2 ** attempt)


def write_jsonl(path, rows):
    path = Path(path)
    temporary = path.with_suffix(path.suffix + '.tmp')
    with temporary.open('w', encoding='utf-8', newline='') as target:
        for row in rows:
            target.write(json.dumps(row, ensure_ascii=False) + '\n')
    temporary.replace(path)


def normalize(text):
    text = unicodedata.normalize('NFKD', text).casefold().replace('&', ' and ')
    text = ''.join(c for c in text if not unicodedata.combining(c))
    return ' '.join(re.findall(r'[^\W_]+', text))


def oid(text):
    match = re.fullmatch(r'(?:https://openalex.org/)?(I\d+)', text or '')
    return match.group(1) if match else None


def grams(text):
    text = text.replace(' ', '')
    return set(text[i:i+3] for i in range(len(text)-2))


class SnapshotIndex:
    def __init__(self, records, aliases):
        self.records = records
        self.exact = collections.defaultdict(dict)
        self.by_id = collections.defaultdict(dict)
        self.tokens = collections.defaultdict(set)
        self.trigrams = collections.defaultdict(set)
        for alias in aliases:
            key = normalize(alias['alias'])
            if not key:
                continue
            alias = dict(alias, normalized=key)
            entity = alias['id']
            priority = {'canonical': 0, 'alternative': 1, 'acronym': 2}.get(alias['source'], 3)
            alias['priority'] = priority
            prior = self.by_id[entity].get(key)
            if prior is None or (priority, alias['alias'], alias['source']) < (prior['priority'], prior['alias'], prior['source']):
                self.by_id[entity][key] = alias
            if alias['source'] in OFFICIAL:
                prior = self.exact[key].get(entity)
                if prior is None or (priority, alias['alias']) < (prior['priority'], prior['alias']):
                    self.exact[key][entity] = alias
            for token in set(key.split()) - STOP:
                self.tokens[token].add(entity)
            for gram in grams(key):
                self.trigrams[gram].add(entity)

    def identity(self, alias, score=100):
        record = self.records[alias['id']]
        return dict(openalex_id=alias['id'], canonical_name=record['canonical_name'],
                    matched_alias=alias['alias'],
                    matched_snapshot_alias=alias['alias'] if alias['source'] in OFFICIAL | {'acronym'} else None,
                    alias_source=alias['source'],
                    institution_type=record['type'], is_super_system=record['is_super_system'],
                    score=score)

    def exact_result(self, name):
        key = normalize(name)
        matches = [self.identity(a) for _, a in sorted(self.exact[key].items())]
        if len(matches) > 1:
            status = 'ambiguous'
        elif not matches:
            status = 'no_exact_match'
        elif matches[0]['institution_type'] != 'education':
            status = 'non_education'
        elif matches[0]['is_super_system']:
            status = 'system'
        else:
            status = 'eligible_unique_exact'
        return dict(proposed_name=name, normalized_query=key, exact_status=status, exact_matches=matches)

    def retrieved(self, name):
        key = normalize(name)
        ids = set(self.exact[key])
        counts = collections.Counter()
        for gram in grams(key):
            if len(self.trigrams[gram]) < 3000:
                counts.update(self.trigrams[gram])
        for token in set(key.split()) - STOP:
            if len(self.tokens[token]) <= 500:
                ids.update(self.tokens[token])
        ids.update(entity for entity, _ in sorted(counts.items(), key=lambda x: (-x[1], x[0]))[:80])
        return ids


def load_snapshot(snapshot, crosswalk_files):
    records = {}
    aliases = []
    for path in sorted(snapshot.rglob('*.gz')):
        with gzip.open(path, 'rt', encoding='utf-8') as source:
            for line in source:
                row = json.loads(line)
                entity = oid(row['id'])
                if not entity or entity in records:
                    raise ValueError('Invalid or duplicated snapshot ID')
                records[entity] = dict(openalex_id=entity, canonical_name=row['display_name'],
                                       type=row['type'], is_super_system=row.get('is_super_system', False),
                                       source_file=str(path), updated_date=row.get('updated_date'))
                for kind, names in [('canonical', [row['display_name']]),
                                    ('alternative', row.get('display_name_alternatives') or []),
                                    ('acronym', row.get('display_name_acronyms') or [])]:
                    aliases.extend(dict(id=entity, alias=name, source=kind) for name in names)
    rejected = []
    for path in crosswalk_files:
        with path.open(encoding='utf-8-sig', newline='') as source:
            for row in csv.DictReader(source):
                name = row.get('alias_example', row.get('university'))
                for field in ('oa_id', 'OA_id', 'oa_id_alt', 'OA_id_alt'):
                    if not row.get(field):
                        continue
                    entity = oid(row[field])
                    if entity not in records:
                        rejected.append(dict(source=path.name, alias=name, supplied_id=row[field], reason='malformed_or_absent_snapshot_ID'))
                    elif name:
                        aliases.append(dict(id=entity, alias=name, source=path.name))
    return records, aliases, rejected


def score_queries(index, names, stage, rscript):
    queries = {hashlib.sha256(name.encode('utf-8')).hexdigest()[:20]: name for name in sorted(names)}
    if len(queries) != len(names):
        raise ValueError('Query ID hash collision')
    target = stage/'candidate_pairs.csv'
    with target.open('w', encoding='utf-8-sig', newline='') as destination:
        writer = csv.DictWriter(destination, fieldnames=['query_id', 'id', 'alias', 'source', 'priority', 'query', 'candidate'])
        writer.writeheader()
        count = 0
        for query_id, name in queries.items():
            for entity in sorted(index.retrieved(name)):
                for key, alias in sorted(index.by_id[entity].items()):
                    writer.writerow(dict(query_id=query_id, id=entity, alias=alias['alias'], source=alias['source'],
                                         priority=alias['priority'], query=normalize(name), candidate=key))
                    count += 1
    subprocess.run([str(rscript), str(HERE/'match_cleaned_names_snapshot.R'), str(stage)], check=True)
    scored = collections.defaultdict(list)
    with (stage/'candidate_scores.csv').open(encoding='utf-8-sig', newline='') as source:
        for row in csv.DictReader(source):
            if row['id'] not in index.records or row['query_id'] not in queries:
                raise ValueError('Scorer returned unsupported ID/query')
            score = float(row['score'])
            if not 0 <= score <= 100:
                raise ValueError('Invalid similarity score')
            scored[queries[row['query_id']]].append(index.identity(dict(id=row['id'], alias=row['alias'], source=row['source']), score))
    results = {}
    for query_id, name in queries.items():
        row = dict(index.exact_result(name), query_id=query_id, candidates=scored[name],
                   method='official_alias_exact_then_ordered_normalized_DL', identity_accuracy_verified=False)
        if len(row['candidates']) > 5 or len({c['openalex_id'] for c in row['candidates']}) != len(row['candidates']):
            raise ValueError('Candidates must contain up to five distinct IDs')
        checkpoint = storage.checkpoint_dir(stage,'query_checkpoints')/(query_id+'.json')
        if checkpoint.exists() and read(checkpoint) != row:
            raise ValueError('Recomputed query differs from checkpoint')
        if not checkpoint.exists():
            write(checkpoint, row)
        results[name] = row
    return results, count


def match_raw(cleanup, original, proposed):
    components = []
    conditions = original.get('prior_conditions') or []
    for name in cleanup['standard_names']:
        query = proposed[name]
        allowed = cleanup['status'] == 'clear' and not conditions and query['exact_status'] == 'eligible_unique_exact'
        match = query['exact_matches'][0] if allowed else None
        components.append(dict(proposed_name=name, query_id=query['query_id'], exact_status=query['exact_status'],
                               accepted_provisional_match=match, candidates=query['candidates'],
                               method='official_unique_exact' if allowed else 'candidate_only',
                               provisional_status='provisional_exact' if allowed else query['exact_status']))
    accepted = [c['accepted_provisional_match'] for c in components if c['accepted_provisional_match']]
    if conditions:
        status = 'conditional_preserved'
    elif not components:
        status = 'cleanup_' + cleanup['status']
    elif len(accepted) == len(components):
        status = 'fully_provisional_matched'
    elif accepted:
        status = 'partially_provisional_matched'
    elif any(c['exact_status'] == 'ambiguous' for c in components):
        status = 'ambiguous'
    elif any(c['candidates'] for c in components):
        status = 'candidate_only'
    else:
        status = 'no_candidate'
    return dict(record_id=original['record_id'], institution_as_written=original['institution_as_written'],
                origin='offline_cleanup', cleanup_status=cleanup['status'], cleanup_decision=cleanup,
                award_count=original['award_count'], proposed_names=cleanup['standard_names'], components=components,
                accepted_provisional_ids=sorted({m['openalex_id'] for m in accepted}), status=status,
                prior_conditions=conditions, prior_accepted_ids=original.get('prior_accepted_ids', []),
                identity_accuracy_verified=False)


def coverage(rows, inventory):
    groups = collections.defaultdict(lambda: collections.Counter())
    full_awards = set()
    partial_awards = set()
    candidate_awards = set()
    seen_awards = set()
    for row in rows:
        original = inventory[row['record_id']]
        refs = {(a['source_key'], str(a['award_index'])) for a in original['award_refs']}
        if len(refs) != row['award_count'] or seen_awards & refs:
            raise ValueError('Duplicated award reference or inventory award count mismatch')
        seen_awards.update(refs)
        group = groups[row['cleanup_status']]
        group['entries'] += 1
        group['award_references'] += len(refs)
        group[row['status']] += 1
        if row['status'] == 'fully_provisional_matched':
            full_awards.update(refs)
            group['fully_provisional_matched_awards'] += len(refs)
        elif row['status'] == 'partially_provisional_matched':
            partial_awards.update(refs)
            group['partially_provisional_matched_awards'] += len(refs)
        if any(c['candidates'] for c in row['components']):
            candidate_awards.update(refs)
            group['entries_with_candidates'] += 1
            group['awards_with_candidates'] += len(refs)
    return dict(entries=len(rows), status_counts=dict(collections.Counter(r['status'] for r in rows)),
                by_cleanup_status=dict(groups), unique_award_references=len(seen_awards),
                fully_provisional_matched_award_references=len(full_awards),
                partially_provisional_matched_award_references=len(partial_awards),
                candidate_award_references=len(candidate_awards),
                distinct_provisional_ids=len({i for r in rows for i in r['accepted_provisional_ids']}),
                interpretation='Candidates overlap provisional matches; full and partial award sets are disjoint. Identity accuracy is unverified.')


def verify_checksums(checks):
    for path, checksum in checks.items():
        storage.verify_checksum(path,checksum)


def build(root, allocation, output, rscript):
    root, allocation, output = Path(root), Path(allocation), Path(output)
    if output.exists():
        manifest = read(output/'completion_manifest.json')
        verify_checksums(manifest['source_checksums'])
        verify_checksums({str(output/name): sha for name, sha in manifest['output_checksums'].items()})
        storage.verify_release(output)
        return dict(status='already_complete', coverage=read(output/'coverage_report.json')['offline_cleanup'])
    stage = output.with_name('.'+output.name+'.staging')
    stage.mkdir(parents=True, exist_ok=True)
    offline = root/'institution_names_offline_20261011'
    research = root/'openalex_id_research_20261011'
    naming = root/'institution_name_pilot_20261011'
    snapshot = allocation/'Data/external/oa_snapshot/data/institutions'
    crosswalks = [allocation/'Data/prep/crosswalks'/name for name in ('master_alias_oa.csv', 'OA_revelio_shanghai.csv')]
    files = [offline/'results.json', offline/'sample_manifest.json', offline/'completion_manifest.json',
             research/'inventory.jsonl', research/'research_results.jsonl', research/'retained_unconditional_matches.json',
             research/'retained_conditional_matches.json', research/'source_manifest.json',
             naming/'results.json', HERE/'match_cleaned_names_snapshot.py', HERE/'match_cleaned_names_snapshot.R',
             HERE/'test_match_cleaned_names_snapshot.py', HERE/'checkpoint_storage.py']
    files += sorted(p for p in snapshot.rglob('*') if p.is_file()) + crosswalks
    protected = read(offline/'sample_manifest.json')['protected_files']
    checks = {str(path): digest(path) for path in files}
    checks.update(protected)
    verify_checksums(checks)
    provenance = dict(version=VERSION, started_at_utc=now(), source_checksums=checks,
                      reference=str(snapshot), normalization='NFKD/casefold/diacritics/punctuation/whitespace/&; ordered tokens',
                      automatic_policy='clear; unique official canonical/alternative across all types; education; not system; no prior conditions',
                      fuzzy_policy='top five distinct IDs, ordered normalized Damerau-Levenshtein; candidates only',
                      network_access=False, semantic_review=False, training_geocoding=False)
    manifest_file = stage/'snapshot_manifest.json'
    if manifest_file.exists():
        old = read(manifest_file)
        if old['source_checksums'] != checks or old['version'] != VERSION:
            raise ValueError('Staging inputs/code changed; preserve stage and select a new release')
        provenance = old
    else:
        write(manifest_file, provenance)
    cleanup = read(offline/'results.json')
    inputs = jsonl(research/'inventory.jsonl')
    inventory = {r['record_id']: r for r in inputs}
    researched = jsonl(research/'research_results.jsonl')
    naming_rows = read(naming/'results.json')
    retained = read(research/'retained_unconditional_matches.json')
    if len(inventory) != 17460 or len(cleanup) != 15981 or len(retained) != 1449 or len(researched) != 1379 or len(naming_rows) != 100:
        raise ValueError('Unexpected input counts')
    groups = [{r['record_id'] for r in group} for group in (cleanup, researched, naming_rows)]
    if any(groups[a] & groups[b] for a in range(3) for b in range(a+1, 3)) or set.union(*groups) != set(inventory):
        raise ValueError('Input groups do not partition inventory')
    for row in cleanup:
        if row['institution_as_written'] != inventory[row['record_id']]['institution_as_written']:
            raise ValueError('Cleanup raw name changed')
    records, aliases, rejected = load_snapshot(snapshot, crosswalks)
    if len(records) != 120658:
        raise ValueError('Unexpected snapshot count')
    index = SnapshotIndex(records, aliases)
    names = {n for row in cleanup for n in row['standard_names']}
    proposed, pair_count = score_queries(index, names, stage, rscript)
    write_jsonl(stage/'proposed_name_matches.jsonl', [proposed[n] for n in sorted(proposed)])
    raw_rows = [match_raw(row, inventory[row['record_id']], proposed) for row in cleanup]
    by_id = {r['record_id']: r for r in raw_rows}
    for origin, prior_rows in [('oa_research_preserved', researched), ('name_research_pilot_preserved', naming_rows)]:
        for prior in prior_rows:
            original = inventory[prior['record_id']]
            if prior['institution_as_written'] != original['institution_as_written']:
                raise ValueError('Prior raw name changed')
            by_id[prior['record_id']] = dict(record_id=prior['record_id'], institution_as_written=prior['institution_as_written'],
                    origin=origin, award_count=original['award_count'], status=prior['status'], prior_decision=prior)
    crosswalk = [by_id[row['record_id']] for row in inputs]
    crosswalk += [dict(record_id=row['name_id'], institution_as_written=row['name'], origin='retained_unconditional_preserved',
                      status=row['status'], prior_decision=row) for row in retained]
    if len(crosswalk) != 18909 or len({r['record_id'] for r in crosswalk}) != 18909 or len({r['institution_as_written'] for r in crosswalk}) != 18909:
        raise ValueError('Full crosswalk does not cover 18,909 unique raw entries')
    write_jsonl(stage/'raw_entry_crosswalk.jsonl', crosswalk)
    write_jsonl(stage/'offline_raw_entry_crosswalk.jsonl', raw_rows)
    write_jsonl(stage/'ranked_alternatives.jsonl', [dict(query_id=q['query_id'], proposed_name=q['proposed_name'], candidates=q['candidates']) for q in proposed.values()])
    write(stage/'rejected_external_leads.json', rejected)
    for path in [research/'research_results.jsonl', naming/'results.json', research/'retained_unconditional_matches.json', research/'retained_conditional_matches.json']:
        subfolder = stage/'preserved'/path.parent.name
        subfolder.mkdir(parents=True, exist_ok=True)
        shutil.copy2(path, subfolder/path.name)
        if digest(subfolder/path.name) != digest(path):
            raise ValueError('Prior decisions copy changed')
    # Reconcile against the earned-degree ledger without interpreting biographies or dates.
    ledger = Path(inputs[0]['award_evidence_file'])
    ledger_counts = collections.Counter()
    ledger_refs = collections.defaultdict(set)
    with ledger.open(encoding='utf-8-sig', newline='') as source:
        for award in csv.DictReader(source):
            if award['requested_family'] in ('ba_bs', 'ma', 'ms', 'phd') and award['family_award_index'] and award['honorary'] == 'False' and award['institution_as_written']:
                name = award['institution_as_written']
                ref = (award['source_key'], str(award['award_index']))
                if ref in ledger_refs[name]:
                    raise ValueError('Duplicate ledger award position')
                ledger_refs[name].add(ref)
                ledger_counts[name] += 1
    if set(ledger_counts) != {r['institution_as_written'] for r in crosswalk}:
        raise ValueError('Crosswalk names do not cover ledger names')
    for original in inputs:
        name = original['institution_as_written']
        if {(a['source_key'], str(a['award_index'])) for a in original['award_refs']} != ledger_refs[name] or original['award_count'] != ledger_counts[name]:
            raise ValueError('Inventory/ledger award links differ')
    for row in crosswalk:
        row['award_count'] = ledger_counts[row['institution_as_written']]
    write_jsonl(stage/'raw_entry_crosswalk.jsonl', crosswalk)
    report = dict(offline_cleanup=coverage(raw_rows, inventory),
                  prior_groups={origin:dict(entries=sum(r['origin']==origin for r in crosswalk),
                                            award_references=sum(r['award_count'] for r in crosswalk if r['origin']==origin),
                                            decisions_preserved=True) for origin in ('oa_research_preserved','name_research_pilot_preserved','retained_unconditional_preserved')},
                  full_inventory_entries=len(crosswalk), full_inventory_award_references=sum(ledger_counts.values()),
                  distinct_proposed_names=len(names), snapshot_records=len(records), scored_alias_pairs=pair_count,
                  proposed_name_exact_statuses=dict(collections.Counter(q['exact_status'] for q in proposed.values())),
                  semantic_review=False, identity_accuracy_verified=False, geolocation_modified=False)
    if sum(r['award_count'] for r in crosswalk) != sum(ledger_counts.values()):
        raise ValueError('Award-weighted group totals do not reconcile')
    validate_outputs(raw_rows, cleanup, inventory, proposed, index)
    # Recompute every raw assignment to test deterministic checkpoint aggregation.
    if raw_rows != [match_raw(row, inventory[row['record_id']], proposed) for row in cleanup]:
        raise ValueError('Nonreproducible raw matching')
    prior_missing = set()
    for row in retained:
        prior_missing.update(i for i in row['accepted_ids'] if oid(i) not in records)
    for row in researched:
        prior_missing.update(m['openalex_id'] for m in row.get('matches', []) if oid(m['openalex_id']) not in records)
    verify_checksums(checks)
    write(stage/'coverage_report.json', report)
    validation = dict(passed=True, validated_at_utc=now(), raw_identifier_and_name_preservation=True,
                      coverage_entries=18909, cleanup_entries=15981, exact_coverage=True,
                      new_accepted_ids_verified_against_snapshot=True, prior_decisions_preserved=True,
                      prior_IDs_absent_from_snapshot=sorted(prior_missing), immutable_source_checksums_verified=len(checks),
                      award_links_reconciled=True, award_totals_reconciled_without_double_counting=True,
                      deterministic_reaggregation=True, query_checkpoints=len(proposed),
                      semantic_review=False, identity_accuracy_verified=False)
    write(stage/'validation_report.json', validation)
    timing = dict(started_at_utc=provenance['started_at_utc'], completed_at_utc=now(),
                  elapsed_wall_seconds=(datetime.datetime.fromisoformat(now())-datetime.datetime.fromisoformat(provenance['started_at_utc'])).total_seconds())
    write(stage/'timing_report.json', timing)
    for file in [HERE/'match_cleaned_names_snapshot.py', HERE/'match_cleaned_names_snapshot.R', HERE/'test_match_cleaned_names_snapshot.py', HERE/'checkpoint_storage.py']:
        destination = stage/'code_snapshot'/file.name
        destination.parent.mkdir(exist_ok=True)
        shutil.copy2(file, destination)
    summary = report['offline_cleanup']
    findings = f'''# Provisional offline OpenAlex snapshot matches

Completed offline matching for 15,981 cleaned-name entries; the complete crosswalk accounts for 18,909 raw names, including all preserved prior groups.

Only clear names with a unique official canonical/alternative-name match across all snapshot types can receive a provisional ID. Education type, non-system identity, and absence of existing date/evidence conditions are required. Fuzzy matches, acronym aliases, and external crosswalks supply candidates only. Word order and campus qualifiers are preserved in normalization.

| Offline result | Entries |
| --- | ---: |
''' + ''.join(f'| {status} | {count:,} |\n' for status,count in sorted(summary['status_counts'].items())) + f'''
Distinct provisional IDs: {summary['distinct_provisional_ids']:,}. Fully matched award references: {summary['fully_provisional_matched_award_references']:,}; partially matched award references: {summary['partially_provisional_matched_award_references']:,}. Candidate coverage overlaps these provisional matches and is reported separately in coverage_report.json. Award references are source/award positions, not unique scientists.

The 1,379 OA research decisions, 100 naming research pilot decisions, 1,449 retained matches, and existing conditional mappings remain unchanged. Prior IDs absent from this older snapshot are reported separately, without replacement.

Validation passed for unchanged raw identifiers/names, complete disjoint coverage, preserved decisions and checksums, new ID membership in the snapshot, deterministic assignments, and award-level reconciliation. No semantic review occurred; identity accuracy is unverified. No web/API calls, training-location geocoding, or county-outcome updates were performed.

Files: raw_entry_crosswalk.jsonl (all groups), offline_raw_entry_crosswalk.jsonl, proposed_name_matches.jsonl, ranked_alternatives.jsonl, coverage_report.json, validation_report.json, snapshot_manifest.json, timing_report.json, individual query checkpoints, and preserved prior decisions.
'''
    (stage/'findings.md').write_text(findings, encoding='utf-8')
    storage.describe_new_checkpoints(stage,['query_checkpoints'])
    outputs = {str(path.relative_to(stage)):digest(path) for path in sorted(stage.rglob('*')) if path.is_file()}
    write(stage/'completion_manifest.json', dict(status='complete', completed_at_utc=now(),
          source_checksums=checks, output_checksums=outputs, semantic_review=False, identity_accuracy_verified=False))
    stage.rename(output)
    return dict(status='complete', output=str(output), coverage=summary, elapsed_seconds=timing['elapsed_wall_seconds'])


def validate_outputs(rows, cleanup, inventory, proposed, index):
    if len(rows) != len(cleanup) or len({r['record_id'] for r in rows}) != len(rows):
        raise ValueError('Missing/duplicated offline result')
    for row, old in zip(rows, cleanup):
        original = inventory[row['record_id']]
        if row['cleanup_decision'] != old or row['institution_as_written'] != original['institution_as_written'] or row['record_id'] != old['record_id']:
            raise ValueError('Raw identity or cleanup decision changed')
        if len(row['components']) != len(old['standard_names']):
            raise ValueError('Missing co-named institution component')
        for component in row['components']:
            query = proposed[component['proposed_name']]
            if index.exact_result(component['proposed_name'])['exact_matches'] != query['exact_matches']:
                raise ValueError('Exact match differs from official snapshot')
            match = component['accepted_provisional_match']
            if match:
                if old['status'] != 'clear' or original.get('prior_conditions') or query['exact_status'] != 'eligible_unique_exact' or match != query['exact_matches'][0]:
                    raise ValueError('Unsupported automatic acceptance')
                if not oid(match['openalex_id']) or match['openalex_id'] not in index.records:
                    raise ValueError('Invalid accepted snapshot ID')
            candidate_ids = [c['openalex_id'] for c in component['candidates']]
            if len(candidate_ids)>5 or len(set(candidate_ids))!=len(candidate_ids) or any(i not in index.records for i in candidate_ids):
                raise ValueError('Invalid distinct-ID candidate set')


if __name__ == '__main__':
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument('--data-root', type=Path, default=Path(os.environ.get('TALENT_DETS_DATA_DIR', str(Path.home()/'Globtalent Dropbox/gtl_talent_dets'))))
    cli.add_argument('--allocation-root', type=Path, default=Path(os.environ.get('GTALLOCATION_DATA_DIR', str(Path.home()/'Globtalent Dropbox/GTAllocation'))))
    cli.add_argument('--output', type=Path)
    cli.add_argument('--rscript', type=Path, default=Path(os.environ.get('RSCRIPT_PATH','C:/Program Files/R/R-4.5.2/bin/Rscript.exe')))
    args = cli.parse_args()
    output = args.output or args.data_root/'output/amws/openalex_snapshot_matches_20261011'
    print(json.dumps(build(args.data_root/'output/amws', args.allocation_root, output, args.rscript), indent=2))

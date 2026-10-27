"""Versioned offline cleaned/processed-name matching with provisional fallback."""
import argparse
import collections
import copy
import csv
import datetime
import json
import os
from pathlib import Path
import re
import shutil
import time

import match_cleaned_names_snapshot as base
import checkpoint_storage as storage

HERE = Path(__file__).resolve().parent
VERSION = 'snapshot_name_v2_processed'


def process_name(name):
    delimiter = re.search(r'[,\(]', name)
    if delimiter is None:
        return dict(processed_name=name, removed_suffix='', processing_cut_position=None)
    return dict(processed_name=name[:delimiter.start()].strip(), removed_suffix=name[delimiter.start():],
                processing_cut_position=delimiter.start())


def empty_result(name):
    return dict(proposed_name=name, normalized_query='', exact_status='empty_processed_name',
                exact_matches=[], candidates=[], query_id=None)


def merge_candidates(cleaned, processed):
    """Merge top-five lists by ID; preserve the version-specific winning aliases."""
    by_id = collections.defaultdict(list)
    for version, query in [('cleaned', cleaned), ('processed', processed)]:
        for candidate in query['candidates']:
            by_id[candidate['openalex_id']].append(dict(candidate, query_version=version,
                                                       query_text=query['proposed_name']))
    source_priority = {'canonical':0, 'alternative':1, 'acronym':2}
    def order(candidate):
        return (-candidate['score'], source_priority.get(candidate['alias_source'],3),
                0 if candidate['query_version']=='cleaned' else 1,
                candidate['matched_alias'], candidate['alias_source'])
    merged = []
    for entity, matches in by_id.items():
        winner = min(matches, key=order)
        row = {k:v for k,v in winner.items() if k not in ('query_version','query_text')}
        row.update(best_query_version=winner['query_version'], best_query_text=winner['query_text'],
                   contributing_query_versions=sorted({m['query_version'] for m in matches}),
                   query_version_scores={m['query_version']:m['score'] for m in matches},
                   query_matches=sorted(matches,key=lambda m:m['query_version']))
        merged.append(row)
    return sorted(merged,key=lambda c:(-c['score'],source_priority.get(c['alias_source'],3),c['openalex_id']))[:5]


def proposal(name, queries):
    processing = process_name(name)
    processed_name = processing['processed_name']
    cleaned = queries[name]
    processed = queries[processed_name] if base.normalize(processed_name) else empty_result(processed_name)
    if cleaned['exact_matches']:
        selected_version = 'cleaned' if cleaned['exact_status']=='eligible_unique_exact' else None
        effective_status = cleaned['exact_status']
    else:
        selected_version = 'processed' if processed['exact_status']=='eligible_unique_exact' else None
        effective_status = processed['exact_status']
    return dict(proposed_name=name, cleaned_name=name, **processing,
                cleaned_exact_result={k:cleaned[k] for k in ('query_id','normalized_query','exact_status','exact_matches')},
                processed_exact_result={k:processed[k] for k in ('query_id','normalized_query','exact_status','exact_matches')},
                candidates=merge_candidates(cleaned,processed), eligible_matching_version=selected_version,
                effective_exact_status=effective_status,
                flags=['empty_processed_name'] if not base.normalize(processed_name) else [],
                method='full_cleaned_priority_then_processed_unique_official_exact',
                identity_accuracy_verified=False)


def match_raw(cleanup, original, proposed):
    components = []
    conditions = original.get('prior_conditions') or []
    for name in cleanup['standard_names']:
        query = proposed[name]
        version = query['eligible_matching_version'] if cleanup['status']=='clear' and not conditions else None
        match = query[version+'_exact_result']['exact_matches'][0] if version else None
        flags = list(query['flags'])
        if version=='processed':
            flags.append('qualifier_removed')
        components.append(dict(proposed_name=name, cleaned_name=name,
                    processed_name=query['processed_name'], removed_suffix=query['removed_suffix'],
                    processing_cut_position=query['processing_cut_position'],
                    cleaned_exact_result=query['cleaned_exact_result'], processed_exact_result=query['processed_exact_result'],
                    exact_status=query['effective_exact_status'], accepted_provisional_match=match,
                    matching_version=version, candidates=query['candidates'], flags=flags,
                    method='official_unique_exact_'+version if version else 'candidate_only',
                    provisional_status='provisional_exact' if version else 'candidate_only'))
    accepted = [c['accepted_provisional_match'] for c in components if c['accepted_provisional_match']]
    if conditions:
        status = 'conditional_preserved'
    elif not components:
        status = 'cleanup_'+cleanup['status']
    elif len(accepted)==len(components):
        status = 'fully_provisional_matched'
    elif accepted:
        status = 'partially_provisional_matched'
    elif any(c['exact_status']=='ambiguous' for c in components):
        status = 'ambiguous'
    elif any(c['candidates'] for c in components):
        status = 'candidate_only'
    else:
        status = 'no_candidate'
    return dict(record_id=original['record_id'], institution_as_written=original['institution_as_written'],
                origin='offline_cleanup', cleanup_status=cleanup['status'], cleanup_decision=cleanup,
                award_count=original['award_count'], proposed_names=cleanup['standard_names'],
                processed_names=[c['processed_name'] for c in components], components=components,
                accepted_provisional_ids=sorted({m['openalex_id'] for m in accepted}), status=status,
                flags=sorted({f for c in components for f in c['flags']}),
                prior_conditions=conditions, prior_accepted_ids=original.get('prior_accepted_ids',[]),
                rule_version=VERSION, identity_accuracy_verified=False)


def validate_rows(rows, previous, cleanup, inventory, proposed, index):
    if len(rows)!=len(cleanup) or len({r['record_id'] for r in rows})!=len(rows):
        raise ValueError('Missing/duplicate cleanup rows')
    for row, original_cleanup in zip(rows,cleanup):
        key = row['record_id']; original = inventory[key]; old = previous[key]
        if row['institution_as_written']!=original['institution_as_written'] or row['cleanup_decision']!=original_cleanup:
            raise ValueError('Raw name or cleanup decision changed')
        if row!=match_raw(original_cleanup,original,proposed):
            raise ValueError('Raw result does not reproduce policy')
        if not set(old['accepted_provisional_ids'])<=set(row['accepted_provisional_ids']):
            raise ValueError('Previous accepted ID lost')
        actual_ids = set()
        for i,component in enumerate(row['components']):
            name = component['proposed_name']; processed = process_name(name)['processed_name']
            for version,text in [('cleaned',name),('processed',processed)]:
                expected = index.exact_result(text) if base.normalize(text) else empty_result(text)
                actual = component[version+'_exact_result']
                if actual['exact_status']!=expected['exact_status'] or actual['exact_matches']!=expected['exact_matches']:
                    raise ValueError('Exact result differs from snapshot')
            match = component['accepted_provisional_match']; version = component['matching_version']
            if match:
                if original_cleanup['status']!='clear' or original.get('prior_conditions') or version not in ('cleaned','processed'):
                    raise ValueError('Unsupported automatic acceptance')
                selected = component[version+'_exact_result']
                if selected['exact_status']!='eligible_unique_exact' or selected['exact_matches']!=[match]:
                    raise ValueError('Accepted ID is not unique education non-system exact match')
                if version=='processed' and (component['cleaned_exact_result']['exact_matches'] or 'qualifier_removed' not in component['flags']):
                    raise ValueError('Full-name precedence or qualifier flag violated')
                entity = match['openalex_id']
                if not base.oid(entity) or entity not in index.records:
                    raise ValueError('Accepted ID absent/invalid')
                actual_ids.add(entity)
            old_match = old['components'][i]['accepted_provisional_match']
            if old_match and (match!=old_match or version!='cleaned'):
                raise ValueError('Previously accepted component changed')
            candidates = component['candidates']; ids=[c['openalex_id'] for c in candidates]
            if len(ids)>5 or len(ids)!=len(set(ids)) or any(i not in index.records for i in ids):
                raise ValueError('Invalid top-five distinct-ID candidates')
            for candidate in candidates:
                if candidate['score']!=max(candidate['query_version_scores'].values()):
                    raise ValueError('Candidate does not retain its highest score')
        if actual_ids!=set(row['accepted_provisional_ids']):
            raise ValueError('Component and row IDs do not reconcile')


def row_ids(row):
    if row['origin']=='offline_cleanup':
        return set(row['accepted_provisional_ids'])
    decision = row['prior_decision']
    if row['origin']=='retained_unconditional_preserved':
        return {base.oid(i) for i in decision['accepted_ids']}
    if row['origin']=='oa_research_preserved' and decision['status']=='resolved':
        return {base.oid(m['openalex_id']) for m in decision['matches']}
    return set()


def reconcile_coverage(old_rows, new_rows, inventory, ledger, people_file, stage):
    old_by_name = {r['institution_as_written']:r for r in old_rows}
    new_by_name = {r['institution_as_written']:r for r in new_rows}
    if set(old_by_name)!=set(new_by_name) or len(new_rows)!=len(old_rows) or len(new_by_name)!=len(new_rows):
        raise ValueError('Full raw inventory coverage changed')
    old_ids = {n:row_ids(r) for n,r in old_by_name.items()}
    new_ids = {n:row_ids(r) for n,r in new_by_name.items()}
    for name in old_ids:
        if not old_ids[name]<=new_ids[name]:
            raise ValueError('Existing ID lost in full inventory')
    people = base.read(people_file) if people_file.suffix=='.json' else None
    if people is None:
        with people_file.open(encoding='utf-8-sig',newline='') as source:
            people = list(csv.DictReader(source))
    person_info = {r['source_key']:r['edition'] for r in people}
    if len(person_info)!=len(people):
        raise ValueError('Duplicated person entry')
    person_old = collections.defaultdict(set); person_new = collections.defaultdict(set)
    refs = collections.defaultdict(set); award_old=set(); award_new=set(); fully_old=set(); fully_new=set()
    with ledger.open(encoding='utf-8-sig',newline='') as source:
        for award in csv.DictReader(source):
            if award['requested_family'] not in ('ba_bs','ma','ms','phd') or not award['family_award_index'] or award['honorary']!='False' or not award['institution_as_written']:
                continue
            name=award['institution_as_written']; key=award['source_key']; ref=(key,str(award['award_index']))
            if name not in new_by_name or key not in person_info or ref in refs[name]:
                raise ValueError('Unexpected/duplicate ledger identity')
            refs[name].add(ref); person_old[key].update(old_ids[name]); person_new[key].update(new_ids[name])
            if old_ids[name]:award_old.add(ref)
            if new_ids[name]:award_new.add(ref)
            # Full-award coverage is meaningful for newly matched cleanup names.
            if old_by_name[name]['origin']=='offline_cleanup' and old_by_name[name]['status']=='fully_provisional_matched':fully_old.add(ref)
            if new_by_name[name]['origin']=='offline_cleanup' and new_by_name[name]['status']=='fully_provisional_matched':fully_new.add(ref)
    if set(refs)!=set(new_by_name):
        raise ValueError('Ledger does not cover full raw inventory')
    for name,positions in refs.items():
        if len(positions)!=new_by_name[name]['award_count'] or len(positions)!=old_by_name[name]['award_count']:
            raise ValueError('Ledger award counts differ')
    for original in inventory.values():
        positions={(a['source_key'],str(a['award_index'])) for a in original['award_refs']}
        if positions!=refs[original['institution_as_written']]:
            raise ValueError('Inventory award links differ')
    before_ids=set().union(*old_ids.values()); after_ids=set().union(*new_ids.values())
    previous_people={k for k in person_info if person_old[k]}; current_people={k for k in person_info if person_new[k]}
    if not previous_people<=current_people or not award_old<=award_new:
        raise ValueError('Person or award coverage decreased')
    report=dict(institution_entries_with_ID_before=sum(bool(ids) for ids in old_ids.values()),
                institution_entries_with_ID_after=sum(bool(ids) for ids in new_ids.values()),
                additional_institution_strings_with_any_ID=sum(not old_ids[n] and bool(new_ids[n]) for n in new_ids),
                institution_strings_gaining_distinct_IDs=sum(bool(new_ids[n]-old_ids[n]) for n in new_ids),
                distinct_IDs_before=len(before_ids), distinct_IDs_after=len(after_ids), additional_distinct_IDs=len(after_ids-before_ids),
                award_references_with_ID_before=len(award_old), award_references_with_ID_after=len(award_new),
                additional_award_references_with_any_ID=len(award_new-award_old),
                cleanup_fully_matched_award_references_before=len(fully_old), cleanup_fully_matched_award_references_after=len(fully_new),
                additional_fully_matched_cleanup_award_references=len(fully_new-fully_old),
                AMWS_total_source_entries=len(person_info), AMWS_entries_with_ID_before=len(previous_people),
                AMWS_entries_with_ID_after=len(current_people), additional_AMWS_entries_with_any_ID=len(current_people-previous_people),
                AMWS_entries_gaining_distinct_IDs=sum(bool(person_new[k]-person_old[k]) for k in person_info),
                AMWS_entries_with_multiple_IDs_before=sum(len(person_old[k])>1 for k in person_info),
                AMWS_entries_with_multiple_IDs_after=sum(len(person_new[k])>1 for k in person_info),
                by_edition={ed:dict(total=sum(v==ed for v in person_info.values()),
                           before=sum(person_info[k]==ed for k in previous_people), after=sum(person_info[k]==ed for k in current_people))
                           for ed in sorted(set(person_info.values()))},
                total_institution_bearing_award_references=sum(len(p) for p in refs.values()),
                counting='Unique source_key per edition; unique source_key/award_index; distinct IDs; no cross-edition person linkage; conditional mappings excluded')
    with (stage/'person_id_coverage.csv').open('w',encoding='utf-8',newline='') as target:
        writer=csv.DictWriter(target,fieldnames=['source_key','edition','IDs_before','IDs_after','additional_ID_count'])
        writer.writeheader()
        for key,edition in person_info.items():
            writer.writerow(dict(source_key=key,edition=edition,IDs_before=json.dumps(sorted(person_old[key])),
                                 IDs_after=json.dumps(sorted(person_new[key])),additional_ID_count=len(person_new[key]-person_old[key])))
    return report


def checkpoint(path, row):
    if path.exists():
        if base.read(path)!=row:
            raise ValueError('Existing checkpoint disagrees with reproducible result')
    else:
        base.write(path,row)


def verify_release(path, manifest):
    base.verify_checksums(manifest['source_checksums'])
    base.verify_checksums({str(path/name):sha for name,sha in manifest['output_checksums'].items()})
    storage.verify_release(path)


def publish(stage, output, manifest):
    stage=Path(stage).resolve(); output=Path(output).resolve()
    if stage.parent!=output.parent or stage==output:
        raise ValueError('Publication paths must be distinct siblings')
    verify_release(stage,manifest)
    mode='rename'
    if not output.exists():
        for attempt in range(5):
            try:
                stage.rename(output)
                break
            except PermissionError:
                if attempt<4:time.sleep(.1*2**attempt)
        else:
            mode='verified_copy'
    else:
        mode='verified_copy_resume'
    if stage.exists():
        output.mkdir(exist_ok=True)
        # Copy only validated files; refuse conflicting existing destinations.
        for relative,sha in manifest['output_checksums'].items():
            source=stage/relative; destination=output/relative
            if destination.exists():
                if base.digest(destination)!=sha:raise ValueError('Conflicting publication destination: '+relative)
            else:
                destination.parent.mkdir(parents=True,exist_ok=True)
                shutil.copy2(source,destination)
    verify_release(output,manifest)
    published=dict(manifest,published_at_utc=base.now(),publication_method=mode)
    base.write(output/'completion_manifest.json',published)
    return mode


def build(root, allocation, output, rscript):
    root=Path(root); allocation=Path(allocation); output=Path(output)
    previous=root/'openalex_snapshot_matches_20261011'
    previous_manifest=base.read(previous/'completion_manifest.json')
    verify_release(previous,previous_manifest)
    if (output/'completion_manifest.json').exists():
        verify_release(output,base.read(output/'completion_manifest.json'))
        return dict(status='already_complete',coverage_change=base.read(output/'coverage_change_report.json'))
    stage=output.with_name('.'+output.name+'.staging')
    if (stage/'ready_to_publish.json').exists():
        manifest=base.read(stage/'ready_to_publish.json')
        mode=publish(stage,output,manifest)
        return dict(status='complete',publication_method=mode,coverage_change=base.read(output/'coverage_change_report.json'))
    if output.exists():raise ValueError('Incomplete destination has no validated staging manifest')
    stage.mkdir(parents=True,exist_ok=True)
    offline=root/'institution_names_offline_20261011'
    research=root/'openalex_id_research_20261011'
    education=root/'education_enriched_20261011_updated_final'
    source_checks=dict(previous_manifest['source_checksums'])
    source_checks.update({str(previous/name):sha for name,sha in previous_manifest['output_checksums'].items()})
    files=[previous/'completion_manifest.json',education/'education_records.csv',
           HERE/'match_cleaned_names_snapshot_v2.py',HERE/'test_match_cleaned_names_snapshot_v2.py',HERE/'checkpoint_storage.py']
    source_checks.update({str(file):base.digest(file) for file in files})
    base.verify_checksums(source_checks)
    metadata=dict(version=VERSION,started_at_utc=base.now(),source_checksums=source_checks,
                  previous_release=str(previous),processing='Keep prefix before first comma or opening parenthesis; unchanged names unchanged',
                  acceptance='Full cleaned-name official exact matches block fallback; clear-only unique education non-system processed fallback; prior conditions block acceptance',
                  qualifier_removed_flags_required=True,network_access=False,semantic_review=False,identity_accuracy_verified=False)
    manifest_file=stage/'snapshot_manifest.json'
    if manifest_file.exists():
        saved=base.read(manifest_file)
        if saved['source_checksums']!=source_checks or saved['version']!=VERSION:raise ValueError('Staging inputs/code changed')
        metadata=saved
    else:base.write(manifest_file,metadata)
    previous_rows=base.jsonl(previous/'raw_entry_crosswalk.jsonl')
    previous_by_id={r['record_id']:r for r in previous_rows}
    cleanup=base.read(offline/'results.json'); inputs=base.jsonl(research/'inventory.jsonl')
    inventory={r['record_id']:r for r in inputs}
    if len(previous_rows)!=18909 or len(cleanup)!=15981 or len(inventory)!=17460:
        raise ValueError('Unexpected inventory counts')
    snapshot=allocation/'Data/external/oa_snapshot/data/institutions'
    crosswalks=[allocation/'Data/prep/crosswalks'/n for n in ('master_alias_oa.csv','OA_revelio_shanghai.csv')]
    records,aliases,rejected=base.load_snapshot(snapshot,crosswalks)
    if len(records)!=120658:raise ValueError('Snapshot count changed')
    index=base.SnapshotIndex(records,aliases)
    names={n for row in cleanup for n in row['standard_names']}
    query_names=names|{process_name(n)['processed_name'] for n in names if base.normalize(process_name(n)['processed_name'])}
    queries,pair_count=base.score_queries(index,query_names,stage,rscript)
    proposed={n:proposal(n,queries) for n in sorted(names)}
    for name,row in proposed.items():
        key=queries[name]['query_id']; checkpoint(storage.checkpoint_dir(stage,'proposed_name_checkpoints')/(key+'.json'),row)
    raw_rows=[match_raw(row,inventory[row['record_id']],proposed) for row in cleanup]
    validate_rows(raw_rows,previous_by_id,cleanup,inventory,proposed,index)
    for row in raw_rows:checkpoint(storage.checkpoint_dir(stage,'raw_entry_checkpoints')/(row['record_id']+'.json'),row)
    raw_by_id={r['record_id']:r for r in raw_rows}
    current=[raw_by_id[r['record_id']] if r['origin']=='offline_cleanup' else copy.deepcopy(r) for r in previous_rows]
    for old,new in zip(previous_rows,current):
        if old['record_id']!=new['record_id'] or old['institution_as_written']!=new['institution_as_written']:
            raise ValueError('Full inventory identifiers/order/raw names changed')
        if old['origin']!='offline_cleanup' and old!=new:raise ValueError('Prior researched decision changed')
    base.write_jsonl(stage/'proposed_name_matches.jsonl',list(proposed.values()))
    base.write_jsonl(stage/'raw_entry_crosswalk.jsonl',current)
    base.write_jsonl(stage/'offline_raw_entry_crosswalk.jsonl',raw_rows)
    base.write_jsonl(stage/'ranked_alternatives.jsonl',[dict(proposed_name=n,processed_name=q['processed_name'],candidates=q['candidates']) for n,q in proposed.items()])
    base.write(stage/'rejected_external_leads.json',rejected)
    for file in (previous/'preserved').rglob('*'):
        if file.is_file():
            destination=stage/'preserved'/file.relative_to(previous/'preserved')
            destination.parent.mkdir(parents=True,exist_ok=True); shutil.copy2(file,destination)
            if base.digest(file)!=base.digest(destination):raise ValueError('Preserved prior copy differs')
    change=reconcile_coverage(previous_rows,current,inventory,Path(inputs[0]['award_evidence_file']),education/'education_records.csv',stage)
    report=dict(offline_cleanup=base.coverage(raw_rows,inventory),full_inventory_entries=len(current),
                preserved_prior_group_counts=dict(collections.Counter(r['origin'] for r in current if r['origin']!='offline_cleanup')),
                distinct_cleaned_names=len(names),changed_proposed_names=sum(q['removed_suffix']!='' for q in proposed.values()),
                distinct_query_texts=len(query_names),scored_alias_pairs=pair_count,
                processed_fallback_components=sum(c['matching_version']=='processed' for r in raw_rows for c in r['components']),
                entries_using_processed_fallback=sum(any(c['matching_version']=='processed' for c in r['components']) for r in raw_rows),
                cleaned_exact_statuses=dict(collections.Counter(q['cleaned_exact_result']['exact_status'] for q in proposed.values())),
                processed_exact_statuses=dict(collections.Counter(q['processed_exact_result']['exact_status'] for q in proposed.values())),
                candidate_provenance='Contributing query versions are those whose individual top-five list contains the ID',
                semantic_review=False,identity_accuracy_verified=False,training_geocoding=False)
    if len({r['record_id'] for r in current})!=18909 or len({r['institution_as_written'] for r in current})!=18909:
        raise ValueError('Duplicate full raw entries')
    validate_rows(raw_rows,previous_by_id,cleanup,inventory,proposed,index)
    for row in raw_rows:
        if base.read(storage.checkpoint_dir(stage,'raw_entry_checkpoints')/(row['record_id']+'.json'))!=row:raise ValueError('Checkpoint recovery mismatch')
    base.verify_checksums(source_checks)
    base.write(stage/'coverage_report.json',report); base.write(stage/'coverage_change_report.json',change)
    validation=dict(passed=True,validated_at_utc=base.now(),full_raw_entries=18909,offline_cleanup_entries=15981,
                    source_checksums_verified=len(source_checks),full_inventory_order_identifiers_names_preserved=True,
                    prior_decisions_and_accepted_components_unchanged=True,conditional_mappings_preserved=True,
                    all_new_IDs_satisfy_processed_fallback_policy=True,new_IDs_verified_against_snapshot=True,
                    qualifier_removed_flags_complete=True,candidate_ID_deduplication_and_highest_scores_verified=True,
                    deterministic_checkpoints_and_reaggregation=True,award_and_person_coverage_reconciled=True,
                    coverage_does_not_decrease=True,semantic_review=False,identity_accuracy_verified=False)
    base.write(stage/'validation_report.json',validation)
    for file in [Path(__file__),HERE/'test_match_cleaned_names_snapshot_v2.py',HERE/'match_cleaned_names_snapshot.py',HERE/'match_cleaned_names_snapshot.R',HERE/'checkpoint_storage.py']:
        target=stage/'code_snapshot'/file.name; target.parent.mkdir(exist_ok=True); shutil.copy2(file,target)
    completed=base.now(); timing=dict(started_at_utc=metadata['started_at_utc'],validated_at_utc=completed,
             matching_and_validation_wall_seconds=(datetime.datetime.fromisoformat(completed)-datetime.datetime.fromisoformat(metadata['started_at_utc'])).total_seconds())
    base.write(stage/'timing_report.json',timing)
    findings=f'''# Cleaned and processed-name OpenAlex matching — provisional, unreviewed

The original cleaned names remain unchanged. Processed names retain only the prefix before the first comma or opening parenthesis. The full cleaned name takes priority: any official exact match blocks processed-name fallback. Only clear entries without existing conditions can receive a processed unique education/non-system ID; each such acceptance is flagged qualifier_removed. No historical campus is established by this procedure.

| Coverage measure | Previous | Processed release | Added |
| --- | ---: | ---: | ---: |
| Raw institution strings with any accepted/provisional ID | {change['institution_entries_with_ID_before']:,} | {change['institution_entries_with_ID_after']:,} | {change['additional_institution_strings_with_any_ID']:,} |
| Distinct accepted/provisional IDs, including preserved prior groups | {change['distinct_IDs_before']:,} | {change['distinct_IDs_after']:,} | {change['additional_distinct_IDs']:,} |
| Award references with at least one ID | {change['award_references_with_ID_before']:,} | {change['award_references_with_ID_after']:,} | {change['additional_award_references_with_any_ID']:,} |
| AMWS source entries with at least one ID | {change['AMWS_entries_with_ID_before']:,} | {change['AMWS_entries_with_ID_after']:,} | {change['additional_AMWS_entries_with_any_ID']:,} |

The raw crosswalk contains all 18,909 institution strings; 15,981 have offline cleanup decisions. {report['processed_fallback_components']:,} components use processed-name exact fallback. Candidate coverage is separate from provisional acceptance. The 1,379 OA-researched entries, 100 researched naming-pilot entries, 1,449 retained matches, prior accepted components, and conditional mappings remain unchanged.

AMWS coverage counts each source entry once within its edition, not unique people across editions. Award coverage counts each source_key/award_index once, including a partial co-institution match only once. Conditional mappings are excluded from unconditional/provisional coverage. Detailed status, edition, full/partial, and additional-distinct-ID counts appear in coverage reports and person_id_coverage.csv.

Validation passed for protected inputs, raw identifiers/names/order, full-name precedence, snapshot ID membership, processed acceptance flags, distinct-ID candidate aggregation, reproducible checkpoints, preserved prior decisions, and award/person reconciliation. No semantic review, web/API calls, training geocoding, or county outcome updates occurred. All new assignments remain provisional and identity accuracy is unverified.

Main files: raw_entry_crosswalk.jsonl, offline_raw_entry_crosswalk.jsonl, proposed_name_matches.jsonl, ranked_alternatives.jsonl, coverage_report.json, coverage_change_report.json, validation_report.json, snapshot_manifest.json, timing_report.json, person_id_coverage.csv, and per-name/per-entry checkpoints.
'''
    (stage/'findings.md').write_text(findings,encoding='utf-8')
    storage.describe_new_checkpoints(stage,['query_checkpoints','proposed_name_checkpoints','raw_entry_checkpoints'])
    output_checks={str(file.relative_to(stage)):base.digest(file) for file in sorted(stage.rglob('*')) if file.is_file()}
    manifest=dict(status='complete',validated_at_utc=completed,source_checksums=source_checks,
                  output_checksums=output_checks,version=VERSION,semantic_review=False,identity_accuracy_verified=False)
    base.write(stage/'ready_to_publish.json',manifest)
    mode=publish(stage,output,manifest)
    return dict(status='complete',output=str(output),publication_method=mode,coverage_change=change,
                offline_status_counts=report['offline_cleanup']['status_counts'],elapsed_seconds=timing['matching_and_validation_wall_seconds'])


if __name__=='__main__':
    cli=argparse.ArgumentParser(description=__doc__)
    cli.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',str(Path.home()/'Globtalent Dropbox/gtl_talent_dets'))))
    cli.add_argument('--allocation-root',type=Path,default=Path(os.environ.get('GTALLOCATION_DATA_DIR',str(Path.home()/'Globtalent Dropbox/GTAllocation'))))
    cli.add_argument('--output',type=Path)
    cli.add_argument('--rscript',type=Path,default=Path(os.environ.get('RSCRIPT_PATH','C:/Program Files/R/R-4.5.2/bin/Rscript.exe')))
    args=cli.parse_args(); root=args.data_root/'output/amws'
    print(json.dumps(build(root,args.allocation_root,args.output or root/'openalex_snapshot_matches_20261012_processed',args.rscript),indent=2))

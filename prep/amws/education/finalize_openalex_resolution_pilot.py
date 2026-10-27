"""Review-gated aggregation/publication for the 50-name resolution pilot."""
import argparse
import collections
import csv
import datetime
import json
from pathlib import Path
import shutil
import time

import openalex_resolution_pilot as pilot
import checkpoint_storage as storage
import match_cleaned_names_snapshot as base
import match_cleaned_names_snapshot_v2 as v2


def evaluated_ids(decision, award):
    ids=pilot.condition_ids(decision.get('matches',[]),award)
    if decision['status']=='conditional' and len(ids)>1:
        return set(),True
    return ids,False


def reviewed(original, decision, review):
    assert review['record_id']==original['record_id']
    assert review['institution_as_written']==original['institution_as_written']
    assert review['verdict'] in ('confirmed','corrected','withheld') and review['reason']
    result=dict(decision)
    if review['verdict']!='confirmed':
        replacement=review['replacement_decision']
        assert replacement['record_id']==original['record_id'] and replacement['institution_as_written']==original['institution_as_written']
        result=replacement
    return dict(result,original_agent_decision=decision,coordinator_review=review)


def write_csv(path, rows, fields):
    with Path(path).open('w',encoding='utf-8',newline='') as f:
        writer=csv.DictWriter(f,fieldnames=fields);writer.writeheader();writer.writerows(rows)


def coverage(rows, current, ledger, originals):
    by_name={r['institution_as_written']:r for r in rows}
    before={r['source_key']:set(json.loads(r['IDs_after'])) for r in current}
    after={k:set(v) for k,v in before.items()}
    group_people=collections.defaultdict(set);group_awards=collections.defaultdict(set)
    award_rows=[];all_institution_people=set();old_awards=set();new_awards=set();seen=set();counters=collections.Counter()
    old_by_name={r['institution_as_written']:v2.row_ids(r) for r in originals}
    for a in ledger:
        if not pilot.qualifying(a):continue
        key=a['source_key'];ref=(key,a['award_index']);name=a['institution_as_written']
        assert ref not in seen;seen.add(ref);all_institution_people.add(key)
        old=old_by_name[name]
        if old:old_awards.add(ref);new_awards.add(ref)
        if name not in by_name:continue
        result=by_name[name]
        # Full bounded context was prepared for every selected award.
        context=next_context[(key,a['award_index'])]
        assert all(context[k]==v for k,v in a.items()), 'Local award evidence differs from frozen source'
        ids,conflict=evaluated_ids(result,context)
        after[key].update(ids)
        if ids:
            new_awards.add(ref);group_people[result['group']].add(key);group_awards[result['group']].add(ref)
            counters['matched_sample_awards']+=1
        if conflict:counters['conflicting_sample_awards']+=1
        award_rows.append(dict(record_id=result['record_id'],institution_as_written=name,group=result['group'],source_key=key,
                 edition=a['edition'],award_index=a['award_index'],requested_family=a['requested_family'],year=a['year'],flags=a['flags'],
                 evidence=a['evidence'],education_context=context['education_context'],IDs_before=json.dumps(sorted(old)),
                 applied_IDs=json.dumps(sorted(ids)),IDs_after=json.dumps(sorted(old|ids)),
                 application_status='conflicting_conditions' if conflict else 'condition_satisfied' if ids and result['status']=='conditional'
                                    else 'reviewed_provisional' if ids else 'no_supported_ID',
                 decision_status=result['status']))
    before_people={k for k,v in before.items() if v};after_people={k for k,v in after.items() if v}
    assert before_people<=after_people and old_awards<=new_awards
    assert len(before)==156139 and len(before_people)==120682 and len(all_institution_people)==144455 and len(old_awards)==241639
    per_group={g:dict(matched_awards=len(group_awards[g]),people_receiving_assignment=len(group_people[g]),
                      newly_covered_people=len(group_people[g]-before_people)) for g in ('conditional','likely_exact','web_research')}
    # Groups may cover the same person. The overall delta is a union, never the sum of group deltas.
    report=dict(total_sample=50,status_counts=dict(collections.Counter(r['status'] for r in rows)),
                by_group={g:dict(status_counts=dict(collections.Counter(r['status'] for r in rows if r['group']==g)),**per_group[g]) for g in per_group},
                AMWS_total_entries=len(before),institution_bearing_entries=len(all_institution_people),
                AMWS_entries_with_ID_before=len(before_people),AMWS_entries_with_ID_after=len(after_people),
                additional_AMWS_entries_with_any_ID=len(after_people-before_people),
                AMWS_entries_gaining_distinct_IDs=sum(bool(after[k]-before[k]) for k in before),
                award_references_with_ID_before=len(old_awards),award_references_with_ID_after=len(new_awards),
                additional_award_references_with_any_ID=len(new_awards-old_awards),
                selected_awards=len(award_rows),**counters,
                sample_names_with_applied_ID=len({r['record_id'] for r in award_rows if json.loads(r['applied_IDs'])}),
                sample_accepted_distinct_IDs=len({i for r in award_rows for i in json.loads(r['applied_IDs'])}),
                institution_bearing_entries_without_ID_after=len(all_institution_people-after_people),
                conditional_counting='Conditions evaluated at each award; raw mappings remain conditional',
                counting='Unique source_key within edition; people are not deduplicated across editions',
                identity_accuracy='Stratified feasibility pilot; coverage does not establish identity accuracy')
    people_rows=[dict(source_key=r['source_key'],edition=r['edition'],IDs_before=json.dumps(sorted(before[r['source_key']])),
                     IDs_after=json.dumps(sorted(after[r['source_key']])),additional_ID_count=len(after[r['source_key']]-before[r['source_key']])) for r in current]
    return report,award_rows,people_rows


def finalize(data,local):
    started=time.perf_counter();root=data/'output/amws';output=root/pilot.NAME;stage=root/('.'+pilot.NAME+'.staging')
    if (output/'completion_manifest.json').exists():
        v2.verify_release(output,pilot.read(output/'completion_manifest.json'))
        print(json.dumps(dict(already_complete=True,coverage=pilot.read(output/'coverage_report.json'))));return
    manifest=pilot.read(local/'sample_manifest.json');records=pilot.read(local/'snapshot_records.json')
    base.verify_checksums(manifest['source_checksums'])
    reviews=pilot.read(local/'review/review_results.json')
    review_index={r['record_id']:r for r in reviews};assert len(review_index)==len(reviews)==50
    originals=[];results=[];batch_times=[]
    global next_context
    next_context={}
    for b in manifest['batches']:
        assert storage.raw_digest(b['input_file'])==b['input_sha256']
        inputs=pilot.read(b['input_file']);folder=Path(b['input_file']).parent
        batch_times.append(pilot.read(folder/'timing_report.json'))
        for original in inputs:
            raw=pilot.read(original['result_file']);pilot.validate_decision(raw,original,records)
            result=reviewed(original,raw,review_index[original['record_id']]);pilot.validate_decision(result,original,records)
            # Every match is unique and any compound acceptance explicitly accounts for all components.
            ids=[pilot.rules.valid_id(m['openalex_id']) for m in result.get('matches',[])]
            assert len(ids)==len(set(ids))
            if len(ids)>1 and result['status']=='resolved':assert result.get('explicit_co_named_institutions')
            originals.append(original);results.append(result)
            for a in pilot.read(original['full_awards_file']):next_context[(a['source_key'],a['award_index'])]=a
    assert len(results)==50 and len({r['record_id'] for r in results})==50
    assert manifest['selection']==[dict(record_id=r['record_id'],institution_as_written=r['institution_as_written'],group=r['group']) for r in originals]
    previous=root/'openalex_snapshot_matches_20261012_processed';education=root/'education_enriched_20261011_updated_final'
    with (previous/'person_id_coverage.csv').open(encoding='utf-8-sig',newline='') as f:current=list(csv.DictReader(f))
    with (education/'education_awards.csv').open(encoding='utf-8-sig',newline='') as f:ledger=list(csv.DictReader(f))
    all_originals=base.jsonl(previous/'raw_entry_crosswalk.jsonl')
    report,awards,people=coverage(results,current,ledger,all_originals)
    parser=pilot.load_parser(education)
    wanted_contexts={a['source_key']:a['education_context'] for a in next_context.values()}
    checked=set()
    with (education/'source_biographies.csv').open(encoding='utf-8-sig',newline='') as source:
        for bio in csv.DictReader(source):
            if bio['source_key'] not in wanted_contexts:continue
            region,flags=parser.bounds(bio['raw_text_adjusted'],int(bio['edition']))
            expected=bio['raw_text_adjusted'][region[0]:region[1]] if region else ''
            assert wanted_contexts[bio['source_key']]==expected
            checked.add(bio['source_key'])
    assert checked==set(wanted_contexts)
    # Deterministic replay checks apply the same decisions without a second research pass.
    again=coverage(results,current,ledger,all_originals)
    assert (report,awards,people)==again
    stage.mkdir(exist_ok=True)
    pilot.write(stage/'sample_manifest.json',manifest)
    pilot.write(stage/'input_batches.json',originals)
    pilot.jsonl(stage/'results.jsonl',results)
    pilot.jsonl(stage/'raw_entry_crosswalk.jsonl',[dict(r,prior_decision=original['prior_row']) for r,original in zip(results,originals)])
    pilot.write(stage/'review_report.json',dict(total_reviewed=50,manual_decisions_reviewed=35,
                verdicts=dict(collections.Counter(r['verdict'] for r in reviews)),reviews=reviews))
    pilot.write(stage/'coverage_report.json',report)
    write_csv(stage/'award_id_ledger.csv',awards,list(awards[0]))
    write_csv(stage/'person_id_coverage.csv',people,list(people[0]))
    test_report=pilot.read(local/'tests/test_report.json');assert test_report['passed']
    validation=dict(passed=True,all_50_reviewed=True,unchanged_identifiers_names=True,immutable_inputs_verified=True,
              snapshot_IDs_verified=True,prior_decisions_preserved=True,conditional_scope_preserved=True,
              no_duplicate_awards=True,no_duplicate_people=True,reproducible_aggregation=True,
              full_checkpoint_coverage=True,bounded_education_contexts_reproduced=True,tests=test_report,web_policy='No OpenAlex API calls; opened web identity required for new web acceptances',
              blocked_lookups_retried=True,remaining_blocked=report['status_counts'].get('access_blocked',0))
    pilot.write(stage/'validation_report.json',validation)
    start_info=pilot.read(local/'started.json');review_timing=pilot.read(local/'review/timing_report.json')
    timings=dict(started_at_utc=start_info['started_at_utc'],finished_at_utc=base.now(),preparation=start_info,
             total_elapsed_seconds=(datetime.datetime.now(datetime.timezone.utc)-datetime.datetime.fromisoformat(start_info['started_at_utc'])).total_seconds()+start_info['prepare_seconds'],
             batches=batch_times,coordinator_review=review_timing,aggregation_seconds=time.perf_counter()-started,
             timing_note='Overlapping agent wall times are not summed as elapsed runtime; shared overhead reported separately')
    pilot.write(stage/'timing_report.json',timings)
    assessment=pilot.read(local/'review/assessment_report.json')
    pilot.write(stage/'assessment_report.json',assessment)
    blocked=report['status_counts'].get('access_blocked',0)
    text=(f"# OpenAlex ID resolution pilot\n\n50 random names: 15 conditional, 15 likely exact, 20 web; seed {pilot.SEED}.\n\n"
          f"Statuses: {json.dumps(report['status_counts'])}.\n\n"
          f"Newly covered AMWS entries: {report['additional_AMWS_entries_with_any_ID']:,}; total coverage: {report['AMWS_entries_with_ID_after']:,} / 156,139. "
          f"Additional award positions covered: {report['additional_award_references_with_any_ID']:,}.\n\n"
          f"{blocked} web cases remain blocked after retry. No OpenAlex API calls were made. "
          "Coverage includes earlier provisional IDs; these results do not establish overall identity accuracy. "
          "The stratified sample is unsuitable for directly projecting overall recovery rates.\n\n"
          "Checkpoints remain local with one ZIP backup. Previous releases, education data, geolocation mappings, county outcomes, and the paused OA research release are unchanged. "
          "The pilot stops here.\n")
    (stage/'findings.md').write_text(text,encoding='utf-8')
    for p in (Path(pilot.__file__),Path(__file__),Path(pilot.__file__).parent/'test_openalex_resolution_pilot.py'):
        target=stage/'code_snapshot'/p.name;target.parent.mkdir(exist_ok=True);shutil.copy2(p,target)
    # One archive of the local working tree; register both paths so publication can be verified after rename.
    storage.register_directory(stage/'working',local);storage.register_directory(output/'working',local)
    storage.describe_new_checkpoints(stage,['working'])
    base.verify_checksums(manifest['source_checksums'])
    outputs={str(p.relative_to(stage)):storage.raw_digest(p) for p in sorted(stage.rglob('*')) if p.is_file() and p.name!='completion_manifest.json'}
    final_sources=dict(manifest['source_checksums'])
    for p in (Path(__file__),Path(pilot.__file__).parent/'test_openalex_resolution_pilot.py'):
        final_sources[str(p)]=storage.raw_digest(p)
    completion=dict(status='pilot_complete',all_documented=True,blocked_lookups=blocked,validated_at_utc=base.now(),
                   source_checksums=final_sources,output_checksums=outputs,coverage=report,
                   continuation_authorized=False)
    v2.publish(stage,output,completion)
    print(json.dumps(dict(output=str(output),coverage=report,timing=timings),ensure_ascii=True))


def main():
    ap=argparse.ArgumentParser();ap.add_argument('--data-root',type=Path,default=pilot.DEFAULT_DATA)
    ap.add_argument('--local',type=Path,default=storage.storage_root()/pilot.NAME);args=ap.parse_args()
    finalize(args.data_root,args.local)


if __name__=='__main__':main()

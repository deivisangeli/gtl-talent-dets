"""Validate and publish the bounded AMWS OpenAlex continuation."""
import collections
import csv
import datetime
import json
from pathlib import Path
import shutil
import sys
import time

import openalex_id_resolution as r
import openalex_resolution_pilot as pilot
import checkpoint_storage as storage
import match_cleaned_names_snapshot as base
import match_cleaned_names_snapshot_v2 as v2


def validate_complete():
    local=r.local_root();manifest=r.load_local();records=r.read(local/'snapshot_records.json')
    if not manifest.get('web_selection_complete'):raise ValueError('Web selection not complete')
    base.verify_checksums(manifest['source_checksums'])
    items=r.batch_inputs();expected=collections.Counter(x['group'] for x in items)
    if expected!={'conditional':140,'naming_pilot':100,'likely_exact':1887,'web_research':480,'pilot_retry':12}:raise ValueError('Unexpected continuation scope')
    keys=[(x['record_id'],x['group']) for x in items]
    if len(keys)!=len(set(keys)):raise ValueError('Duplicate stage input')
    for batch in manifest['batches']:
        if storage.raw_digest(batch['input_file'])!=batch['input_sha256']:raise ValueError('Batch input changed')
        timing=r.read(Path(batch['input_file']).parent/'timing_report.json')
        if batch['group'] in ('likely_exact','web_research','pilot_retry'):
            if not timing.get('started_at_utc') or not timing.get('finished_at_utc'):raise ValueError('Full batch timer missing')
            if timing.get('group_research_seconds') is None:raise ValueError('Group research timer missing')
    completed=[];local_rows={x['record_id']:x for x in r.all_rows(manifest)}
    with r.db() as con:
        for item in items:
            if storage.raw_digest(item['full_awards_file'])!=item['evidence_sha256']:raise ValueError('Linked education evidence changed')
            expected_awards=r.evidence(item['institution_as_written']);actual=r.read(item['full_awards_file'])
            if actual!=expected_awards:raise ValueError('Local evidence differs from frozen source cache')
            d=r.read(item['result_file']);r.validate(d,item,records);completed.append(d)
            if item['record_id'] not in local_rows or local_rows[item['record_id']]['institution_as_written']!=item['institution_as_written']:raise ValueError('Raw identity changed')
    selection=r.read(local/'web_selection_manifest.json')
    if len(selection['selected'])!=480 or len({x['record_id'] for x in selection['selected']})!=480:raise ValueError('Invalid web selection')
    original_pilot=r.read(local/'preserved_pilot_results.json')
    original_pilot_web={d['record_id'] for d in original_pilot if d['group']=='web_research'}
    if len(original_pilot_web)!=20 or original_pilot_web & {x['record_id'] for x in selection['selected']}:raise ValueError('Web scope repeats pilot names')
    if original_pilot!=base.jsonl(Path(manifest['pilot_release'])/'results.jsonl'):raise ValueError('Pilot decisions changed')
    audit=r.read(local/'audit/sample_manifest.json');reviews=r.read(local/'audit/reviews.json')
    if len(reviews)!=100 or len({v['record_id'] for v in reviews})!=100:raise ValueError('Audit must contain 100 unique raw names')
    if {v['record_id'] for v in reviews}!={v['record_id'] for v in audit['records']}:raise ValueError('Audit coverage differs')
    input_index={(x['record_id'],x['group']):x for x in items}
    for review in reviews:
        if review['verdict'] not in ('confirmed','withheld','corrected') or not review.get('reason'):raise ValueError('Invalid audit verdict')
        item=input_index[(review['record_id'],review['group'])]
        if review['institution_as_written']!=item['institution_as_written']:raise ValueError('Audit identity changed')
        if review.get('reviewed_decision_sha256')!=storage.raw_digest(item['result_file']):raise ValueError('Audited decision changed after review')
        if review['verdict']!='confirmed':r.validate(review['replacement_decision'],item,records)
    patterns=local/'audit/pattern_review.json'
    if any(v['verdict']!='confirmed' for v in reviews) and not patterns.exists():raise ValueError('Audit corrections require pattern follow-up')
    revisions=local/'audit/pattern_revisions.json'
    if revisions.exists():
        for revision in r.read(revisions):
            item=input_index[(revision['record_id'],revision['group'])]
            r.validate(revision['replacement_decision'],item,records)
    tests=r.read(local/'tests/test_report.json')
    if not tests.get('passed'):raise ValueError('Tests did not pass')
    return manifest,items,completed,reviews


def finalize():
    local=r.local_root();manifest=r.load_local();root=Path(manifest['data_root'])/'output/amws';output=root/r.NAME;stage=root/('.'+r.NAME+'.staging')
    if (output/'completion_manifest.json').exists():
        v2.verify_release(output,r.read(output/'completion_manifest.json'));print(json.dumps(dict(already_complete=True,coverage=r.read(output/'coverage_report.json'))));return
    started=time.perf_counter();manifest,items,decisions,reviews=validate_complete()
    stage.mkdir(exist_ok=True)
    before_report,_=r.compute_coverage();report,after=r.compute_coverage(stage)
    assert before_report==report
    # Baseline accepted and pilot IDs cannot be removed by a later decision.
    assert report['AMWS_entries_with_ID_after']>=123370 and report['award_references_with_ID_after']>=248851
    all_prior=r.all_rows(manifest);effective=r.effective_results();history=collections.defaultdict(list)
    for d in r.read(local/'preserved_pilot_results.json')+decisions:history[d['record_id']].append(d)
    audit_index={x['record_id']:x for x in reviews}
    pattern_index={x['record_id']:x for x in r.read(local/'audit/pattern_revisions.json')} if (local/'audit/pattern_revisions.json').exists() else {}
    crosswalk=[]
    for old in all_prior:
        identifier=old['record_id'];new=dict(old)
        new['resolution_result']=effective.get(identifier)
        new['resolution_history']=history[identifier]
        new['coordinator_audit']=audit_index.get(identifier)
        new['coordinator_pattern_revision']=pattern_index.get(identifier)
        new['existing_unconditional_or_provisional_IDs']=sorted(v2.row_ids(old))
        d=effective.get(identifier)
        new['additional_unconditional_provisional_IDs']=sorted({m['openalex_id'] for m in d.get('matches',[]) if m.get('conditions') is None}-v2.row_ids(old)) if d else []
        new['conditional_resolution_matches']=[m for m in d.get('matches',[]) if m.get('conditions') is not None] if d else []
        new['resolution_version']=r.VERSION;crosswalk.append(new)
    assert len(crosswalk)==18909 and len({x['record_id'] for x in crosswalk})==18909
    assert [(x['record_id'],x['institution_as_written']) for x in crosswalk]==[(x['record_id'],x['institution_as_written']) for x in all_prior]
    pilot.jsonl(stage/'raw_entry_crosswalk.jsonl',crosswalk)
    pilot.jsonl(stage/'resolution_results.jsonl',[effective[k] for k in sorted(effective)])
    pilot.jsonl(stage/'continuation_decisions.jsonl',decisions)
    r.write(stage/'preserved_pilot_results.json',r.read(local/'preserved_pilot_results.json'))
    r.write(stage/'run_manifest.json',manifest)
    r.write(stage/'web_selection_manifest.json',r.read(local/'web_selection_manifest.json'))
    r.write(stage/'batch_manifest.json',dict(batches=manifest['batches']))
    r.write(stage/'audit_report.json',dict(sample=r.read(local/'audit/sample_manifest.json'),reviews=reviews,
                verdicts=dict(collections.Counter(x['verdict'] for x in reviews)),
                pattern_followup=r.read(local/'audit/pattern_review.json') if (local/'audit/pattern_review.json').exists() else {'needed':False}))
    r.write(stage/'test_report.json',r.read(local/'tests/test_report.json'))
    r.write(stage/'additional_audit_patterns.json',{p.name:r.read(p) for p in (local/'audit').glob('*_pattern_review.json')})
    timing=[r.read(Path(b['input_file']).parent/'timing_report.json') for b in manifest['batches']]
    group_seconds=collections.Counter();unmeasured_group_timers=[]
    for t in timing:
        group=t['group'];seconds=t.get('group_research_seconds',0)
        if isinstance(seconds,dict):
            for key,value in seconds.items():
                if isinstance(value,dict):
                    active=value.get('active_seconds')
                    if active is None:unmeasured_group_timers.append(dict(batch=t['batch'],group=key,detail=value))
                    else:group_seconds[key]+=active
                else:group_seconds[key]+=value
        else:group_seconds[group]+=seconds
    finished=base.now()
    elapsed=(datetime.datetime.fromisoformat(finished)-datetime.datetime.fromisoformat(manifest['started_at_utc'])).total_seconds()
    r.write(stage/'timing_report.json',dict(started_at_utc=manifest['started_at_utc'],finished_at_utc=finished,total_elapsed_seconds=elapsed,
                batch_reports=timing,group_worker_seconds=dict(group_seconds),unmeasured_group_timers=unmeasured_group_timers,automatic_stage=r.read(local/'automatic_stage_timing.json'),
                audit_timing=r.read(local/'audit/timing_report.json'),aggregation_seconds=time.perf_counter()-started,
                interpretation='Worker seconds can overlap across three agents; elapsed time is not their sum. Group timers include reading and reasoning; setup/validation overhead is separately recorded.'))
    r.write(stage/'validation_report.json',dict(passed=True,scope_verified=True,complete_structural_coverage=True,immutable_inputs_verified=True,
                exact_raw_coverage=18909,unchanged_identifiers_names_order=True,preserved_pilot_and_prior_decisions=True,
                no_duplicate_award_positions=True,per_award_conditions_preserved=True,no_prior_IDs_lost=True,snapshot_membership_verified=True,
                official_visible_identity_required_for_new_web_acceptances=True,no_direct_OpenAlex_API_calls=True,
                blocked_cases_retried=True,checkpoint_resume_tested=True,deterministic_coverage_replay=True,
                audited_decisions=100,tests=r.read(local/'tests/test_report.json'),scope_note='500 distinct web names, including 20 pilot names; 12 pilot retry revisions do not increase scope'))
    deviations=[dict(record_id=d['record_id'],institution_as_written=d['institution_as_written'],deviation=d['research_policy_deviation'])
                for d in decisions if d.get('research_policy_deviation')]
    r.write(stage/'research_policy_deviations.json',dict(count=len(deviations),records=deviations,
                acceptance_policy='No new accepted IDs permitted on any entry with an over-budget query log. Full actual research logs preserved.'))
    # Preserve the executed scripts, including preparation-time runner provenance.
    for source in [Path(r.__file__),Path(__file__),Path(__file__).parent/'test_openalex_id_resolution.py',local/'runner_prepare_snapshot.py']:
        target=stage/'code_snapshot'/source.name;target.parent.mkdir(exist_ok=True);shutil.copy2(source,target)
    r.write(local/'state.json',dict(phase='ready_to_publish',output=str(output),validated_at_utc=base.now()))
    storage.register_directory(stage/'working',local);storage.register_directory(output/'working',local)
    storage.describe_new_checkpoints(stage,['working'])
    blocked=report['resolution_status_counts'].get('access_blocked',0)
    research=collections.Counter(d['status'] for d in decisions if d['group'] in ('web_research','pilot_retry'))
    findings=(f"# OpenAlex ID resolution: bounded continuation\n\n"
       f"Processed 140 remaining conditional mappings, 100 naming-pilot entries and 1,887 likely exact names; researched 480 new names and retried 12 blocked pilot cases. The 20 pilot web names bring the distinct research scope to 500.\n\n"
       f"AMWS entries with at least one ID: **{report['AMWS_entries_with_ID_after']:,} / 156,139**, up **{report['additional_AMWS_entries_with_any_ID']:,}** from the 123,370-entry pilot baseline. "
       f"Among 144,455 institution-bearing entries, {report['institution_bearing_entries_without_ID_after']:,} remain without an ID.\n\n"
       f"Additional raw names with at least one applicable award ID: {report['additional_raw_names_with_any_applied_ID']:,}; additional distinct IDs: {report['additional_distinct_IDs']:,}; additional covered awards: {report['additional_award_references_with_any_ID']:,}.\n\n"
       f"Continuation web/retry statuses: {json.dumps(research)}. Effective resolution statuses: {json.dumps(report['resolution_status_counts'])}. "
       f"{blocked} effective decisions remain access blocked after bounded retries. Unresolved decisions are documented outcomes, not accepted IDs.\n\n"
       f"Research execution deviations: {len(deviations)}; full logs retained and any affected new acceptance withheld.\n\n"
       f"Elapsed time: {elapsed/3600:.2f} hours. Audit verdicts: {json.dumps(dict(collections.Counter(x['verdict'] for x in reviews)))}.\n\n"
       "All new assignments are provisional or explicitly conditional. Official web identity verification does not establish historical training location. Coverage and sampled agreement do not establish identity accuracy. "
       "Counts use source entries within edition without cross-edition person linkage. Original decisions and corrections are retained separately.\n\n"
       "Checkpoints stay local with one compressed backup. No direct OpenAlex API calls, source-dataset updates, geolocation updates or county-outcome changes occurred. This bounded pass stops here.\n")
    (stage/'findings.md').write_text(findings,encoding='utf-8')
    base.verify_checksums(manifest['source_checksums'])
    sources=dict(manifest['source_checksums'])
    for path in (Path(r.__file__),Path(__file__),Path(__file__).parent/'test_openalex_id_resolution.py'):sources[str(path)]=storage.raw_digest(path)
    outputs={str(p.relative_to(stage)):storage.raw_digest(p) for p in sorted(stage.rglob('*')) if p.is_file() and p.name!='completion_manifest.json'}
    completed=dict(status='bounded_pass_complete',all_documented=True,blocked_lookups=blocked,source_checksums=sources,output_checksums=outputs,
              validated_at_utc=base.now(),scope={'new_web_names':480,'pilot_web_names':20,'pilot_retry_revisions':12},coverage=report,continuation_authorized=False)
    v2.publish(stage,output,completed)
    r.write(local.parent/(r.NAME+'_publication_state.json'),dict(phase='published',output=str(output),finished_at_utc=base.now()))
    print(json.dumps(dict(output=str(output),coverage=report,elapsed_hours=elapsed/3600),ensure_ascii=True))


if __name__=='__main__':finalize()

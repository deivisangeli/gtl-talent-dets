"""Structural-only preparation and aggregation of unreviewed offline name cleanup."""
import collections
import hashlib
import json
from pathlib import Path
import shutil

import offline_institution_name_pilot as p


def jsonl(path):
    return [json.loads(x) for x in Path(path).read_text(encoding='utf-8-sig').splitlines() if x.strip()]


def digest(path):
    h=hashlib.sha256()
    with p.storage.resolve(path).open('rb') as source:
        for block in iter(lambda:source.read(1048576),b''):h.update(block)
    return h.hexdigest()


def prepare(root, output):
    root=Path(root);output=Path(output)
    if output.exists():raise ValueError('Existing release must be resumed, not overwritten')
    old=root/'openalex_id_research_20261011'
    researched=root/'institution_name_pilot_20261011'
    pilot=root/'institution_name_offline_pilot_20261011'
    original=jsonl(old/'inventory.jsonl');oa=jsonl(old/'research_results.jsonl')
    web_ids=set(p.read(researched/'sample_manifest.json')['selected_ids'])
    pilot_rows=p.read(pilot/'results.json');pilot_ids={r['record_id'] for r in pilot_rows}
    oa_ids={r['record_id'] for r in oa}
    if oa_ids&web_ids or oa_ids&pilot_ids or web_ids&pilot_ids:raise ValueError('Excluded input groups overlap')
    pending=[r for r in original if r['record_id'] not in oa_ids|web_ids|pilot_ids]
    assert len(oa_ids)==1379 and len(web_ids)==100 and len(pilot_ids)==300 and len(pending)==15681
    output.mkdir(parents=True)
    p.storage.checkpoint_dir(output,'batches')
    immutable={};numbers=[];selected=[]
    for start in range(0,len(pending),100):
        number=len(numbers)+1;numbers.append(number)
        rows=[dict(record_id=r['record_id'],institution_as_written=r['institution_as_written']) for r in pending[start:start+100]]
        relative=f'batches/batch_{number:03d}/input.json';p.write(output/relative,rows)
        immutable[relative]=digest(output/relative);selected += [r['record_id'] for r in rows]
    assert len(numbers)==157 and len(rows)==81
    preserved_sources=[(pilot/'results.json','preserved/offline_pilot_results.json'),
                       (old/'research_results.jsonl','preserved/oa_research_results.jsonl'),
                       (researched/'results.json','preserved/researched_name_pilot_results.json'),
                       (old/'retained_unconditional_matches.json','preserved/retained_unconditional_matches.json')]
    for source,relative in preserved_sources:
        destination=output/relative;destination.parent.mkdir(exist_ok=True)
        shutil.copy2(source,destination);immutable[relative]=digest(destination)
        assert digest(source)==immutable[relative]
    excluded=dict(oa_researched_names=len(oa_ids),researched_name_pilot_names=len(web_ids),retained_unconditional_names=len(p.read(old/'retained_unconditional_matches.json')),imported_offline_pilot_names=len(pilot_ids))
    assert sum(excluded.values())+len(pending)==18909
    protected=dict(p.read(old/'source_manifest.json')['sources'])
    for folder,files in [(old,['inventory.jsonl','research_results.jsonl','consolidated_crosswalk.jsonl','worker_assignments.json','pause_checkpoint.json']),
                         (researched,['sample_manifest.json','results.json','coverage_report.json']),
                         (pilot,['sample_manifest.json','results.json','coverage_report.json','findings.md'])]:
        for name in files:protected[str(folder/name)]=digest(folder/name)
    p.write(output/'sample_manifest.json',dict(prepared_at_utc=p.now(),batch_numbers=numbers,batch_sizes=[100]*156+[81],
        selected_ids=selected,immutable_files=immutable,protected_files=protected,continuation_names=len(pending),
        final_offline_names=len(pending)+len(pilot_ids),all_raw_inventory_names=18909,accounted_prior_groups=excluded,
        input_order='original inventory order, excluding completed groups',reasoning_effort='medium',semantic_review=False,
        worker_input_fields=['record_id','institution_as_written'],network_access_allowed=False))
    p.write(output/'worker_assignments.json',dict(workers={},completed_batches=[],unassigned_batches=numbers,status='prepared'))
    return aggregate(output)


def validate_batch(run,number):
    folder=p.batch_folder(run,number);originals=p.inputs(run,number)
    files=list((folder/'results').glob('*.json'));keys=set()
    counts=collections.Counter()
    for path in files:
        row=p.read(path)
        if path.stem!=row['record_id'] or row['record_id'] in keys:raise ValueError('Duplicate/mismatched checkpoint identity')
        p.validate(row,originals[row['record_id']]);keys.add(row['record_id']);counts[row['status']]+=1
    if keys!=set(originals):raise ValueError('Batch has missing/extra records')
    timer=p.read(folder/'timing.json')
    if timer['status']!='complete':raise ValueError('Worker has not completed batch timer')
    return dict(batch=number,records=len(files),status_counts=dict(counts),elapsed_seconds=timer['elapsed_seconds'],structural_validation_passed=True,semantic_review=False)


def aggregate(run):
    run=Path(run);manifest=p.read(run/'sample_manifest.json')
    for relative,sha in manifest['immutable_files'].items():
        if digest(run/relative)!=sha:raise ValueError('Immutable release input changed')
    base=p.aggregate(run,result_filename='continuation_results.json',dedup_filename='continuation_proposed_names.json',report_filename='continuation_coverage_report.json')
    continuation=p.read(run/'continuation_results.json')
    pilot=p.read(run/'preserved/offline_pilot_results.json')
    merged=pilot+continuation
    if len({r['record_id'] for r in merged})!=len(merged):raise ValueError('Imported and new outputs overlap')
    names=collections.defaultdict(list)
    for row in merged:
        for name in row['standard_names']:names[name].append(row['record_id'])
    report=dict(continuation_completed=len(continuation),continuation_outstanding=base['outstanding'],
        imported_pilot_entries=len(pilot),offline_entries=len(merged),offline_target_entries=manifest['final_offline_names'],
        complete=base['complete'],continuation_status_counts=base['status_counts'],
        combined_status_counts=dict(collections.Counter(r['status'] for r in merged)),distinct_proposed_names=len(names),
        batches=base['batches'],accounted_prior_groups=manifest['accounted_prior_groups'],all_raw_inventory_names=18909,
        continuation_semantic_review=False,method='Unreviewed provisional raw-name-only cleanup; exact-name deduplication')
    p.write(run/'results.json',merged)
    proposed=[dict(standard_name=name,raw_record_ids=ids) for name,ids in sorted(names.items())]
    p.write(run/'proposed_names.json',proposed)
    p.write(run/'deduplicated_names.json',proposed)
    p.write(run/'coverage_report.json',report)
    return report


def dispatch_completed(run):
    """Coordinator-only: validate finished formats and reserve the next batch."""
    run=Path(run);assignments=p.read(run/'worker_assignments.json');dispatch=[]
    for worker,item in list(assignments['workers'].items()):
        if item['status']=='done':continue
        number=item['batch'];timer=p.batch_folder(run,number)/'timing.json'
        if not timer.exists() or p.read(timer).get('status')!='complete':continue
        result=validate_batch(run,number)
        p.write(p.batch_folder(run,number)/'structural_validation.json',result)
        assignments['completed_batches']=sorted(set(assignments['completed_batches']+[number]))
        if assignments['unassigned_batches']:
            new=assignments['unassigned_batches'].pop(0)
            assignments['workers'][worker]=dict(batch=new,status='assigned',reasoning_effort='medium',assigned_at_utc=p.now())
            dispatch.append(dict(worker=worker,completed_batch=number,next_batch=new,next_records=len(p.inputs(run,new))))
        else:
            item['status']='done'
    if len(assignments['completed_batches'])==len(p.read(run/'sample_manifest.json')['batch_numbers']):assignments['status']='complete'
    p.write(run/'worker_assignments.json',assignments)
    return dict(dispatch=dispatch,completed_batches=len(assignments['completed_batches']),unassigned_batches=len(assignments['unassigned_batches']),status=assignments['status'])


def verify_protected(run):
    manifest=p.read(Path(run)/'sample_manifest.json')
    for path,sha in manifest['protected_files'].items():
        if digest(path)!=sha:raise ValueError('Protected source changed: '+path)
    return list(manifest['protected_files'])


if __name__=='__main__':
    import argparse
    cli=argparse.ArgumentParser(description=__doc__)
    cli.add_argument('command',choices=['dispatch','aggregate','verify-protected'])
    cli.add_argument('--run',type=Path,required=True)
    args=cli.parse_args()
    if args.command=='dispatch':result=dispatch_completed(args.run)
    elif args.command=='verify-protected':result=dict(protected_files=len(verify_protected(args.run)),passed=True)
    else:
        result=aggregate(args.run);result={k:v for k,v in result.items() if k!='batches'}
    print(json.dumps(result,indent=2))

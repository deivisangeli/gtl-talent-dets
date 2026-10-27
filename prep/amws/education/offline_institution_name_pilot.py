"""Local checkpoints for provisional raw-name cleanup. No network or biography access."""
import collections
import datetime
import hashlib
import json
from pathlib import Path
import time
import checkpoint_storage as storage

STATUSES = {'clear', 'likely', 'unresolved', 'not_an_institution'}
RESULT_FIELDS = {'record_id', 'institution_as_written', 'standard_names', 'status', 'note'}


def now():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def read(path):
    return json.loads(storage.resolve(path).read_text(encoding='utf-8-sig'))


def write(path, obj):
    path=storage.resolve(path);path.parent.mkdir(parents=True,exist_ok=True)
    temporary=path.with_suffix(path.suffix+'.tmp')
    temporary.write_text(json.dumps(obj,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
    # Dropbox/Windows can briefly keep the previous destination open.
    for attempt in range(7):
        try:
            temporary.replace(path)
            break
        except PermissionError:
            if attempt==6:raise
            time.sleep(0.1*(2**attempt))


def batch_folder(run, number):
    return storage.resolve(Path(run)/'batches')/f'batch_{number:03d}'


def begin_batch(run, number):
    """Call before reading names; retain the original start across checkpoints."""
    folder=batch_folder(run,number)
    if not (folder/'input.json').exists():raise ValueError('Batch does not exist')
    target=folder/'timing.json'
    if not target.exists():write(target,dict(started_at_utc=now(),status='running'))
    return read(target)


def inputs(run, number):
    folder=batch_folder(run,number)
    manifest=read(Path(run)/'sample_manifest.json')
    relative=f'batches/batch_{number:03d}/input.json'
    if hashlib.sha256((folder/'input.json').read_bytes()).hexdigest()!=manifest['immutable_files'][relative]:
        raise ValueError('Immutable raw input changed')
    rows=read(folder/'input.json')
    if len({r['record_id'] for r in rows})!=len(rows):raise ValueError('Duplicate raw input')
    return {r['record_id']:r for r in rows}


def validate(result, original):
    if set(result)-RESULT_FIELDS:raise ValueError('Offline results contain unexpected research fields')
    if any(result.get(k)!=original[k] for k in ('record_id','institution_as_written')):
        raise ValueError('Raw input identity changed')
    if result.get('status') not in STATUSES:raise ValueError('Unknown cleanup status')
    names=result.get('standard_names')
    if not isinstance(names,list) or any(not isinstance(n,str) or not n.strip() for n in names):
        raise ValueError('Standard names must be a list of nonempty names')
    if len(names)!=len(set(names)):raise ValueError('Duplicate proposed name')
    if bool(names)!=(result['status'] in {'clear','likely'}):raise ValueError('Proposed names inconsistent with status')
    if result['status']!='clear' and not result.get('note'):raise ValueError('Uncertainty or rejection needs a short note')
    if 'http://' in json.dumps(result).lower() or 'https://' in json.dumps(result).lower():
        raise ValueError('No research URLs in offline cleanup output')
    return result


def save(run, number, result, revision_reason=None):
    folder=batch_folder(run,number)
    if not (folder/'timing.json').exists():raise ValueError('Begin batch timer before reading or saving names')
    original=inputs(run,number)[result['record_id']]
    validate(result,original)
    target=folder/'results'/(result['record_id']+'.json')
    if target.exists():
        old=read(target)
        if old==result:return old
        if not revision_reason:raise ValueError('Conflicting overwrite needs versioned correction')
        history=folder/'revisions'/result['record_id']
        n=len(list(history.glob('*.json')))+1
        write(history/f'{n:03d}.json',dict(original_decision=old,revision_reason=revision_reason,revised_at_utc=now()))
    write(target,result)
    return result


def finish_batch(run,number):
    folder=batch_folder(run,number);rows=inputs(run,number)
    saved={f.stem for f in (folder/'results').glob('*.json')}
    if saved!=set(rows):raise ValueError('Batch cannot finish with missing or extra results')
    for key in saved:validate(read(folder/'results'/(key+'.json')),rows[key])
    timer=read(folder/'timing.json')
    if 'completed_at_utc' not in timer:
        timer.update(completed_at_utc=now(),status='complete')
        timer['elapsed_seconds']=(datetime.datetime.fromisoformat(timer['completed_at_utc'])-datetime.datetime.fromisoformat(timer['started_at_utc'])).total_seconds()
        write(folder/'timing.json',timer)
    return timer


def aggregate(run, result_filename='results.json', dedup_filename='deduplicated_names.json', report_filename='coverage_report.json'):
    run=Path(run);manifest=read(run/'sample_manifest.json')
    all_results=[];pending=[];batch_reports=[];seen=set()
    for number in manifest['batch_numbers']:
        folder=batch_folder(run,number);originals=inputs(run,number)
        if seen&set(originals):raise ValueError('Batches overlap')
        seen.update(originals);saved=set()
        for path in sorted((folder/'results').glob('*.json')):
            row=read(path)
            if path.stem!=row['record_id'] or path.stem not in originals:raise ValueError('Unexpected output identity')
            validate(row,originals[path.stem]);saved.add(path.stem);all_results.append(row)
        pending+=sorted(set(originals)-saved)
        batch_reports.append(dict(batch=number,completed=len(saved),outstanding=len(originals)-len(saved)))
    if sorted(seen)!=sorted(manifest['selected_ids']):raise ValueError('Manifest coverage changed')
    catalog={}
    for row in all_results:
        for name in row['standard_names']:
            catalog.setdefault(name,[]).append(row['record_id'])
    report=dict(completed=len(all_results),outstanding=len(pending),complete=not pending,
                status_counts=dict(collections.Counter(r['status'] for r in all_results)),
                distinct_proposed_names=len(catalog),batches=batch_reports,offline_provisional=True)
    write(run/result_filename,all_results)
    write(run/dedup_filename,[dict(standard_name=name,raw_record_ids=ids) for name,ids in sorted(catalog.items())])
    write(run/report_filename,report);write(run/'outstanding_record_ids.json',pending)
    return report

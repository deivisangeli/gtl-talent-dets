"""Checkpoint and validate manual institution-name decisions; no network calls."""
import collections
import datetime
import hashlib
import json
from pathlib import Path
import sys

sys.path.insert(0, str(Path.home()/'.codex/skills/recover-openalex-institution-id/scripts'))
import research_batches as batches

STATUSES = {'resolved', 'conditional', 'ambiguous', 'invalid_extraction', 'access_blocked'}


def now():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def read(path):
    return batches.read_json(path)


def start(run, record_id):
    run = Path(run)
    assert record_id in {r['record_id'] for r in read(run/'input.json')}
    target = run/'timing_starts'/(record_id + '.json')
    if not target.exists():
        batches.atomic_json(target, {'started_at_utc': now()})
    return read(target)


def validate(result, original):
    if result['record_id'] != original['record_id'] or result['institution_as_written'] != original['institution_as_written']:
        raise ValueError('Literal input identity changed')
    if result['status'] not in STATUSES or not result.get('reason'):
        raise ValueError('Status and reason required')
    institutions = result.get('institutions', [])
    if bool(institutions) != (result['status'] in {'resolved', 'conditional'}):
        raise ValueError('Accepted names inconsistent with status')
    if not result.get('source_keys') or not set(result['source_keys']) <= {r['source_key'] for r in original['award_refs']}:
        raise ValueError('Decision needs valid linked evidence keys')
    if len(result.get('queries', [])) > 2 or len(result.get('opened_sources', [])) > 3:
        raise ValueError('Focused research budget exceeded')
    if any('openalex' in query.lower() for query in result.get('queries', [])):
        raise ValueError('No new OpenAlex searches in naming pilot')
    all_urls = result.get('source_urls', []) + result.get('opened_sources', [])
    if any('openalex.org' in url.lower() for url in all_urls):
        raise ValueError('No new OpenAlex source access in naming pilot')
    if any(key in json.dumps(result).lower() for key in ['openalex_id', 'accepted_ids']):
        raise ValueError('Naming results must not add OpenAlex IDs')
    names = []
    for institution in institutions:
        if not institution.get('standard_name') or not institution.get('identity_evidence'):
            raise ValueError('Institution identity evidence required')
        if institution.get('identity_type') not in {'continuing', 'historical_independent'}:
            raise ValueError('Institution identity type required')
        names.append(institution['standard_name'])
        if result['status'] == 'conditional':
            batches.validate_conditions(institution.get('conditions'))
        elif institution.get('conditions'):
            raise ValueError('Resolved names cannot have conditional scope')
    if len(names) != len(set(names)):
        raise ValueError('Duplicate accepted name')
    if institutions and not (result.get('source_urls') or result.get('reused_decision_ids')):
        raise ValueError('Identity support provenance required')
    return result


def save(run, result, revision_reason=None):
    run = Path(run)
    original = {r['record_id']: r for r in read(run/'input.json')}[result['record_id']]
    validate(result, original)
    target = run/'results'/(result['record_id'] + '.json')
    if target.exists():
        old = read(target)
        if all(old.get(k) == v for k, v in result.items()):
            return old
        if not revision_reason:
            raise ValueError('Conflicting overwrite requires versioned revision')
        folder = run/'revisions'/result['record_id']
        number = len(list(folder.glob('*.json'))) + 1
        batches.atomic_json(folder/f'{number:03d}.json', old)
        result['revision_reason'] = revision_reason
        for key in ('research_started_at_utc', 'research_completed_at_utc', 'elapsed_seconds'):
            result[key] = old[key]
        result['revised_at_utc'] = now()
    else:
        started = start(run, result['record_id'])['started_at_utc']
        finished = now()
        result.update(research_started_at_utc=started, research_completed_at_utc=finished,
                      elapsed_seconds=(datetime.datetime.fromisoformat(finished)-datetime.datetime.fromisoformat(started)).total_seconds())
    result['research_date'] = datetime.datetime.now(datetime.timezone.utc).date().isoformat()
    batches.atomic_json(target, result)
    return result


def aggregate(run):
    run = Path(run)
    manifest = read(run/'sample_manifest.json')
    for relative, digest in manifest['immutable_files'].items():
        if hashlib.sha256((run/relative).read_bytes()).hexdigest() != digest:
            raise ValueError('Immutable pilot input changed')
    originals = {r['record_id']: r for r in read(run/'input.json')}
    results = []
    for path in sorted((run/'results').glob('*.json')):
        result = read(path)
        if path.stem != result['record_id']:
            raise ValueError('Checkpoint identity mismatch')
        validate(result, originals[result['record_id']])
        results.append(result)
    pending = sorted(set(originals)-{r['record_id'] for r in results})
    catalog = {}
    for result in results:
        for institution in result.get('institutions', []):
            identity = (institution['standard_name'], institution['identity_type'])
            key = 'name-' + hashlib.sha256(json.dumps(identity,ensure_ascii=False).encode()).hexdigest()[:16]
            institution['local_identity_key'] = key
            row = catalog.setdefault(key,dict(local_identity_key=key,standard_name=identity[0],identity_type=identity[1],raw_record_ids=[],successor_names=[]))
            row['raw_record_ids'].append(result['record_id'])
            row['successor_names'] = sorted(set(row['successor_names'] + institution.get('successor_names', [])))
    report = dict(completed=len(results),outstanding=len(pending),complete=not pending,
                  status_counts=dict(collections.Counter(r['status'] for r in results)),
                  distinct_institutions=len(catalog),reused_identity_entries=sum(bool(r.get('reused_decision_ids')) for r in results),
                  reused_distinct_institutions=len({i['local_identity_key'] for r in results if r.get('reused_decision_ids') for i in r.get('institutions', [])}),
                  historical_independent_institutions=sum(r['identity_type']=='historical_independent' for r in catalog.values()),
                  no_new_openalex_lookups=True)
    batches.atomic_json(run/'results.json',results)
    batches.atomic_json(run/'institution_catalog.json',list(catalog.values()))
    batches.atomic_json(run/'coverage_report.json',report)
    batches.atomic_json(run/'outstanding_record_ids.json',pending)
    return report

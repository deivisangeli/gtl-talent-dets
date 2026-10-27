"""Bounded, evidence-reviewed institution pilot. Never makes network requests."""
import argparse
import collections
import csv
import datetime
import hashlib
import importlib.util
import json
from pathlib import Path
import random
import re
import shutil
import sys
import time
import zipfile

import checkpoint_storage as storage
import match_cleaned_names_snapshot as base
import match_cleaned_names_snapshot_v2 as v2

SKILL = Path.home()/'.codex/skills/recover-openalex-institution-id'
sys.path.insert(0, str(SKILL/'scripts'))
import research_batches as rules

DEFAULT_DATA = Path.home()/'Globtalent Dropbox/gtl_talent_dets'
NAME = 'openalex_id_resolution_pilot_20261012'
SEED = 20261012


def read(path):
    return json.loads(Path(path).read_text(encoding='utf-8-sig'))


def write(path, value):
    storage.write_json(path, value)


def immutable(path, value):
    if Path(path).exists():
        if read(path) != value:
            raise ValueError('Immutable checkpoint differs: '+str(path))
    else:
        write(path, value)


def jsonl(path, rows):
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    Path(path).write_text(''.join(json.dumps(r, ensure_ascii=False)+'\n' for r in rows), encoding='utf-8')


def normalize_conditions(value):
    if isinstance(value, dict):
        return {k:({'gte':'ge','lte':'le'}.get(v,v) if k=='op' else normalize_conditions(v)) for k,v in value.items()}
    if isinstance(value, list):
        return [normalize_conditions(v) for v in value]
    return value


def prior_matches(row):
    if row['origin']=='oa_research_preserved':
        return [dict(m, conditions=normalize_conditions(m['conditions'])) for m in row['prior_decision']['matches']]
    result=[]
    for c in row['prior_conditions']:
        for entity in c['accepted_ids']:
            result.append(dict(openalex_id=rules.valid_id(entity), conditions=normalize_conditions({k:v for k,v in c.items() if k in ('all','any')}),
                               source_urls=c['supporting_urls'], evidence=c['reason']))
    return result


def select(rows):
    conditional=[r for r in rows if r['status'] in ('conditional','conditional_preserved') and r['origin']!='name_research_pilot_preserved']
    likely=[r for r in rows if r.get('cleanup_status')=='likely' and not r.get('prior_conditions') and r['components']
            and all(c['exact_status']=='eligible_unique_exact' for c in r['components'])]
    excluded={r['record_id'] for r in conditional+likely}
    residual=[r for r in rows if r['record_id'] not in excluded and not v2.row_ids(r)
              and r['status'] in ('candidate_only','ambiguous','cleanup_unresolved','not_found')]
    pools={'conditional':conditional,'likely_exact':likely,'web_research':residual}
    rng=random.Random(SEED)
    picked={g:rng.sample(sorted(pool,key=lambda r:r['record_id']), n)
            for (g,pool),n in zip(pools.items(),(15,15,20))}
    batches=[]
    for i in range(3):
        web_start=7*i
        selected=[(g,r) for g in ('conditional','likely_exact') for r in picked[g][5*i:5*i+5]]
        selected += [('web_research',r) for r in picked['web_research'][web_start:web_start+(7 if i<2 else 6)]]
        batches.append(selected)
    assert len({r['record_id'] for batch in batches for g,r in batch})==50
    return batches,{g:len(pool) for g,pool in pools.items()}


def qualifying(a):
    return a['requested_family'] in ('ba_bs','ma','ms','phd') and a['family_award_index'] and a['honorary']=='False' and a['institution_as_written']


def load_parser(education):
    manifest=read(education/'manifest.json')
    path=education/manifest['frozen_parser_snapshot']
    assert storage.raw_digest(path)==manifest['parser']['parser_sha256']
    spec=importlib.util.spec_from_file_location('pilot_frozen_parser',path)
    parser=importlib.util.module_from_spec(spec);spec.loader.exec_module(parser)
    return parser


def prepare(data):
    started=time.perf_counter();root=data/'output/amws';previous=root/'openalex_snapshot_matches_20261012_processed'
    education=root/'education_enriched_20261011_updated_final'
    local=storage.storage_root()/NAME;local.mkdir(parents=True,exist_ok=True)
    provenance=read(previous/'snapshot_manifest.json')
    # Verify relevant frozen source bytes, including each snapshot part; old runner code is not re-executed.
    snapshot_files=[Path(p) for p in provenance['source_checksums'] if '/oa_snapshot/data/institutions/' in p.replace('\\','/')]
    sources=[previous/'raw_entry_crosswalk.jsonl',previous/'person_id_coverage.csv',previous/'snapshot_manifest.json',
             education/'education_awards.csv',education/'source_biographies.csv',education/'manifest.json',
             education/'frozen_education_regex.py',root/'education_geolocated/published_crosswalk.json',
             root/'openalex_id_research_20261011/pause_checkpoint.json',Path(__file__),SKILL/'scripts/research_batches.py']+snapshot_files
    checks={str(p):storage.raw_digest(p) for p in sources}
    for p in sources:
        expected=provenance['source_checksums'].get(str(p))
        if expected and checks[str(p)]!=expected:
            raise ValueError('Protected source changed: '+str(p))
    rows=base.jsonl(previous/'raw_entry_crosswalk.jsonl');batches,pool_sizes=select(rows)
    wanted={r['institution_as_written']:r for batch in batches for g,r in batch}
    awards=collections.defaultdict(list)
    with (education/'education_awards.csv').open(encoding='utf-8-sig',newline='') as f:
        for a in csv.DictReader(f):
            if qualifying(a) and a['institution_as_written'] in wanted:awards[a['institution_as_written']].append(a)
    needed={a['source_key'] for group in awards.values() for a in group}
    contexts={};parser=load_parser(education)
    with (education/'source_biographies.csv').open(encoding='utf-8-sig',newline='') as f:
        for bio in csv.DictReader(f):
            if bio['source_key'] not in needed:continue
            region,boundary_flags=parser.bounds(bio['raw_text_adjusted'],int(bio['edition']))
            contexts[bio['source_key']]=dict(education_context=bio['raw_text_adjusted'][region[0]:region[1]] if region else '',
                                             education_boundary_flags=boundary_flags)
    assert set(contexts)==needed
    records,aliases,rejected=base.load_snapshot(snapshot_files[0].parents[1],[])
    assert len(records)==120658
    inventory=[];batch_manifest=[]
    for number,batch in enumerate(batches,1):
        folder=local/f'batch_{number:03d}';inputs=[]
        for group,row in batch:
            linked=[dict(a,**contexts[a['source_key']]) for a in awards[row['institution_as_written']]]
            assert len(linked)==row['award_count']
            # Complete award evidence stays local. Inputs carry a varied representative preview.
            evidence_file=folder/'evidence'/f"{row['record_id']}.json"
            immutable(evidence_file,linked)
            preview=[];seen=set()
            for a in linked:
                bucket=(a['edition'],a['requested_family'],a['year'])
                if bucket not in seen:preview.append(a);seen.add(bucket)
                if len(preview)==12:break
            item=dict(record_id=row['record_id'],institution_as_written=row['institution_as_written'],group=group,
                      award_count=row['award_count'],prior_row=row,contexts=preview,full_awards_file=str(evidence_file),
                      result_file=str(folder/'results'/f"{row['record_id']}.json"))
            if group=='conditional':
                item['existing_matches']=prior_matches(row)
                for m in item['existing_matches']:m['canonical_name']=records[rules.valid_id(m['openalex_id'])]['canonical_name']
            inputs.append(item);inventory.append(item)
        immutable(folder/'input.json',inputs)
        batch_manifest.append(dict(batch=number,records=len(inputs),groups=dict(collections.Counter(r['group'] for r in inputs)),
                                   input_file=str(folder/'input.json'),input_sha256=storage.raw_digest(folder/'input.json')))
    immutable(local/'snapshot_records.json',records)
    # Official aliases allow workers to verify candidates locally, without API access.
    immutable(local/'snapshot_aliases.json',aliases)
    immutable(local/'sample_manifest.json',dict(seed=SEED,total=50,pool_sizes=pool_sizes,groups={'conditional':15,'likely_exact':15,'web_research':20},
              selection=[dict(record_id=r['record_id'],institution_as_written=r['institution_as_written'],group=r['group']) for r in inventory],
              batches=batch_manifest,source_checksums=checks,network_policy='web research only; no OpenAlex API calls',
              education_context='Frozen parser education region; no employment or birthplace inference'))
    if not (local/'started.json').exists():write(local/'started.json',dict(started_at_utc=base.now(),prepare_seconds=time.perf_counter()-started))
    print(json.dumps(dict(local=str(local),batches=batch_manifest,pool_sizes=pool_sizes),ensure_ascii=True))


def condition_ids(matches, award):
    selected=set()
    for m in matches:
        condition=m.get('conditions')
        if condition is None or rules.condition_applies(normalize_conditions(condition),award):selected.add(rules.valid_id(m['openalex_id']))
    return selected


def validate_decision(decision, original, records):
    assert decision['record_id']==original['record_id'] and decision['institution_as_written']==original['institution_as_written']
    assert decision['group']==original['group']
    assert decision['status'] in rules.STATUSES and decision.get('reason')
    datetime.date.fromisoformat(decision['research_date'])
    assert bool(decision.get('matches'))==(decision['status'] in ('resolved','conditional'))
    for m in decision.get('matches',[]):
        identifier=rules.valid_id(m['openalex_id']);record=records[identifier]
        assert record['canonical_name']==m['canonical_name'] and record['type']=='education' and not record['is_super_system']
        assert m.get('evidence') and m.get('source_urls')
        if decision['status']=='conditional':rules.validate_conditions(normalize_conditions(m['conditions']))
        else:assert m.get('conditions') is None
    if original['group']=='conditional':
        assert decision['status']=='conditional'
        assert decision['matches']==original['existing_matches']
    if original['group']=='web_research':
        log=decision['research_log']
        assert len(log.get('searches',[]))<=2
        assert len(log.get('opened_urls',[]))<=3
        assert not any('api.openalex.org' in url for url in log.get('opened_urls',[]))
        for m in decision.get('matches',[]):
            verification=m.get('official_page_verification',{})
            assert verification.get('identity_visible') is True
            assert verification.get('canonical_id')==rules.valid_id(m['openalex_id'])
            assert verification.get('url') in log['opened_urls']
            assert re.match(r'https://(?:explore\.)?openalex\.org/(?:institutions/)?I\d+',verification['url'])
        if decision['status']=='access_blocked':assert log.get('access_retry')
    return True


def save(local, batch, decision_file):
    inputs=read(local/f'batch_{batch:03d}/input.json');decision=read(decision_file)
    original=next(r for r in inputs if r['record_id']==decision['record_id'])
    validate_decision(decision,original,read(local/'snapshot_records.json'))
    immutable(Path(original['result_file']),decision)
    print(json.dumps(dict(saved=decision['record_id'],status=decision['status'])))


def main():
    ap=argparse.ArgumentParser();ap.add_argument('command',choices=['prepare','save']);ap.add_argument('--data-root',type=Path,default=DEFAULT_DATA)
    ap.add_argument('--local',type=Path,default=storage.storage_root()/NAME);ap.add_argument('--batch',type=int);ap.add_argument('--decision',type=Path)
    args=ap.parse_args()
    if args.command=='prepare':prepare(args.data_root)
    else:save(args.local,args.batch,args.decision)


if __name__=='__main__':main()

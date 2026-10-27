"""Continuation of the AMWS resolution pilot; network-free orchestration."""
import argparse
import collections
import copy
import csv
import datetime
import hashlib
import json
from pathlib import Path
import random
import re
import shutil
import sqlite3
import sys
import time

import openalex_resolution_pilot as pilot
import checkpoint_storage as storage
import match_cleaned_names_snapshot as base
import match_cleaned_names_snapshot_v2 as v2

NAME='openalex_id_resolution_20261012'
VERSION='resolution_v1'
STATUSES=pilot.rules.STATUSES
GROUPS=('conditional','naming_pilot','likely_exact','web_research','pilot_retry')


def local_root():return storage.storage_root()/NAME


def read(path):return pilot.read(path)


def write(path,value):pilot.write(path,value)


def immutable(path,value):pilot.immutable(path,value)


def load_local():return read(local_root()/'run_manifest.json')


def db():
    return sqlite3.connect((local_root()/'education.sqlite').resolve().as_uri()+'?mode=ro',uri=True)


def all_rows(manifest):return base.jsonl(Path(manifest['previous_release'])/'raw_entry_crosswalk.jsonl')


def evidence(name):
    with db() as con:
        return [dict(json.loads(a),education_context=c,education_boundary_flags=json.loads(f))
                for a,c,f in con.execute('SELECT a.data,b.context,b.flags FROM awards a JOIN biographies b USING(source_key) WHERE a.name=? ORDER BY a.position',(name,))]


def component_queries(row):
    out=[]
    for c in row.get('components',[]):
        version='cleaned' if c['cleaned_exact_result']['exact_matches'] else 'processed'
        exact=c[version+'_exact_result']
        out.append(dict(proposed_name=c['proposed_name'],processed_name=c['processed_name'],removed_suffix=c['removed_suffix'],
                   exact_status=exact['exact_status'],exact_matches=exact['exact_matches'],matching_version=version,
                   candidates=[{k:x[k] for k in ('openalex_id','canonical_name','institution_type','is_super_system','matched_alias','alias_source','score')} for x in c['candidates']]))
    return out


def batch_inputs():
    manifest=load_local()
    return [item for batch in manifest['batches'] for item in read(batch['input_file'])]


def preserved_matches(row):
    if row['origin']=='oa_research_preserved':return pilot.prior_matches(row)
    grouped=collections.defaultdict(list)
    for c in row['prior_conditions']:
        for entity in c['accepted_ids']:grouped[pilot.rules.valid_id(entity)].append(c)
    matches=[]
    for entity,conditions in grouped.items():
        groups=[pilot.normalize_conditions({k:v for k,v in c.items() if k in ('all','any')}) for c in conditions]
        urls=sorted({url for c in conditions for url in c.get('supporting_urls',[])})
        matches.append(dict(openalex_id=entity,conditions=groups[0] if len(groups)==1 else {'any':groups},
             source_urls=urls or ['local:preserved_published_crosswalk'],evidence=' | '.join(c['reason'] for c in conditions),
             provenance='Existing mapping preserved; duplicate same-ID scopes combined by logical OR.'))
    return matches


def make_batches(rows,group,start):
    local=local_root();batches=[];records=read(local/'snapshot_records.json')
    for offset in range(0,len(rows),100):
        number=start+len(batches);folder=local/'batches'/f'batch_{number:03d}';items=[]
        for row in rows[offset:offset+100]:
            linked=evidence(row['institution_as_written'])
            assert len(linked)==row['award_count']
            ep=folder/'evidence'/f"{row['record_id']}.json";immutable(ep,linked)
            preview=[];seen=set()
            for a in linked:
                bucket=(a['edition'],a['requested_family'],a['year'],a['education_context'])
                if bucket not in seen:preview.append(a);seen.add(bucket)
                if len(preview)==6:break
            item=dict(record_id=row['record_id'],institution_as_written=row['institution_as_written'],group=group,
                     prior_status=row['status'],cleanup_status=row.get('cleanup_status'),award_count=row['award_count'],
                     components=component_queries(row),contexts=preview,full_awards_file=str(ep),evidence_sha256=storage.raw_digest(ep),
                     result_file=str(folder/'results'/f"{row['record_id']}.json"))
            if group=='conditional':
                item['existing_matches']=preserved_matches(row)
                for m in item['existing_matches']:m['canonical_name']=records[pilot.rules.valid_id(m['openalex_id'])]['canonical_name']
            if group=='naming_pilot':item['prior_naming_decision']=row['prior_decision']
            if group=='pilot_retry':item['original_pilot_decision']=row['pilot_result']
            items.append(item)
        immutable(folder/'input.json',items)
        batches.append(dict(batch=number,group=group,records=len(items),input_file=str(folder/'input.json'),input_sha256=storage.raw_digest(folder/'input.json')))
    return batches


def prepare(data):
    local=local_root();root=data/'output/amws';previous=root/'openalex_snapshot_matches_20261012_processed';pr=root/pilot.NAME
    if (local/'run_manifest.json').exists():
        m=load_local();base.verify_checksums(m['source_checksums']);print(json.dumps(dict(resumed=True,batches=m['batches'])));return
    started=base.now();timer=time.perf_counter();local.mkdir(parents=True,exist_ok=True)
    v2.verify_release(pr,read(pr/'completion_manifest.json'))
    m=read(pr/'sample_manifest.json');checks=dict(m['source_checksums'])
    for file in sorted(pr.rglob('*')):
        if file.is_file():checks[str(file)]=storage.raw_digest(file)
    rows=base.jsonl(previous/'raw_entry_crosswalk.jsonl');pilot_results=base.jsonl(pr/'results.jsonl')
    done={r['record_id'] for r in pilot_results}
    conditional=[r for r in rows if r['record_id'] not in done and r['status'] in ('conditional','conditional_preserved') and r['origin']!='name_research_pilot_preserved']
    likely=[r for r in rows if r['record_id'] not in done and r.get('cleanup_status')=='likely' and not r.get('prior_conditions')
            and r['components'] and all(c['exact_status']=='eligible_unique_exact' for c in r['components'])]
    naming=[r for r in rows if r['origin']=='name_research_pilot_preserved']
    assert (len(conditional),len(likely),len(naming))==(140,1887,100)
    pilot_local=storage.storage_root()/pilot.NAME
    for filename in ('snapshot_records.json','snapshot_aliases.json'):
        target=local/filename;source=pilot_local/filename
        if target.exists():assert storage.raw_digest(target)==storage.raw_digest(source)
        else:shutil.copy2(source,target)
        checks[str(target)]=storage.raw_digest(target)
    education=root/'education_enriched_20261011_updated_final';parser=pilot.load_parser(education)
    database=local/'education.sqlite'
    if not database.exists():
        temp=local/'education.sqlite.building'
        # An interrupted build is reconstructed before the immutable DB is published.
        if temp.exists():temp.unlink()
        with sqlite3.connect(temp) as con:
            con.executescript('CREATE TABLE biographies(source_key TEXT PRIMARY KEY,context TEXT,flags TEXT); CREATE TABLE awards(source_key TEXT,award_index TEXT,name TEXT,position INTEGER,data TEXT,PRIMARY KEY(source_key,award_index));')
            with (education/'source_biographies.csv').open(encoding='utf-8-sig',newline='') as f:
                for bio in csv.DictReader(f):
                    region,flags=parser.bounds(bio['raw_text_adjusted'],int(bio['edition']))
                    context=bio['raw_text_adjusted'][region[0]:region[1]] if region else ''
                    con.execute('INSERT INTO biographies VALUES(?,?,?)',(bio['source_key'],context,json.dumps(flags)))
            with (education/'education_awards.csv').open(encoding='utf-8-sig',newline='') as f:
                for position,a in enumerate(csv.DictReader(f)):
                    if pilot.qualifying(a):con.execute('INSERT INTO awards VALUES(?,?,?,?,?)',(a['source_key'],a['award_index'],a['institution_as_written'],position,json.dumps(a,ensure_ascii=False)))
            con.execute('CREATE INDEX award_name ON awards(name)')
            assert con.execute('SELECT count(*) FROM biographies').fetchone()[0]==156139
            assert con.execute('SELECT count(*) FROM awards').fetchone()[0]==330777
        con.close()
        temp.replace(database)
    checks[str(database)]=storage.raw_digest(database)
    immutable(local/'preserved_pilot_results.json',pilot_results)
    batches=make_batches(sorted(conditional,key=lambda r:r['record_id']),'conditional',1)
    batches+=make_batches(sorted(naming,key=lambda r:r['record_id']),'naming_pilot',len(batches)+1)
    # Larger names first improves early coverage while all eligible names are reviewed.
    batches+=make_batches(sorted(likely,key=lambda r:(-r['award_count'],r['institution_as_written'],r['record_id'])),'likely_exact',len(batches)+1)
    manifest=dict(version=VERSION,started_at_utc=started,preparation_seconds=time.perf_counter()-timer,data_root=str(data),
                  previous_release=str(previous),pilot_release=str(pr),source_checksums=checks,batches=batches,
                  local_groups={'conditional':140,'naming_pilot':100,'likely_exact':1887},pilot_preserved=50,
                  web_new_target=480,web_pilot_preserved=20,pilot_retries=12,seed=pilot.SEED,
                  direct_OpenAlex_API_calls=False,identity_not_geocoding=True)
    write(local/'run_manifest.json',manifest)
    write(local/'state.json',dict(phase='local_reviews',started_at_utc=started))
    shutil.copy2(Path(__file__),local/'runner_prepare_snapshot.py')
    print(json.dumps(dict(local=str(local),batches=batches,preparation_seconds=manifest['preparation_seconds']),ensure_ascii=True))


def apply_matches(matches,award,explicit=False):
    components=collections.defaultdict(set)
    for m in matches:
        cond=m.get('conditions')
        if cond is None or pilot.rules.condition_applies(pilot.normalize_conditions(cond),award):
            components[m.get('component_index',0)].add(pilot.rules.valid_id(m['openalex_id']))
    conflict=any(len(v)>1 for v in components.values())
    if not explicit and len(components)>1:conflict=True
    return (set() if conflict else set().union(*components.values())),conflict


def validate(d,original,records):
    if any(d.get(k)!=original[k] for k in ('record_id','institution_as_written','group')):raise ValueError('Identity/group changed')
    if d.get('status') not in STATUSES or not d.get('reason'):raise ValueError('Missing status/reason')
    datetime.date.fromisoformat(d['research_date'])
    if bool(d.get('matches'))!=(d['status'] in ('resolved','conditional')):raise ValueError('Matches/status mismatch')
    seen=set()
    for match in d.get('matches',[]):
        entity=pilot.rules.valid_id(match['openalex_id']);record=records.get(entity)
        if record is None or match['canonical_name']!=record['canonical_name']:raise ValueError('Unsupported snapshot identity')
        if original['group']!='conditional' and (record['type']!='education' or record['is_super_system']):raise ValueError('Ineligible new identity')
        if entity in seen:raise ValueError('Duplicate match ID')
        seen.add(entity)
        if not match.get('evidence') or not match.get('source_urls'):raise ValueError('Missing evidence')
        condition=match.get('conditions')
        if condition is not None:pilot.rules.validate_conditions(pilot.normalize_conditions(condition))
        if d['status']=='resolved' and condition is not None:raise ValueError('Resolved result cannot carry conditions')
    if d['status']=='conditional' and not any(m.get('conditions') is not None for m in d['matches']):raise ValueError('Conditional result needs explicit scope')
    if len(seen)>1 and original['group']!='conditional' and not d.get('explicit_co_named_institutions'):raise ValueError('Multiple IDs need co-naming evidence')
    if original['group']=='conditional' and (d['status']!='conditional' or d['matches']!=original['existing_matches']):raise ValueError('Existing conditional decision changed')
    if original['group'] in ('web_research','pilot_retry'):
        log=d.get('research_log',{})
        if len(log.get('searches',[]))>2:
            deviation=d.get('research_policy_deviation',{})
            if (d.get('matches') or deviation.get('kind')!='query_budget_exceeded' or deviation.get('limit')!=2
                    or deviation.get('actual')!=len(log['searches']) or deviation.get('acceptance_withheld') is not True
                    or not deviation.get('reason')):raise ValueError('Research budget exceeded; preserve full log and withhold acceptance')
        if len(log.get('opened_urls',[]))>3:raise ValueError('Source-open budget exceeded')
        if any('api.openalex.org' in url for url in log.get('opened_urls',[])):raise ValueError('API URL opened')
        for match in d.get('matches',[]):
            verified=match.get('official_page_verification',{})
            if verified.get('identity_visible') is not True or verified.get('canonical_id')!=match['openalex_id']:raise ValueError('Official identity not visible')
            if verified.get('url') not in log.get('opened_urls',[]) or not re.fullmatch(r'https://(?:explore\.)?openalex\.org/(?:institutions/)?[Ii]\d+',verified['url']):raise ValueError('Invalid official verification URL')
            if not verified.get('observed_name') or not verified.get('evidence'):raise ValueError('Official observation missing')
        if d['status']=='access_blocked' and not log.get('access_retry'):raise ValueError('Blocked lookup was not retried')
    if original['group']=='likely_exact':
        awards=read(original['full_awards_file'])
        expected={(a['source_key'],str(a['award_index'])) for a in awards}
        supplied={(a['source_key'],str(a['award_index'])) for a in d.get('reviewed_awards',[])}
        if supplied!=expected or len(supplied)!=len(d.get('reviewed_awards',[])):raise ValueError('Incomplete full award review')
        eligible={m['openalex_id'] for c in original['components'] for m in c['exact_matches']}
        if not seen<=eligible:raise ValueError('Local acceptance is not an eligible exact candidate')
    return True


def save(number,path):
    manifest=load_local();batch=next(b for b in manifest['batches'] if b['batch']==number)
    if storage.raw_digest(batch['input_file'])!=batch['input_sha256']:raise ValueError('Batch input changed')
    decision=read(path);original=next(r for r in read(batch['input_file']) if r['record_id']==decision['record_id'])
    if storage.raw_digest(original['full_awards_file'])!=original['evidence_sha256']:raise ValueError('Evidence input changed')
    validate(decision,original,read(local_root()/'snapshot_records.json'))
    immutable(original['result_file'],decision)
    print(json.dumps(dict(saved=decision['record_id'],status=decision['status'])))


def results(require_local=False):
    items=batch_inputs();out=[]
    for item in items:
        if Path(item['result_file']).exists():out.append(read(item['result_file']))
        elif require_local and item['group'] in ('conditional','naming_pilot','likely_exact'):raise ValueError('Local stage incomplete: '+item['record_id'])
    return out


def effective_results():
    local=local_root();out={r['record_id']:r for r in read(local/'preserved_pilot_results.json')}
    for result in results():
        # The older naming pilot overlaps one later web-pilot raw name (Grant).
        # Keep its naming decision as provenance without replacing the newer pilot outcome.
        if result['group']=='naming_pilot' and result['record_id'] in out:continue
        out[result['record_id']]=result
    pattern=local/'audit/pattern_revisions.json'
    if pattern.exists():
        for revision in read(pattern):out[revision['record_id']]=revision['replacement_decision']
    revisions=local/'audit/reviews.json'
    if revisions.exists():
        for review in read(revisions):
            if review.get('replacement_decision'):out[review['record_id']]=review['replacement_decision']
    return out


def compute_coverage(output=None):
    m=load_local();rows=all_rows(m);prior={r['institution_as_written']:r for r in rows}
    decisions=effective_results();effective={r['institution_as_written']:r for r in decisions.values()}
    pilot_decisions={r['institution_as_written']:r for r in read(local_root()/'preserved_pilot_results.json')}
    with (Path(m['pilot_release'])/'person_id_coverage.csv').open(encoding='utf-8-sig',newline='') as f:people=list(csv.DictReader(f))
    before={r['source_key']:set(json.loads(r['IDs_after'])) for r in people};reconstructed=collections.defaultdict(set)
    after={k:set(v) for k,v in before.items()};old_awards=set();new_awards=set();names_with_ids=set();old_names=set();seen=set()
    group_awards=collections.defaultdict(set);group_people=collections.defaultdict(set);all_institution_people=set();award_counts=collections.Counter()
    ledger_file=None;writer=None
    if output:
        output=Path(output);output.mkdir(parents=True,exist_ok=True)
        ledger_file=(output/'award_id_ledger.csv').open('w',encoding='utf-8',newline='')
        writer=csv.DictWriter(ledger_file,fieldnames=['source_key','edition','award_index','record_id','institution_as_written','requested_family','year','flags',
                  'IDs_before','IDs_after','additional_IDs','conditional_IDs','assignment_class','condition_conflict','fully_matched','matched_components','component_count'])
        writer.writeheader()
    with db() as con:
        for data,context,boundary_flags in con.execute('SELECT a.data,b.context,b.flags FROM awards a JOIN biographies b USING(source_key) ORDER BY a.position'):
            a=dict(json.loads(data),education_context=context,education_boundary_flags=json.loads(boundary_flags));name=a['institution_as_written']
            key=a['source_key'];ref=(key,a['award_index']);row=prior[name]
            assert ref not in seen;seen.add(ref);all_institution_people.add(key)
            ids_before=set(v2.row_ids(row))
            if name in pilot_decisions:
                d=pilot_decisions[name];assigned,conflict=apply_matches(d.get('matches',[]),a,d.get('explicit_co_named_institutions',False));ids_before.update(assigned)
            reconstructed[key].update(ids_before)
            ids_after=set(ids_before);conditional_ids=set();matched_components=set();classifications=set();conflict=False
            d=effective.get(name)
            if d:
                assigned,conflict=apply_matches(d.get('matches',[]),a,d.get('explicit_co_named_institutions',False))
                ids_after.update(assigned)
                if assigned:
                    group_awards[d['group']].add(ref);group_people[d['group']].add(key)
                    for match in d['matches']:
                        if match['openalex_id'] in assigned:
                            matched_components.add(match.get('component_index',0))
                            if match.get('conditions') is not None:conditional_ids.add(match['openalex_id'])
                    classifications.add('condition_satisfied' if conditional_ids else 'reviewed_provisional')
            if v2.row_ids(row):classifications.add('previously_accepted_or_provisional')
            after[key].update(ids_after)
            if ids_before:old_awards.add(ref);old_names.add(name)
            if ids_after:new_awards.add(ref);names_with_ids.add(name)
            if conflict:award_counts['conflicting_awards']+=1
            count=d.get('component_count') if d else None
            count=count or (len(row.get('components',[])) if row['origin']=='offline_cleanup' else 1)
            full=bool(ids_after) and (len(matched_components)>=count if d else row['status'] not in ('partially_provisional_matched',))
            if row['status']=='fully_provisional_matched' and v2.row_ids(row):full=True
            if full:award_counts['fully_matched_awards']+=1
            elif ids_after:award_counts['partially_matched_awards']+=1
            if writer:writer.writerow(dict(source_key=key,edition=a['edition'],award_index=a['award_index'],record_id=row['record_id'],institution_as_written=name,
                requested_family=a['requested_family'],year=a['year'],flags=a['flags'],IDs_before=json.dumps(sorted(ids_before)),IDs_after=json.dumps(sorted(ids_after)),
                additional_IDs=json.dumps(sorted(ids_after-ids_before)),conditional_IDs=json.dumps(sorted(conditional_ids)),assignment_class=json.dumps(sorted(classifications)),
                condition_conflict=conflict,fully_matched=full,matched_components=json.dumps(sorted(matched_components)),component_count=count))
    if ledger_file:ledger_file.close()
    assert len(seen)==330777 and len(people)==156139 and len(all_institution_people)==144455
    assert all(before[k]==reconstructed[k] for k in before), 'Pilot person coverage reconstruction differs'
    b={k for k,v in before.items() if v};a={k for k,v in after.items() if v};assert len(b)==123370 and len(old_awards)==248851
    assert b<=a and old_awards<=new_awards
    before_ids=set().union(*before.values());after_ids=set().union(*after.values())
    counts=collections.Counter(r['status'] for r in decisions.values())
    report=dict(AMWS_total_entries=len(people),institution_bearing_entries=len(all_institution_people),
              AMWS_entries_with_ID_before=len(b),AMWS_entries_with_ID_after=len(a),additional_AMWS_entries_with_any_ID=len(a-b),
              AMWS_entries_gaining_distinct_IDs=sum(bool(after[k]-before[k]) for k in before),
              institution_bearing_entries_without_ID_after=len(all_institution_people-a),
              distinct_IDs_before=len(before_ids),distinct_IDs_after=len(after_ids),additional_distinct_IDs=len(after_ids-before_ids),
              award_references_with_ID_before=len(old_awards),award_references_with_ID_after=len(new_awards),additional_award_references_with_any_ID=len(new_awards-old_awards),
              raw_names_with_any_applied_ID_before=len(old_names),raw_names_with_any_applied_ID_after=len(names_with_ids),additional_raw_names_with_any_applied_ID=len(names_with_ids-old_names),
              raw_names_without_any_applied_ID_after=len(prior)-len(names_with_ids),total_raw_names=len(prior),
              prior_origin_accounting={origin:dict(raw_names=sum(row['origin']==origin for row in rows),
                   names_with_applied_ID_before=sum(row['origin']==origin and row['institution_as_written'] in old_names for row in rows),
                   names_with_applied_ID_after=sum(row['origin']==origin and row['institution_as_written'] in names_with_ids for row in rows))
                   for origin in sorted({row['origin'] for row in rows})},
              resolution_decisions=len(decisions),resolution_status_counts=dict(counts),**award_counts,
              by_group={g:dict(decisions=sum(r['group']==g for r in decisions.values()),
                   status_counts=dict(collections.Counter(r['status'] for r in decisions.values() if r['group']==g)),
                   matched_awards=len(group_awards[g]),people_with_assignment=len(group_people[g]),newly_covered_people=len(group_people[g]-b))
                   for g in GROUPS},
              by_edition={ed:dict(before=sum(r['edition']==ed and r['source_key'] in b for r in people),
                   after=sum(r['edition']==ed and r['source_key'] in a for r in people)) for ed in sorted({r['edition'] for r in people})},
              counting='Distinct source_key within edition; distinct source_key/award_index; no cross-edition person linkage; conditions evaluated separately at each award',
              identity_accuracy='Coverage is not verified identity accuracy; new local assignments remain provisional')
    if output:
        with (output/'person_id_coverage.csv').open('w',encoding='utf-8',newline='') as f:
            w=csv.DictWriter(f,fieldnames=['source_key','edition','IDs_before','IDs_after','additional_ID_count']);w.writeheader()
            for p in people:
                key=p['source_key'];w.writerow(dict(source_key=key,edition=p['edition'],IDs_before=json.dumps(sorted(before[key])),
                         IDs_after=json.dumps(sorted(after[key])),additional_ID_count=len(after[key]-before[key])))
        write(output/'coverage_report.json',report)
    return report,after


def select_web():
    local=local_root();m=load_local()
    if m.get('web_selection_complete'):
        print(json.dumps(dict(already_selected=True,batches=[b for b in m['batches'] if b['group'] in ('web_research','pilot_retry')])));return
    results(require_local=True);timer=time.perf_counter();coverage,people=compute_coverage()
    rows=all_rows(m);by_id={r['record_id']:r for r in rows};effective=effective_results()
    pilot_ids={r['record_id'] for r in read(local/'preserved_pilot_results.json')}
    candidates=[]
    for row in rows:
        identifier=row['record_id'];decision=effective.get(identifier)
        if identifier in pilot_ids or v2.row_ids(row) or row['origin']=='name_research_pilot_preserved':continue
        if decision:
            if decision['group']!='likely_exact' or decision.get('matches') or decision['status'] not in ('ambiguous','not_found'):continue
        elif row['status'] not in ('candidate_only','ambiguous','cleanup_unresolved','not_found'):continue
        candidates.append(row)
    scored=[]
    for row in candidates:
        awards=evidence(row['institution_as_written']);uncovered={a['source_key'] for a in awards if not people[a['source_key']]}
        scored.append((len(uncovered),len(awards),row['institution_as_written'],row['record_id']))
    selected=sorted(scored,key=lambda s:(-s[0],-s[1],s[2],s[3]))[:480];assert len(selected)==480
    selected_rows=[by_id[x[3]] for x in selected]
    pilot_results=read(local/'preserved_pilot_results.json')
    retry_rows=[dict(by_id[r['record_id']],pilot_result=r) for r in pilot_results if r['group']=='web_research' and r['status']=='access_blocked']
    assert len(retry_rows)==12 and len({r['record_id'] for r in selected_rows+retry_rows})==492
    batches=make_batches(selected_rows,'web_research',max(b['batch'] for b in m['batches'])+1)
    if not any(b['group']=='pilot_retry' for b in m['batches']):
        batches+=make_batches(sorted(retry_rows,key=lambda r:r['record_id']),'pilot_retry',max(b['batch'] for b in batches)+1)
    immutable(local/'web_selection_manifest.json',dict(seed=pilot.SEED,selection_rule='Descending marginal uncovered source entries, award frequency, literal name, record ID',
              candidates=len(candidates),selected=[dict(record_id=x[3],institution_as_written=x[2],uncovered_AMWS_entries=x[0],award_count=x[1]) for x in selected],
              retries=[r['record_id'] for r in retry_rows],pilot_web_names_preserved=20,new_web_names=480,total_web_names=500,
              local_stage_coverage=coverage,selection_seconds=time.perf_counter()-timer))
    m['batches']+=batches;m['web_selection_complete']=True;write(local/'run_manifest.json',m)
    write(local/'state.json',dict(phase='web_research',started_at_utc=m['started_at_utc']))
    print(json.dumps(dict(batches=batches,local_coverage=coverage),ensure_ascii=True))


def prepare_retries():
    m=load_local();local=local_root()
    existing=[b for b in m['batches'] if b['group']=='pilot_retry']
    if existing:print(json.dumps(dict(already_prepared=True,batches=existing)));return
    prior={x['record_id']:x for x in all_rows(m)}
    rows=[dict(prior[x['record_id']],pilot_result=x) for x in read(local/'preserved_pilot_results.json') if x['group']=='web_research' and x['status']=='access_blocked']
    assert len(rows)==12
    batches=make_batches(sorted(rows,key=lambda x:x['record_id']),'pilot_retry',max(b['batch'] for b in m['batches'])+1)
    m['batches']+=batches;write(local/'run_manifest.json',m)
    print(json.dumps(dict(batches=batches)))


def prepare_audit():
    local=local_root();m=load_local();decisions=results();expected=sum(b['records'] for b in m['batches'])
    if len(decisions)!=expected:raise ValueError('Continuation work incomplete')
    by_group={g:sorted([d for d in decisions if d['group']==g],key=lambda d:d['record_id']) for g in ('likely_exact','web_research')}
    rng=random.Random(pilot.SEED);picked=[]
    for group in ('likely_exact','web_research'):
        used={d['record_id'] for d in picked};pool=[d for d in by_group[group] if d['record_id'] not in used]
        picked+=rng.sample(pool,min(50,len(pool)))
    if len(picked)<100:
        used={d['record_id'] for d in picked};pool=[d for group in by_group.values() for d in group if d['record_id'] not in used]
        picked+=rng.sample(pool,min(100-len(picked),len(pool)))
    items={(x['record_id'],x['group']):x for x in batch_inputs()}
    immutable(local/'audit/sample_manifest.json',dict(seed=pilot.SEED,count=len(picked),records=[dict(record_id=d['record_id'],institution_as_written=d['institution_as_written'],
               group=d['group'],original_decision_file=items[(d['record_id'],d['group'])]['result_file'],full_awards_file=items[(d['record_id'],d['group'])]['full_awards_file']) for d in picked]))
    print(json.dumps(dict(audit_records=len(picked))))


def auto():
    timer=time.perf_counter();started=base.now();local=local_root();records=read(local/'snapshot_records.json')
    index=base.SnapshotIndex(records,read(local/'snapshot_aliases.json'));completed=collections.Counter()
    for item in batch_inputs():
        if item['group'] not in ('conditional','naming_pilot'):continue
        if Path(item['result_file']).exists():completed[item['group']]+=1;continue
        matches=[];candidates=[];source_urls=[]
        if item['group']=='conditional':
            matches=item['existing_matches'];status='conditional';reason='Preserved existing conditional mapping, evaluated independently at each award.'
        else:
            prior=item['prior_naming_decision'];status=prior['status'];source_urls=prior.get('source_urls',[])
            if status in ('resolved','conditional'):
                for component,inst in enumerate(prior['institutions']):
                    name=inst['standard_name'];processed=v2.process_name(name)['processed_name']
                    full=index.exact_result(name);short=index.exact_result(processed) if processed else dict(exact_status='empty_processed_name',exact_matches=[])
                    selected=full if full['exact_matches'] else short;version='cleaned' if full['exact_matches'] else 'processed'
                    if selected['exact_status']=='eligible_unique_exact':
                        candidate=selected['exact_matches'][0]
                        if inst.get('identity_type')=='historical_independent' and base.normalize(candidate['canonical_name'])!=base.normalize(name):
                            candidates.append(dict(candidate,reason='Historical independent institution preserved separately from a successor record.'));continue
                        matches.append(dict(openalex_id=candidate['openalex_id'],canonical_name=candidate['canonical_name'],conditions=pilot.normalize_conditions(inst.get('conditions')),
                              component_index=component,standard_name=name,matched_snapshot_alias=candidate['matched_alias'],matching_version=version,
                              qualifier_removed=version=='processed' and processed!=name,source_urls=source_urls+[str(local/'snapshot_records.json')],
                              evidence=inst['identity_evidence']))
                    else:candidates.extend(selected['exact_matches'])
                if not matches:status='ambiguous' if candidates else 'not_found'
                elif any(m['conditions'] is not None for m in matches):status='conditional'
                elif len(matches)!=len(prior['institutions']):
                    # Retain partial components without declaring the complete raw entry matched.
                    status='resolved'
                else:status='resolved'
            reason='Prior naming decision preserved; official full/processed snapshot exact policy applied to its supported identities.'
        decision=dict(record_id=item['record_id'],institution_as_written=item['institution_as_written'],group=item['group'],status=status,
              matches=matches,candidates=candidates,reason=reason,source_urls=source_urls,queries=[],research_date=datetime.date.today().isoformat(),
              verification_method='preserved_prior_conditions' if item['group']=='conditional' else 'prior_researched_name_and_local_snapshot',
              provisional=item['group']!='conditional',explicit_co_named_institutions=item['group']!='conditional' and len(matches)>1,
              component_count=len(item.get('prior_naming_decision',{}).get('institutions',[])),started_at_utc=base.now(),finished_at_utc=base.now())
        validate(decision,item,records);immutable(item['result_file'],decision);completed[item['group']]+=1
    for b in load_local()['batches']:
        if b['group'] in ('conditional','naming_pilot'):
            immutable(Path(b['input_file']).parent/'timing_report.json',dict(batch=b['batch'],group=b['group'],records=b['records'],
                      timing_method='Coordinator deterministic local stage; aggregate wall time in automatic_stage_timing.json'))
    write(local/'automatic_stage_timing.json',dict(started_at_utc=started,finished_at_utc=base.now(),elapsed_seconds=time.perf_counter()-timer,completed=dict(completed)))
    print(json.dumps(dict(completed=completed,seconds=time.perf_counter()-timer)))


def status():
    m=load_local();done=results();counts=collections.Counter(r['group'] for r in done)
    out=[]
    for b in m['batches']:
        remaining=[r['record_id'] for r in read(b['input_file']) if not Path(r['result_file']).exists()]
        out.append(dict(batch=b['batch'],group=b['group'],remaining=len(remaining),total=b['records']))
    print(json.dumps(dict(completed=dict(counts),batches=out,phase=read(local_root()/'state.json')['phase']),ensure_ascii=True))


def main():
    ap=argparse.ArgumentParser();ap.add_argument('command',choices=['prepare','auto','save','status','coverage','select-web','prepare-audit','prepare-retries']);ap.add_argument('--data-root',type=Path,default=pilot.DEFAULT_DATA)
    ap.add_argument('--batch',type=int);ap.add_argument('--decision',type=Path);args=ap.parse_args()
    if args.command=='prepare':prepare(args.data_root)
    elif args.command=='auto':auto()
    elif args.command=='save':save(args.batch,args.decision)
    elif args.command=='coverage':print(json.dumps(compute_coverage()[0],ensure_ascii=True))
    elif args.command=='select-web':select_web()
    elif args.command=='prepare-audit':prepare_audit()
    elif args.command=='prepare-retries':prepare_retries()
    else:status()


if __name__=='__main__':main()

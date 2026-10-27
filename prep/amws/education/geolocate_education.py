"""Versioned matching, independent audits and additive AMWS location outputs.

Run after institution_geolocation.py prepare/candidates/baseline and explicit
development reviews. Only files inside the new geolocation directory are written.
"""
import argparse,collections,csv,gzip,json,math,random,re,statistics
from pathlib import Path
from institution_geolocation import (OUT,ENRICHED,HERE,ALLOCATION,oid,coordinate,norm,quality_flags,inventory,
                                    target_awards,read_csv,write_csv,write_json,digest)
from geolocation_review import catalog as record_catalog
from education_regex import bounds

LEXICAL = {'untv':'university','unlv':'university','umv':'university',
 'tcchnol':'technology','technol':'technology','tech':'technology',
 'polytech':'polytechnic','polytcch':'polytechnic','agr':'agricultural',
 'agric':'agricultural','mech':'mechanical','mass':'massachusetts',
 'calif':'california','mich':'michigan','minn':'minnesota','wis':'wisconsin',
 'pa':'pennsylvania','kans':'kansas','okla':'oklahoma','ore':'oregon',
 'fla':'florida','ariz':'arizona','tex':'texas','nchr':'nebraska',
 'nebr':'nebraska','colo':'colorado','tenn':'tennessee','ky':'kentucky',
 'ind':'indiana','md':'maryland','la':'louisiana','mo':'missouri',
 'del':'delaware','conn':'connecticut','ala':'alabama','ga':'georgia'}

def normalize(text,ocr=False):
    q=norm(text)
    if ocr:q=re.sub(r'\buniversity (?:111|iii)\b','university illinois',q)
    # State/postal abbreviations expand only in an institutional phrase.
    institutional=bool(re.search(r'\b(?:university|college|institute|untv|unlv|umv|state|polytech)\b',q))
    if institutional:
        q=re.sub(r'\bn y\b|\bny\b','new york',q)
        q=re.sub(r'\bn c\b|\bnc\b','north carolina',q)
        q=re.sub(r'\bs c\b|\bsc\b','south carolina',q)
        q=re.sub(r'\bb c\b|\bbc\b','british columbia',q)
        q=' '.join(LEXICAL.get(t,t) for t in q.split())
    return q

def reviews(phase='development'):
    paths=[OUT/f'review_{k}.jsonl' for k in ('ABCD' if phase=='development' else ['final_A','final_B','final_C','final_D'])]
    if phase=='development':
        paths += [OUT/f'review_{k}_corrections.jsonl' for k in 'ABC']
        paths.append(OUT/'adjudication_corrections.jsonl')
    else:paths += [OUT/f'review_final_{k}_corrections.jsonl' for k in 'ABCD']
    decisions={};allrows=[]
    for p in paths:
        if not p.exists():continue
        for line in p.read_text(encoding='utf-8').splitlines():
            d=json.loads(line)
            if 'replacement_decision' in d:d=d['replacement_decision']
            elif 'new_decision' in d:d=d['new_decision']
            assert d['manual_review'];allrows.append(dict(d,source_file=p.name));decisions[d['name_id']]=d
    return decisions,allrows

def reference_records():
    rs=record_catalog()['records']
    for r in rs.values():
        for k in ('latitude','longitude'):
            r[k]=float(r[k]) if r[k] else None
    return rs

def validate_snapshot_locations(records,ids):
    """Check published locations against the actual protected gzip snapshot."""
    seen=set()
    for path in sorted((ALLOCATION/'Data/external/oa_snapshot/data/institutions').rglob('*.gz')):
        with gzip.open(path,'rt',encoding='utf-8') as source:
            for line in source:
                original=json.loads(line);id=oid(original['id'])
                if id not in ids:continue
                assert id not in seen,'Duplicate accepted snapshot ID'
                seen.add(id);record=records[id];geo=original.get('geo') or {}
                assert original['type']=='education' and coordinate(geo)
                assert record['name']==original['display_name']
                for key in ('latitude','longitude'):
                    assert record[key]==geo.get(key),(id,key,'snapshot coordinate mismatch')
                for key in ('city','region'):
                    assert (record[key] or None)==(geo.get(key) or None),(id,key)
                assert (record['country'] or None)==(original.get('country_code') or None),id
    assert seen==ids,'Published ID absent from snapshot'
    return len(seen)

def validate_award_evidence(ledger):
    """Offsets remain literal offsets in the unchanged supplied biographies."""
    grouped=collections.defaultdict(list)
    for award in ledger:grouped[award['source_key']].append(award)
    checked=0
    with (ENRICHED/'source_biographies.csv').open(encoding='utf-8-sig',newline='') as source:
        for row in csv.DictReader(source):
            for award in grouped.pop(row['source_key'],[]):
                text=row['raw_text_adjusted']
                assert text[int(award['start']):int(award['end'])]==award['evidence'],award['source_key']
                assert text[int(award['label_start']):int(award['label_end'])]==award['label_as_written'],award['source_key']
                checked+=1
    assert not grouped and checked==len(ledger)
    return checked

def condition_matches(condition,award,context):
    assert condition.get('all'),'Empty condition is not authorized'
    flags=json.loads(award['flags']) if isinstance(award.get('flags'),str) else award.get('flags',[])
    for test in condition['all']:
        field,op,value=test['field'],test['op'],test['value']
        assert field in ('year','edition','requested_family','education_context','evidence','institution_as_written','label_as_written')
        actual=context if field=='education_context' else award.get(field)
        if field in ('year','edition'):
            if not actual or (field=='year' and 'chronology_conflict' in flags):return False
            actual=int(actual)
        if op=='lt':ok=actual<value
        elif op in ('le','lte'):ok=actual<=value
        elif op in ('ge','gte'):ok=actual>=value
        elif op=='gt':ok=actual>value
        elif op=='eq':ok=actual==value
        elif op=='in':ok=actual in value
        elif op=='regex':ok=bool(re.search(value,actual or '',re.I))
        else:raise ValueError('Unsupported condition operator '+op)
        if not ok:return False
    return True

def education_contexts(needed):
    result={}
    with (ENRICHED/'source_biographies.csv').open(encoding='utf-8-sig',newline='') as f:
        for row in csv.DictReader(f):
            if row['source_key'] in needed:
                region,flags=bounds(row['raw_text_adjusted'],int(row['edition']))
                result[row['source_key']]=row['raw_text_adjusted'][slice(*region)] if region else ''
    assert needed<=result.keys()
    return result

def resolve(row,award,context=''):
    if row is None:return 'missing_institution',[],[]
    conditions=row.get('conditions',[])
    if conditions:
        choices={tuple(sorted(c['accepted_ids'])) for c in conditions if condition_matches(c,award,context)}
        if len(choices)==1:return 'reviewed_conditional',list(next(iter(choices))),['conditional_education_evidence']
        return 'ambiguous',[],['conflicting_conditions' if len(choices)>1 else 'condition_not_satisfied']
    return row['status'],row['accepted_ids'],row['flags']

def evaluated_counts(rows):
    byname={r['name']:r for r in rows}
    conditional={r['name'] for r in rows if r.get('conditions')}
    needed={a['source_key'] for a in target_awards() if a['institution_as_written'] in conditional}
    contexts=education_contexts(needed)
    names=set();matched=0;counts=collections.Counter()
    for a in target_awards():
        if not a['institution_as_written']:continue
        status,ids,_=resolve(byname[a['institution_as_written']],a,contexts.get(a['source_key'],''))
        if ids:matched+=1;names.add(a['institution_as_written'])
        counts[status]+=1
    return dict(matched_names=len(names),matched_awards=matched,institution_awards=286848,
                award_coverage=matched/286848,statuses_award=dict(counts))

def build_rules(version,expanded=False,ocr=False,strict=False):
    decisions,log=reviews();selection=json.loads((OUT/'review_selection.json').read_text(encoding='utf-8'))
    assert set(selection['selected_ids'])<=decisions.keys(),'Development reviews incomplete'
    records=reference_records();inv=inventory();byid={r['name_id']:r for r in inv}
    aliases=collections.defaultdict(dict)
    fn=lambda x: normalize(x,ocr) if expanded else norm(x)
    for a in read_csv(OUT/'reference_aliases.csv'):
        if a['automatic']=='True' and records[a['id']]['type']=='education':
            aliases[fn(a['alias'])][a['id']]=a['alias']
    verified=collections.defaultdict(set);blocked=set()
    for id,d in decisions.items():
        key=fn(byid[id]['name'])
        if d['scope']=='global' and d['accepted_ids']:
            if all(records[i]['type']=='education' and records[i]['valid_coordinates'] for i in d['accepted_ids']):
                verified[key].add(tuple(sorted(d['accepted_ids'])))
        else:blocked.add(key)
    rows=json.loads((OUT/'iterations/geo_v01/predictions.json').read_text(encoding='utf-8'))
    for r in rows:
        r['rule_version']=version;r['conditions']=[];key=fn(r['name']);r['normalized_versioned']=key
        decision=decisions.get(r['name_id']);exact=aliases[key]
        if decision:
            valid=all(records[i]['type']=='education' and records[i]['valid_coordinates'] for i in decision['accepted_ids'])
            r['accepted_ids']=decision['accepted_ids'] if valid else []
            r['conditions']=decision.get('conditions',[])
            for c in r['conditions']:
                assert all(records[i]['type']=='education' and records[i]['valid_coordinates'] for i in c['accepted_ids'])
            r['status']='reviewed_match' if r['accepted_ids'] else 'conditional' if r['conditions'] else decision['verdict']
            if not valid:r['status']='missing_reference_education_entity'
            r['method']='manual_adjudication';r['flags']=[] if r['accepted_ids'] else [decision['error_cause'] or decision['verdict']]
        elif key in blocked:
            r['accepted_ids']=[];r['status']='ambiguous';r['method']='reviewed_alias_guard'
        elif len(verified[key])==1:
            r['accepted_ids']=list(next(iter(verified[key])));r['status']='verified_alias';r['method']='reviewed_alias_normalization';r['flags']=[]
        elif len(verified[key])>1:
            r['accepted_ids']=[];r['status']='ambiguous';r['flags']=['conflicting_reviewed_aliases']
        elif len(exact)==1:
            id=next(iter(exact));flags=quality_flags(r['name'],records[id],None)
            # OCR digits are allowed solely when repaired by the explicit
            # university-Illinois rule and supported by a unique full alias.
            if ocr and re.search(r'\buniversity (?:111|iii)\b',norm(r['name'])):
                flags=[f for f in flags if f!='damaged_or_extra_text']
            if not flags:
                r['accepted_ids']=[id];r['status']='auto_exact';r['method']='normalized_unique_exact';r['flags']=[]
                r['candidates']=[dict(id=id,alias=exact[id],score=100,source='openalex_unique_normalized',automatic=True,reviewed=False)]+[c for c in r['candidates'] if c['id']!=id]
                r['candidates']=r['candidates'][:5]
            elif strict and r['accepted_ids']:
                r['accepted_ids']=[];r['status']='ambiguous';r['flags']=flags
        elif len(exact)>1:
            r['accepted_ids']=[];r['status']='ambiguous';r['flags']=['distinct_institution_exact_collision']
        if strict and r['accepted_ids'] and r['method'] not in ('manual_adjudication','reviewed_alias_normalization'):
            # Whole institutional phrase must survive matching; never split
            # legitimate conjunctions, strip cities, or use a parent fallback.
            if ';' in r['name'] or re.search(r'\b(?:sch|hospital|dept|branch|campus)\b',r['name'],re.I):
                r['accepted_ids']=[];r['status']='ambiguous';r['flags']=['combined_or_subunit_requires_review']
        if r['accepted_ids']:
            r['flags']+=['current_reference_location','historical_campus_not_reconstructed']
    folder=OUT/'iterations'/version;folder.mkdir(parents=True,exist_ok=True)
    write_json(folder/'predictions.json',rows)
    write_csv(folder/'predictions.csv',[{k:json.dumps(v,ensure_ascii=False) if isinstance(v,(dict,list)) else v for k,v in r.items()} for r in rows])
    write_json(folder/'rules.json',dict(version=version,expanded=expanded,ocr=ocr,strict=strict,
       lexical=LEXICAL if expanded else {},fuzzy='Frozen geo_v01 R stringdist DL candidates; 96 minimum,8 distinct-ID margin',
       unique_exact='One normalized OpenAlex education ID; candidate-only crosswalks cannot authorize a match',
       verified_aliases={k:[list(t) for t in sorted(v)] for k,v in verified.items()},blocked_aliases=sorted(blocked)))
    counts=evaluated_counts(rows);counts['version']=version;write_json(folder/'metrics.json',counts)
    write_json(folder/'development_review_log.json',log)
    audited=[]
    reasons=selection['reasons']
    for r in rows:
        if any(s.startswith('development_') for s in reasons.get(r['name_id'],[])):
            d=decisions[r['name_id']]
            accepted=bool(r['accepted_ids']);correct=accepted and set(r['accepted_ids'])==set(d['accepted_ids'])
            audited.append(dict(name_id=r['name_id'],name=r['name'],awards=r['awards'],accepted=accepted,correct=correct,
                proposed_ids=r['accepted_ids'],reviewed_ids=d['accepted_ids'],verdict=d['verdict'],cause=d['error_cause']))
    write_csv(folder/'development_audit.csv',audited)
    n=sum(a['accepted'] for a in audited);k=sum(a['correct'] for a in audited)
    counts['development_accepted_audited']=n;counts['development_precision']=k/n if n else None
    write_json(folder/'metrics.json',counts);print(json.dumps(counts),flush=True)
    return rows,counts

def freeze():
    development,log=reviews();baseline=json.loads((OUT/'iterations/geo_v01/predictions.json').read_text(encoding='utf-8'))
    selection=json.loads((OUT/'review_selection.json').read_text(encoding='utf-8'));baseline_errors=[]
    for r in baseline:
        if r['accepted_ids'] and r['name_id'] in development:
            d=development[r['name_id']];confirmed=d['scope']=='global' and set(r['accepted_ids'])==set(d['accepted_ids'])
            baseline_errors.append(dict(name_id=r['name_id'],name=r['name'],awards=r['awards'],confirmed=confirmed,
                proposed_ids=r['accepted_ids'],reviewed_ids=d['accepted_ids'],verdict=d['verdict'],cause=d['error_cause']))
    write_csv(OUT/'iterations/geo_v01/development_audit.csv',baseline_errors)
    write_csv(OUT/'development_error_table.csv',[r for r in baseline_errors if not r['confirmed']])
    history=[]
    for v,expand,ocr,strict in [('geo_v02',False,False,False),('geo_v03',True,False,False),
                              ('geo_v04',True,True,False),('geo_v05',True,True,True)]:
        rows,m=build_rules(v,expand,ocr,strict);history.append(m)
    # If the OCR change is still material, a separate conservative punctuation
    # review iteration makes no new acceptance rule. This records a genuine
    # no-supported-additional-rule plateau, rather than relaxing thresholds.
    if len(history)>1 and history[-2]['award_coverage']-history[-3]['award_coverage']>=.005:
        rows,m=build_rules('geo_v06',True,True,True);m['change']='Residual review supplied no further general acceptance rules';history.append(m)
    final=history[-1];assert final['development_precision']>=.99
    assert history[-1]['award_coverage']-history[-2]['award_coverage']<.005
    assert history[-2]['award_coverage']-history[-3]['award_coverage']<.005
    write_json(OUT/'iteration_metrics.json',history)
    write_csv(OUT/'iteration_metrics.csv',history)
    version=final['version'];write_json(OUT/'frozen_rules.json',dict(version=version,
        rules_sha256=digest(OUT/'iterations'/version/'rules.json'),predictions_sha256=digest(OUT/'iterations'/version/'predictions.json'),
        reference_sha256=digest(OUT/'reference_v1.json'),development_precision=final['development_precision'],
        stopping_reason='Two final revisions improve institution-bearing award coverage by less than 0.5 percentage points; remaining reviewed cases lack defensible general rules.'))
    write_csv(OUT/'reviewed_crosswalk.csv',[{k:json.dumps(v,ensure_ascii=False) if isinstance(v,list) else v for k,v in d.items()} for d in reviews()[0].values()])
    select_final(rows)

def select_final(rows):
    reviewed=set(reviews()[0]);rng=random.Random(1042)
    accepted=[r for r in rows if r['accepted_ids'] and r['name_id'] not in reviewed]
    unmatched=[r for r in rows if not r['accepted_ids'] and r['name_id'] not in reviewed]
    rng.shuffle(accepted);rng.shuffle(unmatched)
    chosen=accepted[:500]+unmatched[:200]
    records=reference_records();wanted={r['name']:r for r in chosen};examples=collections.defaultdict(list)
    for a in target_awards():
        s=a['institution_as_written']
        if s in wanted and not any(e['source_key']==a['source_key'] for e in examples[s]) and len(examples[s])<2:
            examples[s].append({k:a[k] for k in ('source_key','edition','requested_family','year','year_raw','evidence','flags')})
    needed={e['source_key'] for es in examples.values() for e in es};contexts=education_contexts(needed)
    batches={k:[] for k in 'ABCD'}
    for j,r in enumerate(chosen):
        entry=dict(r);entry['evidence']=[dict(e,education_context=contexts[e['source_key']]) for e in examples[r['name']]]
        entry['candidates']=[dict(c,reference=records[c['id']]) for c in r['candidates']]
        # Root handles100; three independent agents cover200each.
        batches['D' if j<100 else 'ABC'[(j-100)%3]].append(entry)
    for k,rs in batches.items():write_json(OUT/f'review_batch_final_{k}.json',rs)
    write_json(OUT/'final_audit_selection.json',dict(seed=1042,accepted_ids=[r['name_id'] for r in accepted[:500]],
       unmatched_ids=[r['name_id'] for r in unmatched[:200]],counts={k:len(v) for k,v in batches.items()},rules_frozen_before_selection=True))
    print('Independent final audits', {k:len(v) for k,v in batches.items()},flush=True)

def wilson(k,n):
    if not n:return None,None
    z=1.95996398454;p=k/n;den=1+z*z/n
    center=(p+z*z/(2*n))/den;delta=z*math.sqrt(p*(1-p)/n+z*z/(4*n*n))/den
    return max(0,center-delta),min(1,center+delta)

def final_metrics(rows):
    ds,log=reviews('final');selection=json.loads((OUT/'final_audit_selection.json').read_text(encoding='utf-8'))
    accepted=set(selection['accepted_ids']);unmatched_ids=set(selection['unmatched_ids'])
    assert len(accepted)==500 and len(unmatched_ids)==200 and not accepted&unmatched_ids
    selected=accepted|unmatched_ids
    assert selected==ds.keys(),'Independent final reviews incomplete or outside saved selection'
    assert not selected&reviews()[0].keys(),'Independent audit overlaps development reviews'
    byid={r['name_id']:r for r in rows};audit=[]
    for id in selection['accepted_ids']:
        r=byid[id];d=ds[id];correct=d['verdict'] in ('correct','resolved') and d['scope']=='global' and set(r['accepted_ids'])==set(d['accepted_ids'])
        audit.append(dict(name_id=id,name=r['name'],awards=r['awards'],correct=correct,proposed_ids=r['accepted_ids'],reviewed_ids=d['accepted_ids'],verdict=d['verdict'],cause=d['error_cause'],supporting_urls=d['supporting_urls']))
    n=len(audit);k=sum(r['correct'] for r in audit);weight=sum(r['awards'] for r in audit)
    weighted=sum(r['awards'] for r in audit if r['correct'])/weight if weight else None
    rng=random.Random(2042);boot=[]
    for _ in range(3000):
        sample=rng.choices(audit,k=n);boot.append(sum(r['awards'] for r in sample if r['correct'])/sum(r['awards'] for r in sample))
    boot.sort();unmatched=[]
    for id in selection['unmatched_ids']:
        d=ds[id];r=byid[id]
        unmatched.append(dict(name_id=id,name=r['name'],awards=r['awards'],missed_match=bool(d['accepted_ids']) and d['scope']=='global',partial_missed_match=bool(d.get('conditions')),verdict=d['verdict'],cause=d['error_cause'],accepted_ids=d['accepted_ids']))
    write_csv(OUT/'final_audit_accepted.csv',audit);write_csv(OUT/'final_audit_unmatched.csv',unmatched)
    write_json(OUT/'final_adjudication_log.json',log)
    m=dict(accepted_names_audited=n,correct_names=k,name_precision=k/n if n else None,name_precision_ci95=wilson(k,n),
        award_frequency_audited=weight,award_weighted_precision=weighted,award_weighted_precision_ci95_bootstrap=[boot[75],boot[2924]],
        unmatched_names_audited=len(unmatched),missed_matches=sum(r['missed_match'] for r in unmatched),
        missed_match_rate=sum(r['missed_match'] for r in unmatched)/len(unmatched),
        missed_match_rate_ci95=wilson(sum(r['missed_match'] for r in unmatched),len(unmatched)),
        partial_missed_matches=sum(r['partial_missed_match'] for r in unmatched),
        acceptance_target=.99,target_met=k/n>=.99 and weighted>=.99,bootstrap_seed=2042,bootstrap_unit='institution name; award frequency weights')
    write_json(OUT/'final_audit_metrics.json',m);return m,ds

def publish():
    enrichment_manifest=json.loads((ENRICHED/'manifest.json').read_text(encoding='utf-8'))
    assert digest(HERE/'education_regex.py')==enrichment_manifest['parser']['parser_sha256'],'Education boundary/parser changed'
    frozen=json.loads((OUT/'frozen_rules.json').read_text(encoding='utf-8'));version=frozen['version'];folder=OUT/'iterations'/version
    assert digest(folder/'rules.json')==frozen['rules_sha256'] and digest(folder/'predictions.json')==frozen['predictions_sha256']
    assert digest(OUT/'reference_v1.json')==frozen['reference_sha256']
    rows=json.loads((folder/'predictions.json').read_text(encoding='utf-8'));metrics,final_ds=final_metrics(rows);dev_ds=reviews()[0]
    write_csv(OUT/'reviewed_crosswalk.csv',[{**{k:json.dumps(v,ensure_ascii=False) if isinstance(v,(list,dict)) else v for k,v in d.items()},'review_phase':phase}
       for phase,decisions in [('development',dev_ds),('independent_final',final_ds)] for d in decisions.values()])
    records=reference_records();byname={r['name']:dict(r) for r in rows};byid={r['name_id']:r for r in rows}
    # Audit is evaluated against the unchanged frozen predictions. Publication
    # decisions are explicit per-name reviews; no rules are tuned on this audit.
    for id,d in final_ds.items():
        r=byname[byid[id]['name']]
        r['accepted_ids']=d['accepted_ids'] if d['scope']=='global' else []
        r['conditions']=d.get('conditions',[])
        r['status']='reviewed_match' if r['accepted_ids'] else 'conditional' if r['conditions'] else d['verdict']
        r['method']='independent_audit_adjudication';r['flags']=['current_reference_location','historical_campus_not_reconstructed'] if r['accepted_ids'] else [d['error_cause'] or d['verdict']]
    if not metrics['target_met']:
        for r in byname.values():
            if r['name_id'] not in dev_ds and r['name_id'] not in final_ds and r['accepted_ids']:
                r['candidate_ids']=r['accepted_ids'];r['accepted_ids']=[];r['status']='candidate_requires_review'
    for r in byname.values():
        if r['accepted_ids'] and not all(records[i]['type']=='education' and records[i]['valid_coordinates'] for i in r['accepted_ids']):
            r['accepted_ids']=[];r['status']='missing_reference_education_entity'
    write_json(OUT/'published_crosswalk.json',list(byname.values()))
    write_csv(OUT/'unmatched_inventory.csv',[{k:json.dumps(v,ensure_ascii=False) if isinstance(v,(list,dict)) else v for k,v in r.items()} for r in byname.values() if not r['accepted_ids']])
    conditional_names={r['name'] for r in byname.values() if r.get('conditions')}
    needed={a['source_key'] for a in target_awards() if a['institution_as_written'] in conditional_names};contexts=education_contexts(needed)
    lookup=collections.defaultdict(lambda:collections.defaultdict(list));ledger=[];counts=collections.Counter();namesets=collections.defaultdict(set)
    for a in target_awards():
        row=byname.get(a['institution_as_written']);status,ids,flags=resolve(row,a,contexts.get(a['source_key'],''))
        assert all(i in records and records[i]['type']=='education' and records[i]['valid_coordinates'] for i in ids)
        locations=[{k:records[i][k] for k in ('id','name','latitude','longitude','country','region','city')} for i in ids]
        object=dict(status=status,institution_as_written=a['institution_as_written'] or None,locations=locations,
            method=row['method'] if row else 'missing',rule_version=version,review_flags=flags)
        assert int(a['family_award_index'])==len(lookup[a['source_key']][a['requested_family']])+1
        lookup[a['source_key']][a['requested_family']].append(object)
        ledger.append(dict(**a,geolocation_status=status,reference_ids=json.dumps(ids),reference_locations=json.dumps(locations,ensure_ascii=False),geolocation_flags=json.dumps(flags),geolocation_method=object['method'],geolocation_rule_version=version))
        for key in [('all','all'),(a['edition'],'all'),(a['edition'],a['requested_family']),('all',a['requested_family'])]:
            counts[key,'total']+=1
            if a['institution_as_written']:
                counts[key,'institution_bearing']+=1;namesets[key,'total'].add(a['institution_as_written'])
            if ids:counts[key,'matched']+=1;namesets[key,'matched'].add(a['institution_as_written'])
    write_csv(OUT/'award_location_ledger.csv',ledger)
    evidence_checked=validate_award_evidence(ledger)
    accepted_ids={id for award in ledger for id in json.loads(award['reference_ids'])}
    snapshot_ids_checked=validate_snapshot_locations(records,accepted_ids)
    coverage=[]
    for ed,fam in sorted({k[0] for k in counts}):
        key=(ed,fam);n=counts[key,'institution_bearing'];m=counts[key,'matched'];nu=len(namesets[key,'total']);mu=len(namesets[key,'matched'])
        coverage.append(dict(edition=ed,family=fam,requested_awards=counts[key,'total'],institution_bearing_awards=n,matched_awards=m,award_coverage=m/n if n else None,
          award_coverage_all_awards=m/counts[key,'total'],unique_names=nu,matched_unique_names=mu,name_coverage=mu/nu if nu else None,
          interval_note='Full enriched-ledger census; coverage is descriptive, not a sampled accuracy estimate'))
    write_csv(OUT/'coverage_by_edition_degree.csv',coverage)
    write_json(OUT/'published_summary.json',dict(distinct_reference_institutions=len(accepted_ids),
       award_statuses=dict(collections.Counter(a['geolocation_status'] for a in ledger)),
       name_statuses=dict(collections.Counter(r['status'] for r in byname.values())),
       note='Conditional names can match some awards; unmatched_inventory includes names without an unconditional accepted mapping.'))
    status_causes=collections.defaultdict(lambda:dict(unique_names=0,awards=0))
    for r in byname.values():
        category=('accepted' if r['accepted_ids'] else 'conditional_or_ambiguous_identity' if r['status'] in ('ambiguous','conditional')
          else 'missing_reference_institution' if r['status'] in ('absent_reference','missing_reference_education_entity')
          else 'invalid_extraction' if r['status']=='invalid_extraction'
          else 'unreviewed_candidate' if r['status']=='candidate_requires_review' else 'unresolved_unclassified')
        status_causes[category]['unique_names']+=1;status_causes[category]['awards']+=r['awards']
    status_causes['missing_coordinates']=dict(unique_names=0,awards=0)
    write_csv(OUT/'resolution_categories.csv',[dict(category=k,**v) for k,v in status_causes.items()])
    files=[];person_folder=OUT/'person_files';person_folder.mkdir(exist_ok=True)
    for path in sorted(ENRICHED.glob('*_education.csv')):
        destination=person_folder/path.name.replace('_education.csv','_education_geolocated.csv')
        count=0
        with path.open(encoding='utf-8-sig',newline='') as source,destination.open('w',encoding='utf-8-sig',newline='') as dest:
            reader=csv.DictReader(source);original_columns=reader.fieldnames;added=[f'{fam}_institution_geocodes' for fam in ('ba_bs','ms','ma','phd')]
            writer=csv.DictWriter(dest,fieldnames=original_columns+added);writer.writeheader()
            for row in reader:
                edition=1986 if 'ed86' in path.name else int(float(row['edition'])) if 'combined' in path.name else int(path.name.split('_')[1])
                identifier=str(int(float(row['entry_id'] if edition==1986 else row['lineid'])))
                key=f'{edition}:{identifier}'
                for fam in ('ba_bs','ms','ma','phd'):
                    geocodes=lookup[key][fam];institutions=json.loads(row[f'{fam}_institutions'])
                    assert len(geocodes)==len(institutions)
                    assert [g['institution_as_written'] for g in geocodes]==institutions
                    row[f'{fam}_institution_geocodes']=json.dumps(geocodes,ensure_ascii=False,separators=(',',':'))
                writer.writerow(row);count+=1
        # Stream verification retains duplicate candidates, row/column order.
        with path.open(encoding='utf-8-sig',newline='') as a,destination.open(encoding='utf-8-sig',newline='') as b:
            ra=csv.DictReader(a);rb=csv.DictReader(b)
            assert rb.fieldnames[:len(ra.fieldnames)]==ra.fieldnames
            verified=0
            for x,y in zip(ra,rb,strict=True):
                assert all(x[k]==y[k] for k in x);verified+=1
            assert verified==count
        files.append(dict(source=str(path),output=str(destination),rows=count,sha256=digest(destination)))
    checks=json.loads((ENRICHED/'preservation_manifest.json').read_text(encoding='utf-8'))
    checks.update(json.loads((OUT/'input_checksums.json').read_text(encoding='utf-8')))
    changed=[p for p,h in checks.items() if digest(Path(p))!=h]
    assert not changed,'Protected input checksum changed: '+str(changed)
    named=sum(bool(a['institution_as_written']) for a in ledger);assert len(ledger)==292961 and named==286848
    assert len(byname)==16525 and sum(r['awards'] for r in byname.values())==named
    assert len(files)==5
    write_json(OUT/'validation_report.json',dict(passed=True,protected_files=len(checks),unchanged_protected_checksums=True,reference_integrity=True,
       unique_names=len(byname),requested_awards=len(ledger),institution_bearing_awards=named,complete_development_reviews=len(dev_ds),
       independent_final_reviews=len(final_ds),source_columns_rows_order_duplicates_preserved=True,award_arrays_aligned=True,
       independent_audit_disjoint_from_development=True,exact_final_audit_quotas=True,unique_source_award_positions=len(ledger),
       metrics_and_ledger_counts_reconciled=True,
       all_accepted_locations_from_reference=True,accepted_ids_verified_against_gzip_snapshot=snapshot_ids_checked,
       exact_award_evidence_and_label_offsets_verified=evidence_checked,frozen_rules_unchanged=True,files=files))
    snapshots=OUT/'code_snapshot';snapshots.mkdir(exist_ok=True)
    for name in ('institution_geolocation.py','geolocate_education.py','geolocation_review.py','match_institution_candidates.R','test_institution_geolocation.py','geolocation_regression_cases.json'):
        (snapshots/name).write_bytes((HERE/name).read_bytes())
    write_json(snapshots/'checksums.json',{p.name:digest(p) for p in snapshots.iterdir() if p.name!='checksums.json'})
    report(coverage,metrics,files)
    print(json.dumps(dict(coverage=coverage[0],audit=metrics,files=files)),flush=True)

def report(coverage,metrics,files):
    overall=next(r for r in coverage if r['edition']=='all' and r['family']=='all')
    table='| Edition / degree family | Unique names located / observed | Awards located / institution-bearing | Award coverage |\n|---|---:|---:|---:|\n'
    for r in coverage:
        if r['family']=='all' or r['edition']=='all':
            label=r['edition'] if r['family']=='all' else r['family'].upper().replace('_','/')
            table+=f"| {label} | {r['matched_unique_names']:,} / {r['unique_names']:,} | {r['matched_awards']:,} / {r['institution_bearing_awards']:,} | {r['award_coverage']:.1%} |\n"
    text=f"""# AMWS institution matching and geolocation

Matched {overall['matched_awards']:,} of {overall['institution_bearing_awards']:,} institution-bearing earned BA/BS, MS, MA and PhD awards ({overall['award_coverage']:.1%}), involving {overall['matched_unique_names']:,} of 16,525 literal names ({overall['name_coverage']:.1%}). There are 292,961 requested awards; 6,113 lack a recovered institution.

The main reference is the checksummed GTAllocation OpenAlex snapshot: 23,885 education institutions with valid coordinates, drawn from 120,658 institution records of all types. OpenAlex canonical and alternative names generate exact matches and installed R stringdist computes normalized Damerau–Levenshtein candidates. Crosswalk files master_alias_oa.csv and OA_revelio_shanghai.csv only supply candidates; malformed IDs and unverified aliases cannot authorize matches. Five distinct institution candidates are retained, with a baseline fuzzy threshold 96 and distinct-ID lead 8. Later exact-rule revisions retain those baseline candidate scores and prepend newly verified exact candidates; fuzzy scores are not recalculated using the later normalizations.

Development reviewed the 100 most frequent names, 500 proposed matches (443 exact/57 fuzzy), 250 high-frequency unresolved names and 250 residual random names: 1,018 distinct strings after overlaps. Each decision is an explicit reviewer entry. Later rules add verified aliases, institutional abbreviation expansions, bounded OCR repairs and conservative collision/subunit guards. No birthplace or employment geography identifies an institution. Legitimate conjunctions are retained; multiple reference IDs require explicit joint-award review. All frozen versions and metrics are saved in iterations/. Institution-bearing award coverage rose from 23.46% in the baseline to 60.96% before the independent audit. The last two revisions improved coverage by 0.044 and 0.000 percentage points, triggering the prescribed plateau stop. The corrected development audit retained 464 confirmed accepted mappings, all confirmed after rejected/conditional cases were removed; this is a post-adjudication diagnostic, not an independent precision estimate.

The independent frozen-rule audit reviewed {metrics['accepted_names_audited']} previously unreviewed accepted names and {metrics['unmatched_names_audited']} unresolved names. Name precision was {metrics['name_precision']:.2%} ({metrics['correct_names']}/{metrics['accepted_names_audited']}; 95% Wilson interval {metrics['name_precision_ci95'][0]:.2%}–{metrics['name_precision_ci95'][1]:.2%}); award-weighted precision was {metrics['award_weighted_precision']:.2%} (95% name-cluster bootstrap {metrics['award_weighted_precision_ci95_bootstrap'][0]:.2%}–{metrics['award_weighted_precision_ci95_bootstrap'][1]:.2%}). {metrics['missed_matches']} of the {metrics['unmatched_names_audited']} unresolved audited names were recoverable unconditional missed matches ({metrics['missed_match_rate']:.1%}; 95% Wilson interval {metrics['missed_match_rate_ci95'][0]:.1%}–{metrics['missed_match_rate_ci95'][1]:.1%}), plus {metrics['partial_missed_matches']} conditional cases. The 99% acceptance target {'passed' if metrics['target_met'] else 'failed; unreviewed automatic results remain candidates requiring review'}. Both name and award-weighted point estimates must reach 99%. Individual audit decisions are integrated as reviewed exceptions; frozen rules are unchanged. Conditional, ambiguous and unsupported extractions count as unconfirmed unconditional name mappings, even if a subset of awards can be located. Weighted intervals resample names, not independent copies of every award. Accuracy describes the audited matching decisions, not original regex extraction accuracy.

{table}
These are descriptive counts for the complete enriched ledger. The edition/family combinations and explicit all-award denominators are in coverage_by_edition_degree.csv. They are separate from the sampled accuracy intervals above.

Clear successes include Harvard University, explicitly qualified California/Berkeley, MIT abbreviations and verified Iowa State College historical names. The final audit found concrete collisions: Notre Dame Univ matched the Philippine institution although the education clauses did not distinguish it from the US university; Punjab Univ did not distinguish the Pakistani and Indian institutions; Madison Col had been assigned to Madison Area Technical College rather than the historical Virginia institution. Shared institution inheritance also produced an unsupported 1977 PhD at Wisconsin/Oshkosh: its [first doctoral program was approved in 2010](https://www.uwosh.edu/today/1937/regents-approve-uwos-first-doctoral-program/). That name is conditional on the degree family, with the PhD geocode withheld. Concordia Col, Minn is another historical collision: [Concordia St Paul first awarded bachelor degrees in 1964](https://www.csp.edu/about/history-of-concordia-university-st-paul/), so a reliable earlier award can distinguish the Moorhead institution, whereas later unspecified entries remain ambiguous. Full evidence, reference IDs, decisions and supporting links remain in the review logs.

Other unresolved cases include bare California/Washington, short acronyms, parent/subunit differences, OCR-corrupted labels and institutions absent from the snapshot. Reference city/coordinate errors flagged in manual notes are not repaired with outside coordinates. Institutions lacking an education-type reference ID remain unresolved even if identity can be researched. Remaining coverage should be improved through additional explicit reviews; the failed independent target does not support releasing all automatic candidates as verified geocodes.

coverage_by_edition_degree.csv reports edition/family and total coverage, using unique source-award positions rather than duplicated person-file geographic candidates. unmatched_inventory.csv retains unresolved names, reasons and candidates. resolution_categories.csv separates candidate or reviewed identity ambiguity, reviewed absent reference institutions and invalid extraction from the unclassified residual; unreviewed residual strings have not been assigned invented causes. No selected reference institution lacked coordinates, since acceptance requires valid snapshot coordinates. The award_location_ledger preserves all 292,961 requested award rows with zero/one/multiple reference locations. The five person_files copies append aligned geocode arrays; every original value, row, row order and duplicate candidate is preserved. Existing education/year arrays and estimation panels are unchanged.

Coordinates represent the supplied reference location. Unqualified university identity does not establish a historical teaching campus; old relocations are not reconstructed. Conditional rules use only edition, reliable reported years and the frozen education section. Missing/unresolved/chronology-conflict dates fail year conditions. Ambiguous conditions assigning different institutions remain unresolved. Absence of a match means not recoverable from these data/reference, not absence of education.

Validation: all protected source/pilot/enriched/reference/panel files unchanged; accepted IDs/coordinates verified against reference; arrays reconcile to award positions; five complete copies checked cell-by-cell. See validation_report.json, input_checksums.json, reviewed_crosswalk.csv, final_adjudication_log.json and frozen_rules.json.
"""
    (OUT/'findings.md').write_text(text,encoding='utf-8')
    write_json(OUT/'manifest.json',dict(reference='reference_v1.json',main_reference='GTAllocation OpenAlex institution snapshot',files=files,coverage=overall,audit=metrics))

if __name__=='__main__':
    ap=argparse.ArgumentParser();ap.add_argument('command',choices=['freeze','publish']);a=ap.parse_args()
    {'freeze':freeze,'publish':publish}[a.command]()

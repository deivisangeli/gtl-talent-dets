"""Separate post-audit revision; never overwrite geo_v05 or its audit.

Regenerate normalized candidates using the same OpenAlex snapshot. Explicit
reviews live in separate rev2 JSONL files. All outputs are revision-versioned.
"""
import argparse,collections,csv,json,os,random,re,subprocess
from pathlib import Path
import geolocate_education as old
from institution_geolocation import STOP,grams,read_csv,write_csv,write_json,digest,target_awards

BASE=old.OUT
OUT=BASE/'revision_v2'
EXTRA={'sdak':'south dakota','ndak':'north dakota','dak':'dakota',
 'nhamp':'new hampshire','nh':'new hampshire','njer':'new jersey','nj':'new jersey',
 'ncar':'north carolina','scar':'south carolina','nc':'north carolina','sc':'south carolina',
 'nd':'north dakota','sd':'south dakota','sask':'saskatchewan',
 'n mex':'new mexico','nmex':'new mexico','n mexic':'new mexico',
 'n dak':'north dakota','s dak':'south dakota','n car':'north carolina','s car':'south carolina',
 'n hamp':'new hampshire','n j':'new jersey','n h':'new hampshire',
 'w va':'west virginia','wva':'west virginia','w virg':'west virginia',
 's dakota':'south dakota','n dakota':'north dakota'}

def normalize(text):
    q=old.normalize(text,True)
    institutional=bool(re.search(r'\b(?:university|college|institute|state|school|polytechnic)\b',q))
    if institutional:
        for a,b in sorted(EXTRA.items(),key=lambda x:-len(x[0])):
            q=re.sub(r'\b'+re.escape(a)+r'\b',b,q)
    return q

def existing_reviews():
    ds=old.reviews()[0];ds.update(old.reviews('final')[0]);return ds

def revision_reviews(final=False):
    ds={}
    prefix='rev2_final_' if final else 'rev2_'
    for batch in 'ABCD':
        for suffix in ('','.corrections'):
            path=BASE/f'review_{prefix}{batch}{"_corrections" if suffix else ""}.jsonl'
            if not path.exists():continue
            for line in path.read_text(encoding='utf-8').splitlines():
                d=json.loads(line).get('replacement_decision') or json.loads(line)
                assert d['manual_review'];ds[d['name_id']]=d
    return ds

def source_rows():
    return json.loads((BASE/'published_crosswalk.json').read_text(encoding='utf-8'))

def generate_candidates():
    OUT.mkdir(parents=True,exist_ok=True)
    protected=[BASE/'frozen_rules.json',BASE/'final_audit_metrics.json',BASE/'published_crosswalk.json',BASE/'reference_v1.json']
    checks={str(p):digest(p) for p in protected}
    if (OUT/'prior_checksums.json').exists():assert json.loads((OUT/'prior_checksums.json').read_text(encoding='utf-8'))==checks
    else:write_json(OUT/'prior_checksums.json',checks)
    records=old.reference_records();aliases=collections.defaultdict(dict);exact=collections.defaultdict(set)
    tokens=collections.defaultdict(set);trigrams=collections.defaultdict(set)
    for a in read_csv(BASE/'reference_aliases.csv'):
        if records[a['id']]['type']!='education':continue
        q=normalize(a['alias'])
        if not q:continue
        auto=a['automatic']=='True';prior=aliases[a['id']].get(q)
        if prior is None or auto and not prior['automatic']:
            aliases[a['id']][q]=dict(id=a['id'],alias=a['alias'],source=a['source'],automatic=auto,reviewed=a['reviewed']=='True',normalized=q)
        exact[q].add(a['id'])
        # Whole normalized token sequences in either order also generate IDs.
        exact[' '.join(sorted(q.split()))].add(a['id'])
        for token in set(q.split())-STOP:tokens[token].add(a['id'])
        for gram in grams(q):trigrams[gram].add(a['id'])
    fields=['name_id','id','alias','source','automatic','reviewed','query','candidate','query_sorted','candidate_sorted']
    count=0
    with (OUT/'candidate_pairs.csv').open('w',encoding='utf-8-sig',newline='') as f:
        writer=csv.DictWriter(f,fieldnames=fields);writer.writeheader()
        for r in source_rows():
            q=normalize(r['name']);scores=collections.Counter();ids=set(exact[q])|exact[' '.join(sorted(q.split()))]
            for gram in grams(q):
                if len(trigrams[gram])<3000:scores.update(trigrams[gram])
            for token in set(q.split())-STOP:
                if len(tokens[token])<=500:ids.update(tokens[token])
            ids.update(id for id,_ in scores.most_common(100))
            for id in sorted(ids):
                for a in aliases[id].values():
                    writer.writerow(dict(name_id=r['name_id'],id=id,alias=a['alias'],source=a['source'],automatic=a['automatic'],reviewed=a['reviewed'],query=q,candidate=a['normalized'],query_sorted=' '.join(sorted(q.split())),candidate_sorted=' '.join(sorted(a['normalized'].split()))));count+=1
    print('Revision candidate alias comparisons:',count,flush=True)
    subprocess.run([os.environ.get('RSCRIPT_PATH','C:/Program Files/R/R-4.5.2/bin/Rscript.exe'),str(old.HERE/'match_institution_candidates.R'),str(OUT)],check=True)

def propose(decisions=None):
    decisions=decisions or existing_reviews();records=old.reference_records();scores=collections.defaultdict(list)
    for c in read_csv(OUT/'candidate_scores.csv'):
        c['score']=float(c['score']);c['automatic']=c['automatic']=='TRUE';c['reviewed']=c['reviewed']=='TRUE';scores[c['name_id']].append(c)
    # Propagate only explicit unambiguous reviewed identities, with collisions
    # checked against both other reviews and exact OpenAlex aliases.
    verified=collections.defaultdict(set);blocked=set();exact=collections.defaultdict(set)
    for a in read_csv(BASE/'reference_aliases.csv'):
        if a['automatic']=='True' and records[a['id']]['type']=='education':
            exact[' '.join(sorted(normalize(a['alias']).split()))].add(a['id'])
    original=source_rows()
    for r in original:
        d=decisions.get(r['name_id']);key=' '.join(sorted(normalize(r['name']).split()))
        if d:
            if d['scope']=='global' and d['accepted_ids']:verified[key].add(tuple(sorted(d['accepted_ids'])))
            else:blocked.add(key)
    for r in original:
        q=normalize(r['name']);key=' '.join(sorted(q.split()));cs=scores[r['name_id']];d=decisions.get(r['name_id'])
        r.update(candidates=cs,normalized_versioned=q,rule_version='geo_v06',conditions=[],accepted_ids=[],flags=[],method='recomputed_DL')
        if d:
            r['accepted_ids']=d['accepted_ids'] if d['scope']=='global' else [];r['conditions']=d.get('conditions',[])
            r['status']='reviewed_match' if r['accepted_ids'] else 'conditional' if r['conditions'] else d['verdict']
            r['flags']=[] if r['accepted_ids'] else [d['error_cause'] or d['verdict']];r['method']='explicit_review'
        elif key in blocked or len(verified[key])>1:
            r['status']='ambiguous';r['flags']=['reviewed_identity_collision']
        elif len(verified[key])==1 and (not exact[key] or exact[key]<=set(next(iter(verified[key])))):
            r['accepted_ids']=list(next(iter(verified[key])));r['status']='auto_exact';r['method']='verified_alias_normalization'
        else:
            best=cs[0] if cs else None;margin=best['score']-(cs[1]['score'] if len(cs)>1 else 0) if best else 0
            flags=old.quality_flags(r['name'],records[best['id']],None) if best else ['no_candidate']
            if re.search(r'\buniversity (?:111|iii)\b',old.norm(r['name'])):
                flags=[f for f in flags if f!='damaged_or_extra_text']
            unique_exact=best and best['automatic'] and best['score']==100 and len(exact[key])==1
            acceptable=best and best['automatic'] and not flags and (unique_exact or best['score']>=96 and margin>=8)
            r['accepted_ids']=[best['id']] if acceptable else []
            r['status']='auto_exact' if acceptable and best['score']==100 else 'auto_fuzzy' if acceptable else 'ambiguous' if best and best['score']>=85 else 'unmatched'
            r['flags']=flags;r['margin']=margin
        if r['accepted_ids']:r['flags']+=['current_reference_location','historical_campus_not_reconstructed']
    return original

def review_batches(rows,ids,prefix):
    records=old.reference_records();wanted={r['name']:r for r in rows if r['name_id'] in ids}
    examples=collections.defaultdict(lambda:collections.defaultdict(list))
    for a in target_awards():
        if a['institution_as_written'] not in wanted:continue
        group=examples[a['institution_as_written']][a['edition']]
        if len(group)<2 and not any(e['source_key']==a['source_key'] for e in group):
            group.append({k:a[k] for k in ('source_key','edition','requested_family','label_as_written','year_raw','year','evidence','flags')})
    contexts=old.education_contexts({e['source_key'] for eds in examples.values() for es in eds.values() for e in es})
    batches={k:[] for k in 'ABCD'}
    selected=sorted(wanted.values(),key=lambda r:(-r['awards'],r['name_id']))
    for j,r in enumerate(selected):
        entry=dict(r);entry['evidence']=[dict(e,education_context=contexts[e['source_key']]) for ed in sorted(examples[r['name']]) for e in examples[r['name']][ed]]
        entry['candidates']=[dict(c,reference=records[c['id']]) for c in r['candidates']]
        batches['ABCD'[j%4]].append(entry)
    for key,batch in batches.items():write_json(BASE/f'review_batch_{prefix}{key}.json',batch)
    print('Review batches',prefix,{k:len(v) for k,v in batches.items()},flush=True)

def select_development():
    prior=source_rows();rows=propose() if (OUT/'candidate_scores.csv').exists() else prior
    unresolved=[r for r in prior if not r['accepted_ids'] and not r['conditions']]
    high=sorted(unresolved,key=lambda r:(-r['awards'],r['name_id']))[:225]
    selected={r['name_id'] for r in high};rng=random.Random(4042)
    added=[r for r in rows if (r['accepted_ids'] or r.get('candidate_ids')) and r['name_id'] not in existing_reviews() and r['name_id'] not in selected]
    rng.shuffle(added);selected.update(r['name_id'] for r in added[:75])
    if len(selected)<300:
        residual=[r for r in unresolved if r['name_id'] not in selected];rng.shuffle(residual);selected.update(r['name_id'] for r in residual[:300-len(selected)])
    assert len(selected)==300
    write_json(OUT/'development_selection.json',dict(seed=4042,selected_ids=sorted(selected),frequent_names=225,random_proposed_names=75))
    write_json(OUT/'baseline_predictions.json',rows)
    review_batches(rows,selected,'rev2_')

if __name__=='__main__':
    ap=argparse.ArgumentParser();ap.add_argument('command',choices=['candidates','development']);args=ap.parse_args()
    {'candidates':generate_candidates,'development':select_development}[args.command]()

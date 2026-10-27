"""Auditable institution resolution against the local GTAllocation reference."""
import argparse, collections, csv, gzip, json, math, os, random, re, subprocess, unicodedata
from pathlib import Path
from functools import lru_cache
from pilot_io import DATA_ROOT, digest, read_csv, write_csv, write_json

HERE = Path(__file__).resolve().parent
OUT = DATA_ROOT/'output/amws/education_geolocated'
ALLOCATION = Path(os.environ.get('GTALLOCATION_DATA_DIR',str(Path.home()/'Globtalent Dropbox/GTAllocation')))
ENRICHED = DATA_ROOT/'output/amws/education_enriched'
FAMILIES = ('ba_bs','ms','ma','phd')
ABBREVIATIONS = {'univ':'university','col':'college','inst':'institute','technol':'technology'}
STOP = set('university college institute school technology the of and for at state national central medical science sciences education polytechnic technical agricultural international new saint st hospital'.split())

def norm(s):
    s=unicodedata.normalize('NFKD',s or '').casefold().replace('&',' and ')
    s=''.join(c for c in s if not unicodedata.combining(c))
    tokens=re.findall(r'[^\W_]+',s,flags=re.UNICODE)
    return ' '.join(ABBREVIATIONS.get(t,t) for t in tokens if t not in ('the','of'))

def oid(s):
    m=re.fullmatch(r'(?:https://openalex.org/)?(I\d+)',s or '')
    return m.group(1) if m else None

def coordinate(g):
    a,b=g.get('latitude'),g.get('longitude')
    return isinstance(a,(int,float)) and isinstance(b,(int,float)) and math.isfinite(a) and math.isfinite(b) and -90<=a<=90 and -180<=b<=180 and (a,b)!=(0,0)

def target_awards():
    with (ENRICHED/'education_awards.csv').open(encoding='utf-8-sig',newline='') as f:
        for r in csv.DictReader(f):
            if r['requested_family'] in FAMILIES and r['family_award_index'] and r['honorary']=='False':yield r

def prepare():
    OUT.mkdir(parents=True,exist_ok=True)
    manifest_path=OUT/'input_checksums.json'
    paths=list((ALLOCATION/'Data/external/oa_snapshot/data/institutions').rglob('*.gz'))
    paths += [ALLOCATION/'Data/prep/crosswalks'/f for f in ('master_alias_oa.csv','OA_revelio_shanghai.csv')]
    paths += [p for p in ENRICHED.rglob('*') if p.is_file()]
    paths += [p for p in (DATA_ROOT/'output/amws/education_pilot').rglob('*') if p.is_file()]
    paths += [p for p in (DATA_ROOT/'output').rglob('*.csv') if 'amws' in p.name.lower() and any(x in p.name.lower() for x in ('panel','county','state')) and OUT not in p.parents and ENRICHED not in p.parents]
    checks={str(p):digest(p) for p in sorted(set(paths))}
    if manifest_path.exists():assert json.loads(manifest_path.read_text(encoding='utf-8'))==checks,'Protected inputs changed'
    else:write_json(manifest_path,checks)
    records={}; aliases=[]
    for p in sorted((ALLOCATION/'Data/external/oa_snapshot/data/institutions').rglob('*.gz')):
        with gzip.open(p,'rt',encoding='utf-8') as f:
            for line in f:
                d=json.loads(line);id=oid(d['id']);g=d.get('geo') or {}
                assert id not in records,'Duplicate reference ID'
                records[id]=dict(id=id,name=d['display_name'],type=d['type'],country=d.get('country_code'),
                    city=g.get('city'),region=g.get('region'),latitude=g.get('latitude'),longitude=g.get('longitude'),
                    valid_coordinates=coordinate(g),ror=d.get('ror'),parents=d.get('lineage') or [],
                    is_super_system=d.get('is_super_system',False),geonames_city_id=g.get('geonames_city_id'),
                    updated_date=d.get('updated_date'),source_file=str(p))
                for kind,names in [('canonical',[d['display_name']]),('alternative',d.get('display_name_alternatives') or []),('acronym',d.get('display_name_acronyms') or [])]:
                    for name in names:
                        if norm(name):aliases.append(dict(id=id,alias=name,normalized=norm(name),source='openalex_'+kind,reviewed=True,automatic=kind!='acronym'))
    rejected=[]
    for file in paths:
        if file.name not in ('master_alias_oa.csv','OA_revelio_shanghai.csv'):continue
        for r in read_csv(file):
            id=oid(r.get('oa_id',r.get('OA_id')))
            name=r.get('alias_example',r.get('university'))
            if not id or id not in records:
                rejected.append(dict(source=file.name,alias=name,reference_id=r.get('oa_id',r.get('OA_id')),reason='malformed_or_absent_id'));continue
            # Allocation crosswalks sometimes intentionally collapse campuses;
            # review status alone cannot authorize institution geography.
            aliases.append(dict(id=id,alias=name,normalized=norm(name),source=file.name,
                reviewed=r.get('reviewed')=='TRUE',automatic=False))
            alt=oid(r.get('oa_id_alt',r.get('OA_id_alt')))
            if alt and alt in records:aliases.append(dict(id=alt,alias=name,normalized=norm(name),source=file.name+'_alternative',reviewed=False,automatic=False))
    uniq={(a['id'],a['alias'],a['source']):a for a in aliases}
    aliases=list(uniq.values())
    write_json(OUT/'reference_v1.json',dict(records=records,aliases=aliases))
    write_csv(OUT/'reference_institutions.csv',records.values())
    write_csv(OUT/'reference_aliases.csv',aliases)
    write_csv(OUT/'rejected_reference_aliases.csv',rejected)
    freq=collections.Counter(); evidence=collections.defaultdict(list); editions=collections.defaultdict(set);families=collections.defaultdict(set)
    for r in target_awards():
        s=r['institution_as_written']
        if not s:continue
        freq[s]+=1;editions[s].add(r['edition']);families[s].add(r['requested_family'])
        ev={k:r[k] for k in ('source_key','edition','requested_family','label_as_written','year_raw','year','evidence','flags')}
        if len(evidence[s])<6 and not any(v['evidence']==ev['evidence'] and v['edition']==ev['edition'] for v in evidence[s]):evidence[s].append(ev)
    inventory=[dict(name_id=f'N{i:05}',name=s,normalized=norm(s),awards=freq[s],editions=sorted(editions[s]),families=sorted(families[s]),evidence=evidence[s]) for i,s in enumerate(sorted(freq),1)]
    assert len(inventory)==16525 and sum(freq.values())==286848
    write_json(OUT/'institution_inventory.json',inventory)
    write_csv(OUT/'institution_inventory.csv',[{**r,'editions':json.dumps(r['editions']),'families':json.dumps(r['families']),'evidence':json.dumps(r['evidence'],ensure_ascii=False)} for r in inventory])
    print(json.dumps(dict(reference_ids=len(records),education_ids=sum(r['type']=='education' and r['valid_coordinates'] for r in records.values()),aliases=len(aliases),names=len(inventory))))

@lru_cache(maxsize=1)
def catalog():return json.loads((OUT/'reference_v1.json').read_text(encoding='utf-8'))
def inventory():return json.loads((OUT/'institution_inventory.json').read_text(encoding='utf-8'))

def grams(s):
    s=s.replace(' ','')
    return set(s[i:i+3] for i in range(len(s)-2))

def candidate_pairs():
    cat=catalog(); records=cat['records'];aliases=cat['aliases']
    byid=collections.defaultdict(dict); exact=collections.defaultdict(set); tokens=collections.defaultdict(set);trigrams=collections.defaultdict(set)
    for a in aliases:
        old=byid[a['id']].get(a['normalized'])
        if old is None or (a['automatic'],a['source']=='openalex_canonical')>(old['automatic'],old['source']=='openalex_canonical'):
            byid[a['id']][a['normalized']]=a
        exact[a['normalized']].add(a['id'])
        if records[a['id']]['type']=='education':
            for t in set(a['normalized'].split())-STOP:
                tokens[t].add(a['id'])
            for g in grams(a['normalized']):trigrams[g].add(a['id'])
    fields=['name_id','id','alias','source','automatic','reviewed','query','candidate','query_sorted','candidate_sorted']
    count=0
    with (OUT/'candidate_pairs.csv').open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=fields);w.writeheader()
        for r in inventory():
            q=r['normalized'];scores=collections.Counter()
            for g in grams(q):
                if len(trigrams[g])<3000:
                    for id in trigrams[g]:scores[id]+=1
            ids=set(exact[q])
            for t in set(q.split())-STOP:
                if len(tokens[t])<=500:ids.update(tokens[t])
            ids.update(id for id,_ in scores.most_common(80))
            for id in sorted(ids):
                # Distinct alias score reduction happens in R before top5 IDs.
                for a in byid[id].values():
                    w.writerow(dict(name_id=r['name_id'],id=id,alias=a['alias'],source=a['source'],automatic=a['automatic'],reviewed=a['reviewed'],query=q,candidate=a['normalized'],query_sorted=' '.join(sorted(q.split())),candidate_sorted=' '.join(sorted(a['normalized'].split()))));count+=1
    print('candidate alias pairs',count,flush=True)
    subprocess.run([os.environ.get('RSCRIPT_PATH','C:/Program Files/R/R-4.5.2/bin/Rscript.exe'),str(HERE/'match_institution_candidates.R'),str(OUT)],check=True)

def quality_flags(name,record,cat):
    q=norm(name); ts=q.split(); flags=[]
    places={norm(record.get(k) or '') for k in ('city','region','country')}
    # Preserve geographic short forms even if the reference calls one campus
    # by a statewide name. Country/region/city alone never identify a university.
    if q in places or q in {'california','washington','miami','london','paris','berlin','vienna','zurich','dublin','illinois','wisconsin','michigan','pennsylvania','minnesota','texas','iowa','indiana','ohio','missouri','kansas','georgia','florida','arizona','alabama','tennessee','kentucky','nebraska','colorado','oregon','oklahoma','delaware','maryland','maine','montana','wyoming','utah','nevada','vermont','virginia','carolina','dakota','cincinnati','chicago','rochester','boston','buffalo','toronto','ottawa','montreal'}:flags.append('bare_geographic_name')
    if len(q.replace(' ',''))<=4 or (name.isupper() and len(ts)==1):flags.append('short_or_acronym')
    if re.search(r'[0-9<>=^\ufffd$@]|\b(?:prof|dept|diploma|laboratory|grad|res|fellow)\b',name,re.I):flags.append('damaged_or_extra_text')
    if record['type']!='education':flags.append('non_education_entity')
    if record.get('is_super_system'):flags.append('university_system')
    if not record['valid_coordinates']:flags.append('missing_coordinates')
    if any(t in ts for t in ('campus','branch','center','centre','medical','sch','hospital')):flags.append('campus_or_subunit')
    return flags

def baseline():
    cat=catalog();byid=cat['records']; inv=inventory();scores=collections.defaultdict(list)
    for c in read_csv(OUT/'candidate_scores.csv'):
        c['score']=float(c['score']);c['automatic']=c['automatic']=='TRUE';c['reviewed']=c['reviewed']=='TRUE'
        scores[c['name_id']].append(c)
    rows=[]
    for r in inv:
        cs=scores[r['name_id']];best=cs[0] if cs else None
        margin=best['score']-(cs[1]['score'] if len(cs)>1 else 0) if best else 0
        flags=quality_flags(r['name'],byid[best['id']],cat) if best else ['no_candidate']
        automatic=bool(best and best['automatic'] and best['score']>=96 and margin>=8 and not flags)
        status='auto_exact' if automatic and best['score']==100 else 'auto_fuzzy' if automatic else 'ambiguous' if best and best['score']>=85 else 'unmatched'
        rows.append(dict(**r,status=status,accepted_ids=[best['id']] if automatic else [],candidates=cs,flags=flags,margin=margin,method='exact' if best and best['score']==100 else 'fuzzy',rule_version='geo_v01'))
    save_iteration('geo_v01',rows,'Unique full aliases or DL>=96 with an 8-point margin; guarded names remain candidates.')
    select_review(rows)

def save_iteration(version,rows,note):
    folder=OUT/'iterations'/version;folder.mkdir(parents=True,exist_ok=True)
    write_json(folder/'predictions.json',rows)
    write_json(folder/'rules.json',dict(version=version,normalization=ABBREVIATIONS,minimum_score=96,minimum_margin=8,note=note))
    write_csv(folder/'predictions.csv',[{**{k:v for k,v in r.items() if k not in ('evidence','candidates')},'accepted_ids':json.dumps(r['accepted_ids']),'flags':json.dumps(r['flags']),'candidates':json.dumps(r['candidates'],ensure_ascii=False)} for r in rows])
    summary=dict(version=version,names=len(rows),institution_awards=sum(r['awards'] for r in rows),matched_names=sum(bool(r['accepted_ids']) for r in rows),matched_awards=sum(r['awards'] for r in rows if r['accepted_ids']),statuses=dict(collections.Counter(r['status'] for r in rows)))
    write_json(folder/'metrics.json',summary)
    print(json.dumps(summary),flush=True)

def select_review(rows):
    rng=random.Random(42); selected={};reasons=collections.defaultdict(list)
    def take(items,reason,n):
        for r in items[:n]:selected[r['name_id']]=r;reasons[r['name_id']].append(reason)
    take(sorted(rows,key=lambda r:(-r['awards'],r['name_id'])),'top_100',100)
    exact=[r for r in rows if r['status']=='auto_exact'];fuzzy=[r for r in rows if r['status']=='auto_fuzzy']
    rng.shuffle(exact);rng.shuffle(fuzzy)
    ne=min(250,len(exact));nf=min(250,len(fuzzy));extra=500-ne-nf
    ne+=min(extra,len(exact)-ne);extra=500-ne-nf;nf+=min(extra,len(fuzzy)-nf)
    take(exact,'development_exact',ne);take(fuzzy,'development_fuzzy',nf)
    unresolved=[r for r in rows if not r['accepted_ids']]
    high=sorted(unresolved,key=lambda r:(-r['awards'],r['name_id']))
    take(high,'unresolved_high_frequency',250)
    residual=[r for r in unresolved if r['name_id'] not in {x['name_id'] for x in high[:250]}]
    rng.shuffle(residual);take(residual,'unresolved_random',250)
    # Frequency-sort then round-robin: every reviewer gets clear/ambiguous cases.
    cat=catalog()['records'];batches={k:[] for k in 'ABCD'}
    # Spread contexts across editions rather than using the first ledger rows.
    selected_names={r['name']:r for r in selected.values()}
    examples=collections.defaultdict(lambda:collections.defaultdict(list))
    for a in target_awards():
        if a['institution_as_written'] not in selected_names:continue
        group=examples[a['institution_as_written']][a['edition']]
        if len(group)<2 and not any(e['source_key']==a['source_key'] for e in group):
            group.append({k:a[k] for k in ('source_key','edition','requested_family','label_as_written','year_raw','year','evidence','flags')})
    for name,r in selected_names.items():r['evidence']=[e for ed in sorted(examples[name]) for e in examples[name][ed]]
    needed={e['source_key'] for r in selected.values() for e in r['evidence']}
    contexts={}
    with (ENRICHED/'source_biographies.csv').open(encoding='utf-8-sig',newline='') as f:
        for row in csv.DictReader(f):
            if row['source_key'] in needed:
                from education_regex import bounds
                text=row['raw_text_adjusted'];region,boundary_flags=bounds(text,int(row['edition']))
                contexts[row['source_key']]=(text[slice(*region)] if region else '',boundary_flags)
    for i,r in enumerate(sorted(selected.values(),key=lambda r:(-r['awards'],r['name_id']))):
        entry=dict(**r,review_reasons=reasons[r['name_id']])
        entry['evidence']=[dict(e,education_context=contexts[e['source_key']][0],boundary_flags=contexts[e['source_key']][1]) for e in r['evidence']]
        entry['candidates']=[dict(**c,reference=cat[c['id']]) for c in r['candidates']]
        batches['D' if i<100 else 'ABC'[(i-100)%3]].append(entry)
    for k,b in batches.items():write_json(OUT/f'review_batch_{k}.json',b)
    write_json(OUT/'review_selection.json',dict(seed=42,selected_ids=sorted(selected),counts={k:len(b) for k,b in batches.items()},reasons=dict(reasons)))
    print('Review batches', {k:len(b) for k,b in batches.items()})

if __name__=='__main__':
    ap=argparse.ArgumentParser();ap.add_argument('command',choices=['prepare','candidates','baseline']);a=ap.parse_args()
    {'prepare':prepare,'candidates':candidate_pairs,'baseline':baseline}[a.command]()

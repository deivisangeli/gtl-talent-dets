"""Reproducible balanced sample and linked-record stratified split."""
import collections, random, re, json
from pathlib import Path
from pilot_io import *

EDITIONS=[1906,1938,1955,1986]
def key86(r): return '1986:'+':'.join(r[k] for k in ['doc_id','lineid','entry_instance'])
def normalized(x): return re.sub('[^a-z]','',x.lower())
def allocate_linked(groups,rows,seeded_hold,quota=50):
    """Pure-Python constrained split; singleton rows complete each quota."""
    linked=sorted((sorted(g) for g in groups if len(g)>1),key=lambda g:g[0])
    singles={ed:sorted(g[0] for g in groups if len(g)==1 and int(rows[g[0]]['edition'])==ed) for ed in EDITIONS}
    vectors=[tuple(sum(int(rows[i]['edition'])==ed for i in g) for ed in EDITIONS) for g in linked]
    remaining_max=[None]*(len(linked)+1);remaining_max[-1]=tuple(len(singles[ed]) for ed in EDITIONS)
    for j in range(len(linked)-1,-1,-1):remaining_max[j]=tuple(a+b for a,b in zip(remaining_max[j+1],vectors[j]))
    failed=set()
    def choose(j,left):
        if any(n<0 or n>m for n,m in zip(left,remaining_max[j])):return None
        if j==len(linked):return []
        if (j,left) in failed:return None
        g=linked[j];pref=sorted([0,1],key=lambda x:(sum((i in seeded_hold)!=bool(x) for i in g),-x))
        for x in pref:
            rest=tuple(n-x*v for n,v in zip(left,vectors[j]));tail=choose(j+1,rest)
            if tail is not None:return [x]+tail
        failed.add((j,left));return None
    assignment=choose(0,(quota,)*4)
    assert assignment is not None,'Cannot allocate linked groups with exact edition quotas'
    hold={i for g,x in zip(linked,assignment) if x for i in g}
    for ed in EDITIONS:
        need=quota-sum(int(rows[i]['edition'])==ed for i in hold)
        pool=sorted(singles[ed],key=lambda i:(i not in seeded_hold,i));hold.update(pool[:need])
    return hold
def prepare():
    OUT.mkdir(parents=True,exist_ok=True)
    src={ed:DATA_ROOT/'input/amws'/f'amws_{ed}.{ "xlsx" if ed==1906 else "dta" }' for ed in EDITIONS[:-1]}
    src[1986]=DATA_ROOT/'Data/intermediary/amws/amws_ed86_full.csv'
    previous=DATA_ROOT/'output/amws/education_pilot/sample_1000.csv'
    excluded={key86(r) for r in read_csv(previous)}
    hashes={str(p):digest(p) for p in [*src.values(),previous]}
    for ed in EDITIONS[:-1]:
        p=DATA_ROOT/'output/amws'/('amws_1955_split_corrected.csv' if ed==1955 else f'amws_{ed}_cleaned_corrected.csv')
        hashes[str(p)]=digest(p)
    rng=random.Random(42); selected=[]; frames={}
    for ed in EDITIONS:
        if ed==1986:
            rows=[]
            for n,r in enumerate(read_csv(src[ed]),1):
                key=key86(r)
                if key not in excluded:
                    rows.append(dict(edition=ed,source_key=key,lineid=r['lineid'],source_row=n,name_raw=r['name_raw'],birth_year=r['birth_year'],
                                     raw_text=r['raw_text'],raw_text_adjusted=r['raw_text_adjusted'],encoding_note='UTF-8 as read'))
            original_n=len(rows)+len(excluded)
        else:
            rows=read_csv(OUT/'ingestion'/f'source_{ed}.csv'); original_n=len(rows)
        rows.sort(key=lambda r:r['source_key'])
        assert len({r['source_key'] for r in rows})==len(rows)
        frames[str(ed)]=dict(full_records=original_n,eligible_for_new_sample=len(rows),excluded_previous_sample=original_n-len(rows))
        draw=rng.sample(rows,250)
        for r in draw:
            r=dict(r); r['sample_id']=len(selected)+1; r['source_file']=str(src[ed]); selected.append(r)
    # Stable IDs remain tied to random draw order; save shuffled split separately.
    fields=['sample_id','edition','source_key','source_file','source_row','lineid','name_raw','birth_year','raw_text','raw_text_adjusted','encoding_note']
    path=OUT/'sample_1000.csv'
    if path.exists():
        assert [{k:str(r.get(k,'')) for k in fields} for r in selected]==read_csv(path),'Sample already exists with different inputs'
    else: write_csv(path,selected,fields)
    rows={r['sample_id']:r for r in selected}; parent={i:i for i in rows}
    def find(i):
        while parent[i]!=i: parent[i]=parent[parent[i]]; i=parent[i]
        return i
    def union(a,b): parent[find(b)]=find(a)
    links=[]; bytext={}; byperson={}; byweak=collections.defaultdict(list)
    for i,r in rows.items():
        text=normalized(r['raw_text_adjusted']); name=normalized(r['name_raw']); by=str(r['birth_year'])
        if text and len(text)>80:
            if text in bytext: union(i,bytext[text]); links.append(dict(a=bytext[text],b=i,kind='identical_biography',confident=True))
            else: bytext[text]=i
        person=(name,by)
        if name and by and by!='NA':
            if person in byperson: union(i,byperson[person]); links.append(dict(a=byperson[person],b=i,kind='full_name_birth_year',confident=True))
            else: byperson[person]=i
        parts=r['name_raw'].split(','); last=normalized(parts[0]); first=normalized(parts[1]) if len(parts)>1 else ''
        if last and first: byweak[(last,first[0])].append(i)
    for group in byweak.values():
        for a in group:
            for b in group:
                if a>=b or rows[a]['edition']==rows[b]['edition'] or find(a)==find(b): continue
                try: near=abs(int(rows[a]['birth_year'])-int(rows[b]['birth_year']))<=1
                except ValueError: near=False
                if near: links.append(dict(a=a,b=b,kind='surname_initial_near_birth',confident=False))
    splitrng=random.Random(42); hold=set()
    for ed in EDITIONS:
        ids=sorted(i for i,r in rows.items() if int(r['edition'])==ed); splitrng.shuffle(ids); hold.update(ids[-50:])
    groups=collections.defaultdict(list)
    for i in rows: groups[find(i)].append(i)
    if any(len({i in hold for i in g})>1 for g in groups.values()):
        hold=allocate_linked(list(groups.values()),rows,hold)
    manifest=dict(seed=42,sampling_algorithm='single Random(42), ascending editions, sample(sorted source keys,250)',
                  splitting_algorithm='separate Random(42), shuffle sorted sample IDs per edition, last50 held out; linked-group constrained allocation if needed',
                  frames=frames,source_sha256=hashes,sample_sha256=digest(path),development=sorted(set(rows)-hold),holdout=sorted(hold),links=links)
    mp=OUT/'split_manifest.json'
    if mp.exists(): assert json.loads(mp.read_text(encoding='utf-8'))==manifest
    else: write_json(mp,manifest)
    write_csv(OUT/'split_membership.csv',[dict(sample_id=i,edition=r['edition'],source_key=r['source_key'],partition='holdout' if i in hold else 'development') for i,r in rows.items()])
    for ed in EDITIONS: assert sum(int(rows[i]['edition'])==ed for i in hold)==50
    assert len(selected)==1000 and len(hold)==200
    print(json.dumps({'frames':frames,'linked_records':links,'development':800,'holdout':200}))
if __name__=='__main__': prepare()

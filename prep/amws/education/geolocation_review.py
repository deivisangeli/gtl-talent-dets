"""Explicit reviewer decisions; no generated classifier verdicts."""
import json,re,csv
from functools import lru_cache
from institution_geolocation import OUT

@lru_cache(maxsize=1)
def catalog():
    # Reviewers only need reference records. Avoid loading 392k aliases in
    # each concurrent review process.
    with (OUT/'reference_institutions.csv').open(encoding='utf-8-sig',newline='') as f:
        rows=list(csv.DictReader(f))
    for r in rows:
        r['valid_coordinates']=r['valid_coordinates']=='True'
        r['is_super_system']=r['is_super_system']=='True'
    return {'records':{r['id']:r for r in rows}}

class ReviewBatch:
    def __init__(self,batch):
        self.batch=batch
        self.rows={r['name_id']:r for r in json.loads((OUT/f'review_batch_{batch}.json').read_text(encoding='utf-8'))}
        self.decisions=[]
    def show(self,start=0,count=20):
        rows=list(self.rows.values())
        for i,r in enumerate(rows[start:start+count],start):
            print(f"\n{i} {r['name_id']} {r['name']!r} awards={r['awards']} {r['status']} proposed={r['accepted_ids']} flags={r['flags']}")
            for c in r['candidates']:
                t=c['reference'];print(f"  {c['id']} {c['score']:.2f} {t['name']} ({t['city']}, {t['country']}) via {c['alias']!r} [{c['source']}]")
            for e in r['evidence']:print('  BIO',e['source_key'],e['year'],e.get('education_context',e['evidence']))
    def add(self,name_id,verdict,ids=(),scope='global',cause='',note='',urls=(),conditions=()):
        assert name_id in self.rows
        assert verdict in ('correct','incorrect','resolved','ambiguous','invalid_extraction','absent_reference')
        assert scope in ('global','conditional','unresolved')
        assert not any(d['name_id']==name_id for d in self.decisions)
        if ids:
            refs=catalog()['records']
            assert all(id in refs and refs[id]['valid_coordinates'] for id in ids)
        assert verdict not in ('correct','resolved') or ids or conditions
        assert not conditions or scope=='conditional'
        self.decisions.append(dict(name_id=name_id,reviewer=self.batch,manual_review=True,
            verdict=verdict,accepted_ids=list(ids),scope=scope,error_cause=cause,note=note,
            supporting_urls=list(urls),conditions=list(conditions)))
    def save(self):
        path=OUT/f'review_{self.batch}.jsonl'
        existing=[json.loads(s) for s in path.read_text(encoding='utf-8').splitlines()] if path.exists() else []
        seen={r['name_id'] for r in existing}
        assert not seen & {r['name_id'] for r in self.decisions},'Already reviewed ID'
        with path.open('a',encoding='utf-8',newline='\n') as f:
            for r in self.decisions:f.write(json.dumps(r,ensure_ascii=False)+'\n')
        print('Saved explicit decisions',self.batch,len(self.decisions),'total',len(existing)+len(self.decisions))
    def remaining(self):
        path=OUT/f'review_{self.batch}.jsonl'
        seen={json.loads(s)['name_id'] for s in path.read_text(encoding='utf-8').splitlines()} if path.exists() else set()
        return [r for id,r in self.rows.items() if id not in seen]

def search(pattern,limit=30):
    regex=re.compile(pattern,re.I)
    found=[(id,r['name'],r['city'],r['country'],r['type']) for id,r in catalog()['records'].items() if regex.search(r['name'])]
    print(found[:limit]);print('total',len(found))

"""Explicit manual references for the four-edition pilot (no auto-classification)."""
import csv, json, os, hashlib, re, sys
from pathlib import Path
if hasattr(sys.stdout,'reconfigure'): sys.stdout.reconfigure(encoding='utf-8')

DATA_ROOT = Path(os.environ.get('TALENT_DETS_DATA_DIR', str(Path.home()/'Globtalent Dropbox'/'gtl_talent_dets')))
OUT = DATA_ROOT/'output/amws/education_pilot/multi_edition_regex_pilot'
GROUPS = ['bachelors','other_bachelor','masters_academic','masters_professional','phd']
ALL_GROUPS = GROUPS + ['other_doctorate','other_credential']
def digest(path):
    h=hashlib.sha256()
    with Path(path).open('rb') as f:
        for b in iter(lambda:f.read(1048576),b''): h.update(b)
    return h.hexdigest()
def read_csv(path):
    with Path(path).open(encoding='utf-8-sig',newline='') as f: return list(csv.DictReader(f))
def write_csv(path,rows,fields=None):
    Path(path).parent.mkdir(parents=True,exist_ok=True)
    fields=fields or list(dict.fromkeys(k for r in rows for k in r)) or ['sample_id']
    with Path(path).open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=fields); w.writeheader(); w.writerows(rows)
def write_json(path,data):
    Path(path).parent.mkdir(parents=True,exist_ok=True)
    Path(path).write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
def sample(): return {int(r['sample_id']):r for r in read_csv(OUT/'sample_1000.csv')}
def show(start,end):
    for i,r in sample().items():
        if start<=i<=end: print(f"\n[{i}] {r['edition']} {r['name_raw']} birth={r['birth_year']}\n{r['raw_text_adjusted']}")
class ManualBatch:
    def __init__(self,reviewer): self.reviewer=reviewer; self.rows=sample(); self.records=[]
    def row(self,i,degrees=(),status=None,ocr=False,overlap=False,note=''):
        """Tuple: family,label,institution,literal_year,resolved_year[,honorary,note,evidence]."""
        r=self.rows[i]; text=r['raw_text_adjusted']; out=[]
        for values in degrees:
            group,label,inst,yr,year=values[:5]
            honorary=values[5] if len(values)>5 else False
            dn=values[6] if len(values)>6 else ''
            supplied=values[7] if len(values)>7 else None
            # Full original biography is acceptable evidence for inherited schools;
            # offsets are deliberately literal and never refer to a normalized string.
            ev=supplied or text
            assert group in ALL_GROUPS and label in ev and (not inst or inst in ev) and (not yr or yr in ev), (i,values)
            start=text.find(ev); assert start>=0
            out.append(dict(group=group,label_as_written=label,institution_as_written=inst,year_raw=yr,
                        year=year,honorary=honorary,note=dn,evidence=ev,evidence_source='raw_text_adjusted',
                        start=start,end=start+len(ev)))
        status=status or ('partial_information' if any(not d['institution_as_written'] or d['year'] is None for d in out if not d['honorary']) else ('degree_information' if out else 'no_degree_information_reported'))
        self.records.append(dict(sample_id=i,edition=int(r['edition']),source_key=r['source_key'],reviewer=self.reviewer,
                                 full_review=True,status=status,ocr_damage=ocr,overlap=overlap,note=note,degrees=out))
    def save(self):
        path=OUT/f'review_{self.reviewer}.jsonl'
        old=[json.loads(s) for s in path.read_text(encoding='utf-8').splitlines()] if path.exists() else []
        ids={r['sample_id'] for r in old}
        for r in self.records:
            assert r['sample_id'] not in ids; ids.add(r['sample_id'])
        with path.open('a',encoding='utf-8',newline='\n') as f:
            for r in self.records: f.write(json.dumps(r,ensure_ascii=False)+'\n')
        print('Saved',len(self.records),'reviews for',self.reviewer,'total',len(ids))
        self.records=[]

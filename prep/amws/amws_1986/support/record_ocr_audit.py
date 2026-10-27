"""Record explicit scan decisions after inspecting the selected evidence image.

This command does not infer or approve a review. The caller supplies the IDs,
verdict and observations after manually comparing the scan and parsed fields.
"""
import argparse,json,sys
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from ocr_supplement import read_csv,sha,write_json,DEFAULT_ROOT

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--stage',type=Path,default=DEFAULT_ROOT/'output/amws/ocr_supplement_20261011')
p.add_argument('--ids',required=True);p.add_argument('--evidence',required=True,type=Path)
p.add_argument('--note',required=True);p.add_argument('--verdict',choices=['passed','corrected'],default='passed')
a=p.parse_args();_,rows=read_csv(a.stage/'audit_sample200.csv');rows={int(r['audit_id']):r for r in rows}
ids=list(range(int(a.ids.split('-')[0]),int(a.ids.split('-')[-1])+1))
out=a.stage/'audit_reviews.json';prior=json.loads(out.read_text(encoding='utf-8')) if out.exists() else []
reviews={r['source_key']:r for r in prior}
assert a.evidence.exists()
for ident in ids:
    r=rows[ident];scan=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed/images'/f'{Path(r["source_file"]).stem}_page_{int(r["pdf_page"]):04d}.png'
    reviews[r['source_key']]=dict(audit_id=ident,source_key=r['source_key'],scan_reviewed=True,
        birth_fields_checked=True,segmentation_checked=True,geocode_checked=True,
        verdict=a.verdict,note=a.note,source_scan=str(scan),scan_sha256=sha(scan),
        evidence_image=str(a.evidence),evidence_sha256=sha(a.evidence),
        name=r['name_raw'],birth_city=r['birth_city'],birth_year=r['birth_year'],geo_geoid=r['geo_geoid'])
write_json(out,sorted(reviews.values(),key=lambda r:r['audit_id']))
print('Recorded',ids,'; total',len(reviews))

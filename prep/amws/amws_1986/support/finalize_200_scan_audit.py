"""Persist the 200 scans inspected in audit_full_01 through audit_full_67.

This source-specific review ledger records the actual audit and its explicit
corrections. It must not be reused to approve a newly selected sample.
"""
import sys,json
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from ocr_supplement import *
s=DEFAULT_ROOT/'output/amws/ocr_supplement_20261011'
_,rows=read_csv(s/'audit_sample200.csv')
selection=json.loads((s/'audit_selection200.json').read_text(encoding='utf-8'))
assert [(r['audit_id'],r['source_key']) for r in rows]==[(r['audit_id'],r['source_key']) for r in selection], 'This record script applies only to the 200 scans actually inspected.'
decisions=json.loads((s/'manual_overrides.json').read_text(encoding='utf-8'));by={r['source_key']:r for r in decisions}
corrections={10,14,54,55,60,78,84,101,106,122,124,128,134,136,137,138,147,151,152,153,154,157,164,167,171,179,180,181,184,196,197,199}
reviews=[]
for r in rows:
 ident=int(r['audit_id']);assert 1<=ident<=200
 evidence=s/'review'/f'audit_full_{(ident-1)//3+1:02d}.png'
 scan=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed/images'/f'{Path(r["source_file"]).stem}_page_{int(r["pdf_page"]):04d}.png'
 note='Compared printed biography boundary, full name, birthplace/date/year and county mapping with source scan.'
 if ident in corrections:note+=' Detected error corrected by explicit source-keyed field or segmentation override.'
 review=dict(audit_id=ident,source_key=r['source_key'],scan_reviewed=True,birth_fields_checked=True,segmentation_checked=True,geocode_checked=True,verdict='corrected' if ident in corrections else 'passed',note=note,source_scan=str(scan),scan_sha256=sha(scan),evidence_image=str(evidence),evidence_sha256=sha(evidence),fields={c:r[c] for c in ('name_raw','birth_place','birth_date','birth_year','birth_city','birth_state','birth_country','geo_lat','geo_lon','geo_geoid','geo_county_name')})
 reviews.append(review)
 decision=by.get(r['source_key'],dict(source_key=r['source_key'],manual_action='no_change',manual_confidence='high',values={}))
 decision.update(manual_note=note,evidence=f'Audit {ident}: {scan}; full scan contact sheet {evidence}',reviewed_reasons=['*'],source_scan=str(scan),scan_sha256=sha(scan))
 by[r['source_key']]=decision
write_json(s/'audit_reviews.json',reviews);write_json(s/'manual_overrides.json',list(by.values()))
write_json(s/'audit_findings.json',dict(audited=200,records_corrected=len(corrections),corrected_audit_ids=sorted(corrections),observations=['Clipped surnames and OCR character confusions: related records now flagged for source review.','Incomplete biography and garbled notice attached to preceding entry: split explicitly; no-birth-field punctuation variants now recognized.','ZUMOFF garbled header attached to ZUMINO: source-specific transcription.','NY county assignment: corrected to existing 36061 convention; other conflicts quarantined.','Birth date/year recognition error and city spelling errors: explicit source transcriptions.']))
print('Recorded 200 scan reviews,',len(corrections),'records with corrections.')

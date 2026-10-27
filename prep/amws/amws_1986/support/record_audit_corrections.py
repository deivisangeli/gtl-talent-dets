"""Explicit transcriptions found in the manually viewed 200-record scan audit.

No surname reconstruction or broad birthplace inference is applied here.
"""
import sys,json
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from ocr_supplement import *
s=DEFAULT_ROOT/'output/amws/ocr_supplement_20261011'
_,rows=read_csv(s/'audit_sample200.csv');rows={int(r['audit_id']):r for r in rows}
names={14:'HEATH, CLARK WRIGHT, JR',54:'LENGYEL, PETER',55:'LEOPOLD, ESTELLA A(BERGERE)',60:'LITTLEDIKE, ERNEST TRAVIS',78:'MUKHEDKAR, DINKAR',84:'MURRELL, LEONARD RICHARD',101:'NOLTMANN, ERNST AUGUST',106:"O'FLAHERTY, LARRANCE MICHAEL ARTHUR",122:'PALLONE, ADRIAN JOSEPH',124:'PANNELL, KEITH HOWARD',128:'PATEL, APPASAHEB RAOJIBHAI',136:'SCHAEFER, PHILIP WILLIAM',137:'SCHAFFER, ERWIN LAMBERT',138:'SCHARF, WALTER',147:'WELFORD, NORMAN TRAVISS',151:'WEYTER, FREDERICK WILLIAM',152:'WHEELOCK, THOMAS DAVID',153:'WHELTON, BARTLETT DAVID',154:'WHERRETT, JOHN ROSS',164:'WILLEMSEN, ROGER WAYNE',180:'WRIGHT, JAMES LOUIS',181:'WU, ALFRED CHI-TAI',196:'ZINSMEISTER, PHILIP PRICE'}
values={i:{'name_raw':name} for i,name in names.items()}
values[134]={'birth_city':'Little Melton','birth_place':'Little Melton, Eng'}
values[147]['birth_city']='London';values[147]['birth_place']='London, Eng'
values[157]={'birth_city':'Clitheroe','birth_place':'Clitheroe, Eng'}
values[167]={'birth_date':'Dec 3, 21'}
values[179]={'birth_year':'1928','birth_date':'Feb 6, 28'}
values[184]={'birth_city':'Salt County','birth_place':'Salt County, China'}
# Reuse the established NYC convention and its existing coordinates/match.
values[171]={k:rows[189][k] for k in ('geo_lat','geo_lon','geo_geoid','geo_county_name','geo_matched_name','geo_match_source','geo_jw','geo_cleaning_jw')}
prior=json.loads((s/'manual_overrides.json').read_text(encoding='utf-8'))
by={r['source_key']:r for r in prior}
for ident,v in values.items():
 r=rows[ident];scan=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed/images'/f'{Path(r["source_file"]).stem}_page_{int(r["pdf_page"]):04d}.png'
 by[r['source_key']]=dict(source_key=r['source_key'],manual_action='correct',manual_confidence='high',manual_note='Explicit source transcription/county correction found in stratified scan audit.',evidence=f'Audit {ident}; original scan {scan}; name and birth clause manually read. NYC geography follows existing New York=36061 convention.',reviewed_reasons=['*'],values=v,source_scan=str(scan),scan_sha256=sha(scan))
# This heading was also read during the interleaved-header source review.
key='16_E_500_600:30:9:1'
by[key]=dict(source_key=key,manual_action='correct',manual_confidence='high',manual_note='Printed surname MOTE, not clipped OTE.',evidence='Original PDF page 30, block 9, directly above MOTEFF header.',reviewed_reasons=['name_outside_source_range','name_uncertain'],values={'name_raw':'MOTE, MICHAEL ISNARDI'})
write_json(s/'manual_overrides.json',list(by.values()))
_,blocks=read_csv(s/'blocks.csv');blocks={r['block_key']:r for r in blocks}
forced=[]
for key in ('16_C_400_600.pdf:170:15','16_G_800_900.pdf:92:3','16_G_800_900.pdf:100:16'):
 forced.append(dict(piece_key=key+':1',expected_text=normalized(blocks[key]['text']),evidence='Independent printed biography/notice verified during 200-record source scan audit; do not attach to preceding biography.'))
write_json(s/'forced_entry_starts.json',forced)
# Repair the garbled ZUMOFF header, retaining original career OCR after it.
fixes=json.loads((s/'block_overrides.json').read_text(encoding='utf-8'))
key='16_G_800_900.pdf:100:16';b=blocks[key]
header='ZUMOFF, BARNETT, b Brooklyn, NY, June 1, 26; m 51; c 3. MEDICINE, MEDICAL RESEARCH.'
fixes=[f for f in fixes if f.get('block_key')!=key]
fixes.append(dict(action='replace',block_key=key,expected_text=b['text'],text=header+' '+b['text'],evidence='Original scan PDF page 100: printed ZUMOFF header and birth clause manually transcribed. Original garbled body retained.'))
forced[-1]['expected_text']=normalized(header+' '+b['text'])
write_json(s/'forced_entry_starts.json',forced);write_json(s/'block_overrides.json',fixes)
guard=s/'segmentation_guard_reviews.json'
if guard.exists():
 records=json.loads(guard.read_text(encoding='utf-8'))
 for r in records:
  if r.get('corrected_name','').startswith('LOWNEY'):r['corrected_name']=r['corrected_name'].replace('LOWNEY','LOWNIE')
 write_json(guard,records)
print('Recorded',len(values),'audit corrections and three explicit biography boundaries.')

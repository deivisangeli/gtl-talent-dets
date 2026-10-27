"""Explicit decisions from the 219 manually viewed guard contact cards.

Only name and segmentation flags are covered here; birthplace/geocoding flags
still require their own evidence. The original OCR is retained in its ledger.
"""
import csv,json,re,sys
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from ocr_supplement import read_csv,write_json,sha,DEFAULT_ROOT
stage=DEFAULT_ROOT/'output/amws/ocr_supplement_20261011'
_,guards=read_csv(stage/'segmentation_guard.csv')
_,entries=read_csv(stage/'entries.csv')
assert len(guards)==219
names={
35:'HEBERLING, JACK WAUGH, JR',81:'LAVE, ROY E(LLIS), JR',
82:'LAWS, EDWARD RAYMOND, JR',86:'LEONARD, B(ENJAMIN) F(RANKLIN), III',
93:'LEYBOURNE, A(LLEN) E(DWARD), III',97:'LINSLEY, RAY K(EYES), JR',
99:'LITTLE, WINSTON WOODARD, JR',103:'LONG, ANDREW FLEMING, JR',
118:'MORROW, GRANT, III',119:'MOTE, C(LAYTON) D(ANIEL), JR',
125:'MUSAL, HENRY M(ICHAEL), JR',128:'NEALE, WILLIAM MCC(ORMICK), JR',
132:'NEVILL, GALE E(RWIN), JR',135:'NICHOLS, LEE L(OCHHEAD), JR',
138:'NORMAN, L(EWIS) A(RTHUR), JR',140:'NUNN, WALTER M(ELROSE), JR',
148:'PACKARD, KARLE SANBORN, JR',149:'PAFFENBARGER, RALPH SEAL, JR',
156:'PARTRIDGE, JOHN JOSEPH, JR',160:'SAMPLE, THOMAS EARL, JR',
164:'SANTINI, WILLIAM A(NDREW JOHN), JR',170:'SCHARBER, SAMUEL ROBERT, JR',
176:'WEST, ARTHUR JAMES, II',179:'WHEELER, CLAYTON EUGENE, JR',
185:'WHITTEMORE, O(SGOOD) J(AMES), JR',196:'WILLIAMS, LEO, JR',
201:'WILSON, F WESLEY, JR',202:'WILSON, ROBERT LEE, JR',
205:'WOLF, JOSEPH A(LLEN), JR',
}
overrides=json.loads((stage/'manual_overrides.json').read_text(encoding='utf-8'))
by_key={r['source_key']:r for r in overrides}
by_key['16_D_800_1000:16:10:1']['reviewed_reasons']=['*']
reviews=[]
for ident,g in enumerate(guards,1):
    doc,page,block=g['block_key'].split(':')
    scan=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed/images'/f'{Path(doc).stem}_page_{int(page):04d}.png'
    evidence=stage/'review'/f'guard_{(ident-1)//8+1:02d}.png'
    note='Scan shows a single biography header; given names and suffix belong together.'
    if ident==162:
        note+=' Contact crop missed the header; the full source page was inspected: SANDERS, JOHN P(AUL), SR, b Hope, Ark, July 4, 26.'
    record=dict(guard_id=ident,block_key=g['block_key'],scan_reviewed=True,
        decision='single_biography_header',note=note,source_scan=str(scan),scan_sha256=sha(scan),
        evidence_image=str(evidence),evidence_sha256=sha(evidence))
    if ident in names:
        prefix=f'{Path(doc).stem}:{page}:{block}:'
        matches=[r for r in entries if r['source_key'].startswith(prefix)]
        assert len(matches)==1,(ident,[r['source_key'] for r in matches])
        r=matches[0]
        record['corrected_name']=names[ident]
        by_key[r['source_key']]=dict(source_key=r['source_key'],manual_action='correct',
            manual_confidence='high',manual_note='Restored clipped/misrecognized name from the printed header.',
            evidence=names[ident],evidence_image=str(evidence),reviewed_reasons=['name_uncertain','segmentation_uncertain'],
            values={'name_raw':names[ident]})
    reviews.append(record)
write_json(stage/'segmentation_guard_reviews.json',reviews)
write_json(stage/'manual_overrides.json',list(by_key.values()))
print('Recorded',len(reviews),'scan decisions and',len(names),'name overrides')

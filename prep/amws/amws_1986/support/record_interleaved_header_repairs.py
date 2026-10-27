"""Source-specific transcription of headers inspected on original scans.

The remainder of each damaged OCR block is retained (line order is garbled).
This recovers biography boundaries and birth fields, without inventing career
text. Original blocks remain untouched in blocks.csv.
"""
import json,re,sys
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from ocr_supplement import read_csv,write_json,sha,DEFAULT_ROOT
stage=DEFAULT_ROOT/'output/amws/ocr_supplement_20261011'
_,blocks=read_csv(stage/'blocks.csv');by={r['block_key']:r for r in blocks}
headers={
'16_E_500_600.pdf:30:10':'MOTEFF, J(OHN), b Youngstown, Ohio, Aug 8, 23; m 51; c 1. PHYSICS, MATERIALS SCIENCE.',
'16_E_500_600.pdf:86:1':'MYERS, CHARLES EDWIN, b Philadelphia, Pa, Dec 15, 40; m 63; c 3. ENVIRONMENTAL ENGINEERING, SCIENCE POLICY.',
'16_E_500_600.pdf:88:1':'MYERS, JAMES R(USSELL), b Middletown, Ohio, June 17, 33. METALLURGY.',
'16_E_500_600.pdf:88:2':'MYERS, JEFFREY, b Philadelphia, Pa, Feb 8, 32; div; c 1. PATHOLOGY, INFORMATION SCIENCE.',
'16_E_700_800.pdf:10:1':'NORRIS, DAVID OTTO, b Ashtabula, Ohio, Oct 1, 39; m 66. COMPARATIVE ENDOCRINOLOGY.',
'16_E_700_800.pdf:42:1':"O'DELL, RALPH DOUGLAS, b Leavenworth, Ind, June 11, 38; m 59; c 4. TECHNICAL MANAGEMENT, NUCLEAR ENGINEERING.",
'16_E_700_800.pdf:42:2':'ODELL, THEODORE TELLEFSEN, JR, cell biology, physiology, see previous edition',
'16_E_700_800.pdf:54:1':'OHM, JACK ELTON, b Milwaukee, Wis, Sept 23, 32. MATHEMATICS.',
'16_E_700_800.pdf:54:2':'OHMAN, GUNNAR P(ETER), b Sweden, Dec 15, 18; nat US; m 48; c 3. ELECTRICAL ENGINEERING.',
'16_E_700_800.pdf:54:3':'OHMART, ROBERT DALE, b Tatum, NMex, Jan 2, 38; m 58; c 4. ZOOLOGY.',
'16_E_800_900.pdf:52:1':'PAMIDI, PRABHAKAR RAMARAO, b Bangalore, India, Feb 18, 42; m 74; c 1. STRUCTURAL ANALYSIS, MECHANISMS & KINEMATICS.',
'16_E_800_900.pdf:66:1':'PARGMAN, DAVID, b New York, NY, July 15, 37; m 63; c 3. EXERCISE PHYSIOLOGY, NUTRITION.',
'16_E_800_900.pdf:66:2':'PARHAM, ELLEN SPEIDEN, NUTRITION. Educ: Univ Tenn, PhD(nutrit), 67. Prof Exp: PROF & COORDR DIETETIC NUTRIT SCI,',
}
fixes=json.loads((stage/'block_overrides.json').read_text(encoding='utf-8'))
existing={r.get('block_key'):r for r in fixes if r['action']!='add'}
reviews=[]
for key,header in headers.items():
    b=by[key];doc,page,index=key.split(':')
    scan=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed/images'/f'{Path(doc).stem}_page_{int(page):04d}.png'
    body=b['text']
    # Explicit source repair; delete displaced malformed header spans in this
    # single inspected biography so they cannot become an extra biography.
    body=re.sub(r"\b[A-Z][A-Z'(). -]{1,65},\s+[A-Z][A-Z'(). ,-]{1,100}[,.]\s*b\s",'',body)
    text=header if key.endswith(':42:2') or key.endswith(':66:2') else header+' '+body
    fix=dict(action='replace',block_key=key,expected_text=b['text'],text=text,
        evidence='Printed header transcribed from manually inspected scan. OCR body belongs to this biography but has interleaved lines.',
        source_scan=str(scan),scan_sha256=sha(scan))
    if key in existing:fixes.remove(existing[key])
    fixes.append(fix)
    reviews.append(dict(block_key=key,scan_reviewed=True,corrected_header=header,
        source_scan=str(scan),scan_sha256=sha(scan),body_line_order='retained_original_OCR'))
write_json(stage/'block_overrides.json',fixes)
write_json(stage/'interleaved_header_reviews.json',reviews)
# The printed surname is LOWNIE; a plausible alternative spelling is unsupported.
overrides=json.loads((stage/'manual_overrides.json').read_text(encoding='utf-8'))
for key in ('16_D_800_1000:73:4:1','16_D_800_1000:75:4:1'):
    overrides=[r for r in overrides if r['source_key']!=key]
    overrides.append(dict(source_key=key,manual_action='correct',manual_confidence='high',
        manual_note='Retain printed spelling LOWNIE rather than infer a surname.',
        evidence='Printed pages 872, PDF pages 73 and 75: LOWNIE, H(AROLD) W(ILLIAM), JR.',
        reviewed_reasons=['name_uncertain','segmentation_uncertain'],
        values={'name_raw':'LOWNIE, H(AROLD) W(ILLIAM), JR'}))
write_json(stage/'manual_overrides.json',overrides)
print('Recorded',len(reviews),'source-specific header repairs')

"""Reproducible reviewed-OCR ingestion, evidence views, and transactional exports.

Parsing/geocoding run in 11_parse_ocr_supplement.R using the existing R rules.
Raw OCR, existing early-edition data, and analysis panels are never rewritten.
"""
from __future__ import annotations
import argparse
from collections import Counter, defaultdict
import csv
import hashlib
import json
import os
from pathlib import Path
import random
import re
import shutil
import sys
import subprocess
from datetime import datetime, timezone

csv.field_size_limit(10_000_000)
REPO = Path(__file__).resolve().parents[3]
DEFAULT_ROOT = Path.home() / 'Globtalent Dropbox/gtl_talent_dets'


def sha(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as f:
        for chunk in iter(lambda: f.read(2**20), b''):
            h.update(chunk)
    return h.hexdigest()


def read_csv(path):
    with Path(path).open(encoding='utf-8-sig', newline='') as f:
        reader = csv.DictReader(f)
        return list(reader.fieldnames), list(reader)


def write_csv(path, rows, columns=None):
    path = Path(path); path.parent.mkdir(parents=True, exist_ok=True)
    columns = columns or list(rows[0])
    with path.open('w', encoding='utf-8-sig', newline='') as f:
        w = csv.DictWriter(f, columns, extrasaction='ignore')
        w.writeheader(); w.writerows(rows)


def write_json(path, data):
    Path(path).write_text(json.dumps(data, indent=2, ensure_ascii=False), encoding='utf-8')


def normalized(s):
    return re.sub(r'\s+', ' ', s).strip()


def prepare(root, stage, source):
    stage.mkdir(parents=True, exist_ok=True)
    manifest_path=stage/'input_manifest.json'
    previous_manifest=json.loads(manifest_path.read_text(encoding='utf-8')) if manifest_path.exists() else None
    if previous_manifest:
        for item in previous_manifest['files']:
            assert sha(source/item['path'])==item['sha256'],'Immutable reviewed delivery changed'
    selection = json.loads((source/'corpus_selection.json').read_text())
    documents = sorted(selection['documents'], key=lambda d: (
        d['source_file'].split('_')[1], int(d['source_file'].split('_')[2])))
    pages = []; blocks = []; hashes = []
    for doc in documents:
        stem = Path(doc['source_file']).stem
        start = int(stem.split('_')[2])
        for page in range(1, doc['page_count']+1):
            path = source/stem/f'page_{page:04d}.json'
            j = json.loads(path.read_text(encoding='utf-8'))
            assert j['source_file'] == doc['source_file'] and j['pdf_page_number'] == page
            prediction = j['corrected_prediction']['parsing_res_list']
            page_key = f'{doc["source_file"]}:{page}'
            pages.append(dict(page_key=page_key, source_file=doc['source_file'], pdf_page=page,
                              printed_page=start+page-1, blocks=len(prediction),
                              status=j['original_record']['status']))
            hashes.append(dict(path=str(path.relative_to(source)), sha256=sha(path)))
            # Reviewed block_order is authoritative; column metadata/boxes remain evidence.
            for idx, b in sorted(enumerate(prediction), key=lambda pair: (
                    pair[1].get('block_order', pair[0]), pair[0])):
                text = b.get('block_content', '')
                label = b.get('block_label', '')
                body = label in ('text', 'paragraph_title', 'reference', 'reference_content') and bool(text.strip())
                box = b.get('block_bbox', [0, 0, 0, 0])
                blocks.append(dict(block_key=f'{page_key}:{idx}', source_file=doc['source_file'],
                    pdf_page=page, printed_page=start+page-1, block_index=idx,
                    column=b.get('column_number', 1 if box[0] < j['original_record']['rendered_width']/2 else 2),
                    block_order=b.get('block_order', idx), label=label,
                    block_status='body' if body else 'excluded_layout',
                    bbox=json.dumps(box), text=text,
                    source_lines=json.dumps(b.get('repair_metadata', {}).get('source_line_indices', []))))
        print(f'Prepared {stem}: {doc["page_count"]} pages', flush=True)
    assert len(pages) == 1792 and len(documents) == 14
    write_csv(stage/'pages.csv', pages); write_csv(stage/'blocks.csv', blocks)
    write_json(manifest_path, previous_manifest or dict(source=str(source), pages=1792, documents=14,
        files=hashes, created_at=datetime.now(timezone.utc).isoformat()))


def segment(stage):
    patterns=json.loads((stage/'segmentation_patterns.json').read_text(encoding='utf-8'))
    _,blocks=read_csv(stage/'blocks.csv')
    fixes=json.loads((stage/'block_overrides.json').read_text(encoding='utf-8')) if (stage/'block_overrides.json').exists() else []
    by_key={r['block_key']:r for r in blocks}
    for fix in fixes:
        if fix['action']=='add':
            row=fix['block']; by_key[row['block_key']]=row; blocks.append(row)
        else:
            row=by_key[fix['block_key']]
            assert row['text']==fix['expected_text'], 'Stale manual block override'
            row['text_adjusted']=fix['text']
    # Ordering is unchanged, except explicitly recovered notices placed in their source column.
    docs=sorted(set(r['source_file'] for r in blocks),key=lambda s:(s.split('_')[1],int(s.split('_')[2])))
    rank={s:i for i,s in enumerate(docs)}
    blocks.sort(key=lambda r:(rank[r['source_file']],int(r['pdf_page']),float(r['block_order'])))
    compiled={k:re.compile(v,re.I if k in ('ENTRY_START_SEE_PREVIOUS','CONTINUATION_START',
        'DAMAGED_SEE_PREVIOUS','DAMAGED_WITH_MONTH_AND_EDUC','DAMAGED_WITH_MONTH','DAMAGED_GLUE_BIRTH_MONTH',
        'DASH_PROF_DAMAGED_START') else 0) for k,v in patterns.items() if k!='SAFE_EMBEDDED_PATTERNS'}
    embedded=[re.compile(p) for p in patterns['SAFE_EMBEDDED_PATTERNS']]
    starts=[('ENTRY_START_BIRTH','strong_birth_start'),('ENTRY_START_SEE_PREVIOUS','see_previous_start'),
            ('ENTRY_START_NO_COMMA_BIRTH','no_comma_birth_start'),('ENTRY_START_NO_BIRTH_FIELD','no_birth_field_start')]
    damaged=[('DAMAGED_SEE_PREVIOUS','damaged_see_previous_start'),('DAMAGED_WITH_MONTH_AND_EDUC','damaged_month_educ_start'),
            ('DAMAGED_WITH_MONTH','damaged_month_start'),('DAMAGED_GLUE_BIRTH_MONTH','damaged_glue_birth_month_start'),
            ('DAMAGED_WITH_FIELD_AND_EDUC','damaged_field_educ_start'),('DAMAGED_POUND_EDUC','damaged_pound_educ_start')]
    def classify(text):
        if compiled['NOISE_RX'].search(text): return False,'noise'
        for key,rule in starts:
            if compiled[key].search(text): return True,rule
        if not compiled['CONTINUATION_START'].search(text):
            for key,rule in damaged:
                if compiled[key].search(text): return True,rule
        for key,rule in [('DASH_PROF_DAMAGED_START','dash_prof_damaged_start'),('ISOLATED_DAMAGED_NAME','isolated_damaged_name_start')]:
            if compiled[key].search(text): return True,rule
        return False,'continued_previous'
    entries=[]; assignments=[];current=None; previous_doc='';previous_printed=None
    breaks=json.loads((stage/'segmentation_breaks.json').read_text(encoding='utf-8')) if (stage/'segmentation_breaks.json').exists() else []
    break_pages={r['page_key'] for r in breaks}
    previous_page=''
    guarded=[]; uncertain_starts=[]
    forced=json.loads((stage/'forced_entry_starts.json').read_text(encoding='utf-8')) if (stage/'forced_entry_starts.json').exists() else []
    forced={r['piece_key']:r for r in forced}
    for r in blocks:
        if r['block_status']!='body': continue
        page_key=f'{r["source_file"]}:{r["pdf_page"]}'
        if page_key!=previous_page and page_key in break_pages:
            if current:entries.append(current)
            current=None
        previous_page=page_key
        original=normalized(r['text']); marked=normalized(r.get('text_adjusted',r['text']))
        for p in embedded:
            def insert_boundary(match):
                prefix=marked[:match.start()].rsplit('<<<ENTRY>>>',1)[-1].strip()
                following=marked[match.end():]
                expanded_suffix=re.match(r"[A-Z][A-Z'’(). -]{0,140},\s*(?:JR|SR|II|III|IV)[,.]?\s*b\b",following)
                if expanded_suffix and prefix and re.fullmatch(r"[A-Z0-9'’().,* -]+",prefix):
                    guarded.append(dict(block_key=r['block_key'],prefix=prefix,following=following[:180],reason='given_name_and_suffix_inside_single_header'))
                    return match.group()
                return '\n<<<ENTRY>>>'
            marked=p.sub(insert_boundary,marked)
        pieces=[normalized(p) for p in marked.split('<<<ENTRY>>>') if p.strip()]
        for piece_no,text in enumerate(pieces,1):
            adjusted=re.sub(r"^([A-Z][A-Z'’ -]{1,65})\.\s+",r'\1, ',text)
            starts_entry,rule=classify(adjusted)
            # The existing no-birth-field grammar is case-sensitive on "Educ"
            # and omits periods inside disciplines. OCR variants of that label
            # and punctuation must not attach a separate incomplete biography.
            variant=patterns['ENTRY_START_NO_BIRTH_FIELD'].replace('[A-Za-z ,&*/-]','[A-Za-z ,&*/.:-]')
            label_normalized=re.sub(r'\bEduc\b','Educ',adjusted,flags=re.I)
            volume=r['source_file'].split('_')[1]
            if not starts_entry and label_normalized[0] in {'C':'HI','D':'L','E':'MNOP','F':'S','G':'WXYZ'}[volume] and re.search(variant,label_normalized):
                starts_entry,rule=True,'no_birth_field_punctuation_variant'
            piece_key=f'{r["block_key"]}:{piece_no}'
            if piece_key in forced:
                assert forced[piece_key]['evidence'] and forced[piece_key]['expected_text']==text
                starts_entry,rule=True,'manual_scan_entry_start'
            possible_heading=re.match(r"^[A-Z][A-Z'’(). -]{1,65}[,.]\s*[A-Z][A-Z'’(). ,0-9-]{2,100}[,.]",adjusted)
            if not starts_entry and possible_heading:
                uncertain_starts.append(dict(piece_key=piece_key,block_key=r['block_key'],text=text,source_key=current['source_key'] if current else '',reason='unclassified_uppercase_heading'))
            same_volume = bool(previous_doc) and r['source_file'].split('_')[1] == previous_doc.split('_')[1]
            if r['source_file']!=previous_doc and previous_doc and (not same_volume or int(r['printed_page'])!=previous_printed+1):
                if current: entries.append(current)
                current=None
            if starts_entry or current is None:
                if current: entries.append(current)
                current=dict(source_key=f'{Path(r["source_file"]).stem}:{r["pdf_page"]}:{r["block_index"]}:{piece_no}',
                    doc_id='amws'+Path(r['source_file']).stem,source_file=r['source_file'],pdf_page=r['pdf_page'],
                    block_index=r['block_index'],piece_no=piece_no,
                    segmentation_rule=rule if starts_entry else 'orphan_continuation',raw_text_adjusted=adjusted,
                    raw_text=original if len(pieces)==1 else text)
            else:
                current['raw_text_adjusted']+=' '+adjusted
                current['raw_text']+=' '+(original if len(pieces)==1 else text)
            assignments.append(dict(block_key=r['block_key'],piece_no=piece_no,source_key=current['source_key'],text=text))
            previous_doc=r['source_file'];previous_printed=int(r['printed_page'])
    if current: entries.append(current)
    write_csv(stage/'segmented_entries.csv',entries);write_csv(stage/'block_assignments.csv',assignments)
    # Virtual recoveries have source geometries too, for evidence views.
    write_csv(stage/'effective_blocks.csv',blocks,list(blocks[0])+['text_adjusted'])
    unique_guards={}
    for item in guarded:
        header=re.split(r'[,.]?\s*b\b',item['prefix']+' '+item['following'])[0].strip()
        unique_guards[(item['block_key'],re.sub('[^A-Z0-9]','',header.upper()))]=item
    write_csv(stage/'segmentation_guard.csv',list(unique_guards.values()),['block_key','prefix','following','reason'])
    write_csv(stage/'uncertain_entry_starts.csv',uncertain_starts,['piece_key','block_key','text','source_key','reason'])
    print(f'Segmented {len(entries)} biographies/fragments from {len(blocks)} blocks')


def eligible(r):
    value = lambda key: r.get(key, '').strip() not in ('', 'NA', 'N/A')
    return (value('birth_city') and value('birth_year') and value('birth_country')
            and r['birth_country'].lower() != 'zo'
            and not (r['is_us_birth'].upper() == 'TRUE' and r['is_us_geocoded'].upper() != 'TRUE'))


def review_covers_flags(row,decision):
    if decision.get('manual_action') not in ('correct','no_change'):return False
    covered=set(decision.get('reviewed_reasons',[]))
    flags={r.strip() for r in row.get('review_reasons','').split(';') if r.strip()}
    return '*' in covered or flags.issubset(covered)


def dispositions(stage):
    _, entries = read_csv(stage/'entries.csv')
    overrides = {d['source_key']: d for d in json.loads((stage/'manual_overrides.json').read_text())} if (stage/'manual_overrides.json').exists() else {}
    uncertain={r['source_key'] for r in read_csv(stage/'uncertain_entry_starts.csv')[1]} if (stage/'uncertain_entry_starts.csv').exists() else set()
    for r in entries:
        decision = overrides.get(r['source_key'], {})
        volume=r['doc_id'].split('_')[1]
        allowed={'C':'HI','D':'L','E':'MNOP','F':'S','G':'WXYZ'}
        name=r.get('name_raw','').lstrip("*^ ")
        extra=[]
        if name and name[0] not in allowed.get(volume,name[0]):extra.append('name_outside_source_range')
        if re.search(r'\d|WTLL|AVTD|RTCK',name):extra.append('name_recognition_suspect')
        raw_header=re.split(r'[,.;]\s*b\b',r['raw_text_adjusted'],maxsplit=1)[0][:200]
        if re.search(r'\b(?:JR|SR|II|III|IV)\b',raw_header) and not re.search(r'\b(?:JR|SR|II|III|IV)\b',name):extra.append('name_suffix_missing')
        if r['source_key'] in uncertain:extra.append('unclassified_biography_heading')
        if r.get('birth_city','').casefold()=='new york' and r.get('birth_state')=='NY' and r.get('geo_geoid')!='36061':extra.append('new_york_county_conflict')
        r['review_reasons']=';'.join(dict.fromkeys(filter(None,r['review_reasons'].split(';')+extra)))
        birth_headers=re.findall(r"\b[A-Z][A-Z'(). -]{1,65},\s+[A-Z][A-Z'(). ,-]{1,100}[,.]\s*b\s",r['raw_text_adjusted'])
        if len(birth_headers)>1 and 'multiple_biography_headers' not in r['review_reasons']:
            r['review_reasons']=';'.join(filter(None,[r['review_reasons'],'multiple_biography_headers']))
        if r.get('birth_date'):
            match=re.fullmatch(r'([A-Za-z]+)\.?\s*(\d{1,2})\s*[, .]\s*(\d{2}|\d{4})',r['birth_date'].strip())
            months={'jan':31,'feb':29,'mar':31,'apr':30,'may':31,'jun':30,'jul':31,'aug':31,'sep':30,'oct':31,'nov':30,'dec':31}
            invalid=not match or match[1][:3].lower() not in months or not 1<=int(match[2])<=months.get(match[1][:3].lower(),0)
            if not invalid and r.get('birth_year'):
                month=list(months).index(match[1][:3].lower())+1
                try:datetime(int(r['birth_year']),month,int(match[2]))
                except ValueError:invalid=True
            if invalid and 'malformed_birth_date' not in r['review_reasons']:
                r['review_reasons']=';'.join(filter(None,[r['review_reasons'],'malformed_birth_date']))
        text = r['raw_text_adjusted']
        header = text[:240]
        r['disposition'] = 'included' if eligible(r) else 'incomplete'
        missing=[f'missing_{key}' for key in ('birth_city','birth_year','birth_country') if not r.get(key,'').strip() or r[key].strip().upper() in ('NA','N/A')]
        if r['birth_country'].lower()=='zo':missing.append('unaccepted_country')
        if r['is_us_birth'].upper()=='TRUE' and r['is_us_geocoded'].upper()!='TRUE':missing.append('US_birth_without_successful_geocode')
        r['disposition_reason'] = 'passes_existing_final_eligibility' if eligible(r) else ';'.join(missing)
        if r['segmentation_rule'] == 'orphan_continuation':
            r['disposition'], r['disposition_reason'] = 'orphan', 'no_biography_start_in_available_range'
        elif re.search(r'\bsee\s+prev(?:ious)?', header, re.I) and (
                not re.search(r'[,.;]\s*b\s', header) or
                re.search(r'\bsee\s+prev(?:ious)?', header, re.I).start() < re.search(r'[,.;]\s*b\s', header).start()):
            r['disposition'], r['disposition_reason'] = 'notice', 'see_previous_edition'
        elif re.search(r'\bdeceased\b', header, re.I) and not re.search(r'[,.;]\s*b\s', header):
            r['disposition'], r['disposition_reason'] = 'notice', 'deceased_without_birth_information'
        if decision.get('manual_action') == 'exclude':
            r['disposition'], r['disposition_reason'] = 'excluded', decision['manual_note']
        elif decision.get('manual_action') == 'review_unclear':
            r['disposition'], r['disposition_reason'] = 'unresolved', decision['manual_note']
    return entries


def make_audit(root, stage):
    entries = dispositions(stage)
    decisions = json.loads((stage/'manual_overrides.json').read_text(encoding='utf-8')) if (stage/'manual_overrides.json').exists() else []
    reviewed = {r['source_key']:r for r in decisions}
    accepted = [r for r in entries if r['disposition'] == 'included' and (not r['review_reasons'] or review_covers_flags(r,reviewed.get(r['source_key'],{})))]
    if (stage/'duplicate_candidates.csv').exists():
        _,candidates=read_csv(stage/'duplicate_candidates.csv')
        duplicate_decisions=json.loads((stage/'duplicate_decisions.json').read_text(encoding='utf-8')) if (stage/'duplicate_decisions.json').exists() else {}
        uncertain={r['source_key'] for r in candidates if duplicate_decisions.get(r['source_key'],{}).get('action')!='distinct'}
        accepted=[r for r in accepted if r['source_key'] not in uncertain]
    write_csv(stage/'review_candidates.csv', [r for r in entries if r['review_reasons']], list(entries[0]))
    if (stage/'audit_sample200.csv').exists():
        _,sample=read_csv(stage/'audit_sample200.csv');current={r['source_key']:r for r in entries}
        selected=[dict(current[r['source_key']],audit_id=r['audit_id']) for r in sample]
        write_csv(stage/'audit_sample200.csv',selected,['audit_id']+list(entries[0]))
        return
    groups = defaultdict(list)
    for r in accepted:
        groups[(r['source_file'], 'US' if r['birth_country'] == 'USA' else 'non-US')].append(r)
    random.seed(20261011)
    selected = []
    for key in sorted(groups):
        selected.extend(random.sample(groups[key], min(7, len(groups[key]))))
    rest = [r for r in accepted if r['source_key'] not in {x['source_key'] for x in selected}]
    selected.extend(random.sample(rest, 200-len(selected)))
    selected.sort(key=lambda r: (r['source_file'], int(r['pdf_page']), int(r['block_index'])))
    for i, r in enumerate(selected, 1):
        r['audit_id'] = i
    write_csv(stage/'audit_sample200.csv', selected, ['audit_id']+list(entries[0]))
    print(Counter(r['disposition'] for r in entries))
    print('Review reasons:', Counter(x for r in entries for x in r['review_reasons'].split(';') if x))


def contact(stage, ids, output, source_images):
    from PIL import Image, ImageDraw, ImageFont
    from difflib import SequenceMatcher
    _, rows = read_csv(stage/'audit_sample200.csv')
    rows = {int(r['audit_id']): r for r in rows}
    _, blocks = read_csv(stage/'effective_blocks.csv')
    blocks = {r['block_key']: r for r in blocks}
    font = ImageFont.truetype('C:/Windows/Fonts/arial.ttf', 19)
    cards = []
    for ident in ids:
        r = rows[ident]
        b = blocks[f'{r["source_file"]}:{r["pdf_page"]}:{r["block_index"]}']
        image_path = source_images/f'{Path(r["source_file"]).stem}_page_{int(r["pdf_page"]):04d}.png'
        with Image.open(image_path) as im:
            x0,y0,x1,y1 = json.loads(b['bbox'])
            j=json.loads((source_images.parent/Path(r['source_file']).stem/f'page_{int(r["pdf_page"]):04d}.json').read_text(encoding='utf-8'))
            sx=im.width/j['original_record']['rendered_width']; sy=im.height/j['original_record']['rendered_height']
            if int(r.get('piece_no','1'))>1:
                # A shared OCR block can contain several biographies. Locate the
                # selected heading in original recognized line geometry.
                needle=re.sub('[^A-Z0-9]','',r['name_raw'].upper())
                options=[]
                for col in j['original_record']['prediction']['column_predictions']:
                    ocr=col['prediction']['overall_ocr_res']
                    for text,box in zip(ocr['rec_texts'],ocr['rec_boxes']):
                        if not y0-20<=box[1]<=y1+20 or not x0-40<=box[0]+col['x_offset']<=x1+40:continue
                        line=re.sub('[^A-Z0-9]','',text.upper())
                        score=SequenceMatcher(None,needle,line[:len(needle)]).ratio()
                        options.append((score,box[1]))
                if options and max(options)[0]>.55:y0=max(options)[1]
                else:raise ValueError(f'Need explicit heading geometry for audit {ident}')
            # First 180 pixels normally cover the printed name and birth clause.
            crop = im.crop((max(0,x0-170)*sx, max(0,y0-260)*sy, min(im.width,(x1+100)*sx), min(im.height,max(y0+190, min(y1+12,y0+250))*sy)))
            crop=crop.resize((1120,round(crop.height*1120/crop.width)))
            crop.thumbnail((1120,450))
            card = Image.new('RGB', (1140, crop.height+100), 'white')
            card.paste(crop, (10,90)); draw=ImageDraw.Draw(card)
            draw.text((8,5), f'{ident} | {r["name_raw"]} | {r["source_file"]} p{r["pdf_page"]}',font=font,fill='black')
            draw.text((8,31), f'{r["birth_place"]} | {r["birth_date"]} => {r["birth_year"]}',font=font,fill='black')
            draw.text((8,57), f'{r["birth_city"]}, {r["birth_state"]}, {r["birth_country"]} | {r["geo_geoid"]} {r["geo_county_name"]}',font=font,fill='black')
            cards.append(card)
    out = Image.new('RGB', (1140, sum(c.height for c in cards)), '#cccccc')
    y=0
    for c in cards: out.paste(c,(0,y)); y+=c.height
    output.parent.mkdir(parents=True,exist_ok=True); out.save(output)
    print(output)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('command', choices=('prepare','segment','audit','contact'))
    p.add_argument('--data-root', type=Path, default=Path(os.environ.get('TALENT_DETS_DATA_DIR', DEFAULT_ROOT)))
    p.add_argument('--stage', type=Path)
    p.add_argument('--source', type=Path)
    p.add_argument('--ids'); p.add_argument('--output', type=Path)
    p.add_argument('--images',type=Path,default=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed/images')
    a = p.parse_args()
    stage = a.stage or a.data_root/'output/amws/ocr_supplement_20261011'
    source = a.source or a.data_root/'output/amws/amws_ed16_paragraph_reviewed_data'
    if a.command == 'prepare': prepare(a.data_root,stage,source)
    elif a.command == 'segment': segment(stage)
    elif a.command == 'audit': make_audit(a.data_root,stage)
    else:
        ids = list(range(int(a.ids.split('-')[0]),int(a.ids.split('-')[-1])+1))
        contact(stage,ids,a.output or stage/'review/contact.png',a.images)


if __name__ == '__main__': main()

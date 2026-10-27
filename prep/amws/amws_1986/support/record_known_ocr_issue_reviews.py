"""Explicit scan decisions by Codex on 2026-10-11, not inferred classifications."""
import csv
import json
from pathlib import Path
import hashlib
BASE=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed'
STAGE=Path.home()/'Globtalent Dropbox/gtl_talent_dets/output/amws/ocr_supplement_20261011'
NOTICES={
 1:('HALL, DELOU PERRIN','surgery','see previous edition'),
 2:('HALL, HUGH EDWARD, JR','nuclear physics','see previous edition'),
 3:('HAMANN, CECIL BOYCE','biology','see previous edition'),
 4:('HARARY, FRANK','mathematics','see previous edition'),
 5:('HARTLEY, ROBERT PAUL','geology','see previous edition'),
 6:('HARTSOOK, ELMER WILLIAM','nutritional biochemistry','deceased'),
 7:('HAWKES, GEORGE ROGERS','soil fertility, agronomy','see previous edition'),
 8:('HAYES, JOHN DURHAM','optics','see previous edition'),
 9:('HEILIGMAN, FRED','food science, microbiology','see previous edition'),
 10:('HEMPHILL, DONALD VINCENT','biology','see previous edition'),
 11:('HERLICH, HARRY','veterinary parasitology','see previous edition'),
 12:('HICKS, DONALD A','physics, science administration','see previous edition'),
 13:('HOLZER, WALTER FRANK','chemistry','deceased'),
 14:('HOOPER, EDWARD TEMPLE, JR','applied physics','see previous edition'),
 15:('HOPPENS, HAROLD ALBERT','organic chemistry','see previous edition'),
 16:('HORTON, JAMES B','metallurgy, chemistry','see previous edition'),
 17:('HOUGHTON, AUGUSTUS SHERRILL','chemistry','see previous edition'),
 18:('HOUSTON, JOHN MAPES','physics','see previous edition'),
 19:('HUNT, ROBERT ELTON','petroleum geology','see previous edition'),
 20:('LASSITER, JAMES WILLIAM','animal nutrition','see previous edition'),
 21:('LEOPOLD, ALDO STARKER','zoology, forestry','deceased'),
 22:('LEWIS, JOHN ALLEN','applied mathematics','see previous edition'),
 23:('LIGHTHOLDER, RICHARD K','civil engineering','see previous edition'),
 25:('MUIR, DONALD EARL','mathematics','see previous edition'),
 27:('NILSON, EDWIN NORMAN','mathematics','see previous edition'),
 28:('NOVAK, G(EORGE) J','chemical engineering','see previous edition'),
 33:('YONETZ, GERALD C(HARLES)','chemical engineering','see previous edition')}
HEADERS={
 24:(10,'LIU-GER, TSU-HUEI, b Kwei-yang, Kwei-chow, Repub of China, Mar 10, ',False),
 31:(12,'WOLFE, HARRY BERNARD, b Vancouver, BC, Dec 29, 27; m 52; c 4. ',True),
 32:(10,'WONG, CHIU-PING, b Canton, China, Dec 24, 43; m 76; c 1. POLYMER ',True)}

def main():
    with (STAGE/'blocks.csv').open(encoding='utf-8-sig',newline='') as f: blocks=list(csv.DictReader(f))
    inventory=json.loads((STAGE/'known_issue_inventory.json').read_text(encoding='utf-8'))
    fixes=[]; decisions=[]
    for row in inventory:
        ident=row['issue_id'];stem=Path(row['source_file']).stem;page=int(row['pdf_page_number']);column=int(row['column_number'])
        image=BASE/'images'/f'{stem}_page_{page:04d}.png'
        decision=dict(issue_id=ident,source_file=row['source_file'],pdf_page=page,column=column,
          reviewer='Codex',reviewed_at='2026-10-11',scan_path=str(image),scan_sha256=hashlib.sha256(image.read_bytes()).hexdigest(),
          evidence_sheet=row['evidence_crop'],confidence='high')
        page_blocks=[b for b in blocks if b['source_file']==row['source_file'] and int(b['pdf_page'])==page]
        if ident in HEADERS:
            block_index,header,replace=HEADERS[ident]
            b=next(b for b in page_blocks if int(b['block_index'])==block_index)
            text=header+(b['text'].split('\n',1)[-1] if replace else b['text'])
            fixes.append(dict(action='replace',block_key=b['block_key'],expected_text=b['text'],text=text,issue_id=ident,
                              evidence=header,manual_confidence='high'))
            decision.update(action='restore_biography_header',transcription=header)
        elif ident in NOTICES:
            name,field,notice=NOTICES[ident];text=f'{name}, {field}, {notice}'
            j=json.loads((BASE/stem/f'page_{page:04d}.json').read_text(encoding='utf-8'))
            col=j['original_record']['prediction']['column_predictions'][column-1]
            boxes=col['prediction']['overall_ocr_res']['rec_boxes']; indices=json.loads(row['source_line_indices'])
            target=[boxes[i] for i in indices]
            y0=min(b[1] for b in target);y1=max(b[3] for b in target)
            candidates=[b for b in page_blocks if b['block_status']=='body' and int(b['column'])==column
              and json.loads(b['bbox'])[1]<=y1+15 and json.loads(b['bbox'])[3]>=y0-15 and len(b['text'])<220]
            if len(candidates)==1:
                b=candidates[0];fixes.append(dict(action='replace',block_key=b['block_key'],expected_text=b['text'],text=text,
                                                  issue_id=ident,evidence=text,manual_confidence='high'))
            else:
                before=[b for b in page_blocks if int(b['column'])==column and json.loads(b['bbox'])[1]<=y0]
                order=max(float(b['block_order']) for b in before)+0.5 if before else min(float(b['block_order']) for b in page_blocks if int(b['column'])==column)-0.5
                virtual=dict(block_key=f'{row["source_file"]}:{page}:{100000+ident}',source_file=row['source_file'],pdf_page=str(page),
                  printed_page=str(int(stem.split('_')[2])+page-1),block_index=str(100000+ident),column=str(column),block_order=str(order),
                  label='manual_recovered_notice',block_status='body',bbox=json.dumps([col['x_offset'],y0,col['x_offset']+col['prediction']['width'],y1]),
                  text=text,source_lines=row['source_line_indices'])
                fixes.append(dict(action='add',block=virtual,issue_id=ident,evidence=text,manual_confidence='high'))
            decision.update(action='recover_notice_only',transcription=text,note='No birthplace/date is reported; excluded from final person counts.')
        elif ident==26:
            decision.update(action='retain_continuation',note='Scan contains continuation of research text, no name or birth clause; recognition gap retained in provenance, no inferred birth fields.')
        else:
            decision.update(action='exclude_margin_fragment',note='Top margin/header artifact, not a biography. Keep surrounding body text; retain unresolved fragment in original OCR ledger.')
            if ident==30:
                b=next(b for b in page_blocks if int(b['block_index'])==10)
                fixes.append(dict(action='replace',block_key=b['block_key'],expected_text=b['text'],text=b['text'].lstrip(',.'),
                  issue_id=ident,evidence='Scan begins PATEL, KALYANJI U; leading punctuation is a margin artifact.',manual_confidence='high'))
        decisions.append(decision)
    (STAGE/'block_overrides.json').write_text(json.dumps(fixes,indent=2,ensure_ascii=False),encoding='utf-8')
    (STAGE/'known_issue_reviews.json').write_text(json.dumps(decisions,indent=2,ensure_ascii=False),encoding='utf-8')
    print('Explicitly recorded',len(decisions),'scan reviews and',len(fixes),'block corrections')

if __name__=='__main__':main()

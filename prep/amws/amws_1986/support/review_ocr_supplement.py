"""Build scan evidence sheets for explicitly reviewed OCR issues (no classifier)."""
import csv
import json
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

BASE = Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed'
STAGE = Path.home()/'Globtalent Dropbox/gtl_talent_dets/output/amws/ocr_supplement_20261011'


def issues():
    queue=list(csv.DictReader((BASE/'recognition_review_queue.csv').open(encoding='utf-8-sig')))
    font=ImageFont.truetype('C:/Windows/Fonts/arial.ttf',19)
    ledger=[]; cards=[]
    for ident,r in enumerate(queue,1):
        stem=Path(r['source_file']).stem; page=int(r['pdf_page_number'])
        j=json.loads((BASE/stem/f'page_{page:04d}.json').read_text(encoding='utf-8'))
        column=int(r['column_number']); saved=j['original_record']['prediction']['column_predictions'][column-1]
        pred=saved['prediction']; ocr=pred['overall_ocr_res']
        print(ident,stem,page,r['description'],flush=True)
        indices=json.loads(r['source_line_indices'])
        # Whole column slices centered on the flagged recognized/empty source lines.
        boxes=ocr.get('rec_boxes') or ocr.get('dt_polys')
        selected=[boxes[i] for i in indices if i<len(boxes)]
        if not selected: raise ValueError('Missing evidence geometry')
        def extent(box):
            if isinstance(box[0],list): return min(p[1] for p in box),max(p[1] for p in box)
            return box[1],box[3]
        ymin=min(extent(b)[0] for b in selected); ymax=max(extent(b)[1] for b in selected)
        with Image.open(BASE/'images'/f'{stem}_page_{page:04d}.png') as im:
            left=saved['x_offset']; width=pred['width']
            sx=im.width/j['original_record']['rendered_width']; sy=im.height/j['original_record']['rendered_height']
            crop=im.crop((left*sx,max(0,ymin-100)*sy,min(im.width,(left+width)*sx),min(im.height,(ymax+120)*sy)))
            crop=crop.resize((1120,round(crop.height*1120/crop.width)))
            crop.thumbnail((1120,550))
            card=Image.new('RGB',(1140,crop.height+70),'white'); card.paste(crop,(10,70))
            draw=ImageDraw.Draw(card)
            draw.text((8,5),f'ISSUE {ident}: {stem} page {page} column {column}',font=font,fill='black')
            draw.text((8,32),r['description'][:112],font=font,fill='black')
            cards.append(card)
        ledger.append(dict(issue_id=ident,**r,evidence_crop=f'issues_{(ident-1)//5+1:02d}.png'))
    folder=STAGE/'review'; folder.mkdir(exist_ok=True)
    for offset in range(0,len(cards),5):
        part=cards[offset:offset+5]; out=Image.new('RGB',(1140,sum(c.height for c in part)),'#ccc')
        y=0
        for c in part: out.paste(c,(0,y));y+=c.height
        out.save(folder/f'issues_{offset//5+1:02d}.png')
    (STAGE/'known_issue_inventory.json').write_text(json.dumps(ledger,indent=2),encoding='utf-8')


if __name__=='__main__': issues()

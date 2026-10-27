"""Render each guarded header so the segmentation repair can be audited."""
import csv,json,re
from pathlib import Path
from difflib import SequenceMatcher
from PIL import Image,ImageDraw,ImageFont
BASE=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed'
STAGE=Path.home()/'Globtalent Dropbox/gtl_talent_dets/output/amws/ocr_supplement_20261011'
rows=list(csv.DictReader((STAGE/'segmentation_guard.csv').open(encoding='utf-8-sig')))
font=ImageFont.truetype('C:/Windows/Fonts/arial.ttf',18);cards=[]
for ident,r in enumerate(rows,1):
    doc,page,index=r['block_key'].split(':');page=int(page);stem=Path(doc).stem
    j=json.loads((BASE/stem/f'page_{page:04d}.json').read_text(encoding='utf-8'))
    header=re.split(r'\s+b\b',r['prefix']+' '+r['following'])[0]
    needle=re.sub('[^A-Z0-9]','',header.upper())[:35];matches=[]
    for col in j['original_record']['prediction']['column_predictions']:
        ocr=col['prediction']['overall_ocr_res']
        for text,box in zip(ocr['rec_texts'],ocr['rec_boxes']):
            candidate=re.sub('[^A-Z0-9]','',text.upper())[:len(needle)]
            matches.append((SequenceMatcher(None,needle,candidate).ratio(),box[1],col['x_offset'],col['prediction']['width']))
    score,y,left,width=max(matches)
    with Image.open(BASE/'images'/f'{stem}_page_{page:04d}.png') as im:
        sx=im.width/j['original_record']['rendered_width'];sy=im.height/j['original_record']['rendered_height']
        # OCR boxes are on a straightened page; source photographs retain curvature.
        crop=im.crop((max(0,left-160)*sx,max(0,y-260)*sy,min(im.width,(left+width+100)*sx),min(im.height,(y+180)*sy)))
        crop=crop.resize((1120,round(crop.height*1120/crop.width)))
        card=Image.new('RGB',(1140,crop.height+65),'white');card.paste(crop,(10,65));draw=ImageDraw.Draw(card)
        draw.text((8,5),f'Guard {ident} | {r["block_key"]} | match {score:.2f}',font=font,fill='black')
        draw.text((8,31),header[:115],font=font,fill='black');cards.append(card)
for offset in range(0,len(cards),8):
    part=cards[offset:offset+8];out=Image.new('RGB',(1140,sum(c.height for c in part)),'#ccc');y=0
    for card in part:out.paste(card,(0,y));y+=card.height
    out.save(STAGE/'review'/f'guard_{offset//8+1:02d}.png')
print('Rendered',len(rows),'headers in', (len(rows)+7)//8,'sheets')

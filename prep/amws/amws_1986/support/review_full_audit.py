"""Render full biography blocks, with continuation tails, for the 200-row audit."""
import json,sys
from pathlib import Path
from PIL import Image,ImageDraw,ImageFont
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from ocr_supplement import read_csv,DEFAULT_ROOT
stage=DEFAULT_ROOT/'output/amws/ocr_supplement_20261011'
base=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed'
_,rows=read_csv(stage/'audit_sample200.csv');_,blocks=read_csv(stage/'effective_blocks.csv')
blocks={r['block_key']:r for r in blocks};_,assignments=read_csv(stage/'block_assignments.csv')
tails={r['source_key']:r['block_key'] for r in assignments}
font=ImageFont.truetype('C:/Windows/Fonts/arial.ttf',19)
def crop_block(key):
    b=blocks[key];doc,page,index=key.split(':');stem=Path(doc).stem
    j=json.loads((base/stem/f'page_{int(page):04d}.json').read_text(encoding='utf-8'))
    with Image.open(base/'images'/f'{stem}_page_{int(page):04d}.png') as im:
        sx=im.width/j['original_record']['rendered_width'];sy=im.height/j['original_record']['rendered_height']
        x0,y0,x1,y1=json.loads(b['bbox'])
        crop=im.crop((max(0,x0-170)*sx,max(0,y0-260)*sy,min(im.width,(x1+100)*sx),min(im.height,(y1+150)*sy)))
        crop=crop.resize((1120,round(crop.height*1120/crop.width)))
        return crop
cards=[]
for r in rows:
    first=f'{r["source_file"]}:{r["pdf_page"]}:{r["block_index"]}'
    pieces=[crop_block(first)]
    last=tails[r['source_key']]
    if last!=first:pieces.append(crop_block(last))
    card=Image.new('RGB',(1140,120+sum(p.height for p in pieces)),'white');draw=ImageDraw.Draw(card)
    draw.text((8,4),f'{r["audit_id"]} | {r["name_raw"]} | {first}',font=font,fill='black')
    draw.text((8,30),f'{r["birth_place"]} | {r["birth_date"]} => {r["birth_year"]}',font=font,fill='black')
    draw.text((8,56),f'{r["birth_city"]}, {r["birth_state"]}, {r["birth_country"]} | {r["geo_geoid"]} {r["geo_county_name"]}',font=font,fill='black')
    draw.text((8,82),f'End block {last} | OCR end: {r["raw_text_adjusted"][-95:]}',font=font,fill='black')
    y=120
    for part in pieces:card.paste(part,(10,y));y+=part.height
    cards.append(card)
for offset in range(0,len(cards),3):
    group=cards[offset:offset+3];out=Image.new('RGB',(1140,sum(c.height for c in group)),'#ccc');y=0
    for card in group:out.paste(card,(0,y));y+=card.height
    out.save(stage/'review'/f'audit_full_{offset//3+1:02d}.png')
print('Rendered',len(cards),'full audit cards')

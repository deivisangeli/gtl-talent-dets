"""Manual scan/Word/raw-Paddle review. Never invokes OCR or field extraction."""
from __future__ import annotations
import argparse,csv,hashlib,json,random,shutil,sys
from pathlib import Path
from datetime import datetime,timezone
REPO=Path(__file__).resolve().parents[4]
CHAT=Path(r'C:\Users\megaj\Documents\Codex\2026-10-19\based-on-the-c')
OLD=CHAT/'work/amws_ocr_comparison_20261019'
FIELD=CHAT/'work/amws_field_extraction_audit_20261019'
ROOT=CHAT/'work/manual_entry_audit_20261020'
SEED=20261023
def now():return datetime.now(timezone.utc).isoformat()
def read(p):return json.loads(Path(p).read_text(encoding='utf-8'))
def write(p,x):
    p=Path(p);p.parent.mkdir(parents=True,exist_ok=True)
    t=p.with_suffix(p.suffix+'.tmp');t.write_text(json.dumps(x,ensure_ascii=False,indent=2)+'\n',encoding='utf-8');t.replace(p)
def sha(p):
    with Path(p).open('rb') as f:return hashlib.file_digest(f,'sha256').hexdigest()
def draw(docs,excluded):
    rng=random.Random(SEED);draws=[];base=[]
    for d in sorted(docs,key=lambda x:x['source_file']):
        order=rng.sample(range(1,d['page_count']+1),d['page_count'])
        candidates=[p for p in order if f"{d['source_file']}:{p}" not in excluded]
        draws.append(dict(source_file=d['source_file'],page_order=order,candidates=candidates))
        base.extend(f"{d['source_file']}:{p}" for p in candidates[:2])
    pool=[f"{d['source_file']}:{p}" for d in sorted(docs,key=lambda x:x['source_file']) for p in range(1,d['page_count']+1)
          if f"{d['source_file']}:{p}" not in excluded and f"{d['source_file']}:{p}" not in base]
    extra_order=rng.sample(pool,len(pool))
    return dict(draws=draws,base_proposed=base,extra_page_order=extra_order,extra_proposed=extra_order[:2])
def render(root,file,page):
    import pypdfium2 as pdfium
    d=next(d for d in read(root/'manifest.json')['documents'] if d['source_file']==file)
    folder=root/'scans';folder.mkdir(exist_ok=True);stem=f'{Path(file).stem}_p{page:04d}'
    if (folder/f'{stem}.png').exists():return
    pdf=pdfium.PdfDocument(d['source']['path']);p=pdf[page-1]
    im=p.render(scale=300/72).to_pil().convert('RGB');w,h=im.size
    im.resize((round(w*2/3),round(h*2/3))).save(folder/f'{stem}.png')
    # Overlap avoids cutting either column's leftmost name characters.
    for side,(x0,x1) in {'left':(0,.60),'right':(.35,1)}.items():
        im.crop((int(x0*w),0,int(x1*w),h)).save(folder/f'{stem}_{side}.png')
        for part,(y0,y1) in {'top':(0,.55),'bottom':(.45,1)}.items():
            im.crop((int(x0*w),int(y0*h),int(x1*w),int(y1*h))).save(folder/f'{stem}_{side}_{part}.png')
    p.close();pdf.close()
def prepare(root):
    assert not (root/'manifest.json').exists(),'Refusing to redraw existing audit'
    m=read(OLD/'comparison_manifest.json');f=read(FIELD/'sample_review.json')
    previous=set(read(OLD/'audit/page_review.json'))|{p['page_key'] for p in f['pages']}
    assert len(previous)==36
    sources=[]
    for d in m['documents']:
        for key,hkey in [('source','pdf_sha256'),('baseline','docx_sha256')]:
            assert sha(d[key]['path'])==d[hkey]
            sources.append(dict(path=d[key]['path'],sha256=d[hkey]))
    sampling=draw(m['documents'],previous)
    write(root/'manifest.json',dict(created_at=now(),seed=SEED,documents=m['documents'],sources=sources,
        previous_audit_pages=sorted(previous),sampling=sampling,
        previous_archives=[dict(path=str(p),sha256=sha(p)) for p in [CHAT/'outputs/amws_ocr_comparison_20261019.zip',CHAT/'outputs/amws_field_extraction_audit_20261019.zip']],
        protocol='Every full biography starting on sampled pages; source inventory before transcription inspection; recognition separate from structure; no OCR/parser/repair run'))
    keys=sampling['base_proposed']+sampling['extra_proposed']
    for key in keys:
        file,page=key.split(':');render(root,file,int(page))
    print(json.dumps(keys,indent=2))
def freeze_inventory(root):
    assert not (root/'inventory_freeze.json').exists()
    m=read(root/'manifest.json');pages=read(root/'inventory.json')
    keys=m['sampling']['base_proposed']+m['sampling']['extra_proposed']
    assert {p['page_key'] for p in pages}==set(keys) and len(pages)==20
    assert not set(keys)&set(m['previous_audit_pages'])
    assert all(p['source_only_reviewed'] and p['entries'] for p in pages)
    for p in pages:
        assert len(p['entries'])==len(set(p['entries']))
        file,page=p['page_key'].split(':');p['scan']=f'scans/{Path(file).stem}_p{int(page):04d}.png'
        p['scan_sha256']=sha(root/p['scan'])
    write(root/'inventory.json',pages)
    write(root/'inventory_freeze.json',dict(frozen_at=now(),inventory_sha256=sha(root/'inventory.json'),
        manifest_sha256=sha(root/'manifest.json'),pages=20,biographies=sum(len(p['entries']) for p in pages),
        origin='Codex visual reading of original scans before inspecting either transcription on these pages',independently_human_verified=False))
    print('Frozen',sum(len(p['entries']) for p in pages),'biographies')
def main():
    p=argparse.ArgumentParser();p.add_argument('phase',choices=['prepare','render','freeze-inventory']);p.add_argument('--root',type=Path,default=ROOT)
    p.add_argument('--pdf');p.add_argument('--page',type=int);a=p.parse_args()
    if a.phase=='prepare':prepare(a.root)
    elif a.phase=='render':render(a.root,a.pdf,a.page)
    elif a.phase=='freeze-inventory':freeze_inventory(a.root)
if __name__=='__main__':main()

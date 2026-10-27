"""Paired, source-referenced field extraction audit. Never publishes person data."""
from __future__ import annotations
import argparse
import csv
import hashlib
import json
import random
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
import unicodedata
from datetime import datetime, timezone
from pathlib import Path

REPO = Path(__file__).resolve().parents[4]
CHAT = Path(r'C:\Users\megaj\Documents\Codex\2026-10-19\based-on-the-c')
OLD = CHAT / 'work/amws_ocr_comparison_20261019'
ROOT = CHAT / 'work/amws_field_extraction_audit_20261019'
SEED = 20261021

def now(): return datetime.now(timezone.utc).isoformat()
def read(path): return json.loads(Path(path).read_text(encoding='utf-8'))
def write(path, data):
    path=Path(path);path.parent.mkdir(parents=True,exist_ok=True)
    tmp=path.with_suffix(path.suffix+'.tmp')
    tmp.write_text(json.dumps(data,ensure_ascii=False,indent=2)+'\n',encoding='utf-8');tmp.replace(path)
def sha(path):
    with Path(path).open('rb') as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
def canon(text):
    return ''.join(c for c in unicodedata.normalize('NFKC',str(text or '')).casefold() if c.isalnum())
def csv_write(path, rows, fields=None):
    path=Path(path);path.parent.mkdir(parents=True,exist_ok=True)
    with path.open('w',newline='',encoding='utf-8-sig') as stream:
        writer=csv.DictWriter(stream,fieldnames=fields or list(rows[0]))
        writer.writeheader();writer.writerows(rows)

REGIONS = [
 ['Ala','AL','Alabama'],['Alaska','AK'],['Ariz','AZ','Arizona'],['Ark','AR','Arkansas'],
 ['Calif','CA','California'],['Colo','CO','Colorado'],['Conn','CT','Connecticut'],
 ['Del','DE','Delaware'],['Fla','FL','Florida'],['Ga','GA','Georgia'],['Hawaii','HI'],
 ['Idaho','ID'],['Ill','IL','Illinois'],['Ind','IN','Indiana'],['Iowa','IA'],
 ['Kans','KS','Kansas'],['Ky','KY','Kentucky'],['La','LA','Louisiana'],['Maine','ME'],
 ['Md','MD','Maryland'],['Mass','MA','Massachusetts'],['Mich','MI','Michigan'],
 ['Minn','MN','Minnesota'],['Miss','MS','Mississippi'],['Mo','MO','Missouri'],
 ['Mont','MT','Montana'],['Nebr','NE','Nebraska'],['Nev','NV','Nevada'],
 ['NH','New Hampshire'],['NJ','New Jersey'],['NMex','NM','New Mexico'],['NY','New York'],
 ['NC','North Carolina'],['NDak','ND','North Dakota'],['Ohio','OH'],['Okla','OK','Oklahoma'],
 ['Ore','OR','Oregon'],['Pa','PA','Pennsylvania'],['RI','Rhode Island'],['SC','South Carolina'],
 ['SDak','SD','South Dakota'],['Tenn','TN','Tennessee'],['Tex','TX','Texas'],['Utah','UT'],
 ['Vt','VT','Vermont'],['Va','VA','Virginia'],['WVa','WV','West Virginia'],
 ['Wash','WA','Washington'],['Wis','WI','Wisconsin'],['Wyo','WY','Wyoming'],
 ['DC','District of Columbia'],['BC','British Columbia'],['Alta','Alberta'],
 ['Ont','Ontario'],['Que','Quebec'],['Man','Manitoba'],['Sask','Saskatchewan'],
 ['NS','Nova Scotia'],['PEI','Prince Edward Island'],['Eng','England'],['Ger','Germany'],
 ['Mex','Mexico'],['SAfrica','South Africa'],['Can','Canada'],['NZ','New Zealand'],
 ['SAustralia','South Australia']]

def alias_normalize(value, field):
    """Fixed token expansions only; word order and all campus/unit tokens remain."""
    if field in {'name','birth_year'}:return canon(value)
    aliases={canon(v):canon(group[-1]) for group in REGIONS for v in group}
    if field=='institutions':
        aliases.update(univ='university',col='college',inst='institute',sch='school',
            technol='technology',tech='technical',polytech='polytechnic',nat='national',
            med='medical',eng='engineering',agr='agricultural',am='american',sci='science',
            environ='environmental')
    aliases.update(st='saint',ft='fort')
    if field in {'birthplace','city'}:aliases.update(e='east',w='west',n='north',s='south')
    # Match a two-word full state/country before its individual words.
    tokens=re.findall(r"[^\W_]+",unicodedata.normalize('NFKC',value or '').casefold())
    if field=='institutions':tokens=[t for t in tokens if t not in {'of','the','and','at'}]
    return ''.join(aliases.get(canon(t),canon(t)) for t in tokens)

def equivalent(left,right,field,aliases):
    if canon(left)==canon(right):return True
    a,b=alias_normalize(left,field),alias_normalize(right,field)
    if a==b:return True
    for group in aliases.get(field,[]):
        keys={canon(v) for v in group}|{alias_normalize(v,field) for v in group}
        if (canon(left) in keys or a in keys) and (canon(right) in keys or b in keys):return True
    return False

def distinct(values, aliases):
    result=[]
    for value in values:
        if value and not any(equivalent(value,v,'institutions',aliases) for v in result):result.append(value)
    return result

def reference_freeze(root):
    if (root/'reference_freeze.json').exists():raise ValueError('Reference freeze already exists')
    manifest=read(root/'manifest.json');rows=[];pages=[];excluded=[]
    for draw in manifest['page_draws']:
        stem=Path(draw['source_file']).stem
        for page in draw['proposed_pages']:
            path=root/'reference_notes'/f'{stem}_p{page:04d}.txt'
            order=0
            for line in path.read_text(encoding='utf-8').splitlines():
                if line.startswith('# Excluded'):
                    excluded.append(dict(page_key=f'{draw["source_file"]}:{page}',decision=line[2:]));continue
                if not line.strip() or line.startswith('#'):continue
                parts=[s.strip() for s in line.split('|')]
                assert len(parts)==5,(path,line)
                name,place,year,institutions,notes=parts;order+=1
                def field(value):
                    if value=='<absent>':return dict(status='absent',value=None)
                    if value=='<unreadable>':return dict(status='source_unreadable',value=None)
                    return dict(status='readable',value=value)
                fields=dict(name=field(name),birthplace=field(place),birth_year=field(year))
                fields['city']=field('<absent>' if 'city_absent:' in notes else place.split(',')[0])
                fields['institutions']=field(institutions)
                if fields['institutions']['status']=='readable':fields['institutions']['value']=[s.strip() for s in institutions.split(';') if s.strip()]
                if fields['birth_year']['status']=='readable':assert re.fullmatch(r'(18|19)\d{2}',year)
                rows.append(dict(id=f'{stem}:{page}:{order:02d}',source_file=draw['source_file'],pdf_page=page,
                    page_key=f'{draw["source_file"]}:{page}',reading_order=order,fields=fields,notes=notes,
                    reference_origin='Codex manual source-scan reading before automated extraction predictions',
                    scan=f'scans/{stem}_p{page:04d}.png',annotation_file=path.relative_to(root).as_posix()))
            assert order>0
            image=root/'scans'/f'{stem}_p{page:04d}.png'
            pages.append(dict(page_key=f'{draw["source_file"]}:{page}',eligible=True,
                reason='Source scan contains full biography starts; all starts inventoried',
                biographies=order,reviewer='Codex source-only visual review',scan=image.relative_to(root).as_posix(),
                scan_sha256=sha(image),annotation_sha256=sha(path)))
    assert len(pages)==18 and not ({p['page_key'] for p in pages}&set(manifest['previous_audit_pages']))
    aliases=dict(created_at=now(),name=[],birthplace=[],city=[],birth_year=[],institutions=[
        ['Tex Col Mines','Tex Western Col','Texas College of Mines','Texas Western College','Univ Tex El Paso','University of Texas at El Paso'],
        ['Purdue Univ, Lafayette','Purdue Univ, West Lafayette','Purdue University, West Lafayette'],
        ['Denver Univ','Univ Denver','University of Denver'],
        ['Col Med, Wayne State Univ','Wayne State Univ, Col Med']],
        regional_aliases=REGIONS,word_order='preserved; Washington University and University of Washington remain distinct',
        historical_alias_sources=[dict(names=['Tex Col Mines','Tex Western Col'],
            source='https://www.utep.edu/hoop/section-1/history-and-mission.html',
            evidence='Official history records the Texas College of Mines name and its 1949 renaming to Texas Western College.')],
        policy='No fuzzy spelling matching, inferred campus, parent-school collapse, or post-prediction alias additions')
    write(root/'references.json',rows);write(root/'sample_review.json',dict(pages=pages,excluded_stubs=excluded,
        front_matter_exclusions=[],previous_pages_excluded=manifest['previous_audit_pages']))
    write(root/'aliases.json',aliases)
    write(root/'reference_freeze.json',dict(frozen_at=now(),reference_sha256=sha(root/'references.json'),
        aliases_sha256=sha(root/'aliases.json'),sample_review_sha256=sha(root/'sample_review.json'),
        manifest_sha256=sha(root/'manifest.json'),biographies=len(rows),pages=18,
        independent_human_verification=False,source_rechecks=['recheck_lynn.png','recheck_grahame.png','recheck_kelmers.png','recheck_joseph.png']))
    print(f'Frozen source-only references: {len(rows)} biographies on 18 fresh pages')

def page_draw(documents, previous):
    rng=random.Random(SEED);result=[]
    for doc in sorted(documents,key=lambda d:d['source_file']):
        order=rng.sample(range(1,doc['page_count']+1),doc['page_count'])
        candidates=[p for p in order if f"{doc['source_file']}:{p}" not in previous]
        result.append(dict(source_file=doc['source_file'],page_order=order,
                           candidates=candidates,proposed_pages=candidates[:2]))
    return result

def render_page(root, doc, page):
    import pypdfium2 as pdfium
    folder=root/'scans';folder.mkdir(exist_ok=True)
    stem=f"{Path(doc['source_file']).stem}_p{page:04d}"
    image_path=folder/f'{stem}.png'
    if image_path.exists():return image_path
    pdf=pdfium.PdfDocument(doc['source']['path']);p=pdf[page-1]
    image=p.render(scale=300/72).to_pil().convert('RGB')
    w,h=image.size
    image.resize((round(w*2/3),round(h*2/3))).save(image_path)
    for side,(x0,x1) in dict(left=(0,.60),right=(.35,1)).items():
        for part,(y0,y1) in dict(top=(0,.55),bottom=(.45,1)).items():
            image.crop((int(w*x0),int(h*y0),int(w*x1),int(h*y1))).save(folder/f'{stem}_{side}_{part}.png')
    p.close();pdf.close()
    return image_path

def prepare(root):
    root.mkdir(parents=True,exist_ok=True)
    if (root/'manifest.json').exists():raise ValueError('Refusing to replace an existing frozen audit')
    manifest=read(OLD/'comparison_manifest.json')
    previous={p['page_key'] for p in read(OLD/'audit/aligned.json')['pages']}
    docs=manifest['documents']
    assert len(docs)==9 and sum(d['page_count'] for d in docs)==1360
    sources=[]
    for doc in docs:
        for identity,key in [(doc['source'],'pdf_sha256'),(doc['baseline'],'docx_sha256')]:
            assert sha(identity['path'])==doc[key],f"Source changed: {identity['path']}"
            sources.append(dict(path=identity['path'],sha256=doc[key]))
    relative=[Path('paths.R'),Path('prep/amws/state_alias.R')]
    relative += [p.relative_to(REPO) for p in (REPO/'prep/amws/amws_1986').rglob('*')
                 if p.is_file() and p.suffix in {'.R','.py'} and '__pycache__' not in p.parts]
    relative += [p.relative_to(REPO) for p in (REPO/'prep/amws/education').glob('*.py')]
    frozen=[]
    for rel in sorted(set(relative)):
        target=root/'frozen/repo'/rel;target.parent.mkdir(parents=True,exist_ok=True)
        shutil.copy2(REPO/rel,target);frozen.append(dict(path=rel.as_posix(),sha256=sha(target)))
    draws=page_draw(docs,previous)
    write(root/'manifest.json',dict(created_at=now(),sample_seed=SEED,bootstrap_seed=20261022,
        previous_comparison_manifest_sha256=sha(OLD/'comparison_manifest.json'),
        previous_audit_pages=sorted(previous),documents=docs,page_draws=draws,sources=sources,
        frozen_code=frozen,reference_protocol='Scan-only fields before prediction review; field-level unreadability retained',
        scoring_protocol='Shared frozen automated rules; no sample-specific tuning or manual prediction corrections'))
    for draw in draws:
        doc=next(d for d in docs if d['source_file']==draw['source_file'])
        for page in draw['proposed_pages']:render_page(root,doc,page)
    print(json.dumps([{k:v for k,v in d.items() if k in {'source_file','proposed_pages'}} for d in draws],indent=2))

def check_freeze(root):
    manifest=read(root/'manifest.json');freeze=read(root/'reference_freeze.json')
    for file,key in [('references.json','reference_sha256'),('aliases.json','aliases_sha256'),
                     ('sample_review.json','sample_review_sha256'),('manifest.json','manifest_sha256')]:
        assert sha(root/file)==freeze[key],f'Frozen reference changed: {file}'
    for item in manifest['frozen_code']:
        assert sha(root/'frozen/repo'/item['path'])==item['sha256'],item['path']
    for item in manifest['sources']:assert sha(item['path'])==item['sha256'],item['path']
    return manifest

def predict(root,skip_staged=False,parsed_only=False):
    """Full documents only. This phase never loads reference biography values."""
    manifest=check_freeze(root)
    driver=Path(__file__).with_name('extract_ocr_audit.R')
    folder=root/'prediction';folder.mkdir(exist_ok=True)
    freeze_path=folder/'extraction_run.json'
    contract=dict(driver_sha256=sha(driver),frozen_code_sha256=sha(root/'manifest.json'),
        adapter='Word XML text visited once; Paddle raw prediction, body layout labels in original block_order',
        rules='Frozen R segmentation/name/birthplace/year and generic corrections; multi_v03 plus postsecondary_v1, all degrees, no manual overrides',
        references_used=False,manual_paragraph_repairs=False)
    if freeze_path.exists():
        assert read(freeze_path)['contract']==contract,'Extraction adapter changed; do not reuse predictions'
    else:write(freeze_path,dict(created_at=now(),contract=contract))
    frozen=root/'frozen/repo'
    sys.path.insert(0,str(frozen/'prep/amws/education'))
    import education_regex as education
    import postsecondary_qualifications as additional
    def one(doc,method):
        stem=Path(doc['source_file']).stem;stage=folder/method/stem;stage.mkdir(parents=True,exist_ok=True)
        blocks=[];inputs=[]
        if method=='word':
            path=OLD/'baseline'/f'{stem}.json';baseline=read(path)
            assert baseline['source_sha256']==doc['docx_sha256']
            inputs.append(dict(path=str(path),sha256=sha(path)))
            for p in baseline['paragraphs']:
                blocks.append(dict(method=method,source_file=doc['source_file'],pdf_page='',
                    block_index=p['index'],block_status='body',text=p['text']))
        else:
            for page in range(1,doc['page_count']+1):
                path=OLD/'ocr'/stem/f'page_{page:04d}.json';j=read(path)
                assert j['source_file']==doc['source_file'] and j['pdf_page_number']==page
                inputs.append(dict(path=str(path),sha256=sha(path)))
                for idx,b in sorted(enumerate(j['prediction']['parsing_res_list']),key=lambda v:(v[1].get('block_order',v[0]),v[0])):
                    text=b.get('block_content','');label=b.get('block_label','')
                    body=label in ('text','paragraph_title','reference','reference_content') and bool(text.strip())
                    blocks.append(dict(method=method,source_file=doc['source_file'],pdf_page=page,
                        block_index=f'{page}:{idx}',block_status='body' if body else 'excluded_layout',
                        text=text,label=label,bbox=b.get('block_bbox')))
        input_manifest=dict(inputs=inputs,contract=contract)
        if (stage/'complete.json').exists():
            complete=read(stage/'complete.json')
            assert complete['input_manifest']==input_manifest and sha(stage/'predictions.json')==complete['predictions_sha256']
            return dict(method=method,source_file=doc['source_file'],records=complete['records'],resumed=True)
        write(stage/'inputs.json',input_manifest)
        csv_write(stage/'blocks.csv',blocks)
        if not (stage/'parsed.csv').exists():
            with (stage/'extract.log').open('w',encoding='utf-8') as log:
                subprocess.run([r'C:\Program Files\R\R-4.5.2\bin\Rscript.exe',str(driver),str(frozen),
                    str(stage/'blocks.csv'),str(stage/'parsed.csv')],stdout=log,stderr=subprocess.STDOUT,check=True)
        csv.field_size_limit(10_000_000)
        with (stage/'parsed.csv').open(encoding='utf-8-sig',newline='') as f:records=list(csv.DictReader(f))
        for row in records:
            year=int(row['birth_year']) if re.fullmatch(r'\d{4}',row['birth_year']) else None
            extracted=education.extract(row['raw_text_adjusted'],year,1986)
            awards=extracted['degrees']+additional.extra_awards(row['raw_text_adjusted'],year,1986)
            row['education']=dict(status=extracted['status'],flags=extracted['flags'],awards=awards,
                rule_version=education.RULE_VERSION,additional_version=additional.VERSION)
            row['institutions_raw']=[d['institution_as_written'] for d in awards if d['institution_as_written']]
        write(stage/'predictions.json',records)
        write(stage/'complete.json',dict(input_manifest=input_manifest,records=len(records),
            predictions_sha256=sha(stage/'predictions.json'),parsed_sha256=sha(stage/'parsed.csv'),completed_at=now()))
        return dict(method=method,source_file=doc['source_file'],records=len(records),resumed=False)
    jobs=[(doc,method) for doc in manifest['documents'] for method in ['word','paddle']]
    if skip_staged:
        jobs=[(doc,method) for doc,method in jobs if not (folder/method/Path(doc['source_file']).stem/'inputs.json').exists()]
    if parsed_only:
        jobs=[(doc,method) for doc,method in jobs if (folder/method/Path(doc['source_file']).stem/'parsed.csv').exists()]
    with ThreadPoolExecutor(max_workers=9) as pool:
        futures=[pool.submit(one,*job) for job in jobs]
        results=[]
        for future in futures:
            result=future.result();results.append(result);print(json.dumps(result),flush=True)
    write(folder/'complete.json',dict(completed_at=now(),documents=results))

def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('phase',choices=['prepare','render','freeze-references','predict'])
    p.add_argument('--root',type=Path,default=ROOT)
    p.add_argument('--pdf');p.add_argument('--page',type=int)
    p.add_argument('--skip-staged',action='store_true',help='Skip already staged work while its independent R workers finish')
    p.add_argument('--parsed-only',action='store_true',help='Finish existing parsed checkpoints without launching R workers')
    args=p.parse_args()
    if args.phase=='prepare':prepare(args.root)
    elif args.phase=='freeze-references':reference_freeze(args.root)
    elif args.phase=='predict':predict(args.root,args.skip_staged,args.parsed_only)
    elif args.phase=='render':
        doc=next(d for d in read(args.root/'manifest.json')['documents'] if d['source_file']==args.pdf)
        print(render_page(args.root,doc,args.page))

if __name__=='__main__':main()

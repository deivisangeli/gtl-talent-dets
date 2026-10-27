"""Three-arm, immutable-input comparison of saved AMWS OCR paragraph repairs."""
from __future__ import annotations
import argparse,copy,hashlib,json,random,shutil,sys
from collections import Counter
from datetime import datetime,timezone
from pathlib import Path

CHAT=Path(r'C:\Users\megaj\Documents\Codex\2026-10-19\based-on-the-c')
OLD=CHAT/'work/amws_ocr_comparison_20261019'
FIELD=CHAT/'work/amws_field_extraction_audit_20261019'
ROOT=CHAT/'work/paragraph_repaired_comparison_20261021'
REPO=Path(__file__).resolve().parents[4]
SUPPORT=Path(__file__).parent
def now():return datetime.now(timezone.utc).isoformat()
def read(p):return json.loads(Path(p).read_text(encoding='utf8'))
def sha(p):
    with Path(p).open('rb') as f:return hashlib.file_digest(f,'sha256').hexdigest()
def write(p,v):
    p=Path(p);p.parent.mkdir(parents=True,exist_ok=True);t=p.with_name(p.name+'.tmp')
    t.write_text(json.dumps(v,ensure_ascii=False,indent=2)+'\n',encoding='utf8');t.replace(p)
def put(p,data):
    p.parent.mkdir(parents=True,exist_ok=True);t=p.with_name(p.name+'.tmp');t.write_bytes(data);t.replace(p)
def freeze(root):
    path=root/'manifest.json'
    if path.exists():return read(path)
    m=read(FIELD/'manifest.json');root.mkdir(parents=True,exist_ok=True)
    assert len(m['documents'])==9 and sum(d['page_count'] for d in m['documents'])==1360
    sources=[]
    for x in m['sources']:
        assert sha(x['path'])==x['sha256'];sources.append(x)
    raw=[]
    for d in m['documents']:
        for p in range(1,d['page_count']+1):
            for ext in ('json','txt','md'):
                file=OLD/'ocr'/Path(d['source_file']).stem/f'page_{p:04d}.{ext}'
                raw.append(dict(path=str(file),sha256=sha(file),bytes=file.stat().st_size))
    shutil.copytree(FIELD/'frozen/repo',root/'frozen/repo',dirs_exist_ok=True)
    repairs=[]
    for name in ('repair_ocr_paragraphs.py','repair_ocr_corpus.py','repair_ocr_exceptions.py','manual_word_evidence.py','extract_ocr_audit.R'):
        dest=root/'frozen/repair'/name;dest.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(SUPPORT/name,dest)
        repairs.append(dict(path=str(dest.relative_to(root)),sha256=sha(dest)))
    for name in ('references.json','references_adjudicated.json','reference_adjudications.json','reference_freeze.json','aliases.json','sample_review.json','alignments.json','alignment_review.json','unmatched_fragment_review.json'):
        if (FIELD/name).exists():shutil.copy2(FIELD/name,root/name)
    result=dict(created_at=now(),documents=m['documents'],sources=sources,raw_inputs=raw,
        frozen_parser=m['frozen_code'],frozen_repair=repairs,raw_root=str(OLD/'ocr'),
        review_seed=20261024,bootstrap_seed=20261022,reference_sample_biographies=304,
        previous_deliveries=[dict(path=str(p),sha256=sha(p)) for p in sorted((CHAT/'outputs').glob('*.zip'))],
        policy='Reuse recognized strings and saved geometry only; scan-bound paragraph assignments; no reference-assisted extraction, text correction or parser tuning.')
    write(path,result);return result
def modules(root):
    sys.path.insert(0,str(root/'frozen/repair'))
    import repair_ocr_paragraphs as repair,repair_ocr_corpus as corpus,repair_ocr_exceptions as exceptions
    return repair,corpus,exceptions
def repair_all(root):
    m=freeze(root);repair,corpus,exceptions=modules(root);out=root/'first_pass';rows=[];resumed=0
    contract={x['path']:x['sha256'] for x in m['frozen_repair'] if 'manual_word' not in x['path'] and not x['path'].endswith('.R')}
    for d in m['documents']:
        stem=Path(d['source_file']).stem
        for page in range(1,d['page_count']+1):
            source={e:OLD/'ocr'/stem/f'page_{page:04d}.{e}' for e in ('json','txt','md')}
            hashes={e:sha(p) for e,p in source.items()};paths=corpus.page_paths(out,d['source_file'],page)
            if paths['checkpoint'].exists():
                cp=read(paths['checkpoint'])
                if cp.get('source_sha256')==hashes and cp.get('contract')==contract and all(Path(x['path']).exists() and sha(x['path'])==x['sha256'] for x in cp['outputs']):
                    rows.append(cp['summary']);resumed+=1;continue
            record=read(source['json']);original={e:source[e].read_bytes() for e in ('txt','md')}
            assert record['source_file']==d['source_file'] and record['pdf_page_number']==page
            assert record['status'] in ('ok','verified_blank')
            blocks,audits,txt,md=repair.repair_page(record,original['txt'],original['md'])
            prediction=copy.deepcopy(record['prediction']);prediction['parsing_res_list']=blocks
            row=corpus.page_row(record,audits,hashes,txt,md,original)
            derived=dict(artifact_type='amws_paragraph_repair',schema_version=repair.VERSION,created_at=now(),
                source_file=d['source_file'],pdf_page_number=page,source_json_path=str(source['json']),source_sha256=hashes,
                original_record=record,corrected_prediction=prediction,repair_audit=audits,
                exports=dict(txt_sha256=hashlib.sha256(txt).hexdigest(),md_sha256=hashlib.sha256(md).hexdigest()))
            put(paths['txt'],txt);put(paths['md'],md);write(paths['json'],derived)
            write(paths['checkpoint'],dict(contract=contract,source_sha256=hashes,summary=row,
                outputs=[dict(path=str(paths[e]),sha256=sha(paths[e])) for e in ('json','txt','md')]))
            rows.append(row)
        corpus.document_exports(out,d)
        print(json.dumps(dict(stage='paragraph_first_pass',document=stem,completed=len(rows),resumed=resumed)),flush=True)
    totals=corpus.reports(out,m['documents'],rows)
    write(out/'corpus_selection.json',dict(documents=m['documents']))
    rng=random.Random(m['review_seed']);sample=[]
    for d in m['documents']:
        pool=sorted(r['pdf_page_number'] for r in rows if r['source_file']==d['source_file'] and r['repaired_blocks'])
        for p in rng.sample(pool,min(2,len(pool))):sample.append(dict(source_file=d['source_file'],pdf_page_number=p,reason='seeded random repaired page'))
    known=[('16_D_0_200.pdf',15),('16_E_900_1000.pdf',34),('16_F_800_900.pdf',100)]
    for f,p in known:
        if not any(s['source_file']==f and s['pdf_page_number']==p for s in sample):sample.append(dict(source_file=f,pdf_page_number=p,reason='previously documented reading-order failure'))
    write(root/'visual_review_sample.json',dict(seed=m['review_seed'],pages=sample))
    print(json.dumps(totals),flush=True)
def contexts(root):
    import pypdfium2 as pdfium
    m=read(root/'manifest.json');repair,corpus,exceptions=modules(root);out=root/'first_pass'
    queue=read(out/'review_queue.json')['blocks'];requested={(q['source_file'],q['pdf_page_number']) for q in queue}
    requested.update((p['source_file'],p['pdf_page_number']) for p in read(root/'visual_review_sample.json')['pages'])
    for d in m['documents']:
        with pdfium.PdfDocument(d['source']['path']) as pdf:
            for f,p in sorted(requested):
                if f!=d['source_file']:continue
                image=corpus.page_paths(out,f,p)['scan']
                if not image.exists():corpus.render(pdf,p,image)
    cs=[]
    for q in queue:
        ctx=exceptions.context(out,q);ctx['proposed_groups']=exceptions.automatic_groups(ctx)
        cs.append(ctx)
    write(root/'exception_contexts.json',cs)
    print(json.dumps(dict(exception_blocks=len(cs),exception_pages=len({(c['source_file'],c['pdf_page_number']) for c in cs}),automatic_group_proposals=sum(c['proposed_groups'] is not None for c in cs),reasons=dict(Counter(c['rejection_reason'] for c in cs)))),flush=True)
def finalize_repair(root):
    m=read(root/'manifest.json');repair,corpus,ex=modules(root)
    cs=read(root/'exception_contexts.json');review=read(root/'paragraph_decisions.json')
    assert review['all_exception_blocks_reviewed'] and len(review['decisions'])==len(cs)==122
    by_page={}
    for c in cs:
        ex.decision_valid(c,review['decisions'][c['block_id']]);by_page.setdefault((c['source_file'],c['pdf_page_number']),[]).append(c)
    sample=read(root/'visual_review_sample.json')['pages'];visual=[]
    flags={7:'Kennell John Hawkes heading/date detections sort imperfectly inside his paragraph.',11:'Martin Selye date fragments precede the heading under saved box order.',13:'Peterson John Carl discipline/heading detections sort imperfectly within the final paragraph.',17:'Neal Austin Smith date fragments precede the heading under saved geometry.',18:'Jacober and Jacobi George Thomas remain garbled/interleaved in pre-existing text blocks, outside collapsed-reference recovery.',19:'Peissner/Peitman leading blocks retain original recognition and reading-order damage; left-edge name clipping remains.',20:'Solonche/Solorzano pre-existing text blocks retain recognized interleaving and wrong local line order.'}
    for i,p in enumerate(sample):
        visual.append(dict(**p,reviewed=True,reviewed_at=now(),reviewer='Codex agent; not independently human-verified',
            review_status='flagged_as_expected' if i in flags else 'passed',
            notes=flags.get(i,'Compared source scan with repaired block starts and column sequence; distinct printed biographies and incoming/outgoing continuations are retained. Recognition accuracy is not certified.'),
            evidence=f'review_evidence/sample_{i:02d}.png'))
    write(root/'visual_review.json',dict(seed=20261024,pages=visual,completed_before_extraction=True))
    out=root/'repaired';rows=[];seen=set();retained=0
    decisions_sha=sha(root/'paragraph_decisions.json')
    for d in m['documents']:
        for page in range(1,d['page_count']+1):
            f=d['source_file'];contexts=by_page.get((f,page),[])
            derived,exports,audits=ex.derive(root/'first_pass',f,page,contexts,review['decisions'])
            combined=ex.combined_audits(derived,audits) if contexts else derived['repair_audit']
            original=derived['original_record'];src=corpus.page_paths(OLD/'ocr',f,page)
            assert all(sha(src[e])==derived['source_sha256'][e] for e in ('json','txt','md'))
            # Independent exact source-line and confidence/box conservation check.
            for b in derived['corrected_prediction']['parsing_res_list']:
                if 'paragraph_repair' not in b:continue
                col=next(c for c in original['prediction']['column_predictions'] if c['column_number']==b['column_number'])
                raw=col['prediction']['overall_ocr_res']
                for line in b['paragraph_repair']['lines']:
                    i=line['source_line_index'];key=(f,page,b['column_number'],i)
                    assert key not in seen,('Duplicate repaired source detection',key);seen.add(key)
                    assert line['text']==raw['rec_texts'][i] and line['confidence']==raw['rec_scores'][i]
                    box=list(raw['rec_boxes'][i]);box[0]+=col['x_offset'];box[2]+=col['x_offset'];assert line['bbox']==box
                    retained+=1
            assert {k:v for k,v in derived['corrected_prediction'].items() if k!='parsing_res_list'}=={k:v for k,v in original['prediction'].items() if k!='parsing_res_list'}
            paths=corpus.page_paths(out,f,page);write(paths['json'],derived)
            for e in ('txt','md'):put(paths[e],exports[e])
            row=corpus.page_row(original,combined,derived['source_sha256'],exports['txt'],exports['md'],{e:src[e].read_bytes() for e in ('txt','md')})
            rows.append(row)
            write(paths['checkpoint'],dict(source_sha256=derived['source_sha256'],decision_sha256=decisions_sha,
                outputs={e:dict(path=str(paths[e]),sha256=sha(paths[e])) for e in ('json','txt','md')},summary=row))
            scan=corpus.page_paths(root/'first_pass',f,page)['scan']
            if scan.exists():
                paths['scan'].parent.mkdir(parents=True,exist_ok=True);shutil.copy2(scan,paths['scan'])
                put(paths['viewer'],corpus.view_html(original,derived['corrected_prediction']['parsing_res_list'],row,{e:src[e].read_bytes() for e in ('txt','md')},exports['txt'],exports['md'],{'Comparison':'../../index.html'}))
        corpus.document_exports(out,d)
        print(json.dumps(dict(stage='reviewed_repair_verified',document=d['source_file'],pages=len(rows))),flush=True)
    put(out/'review.js',corpus.VIEW_JS.encode('utf8'))
    totals=corpus.reports(out,m['documents'],rows)
    write(root/'repair_verification.json',dict(complete=True,verified_at=now(),pages=len(rows),totals=totals,
        recognized_detections_conserved=retained,recognized_strings_confidence_and_boxes_unchanged=True,
        original_predictions_preserved=True,geometry_preserved=True,exceptions_reviewed=len(cs),
        visual_review_pages=len(visual),blank_pages=sum(r['outcome']=='verified_blank' for r in rows)))
    write(root/'repair_freeze.json',dict(frozen_at=now(),manifest_sha256=sha(root/'manifest.json'),
        decisions_sha256=decisions_sha,visual_review_sha256=sha(root/'visual_review.json'),
        verification_sha256=sha(root/'repair_verification.json'),
        repaired_pages=[dict(path=str(p.relative_to(root)),sha256=sha(p)) for p in sorted(out.glob('*/page_*.json')) if p.name.count('.')==1]))
    assert len(rows)==1360 and totals['unresolved_blocks']==0
    print(json.dumps(totals),flush=True)
def main():
    parser=argparse.ArgumentParser();parser.add_argument('phase',choices=['repair','contexts','finalize-repair']);parser.add_argument('--root',type=Path,default=ROOT)
    args=parser.parse_args();{'repair':repair_all,'contexts':contexts,'finalize-repair':finalize_repair}[args.phase](args.root)
if __name__=='__main__':main()

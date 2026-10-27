"""Package frozen manual judgments. Does not OCR, extract fields, or repair text."""
from __future__ import annotations
import argparse, csv, hashlib, html, json, re, shutil, zipfile
from collections import Counter
from pathlib import Path
import manual_entry_audit as a

FIELDS=('name','birthplace','birth_year','education_institutions')
METHODS=('word','paddle')
OUT=a.CHAT/'outputs/manual_entry_audit_20261020'

def pct(n,d): return round(100*n/d,2) if d else None

def metrics(rows):
    result={'biographies':len(rows),'paired_decisions':dict(Counter(x['comparison'] for x in rows))}
    for method in METHODS:
        ms=[x[method] for x in rows]; n=len(ms)
        counts={
            'content_error_identified':sum(bool(x['errors']) for x in ms),
            'substantive_error_identified':sum(any(e['severity']=='substantive' for e in x['errors']) for x in ms),
            'minor_error_identified':sum(any(e['severity']=='minor' for e in x['errors']) for x in ms),
            'minor_only':sum(bool(x['errors']) and all(e['severity']=='minor' for e in x['errors']) for x in ms),
            'no_content_error_identified':sum(not x['errors'] for x in ms),
            'structural_defect':sum(bool(x['structure']) for x in ms)}
        counts.update({f'structure_{v}':sum(v in x['structure'] for x in ms)
                       for v in ('merged','fragmented','reading_order','duplication','header_intrusion')})
        counts.update({f'completeness_{v}':sum(x['completeness']==v for x in ms)
                       for v in ('complete','partially omitted','wholly omitted','uncertain')})
        fields={}
        for f in FIELDS:
            c=Counter(x['key_information'][f] for x in ms)
            d=sum(v for k,v in c.items() if k not in ('absent','source-unreadable'))
            fields[f]={'correct':c['correct'],'denominator':d,'percent':pct(c['correct'],d),'outcomes':dict(c)}
        result[method]={'counts':counts,'percentages':{k:pct(v,n) for k,v in counts.items()},'key_information':fields}
    return result

def csv_write(path,rows):
    rows=list(rows)
    with path.open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(rows[0]) if rows else []);w.writeheader();w.writerows(rows)

def validate_frozen(root):
    m=a.read(root/'manifest.json'); inv=a.read(root/'inventory.json'); fr=a.read(root/'inventory_freeze.json')
    rev=a.read(root/'reviewed_entries.json'); rf=a.read(root/'review_freeze.json')
    assert a.sha(root/'inventory.json')==fr['inventory_sha256']
    assert a.sha(root/'manifest.json')==fr['manifest_sha256']
    assert a.sha(root/'reviewed_entries.json')==rf['reviewed_entries_sha256']
    assert a.sha(root/'inventory_errata.json')==rf['inventory_errata_sha256']
    draw=a.draw(m['documents'],set(m['previous_audit_pages'])); assert draw==m['sampling']
    keys=draw['base_proposed']+draw['extra_proposed']
    assert len(keys)==len(set(keys))==20 and not set(keys)&set(m['previous_audit_pages'])
    assert len(m['previous_audit_pages'])==36
    assert {p['page_key'] for p in inv}=={p['page_key'] for p in rev}==set(keys)
    assert sum(len(p['entries']) for p in rev)==339
    invmap={p['page_key']:p for p in inv}
    for p in rev:
        assert p['status']=='reviewed' and len(p['entries'])==len(invmap[p['page_key']]['entries'])
        assert len({x['entry_id'] for x in p['entries']})==len(p['entries'])
        for x in p['entries']:
            assert x['recheck_status']=='reviewed' and x['full_text_reviewed']
            assert x['comparison'] in ('Word better','Paddle better','tie','uncertain')
            for method in METHODS:
                assert x[method]['completeness'] in ('complete','partially omitted','wholly omitted','uncertain')
                assert set(x[method]['key_information'])==set(FIELDS)
                assert all(v in ('correct','incorrect','missing','absent','source-unreadable') for v in x[method]['key_information'].values())
            for f in FIELDS:
                assert (x['word']['key_information'][f] in ('absent','source-unreadable'))==(x['paddle']['key_information'][f] in ('absent','source-unreadable'))
    return m,inv,rev

def copy_file(src,dst):
    dst.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(src,dst)

def sampling_log(m,inv):
    decisions=[]; selected={p['page_key'] for p in inv}; prior=set(m['previous_audit_pages'])
    for d in m['sampling']['draws']:
        taken=0
        for rank,p in enumerate(d['page_order'],1):
            key=f"{d['source_file']}:{p}"
            reason='previously audited page' if key in prior else 'selected: biography starts visually confirmed'
            decisions.append(dict(phase='two per PDF',candidate_rank=rank,page_key=key,decision='rejected' if key in prior else 'selected',reason=reason))
            if key not in prior:taken+=1
            if taken==2:break
    for rank,key in enumerate(m['sampling']['extra_proposed'],1):
        decisions.append(dict(phase='additional two',candidate_rank=rank,page_key=key,decision='selected',reason='biography starts visually confirmed; no quality-based replacement'))
    return decisions

def build(root=a.ROOT,out=OUT):
    m,inv,pages=validate_frozen(root)
    out.mkdir(parents=True,exist_ok=True)
    for f in ('manifest.json','inventory.json','inventory_freeze.json','inventory_errata.json','reviewed_entries.json','review_freeze.json','draft_revision_log.json','full_document_word_searches.json','full_document_paddle_searches.json','recheck_log.json'):
        copy_file(root/f,out/f)
    rows=[x for p in pages for x in p['entries']]
    pooled=metrics(rows)
    per_page={p['page_key']:metrics(p['entries']) for p in pages}
    files=sorted({p['page_key'].split(':')[0] for p in pages})
    per_file={f:metrics([x for x in rows if x['page_key'].split(':')[0]==f]) for f in files}
    all_metrics=dict(pooled=pooled,per_pdf=per_file,per_page=per_page)
    a.write(out/'results.json',all_metrics)
    ledger=[]
    for x in rows:
        for method in METHODS:
            v=x[method]
            ledger.append(dict(entry_id=x['entry_id'],page_key=x['page_key'],printed_name=x['name'],method=method,
                content_error=bool(v['errors']),substantive_error=any(e['severity']=='substantive' for e in v['errors']),
                minor_error=any(e['severity']=='minor' for e in v['errors']),completeness=v['completeness'],
                structural_defects=';'.join(v['structure']),**v['key_information'],
                reference_birthplace=x['reference']['full_birthplace'],reference_printed_birth_year=x['reference']['printed_birth_year'],
                reference_education_institutions=';'.join(x['reference']['education_institutions']),
                source_locations=json.dumps(v['locations'],ensure_ascii=False),error_examples=json.dumps(v['errors'],ensure_ascii=False),
                comparison=x['comparison'],reason=x['comparison_reason'],reviewer='Codex agent',independent_human_verification=False))
    csv_write(out/'entry_method_ledger.csv',ledger)
    csv_write(out/'reference_ledger.csv',[dict(entry_id=x['entry_id'],page_key=x['page_key'],**{k:json.dumps(v,ensure_ascii=False) if isinstance(v,list) else v for k,v in x['reference'].items()}) for x in rows])
    csv_write(out/'sampling_decisions.csv',sampling_log(m,inv))
    csv_write(out/'prior_page_exclusions.csv',[dict(page_key=k,reason='previous audit; excluded before this draw') for k in m['previous_audit_pages']])
    csv_write(out/'page_accounting.csv',[dict(page_key=p['page_key'],biography_starts=len(p['entries']),excluded_stubs=';'.join(p['excluded_stubs']),incoming_continuation=p['incoming_continuation'],outgoing_continuation=p['outgoing_continuation'],status=p['status']) for p in pages])
    flat=[]
    for scope,groups in [('pooled',{'all':pooled}),('pdf',per_file),('page',per_page)]:
        for group,data in groups.items():
            for method in METHODS:
                for measure,n in data[method]['counts'].items():flat.append(dict(scope=scope,group=group,method=method,measure=measure,numerator=n,denominator=data['biographies'],percent=pct(n,data['biographies'])))
                for measure,v in data[method]['key_information'].items():flat.append(dict(scope=scope,group=group,method=method,measure=measure,numerator=v['correct'],denominator=v['denominator'],percent=v['percent']))
    csv_write(out/'metrics.csv',flat)
    csv_write(out/'paired_decisions.csv',[dict(scope=s,group=g,decision=k,count=d['paired_decisions'].get(k,0),denominator=d['biographies'],percent=pct(d['paired_decisions'].get(k,0),d['biographies'])) for s,groups in [('pooled',{'all':pooled}),('pdf',per_file),('page',per_page)] for g,d in groups.items() for k in ('Word better','Paddle better','tie','uncertain')])
    sources={};source_hashes=[]
    for d in m['documents']:
        stem=Path(d['source_file']).stem; w=a.read(root/'word_evidence'/f'{stem}.json')
        sources[stem]=w['paragraphs']
        copy_file(root/'word_evidence'/f'{stem}.json',out/'sources/word'/f'{stem}.json')
        copy_file(Path(d['baseline']['path']),out/'sources/word'/f'{stem}.docx')
        (out/'sources/word'/f'{stem}.txt').write_text('\n\n'.join(f"[{p['index']}] {p['text']}" for p in w['paragraphs']),encoding='utf8')
        copy_file(a.OLD/'ocr'/stem/'document.txt',out/'sources/paddle'/f'{stem}.txt')
        for source,expected in [(d['source']['path'],d['pdf_sha256']),(d['baseline']['path'],d['docx_sha256'])]:
            actual=a.sha(source);assert actual==expected,'Source modified: '+source
            source_hashes.append(dict(path=source,expected_sha256=expected,actual_sha256=actual,unchanged=True))
    for old in m['previous_archives']:assert a.sha(old['path'])==old['sha256']
    continuation=set()
    for p in pages:
        f,pnum=p['page_key'].split(':'); stem=Path(f).stem; num=int(pnum)
        # Explicit source-only inventory records all outgoing page continuations.
        if re.search(r'PDF\s*'+str(num+1)+r'\b',p['outgoing_continuation']):continuation.add((stem,num+1))
        for n in [num]:
            for suffix in ('','_left_top','_left_bottom','_right_top','_right_bottom'):
                copy_file(root/'scans'/f'{stem}_p{n:04d}{suffix}.png',out/'scans'/f'{stem}_p{n:04d}{suffix}.png')
        p['word_fragments']={x['entry_id']:[sources[stem][i] for i in x['word']['locations']] for x in p['entries']}
        p['paddle_text']=(a.OLD/'ocr'/stem/f'page_{num:04d}.txt').read_text(encoding='utf8')
        p['paddle_blocks']=a.read(a.OLD/'ocr'/stem/f'page_{num:04d}.json')['prediction']['parsing_res_list']
        p['continuation_scans']=[]
        if (stem,num+1) in continuation:p['continuation_scans'].append(f'scans/{stem}_p{num+1:04d}.png')
        for ext in ('json','txt','md'):copy_file(a.OLD/'ocr'/stem/f'page_{num:04d}.{ext}',out/'sources/paddle/raw'/stem/f'page_{num:04d}.{ext}')
    for stem,num in continuation:
        for suffix in ('','_left_top','_left_bottom','_right_top','_right_bottom'):
            copy_file(root/'scans'/f'{stem}_p{num:04d}{suffix}.png',out/'scans'/f'{stem}_p{num:04d}{suffix}.png')
        for ext in ('json','txt','md'):copy_file(a.OLD/'ocr'/stem/f'page_{num:04d}.{ext}',out/'sources/paddle/raw'/stem/f'page_{num:04d}.{ext}')
    for f in (root/'word_visual').glob('*'):
        if f.suffix in ('.pdf','.json','.png'):copy_file(f,out/'word_visual'/f.name)
    for f in a.OLD.joinpath('provenance').glob('*'):copy_file(f,out/'provenance/previous_ocr_configuration'/f.name)
    for f in (a.OLD/'ocr').glob('*/manifest.json'):copy_file(f,out/'provenance/ocr_runs'/f.parent.name/f.name)
    for name in ('comparison_validation.json','controller_final_stdout.log','controller_final_stderr.log'):
        copy_file(a.OLD/name,out/'provenance/previous_ocr_configuration'/name)
    for f in ('manual_reviews.py','manual_final_rechecks.py','freeze_manual_reviews.py','manual_recheck_search.py','manual_paddle_search.py'):
        copy_file(a.CHAT/'work'/f,out/'provenance'/f)
    for f in ('manual_entry_audit.py','manual_entry_evidence.py','manual_word_evidence.py','build_manual_entry_audit.py','manual_entry_viewer.html','test_manual_entry_audit.py','recompute_manual_entry_metrics.py'):
        copy_file(Path(__file__).parent/f,out/'provenance'/f)
    template=Path(__file__).with_name('manual_entry_viewer.html').read_text(encoding='utf8')
    payload=json.dumps(dict(pages=pages,metrics=all_metrics),ensure_ascii=False).replace('</','<\\/')
    (out/'index.html').write_text(template.replace('__AUDIT_DATA__',payload),encoding='utf8')
    ocr_checks=[]
    with zipfile.ZipFile(m['previous_archives'][0]['path']) as previous:
        for f in (out/'sources/paddle/raw').rglob('*'):
            if not f.is_file():continue
            relative=f.relative_to(out/'sources/paddle/raw').as_posix()
            archived=hashlib.sha256(previous.read('amws_ocr_comparison_20261019/ocr/'+relative)).hexdigest()
            assert a.sha(f)==archived,'Raw OCR changed: '+relative
            ocr_checks.append(dict(path='sources/paddle/raw/'+relative,sha256=archived,identical_to_previous_archive=True))
        for stem in sources:
            f=out/'sources/paddle'/f'{stem}.txt'
            archived=hashlib.sha256(previous.read(f'amws_ocr_comparison_20261019/ocr/{stem}/document.txt')).hexdigest()
            assert a.sha(f)==archived
            ocr_checks.append(dict(path=f'sources/paddle/{stem}.txt',sha256=archived,identical_to_previous_archive=True))
    a.write(out/'source_verification.json',dict(verified_at=a.now(),sources=source_hashes,raw_ocr_archive_checks=ocr_checks,previous_archives=m['previous_archives'],raw_ocr_reused=True,no_ocr_or_field_extractor_run=True,no_paragraph_repair=True))
    write_report(out,pooled,per_file,per_page,rows)
    print(json.dumps(pooled,ensure_ascii=False,indent=2))
    return out

def write_report(out,pooled,per_file,per_page,rows):
    n=len(rows); lines=['# Manual comparison of Word and raw PaddleOCR','',
        'This is a manual comparison on a small stratified sample: 20 fresh pages from nine edition 16 PDF/Word pairs, containing 339 biography starts. Every biography was read through its printed end, including outgoing continuations. Judgments and scan references were transcribed and reviewed by a Codex agent, without independent human verification.','',
        'Recognition and paragraph structure give different answers. Paddle preserves more full names and education institution sets; Word preserves more full birthplaces and printed birth-year digits. Neither method dominates. The paired overall judgments also include paragraph quality and retain uncertainty when the methods have different strengths.','',
        '| Measure | Word | Raw PaddleOCR |','|---|---:|---:|']
    for label,kind in [('Entries with a content error identified','content_error_identified'),('Entries with a substantive error identified','substantive_error_identified'),('Entries with minor errors only','minor_only'),('Entries with a structural defect','structural_defect'),('Partial omissions','completeness_partially omitted'),('Whole-entry omissions','completeness_wholly omitted'),('Completeness uncertain','completeness_uncertain')]:
        vals=[f"{pooled[m]['counts'][kind]}/{n} ({pooled[m]['percentages'][kind]:.1f}%)" for m in METHODS]
        lines.append(f'| {label} | '+ ' | '.join(vals)+' |')
    lines+=['','Key information is checked directly in the transcription, not through a field extractor. Institution scores require the entire printed education institution set, including explicitly printed units/campuses and honorary qualifications. They are not precision/recall estimates.','',
        '| Key information | Word correct/readable present | Paddle correct/readable present | Paddle − Word (pp) |','|---|---:|---:|---:|']
    for field in FIELDS:
        w=pooled['word']['key_information'][field];p=pooled['paddle']['key_information'][field]
        lines.append(f"| {field.replace('_',' ')} | {w['correct']}/{w['denominator']} ({w['percent']:.1f}%) | {p['correct']}/{p['denominator']} ({p['percent']:.1f}%) | {p['percent']-w['percent']:+.1f} |")
    lines+=['','Birth years are compared exactly as printed (usually two digits), without inferring centuries. Thirteen birthplaces and fifteen birth years are absent; four additional birth years are source-unreadable and excluded from both methods’ year denominators. Ten biographies on PDF 16_D_0_200 page 62 are partly unreadable, so their completeness remains uncertain and errors elsewhere in them may be unobserved. Other readable fields remain scored.','',
        'Substantive means an error in an identity, named factual detail, numeric fact, or meaning. This strict definition includes common glyph confusions such as a year 80 becoming 8O, a ZIP digit becoming a letter, or a named institution being misspelled. It does not imply that every such error would mislead an attentive human reader. Minor errors are recoverable common-word/label spelling errors that do not change the factual identity or numeric information. Counts indicate biographies with at least one observed error, not the number of errors or CER/WER. Error examples are illustrative, not exhaustive. Case, punctuation and whitespace differences are ignored; original wording, including apparent printed typos, is retained.','',
        'Completeness distinguishes an omitted span from garbled but surviving text. Displaced fragments are shown at their original source locations and counted as structure defects, never as recognition omissions. No complete biography was found wholly absent. The Word source reader retains original XML text runs once and separately includes OOXML-generated list labels: the previous XML snapshot had omitted some automatic date labels. This is a correction to reading source evidence, not an OCR or paragraph repair; earlier deliveries were preserved.','',
        '| Overall paired decision | Biographies |','|---|---:|']
    for k in ('Word better','Paddle better','tie','uncertain'):
        c=pooled['paired_decisions'].get(k,0);lines.append(f'| {k} | {c}/{n} ({100*c/n:.1f}%) |')
    lines+=['','An uncertain overall decision often reflects a tradeoff: better recognition in one source and better paragraph boundaries in the other, or different factual errors in each. It is not a forced tie. The viewer exposes the evidence and reasons.','',
        '| PDF | Entries | Word better | Paddle better | Tie | Uncertain |','|---|---:|---:|---:|---:|---:|']
    for f,d in per_file.items():lines.append('| '+f+' | '+str(d['biographies'])+' | '+' | '.join(str(d['paired_decisions'].get(k,0)) for k in ('Word better','Paddle better','tie','uncertain'))+' |')
    lines+=['','Sampling used seed 20261023, PDFs in alphabetical order, two accepted pages per PDF and two additional pages without replacement using the same generator. All 36 prior audit pages were excluded. All 20 initial eligible candidates contained biography starts; no poor-quality page was replaced. Complete RNG permutations, prior exclusions and candidate decisions are retained. Cross-reference/deceased stubs and incoming continuations are accounted for separately in inventory.json, outside the biography denominator. Fourteen inventory heading corrections are logged in inventory_errata.json while preserving the original freeze.','',
        'This sample has equal initial page quotas per PDF plus two extra pages, rather than sampling biographies uniformly from the corpus. Entries share page-level scan defects, and the nine PDFs are a small comparison sample. Percentages describe these reviewed biographies; they are not corpus-wide estimates. Paragraph-repaired PaddleOCR and automated field extraction remain outside this comparison.','',
        'Open index.html for scans, verbatim Word fragments, raw Paddle blocks, reference values and entry-level decisions. Full source transcriptions are linked for document-wide searching. CSV/JSON ledgers and metrics include per-page and per-PDF counts, denominators and percentages. recheck_log.json records visual/native-Word checks, continuation handling and retained uncertainty. source_verification.json and the delivery receipt record hash and archive validation.',
        'Viewer validation checks every entry, navigation/search logic and all local asset links. A browser visual preview was unavailable because local browser use is disabled in Work settings; this limitation is recorded in viewer_validation.json. The scan and native Word evidence images themselves were visually inspected.',
        'To reproduce metrics from the archive without accessing original sources, run: python provenance/recompute_manual_entry_metrics.py --audit-root .','']
    (out/'findings_report.md').write_text('\n'.join(lines),encoding='utf8')
    (out/'findings_report.txt').write_text('\n'.join(lines),encoding='utf8')
    body=[];in_table=False
    for line in lines:
        if line.startswith('|'):
            if re.fullmatch(r'[|:\- ]+',line):continue
            if not in_table:body.append('<table>');in_table=True
            body.append('<tr>'+''.join('<td>'+html.escape(c.strip())+'</td>' for c in line.strip('|').split('|'))+'</tr>')
        else:
            if in_table:body.append('</table>');in_table=False
            if line.startswith('# '):body.append('<h1>'+html.escape(line[2:])+'</h1>')
            elif line:body.append('<p>'+html.escape(line)+'</p>')
    if in_table:body.append('</table>')
    (out/'findings_report.html').write_text('<!doctype html><html lang="en"><meta charset="utf-8"><title>AMWS manual audit findings</title><style>body{font:16px/1.6 system-ui;max-width:1100px;margin:40px auto;padding:20px;color:#18323d}table{border-collapse:collapse;width:100%;font-size:14px}td{padding:9px;border:1px solid #c9d5dc}tr:first-child{background:#e8f0f3;font-weight:600}a{color:#205c91}</style><a href="index.html">Open comparison viewer</a>'+''.join(body)+'</html>',encoding='utf8')

if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('--root',type=Path,default=a.ROOT);parser.add_argument('--out',type=Path,default=OUT)
    args=parser.parse_args();build(args.root,args.out)

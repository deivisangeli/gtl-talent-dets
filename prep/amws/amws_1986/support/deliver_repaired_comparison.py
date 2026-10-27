"""Build the portable three-arm audit, validate provenance, and seal delivery."""
import argparse,csv,hashlib,html,json,re,shutil,zipfile
from collections import Counter
from pathlib import Path
import repaired_ocr_comparison as a
from repaired_comparison_extract import METHODS
import repaired_comparison_score as scoring

OUT=a.CHAT/'outputs/paragraph_repaired_comparison_20261021'
DROPBOX=Path(r'C:\Users\megaj\Globtalent Dropbox\gtl_talent_dets\output\amws\paragraph_repaired_comparison_20261021')
LABELS=dict(name='Full name',birthplace='Full birthplace',birth_year='Birth year',city='City only',institution_recall='Institution recall',institution_precision='Institution precision',institution_exact_set='Entire institution set',complete_record='Complete four-field record',biography_detection='Biography detection')
def copy(src,dst):dst.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(src,dst)
def metric_arm(metrics,measure,method):
    pair=metrics['word__paddle'] if method in ('word','paddle') else metrics['word__paddle_repaired']
    return pair['pooled'][measure]['methods']['word' if method=='word' else 'paddle']
def build(root,out):
    m=a.read(root/'manifest.json');rep=a.read(root/'repair_verification.json');freeze=a.read(root/'repair_freeze.json')
    assert rep['complete'] and rep['pages']==1360 and rep['totals']['unresolved_blocks']==0
    assert a.sha(root/'paragraph_decisions.json')==freeze['decisions_sha256']
    assert a.sha(root/'visual_review.json')==freeze['visual_review_sha256']
    assert len(a.read(root/'alignment_review_new.json')['decisions'])==912
    assert a.read(root/'alignment_review_new.json')['reviewed_all']
    metrics=a.read(root/'metrics.json');ledger=a.read(root/'scored.json');assert len(ledger)==912
    out.mkdir(parents=True,exist_ok=True)
    source_checks=[]
    for x in m['sources']:
        actual=a.sha(x['path']);assert actual==x['sha256'];source_checks.append(dict(**x,actual_sha256=actual,unchanged=True))
    for x in m['raw_inputs']:assert a.sha(x['path'])==x['sha256']
    for x in m['previous_deliveries']:assert a.sha(x['path'])==x['sha256']
    for x in m['frozen_parser']:assert a.sha(root/'frozen/repo'/x['path'])==x['sha256']
    # Each delivered prediction is bound to the exact complete-run receipt.
    runs=a.read(root/'prediction/complete.json')['documents'];assert len(runs)==27
    for d in m['documents']:
        for method in METHODS:
            stage=root/'prediction'/method/Path(d['source_file']).stem;c=a.read(stage/'complete.json')
            for name,digest in c['outputs'].items():assert a.sha(stage/name)==digest
    for p in root.glob('*.json'):copy(p,out/p.name)
    for p in root.glob('*.csv'):copy(p,out/p.name)
    for p in root.glob('tests_*.txt'):copy(p,out/'provenance'/p.name)
    for folder in ('prediction','word_inputs','review_evidence','frozen'):
        shutil.copytree(root/folder,out/folder,dirs_exist_ok=True)
    # Include all original OCR checkpoint triplets and the complete derived set.
    repair_views=[]
    for d in m['documents']:
        stem=Path(d['source_file']).stem
        copy(a.OLD/'ocr'/stem/'document.txt',out/'sources/raw'/f'{stem}.txt')
        copy(a.OLD/'ocr'/stem/'manifest.json',out/'provenance/original_ocr_runs'/stem/'manifest.json')
        copy(Path(d['baseline']['path']),out/'sources/word'/f'{stem}.docx')
        for page in range(1,d['page_count']+1):
            for ext in ('json','txt','md'):
                copy(a.OLD/'ocr'/stem/f'page_{page:04d}.{ext}',out/'sources/raw_pages'/stem/f'page_{page:04d}.{ext}')
                copy(root/'repaired'/stem/f'page_{page:04d}.{ext}',out/'repaired'/stem/f'page_{page:04d}.{ext}')
            copy(root/'repaired'/stem/f'page_{page:04d}.repair.json',out/'repaired'/stem/f'page_{page:04d}.repair.json')
            view=root/'repaired/views'/f'{stem}_page_{page:04d}.html'
            if view.exists():
                copy(view,out/'repaired/views'/view.name)
                image=root/'repaired/images'/f'{stem}_page_{page:04d}.png';copy(image,out/'repaired/images'/image.name)
                repair_views.append(dict(path=f'repaired/views/{view.name}',label=f"{d['source_file']} · PDF page {page}"))
        for ext in ('txt','md'):copy(root/'repaired'/stem/f'document.{ext}',out/'repaired'/stem/f'document.{ext}')
    for name in ('review.js','audit_report.json','review_queue.json','review_queue.csv'):copy(root/'repaired'/name,out/'repaired'/name)
    # Corpus index links only captured scan viewers; all pages retain JSON/TXT/MD.
    (out/'repaired/index.html').write_text('<!doctype html><meta charset="utf-8"><title>Paragraph repair evidence</title><a href="../index.html">Comparison</a><h1>Captured scan reviews</h1><p>All 1,360 pages have derived data. These links cover the exception and sampled scan reviews.</p><ul>'+''.join('<li><a href="'+html.escape(v['path'].removeprefix('repaired/'))+'">'+html.escape(v['label'])+'</a></li>' for v in repair_views)+'</ul>',encoding='utf8')
    copy(a.FIELD/'report.txt',out/'provenance/historical_field_extraction_report.txt')
    copy(a.FIELD/'metrics.json',out/'provenance/historical_field_extraction_metrics.json')
    copy(a.FIELD/'manifest.json',out/'provenance/historical_field_extraction_manifest.json')
    copy(a.FIELD/'scoring_contract.json',out/'provenance/historical_scoring_contract.json')
    for name in ('repaired_ocr_comparison.py','repaired_comparison_extract.py','repaired_comparison_score.py','deliver_repaired_comparison.py','validate_repaired_comparison.py','repaired_comparison_viewer.html','manual_word_evidence.py','manual_entry_audit.py','compare_ocr_fields.py','score_ocr_fields.py','test_repaired_comparison.py','test_repair_ocr_paragraphs.py','test_repair_ocr_corpus.py','test_repair_ocr_exceptions.py','test_compare_ocr_fields.py'):
        copy(a.SUPPORT/name,out/'provenance'/name)
    for name in ('repair_ocr_paragraphs.py','repair_ocr_corpus.py','repair_ocr_exceptions.py'):copy(root/'frozen/repair'/name,out/'provenance'/name)
    for name in ('review_repaired_comparison.py','record_repaired_paragraph_reviews.py','record_repaired_comparison_alignments.py','test_repaired_comparison_viewer.cjs'):
        copy(a.CHAT/'work'/name,out/'provenance'/name)
    copy(a.REPO/'prep/amws/README.md',out/'provenance/AMWS_README.md')
    copy(a.REPO/'prep/amws/amws_1986/PADDLEOCR.md',out/'provenance/PADDLEOCR.md')
    shutil.copytree(a.FIELD/'scans',out/'scans',dirs_exist_ok=True)
    refs=a.read(root/'references_adjudicated.json');scans={};page_texts={};raw_files={};repaired_files={}
    for ref in refs:
        key=ref['page_key'];file=ref['source_file'];stem=Path(file).stem;p=ref['pdf_page']
        scans[key]=ref['scan'];assert (out/ref['scan']).exists()
        raw=a.read(out/'sources/raw_pages'/stem/f'page_{p:04d}.json');derived=a.read(out/'repaired'/stem/f'page_{p:04d}.json')
        texts=lambda blocks:'\n\n'.join(f"[block {i}; {b.get('block_label')}]\n{b.get('block_content','')}" for i,b in enumerate(blocks))
        page_texts[key]=dict(raw=texts(raw['prediction']['parsing_res_list']),repaired=texts(derived['corrected_prediction']['parsing_res_list']))
        raw_files[key]=f'sources/raw_pages/{stem}/page_{p:04d}.json';repaired_files[key]=f'repaired/{stem}/page_{p:04d}.json'
    summary=[]
    for measure,label in LABELS.items():
        delta=metrics['paddle__paddle_repaired']['pooled'][measure]['paddle_minus_word_pp']
        summary.append(dict(label=label,measure=measure,delta=delta,**{method:metric_arm(metrics,measure,method) for method in METHODS}))
    payload=dict(references=refs,scored=ledger,summary=summary,scans=scans,pageTexts=page_texts,rawFiles=raw_files,repairedFiles=repaired_files,
        full=a.read(root/'full_document_summary.json'),documents=[dict(file=d['source_file'],stem=Path(d['source_file']).stem) for d in m['documents']],repairViews=repair_views)
    encoded=json.dumps(payload,ensure_ascii=False).replace('<','\\u003c')
    template=(a.SUPPORT/'repaired_comparison_viewer.html').read_text(encoding='utf8')
    (out/'index.html').write_text(template.replace('__AUDIT_DATA__',encoded),encoding='utf8')
    report(root,out,summary,repair_views)
    a.write(out/'source_verification.json',dict(verified_at=a.now(),source_files=source_checks,raw_checkpoint_files_unchanged=len(m['raw_inputs']),prior_archives_unchanged=m['previous_deliveries'],parser_hashes_unchanged=True,no_new_ocr=True,no_recognition_corrections=True))
    (out/'START_HERE.txt').write_text('Open findings_report.html for the conclusions and index.html for the three-arm viewer.\nExtract the complete archive, keeping its folder structure.\nEvery page has raw and repaired JSON/TXT/Markdown. Captured scan viewers cover the 119 exception pages plus the seeded/known-failure reviews. The accuracy viewer uses the existing 304 references on 18 pages.\nFull-document agreement is not ground-truth accuracy. References and reviews are agent-produced, not independently human-verified.\nRecompute metrics offline: python provenance/repaired_comparison_score.py score --root .\nRecompute full-document linkage: python provenance/repaired_comparison_score.py full --root .\nA browser visual preview was unavailable under the existing local-browser setting; JavaScript and links are checked programmatically.\n',encoding='utf8')
    print(json.dumps(dict(built=str(out),references=len(refs),scored=len(ledger),repair_scan_views=len(repair_views))),flush=True)
def report(root,out,summary,repair_views):
    full=a.read(root/'full_document_summary.json');metrics=a.read(root/'metrics.json');rep=a.read(root/'repair_verification.json')
    lines=['# AMWS comparison after PaddleOCR paragraph repair','','All nine original PDF/Word pairs and all 1,360 pages were processed. Recognized strings, confidence scores, line identities and original predictions were preserved. No OCR models were rerun; no names, dates, spelling or parser rules were manually corrected.','',
        'The accuracy comparison reuses 304 adjudicated scan-reference biographies on the same 18 pages. These references and the paragraph/identity judgments were reviewed by a Codex agent, without independent human verification. References were already known from the earlier audits; no new independent holdout is claimed.','',
        '| Complete-document output | Extracted records |','|---|---:|']
    for method in METHODS:lines.append(f"| {method} | {full['totals'][method]:,} |")
    lines+=['','These are parser output records, including possible fragments, stubs and false starts, not a verified scientist census. The historical counts were 23,601 Word and 21,908 raw Paddle records. The current Word reader includes OOXML-generated date/list labels, including Roman/letter labels, while retaining each literal text run once. Its results are therefore shown as a rerun; historical outputs remain unchanged. Raw Paddle predictions are required to reproduce the historical arm exactly.','',
        '| Scan-referenced measure | Word | Raw Paddle | Repaired Paddle | Repaired − raw (pp) |','|---|---:|---:|---:|---:|']
    for row in summary:
        cells=[f"{row[m]['correct']}/{row[m]['denominator']} ({row[m]['percent']:.1f}%)" for m in METHODS]
        lines.append('| '+row['label']+' | '+' | '.join(cells)+f" | {row['delta']:+.1f} |")
    lines+=['','Paragraph repair improves biography detection and institution recovery, while its gain in fully correct records is modest. Complete four-field records rise from 156/288 (54.2%) to 159/288 (55.2%), compared with Word at 175/288 (60.8%). Names remain below Word; institution recall and complete institution sets have higher point estimates after repair, with intervals that overlap no difference from Word. The results do not support a blanket replacement of Word.','',
        'The effect of paragraph repair is the repaired-versus-raw comparison. Word-versus-repaired uses the same frozen parser and faithful Word evidence reader. These rates combine recognition, segmentation and field-parser performance; they are not OCR recognition rates alone.','',
        '| Measure | Repaired − raw, pp [95% CI] | Repaired − Word, pp [95% CI] |','|---|---:|---:|']
    for measure,label in LABELS.items():
        cells=[]
        for key in ('paddle__paddle_repaired','word__paddle_repaired'):
            r=metrics[key]['pooled'][measure];lo,hi=r['paired_bootstrap_95_percentile_ci'];cells.append(f"{r['paddle_minus_word_pp']:+.1f} [{lo:+.1f}, {hi:+.1f}]")
        lines.append('| '+label+' | '+' | '.join(cells)+' |')
    lines+=['','Intervals use the original paired-page bootstrap: 10,000 draws, seed 20261022, resampling two pages within each PDF. Per-PDF intervals and per-page numerators/denominators are in metrics.json/CSV. Two pages per PDF and only nine selected PDFs limit inference; these intervals do not establish corpus-wide superiority.','',
        f"Paragraph recovery assigned all {rep['totals']['repaired_blocks']:,} collapsed reference blocks to {rep['totals']['recovered_paragraphs']:,} text paragraph regions. The first pass accepted 1,195 blocks; 122 exceptions on 119 pages received scan-bound review. Additional boundary splits, two header assignments and two line-membership adjustments are recorded explicitly; the full decision ledger is authoritative. All {rep['recognized_detections_conserved']:,} detections retained in paragraphs preserve their source identities, strings, confidence and boxes exactly once; another 48 empty exception detections are retained separately in audit records. The existing audit field retained_lines includes those empty records. Two verified blank pages remain explicit.",
        '', 'The 18 seeded random repaired pages (seed 20261024) and three known failure pages were visually reviewed before extraction. All reference-block exceptions have assignments, but this does not certify every biography boundary. Existing ordinary text blocks on D0 page 15, E900 page 34 and F800 page 100 retain garbling or local interleaving; some other paragraphs sort short birth-date or discipline detections before the heading. Left-edge clipping and digit substitutions remain. Recognition debris and repeated detections were retained rather than silently deleted.','',
        'References, field definitions and fixed aliases are unchanged from the earlier adjudicated audit. Institution scoring excludes separate honorary-degree sections, counts distinct providers once, and requires printed school/campus units. Missing biographies fail all readable present fields; orphan education fragments contribute precision false positives without stitching or recall credit. One-to-one identity decisions and uncertainties are retained.','',
        'No duplicated primary records were found in the reviewed 304-biography cohort in any arm. Across the complete documents, 22 records are flagged as duplicate candidates; they remain unverified candidates and are excluded from forced linkage. Three historical Word identity alignments retain uncertainty flags. The new repaired-arm identities were reviewed separately; no prediction was assigned to two reference people.','',
        'Full-document comparison uses conservative, unique normalized name plus birth year, then name plus birthplace with no conflicting known year. Ambiguous or unmatched records are retained; no fuzzy automatic matches or reference-assisted extraction are used. Agreement is conditional on these linkable records, whose identities may be cleaner than the rest of the corpus. It must not be interpreted as scan-verified accuracy.','',
        '| Full-document pair | Linked records |','|---|---:|']
    for pair,n in full['matched_pairs'].items():lines.append(f'| {pair} | {n:,} |')
    agreement=a.read(root/'full_document_agreement.json')['estimates']
    lines+=['','| Linked-record agreement (not accuracy) | Birthplace | Birth year | Complete institution set |','|---|---:|---:|---:|']
    for pair in full['matched_pairs']:
        cells=[]
        for field in ('birthplace','birth_year','institutions'):
            v=next(r for r in agreement if r['comparison']==pair and r['scope']=='pooled' and r['field']==field)
            cells.append(f"{v['agreement']}/{v['denominator_either_present']} ({v['percent']:.1f}%)")
        lines.append('| '+pair+' | '+' | '.join(cells)+' |')
    lines+=['','Agreement denominators include linked records with a value in either source; both-missing pairs are disclosed separately. Name agreement is 100% by construction of the exact-name linkage and is not evidence of name accuracy. Birth year/place also contribute to linkage, so these selected agreements are upward-selected. full_document_agreement.csv provides per-PDF counts and denominators. The high raw/repaired agreement among already-linkable records is consistent with paragraph repair mainly increasing recoverable boundaries rather than correcting recognized factual strings.']
    lines+=['', 'Read full_document_counts.csv for per-PDF extraction/availability counts, full_document_comparison.csv for the pairwise field values and agreement flags, full_document_unmatched.csv for unresolved linkage, and duplicate_candidates.csv for repeated identity candidates. Candidate duplicates are not confirmed duplicates.','',
        'The viewer shows all three predictions, raw passages, scan reference values, whole-page Paddle blocks and reasons for the reviewed alignments. Complete document predictions and original Word/raw/repaired transcriptions are linked. Every raw checkpoint and every repaired page is included; captured scan viewers cover the exception and sampled paragraph reviews.','',
        'Validation requires 1,360 complete derived page triplets, all 27 extraction outputs, reviewed alignments, conserved detections, reproducible metrics, unchanged source/parser hashes and prior archives, working local viewer links, and verified ZIP/member/Dropbox hashes. A browser visual preview is unavailable because local browser use is disabled in Work settings; JavaScript behavior and links are checked programmatically. Matching Dropbox local hashes does not confirm cloud synchronization.','']
    text='\n'.join(lines)
    (out/'findings_report.md').write_text(text,encoding='utf8');(out/'findings_report.txt').write_text(text,encoding='utf8')
    parts=[];table=False
    for line in lines:
        if line.startswith('|'):
            if re.fullmatch(r'[|:\- ]+',line):continue
            if not table:parts.append('<table>');table=True
            parts.append('<tr>'+''.join('<td>'+html.escape(c.strip())+'</td>' for c in line.strip('|').split('|'))+'</tr>')
        else:
            if table:parts.append('</table>');table=False
            if line.startswith('# '):parts.append('<h1>'+html.escape(line[2:])+'</h1>')
            elif line:parts.append('<p>'+html.escape(line)+'</p>')
    if table:parts.append('</table>')
    (out/'findings_report.html').write_text('<!doctype html><meta charset="utf-8"><title>AMWS repaired OCR findings</title><style>body{font:16px/1.6 system-ui;max-width:1150px;margin:30px auto;padding:20px;color:#183640}table{width:100%;border-collapse:collapse;font-size:14px}td{border:1px solid #ccd9de;padding:8px}tr:first-child{background:#e8f0f2;font-weight:bold}</style><a href="index.html">Open three-arm comparison viewer</a>'+''.join(parts),encoding='utf8')
def package(root,out):
    validation=a.read(out/'delivery_validation.json');assert validation['complete']
    assert a.read(out/'viewer_validation.json')['passed']
    archive=out.with_suffix('.zip');assert not archive.exists(),'Refusing to replace a prior archive'
    files=[p for p in sorted(out.rglob('*')) if p.is_file() and p.name!='file_manifest.json']
    manifest=[dict(path=p.relative_to(out).as_posix(),bytes=p.stat().st_size,sha256=a.sha(p)) for p in files]
    a.write(out/'file_manifest.json',dict(algorithm='SHA-256',scope='Every file except this manifest; final archive hash is in the external receipt',files=manifest));files.append(out/'file_manifest.json')
    print(json.dumps(dict(stage='packaging',files=len(files),bytes=sum(p.stat().st_size for p in files))),flush=True)
    with zipfile.ZipFile(archive,'x',compression=zipfile.ZIP_DEFLATED,compresslevel=6,allowZip64=True) as z:
        for p in files:z.write(p,out.name+'/'+p.relative_to(out).as_posix())
    with zipfile.ZipFile(archive) as z:
        assert z.testzip() is None
        assert set(z.namelist())=={out.name+'/'+p.relative_to(out).as_posix() for p in files}
        for p in files:
            h=hashlib.sha256()
            with z.open(out.name+'/'+p.relative_to(out).as_posix()) as f:
                for block in iter(lambda:f.read(4*1024*1024),b''):h.update(block)
            assert h.hexdigest()==a.sha(p)
    digest=a.sha(archive);DROPBOX.mkdir(parents=True,exist_ok=True);dest=DROPBOX/archive.name;assert not dest.exists()
    copy(archive,dest);assert a.sha(dest)==digest and dest.stat().st_size==archive.stat().st_size
    with zipfile.ZipFile(dest) as z:assert z.testzip() is None
    receipt=dict(completed_at=a.now(),status='complete',archive=str(archive),dropbox_archive=str(dest),bytes=archive.stat().st_size,sha256=digest,
        member_count=len(files),zip_crc_passed=True,member_sha256_passed=True,dropbox_sha256_matches=True,dropbox_crc_passed=True,cloud_sync_confirmed=False,validation=validation)
    path=out.parent/(out.name+'_delivery_receipt.json');a.write(path,receipt);copy(path,DROPBOX/path.name)
    for name in ('findings_report.html','findings_report.md'):copy(out/name,DROPBOX/name)
    print(json.dumps(receipt),flush=True)
def main():
    p=argparse.ArgumentParser();p.add_argument('phase',choices=['build','package']);p.add_argument('--root',type=Path,default=a.ROOT);p.add_argument('--out',type=Path,default=OUT);args=p.parse_args()
    {'build':build,'package':package}[args.phase](args.root,args.out)
if __name__=='__main__':main()

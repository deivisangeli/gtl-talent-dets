"""Offline field-audit viewer, narrative and integrity-gated archive."""
from __future__ import annotations
import argparse
from collections import Counter
import html
import json
from pathlib import Path
import shutil
import subprocess
import zipfile
import compare_ocr_fields as audit
import score_ocr_fields as score

LABELS=dict(name='Full name',birthplace='Full birthplace',birth_year='Birth year',city='City only',
    institution_recall='Institution recall',institution_precision='Institution precision',
    institution_exact_set='Entire institution set',complete_record='Complete four-field record',biography_detection='Biography detection')

def fmt(value):return 'unscored' if value is None else f'{value:.1f}'
def report(root):
    m=audit.read(root/'metrics.json');refs=audit.read(root/'references.json');ledger=audit.read(root/'scored.json')
    lines=['AMWS AUTOMATED FIELD EXTRACTION AUDIT — 2026-10-19','',
        '304 biographies beginning on 18 fresh randomly selected pages (two from each of nine PDFs).',
        'Existing Word transcription versus raw PaddleOCR, with identical frozen automated extraction rules.',
        'This measures combined OCR, segmentation and parser performance on a small paired sample. It does not measure OCR recognition alone.',
        '', 'Pooled results: correct / eligible denominator (percentage); difference = Paddle minus Word, percentage points.',
        'Intervals are paired page bootstrap percentile 95% intervals, stratified by PDF; 10,000 draws, seed 20261022.','']
    for key,label in LABELS.items():
        v=m['pooled'][key];w=v['methods']['word'];p=v['methods']['paddle'];lo,hi=v['paired_bootstrap_95_percentile_ci']
        lines.append(f"{label}: Word {w['correct']}/{w['denominator']} ({fmt(w['percent'])}%); Paddle {p['correct']}/{p['denominator']} ({fmt(p['percent'])}%); difference {v['paddle_minus_word_pp']:+.1f} pp [{lo:+.1f}, {hi:+.1f}].")
    lines+=['','Field outcomes, including exclusions:']
    for method in score.METHODS:
        lines.append(method+': '+json.dumps(m['status_counts'][method],ensure_ascii=False,sort_keys=True))
        lines.append('Detection and duplicates: '+json.dumps(m['detection'][method],sort_keys=True))
    wins=[LABELS[k] for k,v in m['pooled'].items() if v['paddle_minus_word_pp']>0]
    losses=[LABELS[k] for k,v in m['pooled'].items() if v['paddle_minus_word_pp']<0]
    lines+=['','Interpretation:',
        'Paddle has a higher observed point estimate for: '+(', '.join(wins) or 'none')+'.',
        'Word has a higher observed point estimate for: '+(', '.join(losses) or 'none')+'.',
        'Differences are descriptive and can vary by PDF and page. The intervals reflect page resampling within these nine selected PDFs, not uncertainty over the full AMWS corpus. Each stratum contains only two pages; confidence intervals can understate broader sampling uncertainty. Single-page intervals cannot be estimated from one sampled page.',
        'Paddle improves complete-record accuracy in five of nine PDFs (A0, B400, D0, D200 and E200), but large losses in the two F-range PDFs reverse the pooled comparison. Their raw Paddle text contains many clipped or corrupted surnames (e.g. SMITH becomes MITH/IITH) and interleaved fragments even where the scan is readable. This audit documents the failure; it does not establish which preprocessing/model component caused it.',
        '', 'Reference correction and sensitivity:',
        'One protocol deviation was recorded after prediction review: the scan clearly prints SMITH, SHARRON WILLIAMS, while the original source-only annotation omitted the second R. reference_adjudications.json records the evidence and correction; references.json remains unchanged. Main results use references_adjudicated.json. metrics_frozen_references.json and matching ledgers retain the original frozen-reference results. The correction raises Word name accuracy by 1/304 (0.33 pp) and complete-record accuracy by 1/288 (0.35 pp); Paddle results are unchanged. No predictions, extraction rules or aliases were changed.',
        '', 'Protocol and limitations:',
        '- Page seed 20261021, alphabetical PDF order, complete page permutations; all prior audit pages excluded. All 18 first eligible candidates contain biographies; no front matter or poor-quality replacements.',
        '- Every biography starting on these pages is included. Incoming continuations and cross-reference/deceased stubs are separately inventoried in the annotation notes. Outgoing continuations were read on A99→100, B168→169, D185→186, D200-range68→69 and F800-range40→41; their education sections were already on the sampled pages.',
        '- References were agent-transcribed from source scans before automated prediction extraction/review. They were reviewed by the same agent and are not independently human-verified. No field was classified source-unreadable in this sample; absent fields are unscored for their corresponding accuracy denominator.',
        '- Birth years are four-digit references resolved from printed dates and biography chronology; ambiguous centuries would be unscored. No ambiguous reference century remained in this sample. Impossible printed dates are not repaired: for example, a printed 1929 year is retained even if its February day is invalid.',
        '- Full birthplace requires every printed geographic component. City-only is a separate, narrower score. Region/country-only references have no city denominator. No modern spelling corrections or automatic spelling forgiveness.',
        '- Names, punctuation and whitespace use Unicode normalization and case-insensitive comparison. The pre-prediction fixed alias table expands ordinary institutional/geographic abbreviations, retains word order and requires printed campus/unit. No post-review alias additions. Washington University remains distinct from University of Washington; a medical school remains distinct from its parent university.',
        '- Education references include distinct providers in the Educ section, attendance without a degree and additional qualifications. Employers, memberships, non-educational certification boards and separate Hon Degrees sections are excluded. Repeated degrees at one provider count once. Explicitly printed co-providers count separately (e.g. SUNY College of Environmental Science and Forestry and Syracuse University).',
        '- Historical identity deduplication: Texas College of Mines and Texas Western College are one institution (official UTEP history: https://www.utep.edu/hoop/section-1/history-and-mission.html).',
        '- Source input adapters preserve corrected single-visit Word XML paragraph/text-box extraction and original raw Paddle layout blocks. Only existing body layout labels are submitted to the common parser. Complete individual documents are processed; independent source documents are not joined across PDF boundaries.',
        '- The frozen shared R segmentation, name/birthplace/year rules and generic regex corrections are applied identically. Institution predictions use multi_v03 plus postsecondary_v1 across all qualification groups, with predicted birth year only. Bare attendance missed by the degree parser remains a failure. Honorary or employer text incorrectly captured by the frozen parser remains an extra prediction.',
        '- No reference values, sampled biography boundaries, person-specific overrides, manual paragraph repairs or manual prediction stitching enter extraction. Alignment occurs afterward and is one-to-one, based on identity and surrounding source context. Fuzzy identity proposals never affect field correctness.',
        '- Institution precision includes primary predictions, attributable duplicate records and reviewed unmatched education fragments from sampled biographies. Nine unmatched fragments (seven Word, two Paddle) add 12 Word and two Paddle false-positive institution predictions, without recall credit or manual stitching. They remain one-to-zero unmatched outputs, not extra correct institutions for an aligned person. It is not precision over unsampled biographies. Missed biographies contribute all readable reference fields and institutions as failures.',
        '- Duplicate counts concern repeated primary identity records; no such duplicates were found among the reviewed cohort. Nine orphan fragments are reported separately. Three alignment decisions have explicit uncertainty flags: two destroyed Word name fragments and one conservatively missed biography. The scan references themselves remain readable.',
        '- Error causes are conservative diagnostics: missing/merged segmentation, wrong-person attribution, parser field selection where literal evidence survives, or recognition/reading-order/parser not uniquely separable. A damaged OCR token and parser sensitivity can act jointly; cause counts should not be interpreted as an independent causal decomposition.',
        '- The Word baseline is called the existing Word transcription. Its original OCR engine is undocumented. Raw PaddleOCR is scored before paragraph repair.',
        '', 'Pooled error-cause counts (scalar fields):']
    for method in score.METHODS:
        c=Counter(v for r in ledger if r['method']==method for v in r['causes'].values() if v not in {'correct','absent','source_unreadable'})
        lines.append(method+': '+json.dumps(c,sort_keys=True))
    lines+=['','Per-PDF and per-page numerators, denominators, percentages and paired differences are in metrics.csv; pooled and per-PDF bootstrap intervals are in metrics.json.',
        'fields.csv records all scalar outcomes; institutions.csv records institution matches, omissions and extras; biographies.csv records detection, duplicates, complete records and institution counts.',
        'references.json and reference_notes/ preserve scan references; alignment_review.json and alignments.json preserve identity decisions. prediction/ contains full-document source blocks, frozen predictions, raw education evidence and logs. input/ retains raw OCR checkpoints and Word XML-derived paragraphs, with input_inventory.json checksums.',
        'All original source hashes and frozen parser files are validated before packaging. The earlier OCR comparison archive and production datasets are preserved. Archive verification.json and the adjacent delivery_receipt.json record integrity checks.']
    (root/'report.txt').write_text('\n'.join(lines)+'\n',encoding='utf-8')

VIEWER=r'''<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>AMWS · Field extraction audit</title><style>
:root{font-family:system-ui,sans-serif;color:#152a36;background:#f3f5f5}*{box-sizing:border-box}body{margin:0}header{background:#173a49;color:white;padding:22px 28px}h1{font-size:25px;margin:0 0 8px}p{max-width:1100px;line-height:1.5}header a{color:#bce4e9}main{padding:20px 28px}.summary{overflow:auto;background:white;border:1px solid #d5dfe2;border-radius:8px;padding:14px}table{border-collapse:collapse;width:100%;font-size:14px}th,td{text-align:left;padding:9px;border-bottom:1px solid #dce4e5;vertical-align:top}th{font-weight:650;background:#f1f5f5}.toolbar{display:flex;gap:14px;align-items:center;margin:20px 0;flex-wrap:wrap}select,button{font:inherit;padding:9px;border:1px solid #9bacb3;border-radius:5px;background:white}.layout{display:grid;grid-template-columns:minmax(330px,44%) minmax(500px,1fr);gap:20px}.scan{position:sticky;top:12px;max-height:92vh;overflow:auto;background:white;padding:10px;border:1px solid #cbd8dc}.scan img{width:100%;cursor:zoom-in}.crop{display:flex;gap:12px;flex-wrap:wrap;font-size:13px;margin:10px 0}.bio{background:white;border:1px solid #d5dfe2;border-radius:8px;margin-bottom:18px;padding:16px}.bio h2{font-size:18px;margin:0 0 5px}.note{font-size:13px;color:#51636c;line-height:1.5}.correct{color:#075c46;background:#e5f5ed}.incorrect,.missing,.wrong_person_attribution{color:#8a2030;background:#fff0f0}.absent,.source_unreadable,.ambiguous_reference{color:#596970;background:#f0f3f4}.status{display:block;font-size:12px;font-weight:650;margin-top:5px}.value{white-space:pre-wrap;overflow-wrap:anywhere}.bio td{width:30%}.bio td:first-child{width:10%}details{margin-top:12px}pre{white-space:pre-wrap;overflow-wrap:anywhere;font:13px/1.5 ui-monospace,monospace;max-height:450px;overflow:auto}.links{line-height:2}#pageStats{font-size:13px;color:#314b58}.pill{background:#e5eef1;border-radius:5px;padding:4px 8px;font-size:13px}.limitations{font-size:14px;max-width:1150px}@media(max-width:980px){main{padding:14px}.layout{grid-template-columns:1fr}.scan{position:relative;max-height:65vh}.bio{overflow:auto}header{padding:18px}table{min-width:620px}}
</style><header><h1>AMWS · Automated field extraction accuracy</h1><div>304 biographies · 18 fresh random pages · 9 PDFs · paired comparison</div><p>Existing Word transcription and raw PaddleOCR use identical frozen extraction rules. Scan references were agent-transcribed before prediction review and have not been independently human-verified.</p><div class="links"><a href="report.txt">Findings report</a> · <a href="metrics.csv">All metrics CSV</a> · <a href="fields.csv">Field decisions</a> · <a href="institutions.csv">Institution decisions</a> · <a href="references.json">References</a> · <a href="alignments.json">Alignments</a> · <a href="verification.json">Verification</a></div></header>
<main><section class="summary"><table><thead><tr><th>Outcome</th><th>Word: correct/denominator</th><th>Paddle: correct/denominator</th><th>Paddle − Word</th><th>Paired 95% bootstrap CI</th></tr></thead><tbody id="summary"></tbody></table></section>
<p class="limitations">These are automated extraction results on a small paired sample, including recognition, segmentation and parser failures. Full birthplace requires all printed components; institutions require the correct campus/unit. Case, punctuation and whitespace are ignored; only fixed aliases are accepted. Higher scores are better. Bootstrap intervals use 10,000 paired page resamples within each PDF; only two pages per PDF limits inference. <a href="reference_adjudications.json">One name reference was corrected after prediction review</a>; <a href="metrics_frozen_references.json">original frozen-reference results</a> remain available. Institution precision includes nine reviewed <a href="unmatched_predictions_review.json">unmatched fragments</a> as false positives, without stitching or recall credit.</p>
<div class="toolbar"><label for="page">Source page</label><select id="page"></select><button id="prev">Previous page</button><button id="next">Next page</button><span id="pageStats"></span></div><div class="layout"><aside class="scan"><div id="scanTitle"></div><div class="crop" id="crops"></div><a id="scanLink" target="_blank"><img id="scanImage" alt="Original AMWS scanned page"></a><div class="note">Click the scan to open full size. The four overlapping 300 DPI crops above retain original print for closer inspection. Reference notes record continuations and excluded stubs.</div></aside><section id="bios"></section></div></main>
<script id="data" type="application/json">__DATA__</script><script>
const D=JSON.parse(document.querySelector('#data').textContent), E=s=>String(s??'').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const labels=__LABELS__, pages=[...new Set(D.references.map(r=>r.page_key))], el=document.querySelector('#page');
const f=n=>n==null?'unscored':n.toFixed(1), sign=n=>n>0?'+'+f(n):f(n);
document.querySelector('#summary').innerHTML=Object.entries(D.metrics.pooled).map(([k,v])=>`<tr><td>${E(labels[k])}</td>${['word','paddle'].map(m=>{let a=v.methods[m];return `<td>${a.correct}/${a.denominator} · <b>${f(a.percent)}%</b></td>`}).join('')}<td>${sign(v.paddle_minus_word_pp)} pp</td><td>[${v.paired_bootstrap_95_percentile_ci.map(sign).join(', ')}] pp</td></tr>`).join('');
el.innerHTML=pages.map(p=>`<option>${E(p)}</option>`).join('');
const val=x=>Array.isArray(x)?x.map(E).join('<br>'):E(x||'—');
function show(){let page=el.value,refs=D.references.filter(r=>r.page_key===page),rows=D.scored.filter(r=>r.page_key===page),scan=refs[0].scan;
 document.querySelector('#scanTitle').textContent=page;document.querySelector('#scanImage').src=scan;document.querySelector('#scanLink').href=scan;
 document.querySelector('#crops').innerHTML=['left_top','left_bottom','right_top','right_bottom'].map(c=>`<a target="_blank" href="${scan.replace('.png','_'+c+'.png')}">${c.replace('_',' ')}</a>`).join('');
 document.querySelector('#pageStats').textContent=refs.length+' biography starts · '+rows.filter(r=>r.method==='word'&&r.detected).length+' Word detected · '+rows.filter(r=>r.method==='paddle'&&r.detected).length+' Paddle detected';
 document.querySelector('#bios').innerHTML=refs.map(ref=>{let matched=['word','paddle'].map(m=>rows.find(r=>r.reference_id===ref.id&&r.method===m));return `<article class="bio"><h2>${E(ref.fields.name.value)}</h2><div class="note">${E(ref.id)} · ${E(ref.notes)}</div><table><thead><tr><th>Field</th><th>Scan reference</th><th>Word extraction</th><th>Paddle extraction</th></tr></thead><tbody>${['name','birthplace','birth_year','city','institutions'].map(field=>`<tr><th>${E(labels[field]||'Institutions')}</th><td class="value">${val(ref.fields[field].value)}<span class="status">${E(ref.fields[field].status)}</span></td>${matched.map(r=>`<td class="value ${r.scores[field]}">${val(r.predicted[field])}<span class="status">${E(r.scores[field].replaceAll('_',' '))}</span>${field==='institutions'?`<small>${r.institutions.correct}/${r.institutions.reference_count} recalled; ${r.institutions.correct}/${r.institutions.predicted_count} precision</small>`:`<small>${E(r.causes[field])}</small>`}</td>`).join('')}</tr>`).join('')}</tbody></table>${matched.map(r=>`<details><summary>${r.method==='word'?'Word':'Paddle'} identity decision and raw parser evidence</summary><p class="note">${E(r.alignment_reason)}<br>Detected: ${r.detected}; duplicates: ${r.duplicates}; uncertain alignment: ${r.uncertain_alignment}; complete record: ${r.complete_eligible?r.complete_correct:'unscored'}<br>${E(r.source_key||'No distinct predicted record')}</p><p>Missing institutions: ${val(r.institutions.missing)}<br>Extra institutions: ${val(r.institutions.extras)}</p><pre>${E(r.raw_text)}</pre><details><summary>Education parser evidence</summary><pre>${E(JSON.stringify(r.education_awards,null,2))}</pre></details></details>`).join('')}<div class="note"><a href="${E(ref.annotation_file)}">Original source-only annotation notes</a></div></article>`}).join('');}
const showPrimary=show;show=function(){showPrimary();const extra=D.unmatched.filter(x=>x.page_key===el.value);if(extra.length)document.querySelector('#bios').insertAdjacentHTML('beforeend',`<article class="bio"><h2>Unmatched output fragments</h2><p class="note">These institution predictions count as page-level false positives for precision. They receive no recall credit and are not stitched into a biography.</p>${extra.map(x=>`<details><summary>${E(x.method)} · ${E(x.source_key)}</summary><p>Predicted institutions: ${val(x.institutions)}</p><pre>${E(x.raw)}</pre></details>`).join('')}</article>`)};
el.onchange=show;document.querySelector('#prev').onclick=()=>{el.selectedIndex=Math.max(0,el.selectedIndex-1);show()};document.querySelector('#next').onclick=()=>{el.selectedIndex=Math.min(pages.length-1,el.selectedIndex+1);show()};show();
</script></html>'''

def viewer(root):
    data=dict(references=audit.read(root/'references_adjudicated.json'),scored=audit.read(root/'scored.json'),metrics=audit.read(root/'metrics.json'),unmatched=audit.read(root/'unmatched_prediction_scope_candidates.json'))
    text=VIEWER.replace('__DATA__',json.dumps(data,ensure_ascii=False).replace('<','\\u003c')).replace('__LABELS__',json.dumps(LABELS))
    text=text.replace('table{min-width:620px}','table{min-width:0;font-size:12px}th,td{padding:7px}')
    text=text.replace('href="references.json">References','href="references_adjudicated.json">References')
    (root/'index.html').write_text(text,encoding='utf-8')

def validate(root):
    manifest=audit.check_freeze(root)
    assert audit.page_draw(manifest['documents'],set(manifest['previous_audit_pages']))==manifest['page_draws']
    refs=audit.read(root/'references.json');assert len(refs)==304 and len({r['page_key'] for r in refs})==18
    for item in audit.read(root/'sample_review.json')['pages']:
        assert audit.sha(root/item['scan'])==item['scan_sha256']
        annotation=next(r['annotation_file'] for r in refs if r['page_key']==item['page_key'])
        assert audit.sha(root/annotation)==item['annotation_sha256']
    aligns=audit.read(root/'alignments.json');assert len(aligns)==608 and all(a['reviewed'] for a in aligns)
    used=[(a['method'],a['source_key']) for a in aligns if a['source_key']];assert len(used)==len(set(used))
    scores=audit.read(root/'scored.json');assert len(scores)==608
    assert score.all_metrics(scores)==audit.read(root/'metrics.json'),'Metrics do not reproduce'
    assert score.all_metrics(audit.read(root/'scored_frozen_references.json'))==audit.read(root/'metrics_frozen_references.json')
    assert score.evaluation_references(root)==audit.read(root/'references_adjudicated.json')
    contract=audit.read(root/'scoring_contract.json')
    assert audit.sha(root/'reference_adjudications.json')==contract['adjudications_sha256']
    assert audit.sha(root/'unmatched_predictions_review.json')==contract['unmatched_scope_sha256']
    assert audit.sha(root/'aliases.json')==contract['alias_table_sha256']
    af=audit.read(root/'alignment_freeze.json');assert audit.sha(root/'alignments.json')==af['alignments_sha256']
    for item in af['predictions']:assert audit.sha(root/item['path'])==item['sha256']
    assert len(audit.read(root/'prediction/complete.json')['documents'])==18
    complete=[];raw_count=0
    for doc in manifest['documents']:
        for method in score.METHODS:
            stage=root/'prediction'/method/Path(doc['source_file']).stem
            cert=audit.read(stage/'complete.json');complete.append(cert)
            assert audit.sha(stage/'predictions.json')==cert['predictions_sha256']
            assert audit.sha(stage/'parsed.csv')==cert['parsed_sha256']
            for item in cert['input_manifest']['inputs']:
                assert audit.sha(item['path'])==item['sha256']
                if method=='paddle':
                    j=audit.read(item['path']);assert j['status'] in {'ok','verified_blank'}
                    assert j['source_file']==doc['source_file'];raw_count+=1
    assert raw_count==1360
    previous=audit.CHAT/'outputs/amws_ocr_comparison_20261019.zip'
    assert audit.sha(previous)=='0344344ca530d2de71439cd62894d25ce85c8fa1c448a5c6240bfeee56afe996'
    parser_paths=['prep/amws/state_alias.R','prep/amws/amws_1986/ocr_supplement_helpers.R',
        'prep/amws/amws_1986/support/build_amws_1986_ed16_first10_regex_raw_xlsx.R',
        'prep/amws/amws_1986/01_run_regex_all_docx.R','prep/amws/amws_1986/02_build_birth_year_city.R',
        'prep/amws/amws_1986/10_apply_piloted_regex_corrections_all_countries.R',
        'prep/amws/education/education_regex.py','prep/amws/education/postsecondary_qualifications.py',
        'prep/amws/education/enrich_event_study_sources.py']
    for rel in parser_paths:assert audit.sha(audit.REPO/rel)==audit.sha(root/'frozen/repo'/rel),'Live parser changed: '+rel
    return dict(references=304,alignment_decisions=608,sampled_pages=18,source_pages_reused=1360,
        source_hashes_unchanged=True,frozen_parser_hashes_unchanged=True,metrics_reproduced=True,
        valid_raw_ocr_checkpoints=raw_count,unresolved_processing_failures=0,
        live_extraction_parsers_unchanged=True,previous_archive_unchanged=True,independently_human_verified=False)

def build(root):
    checks=validate(root);report(root);viewer(root)
    audit.write(root/'verification.json',dict(checked_at=audit.now(),checks=checks,status='content validated; archive receipt is separate'))

def finalize(root):
    checks=validate(root)
    qa=audit.read(root/'viewer_qa.json');assert qa['passed'] is True
    tests=audit.read(root/'test_results.json');assert tests['passed'] is True
    inventory=[]
    for p in sorted((root/'prediction').rglob('inputs.json')):
        data=audit.read(p)
        for item in data['inputs']:
            src=Path(item['path']);assert audit.sha(src)==item['sha256']
            if src.suffix=='.json' and src.name.startswith('page_'):
                rel=Path('input/raw_paddle')/src.parent.name/src.name
            else:rel=Path('input/word_xml')/src.name
            target=root/rel;target.parent.mkdir(parents=True,exist_ok=True)
            if not target.exists():shutil.copy2(src,target)
            assert audit.sha(target)==item['sha256']
            inventory.append(dict(path=rel.as_posix(),sha256=item['sha256'],original_path=item['path']))
    audit.write(root/'input_inventory.json',inventory)
    code=root/'audit_code';code.mkdir(exist_ok=True)
    for file in ['compare_ocr_fields.py','extract_ocr_audit.R','score_ocr_fields.py','deliver_ocr_fields.py','test_compare_ocr_fields.py']:
        shutil.copy2(Path(__file__).with_name(file),code/file)
    for name in ['record_field_alignments.py','inspect_unmatched_field_predictions.py','inspect_field_alignments.py']:
        shutil.copy2(audit.CHAT/'work'/name,code/name)
    for file in ['prep/amws/README.md','prep/amws/amws_1986/PADDLEOCR.md']:
        target=root/'documentation'/file;target.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(audit.REPO/file,target)
    audit.write(root/'verification.json',dict(checked_at=audit.now(),checks=checks,viewer_qa=qa,tests=tests,
        input_files=len(inventory),status='validated for delivery; archive and Dropbox hashes recorded in delivery_receipt.json'))
    output=audit.CHAT/'outputs';destination=output/'amws_field_extraction_audit_20261019'
    destination.mkdir(exist_ok=True)
    shutil.copytree(root,destination,dirs_exist_ok=True,ignore=shutil.ignore_patterns('__pycache__','*.tmp'))
    # Inventory every archive member by content; verify both CRC and SHA256 after writing.
    files=sorted(p for p in destination.rglob('*') if p.is_file() and p.name!='archive_members.json')
    members=[dict(path=p.relative_to(destination).as_posix(),bytes=p.stat().st_size,sha256=audit.sha(p)) for p in files]
    audit.write(destination/'archive_members.json',members)
    archive=output/'amws_field_extraction_audit_20261019.zip'
    with zipfile.ZipFile(archive,'w',compression=zipfile.ZIP_DEFLATED,compresslevel=6) as z:
        for p in sorted(destination.rglob('*')):
            if p.is_file():z.write(p,p.relative_to(destination).as_posix())
    import hashlib
    with zipfile.ZipFile(archive) as z:
        assert z.testzip() is None
        for item in members:assert hashlib.sha256(z.read(item['path'])).hexdigest()==item['sha256'],item['path']
    dropbox=Path.home()/'Globtalent Dropbox/gtl_talent_dets/output/amws/field_extraction_audit_20261019'
    dropbox.mkdir(parents=True,exist_ok=True);copy=dropbox/archive.name;shutil.copy2(archive,copy)
    assert audit.sha(copy)==audit.sha(archive)
    receipt=dict(completed_at=audit.now(),archive=str(archive),bytes=archive.stat().st_size,sha256=audit.sha(archive),
        verified_members=len(members)+1,crc_valid=True,member_sha256_valid=True,dropbox_copy=str(copy),
        dropbox_sha256=audit.sha(copy),checks=checks)
    audit.write(output/'amws_field_extraction_audit_20261019_delivery_receipt.json',receipt)
    audit.write(dropbox/'delivery_receipt.json',receipt)
    shutil.copy2(destination/'report.txt',output/'amws_field_extraction_audit_20261019_report.txt')
    print(json.dumps(receipt,indent=2))

def main():
    p=argparse.ArgumentParser();p.add_argument('phase',choices=['build','finalize']);p.add_argument('--root',type=Path,default=audit.ROOT)
    args=p.parse_args();{'build':build,'finalize':finalize}[args.phase](args.root)
if __name__=='__main__':main()

"""Reproducible raw PaddleOCR / historical Word comparison, kept outside production data."""
from __future__ import annotations

import argparse
from collections import Counter, defaultdict
import csv
from datetime import datetime, timezone
import hashlib
import html
import json
from pathlib import Path
import random
import re
import shutil
import statistics
import sys
import unicodedata
import xml.etree.ElementTree as ET
from zipfile import ZipFile, ZIP_DEFLATED

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import run_paddleocr_ed16 as ocr
import resume_paddleocr_ed16 as controller

EXPECTED = {
    "16_A_0_200.pdf": 212, "16_B_400_600.pdf": 198, "16_D_0_200.pdf": 202,
    "16_D_200_400.pdf": 200, "16_E_1100_1200.pdf": 48, "16_E_200_400.pdf": 200,
    "16_E_900_1000.pdf": 100, "16_F_500_600.pdf": 100, "16_F_800_900.pdf": 100,
}
NS = {"w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main"}


def normalize(text):
    return " ".join(unicodedata.normalize("NFC", text).split())


def alphanumeric(text):
    return ''.join(c.lower() for c in normalize(text) if c.isalnum())


def document_draw(pool):
    rng = random.Random(20261019)
    pool = sorted(pool)
    first = rng.sample(pool, 6)
    return first, rng.sample([p for p in pool if p not in first], 3)


def validate_audit_sample(manifest, inventory, references):
    """Reproduce all three sampling stages before packaging reviewed results."""
    pool = manifest['candidate_pool']
    if len(pool) != 37 or pool != sorted(set(pool)):
        raise ValueError('Frozen candidate pool must contain 37 sorted unique PDFs')
    first, added = document_draw(pool)
    docs = manifest['documents']
    if {d['source_file']:d['page_count'] for d in docs} != EXPECTED or len(docs) != 9:
        raise ValueError('Approved nine-document sample or page counts changed')
    if sorted(first+added) != sorted(EXPECTED) or manifest['total_pages'] != 1360:
        raise ValueError('Frozen document draw no longer reproduces the approved sample')
    rng = random.Random(20261020)
    keys = []
    for doc in sorted(docs,key=lambda d:d['source_file']):
        group = 'original_six' if doc['source_file'] in first else 'added_three'
        if doc['sample_group'] != group:
            raise ValueError('Original/additional document membership changed')
        order = rng.sample(list(range(1,doc['page_count']+1)),doc['page_count'])
        if doc['audit_page_order'] != order or doc['priority_pages'] != order[:2]:
            raise ValueError('Frozen audit-page draw changed')
        keys.extend(f"{doc['source_file']}:{p}" for p in order[:2])
    if set(inventory) != set(keys) or len(references) != 90:
        raise ValueError('Audit requires exactly 18 sampled pages and 90 biographies')
    identities = [(r['page_key'],r['name']) for r in references]
    if len(set(identities)) != 90:
        raise ValueError('Duplicate audit reference')
    for key in keys:
        item = inventory[key]
        pool = item['complete_biographies']
        seed = int.from_bytes(hashlib.sha256(('20261020:'+key).encode()).digest()[:8],'big')
        selected = random.Random(seed).sample(pool,5)
        actual = [r['name'] for r in references if r['page_key'] == key]
        if not item['biography_page'] or len(pool) != len(set(pool)):
            raise ValueError('Biography pool is ambiguous or not a biography page')
        if item['biography_seed'] != seed or item['selected_biographies'] != selected or actual != selected:
            raise ValueError('Frozen random biography selection changed')
    for ref in references:
        if ref['reference_status'] == 'readable':
            if not ref['reference'] or not 1 <= len(ref['reference'].split()) <= 100:
                raise ValueError('Readable reference must contain 1–100 words')
        elif ref['reference_status'] != 'source_unreadable' or ref['reference'] is not None:
            raise ValueError('Unknown or ambiguous source-reference status')


def read_word(path):
    """Read each XML text run once, including nested text-box paragraphs.

    Iterating all paragraphs and then all descendants of each paragraph repeats
    text boxes inside both their outer anchor and their inner paragraphs.
    Paragraph events instead preserve XML order without adding that duplication.
    Floating-text-box visual order remains a separate baseline limitation.
    """
    with ZipFile(path) as z:
        tree = ET.fromstring(z.read("word/document.xml"))
    paragraphs, pieces = [], []
    prefix = '{'+NS['w']+'}'
    def flush():
        if pieces:
            paragraphs.append(''.join(pieces));pieces.clear()
    def walk(node):
        if node.tag == prefix+'t':pieces.append(node.text or '');return
        if node.tag in {prefix+'br',prefix+'cr'}:pieces.append('\n');return
        if node.tag == prefix+'tab':pieces.append('\t');return
        if node.tag == prefix+'p':flush()
        for child in node:walk(child)
        if node.tag == prefix+'p':flush()
    walk(tree);flush()
    return paragraphs


def prepare(args):
    import pypdfium2 as pdfium
    args.root.mkdir(parents=True, exist_ok=True)
    manifest_path = args.root / "comparison_manifest.json"
    if manifest_path.exists():
        print("Existing frozen manifest retained:", manifest_path)
        return
    files = list(args.input_dir.iterdir())
    word = {p.stem.casefold(): p for p in files if p.is_file() and p.suffix.casefold() == ".docx"}
    pool = sorted([p for p in files if p.is_file() and p.suffix.casefold() == ".pdf"
                   and not p.stem.casefold().startswith("16_index_")
                   and p.stem.casefold() in word], key=lambda p: p.name)
    if len(pool) != 37:
        raise ValueError("The approved 37-file pool changed; do not silently redraw")
    first, added = document_draw(pool)
    selected = sorted(first + added, key=lambda p: p.name)
    if {p.name for p in selected} != set(EXPECTED):
        raise ValueError("Random draw differs from approved sample")
    audit_rng = random.Random(20261020)
    documents = []
    baseline_root = args.root / "baseline"
    baseline_root.mkdir(exist_ok=True)
    for pdf in selected:
        baseline = word[pdf.stem.casefold()]
        with pdfium.PdfDocument(str(pdf)) as doc:
            count = len(doc)
        if count != EXPECTED[pdf.name]:
            raise ValueError("Source page count changed")
        # Freeze a full random permutation so any scan-confirmed front matter can
        # be rejected without making a discretionary replacement selection.
        audit_order = audit_rng.sample(list(range(1, count + 1)), count)
        record = dict(source_file=pdf.name, source=ocr.source_identity(pdf),
                      baseline=ocr.source_identity(baseline), page_count=count,
                      pdf_sha256=controller.sha256(pdf), docx_sha256=controller.sha256(baseline),
                      sample_group="original_six" if pdf in first else "added_three",
                      audit_page_order=audit_order, priority_pages=audit_order[:2])
        documents.append(record)
        paragraphs = read_word(baseline)
        ocr.atomic_json(baseline_root / f"{pdf.stem}.json", dict(
            source_file=baseline.name, source_sha256=record["docx_sha256"],
            method="historical Word transcription; original OCR engine unestablished",
            paragraphs=[dict(index=i, text=t) for i, t in enumerate(paragraphs)]))
        ocr.atomic_text(baseline_root / f"{pdf.stem}.txt", "\n\n".join(paragraphs))
        print(pdf.name, count, "audit page candidates", record["priority_pages"], flush=True)
    manifest = dict(artifact_type="amws_ocr_method_comparison", version=1,
                    created_at=ocr.utc_now(), document_seed=20261019, audit_seed=20261020,
                    sampling="Random(20261019): sample six from sorted pool, then three from sorted remaining pool, same RNG",
                    audit_sampling="Random(20261020): full page permutation per sorted selected PDF; take first two scan-confirmed biography pages",
                    candidate_pool=[p.name for p in pool], candidate_count=len(pool),
                    documents=documents, total_pages=sum(d["page_count"] for d in documents),
                    reference_policy="Scan-first, five randomly drawn complete biographies per audit page, first 100 whitespace words or whole biography if shorter; NFC/whitespace only normalization; raw OCR primary")
    ocr.atomic_json(manifest_path, manifest)
    render(args)


def render(args):
    import pypdfium2 as pdfium
    manifest = controller.read_json(args.root / "comparison_manifest.json")
    images = args.root / "audit" / "images"
    images.mkdir(parents=True, exist_ok=True)
    for item in manifest["documents"]:
        with pdfium.PdfDocument(item["source"]["path"]) as pdf:
            for number in item["priority_pages"]:
                name = f"{Path(item['source_file']).stem}_page_{number:04d}"
                target = images / f"{name}.png"
                if target.exists():
                    continue
                page = pdf[number - 1]
                bitmap = page.render(scale=200 / 72)
                image = bitmap.to_pil().convert("RGB")
                image.save(target)
                # Overlapping quadrants for legible manual transcription. They
                # preserve source pixels; no OCR or generated text is used.
                w, h = image.size
                for c, (x1, x2) in enumerate([(0, int(w*.68)), (int(w*.38), w)], 1):
                    for half, (y1, y2) in enumerate([(0, int(h*.54)), (int(h*.46), h)], 1):
                        with image.crop((x1, y1, x2, y2)) as crop:
                            crop.save(images / f"{name}_c{c}_h{half}.png")
                image.close(); bitmap.close(); page.close()
        print("Audit scans rendered:", item["source_file"], flush=True)


def compact_index(text):
    chars, offsets = [], []
    for i, c in enumerate(text):
        if c.isascii() and c.isalnum():
            chars.append(c.lower()); offsets.append(i)
    return "".join(chars), offsets


def locate_name(name, text, index=None):
    """Propose a biography anchor; uncertain matches require recorded review."""
    import regex
    compact, offsets = index or compact_index(text)
    query = compact_index(name)[0]
    exact = compact.find(query)
    if exact >= 0:
        return dict(start=offsets[exact], name_end=offsets[exact+len(query)-1]+1,
                    name_edits=0, anchor="exact_alphanumeric_name")
    allowed = max(2, min(6, len(query)//4))
    # Full-document fuzzy BESTMATCH can be prohibitively expensive on a Word
    # transcript. Exact pieces identify small candidate neighborhoods first.
    neighborhoods = set()
    pieces = [(i, query[i:i+6]) for i in range(0, max(1,len(query)-5), 4)]
    for offset, piece in pieces:
        position = compact.find(piece)
        found = 0
        while position >= 0 and found < 150:
            neighborhoods.add((max(0,position-offset-allowed),
                               min(len(compact),position-offset+len(query)+allowed)))
            found += 1; position = compact.find(piece,position+1)
    best = None
    pattern = "("+regex.escape(query)+"){e<="+str(allowed)+"}"
    for low,high in sorted(neighborhoods):
        match = regex.search(pattern,compact[low:high],flags=regex.BESTMATCH)
        if match:
            candidate = (sum(match.fuzzy_counts), low+match.start(), low+match.end())
            if best is None or candidate < best:
                best = candidate
    if best is None:
        return dict(start=None, name_end=None, name_edits=None, anchor="unmatched")
    edits,start,end = best
    return dict(start=offsets[start], name_end=offsets[end-1]+1,
                name_edits=edits, anchor="fuzzy_alphanumeric_name")


def levenshtein(a, b):
    if len(a) < len(b):
        a, b = b, a
    previous = list(range(len(b)+1))
    for i, x in enumerate(a, 1):
        current = [i]
        for j, y in enumerate(b, 1):
            current.append(min(current[-1]+1, previous[j]+1, previous[j-1]+(x != y)))
        previous = current
    return previous[-1]


def prefix_span(reference, text, start):
    """Anchor at the name, choose a word-ending prefix; keep internal OCR errors."""
    reference = normalize(reference)
    window = text[start:start+max(2000, len(reference)*3)]
    # Map whitespace-normalized characters back to the normalized document,
    # which is already NFC with one space between words.
    previous = list(range(len(window)+1))
    for i, x in enumerate(reference, 1):
        current = [i]
        for j, y in enumerate(window, 1):
            current.append(min(current[-1]+1, previous[j]+1, previous[j-1]+(x != y)))
        previous = current
    boundaries = [m.end() for m in re.finditer(r"\S+", window)]
    end = min(boundaries, key=lambda k: (previous[k], abs(k-len(reference)))) if boundaries else 0
    return text[start:start+end], start+end


def metric(reference, hypothesis):
    reference, hypothesis = normalize(reference), normalize(hypothesis)
    characters = levenshtein(reference, hypothesis)
    words = levenshtein(reference.split(), hypothesis.split())
    letters_ref = "".join(c.lower() for c in reference if c.isalnum())
    letters_hyp = "".join(c.lower() for c in hypothesis if c.isalnum())
    return dict(character_errors=characters, reference_characters=len(reference),
                word_errors=words, reference_words=len(reference.split()),
                cer=characters/len(reference) if reference else None,
                wer=words/len(reference.split()) if reference else None,
                alphanumeric_errors=levenshtein(letters_ref,letters_hyp),
                reference_alphanumeric=len(letters_ref))


def write_csv(path, rows):
    if not rows:
        return
    with path.open("w", encoding="utf-8-sig", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0]))
        writer.writeheader(); writer.writerows(rows)


def align(args):
    root = args.root
    references_path = root / "audit" / "references_frozen.json"
    references = controller.read_json(references_path)
    inventory = controller.read_json(root / "audit" / "scan_inventory.json")
    manifest = controller.read_json(root / "comparison_manifest.json")
    decisions_path = root / "audit" / "alignment_decisions.json"
    decisions = controller.read_json(decisions_path) if decisions_path.exists() else {}
    baseline, indexes = {}, {}
    for doc in manifest["documents"]:
        name = doc["source_file"]
        text = normalize((root / "baseline" / f"{Path(name).stem}.txt").read_text(encoding="utf-8"))
        baseline[name] = text; indexes[name] = compact_index(text)
    rows, coverage, page_data = [], [], []
    for key, item in inventory.items():
        name, number = key.rsplit(":", 1)
        number = int(number)
        folder = root / "ocr" / Path(name).stem
        raw_path = folder / f"page_{number:04d}.json"
        raw = controller.read_json(raw_path) if raw_path.exists() else None
        paddle = normalize((folder/f"page_{number:04d}.txt").read_text(encoding="utf-8")) if raw else None
        p_index = compact_index(paddle) if paddle else None
        expected_names = item["complete_biographies"]
        page_anchors = []
        for biography in expected_names:
            # Field suffixes used to disambiguate scan inventory entries are
            # not printed as part of the person's name.
            search_name = biography.split(" [", 1)[0].split(", MEMBRANE FUSION")[0]
            for method, text, idx in [("word", baseline[name], indexes[name]), ("paddle", paddle, p_index)]:
                anchor = locate_name(search_name, text, idx) if text else dict(start=None, name_end=None, name_edits=None, anchor="pending_ocr")
                row = dict(page_key=key, name=biography, method=method,
                           review_status='unreviewed_anchor_proposal_not_omission_evidence', **anchor)
                if text and anchor["start"] is not None:
                    row["matched_heading"] = text[anchor["start"]:anchor["name_end"]]
                    if method == "word": page_anchors.append(anchor["start"])
                else:
                    row["matched_heading"] = ""
                coverage.append(row)
        page_refs = [r for r in references if r["page_key"] == key]
        for ref in page_refs:
            identity = key + "|" + ref["name"]
            result = dict(id=identity, page_key=key, name=ref["name"], reference=ref["reference"],
                          reference_status=ref["reference_status"], fields=ref["fields"],
                          reference_sha256=hashlib.sha256((ref["reference"] or "").encode()).hexdigest(),
                          notes=ref.get("notes", ""), methods={})
            for method, text, idx in [("word", baseline[name], indexes[name]), ("paddle", paddle, p_index)]:
                anchor = locate_name(ref["fields"]["name"], text, idx) if text else dict(start=None, name_end=None, name_edits=None, anchor="pending_ocr")
                decision = decisions.get(identity, {}).get(method)
                m = dict(**anchor, review_status="pending")
                if decision:
                    if decision["reference_sha256"] != result["reference_sha256"]:
                        raise ValueError("Alignment decision has stale reference hash")
                    m.update(review_status=decision["status"], review_notes=decision.get("notes", ""))
                    if decision.get("source_start") is not None:
                        m["start"] = decision["source_start"]
                        m["anchor"] = "recorded_biography_context_review"
                        m["name_end"] = None
                        m["name_edits"] = None
                    if decision["status"] == "omitted":
                        m["start"] = None
                if text and m["start"] is not None:
                    if ref["reference"]:
                        hypothesis, end = prefix_span(ref["reference"], text, m["start"])
                    else:
                        end = min(len(text), m["start"]+1000); hypothesis = text[m["start"]:end]
                    if decision and decision.get("source_end") is not None:
                        end = decision["source_end"]; hypothesis = text[m["start"]:end]
                    m.update(end=end, hypothesis=hypothesis,
                             context=text[max(0,m["start"]-120):min(len(text),end+160)])
                else:
                    m.update(end=None, hypothesis="", context="")
                m["source_text_sha256"] = hashlib.sha256((text or "").encode()).hexdigest()
                if decision and decision.get("source_text_sha256") != m["source_text_sha256"]:
                    raise ValueError("Alignment decision has stale source text hash")
                if ref["reference"] and text:
                    m["metrics"] = metric(ref["reference"], m["hypothesis"])
                    m["fields_verbatim"] = {field: normalize(value) in normalize(m["hypothesis"])
                                             for field, value in ref["fields"].items()
                                             if value is not None and normalize(value) in normalize(ref["reference"])}
                    m['fields_alphanumeric'] = {field: alphanumeric(value) in alphanumeric(m['hypothesis'])
                                               for field,value in ref['fields'].items()
                                               if field in m['fields_verbatim']}
                result["methods"][method] = m
            rows.append(result)
        # Fuzzy names elsewhere in a large Word file must not pull the viewer
        # context hundreds of pages away from the reviewed audit passages.
        reviewed = [r['methods']['word']['start'] for r in rows
                    if r['page_key'] == key and r['methods']['word']['start'] is not None]
        center = statistics.median(reviewed or page_anchors or [0])
        nearby = [p for p in page_anchors+reviewed if abs(p-center) <= 20000]
        context_start = max(0,min(nearby)-200) if nearby else 0
        context_end = min(len(baseline[name]), (max(nearby)+3000) if nearby else 3000)
        page_data.append(dict(page_key=key, source_file=name, page=number,
                              image=f"audit/images/{Path(name).stem}_page_{number:04d}.png",
                              scan_notes=item["notes"], expected_biographies=len(expected_names),
                              word_context=baseline[name][context_start:context_end],
                              word_context_start=context_start,
                              paddle_raw=(folder/f"page_{number:04d}.txt").read_text(encoding="utf-8") if raw else "OCR pending",
                              paddle_blocks=raw["prediction"]["parsing_res_list"] if raw else []))
        print("Aligned:",key,flush=True)
    output = dict(created_at=ocr.utc_now(), references_file_sha256=controller.sha256(references_path),
                  manifest_sha256=controller.sha256(root/"comparison_manifest.json"),
                  alignment_decisions_sha256=controller.sha256(decisions_path) if decisions_path.exists() else None,
                  rows=rows, coverage=coverage, pages=page_data,
                  alignment_policy="Exact/fuzzy name anchors propose passages; minimal character-edit prefix ending at a word boundary proposes excerpt end; all candidates require recorded review. CER and WER retain case/punctuation, normalize NFC and whitespace only. Word text is historical transcription, not ground truth.")
    ocr.atomic_json(root/"audit"/"aligned.json",output)
    flat=[]
    for row in rows:
        for method, data in row["methods"].items():
            flat.append(dict(id=row["id"],name=row["name"],method=method,reference_status=row["reference_status"],
                             anchor=data["anchor"],name_edits=data["name_edits"],review_status=data["review_status"],
                             start=data["start"],end=data["end"],**data.get("metrics",{}),
                             reference=row["reference"],hypothesis=data["hypothesis"]))
    write_csv(root/"audit"/"aligned_excerpts.csv",flat)
    write_csv(root/"audit"/"biography_coverage.csv",coverage)
    make_viewer(root,output)


def make_viewer(root, data):
    reviews_path=root/'audit'/'page_review.json'
    data['page_reviews']=controller.read_json(reviews_path) if reviews_path.exists() else {}
    payload=json.dumps(data,ensure_ascii=False).replace("<","\\u003c")
    template='''<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>AMWS OCR comparison</title>
<style>*{box-sizing:border-box}body{margin:0;background:#eef2f5;color:#163042;font:15px system-ui}header{background:#153d50;color:white;padding:20px 28px}h1{font-size:25px;margin:0 0 8px}header p{margin:0;max-width:1100px;line-height:1.5}.bar{padding:14px 24px;background:white;display:flex;gap:20px;align-items:center;flex-wrap:wrap}select,button{padding:7px;font:inherit}.grid{display:grid;grid-template-columns:1.1fr 1fr;gap:16px;padding:18px}.panel,.card{background:white;border:1px solid #c3ced6;border-radius:6px;padding:15px}.scan{max-height:83vh;overflow:auto}.scan img{width:100%;max-width:none}.texts{max-height:83vh;overflow:auto}.pair{display:grid;grid-template-columns:1fr 1fr;gap:12px}pre{white-space:pre-wrap;overflow-wrap:anywhere;font:13px/1.55 Consolas,monospace}.card{margin:12px 0}.muted{color:#536775;font-size:13px}.ref{border-left:4px solid #16846e;padding-left:12px}.warn{color:#934a12}h2{font-size:18px}h3{font-size:16px;margin:8px 0}summary{cursor:pointer}a{color:#17638a}@media(max-width:1000px){.grid,.pair{grid-template-columns:1fr}}</style>
<header><h1>AMWS: historical Word transcription and raw PaddleOCR</h1><p>Nine randomly selected edition 16 PDFs. Scan-first references include recorded source rechecks. Three unreadable source passages remain explicitly unscored. Paragraph repair and spelling correction are excluded from the primary comparison.</p></header>
<div class="bar"><label>Audit page <select id="page"></select></label><label>Scan zoom <input id="zoom" type="range" min="100" max="250" step="25" value="100"></label><a href="audit/aligned_excerpts.csv">Excerpt ledger</a><a href="audit/biography_coverage.csv">Heading-match proposals</a><a href="audit/references_frozen.json">Reference provenance</a><a href="comparison_report.txt">Report</a></div>
<div class="grid"><section class="panel"><h2 id="title"></h2><p id="notes" class="muted"></p><div class="scan"><img id="scan" alt="Original photographed AMWS page"></div></section><section class="panel texts"><details open><summary>Page structure review</summary><p id="structure" class="muted"></p></details><div id="cards"></div><details><summary>Full raw PaddleOCR page</summary><pre id="raw"></pre></details><details><summary>Historical Word context (biography-matched, not a PDF page boundary)</summary><pre id="word"></pre></details></section></div>
<script id="data" type="application/json">__DATA__</script><script>const d=JSON.parse(document.getElementById('data').textContent),el=id=>document.getElementById(id);d.pages.forEach((p,i)=>{let o=document.createElement('option');o.value=i;o.textContent=p.page_key;el('page').append(o)});function text(tag,value,parent,cls){let x=document.createElement(tag);x.textContent=value;if(cls)x.className=cls;parent.append(x);return x}function render(){const p=d.pages[+el('page').value];el('title').textContent=p.page_key;el('notes').textContent=p.scan_notes;el('structure').textContent=d.page_reviews[p.page_key]?.notes||'Review pending';el('scan').src=p.image;el('raw').textContent=p.paddle_raw;el('word').textContent=p.word_context;el('cards').replaceChildren();d.rows.filter(r=>r.page_key===p.page_key).forEach(r=>{let c=document.createElement('article');c.className='card';el('cards').append(c);text('h3',r.name,c);text('p',r.reference?'Scan reference ('+r.reference.split(/\\s+/).length+' words)':'Source too blurred for a reliable reference',c,r.reference?'muted':'warn');text('pre',r.reference||r.notes,c,'ref');let pair=document.createElement('div');pair.className='pair';c.append(pair);for(const [method,m] of Object.entries(r.methods)){let box=document.createElement('div');pair.append(box);text('h3',method==='word'?'Historical Word':'Raw PaddleOCR',box);let info=m.review_status+(m.metrics?' | CER '+(100*m.metrics.cer).toFixed(2)+'% | WER '+(100*m.metrics.wer).toFixed(2)+'%':'');text('p',info,box,'muted');text('pre',m.hypothesis||'No matched passage / OCR pending',box);if(m.fields_verbatim){text('p','Field text present verbatim: '+Object.entries(m.fields_verbatim).map(([k,v])=>k+': '+(v?'yes':'no')).join(' | '),box,'muted')}}})}el('page').onchange=render;el('zoom').oninput=()=>el('scan').style.width=el('zoom').value+'%';render();</script></html>'''
    ocr.atomic_text(root/"index.html",template.replace("__DATA__",payload))


def summarize(args):
    root=args.root
    data=controller.read_json(root/'audit'/'aligned.json')
    groups=defaultdict(lambda:dict(character_errors=0,reference_characters=0,word_errors=0,
                                  reference_words=0,alphanumeric_errors=0,reference_alphanumeric=0,
                                  scored_excerpts=0,fields=defaultdict(lambda:dict(verbatim=0,alphanumeric_match=0,total=0))))
    unresolved=[]
    field_rows=[]
    for row in data['rows']:
        if row['reference_status']!='readable': continue
        for method,m in row['methods'].items():
            if m['review_status'] not in {'confirmed','partial_omission','omitted'} or 'metrics' not in m:
                unresolved.append(dict(id=row['id'],method=method,status=m['review_status']))
                continue
            for field,verbatim in m.get('fields_verbatim',{}).items():
                field_rows.append(dict(id=row['id'],method=method,field=field,
                                       reference_field=row['fields'][field],verbatim_present=verbatim,
                                       alphanumeric_present=m.get('fields_alphanumeric',{}).get(field,False),
                                       interpretation='String preservation in the matched excerpt; not semantic field-extraction accuracy'))
            for group in ['pooled',row['page_key'].split(':')[0]]:
                g=groups[(group,method)]; g['scored_excerpts']+=1
                for field in ['character_errors','reference_characters','word_errors','reference_words',
                              'alphanumeric_errors','reference_alphanumeric']:
                    g[field]+=m['metrics'][field]
                for field,correct in m.get('fields_verbatim',{}).items():
                    g['fields'][field]['total']+=1
                    g['fields'][field]['verbatim']+=int(correct)
                    g['fields'][field]['alphanumeric_match']+=int(m.get('fields_alphanumeric',{}).get(field,False))
    results=[]
    for (group,method),g in sorted(groups.items()):
        results.append(dict(group=group,method=method,**g,
                            cer=g['character_errors']/g['reference_characters'],
                            wer=g['word_errors']/g['reference_words'],
                            alphanumeric_cer=g['alphanumeric_errors']/g['reference_alphanumeric']))
    summary=dict(created_at=ocr.utc_now(),audit_pages=len(data['pages']),sampled_biographies=len(data['rows']),
                 readable_references=sum(r['reference_status']=='readable' for r in data['rows']),
                 source_unreadable=[r['id'] for r in data['rows'] if r['reference_status']=='source_unreadable'],
                 unresolved_alignments=unresolved,results=results,
                 primary_normalization='NFC plus collapsed whitespace; case and punctuation retained',
                 secondary_normalization='Alphanumeric characters lowercased; ignores spaces and punctuation; supportive only',
                 interpretation='Pooled micro-averages of the paired sampled excerpts, not estimates for all 1360 pages. Large word-order/omission errors remain in primary metrics. References are agent-transcribed and not independently double-annotated.')
    ocr.atomic_json(root/'audit'/'comparison_summary.json',summary)
    write_csv(root/'audit'/'comparison_summary.csv', [{k:v for k,v in r.items() if k!='fields'} for r in results])
    write_csv(root/'audit'/'field_preservation.csv',field_rows)
    refs=controller.read_json(root/'audit'/'references_frozen.json')
    write_csv(root/'audit'/'reference_excerpts.csv',[dict(page_key=r['page_key'],name=r['name'],
        status=r['reference_status'],word_count=len((r['reference'] or '').split()),reference=r['reference'],
        notes=r.get('notes',''),**{f'field_{k}':v for k,v in r['fields'].items()}) for r in refs])
    print(json.dumps({k:v for k,v in summary.items() if k!='results'},indent=2))
    print(json.dumps([r for r in results if r['group']=='pooled'],indent=2))
    return summary


def finalize(args):
    """Gate delivery on complete OCR, reviewed alignments and source hashes."""
    root=args.root
    if not args.outputs or not args.dropbox_dir:
        raise ValueError('finalize requires --outputs and --dropbox-dir')
    manifest=root/'comparison_manifest.json'
    inv_args=argparse.Namespace(input_dir=args.input_dir,output_dir=root/'ocr',selection_manifest=manifest)
    with ocr.output_lock(root/'ocr','.batch.lock'), ocr.output_lock(root/'ocr'):
        docs,config=controller.inventory(inv_args)
        controller.reconcile(root/'ocr',docs,config)
        verification=controller.verify_complete(root/'ocr',docs,config)
        if not verification['complete'] or verification['valid_pages']!=1360 or len(docs)!=9:
            raise ValueError('All 1360 page checkpoints must validate before delivery')
        summary=summarize(args)
        if summary['audit_pages']!=18 or summary['sampled_biographies']!=90 or summary['unresolved_alignments']:
            raise ValueError('The audit sample or alignment review is incomplete')
        aligned=controller.read_json(root/'audit'/'aligned.json')
        validate_audit_sample(controller.read_json(manifest),
                              controller.read_json(root/'audit'/'scan_inventory.json'),
                              controller.read_json(root/'audit'/'references_frozen.json'))
        if aligned['references_file_sha256']!=controller.sha256(root/'audit'/'references_frozen.json'):
            raise ValueError('References changed after scoring')
        if aligned['manifest_sha256']!=controller.sha256(manifest):
            raise ValueError('Manifest changed after audit')
        if aligned.get('alignment_decisions_sha256')!=controller.sha256(root/'audit'/'alignment_decisions.json'):
            raise ValueError('Alignment decisions changed after scoring')
        metric_check=controller.read_json(root/'audit'/'metric_verification.json')
        if not metric_check.get('all_counts_match') or metric_check.get('aligned_sha256')!=controller.sha256(root/'audit'/'aligned.json'):
            raise ValueError('Independent edit-count verification is missing or stale')
        viewer_check=controller.read_json(root/'audit'/'viewer_validation.json')
        if viewer_check.get('index_sha256')!=controller.sha256(root/'index.html'):
            raise ValueError('Viewer validation is stale')
        baseline_hashes={doc['source_file']:hashlib.sha256(normalize(
            (root/'baseline'/f"{Path(doc['source_file']).stem}.txt").read_text(encoding='utf-8')).encode()).hexdigest()
            for doc in docs}
        for row in aligned['rows']:
            filename=row['page_key'].split(':')[0]
            if row['methods']['word']['source_text_sha256']!=baseline_hashes[filename]:
                raise ValueError('Historical Word extraction changed after alignment')
        for page in aligned['pages']:
            file,number=page['page_key'].rsplit(':',1)
            current=(root/'ocr'/Path(file).stem/f'page_{int(number):04d}.txt').read_text(encoding='utf-8')
            if current!=page['paddle_raw']:
                raise ValueError('Audit OCR changed after scoring')
        if not (root/'audit'/'page_review.json').exists():
            raise ValueError('Recorded page-level structural review is required')
        reviews=controller.read_json(root/'audit'/'page_review.json')
        if set(reviews)!=set(p['page_key'] for p in aligned['pages']):
            raise ValueError('Every audit page needs a structural review')
        for page in aligned['pages']:
            row=reviews[page['page_key']]
            digest=hashlib.sha256(page['paddle_raw'].encode()).hexdigest()
            if row.get('paddle_raw_sha256')!=digest or not row.get('reviewer') or not row.get('notes'):
                raise ValueError('Page review missing/stale')
            if row.get('original_scan_sha256')!=controller.sha256(root/page['image']):
                raise ValueError('Reviewed source image changed')
        if not (root/'comparison_report.txt').exists():
            raise ValueError('Findings report is required')
        provenance=root/'provenance';provenance.mkdir(exist_ok=True)
        for path in [Path(ocr.__file__),Path(controller.__file__),Path(__file__),
                     Path(__file__).with_name('test_paddleocr_ed16.py'),Path(__file__).with_name('test_compare_ocr_methods.py')]:
            shutil.copy2(path,provenance/path.name)
        shutil.copy2(Path(ocr.__file__).with_name('PADDLEOCR.md'),provenance/'PADDLEOCR.md')
        shutil.copy2(Path(ocr.__file__).parent.parent/'README.md',provenance/'AMWS_README.md')
        delivery_verification=dict(verified_at=ocr.utc_now(),ocr=verification,
                                   manifest_sha256=controller.sha256(manifest),
                                   report_sha256=controller.sha256(root/'comparison_report.txt'),
                                   audit_references_sha256=controller.sha256(root/'audit'/'references_frozen.json'),
                                   audit_alignment_sha256=controller.sha256(root/'audit'/'aligned.json'),
                                   all_source_pdf_and_docx_hashes_unchanged=True,
                                   cloud_synchronization='unconfirmed; local Dropbox copy verification reported separately')
        ocr.atomic_json(root/'comparison_validation.json',delivery_verification)
        # Produce a portable review folder beside the archive. Full raw OCR is
        # in the archive; the viewer itself embeds the sampled text.
        args.outputs.mkdir(parents=True,exist_ok=True)
        shutil.copy2(root/'comparison_report.txt',args.outputs/'amws_ocr_comparison_findings_20261019.txt')
        review=args.outputs/'amws_ocr_comparison_review';review.mkdir(exist_ok=True)
        (review/'audit'/'images').mkdir(parents=True,exist_ok=True)
        for filename in ['index.html','comparison_report.txt','comparison_manifest.json','comparison_validation.json']:
            shutil.copy2(root/filename,review/filename)
        for filename in ['references_frozen.json','aligned_excerpts.csv','biography_coverage.csv',
                         'comparison_summary.csv','comparison_summary.json','page_review.json','alignment_decisions.json',
                         'reference_revisions.json','scan_inventory.json','field_preservation.csv',
                         'reference_excerpts.csv','page_exclusions.json','word_extraction_review.json','viewer_validation.json',
                         'baseline_engine_metadata.json','metric_verification.json','excerpt_end_review.json']:
            shutil.copy2(root/'audit'/filename,review/'audit'/filename)
        for page in aligned['pages']:
            relative=Path(page['image']);shutil.copy2(root/relative,review/relative)
        archive=args.outputs/'amws_ocr_comparison_20261019.zip'
        files=[p for p in sorted(root.rglob('*')) if p.is_file() and p.name not in
               {'.batch.lock','.ocr.lock','delivery_manifest.json','delivery_report.json'}
               and p.suffix not in {'.tmp','.partial'} and '__pycache__' not in p.parts]
        inventory=[dict(path=p.relative_to(root).as_posix(),size_bytes=p.stat().st_size,sha256=controller.sha256(p)) for p in files]
        ocr.atomic_json(root/'delivery_manifest.json',dict(files=inventory,file_count=len(inventory),created_at=ocr.utc_now()))
        temporary=archive.with_suffix('.zip.partial')
        with ZipFile(temporary,'w',compression=ZIP_DEFLATED,compresslevel=6) as z:
            for p in files+[root/'delivery_manifest.json']:
                z.write(p,root.name+'/'+p.relative_to(root).as_posix())
        with ZipFile(temporary) as z:
            if z.testzip():raise ValueError('Archive CRC failure')
            if z.read(root.name+'/delivery_manifest.json')!=(root/'delivery_manifest.json').read_bytes():
                raise ValueError('Archive inventory differs from the sealed payload manifest')
            for item in inventory:
                with z.open(root.name+'/'+item['path']) as f:
                    digest=hashlib.file_digest(f,'sha256').hexdigest()
                if digest!=item['sha256']:raise ValueError('Archive member hash mismatch')
        temporary.replace(archive)
        digest=controller.sha256(archive)
        for doc in docs:
            for identity,key in [(doc['source'],'pdf_sha256'),(doc['baseline'],'docx_sha256')]:
                path=Path(identity['path'])
                if ocr.source_identity(path)!=identity or controller.sha256(path)!=doc[key]:
                    raise ValueError('Source changed during archive construction; delivery withheld')
        args.dropbox_dir.mkdir(parents=True,exist_ok=True)
        destination=args.dropbox_dir/archive.name
        if destination.exists():
            if controller.sha256(destination)!=digest:raise ValueError('Refusing conflicting Dropbox archive')
        else:
            partial=destination.with_suffix('.zip.partial');shutil.copy2(archive,partial)
            if controller.sha256(partial)!=digest:raise ValueError('Dropbox copy hash mismatch')
            partial.replace(destination)
        report=dict(completed_at=ocr.utc_now(),local_archive=str(archive),dropbox_archive=str(destination),
                    sha256=digest,size_bytes=archive.stat().st_size,payload_files=len(files),
                    archive_member_hashes_verified=True,local_dropbox_copy_sha256_verified=True,
                    cloud_synchronization='unconfirmed')
        ocr.atomic_json(root/'delivery_report.json',report)
        ocr.atomic_json(review/'delivery_report.json',report)
        ocr.atomic_json(args.dropbox_dir/'delivery_report.json',report)
        shutil.copy2(root/'comparison_report.txt',args.dropbox_dir/'comparison_report.txt')
        controller.progress(root/'ocr',docs,config,'comparison_delivered',delivery=report)
        print(json.dumps(report,indent=2))


def main():
    defaults = ocr.parse_args([])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("phase", choices=["prepare", "render", "align", "summarize", "finalize"])
    parser.add_argument("--input-dir", type=Path, default=defaults.input_dir)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--outputs",type=Path)
    parser.add_argument("--dropbox-dir",type=Path)
    args = parser.parse_args()
    {"prepare": prepare, "render": render, "align": align, "summarize":summarize,"finalize":finalize}[args.phase](args)


if __name__ == "__main__":
    main()

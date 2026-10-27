"""Corpus orchestration for saved-OCR paragraph repair; no model dependencies."""
from __future__ import annotations

from collections import Counter, defaultdict
from contextlib import ExitStack
import copy
import csv
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import random
import zipfile

import repair_ocr_paragraphs as repair


CORPUS_VERSION = 1
EXPECTED = dict(fully_repaired=1007, partially_repaired=78, unresolved_only=54,
                unchanged=652, verified_blank=1)


def now():
    return datetime.now(timezone.utc).isoformat()


def write_bytes(path, contents):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_bytes(contents)
    temporary.replace(path)


def write_json(path, value):
    write_bytes(path, (json.dumps(value, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))


def load(path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


def inventory(source):
    docs = load(source / "selection.json")["documents"]
    names = [d["source_file"] for d in docs]
    if len(names) != len(set(names)) or not docs:
        raise ValueError("Inventory must contain unique PDF documents")
    for doc in docs:
        if Path(doc["source_file"]).name != doc["source_file"] or doc["page_count"] < 1:
            raise ValueError("Invalid corpus inventory entry")
        path = Path(doc["source"]["path"])
        stat = path.stat()
        if stat.st_size != doc["source"]["size_bytes"] or stat.st_mtime_ns != doc["source"]["mtime_ns"]:
            raise ValueError(f"Source PDF identity changed: {path}")
    return docs


def page_paths(root, filename, page):
    stem, base = Path(filename).stem, f"page_{page:04d}"
    return dict(json=root / stem / f"{base}.json", txt=root / stem / f"{base}.txt",
                md=root / stem / f"{base}.md", checkpoint=root / stem / f"{base}.repair.json",
                viewer=root / "views" / f"{stem}_{base}.html",
                scan=root / "images" / f"{stem}_{base}.png")


def source_page(source, doc, page):
    paths = page_paths(source, doc["source_file"], page)
    content = {ext: paths[ext].read_bytes() for ext in ("json", "txt", "md")}
    hashes = {ext: hashlib.sha256(data).hexdigest() for ext, data in content.items()}
    record = json.loads(content["json"])
    if (record["source_file"] != doc["source_file"] or record["pdf_page_number"] != page
            or record["pdf_page_count"] != doc["page_count"]
            or record["status"] not in ("ok", "verified_blank")):
        raise ValueError(f"Invalid source page: {doc['source_file']}:{page}")
    if Path(record["source_path"]).resolve() != Path(doc["source"]["path"]).resolve():
        raise ValueError("Source record points to a different PDF")
    if record["status"] == "verified_blank":
        review = record["blank_verification"]
        marker = f"[VISUALLY VERIFIED BLANK PAGE | {doc['source_file']} | PDF page {page}]"
        if (review["user_approved"] is not True or review["source"] != doc["source"]
                or repair.sha256(source / review["review_image"]) != review["review_image_sha256"]
                or record["prediction"]["parsing_res_list"] or record["text_characters"] != 0
                or content["txt"].decode("utf-8").strip() != marker
                or content["md"].decode("utf-8").strip() != marker):
            raise ValueError("Blank-page approval or marker validation failed")
    elif not content["txt"].strip() or not content["md"].strip():
        raise ValueError("Empty nonblank source export")
    return record, content, hashes


def outcome(record, audits):
    if record["status"] == "verified_blank":
        return "verified_blank"
    repaired = sum(a["status"] == "repaired" for a in audits)
    unresolved = sum(a["status"] == "unresolved" for a in audits)
    return ("partially_repaired" if repaired and unresolved else "fully_repaired" if repaired
            else "unresolved_only" if unresolved else "unchanged")


def page_row(record, audits, hashes, txt, md, original):
    good = [a for a in audits if a["status"] == "repaired"]
    bad = [a for a in audits if a["status"] == "unresolved"]
    return dict(source_file=record["source_file"], pdf_page_number=record["pdf_page_number"],
                outcome=outcome(record, audits), repaired_blocks=len(good),
                recovered_paragraphs=sum(a["paragraph_count"] for a in good),
                retained_lines=sum(a["source_line_count"] for a in good),
                unresolved_blocks=len(bad), block_audits=audits, source_sha256=hashes,
                txt_unchanged=txt == original["txt"], md_unchanged=md == original["md"],
                viewer=f"views/{Path(record['source_file']).stem}_page_{record['pdf_page_number']:04d}.html")


def checkpoint_valid(root, checkpoint, source_hashes):
    try:
        saved = load(checkpoint)
        if (saved["corpus_version"] != CORPUS_VERSION or saved["repair_version"] != repair.VERSION
                or saved["source_sha256"] != source_hashes
                or set(saved["outputs"]) != {"json", "txt", "md", "viewer", "scan"}):
            return False
        for output in saved["outputs"].values():
            path = (root / output["path"]).resolve()
            if root.resolve() not in path.parents or repair.sha256(path) != output["sha256"]:
                return False
        return True
    except (OSError, ValueError, KeyError, TypeError):
        return False


def render(pdf, page_number, target):
    target.parent.mkdir(parents=True, exist_ok=True)
    page = pdf[page_number - 1]
    bitmap = page.render(scale=150 / 72)
    image = bitmap.to_pil()
    temporary = target.with_name(target.name + ".tmp")
    try:
        image.save(temporary, format="PNG")
        temporary.replace(target)
    finally:
        image.close()
        bitmap.close()
        page.close()


VIEW_JS = r'''const p=JSON.parse(document.getElementById('data').textContent).pages[0];
const el=id=>document.getElementById(id),ns='http://www.w3.org/2000/svg';
const option=document.createElement('option');option.textContent=p.source_file+' / PDF '+p.pdf_page_number;el('pages').append(option);
for(const [label,url] of Object.entries(p.navigation)){const a=document.createElement('a');a.textContent=label;a.href=url;document.querySelector('.bar').append(a)}
function render(){const format=el('format').value;el('title').textContent=p.source_file+' — PDF page '+p.pdf_page_number+' / '+p.pdf_page_count;
el('meta').textContent=p.outcome.replaceAll('_',' ')+' · '+p.repaired_blocks+' reference blocks repaired · '+p.recovered_paragraphs+' recovered paragraphs · '+p.source_lines+' saved lines retained · '+p.unresolved+' unresolved blocks.';
el('scan').src=p.image;el('scanlink').href=p.image;el('scan').style.width=el('zoom').value+'%';el('before').textContent=p.original[format];el('after').textContent=p.corrected[format];
const mark=el('marked').checked;el('after').hidden=mark;el('blocks').hidden=!mark;el('blocks').replaceChildren();const map=el('map');map.replaceChildren();map.setAttribute('viewBox','0 0 '+p.width+' '+p.height);let region=0;const counts={existing:0,recovered:0,unresolved:0};
p.blocks.forEach(b=>{const kind=b.paragraph_repair?'recovered':b.block_label==='reference'?'unresolved':'existing',body=!['header','footer','number'].includes(b.block_label)&&b.block_content.trim();
const card=document.createElement('div');card.className=body?kind:'unchanged';
if(body){region++;counts[kind]++;card.id='region-'+region;const badge=document.createElement('span');badge.className='badge';badge.textContent='Region '+region+' · column '+b.column_number+' · '+kind+(b.paragraph_repair?' · '+b.paragraph_repair.line_count+' OCR lines':'');card.append(badge);
const box=b.block_bbox,rect=document.createElementNS(ns,'rect');rect.dataset.kind=kind;rect.setAttribute('x',box[0]);rect.setAttribute('y',box[1]);rect.setAttribute('width',box[2]-box[0]);rect.setAttribute('height',box[3]-box[1]);rect.setAttribute('fill',{existing:'#e2e8ec',recovered:'#bde6dd',unresolved:'#ffe1a1'}[kind]);rect.setAttribute('stroke',{existing:'#74838d',recovered:'#177e80',unresolved:'#b07817'}[kind]);rect.setAttribute('stroke-width','5');rect.style.cursor='pointer';rect.onclick=()=>{el('marked').checked=true;el('after').hidden=true;el('blocks').hidden=false;card.scrollIntoView({block:'nearest'})};map.append(rect);
const label=document.createElementNS(ns,'text');label.setAttribute('x',box[0]+15);label.setAttribute('y',box[1]+55);label.setAttribute('font-size','48');label.textContent=region;map.append(label)}
const text=document.createElement('pre');text.textContent=b.block_content;card.append(text);el('blocks').append(card)});
if(p.outcome==='verified_blank'){const marker=document.createElement('pre');marker.textContent=p.corrected[format];el('blocks').append(marker)}
el('coverage').textContent='Existing regions: '+counts.existing+' · recovered regions: '+counts.recovered+' · unresolved blocks: '+counts.unresolved+(p.outcome==='verified_blank'?' · Approved blank page; its export marker is preserved.':'');
el('links').replaceChildren();Object.entries(p.files).forEach(([label,url])=>{const a=document.createElement('a');a.href=url;a.textContent=label;el('links').append(a)})}
el('format').onchange=render;el('marked').onchange=render;el('zoom').oninput=()=>el('scan').style.width=el('zoom').value+'%';render();'''


def view_html(record, blocks, row, original, txt, md, page_navigation):
    compact = []
    for block in blocks:
        value = {k: v for k, v in block.items() if k in ("block_label", "block_content", "block_bbox", "column_number")}
        if "paragraph_repair" in block:
            value["paragraph_repair"] = dict(line_count=len(block["paragraph_repair"]["lines"]))
        compact.append(value)
    stem, base = Path(record["source_file"]).stem, f"page_{record['pdf_page_number']:04d}"
    data = dict(source_file=record["source_file"], pdf_page_number=record["pdf_page_number"],
                pdf_page_count=record["pdf_page_count"], outcome=row["outcome"],
                image=f"../images/{stem}_{base}.png", width=record["prediction"]["width"],
                height=record["prediction"]["height"], blocks=compact,
                repaired_blocks=row["repaired_blocks"], recovered_paragraphs=row["recovered_paragraphs"],
                source_lines=row["retained_lines"], unresolved=row["unresolved_blocks"],
                original={ext: original[ext].decode("utf-8") for ext in ("txt", "md")},
                corrected=dict(txt=txt.decode("utf-8"), md=md.decode("utf-8")), navigation=page_navigation,
                files={"Corrected TXT": f"../{stem}/{base}.txt", "Corrected Markdown": f"../{stem}/{base}.md",
                       "Derived JSON": f"../{stem}/{base}.json"})
    payload = json.dumps(dict(pages=[data]), ensure_ascii=False).replace("<", "\\u003c")
    html = repair.HTML.replace("__DATA__", payload).replace('href="audit_report.json"', 'href="../audit_report.json"')
    html = html.replace('src="review.js"', 'src="../review.js"').replace("paragraph repair pilot", "paragraph repair review")
    html = html.replace("paragraph separation pilot", "paragraph separation review")
    html = html.replace("Green cards mark recovered paragraph regions.", "Gray marks existing regions, green marks recovered paragraphs, and amber marks unresolved blocks.")
    html = html.replace("Mark recovered paragraphs", "Show labeled regions")
    html = html.replace("Recovered boundaries in the OCR coordinate frame", "Regions in the OCR coordinate frame")
    html = html.replace("Each numbered rectangle corresponds to a green paragraph card", "Each numbered rectangle corresponds to a labeled text region")
    html = html.replace("</style>", ".existing{border-left:3px solid #87939a;padding:10px;margin-bottom:14px}.recovered{border-left:4px solid #177e80;background:#effafa;padding:10px;margin-bottom:14px}.unresolved{border-left:4px solid #b07817;background:#fff5df;padding:10px;margin-bottom:14px}</style>")
    return html.encode("utf-8")


def process_page(source, output, doc, page, pdf, navigation):
    record, original, hashes = source_page(source, doc, page)
    paths = page_paths(output, doc["source_file"], page)
    if checkpoint_valid(output, paths["checkpoint"], hashes):
        return load(paths["checkpoint"])["summary"], True
    blocks, audits, txt, md = repair.repair_page(record, original["txt"], original["md"])
    row = page_row(record, audits, hashes, txt, md, original)
    corrected = copy.deepcopy(record["prediction"])
    corrected["parsing_res_list"] = blocks
    derived = dict(artifact_type="amws_paragraph_repair", schema_version=repair.VERSION,
                   created_at=now(), source_file=record["source_file"], pdf_page_number=page,
                   source_json_path=str(page_paths(source, doc["source_file"], page)["json"]),
                   source_sha256=hashes, original_record=record, corrected_prediction=corrected,
                   repair_audit=audits,
                   exports=dict(txt_sha256=hashlib.sha256(txt).hexdigest(), md_sha256=hashlib.sha256(md).hexdigest()))
    write_bytes(paths["txt"], txt)
    write_bytes(paths["md"], md)
    write_json(paths["json"], derived)
    render(pdf, page, paths["scan"])
    write_bytes(paths["viewer"], view_html(record, blocks, row, original, txt, md, navigation))
    outputs = {k: dict(path=paths[k].relative_to(output).as_posix(), sha256=repair.sha256(paths[k]))
               for k in ("json", "txt", "md", "viewer", "scan")}
    write_json(paths["checkpoint"], dict(corpus_version=CORPUS_VERSION, repair_version=repair.VERSION,
               source_sha256=hashes, outputs=outputs, summary=row))
    return row, False


def document_exports(output, doc):
    for ext in ("txt", "md"):
        parts = []
        for page in range(1, doc["page_count"] + 1):
            path = page_paths(output, doc["source_file"], page)[ext]
            if not path.is_file():
                raise ValueError(f"Missing derived page: {path}")
            parts.append(f"[{doc['source_file']} | PDF page {page}]\n".encode("utf-8") + path.read_bytes())
        write_bytes(output / Path(doc["source_file"]).stem / f"document.{ext}", b"\n".join(parts))


INDEX_HTML = r'''<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>AMWS repaired corpus</title><style>
body{font:15px system-ui;color:#213746;background:#f3f6f8;margin:0}header{background:#153c50;color:white;padding:22px}h1{margin:0 0 8px;font-size:25px}main{padding:22px}p{line-height:1.6}.controls{display:flex;gap:14px;flex-wrap:wrap;margin:15px 0}input,select{font:inherit;padding:9px}a{color:#17618b}header a{color:#c5eefc}.links{display:flex;gap:18px;flex-wrap:wrap}table{background:white;border-collapse:collapse;width:100%}th,td{padding:10px;text-align:left;border-bottom:1px solid #dae3e8}th{background:#e4edf2}.table{overflow:auto}.note{max-width:1000px}.count{font-weight:600}td:last-child{white-space:nowrap}</style></head><body>
<header><h1>AMWS paragraph repair — 14 PDFs</h1><p id="summary"></p><div class="links"><a href="review_queue.csv">Unresolved review queue (CSV)</a><a href="review_queue.json">Review queue (JSON)</a><a href="audit_report.json">Full audit</a><a href="verification.json">Structural verification</a><a href="visual_review_sample.json">Visual review sample</a></div></header><main>
<p class="note">Gray regions were already present, green regions were recovered from saved layout boxes, and amber reference blocks remain unresolved. Recognition errors and omitted text remain as in the saved OCR. Every page has a separate scan-versus-text viewer.</p>
<div class="controls"><input id="search" placeholder="Search PDF, page, or reason" aria-label="Search pages"><select id="document" aria-label="Filter PDF"><option value="">All PDFs</option></select><select id="status" aria-label="Filter outcome"><option value="">All outcomes</option><option value="fully_repaired">Fully repaired</option><option value="partially_repaired">Partially repaired</option><option value="unresolved_only">Unresolved only</option><option value="unchanged">Unchanged</option><option value="verified_blank">Verified blank</option><option value="needs_review">All pages needing review</option></select></div>
<p class="count" id="count"></p><div class="table"><table><thead><tr><th>PDF</th><th>PDF page</th><th>Outcome</th><th>Recovered paragraphs</th><th>Unresolved blocks</th><th>Review</th></tr></thead><tbody id="rows"></tbody></table></div></main><script id="data" type="application/json">__DATA__</script><script src="index.js"></script></body></html>'''

INDEX_JS = r'''const data=JSON.parse(document.getElementById('data').textContent),el=id=>document.getElementById(id);
el('summary').textContent=data.pages.length+' pages accounted for · '+data.totals.repaired_blocks+' reference blocks repaired · '+data.totals.recovered_paragraphs+' recovered paragraph regions · '+data.totals.unresolved_blocks+' unresolved blocks on '+data.totals.review_pages+' pages.';
[...new Set(data.pages.map(p=>p.source_file))].forEach(name=>{const o=document.createElement('option');o.value=name;o.textContent=name;el('document').append(o)});
function render(){const query=el('search').value.toLowerCase(),doc=el('document').value,status=el('status').value;
const chosen=data.pages.filter(p=>(!doc||p.source_file===doc)&&(!status||(status==='needs_review'?p.unresolved_blocks>0:p.outcome===status))&&(!query||(p.source_file+' '+p.pdf_page_number+' '+p.reasons.join(' ')).toLowerCase().includes(query)));
el('count').textContent=chosen.length+' / '+data.pages.length+' pages shown';el('rows').replaceChildren();chosen.forEach(p=>{const tr=document.createElement('tr');for(const value of [p.source_file,p.pdf_page_number,p.outcome.replaceAll('_',' '),p.recovered_paragraphs,p.unresolved_blocks]){const td=document.createElement('td');td.textContent=value;tr.append(td)}const td=document.createElement('td'),a=document.createElement('a');a.href=p.viewer;a.textContent='Compare scan and exports';td.append(a);tr.append(td);el('rows').append(tr)})}
el('search').oninput=render;el('document').onchange=render;el('status').onchange=render;render();'''


def reports(output, docs, rows):
    counts = Counter(r["outcome"] for r in rows)
    totals = dict(pages=len(rows), source_pdfs=len(docs), outcomes=dict(counts),
                  repaired_blocks=sum(r["repaired_blocks"] for r in rows),
                  recovered_paragraphs=sum(r["recovered_paragraphs"] for r in rows),
                  retained_lines=sum(r["retained_lines"] for r in rows),
                  unresolved_blocks=sum(r["unresolved_blocks"] for r in rows),
                  review_pages=sum(r["unresolved_blocks"] > 0 for r in rows))
    queue = [dict(source_file=r["source_file"], pdf_page_number=r["pdf_page_number"],
                  viewer=r["viewer"], **audit) for r in rows for audit in r["block_audits"] if audit["status"] == "unresolved"]
    write_json(output / "audit_report.json", dict(artifact_type="amws_corpus_paragraph_audit",
               corpus_version=CORPUS_VERSION, totals=totals, pages=rows))
    write_json(output / "review_queue.json", dict(unresolved_blocks=len(queue), pages=totals["review_pages"], blocks=queue))
    fields = ["source_file", "pdf_page_number", "column_number", "original_block_order", "reason", "viewer", "original_block_bbox", "uncovered_line_indices", "ambiguous_line_indices"]
    import io
    stream = io.StringIO(newline="")
    writer = csv.DictWriter(stream, fieldnames=fields)
    writer.writeheader()
    for entry in queue:
        writer.writerow({k: json.dumps(entry[k]) if isinstance(entry.get(k), list) else entry.get(k, "") for k in fields})
    write_bytes(output / "review_queue.csv", stream.getvalue().encode("utf-8-sig"))
    minimal = [dict(source_file=r["source_file"], pdf_page_number=r["pdf_page_number"], outcome=r["outcome"],
                    recovered_paragraphs=r["recovered_paragraphs"], unresolved_blocks=r["unresolved_blocks"],
                    reasons=sorted({a["reason"] for a in r["block_audits"] if a["status"] == "unresolved"}), viewer=r["viewer"]) for r in rows]
    payload = json.dumps(dict(totals=totals, pages=minimal), ensure_ascii=False).replace("<", "\\u003c")
    write_bytes(output / "index.html", INDEX_HTML.replace("__DATA__", payload).encode("utf-8"))
    write_bytes(output / "index.js", INDEX_JS.encode("utf-8"))
    return totals


def verify_corpus(source, output, docs, require_expected=False):
    rows, seen = [], set()
    for doc in docs:
        for page in range(1, doc["page_count"] + 1):
            record, original, hashes = source_page(source, doc, page)
            paths = page_paths(output, doc["source_file"], page)
            if not checkpoint_valid(output, paths["checkpoint"], hashes):
                raise ValueError(f"Invalid or incomplete output checkpoint: {doc['source_file']}:{page}")
            derived = load(paths["json"])
            blocks, audits, txt, md = repair.repair_page(record, original["txt"], original["md"])
            expected_prediction = copy.deepcopy(record["prediction"])
            expected_prediction["parsing_res_list"] = blocks
            expected_row = page_row(record, audits, hashes, txt, md, original)
            if (derived["original_record"] != record or derived["corrected_prediction"] != expected_prediction
                    or derived["repair_audit"] != audits or paths["txt"].read_bytes() != txt
                    or paths["md"].read_bytes() != md or load(paths["checkpoint"])["summary"] != expected_row
                    or derived["source_sha256"] != hashes
                    or derived["exports"]["txt_sha256"] != repair.sha256(paths["txt"])
                    or derived["exports"]["md_sha256"] != repair.sha256(paths["md"])):
                raise ValueError(f"Structural repair verification failed: {doc['source_file']}:{page}")
            for block in blocks:
                if "paragraph_repair" not in block:
                    continue
                for line in block["paragraph_repair"]["lines"]:
                    key = (doc["source_file"], page, line["column_number"], line["source_line_index"])
                    if key in seen:
                        raise ValueError(f"Repeated repaired source line: {key}")
                    seen.add(key)
            rows.append(expected_row)
        # Verify document-level exports independently of their write operation.
        for ext in ("txt", "md"):
            expected = b"\n".join(f"[{doc['source_file']} | PDF page {p}]\n".encode("utf-8")
                       + page_paths(output, doc["source_file"], p)[ext].read_bytes()
                       for p in range(1, doc["page_count"] + 1))
            if (output / Path(doc["source_file"]).stem / f"document.{ext}").read_bytes() != expected:
                raise ValueError("Document export lost a page or source marker")
    counts = Counter(r["outcome"] for r in rows)
    if require_expected and (len(docs) != 14 or len(rows) != 1792 or dict(counts) != EXPECTED
            or sum(r["repaired_blocks"] for r in rows) != 1463
            or sum(r["recovered_paragraphs"] for r in rows) != 13496
            or sum(r["unresolved_blocks"] for r in rows) != 138):
        raise ValueError(f"Ed16 outcome counts changed: {dict(counts)}")
    result = dict(verified_at=now(), complete=True, pages=len(rows), source_pdfs=len(docs),
                  outcomes=dict(counts), repaired_lines_retained_exactly_once=len(seen),
                  source_triplets_unchanged=True, original_predictions_preserved=True,
                  unaffected_export_bytes_preserved=True, geometry_preserved=True,
                  approved_blank_pages_preserved=counts["verified_blank"],
                  unresolved_blocks_explicitly_preserved=sum(r["unresolved_blocks"] for r in rows))
    write_json(output / "verification.json", result)
    return rows, result


def review_sample(output, docs, rows):
    selected = defaultdict(set)
    by_key = {(r["source_file"], r["pdf_page_number"]): r for r in rows}
    for key in repair.PILOT:
        if key in by_key:
            selected[key].add("pilot_or_control")
    rng = random.Random(1986)
    for doc in docs:
        candidates = sorted(r["pdf_page_number"] for r in rows if r["source_file"] == doc["source_file"] and r["outcome"] == "fully_repaired")
        for page in rng.sample(candidates, min(2, len(candidates))):
            selected[(doc["source_file"], page)].add("random_fully_repaired")
    reasons = set()
    for row in rows:
        for audit in row["block_audits"]:
            if audit["status"] == "unresolved" and audit["reason"] not in reasons:
                reasons.add(audit["reason"])
                selected[(row["source_file"], row["pdf_page_number"])].add("rejection_example:" + audit["reason"])
    pages = []
    for (name, page), tags in sorted(selected.items()):
        paths = page_paths(output, name, page)
        pages.append(dict(source_file=name, pdf_page_number=page, tags=sorted(tags),
                          derived_json_sha256=repair.sha256(paths["json"]), viewer=by_key[(name, page)]["viewer"]))
    write_json(output / "visual_review_sample.json", dict(seed=1986, pages=pages,
               note="Review paragraph boundaries and entry order; this does not certify recognition accuracy."))
    return pages


def package(output):
    verification = load(output / "verification.json")
    if not verification["complete"]:
        raise ValueError("Cannot package an incomplete corpus")
    requested = load(output / "visual_review_sample.json")["pages"]
    reviewed = load(output / "visual_review.json")
    actual = {(p["source_file"], p["pdf_page_number"]): p for p in reviewed["pages"]}
    for p in requested:
        row = actual.get((p["source_file"], p["pdf_page_number"]), {})
        current_hash = repair.sha256(page_paths(output, p["source_file"], p["pdf_page_number"])["json"])
        if (row.get("derived_json_sha256") != p["derived_json_sha256"]
                or current_hash != p["derived_json_sha256"]
                or row.get("review_status") not in ("passed", "flagged_as_expected")):
            raise ValueError("Visual review is missing or stale for a sampled page")
    files = sorted(p for p in output.rglob("*") if p.is_file() and not p.name.endswith(".tmp") and p.name not in ("run_state.json", "packaging.json"))
    manifest_path = output / "output_manifest.json"
    files = [p for p in files if p != manifest_path]
    members = {p.relative_to(output).as_posix(): repair.sha256(p) for p in files}
    write_json(manifest_path, dict(files=members, file_count=len(members)))
    files.append(manifest_path)
    archive = output.with_suffix(".zip")
    temporary = archive.with_name(archive.name + ".tmp")
    with zipfile.ZipFile(temporary, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=6, allowZip64=True) as z:
        for path in files:
            z.write(path, path.relative_to(output).as_posix())
    with zipfile.ZipFile(temporary) as z:
        if z.testzip() is not None or set(z.namelist()) != set(members) | {"output_manifest.json"}:
            raise ValueError("ZIP membership or CRC verification failed")
        # CRC plus the manifest verifies the bytes archived, not just disk files.
        for name, expected in members.items():
            digest = hashlib.sha256()
            with z.open(name) as stream:
                for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                    digest.update(chunk)
            if digest.hexdigest() != expected:
                raise ValueError(f"ZIP member hash mismatch: {name}")
    temporary.replace(archive)
    result = dict(archive=str(archive), sha256=repair.sha256(archive), size_bytes=archive.stat().st_size,
                  members=len(files), crc_verified=True, member_hashes_verified=True, created_at=now(), destination="local")
    write_json(output / "packaging.json", result)
    return result


def run_corpus(args):
    source, output = args.ocr_dir.resolve(), args.output_dir.resolve()
    output.mkdir(parents=True, exist_ok=True)
    docs = inventory(source)
    selected_hash = repair.sha256(source / "selection.json")
    selection_path = output / "corpus_selection.json"
    existing = load(selection_path) if selection_path.exists() else None
    identity = dict(source_dir=str(source), selection_sha256=selected_hash, documents=docs)
    if existing and existing != identity:
        raise ValueError("The source corpus inventory differs from this derived run")
    write_json(selection_path, identity)
    if not args.verify_only and not args.package_only:
        import pypdfium2 as pdfium
        write_bytes(output / "review.js", VIEW_JS.encode("utf-8"))
        all_pages = [(d["source_file"], p) for d in docs for p in range(1, d["page_count"] + 1)]
        rows, completed, resumed = [], 0, 0
        with ExitStack() as stack:
            # Reuse one PDF handle per document, keeping image memory page-local.
            for doc in docs:
                with pdfium.PdfDocument(doc["source"]["path"]) as pdf:
                    for page in range(1, doc["page_count"] + 1):
                        index = completed
                        navigation = {"Corpus index": "../index.html"}
                        for label, position in (("Previous page", index - 1), ("Next page", index + 1)):
                            if 0 <= position < len(all_pages):
                                name, number = all_pages[position]
                                navigation[label] = f"{Path(name).stem}_page_{number:04d}.html"
                        row, skipped = process_page(source, output, doc, page, pdf, navigation)
                        completed += 1
                        resumed += skipped
                        rows.append(row)
                        if completed % 10 == 0 or completed == len(all_pages):
                            write_json(output / "run_state.json", dict(state="processing", completed=completed,
                                       total=len(all_pages), resumed=resumed, current_pdf=doc["source_file"], current_page=page))
                        if completed % 100 == 0 or page == doc["page_count"]:
                            print(f"{completed}/{len(all_pages)} pages complete ({resumed} resumed); {doc['source_file']} page {page}", flush=True)
                document_exports(output, doc)
        if repair.sha256(source / "selection.json") != selected_hash:
            raise ValueError("Source inventory changed during processing")
        reports(output, docs, rows)
    rows, verification = verify_corpus(source, output, docs, args.verify_expected_ed16)
    if not args.verify_only and not args.package_only:
        review_sample(output, docs, rows)
        # Preserve the original approved blank review pixels alongside its metadata.
        for row in rows:
            if row["outcome"] == "verified_blank":
                record = load(page_paths(output, row["source_file"], row["pdf_page_number"])["json"])["original_record"]
                relative = record["blank_verification"]["review_image"]
                write_bytes(output / "approved_blank_review" / Path(relative).name, (source / relative).read_bytes())
    if args.package_only:
        result = package(output)
        write_json(output / "run_state.json", dict(state="complete_with_review_queue", completed=len(rows),
                   total=len(rows), unresolved_blocks=verification["unresolved_blocks_explicitly_preserved"], archive=result["archive"]))
        print(json.dumps(result, indent=2), flush=True)
    else:
        if not args.verify_only:
            write_json(output / "run_state.json", dict(state="verified_ready_for_review", completed=len(rows),
                       total=len(rows), unresolved_blocks=verification["unresolved_blocks_explicitly_preserved"]))
        print(json.dumps(verification, indent=2), flush=True)
        print(f"Corpus index: {output / 'index.html'}", flush=True)
    return 0

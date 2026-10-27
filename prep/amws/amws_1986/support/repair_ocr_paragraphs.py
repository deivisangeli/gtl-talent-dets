"""Restore saved paragraph regions inside collapsed AMWS reference blocks.

This creates derived exports, never OCR resume checkpoints. No models are loaded.
"""
from __future__ import annotations

import argparse
from collections import Counter
import copy
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path


VERSION = 1
PILOT = [("16_C_400_600.pdf", 1), ("16_C_400_600.pdf", 67),
         ("16_C_400_600.pdf", 81), ("16_G_500_600.pdf", 1)]
PARAGRAPH_LABELS = {"text", "reference_content", "paragraph_title"}


def sha256(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def area(box):
    return max(0, box[2] - box[0]) * max(0, box[3] - box[1])


def intersection(a, b):
    return area([max(a[0], b[0]), max(a[1], b[1]),
                 min(a[2], b[2]), min(a[3], b[3])])


def contained(child, parent):
    return area(child) > 0 and intersection(child, parent) / area(child) >= .9


def repair_reference(parent, column, source_file, page_number):
    """Return replacement blocks and an audit, or an explicit rejection."""
    prediction = column["prediction"]
    offset = column["x_offset"]
    box = list(parent["block_bbox"])
    box[0] -= offset
    box[2] -= offset
    audit = dict(column_number=column["column_number"],
                 original_block_order=parent.get("block_order"),
                 original_block_bbox=parent["block_bbox"], status="unresolved")

    def reject(reason, **details):
        audit.update(reason=reason, **details)
        return None, audit

    candidates = sorted((b for b in prediction.get("layout_det_res", {}).get("boxes", [])
                         if b["label"] in PARAGRAPH_LABELS
                         and contained(b["coordinate"], box)
                         and area(b["coordinate"]) < area(box)),
                        key=lambda b: (b["coordinate"][1], b["coordinate"][0]))
    if len(candidates) < 2:
        return reject("fewer_than_two_paragraph_regions", candidate_count=len(candidates))
    if any(a["coordinate"][3] >= b["coordinate"][1]
           for a, b in zip(candidates, candidates[1:])):
        return reject("paragraph_regions_not_vertically_separated")
    raw = prediction.get("overall_ocr_res", {})
    texts, boxes, scores = (raw.get(k, []) for k in ("rec_texts", "rec_boxes", "rec_scores"))
    if not texts or len(texts) != len(boxes) or len(texts) != len(scores):
        return reject("missing_or_mismatched_ocr_arrays")
    members = [i for i, r in enumerate(boxes)
               if box[0] <= (r[0] + r[2]) / 2 <= box[2]
               and box[1] <= (r[1] + r[3]) / 2 <= box[3]]
    assignments = [[] for _ in candidates]
    uncovered, ambiguous = [], []
    for i in members:
        y = (boxes[i][1] + boxes[i][3]) / 2
        owners = [j for j, b in enumerate(candidates)
                  if b["coordinate"][1] <= y <= b["coordinate"][3]]
        if len(owners) == 1:
            assignments[owners[0]].append(i)
        elif owners:
            ambiguous.append(i)
        else:
            uncovered.append(i)
    if uncovered or ambiguous:
        return reject("incomplete_or_ambiguous_line_assignment",
                      uncovered_line_indices=uncovered, ambiguous_line_indices=ambiguous)
    if not members or any(not indices for indices in assignments):
        return reject("empty_reference_or_paragraph")
    # IDs, rather than strings, distinguish repeated text and prove conservation.
    flattened = [i for group in assignments for i in group]
    if Counter(flattened) != Counter(members):
        return reject("line_conservation_failed")
    blocks = []
    for number, (candidate, indices) in enumerate(zip(candidates, assignments), 1):
        indices.sort(key=lambda i: (boxes[i][1], boxes[i][0], i))
        bbox = list(candidate["coordinate"])
        bbox[0] += offset
        bbox[2] += offset
        lines = []
        for i in indices:
            line_bbox = list(boxes[i])
            line_bbox[0] += offset
            line_bbox[2] += offset
            lines.append(dict(source_line_index=i, text=texts[i], confidence=scores[i],
                              bbox=line_bbox, source_file=source_file,
                              pdf_page_number=page_number, column_number=column["column_number"]))
        blocks.append(dict(block_label="text", block_bbox=bbox,
                           block_content="\n".join(texts[i] for i in indices),
                           column_number=column["column_number"],
                           paragraph_repair=dict(original_block_order=parent.get("block_order"),
                               paragraph_number=number, original_layout_label=candidate["label"],
                               source_line_indices=indices, lines=lines)))
    audit.update(status="repaired", paragraph_count=len(blocks),
                 source_line_count=len(members), assigned_line_count=len(flattened),
                 source_line_indices=members, uncovered_line_indices=[], ambiguous_line_indices=[])
    return blocks, audit


def replace_export(contents, replacements):
    """Replace unique target spans while retaining every byte outside those spans."""
    newline = "\r\n" if b"\r\n" in contents else "\n"
    spans = []
    for old, new in replacements:
        # The OCR runner strips trailing whitespace at the end of the TXT file.
        needle = old.replace("\n", newline).encode("utf-8")
        if contents.count(needle) != 1:
            needle = old.rstrip().replace("\n", newline).encode("utf-8")
        if not needle or contents.count(needle) != 1:
            raise ValueError("Reference content is not unique in the saved export; no replacement made")
        start = contents.index(needle)
        spans.append((start, start + len(needle), new.replace("\n", newline).encode("utf-8")))
    spans.sort()
    if any(a[1] > b[0] for a, b in zip(spans, spans[1:])):
        raise ValueError("Overlapping export replacements")
    result = contents
    for start, end, replacement in reversed(spans):
        result = result[:start] + replacement + result[end:]
    return result


def repair_page(record, original_txt, original_md):
    if record.get("status") == "verified_blank":
        return copy.deepcopy(record["prediction"]["parsing_res_list"]), [], original_txt, original_md
    columns = {c["column_number"]: c for c in record["prediction"]["column_predictions"]}
    blocks, audits, replacements = [], [], []
    for parent in record["prediction"]["parsing_res_list"]:
        if parent["block_label"] != "reference":
            blocks.append(copy.deepcopy(parent))
            continue
        recovered, audit = repair_reference(parent, columns[parent["column_number"]],
                                            record["source_file"], record["pdf_page_number"])
        if recovered:
            replacement = "\n\n".join(b["block_content"] for b in recovered)
            trial = replacements + [(parent["block_content"], replacement)]
            try:
                replace_export(original_txt, trial)
                replace_export(original_md, trial)
            except ValueError as exc:
                audit.update(status="unresolved", reason="export_content_mismatch", detail=str(exc))
                recovered = None
            else:
                replacements = trial
        blocks.extend(recovered if recovered else [copy.deepcopy(parent)])
        audits.append(audit)
    if replacements:
        for order, block in enumerate(blocks, 1):
            block["block_order"] = order
    return blocks, audits, replace_export(original_txt, replacements), replace_export(original_md, replacements)


def write_json(path, value):
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def render_scan(record, target):
    import pypdfium2 as pdfium
    with pdfium.PdfDocument(record["source_path"]) as pdf:
        page = pdf[record["pdf_page_number"] - 1]
        bitmap = page.render(scale=150 / 72)
        image = bitmap.to_pil()
        try:
            image.save(target)
        finally:
            image.close()
            bitmap.close()
            page.close()


HTML = r'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>AMWS paragraph repair pilot</title><style>
*{box-sizing:border-box}body{margin:0;background:#f2f5f7;color:#20313c;font:15px system-ui,sans-serif}
header{padding:20px 24px;background:#153c50;color:white}h1{font-size:24px;margin:0 0 8px}p{line-height:1.5}
header p{margin:0;max-width:1100px}.bar{padding:14px 24px;display:flex;gap:16px;flex-wrap:wrap;align-items:center;background:white}
select,button,input{font:inherit}select,button{padding:7px}button{cursor:pointer}main{padding:18px}h2{margin:0 0 8px;font-size:20px}
.grid{display:grid;grid-template-columns:1.3fr 1fr 1fr;gap:14px}.panel{border:1px solid #c4d1d8;background:white;border-radius:8px;overflow:hidden}
.head{padding:11px;background:#e5eef3;display:flex;gap:12px;align-items:center;flex-wrap:wrap}.pane{height:720px;overflow:auto;padding:12px}
.pane img{display:block;width:100%;max-width:none}pre{white-space:pre-wrap;overflow-wrap:anywhere;font:13px/1.65 Consolas,monospace;margin:0}
.repaired{border-left:4px solid #177e80;background:#effafa;margin:0 0 14px;padding:10px}.unchanged{margin-bottom:14px}
.badge{display:block;color:#146368;font:12px system-ui;margin-bottom:7px}.meta{margin:0 0 14px}.map{display:flex;gap:20px;margin-top:16px;align-items:start}
.map svg{background:white;border:1px solid #c4d1d8;width:210px;flex-shrink:0}.map p{max-width:800px;margin:0 0 8px}.links{display:flex;gap:12px;flex-wrap:wrap;margin-top:12px}a{color:#17618b}
@media(max-width:1000px){.grid{grid-template-columns:1fr}.pane{height:600px}.map{flex-wrap:wrap}}
</style></head><body><header><h1>AMWS paragraph separation pilot</h1><p>Compare the original PDF scan with the saved export and the corrected paragraphs. Green cards mark recovered paragraph regions. Recognition text is reused; spelling, dates, and scan distortion have not been corrected.</p></header>
<div class="bar"><label>Page <select id="pages"></select></label><label>Scan zoom <input id="zoom" type="range" min="100" max="250" step="25" value="100"></label><label><input id="marked" type="checkbox" checked> Mark recovered paragraphs</label><a href="audit_report.json">Audit report</a></div>
<main><h2 id="title"></h2><p class="meta" id="meta"></p><div class="grid">
<section class="panel"><div class="head"><b>Original PDF scan</b><a id="scanlink" target="_blank">Open full scan</a></div><div class="pane" id="scanpane"><img id="scan" alt="Original AMWS PDF page"></div></section>
<section class="panel"><div class="head"><b>Original export</b><select id="format"><option value="txt">TXT</option><option value="md">Markdown source</option></select></div><div class="pane"><pre id="before"></pre></div></section>
<section class="panel"><div class="head"><b>Corrected export</b></div><div class="pane" id="afterpane"><pre id="after"></pre><div id="blocks"></div></div></section></div>
<div class="map"><svg id="map" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Recovered regions in the OCR coordinate frame"></svg><div><p><b>Recovered boundaries in the OCR coordinate frame</b></p><p>The diagram uses the straightened OCR image coordinates. It is not an overlay on the original scan. Each numbered rectangle corresponds to a green paragraph card; click it to jump to that paragraph.</p><p id="coverage"></p><div class="links" id="links"></div></div></div></main>
<script id="data" type="application/json">__DATA__</script><script src="review.js"></script></body></html>'''

JS = r'''const pages=JSON.parse(document.getElementById('data').textContent).pages;
const el=id=>document.getElementById(id),ns='http://www.w3.org/2000/svg';
pages.forEach((p,i)=>{const o=document.createElement('option');o.value=i;o.textContent=p.source_file+' / PDF '+p.pdf_page_number;el('pages').append(o)});
function render(){const p=pages[Number(el('pages').value)||0],format=el('format').value;
el('title').textContent=p.source_file+' — PDF page '+p.pdf_page_number+' / '+p.pdf_page_count;
el('meta').textContent=p.repaired_blocks+' reference block(s) repaired; '+p.recovered_paragraphs+' paragraph regions; '+p.source_lines+' lines accounted for; '+p.unresolved+' unresolved cases.'+(p.repaired_blocks===0?' Control: TXT and Markdown are byte-for-byte unchanged.':'');
el('scan').src=p.image;el('scanlink').href=p.image;el('scan').style.width=el('zoom').value+'%';el('before').textContent=p.original[format];el('after').textContent=p.corrected[format];
const mark=el('marked').checked;el('after').hidden=mark;el('blocks').hidden=!mark;el('blocks').replaceChildren();let number=0;
const map=el('map');map.replaceChildren();map.setAttribute('viewBox','0 0 '+p.width+' '+p.height);
p.blocks.forEach(b=>{const card=document.createElement('div'),repair=b.paragraph_repair;card.className=repair?'repaired':'unchanged';
if(repair){number++;card.id='paragraph-'+number;const badge=document.createElement('span');badge.className='badge';badge.textContent='Region '+number+' · column '+b.column_number+' · '+repair.lines.length+' OCR lines';card.append(badge);
const rect=document.createElementNS(ns,'rect'),box=b.block_bbox;rect.setAttribute('x',box[0]);rect.setAttribute('y',box[1]);rect.setAttribute('width',box[2]-box[0]);rect.setAttribute('height',box[3]-box[1]);rect.setAttribute('fill','#bde6dd');rect.setAttribute('stroke','#177e80');rect.setAttribute('stroke-width','5');rect.style.cursor='pointer';rect.addEventListener('click',()=>card.scrollIntoView({block:'nearest',behavior:'smooth'}));map.append(rect);
const label=document.createElementNS(ns,'text');label.setAttribute('x',box[0]+15);label.setAttribute('y',box[1]+55);label.setAttribute('font-size','48');label.textContent=number;map.append(label)}
const text=document.createElement('pre');text.textContent=b.block_content;card.append(text);el('blocks').append(card)});
el('coverage').textContent=p.recovered_paragraphs?p.source_lines+' source OCR lines retained exactly once in '+p.recovered_paragraphs+' recovered regions.':'No enclosing reference block required repair on this control page.';
el('links').replaceChildren();Object.entries(p.files).forEach(([label,href])=>{const a=document.createElement('a');a.href=href;a.textContent=label;el('links').append(a)})}
el('pages').onchange=()=>{render();el('scanpane').scrollTop=0;el('afterpane').scrollTop=0};el('format').onchange=render;el('marked').onchange=render;el('zoom').oninput=()=>el('scan').style.width=el('zoom').value+'%';render();'''


def selection(value):
    try:
        filename, page = value.rsplit(":", 1)
        page = int(page)
        if Path(filename).name != filename or not filename.lower().endswith(".pdf") or page < 1:
            raise ValueError()
        return filename, page
    except ValueError as exc:
        raise argparse.ArgumentTypeError("Use a PDF filename and one-based page, e.g. 16_C_400_600.pdf:67") from exc


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ocr-dir", type=Path, default=Path.home() / "ocr_runs" / "amws_ed16")
    parser.add_argument("--output-dir", type=Path)
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--page", type=selection, action="append", help="Repeat PDF:PAGE; defaults to the three-page pilot plus control")
    group.add_argument("--all-pages", action="store_true", help="Repair the complete inventory locally, with resume and verification")
    parser.add_argument("--verify-only", action="store_true", help="Verify existing corpus results without processing or packaging")
    parser.add_argument("--package-only", action="store_true", help="Verify and package existing corpus results")
    parser.add_argument("--verify-expected-ed16", action="store_true", help="Require the reviewed Ed16 outcome counts")
    args = parser.parse_args()
    if args.verify_only and args.package_only:
        parser.error("Choose either --verify-only or --package-only")
    if (args.verify_only or args.package_only or args.verify_expected_ed16) and not args.all_pages:
        parser.error("Corpus verification options require --all-pages")
    if args.output_dir is None:
        args.output_dir = Path.home() / "ocr_runs" / ("amws_ed16_paragraph_repaired" if args.all_pages else "amws_ed16_paragraph_pilot")
    source, output = args.ocr_dir.resolve(), args.output_dir.resolve()
    if source == output or source in output.parents or output in source.parents:
        parser.error("Derived output must be separate from the source OCR tree")
    if args.all_pages:
        from repair_ocr_corpus import run_corpus
        return run_corpus(args)
    chosen = list(dict.fromkeys(args.page or PILOT))
    output.mkdir(parents=True, exist_ok=True)
    (output / "images").mkdir(exist_ok=True)
    summary, viewer, checksums = [], [], {}
    for name, page in chosen:
        stem = Path(name).stem
        source_folder, target_folder = source / stem, output / stem
        target_folder.mkdir(exist_ok=True)
        base = f"page_{page:04d}"
        paths = {ext: source_folder / f"{base}.{ext}" for ext in ("json", "txt", "md")}
        hashes = {ext: sha256(path) for ext, path in paths.items()}
        record = json.loads(paths["json"].read_text(encoding="utf-8"))
        if record["source_file"] != name or record["pdf_page_number"] != page or record["status"] != "ok":
            raise ValueError(f"Unexpected source record: {name} page {page}")
        txt, md = paths["txt"].read_bytes(), paths["md"].read_bytes()
        blocks, audits, repaired_txt, repaired_md = repair_page(record, txt, md)
        repaired = [a for a in audits if a["status"] == "repaired"]
        unresolved = [a for a in audits if a["status"] == "unresolved"]
        corrected = copy.deepcopy(record["prediction"])
        corrected["parsing_res_list"] = blocks
        derived = dict(artifact_type="amws_paragraph_repair", schema_version=VERSION,
                       created_at=datetime.now(timezone.utc).isoformat(),
                       source_file=name, pdf_page_number=page,
                       source_json_path=str(paths["json"]), source_sha256=hashes,
                       original_record=record, corrected_prediction=corrected,
                       repair_audit=audits,
                       exports=dict(txt_sha256=hashlib.sha256(repaired_txt).hexdigest(),
                                    md_sha256=hashlib.sha256(repaired_md).hexdigest()))
        write_json(target_folder / f"{base}.json", derived)
        (target_folder / f"{base}.txt").write_bytes(repaired_txt)
        (target_folder / f"{base}.md").write_bytes(repaired_md)
        image_name = f"{stem}_{base}.png"
        render_scan(record, output / "images" / image_name)
        row = dict(source_file=name, pdf_page_number=page,
                   repaired_blocks=len(repaired), recovered_paragraphs=sum(a["paragraph_count"] for a in repaired),
                   source_lines=sum(a["source_line_count"] for a in repaired),
                   unresolved_cases=unresolved, block_audits=audits,
                   txt_unchanged=txt == repaired_txt, md_unchanged=md == repaired_md,
                   source_sha256=hashes)
        summary.append(row)
        viewer.append(dict(source_file=name, pdf_page_number=page, pdf_page_count=record["pdf_page_count"],
                           image=f"images/{image_name}", width=corrected["width"], height=corrected["height"],
                           blocks=blocks, repaired_blocks=row["repaired_blocks"],
                           recovered_paragraphs=row["recovered_paragraphs"], source_lines=row["source_lines"],
                           unresolved=len(unresolved), original=dict(txt=txt.decode("utf-8"), md=md.decode("utf-8")),
                           corrected=dict(txt=repaired_txt.decode("utf-8"), md=repaired_md.decode("utf-8")),
                           files={"Corrected TXT": f"{stem}/{base}.txt", "Corrected Markdown": f"{stem}/{base}.md",
                                  "Derived JSON": f"{stem}/{base}.json"}))
        checksums.update({str(path): hashes[ext] for ext, path in paths.items()})
        print(f"{name} page {page}: {row['recovered_paragraphs']} recovered regions, {row['source_lines']} lines, {len(unresolved)} unresolved", flush=True)
    unchanged = all(sha256(path) == digest for path, digest in checksums.items())
    report = dict(artifact_type="amws_paragraph_repair_audit", schema_version=VERSION,
                  pages=summary, source_files_unchanged=unchanged,
                  unresolved_cases=sum(len(r["unresolved_cases"]) for r in summary))
    write_json(output / "audit_report.json", report)
    write_json(output / "review_data.json", dict(pages=viewer))
    payload = json.dumps(dict(pages=viewer), ensure_ascii=False).replace("<", "\\u003c")
    (output / "index.html").write_text(HTML.replace("__DATA__", payload), encoding="utf-8")
    (output / "review.js").write_text(JS, encoding="utf-8")
    if not unchanged:
        raise RuntimeError("A source OCR file changed during the repair run")
    print(f"Comparison viewer: {output / 'index.html'}", flush=True)
    return 1 if report["unresolved_cases"] else 0


if __name__ == "__main__":
    raise SystemExit(main())

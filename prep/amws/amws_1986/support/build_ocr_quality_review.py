"""Build a local, scan-versus-text review sample without modifying OCR output."""
from __future__ import annotations

import argparse
from collections import defaultdict
from datetime import datetime, timezone
import json
from pathlib import Path
import random
import statistics


HTML = r'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>AMWS OCR quality review</title><style>
*{box-sizing:border-box}body{margin:0;background:#f4f6f8;color:#1f2937;font:15px system-ui,sans-serif}
header{background:#102f45;color:white;padding:18px 24px}h1{font-size:23px;margin:0 0 6px}header p{margin:0;max-width:1100px;line-height:1.5}
button,select,input,textarea{font:inherit}button{cursor:pointer;background:#fff;border:1px solid #b8c5cf;border-radius:6px;padding:7px 11px}button:hover{background:#e7eff4}select{padding:7px;max-width:100%}
.toolbar{display:flex;flex-wrap:wrap;align-items:center;gap:10px;padding:12px 20px;background:white;border-bottom:1px solid #d6dde3}.toolbar strong{margin-right:auto}
.layout{display:grid;grid-template-columns:230px 1fr;min-height:700px}aside{padding:14px;background:#e8eef2;border-right:1px solid #ccd5dc}.nav{width:100%;text-align:left;margin-top:7px;font-size:12px}.nav.active{border:2px solid #116c96;background:white}.nav.done{border-left:5px solid #258567}
main{padding:16px;min-width:0}.intro{margin:0 0 12px;line-height:1.5}.meta{background:#fff;border:1px solid #ccd5dc;padding:12px;border-radius:8px;margin-bottom:12px}.meta h2{font-size:19px;margin:0 0 5px}.meta p{margin:5px 0;color:#465a68}.tag{display:inline-block;background:#e9f3f8;border-radius:4px;padding:2px 6px;margin-right:5px;font-size:12px}
.compare{display:grid;grid-template-columns:1fr 1fr;gap:14px}.panel{background:white;border:1px solid #ccd5dc;border-radius:8px;overflow:hidden}.panelhead{padding:10px 12px;background:#eef3f7;display:flex;align-items:center;gap:10px;flex-wrap:wrap}.panelhead strong{margin-right:auto}.pane{height:650px;overflow:auto;padding:10px}.scan{display:block;max-width:none;width:100%;height:auto}.ocr{white-space:pre-wrap;overflow-wrap:anywhere;font:14px/1.7 ui-monospace,Consolas,monospace;margin:0}.line{display:grid;grid-template-columns:56px 1fr;gap:8px;padding:5px 0;border-bottom:1px solid #edf0f2;font-size:13px}.low{background:#fff1dc}.score{color:#6a4a15;font-variant-numeric:tabular-nums}
.review{margin-top:14px;background:white;border:1px solid #ccd5dc;border-radius:8px;padding:15px}.review h3{margin:0 0 10px}.reviewgrid{display:grid;grid-template-columns:repeat(3,minmax(150px,1fr));gap:12px}.review label{display:block;margin:7px 0}.numbers{display:flex;gap:8px}.numbers label{flex:1}.numbers input{width:100%;padding:7px;border:1px solid #b8c5cf;border-radius:5px}.review textarea{width:100%;min-height:90px;padding:10px;border:1px solid #b8c5cf;border-radius:5px}.checklist{display:flex;gap:18px;flex-wrap:wrap}.error{color:#a42828}.summary{margin:12px 0;color:#374b5a;line-height:1.6}.help{font-size:13px;color:#465a68;line-height:1.5}.blank{padding:12px;background:#fff5d9;border-radius:6px}
@media(max-width:1000px){.layout{grid-template-columns:1fr}aside{border-right:0}.nav{width:auto;max-width:230px;margin-right:5px}.compare{grid-template-columns:1fr}.pane{height:550px}.reviewgrid{grid-template-columns:1fr}}
</style></head><body>
<header><h1>AMWS OCR quality review</h1><p>Compare the photographed scan with the ordered OCR. Check the entire page for missing lines and column order, then inspect five complete biographies spread across the top, middle and bottom. Record errors against the scan; no Word file is needed.</p></header>
<div class="toolbar"><strong id="progress"></strong><button id="previous">Previous</button><button id="next">Next</button><button id="jsonexport">Export review JSON</button><button id="csvexport">Export review CSV</button><label>Import saved review <input id="import" type="file" accept=".json" style="max-width:230px"></label></div>
<div class="layout"><aside><label>Show pages <select id="filter"><option value="all">All sampled pages</option><option value="random">Random sample</option><option value="targeted">Targeted review</option><option value="unreviewed">Unreviewed pages</option></select></label><p class="help">Random pages cover all 14 PDFs. Targeted pages cover low-confidence text, column adjustments and the prior pilot. Report their error rates separately.</p><div id="navigation"></div></aside>
<main><p class="intro">Check <b>names, birth dates and birthplaces</b> exactly as printed, including digits and abbreviations. A high confidence score can coexist with omitted text. Source page damage or clipping should be noted separately.</p>
<div class="meta"><h2 id="title"></h2><div id="tags"></div><p id="details"></p><div id="blanknotice" class="blank" hidden>This source page is genuinely blank and was visually reviewed with user approval. Its export marker is metadata; no OCR text was invented.</div></div>
<div class="compare"><section class="panel"><div class="panelhead"><strong>Original PDF scan</strong><label>Zoom <input id="zoom" type="range" min="100" max="250" value="100" step="25"></label><a id="fullimage" target="_blank">Open full scan</a></div><div class="pane" id="scanpane"><img class="scan" id="scan" alt="Photographed AMWS source page"></div></section>
<section class="panel"><div class="panelhead"><strong>OCR output</strong><select id="view"><option value="ordered">Ordered page text</option><option value="lines">Recognized lines and confidence</option></select></div><div class="pane" id="textpane"><pre id="text" class="ocr"></pre><div id="lines" hidden></div></div></section></div>
<section class="review"><h3>Record this page’s review</h3><label>Status <select id="status"><option value="unreviewed">Not reviewed</option><option value="pass">Acceptable for field extraction</option><option value="minor_errors">Minor errors noted</option><option value="needs_redo">Needs OCR correction or rerun</option><option value="source_issue">Source scan issue</option></select></label>
<div class="checklist"><label><input id="order_issue" type="checkbox"> Reading-order problem</label><label><input id="missing_text" type="checkbox"> Missing printed lines or entry</label><label><input id="source_issue" type="checkbox"> Clipped, blurred or damaged source</label></div>
<div class="reviewgrid"><div><b>Names</b><div class="numbers"><label>Checked<input id="names_checked" type="number" min="0" value="0"></label><label>Incorrect<input id="names_errors" type="number" min="0" value="0"></label></div></div><div><b>Birth dates</b><div class="numbers"><label>Checked<input id="dates_checked" type="number" min="0" value="0"></label><label>Incorrect<input id="dates_errors" type="number" min="0" value="0"></label></div></div><div><b>Birthplaces</b><div class="numbers"><label>Checked<input id="places_checked" type="number" min="0" value="0"></label><label>Incorrect<input id="places_errors" type="number" min="0" value="0"></label></div></div></div>
<label>Notes: name / printed value / OCR value; omitted lines; column transitions<textarea id="notes" placeholder="Example: Hadley — printed Aug 1, 50; OCR Aug I, 5O. Check whether a full entry or line is absent."></textarea></label><p id="validation" class="error"></p><p class="help" id="savehint">Notes save in this browser when local storage is available. Export JSON regularly to keep a portable copy; import it to resume elsewhere. The original OCR files are never edited.</p>
<div id="summary" class="summary"></div></section></main></div>
<script id="dataset" type="application/json">__DATA__</script><script>
const data=JSON.parse(document.getElementById('dataset').textContent),pages=data.pages;
const storageKey='amws-ocr-review-'+data.sample_id;
const fields=['status','order_issue','missing_text','source_issue','names_checked','names_errors','dates_checked','dates_errors','places_checked','places_errors','notes'];
let index=0,reviews={};
try{reviews=JSON.parse(localStorage.getItem(storageKey)||'{}')}catch(e){document.getElementById('savehint').textContent='This browser cannot save local notes. Export JSON before closing; import it to resume.'}
const el=id=>document.getElementById(id),key=p=>p.source_file+':'+p.pdf_page_number;
function defaults(){return {status:'unreviewed',order_issue:false,missing_text:false,source_issue:false,names_checked:0,names_errors:0,dates_checked:0,dates_errors:0,places_checked:0,places_errors:0,notes:''}}
function checked(r){return ['names','dates','places'].every(f=>r[f+'_errors']<=r[f+'_checked'])}
function filtered(p){const filter=el('filter').value;return filter==='all'||(filter==='random'&&p.tags.includes('random'))||(filter==='targeted'&&p.tags.some(t=>t!=='random'))||(filter==='unreviewed'&&(reviews[key(p)]||defaults()).status==='unreviewed')}
function navigation(){const nav=el('navigation');nav.replaceChildren();pages.forEach((p,i)=>{if(!filtered(p))return;const b=document.createElement('button');b.className='nav'+(i===index?' active':'')+((reviews[key(p)]||defaults()).status!=='unreviewed'?' done':'');b.textContent=p.source_file.replace('.pdf','')+' · PDF '+p.pdf_page_number;b.onclick=()=>{index=i;render()};nav.appendChild(b)});const done=pages.filter(p=>(reviews[key(p)]||defaults()).status!=='unreviewed').length;el('progress').textContent=done+' / '+pages.length+' sampled pages reviewed'}
function summarize(subset,label){const reviewed=subset.filter(p=>(reviews[key(p)]||defaults()).status!=='unreviewed');const values=reviewed.map(p=>({...defaults(),...reviews[key(p)]})).filter(checked);const sums=f=>values.reduce((n,r)=>n+(Number(r[f])||0),0);const rates=['names','dates','places'].map(f=>{const n=sums(f+'_checked'),errors=sums(f+'_errors');return f+': '+errors+'/'+n+' incorrect'+(n?' ('+(100*errors/n).toFixed(1)+'%)':'')});return label+': '+reviewed.length+' pages reviewed; '+rates.join('; ')+'; missing-text flags '+values.filter(r=>r.missing_text).length+'; reading-order flags '+values.filter(r=>r.order_issue).length+'.'}
function summary(){el('summary').textContent=summarize(pages.filter(p=>p.tags.includes('random')),'Random sample')+' '+summarize(pages.filter(p=>!p.tags.includes('random')),'Targeted-only pages');el('validation').textContent=checked({...defaults(),...reviews[key(pages[index])]})?'':'An error count cannot exceed the number checked.'}
function save(){const r=defaults();fields.forEach(f=>{const input=el(f);r[f]=input.type==='checkbox'?input.checked:input.type==='number'?Math.max(0,Math.floor(Number(input.value)||0)):input.value});r.reviewed_at=new Date().toISOString();reviews[key(pages[index])]=r;try{localStorage.setItem(storageKey,JSON.stringify(reviews))}catch(e){el('savehint').textContent='Notes are held in memory. Export JSON before closing.'}navigation();summary()}
function showText(){const p=pages[index],show=el('view').value==='ordered';el('text').hidden=!show;el('lines').hidden=show;if(show){el('text').textContent=p.text;return}const list=el('lines');list.replaceChildren();p.columns.forEach(c=>{const title=document.createElement('h3');title.textContent='Column '+c.column_number;list.appendChild(title);c.lines.forEach(line=>{const row=document.createElement('div');row.className='line'+(line.confidence<.8?' low':'');const score=document.createElement('span');score.className='score';score.textContent=(line.confidence*100).toFixed(1)+'%';const text=document.createElement('span');text.textContent=line.text;row.append(score,text);list.appendChild(row)})})}
function render(){const p=pages[index],r={...defaults(),...reviews[key(p)]};el('title').textContent=p.source_file+' — PDF page '+p.pdf_page_number+' / '+p.pdf_page_count;el('tags').replaceChildren();p.tags.forEach(t=>{const s=document.createElement('span');s.className='tag';s.textContent=t.replaceAll('_',' ');el('tags').appendChild(s)});el('details').textContent=p.text_characters+' OCR characters; '+p.recognized_lines+' recognized lines'+(p.mean_confidence!==null?'; mean confidence '+(100*p.mean_confidence).toFixed(1)+'%; lines below 80%: '+(100*p.low_confidence_fraction).toFixed(1)+'%':'');el('blanknotice').hidden=p.status!=='verified_blank';el('scan').src=p.image;el('fullimage').href=p.image;el('scan').style.width=el('zoom').value+'%';fields.forEach(f=>{if(el(f).type==='checkbox')el(f).checked=r[f];else el(f).value=r[f]});el('scanpane').scrollTop=0;el('textpane').scrollTop=0;showText();navigation();summary()}
function move(direction){let i=index;for(let tries=0;tries<pages.length;tries++){i=(i+direction+pages.length)%pages.length;if(filtered(pages[i])){index=i;render();break}}}
function download(name,contents,type){const a=document.createElement('a'),url=URL.createObjectURL(new Blob([contents],{type}));a.href=url;a.download=name;a.click();setTimeout(()=>URL.revokeObjectURL(url),5000)}
function result(){return {schema_version:1,sample_id:data.sample_id,seed:data.seed,exported_at:new Date().toISOString(),pages:pages.map(p=>({source_file:p.source_file,pdf_page_number:p.pdf_page_number,sampling_tags:p.tags,review:{...defaults(),...reviews[key(p)]}}))}}
el('jsonexport').onclick=()=>download('amws_ocr_quality_review.json',JSON.stringify(result(),null,2),'application/json');
el('csvexport').onclick=()=>{const quote=v=>'"'+String(v).replaceAll('"','""')+'"';const rows=[['source_file','pdf_page_number','sampling_tags',...fields]];result().pages.forEach(p=>rows.push([p.source_file,p.pdf_page_number,p.sampling_tags.join('|'),...fields.map(f=>p.review[f])]));download('amws_ocr_quality_review.csv',rows.map(row=>row.map(quote).join(',')).join('\r\n'),'text/csv;charset=utf-8')};
el('import').onchange=async event=>{try{const imported=JSON.parse(await event.target.files[0].text());if(imported.sample_id!==data.sample_id)throw Error('This JSON belongs to a different review sample.');reviews=Object.fromEntries(imported.pages.map(p=>[p.source_file+':'+p.pdf_page_number,p.review]));try{localStorage.setItem(storageKey,JSON.stringify(reviews))}catch(e){}render()}catch(e){alert('Import failed: '+e.message)}};
fields.forEach(f=>el(f).addEventListener('input',save));el('filter').onchange=()=>{if(!filtered(pages[index])){const next=pages.findIndex(filtered);if(next>=0)index=next}render()};el('previous').onclick=()=>move(-1);el('next').onclick=()=>move(1);el('zoom').oninput=()=>el('scan').style.width=el('zoom').value+'%';el('view').onchange=showText;
render();
</script></body></html>'''


def load(path):
    return json.loads(path.read_text(encoding="utf-8"))


def confidence(record):
    scores = [float(score) for c in record["prediction"].get("column_predictions", [])
              for score in c["prediction"].get("overall_ocr_res", {}).get("rec_scores", [])]
    return dict(recognized_lines=len(scores), mean_confidence=statistics.mean(scores) if scores else None,
                low_confidence_fraction=sum(s < .8 for s in scores) / len(scores) if scores else None)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ocr-dir", type=Path, default=Path.home() / "ocr_runs" / "amws_ed16")
    parser.add_argument("--output-dir", type=Path, default=Path.home() / "ocr_runs" / "amws_ed16_quality_review")
    parser.add_argument("--seed", type=int, default=1986)
    args = parser.parse_args()
    import pypdfium2 as pdfium

    docs = load(args.ocr_dir / "selection.json")["documents"]
    rng = random.Random(args.seed)
    chosen = defaultdict(set)
    statistics_rows = []
    for doc in docs:
        candidates = []
        for page in range(1, doc["page_count"] + 1):
            record = load(args.ocr_dir / Path(doc["source_file"]).stem / f"page_{page:04d}.json")
            row = dict(source_file=doc["source_file"], pdf_page_number=page,
                       status=record["status"], text_characters=record["text_characters"], **confidence(record))
            statistics_rows.append(row)
            if record["status"] == "ok":
                candidates.append(page)
        for page in rng.sample(candidates, min(2, len(candidates))):
            chosen[(doc["source_file"], page)].add("random")
    for name, page in [("16_C_400_600.pdf", 81), ("16_E_800_900.pdf", 79),
                       ("16_E_800_900.pdf", 80), ("16_G_800_900.pdf", 99)]:
        chosen[(name, page)].add("column_adjustment")
    chosen[("16_C_400_600.pdf", 1)].add("prior_pilot")
    chosen[("16_G_800_900.pdf", 106)].add("verified_blank")
    risk = sorted((r for r in statistics_rows if r["mean_confidence"] is not None),
                  key=lambda r: r["mean_confidence"])[:6]
    for row in risk:
        chosen[(row["source_file"], row["pdf_page_number"])].add("lower_confidence")
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "images").mkdir(exist_ok=True)
    pages = []
    for doc in docs:
        selected = sorted(page for name, page in chosen if name == doc["source_file"])
        with pdfium.PdfDocument(doc["source"]["path"]) as pdf:
            for page_number in selected:
                folder = args.ocr_dir / Path(doc["source_file"]).stem
                record = load(folder / f"page_{page_number:04d}.json")
                image_name = f"{folder.name}_page_{page_number:04d}.png"
                path = args.output_dir / "images" / image_name
                page = pdf[page_number - 1]
                bitmap = page.render(scale=150 / 72)
                image = bitmap.to_pil()
                try:
                    image.save(path)
                finally:
                    image.close()
                    bitmap.close()
                    page.close()
                columns = []
                for c in record["prediction"].get("column_predictions", []):
                    raw = c["prediction"].get("overall_ocr_res", {})
                    texts, scores = raw.get("rec_texts", []), raw.get("rec_scores", [])
                    if len(texts) != len(scores):
                        raise ValueError(f"Mismatched text/confidence arrays: {doc['source_file']} page {page_number}")
                    columns.append(dict(column_number=c["column_number"],
                        lines=[dict(text=t, confidence=float(s)) for t, s in zip(texts, scores)]))
                pages.append(dict(source_file=doc["source_file"], pdf_page_number=page_number,
                    pdf_page_count=doc["page_count"], status=record["status"],
                    text_characters=record["text_characters"], **confidence(record),
                    tags=sorted(chosen[(doc["source_file"], page_number)]), image=f"images/{image_name}",
                    text=(folder / f"page_{page_number:04d}.txt").read_text(encoding="utf-8"), columns=columns))
        print(f"Rendered {doc['source_file']}: {len(selected)} sampled pages", flush=True)
    dataset = dict(sample_id=f"ed16-seed{args.seed}-random2-risk6-v1", seed=args.seed,
                   created_at=datetime.now(timezone.utc).isoformat(), pages=pages,
                   note="Two randomly sampled nonblank pages per PDF plus targeted pages; raw OCR scores are not accuracy estimates.")
    # Escape HTML delimiters so source transcription can never terminate the JSON element.
    payload = json.dumps(dataset, ensure_ascii=False).replace("<", chr(92) + "u003c")
    output = args.output_dir / "index.html"
    output.write_text(HTML.replace("__DATA__", payload), encoding="utf-8")
    (args.output_dir / "sample.json").write_text(json.dumps(dataset, ensure_ascii=False, indent=2), encoding="utf-8")
    (args.output_dir / "page_confidence.json").write_text(json.dumps(statistics_rows, indent=2), encoding="utf-8")
    print(json.dumps(dict(review=str(output), pages=len(pages), random_pages=sum("random" in p["tags"] for p in pages),
                         source_pdfs=len(docs)), indent=2))


if __name__ == "__main__":
    main()

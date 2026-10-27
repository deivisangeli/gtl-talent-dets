"""Visually reviewed exception repairs, using saved OCR and immutable baseline assets."""
from __future__ import annotations

import argparse
from collections import Counter, defaultdict
import copy
import csv
import hashlib
import io
import json
import os
from pathlib import Path
import re
import shutil
import zipfile

import repair_ocr_corpus as corpus
import repair_ocr_paragraphs as first

VERSION = 4
PILOT = [('16_C_400_600.pdf', p) for p in (28, 86, 121, 56, 46)] + [
    ('16_C_800_1000.pdf', 162), ('16_E_600_700.pdf', 58), ('16_E_700_800.pdf', 58)]
HOME = Path.home() / 'ocr_runs'
BASELINE = HOME / 'amws_ed16_paragraph_repaired'
PILOT_OUTPUT = HOME / 'amws_ed16_exception_pilot'
FINAL_OUTPUT = HOME / 'amws_ed16_paragraph_reviewed'
RESERVE = 1024 ** 3


def digest(value):
    return hashlib.sha256(json.dumps(value, ensure_ascii=False, sort_keys=True,
                                    separators=(',', ':')).encode('utf8')).hexdigest()


def linked_copy(source, target):
    """Never modify a linked inode: all subsequent writes use atomic replacement."""
    source, target = Path(source), Path(target)
    target.parent.mkdir(parents=True, exist_ok=True)
    if target.exists() and first.sha256(target) == first.sha256(source):
        return
    temporary = target.with_name(target.name + '.tmp')
    if temporary.exists():
        temporary.unlink()
    try:
        os.link(source, temporary)
    except OSError:
        if shutil.disk_usage(target.parent).free < source.stat().st_size + RESERVE:
            raise OSError('Insufficient space for asset copying with 1 GiB headroom')
        shutil.copyfile(source, temporary)
    temporary.replace(target)


def context(baseline, entry):
    paths = corpus.page_paths(baseline, entry['source_file'], entry['pdf_page_number'])
    baseline_hashes = {e: first.sha256(paths[e]) for e in ('json', 'txt', 'md')}
    derived = corpus.load(paths['json'])
    original = derived['original_record']
    parent = next(b for b in original['prediction']['parsing_res_list']
                  if b.get('block_order') == entry['original_block_order']
                  and b['column_number'] == entry['column_number']
                  and b['block_bbox'] == entry['original_block_bbox'])
    column = next(c for c in original['prediction']['column_predictions']
                  if c['column_number'] == entry['column_number'])
    offset, local = column['x_offset'], list(parent['block_bbox'])
    local[0] -= offset
    local[2] -= offset
    raw = column['prediction']['overall_ocr_res']
    if len({len(raw[k]) for k in ('rec_texts', 'rec_boxes', 'rec_scores')}) != 1:
        raise ValueError('Inconsistent saved detection arrays')
    candidates = sorted([
        dict(source_region_index=i, label=b['label'], local_bbox=b['coordinate'],
             bbox=[b['coordinate'][0] + offset, b['coordinate'][1],
                   b['coordinate'][2] + offset, b['coordinate'][3]])
        for i, b in enumerate(column['prediction']['layout_det_res']['boxes'])
        if b['label'] in first.PARAGRAPH_LABELS and first.contained(b['coordinate'], local)
        and first.area(b['coordinate']) < first.area(local)], key=lambda b: (b['bbox'][1], b['bbox'][0]))
    lines = []
    for i, (text, box, score) in enumerate(zip(raw['rec_texts'], raw['rec_boxes'], raw['rec_scores'])):
        x, y = (box[0] + box[2]) / 2, (box[1] + box[3]) / 2
        center_member = local[0] <= x <= local[2] and local[1] <= y <= local[3]
        # Layout edges sometimes cross the first recognized line. Include an
        # intersecting edge record only when its exact string belongs uniquely
        # to this reference's saved content, never to another exported block.
        edge_member = (text != '' and text.strip() != '' and text.strip() in parent['block_content']
                       and first.intersection(box, local) > 0
                       and not any(text.strip() in b['block_content'] for b in original['prediction']['parsing_res_list']
                                   if b is not parent and b.get('column_number') == entry['column_number']))
        if not (center_member or edge_member):
            continue
        lines.append(dict(source_line_index=i, text=text, confidence=score,
                          bbox=[box[0] + offset, box[1], box[2] + offset, box[3]],
                          local_bbox=box, y=y, membership_method='center' if center_member else 'unique_content_at_edge',
                          owners=[j for j, b in enumerate(candidates) if b['bbox'][1] <= y <= b['bbox'][3]]))
    identity = dict(source_file=entry['source_file'], pdf_page_number=entry['pdf_page_number'],
                    column_number=entry['column_number'], original_block_order=entry['original_block_order'],
                    original_block_bbox=parent['block_bbox'], content_sha256=digest(parent['block_content']),
                    source_json_sha256=derived['source_sha256']['json'])
    return dict(**identity, block_id=digest(identity), baseline_sha256=baseline_hashes,
                source_sha256=derived['source_sha256'], scan_sha256=first.sha256(paths['scan']),
                rejection_reason=entry['reason'], parent=parent, candidates=candidates, lines=lines)


def automatic_groups(ctx):
    groups = [dict(source_line_indices=[], source_region_indices=[b['source_region_index']],
                   block_label='text', bbox=b['bbox']) for b in ctx['candidates']]
    for line in ctx['lines']:
        if line['text'] == '':
            continue
        if len(line['owners']) != 1:
            return None
        groups[line['owners'][0]]['source_line_indices'].append(line['source_line_index'])
    if len(groups) < 2 or any(not g['source_line_indices'] for g in groups):
        return None
    try:
        validate_groups(ctx, groups)
    except ValueError:
        return None
    return groups


def validate_groups(ctx, groups):
    by_index = {l['source_line_index']: l for l in ctx['lines']}
    expected = {i for i, l in by_index.items() if l['text'] != ''}
    assigned = [i for g in groups for i in g['source_line_indices']]
    if Counter(assigned) != Counter(expected):
        raise ValueError('Nonempty detections must be assigned exactly once')
    last_y = float('-inf')
    for group in groups:
        indices = group['source_line_indices']
        if not indices or group.get('block_label', 'text') not in ('text', 'header', 'footer', 'number', 'unclassified'):
            raise ValueError('Empty or invalid accepted region')
        ys = [by_index[i]['y'] for i in indices]
        if min(ys) < last_y:
            raise ValueError('Accepted region groups interleave vertically')
        last_y = max(ys)
        if 'bbox' in group and (len(group['bbox']) != 4 or first.area(group['bbox']) <= 0):
            raise ValueError('Invalid accepted region geometry')
    if sum(g.get('block_label', 'text') == 'text' for g in groups) < 2:
        raise ValueError('Reference must retain at least two paragraphs')


def decision_valid(ctx, decision):
    if decision.get('context_sha256') != digest(ctx):
        raise ValueError('Review decision is stale for its source or baseline')
    if decision.get('scan_sha256') != ctx['scan_sha256'] or not decision.get('reviewed_at') or not decision.get('notes'):
        raise ValueError('Missing bound visual-inspection evidence')
    if decision.get('review_status') not in ('accepted', 'retained_unresolved'):
        raise ValueError('Unknown review status')
    if decision['review_status'] == 'accepted':
        validate_groups(ctx, decision['groups'])
    elif not decision.get('reason'):
        raise ValueError('Unresolved decision needs a reason')


def replacements(ctx, groups):
    validate_groups(ctx, groups)
    lookup = {l['source_line_index']: l for l in ctx['lines']}
    blocks = []
    for n, group in enumerate(groups, 1):
        indices = sorted(group['source_line_indices'], key=lambda i: (lookup[i]['bbox'][1], lookup[i]['bbox'][0], i))
        lines = [dict(source_line_index=i, text=lookup[i]['text'], confidence=lookup[i]['confidence'],
                      bbox=lookup[i]['bbox'], source_file=ctx['source_file'], pdf_page_number=ctx['pdf_page_number'],
                      column_number=ctx['column_number']) for i in indices]
        box = group.get('bbox') or [min(l['bbox'][0] for l in lines), min(l['bbox'][1] for l in lines),
                                   max(l['bbox'][2] for l in lines), max(l['bbox'][3] for l in lines)]
        blocks.append(dict(block_label=group.get('block_label', 'text'), block_bbox=box,
                           block_content='\n'.join(l['text'] for l in lines), column_number=ctx['column_number'],
                           paragraph_repair=dict(original_block_order=ctx['original_block_order'], paragraph_number=n,
                               original_layout_label='reviewed_saved_regions', source_line_indices=indices, lines=lines),
                           exception_repair=dict(block_id=ctx['block_id'], source_region_indices=group.get('source_region_indices', []))))
    return blocks


def derive(baseline, filename, page, contexts, decisions):
    paths = corpus.page_paths(baseline, filename, page)
    before = corpus.load(paths['json'])
    derived = copy.deepcopy(before)
    blocks = derived['corrected_prediction']['parsing_res_list']
    exports = {e: paths[e].read_bytes() for e in ('txt', 'md')}
    changes, audits = [], []
    for ctx in contexts:
        # The reference export can dehyphenate a line ending, but no saved
        # reference character may be silently excluded by geometric selection.
        parent_chars = Counter(c for c in ctx['parent']['block_content'] if not c.isspace())
        saved_chars = Counter(c for line in ctx['lines'] for c in line['text'] if not c.isspace())
        if parent_chars - saved_chars:
            raise ValueError('Target detection selection omits reference content')
        decision = decisions.get(ctx['block_id'])
        audit = dict(block_id=ctx['block_id'], source_file=filename, pdf_page_number=page,
                     column_number=ctx['column_number'], original_block_order=ctx['original_block_order'],
                     original_block_bbox=ctx['original_block_bbox'], status='unresolved', reason='awaiting_visual_review',
                     source_line_count=len(ctx['lines']), assigned_line_count=0,
                     empty_detection_records=[{k: l[k] for k in ('source_line_index', 'text', 'confidence', 'bbox', 'local_bbox')}
                                             for l in ctx['lines'] if l['text'] == ''],
                     retained_unresolved_detection_records=[])
        if decision:
            decision_valid(ctx, decision)
            audit.update(review=copy.deepcopy(decision), reason=decision.get('reason', ''))
            if decision['review_status'] == 'accepted':
                new = replacements(ctx, decision['groups'])
                matches = [i for i, b in enumerate(blocks) if b['block_label'] == 'reference'
                           and b['column_number'] == ctx['column_number'] and b['block_bbox'] == ctx['original_block_bbox']
                           and b['block_content'] == ctx['parent']['block_content']]
                if len(matches) != 1:
                    raise ValueError('Cannot uniquely locate first-pass target reference')
                changes.append((ctx['parent']['block_content'], '\n\n'.join(b['block_content'] for b in new)))
                for ext, value in exports.items():
                    first.replace_export(value, changes)
                index = matches[0]
                blocks[index:index + 1] = new
                audit.update(status='repaired', paragraph_count=sum(b['block_label'] == 'text' for b in new),
                             region_count=len(new), source_line_count=len(ctx['lines']),
                             assigned_line_count=sum(len(g['source_line_indices']) for g in decision['groups']))
            else:
                audit['retained_unresolved_detection_records'] = [
                    {k: l[k] for k in ('source_line_index', 'text', 'confidence', 'bbox', 'local_bbox')}
                    for l in ctx['lines'] if l['text'] != '']
        audits.append(audit)
    if changes:
        for i, block in enumerate(blocks, 1):
            block['block_order'] = i
        exports = {e: first.replace_export(v, changes) for e, v in exports.items()}
        derived['exports'] = {e + '_sha256': hashlib.sha256(v).hexdigest() for e, v in exports.items()}
    if contexts:
        derived.update(exception_repair_version=VERSION, baseline_json_sha256=first.sha256(paths['json']),
                       exception_audit=audits)
    return derived, exports, audits


def combined_audits(derived, audits):
    result = copy.deepcopy(derived['repair_audit'])
    for update in audits:
        original = next(a for a in result if a['column_number'] == update['column_number']
                        and a['original_block_bbox'] == update['original_block_bbox'] and a['status'] == 'unresolved')
        original.update({k: v for k, v in update.items() if k not in ('source_file', 'pdf_page_number')})
    return result


def html_page(derived, row, baseline_exports, exports, contexts, decisions, navigation):
    html = corpus.view_html(derived['original_record'], derived['corrected_prediction']['parsing_res_list'],
                           row, baseline_exports, exports['txt'], exports['md'], navigation).decode('utf8')
    match = re.search(r'(<script id="data" type="application/json">)(.*?)(</script>)', html, re.S)
    data = json.loads(match.group(2))
    page = data['pages'][0]
    for compact, full in zip(page['blocks'], derived['corrected_prediction']['parsing_res_list']):
        if 'exception_repair' in full:
            compact['exception_repair'] = full['exception_repair']
    preview_changes = []
    for ctx in contexts:
        groups = automatic_groups(ctx)
        if ctx['block_id'] in decisions or groups is None:
            continue
        new = replacements(ctx, groups)
        preview_changes.append((ctx['parent']['block_content'], '\n\n'.join(b['block_content'] for b in new)))
        index = next(i for i, b in enumerate(page['blocks']) if b['block_label'] == 'reference'
                     and b['column_number'] == ctx['column_number'] and b['block_bbox'] == ctx['original_block_bbox'])
        compact = []
        for block in new:
            compact.append({**{k: block[k] for k in ('block_label', 'block_content', 'block_bbox', 'column_number')},
                'paragraph_repair': dict(line_count=len(block['paragraph_repair']['lines'])),
                'exception_repair': dict(block_id=ctx['block_id'], proposal=True)})
        page['blocks'][index:index + 1] = compact
    if preview_changes:
        page['corrected'] = {e: first.replace_export(exports[e], preview_changes).decode('utf8') for e in ('txt', 'md')}
        page['outcome'] = 'proposal_pending_review'
    page['exception_targets'] = [dict(block_id=c['block_id'], column_number=c['column_number'],
        rejection_reason=c['rejection_reason'], candidates=c['candidates'], lines=c['lines'],
        groups=(decisions.get(c['block_id']) or {}).get('groups', automatic_groups(c)),
        review=decisions.get(c['block_id'], dict(review_status='pending'))) for c in contexts]
    payload = json.dumps(data, ensure_ascii=False).replace('<', '\\u003c')
    html = html[:match.start(2)] + payload + html[match.end(2):]
    html = html.replace('Original export', 'First-pass export').replace('Corrected export', 'Reviewed export')
    if preview_changes:
        html = html.replace('Reviewed export', 'Proposed export — awaiting inspection')
    html = html.replace('</main>', '<section class="panel"><div class="head"><b>Exception assignment ledger</b></div><div id="exception-ledger" style="padding:18px"></div></section></main>')
    return html.encode('utf8')


EXTRA_JS = r'''
if(p.exception_targets){
const ledger=el('exception-ledger');
for(const t of p.exception_targets){const title=document.createElement('h3');title.textContent='Column '+t.column_number+' · '+t.review.review_status+' · '+t.rejection_reason;ledger.append(title);
const note=document.createElement('p');note.textContent=t.review.notes||'Proposed assignments await visual inspection; saved exports remain unchanged until accepted.';ledger.append(note);
const table=document.createElement('table');table.style.width='100%';table.style.fontSize='12px';
const owners=new Map();(t.groups||[]).forEach((g,n)=>g.source_line_indices.forEach(i=>owners.set(i,'region '+(n+1)+' / '+(g.block_label||'text'))));
for(const l of t.lines){const tr=document.createElement('tr');for(const v of [l.source_line_index,l.text===''?'empty detection':owners.get(l.source_line_index)||'UNASSIGNED',l.text,l.confidence.toFixed(4)]){const td=document.createElement('td');td.textContent=v;td.style.padding='4px';tr.append(td)}table.append(tr)}ledger.append(table);
}
const legend=document.createElement('p');legend.textContent='Dashed purple: original candidate boxes. Blue: accepted exception assignments. Green: first-pass recovered paragraphs. Amber: unresolved blocks. Empty detections are retained in the audit.';ledger.prepend(legend);
function drawExceptions(){
for(const t of p.exception_targets)for(const b of t.candidates){const rect=document.createElementNS(ns,'rect');rect.setAttribute('x',b.bbox[0]);rect.setAttribute('y',b.bbox[1]);rect.setAttribute('width',b.bbox[2]-b.bbox[0]);rect.setAttribute('height',b.bbox[3]-b.bbox[1]);rect.setAttribute('fill','none');rect.setAttribute('stroke','#8b5bb0');rect.setAttribute('stroke-width','3');rect.setAttribute('stroke-dasharray','12 8');rect.dataset.kind='original-candidate';el('map').append(rect)}
const cards=[...el('blocks').children];let rectIndex=0;const rects=[...el('map').querySelectorAll('rect:not([data-kind="original-candidate"])')];
p.blocks.forEach((b,i)=>{const body=!['header','footer','number'].includes(b.block_label)&&b.block_content.trim();const rect=body?rects[rectIndex++]:null;if(!b.exception_repair)return;const card=cards[i];card.style.borderLeft='4px solid #1f57b0';card.style.background='#edf3ff';const badge=card.querySelector('.badge');if(badge)badge.textContent+=' · reviewed exception';else{const label=document.createElement('span');label.className='badge';label.textContent='Reviewed exception · '+b.block_label;card.prepend(label)}if(rect){rect.dataset.kind='reviewed-exception';rect.setAttribute('fill','#cfddff');rect.setAttribute('stroke','#1f57b0')}});
}
const baseRender=render;render=function(){baseRender();drawExceptions()};el('format').onchange=render;el('marked').onchange=render;drawExceptions();
}
'''


def selected_contexts(baseline, pilot=False):
    queue = corpus.load(baseline / 'review_queue.json')['blocks']
    if len(queue) != 138 or len({(e['source_file'], e['pdf_page_number']) for e in queue}) != 132:
        raise ValueError('Unexpected baseline exception inventory')
    return [context(baseline, entry) for entry in queue
            if not pilot or (entry['source_file'], entry['pdf_page_number']) in PILOT]


def validate_baseline(baseline):
    manifest = corpus.load(baseline / 'output_manifest.json')['files']
    for relative, expected in manifest.items():
        if first.sha256(baseline / relative) != expected:
            raise ValueError(f'Immutable first-pass baseline changed: {relative}')
    selection = corpus.load(baseline / 'corpus_selection.json')
    if corpus.inventory(Path(selection['source_dir'])) != selection['documents']:
        raise ValueError('Original corpus selection changed')


def checkpoint_valid(output, path, identity):
    try:
        record = corpus.load(path)
        if record['identity'] != identity or set(record['outputs']) != {'json', 'txt', 'md', 'scan', 'viewer'}:
            return False
        return all(output.resolve() in (output / item['path']).resolve().parents
                   and first.sha256(output / item['path']) == item['sha256']
                   for item in record['outputs'].values())
    except (OSError, ValueError, KeyError, TypeError):
        return False


def page_identity(baseline_paths, contexts, decisions):
    return dict(version=VERSION,
                baseline={e: first.sha256(baseline_paths[e]) for e in ('json', 'txt', 'md', 'scan', 'viewer')},
                contexts_sha256=digest(contexts),
                decisions_sha256=digest({c['block_id']: decisions.get(c['block_id']) for c in contexts}))


def process_page(baseline, output, doc, page, contexts, decisions, navigation):
    filename = doc['source_file']
    src, dst = corpus.page_paths(baseline, filename, page), corpus.page_paths(output, filename, page)
    identity = page_identity(src, contexts, decisions)
    checkpoint = dst['checkpoint'].with_suffix('.exception.json')
    if checkpoint_valid(output, checkpoint, identity):
        return corpus.load(checkpoint)['summary'], True
    if not contexts:
        for ext in ('json', 'txt', 'md', 'scan', 'viewer'):
            linked_copy(src[ext], dst[ext])
        derived = corpus.load(src['json'])
        exports = {e: src[e].read_bytes() for e in ('txt', 'md')}
        audits = []
    else:
        derived, exports, audits = derive(baseline, filename, page, contexts, decisions)
        corpus.write_json(dst['json'], derived)
        for ext, value in exports.items():
            corpus.write_bytes(dst[ext], value)
        linked_copy(src['scan'], dst['scan'])
    merged = combined_audits(derived, audits)
    source = Path(corpus.load(baseline / 'corpus_selection.json')['source_dir'])
    original = {e: corpus.page_paths(source, filename, page)[e].read_bytes() for e in ('txt', 'md')}
    row = corpus.page_row(derived['original_record'], merged, derived['source_sha256'], exports['txt'], exports['md'], original)
    row.update(exception_blocks=len(contexts), accepted_exception_blocks=sum(a['status'] == 'repaired' for a in audits),
               exception_audits=audits, baseline_exports_unchanged=all(exports[e] == src[e].read_bytes() for e in ('txt', 'md')))
    if contexts:
        corpus.write_bytes(dst['viewer'], html_page(derived, row, {e: src[e].read_bytes() for e in ('txt', 'md')},
                           exports, contexts, decisions, navigation))
    outputs = {e: dict(path=dst[e].relative_to(output).as_posix(), sha256=first.sha256(dst[e]))
               for e in ('json', 'txt', 'md', 'scan', 'viewer')}
    corpus.write_json(checkpoint, dict(identity=identity, outputs=outputs, summary=row))
    return row, False


def verify(baseline, output, docs, contexts, decisions, pilot=False):
    validate_baseline(baseline)
    by_page = defaultdict(list)
    for ctx in contexts:
        by_page[(ctx['source_file'], ctx['pdf_page_number'])].append(ctx)
        if ctx['block_id'] not in decisions:
            raise ValueError('Every target block must have a completed visual decision')
        decision_valid(ctx, decisions[ctx['block_id']])
    rows = []
    for doc in docs:
        for page in range(1, doc['page_count'] + 1):
            target_contexts = by_page.get((doc['source_file'], page), [])
            if pilot and not target_contexts:
                continue
            derived, exports, audits = derive(baseline, doc['source_file'], page, target_contexts, decisions)
            paths = corpus.page_paths(output, doc['source_file'], page)
            if corpus.load(paths['json']) != derived or any(paths[e].read_bytes() != exports[e] for e in ('txt', 'md')):
                raise ValueError(f'Derived page differs from reviewed assignments: {doc["source_file"]}:{page}')
            original = corpus.load(corpus.page_paths(baseline, doc['source_file'], page)['json'])['original_record']
            if derived['original_record'] != original:
                raise ValueError('Original prediction changed')
            source_root = Path(corpus.load(baseline / 'corpus_selection.json')['source_dir'])
            if any(first.sha256(corpus.page_paths(source_root, doc['source_file'], page)[e]) != derived['source_sha256'][e]
                   for e in ('json', 'txt', 'md')):
                raise ValueError('Original OCR source triplet changed')
            baseline_blocks = corpus.load(corpus.page_paths(baseline, doc['source_file'], page)['json'])['corrected_prediction']['parsing_res_list']
            accepted = [c for c in target_contexts if decisions[c['block_id']]['review_status'] == 'accepted']
            expected_unchanged = [b for b in baseline_blocks if not any(b['block_label'] == 'reference'
                and b['column_number'] == c['column_number'] and b['block_bbox'] == c['original_block_bbox'] for c in accepted)]
            actual_unchanged = [b for b in derived['corrected_prediction']['parsing_res_list'] if 'exception_repair' not in b]
            without_order = lambda b: {k: v for k, v in b.items() if k != 'block_order'}
            if [without_order(b) for b in expected_unchanged] != [without_order(b) for b in actual_unchanged]:
                raise ValueError('An unaffected first-pass block changed')
            if {k: v for k, v in derived['corrected_prediction'].items() if k != 'parsing_res_list'} != {
                    k: v for k, v in corpus.load(corpus.page_paths(baseline, doc['source_file'], page)['json'])['corrected_prediction'].items()
                    if k != 'parsing_res_list'}:
                raise ValueError('Column or gutter geometry changed')
            checkpoint = paths['checkpoint'].with_suffix('.exception.json')
            saved = corpus.load(checkpoint)
            if not checkpoint_valid(output, checkpoint, page_identity(
                    corpus.page_paths(baseline, doc['source_file'], page), target_contexts, decisions)):
                raise ValueError('Output checkpoint hashes are invalid')
            rows.append(saved['summary'])
        if not pilot:
            for ext in ('txt', 'md'):
                expected = b'\n'.join(f'[{doc["source_file"]} | PDF page {p}]\n'.encode('utf8')
                    + corpus.page_paths(output, doc['source_file'], p)[ext].read_bytes() for p in range(1, doc['page_count'] + 1))
                if (output / Path(doc['source_file']).stem / f'document.{ext}').read_bytes() != expected:
                    raise ValueError('Invalid document page markers')
    accepted = sum(decisions[c['block_id']]['review_status'] == 'accepted' for c in contexts)
    assigned = empty = retained = 0
    for ctx in contexts:
        nonempty = sum(l['text'] != '' for l in ctx['lines'])
        empty += len(ctx['lines']) - nonempty
        if decisions[ctx['block_id']]['review_status'] == 'accepted':
            assigned += nonempty
        else:
            retained += nonempty
    expected_pages = len(by_page) if pilot else sum(d['page_count'] for d in docs)
    blank_pages = sum(r['outcome'] == 'verified_blank' for r in rows)
    if len(rows) != expected_pages or (not pilot and (expected_pages != 1792 or blank_pages != 1)):
        raise ValueError('Incomplete corpus or approved blank accounting')
    result = dict(complete=True, verified_at=corpus.now(), pilot=pilot, pages=len(rows), target_blocks=len(contexts),
        accepted_exception_blocks=accepted, retained_unresolved_blocks=len(contexts) - accepted,
        version=VERSION, verified_blank_pages=blank_pages, target_detection_records=assigned + empty + retained,
        assigned_nonempty_records=assigned, audited_empty_records=empty, retained_unresolved_nonempty_records=retained,
        detection_partition_verified=True, nonempty_strings_unchanged=True, original_predictions_preserved=True,
        first_pass_blocks_preserved=True, column_geometry_preserved=True, visual_review_complete=True)
    corpus.write_json(output / 'verification.json', result)
    return result


def exception_reports(output, contexts, decisions, rows):
    blocks = [a for r in rows for a in r['exception_audits']]
    recognition = []
    for ctx in contexts:
        decision = decisions.get(ctx['block_id'], {})
        for issue in decision.get('recognition_issues', []):
            item = dict(block_id=ctx['block_id'], source_file=ctx['source_file'],
                        pdf_page_number=ctx['pdf_page_number'], column_number=ctx['column_number'],
                        viewer=f'views/{Path(ctx["source_file"]).stem}_page_{ctx["pdf_page_number"]:04d}.html')
            item.update(issue if isinstance(issue, dict) else dict(note=issue))
            recognition.append(item)
    corpus.write_json(output / 'recognition_review_queue.json', dict(
        scope='Visual observations only; recognition was not rerun or corrected.',
        suspected_issues=len(recognition), pages=len({(r['source_file'], r['pdf_page_number']) for r in recognition}),
        issues=recognition))
    fields = sorted({k for r in recognition for k in r}) or ['source_file', 'pdf_page_number', 'note']
    stream = io.StringIO(newline='')
    writer = csv.DictWriter(stream, fieldnames=fields)
    writer.writeheader()
    for item in recognition:
        writer.writerow({k: json.dumps(v, ensure_ascii=False) if isinstance(v, (list, dict)) else v for k, v in item.items()})
    corpus.write_bytes(output / 'recognition_review_queue.csv', stream.getvalue().encode('utf-8-sig'))
    corpus.write_json(output / 'exception_audit.json', dict(version=VERSION, blocks=blocks,
        inspected_blocks=sum(c['block_id'] in decisions for c in contexts),
        accepted_blocks=sum(a['status'] == 'repaired' for a in blocks),
        retained_unresolved_blocks=sum(a['status'] == 'unresolved' for a in blocks),
        recovered_paragraphs=sum(a.get('paragraph_count', 0) for a in blocks),
        empty_detection_records=sum(len(a['empty_detection_records']) for a in blocks),
        pending_visual_decisions=sum(c['block_id'] not in decisions for c in contexts)))
    index = (output / 'index.html').read_text(encoding='utf8')
    index = index.replace('Visual review sample', 'Complete exception visual review')
    index = index.replace('</div></header>', '<a href="recognition_review_queue.csv">Suspected recognition issues (CSV)</a>'
                          '<a href="exception_audit.json">Exception audit</a></div></header>')
    corpus.write_bytes(output / 'index.html', index.encode('utf8'))


def package(output):
    """Reuse verified membership rules while enforcing headroom during writing."""
    if not corpus.load(output / 'verification.json')['complete']:
        raise ValueError('Cannot package an incomplete reviewed corpus')
    requested = corpus.load(output / 'visual_review_sample.json')['pages']
    reviewed = {(p['source_file'], p['pdf_page_number']): p
                for p in corpus.load(output / 'visual_review.json')['pages']}
    for page in requested:
        review = reviewed.get((page['source_file'], page['pdf_page_number']), {})
        current = first.sha256(corpus.page_paths(output, page['source_file'], page['pdf_page_number'])['json'])
        if (review.get('derived_json_sha256') != page['derived_json_sha256']
                or current != page['derived_json_sha256']
                or review.get('review_status') not in ('passed', 'flagged_as_expected')):
            raise ValueError('Visual review is missing or stale')
    manifest = output / 'output_manifest.json'
    files = sorted(p for p in output.rglob('*') if p.is_file() and p != manifest
                   and not p.name.endswith('.tmp') and p.name not in ('run_state.json', 'packaging.json'))
    members = {p.relative_to(output).as_posix(): first.sha256(p) for p in files}
    corpus.write_json(manifest, dict(files=members, file_count=len(members)))
    members['output_manifest.json'] = first.sha256(manifest)
    files.append(manifest)
    archive = output.with_suffix('.zip')
    temporary = archive.with_name(archive.name + '.tmp')
    minimum_free = shutil.disk_usage(output).free
    with zipfile.ZipFile(temporary, 'w', compression=zipfile.ZIP_DEFLATED, compresslevel=6, allowZip64=True) as z:
        for number, path in enumerate(files, 1):
            free = shutil.disk_usage(output).free
            # An uncompressed member is a conservative bound on its next write.
            if free < path.stat().st_size + RESERVE + 1024**2:
                raise OSError('ZIP writing stopped to preserve 1 GiB free headroom')
            z.write(path, path.relative_to(output).as_posix())
            minimum_free = min(minimum_free, shutil.disk_usage(output).free)
            if number % 1000 == 0:
                print(f'ZIP: {number}/{len(files)} members written', flush=True)
    with zipfile.ZipFile(temporary) as z:
        if set(z.namelist()) != set(members) or z.testzip() is not None:
            raise ValueError('ZIP membership or CRC verification failed')
        for name, expected in members.items():
            sha = hashlib.sha256()
            with z.open(name) as stream:
                for chunk in iter(lambda: stream.read(1024**2), b''):
                    sha.update(chunk)
            if sha.hexdigest() != expected:
                raise ValueError(f'ZIP member hash mismatch: {name}')
    if shutil.disk_usage(output).free < RESERVE:
        raise OSError('Free space fell below the required headroom')
    temporary.replace(archive)
    result = dict(archive=str(archive), sha256=first.sha256(archive), size_bytes=archive.stat().st_size,
                  members=len(members), crc_verified=True, member_hashes_verified=True,
                  manifest_sha256=members['output_manifest.json'], minimum_free_bytes=minimum_free,
                  created_at=corpus.now(), destination='local')
    corpus.write_json(output / 'packaging.json', result)
    return result


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--baseline-dir', type=Path, default=BASELINE)
    parser.add_argument('--output-dir', type=Path)
    parser.add_argument('--phase', choices=('pilot', 'all', 'verify', 'package'), default='pilot')
    args = parser.parse_args(argv)
    baseline = args.baseline_dir.resolve()
    pilot = args.phase == 'pilot'
    output = (args.output_dir or (PILOT_OUTPUT if pilot else FINAL_OUTPUT)).resolve()
    if output == baseline or baseline in output.parents:
        raise ValueError('Output must be separate from immutable baseline')
    output.mkdir(parents=True, exist_ok=True)
    selection = corpus.load(baseline / 'corpus_selection.json')
    docs = selection['documents']
    contexts = selected_contexts(baseline, pilot)
    decisions_path = output / 'decisions.json'
    decisions = corpus.load(decisions_path) if decisions_path.exists() else {}
    if args.phase == 'all':
        if not corpus.load(PILOT_OUTPUT / 'verification.json')['complete']:
            raise ValueError('Pilot verification must pass before full processing')
        pilot_decisions = corpus.load(PILOT_OUTPUT / 'decisions.json')
        for key, value in pilot_decisions.items():
            decisions.setdefault(key, value)
    for ctx in contexts:
        if ctx['block_id'] in decisions:
            decision_valid(ctx, decisions[ctx['block_id']])
    corpus.write_json(decisions_path, decisions)
    if args.phase in ('verify', 'package'):
        print(json.dumps(verify(baseline, output, docs, contexts, decisions), indent=2), flush=True)
        if args.phase == 'package':
            partial = output.with_suffix('.zip').with_name(output.with_suffix('.zip').name + '.tmp')
            # This utility's incomplete ZIP can be rebuilt; an existing final
            # archive stays untouched until all integrity checks pass.
            if partial.is_file():
                partial.unlink()
            size = baseline.with_suffix('.zip').stat().st_size
            if shutil.disk_usage(output).free < size + RESERVE:
                raise OSError('Packaging needs estimated ZIP size plus 1 GiB free headroom')
            result = package(output)
            corpus.write_json(output / 'run_state.json', dict(state='complete_with_review_queue', **result))
            print(json.dumps(result, indent=2), flush=True)
        return 0
    if shutil.disk_usage(output).free < 350 * 1024**2 + RESERVE:
        raise OSError('Insufficient assembly headroom')
    corpus.write_json(output / 'corpus_selection.json', selection)
    corpus.write_json(output / 'proposals.json', dict(version=VERSION, blocks=[dict(**c, proposed_groups=automatic_groups(c)) for c in contexts]))
    corpus.write_bytes(output / 'review.js', (corpus.VIEW_JS + EXTRA_JS).encode('utf8'))
    by_page = defaultdict(list)
    for ctx in contexts:
        by_page[(ctx['source_file'], ctx['pdf_page_number'])].append(ctx)
    rows, resumed = [], 0
    pages = [(d['source_file'], p) for d in docs for p in range(1, d['page_count'] + 1)
             if not pilot or (d['source_file'], p) in by_page]
    for doc in docs:
        for page in range(1, doc['page_count'] + 1):
            key = (doc['source_file'], page)
            if key not in pages:
                continue
            index = len(rows)
            nav = {'Corpus index': '../index.html'}
            for label, position in [('Previous page', index - 1), ('Next page', index + 1)]:
                if 0 <= position < len(pages):
                    name, number = pages[position]
                    nav[label] = f'{Path(name).stem}_page_{number:04d}.html'
            row, skipped = process_page(baseline, output, doc, page, by_page[key], decisions, nav)
            rows.append(row)
            resumed += skipped
            if len(rows) % 100 == 0:
                print(f'{len(rows)}/{len(pages)} pages assembled; {resumed} resumed', flush=True)
        if not pilot:
            corpus.document_exports(output, doc)
    corpus.reports(output, docs, rows)
    exception_reports(output, contexts, decisions, rows)
    if not pilot:
        for source in (baseline / 'approved_blank_review').rglob('*'):
            if source.is_file():
                linked_copy(source, output / 'approved_blank_review' / source.relative_to(baseline / 'approved_blank_review'))
    requested, reviews = [], []
    for filename, page in sorted(by_page):
        paths = corpus.page_paths(output, filename, page)
        item = dict(source_file=filename, pdf_page_number=page, derived_json_sha256=first.sha256(paths['json']),
                    viewer=f'views/{Path(filename).stem}_page_{page:04d}.html')
        requested.append(item)
        target_decisions = [decisions.get(c['block_id']) for c in by_page[(filename, page)]]
        if all(target_decisions):
            reviews.append(dict(**item, review_status='passed' if all(d['review_status'] == 'accepted' for d in target_decisions)
                                else 'flagged_as_expected', block_ids=[c['block_id'] for c in by_page[(filename, page)]]))
    corpus.write_json(output / 'visual_review_sample.json', dict(pages=requested, note='Every target block is visually reviewed.'))
    corpus.write_json(output / 'visual_review.json', dict(pages=reviews))
    state = dict(state='awaiting_visual_review', pages=len(rows), resumed=resumed,
                 target_blocks=len(contexts), pending=len(contexts) - len([c for c in contexts if c['block_id'] in decisions]))
    if state['pending'] == 0:
        verify(baseline, output, docs, contexts, decisions, pilot)
        state['state'] = 'verified_ready_for_packaging' if not pilot else 'pilot_verified'
    corpus.write_json(output / 'run_state.json', state)
    print(json.dumps(state, indent=2), flush=True)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())

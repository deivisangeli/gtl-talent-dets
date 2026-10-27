"""Bounded, evidence-preserving AMWS edition-16 discipline regex classifier."""
import json
import re
import unicodedata
from pathlib import Path

RULES = json.loads(Path(__file__).with_name('rules_v1.json').read_text(encoding='utf-8'))
COMPILED = {family: [(r, re.compile(r['pattern'])) for r in RULES[family]]
            for family in ('specific_rules', 'general_rules')}
SECTION = re.compile(r'(?:Educ|Edue|Edwc|Edoc|Edut)(?=\s|[:.;,\-])|\bEd\b(?=\s+[A-Z])|\bf[.]?Juc\b|\bEDI[^A-Z]{0,3}F\b|\b(?:PROF\s*(?:EXP|EXPERIENCE)|CONCURRENT\s+POS|HONORS|MEM\s*[:;.]|RES\s*[:;.]|MAILING\s+ADD)\b', re.I)
FIELD_DEBRIS = re.compile(r'\b(?:EDUC|EDUE|PROF|PROFE|UNIV|PHD|DEPT|MAILING|ADDR|STREET|AVENUE|MEM)\b|\bAM\s+SOC\b|\bB\s+(?:NEW|CABOOL|MOBILE)\b', re.I)
MONTH = r'(?:JAN(?:UARY)?|FEB(?:RUARY)?|MAR(?:CH)?|APR(?:IL)?|MAY|JUN(?:E)?|JUL(?:Y)?|AUG(?:UST)?|SEP(?:T(?:EMBER)?)?|OCT(?:OBER)?|NOV(?:EMBER)?|DEC(?:EMBER)?)'
DATE = re.compile(r'\b' + MONTH + r'[.,\s]*[0-9OISZLB]{1,2}[,.;\s]*[0-9OISZLB]{1,4}\b', re.I)
DEMOGRAPHIC = re.compile(r'(?:\b(?:M|C|S|TN|E)(?=[\s0-9])\s*(?:[0-9]{1,4}|[OISZLB](?=[\s,;:.]|$))(?:\s*[,;]\s*[0-9]{2})?|\b(?:WID|DIV)\b)[,;:.\s]*', re.I)


def normalize(text):
    """Normalize each code point, retaining a map to literal original offsets."""
    out, positions = [], []
    for i, char in enumerate(text):
        expanded = unicodedata.normalize('NFKC', char).upper()
        for c in expanded:
            if c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789':
                out.append(c); positions.append(i)
            else:
                if out and out[-1] != ' ':
                    out.append(' '); positions.append(i)
    if out and out[-1] == ' ':
        out.pop(); positions.pop()
    return ''.join(out), positions


def hits(text):
    norm, mapping = normalize(text)
    used, found = [], []
    for family in ('specific_rules', 'general_rules'):
        for rule, pattern in COMPILED[family]:
            for m in pattern.finditer(norm):
                if family == 'general_rules' and any(m.start() < b and m.end() > a for a, b in used):
                    continue
                start, end = mapping[m.start()], mapping[m.end()-1]+1
                found.append(dict(rule_id=rule['id'], group=rule['group'], evidence=text[start:end], start=start, end=end))
                if family == 'specific_rules':
                    used.append(m.span())
    return sorted(found, key=lambda h: (h['start'], h['end'], h['rule_id']))


def heading(text, name=''):
    """Read only the initial discipline region, stopping before biography sections."""
    stop = SECTION.search(text[:600])
    end = stop.start() if stop else min(len(text), 260)
    region = text[:end]
    start = len(name) if name and region.startswith(name) else 0
    birth = re.search(r'(?:^|[,.;\s])b\s+', region[start:], re.I)
    if birth:
        start += birth.end()
    dates = list(DATE.finditer(region, start))
    if dates:
        start = dates[-1].end()
    demos = list(DEMOGRAPHIC.finditer(region, start))
    if demos:
        start = demos[-1].end()
    candidate = region[start:].strip(' ,;:.\t\n-')
    start = region.find(candidate, start) if candidate else start
    if not candidate or len(candidate) > 180:
        return '', start, 'damaged' if candidate else 'missing'
    recognized = hits(candidate)
    if FIELD_DEBRIS.search(candidate):
        if not recognized:
            return '', start, 'damaged'
        first = min(h['start'] for h in recognized)
        prefix, suffix = candidate[:first], candidate[first:]
        # Recover a clear, uppercase terminal discipline after OCR debris, but
        # never turn a degree or employment field into heading evidence.
        letters = [c for c in suffix if c.isalpha()]
        uppercase_ratio = sum(c.isupper() for c in letters) / max(1, len(letters))
        if (FIELD_DEBRIS.search(suffix) or re.search(r'\b(?:PHD|BS|BA|MS|MA|MD|BSC)\b', prefix, re.I)
                or re.search(r'\d', suffix) or uppercase_ratio < 0.7):
            return '', start, 'damaged'
        candidate, start = suffix, start + first
    return candidate, start, ''


def field_ok(field):
    return bool(field.strip()) and len(field.strip()) <= 180 and not FIELD_DEBRIS.search(field) and not re.search(r'[0-9^?\\<>\u25a0\ufffd]', field)


def classify_entry(field_raw, raw_text_adjusted, name_raw=''):
    head, head_start, failure = heading(raw_text_adjusted, name_raw)
    field_hits = hits(field_raw) if field_ok(field_raw) else []
    head_hits = hits(head) if head else []
    normfield = normalize(field_raw)[0]
    normhead = normalize(head)[0]
    # A plausible recovered label must be supported in the bounded heading.
    # It cannot be a later society name, degree or affiliation.
    supported = bool(normfield) and normfield in normhead
    chosen, evidence_source, offset, selected = '', 'none', 0, []
    if field_hits and supported:
        chosen, evidence_source, selected = field_raw, 'recovered_field', field_hits
        fg = {h['group'] for h in field_hits if h['group'] != 'other'}
        hg = {h['group'] for h in head_hits if h['group'] != 'other'}
        if hg - fg:
            chosen, evidence_source, offset, selected = head, 'heading_completeness_fallback', head_start, head_hits
    elif head_hits:
        chosen, evidence_source, offset, selected = head, 'heading_fallback', head_start, head_hits
    elif field_ok(field_raw) and supported:
        chosen, evidence_source = field_raw, 'recovered_field'
    elif head:
        chosen, evidence_source, offset = head, 'heading_fallback', head_start
    target_groups = {h['group'] for h in selected if h['group'] != 'other'}
    if len(target_groups) == 2:
        group, reason, review = 'other', 'mixed', True
    elif target_groups:
        group, reason, review = next(iter(target_groups)), '', False
    elif any(h['group'] == 'other' for h in selected):
        group, reason, review = 'other', 'non_target', False
    else:
        reason = ('damaged' if failure == 'damaged' or (field_raw and not field_ok(field_raw))
                  else 'unmatched' if chosen else 'missing')
        group, review = 'other', True
    evidence = []
    for h in selected:
        record = dict(h, evidence_source='raw_text_adjusted' if 'heading' in evidence_source else 'field_raw')
        if 'heading' in evidence_source:
            record['start'] += offset; record['end'] += offset
        evidence.append(record)
    return dict(science_group=group, other_reason=reason, review_required=review,
                physical_match='physical_science' in target_groups,
                biological_match='biological_science' in target_groups,
                input_source=evidence_source, selected_heading=chosen,
                matched_rule_ids=sorted({h['rule_id'] for h in selected}),
                evidence=evidence, rules_version=RULES['version'])

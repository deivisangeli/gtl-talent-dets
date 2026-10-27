"""Version 2: bounded vocabulary expansion and allowlisted OCR normalization.

Original classifier modules and rules are immutable dependencies. Repairs are
applied only to extracted headings/fields; literal text and offsets are retained.
"""
import json
import re
import unicodedata
from pathlib import Path
import classifier as base
from classifier_boundary_guard import GUARD

RULES=json.loads(Path(__file__).with_name('rules_v2.json').read_text(encoding='utf-8'))
VERSION=RULES['version']
COMPILED={family:[(r,re.compile(r['pattern'])) for r in RULES[family]]
          for family in ['specific_rules','general_rules']}


def normalize(text):
    """Return normalized characters, source positions and documented repairs."""
    chars=[]; positions=[]; repairs=[]
    for i,char in enumerate(text):
        expanded=unicodedata.normalize('NFKD',char).upper()
        stripped=''.join(c for c in expanded if not unicodedata.combining(c))
        if stripped!=expanded: repairs.append(dict(kind='accent_removal',start=i,end=i+1,original=char))
        for c in stripped:
            if c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789':
                chars.append(c); positions.append(i)
            elif chars and chars[-1]!=' ':
                chars.append(' '); positions.append(i)
    if chars and chars[-1]==' ': chars.pop(); positions.pop()
    norm=''.join(chars)
    # Only explicit dictionary words can be repaired. At most two internal
    # punctuation/spacing breaks; never join across clause punctuation.
    for word in RULES['ocr_join_words']:
        pattern=r'\b'+r'\s*'.join(re.escape(c) for c in word)+r'\b'
        for m in reversed(list(re.finditer(pattern,norm))):
            if ' ' not in m.group(): continue
            lo,hi=positions[m.start()],positions[m.end()-1]+1
            original=text[lo:hi]
            if len(re.findall(r'[^A-Za-z0-9\u00c0-\u024f]+',original))>2: continue
            if re.search(r'[^A-Za-z0-9\u00c0-\u024f\s\u00ad\-\u2010-\u2019\x27]',original): continue
            indexes=[positions[j] for j in range(m.start(),m.end()) if norm[j]!=' ']
            norm=norm[:m.start()]+word+norm[m.end():]
            positions[m.start():m.end()]=indexes
            repairs.append(dict(kind='allowlisted_internal_join',start=lo,end=hi,original=original,replacement=word))
    for alias,target in RULES['ocr_aliases'].items():
        for m in reversed(list(re.finditer(r'\b'+re.escape(alias)+r'\b',norm))):
            lo,hi=positions[m.start()],positions[m.end()-1]+1
            original_positions=positions[m.start():m.end()]
            norm=norm[:m.start()]+target+norm[m.end():]
            positions[m.start():m.end()]=[original_positions[round(j*(len(alias)-1)/max(1,len(target)-1))] for j in range(len(target))]
            repairs.append(dict(kind='explicit_ocr_alias',start=lo,end=hi,original=text[lo:hi],replacement=target))
    return norm,positions,repairs


def hits(text):
    norm,mapping,repairs=normalize(text); used=[]; found=[]
    for family in ['specific_rules','general_rules']:
        for rule,pattern in COMPILED[family]:
            for m in pattern.finditer(norm):
                if any(m.start()<b and m.end()>a for a,b in used): continue
                start,end=mapping[m.start()],mapping[m.end()-1]+1
                assisted=[r for r in repairs if r['start']<end and r['end']>start]
                for group in rule.get('groups',[rule.get('group')]):
                    found.append(dict(rule_id=rule['id'],group=group,evidence=text[start:end],
                                      start=start,end=end,ocr_repairs=assisted))
                if family=='specific_rules': used.append(m.span())
    return sorted(found,key=lambda h:(h['start'],h['end'],h['rule_id'],h['group']))


def heading(text,name=''):
    stop=base.SECTION.search(text[:600]); end=stop.start() if stop else min(len(text),260)
    region=text[:end]; start=len(name) if name and region.startswith(name) else 0
    birth=re.search(r'(?:^|[,.;\s])b\s+',region[start:],re.I)
    if birth: start+=birth.end()
    dates=list(base.DATE.finditer(region,start))
    if dates: start=dates[-1].end()
    demos=list(base.DEMOGRAPHIC.finditer(region,start))
    if demos: start=demos[-1].end()
    candidate=region[start:].strip(' ,;:.\t\n-')
    start=region.find(candidate,start) if candidate else start
    if not candidate or len(candidate)>180: return '',start,'damaged' if candidate else 'missing'
    recognized=hits(candidate)
    if base.FIELD_DEBRIS.search(candidate):
        if not recognized: return '',start,'damaged'
        first=min(h['start'] for h in recognized); prefix,suffix=candidate[:first],candidate[first:]
        letters=[c for c in suffix if c.isalpha()]
        if (base.FIELD_DEBRIS.search(suffix) or re.search(r'\b(?:PHD|BS|BA|MS|MA|MD|BSC)\b',prefix,re.I)
            or re.search(r'\d',suffix) or sum(c.isupper() for c in letters)/max(1,len(letters))<.7):
            return '',start,'damaged'
        candidate,start=suffix,start+first
    return candidate,start,''


def classify_entry(field_raw,raw_text_adjusted,name_raw=''):
    start=len(name_raw) if name_raw and raw_text_adjusted.startswith(name_raw) else 0
    birth=re.search(r'(?:^|[,.;\s])b\s+',raw_text_adjusted[start:],re.I)
    if birth: start+=birth.end()
    dob=base.DATE.search(raw_text_adjusted,start,min(start+120,len(raw_text_adjusted)))
    if dob: start=dob.end()
    guard=GUARD.search(raw_text_adjusted,start); limit=guard.start() if guard else len(raw_text_adjusted)
    head,head_start,failure=heading(raw_text_adjusted[:limit],name_raw)
    fh=hits(field_raw) if base.field_ok(field_raw) else []; hh=hits(head) if head else []
    nf=normalize(field_raw)[0]; nh=normalize(head)[0]
    supported=bool(nf) and nf in nh
    selected=[]; chosen=''; source='none'; offset=0
    if fh and supported:
        chosen,source,selected=field_raw,'recovered_field',fh
        fg={h['group'] for h in fh if h['group']!='other'}; hg={h['group'] for h in hh if h['group']!='other'}
        if hg-fg: chosen,source,offset,selected=head,'heading_completeness_fallback',head_start,hh
    elif hh: chosen,source,offset,selected=head,'heading_fallback',head_start,hh
    elif base.field_ok(field_raw) and supported: chosen,source=field_raw,'recovered_field'
    elif head: chosen,source,offset=head,'heading_fallback',head_start
    groups={h['group'] for h in selected if h['group']!='other'}
    if len(groups)==2: group,reason,review='other','mixed',True
    elif groups: group,reason,review=next(iter(groups)),'',False
    elif any(h['group']=='other' for h in selected): group,reason,review='other','non_target',False
    else:
        reason='damaged' if failure=='damaged' or (field_raw and not base.field_ok(field_raw)) else 'unmatched' if chosen else 'missing'
        group,review='other',True
    evidence=[]
    for h in selected:
        e=dict(h,evidence_source='raw_text_adjusted' if 'heading' in source else 'field_raw')
        if 'heading' in source:
            e['start']+=offset; e['end']+=offset
            e['ocr_repairs']=[dict(r,start=r['start']+offset,end=r['end']+offset) for r in e['ocr_repairs']]
        evidence.append(e)
    return dict(science_group=group,other_reason=reason,review_required=review,
        physical_match='physical_science' in groups,biological_match='biological_science' in groups,
        input_source=source,selected_heading=chosen,matched_rule_ids=sorted({h['rule_id'] for h in selected}),
        evidence=evidence,rules_version=VERSION,scope_guard_offset=limit,
        scope_guard_trigger=guard.group() if guard else '',ocr_assisted=any(h['ocr_repairs'] for h in evidence))

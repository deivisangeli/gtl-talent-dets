"""Evidence-preserving, clause-based education regex parser for AMWS 1986.

No sample IDs, reference labels, or person-specific corrections are used here.
All offsets refer to the unchanged input string (Python Unicode code points).
"""
import re
import unicodedata

RULE_VERSION = 'v08'
LABELS = {
    'bachelors': 'BA AB BS SB BSc ScB BSA BSEE BSChE BSEd BSE BSME BSEduc BScE BSCE BScEE BSCheE BSPharm'.split(),
    'other_bachelor': 'BEE BE BEng BChE MB BCh BDSc BCE BASc BEd BEP BME BLS BChem BDS ChB BAeroEng BAS BAgrSci BMedS BIE BTech BPharm'.split(),
    'masters_academic': 'MA AM MS SM MSc ScM MPhil MSCE MSEE MSME MSChE MSAg MSA MScTech MScE MSPharm MDSc MScEng MNS MSD MSPH MScA MASc MST'.split(),
    'masters_professional': 'MEng ME MSE MChE MEE EdM MPH MBA MBA-XP MEd MF MPA MLS MLIS MArch MAgr MAg MTech MDiv MTh MCompSci MSW'.split(),
    'phd': ['PhD', 'DPhil', 'PbD', 'PnD', 'fhD', 'rtiD', 'hD'],
    'other_doctorate': 'MD DSc ScD DVM DDS LLD DScEng Dr DMD JD EdD EngScD DrPH LHD DLitt Drhabil DEcon DEE DF'.split(),
    'other_credential': 'AE ChE EE Drs Dipl DiplIng PhG PhC DRE MetE Engr'.split(),
}
LABEL_GROUP = {re.sub(r'[^a-z]', '', s.lower()): g for g, labels in LABELS.items() for s in labels}


def spelling(label):
    return r'[ .-]{0,2}'.join(re.escape(c) for c in label)


DEGREE = re.compile(r'(?<![A-Za-z])(?:' + '|'.join(
    spelling(s) + (r'(?=[fItl][a-z])' if LABEL_GROUP[s]=='phd' else r'(?!)') + '|' + spelling(s) + r'(?![A-Za-z])'
    for s in sorted(LABEL_GROUP, key=lambda x: (-len(x), x))) + r')', re.I)
EDUC = re.compile(r'\b(?:E[\s,]*duc(?=\b|Univ)|(?:Edue|Eduv|Edvc|Edix|EdiK|Edi\.v|Edu|fduv)\b)\s*[:.;,]?', re.I)
STOP = re.compile(r'(?:Pro[f/][\s/]*[E£][xipdv][pvn]?|Prof[\s/]*[EF][xr][np]|Prof\s*Pip|Pnf[\s/]*Eap|Professional\s+Experience|Concurrent\s*Pos|Hon\s*Degrees|\bHonors(?:\s*&\s*Awards)?|\bMem|\bRes(?=\s*[:;])|Mailing\s*Add)\s*[:.;,]?', re.I)
PERSON = re.compile(r'[A-Z][A-Z\x27-]{2,},\s+[A-Z][A-Z ()\x27.-]{2,},\s*b\s', re.M)
YEAR = re.compile(r'(?<![A-Za-z0-9])(?:18\d{2}|19\d{2}|\d{2})(?![A-Za-z0-9])')
INST = re.compile(r'\b(?:Univ(?:ersit[yé]|ersidade|ersite)?|Untv|Uni[\s*]+v?|Uoiv|[CLU]niv|Umv|[CLU][\x27’]niv|Col\w*|Co1|Inst\w*|Sch\w*|Tech\w*|Poly\w*|Acad\w*|Ecole|ETH|MIT|Oxford|Cambridge|Harvard|Yale|Princeton|Cornell|Stanford|Dartmouth|Rutgers|Purdue|McGill|Sorbonne|Caltech|Mt Allison|Wis Madison|Berkeley|Los Angeles)\b', re.I)
TARGET_GROUPS = set(LABELS) - {'other_doctorate', 'other_credential'}


def canon(value):
    return re.sub(r'[^a-z0-9]', '', unicodedata.normalize('NFKC', value or '').lower())


def trim(value, offset):
    left = len(value) - len(value.lstrip(' \t\r\n,;:.&-'))
    value = value[left:].rstrip(' \t\r\n,;:.&-')
    return value, offset + left


def subject_end(text, end, limit, ocr=False):
    opener = r'(?:[ .]*[({]|[fItl](?=[a-z]))' if ocr else r'[ .]*[({]'
    m = re.match(opener + r'[^)>}]{0,100}[)>}]', text[end:limit], re.I)
    return end + m.end() if m else end


def institution(value, offset):
    # Semicolons separate schools/credentials. Location abbreviations remain.
    if ';' in value:
        n = value.rfind(';') + 1
        value, offset = value[n:], offset+n
    value, offset = trim(value, offset)
    # Remove numeric attendance ranges before the next qualification.
    stop = re.search(r'[, .]+\d{2}(?:-\d{2})?\s*$', value)
    if stop:
        value = value[:stop.start()].rstrip(' ,.;')
    if not INST.search(value):
        return '', None
    if re.fullmatch(r'[A-Za-z]\s+Univ',value,re.I):
        return '',None
    # Do not recover an institution from arbitrary corrupt text merely because
    # it contains a surviving Univ token.
    if re.search(r'[<=^�]|\b(?:from|prof|res)\b', value, re.I):
        return '', None
    return value, offset


def year_match(tail):
    # Only the award's immediate date is eligible; never hunt into employment.
    m = re.match(r"^[\s,.;:'?\-]*(?:Hons[\s,.;]*)?(?:f\s+)?(?P<year>18\d{2}|19\d{2}|\d[ ]\d|\d[oO]|\d{2}|\d)(?!\d)", tail)
    return m


def resolve_year(raw, birth_year=None):
    raw = (raw or '').replace(' ', '')
    if re.fullmatch(r'\d[oO]', raw):
        raw = raw[0] + '0'
    if not raw or not raw.isdigit() or len(raw) not in (2,4):
        return None
    n = int(raw)
    if len(raw) == 4:
        return n if 1800 <= n <= 1986 else None
    # Birth year is only a constraint when it is a plausible parsed year;
    # callers additionally carry the chronology flag into their review queue.
    candidates = [c + n for c in (1800, 1900) if c + n <= 1986]
    if birth_year is not None and 1800 <= birth_year <= 1986:
        candidates = [y for y in candidates if y >= birth_year]
    return candidates[0] if len(candidates) == 1 else None


def extract(text, birth_year=None):
    """Return awards plus a record status; honorary sections are excluded."""
    start = EDUC.search(text)
    if not start:
        start = re.search(r'\bEducation\s*[:.]',text,re.I)
    if not start:
        return {'status': 'no_match', 'flags': ['education_heading_not_found'], 'degrees': []}
    preceding = text[:start.start()]
    if birth_year is None:
        b = re.search(r'\b(?:Jan|Feb|Mar|Apr|Api|May|June|July|Aug|Sept|Oct|Nov|Dec|Dee|Mur)\.?\s+[\dIlIt]{1,3}\s*[, .]+\s*(\d{2}|18\d{2}|19\d{2})(?!\d)', preceding, re.I)
        if b:
            raw_birth = b.group(1)
            by = int(raw_birth)
            birth_year = by if len(raw_birth)==4 else (1900+by if by<=86 else 1800+by)
    people = list(PERSON.finditer(text))
    # A focal see-previous/deceased stub followed by a second person's Educ
    # must not supply education to the first person.
    if re.search(r'\b(?:see previous edition|deceased)', preceding, re.I):
        return {'status': 'no_match', 'flags': ['cross_reference_or_deceased_before_education'], 'degrees': []}
    if len([p for p in people if p.start() < start.start()]) > 1:
        return {'status': 'ambiguous', 'flags': ['education_belongs_to_later_person'], 'degrees': []}
    end = STOP.search(text, start.end())
    section_end = end.start() if end else len(text)
    later_person = next((p for p in people if start.end() < p.start() < section_end), None)
    if later_person:
        section_end = later_person.start()
    section_start = start.end()
    candidates = list(DEGREE.finditer(text, section_start, section_end))
    matches = []
    blocked_until = section_start
    for k, m in enumerate(candidates):
        if m.start() < blocked_until:
            continue
        token_end = subject_end(text, m.end(), section_end, LABEL_GROUP[canon(m.group())]=='phd')
        blocked_until = token_end
        key = canon(m.group())
        # State/country abbreviations in university names are not MD/AM awards.
        prefix = text[max(section_start,m.start()-15):m.start()]
        tail = text[m.end():candidates[k+1].start() if k+1<len(candidates) else section_end]
        if key == 'am' and re.match(r'\s+(?:Bd|Col|Soc|Univ)\b', tail, re.I):
            continue
        if key == 'med' and not year_match(tail) and not re.match(r'\s*\(',tail):
            continue
        if key == 'msw' and m.group().replace('.','').replace(' ','').lower() != m.group().replace('.','').lower():
            continue
        if key in ('md','am','ma','me','be','med') and not year_match(tail) and (
                re.search(r'\b(?:Univ|Col)\s*$',prefix,re.I) or INST.search(tail)):
            continue
        matches.append(m)
    out, inst, inst_start, flags = [], '', None, []
    previous_tail = section_start
    for k, m in enumerate(matches):
        label = m.group()
        key = canon(label)
        group = LABEL_GROUP[key]
        next_start = matches[k+1].start() if k+1 < len(matches) else section_end
        prefix, prefix_start = trim(text[previous_tail:m.start()], previous_tail)
        # What remains between awards is either a new institution or a joint
        # conjunction. Do not treat a damaged subject/year as an institution.
        if prefix and INST.search(prefix):
            inst, inst_start = institution(prefix, prefix_start)
        elif prefix and re.search(r'[A-Za-z]', prefix) and not re.fullmatch(r'(?:Sc|St|&|and)',prefix,re.I):
            inst, inst_start = '', None
        token_end = subject_end(text, m.end(), next_start, group=='phd')
        tail = text[token_end:next_start]
        ym = year_match(tail)
        year_raw = ym.group('year') if ym else ''
        year_end = token_end + ym.end() if ym else token_end
        # Degree-first forms can place a recognizable institution before date.
        if not ym and INST.search(tail) and not re.search(r'\b(?:prof|res|asst)\b',tail,re.I):
            candidate = YEAR.search(tail)
            if candidate:
                before_year, by_start = trim(tail[:candidate.start()], token_end)
                maybe_inst, maybe_start = institution(before_year, by_start)
                if maybe_inst:
                    inst, inst_start = maybe_inst, maybe_start
                    year_raw, year_end = candidate.group(), token_end+candidate.end()
        # Medical MB BS is one professional bachelor qualification.
        if key == 'bs' and k and canon(matches[k-1].group()) == 'mb':
            group = 'other_bachelor'
        if group in LABELS:
            evidence_start = min(m.start(), inst_start) if inst_start is not None else m.start()
            evidence_end = max(m.end(), year_end)
            year = resolve_year(year_raw, birth_year)
            degree_flags = []
            if not inst:
                degree_flags.append('institution_unresolved')
            if not year_raw:
                degree_flags.append('year_missing_or_corrupt')
            elif year is None:
                degree_flags.append('year_century_or_chronology_unresolved')
            if year is not None and birth_year and year < birth_year:
                degree_flags.append('chronology_conflict')
            out.append(dict(group=group, label_as_written=label,
                            institution_as_written=inst, year_raw=year_raw,
                            year=year, honorary=False, evidence=text[evidence_start:evidence_end],
                            evidence_source='raw_text_adjusted', start=evidence_start, end=evidence_end,
                            label_start=m.start(), label_end=m.end(),
                            rule_id='education_clause_' + RULE_VERSION, flags=degree_flags))
            if ym:
                repeat = re.match(r'[\s.,]*(?:&|and|A)[\s.,]+(18\d{2}|19\d{2}|\d{2})(?!\d)', text[year_end:next_start], re.I)
                if repeat:
                    second = dict(out[-1])
                    second['flags'] = list(second['flags'])
                    second['year_raw'] = repeat.group(1)
                    second['year'] = resolve_year(second['year_raw'],birth_year)
                    second['end'] = year_end+repeat.end()
                    second['evidence'] = text[second['start']:second['end']]
                    second['rule_id'] = 'multiple_award_years_' + RULE_VERSION
                    out.append(second)
                    year_end = second['end']
        previous_tail = year_end
    if not out:
        return {'status': 'no_match', 'flags': flags, 'degrees': []}
    # A single year following explicitly conjoined awards applies to each.
    for k in range(len(out)-1):
        a, b = out[k], out[k+1]
        between = text[a['label_end']:b['label_start']]
        joint = re.fullmatch(r'\s*(?:&|and|Sc|St)\s*', between, re.I)
        medical = canon(a['label_as_written'])=='mb' and canon(b['label_as_written']) in ('bs','bch','chb') and re.fullmatch(r'[\s.,;]*',between)
        if a['year'] is None and not a['year_raw'] and (joint or medical) and b['year_raw']:
            a['year_raw'], a['year'] = b['year_raw'], b['year']
            a['end'] = b['end']
            a['evidence'] = text[a['start']:a['end']]
            a['flags'] = [f for f in a['flags'] if f != 'year_missing_or_corrupt']
    out = [d for d in out if d['group'] in TARGET_GROUPS]
    return {'status': 'ambiguous' if any(d['flags'] for d in out) else ('matched' if out else 'no_match'),
            'flags': flags, 'degrees': out}

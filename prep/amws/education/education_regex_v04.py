"""Evidence-preserving, clause-based education regex parser for AMWS editions 1906–1986.

multi_v04 is the frozen multi_v03 grammar (education_regex.py, unchanged)
plus the rules from the unknown-training audit; see POSTSECONDARY.md.
No sample IDs, reference labels, or person-specific corrections are used here.
All offsets refer to the unchanged input string (Python Unicode code points).
"""
import re
import unicodedata

RULE_VERSION = 'multi_v04'
LABELS = {
    'bachelors': 'BScF BSIE BSPhann BJS B8 B8A BA AB BS SB BSc ScB BSA BSEE BSChE BSEd BSE BSME BSEduc BScE BSCE BScEE BSCheE BSPharm'.split(),
    'other_bachelor': 'BL BD BMetE BCerE PhB LitB BPd BMus LLB BEE BE BEng BChE MB BCh BDSc BCE BASc BEd BEP BME BLS BChem BDS ChB BAeroEng BAS BAgrSci BMedS BIE BTech BPharm'.split(),
    'masters_academic': 'MPh M8 MjS PhM MA AM MS SM MSc ScM MPhil MSCE MSEE MSME MSChE MSAg MSA MScTech MScE MSPharm MDSc MScEng MNS MSD MSPH MScA MASc MST'.split(),
    'masters_professional': 'MSEd MFS ML CM LLM MCE MTechnol MPharm MEng ME MSE MChE MEE EdM MPH MBA MBA-XP MEd MF MPA MLS MLIS MArch MAgr MAg MTech MDiv MTh MCompSci MSW'.split(),
    'phd': ['PhJD','PhJ','PkD','PhD', 'DPhil', 'PbD', 'PnD', 'fhD', 'rtiD', 'hD'],
    'other_doctorate': 'DVS PharmD PharD DrSc MD DSc ScD DVM DDS LLD DScEng Dr DMD JD EdD EngScD DrPH LHD DLitt Drhabil DEcon DEE DF'.split(),
    'other_credential': 'EM EMet CE CerE AE ChE EE Drs Dipl DiplIng PhG PhC DRE MetE Engr'.split(),
}
LABEL_GROUP = {re.sub(r'[^a-z0-9]', '', s.lower()): g for g, labels in LABELS.items() for s in labels}


def spelling(label):
    return r'[ .-]{0,2}'.join(re.escape(c) for c in label)


def build_degree(label_group):
    # A lowercase marriage field fused with the next label ('m. B.S', 'm 83')
    # is skipped so the scan resumes at the real label.
    return re.compile(r'(?<![A-Za-z])(?!(?-i:m[ .]+[A-Z8]))(?:' + '|'.join(
        spelling(s) + (r'(?=[fItl][a-z])' if label_group[s]=='phd' else r'(?!)') + '|' + spelling(s) + r'(?![A-Za-z])'
        for s in sorted(label_group, key=lambda x: (-len(x), x))) + r')', re.I)


DEGREE = build_degree(LABEL_GROUP)
EDUC = re.compile(r'\b(?:E[\s,]*duc(?=\b|Univ)|(?:Edue|Eduv|Edvc|Edix|EdiK|Edi\.v|Edu|fduv)\b)\s*[:.;,]?', re.I)
STOP = re.compile(r'(?:Pro[f/][\s/]*[E\u00a3][xipdv][pvn]?|Prof[\s/]*[EF][xr][np]|Prof\s*Pip|Pnf[\s/]*Eap|Professional\s+Experience|Concurrent\s*Pos|Hon\s*Degrees|\bHonors(?:\s*&\s*Awards)?|\bMem|\bRes(?=\s*[:;])|Mailing\s*Add|(?-i:Pro[f/][\s/]*[A-Z\u00a3][.]?[ ]?[a-z]{1,3}\s*[:;]))\s*[:.;,]?', re.I)
PERSON = re.compile(r'[A-Z][A-Z\x27-]{2,},\s+[A-Z][A-Z ()\x27.-]{2,},\s*b\s', re.M)
YEAR = re.compile(r'(?<![A-Za-z0-9])(?:18\d{2}|19\d{2}|\d{2})(?![A-Za-z0-9])')
INST = re.compile(r'\b(?:Univ(?:ersit[y\u00e9]|ersidade|ersite)?|Untv|Uni[\s*]+v?|Uoiv|[CLU]niv|Umv|[CLU][\x27\u2019]niv|Uni[sy]|Uniiv|Univy|Un[il]x|[Ll][ ]?nix|[Ll][ ]?mv|Lniv|[Il]n[sx]t\w*|T[ce]{2}hn\w*|Col\w*|Co1|Inst\w*|Sch\w*|Tech\w*|Poly\w*|Acad\w*|Ecole|ETH|MIT|Oxford|Cambridge|Harvard|Yale|Princeton|Cornell|Stanford|Dartmouth|Rutgers|Purdue|McGill|Sorbonne|Caltech|The Citadel|Un|Uni|Mt Allison|Wis Madison|Berkeley|Los Angeles)\b', re.I)
TARGET_GROUPS = set(LABELS) - {'other_doctorate', 'other_credential'}
# OCR-damaged 1986 headings ('E due:', 'EdIC:') and a heading fused to the
# first school ('Educ111 Col'). Consulted only when EDUC finds nothing.
EDUC_OCR = re.compile(r'\b[EF][ .]?[dl][ ]?[uUiIl1]{0,2}[ ]?[cCeKk][ ]?[:;]|\bEduc(?=[0-9A-Z])')
# Unambiguous career/membership sections; a heading after one is not the heading.
CAREER = re.compile(r'Pro[f/][\s/]*[E\u00a3][xipdv][pvn]?\s*[:;]|Prof[\s/]*[EF][xr][np]\s*[:;]|Professional\s+Experience|Concurrent\s*Pos|\bMem\s*[:;]|Mailing\s*Add', re.I)
# The birth date closes the 1938/1955 name, title and address header.
BIRTH = re.compile(r'\b(?:Jan|Feb|Mar|March|Apr|April|May|June|July|Aug|Sept?|Oct|Nov|Dec)[a-z]*[.,]?\s*\d{1,2}\s*[,.]\s*\d{2}(?!\d)')
# 1986 header ' b Town, St, Mon d, yy;' and the capitalised specialty after it.
BIRTH86 = re.compile(r'\bb\s?[A-Z][^;]{0,80}?\b\d{2}(?!\d)\s*[;:]')
CAPS_RUN = re.compile(r'\b[A-Z][A-Z&,\- ]{3,}[A-Z][.]?(?=\s|$)')
# Specialty-board and society diplomas are licences, not training.
BOARD = re.compile(r'\b(?:Bd|Bds|Board)\b|^\s*(?:Am|Nat|Int|Royal|Europ)\b[^;]*\b(?:Acad|Col|Soc|Asn|Inst|Coun|Cong)\b')
FALLBACK_STOP = re.compile(r'(?<![\d-])\d{2}\s*-\s*\d{2}(?!\d)|\bengr\b', re.I)
GENERIC_CREDENTIALS = {'dipl', 'diploma', 'cert', 'certificate'}
ROLE = r'(?:fellow|feUow|feliow|fallow|fel-\s*[’\x27]?\s*low|scholar|grant|student|bursar)'


def canon(value):
    return re.sub(r'[^a-z0-9]', '', unicodedata.normalize('NFKC', value or '').lower())


def trim(value, offset):
    left = len(value) - len(value.lstrip(' \t\r\n,;:.&-'))
    value = value[left:].rstrip(' \t\r\n,;:.&-')
    return value, offset + left


def subject_end(text, end, limit, ocr=False):
    opener = r'[ .,)>}\]]*(?:[({]|[fItl](?=[a-z]))' if ocr else r'[ .,]*[({]'
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
    if re.search(r'[<=^\ufffd]|\b(?:from|prof|res)\b', value, re.I):
        return '', None
    return value, offset


def year_match(tail):
    # Only the award's immediate date is eligible; never hunt into employment.
    m = re.match(r"^[\s,.;:\x27\u2019?*\-]*(?:Hons[\s,.;]*)?(?:f\s+)?(?P<year>18\d{2}|19\d{2}|\d[ ]\d|\d[oO]|\d{2}|\d)(?!\d)", tail)
    return m


def resolve_year(raw, birth_year=None, edition=1986, preceding=None):
    raw = (raw or '').replace(' ', '')
    if re.fullmatch(r'\d[oO]',raw): raw=raw[0]+'0'
    if not raw.isdigit() or len(raw) not in (2,4): return None
    n=int(raw)
    if len(raw)==4: return n if 1800<=n<=edition else None
    else: candidates=[c+n for c in (1800,1900) if c+n<=edition]
    if birth_year is not None and 1700<=birth_year<=edition:
        candidates=[y for y in candidates if y>=birth_year]
    if preceding is not None: candidates=[y for y in candidates if y>=preceding]
    return candidates[0] if len(candidates)==1 else None


EARLY_STOP = re.compile(r"\b(?:[Il]nstr(?:uctor)?|Engineer|Physicist|Intern|Teacher|Teaching|Prof(?:essor)?|Asst|Assistant|Assoc|Lecturer|Supt|Superintendent|Veterinarian|Director|Demonstrator|Private\s+asst|From\s+asst|From\s+instr|Chemist|Research\s+(?:chemist|physicist|asst)|Consulting\s+(?:chemist|engineer)|With|Geologist|Librarian|Develop[., ]+engineer|Field[ ]+geologist|Tech[., ]+staff|Research[ ]+(?:engineer|assoc|proj)|Staff[ ]+mem|Test[ ]+dept)\b", re.I)
EARLY_PERSON = re.compile(r"(?:[A-Z][a-z]+,\s+(?:Dr[.]|Prof[.]|Col[.]|Miss|Mrs[.])?\s*[A-Z][a-z(]+.*?(?:SOILS|BIOLOGY|CHEMISTRY|SPEECH|PHYSICS)[.])")
HON_SECTION = re.compile(r'\bHon(?:orary)?[ .]*Degrees[ .]*[:;]?',re.I)
HON_INLINE = re.compile(r'\bhon[.]?\s*$',re.I)


def school(value, offset, early=False):
    if ';' in value:
        j=value.rfind(';')+1;value,offset=value[j:],offset+j
    value,offset=trim(value,offset)
    role=list(re.finditer(r'\b'+ROLE+r'\s*[, .]+',value,re.I))
    if role:
        # Last role only changes school when followed by a school, not a date.
        for r in reversed(role):
            rest,ro=trim(value[r.end():],offset+r.end())
            if rest and re.match(r'[A-Za-z]',rest):value,offset=rest,ro;break
    value=re.sub(r'^[, .]*(?:hon[.]?|Hons)[, .]*','',value,flags=re.I)
    yr=re.search(r'(?<![A-Za-z0-9])(?:18\d{2}|19\d{2}|\d{2}|\d)(?!\d)',value)
    if yr and yr.start()>0:value=value[:yr.start()].rstrip(' ,.;:-')
    value,offset=trim(value,offset)
    if not value or not re.search('[A-Za-z]',value):return '',None
    if re.search(r'\b(?:fellow|feUow|feliow|fallow|scholar|research|asst|instr|prof|teacher)\b',value,re.I):return '',None
    if re.fullmatch(r'(?:Univ|Uni|Col|Inst|Sc|St|ft|and)',value,re.I):return '',None
    if re.search(r'[<=^\ufffd]',value):return '',None
    if not early and not INST.search(value):return '',None
    # A one- or two-letter bare school is a damaged label ('Mi.35'), not a school.
    if early and len(canon(value))<=2:return '',None
    if early and (len(value)>100 or re.search(r'\b(?:Jan|Feb|March|April|Sept|Nov|Dec)\b',value,re.I)):return '',None
    return value,offset


PRECEDING = re.compile(r'(?:^|(?<=[\s;.]))(?P<tok>[A-Z][A-Za-z0-9«».\x27]{0,4})[, ]+(?P<school>[A-Z][^,;.0-9]{1,60}?)[,. ]+\d{2}(?!\d)\s*[,;.]\s*$')


PRECEDING_ROLE = re.compile(r'\b'+ROLE+r'[, .]+(?P<school>(?-i:[A-Z])[^,;.0-9]{1,60}?)[,. ]+\d{2}(?:\s*-\s*\d{2})?(?!\d)\s*[,;.]\s*$', re.I)


def preceding_school(text,lo,hi):
    """School just before the first readable label: that of an unreadable
    label (a short or non-alphabetic token), or of a fellowship held there
    ('fellow, Ohio State, 41, Ph.D, 41')."""
    m=PRECEDING_ROLE.search(text[lo:hi])
    if m:return school(m.group('school'),lo+m.start('school'),True)
    m=PRECEDING.search(text[lo:hi])
    if not m:return '',None
    tok=m.group('tok')
    if len(tok)>=4 and tok.isalpha():return '',None
    if re.fullmatch(r'(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)[.]?',tok,re.I):return '',None
    return school(m.group('school'),lo+m.start('school'),True)


def invalid_label(text,m):
    k=canon(m.group());tail=text[m.end():m.end()+35]
    if k=='dr':return True
    # Mixed-case undotted Am/Md abbreviate American and Maryland unless a
    # single award year follows ('Harvard Univ, AB, 48, Am, 50').
    if m.group() in ('Am','Md') and not re.match(r'[ ,.]+\d{2}(?!\d)(?!\s*-)',text[m.end():m.end()+8]):return True
    if k in ('ml','cm') and m.group().islower():return True
    if k=='bd' and (re.search(r'\bAm[ .]*$',text[max(0,m.start()-8):m.start()],re.I) or re.match(r'[ .]*(?:Clin|Internal|Path|Prev|Psychiat)',tail,re.I)):return True
    if k=='march' and m.group() in ('March','MARCH'):return True
    if k in ('md','me','ma') and re.match(r'[ ,.]+(?:Jan|Feb|Mar|Apr|May|June|July|Aug|Sept|Oct|Nov|Dec)',tail,re.I):return True
    if k in ('am','ma') and re.match(r'[ .]*(?:A[.;]|Soc|Asn|Ass|Acad|Bd|Col|Univ|Inst|Med|fellow|Water|Geog|Hist|Smelting|Trading|Lithograph)',tail,re.I):return True
    return False

def header_end(text,edition):
    """End of the 1938/1955 name, title and address header (0 when absent):
    the 1955 '@SPECIALTY@' marker, else a birth date near the start."""
    if edition==1955 and '@' in text[:400]:
        at=text.index('@')
        # A label followed by 'School, yy' before the marker is the tail of a
        # merged preceding entry, not header text.
        for m in DEGREE.finditer(text,0,at):
            if re.match(r'[ ,.]+[A-Z][^,;0-9@]{1,50}[,.]\s*\d{2}(?!\d)',text[m.end():at]):return 0
        return at
    if edition not in (1938,1955):return 0
    b=BIRTH.search(text,0,120 if edition==1938 else 300)
    return b.end() if b else 0


def ocr_heading(text):
    """A damaged 1986 Educ heading, accepted only before the first career section."""
    h=EDUC_OCR.search(text)
    if not h:return None
    st=STOP.search(text)
    return h if not st or h.start()<st.start() else None


def unheaded_1986(text):
    """Education clause of a 1986 entry whose heading is lost: after the birth
    line and the capitalised specialty, starting at the first degree label and
    only when that label precedes the first career or membership section."""
    b=BIRTH86.search(text)
    floor=b.end() if b else 0
    degrees=[m for m in DEGREE.finditer(text,floor) if not invalid_label(text,m)]
    if not degrees:return None
    d0=degrees[0]
    st=STOP.search(text,floor)
    if st and st.start()<d0.start():return None
    if not st and d0.start()-floor>300:return None
    caps=list(CAPS_RUN.finditer(text,floor,d0.start()))
    start=caps[-1].end() if caps else floor
    # Residue of the damaged heading itself ('ATICSEduc:', 'EdIC.Univ Ga').
    colon=text.rfind(':',start,d0.start())
    if colon>=0:start=colon+1
    m=re.match(r'\s*[EF][ .]?[dl][A-Za-z]{0,3}(?:[.:;,]\s*|\s+)',text[start:d0.start()])
    return start+m.end() if m else start


def bounds(text,edition):
    flags=[]
    heading=(EDUC.search(text) or re.search(r'\bEducation\s*[:.]',text,re.I)) if edition==1986 else None
    if edition==1986 and heading:
        # 'MED EDUC, 60' or 'Soc Environ Educ:' inside the career or membership
        # text is not the heading; prefer a damaged heading or an unheaded
        # clause before that section when one exists.
        late=CAREER.search(text,0,heading.start())
        st=STOP.search(text,heading.end())
        empty=not any(not invalid_label(text,m) for m in DEGREE.finditer(text,heading.end(),st.start() if st else len(text)))
        if late and empty:
            alt=ocr_heading(text[:late.start()])
            if alt:heading,flags=alt,['education_heading_ocr']
            elif unheaded_1986(text[:late.start()]) is not None:heading=None
    elif edition==1986:
        heading=ocr_heading(text)
        if heading:flags=['education_heading_ocr']
    if heading:
        if re.search(r'\b(?:see previous edition|deceased)',text[:heading.start()],re.I):return None,['cross_reference_or_deceased_before_education']
        first=heading.end();scan=first
    elif edition==1986:
        if re.search(r'\b(?:see previous edition|deceased)',text,re.I):return None,['education_heading_not_found']
        first=unheaded_1986(text)
        if first is None:return None,['education_heading_not_found']
        scan=first;flags=['education_heading_inferred']
    else:
        if re.search(r'\b(?:see previous edition|deceased)',text,re.I):return None,['cross_reference_or_deceased']
        floor=header_end(text,edition)
        degrees=[m for m in DEGREE.finditer(text) if m.start()>=floor and not invalid_label(text,m)]
        if not degrees:return None,[]
        scan=degrees[0].start()
        first=0 if edition==1906 else scan
        career=EARLY_STOP.search(text,0,scan) if edition==1906 else None
        if career:return None,['degree_candidate_after_career_transition']
    finish=len(text)
    st=STOP.search(text,scan)
    while st and HON_SECTION.match(text,st.start()):st=STOP.search(text,st.end())
    if st:finish=st.start()
    if edition!=1986 or flags:
        st=EARLY_STOP.search(text,scan)
        if st:finish=min(finish,st.start())
    if flags:
        # Recovered 1986 sections end at the first career date range or title
        # too, since a damaged heading often comes with a damaged 'Prof Exp'.
        st=FALLBACK_STOP.search(text,scan)
        if st:finish=min(finish,st.start())
    ps=[p for p in PERSON.finditer(text) if p.start()>scan]
    if ps:finish=min(finish,ps[0].start())
    ps=list(EARLY_PERSON.finditer(text,scan,finish))
    if ps:finish=min(finish,ps[0].start())
    return (first,finish),flags

def extract(text,birth_year=None,edition=1986):
    """Extract literal qualifications using edition-specific biography grammar."""
    region,flags=bounds(text,edition)
    if region is None:return dict(status='no_match',flags=flags,degrees=[])
    first,finish=region
    candidates=list(DEGREE.finditer(text,first,finish));matches=[];blocked=first
    for j,m in enumerate(candidates):
        if m.start()<blocked or invalid_label(text,m):continue
        key=canon(m.group());nextpos=candidates[j+1].start() if j+1<len(candidates) else finish
        tail=text[m.end():nextpos];prefix=text[max(first,m.start()-15):m.start()]
        # Country/state and society abbreviations embedded in school names.
        if key in ('md','ma','am','me','be') and (re.search(r'\b(?:Univ|Col)\s*$',prefix,re.I) or re.match(r'\s+(?:Univ|Col|Soc|Bd|Inst)\b',tail,re.I)) and not year_match(tail):continue
        if key=='med' and not year_match(tail) and not re.match(r'\s*[(]',tail):continue
        blocked=subject_end(text,m.end(),finish,LABEL_GROUP[key]=='phd')
        matches.append(m)
    out=[];inst='';inst_start=None;previous=first;last_earned_year=None
    for j,m in enumerate(matches):
        nextpos=matches[j+1].start() if j+1<len(matches) else finish
        key=canon(m.group());group=LABEL_GROUP[key]
        if key=='mph' and re.fullmatch(r'M[ .-]+Ph',m.group(),re.I):group='masters_academic'
        token_end=subject_end(text,m.end(),nextpos,group=='phd')
        prefix,prefix_start=trim(text[previous:m.start()],previous)
        prefix=re.sub(r'\bHon(?:orary)?[ .]*Degrees[ .]*[:;]?','',prefix,flags=re.I)
        honorary=bool(HON_INLINE.search(text[max(0,m.start()-12):m.start()]))
        hs=list(HON_SECTION.finditer(text,first,m.start()))
        if hs:honorary=True
        # A genuine new school resets carryover. Bare schools are permitted in
        # early editions, whose standard grammar is degree,school,date.
        pinst,pstart=school(prefix,prefix_start,edition!=1986)
        if pinst:inst,inst_start=pinst,pstart
        elif re.search(r'\b'+ROLE+r'\b',prefix,re.I):
            pass
        elif prefix and not re.fullmatch(r'[ ,.;:&-]*(?:and|Sc|St|ft|hon[.]?)?[ ,.;:&-]*',prefix,re.I):
            if not re.fullmatch(r'[ ,.;-]*\d{2}(?:-\s*\d{2})?[ ,.;-]*',prefix):inst,inst_start='',None
        tail=text[token_end:nextpos];ym=year_match(tail)
        yr=ym.group('year') if ym else '';end=token_end+ym.end() if ym else token_end
        if not ym:
            bounded_tail=tail.split(';',1)[0]
            dt=YEAR.search(bounded_tail)
            if dt:
                ti,ts=school(tail[:dt.start()],token_end,edition!=1986)
                if ti:inst,inst_start=ti,ts;yr=dt.group();end=token_end+dt.end()
            else:
                # An institution with no printed date still has a recoverable
                # school. Never include professional transitions in that tail.
                ti,ts=school(tail.split(';',1)[0],token_end,edition!=1986)
                if ti and not re.search(r'\b(?:fellow|scholar|asst|prof|research)\b',tail,re.I):inst,inst_start=ti,ts;end=nextpos
        df=[]
        if not inst and j==0 and edition!=1986:
            # 'B«S, California, 34, M.S, 36' and 'fellow, Ohio State, 41, Ph.D':
            # the first readable label shares the school of the clause before it.
            ti,ts=preceding_school(text,max(header_end(text,edition),m.start()-80),m.start())
            if ti:inst,inst_start=ti,ts;df.append('institution_from_preceding_clause')
        year=resolve_year(yr,birth_year,edition,last_earned_year if birth_year is None and not honorary else None)
        if not honorary and year is not None:last_earned_year=year
        if key=='bs' and j and canon(matches[j-1].group())=='mb':group='other_bachelor'
        start=min(m.start(),inst_start) if inst_start is not None else m.start()
        end=max(m.end(),end)
        if key in GENERIC_CREDENTIALS:
            cut=max(text.rfind(';',first,m.start()),text.rfind(':',first,m.start()))
            if BOARD.search(text[max(first,cut+1):m.start()]):df.append('board_certification')
        if year is not None and birth_year is not None and year<birth_year:df.append('chronology_conflict')
        if not inst:df.append('institution_unresolved')
        if not yr:df.append('year_missing_or_corrupt')
        elif year is None:df.append('year_century_or_chronology_unresolved')
        out.append(dict(group=group,label_as_written=m.group(),institution_as_written=inst or None,
            year_raw=yr or None,year=year,honorary=honorary,evidence=text[start:end],evidence_source='raw_text_adjusted',start=start,end=end,
            label_start=m.start(),label_end=m.end(),rule_id='edition_clause_'+RULE_VERSION,flags=df))
        previous=end
    # Adjacent labels separated only by punctuation/conjunction share the
    # trailing school and date (M.S,Ph.D,Princeton,16; SB & SM,67), also after
    # a parenthesised subject (B.S.(chem. eng) & B.S.(chem), Alpha, 49).
    for j in range(len(out)-2,-1,-1):
        left,right=out[j],out[j+1]
        between=text[left['label_end']:right['label_start']]
        if re.fullmatch(r'[ ,.;:&-]*(?:[({][^)}]{0,60}[)}])?[ ,.;:&-]*(?:and|Sc|St|ft)?[ ,.;:&-]*',between,re.I):
            if right['institution_as_written'] is not None and left['institution_as_written'] is None:
                left['institution_as_written']=right['institution_as_written'];left['end']=right['end'];left['evidence']=text[left['start']:left['end']];left['flags']=[f for f in left['flags']if f!='institution_unresolved']
            if left['year_raw'] is None and right['year_raw'] is not None:
                left['year_raw']=right['year_raw'];left['year']=right['year']
                if right['institution_as_written'] is not None:left['institution_as_written']=right['institution_as_written']
                left['start']=min(left['start'],right['start']);left['end']=right['end'];left['evidence']=text[left['start']:left['end']]
                left['flags']=[f for f in left['flags']if f not in ('year_missing_or_corrupt','institution_unresolved')]
    return dict(status='ambiguous' if any(d['flags']for d in out)else('matched'if out else'no_match'),flags=flags,degrees=out)

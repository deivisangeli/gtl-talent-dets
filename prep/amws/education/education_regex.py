"""Evidence-preserving, clause-based education regex parser for AMWS editions 1906–1986.

No sample IDs, reference labels, or person-specific corrections are used here.
All offsets refer to the unchanged input string (Python Unicode code points).
"""
import re
import unicodedata

RULE_VERSION = 'multi_v03'
LABELS = {
    'bachelors': 'BScF BSIE BSPhann BJS B8 B8A BA AB BS SB BSc ScB BSA BSEE BSChE BSEd BSE BSME BSEduc BScE BSCE BScEE BSCheE BSPharm'.split(),
    'other_bachelor': 'BL BD BMetE BCerE PhB LitB BPd BMus LLB BEE BE BEng BChE MB BCh BDSc BCE BASc BEd BEP BME BLS BChem BDS ChB BAeroEng BAS BAgrSci BMedS BIE BTech BPharm'.split(),
    'masters_academic': 'MPh M8 MjS PhM MA AM MS SM MSc ScM MPhil MSCE MSEE MSME MSChE MSAg MSA MScTech MScE MSPharm MDSc MScEng MNS MSD MSPH MScA MASc MST'.split(),
    'masters_professional': 'MSEd MFS ML CM LLM MCE MTechnol MPharm MEng ME MSE MChE MEE EdM MPH MBA MBA-XP MEd MF MPA MLS MLIS MArch MAgr MAg MTech MDiv MTh MCompSci MSW'.split(),
    'phd': ['PhJD','PhJ','PkD','PhD', 'DPhil', 'PbD', 'PnD', 'fhD', 'rtiD', 'hD'],
    'other_doctorate': 'DVS PharD DrSc MD DSc ScD DVM DDS LLD DScEng Dr DMD JD EdD EngScD DrPH LHD DLitt Drhabil DEcon DEE DF'.split(),
    'other_credential': 'EM CE CerE AE ChE EE Drs Dipl DiplIng PhG PhC DRE MetE Engr'.split(),
}
LABEL_GROUP = {re.sub(r'[^a-z0-9]', '', s.lower()): g for g, labels in LABELS.items() for s in labels}


def spelling(label):
    return r'[ .-]{0,2}'.join(re.escape(c) for c in label)


DEGREE = re.compile(r'(?<![A-Za-z])(?:' + '|'.join(
    spelling(s) + (r'(?=[fItl][a-z])' if LABEL_GROUP[s]=='phd' else r'(?!)') + '|' + spelling(s) + r'(?![A-Za-z])'
    for s in sorted(LABEL_GROUP, key=lambda x: (-len(x), x))) + r')', re.I)
EDUC = re.compile(r'\b(?:E[\s,]*duc(?=\b|Univ)|(?:Edue|Eduv|Edvc|Edix|EdiK|Edi\.v|Edu|fduv)\b)\s*[:.;,]?', re.I)
STOP = re.compile(r'(?:Pro[f/][\s/]*[E\u00a3][xipdv][pvn]?|Prof[\s/]*[EF][xr][np]|Prof\s*Pip|Pnf[\s/]*Eap|Professional\s+Experience|Concurrent\s*Pos|Hon\s*Degrees|\bHonors(?:\s*&\s*Awards)?|\bMem|\bRes(?=\s*[:;])|Mailing\s*Add)\s*[:.;,]?', re.I)
PERSON = re.compile(r'[A-Z][A-Z\x27-]{2,},\s+[A-Z][A-Z ()\x27.-]{2,},\s*b\s', re.M)
YEAR = re.compile(r'(?<![A-Za-z0-9])(?:18\d{2}|19\d{2}|\d{2})(?![A-Za-z0-9])')
INST = re.compile(r'\b(?:Univ(?:ersit[y\u00e9]|ersidade|ersite)?|Untv|Uni[\s*]+v?|Uoiv|[CLU]niv|Umv|[CLU][\x27\u2019]niv|Col\w*|Co1|Inst\w*|Sch\w*|Tech\w*|Poly\w*|Acad\w*|Ecole|ETH|MIT|Oxford|Cambridge|Harvard|Yale|Princeton|Cornell|Stanford|Dartmouth|Rutgers|Purdue|McGill|Sorbonne|Caltech|The Citadel|Un|Uni|Mt Allison|Wis Madison|Berkeley|Los Angeles)\b', re.I)
TARGET_GROUPS = set(LABELS) - {'other_doctorate', 'other_credential'}


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
    role=list(re.finditer(r'\b(?:fellow|feUow|fallow|fel-\s*[’\x27]?\s*low|scholar|grant|student|bursar)\s*[, .]+',value,re.I))
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
    if re.search(r'\b(?:fellow|feUow|fallow|scholar|research|asst|instr|prof|teacher)\b',value,re.I):return '',None
    if re.fullmatch(r'(?:Univ|Uni|Col|Inst|Sc|St|ft|and)',value,re.I):return '',None
    if re.search(r'[<=^\ufffd]',value):return '',None
    if not early and not INST.search(value):return '',None
    if early and (len(value)>100 or re.search(r'\b(?:Jan|Feb|March|April|Sept|Nov|Dec)\b',value,re.I)):return '',None
    return value,offset


def invalid_label(text,m):
    k=canon(m.group());tail=text[m.end():m.end()+35]
    if k=='dr':return True
    if k in ('ml','cm') and m.group().islower():return True
    if k=='bd' and (re.search(r'\bAm[ .]*$',text[max(0,m.start()-8):m.start()],re.I) or re.match(r'[ .]*(?:Clin|Internal|Path|Prev|Psychiat)',tail,re.I)):return True
    if k=='march' and m.group() in ('March','MARCH'):return True
    if k in ('md','me','ma') and re.match(r'[ ,.]+(?:Jan|Feb|Mar|Apr|May|June|July|Aug|Sept|Oct|Nov|Dec)',tail,re.I):return True
    if k in ('am','ma') and re.match(r'[ .]*(?:A[.;]|Soc|Asn|Ass|Acad|Bd|Col|Univ|Inst|Med|fellow|Water|Geog|Hist|Smelting|Trading|Lithograph)',tail,re.I):return True
    return False

def bounds(text,edition):
    heading=(EDUC.search(text) or re.search(r'\bEducation\s*[:.]',text,re.I)) if edition==1986 else None
    if edition==1986 and not heading:return None,['education_heading_not_found']
    if heading:
        if re.search(r'\b(?:see previous edition|deceased)',text[:heading.start()],re.I):return None,['cross_reference_or_deceased_before_education']
        first=heading.end();scan=first
    else:
        if re.search(r'\b(?:see previous edition|deceased)',text,re.I):return None,['cross_reference_or_deceased']
        degrees=[m for m in DEGREE.finditer(text) if not invalid_label(text,m)]
        if not degrees:return None,[]
        scan=degrees[0].start()
        first=0 if edition==1906 else scan
        career=EARLY_STOP.search(text,0,scan) if edition==1906 else None
        if career:return None,['degree_candidate_after_career_transition']
    finish=len(text)
    st=STOP.search(text,scan)
    while st and HON_SECTION.match(text,st.start()):st=STOP.search(text,st.end())
    if st:finish=st.start()
    if edition!=1986:
        st=EARLY_STOP.search(text,scan)
        if st:finish=min(finish,st.start())
    ps=[p for p in PERSON.finditer(text) if p.start()>scan]
    if ps:finish=min(finish,ps[0].start())
    ps=list(EARLY_PERSON.finditer(text,scan,finish))
    if ps:finish=min(finish,ps[0].start())
    return (first,finish),[]

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
        elif re.search(r'\b(?:fellow|feUow|fallow|scholar|grant|student|bursar)\b',prefix,re.I):
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
        year=resolve_year(yr,birth_year,edition,last_earned_year if birth_year is None and not honorary else None)
        if not honorary and year is not None:last_earned_year=year
        if key=='bs' and j and canon(matches[j-1].group())=='mb':group='other_bachelor'
        start=min(m.start(),inst_start) if inst_start is not None else m.start()
        end=max(m.end(),end)
        df=[]
        if year is not None and birth_year is not None and year<birth_year:df.append('chronology_conflict')
        if not inst:df.append('institution_unresolved')
        if not yr:df.append('year_missing_or_corrupt')
        elif year is None:df.append('year_century_or_chronology_unresolved')
        out.append(dict(group=group,label_as_written=m.group(),institution_as_written=inst or None,
            year_raw=yr or None,year=year,honorary=honorary,evidence=text[start:end],evidence_source='raw_text_adjusted',start=start,end=end,
            label_start=m.start(),label_end=m.end(),rule_id='edition_clause_'+RULE_VERSION,flags=df))
        previous=end
    # Adjacent labels separated only by punctuation/conjunction share the
    # trailing school and date (M.S,Ph.D,Princeton,16; SB & SM,67).
    for j in range(len(out)-2,-1,-1):
        left,right=out[j],out[j+1]
        between=text[left['label_end']:right['label_start']]
        if re.fullmatch(r'[ ,.;:&-]*(?:and|Sc|St|ft)?[ ,.;:&-]*',between,re.I):
            if right['institution_as_written'] is not None and left['institution_as_written'] is None:
                left['institution_as_written']=right['institution_as_written'];left['end']=right['end'];left['evidence']=text[left['start']:left['end']];left['flags']=[f for f in left['flags']if f!='institution_unresolved']
            if left['year_raw'] is None and right['year_raw'] is not None:
                left['year_raw']=right['year_raw'];left['year']=right['year']
                if right['institution_as_written'] is not None:left['institution_as_written']=right['institution_as_written']
                left['start']=min(left['start'],right['start']);left['end']=right['end'];left['evidence']=text[left['start']:left['end']]
                left['flags']=[f for f in left['flags']if f not in ('year_missing_or_corrupt','institution_unresolved')]
    return dict(status='ambiguous' if any(d['flags']for d in out)else('matched'if out else'no_match'),flags=flags,degrees=out)

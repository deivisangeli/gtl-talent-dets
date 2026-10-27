"""Edition-specific, dictionary-free university employment extraction.

Offsets are Python Unicode codepoints in original fields, end exclusive. The
150-entry exercise is development calibration, never an independent test.
"""
import re
import unicodedata

VERSIONS=('v00_explicit_markers','v01_academic_abbreviations','v02_bounded_carry','v03_core_and_boundaries','v04_conservative_slots','v05_appointment_boundaries','v06_carry_termination','v07_dated_sentence_spans','v08_role_resets','v09_medical_unit_forms')
EDITION_STRATEGIES={
 '1906':dict(header_field='address_text',career_field='biography_text',body='biography column, no header titles',carry='semicolon and dated appointment boundaries'),
 '1938':dict(header_field='address_text',original_header_field='address_legacy',career_field='biography_text',original_career_field='a',body='birth and education precede rank sequences',carry='successive ranks, dated employer clauses'),
 '1955':dict(header_field='biography_text',original_header_field='original',career_field='biography_text',body='full original bounded by specialty markup and original birthplace field',comparison_fields=['address_text','rest_text'],carry='dated clauses within bounded career text')
}
PATTERNS={
 'strong_role':r'\b(?:[Il]nstr(?:uctor)?|prof(?:essor)?|lect(?:urer)?|tutor|teacher|demonstrator|dean|head\s+dept|chairman\s+(?:grad\.?\s+)?dept|teaching\s+(?:asst|assistant|fellow)|teach\s*-\s*lng\s+fellow|student\s+asst|grad\.?[, ]+asst)\b',
 'weak_role':r'\b(?:asst|assistant|assoc|technician|research\s+(?:assoc|asst|assistant|fellow)|lab\.?\s+(?:asst|[Il]nstr)|physicist|pathologist|director|pres|supt|storekeeper)\b',
 'marker':r'\b(?:univ(?:ersity)?|unlv|col(?:lege)?|polytech(?:nic)?|inst(?:itute)?\.?\s+(?:of\s+)?tech(?:nology)?|sch(?:ool)?\.?\s+(?:of\s+)?mines|postgrad(?:uate)?\.?\s+sch(?:ool)?)\b',
 'nonacademic':r'\b(?:high\s*sch|pub(?:lic)?\.?\s+sch|mil\.?\s+sch|schs|academy|acad|trustees?|soc(?:iety)?|asn|assn|association|corp|corpn|company|co|inc|insurance|bur(?:eau)?|U\.?\s*S\.?\s+(?:DEPT|ARMY|NAVY)|nat(?:ional)?\s+(?:lab|inst)|geol\.?\s+surv|govt|engineer|attorney|manufacturing|mfg|chem(?:ical)?\.?\s+works|foundation)\b',
 'subject':r'\b(?:chem|physics|math|biol|bot|zool|geol|surg|hort|path|bacter|physiol|anatomy|anat|med|agr|agron|entom|anthrop|philos|eng|elec|mechanics|science|floricul|pomol|geog|biophys|phys|astron|org|metal|histol|pharmacy|pharmac|blol|biochem|dairy|research)\b',
 'unit':r'\b(?:dept|department|lab|labs|laboratory|exp\.?\s+sta|observatory|observ|hosp|hospital|mus|museum|sch\.?\s+med|med\.?\s+sch|col\.?\s+pharmacy|col\.?\s+arts|research\s+found)\b',
 'date':r'(?<![\w])(?:1[6789]\d{2}|\d{2})(?:\s*[-—–]\s*(?:1[6789]\d{2}|\d{2})?)?(?!\d)',
 'membership_stop':r'(?<!\w)(?:F\.?\s*A\.?\s*A\.?|M\.?\s*A\.?\s*A\.?|A\.\s*A\.?)(?=\W|$)|\b(?:Chem|Physical|Math|Geol|Ceramic|Entom|Philos|Bot|Astron|Phytopath|Dairy\s+Sci)\.?\s+(?:Soc|Asn)|\b(?:Mem|Fel)\.?\s+(?:Col\.?\s+(?:Surg|Physicians)|Geol\.?\s+Soc)|\b(?:Math|Pharmaceut|Physics\s+Teachers)\.?\s+Asn',
 'neighbor_heading':r'(?<=[.!?]\s)(?:[A-Z][a-z]+,\s+(?:Dr|Prof)\.?\s+[A-Z]|[A-Z]{3,},\s+[A-Z][(A-Z])',
 'degree':r'\b(?:Ph\.?\s*D|PhJ|PhJ\)|Ph\.D|M\.?D|B\.?S|A\.?B|A\.?M|M\.?S|M\.?A|LL\.?D|hon\.?\s+degree)\b',
}
RX={k:re.compile(p,re.I if k!='neighbor_heading' else 0) for k,p in PATTERNS.items()}

def norm(text):return re.sub(r'[^\w]+','',unicodedata.normalize('NFKC',text).casefold())
def ev(entry,field,a,b):return dict(field=field,start=a,end=b,text=entry[field][a:b])
def trim(text,a,b):
    while a<b and (text[a].isspace() or text[a] in ',;:'):a+=1
    while b>a and (text[b-1].isspace() or text[b-1] in ',;:'):b-=1
    return a,b

def status(text):
    dates=[m.group() for m in RX['date'].finditer(text)]
    opened=any(re.search(r'[-—–]\s*$',d) for d in dates)
    closed=any(re.search(r'\d\s*[-—–]\s*\d',d) for d in dates)
    if re.search(r'\bretired\b',text,re.I):return 'past'
    if opened:return 'mixed' if closed else 'current'
    return 'past' if dates else 'uncertain'

def appointment_types(text):
    out=[]
    for pattern,label in [(r'\bvisiting|\bvis\b','visiting'),(r'\bemer','emeritus'),(r'\bconsult','consulting'),(r'\bresearch\s+fellow','research_fellow'),(r'\bteaching\s+fellow|teach\s*-\s*lng\s+fellow','teaching_fellow'),(r'\bresearch\s+(?:assoc|asst)|radiation\s+lab','research')]:
        if re.search(pattern,text,re.I):out.append(label)
    return out or ['regular']

def core_span(text,a,b,version):
    """Separate unit/campus suffixes without restoring spelling or naming IDs."""
    a,b=trim(text,a,b)
    qualifiers=[]
    if version in VERSIONS[3:]:
        value=text[a:b]
        # University followed by medical-school/laboratory qualifier.
        m=re.search(r'\s+(?:School\s+of\s+Medicine|Medical\s+School|Med\.?\s+Sch|Univ\.?\s+Mus|XJniv\.?\s+Mus|Dept\b|Department\b|School\s+of\s+\w+)',value,re.I)
        if m and m.start()>0:qualifiers.append((a+m.start(),b));b=a+m.start()
        value=text[a:b]
        m=re.search(r'\s*\([^)]*\)\s*$',value)
        if m and m.start()>0:qualifiers.append((a+m.start(),b));b=a+m.start()
        value=text[a:b]
        # "College of Staten Island" is preserved as literal institution unless
        # a separate geographic slot supplies the campus. No city dictionary.
        m=re.search(r'\s+of\s+(?=[A-Z])',value) if version=='v03_core_and_boundaries' else None
        if m and re.search(r'\bCol\.',value[:m.start()]) and not re.search(r'\b(?:Agr|Tech|Science|Arts|Women|Ceramics)\b',value[m.end():],re.I):
            qualifiers.append((a+m.start(),b));b=a+m.start()
        if version in VERSIONS[7:]:
            m=re.search(r'\s+of\s+[A-Z][A-Za-z ]*\b(?:Island|County|City)\b',text[a:b])
            if m and re.search(r'\bCol\.',text[a:a+m.start()]):qualifiers.append((a+m.start(),b));b=a+m.start()
    a,b=trim(text,a,b)
    return a,b,qualifiers

def body_start(entry,edition):
    text=entry.get('biography_text','')
    if edition!='1955':return 0
    birthplace=entry.get('birthplace_text','').strip()
    p=text.find(birthplace) if birthplace else -1
    if p>=0:return p
    m=re.search(r'@[^@]+@\.?',text)
    return m.end() if m else 0

def headers(entry,edition,version):
    field='address_text' if edition in {'1906','1938'} else 'biography_text'
    text=entry.get(field,'');lo=0;hi=len(text) if field=='address_text' else max(0,text.find('@'))
    out=[]
    for m in re.finditer(r'[^,;]+',text[lo:hi]):
        a,b=trim(text,m.start()+lo,m.end()+lo);value=text[a:b]
        if not RX['marker'].search(value) or re.search(r'\b(?:Dr|Prof|Club|Hosp|Road|Street)\b|\d',value,re.I):continue
        a,b,quals=core_span(text,a,b,version)
        if a<b:out.append(dict(institution=ev(entry,field,a,b),qualifiers=[ev(entry,field,*q) for q in quals],evidence=ev(entry,field,m.start()+lo,m.end()+lo),employment_established=False))
    return out

def academic_context(clause):
    strong=bool(RX['strong_role'].search(clause))
    weak=bool(RX['weak_role'].search(clause))
    lab=bool(re.search(r'\b(?:lab|labs|department|dept|sch|col|radiation)\b',clause,re.I))
    # A plain research meteorologist/engineer with a bare city is insufficient.
    technical=bool(re.search(r'\b(?:technician|lab\.?\s+asst|student\s+asst|research\s+(?:assoc|asst|fellow)|asst\b|assoc\b|teaching)\b',clause,re.I))
    bare_assistant=bool(re.match(r'\s*asst\.?\s+[A-Z][a-z]+\s*,\s*\d',clause))
    return strong or bare_assistant or weak and (technical and (RX['subject'].search(clause) or lab) or lab)

def institution_slots(text,a,b,version,strong,carry):
    """Candidate literal slots constrained by appointment grammar, never a lexicon."""
    clause=text[a:b];dates=list(RX['date'].finditer(clause));slots=[]
    # Rank changes following the first printed year belong to the current
    # appointment, not new institution slots. A later explicit institution is
    # parsed on the next semicolon; multiple pre-date universities are retained.
    hi=dates[0].start() if dates else len(clause)
    bounded=clause[:hi]
    if version in VERSIONS[4:]:
        extra=re.search(r'\b(?:aid\s+curator|and\s+curator)\b',bounded,re.I)
        if extra:bounded=bounded[:extra.start()]
    for m in re.finditer(r'[^,]+',bounded):
        sa,sb=trim(text,a+m.start(),a+m.end());value=text[sa:sb]
        if not value:continue
        if RX['strong_role'].search(value) or RX['weak_role'].search(value):
            # Same-slot "Tutor. Example" and "Asst. Example" are allowed only
            # when the role is an opening title, with no unbounded field removal.
            role=re.match(r'\s*(?:tutor|lecturer|asst|assistant|[Il]nstr)\.?\s+(?=[A-Z])',value,re.I)
            if role:sa+=role.end();value=text[sa:sb]
            else:continue
        if re.search(r'\d|[^\w\s.()’\'&@\-¬]',value):continue
        marker=bool(RX['marker'].search(value))
        if RX['nonacademic'].search(value):continue
        if RX['unit'].search(value) and not marker:continue
        if version in VERSIONS[5:] and re.search(r'\b(?:institution|institutes|board|commission|resources|country|territorial|Artil)\b',value,re.I):continue
        if version in VERSIONS[8:] and norm(value) in {'director','superintendent','capt','captain','major','lieutenant','principal'}:continue
        if re.fullmatch(r'(?:univ|col|college|university|sch\.?\s+med|med\.?\s+sch|col\.?\s+pharmacy)\.?',value,re.I):continue
        # No generic field/unit label can stand in for an institution name.
        if not marker and (RX['subject'].search(value) or re.search(r'\b(?:psychol|miner|scientist|geologist|analyst|supervisor|chief)\b',value,re.I)):continue
        if not marker and re.search(r'\b(?:inst|sch|sta|fellow|hon|award|research|dept|director|sec|chief|minister|pres|county|summer|in\s+charge)\b',value,re.I):continue
        if not marker and (not strong and not carry or not dates):continue
        if not marker and not re.fullmatch(r"[A-Z][A-Za-zÀ-ž .()'’&¬-]{1,75}",value):continue
        if version==VERSIONS[0] and not marker:continue
        sa,sb,quals=core_span(text,sa,sb,version)
        if sa<sb:slots.append((sa,sb,quals,'explicit_college_marker' if marker else 'bounded_institution_carry' if carry and not strong else 'academic_abbreviation'))
    if version in VERSIONS[4:] and slots:
        marked=[s for s in slots if s[3]=='explicit_college_marker']
        if marked:
            # Geographic/department slots after an explicit college are not
            # second employers. A following bare name could be a parent
            # university; leave that relationship unresolved rather than guess.
            if len(marked)==1:
                s=marked[0]
                later=[q for q in slots if q[0]>s[1]]
                if later and re.search(r'\b(?:Asst|fellow)\b',clause,re.I) and not RX['strong_role'].search(clause) and not re.search(r'\b(?:Agr|Tech|Teachers|Postgrad)\b',text[s[0]:s[1]],re.I):return []
            if version in VERSIONS[5:] and len(marked)==1:
                s=marked[0]
                later=[q for q in slots if q[0]>s[1] and q[3]!='explicit_college_marker']
                # A college followed directly by another institutional name is
                # a constituent/parent relationship, not two independent jobs.
                if later and re.search(r'\b(?:Asst|teaching)\b',clause,re.I) and not re.search(r'\b(?:Agr|Tech|Teachers|Postgrad)\b',text[s[0]:s[1]],re.I):return []
            return marked
        # Multiple bare slots before a date usually contain city/state
        # qualifiers. Keep only the terminal candidate unless a second role
        # explicitly introduces another institution.
        return slots if len(slots)>1 and len(list(RX['strong_role'].finditer(bounded)))>1 else slots[:1] if version in VERSIONS[6:] else slots[-1:]
    return slots

def extract_universities(entry,edition=None,version='v09_medical_unit_forms'):
    """Extract supported employment; report institutional addresses separately."""
    edition=str(edition or entry['edition'])
    if edition not in {'1906','1938','1955'}:raise ValueError('Only 1906, 1938 and 1955 supported')
    if version not in VERSIONS:raise ValueError('Unknown development version')
    text=entry.get('biography_text','');pred=[];flags=[];by={}
    reported=headers(entry,edition,version)
    start=body_start(entry,edition);end=len(text)
    if not text.strip():return dict(affiliations=[],reported_header_affiliations=reported,review_flags=[dict(reason='missing_biography',evidence=None)])
    first=None
    role_pattern=re.compile(PATTERNS['strong_role']+'|'+PATTERNS['weak_role'],re.I)
    for m in role_pattern.finditer(text,start):
        segend=text.find(';',m.end());segend=segend if segend>=0 else len(text)
        segment=text[m.start():segend]
        if academic_context(segment):first=m.start();break
    if first is None:return dict(affiliations=[],reported_header_affiliations=reported,review_flags=[])
    stop=RX['membership_stop'].search(text,first)
    if stop:end=min(end,stop.start())
    if version in VERSIONS[5:]:
        stop=re.search(r'\b(?:[A-Za-z]+\.?\s+){1,3}(?:Soc|Asn|Ass)\b',text[first:end])
        if stop:end=min(end,first+stop.start())
    if version in VERSIONS[3:]:
        neighbor_pattern=RX['neighbor_heading'] if version==VERSIONS[3] else re.compile(r'(?<=[.!?]\s)(?:[A-Z][a-z]+,\s+(?:Dr|Prof)\.?\s+[A-Z]|[A-Z]{3,},\s+[A-Z][(][A-Z])')
        neighbor=neighbor_pattern.search(text,first)
        if neighbor:end=min(end,neighbor.start());flags.append(dict(reason='neighboring_person_heading',evidence=ev(entry,'biography_text',neighbor.start(),neighbor.end())))
    previous=False
    bounds=[first]
    if version in VERSIONS[7:]:
        for delimiter in re.finditer(r';|(?<=\d[-—–])\s+(?=[A-Z])|(?<=\d)\.\s+(?=[A-Z])',text[first:end]):
            bounds.extend([first+delimiter.start(),first+delimiter.end()])
        bounds.append(end)
        parts=list(zip(bounds[::2],bounds[1::2]))
    else:parts=[(first+p.start(),first+p.end()) for p in re.finditer(r'[^;]+',text[first:end])]
    for pa,pb in parts:
        a,b=trim(text,pa,pb);clause=text[a:b]
        terminated=False
        if version in VERSIONS[5:]:
            # Stop a trailing award, expedition or honorary-degree sentence
            # after a valid appointment instead of throwing the appointment out.
            cutoff=re.search(r'\b(?:Hon\.?\s+(?:A\.?M|LL\.?D)|LL\.?D|Medal|With\s+|Assoc[,.]?\s+ed|Ed[, ]|Mem\.?\s+(?:jury|cmt))',clause,re.I)
            if cutoff and RX['date'].search(clause[:cutoff.start()]):
                terminated=version in VERSIONS[6:]
                b=a+cutoff.start();a,b=trim(text,a,b);clause=text[a:b]
        strong=bool(RX['strong_role'].search(clause));academic=academic_context(clause)
        carry=version in VERSIONS[2:] and previous
        if version in VERSIONS[4:]:
            # New occupations/employers terminate inheritance even when a weak
            # generic word such as "director" or "associate" is present.
            independent=bool(re.search(r'\b(?:inst|institutes?|sch|school|hosp|hospital|mus|museum|surv|geologist|psychologist|psychiatrist|alienist|commissioner|grant|expedition|hon|LL\.?D|Sc\.?D)\b',clause,re.I))
            marked=bool(RX['marker'].search(clause))
            if version in VERSIONS[8:]:
                military=bool(re.search(r'\bU\.?\s*S\.?\s*(?:A|N|N\.?\s*R|Torpedo|Artil)\b|\b(?:capt|major|lieut|commander|ordnance)\b',clause,re.I))
                secondary=bool(re.search(r'\b(?:high|pub\.?\s+schools?|public\s+schools?|principal|examiner)\b',clause,re.I))
                if (military or secondary) and not marked:previous=False;continue
                # "College" can be used by secondary schools. A generic teacher
                # title alone does not disambiguate that level of education.
                if re.search(r'\bteacher\b',clause,re.I) and not re.search(r'\b(?:univ|university)\b',clause,re.I):previous=False;continue
            university_header=any(norm(h['institution']['text']) and norm(h['institution']['text']) in norm(clause) for h in reported)
            if independent and not marked and not strong and not university_header:previous=False;continue
            if re.search(r'\b(?:sch|school|hosp|hospital|mus|museum|inst|institutes?)\b',clause,re.I) and not marked:
                # A bare university after a departmental medical-school label is
                # allowed. A named standalone school/hospital is not.
                stripped=re.sub(r'\b(?:sch\.?\s+med|med\.?\s+sch|sell\.?\s+med|grad\.?\s+sch|sch\.?\s+pharmacy)\.?','',clause,flags=re.I)
                if version in VERSIONS[9:]:stripped=re.sub(r'\b(?:Medical\s+School|School\s+of\s+Medicine)\b','',stripped,flags=re.I)
                if re.search(r'\b(?:sch|school|hosp|hospital|mus|museum|inst|institutes?)\b',stripped,re.I):previous=False;continue
            if re.search(r'\b(?:state\s+geologist|analyst|chemist|director|supt)\b',clause,re.I) and not strong and not re.search(r'\b(?:asst|assoc|fellow|teaching)\b|radiation\s+lab',clause,re.I) and not marked:previous=False;continue
            if re.search(r'\b(?:LL\.?D|hon|medal|award|expedition)\b',clause,re.I):previous=False;continue
            if version in VERSIONS[5:]:
                if re.search(r'\b(?:territorial|grants?|expeditions?|civil\s+service|govt|state\s+geologist|water\s+consultant)\b',clause,re.I):previous=False;continue
                if not academic and re.search(r'\b(?:With|Expos|expedition)\b',clause,re.I):previous=False;continue
            if version in VERSIONS[6:] and re.search(r'\bhigh\s+s(?:ch|eh)\b',clause,re.I):previous=False;continue
        # Each nonacademic employer or new nonacademic role terminates carry.
        incompatible=RX['nonacademic'].search(clause)
        if incompatible:
            # Academic institution can coexist with a government consulting role
            # in a separate sentence; limit to first sentence where possible.
            prior=clause[:incompatible.start()]
            if not strong or not RX['marker'].search(prior):previous=False;continue
        if not academic and not carry:previous=False;continue
        if re.search(r'\b(?:trustee|honorary|hon\.?\s+(?:assoc|fellow)|by\s+courtesy|memberships?)\b',clause,re.I):previous=False;continue
        # A fellowship in an education clause cannot start employment. Explicit
        # teaching/research appointments may still be genuine graduate jobs.
        if RX['degree'].search(clause) and not strong and not re.search(r'\b(?:asst|technician|research\s+assoc)\b',clause,re.I):previous=False;continue
        slots=institution_slots(text,a,b,version,strong or academic,carry)
        if version in VERSIONS[6:]:
            slots=[s for s in slots if not any(norm(text[s[0]:s[1]].split('-')[0])==key and '-' in text[s[0]:s[1]] for key in by)]
        # A newly introduced institution requires either its own academic context
        # or direct uninterrupted inheritance from the preceding academic clause.
        for sa,sb,qualifiers,family in slots:
            context=ev(entry,'biography_text',a,b)
            roles=[ev(entry,'biography_text',a+m.start(),a+m.end()) for m in role_pattern.finditer(clause)]
            dates=[ev(entry,'biography_text',a+m.start(),a+m.end()) for m in RX['date'].finditer(clause)]
            appointment=dict(institution=ev(entry,'biography_text',sa,sb),context=context,roles=roles,dates=dates,temporal_status=status(clause),appointment_types=appointment_types(clause),qualifiers=[ev(entry,'biography_text',*q) for q in qualifiers])
            item=dict(institution=appointment['institution'],contexts=[context],roles=roles,dates=dates,qualifiers=appointment['qualifiers'],appointments=[appointment],temporal_status=appointment['temporal_status'],appointment_types=appointment['appointment_types'],resolved_years=[],rule_family=family,trace=[dict(rule_id=edition+'_'+family,order=10 if family=='explicit_college_marker' else 20 if family=='academic_abbreviation' else 30,affected=context,replacement=text[sa:sb])])
            key=norm(item['institution']['text'])
            if key in by:
                p=by[key]
                for k in ('contexts','roles','dates','qualifiers','appointments','trace'):p[k]+=item[k]
                states={p['temporal_status'],item['temporal_status']}
                if 'mixed' in states or {'current','past'}<=states:p['temporal_status']='mixed'
                elif 'current' in states:p['temporal_status']='current'
                elif 'uncertain' in states:p['temporal_status']='uncertain'
                p['appointment_types']=sorted(set(p['appointment_types']+item['appointment_types']))
            else:by[key]=item;pred.append(item)
        previous=bool(slots) and not terminated
        if not slots and academic:flags.append(dict(reason='academic_employer_unresolved',evidence=ev(entry,'biography_text',a,b)))
    # Matching institutional addresses corroborate existing predictions only.
    generic={'university','univ','college','col','of','the'}
    for h in reported:
        ht=[w for w in re.findall(r'[a-z]+',h['institution']['text'].lower()) if w not in generic]
        for p in pred:
            pt=[w for w in re.findall(r'[a-z]+',p['institution']['text'].lower()) if w not in generic]
            if ht and pt and ht==pt:p.setdefault('supporting_headers',[]).append(h['institution'])
    return dict(affiliations=pred,reported_header_affiliations=reported,review_flags=flags)

def rule_specification():
    return dict(versions=list(VERSIONS),patterns=PATTERNS,preprocessing='None; original text and offsets retained. Normalization used only for duplicate scoring.',
        edition_sources=EDITION_STRATEGIES,
        order=['bound original header and body','bound membership and neighboring entry','identify academic role clauses','extract institutional markers','extract academic abbreviations','bounded carry to successive employer','separate unit/campus qualifiers','consolidate exact literal repeats'],
        exclusions='Degrees, honorary roles, governance, secondary schools and nonacademic employers; unclear institutes/hospitals flagged. No institution dictionary, person overrides or spelling repairs.')

"""Dictionary-free, source-offset-preserving AMWS academic employment extractor.

Versions are additive development experiments, never person/institution overrides.
All matches refer to original Python Unicode string offsets (end exclusive).
"""
import re
import unicodedata

VERSIONS = ('v00_headers', 'v01_career', 'v02_carry_forward', 'v03_structural_guards')
PATTERNS = {
    'section_start': r'\bProf\s*\.?\s*(?:Exp|F\.xp)\s*[:.]?',
    'section_concurrent': r'\bConcurrent\s+Po[sa]\s*[:.]?',
    'section_stop': r'\b(?:Concurrent\s+Po[sa]|Mem|Honors\s*&?\s*Awards|Res|Mailing\s+Add)\s*:',
    'career_start': r'\b(?:[Il]nstr|prof(?:essor)?|lect(?:urer|r)|tutor|demonstrator|asst|assistant|research\s+asst|teaching\s+(?:asst|fellow))\b',
    'academic_role': r'\b(?:[Il]nstr(?:uctor)?|prof(?:essor)?|lect(?:urer|r)|tutor|demonstrator|dean|head\s+dept|chairman\s+dept|chmn\s+dept|teaching\s+(?:asst|fellow)|asst|assistant|res(?:earch)?\s+(?:assoc|asst|fellow|fel|scientist)|fel(?:low)?|scientist|mem\s+(?:tech\s+)?staff)\b',
    'strong_role': r'\b(?:[Il]nstr(?:uctor)?|prof(?:essor)?|lect(?:urer|r)|tutor|demonstrator|dean|head\s+dept|chairman\s+dept|chmn\s+dept|teaching\s+(?:asst|fellow))\b',
    'institution_marker': r'\b(?:univ(?:ersit[yé])?|col(?:lege)?|polytech(?:nic)?|(?:inst(?:itute)?\.?\s+(?:of\s+)?(?:tech(?:nology)?|mining))|(?:sch(?:ool)?\.?\s+(?:of\s+)?mines)|(?:med(?:ical)?\.?\s+(?:col(?:lege)?|sch(?:ool)?))|(?:nor(?:mal)?\.?\s+sch(?:ool)?)|(?:naval\s+postgrad\s+sch(?:ool)?))\b',
    'nonacademic': r'\b(?:high\s*sch|pub(?:lic)?\s+sch|acad(?:emy)?|trustee|hon\.?\s+(?:pres|degree)|honorary\s+(?:degree|pres)|membership|soc(?:iety)?|asn|assn|association|corp|company|co|inc|industr(?:y|ies)|insurance|bur(?:eau)?|USDA|U\.?\s*S\.?\s+(?:DEPT|ARMY|NAVY|FOREST)|nat(?:ional)?\s+(?:lab|inst)|research\s+foundation)\b',
    'date': r'(?<![\w])(?:1[6789]\d{2}|\d{2})(?:\s*[-—–]\s*(?:1[6789]\d{2}|\d{2})?)?(?!\d)',
    'stop_old': r'\b(?:F\.?\s*A\.?\s*A|M\.?\s*A\.?\s*A|A\.\s*A|Chem\. Soc|Physical Soc|Math\. Soc|Soc\.\s|Mem\b|Hon\. mem)'
}
RX={k:re.compile(v,re.I) for k,v in PATTERNS.items()}


def literal(row, field, start, end):
    return dict(field=field,start=start,end=end,text=row.get(field,'')[start:end])


def trim(text,start,end):
    while start<end and (text[start].isspace() or text[start] in ',;@'): start+=1
    while end>start and (text[end-1].isspace() or text[end-1] in ',;@'): end-=1
    return start,end


def norm(text):
    return re.sub(r'[^\w]+','',unicodedata.normalize('NFKC',text).casefold())


def ranges(row, edition, guarded=False):
    text=row.get('biography_text','')
    if edition=='1986':
        starts=list(RX['section_start'].finditer(text))
        if not starts:return []
        out=[]
        for m in starts[:1]:
            following=RX['section_stop'].search(text,m.end())
            end=following.start() if following else len(text)
            out.append((m.end(),end,'professional_experience'))
            concurrent=RX['section_concurrent'].match(text,end)
            if concurrent:
                following=RX['section_stop'].search(text,concurrent.end())
                out.append((concurrent.end(),following.start() if following else len(text),'concurrent_positions'))
        return out
    start=0
    if guarded and edition=='1955':
        # Names/titles in a full-entry header are not career starts.
        at=text.rfind('@',0,text.find('Prof Exp') if 'Prof Exp' in text else min(len(text),350))
        start=max(0,at+1)
    m=RX['career_start'].search(text,start)
    if not m:return []
    stop=RX['stop_old'].search(text,m.end())
    return [(m.start(),stop.start() if stop else len(text),'career_sequence')]


def header(row, edition):
    if edition in {'1906','1938'}:
        field='address_text';text=row.get(field,'');lo=0;hi=len(text)
    elif edition=='1955':
        field='biography_text';text=row.get(field,'');hi=text.find('@')
        if hi<0:return []
        # Header begins after the source's name, including damaged name fields.
        name=row.get('raw_name','').strip();lo=text.find(name)+len(name) if name and name in text[:hi] else 0
    else:return [] # 1986 mailing addresses only corroborate career predictions.
    out=[]
    for m in re.finditer(r'[^,;]+',text[lo:hi]):
        a,b=trim(text,lo+m.start(),lo+m.end());piece=text[a:b]
        if RX['institution_marker'].search(piece) and not RX['nonacademic'].search(piece):
            # A department label alone cannot identify a college.
            if re.fullmatch(r'(?:department|dept|college|col|school|sch)\.?\s+(?:of\s+)?[A-Za-z ]+',piece,re.I) and not re.search(r'\buniv|\bstate|\bnor|\bmed',piece,re.I):
                continue
            out.append((field,a,b))
    return out


def temporal(text, edition, header_current=False):
    dates=list(RX['date'].finditer(text))
    closed=any(re.search(r'\d\s*[-—–]\s*\d',m.group()) for m in dates)
    opened=any(re.search(r'[-—–]\s*$',m.group()) for m in dates)
    if re.search(r'\bretired\b',text,re.I):return 'past'
    if header_current or opened:
        return 'mixed' if closed else 'current'
    return 'past' if dates else 'uncertain'


def types(text):
    result=[]
    for pat,label in [(r'\b(?:visiting|vis|exchange)\b','visiting'),(r'\bemer','emeritus'),(r'\badj','adjunct'),(r'\bconsult','consulting'),(r'\b(?:res|research)\s+(?:fel|fellow)','research_fellow'),(r'\bteaching\s+(?:fel|fellow)','teaching_fellow'),(r'\b(?:intern|resident|trainee)\b','clinical_training')]:
        if re.search(pat,text,re.I):result.append(label)
    return result or ['regular']


def extract_affiliations(entry, edition=None, version='v03_structural_guards'):
    """Return literal university/college predictions and review flags, not identities."""
    edition=str(edition or entry['edition'])
    if version not in VERSIONS:raise ValueError('Unknown frozen rule version')
    guarded=version=='v03_structural_guards'
    text=entry.get('biography_text','');pred=[];flags=[];seen=set()
    def add(field,a,b,lo,hi,family,order,status=None):
        a,b=trim(entry[field],a,b)
        if a>=b:return
        inst=entry[field][a:b];key=norm(inst)
        if not key:return
        context=entry[field][lo:hi]
        roles=[literal(entry,field,lo+m.start(),lo+m.end()) for m in RX['academic_role'].finditer(context)]
        dates=[literal(entry,field,lo+m.start(),lo+m.end()) for m in RX['date'].finditer(context)]
        item=dict(institution=literal(entry,field,a,b),contexts=[literal(entry,field,lo,hi)],roles=roles,dates=dates,
                  resolved_years=[],temporal_status=status or temporal(context,edition),appointment_types=types(context),
                  rule_family=family,trace=[dict(rule_id=family,order=order,affected=literal(entry,field,lo,hi),replacement=inst)])
        if key in seen:
            old=next(p for p in pred if norm(p['institution']['text'])==key)
            old['contexts']+=item['contexts'];old['roles']+=roles;old['dates']+=dates;old['trace']+=item['trace']
            states={old['temporal_status'],item['temporal_status']}
            if 'mixed' in states or {'past','current'}<=states:old['temporal_status']='mixed'
            elif 'current' in states:old['temporal_status']='current'
            old['appointment_types']=sorted(set(old['appointment_types']+item['appointment_types']))
            return
        seen.add(key);pred.append(item)
    for field,a,b in header(entry,edition):
        value=entry[field][a:b]
        if guarded and (re.search(r'\d|\b(?:club|heights|street|st|road|rd|hospitals?|dr|prof|col\.?\s+[A-Z]+\s+[A-Z]\()',value,re.I) or re.fullmatch(r'(?:state\s+)?(?:university|college|univ|col)\.?',value,re.I)):continue
        add(field,a,b,a,b,'institutional_header',10,'current')
    if version=='v00_headers':return dict(affiliations=pred,review_flags=flags)
    for lo,hi,section in ranges(entry,edition,guarded):
        previous_academic=False
        for part in re.finditer(r'[^;]+',text[lo:hi]):
            a=lo+part.start();b=lo+part.end();clause=text[a:b]
            role=RX['academic_role'].search(clause)
            carry=version in {'v02_carry_forward','v03_structural_guards'} and previous_academic
            strong=bool(RX['strong_role'].search(clause))
            if not role and not carry:
                previous_academic=False;continue
            # Reject neighboring entries and governance/education-only sequences.
            if re.search(r'\b(?:Educ|Mailing\s+Add|Hon\s+Degrees)\s*:',clause,re.I):
                previous_academic=False;continue
            date_matches=list(RX['date'].finditer(clause))
            slots=list(re.finditer(r'[^,]+',clause))
            accepted=[]
            for s in slots:
                sa,sb=trim(text,a+s.start(),a+s.end());value=text[sa:sb]
                # Only the literal institution-bearing part before its date.
                date=RX['date'].search(value)
                if date:sb=sa+date.start();sa,sb=trim(text,sa,sb);value=text[sa:sb]
                if not value or len(value)>110 or re.search(r'\d',value):continue
                marker=RX['institution_marker'].search(value)
                if marker and not RX['nonacademic'].search(value):
                    if RX['academic_role'].search(value):continue
                    if guarded and re.fullmatch(r'(?:univ|col|med\.?\s+(?:sch|col)|col\.?\s+(?:med|vet\.?\s+med)|sch\.?\s+(?:med|of\s+mines))\.?',value,re.I):
                        tail=clause[s.end():].lstrip(', ')
                        nextpart=tail.split(',')[0]
                        if nextpart and not RX['date'].search(nextpart) and not RX['academic_role'].search(nextpart):
                            sb=a+s.end()+len(clause[s.end():])-len(tail)+len(nextpart);value=text[sa:sb]
                        else:continue
                    if guarded and (re.search(r'\b(?:exped|expeditions|training|hosp|mus|exten\.?\s+dept)\b',value,re.I) or re.fullmatch(r'(?:univ|col|college|university)\.?',value,re.I)):continue
                    accepted.append((sa,sb,'career_institution_marker'))
            if not accepted and strong and date_matches:
                # A dated academic role may use a bare abbreviated proper name.
                # Last comma slot before first date; never spell-correct or dictionary-match.
                before=clause[:date_matches[0].start()].rstrip(' ,.')
                comma=before.rfind(',')
                value=before[comma+1:].strip() if comma>=0 else ''
                if value and not RX['academic_role'].search(value) and not RX['nonacademic'].search(before) and not re.search(r'\b(?:dept|chem|physics|math|eng|biol|geol|surg|hort|path|bacter|physiol)\b',value,re.I) and re.fullmatch(r"[A-Z][A-Za-zÀ-ž .()'’&-]{1,69}",value) and (not guarded or not re.search(r'\b(?:inst|lab|sta|sch|council|surv|army|force|naval|service|january|feb|march|april|june|july|aug|sept|oct|nov|dec)\b|[•¬]|-\s|\s-|[^A-Za-z]$',value,re.I)):
                    sa=a+before.rfind(value);accepted.append((sa,sa+len(value),'dated_academic_abbreviation'))
            if not accepted and carry and date_matches and not role:
                first=clause[:date_matches[0].start()].strip(' ,.')
                if first and not RX['nonacademic'].search(clause) and re.fullmatch(r"[A-Z][A-Za-zÀ-ž .()'’&-]{1,69}",first) and (not guarded or not re.search(r'\b(?:geologist|engineer|attorney|af|army|surv|govt|cmn|educ|inst|lab|sch|sta)\b',first,re.I)):
                    sa=a+clause.index(first);accepted.append((sa,sa+len(first),'successive_institution_carry_forward'))
            for sa,sb,family in accepted:
                # Broad employment plus a generic institution marker, or strong academic role.
                add('biography_text',sa,sb,a,b,family,20 if family=='career_institution_marker' else 30)
            if not accepted and role and re.search(r'\b(?:inst|hospital|hosp|univ|col|sch)\b',clause,re.I):
                flags.append(dict(reason='institution_relationship_or_boundary_unresolved',evidence=literal(entry,'biography_text',a,b),section=section))
            previous_academic=bool(accepted)
    if guarded:
        # Consolidate only structurally compatible within-entry full/header forms.
        # No institution-name lookup: each non-generic word must prefix-match.
        generic={'univ','university','of','the','college','col','dept','department'}
        def tokens(value):return [w for w in re.findall(r'[a-z]+',value.lower()) if w not in generic]
        headers=[p for p in pred if p['rule_family']=='institutional_header']
        for h in headers:
            ht=tokens(h['institution']['text'])
            matches=[]
            for p in pred:
                if p is h or p['rule_family']=='institutional_header':continue
                pt=tokens(p['institution']['text'])
                if ht and pt and all(any(a.startswith(b) or b.startswith(a) for a in ht) for b in pt) and all(any(a.startswith(b) or b.startswith(a) for b in pt) for a in ht):matches.append(p)
            if len(matches)==1:
                p=matches[0];p['contexts']+=h['contexts'];p['trace']+=h['trace'];p['temporal_status']='mixed' if p['temporal_status']=='past' else 'current' if p['temporal_status']=='uncertain' else p['temporal_status']
                p['supporting_header']=h['institution'];pred.remove(h)
    if not text.strip():flags.append(dict(reason='missing_biography',evidence=None))
    return dict(affiliations=pred,review_flags=flags)

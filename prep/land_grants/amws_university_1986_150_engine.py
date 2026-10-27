"""Source-offset-preserving 1986 university/college employment extractor.

Development versions use generic grammar only. No reference labels, institution
dictionary, person override, spelling repair or geocoding is imported here.
"""
import re
import unicodedata

VERSIONS=('v00_marker_baseline','v01_section_boundaries','v02_employment_context','v03_units_and_carry','v04_relationship_guards','v05_structural_regressions','v06_address_evidence','v07_role_spans')
PATTERNS={
 'labels':r'(?<![A-Za-z])(?P<label>Pro[fi]\s*Exp|Con(?:current|currcnt|eurrent|cuncnt|currenl)\s*(?:Po\s*[sx]|Paw|Pm)|Hon(?:ors\s*&?\s*Awards|\s*Degrees)|M(?:ailing|.iling)\s*Add|Mem|Mein|Vfem|Re[sx])\s*[:.;]?',
 'institution_marker':r'\b(?:univ(?:ersity)?|[UL]mv|L[n\x27’]iv|col(?:lege)?s?|polytech(?:nic)?\s+inst|inst\s+(?:of\s+)?t[ec]ch(?:nol(?:ogy)?)?|sch\s+(?:of\s+)?mine[s*]?|med(?:ical)?\s+sch(?:ool)?)\b',
 'role':r'\b(?:(?:sr|prin|asst|assoc|adj|vis|visiting|guest|invited|emer|clin|actg|acting|res|research|teaching|fac|proj)\s+)*(?:prof(?:essor)?|instr(?:uctor)?|lectr|lecturer|tutor|dean|scholar|fac\s+mem|f[ec]l(?:low)?|asst|assistant|assoc|scientist|physicist|mathematician|technician|lab\s+attendant|researcher|agronomist|entomologist|pathologist|biochemist|analyst|coordr(?:\s+grad\s+studies)?|dir(?:ector)?(?:\s+lab)?|head(?:\s+dept)?|chmn(?:\s+dept)?|mgr|consult|mem\s+(?:tech\s+)?staff|staff\s+mem)\b',
 'strong_role':r'\b(?:prof(?:essor)?|instr(?:uctor)?|lectr|lecturer|tutor|dean|vis\s+scholar|fac\s+mem|teaching\s+(?:asst|assoc|fellow))\b',
 'nonacademic':r'\b(?:corp(?:oration)?|company|co|inc|USDA|US\s+(?:Army|Navy|Dept|Bur|Geol)|NASA|Nat\s+(?:Lab|Acad|Res\s+Coun)|Soc|Asn|associat(?:ion|ions)|trustee|board|bd|comt|comn|council|public\s+sch|pub\s+sch|high\s+sch|mil\s+sch|RR|Food\s*&?\s*Drug\s+Admin)\b',
 'training':r'\b(?:intern|resident|trainee|hon(?:orary)?\s+(?:prof|fel)|by\s+courtesy|trustee)\b',
 'subject':r'\b(?:chem|biochem|biol|zool|bot|physics|math|geol|genetics|pharmacol|pharm|physiol|anat|surg|ophthal|med|immunol|microbiol|pediat|psychol|psychiat|statist|eng|elec|comput\s+sci|astron|agron|food\s+sci|geophys|parasit(?:ol)?|res|research|cell|develop|dairy|appl|fluid\s+dynamics)\b',
 'date':r'(?<![\w])(?:1[89]\d{2}|\d{2})(?:\s*[-—–]\s*(?:1[89]\d{2}|\d{2})?)?(?!\w)',
 'damaged_date':r'\b\d[A-Za-z]\s*[-—–]|\b[A-Z]\d\s*[-—–]|\b\d{2}[*]\d{2}\b|\b(?!1[89])\d{4}\b|\b\d{2}-\d(?!\d)',
 'neighbor':r'(?<=[.!?]\s)(?:[A-Z]{3,},\s*[A-Z][A-Z(]|[A-Z][a-z]+,\s*(?:Dr|Prof)\.?\s+[A-Z])',
}
RX={k:re.compile(p,0 if k=='neighbor' else re.I) for k,p in PATTERNS.items()}

def norm(text):return re.sub(r'[^\w]+','',unicodedata.normalize('NFKC',text).casefold())
def ev(entry,a,b):return dict(field='biography_text',original_source_field='raw_text_adjusted',start=a,end=b,text=entry['biography_text'][a:b])
def trim(text,a,b):
    while a<b and (text[a].isspace() or text[a] in ',;:.'):a+=1
    while b>a and (text[b-1].isspace() or text[b-1] in ',;:.'):b-=1
    return a,b

def labels(text,version):
    events=[]
    for m in RX['labels'].finditer(text):
        value=re.sub(r'\s','',m['label']).lower();tail=text[m.end():]
        kind='professional_experience' if value.startswith('pro') else 'concurrent_positions' if value.startswith('con') else 'mailing_address' if 'ailing' in value or 'iling' in value else 'membership' if value in {'mem','mein','vfem'} else 'research' if value.startswith('re') else 'honors'
        printed=m.group();delimited=bool(re.search(r'[:.;]\s*$',printed))
        # Mem tech staff / res assoc are roles, not section headings. Explicit
        # punctuation or a preceding printed date distinguishes glued labels.
        if kind=='membership' and not delimited:
            if re.match(r'(?:tech|sr|staff|bd|board|comt|coun|lab|of\s+staff)\b',tail,re.I):continue
            if not (m.start()>0 and text[m.start()-1].isdigit()) and not re.match(r'(?:Fel|AAAS|Am|Sigma|Soc|Inst|Nat|Fcl)\b',tail,re.I):continue
        if kind=='research' and not delimited:
            if value=='res' or not (m.start()>0 and text[m.start()-1].isdigit()):continue
        if version==VERSIONS[0] and m.start()>0 and text[m.start()-1].isdigit() and kind in {'membership','honors','concurrent_positions','research'}:continue
        events.append((m.start(),m.end(),kind))
    return events

def sections(entry,version):
    text=entry.get('biography_text','');events=labels(text,version);out=[];flags=[]
    firstmail=next((a for a,b,k in events if k=='mailing_address'),len(text))
    professional=[(a,b,k) for a,b,k in events if k=='professional_experience' and a<firstmail]
    concurrent=[(a,b,k) for a,b,k in events if k=='concurrent_positions' and a<firstmail]
    if not professional:flags.append(dict(reason='missing_professional_experience_heading',evidence=None))
    starts=professional[:1]+concurrent[:1]
    for a,b,k in starts:
        hi=next((x for x,y,z in events if x>=b),len(text))
        neighbor=next((m for m in RX['neighbor'].finditer(text,b,hi) if version not in VERSIONS[5:] or not RX['role'].fullmatch(m.group().split(',')[0])),None)
        if neighbor:hi=neighbor.start();flags.append(dict(reason='neighboring_person_heading',evidence=ev(entry,neighbor.start(),neighbor.end())))
        out.append((b,hi,k,ev(entry,a,b)))
    for a,b,k in events:
        if k in {'professional_experience','concurrent_positions'} and not re.search(r'[:.;]\s*$',text[a:b]):
            flags.append(dict(reason='heading_delimiter_missing_or_damaged',evidence=ev(entry,a,b)))
    return sorted(out),flags,events

def core(entry,a,b,version):
    text=entry['biography_text'];a,b=trim(text,a,b);quals=[]
    if version in VERSIONS[3:]:
        value=text[a:b]
        # A university followed by a named unit remains one institution. Medical
        # colleges without an explicit university parent retain their full name.
        uni=re.search(r'\b(?:univ(?:ersity)?|[UL]mv|L[n\x27’]iv)\b',value,re.I)
        if uni:
            q=re.search(r'\s+(?:MED\s+(?:SCH|CTR)|COL\s+(?:MED|DENT|PHARM)|HEALTH\s+SCI\s+CTR|SCHOOL\s+OF\s+MEDICINE)\b',value[uni.end():],re.I)
            if version in VERSIONS[4:]:
                q=re.search(r'\s+(?:(?:(?:north|south|east|west)(?:east|west)?(?:ern)?\s+)?MED\s+(?:SCH|CTR)|COL\s+(?:MED|DENT|PHARM)|HEALTH\s+SCI\s+CTR|SCHOOL\s+OF\s+MEDICINE)\b',value[uni.end():],re.I)
            if q:at=a+uni.end()+q.start();quals.append(ev(entry,at,b));b=at
            # State University + abbreviated state + following campus text.
            value=text[a:b]
            m=re.match(r'(State\s+Univ\s+[A-Z]{2,4})([ -]+.+)',value) if version==VERSIONS[3] else re.match(r'((?i:State\s+Univ)\s+[A-Z]{2,4})([ -]+.+)',value)
            if m:at=a+len(m[1]);quals.append(ev(entry,at,b));b=at
            value=text[a:b]
            m=re.search(r'(?<=\w)-[A-Z][A-Za-z]+(?:\s+[A-Za-z]+)*$',value)
            if m and uni.start()==0:at=a+m.start();quals.append(ev(entry,at,b));b=at
        else:
            med=re.search(r'\s+Med\s+Sch\b',value,re.I)
            if med:at=a+med.start();quals.append(ev(entry,at,b));b=at
    a,b=trim(text,a,b);return a,b,quals

def candidates(entry,a,b,version):
    text=entry['biography_text'];out=[]
    # Commas, semicolons, colons and period-space separate printed source slots.
    # Ampersands split two employers only when both sides contain markers.
    pieces=[]
    for p in re.finditer(r'[^,;:]+',text[a:b]):
        pa=a+p.start();pb=a+p.end()
        cuts=[pa]
        for dot in re.finditer(r'\.\s+|(?<=\d)\s+A\s+(?=Univ\b)',text[pa:pb],re.I) if version in VERSIONS[5:] else re.finditer(r'\.\s+',text[pa:pb]):
            cuts.extend([pa+dot.start(),pa+dot.end()])
        cuts.append(pb)
        pieces.extend(zip(cuts[::2],cuts[1::2]))
    for pa,pb in pieces:
        marker=list(RX['institution_marker'].finditer(text[pa:pb]))
        if not marker:continue
        parts=[(pa,pb)]
        for amp in re.finditer(r'\s*&\s*',text[pa:pb]):
            at=pa+amp.start();after=pa+amp.end()
            if (RX['institution_marker'].search(text[pa:at]) or version in VERSIONS[4:] and re.search(r'\d\s*$',text[pa:at])) and RX['institution_marker'].search(text[after:pb]):parts=[(pa,at),(after,pb)];break
        for sa,sb in parts:
            value=text[sa:sb];markers=list(RX['institution_marker'].finditer(value))
            if not markers:continue
            first=markers[0]
            # Strip role and discipline prefixes in the same source slot.
            before=value[:first.start()];roles=list(RX['role'].finditer(before))
            if roles:
                offset=roles[-1].end();subjects=list(RX['subject'].finditer(before,offset))
                if subjects:offset=subjects[-1].end()
                if version in VERSIONS[4:]:
                    subject=re.search(r'\banimal\s+sci\b',before[offset:],re.I)
                    if subject:offset+=subject.end()
                while offset<len(before) and (before[offset].isspace() or before[offset] in ('&.\x27’' if version in VERSIONS[4:] else '&.')):offset+=1
                sa+=offset;value=text[sa:sb]
            # Dates and post-date role changes are not part of institution names.
            date=RX['date'].search(value)
            if date:sb=sa+date.start()
            slash=text[sa:sb].find('/')
            if slash>=0:sb=sa+slash
            if version in VERSIONS[4:]:
                suffix=re.search(r'\bUniv(?:ersity)?\.(?=[A-Z])',text[sa:sb],re.I)
                if suffix:sb=sa+suffix.end()-1
            sa,sb=trim(text,sa,sb)
            if sa>=sb:continue
            value=text[sa:sb]
            if len(value)>120:continue
            if re.fullmatch(r'(?:Univ|University|Col|College|Col\s+(?:Med|Dent|Pharm|Eng)|Med\s+Sch|Sch\s+Med|Sch|Cols)',value,re.I):continue
            # Source unit labels cannot serve as the employer's institution name.
            if re.search(r'\b(?:dept|department|faculty|grad\s+sch|sch\s+nat|col\s+human|col\s+eng)\b',value,re.I):continue
            if RX['nonacademic'].search(value):continue
            if version in VERSIONS[4:]:
                if re.search(r'\b(?:var|various|rcls|rels|relations)\b',value,re.I):continue
                if re.search(r'[A-Z][a-z]{3,}[A-Z](?=\W|$)',value):continue
                marker=RX['institution_marker'].search(value)
                if marker and RX['role'].search(value,marker.end()):continue
            if version in VERSIONS[2:]:
                if re.search(r'\b(?:hosp|hospital|sch\s+med)\b',value,re.I):continue
                if re.search(r'\bUniv\s+Res\s*&?\s*Reactor\s+Ctr',value,re.I):continue
                if re.search(r'[^\w\s&’\x27.()/-]',value):continue
            sa,sb,quals=core(entry,sa,sb,version)
            if sa<sb:out.append((sa,sb,quals))
    # Multiple markers in a shared slot represent one emitted name, not several.
    seen=set();result=[]
    for s in out:
        key=norm(text[s[0]:s[1]])
        if key not in seen:seen.add(key);result.append(s)
    return result

def temporal(context,later=''):
    dates=list(RX['date'].finditer(context))
    if RX['damaged_date'].search(context):return 'uncertain'
    if not dates:return 'uncertain'
    printed=[m.group() for m in dates]
    opened=any(re.search(r'[-—–]\s*$',d) for d in printed)
    closed=any(re.search(r'\d\s*[-—–]\s*\d',d) for d in printed)
    if re.search(r'\bretired\b',context,re.I):return 'past'
    if opened:return 'mixed' if closed else 'current'
    last=printed[-1]
    if re.search(r'\d\s*[-—–]\s*\d',last):return 'past'
    single=int(last) if last.isdigit() and len(last)==2 else None
    later_years=[int(m.group()[:2]) for m in RX['date'].finditer(later) if re.match(r'^\d{2}(?!\d)',m.group())]
    return 'past' if single is not None and any(y>single for y in later_years) else 'uncertain'

def types(context):
    result=[]
    for pattern,label in [(r'\b(?:vis|visiting|guest|invited)\b','visiting'),(r'\bemer','emeritus'),(r'\badj','adjunct'),(r'\bconsult','consulting'),(r'\b(?:res\s*)?f[ec]l\b|\bfellow','research_fellow'),(r'\bteaching\s+(?:asst|assoc|fellow)','teaching_fellow'),(r'\bres(?:earch)?\s*(?:assoc|asst|scientist)','research')]:
        if re.search(pattern,context,re.I):result.append(label)
    return result or ['regular']

def extract_universities(entry,edition='1986',version='v07_role_spans'):
    if str(edition)!='1986':raise ValueError('1986 extractor only')
    if version not in VERSIONS:raise ValueError('Unknown rule version')
    text=entry.get('biography_text',entry.get('raw_text_adjusted',''))
    if 'biography_text' not in entry:entry=dict(entry,biography_text=text)
    ranges,flags,events=sections(entry,version);pred=[];by={};addresses=[]
    mail=next(((a,b) for a,b,k in events if k=='mailing_address'),None)
    mailing_evidence=None
    if mail:
        hi=next((a for a,b,k in events if a>=mail[1]),len(text))
        if version in VERSIONS[6:]:
            postal=re.search(r'\b\d{5}(?:-\d{4})?\b',text[mail[1]:hi])
            if postal:hi=mail[1]+postal.end()
            mailing_evidence=ev(entry,mail[1],hi)
        else:
            for a,b,qs in candidates(entry,mail[1],hi,version):addresses.append(dict(institution=ev(entry,a,b),qualifiers=qs,evidence=ev(entry,mail[1],hi),employment_established=False))
    for lo,hi,kind,heading in ranges:
        previous=None
        # Semicolons and colons introduce new employers or appointment sequences.
        for m in re.finditer(r'[^;:]+',text[lo:hi]):
            a,b=trim(text,lo+m.start(),lo+m.end());context=text[a:b]
            slots=candidates(entry,a,b,version)
            roles=[ev(entry,a+r.start(),a+r.end()) for r in RX['role'].finditer(context)]
            carry=version in VERSIONS[3:] and previous is not None
            if version in VERSIONS[2:]:
                if not roles and not carry:
                    if slots:flags.append(dict(reason='institution_without_employment_role',evidence=ev(entry,a,b)))
                    previous=None;continue
                if version in VERSIONS[4:]:
                    if re.search(r'\bhon(?:orary)?\s+(?:res\s+)?f[ec]l\b',context,re.I):
                        flags.append(dict(reason='honorary_fellowship_not_employment',evidence=ev(entry,a,b)));previous=None;continue
                    if carry and not roles and slots and context[:slots[0][0]-a].strip(' ,.&'):
                        flags.append(dict(reason='unrecognized_role_prevents_carry',evidence=ev(entry,a,b)));previous=None;continue
                    # A college-named place after a university in the same
                    # appointment is a possible campus/unit, not another employer.
                    first_uni=next((s for s in slots if re.search(r'\bUniv\b',text[s[0]:s[1]],re.I)),None)
                    if first_uni:slots=[s for s in slots if not (s[0]>first_uni[1] and re.match(r'College\s+',text[s[0]:s[1]],re.I))]
                if re.search(r'\bAT\s+(?:SCH|SCHOOL|DEPT|DEP|PHYSICS|LUNAR|LAB)',context,re.I) and not roles:
                    flags.append(dict(reason='at_unit_affiliation_without_appointment',evidence=ev(entry,a,b)));previous=None;continue
                training=RX['training'].search(context)
                if training and (version not in VERSIONS[5:] or not RX['strong_role'].search(context) or re.search(r'\b(?:hon(?:orary)?|trustee|by\s+courtesy)\b',context,re.I)):
                    flags.append(dict(reason='clinical_training_honorary_or_governance',evidence=ev(entry,a,b)));previous=None;continue
                if re.search(r'\b(?:grant|grantee|funding|res\s+participation)\b',context,re.I) and not RX['strong_role'].search(context):
                    flags.append(dict(reason='funding_or_participation_not_employment',evidence=ev(entry,a,b)));previous=None;continue
                # Named companies/associations stop carry. When a clear academic
                # marker appears before such a new employer, restrict to it.
                bad=RX['nonacademic'].search(context)
                if bad and not re.match(r'(?:NSF|NIH)\s+f[ec]l',context,re.I):
                    if not slots or not RX['strong_role'].search(context[:bad.start()]):previous=None;continue
                    slots=[s for s in slots if s[1]<=a+bad.start()]
            if not slots:
                if roles and (RX['strong_role'].search(context) or re.search(r'\b(?:univ|inst|hosp|lab|sch|col)\b',context,re.I)):flags.append(dict(reason='university_relationship_or_damaged_name_unresolved',evidence=ev(entry,a,b)))
                previous=None;continue
            for sa,sb,qs in slots:
                dates=[ev(entry,a+d.start(),a+d.end()) for d in RX['date'].finditer(context)]
                appointment=dict(institution=ev(entry,sa,sb),context=ev(entry,a,b),roles=roles,dates=dates,qualifiers=qs,
                    temporal_status=temporal(context,text[b:hi]),appointment_types=types(context),inherited_roles=previous['roles'] if carry and not roles else [])
                family='academic_institution_marker' if roles else 'bounded_role_carry'
                item=dict(institution=appointment['institution'],contexts=[appointment['context']],roles=roles,dates=dates,qualifiers=qs,appointments=[appointment],
                    temporal_status=appointment['temporal_status'],appointment_types=appointment['appointment_types'],resolved_years=[],rule_family=family,
                    trace=[dict(rule_id='1986_'+family,order=30 if carry and not roles else 20,section=kind,section_heading=heading,affected=appointment['context'],replacement=text[sa:sb])])
                key=norm(item['institution']['text'])
                if key in by:
                    p=by[key]
                    for k in ('contexts','roles','dates','qualifiers','appointments','trace'):p[k]+=item[k]
                    states={p['temporal_status'],item['temporal_status']}
                    p['temporal_status']='mixed' if 'mixed' in states or {'current','past'}<=states else 'current' if 'current' in states else 'uncertain' if 'uncertain' in states else 'past'
                    p['appointment_types']=sorted(set(p['appointment_types']+item['appointment_types']))
                else:by[key]=item;pred.append(item)
                previous=item
    if mailing_evidence:
        # Corroboration matches the already extracted literal institution. It
        # neither supplies an employer nor changes appointment timing.
        for p in pred:
            words=re.findall(r'\w+',p['institution']['text'])
            if not words:continue
            pat=r'(?<!\w)'+r'[\W_]+'.join(re.escape(w) for w in words)+r'(?!\w)'
            match=re.search(pat,mailing_evidence['text'],re.I)
            if match:
                a=mailing_evidence['start']+match.start();b=mailing_evidence['start']+match.end()
                addresses.append(dict(institution=ev(entry,a,b),qualifiers=[],evidence=mailing_evidence,employment_established=False,relationship='literal_corroboration_only'))
        if not addresses:addresses.append(dict(institution=None,qualifiers=[],evidence=mailing_evidence,employment_established=False,relationship='address_only_institution_boundary_unresolved'))
    for h in addresses:
        for p in pred:
            if h['institution'] and norm(h['institution']['text'])==norm(p['institution']['text']):p.setdefault('supporting_addresses',[]).append(h['institution'])
    if not text.strip():flags.append(dict(reason='missing_biography',evidence=None))
    return dict(affiliations=pred,reported_address_affiliations=addresses,review_flags=flags)

def rule_specification():
    return dict(versions=list(VERSIONS),patterns=PATTERNS,
        processing_order=['identify literal section labels','bound Prof Exp and Concurrent Pos separately','exclude neighboring entry and later non-employment sections','scan literal institution slots','require employment context','separate unit and campus qualifiers','bounded role carry','consolidate exact normalized repeats','corroborate with separate mailing-address evidence'],
        preprocessing='No source-text modifications. NFKC/casefold/alphanumeric normalization for literal duplicate scoring only; evidence uses original codepoint offsets, end exclusive.',
        exclusions='No institution dictionaries, person-specific overrides, name spelling repair, canonical IDs or location lookup. Address alone cannot establish employment or reopen a closed appointment.')

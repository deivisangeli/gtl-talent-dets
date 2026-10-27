"""Build observed college and fixed-2020 county faculty panels from archived EBN.

Python standard library plus R packages sf and data.table; no network on rebuild.
All source data and outputs live under TALENT_DETS_DATA_DIR, not in Git.
"""
from __future__ import annotations
import argparse, collections, csv, difflib, hashlib, html, json, os, re, shutil, subprocess, unicodedata
from pathlib import Path
from datetime import datetime, timezone

HERE = Path(__file__).resolve().parent
STATES = dict(zip('AL AK AZ AR CA CO CT DE DC FL GA HI ID IL IN IA KS KY LA ME MD MA MI MN MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV WI WY PR'.split(),
    ['Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','District of Columbia','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming','Puerto Rico']))
FIELDS = ['faculty_headcount','faculty_male','faculty_female','faculty_fte','faculty_fte_male','faculty_fte_female',
          'faculty_preparatory','faculty_collegiate','faculty_professional','faculty_resident_instruction_fte','faculty_extension_instruction_fte',
          'faculty_resident_instruction_headcount','faculty_extension_instruction_headcount','institutional_staff_headcount','institutional_staff_fte']
AGG = ['faculty_headcount','faculty_fte']


def norm(value):
    value = unicodedata.normalize('NFKD', value or '').casefold().replace('&',' and ')
    value = ''.join(c for c in value if not unicodedata.combining(c))
    aliases = {'univ':'university','col':'college','inst':'institute','st':'saint','mt':'mount','technol':'technology'}
    return ' '.join(aliases.get(t,t) for t in re.findall(r'[^\W_]+',value) if t != 'the')


def digest(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda:f.read(2**20),b''): h.update(block)
    return h.hexdigest()


def read(path):
    with path.open(encoding='utf-8-sig',newline='') as f: return list(csv.DictReader(f))


def write(path, rows, fields=None):
    path.parent.mkdir(parents=True,exist_ok=True)
    rows = list(rows)
    if fields is None: fields = list(dict.fromkeys(k for row in rows for k in row))
    with path.open('w',encoding='utf-8-sig',newline='') as f:
        w = csv.DictWriter(f,fieldnames=fields,extrasaction='ignore'); w.writeheader()
        for row in rows:
            w.writerow({k: json.dumps(v,ensure_ascii=False,sort_keys=True) if isinstance(v,(list,dict)) else v for k,v in row.items()})


def jsave(path, obj):
    path.parent.mkdir(parents=True,exist_ok=True)
    path.write_text(json.dumps(obj,ensure_ascii=False,indent=2),encoding='utf-8')


def numeric(text):
    text = (text or '').strip().replace('\u00a0','')
    # No coercion of dashes, footnoted strings, ranges, blanks, or ditto marks.
    if not re.fullmatch(r'(?:\d{1,3}(?:,\d{3})+|\d+)(?:\.\d+)?',text): return None
    value = float(text.replace(',',''))
    return int(value) if value.is_integer() else value


def state_code(text):
    return next((code for code,name in STATES.items() if norm(text) in {norm(code),norm(name)}),'')


def name_parts(name):
    qualifiers = re.findall(r'\(([^)]*)\)',name)
    department = any(re.search(r'arts|sciences|engineering|department|collegiate|medicine|professional',q,re.I) for q in qualifiers)
    core = re.sub(r'\(([^)]*)\)', lambda m: '' if re.search(r'arts|sciences|engineering|department|collegiate|medicine|professional',m[1],re.I) else m[0],name)
    return core.strip(' ,;'), department


def scope(name, kind, source_type):
    text = norm(name); typ = norm(source_type)
    if not text or re.match(r'^(total|totals|grand total|all institutions|united states)$',text): return 'excluded','aggregate_or_blank'
    if 'junior' in typ or 'junior college' in text: return 'included','junior_college'
    if 'teachers college' in typ or 'teacher college' in typ or re.search(r'\bteachers? college\b',text):
        return 'included','teachers_college'
    if re.search(r'\bnormal (school|institute)\b',text) and 'college' not in text and 'university' not in text:
        return 'excluded','standalone_normal_school'
    # Mixed source group is not evidence of the degree-granting status of a normal school.
    if 'normal school' in typ and 'college' not in text and 'university' not in text:
        return 'excluded','standalone_normal_school'
    professional = re.search(r'\b(theological|theology|divinity|medical|medicine|dental|dentistry|pharmacy|pharmaceutical|law school|college of law|business college|commercial college|chiropractic|osteopath\w*|veterinary|bible|biblical|rabbin\w*|talmud\w*|optometry|conservatory|school of music|school of nursing|school of social work|institute of religion|institute of podiatry)\b',text)
    if professional and not re.search(r'\b(university of|university department)\b',text):
        return 'excluded','standalone_professional_school'
    if 'normal school' in typ and 'teachers college' not in typ:
        return 'excluded','standalone_normal_school'
    return 'included',kind


def load_observations(raw, specs):
    observations=[]; cells=[]; table_audit=[]
    for spec in specs:
        table_audit.append({k:v for k,v in spec.items() if k not in ['headers','recipes','components']})
        if not spec['included']: continue
        with (raw/spec['csv_file']).open(encoding='utf-8-sig',newline='') as f:
            reader=csv.DictReader(f)
            assert reader.fieldnames == spec['headers'],f"Schema changed: {spec['csv_file']}"
            for row_number,row in enumerate(reader,2):
                original=row[spec['name_column']].strip(); core,department=name_parts(original)
                source_city=row.get(spec['city_column'],'').strip() if spec['city_column'] else ''
                source_state=row.get(spec['state_column'],'').strip() if spec['state_column'] else ''
                status,kind=scope(core,spec['institution_type'],row.get('Type',''))
                oid='obs_'+hashlib.sha256(f"{spec['csv_file']}|{row_number}".encode()).hexdigest()[:18]
                flags=[]
                if department: flags.append('department_only_report')
                if 'unverified' in spec['year_basis']: flags.append('observation_date_unverified_volume_year')
                ob=dict(observation_id=oid, source_csv=spec['csv_file'],source_row=row_number,
                        source_url=spec['source_url'],source_heading=spec['heading'],
                        source_family=spec['source_family'],source_priority=spec['source_priority'],
                        year=spec['year'],reported_period=spec['reported_period'],year_basis=spec['year_basis'],
                        publication_year=spec['publication_year'],college_name_original=original,college_name_core=core,
                        source_citation=spec.get('source_citation',''),
                        table_period_evidence=spec.get('period_evidence',''),
                        city_original=source_city,state_original=source_state,state=state_code(source_state),
                        city=source_city, source_institution_type=row.get('Type',''),institution_type=kind,
                        inclusion_status=status,department_only=department,measure_basis=spec['measure_basis'],
                        measure_definition=spec['definition'],department_coverage=spec['coverage'],
                        source_notes=row.get('Notes',''), raw_row_json=row,flags=flags)
                opening=next((row[c] for c in ['Year of first opening','Year of Opening','Founded','Date of Organization','Date of organization','Date of organization of collegiate department'] if row.get(c)), '')
                opening_year=re.search(r'\b((?:17|18|19)\d{2})\b',opening)
                ob['reported_opening_year']=int(opening_year[1]) if opening_year else None
                values={key:numeric(value) for key,value in row.items()}
                for field in FIELDS:
                    result=None;terms=[]
                    for recipe in spec['recipes'].get(field,[]):
                        if all(values[c] is not None for c in recipe):
                            result=sum(values[c] for c in recipe);terms=recipe;break
                    ob[field]=result
                    ob[field+'_terms']=terms
                    if result is not None:
                        for column in terms:
                            cells.append(dict(observation_id=oid, output_field=field, source_csv=spec['csv_file'],source_row=row_number,
                                              source_column=column,raw_value=row[column],parsed_value=values[column],coefficient=1))
                if spec['measure_basis']=='headcount' and ob['faculty_headcount'] is not None and ob['faculty_headcount']%1:
                    ob['faculty_headcount']=None;flags.append('fractional_headcount_quarantined')
                if any(ob[field] is None and spec['recipes'].get(field) for field in AGG): flags.append('missing_or_ambiguous_count_component')
                ob['components_json']={column:dict(raw=row[column],numeric=values[column]) for column in spec['components']}
                observations.append(ob)
    return observations,cells,table_audit


def apply_observation_reviews(observations,cur):
    path=cur/'observation_reviews.csv'
    if not path.exists(): return []
    index={o['observation_id']:o for o in observations}; applied=[]
    for review in read(path):
        assert review['observation_id'] in index,review
        ob=index[review['observation_id']]
        if review['action']=='quarantine_count':
            assert review['field'] in FIELDS
            prior=ob[review['field']];ob[review['field']]=None
            ob['flags'].append('reviewed_source_count_anomaly_quarantined')
            applied.append(dict(**review,quarantined_value=prior))
        elif review['action']=='quarantine_county':
            ob['county_review_quarantine']=True;ob['flags'].append('reviewed_identity_or_location_unresolved')
            applied.append(review)
        else: raise ValueError('Unknown observation review action')
    return applied


def resolve_names(root, observations, cur):
    # Reference canonical/alternative names are accepted only on exact name,
    # exact state and exact reported locality. Coordinates are not used here.
    refs_path=root/'output/amws/education_geolocated/reference_institutions.csv'
    aliases_path=refs_path.with_name('reference_aliases.csv')
    names={norm(o['college_name_core']) for o in observations}
    refs={r['id']:r for r in read(refs_path) if r['country']=='US' and r['type']=='education'}
    alias_index=collections.defaultdict(set)
    with aliases_path.open(encoding='utf-8-sig',newline='') as f:
        for a in csv.DictReader(f):
            if a['id'] in refs and norm(a['alias']) in names and a['source'] in {'openalex_canonical','openalex_alternative'}:
                alias_index[norm(a['alias'])].add(a['id'])
    overrides=read(cur/'institution_overrides.csv') if (cur/'institution_overrides.csv').exists() else []
    identity_rows={}
    # Missing localities in the research sample can be recovered from an
    # independent report of the exact reference institution in the same year,
    # or matching localities in reports on both sides of that year. Counts are
    # never interpolated. Single-sided extrapolation is prohibited.
    archive_locations=collections.defaultdict(list)
    for ob in observations:
        for candidate in alias_index[norm(ob['college_name_core'])]:
            ref=refs[candidate]
            if ob['state'] and ob['city'] and state_code(ref['region'])==ob['state'] and norm(ref['city'])==norm(ob['city']):
                archive_locations[candidate].append(ob)
    for ob in observations:
        if ob['city'] or ob['source_family']!='research_sample': continue
        ids=alias_index[norm(ob['college_name_core'])]
        if len(ids)!=1: continue
        candidates=archive_locations[next(iter(ids))]
        same=[r for r in candidates if r['year']==ob['year']]
        if same and len({(r['state'],norm(r['city'])) for r in same})==1:
            selected=same[0];method='same_year_independent_institution_report'
        else:
            before=sorted([r for r in candidates if r['year']<ob['year']],key=lambda r:-r['year'])
            after=sorted([r for r in candidates if r['year']>ob['year']],key=lambda r:r['year'])
            if not before or not after or (before[0]['state'],norm(before[0]['city']))!=(after[0]['state'],norm(after[0]['city'])): continue
            selected=before[0];method='same_historical_locality_in_bracketing_reports'
        ob['state']=selected['state'];ob['city']=selected['city']
        ob['locality_support_observation']=selected['observation_id'];ob['locality_support_method']=method
        ob['flags'].append('locality_supported_by_independent_archive_report')
    for ob in observations:
        original_name=norm(ob['college_name_core']); city=norm(ob['city']); state=ob['state']
        key=(original_name,state,city)
        candidates=sorted(alias_index[original_name])
        accepted=[i for i in candidates if state and city and state_code(refs[i]['region'])==state and norm(refs[i]['city'])==city]
        reference_id=accepted[0] if len(accepted)==1 else ''
        token=f"{reference_id or original_name}|{state}|{city}"
        ob['college_id']='ebn_'+hashlib.sha256(token.encode()).hexdigest()[:16]
        ob['college_name_standardized']=refs[reference_id]['name'] if reference_id else ob['college_name_core']
        ob['identity_method']='exact_reference_alias_same_reported_locality' if reference_id else 'exact_source_name_state_locality'
        ob['reference_id']=reference_id
        if len(candidates)>1 and not reference_id: ob['flags'].append('ambiguous_reference_identity_not_merged')
        for override in overrides:
            if norm(override['alias'])==original_name and (not override['state'] or override['state']==state) and (not override['city'] or norm(override['city'])==city) and int(override['year_start'])<=ob['year']<=int(override['year_end']):
                ob['college_id']=override['college_id']; ob['college_name_standardized']=override['standard_name']
                ob['identity_method']='reviewed_override';ob['identity_evidence']=override['evidence']
                if override.get('state'): ob['state']=override['state']
                if override.get('historical_city'): ob['city']=override['historical_city']
        key=(*key,ob['college_id'])
        years=identity_rows.get(key,{}).get('observed_years',[])+[ob['year']]
        identity_rows[key]=dict(college_name_original=ob['college_name_core'],state=state,city=city,college_id=ob['college_id'],
                                college_name_standardized=ob['college_name_standardized'],identity_method=ob['identity_method'],
                                reference_id=reference_id,reference_candidates=candidates,evidence=ob.get('identity_evidence',''),
                                reference_source=str(aliases_path),observed_years=sorted(set(years)))
    # A historical report may list an internal scientific/agricultural school
    # as a separate land-grant line as well as reporting its parent college.
    # Reviewed parent-unit links prevent adding overlapping institutional totals.
    units_path=cur/'parent_unit_overrides.csv'
    if units_path.exists():
        for unit in read(units_path):
            for child in observations:
                if (norm(child['college_name_core'])!=norm(unit['alias']) or child['state']!=unit['state']
                    or norm(child['city'])!=norm(unit['city']) or not int(unit['year_start'])<=child['year']<=int(unit['year_end'])): continue
                parents=[o for o in observations if o['year']==child['year'] and o['state']==child['state']
                         and norm(o['city'])==norm(child['city']) and norm(o['college_name_core'])==norm(unit['parent_alias'])]
                assert parents and len({o['college_id'] for o in parents})==1,unit
                child['college_id']=parents[0]['college_id'];child['college_name_standardized']=parents[0]['college_name_standardized']
                child['identity_method']='reviewed_parent_unit_same_year';child['identity_evidence']=unit['evidence']
                child['department_only']=True;child['flags'].append('internal_unit_not_added_to_parent_college')
    final={}
    for ob in observations:
        key=(norm(ob['college_name_core']),ob['state'],norm(ob['city']),ob['college_id'])
        years=final.get(key,{}).get('observed_years',[])+[ob['year']]
        final[key]=dict(college_name_original=ob['college_name_core'],state=ob['state'],city=ob['city'],college_id=ob['college_id'],
                        college_name_standardized=ob['college_name_standardized'],identity_method=ob['identity_method'],
                        reference_id=ob['reference_id'],reference_candidates=sorted(alias_index[norm(ob['college_name_core'])]),
                        evidence=ob.get('identity_evidence',''),reference_source=str(aliases_path),observed_years=sorted(set(years)))
    return list(final.values())


def locality_candidates(root, observations, out):
    wanted={(o['state'],norm(o['city'])) for o in observations if o['state'] and o['city'] and o['inclusion_status']=='included'}
    wanted_by_state=collections.defaultdict(set)
    for state,city in wanted: wanted_by_state[state].add(city)
    target=out/'geonames_candidates.csv'
    source=root/'input/geonames_US.txt'
    cache=out/'geonames_candidates_manifest.json'
    fingerprint=dict(algorithm_version=2,source_sha256=digest(source),wanted=[list(pair) for pair in sorted(wanted)])
    if target.exists() and cache.exists() and json.loads(cache.read_text(encoding='utf-8'))==fingerprint:
        return target
    rows=[]
    with source.open(encoding='utf-8-sig',newline='') as f:
        for fields in csv.reader(f,delimiter='\t'):
            if len(fields)<19 or fields[6]!='P': continue
            state=fields[10]
            names={norm(fields[1]),norm(fields[2])} | {norm(n) for n in fields[3].split(',') if n}
            wanted_here=names & wanted_by_state[state]
            if not wanted_here: continue
            for city in wanted_here:
                rows.append(dict(state=state,city_key=city,geonameid=fields[0],place_name=fields[1],
                                 latitude=fields[4],longitude=fields[5],feature_code=fields[7],
                                 admin_county=fields[11],population=fields[14]))
    write(target,rows,['state','city_key','geonameid','place_name','latitude','longitude','feature_code','admin_county','population'])
    jsave(cache,fingerprint)
    return target


def assign_geography(root, observations, out, cur, counties):
    candidates=locality_candidates(root,observations,out)
    spatial=out/'geonames_candidates_county2020.csv'
    shape=root/'raw/tigris/cb_2020_us_county_20m.shp'
    spatial_cache=out/'spatial_cache_manifest.json'
    fingerprint=dict(candidates=digest(candidates),geography_code=digest(HERE/'assign_counties.R'),
                     shape={p.name:digest(p) for p in shape.parent.glob(shape.stem+'.*')})
    if not (spatial.exists() and spatial_cache.exists() and json.loads(spatial_cache.read_text(encoding='utf-8'))==fingerprint):
        rscript=shutil.which('Rscript')
        if not rscript: raise RuntimeError('Rscript with sf and data.table is required')
        subprocess.run([rscript,str(HERE/'assign_counties.R'),str(candidates),str(shape),str(spatial)],check=True)
        jsave(spatial_cache,fingerprint)
    index=collections.defaultdict(list)
    for row in read(spatial): index[row['state'],row['city_key']].append(row)
    overrides=read(cur/'geography_overrides.csv') if (cur/'geography_overrides.csv').exists() else []
    known={r['GEOID'] for r in counties}
    locrows={}
    for ob in observations:
        state=ob['state']; city=norm(ob['city']); choices=index[state,city]
        geos={r['polygon_GEOID'] for r in choices if r['polygon_GEOID'] in known and r['polygon_matches']=='1'}
        geoid='';method='unresolved_missing_or_ambiguous_locality';evidence=''; point=None
        # A unique exact populated-place name and state identifies the reported
        # locality. We retain the locality-level precision explicitly.
        if len(geos)==1 and not any(r['polygon_matches']!='1' for r in choices):
            geoid=next(iter(geos));method='historical_reported_locality_exact_geonames_polygon2020'
            point=next(r for r in choices if r['polygon_GEOID']==geoid)
            evidence='Historical locality: '+ob['source_url']+'; present locality point: GeoNames '+point['geonameid']
        if (state,city) in {('NY','new york'),('NY','new york city'),('MO','kansas city'),('KS','kansas city'),('GA','atlanta'),('OH','columbus')}:
            geoid='';method='unresolved_known_multi_county_city'
        if city.startswith('near '): geoid='';method='unresolved_near_city_is_not_city_location'
        if state=='DC':
            geoid='11001';method='unique_2020_county_for_reported_DC';evidence='Historical source reports District of Columbia; 2020 Census roster has one county equivalent.'
        # Known multi-campus labels require a reviewed allocation, even if the
        # report prints the headquarters city. This is intentionally conservative.
        name=norm(ob['college_name_core'])
        system_risk=(('university of california'==name and ob['year']>=1919) or
                     ('cornell university'==name and ob['year']>=1898) or
                     ('university of texas'==name and ob['year']>=1891) or
                     ('new york university'==name and ob['year']>=1894) or
                     (name in {'harvard university','tufts college','tufts university','leland stanford junior university','stanford university','university of illinois'}
                      and (ob['faculty_professional'] or 0)>0))
        if system_risk:
            geoid='';method='unresolved_possible_multi_county_institutional_total'
            ob['flags'].append('possible_multi_county_total')
        if ob.get('historical_location_evidence'):
            evidence=ob['historical_location_evidence']+'; '+evidence
        for override in overrides:
            if (override['alias']=='*' or norm(override['alias'])==name) and (not override['state'] or override['state']==state) and (not override['city'] or norm(override['city'])==city) and int(override['year_start'])<=ob['year']<=int(override['year_end']):
                geoid=override['GEOID'];method='reviewed_historical_location';evidence=override['evidence']
                assert geoid in known,override
        if ob.get('county_review_quarantine'):
            geoid='';method='reviewed_identity_or_historical_locality_unresolved'
        ob.update(GEOID=geoid,county_geography_year=2020,geography_method=method,geography_evidence=evidence,
                  locality_latitude=point['latitude'] if point else None,locality_longitude=point['longitude'] if point else None,
                  geography_precision='reported_locality_not_campus_coordinate' if geoid and method!='reviewed_historical_location' else method)
        if not geoid and ob['inclusion_status']=='included': ob['flags'].append('county_unallocated')
        locrows[(ob['college_id'],state,city,geoid,method)]=dict(college_id=ob['college_id'],college_name=ob['college_name_core'],
              state=state,city=ob['city'],GEOID=geoid,method=method,evidence=evidence,candidate_counties=sorted(geos))
    return list(locrows.values())


def choose_panel(observations):
    grouped=collections.defaultdict(list)
    for ob in observations:
        if ob['inclusion_status']=='included' and 1850<=ob['year']<=1950: grouped[ob['college_id'],ob['year']].append(ob)
    panel=[]; conflicts=[]
    for (college,year),obs in sorted(grouped.items()):
        def ordering(o): return (o['department_only'],o['source_priority'],o['observation_id'])
        overall=sorted(obs,key=lambda o:(not any(o[f] is not None for f in AGG),*ordering(o)))[0]
        row={k:v for k,v in overall.items() if k not in ['raw_row_json','components_json'] and not k.endswith('_terms')}
        row['all_observation_ids']=[o['observation_id'] for o in obs];row['n_competing_observations']=len(obs)
        row['reported_faculty_components_json']={o['observation_id']:o['components_json'] for o in obs}
        row['flags']=sorted(set(flag for o in obs for flag in o['flags']))
        for field in AGG:
            available=sorted([o for o in obs if o[field] is not None],key=ordering)
            chosen=available[0] if available else overall
            row[field]=chosen[field];row[field+'_source_observation']=chosen['observation_id'] if available else ''
            row[field+'_definition']=chosen['measure_definition'] if available else ''
            row[field+'_scope']=chosen['department_coverage'] if available else ''
            row[field+'_department_only']=chosen['department_only'] if available else None
            row[field+'_GEOID']=chosen['GEOID'] if available else ''
            row[field+'_geography_method']=chosen['geography_method'] if available else ''
            row[field+'_source_family']=chosen['source_family'] if available else ''
            distinct=sorted({o[field] for o in available})
            if len(distinct)>1:
                row['flags'].append('conflicting_'+field)
                conflicts.append(dict(college_id=college,year=year,field=field,chosen_observation=chosen['observation_id'],
                                      chosen_value=chosen[field],competing_values=distinct,observation_ids=[o['observation_id'] for o in available]))
        for field in ['faculty_preparatory','faculty_collegiate','faculty_professional','faculty_resident_instruction_fte','faculty_extension_instruction_fte']:
            available=sorted([o for o in obs if o[field] is not None],key=ordering)
            chosen=available[0] if available else overall
            row[field]=chosen[field]
            row[field+'_source_observation']=chosen['observation_id'] if available else ''
        # Sex-specific values must come from the chosen measure's source, not a
        # different report whose institutional boundary or period may differ.
        for field,sex_fields in [('faculty_headcount',['faculty_male','faculty_female']),('faculty_fte',['faculty_fte_male','faculty_fte_female'])]:
            selected=next((o for o in obs if o['observation_id']==row[field+'_source_observation']),None)
            if selected is None: selected=overall
            for sex in sex_fields:
                row[sex]=selected[sex]
                row[sex+'_source_observation']=selected['observation_id'] if selected[sex] is not None else ''
        row['county_eligible']=True
        panel.append(row)
    # Detect close names in the same county/year before aggregating sources.
    # Exact normalizations are handled above; near matches are never auto-merged.
    buckets=collections.defaultdict(list); possible=[]
    for row in panel:
        if row['GEOID']: buckets[row['GEOID'],row['year']].append(row)
    for (geoid,year),rows in buckets.items():
        for i,a in enumerate(rows):
            for b in rows[i+1:]:
                if a['college_id']==b['college_id']: continue
                na,nb=norm(a['college_name_core']),norm(b['college_name_core'])
                if min(len(na),len(nb))<12: continue
                score=difflib.SequenceMatcher(None,na,nb).ratio()
                if score>=.86:
                    # National/land-grant overlap is a plausible duplicate;
                    # distinct women's/men's institutions remain distinct.
                    if a['source_family']==b['source_family'] or ('women' in na)!=('women' in nb): continue
                    possible.append(dict(year=year,GEOID=geoid,college_id_a=a['college_id'],name_a=a['college_name_core'],
                                         college_id_b=b['college_id'],name_b=b['college_name_core'],similarity=round(score,3)))
                    for row in (a,b): row['county_eligible']=False;row['flags'].append('possible_duplicate_cross_source_unallocated')
    return panel,conflicts,possible


def aggregate(panel,counties):
    groups=collections.defaultdict(list); year_sources=collections.defaultdict(set)
    for row in panel:
        year_sources[row['year']].add(row['source_family'])
        if row['GEOID']: groups[row['GEOID'],row['year']].append(row)
    output=[]
    for county in counties:
        for year in range(1850,1951):
            obs=groups[county['GEOID'],year]
            eligible=[r for r in obs if r['county_eligible']]
            row=dict(GEOID=county['GEOID'],county_name=county['COUNTYNAME'],state=county['STATE'],year=year,
                     county_geography_year=2020,n_colleges_reported=len(obs),
                     n_colleges_duplicate_review_excluded=sum(not r['county_eligible'] for r in obs),
                     reporting_scope='sample_only' if year_sources[year]=={'research_sample'} else
                         'institution_tables_available' if year_sources[year] else 'no_source_observations',
                     coverage_complete=False)
            for field in AGG:
                usable=[r for r in eligible if r[field] is not None and r[field+'_GEOID']==county['GEOID']]
                row[field]=sum(r[field] for r in usable) if usable else None
                row['n_colleges_with_'+field]=len(usable)
                row['n_colleges_missing_'+field]=sum(r[field] is None for r in eligible)
                row['n_department_only_'+field]=sum(bool(r[field+'_department_only']) for r in usable)
            row['coverage_status']='observed_source_sum_partial_universe' if any(row[f] is not None for f in AGG) else 'unobserved_not_zero'
            output.append(row)
    return output


def validate(raw,raw_checksums,observations,cells,panel,county_panel,counties):
    checks=[]
    def check(name,ok,detail=''):
        checks.append(dict(check=name,passed=bool(ok),detail=detail))
        if not ok: raise AssertionError(f'{name}: {detail}')
    check('raw_inputs_unchanged',all(digest(raw/rel)==sha for rel,sha in raw_checksums.items()))
    check('unique_college_year',len({(r['college_id'],r['year']) for r in panel})==len(panel))
    check('unique_county_year',len({(r['GEOID'],r['year']) for r in county_panel})==len(county_panel))
    known={r['GEOID'] for r in counties}
    check('valid_county_ids',all(re.fullmatch(r'\d{5}',r['GEOID']) and r['GEOID'] in known for r in county_panel))
    check('complete_county_year_grid',len(county_panel)==len(counties)*101)
    check('nonnegative_finite_counts',all(r[f] is None or (r[f]>=0 and r[f]<float('inf')) for r in panel+county_panel for f in AGG))
    check('headcounts_are_integers',all(r['faculty_headcount'] is None or r['faculty_headcount']%1==0 for r in panel))
    check('only_included_institutions',all(r['inclusion_status']=='included' and r['institution_type'] not in ['standalone_normal_school','standalone_professional_school'] for r in panel))
    obs={o['observation_id']:o for o in observations}
    terms=collections.defaultdict(list)
    for cell in cells: terms[cell['observation_id'],cell['output_field']].append(cell)
    for row in panel:
        for field in AGG:
            if row[field] is None: continue
            oid=row[field+'_source_observation'];check_value=sum(t['parsed_value'] for t in terms[oid,field])
            check('source_terms_'+oid+'_'+field, abs(check_value-row[field])<1e-9)
            if field=='faculty_headcount':
                assert obs[oid]['measure_basis']=='headcount','FTE/staff contamination in headcount'
    reconciliation=[]
    for year in range(1850,1951):
        for field in AGG:
            college_total=sum(r[field] for r in panel if r['year']==year and r[field] is not None)
            allocated=sum(r[field] for r in county_panel if r['year']==year and r[field] is not None)
            unallocated=sum(r[field] for r in panel if r['year']==year and r[field] is not None and
                            (not r['county_eligible'] or not r[field+'_GEOID']))
            # A source-specific county must agree with the displayed college row.
            mismatched=[r for r in panel if r['year']==year and r[field] is not None and r[field+'_GEOID'] and r[field+'_GEOID']!=r['GEOID']]
            check('county_source_alignment_'+str(year)+'_'+field,not mismatched)
            check('reconciliation_'+str(year)+'_'+field,abs(college_total-allocated-unallocated)<1e-8)
            reconciliation.append(dict(year=year,measure=field,college_total=college_total,county_total=allocated,unallocated_total=unallocated,
                                       difference=college_total-allocated-unallocated))
    # Collapse the per-cell evidence checks into one concise published result.
    cells_ok=[c for c in checks if c['check'].startswith('source_terms_')]
    checks=[c for c in checks if not c['check'].startswith('source_terms_')]
    checks.append(dict(check='all_selected_counts_reconcile_to_source_cells',passed=True,detail=f'{len(cells_ok)} selected measures'))
    check('no_count_without_reporting_college',all(r[f] is None or r['n_colleges_with_'+f]>0 for r in county_panel for f in AGG))
    return checks,reconciliation


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',str(Path.home()/'Globtalent Dropbox/gtl_talent_dets'))))
    args=parser.parse_args();root=args.data_root.resolve()
    raw=root/'Data/raw/faculty_data';inter=root/'Data/intermediary/faculty_data';out=root/'Data/processed/faculty_data';cur=inter/'curation'
    for p in [inter,out,cur]: p.mkdir(parents=True,exist_ok=True)
    specs=json.loads((HERE/'source_table_mapping.json').read_text(encoding='utf-8'))['tables']
    raw_files=[p for p in raw.rglob('*') if p.is_file()]
    raw_checksums={str(p.relative_to(raw)):digest(p) for p in raw_files}
    county_ref=root/'output/national_county2020.txt'
    with county_ref.open(encoding='utf-8-sig',newline='') as f: counties=list(csv.DictReader(f,delimiter='|'))
    for county in counties: county['GEOID']=county['STATEFP'].zfill(2)+county['COUNTYFP'].zfill(3)
    print('Extracting source observations',flush=True)
    observations,cells,table_audit=load_observations(raw,specs)
    from faculty_corrections import apply_corrections,apply_landgrant_locations,apply_boundary_reviews
    cells,unit_reviews,unit_audit=apply_corrections(observations,cells,specs,numeric)
    apply_landgrant_locations(observations,cur)
    apply_boundary_reviews(observations,cur)
    applied_reviews=apply_observation_reviews(observations,cur)
    print(f'{len(observations)} observations; resolving exact institution identities',flush=True)
    identities=resolve_names(root,observations,cur)
    print('Checking reported historical localities against fixed 2020 counties',flush=True)
    geography=assign_geography(root,observations,inter,cur,counties)
    panel,conflicts,duplicates=choose_panel(observations)
    county_panel=aggregate(panel,counties)
    print(f'{len(panel)} college-years; {len(county_panel)} county-years; validating',flush=True)
    checks,reconciliation=validate(raw,raw_checksums,observations,cells,panel,county_panel,counties)
    write(inter/'faculty_source_observations.csv',observations)
    write(inter/'faculty_calculation_terms.csv',cells)
    write(inter/'unit_date_review_ledger.csv',unit_reviews)
    write(inter/'table_unit_audit.csv',unit_audit)
    write(inter/'applied_observation_reviews.csv',applied_reviews,['observation_id','field','action','evidence','quarantined_value'])
    write(inter/'excluded_source_observations.csv',[o for o in observations if o['inclusion_status']=='excluded'])
    write(inter/'source_table_mapping.csv',table_audit)
    jsave(inter/'source_table_mapping.json',dict(tables=specs))
    write(inter/'institution_crosswalk.csv',identities)
    write(inter/'historical_location_crosswalk.csv',geography)
    write(inter/'college_faculty_panel.csv',panel)
    write(inter/'competing_counts.csv',conflicts,['college_id','year','field','chosen_observation','chosen_value','competing_values','observation_ids'])
    write(inter/'possible_duplicate_colleges.csv',duplicates,['year','GEOID','college_id_a','name_a','college_id_b','name_b','similarity'])
    unresolved=[r for r in panel if not r['GEOID'] or not r['county_eligible'] or all(r[f] is None for f in AGG)
                or any('unverified' in x or 'ambiguous' in x for x in r['flags'])]
    write(inter/'unresolved_cases.csv',unresolved)
    write(out/'county_faculty_panel.csv',county_panel)
    write(out/'reconciliation_by_year.csv',reconciliation)
    write(out/'validation.csv',checks)
    coverage=[]
    for year in range(1850,1951):
        rows=[r for r in panel if r['year']==year]
        cov=dict(year=year,n_colleges_reported=len(rows),n_colleges_geocoded=sum(bool(r['GEOID']) for r in rows),
                 n_counties_with_headcount=sum(r['year']==year and r['faculty_headcount'] is not None for r in county_panel))
        for f in AGG:
            available=[r for r in rows if r[f] is not None]
            cov['n_colleges_with_'+f]=len(available)
            cov['n_colleges_allocated_'+f]=sum(r['county_eligible'] and bool(r[f+'_GEOID']) for r in available)
            cov[f+'_allocation_share']=sum(r[f] for r in available if r['county_eligible'] and r[f+'_GEOID'])/sum(r[f] for r in available) if sum(r[f] for r in available) else None
        coverage.append(cov)
    write(out/'coverage_by_year.csv',coverage)
    typecoverage=[]
    for kind in sorted({r['institution_type'] for r in observations}):
        rows=[r for r in panel if r['institution_type']==kind]
        typecoverage.append(dict(institution_type=kind,college_years=len(rows),unique_colleges=len({r['college_id'] for r in rows}),
                                 with_headcount=sum(r['faculty_headcount'] is not None for r in rows),with_fte=sum(r['faculty_fte'] is not None for r in rows)))
    write(out/'coverage_by_institution_type.csv',typecoverage)
    spatial_coverage=[]
    for state in sorted({r['state'] for r in panel}):
        rows=[r for r in panel if r['state']==state]
        spatial_coverage.append(dict(state=state,college_years=len(rows),county_allocated=sum(bool(r['GEOID']) and r['county_eligible'] for r in rows),
                                     headcount_observed=sum(r['faculty_headcount'] or 0 for r in rows),
                                     headcount_county_allocated=sum(r['faculty_headcount'] or 0 for r in rows if r['county_eligible'] and r['faculty_headcount_GEOID'])))
    write(out/'coverage_by_state.csv',spatial_coverage)
    jumps=[]
    bycollege=collections.defaultdict(list)
    for row in panel: bycollege[row['college_id']].append(row)
    for id,rows in bycollege.items():
        for field in AGG:
            usable=sorted([r for r in rows if r[field] is not None],key=lambda r:r['year'])
            for a,b in zip(usable,usable[1:]):
                if a[field]>0 and (b[field]/a[field]>2.5 or b[field]/a[field]<.5):
                    jumps.append(dict(college_id=id,college_name=b['college_name_standardized'],measure=field,year_a=a['year'],year_b=b['year'],
                                      value_a=a[field],value_b=b[field],ratio=b[field]/a[field]))
    write(inter/'large_changes_review.csv',jumps,['college_id','college_name','measure','year_a','year_b','value_a','value_b','ratio'])
    founding_conflicts=[]
    for college,rows in bycollege.items():
        years=sorted({o['reported_opening_year'] for o in observations if o['college_id']==college and o['reported_opening_year'] is not None})
        if years and years[-1]-years[0]>=20:
            founding_conflicts.append(dict(college_id=college,college_name=rows[-1]['college_name_standardized'],reported_opening_years=years,
                                           review_status='potential_identity_collision_or_different_opening_definition'))
    write(inter/'founding_year_conflicts_review.csv',founding_conflicts,['college_id','college_name','reported_opening_years','review_status'])
    build=dict(built_at=datetime.now(timezone.utc).isoformat(),raw_input_sha256=raw_checksums,
               mapping_sha256=digest(HERE/'source_table_mapping.json'),code_sha256=digest(Path(__file__)),
               pipeline_code_sha256={p.name:digest(p) for p in sorted(HERE.iterdir()) if p.suffix in {'.py','.R'}},
               college_years=len(panel),unique_colleges=len(bycollege),county_years=len(county_panel),counties=len(counties),
               source_observations=len(observations),included_tables=sum(s['included'] for s in specs),
               college_years_with_headcount=sum(r['faculty_headcount'] is not None for r in panel),
               college_years_with_fte=sum(r['faculty_fte'] is not None for r in panel),
               college_years_county_allocated=sum(bool(r['GEOID']) and r['county_eligible'] for r in panel),
               conflicting_measure_cells=len(conflicts),possible_duplicate_pairs=len(duplicates),large_changes=len(jumps),
               founding_year_conflicts=len(founding_conflicts),quarantined_source_values=sum(r['action']=='quarantine_count' for r in applied_reviews),
               validation_passed=all(c['passed'] for c in checks),
               reference_inputs_sha256={str(p.relative_to(root)):digest(p) for p in [county_ref,root/'input/geonames_US.txt',
                   root/'output/amws/education_geolocated/reference_institutions.csv',root/'output/amws/education_geolocated/reference_aliases.csv']},
               curation_sha256={str(p.relative_to(root)):digest(p) for p in cur.glob('*.csv')},
               assumptions=['No count imputation or carry-forward. Missing is not zero.',
                            'Source-observed teaching totals only, not complete county faculty censuses.',
                            'Headcounts and full-time equivalents never substituted for one another.',
                            'Department-only reports flagged; overlapping department counts not summed.',
                            'Historical reported locality mapped with exact current populated-place point to fixed 2020 polygon; campus precision not claimed.',
                            'Potential multi-county institutional totals and ambiguous near-duplicate reports remain unallocated.'])
    jsave(out/'build_manifest.json',build)
    from panel_documentation import publish_documentation
    publish_documentation(inter,out,panel,county_panel,build)
    print(json.dumps({k:v for k,v in build.items() if k not in ['raw_input_sha256','assumptions']},indent=2),flush=True)
    return 0


if __name__=='__main__': raise SystemExit(main())

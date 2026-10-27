"""Reviewed unit/date corrections; raw archived cells are never rewritten."""
from __future__ import annotations
import collections, csv, json, re
from pathlib import Path

SURVEY_1935='https://files.eric.ed.gov/fulltext/ED543690.pdf#page=111'
SURVEY_1939='https://files.eric.ed.gov/fulltext/ED621969.pdf#page=372'
FIRST_FTE='https://files.eric.ed.gov/fulltext/ED543644.pdf#page=16'
LANDGRANT='https://files.eric.ed.gov/fulltext/ED543616.pdf#page=7'
EXTRA_FIELDS=['faculty_resident_instruction_headcount','faculty_extension_instruction_headcount',
 'institutional_staff_headcount','institutional_staff_fte','faculty_male_unit','faculty_female_unit']


def note_units(note, default='fte'):
    """Interpret the finite, reviewed faculty-unit footnote vocabulary."""
    note=note.lower().replace('full-time-basis','full-time basis')
    units={'male':default,'female':default}
    for clause in re.split(r';',note):
        if not re.search(r'faculty.*\b(?:not|no) reduced',clause): continue
        if re.search(r'\bfemale faculty',clause): units['female']='headcount'
        elif re.search(r'\bmale faculty',clause): units['male']='headcount'
        else: units={'male':'headcount','female':'headcount'}
    return units


def faculty_period(note,year,period):
    """Only whole-row or explicitly faculty-specific dates change faculty time."""
    for clause in re.split(r';',note):
        clause=clause.strip()
        clean=re.sub(r"[’']",'',clause).rstrip('.')
        match=re.fullmatch(r'(?:HBCU[,;:]?\s*)?(?:(?:All |These )?(?:statistics|figures)(?: are)? (?:from|of|for) (?:the year )?|For |From Report of the Commissioner of Education for (?:the )?year |Faculty numbers are (?:from|form) )((?:18|19)\d{2})(?:\s*[-–]\s*(\d{2,4}))?',clean,re.I)
        if match:
            start=int(match[1]);end=match[2];reported=match[1]+('-'+end if end else '')
            return start,reported,'reviewed_whole_row_or_faculty_date_footnote' if end else 'reviewed_prior_single_year_footnote_period_unspecified'
    return year,period,None


def apply_corrections(observations,cells,specs,numeric):
    corrected=[];reviews=[]
    # The sample abbreviates names and omits headcount exception footnotes.
    aliases={'university of iowa':'state university of iowa'}
    national={}
    for o in observations:
        if o['source_family']=='national_institution_table' and o['year'] in [1935,1939]:
            raw=o['raw_row_json'];keys=[k for k in raw if k.startswith(('Male faculty','Female faculty'))]
            if len(keys)==2:
                national[(o['year'],o['college_name_core'].lower())]=(o,keys)
    changed_ids=set()
    for o in observations:
        for f in EXTRA_FIELDS: o.setdefault(f,None)
        o['source_catalog_year']=o['year'];o['source_catalog_period']=o['reported_period']
        o['unit_review_evidence']='';o['unit_review_status']='archived_header_and_notes_reviewed'
        if o['year'] in [1935,1939]:
            original_year=o['year'];raw=o['raw_row_json']
            male=next((k for k in raw if k.startswith('Male faculty')),None)
            female=next((k for k in raw if k.startswith('Female faculty')),None)
            if male and female:
                units=note_units(o['source_notes']);note=o['source_notes']
                evidence=SURVEY_1935 if original_year==1935 else SURVEY_1939
                if o['source_family']=='research_sample':
                    name=aliases.get(o['college_name_core'].lower(),o['college_name_core'].lower())
                    support=national.get((original_year,name))
                    if support:
                        ref,cols=support
                        same=all(numeric(raw[k])==numeric(ref['raw_row_json'][c]) for k,c in zip([male,female],cols))
                        if same:
                            units=note_units(ref['source_notes']);note=ref['source_notes']
                            o['unit_support_observation']=ref['observation_id']
                            o['flags'].append('sample_units_inherited_from_matching_national_cells')
                            if original_year==1935 and 'headcount' in units.values():o['flags'].append('sample_fte_note_conflicts_with_original_exception')
                        else:o['flags'].append('sample_national_cell_disagreement_preserved')
                changed_ids.add(o['observation_id'])
                for field in ['faculty_headcount','faculty_fte','faculty_male','faculty_female','faculty_fte_male','faculty_fte_female']:
                    o[field]=None;o[field+'_terms']=[]
                for sex,column in [('male',male),('female',female)]:
                    unit=units[sex];field='faculty_'+('fte_' if unit=='fte' else '')+sex
                    o[field]=numeric(raw[column]);o[field+'_terms']=[column] if o[field] is not None else []
                    o['faculty_'+sex+'_unit']=unit
                for field,parts in [('faculty_headcount',['faculty_male','faculty_female']),('faculty_fte',['faculty_fte_male','faculty_fte_female'])]:
                    if all(o[p] is not None for p in parts):
                        o[field]=sum(o[p] for p in parts);o[field+'_terms']=[male,female]
                o['measure_basis']='headcount' if set(units.values())=={'headcount'} else 'fte' if set(units.values())=={'fte'} else 'mixed_sex_units'
                o['measure_definition']='Reported faculty/professional staff, including noninstructional professional roles where the survey includes them; sex-specific unit exceptions applied. Not a verified teaching-only measure.'
                o['department_coverage']='reported_faculty_professional_staff_scope'
                o['flags'].append('professional_staff_scope_may_include_noninstructional_roles')
                o['unit_review_evidence']=evidence;o['unit_review_status']='original_header_with_archived_row_footnote'
                if o['measure_basis']=='mixed_sex_units':o['flags'].append('mixed_sex_units_no_reported_combined_total')
                newyear,newperiod,basis=faculty_period(note,o['year'],o['reported_period'])
                if basis:
                    o.update(year=newyear,reported_period=newperiod,year_basis=basis);o['flags'].append('faculty_date_corrected_from_source_footnote')
                reviews.append(dict(observation_id=o['observation_id'],source_csv=o['source_csv'],source_row=o['source_row'],
                    catalog_year=original_year,year=o['year'],reported_period=o['reported_period'],male_unit=units['male'],female_unit=units['female'],
                    notes=o['source_notes'],support_observation=o.get('unit_support_observation',''),evidence=evidence))
        if o['source_csv'].startswith('csv/1950-land-grant'):
            changed_ids.add(o['observation_id']);raw=o['raw_row_json']
            for field in ['faculty_headcount','faculty_fte','faculty_male','faculty_female','faculty_fte_male','faculty_fte_female','faculty_resident_instruction_fte','faculty_extension_instruction_fte']:
                o[field]=None;o[field+'_terms']=[]
            recipes={'faculty_male':['Male resident instruction'],'faculty_female':['Female resident instruction'],
                'faculty_headcount':['Male resident instruction','Female resident instruction'],
                'faculty_resident_instruction_headcount':['Male resident instruction','Female resident instruction'],
                'faculty_extension_instruction_headcount':['Extension instruction – Male','Extension instruction – Female'],
                'institutional_staff_headcount':['Total full and part-time staff'],
                'institutional_staff_fte':['Total staff reduced to full-time equivalent']}
            for field,cols in recipes.items():
                values=[numeric(raw[c]) for c in cols];o[field]=sum(values) if all(v is not None for v in values) else None
                o[field+'_terms']=cols if o[field] is not None else []
            o['institutional_staff_fte_usable']='not reduced' not in o['source_notes'].lower()
            o['faculty_male_unit']=o['faculty_female_unit']='headcount'
            o['measure_basis']='headcount';o['department_coverage']='resident_instruction_only'
            o['measure_definition']='Full- and part-time resident-instruction headcount, excluding extension, research and general administration; functions are not added because they may overlap.'
            o['unit_review_evidence']=LANDGRANT;o['unit_review_status']='original_comparative_table_distinguishes_function_headcounts_and_total_staff_fte'
            o['flags']=[f for f in o['flags'] if f!='mixed_fte_headcount_source_footnote_quarantined']
            if not o['institutional_staff_fte_usable']:o['flags'].append('institutional_staff_fte_calibration_excluded_not_fully_converted')
        if 'americanalmanac' in o['source_csv']:
            o['department_coverage']='undergraduate_instructors_only'
            o['measure_definition']='Number of instructors connected with undergraduates, as defined by the original Almanac; not all institutional teaching staff.'
            o['flags'].append('undergraduate_instruction_scope_only')
        elif o['source_catalog_year'] not in [1935,1939]:
            newyear,newperiod,basis=faculty_period(o['source_notes'],o['year'],o['reported_period'])
            if basis:
                old=o['year'];o.update(year=newyear,reported_period=newperiod,year_basis=basis)
                o['flags'].append('faculty_date_corrected_from_source_footnote')
                reviews.append(dict(observation_id=o['observation_id'],source_csv=o['source_csv'],source_row=o['source_row'],
                    catalog_year=old,year=newyear,reported_period=newperiod,male_unit=o['measure_basis'],female_unit=o['measure_basis'],
                    notes=o['source_notes'],support_observation='',evidence=o['source_citation']+'; original footnote retained in archive'))
        if o['measure_basis']=='headcount':
            for sex in ['male','female']:
                if o.get('faculty_'+sex) is not None:o['faculty_'+sex+'_unit']='headcount'
        if o['year_basis']=='reported_single_year':o['flags'].append('single_year_label_academic_period_not_independently_established')
        if o['observation_id'] in changed_ids:
            for field in ['faculty_headcount','faculty_fte','faculty_male','faculty_female','faculty_fte_male','faculty_fte_female']+EXTRA_FIELDS[:4]:
                for col in o.get(field+'_terms',[]):
                    corrected.append(dict(observation_id=o['observation_id'],output_field=field,source_csv=o['source_csv'],source_row=o['source_row'],source_column=col,raw_value=o['raw_row_json'][col],parsed_value=numeric(o['raw_row_json'][col]),coefficient=1))
    cells=[c for c in cells if c['observation_id'] not in changed_ids]+corrected
    audit=[]
    for spec in specs:
        if not spec['included']:continue
        rows=[o for o in observations if o['source_csv']==spec['csv_file']]
        late=any(y in spec['csv_file'] for y in ['1934-36','1940-private','1940-public']) or spec['year'] in [1935,1939]
        evidence=LANDGRANT if '1950-land-grant' in spec['csv_file'] else (SURVEY_1935 if spec['year']==1935 else SURVEY_1939 if late else FIRST_FTE)
        if 'americanalmanac' in spec['csv_file']:evidence=spec.get('source_citation','')+'; original pages retained in faculty_data_source_reports'
        audit.append(dict(source_csv=spec['csv_file'],source_citation=spec.get('source_citation',''),source_url=spec['source_url'],table_period_evidence=spec.get('period_evidence',''),
            rows_reviewed=len(rows),unit_bases=sorted({o['measure_basis'] for o in rows}),faculty_years=sorted({o['year'] for o in rows}),
            scopes=sorted({o['department_coverage'] for o in rows}),evidence=evidence,
            review_level='Full archived headers/row notes plus primary-source methodology and relevant original headings; not a line-by-line re-transcription audit',
            unresolved='Single-year labels and incomplete/overlapping components retain explicit flags; see source observations and unit/date ledger.'))
    return cells,reviews,audit


def apply_landgrant_locations(observations,cur):
    path=cur/'landgrant_1949_locations.csv'
    if not path.exists():return
    with path.open(encoding='utf-8-sig',newline='') as f:locations={r['source_row']:r for r in csv.DictReader(f)}
    for o in observations:
        if not o['source_csv'].startswith('csv/1950-land-grant'):continue
        r=locations.get(str(o['source_row']))
        if not r:continue
        o['city']=r['city'];o['locality_support_method']='reviewed_contemporary_directory_and_historical_locality'
        o['historical_location_evidence']=r['evidence'];o['flags'].append('1949_resident_instruction_location_reviewed')
        if r['allocation_status']!='allocate':o['county_review_quarantine']=True;o['flags'].append(r['allocation_status'])


def apply_boundary_reviews(observations,cur):
    path=cur/'multicounty_boundary_reviews.csv'
    if not path.exists():return
    with path.open(encoding='utf-8-sig',newline='') as f:rules=list(csv.DictReader(f))
    for o in observations:
        for r in rules:
            if o['college_name_core'].casefold()==r['alias'].casefold() and o['state']==r['state'] and int(r['year_start'])<=o['year']<=int(r['year_end']) and not o['department_only']:
                o['county_review_quarantine']=True;o['flags'].append('reviewed_possible_multi_county_total')
                o['historical_location_evidence']=o.get('historical_location_evidence','')+'; '+r['evidence']

"""Recreate the explicit EBN table specification; review JSON before publishing.

Only schema/provenance metadata is written into the repository, never data rows.
"""
import argparse, csv, json, re, html
from pathlib import Path


def create(raw):
    manifest = json.loads((raw / 'manifest.json').read_text(encoding='utf-8'))
    specifications = []
    for page in manifest['pages']:
        slug = page['slug']
        source_html=(raw/page['original_html']).read_text(encoding='utf-8-sig')
        citations=[]
        for paragraph in re.findall(r'<p\b[^>]*>(.*?)</p>',source_html,re.S|re.I):
            text=' '.join(html.unescape(re.sub(r'<[^>]+>',' ',paragraph)).split())
            if re.match(r'^Source\s*:',text): citations.append(text)
        for table in page['tables']:
            with (raw / table['csv_file']).open(encoding='utf-8-sig', newline='') as f:
                headers = next(csv.reader(f))
            heading = table['heading']
            spec = dict(csv_file=table['csv_file'], source_url=page['source_url'],
                        heading=heading, headers=headers, included=False,
                        exclusion_reason='not an institutional faculty table', recipes={}, components={})
            spec['source_citation']=citations[table['table_number']-1] if len(citations)>=table['table_number'] else ''
            if 'historically-black-colleges-and-normal' in slug and table['table_number']==2:
                with (raw/page['tables'][0]['csv_file']).open(encoding='utf-8-sig',newline='') as f: normal_rows=list(csv.reader(f))
                with (raw/table['csv_file']).open(encoding='utf-8-sig',newline='') as f: college_rows=list(csv.reader(f))
                if normal_rows==college_rows:
                    spec['exclusion_reason']='source error: supposed college table is an exact copy of the normal-school table; institution classification and totals unverified'
                    specifications.append(spec);continue
            specs = spec['recipes']
            components = spec['components']
            def col(*names):
                return next((s for s in names if s in headers), None)
            def recipe(field, *names):
                found = [s for s in names if s in headers]
                if len(found) == len(names):
                    specs[field] = [found]
            def total(*names):
                found = col(*names)
                if found: specs.setdefault('faculty_headcount', []).append([found])
            def sexes(male, female):
                if male in headers and female in headers:
                    recipe('faculty_male', male); recipe('faculty_female', female)
                    specs.setdefault('faculty_headcount', []).append([male, female])
            name_col = col('College', 'College Name', 'College name', 'Institution')
            broad_national = (not any(x in slug for x in ['normal', 'teacher', 'land-grant', 'benefaction',
                              'phd', 'alumni', 'enrollment', 'support', 'historical-summary', 'professional-staff', 'law-school'])
                              and (name_col is not None) and table['table_number'] == 1)
            special = ('americanalmanac' in slug or 'sample-public-research' in slug
                       or 'land-grant' in slug or ('womens-colleges' in slug and any(x in heading.lower() for x in ['faculty', 'division b']))
                       or ('historically-black' in slug and table['table_number'] == 2)
                       or ('1920-teachers-' in slug and table['table_number'] == 2)
                       or ('1924-26-teachers-' in slug and table['table_number'] == 4)
                       or ('1930-teachers-' in slug and table['table_number'] == 4))
            faculty_columns = [h for h in headers if re.search(r'faculty|instructor|professor|teaching all|resident instruction', h, re.I)
                               and not re.search(r'salar|endowed|increase|decrease|occupation|paid', h, re.I)]
            if not ((broad_national or special) and faculty_columns and name_col):
                if '1930-public' in slug and table['table_number'] == 3:
                    spec['exclusion_reason'] = 'misplaced K-12 city-school table; no college identifier'
                specifications.append(spec); continue
            if 'land-grant' in slug and not re.search(r'faculty|instructor|professor|resident instruction', ' '.join(headers), re.I):
                specifications.append(spec); continue
            if 'Occupations of' in heading:
                spec['exclusion_reason'] = 'occupations of graduates, not current institutional faculty'
                specifications.append(spec); continue
            if re.search(r'expenditures|receipts|income|property|benefactions',heading,re.I):
                spec['exclusion_reason']='financial table; instruction expenditure is not faculty count'
                specifications.append(spec); continue
            source_family = ('research_sample' if 'sample-public-research' in slug else
                             'land_grant' if 'land-grant' in slug else 'national_institution_table')
            spec.update(included=True, exclusion_reason='', name_column=name_col,
                        city_column=col('City', 'City or town'), state_column=col('State'),
                        source_family=source_family, source_priority={'national_institution_table': 1, 'land_grant': 2, 'research_sample': 3}[source_family],
                        institution_type='teachers_college' if 'teachers-colleges-normal' in slug else 'college_or_university',
                        measure_basis='headcount', coverage='reported_teaching_total',
                        definition='Reported instructional faculty; preparatory teaching retained when included in reported total.')
            period_match = re.search(r'((?:18|19)\d{2})\s*[-–]\s*(\d{2,4})', heading)
            if period_match:
                year = int(period_match[1]); period = period_match[0]
                basis = 'explicit_academic_period_start'
            else:
                match = re.search(r'(?:18|19)\d{2}', heading) or re.search(r'(?:18|19)\d{2}', page['title'])
                year = int(match[0]); period = str(year); basis = 'reported_single_year'
            if slug.startswith('1924-26-'):
                year, period, basis = 1925, '1925-26', 'source_citation_academic_period_start'
            if slug.startswith('1934-36-'):
                year, period, basis = 1935, '1935-36', 'source_citation_academic_period_start'
            if 'americanalmanac' in slug: basis = 'almanac_volume_year_observation_date_unverified'
            if 'sample-public-research' in slug and year in [1920,1930,1940]:
                year-=1;period=f'{year}-{str(year+1)[-2:]}';basis='source_survey_academic_period_start'
            if slug.startswith(('1910-universities','1910-womens')) or ('sample-public-research' in slug and year==1910):
                year,period,basis=1909,'1909-10','original_chapter_year_ended_June_1910'
                spec['period_evidence']='https://archive.org/details/reportofcommissi02unit_1/page/843/ ; chapter begins For the year ended June, 1910'
            if slug.startswith('1916-universities'):
                year,period,basis=1914,'1914-15','original_chapter_explicit_academic_period'
                spec['period_evidence']='https://files.eric.ed.gov/fulltext/ED621143.pdf ; Chapter III Universities, colleges and technological schools, 1914-15; reported in 1916 volume'
            if 'sample-public-research' in slug and year==1915:
                period,basis='1915-16','original_state_universities_report_year_ended_June_1916'
                spec['period_evidence']='https://files.eric.ed.gov/fulltext/ED542734.pdf ; title year ended June 30, 1916'
            spec.update(year=year, reported_period=period, year_basis=basis,
                        publication_year=None, publication_year_status='not established from title alone')
            published=re.search(r'(?:Office|Officer|Press),?\s+((?:18|19)\d{2})',spec['source_citation'])
            if published:
                spec['publication_year']=int(published[1]);spec['publication_year_status']='source bibliographic citation'
            # Broad reported totals take precedence over mutually exclusive sex columns.
            total('Total faculty excluding duplicates', 'Total faculty',
                  'Total faculty in collegiate and preparatory departments', 'Total professors and instructors',
                  'Number of Instructors', 'Number of instructors', 'Instructors' if 'americanalmanac' in slug else '__none__', 'Number of faculty')
            for pair in [
                ('Total male faculty, excluding duplicates', 'Total female faculty, excluding duplicates'),
                ('Total male faculty, excluding duplicates', 'Total Female faculty, excluding duplicates'),
                ('Total male faculty excluding duplicates', 'Total female faculty excluding duplicates'),
                ('Total male faculty', 'Total female faculty'), ('Total number of male faculty', 'Total number of female faculty'),
                ('Male faculty', 'Female faculty'), ('Male Professors and Instructors', 'Female Professors and Instructors'),
                ('Male professors and instructors', 'Female professors and instructors'),
                ('Men teaching all courses, excluding duplicates', 'Women teaching in all courses, excluding duplicates'),
                ('All faculty excluding duplicates - men', 'All faculty excluding duplicates - women'),
                ('Male instructors including principal, whole year', 'Female instructors including principal whole year')]:
                sexes(*pair)
            if 'Male faculty (reduced to full-time basis)' in headers or (year==1939 and 'Male faculty' in headers):
                spec['measure_basis'] = 'fte'; specs.clear()
                m=col('Male faculty (reduced to full-time basis)','Male faculty');f=col('Female faculty (reduced to full-time basis)','Female faculty')
                recipe('faculty_fte',m,f);recipe('faculty_fte_male',m);recipe('faculty_fte_female',f)
                spec['definition']='Reported faculty/professional staff reduced to full-time basis by default; reviewed row/sex exceptions and dates applied by faculty_corrections.py.'
            if 'sample-public-research' in slug and year==1935:
                spec['measure_basis']='fte'; specs.clear()
                recipe('faculty_fte','Male faculty','Female faculty')
                recipe('faculty_fte_male','Male faculty');recipe('faculty_fte_female','Female faculty')
                spec['definition']='1935-36 source survey reports faculty reduced to full-time basis; sample transcription retains shorter column labels.'
            if slug.startswith('1950-land-grant'):
                spec['measure_basis'] = 'headcount'; specs.clear()
                recipe('faculty_headcount','Male resident instruction','Female resident instruction')
                recipe('faculty_male','Male resident instruction');recipe('faculty_female','Female resident instruction')
                recipe('faculty_resident_instruction_headcount','Male resident instruction','Female resident instruction')
                recipe('faculty_extension_instruction_headcount','Extension instruction – Male','Extension instruction – Female')
                recipe('institutional_staff_headcount','Total full and part-time staff')
                recipe('institutional_staff_fte','Total staff reduced to full-time equivalent')
                spec['definition'] = 'Resident-instruction headcount; extension and broader institutional staff stored separately, never summed into faculty.'
                spec['coverage'] = 'resident_instruction_only'
            if '1872-land-grant' in slug:
                recipe('faculty_headcount', 'Resident professors', 'Non-resident professors and lecturers', 'Instructors', 'Tutors')
                spec['definition'] = 'Sum of distinct reported occupational ranks: resident/nonresident professors, instructors, tutors.'
            # Department components are preserved but never added when a teacher
            # could appear in more than one department and no unduplicated total exists.
            for header in faculty_columns:
                if header not in ['Faculty in Am Men of Science']:
                    components[header] = header
            if not specs:
                spec['coverage'] = 'department_components_only_union_not_identified'
                spec['definition'] = 'Only departmental/rank components reported; no verified unduplicated all-teaching total. Primary total remains missing.'
            if any(x in slug for x in ['1875-colleges', '1880-college-and-universities']):
                spec['coverage'] = 'reported_total_preparatory_inclusion_unspecified'
            for field,word in [('faculty_preparatory','preparatory'),('faculty_collegiate','collegiate'),('faculty_professional','professional')]:
                matches=[h for h in faculty_columns if word in h.lower()
                         and not (word=='preparatory' and 'collegiate' in h.lower())
                         and not (word=='collegiate' and 'preparatory' in h.lower())]
                if len(matches)==1: recipe(field,matches[0])
                elif len(matches)==2 and any('male' in h.lower() for h in matches) and any('female' in h.lower() for h in matches):
                    recipe(field,*matches)
            specifications.append(spec)
    return dict(version=1, instructions='Explicit reviewed table mapping. Missing components are not zeros. No imputation.', tables=specifications)


if __name__ == '__main__':
    p = argparse.ArgumentParser(); p.add_argument('--raw', type=Path, required=True)
    args = p.parse_args()
    result = create(args.raw)
    target = Path(__file__).with_name('source_table_mapping.json')
    target.write_text(json.dumps(result, ensure_ascii=False, indent=2), encoding='utf-8')
    print('Mapped', len(result['tables']), 'tables;', sum(t['included'] for t in result['tables']), 'included')

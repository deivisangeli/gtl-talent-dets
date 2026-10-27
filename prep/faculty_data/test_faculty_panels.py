import unittest,json
from pathlib import Path
from build_faculty_panels import numeric,scope,choose_panel,aggregate,FIELDS


def observation(id,priority=1,head=None,fte=None,**changes):
    row=dict(observation_id=id,college_id='one',college_name_core='Example University',college_name_original='Example University',
             college_name_standardized='Example University',year=1900,GEOID='01001',flags=[],department_only=False,
             source_priority=priority,source_family='national_institution_table' if priority==1 else 'land_grant',
             measure_definition='test observed teaching count',department_coverage='reported_teaching_total',
             geography_method='test',inclusion_status='included',components_json={},raw_row_json={})
    row.update({field:None for field in FIELDS});row.update(faculty_headcount=head,faculty_fte=fte);row.update(changes)
    return row


class FacultyPanels(unittest.TestCase):
    def test_explicit_mapping_excludes_known_source_errors_and_finances(self):
        specs=json.loads(Path(__file__).with_name('source_table_mapping.json').read_text(encoding='utf-8'))['tables']
        for spec in specs:
            if ('historically-black' in spec['csv_file'] and '__table_02' in spec['csv_file']) or ('1950-' in spec['csv_file'] and '__table_07' in spec['csv_file']):
                self.assertFalse(spec['included'])

    def test_missing_symbols_are_not_zero(self):
        for value in ['', '-', '—','..','NA','1*','1,23','10 891']:
            self.assertIsNone(numeric(value))
        self.assertEqual(numeric('0'),0);self.assertEqual(numeric('1,234'),1234)
        self.assertEqual(numeric('12.5'),12.5)

    def test_scope(self):
        self.assertEqual(scope('Eastern Teachers College','college_or_university','')[0],'included')
        self.assertEqual(scope('Example Junior College','college_or_university','')[1],'junior_college')
        self.assertEqual(scope('Example Normal School','college_or_university','')[0],'excluded')
        self.assertEqual(scope('Example Medical College','college_or_university','')[0],'excluded')

    def test_competing_reports_not_added_and_sex_sources_align(self):
        a=observation('a',head=12,faculty_male=10,faculty_female=2)
        b=observation('b',priority=2,head=20,faculty_male=15,faculty_female=5)
        rows,conflicts,_=choose_panel([a,b])
        self.assertEqual(len(rows),1);self.assertEqual(rows[0]['faculty_headcount'],12)
        self.assertEqual(rows[0]['faculty_female'],2);self.assertEqual(len(conflicts),1)

    def test_fte_never_substitutes_for_headcount(self):
        rows,_,_=choose_panel([observation('a',fte=8)])
        self.assertIsNone(rows[0]['faculty_headcount']);self.assertEqual(rows[0]['faculty_fte'],8)

    def test_internal_school_not_added_to_parent(self):
        parent=observation('parent',head=75)
        unit=observation('school',priority=2,head=8,department_only=True)
        rows,_,_=choose_panel([parent,unit])
        self.assertEqual(len(rows),1);self.assertEqual(rows[0]['faculty_headcount'],75)
        self.assertEqual(rows[0]['n_competing_observations'],2)

    def test_county_missing_zero_and_unallocated(self):
        a=observation('a',head=0);rows,_,_=choose_panel([a])
        counties=[dict(GEOID='01001',COUNTYNAME='Example',STATE='AL'),dict(GEOID='01003',COUNTYNAME='Other',STATE='AL')]
        county=aggregate(rows,counties)
        self.assertEqual(next(r for r in county if r['GEOID']=='01001' and r['year']==1900)['faculty_headcount'],0)
        self.assertIsNone(next(r for r in county if r['GEOID']=='01003' and r['year']==1900)['faculty_headcount'])
        self.assertIsNone(next(r for r in county if r['GEOID']=='01001' and r['year']==1901)['faculty_headcount'])
        rows[0]['county_eligible']=False
        self.assertIsNone(next(r for r in aggregate(rows,counties) if r['GEOID']=='01001' and r['year']==1900)['faculty_headcount'])


if __name__=='__main__': unittest.main()

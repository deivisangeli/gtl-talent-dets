import json,unittest
from pathlib import Path
from institution_geolocation import OUT,norm,oid,coordinate,quality_flags
from geolocate_education import normalize,condition_matches,resolve

class InstitutionGeolocationTests(unittest.TestCase):
    def test_strict_snapshot_id(self):
        self.assertEqual(oid('https://openalex.org/I123'),'I123')
        for bad in ('Ixyz','123','I123;I456','https://example.com/I123'):
            self.assertIsNone(oid(bad))
    def test_abbreviation_without_partial_matching(self):
        self.assertEqual(normalize('Univ Calif, Berkeley'),normalize('University of California, Berkeley'))
        self.assertNotEqual(normalize('Univ Calif'),normalize('University of California, Berkeley'))
        self.assertNotEqual(normalize('Calif'),normalize('California Institute of Technology'))
    def test_acronyms_stay_guarded(self):
        r=dict(city='Cambridge',region='Massachusetts',country='US',type='education',valid_coordinates=True,is_super_system=False)
        self.assertIn('short_or_acronym',quality_flags('MIT',r,None))
    def test_bare_geography_guard(self):
        r=dict(city='Los Angeles',region='California',country='US',type='education',valid_coordinates=True)
        self.assertIn('bare_geographic_name',quality_flags('California',r,None))
    def test_ocr_bounded(self):
        self.assertEqual(normalize('Univ 111, Urbana',True),'university illinois urbana')
        self.assertEqual(normalize('111',True),'111')
    def test_conjunction_and_multiple_locations(self):
        self.assertIn('and',norm('College of William & Mary'))
        row=dict(status='reviewed_match',accepted_ids=['I1','I2'],flags=[])
        self.assertEqual(resolve(row,{},'')[1],['I1','I2'])
        self.assertNotEqual(norm('Washington and Lee'),norm('Washington'))
    def test_campus_preserved(self):
        self.assertNotEqual(normalize('University of Washington Tacoma'),normalize('University of Washington'))
        self.assertNotEqual(normalize('Harvard Medical School'),normalize('Harvard University'))
    def test_reliable_year_only(self):
        c=dict(all=[dict(field='year',op='lt',value=1955)],accepted_ids=['I1'])
        self.assertTrue(condition_matches(c,dict(year='1930',flags='[]'),''))
        self.assertFalse(condition_matches(c,dict(year='',flags='[]'),''))
        self.assertFalse(condition_matches(c,dict(year='1901',flags='["chronology_conflict"]'),''))
        self.assertFalse(condition_matches(c,dict(year='1960',flags='[]'),''))
    def test_conflicting_conditions_not_joint_award(self):
        row=dict(status='conditional',accepted_ids=[],flags=[],conditions=[
          dict(all=[dict(field='edition',op='eq',value=1986)],accepted_ids=['I1']),
          dict(all=[dict(field='year',op='lt',value=1980)],accepted_ids=['I2'])])
        status,ids,flags=resolve(row,dict(edition='1986',year='1970',flags='[]'))
        self.assertEqual((status,ids),('ambiguous',[]));self.assertIn('conflicting_conditions',flags)
    def test_coordinate_validation(self):
        self.assertTrue(coordinate(dict(latitude=42.,longitude=-71.)))
        for lat,lon in [(0,0),(91,1),(1,181),(float('nan'),1)]:
            self.assertFalse(coordinate(dict(latitude=lat,longitude=lon)))
    def test_absent_foreign_name_stays_absent(self):
        row=dict(status='absent_reference',accepted_ids=[],flags=['absent_reference'])
        self.assertEqual(resolve(row,{}),('absent_reference',[],['absent_reference']))
    def test_invalid_extraction_never_gets_parent_coordinates(self):
        row=dict(status='invalid_extraction',accepted_ids=[],flags=['invalid_extraction'])
        self.assertFalse(resolve(row,{})[1])

@unittest.skipUnless((OUT/'validation_report.json').exists(),'Published local artifacts unavailable')
class PublishedAdjudicationRegressionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rows={r['name_id']:r for r in json.loads((OUT/'published_crosswalk.json').read_text(encoding='utf-8'))}
        cls.cases=json.loads(Path(__file__).with_name('geolocation_regression_cases.json').read_text(encoding='utf-8'))['cases']
    def test_reviewed_namesakes_historical_entities_and_damaged_awards(self):
        for case in self.cases:
            with self.subTest(name_id=case['name_id'],cause=case['cause']):
                row=self.rows[case['name_id']]
                self.assertEqual(row['status'],case['status'])
                self.assertEqual(row['accepted_ids'],case['accepted_ids'])
    def test_shared_institutions_do_not_override_degree_or_date_conditions(self):
        for case in self.cases:
            for award in case.get('award_cases',[]):
                with self.subTest(name_id=case['name_id'],award=award):
                    data=dict(award,edition='1986',flags='[]')
                    self.assertEqual(resolve(self.rows[case['name_id']],data,'')[1],award['ids'])
    def test_failed_precision_gate_keeps_unreviewed_matches_as_candidates(self):
        audit=json.loads((OUT/'final_audit_metrics.json').read_text(encoding='utf-8'))
        if audit['target_met']:self.skipTest('This run passed the independent gate')
        candidate_rows=[r for r in self.rows.values() if r['status']=='candidate_requires_review']
        self.assertTrue(candidate_rows)
        self.assertTrue(all(not r['accepted_ids'] and r['candidate_ids'] for r in candidate_rows))

if __name__=='__main__':unittest.main()

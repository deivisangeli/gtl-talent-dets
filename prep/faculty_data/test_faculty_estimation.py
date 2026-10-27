"""Meaningful source-unit, calibration and missingness regression scenarios."""
import unittest
from decimal import Decimal,localcontext
from estimate_faculty_headcounts import estimate_observation,crosswalk,calibration_table,aggregate_estimates
from faculty_corrections import note_units,faculty_period


class FacultyEstimation(unittest.TestCase):
    def test_whole_row_and_sex_specific_units(self):
        self.assertEqual(note_units('Faculty not reduced to full-time status'),{'male':'headcount','female':'headcount'})
        self.assertEqual(note_units('Female faculty not reduced to full-time basis'),{'male':'fte','female':'headcount'})
        self.assertEqual(note_units('Male faculty not reduced to full-time basis'),{'male':'headcount','female':'fte'})
        self.assertEqual(note_units('Faculty no reduced to full-time status'),{'male':'headcount','female':'headcount'})
        self.assertEqual(note_units('Faculty reduced to full-time status'),{'male':'fte','female':'fte'})

    def test_faculty_dates_do_not_follow_student_only_notes(self):
        self.assertEqual(faculty_period('HBCU, Figures for 1937-38',1939,'1939-40')[:2],(1937,'1937-38'))
        self.assertEqual(faculty_period('HBCU; Faculty numbers are form 1933-34',1935,'1935-36')[:2],(1933,'1933-34'))
        for n in ['Freshmen men and women: figures for 1937-38','Faculty not reduced to full-time status; Nonresident pupils - figures for 1937-38','Degrees awarded: Figures for 1933-34']:
            y=1935 if '1933' in n else 1939;self.assertEqual(faculty_period(n,y,str(y))[0],y)
        self.assertEqual(faculty_period('All statistics from 1908-09',1909,'1909-10')[:2],(1908,'1908-09'))
        self.assertEqual(faculty_period("From Report of the Commissioner of Education for year 1884-'85; students not specified",1885,'1885-86')[:2],(1884,'1884-85'))
        self.assertEqual(faculty_period('Statistics of 1924',1925,'1925-26')[:2],(1924,'1924'))
        self.assertEqual(faculty_period('Number of male faculty in preparatory from 1899-1900',1900,'1900-01')[0],1900)

    def test_factor_direction_zero_and_fraction(self):
        o=dict(faculty_male='',faculty_female='',faculty_fte_male='1',faculty_fte_female='0',faculty_fte='1')
        result=estimate_observation(o,Decimal(3),Decimal(2))
        self.assertEqual(result['estimated'],Decimal('1.5'));self.assertEqual(result['status'],'estimated')
        o['faculty_fte_male']='0';o['faculty_fte']='0';self.assertEqual(estimate_observation(o,Decimal(3),Decimal(2))['estimated'],0)

    def test_mixed_units_preserve_observed_component(self):
        o=dict(faculty_male='8',faculty_female='',faculty_fte_male='',faculty_fte_female='3',faculty_fte='')
        r=estimate_observation(o,Decimal(10),Decimal(8))
        self.assertEqual(r['estimated'],Decimal('11.75'));self.assertEqual(r['reported'],8);self.assertEqual(r['modeled'],Decimal('3.75'));self.assertEqual(r['status'],'mixed')

    def test_incomplete_components_are_missing(self):
        o=dict(faculty_male='8',faculty_female='',faculty_fte_male='',faculty_fte_female='',faculty_fte='')
        self.assertIsNone(estimate_observation(o,Decimal(10),Decimal(8)))

    def test_identity_and_boundary_fallback(self):
        o=dict(observation_id='source',college_id='one',college_name_core='Example University',state='MA',city='Cambridge',year='1935',
            source_url='source',inclusion_status='included',faculty_fte='10',faculty_fte_male='8',faculty_fte_female='2',department_only='False',flags='[]')
        c=dict(usable=True,state='MA',college_id='one',college='Example University',city='Cambridge',boundary_comparable=True,calibration_observation='1949source',source_url='1949')
        self.assertEqual(crosswalk([o],[c])[0]['calibration_method'],'college_specific')
        c['boundary_comparable']=False;self.assertEqual(crosswalk([o],[c])[0]['calibration_method'],'pooled')
        c['boundary_comparable']=True;o['city']='Other';self.assertEqual(crosswalk([o],[c])[0]['fallback_reason'],'historical_locality_not_identical')
        self.assertEqual(crosswalk([o],[])[0]['calibration_method'],'pooled')

    def test_flagged_calibration_pair_excluded(self):
        import json
        def source(oid,h,f,usable):
            return dict(source_csv='csv/1950-land-grant-test',observation_id=oid,college_id=oid,college_name_core=oid,state='MA',city='Cambridge',
                source_row=2,source_url='source',institutional_staff_headcount=str(h),institutional_staff_fte=str(f),institutional_staff_fte_usable=str(usable),flags='[]',
                raw_row_json=json.dumps({'Male full and part-time staff':str(h),'Female full and part-time staff':'0','Male staff reduced to full-time equivalent':str(f),'Female staff reduced to full-time equivalent':'0'}))
        rows,p=calibration_table([source('ok',100,80,True),source('bad',200,200,False)])
        self.assertEqual(p['headcount'],100);self.assertEqual(p['fte'],80);self.assertFalse(rows[1]['usable'])


if __name__=='__main__':unittest.main()

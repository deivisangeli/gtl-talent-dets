"""Structural fixtures for the separate 150-entry development extractor."""
import unittest
from amws_university_150_engine import extract_universities,norm

def row(text,edition='1906',address='',birthplace=''):
    return dict(biography_text=text,address_text=address,birthplace_text=birthplace,raw_name='Example, Dr. A',edition=edition,rest_text='')

def names(result):return {p['institution']['text'] for p in result['affiliations']}

def check_offsets(test,entry,obj):
    if isinstance(obj,dict):
        if {'field','start','end','text'}<=obj.keys():test.assertEqual(entry[obj['field']][obj['start']:obj['end']],obj['text'])
        for v in obj.values():check_offsets(test,entry,v)
    elif isinstance(obj,list):
        for v in obj:check_offsets(test,entry,v)

class UniversityExtractionTests(unittest.TestCase):
    def test_education_and_address_are_separate(self):
        e=row('A.B, Northbridge, 01; Ph.D, Lakeside, 05. Chem. Soc.',address='Cedar University, Town')
        p=extract_universities(e);self.assertEqual(names(p),set());self.assertEqual(len(p['reported_header_affiliations']),1)
        self.assertFalse(p['reported_header_affiliations'][0]['employment_established'])

    def test_multiple_institutions_and_bounded_role_carry(self):
        e=row('Ph.D, Elm, 99. Instr. chem, Northbridge, 00-05; Lakeside, 05-10; research chemist, Acme Corp, 10-20; Westhaven, 20-')
        p=extract_universities(e);self.assertEqual(names(p),{'Northbridge','Lakeside'});check_offsets(self,e,p)

    def test_successive_appointments_do_not_inflate_institutions(self):
        e=row('Instr. math, Northbridge, 01-05; prof. math, Northbridge, 05-10; emer. prof, Northbridge, 10-')
        p=extract_universities(e);self.assertEqual(len(p['affiliations']),1)
        self.assertEqual(len(p['affiliations'][0]['appointments']),3)
        self.assertEqual(p['affiliations'][0]['temporal_status'],'mixed')
        self.assertIn('emeritus',p['affiliations'][0]['appointment_types'])

    def test_medical_school_qualifier_and_department_header(self):
        e=row('M.D, Elm, 01. Clin. prof, Northbridge Medical School, 05-',edition='1938',address='Dept. of Anatomy, Northbridge University School of Medicine')
        p=extract_universities(e);self.assertEqual(names(p),{'Northbridge'})
        self.assertTrue(p['affiliations'][0]['qualifiers']);check_offsets(self,e,p)

    def test_1955_original_header_birth_education_career(self):
        e=row('EXAMPLE, DR. A, Cedar University, Town. @PHYSICS@. Town, 1900. Ph.D, Elm, 25. Lnstr. physics, Cedar University, 25-30, prof, 30-',edition='1955',birthplace='Town, 1900.')
        p=extract_universities(e);self.assertEqual(names(p),{'Cedar University'});check_offsets(self,e,p)

    def test_retirement_does_not_reopen_closed_appointment(self):
        e=row('Prof. chem, Northbridge, 01-30; RETIRED.',address='Northbridge University')
        p=extract_universities(e);self.assertEqual(p['affiliations'][0]['temporal_status'],'past')

    def test_secondary_school_and_military_reset(self):
        for text in ['Teacher, high school, Town, 01-05; OTHER, 05-', 'Instr. physics, Military Sch, Town, 01-05;', 'Prof. chem, Northbridge, 01-05; U.S.N.R, 05-10; Elsewhere, 10-']:
            p=extract_universities(row(text));self.assertFalse(names(p)-{'Northbridge'})

    def test_membership_and_honorary_degrees_stop_carry(self):
        e=row('Asst. prof, Northbridge, 01-05, prof, 05- Hon. LL.D, Elm, 10; Lakeside, 11; Chem. Soc.')
        self.assertEqual(names(extract_universities(e)),{'Northbridge'})

    def test_neighboring_person_is_not_extracted(self):
        e=row('Instr. math, Northbridge, 01-05. Research topic. Other, Dr. John, Town. Prof. math, Lakeside, 05-')
        p=extract_universities(e);self.assertEqual(names(p),{'Northbridge'})
        self.assertIn('neighboring_person_heading',[f['reason'] for f in p['review_flags']])

    def test_ocr_damage_is_not_spell_corrected(self):
        e=row('Prof. chem, Nor^thbridge, 01-05; research chemist, Acme Co, 05-')
        p=extract_universities(e);self.assertEqual(names(p),set());self.assertTrue(p['review_flags'])

    def test_hospital_and_independent_institute_remain_unresolved(self):
        for text in ['Research assoc, Independent Inst, 01-05;', 'Asst, Town Hosp, 01-05;']:
            self.assertEqual(names(extract_universities(row(text))),set())

    def test_deterministic_and_unique_normalized_names(self):
        e=row('Instr. math, Cedar University, 01-05; prof, CEDAR UNIVERSITY, 05-10')
        a=extract_universities(e);self.assertEqual(a,extract_universities(e))
        self.assertEqual(len(a['affiliations']),len({norm(p['institution']['text']) for p in a['affiliations']}))
        check_offsets(self,e,a)

if __name__=='__main__':unittest.main()

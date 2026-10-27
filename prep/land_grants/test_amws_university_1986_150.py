"""Structural regression fixtures for the separate 1986 development extractor."""
import unittest
from amws_university_1986_150_engine import extract_universities,norm
from amws_university_regex_150 import validate_evidence

def row(text):return dict(edition='1986',biography_text=text)
def names(result):return {a['institution']['text'] for a in result['affiliations']}

class ExtractionTests(unittest.TestCase):
    def check(self,text):
        r=row(text);a=extract_universities(r);validate_evidence(r,a)
        self.assertEqual(a,extract_universities(r))
        keys=[norm(p['institution']['text']) for p in a['affiliations']];self.assertEqual(len(keys),len(set(keys)))
        return a

    def test_glued_membership_boundary(self):
        p=self.check('Educ: Beta Univ, PhD, 70. Prof Exp: PROF CHEM, ALPHA UNIV, 80Mem: Am Col Physicians; instr, Beta Univ, 81. Res: Chemistry.')
        self.assertEqual(names(p),{'ALPHA UNIV'})

    def test_concurrent_separate_from_professional_experience(self):
        p=self.check('Prof Exp: Prof, Alpha Univ, 60-70Concurrent Pos: Vis prof, Beta Univ, 75-76. Mem: Am Col Science.')
        self.assertEqual(names(p),{'Alpha Univ','Beta Univ'})
        self.assertEqual({t['section'] for a in p['affiliations'] for t in a['trace']},{'professional_experience','concurrent_positions'})

    def test_missing_heading_never_extracts_education(self):
        p=self.check('Educ: Alpha Univ, PhD, 70. Mailing Add: Alpha Univ Box 1 Town CA 90000')
        self.assertEqual(names(p),set());self.assertTrue(p['reported_address_affiliations'])
        self.assertIn('missing_professional_experience_heading',[f['reason'] for f in p['review_flags']])

    def test_bounded_damaged_heading_recognition(self):
        p=self.check('Proi Exp: Instr, Alpha Univ, 70-72Concurrent Pox Vis prof, Beta Univ, 73-74. Mem: AAAS.')
        self.assertEqual(names(p),{'Alpha Univ','Beta Univ'})

    def test_closed_job_not_reopened_by_address(self):
        p=self.check('Prof Exp: Prof, Alpha Univ, 60-70. Mem: AAAS. Mailing Add: Dept Chem Alpha Univ Town CA 90000')
        self.assertEqual(p['affiliations'][0]['temporal_status'],'past')
        self.assertTrue(p['affiliations'][0]['supporting_addresses'])
        self.assertFalse(p['reported_address_affiliations'][0]['employment_established'])

    def test_bounded_role_carry_stops_at_new_employer(self):
        p=self.check('Prof Exp: Instr, Alpha Univ, 60-65; Beta Univ, 65-70; chemist, Acme Corp, 70-80; Gamma Univ, 80- Mem: AAAS.')
        self.assertEqual(names(p),{'Alpha Univ','Beta Univ'})

    def test_roles_before_late_institution_and_multiple_universities(self):
        p=self.check('Prof Exp: From asst prof to assoc prof, 60-70, PROF CHEM, ALPHA COL & BETA UNIV, 70- Mem: AAAS.')
        self.assertEqual(names(p),{'ALPHA COL','BETA UNIV'})

    def test_units_and_campus_are_not_extra_universities(self):
        p=self.check('Prof Exp: PROF ANAT, UNIV ALPHA COL MED, 70-; prof, UNIV BETA, COLLEGE PARK, 80- Mem: AAAS.')
        self.assertEqual(names(p),{'UNIV ALPHA','UNIV BETA'})
        self.assertTrue(p['affiliations'][0]['qualifiers'])

    def test_honorary_governance_and_secondary_school_excluded(self):
        p=self.check('Prof Exp: Instr math, high sch, 60-65. Concurrent Pos: Hon res fel, Alpha Univ, 70-71; trustee, Beta Univ, 71- Mem: AAAS.')
        self.assertEqual(names(p),set())

    def test_hospital_training_does_not_establish_university_job(self):
        p=self.check('Prof Exp: Intern, Alpha Univ Hosp, 60-61; resident, Beta Univ, 61-65. Mem: AAAS.')
        self.assertEqual(names(p),set());self.assertTrue(p['review_flags'])

    def test_explicit_faculty_after_training_is_retained(self):
        p=self.check('Prof Exp: Intern, Alpha Univ, 60-61, asst instr, 61-62, prof, 62-70. Mem: AAAS.')
        self.assertEqual(names(p),{'Alpha Univ'})

    def test_funding_alone_not_employment(self):
        p=self.check('Prof Exp: Chemist, Acme Corp, 60-70. Concurrent Pos: NSF grant, Alpha Univ, 71-72. Mem: AAAS.')
        self.assertEqual(names(p),set())

    def test_emeritus_and_retirement(self):
        p=self.check('Prof Exp: Prof, Alpha Univ, 60-80, EMER PROF, 80-; RETIRED. Mem: AAAS.')
        self.assertEqual(p['affiliations'][0]['temporal_status'],'mixed')
        self.assertIn('emeritus',p['affiliations'][0]['appointment_types'])

    def test_undated_and_terminal_single_year_uncertain(self):
        for text in ['Prof Exp: Prof, Alpha Univ, 80Mem: AAAS.','Prof Exp: Prof, Alpha Univ. Mem: AAAS.']:
            p=self.check(text);self.assertEqual(p['affiliations'][0]['temporal_status'],'uncertain')

    def test_damaged_letters_not_restored(self):
        p=self.check('Prof Exp: Prof, Al^pha Un!v, 80- Mem: AAAS.')
        self.assertEqual(names(p),set());self.assertTrue(p['review_flags'])

    def test_real_neighbor_heading_and_false_rank_heading(self):
        p=self.check('Prof Exp: Assoc prof, 60-70. PROF, STATE UNIV NY STONY BROOK, 70- Mem: AAAS.')
        self.assertEqual(names(p),{'STATE UNIV NY'})
        p=self.check('Prof Exp: Prof, Alpha Univ, 60-70. OTHER, JOHN, Town. Prof Exp: Prof, Beta Univ, 70- Mem: AAAS.')
        self.assertEqual(names(p),{'Alpha Univ'})

    def test_corrupted_mixed_case_name_flags_without_repair(self):
        p=self.check('Prof Exp: Prof, Univ AlphA, 70- Mem: AAAS.')
        self.assertEqual(names(p),set());self.assertTrue(p['review_flags'])

    def test_original_raw_field_interface(self):
        p=extract_universities(dict(raw_text_adjusted='Prof Exp: Prof, Alpha Univ, 70- Mem: AAAS.'))
        self.assertEqual(names(p),{'Alpha Univ'})

if __name__=='__main__':unittest.main()

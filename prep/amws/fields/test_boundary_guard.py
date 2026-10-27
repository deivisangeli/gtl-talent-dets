import unittest
from classifier_boundary_guard import classify_entry


class BoundaryGuardTests(unittest.TestCase):
    def test_damaged_employment_marker_does_not_supply_field(self):
        text = 'DOE, JANE, b Boston, Jan 1, 40; m 60. p\ufffdf Eip: chief PSYCHIATRY.'
        p = classify_entry('', text, 'DOE, JANE')
        self.assertEqual(p['science_group'], 'other')
        self.assertFalse(p['biological_match'])
        self.assertEqual(p['evidence'], [])

    def test_unknown_education_marker_cannot_supply_degree_field(self):
        text = 'DOE, JANE, b Boston, Jan 1, 40. EdZZ: Yale Univ, PhD(physics). Prof Exp: Biology.'
        p = classify_entry('', text, 'DOE, JANE')
        self.assertEqual(p['science_group'], 'other')
        self.assertEqual(p['evidence'], [])

    def test_clear_heading_survives_conservative_cut(self):
        text = 'DOE, JANE, b Boston, Jan 1, 40; c 2. MICROBIOLOGY. EdZZ: Yale Univ, PhD(physics).'
        p = classify_entry('MICROBIOLOGY', text, 'DOE, JANE')
        self.assertEqual(p['science_group'], 'biological_science')
        self.assertFalse(p['physical_match'])

    def test_name_initials_are_not_credential_boundaries(self):
        text = 'MA, JANE, b Boston, Jan 1, 40; c 2. PHYSICS. Educ: Yale Univ.'
        self.assertEqual(classify_entry('PHYSICS', text, 'MA, JANE')['science_group'], 'physical_science')

    def test_maryland_birthplace_is_not_a_medical_degree(self):
        text = 'DOE, JANE, b Baltimore, Md, Jan 1, 40; c 2. PHYSICS. Educ: Yale Univ.'
        self.assertEqual(classify_entry('PHYSICS', text, 'DOE, JANE')['science_group'], 'physical_science')

    def test_guard_keeps_literal_offsets(self):
        text = 'DOE, JANE, b Boston, Jan 1, 40; c 2. BIOCHEMISTRY. EdZZ: Yale Univ.'
        p = classify_entry('', text, 'DOE, JANE')
        for ev in p['evidence']:
            source = text if ev['evidence_source']=='raw_text_adjusted' else ''
            self.assertEqual(source[ev['start']:ev['end']], ev['evidence'])


if __name__=='__main__':
    unittest.main()

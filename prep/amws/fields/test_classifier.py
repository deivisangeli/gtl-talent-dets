"""Behavioral/adversarial checks for heading scope and three-way policy."""
import unittest
from classifier import classify_entry, normalize


def entry(field, header=None):
    h = field if header is None else header
    return classify_entry(field, f'DOE, JANE, b Boston, Mass, Jan 1, 40; m 65; c 2. {h}. Educ: Yale Univ, BS, 61. Prof Exp: Chemist. Res: Ecology.', 'DOE, JANE')


class ClassifierTests(unittest.TestCase):
    def test_life_compounds_do_not_match_generic_physics_chemistry(self):
        for field in ['BIOCHEMISTRY', 'BIOPHYSICS', 'BIOPHYSICAL CHEMISTRY', 'IMMUNOCHEMISTRY', 'BIOSTATISTICS', 'BIOMEDICAL ENGINEERING', 'PHARMACEUTICAL CHEMISTRY']:
            with self.subTest(field=field):
                p = entry(field)
                self.assertEqual(p['science_group'], 'biological_science')
                self.assertFalse(p['physical_match'])

    def test_mixed_means_physical_and_biological(self):
        for field in ['PHYSICS, PHYSIOLOGY', 'ELECTRICAL ENGINEERING, BIOENGINEERING', 'MEDICINAL CHEMISTRY. ORGANIC CHEMISTRY']:
            p = entry(field)
            self.assertEqual((p['science_group'], p['other_reason']), ('other', 'mixed'))
            self.assertTrue(p['physical_match'] and p['biological_match'] and p['review_required'])

    def test_non_target_addition_does_not_create_mixed(self):
        p = entry('CHEMISTRY, PSYCHOLOGY')
        self.assertEqual(p['science_group'], 'physical_science')
        self.assertFalse(p['biological_match'])

    def test_social_compounds_are_not_generic_physics_or_forestry(self):
        for field in ['FOREST ECONOMICS', 'VISUAL PSYCHOPHYSICS', 'HISTORY OF PHYSICS']:
            self.assertEqual((entry(field)['science_group'], entry(field)['other_reason']), ('other', 'non_target'))

    def test_broad_engineering_math_computer_policy(self):
        for field in ['CIVIL ENGINEERING', 'MATHEMATICS', 'COMPUTER SCIENCE']:
            self.assertEqual(entry(field)['science_group'], 'physical_science')

    def test_medicine_is_biological(self):
        for field in ['PUBLIC HEALTH', 'PEDIATRICS', 'VETERINARY MEDICINE', 'PSYCHIATRY']:
            self.assertEqual(entry(field)['science_group'], 'biological_science')

    def test_missing_field_uses_heading_and_exact_evidence(self):
        p = entry('', 'PLANT PATHOLOGY')
        self.assertEqual(p['science_group'], 'biological_science')
        self.assertEqual(p['input_source'], 'heading_fallback')
        text = 'DOE, JANE, b Boston, Mass, Jan 1, 40; m 65; c 2. PLANT PATHOLOGY. Educ: Yale Univ, BS, 61. Prof Exp: Chemist. Res: Ecology.'
        for ev in p['evidence']:
            self.assertEqual(text[ev['start']:ev['end']], ev['evidence'])

    def test_recovered_field_must_not_omit_second_discipline(self):
        p = entry('PALEOECOLOGY', 'PALEOECOLOGY. BASIN ANALYSIS')
        self.assertEqual(p['other_reason'], 'mixed')
        self.assertEqual(p['input_source'], 'heading_completeness_fallback')

    def test_birthplace_debris_is_not_a_field(self):
        self.assertEqual(entry('B NEW YORK', 'BIOLOGY')['science_group'], 'biological_science')

    def test_demographics_do_not_eat_start_of_biology(self):
        p = classify_entry('', 'DOE, JANE, b Boston, Mass, Jan 1, 40; s 1BIOLOGY. Educ: Yale.', 'DOE, JANE')
        self.assertEqual(p['science_group'], 'biological_science')

    def test_degree_and_affiliation_do_not_supply_field(self):
        text = 'DOE, JANE, b Boston, Jan 1, 40. Educ: Yale, PhD(physics). Prof Exp: Dept Biology. Mem: Chemical Society. Res: Ecology.'
        p = classify_entry('', text, 'DOE, JANE')
        self.assertEqual(p['science_group'], 'other')
        self.assertEqual(p['evidence'], [])

    def test_later_employment_field_not_accepted_as_recovered_field(self):
        text = 'DOE, JANE, b Boston, Jan 1, 40. Educ: Yale. Prof Exp: ENGINEERING. Res: Biology.'
        self.assertEqual(classify_entry('ENGINEERING', text, 'DOE, JANE')['science_group'], 'other')

    def test_names_cannot_create_mixed(self):
        text = 'BIOLOGY, JANE, b Boston, Jan 1, 40; c 2. PHYSICS. Educ: Yale.'
        self.assertEqual(classify_entry('PHYSICS', text, 'BIOLOGY, JANE')['science_group'], 'physical_science')

    def test_technique_only_abstains(self):
        p = entry('ELECTRONMICROSCOPY')
        self.assertEqual((p['science_group'], p['other_reason']), ('other', 'unmatched'))
        self.assertTrue(p['review_required'])

    def test_unicode_and_ocr_punctuation_preserve_offsets(self):
        field = 'BIOPHYSICAL\u00a0CHEMISTRY'
        p = entry(field)
        self.assertEqual(p['science_group'], 'biological_science')
        for ev in p['evidence']:
            self.assertEqual(field[ev['start']:ev['end']], ev['evidence'])
        norm, positions = normalize('ﬂuorine')
        self.assertEqual(norm, 'FLUORINE')
        self.assertEqual(positions[:2], [0, 0])

    def test_ocr_education_label_bounds_heading(self):
        text = 'DOE, JANE, b Boston, Jan 1, 40; c 2. MICROBIOLOGY. Edue: Yale, PhD(physics).'
        p = classify_entry('', text, 'DOE, JANE')
        self.assertEqual(p['science_group'], 'biological_science')
        self.assertFalse(p['physical_match'])


if __name__ == '__main__':
    unittest.main()

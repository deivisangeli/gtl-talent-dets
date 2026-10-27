import unittest

import education_regex_v04 as v04
import test_education_regex as base
from education_regex_v04 import extract


class FrozenCasesUnderV04(base.EditionParserTests):
    """Every multi_v03 case must still hold under multi_v04."""
    def setUp(self):
        self.frozen = base.extract
        base.extract = v04.extract

    def tearDown(self):
        base.extract = self.frozen


class V04RuleTests(unittest.TestCase):
    def check(self, text, birth, edition):
        ds = extract(text, birth, edition)['degrees']
        for d in ds:
            self.assertEqual(text[d['start']:d['end']], d['evidence'])
            self.assertEqual(text[d['label_start']:d['label_end']], d['label_as_written'])
            for k in ['institution_as_written', 'year_raw']:
                if d[k]: self.assertIn(d[k], d['evidence'])
        return ds

    def tuples(self, ds):
        return [(d['label_as_written'], d['institution_as_written'], d['year']) for d in ds]

    def test_pharmd_label(self):
        ds = self.check('Educ: Univ Alpha, PharmD, 72. Prof Exp: Lab, 73.', 1945, 1986)
        self.assertEqual([(d['group'], d['year']) for d in ds], [('other_doctorate', 1972)])

    def test_damaged_heading(self):
        ds = self.check('SMITH, JO, b Town, Iowa, June 5, 39; m 62. GENETICS. E due: Alpha Univ, BA, 61; Univ Beta PhD(zool), 68. Prof Exp: Lab, 70.', 1939, 1986)
        self.assertEqual(self.tuples(ds), [('BA', 'Alpha Univ', 1961), ('PhD', 'Univ Beta', 1968)])
        self.assertEqual(extract('X, b T, Ill, May 1, 39. BIO. E due: Alpha Univ, BA, 61. Prof Exp: Lab.', 1939, 1986)['flags'], ['education_heading_ocr'])

    def test_fused_heading_beats_late_educ_token(self):
        ds = self.check('SMITH, JO, b Town, Ill, June 17, 26; m 58. ZOOLOGY. Educ111 Col, AB, 49; Univ 111, MS, 52. Prof Exp: PROF, SCH MED, CTR MED EDUC, 60Res: Embryos.', 1926, 1986)
        self.assertEqual(self.tuples(ds), [('AB', '111 Col', 1949), ('MS', 'Univ 111', 1952)])

    def test_late_educ_token_kept_when_it_has_degrees(self):
        text = 'Prof Exp: Asst, Lab, 48-49; METEOROLOGY. Educ: Alpha Inst Technol, SB, 48, SM, 49. Prof Exp: Lab, 50.'
        self.assertEqual([d['label_as_written'] for d in self.check(text, 1925, 1986)], ['SB', 'SM'])

    def test_unheaded_1986_clause(self):
        ds = self.check('SMITH, JO, b Ripon, Wis, Apr 1, 39; m 61. BIOLOGY. Northern State Col, BS, 61; Univ SDak, MA, 63. Prof Exp: Lab, 66.', 1939, 1986)
        self.assertEqual(self.tuples(ds), [('BS', 'Northern State Col', 1961), ('MA', 'Univ SDak', 1963)])
        self.assertEqual(extract('SMITH, JO, b Ripon, Wis, Apr 1, 39; m 61. BIOLOGY. Prof Exp: Lab, 66. BS, Univ Alpha, 70.', 1939, 1986)['degrees'], [])

    def test_unheaded_trims_heading_residue(self):
        ds = self.check('SMITH, JO, b Town, Ga, Mar 24, 38: m 65. ANALYSIS. EdIC.Univ Ga. BS, 60. MA. 62. Prof Exp: Lab.', 1938, 1986)
        self.assertEqual([d['institution_as_written'] for d in ds], ['Univ Ga', 'Univ Ga'])

    def test_recovered_section_stops_at_damaged_prof_exp(self):
        ds = self.check('SMITH, JO, b Town, Wis, Sept 5, 35; m 80. ENGINEERING. EdIC:Univ Wis. BS. 57. Prof Eap: Asst prof, Univ Beta, 62-64; engr, Shell Co, 66-70.', 1935, 1986)
        self.assertEqual([d['label_as_written'] for d in ds], ['BS'])

    def test_board_certification_flagged(self):
        ds = self.check('Educ: Univ Alpha, MD, 47; Am Bd Internal Med, dipl, 57. Prof Exp: Lab.', 1920, 1986)
        self.assertIn('board_certification', ds[-1]['flags'])
        ds = self.check('Educ: Univ Alpha, BS, 47; Am Acad Environ Engrs, Dipl, 78. Prof Exp: Lab.', 1920, 1986)
        self.assertIn('board_certification', ds[-1]['flags'])
        ds = self.check('Educ: Univ Alpha, BS, 47; Alpha Polytech Inst, Dipl, 50. Prof Exp: Lab.', 1920, 1986)
        self.assertNotIn('board_certification', ds[-1]['flags'])

    def test_american_and_maryland_are_not_degrees(self):
        ds = self.check('Town, Jan. 1, 82. B.S, Alpha, 09, C.E, 24. Draftsman, Am. Bridge Co, Pa, 08.', 1882, 1938)
        self.assertEqual([d['label_as_written'] for d in ds], ['B.S', 'C.E'])
        ds = self.check('Educ: Loyola Col, Md, BS, 37; Univ Beta, MD, 41. Prof Exp: Lab.', 1916, 1986)
        self.assertEqual(self.tuples(ds), [('BS', 'Loyola Col, Md', 1937), ('MD', 'Univ Beta', 1941)])

    def test_1955_header_is_not_education(self):
        ds = self.check('BURDICK, DR. E(VERETTE) M(ARSHALL), Lab Co, Darlington, Md. @CHEMISTRY@. Town, Ill, Aug. 9, 13; m. 37. B.S, Alpha, 35. Chemist, Lab, 41.', 1913, 1955)
        self.assertEqual(self.tuples(ds), [('B.S', 'Alpha', 1935)])
        ds = self.check('BATE, GEORGE L(EE), Lab, Town. @PHYSICS@. Town, Ohio, Feb. 15, 24. A.B, Alpha, 45. Teaching fellow, Beta, 45-46.', 1924, 1955)
        self.assertEqual([d['label_as_written'] for d in ds], ['A.B'])

    def test_merged_tail_before_marker_is_kept(self):
        text = 'a n Alpha Col, 23; A.M, Beta State, 25, Ph.D.(chem), 26. Chemist, Lab, 28. NEXT, DR. JO, Town. @CHEMISTRY@. Town, Oct. 6, 21. B.S, Gamma Col, 42.'
        self.assertIn('Ph.D', [d['label_as_written'] for d in self.check(text, None, 1955)])

    def test_damaged_first_label_supplies_school(self):
        ds = self.check('NAME, Lab, Town. @ENG@. Town, Calif, July 7, 12; m. 36; c. 4. B«S, California, 34, M.S, 36. Inspector, Oil Co, 34-35.', 1912, 1955)
        self.assertEqual(self.tuples(ds), [('M.S', 'California', 1936)])
        self.assertIn('institution_from_preceding_clause', ds[0]['flags'])
        # A dated attendance line is not a damaged label.
        self.assertEqual(self.tuples(self.check('Town, Jan. 1, 82. Alpha, 99; A.B, 01. Instr. bio.', 1882, 1938)), [('A.B', None, 1901)])

    def test_two_letter_bare_school_rejected(self):
        ds = self.check('Town, Dec. 15, 12; m. 41. B.S, Chicago, 34, Mi.35, fellow, 35-37, PhD.(math), 37. Asst, math, Lab, 35.', 1912, 1955)
        self.assertEqual(ds[-1]['institution_as_written'], 'Chicago')

    def test_shared_school_after_subject(self):
        ds = self.check('Town, Sept. 19, 24; m. 45. B.S.(chem. eng) & B.S.(chem), Texas Col. Arts & Indust, 49; M.S, Hopkins, 51. Chem. engineer, Lab, 53.', 1924, 1955)
        self.assertEqual(self.tuples(ds)[:2], [('B.S', 'Texas Col. Arts & Indust', 1949)] * 2)

    def test_ocr_university_tokens(self):
        ds = self.check('Educ: Southern Methodist l nix. BSCE, 63; Ind Uniy, PhD, 70. Prof Exp: Lab, 71.', 1940, 1986)
        self.assertEqual([d['institution_as_written'] for d in ds], ['Southern Methodist l nix', 'Ind Uniy'])

    def test_fused_marriage_field_not_degree(self):
        self.assertEqual(self.check('SMITH, JO, b Riga, Latvia, Apr 10, 27; m F :c3 MATHEMATICS. Educ: Univ Alpha, BS, 50. Prof Exp: Lab.', 1927, 1986)[0]['label_as_written'], 'BS')
        # The scan resumes at the real label after a fused 'm.'.
        ds = self.check('NAME, Lab, Town. @METALLURGY@. Town, Pa, March 22, 20; m. B.S, Alpha State Col, 42. Metal asst, Lab, 41.', 1920, 1955)
        self.assertEqual(self.tuples(ds), [('B.S', 'Alpha State Col', 1942)])

    def test_am_with_award_year_is_a_degree(self):
        ds = self.check('Educ: Harvard Univ, AB, 48, Am, 50. PhD(biol). 52. Prof Exp: Lab.', 1923, 1986)
        self.assertEqual(self.tuples(ds), [('AB', 'Harvard Univ', 1948), ('Am', 'Harvard Univ', 1950), ('PhD', 'Harvard Univ', 1952)])
        self.assertEqual(self.check('Town, Jan. 1, 06. A.B, Duke 28. Vacuum tube eng, Radio Corp. of Am, 28-34.', 1906, 1955)[-1]['label_as_written'], 'A.B')

    def test_fellowship_clause_supplies_first_school(self):
        ds = self.check('NAME, Lab, Town. @CHEMISTRY@. Town, Ohio, Aug. 1, 15; m. 39. as, Alpha, 37; Am. Petrol Inst fellow, Ohio State, 41, Ph.D.(org. chem), 41. Fellow, Beta, 41-43.', 1915, 1955)
        self.assertEqual(self.tuples(ds), [('Ph.D', 'Ohio State', 1941)])


if __name__ == '__main__':
    unittest.main()

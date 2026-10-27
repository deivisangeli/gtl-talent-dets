"""Behavior tests for attribution and regex failures observed in development."""
import unittest
from education_regex import extract
from education_pilot import evaluate


class ParserTests(unittest.TestCase):
    def parse(self, text, birth=1930):
        result = extract(text,birth)
        for d in result['degrees']:
            self.assertEqual(text[d['start']:d['end']],d['evidence'])
            for key in ['label_as_written','institution_as_written','year_raw']:
                if d[key]: self.assertIn(d[key],d['evidence'])
        return result['degrees']

    def test_shared_school(self):
        ds=self.parse('Educ: Univ Alpha, BS, 51, MS, 54, PhD(chem), 58. Prof Exp: Univ Beta, 60.')
        self.assertEqual([(d['group'],d['institution_as_written'],d['year']) for d in ds],
                         [('bachelors','Univ Alpha',1951),('masters_academic','Univ Alpha',1954),('phd','Univ Alpha',1958)])

    def test_joint_awards(self):
        ds=self.parse('Educ: Mass Inst Technol, SB & SM, 67, PhDfelec eng), 70. Prof Exp: Lab, 71.')
        self.assertEqual([d['year'] for d in ds],[1967,1967,1970])

    def test_multiple_schools(self):
        ds=self.parse('Educ: Alpha Col, BA, 50; Beta Univ, MA, 52; Gamma Univ, PhD, 57. Prof Exp: Lab, 60.')
        self.assertEqual([d['institution_as_written'] for d in ds],['Alpha Col','Beta Univ','Gamma Univ'])

    def test_degree_first(self):
        ds=self.parse('Educ: BS, Alpha Univ, 50; MS, Beta Univ, 52. Prof Exp: Lab, 55.')
        self.assertEqual([d['institution_as_written'] for d in ds],['Alpha Univ','Beta Univ'])

    def test_professional_degrees(self):
        ds=self.parse('Educ: Alpha Col, BChE, 50; Beta Inst, MChE, 55; Gamma Univ, MPH, 60.')
        self.assertEqual([d['group'] for d in ds],['other_bachelor','masters_professional','masters_professional'])

    def test_honorary_excluded(self):
        ds=self.parse('Educ: Alpha Col, BS, 50; Beta Univ, PhD, 55. Hon Degrees: MA, Gamma Col, 75. Prof Exp: Lab, 56.')
        self.assertEqual([d['group'] for d in ds],['bachelors','phd'])

    def test_hons_is_earned(self):
        ds=self.parse('Educ: Alpha Univ, BSc, Hons, 62, MSc, 66.')
        self.assertEqual([d['year'] for d in ds],[1962,1966])

    def test_medical_bachelor(self):
        ds=self.parse('Educ: Alpha Univ, MB. BS, 51, MD, 63.')
        self.assertEqual([d['group'] for d in ds],['other_bachelor','other_bachelor'])
        self.assertEqual([d['year'] for d in ds],[1951,1951])

    def test_boards_not_masters(self):
        ds=self.parse('Educ: Alpha Univ, MD, 50; Am Bd Internal Med, dipl, 58. Prof Exp: Lab, 60.')
        self.assertEqual(ds,[])

    def test_state_abbreviation(self):
        ds=self.parse('Educ: Univ Md, BS, 50, MS, 52, PhD, 55.')
        self.assertEqual([d['institution_as_written'] for d in ds],['Univ Md']*3)

    def test_other_credential_not_subject(self):
        ds=self.parse('Educ: Alpha Inst, BSc, 55, Dipl Ing, 56, MSc, 59; Beta Univ, PhD(eng), 62.')
        self.assertEqual([d['year'] for d in ds],[1955,1959,1962])

    def test_cross_reference_collapsed_boundary(self):
        ds=self.parse('FIRST, see previous editionSECOND, OTHER PERSON, b New York, Jan 1, 30. Educ: Alpha Univ, BS, 50.')
        self.assertEqual(ds,[])

    def test_overlapping_biographies(self):
        ds=self.parse('FIRST, FOCAL PERSON, b Boston, Jan 1, 30. Educ: Alpha Univ, BS, 50.SECOND, OTHER PERSON, b New York, Jan 1, 40. Educ: Beta Univ, PhD, 65.')
        self.assertEqual([d['group'] for d in ds],['bachelors'])

    def test_corrupt_year_not_career_date(self):
        ds=self.parse('Educ: Alpha Univ, PhD(chem), Si. Prof Exp: Lab, 60-65.')
        self.assertIsNone(ds[0]['year'])
        self.assertEqual(ds[0]['year_raw'],'')

    def test_unresolved_century(self):
        ds=self.parse('Educ: Alpha Univ, PhD, 80.',None)
        self.assertIsNone(ds[0]['year'])

    def test_literal_four_digit_year(self):
        ds=self.parse('Educ: Alpha Univ, PhD, 1958.',None)
        self.assertEqual(ds[0]['year'],1958)

    def test_multiple_award_years(self):
        ds=self.parse('Educ: Alpha Col, BS, 46 & 47.')
        self.assertEqual([d['year'] for d in ds],[1946,1947])

    def test_education_heading(self):
        self.assertEqual(self.parse('Education: Alpha Univ, MS, 52.')[0]['year'],1952)

    def test_education_subject_is_not_heading(self):
        ds=self.parse('SCIENCE EDUCATION. Educ: Alpha Col, BS, 50.')
        self.assertEqual(ds[0]['institution_as_written'],'Alpha Col')

    def test_diploma_does_not_break_shared_institution(self):
        ds=self.parse('Educ: Alpha Inst, BSc, 55, Dipl Ing, 56, MSc, 59; Beta Univ, PhD(eng), 62.')
        self.assertEqual(ds[1]['institution_as_written'],'Alpha Inst')

    def test_ocr_open_parenthesis(self):
        self.assertEqual(self.parse('Educ: Alpha Univ, PhDlchem eng), 62.')[0]['year'],1962)

    def test_punctuated_label(self):
        ds=self.parse('Educ: Alpha Univ, B.S., 50; Beta Univ, Ph.D.(chem), 55.')
        self.assertEqual([d['year'] for d in ds],[1950,1955])

    def test_duplicate_predictions_penalized(self):
        d=self.parse('Educ: Alpha Univ, BS, 50.')[0]
        metrics,_=evaluate([1],{1:{'degrees':[d,d]}},{1:[d]})
        m=next(x for x in metrics if x['group']=='bachelors' and x['metric']=='complete_tuple')
        self.assertEqual((m['tp'],m['fp'],m['fn']),(1,1,0))


if __name__=='__main__': unittest.main()

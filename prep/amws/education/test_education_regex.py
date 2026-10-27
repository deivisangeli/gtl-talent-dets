import unittest
from education_regex import extract,resolve_year

class EditionParserTests(unittest.TestCase):
    def check(self,text,birth,edition):
        ds=extract(text,birth,edition)['degrees']
        for d in ds:
            self.assertEqual(text[d['start']:d['end']],d['evidence'])
            self.assertEqual(text[d['label_start']:d['label_end']],d['label_as_written'])
            for key in ['institution_as_written','year_raw']:
                if d[key]:self.assertIn(d[key],d['evidence'])
        return ds
    def test_degree_first_bare_school(self):
        ds=self.check('Town, Jan. 1, 82. A.B, Wisconsin, 04, A.M, 05; Ph.D, Berlin, 08. Instr. physics, Other, 09.',1882,1938)
        self.assertEqual([(d['institution_as_written'],d['year'])for d in ds],[('Wisconsin',1904),('Wisconsin',1905),('Berlin',1908)])
    def test_1906_ceiling(self):
        ds=self.check('B.S, Alpha, 87; A.M, Beta, 92; Ph.D, Gamma, 01. Prof. chem.',1860,1906)
        self.assertEqual([d['year']for d in ds],[1887,1892,1901])
    def test_shared_joint_degree_first(self):
        ds=self.check('A.B, Alpha, 08; M.S, Ph.D, Beta, 16. Asst. chem.',1880,1938)
        self.assertEqual([(d['institution_as_written'],d['year'])for d in ds],[('Alpha',1908),('Beta',1916),('Beta',1916)])
    def test_school_first_1986(self):
        ds=self.check('Educ: Univ Alpha, BS, 51, MS, 54, PhD(chem), 58. Prof Exp: Lab, 60.',1930,1986)
        self.assertEqual([d['year']for d in ds],[1951,1954,1958])
        self.assertEqual([d['institution_as_written']for d in ds],['Univ Alpha']*3)
    def test_honorary_preserved(self):
        ds=self.check('Educ: Univ Alpha, BA, 51. Hon Degrees: MS, Univ Beta, 72. Prof Exp: Lab.',1930,1986)
        self.assertEqual([d['honorary']for d in ds],[False,True])
    def test_no_birth_pivot(self):
        ds=self.check('Educ: Univ Alpha, BA, 51, PhD, 60. Prof Exp: Lab.',None,1986)
        self.assertEqual([d['year']for d in ds],[None,None])
    def test_earlier_uniquely_resolved_anchor(self):
        self.assertEqual(resolve_year('97',None,1938),1897)
        self.assertEqual(resolve_year('02',None,1938,1897),1902)
        self.assertIsNone(resolve_year('02',None,1938))
    def test_other_degrees_do_not_leak_school(self):
        ds=self.check('A.B, Alpha, 05; M.D, Beta, 09; Ph.D, Gamma, 16. Instr. bio.',1880,1938)
        self.assertEqual([d['institution_as_written']for d in ds],['Alpha','Beta','Gamma'])
    def test_attendance_not_degree(self):
        self.assertEqual(self.check('Town, 80. Alpha, 99; Beta, 03. Prof. bot. Am. Soc.',1880,1938),[])
    def test_ocr_year_not_employer_year(self):
        ds=self.check('Educ: Univ Alpha, PhD(chem), Si. Prof Exp: Lab, 60.',1930,1986)
        self.assertIsNone(ds[0]['year'])
    def test_medical_bs(self):
        ds=self.check('Educ: Univ Alpha, MB, BS, 51. Prof Exp: Lab.',1930,1986)
        self.assertEqual([d['group']for d in ds],['other_bachelor','other_bachelor'])
    def test_honorary_before_earned(self):
        ds=self.check('A.B, Alpha, 08, hon. Sc.D, 36; A.M, Beta, 09. Instr. bio.',1880,1938)
        self.assertEqual([d['year']for d in ds],[1908,1936,1909])

    def test_ocr_digit_labels_do_not_match_marriage(self):
        ds=self.check('PERSON, M(ARTIN), Town, Jan. 1, 20; m. 42; B.8, Alpha, 43; M.8, 45. Chemist, Lab.',1920,1955)
        self.assertEqual([(d['label_as_written'],d['group'])for d in ds],[('B.8','bachelors'),('M.8','masters_academic')])
    def test_reported_four_digit_conflict_retained(self):
        ds=self.check('Educ: Univ Alpha, BA, 1910. Prof Exp: Lab.',1920,1986)
        self.assertEqual(ds[0]['year'],1910)
        self.assertIn('chronology_conflict',ds[0]['flags'])
        self.assertIsNone(resolve_year('1987',1920,1986))
    def test_damaged_single_digit_unresolved(self):
        ds=self.check('Educ: Univ Alpha, BS, 5. Prof Exp: Lab, 53.',1900,1986)
        self.assertEqual(ds[0]['year_raw'],'5')
        self.assertIsNone(ds[0]['year'])
    def test_ocr_subject_closing_artifact(self):
        ds=self.check('B.S, Alpha, 33; PhJ).(chem), Beta, 37. Research chemist, Lab.',1910,1955)
        self.assertEqual(ds[1]['institution_as_written'],'Beta')
        self.assertEqual(ds[1]['year'],1937)
    def test_ambiguous_century_preserves_literal(self):
        ds=self.check('A.B, Alpha, 02. Instr. bio.',None,1938)
        self.assertEqual(ds[0]['year_raw'],'02')
        self.assertIsNone(ds[0]['year'])
    def test_no_professional_board_bachelor(self):
        ds=self.check('Educ: Univ Alpha, MD, 47; Am Bd Internal Med, dipl, 57. Prof Exp: Lab.',1920,1986)
        self.assertFalse(any(d['group']=='other_bachelor'for d in ds))
    def test_yearless_joint_degree_first(self):
        ds=self.check('A.M, Ph.D, Beta. Prof. chem.',1880,1938)
        self.assertEqual([d['institution_as_written']for d in ds],['Beta','Beta'])
        self.assertEqual([d['year']for d in ds],[None,None])
    def test_philosophy_master_punctuation(self):
        ds=self.check('M.Ph, Alpha, 98. Instr. math.',1870,1906)
        self.assertEqual(ds[0]['group'],'masters_academic')

if __name__=='__main__':unittest.main()

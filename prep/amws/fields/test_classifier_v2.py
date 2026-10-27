import unittest
from classifier_v2 import classify_entry


def entry(field,body=''):
    text='DOE, JANE, b Boston, Jan 1, 40; m 60. '+field+'. Educ: Yale, PhD(physics). Prof Exp: Biology. '+body
    return classify_entry(field,text,'DOE, JANE'),text


class V2Tests(unittest.TestCase):
    def test_new_vocabulary(self):
        fields=['ELECTROMAGNETIC THEORY','SCATTERING THEORY','FLUID DYNAMICS','SOLAR ENERGY',
            'ASTRODYNAMICS','RADIATION PRESSURE EFFECTS','PALEONTOLOGY','BIOSTRATIGRAPHY',
            'PHYSICAL GEOGRAPHY','ELECTRICAL INSULATION','DIELECTRICS','AEROSOL SCIENCE',
            'INDUSTRIAL CHEMICALS','CONTINUOUS CHEMICAL REACTOR DESIGN','REFINERY PROCESS DESIGN',
            'CARBON BLACK','RUBBER FILLERS','CONTROL SYSTEMS','EXPERT SYSTEMS','IMAGE PROCESSING',
            'BIOMETRY','BIOMETEOROLOGY','X-RAY CRYSTALLOGRAPHY','METAL CUTTING','HANDICAPPED COMPUTER AIDS']
        for field in fields:
            with self.subTest(field=field): self.assertTrue(entry(field)[0]['physical_match'])

    def test_compounds_and_policy(self):
        for field in ['BIOCHEMISTRY','BIOPHYSICS','BIOLOGICAL CHEMISTRY','BIOPHYSICAL CHEMISTRY',
                      'CLINICAL CHEMISTRY','PHYSIOLOGICAL CHEMISTRY']:
            self.assertFalse(entry(field)[0]['physical_match'],field)
        for field in ['BIOMEDICAL ENGINEERING','BIOENGINEERING','BIOSTATISTICS','ECONOMIC STATISTICS']:
            self.assertTrue(entry(field)[0]['physical_match'],field)
        self.assertEqual(entry('PHYSICS, PHYSIOLOGY')[0]['other_reason'],'mixed')
        self.assertFalse(entry('HISTORY OF PHYSICS')[0]['physical_match'])
        self.assertFalse(entry('VISUAL PSYCHOPHYSICS')[0]['physical_match'])

    def test_ocr_literal_offsets(self):
        for field in ['PLASMA PHY’SICS','ASTRÓNOMY, ASTROPOHYSICS','PHY SICS','CHEM-ISTRY','BJOCHEMISTRY']:
            p,text=entry(field); self.assertTrue(p['ocr_assisted'],field)
            for ev in p['evidence']:
                source=text if ev['evidence_source']=='raw_text_adjusted' else field
                self.assertEqual(source[ev['start']:ev['end']],ev['evidence'])
                for repair in ev['ocr_repairs']:
                    self.assertEqual(source[repair['start']:repair['end']],repair['original'])
        self.assertFalse(entry('BJOCHEMISTRY')[0]['physical_match'])

    def test_no_arbitrary_fuzzy_or_clause_join(self):
        for field in ['PHYSXCS','PHY, SICS','PHY; SICS','PHY/SICS']:
            self.assertFalse(entry(field)[0]['physical_match'])

    def test_body_degree_affiliation_not_evidence(self):
        text='DOE, JANE, b Boston, Jan 1, 40. Educ: Yale, PhD(physics). Prof Exp: ENGINEERING. Mem: Astronomical Soc. Res: SOLAR ENERGY.'
        for field in ['', 'ENGINEERING','SOLAR ENERGY']:
            self.assertFalse(classify_entry(field,text,'DOE, JANE')['physical_match'])
        text='DOE, JANE, b Boston, Jan 1, 40. EdZZ: Yale Univ, PhD(physics). Prof Exp: Biology.'
        self.assertFalse(classify_entry('',text,'DOE, JANE')['physical_match'])
        p=classify_entry('', 'DOE, JANE, b Boston, Jan 1, 40. p\ufffdf Eip: SOLAR ENERGY.','DOE, JANE')
        self.assertFalse(p['physical_match'])

    def test_name_and_birthplace(self):
        text='PHYSICS, JANE, b Baltimore, Md, Jan 1, 40. BIOCHEMISTRY. Educ: Yale.'
        self.assertFalse(classify_entry('BIOCHEMISTRY',text,'PHYSICS, JANE')['physical_match'])
        self.assertEqual(entry('BIOLOGY')[0]['science_group'],'biological_science')


if __name__=='__main__': unittest.main()

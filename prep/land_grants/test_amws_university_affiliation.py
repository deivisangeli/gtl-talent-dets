"""Sampling/sealing/evidence checks; engine cases added after reference review."""
import copy
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import amws_university_affiliation_calibration as c


class SamplingTests(unittest.TestCase):
    def fixture(self):
        return [dict(source_key=f"{ed}:{i}",edition=ed,biography_fingerprint=f"bio-{ed}-{i}",person_fingerprint=f"person-{ed}-{i}")
                for ed in c.EDITIONS for i in range(400)]

    def test_reproducible_balanced_disjoint_draw(self):
        frame=self.fixture();old={f"{ed}:0" for ed in c.EDITIONS}
        rows,pools=c.draw(frame,old)
        self.assertEqual(len(rows),1000)
        self.assertEqual(len({r['source_key'] for r in rows}),1000)
        self.assertEqual([r['sample_id'] for r in rows],[str(i) for i in range(1,1001)])
        self.assertEqual(c.collections.Counter((r['partition'],r['edition']) for r in rows),{(p,ed):125 for p in ['development','test'] for ed in c.EDITIONS})
        self.assertFalse(old&{r['source_key'] for r in rows})
        self.assertEqual((rows,pools),c.draw(list(reversed(frame)),old))

    def test_test_excludes_duplicate_bio_and_person_but_not_empty_hashes(self):
        frame=self.fixture();initial,_=c.draw(frame,set())
        dev=next(r for r in initial if r['partition']=='development')
        test=next(r for r in initial if r['partition']=='test' and r['edition']==dev['edition'])
        for r in frame:
            if r['source_key']==test['source_key']:r['biography_fingerprint']=dev['biography_fingerprint']
        rows,_=c.draw(frame,set())
        self.assertNotIn(test['source_key'],{r['source_key'] for r in rows if r['partition']=='test'})
        for r in frame:r.update(biography_fingerprint='',person_fingerprint='')
        self.assertEqual(len(c.draw(frame,set())[0]),1000)

    def test_person_signature_alone_prevents_test_leakage(self):
        frame=self.fixture();initial,_=c.draw(frame,set())
        dev=initial[0];candidate=next(r for r in initial if r['partition']=='test' and r['edition']==dev['edition'])
        next(r for r in frame if r['source_key']==candidate['source_key'])['person_fingerprint']=dev['person_fingerprint']
        rows,_=c.draw(frame,set())
        self.assertNotIn(candidate['source_key'],{r['source_key'] for r in rows if r['partition']=='test'})


class SealTests(unittest.TestCase):
    def test_test_biographies_not_accessible_before_freeze(self):
        with tempfile.TemporaryDirectory() as temp:
            with patch.object(c,'destination',return_value=Path(temp)):
                with self.assertRaisesRegex(ValueError,'sealed'):c.sample('test')

    def test_original_evidence_offsets_and_field_restrictions(self):
        row=dict(source_key='1906:1',biography_text='Degree, Uni A. Prof, Uni B, 30-40.',address_text='',birthplace_text='Town')
        ev=c.evidence(row,'biography_text','Prof, Uni B, 30-40.')
        self.assertEqual(row['biography_text'][ev['start']:ev['end']],ev['text'])
        with self.assertRaises(ValueError):c.evidence(row,'birthplace_text','Town')
        with self.assertRaises(ValueError):c.evidence(row,'biography_text','Uni C')

    def test_cached_test_evaluation_cannot_be_repeated(self):
        import amws_university_affiliation_engine as e
        with tempfile.TemporaryDirectory() as temp:
            dest=Path(temp);(dest/'test_evaluation').mkdir()
            c.base.write_json(dest/'frozen_rules.json',dict(frozen_checksums={},engine_sha256=c.base.digest(Path(e.__file__))))
            with patch.object(c,'destination',return_value=dest):
                with self.assertRaisesRegex(ValueError,'Single cached'):c.evaluate_test()


class ExtractorTests(unittest.TestCase):
    def run_entry(self,text,edition='1986',address='',name='Example'):
        import amws_university_affiliation_engine as e
        row=dict(edition=edition,biography_text=text,address_text=address,raw_name=name)
        output=e.extract_affiliations(row)
        for a in output['affiliations']:
            for ev in [a['institution'],*a['contexts'],*a['roles'],*a['dates']]:
                self.assertEqual(row[ev['field']][ev['start']:ev['end']],ev['text'])
        self.assertEqual(output,e.extract_affiliations(row))
        return output

    def test_education_and_memberships_are_bounded_out(self):
        out=self.run_entry('Educ: Univ A, PhD, 60. Prof Exp: Chemist, Industrial Corp, 60-70. Mem: Trustee, Univ B. Res: Biology.')
        self.assertEqual(out['affiliations'],[])

    def test_multiple_appointments_visiting_emeritus_and_dates(self):
        out=self.run_entry('Prof Exp: Asst prof, Alpha Univ, 60-70; EMER PROF, Beta Univ, 70- Concurrent Pos: Vis prof, Gamma Univ, 71-72. Mem: Soc Science.')
        self.assertEqual([a['institution']['text'] for a in out['affiliations']],['Alpha Univ','Beta Univ','Gamma Univ'])
        self.assertEqual(out['affiliations'][0]['temporal_status'],'past')
        self.assertIn('emeritus',out['affiliations'][1]['appointment_types'])
        self.assertIn('visiting',out['affiliations'][2]['appointment_types'])

    def test_closed_appointment_and_retirement_not_current(self):
        out=self.run_entry('Prof Exp: Prof, Alpha Univ, 60-80; RETIRED. Mem: Soc Science.')
        self.assertEqual(out['affiliations'][0]['temporal_status'],'past')

    def test_institution_carry_forward_and_header_consolidation(self):
        out=self.run_entry('Instr. math, Alpha, 01-02; Beta, 02-03. A.A; Math. Soc.',edition='1938',address='Alpha University, City')
        self.assertEqual(len(out['affiliations']),2)
        self.assertEqual({a['institution']['text'] for a in out['affiliations']},{'Alpha','Beta'})

    def test_1955_header_title_and_university_street_not_workplace(self):
        out=self.run_entry('EXAMPLE, PROF. J, 14 College St. @CHEMISTRY@. Born 1900. B.S, Alpha, 20. Chemist, Industry Co, 20-30.',edition='1955',name='EXAMPLE, PROF. J')
        self.assertEqual(out['affiliations'],[])

    def test_independent_institute_and_damaged_heading_are_unresolved(self):
        out=self.run_entry('Prof Exp: Research fellow, Independent Inst, 60-61; prof, Un!v ???? 62. Mem: Soc Science.')
        self.assertEqual(out['affiliations'],[])
        self.assertTrue(out['review_flags'])

    def test_neighbor_after_mailing_address_excluded(self):
        out=self.run_entry('Prof Exp: Prof, Alpha Univ, 80- Mem: Soc Science. Mailing Add: Alpha Univ. NEIGHBOR. Prof Exp: Prof, Beta Univ, 81- Mem: X.')
        self.assertEqual([a['institution']['text'] for a in out['affiliations']],['Alpha Univ'])

    @unittest.expectedFailure
    def test_known_frozen_failure_date_glued_to_membership_marker(self):
        # Added after the cached evaluation to document a failure, not to retune.
        out=self.run_entry('Prof Exp: Prof, Alpha Univ, 80Mem: Am Col Physicians; instructor, Beta Univ, 81. Res: X.')
        self.assertEqual([a['institution']['text'] for a in out['affiliations']],['Alpha Univ'])


if __name__=='__main__':unittest.main()

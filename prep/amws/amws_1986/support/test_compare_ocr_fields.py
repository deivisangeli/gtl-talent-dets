import unittest
import subprocess
import tempfile
from copy import deepcopy
import compare_ocr_fields as audit
import score_ocr_fields as score

class FieldAuditTests(unittest.TestCase):
    def setUp(self):
        self.aliases={'institutions':[['Tex Col Mines','Tex Western Col','Univ Tex El Paso']]}
    def test_sampling_reproducible_and_old_pages_excluded(self):
        docs=[dict(source_file='b.pdf',page_count=20),dict(source_file='a.pdf',page_count=20)]
        old={'a.pdf:1','b.pdf:7'}
        first=audit.page_draw(docs,old)
        self.assertEqual(first,audit.page_draw(list(reversed(docs)),old))
        self.assertTrue(all(f"{d['source_file']}:{p}" not in old for d in first for p in d['proposed_pages']))
    def test_frozen_real_draw(self):
        m=audit.read(audit.ROOT/'manifest.json')
        self.assertEqual(m['page_draws'],audit.page_draw(m['documents'],set(m['previous_audit_pages'])))
        self.assertEqual(sum(d['page_count'] for d in m['documents']),1360)
    def test_campus_not_collapsed(self):
        self.assertFalse(audit.equivalent('Univ Calif, Berkeley','Univ Calif, Davis','institutions',self.aliases))
        self.assertFalse(audit.equivalent('Univ Calif, Berkeley','Univ Calif','institutions',self.aliases))
    def test_school_unit_not_collapsed(self):
        self.assertFalse(audit.equivalent('Col Med, Wayne State Univ','Wayne State Univ','institutions',self.aliases))
    def test_order_retains_distinct_universities(self):
        self.assertFalse(audit.equivalent('Washington Univ','Univ Washington','institutions',self.aliases))
    def test_fixed_alias(self):
        self.assertTrue(audit.equivalent('Tex Col Mines','Tex Western Col','institutions',self.aliases))
        self.assertTrue(audit.equivalent('Univ Calif, Berkeley','University of California, Berkeley','institutions',self.aliases))
    def test_spelling_error_not_forgiven(self):
        self.assertFalse(audit.equivalent('Samford Univ','Stanford Univ','institutions',self.aliases))
        self.assertFalse(audit.equivalent('CRIPS, DEREK J','CRIPPS, DEREK J','name',self.aliases))
    def test_full_birthplace_requires_every_component(self):
        self.assertFalse(audit.equivalent('Paris, France','Paris','birthplace',self.aliases))
        self.assertTrue(audit.equivalent('Boston, Mass','boston, MASSACHUSETTS','birthplace',self.aliases))
    def test_exact_year_and_ambiguous_reference(self):
        self.assertEqual(score.field_score(dict(status='readable',value='1929'),'29','birth_year',self.aliases),'incorrect')
        self.assertEqual(score.field_score(dict(status='ambiguous_reference',value=None),'1929','birth_year',self.aliases),'ambiguous_reference')
    def test_missed_biography_fails_readable_field(self):
        self.assertEqual(score.field_score(dict(status='readable',value='NAME'),'NAME','name',self.aliases,detected=False),'missing')
    def test_source_unreadable_and_absent_separate(self):
        for state in ['source_unreadable','absent']:
            self.assertEqual(score.field_score(dict(status=state,value=None),'X','name',self.aliases),state)
    def test_wrong_person_outcome(self):
        self.assertEqual(score.field_score(dict(status='readable',value='Boston'),'Chicago','birthplace',self.aliases,wrong_person=True),'wrong_person_attribution')
    def test_missing_and_extra_institutions(self):
        r=score.institution_scores(['Harvard Univ','Yale Univ'],['Harvard Univ','MIT'],self.aliases)
        self.assertEqual((r['correct'],r['reference_count'],r['predicted_count'],r['exact']),(1,2,2,False))
    def test_repeated_degrees_same_institution(self):
        r=score.institution_scores(['Harvard Univ'],['Harvard Univ','Harvard Univ'],self.aliases)
        self.assertEqual((r['correct'],r['predicted_count'],r['exact']),(1,1,True))
    def test_historical_alias_does_not_inflate_counts(self):
        r=score.institution_scores(['Tex Col Mines','Tex Western Col'],['Univ Tex El Paso'],self.aliases)
        self.assertEqual((r['correct'],r['reference_count'],r['exact']),(1,1,True))
    def test_unmatched_fragment_lowers_precision_without_recall_credit(self):
        row=dict(institutions=dict(correct=1,reference_count=2,predicted_count=1,reference_status='readable'),unmatched_institution_prediction_count=2)
        self.assertEqual(score.counts([row],'institution_precision'),(1,3))
        self.assertEqual(score.counts([row],'institution_recall'),(1,2))
    def test_duplicate_prediction_cannot_match_two_people(self):
        a=dict(reference_id='a',method='word',source_key='record:1',reviewed=True,review_reason='context reviewed')
        b=dict(a,reference_id='b')
        with self.assertRaisesRegex(AssertionError,'One prediction'):
            score.validate_alignment_rows([a,b],expected=2)
    def test_incomplete_or_unreviewed_delivery_rejected(self):
        with self.assertRaisesRegex(AssertionError,'Incomplete'):
            score.validate_alignment_rows([],expected=608)
        a=dict(reference_id='a',method='word',source_key=None,reviewed=False,review_reason='pending')
        with self.assertRaisesRegex(AssertionError,'Unreviewed'):
            score.validate_alignment_rows([a],expected=1)
    def test_duplicate_cannot_also_be_matched(self):
        a=dict(reference_id='a',method='word',source_key='one',reviewed=True,review_reason='checked',duplicate_keys=['one'])
        with self.assertRaisesRegex(AssertionError,'also labelled duplicate'):
            score.validate_alignment_rows([a],expected=1)
    def test_continuation_never_uses_reference_values(self):
        text=audit.Path(__file__).with_name('extract_ocr_audit.R').read_text(encoding='utf-8')
        self.assertIn('paste(current$raw_text_adjusted,adjusted)',text)
        self.assertNotIn('references.json',text)
        self.assertNotIn('manual_overrides',text)
    def test_real_r_continuation_and_next_biography(self):
        # Whole-document extraction must retain an unsampled next-page continuation.
        with tempfile.TemporaryDirectory(dir=audit.CHAT/'work') as tmp:
            tmp=audit.Path(tmp)
            texts=['SMITH, JOHN ALAN, b Boston, Mass, Jan 1, 30; PHYSICS. Educ: Harvard Univ, BA, 52;',
                'Univ Calif, Berkeley, PhD(physics), 58. Prof Exp: Professor physics.',
                'JONES, ROBERT ALAN, b Chicago, Ill, Jan 2, 31; CHEMISTRY. Educ: Yale Univ, BS, 53. Prof Exp: Professor chemistry.']
            audit.csv_write(tmp/'blocks.csv',[dict(method='paddle',source_file='synthetic.pdf',pdf_page=1 if i==0 else 2,
                block_index=i,block_status='body',text=t) for i,t in enumerate(texts)])
            result=subprocess.run([r'C:\Program Files\R\R-4.5.2\bin\Rscript.exe',str(audit.Path(__file__).with_name('extract_ocr_audit.R')),
                str(audit.ROOT/'frozen/repo'),str(tmp/'blocks.csv'),str(tmp/'parsed.csv')],capture_output=True,text=True)
            self.assertEqual(result.returncode,0,result.stderr)
            with (tmp/'parsed.csv').open(encoding='utf-8-sig',newline='') as f:rows=list(audit.csv.DictReader(f))
            self.assertEqual(len(rows),2)
            self.assertIn('Berkeley',rows[0]['raw_text_adjusted'])
            self.assertNotIn('JONES',rows[0]['raw_text_adjusted'])
            self.assertIn('JONES',rows[1]['name_raw'])

if __name__=='__main__':unittest.main()

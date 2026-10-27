"""Regression checks for faithful evidence and the three-arm comparison gates."""
import copy,inspect,tempfile,unittest
from pathlib import Path
import manual_word_evidence as word
import repaired_ocr_comparison as run
import repaired_comparison_extract as extract
import repair_ocr_paragraphs as repair
import repair_ocr_exceptions as exceptions
from test_repair_ocr_paragraphs import fixture

class RepairedComparisonTests(unittest.TestCase):
    def test_generated_roman_and_letter_labels(self):
        for n,fmt,expected in [(4,'upperRoman','IV'),(29,'lowerRoman','xxix'),(27,'upperLetter','AA'),(28,'lowerLetter','ab'),(7,'decimalZero','07')]:
            self.assertEqual(word.numbering_value(n,fmt),expected)
    def test_resume_binds_inputs_parser_and_output_hash(self):
        with tempfile.TemporaryDirectory() as d:
            stage=Path(d);run.put(stage/'predictions.json',b'[]')
            identity=dict(inputs=['source1'],contract=dict(parser='frozen',decisions='a'))
            run.write(stage/'complete.json',dict(input_manifest=identity,outputs={'predictions.json':run.sha(stage/'predictions.json')}))
            self.assertTrue(extract.complete_valid(stage,identity))
            changed=copy.deepcopy(identity);changed['contract']['decisions']='b'
            self.assertFalse(extract.complete_valid(stage,changed))
            changed=copy.deepcopy(identity);changed['inputs']=['source2']
            self.assertFalse(extract.complete_valid(stage,changed))
            run.put(stage/'predictions.json',b'[1]');self.assertFalse(extract.complete_valid(stage,identity))
    def test_duplicate_wording_keeps_distinct_source_ids(self):
        parent,col=fixture();blocks,audit=repair.repair_reference(parent,col,'source.pdf',1)
        self.assertEqual(audit['assigned_line_count'],4)
        self.assertEqual([b['paragraph_repair']['source_line_indices'] for b in blocks],[[0,1],[2,3]])
        self.assertTrue(all('SAME' in b['block_content'] for b in blocks))
    def test_blank_and_nonreference_blocks_remain_unchanged(self):
        record=dict(status='verified_blank',prediction=dict(parsing_res_list=[]))
        self.assertEqual(repair.repair_page(record,b'approved blank',b'approved blank'),([],[],b'approved blank',b'approved blank'))
        record=dict(source_file='x.pdf',pdf_page_number=1,prediction=dict(column_predictions=[],parsing_res_list=[dict(block_label='text',block_content='Incoming continuation',block_order=1)]))
        blocks,audits,txt,md=repair.repair_page(record,b'Incoming continuation',b'Incoming continuation')
        self.assertEqual(blocks,record['prediction']['parsing_res_list']);self.assertEqual(txt,b'Incoming continuation');self.assertFalse(audits)
    def test_ambiguous_boxes_require_explicit_review(self):
        parent,col=fixture();col['prediction']['layout_det_res']['boxes'][1]['coordinate'][3]=70
        blocks,audit=repair.repair_reference(parent,col,'x.pdf',1)
        self.assertIsNone(blocks);self.assertEqual(audit['status'],'unresolved')
    def test_full_frozen_repair_and_review_accounting(self):
        root=run.ROOT;v=run.read(root/'repair_verification.json');f=run.read(root/'repair_freeze.json')
        self.assertEqual(v['pages'],1360);self.assertEqual(v['blank_pages'],2)
        self.assertEqual(v['totals']['repaired_blocks'],1317);self.assertEqual(v['exceptions_reviewed'],122)
        self.assertEqual(len(f['repaired_pages']),1360);self.assertEqual(run.sha(root/'paragraph_decisions.json'),f['decisions_sha256'])
        cs=run.read(root/'exception_contexts.json');ds=run.read(root/'paragraph_decisions.json')['decisions']
        for c in cs:exceptions.decision_valid(c,ds[c['block_id']])
    def test_seeded_review_draw_is_reproducible(self):
        import random
        root=run.ROOT;m=run.read(root/'manifest.json');rows=run.read(root/'first_pass/audit_report.json')['pages'];sample=run.read(root/'visual_review_sample.json')['pages']
        rng=random.Random(20261024);expected=[]
        for d in m['documents']:
            pool=sorted(r['pdf_page_number'] for r in rows if r['source_file']==d['source_file'] and r['repaired_blocks'])
            expected += [(d['source_file'],p) for p in rng.sample(pool,2)]
        self.assertEqual(expected,[(p['source_file'],p['pdf_page_number']) for p in sample[:18]])
        self.assertTrue(all(p['reviewed'] for p in run.read(root/'visual_review.json')['pages']))
    def test_historical_runner_defaults_are_preserved(self):
        import compare_ocr_fields as original
        self.assertEqual(list(inspect.signature(original.predict).parameters),['root','skip_staged','parsed_only'])
        self.assertEqual(original.METHODS if hasattr(original,'METHODS') else ['word','paddle'],['word','paddle'])
if __name__=='__main__':unittest.main()

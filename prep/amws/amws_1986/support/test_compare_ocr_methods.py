"""Accuracy metrics and matching must not erase recognition errors."""
import unittest
from pathlib import Path
from types import SimpleNamespace
import tempfile
from zipfile import ZipFile
from unittest.mock import patch
import compare_ocr_methods as comparison
from compare_ocr_methods import normalize, levenshtein, prefix_span, metric, locate_name

class ComparisonTests(unittest.TestCase):
    def test_word_nested_textbox_runs_are_read_once_in_xml_order(self):
        xml='''<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>
        <w:p><w:r><w:t>Before</w:t><w:pict><w:txbxContent>
        <w:p><w:r><w:t>Inside</w:t></w:r></w:p>
        </w:txbxContent></w:pict><w:t>After</w:t></w:r></w:p>
        <w:p><w:r><w:t>Next</w:t><w:tab/><w:t>paragraph</w:t></w:r></w:p>
        </w:body></w:document>'''
        with tempfile.TemporaryDirectory() as tmp:
            path=Path(tmp)/'sample.docx'
            with ZipFile(path,'w') as z:z.writestr('word/document.xml',xml)
            self.assertEqual(comparison.read_word(path),['Before','Inside','After','Next\tparagraph'])

    def test_seeded_draw_retains_six_and_adds_three_from_remainder(self):
        pool = [f'{i:02d}.pdf' for i in range(37)]
        first,added = comparison.document_draw(list(reversed(pool)))
        self.assertEqual((first,added),comparison.document_draw(pool))
        self.assertEqual(len(first),6)
        self.assertEqual(len(added),3)
        self.assertFalse(set(first)&set(added))

    def test_approved_37_file_draw_and_page_total(self):
        pool='''16_A_0_200.pdf 16_A_200_400.pdf 16_A_400_775.pdf 16_A_775_850.pdf
        16_B_0_200.pdf 16_B_1000_1200.pdf 16_B_200_400.pdf 16_B_400_600.pdf 16_B_600_800.pdf 16_B_800_1000.pdf
        16_C_0_200.pdf 16_C_200_400.pdf 16_D_0_200.pdf 16_D_200_400.pdf 16_D_400_600.pdf
        16_E_0_200.pdf 16_E_1000_1100.pdf 16_E_1100_1200.pdf 16_E_200_400.pdf 16_E_400_500.pdf 16_E_900_1000.pdf
        16_F_0_200.pdf 16_F_1000_1100.pdf 16_F_100_200.pdf 16_F_1100_1200.pdf 16_F_200_300.pdf
        16_F_300_400.pdf 16_F_500_600.pdf 16_F_600_700.pdf 16_F_700_800.pdf 16_F_800_900.pdf 16_F_900_1000.pdf
        16_G_0_100.pdf 16_G_100_200.pdf 16_G_200_300.pdf 16_G_300_400.pdf 16_G_400_500.pdf'''.split()
        first,added=comparison.document_draw(pool)
        self.assertEqual(set(first),{'16_B_400_600.pdf','16_D_200_400.pdf','16_E_1100_1200.pdf',
                                    '16_E_200_400.pdf','16_E_900_1000.pdf','16_F_800_900.pdf'})
        self.assertEqual(set(added),{'16_A_0_200.pdf','16_D_0_200.pdf','16_F_500_600.pdf'})
        self.assertEqual(sum(comparison.EXPECTED.values()),1360)

    def test_comparison_delivery_rejects_incomplete_checkpoints(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);(root/'ocr').mkdir()
            args=SimpleNamespace(root=root,input_dir=root/'input',outputs=root/'out',dropbox_dir=root/'dropbox')
            with patch.object(comparison.controller,'inventory',return_value=([{}]*9,{})), \
                 patch.object(comparison.controller,'reconcile'), \
                 patch.object(comparison.controller,'verify_complete',return_value={'complete':False,'valid_pages':1359}), \
                 patch.object(comparison,'summarize') as summarize:
                with self.assertRaisesRegex(ValueError,'1360'):
                    comparison.finalize(args)
                summarize.assert_not_called()
                self.assertFalse(args.outputs.exists())
                self.assertFalse(args.dropbox_dir.exists())

    def test_complete_ocr_cannot_deliver_an_unreviewed_audit(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);(root/'ocr').mkdir()
            args=SimpleNamespace(root=root,input_dir=root/'input',outputs=root/'out',dropbox_dir=root/'dropbox')
            with patch.object(comparison.controller,'inventory',return_value=([{}]*9,{})), \
                 patch.object(comparison.controller,'reconcile'), \
                 patch.object(comparison.controller,'verify_complete',return_value={'complete':True,'valid_pages':1360}), \
                 patch.object(comparison,'summarize',return_value={'audit_pages':18,'sampled_biographies':90,'unresolved_alignments':[{'id':'pending'}]}):
                with self.assertRaisesRegex(ValueError,'review is incomplete'):
                    comparison.finalize(args)
                self.assertFalse(args.outputs.exists())
                self.assertFalse(args.dropbox_dir.exists())

    def test_normalization_preserves_case_and_punctuation(self):
        self.assertEqual(normalize('A,  B\nCafe\u0301'), 'A, B Café')
        self.assertNotEqual(normalize('A, B'), normalize('a B'))

    def test_known_edit_distances_and_omission(self):
        self.assertEqual(levenshtein('kitten','sitting'),3)
        self.assertEqual(levenshtein(['one','two'],['one','three']),1)
        m=metric('one two','')
        self.assertEqual(m['cer'],1)
        self.assertEqual(m['wer'],1)

    def test_anchor_ignores_punctuation_for_location_only(self):
        result=locate_name("D'ANGELO, HENRY", "before D ANGELO. HENRY, b New York")
        self.assertEqual(result['start'],7)
        self.assertEqual(result['name_edits'],0)
        m=metric("D'ANGELO, HENRY",'D ANGELO. HENRY')
        self.assertGreater(m['cer'],0)

    def test_prefix_retains_internal_recognition_error(self):
        hypothesis,end=prefix_span('NAME, b Boston, 1930.','NAME, b Bostan, 1930. Another biography follows',0)
        self.assertEqual(hypothesis,'NAME, b Bostan, 1930.')
        self.assertEqual(metric('NAME, b Boston, 1930.',hypothesis)['character_errors'],1)

if __name__=='__main__': unittest.main()

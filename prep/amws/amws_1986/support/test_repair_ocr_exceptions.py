"""Exception assignment, review binding, and immutable asset checks."""
import copy
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import zipfile

import repair_ocr_exceptions as repair
import repair_ocr_corpus as corpus


def fixture():
    return dict(block_id='block', source_file='input.pdf', pdf_page_number=1, column_number=2,
                original_block_order=3, scan_sha256='scan',
                candidates=[dict(source_region_index=5, bbox=[100, 0, 200, 55]),
                            dict(source_region_index=6, bbox=[100, 50, 200, 100])],
                lines=[dict(source_line_index=i, text=text, confidence=.9, bbox=[100, y-2, 190, y+2], y=y, owners=owners)
                       for i, text, y, owners in [(0, 'HEADER', 5, [0]), (1, 'SAME', 20, [0]),
                           (2, 'SAME', 75, [1]), (3, '', 105, [])]])


class ExceptionTests(unittest.TestCase):
    def baseline_fixture(self, root):
        ctx = fixture()
        for line in ctx['lines']:
            line['local_bbox'] = line['bbox'][:]
        parent = dict(block_label='reference', block_content='HEADER\nSAME\nSAME',
                      block_bbox=[100, 0, 200, 110], column_number=2, block_order=3)
        ctx.update(parent=parent, original_block_bbox=parent['block_bbox'])
        before = dict(original_record={'prediction': {'saved': True}},
                      corrected_prediction=dict(parsing_res_list=[parent], gutter={'curve': [1, 2]}),
                      repair_audit=[], exports={})
        paths = corpus.page_paths(root, 'input.pdf', 1)
        corpus.write_json(paths['json'], before)
        for ext in ('txt', 'md'):
            corpus.write_bytes(paths[ext], b'UNCHANGED\r\n\r\nHEADER\r\nSAME\r\nSAME\r\n')
        decision = dict(context_sha256=repair.digest(ctx), scan_sha256='scan', reviewed_at='now',
                        notes='Scan inspected', review_status='accepted', groups=repair.automatic_groups(ctx))
        return ctx, decision, before, paths

    def test_derived_page_keeps_empty_audit_and_unaffected_export_bytes(self):
        with tempfile.TemporaryDirectory() as tmp:
            ctx, decision, before, paths = self.baseline_fixture(Path(tmp))
            derived, exports, audits = repair.derive(Path(tmp), 'input.pdf', 1, [ctx], {'block': decision})
            self.assertEqual(exports['txt'], b'UNCHANGED\r\n\r\nHEADER\r\nSAME\r\n\r\nSAME\r\n')
            self.assertEqual(derived['original_record'], before['original_record'])
            self.assertEqual(derived['corrected_prediction']['gutter'], before['corrected_prediction']['gutter'])
            self.assertEqual(audits[0]['assigned_line_count'], 3)
            self.assertEqual(audits[0]['empty_detection_records'][0]['source_line_index'], 3)
            self.assertEqual(audits[0]['empty_detection_records'][0]['local_bbox'], ctx['lines'][3]['bbox'])
            self.assertEqual(paths['txt'].read_bytes(), b'UNCHANGED\r\n\r\nHEADER\r\nSAME\r\nSAME\r\n')

    def test_unresolved_block_keeps_exports_and_accounts_for_nonempty_records(self):
        with tempfile.TemporaryDirectory() as tmp:
            ctx, decision, before, paths = self.baseline_fixture(Path(tmp))
            decision.update(review_status='retained_unresolved', reason='ambiguous margin fragment')
            decision.pop('groups')
            derived, exports, audits = repair.derive(Path(tmp), 'input.pdf', 1, [ctx], {'block': decision})
            self.assertEqual(exports['txt'], paths['txt'].read_bytes())
            self.assertEqual(derived['corrected_prediction'], before['corrected_prediction'])
            self.assertEqual(len(audits[0]['retained_unresolved_detection_records']), 3)
            self.assertEqual(len(audits[0]['empty_detection_records']), 1)

    def test_reference_content_omission_is_rejected_before_writing(self):
        with tempfile.TemporaryDirectory() as tmp:
            ctx, decision, before, paths = self.baseline_fixture(Path(tmp))
            ctx['parent']['block_content'] += ' LOST'
            with self.assertRaisesRegex(ValueError, 'omits reference content'):
                repair.derive(Path(tmp), 'input.pdf', 1, [ctx], {'block': decision})

    def test_fresh_resume_identity_changes_with_decision_and_baseline(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            paths = {}
            for ext in ('json', 'txt', 'md', 'scan', 'viewer'):
                paths[ext] = root / ext
                paths[ext].write_bytes(ext.encode())
            ctx = fixture()
            original = repair.page_identity(paths, [ctx], {'block': {'notes': 'first'}})
            self.assertNotEqual(original, repair.page_identity(paths, [ctx], {'block': {'notes': 'changed'}}))
            paths['txt'].write_bytes(b'changed baseline')
            self.assertNotEqual(original, repair.page_identity(paths, [ctx], {'block': {'notes': 'first'}}))

    def test_interrupted_blank_page_recomputes_missing_output_without_ocr(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            baseline, source, output = root/'baseline', root/'source', root/'output'
            paths = corpus.page_paths(baseline, 'input.pdf', 1)
            record = dict(status='verified_blank', source_file='input.pdf', pdf_page_number=1)
            before = dict(original_record=record, corrected_prediction={'parsing_res_list': []},
                          source_sha256={}, repair_audit=[])
            corpus.write_json(paths['json'], before)
            for ext in ('txt', 'md'):
                corpus.write_bytes(paths[ext], b'[APPROVED BLANK]\r\n')
                corpus.write_bytes(corpus.page_paths(source, 'input.pdf', 1)[ext], b'[APPROVED BLANK]\r\n')
            for ext in ('scan', 'viewer'):
                corpus.write_bytes(paths[ext], ext.encode())
            corpus.write_json(baseline/'corpus_selection.json', dict(source_dir=str(source)))
            doc = dict(source_file='input.pdf')
            row, skipped = repair.process_page(baseline, output, doc, 1, [], {}, {})
            self.assertFalse(skipped)
            self.assertEqual(row['outcome'], 'verified_blank')
            self.assertTrue(repair.process_page(baseline, output, doc, 1, [], {}, {})[1])
            corpus.page_paths(output, 'input.pdf', 1)['md'].unlink()
            self.assertFalse(repair.process_page(baseline, output, doc, 1, [], {}, {})[1])
            self.assertEqual(paths['md'].read_bytes(), b'[APPROVED BLANK]\r\n')

    def test_exception_zip_checks_manifest_member_and_space_headroom(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)/'corpus'
            corpus.write_json(root/'verification.json', dict(complete=True))
            corpus.write_json(root/'visual_review_sample.json', dict(pages=[]))
            corpus.write_json(root/'visual_review.json', dict(pages=[]))
            corpus.write_bytes(root/'text.txt', b'OCR strings\n')
            result = repair.package(root)
            self.assertTrue(result['member_hashes_verified'])
            with zipfile.ZipFile(root.with_suffix('.zip')) as z:
                self.assertEqual(z.read('text.txt'), b'OCR strings\n')
                self.assertEqual(repair.hashlib.sha256(z.read('output_manifest.json')).hexdigest(), result['manifest_sha256'])
            from types import SimpleNamespace
            with patch.object(repair.shutil, 'disk_usage', return_value=SimpleNamespace(free=repair.RESERVE)):
                with self.assertRaisesRegex(OSError, 'headroom'):
                    repair.package(root)

    def test_overlap_with_unique_ownership_and_empty_record_is_repairable(self):
        ctx = fixture()
        groups = repair.automatic_groups(ctx)
        self.assertEqual([g['source_line_indices'] for g in groups], [[0, 1], [2]])
        blocks = repair.replacements(ctx, groups)
        self.assertEqual([b['block_content'] for b in blocks], ['HEADER\nSAME', 'SAME'])
        self.assertEqual(blocks[0]['paragraph_repair']['lines'][0]['confidence'], .9)

    def test_genuine_ambiguity_is_not_automatically_accepted(self):
        ctx = fixture()
        ctx['lines'][1]['owners'] = [0, 1]
        self.assertIsNone(repair.automatic_groups(ctx))

    def test_nonempty_whitespace_and_fragments_cannot_be_discarded(self):
        ctx = fixture()
        ctx['lines'][3]['text'] = ' '
        self.assertIsNone(repair.automatic_groups(ctx))
        with self.assertRaises(ValueError):
            repair.validate_groups(ctx, [dict(source_line_indices=[0, 1]), dict(source_line_indices=[2])])

    def test_headers_continuations_and_short_entries_use_explicit_groups(self):
        ctx = fixture()
        groups = [dict(source_line_indices=[0], block_label='header'),
                  dict(source_line_indices=[1], block_label='text'), dict(source_line_indices=[2], block_label='text')]
        blocks = repair.replacements(ctx, groups)
        self.assertEqual([b['block_label'] for b in blocks], ['header', 'text', 'text'])
        self.assertEqual([b['block_content'] for b in blocks], ['HEADER', 'SAME', 'SAME'])

    def test_duplicate_missing_and_interleaved_assignments_are_rejected(self):
        ctx = fixture()
        for groups in [[dict(source_line_indices=[0, 1]), dict(source_line_indices=[1, 2])],
                       [dict(source_line_indices=[0]), dict(source_line_indices=[2])],
                       [dict(source_line_indices=[0, 2]), dict(source_line_indices=[1])]]:
            with self.subTest(groups=groups), self.assertRaises(ValueError):
                repair.validate_groups(ctx, groups)

    def test_review_decisions_are_bound_to_current_source_scan_and_notes(self):
        ctx = fixture()
        decision = dict(context_sha256=repair.digest(ctx), scan_sha256='scan', reviewed_at='now',
                        notes='Scan checked', review_status='accepted', groups=repair.automatic_groups(ctx))
        repair.decision_valid(ctx, decision)
        for key, value in [('context_sha256', 'stale'), ('scan_sha256', 'wrong'), ('notes', '')]:
            with self.subTest(key=key), self.assertRaises(ValueError):
                repair.decision_valid(ctx, dict(decision, **{key: value}))

    def test_atomic_replacement_of_linked_file_keeps_baseline_bytes(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source, target = root/'baseline.txt', root/'derived.txt'
            source.write_bytes(b'original')
            repair.linked_copy(source, target)
            corpus.write_bytes(target, b'changed')
            self.assertEqual(source.read_bytes(), b'original')
            self.assertEqual(target.read_bytes(), b'changed')

    def test_resume_requires_current_decisions_and_complete_hashes(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            output = root/'output.txt'
            output.write_bytes(b'result')
            checkpoint = root/'checkpoint.json'
            identity = dict(version=1, decisions_sha256='accepted')
            outputs = {e: dict(path='output.txt', sha256=repair.first.sha256(output))
                       for e in ('json', 'txt', 'md', 'scan', 'viewer')}
            corpus.write_json(checkpoint, dict(identity=identity, outputs=outputs))
            self.assertTrue(repair.checkpoint_valid(root, checkpoint, identity))
            self.assertFalse(repair.checkpoint_valid(root, checkpoint, dict(identity, decisions_sha256='changed')))
            corpus.write_json(checkpoint, dict(identity=identity, outputs={'txt': outputs['txt']}))
            self.assertFalse(repair.checkpoint_valid(root, checkpoint, identity))
            corpus.write_json(checkpoint, dict(identity=identity, outputs=outputs))
            output.unlink()
            self.assertFalse(repair.checkpoint_valid(root, checkpoint, identity))


if __name__ == '__main__':
    unittest.main()

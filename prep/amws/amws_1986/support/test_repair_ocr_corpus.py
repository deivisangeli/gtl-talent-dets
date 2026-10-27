"""Corpus accounting, resume and delivery checks using small local fixtures."""
import copy
import hashlib
import json
from pathlib import Path
from types import SimpleNamespace
import tempfile
import unittest
from unittest.mock import patch
import zipfile

import repair_ocr_corpus as corpus
import repair_ocr_paragraphs as repair
from test_repair_ocr_paragraphs import fixture


class CorpusTests(unittest.TestCase):
    def test_inventory_keeps_every_one_based_page_and_rejects_duplicate_docs(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            pdf = root / "input.pdf"
            pdf.write_bytes(b"source")
            stat = pdf.stat()
            doc = dict(source_file="input.pdf", page_count=3,
                       source=dict(path=str(pdf), size_bytes=stat.st_size, mtime_ns=stat.st_mtime_ns))
            corpus.write_json(root / "selection.json", dict(documents=[doc]))
            self.assertEqual(corpus.inventory(root), [doc])
            self.assertEqual([corpus.page_paths(root, doc["source_file"], p)["json"].name for p in range(1, 4)],
                             ["page_0001.json", "page_0002.json", "page_0003.json"])
            corpus.write_json(root / "selection.json", dict(documents=[doc, doc]))
            with self.assertRaises(ValueError):
                corpus.inventory(root)

    def test_approved_blank_is_exported_without_invented_ocr(self):
        marker = b"[VISUALLY VERIFIED BLANK PAGE | input.pdf | PDF page 1]\r\n"
        record = dict(status="verified_blank", prediction=dict(parsing_res_list=[]))
        blocks, audits, txt, md = repair.repair_page(record, marker, marker)
        self.assertEqual((blocks, audits, txt, md), ([], [], marker, marker))
        self.assertEqual(corpus.outcome(record, audits), "verified_blank")

    def test_mixed_page_repairs_clear_column_and_preserves_failed_column(self):
        parent, col = fixture()
        left = copy.deepcopy(col)
        left["column_number"], left["x_offset"] = 1, 0
        left_parent = copy.deepcopy(parent)
        left_parent.update(column_number=1, block_order=1, block_bbox=[0, 0, 90, 100], block_content="bad left")
        left["prediction"]["layout_det_res"]["boxes"][1]["coordinate"][3] = 70
        record = dict(status="ok", source_file="input.pdf", pdf_page_number=1,
                      prediction=dict(column_predictions=[left, col], parsing_res_list=[left_parent, parent]))
        txt = b"bad left\n\ncollapsed\n"
        blocks, audits, changed, md = repair.repair_page(record, txt, txt)
        self.assertEqual(corpus.outcome(record, audits), "partially_repaired")
        self.assertEqual(blocks[0]["block_content"], "bad left")
        self.assertTrue(changed.startswith(b"bad left\n\n"))
        self.assertEqual(len([b for b in blocks if "paragraph_repair" in b]), 2)
        self.assertEqual(len([a for a in audits if a["status"] == "unresolved"]), 1)

    def test_resume_requires_source_version_and_all_output_hashes(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            outputs = {}
            for ext in ("json", "txt", "md", "viewer", "scan"):
                path = root / f"output.{ext}"
                path.write_bytes(ext.encode())
                outputs[ext] = dict(path=path.name, sha256=repair.sha256(path))
            checkpoint = root / "checkpoint.json"
            source_hashes = dict(json="original", txt="original", md="original")
            saved = dict(corpus_version=corpus.CORPUS_VERSION, repair_version=repair.VERSION,
                         source_sha256=source_hashes, outputs=outputs)
            corpus.write_json(checkpoint, saved)
            self.assertTrue(corpus.checkpoint_valid(root, checkpoint, source_hashes))
            self.assertFalse(corpus.checkpoint_valid(root, checkpoint, dict(source_hashes, txt="changed")))
            (root / "output.txt").write_bytes(b"corrupted")
            self.assertFalse(corpus.checkpoint_valid(root, checkpoint, source_hashes))
            (root / "output.txt").write_bytes(b"txt")
            saved["repair_version"] += 1
            corpus.write_json(checkpoint, saved)
            self.assertFalse(corpus.checkpoint_valid(root, checkpoint, source_hashes))

    def test_document_exports_require_all_pages_and_preserve_markers(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            doc = dict(source_file="input.pdf", page_count=2)
            for page in (1, 2):
                paths = corpus.page_paths(root, doc["source_file"], page)
                for ext in ("txt", "md"):
                    corpus.write_bytes(paths[ext], f"page {page}\r\n".encode())
            corpus.document_exports(root, doc)
            text = (root / "input" / "document.txt").read_bytes()
            self.assertEqual(text, b"[input.pdf | PDF page 1]\npage 1\r\n\n[input.pdf | PDF page 2]\npage 2\r\n")
            corpus.page_paths(root, doc["source_file"], 2)["md"].unlink()
            with self.assertRaises(ValueError):
                corpus.document_exports(root, doc)

    def test_resume_skips_render_and_missing_scan_recomputes_page(self):
        parent, column = fixture()
        record = dict(status="ok", source_file="input.pdf", pdf_page_number=1, pdf_page_count=1,
                      prediction=dict(width=200, height=100, column_predictions=[column], parsing_res_list=[parent]))
        original = dict(json=b"original", txt=b"collapsed\n", md=b"collapsed\n")
        hashes = {ext: hashlib.sha256(v).hexdigest() for ext, v in original.items()}
        with tempfile.TemporaryDirectory() as tmp, patch.object(corpus, "source_page", return_value=(record, original, hashes)):
            root = Path(tmp)
            def render(pdf, page, path):
                corpus.write_bytes(path, b"scan")
            with patch.object(corpus, "render", side_effect=render) as renderer:
                doc = dict(source_file="input.pdf")
                row, skipped = corpus.process_page(root / "source", root, doc, 1, None, {})
                self.assertFalse(skipped)
                row2, skipped = corpus.process_page(root / "source", root, doc, 1, None, {})
                self.assertTrue(skipped)
                self.assertEqual(row, row2)
                self.assertEqual(renderer.call_count, 1)
                corpus.page_paths(root, "input.pdf", 1)["scan"].unlink()
                self.assertFalse(corpus.process_page(root / "source", root, doc, 1, None, {})[1])
                self.assertEqual(renderer.call_count, 2)

    def test_review_queue_is_per_block_and_index_has_no_page_text(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            audits = [dict(status="unresolved", reason="uncovered", column_number=2, original_block_order=1,
                           original_block_bbox=[0, 0, 10, 10])]
            row = dict(source_file="input.pdf", pdf_page_number=1, outcome="unresolved_only", repaired_blocks=0,
                       recovered_paragraphs=0, retained_lines=0, unresolved_blocks=1,
                       block_audits=audits, viewer="views/input_page_0001.html")
            totals = corpus.reports(root, [dict(source_file="input.pdf")], [row])
            self.assertEqual(totals["review_pages"], 1)
            self.assertEqual(len(corpus.load(root / "review_queue.json")["blocks"]), 1)
            self.assertNotIn('"block_content"', (root / "index.html").read_text())

    def test_package_requires_completed_visual_review_and_checks_members(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "run"
            root.mkdir()
            derived = corpus.page_paths(root, "input.pdf", 1)["json"]
            corpus.write_bytes(derived, b"derived")
            digest = repair.sha256(derived)
            corpus.write_json(root / "verification.json", dict(complete=True))
            corpus.write_json(root / "visual_review_sample.json", dict(pages=[dict(source_file="input.pdf", pdf_page_number=1, derived_json_sha256=digest)]))
            corpus.write_json(root / "visual_review.json", dict(pages=[]))
            with self.assertRaises(ValueError):
                corpus.package(root)
            corpus.write_json(root / "visual_review.json", dict(pages=[dict(source_file="input.pdf", pdf_page_number=1, derived_json_sha256=digest, review_status="passed")]))
            result = corpus.package(root)
            self.assertTrue(result["crc_verified"] and result["member_hashes_verified"])
            with zipfile.ZipFile(result["archive"]) as z:
                self.assertIsNone(z.testzip())
                self.assertIn("output_manifest.json", z.namelist())
            corpus.write_bytes(derived, b"changed")
            with self.assertRaises(ValueError):
                corpus.package(root)


if __name__ == "__main__":
    unittest.main()

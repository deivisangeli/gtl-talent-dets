"""Small integration checks for selection, checkpoint recovery and page accounting."""
import importlib.util
import json
from pathlib import Path
import tempfile
import sys
from types import SimpleNamespace
import unittest
from unittest.mock import patch

SCRIPT = Path(__file__).resolve().parents[1] / "run_paddleocr_ed16.py"
spec = importlib.util.spec_from_file_location("ocr", SCRIPT)
ocr = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ocr)
sys.path.insert(0, str(SCRIPT.parent))
import resume_paddleocr_ed16 as controller


class RunnerTests(unittest.TestCase):
    def test_reviewed_title_page_keeps_actual_single_column_config(self):
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp);source=root/'example.pdf';source.write_bytes(b'title source')
            image=root/'title.png';image.write_bytes(b'original scan')
            folder=root/'example';folder.mkdir()
            review=dict(layout='single_column_nonbiography',review_authorization='task_authorized_visual_review',
                        reviewer='Codex',notes='Title page has centered text, no biography columns.',
                        source=ocr.source_identity(source),pdf_page_number=2,review_image=str(image),
                        review_image_sha256=controller.sha256(image))
            base=dict(column_mode='two',dpi=300)
            payload=dict(gutter_x=None,parsing_res_list=[dict(block_content='TITLE')],
                         column_predictions=[dict(prediction=dict(parsing_res_list=[dict(block_content='TITLE')]))])
            record=dict(status='ok',config=dict(base,column_mode='single'),pdf_page_number=2,
                        source_file='example.pdf',source_path=str(source),text_characters=5,
                        layout_review=review,prediction=payload)
            ocr.atomic_json(folder/'page_0002.json',record)
            (folder/'page_0002.txt').write_text('TITLE\n');(folder/'page_0002.md').write_text('TITLE\n')
            self.assertTrue(ocr.checkpoint_valid(folder,2,base))
            del record['layout_review']
            ocr.atomic_json(folder/'page_0002.json',record)
            self.assertFalse(ocr.checkpoint_valid(folder,2,base))
            record['layout_review']=review;review['pdf_page_number']=3
            ocr.atomic_json(folder/'page_0002.json',record)
            self.assertFalse(ocr.checkpoint_valid(folder,2,base))
            with self.assertRaises(SystemExit):ocr.parse_args(['--layout-review',str(image)])

    def test_frozen_comparison_manifest_checks_pairs_hashes_and_totals(self):
        class PDF:
            def __init__(self, path): pass
            def __enter__(self): return self
            def __exit__(self, *args): pass
            def __len__(self): return 3
        with tempfile.TemporaryDirectory() as tmp:
            root=Path(tmp); inputs=root/'input'; output=root/'output'
            inputs.mkdir(); output.mkdir()
            pdf=inputs/'16_A_0_200.pdf'; word=inputs/'16_A_0_200.docx'
            pdf.write_bytes(b'pdf'); word.write_bytes(b'baseline')
            item=dict(source_file=pdf.name, source=ocr.source_identity(pdf),
                      baseline=ocr.source_identity(word), page_count=3,
                      pdf_sha256=controller.sha256(pdf), docx_sha256=controller.sha256(word),
                      priority_pages=[2])
            manifest=root/'frozen.json'
            ocr.atomic_json(manifest, dict(total_pages=3, documents=[item]))
            args=SimpleNamespace(input_dir=inputs,output_dir=output,selection_manifest=manifest)
            with patch('pypdfium2.PdfDocument',PDF), patch.object(controller.ocr,'config_identity',return_value={}):
                docs,config=controller.inventory(args)
                self.assertEqual(len(docs),1)
                self.assertEqual(docs[0]['priority_pages'],[2])
                ocr.atomic_json(manifest,dict(total_pages=4,documents=[item]))
                with self.assertRaisesRegex(ValueError,'total'):
                    controller.inventory(args)
                ocr.atomic_json(manifest,dict(total_pages=3,documents=[item,item]))
                with self.assertRaisesRegex(ValueError,'unique'):
                    controller.inventory(args)
                ocr.atomic_json(manifest,dict(total_pages=3,documents=[item]))
                word.write_bytes(b'changed baseline')
                with self.assertRaisesRegex(ValueError,'Word baseline'):
                    controller.inventory(args)

    def test_transcribed_selection_is_explicit_and_still_excludes_index(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for name in ["16_A_0_200.pdf", "16_A_0_200.DOCX", "16_Index_0_100.pdf"]:
                (root / name).touch()
            self.assertEqual(ocr.select_pdfs(root), [])
            self.assertEqual(ocr.select_pdfs(root, "16_a_0_200.PDF", True),
                             [(root / "16_A_0_200.pdf").resolve()])
            with self.assertRaises(ValueError):
                ocr.select_pdfs(root, allow_transcribed=True)
            with self.assertRaises(ValueError):
                ocr.select_pdfs(root, "16_Index_0_100.pdf", True)
            with self.assertRaises(SystemExit):
                ocr.parse_args(["--allow-transcribed"])

    def test_dynamic_page_accounting(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            folder = root / "example"
            folder.mkdir()
            ocr.atomic_json(folder / "manifest.json", dict(page_count=3, pages={
                "1": {"status": "ok"}, "2": {"status": "failed"}}))
            result = controller.progress(root, [{"source_file": "example.pdf", "page_count": 3}], {}, "running")
            self.assertEqual(result["total_pages"], 3)
            self.assertEqual(result["completed_pages"], 1)
            self.assertEqual(result["failed_pages"], 1)
            self.assertEqual(result["pending_pages"], 1)

    def test_reviewed_curved_gutter_masks_without_losing_coordinates(self):
        import numpy as np
        image = np.zeros((400, 600, 3), dtype=np.uint8)
        class Structure:
            calls = []
            def predict(self, input):
                self.calls.append(input.copy())
                return [SimpleNamespace(json={"res": {"parsing_res_list": [
                    {"block_label": "text", "block_bbox": [10, 20, 30, 40], "block_content": "text"}]}},
                    markdown={"markdown_texts": "text"})]
        structure = Structure()
        payload = ocr.prediction_payload(ocr.ColumnPipeline(structure, None,
                    gutter_points=[[0, 350], [400, 250]]).predict(image)[0])
        self.assertTrue(np.all(structure.calls[0][-1, 251:] == 255))
        self.assertTrue(np.all(structure.calls[1][0, :100] == 255))
        self.assertEqual(payload["column_predictions"][1]["x_offset"], 250)
        self.assertEqual(payload["parsing_res_list"][1]["block_bbox"][0], 260)
        self.assertEqual(payload["gutter_method"], "page_specific_visual_review")

    def test_blank_checkpoint_requires_approval_and_unchanged_review(self):
        import hashlib
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source = root / "example.pdf"
            source.write_bytes(b"blank scan")
            folder = root / "example"
            folder.mkdir()
            review = root / "review.png"
            review.write_bytes(b"reviewed image")
            marker = "[VISUALLY VERIFIED BLANK PAGE | example.pdf | PDF page 1]\n"
            record = dict(status="verified_blank", source_file="example.pdf", source_path=str(source),
                    config={}, pdf_page_number=1, text_characters=0,
                    prediction={"parsing_res_list": []}, blank_verification=dict(user_approved=True,
                    source=ocr.source_identity(source), review_image="review.png",
                    review_image_sha256=hashlib.sha256(review.read_bytes()).hexdigest()))
            ocr.atomic_json(folder / "page_0001.json", record)
            (folder / "page_0001.txt").write_text(marker)
            (folder / "page_0001.md").write_text(marker)
            self.assertTrue(ocr.checkpoint_valid(folder, 1, {}))
            record["blank_verification"]["user_approved"] = False
            ocr.atomic_json(folder / "page_0001.json", record)
            self.assertFalse(ocr.checkpoint_valid(folder, 1, {}))
            record['blank_verification'].update(review_authorization='task_authorized_visual_review',
                                                reviewer='Codex',notes='Original scan inspected; blank reverse side.')
            ocr.atomic_json(folder / 'page_0001.json',record)
            self.assertTrue(ocr.checkpoint_valid(folder,1,{}))
            record['blank_verification']['notes']=''
            ocr.atomic_json(folder / 'page_0001.json',record)
            self.assertFalse(ocr.checkpoint_valid(folder,1,{}))
            record["blank_verification"]["user_approved"] = True
            ocr.atomic_json(folder / "page_0001.json", record)
            review.write_bytes(b"different scan")
            self.assertFalse(ocr.checkpoint_valid(folder, 1, {}))

    def test_resume_rejects_empty_and_mismatched_exports(self):
        with tempfile.TemporaryDirectory() as tmp:
            folder = Path(tmp) / "example"
            folder.mkdir()
            record = dict(status="ok", config={}, pdf_page_number=1,
                          source_file="example.pdf", text_characters=0,
                          prediction={"parsing_res_list": []})
            ocr.atomic_json(folder / "page_0001.json", record)
            (folder / "page_0001.txt").write_text("\n")
            (folder / "page_0001.md").write_text("\n")
            self.assertFalse(ocr.checkpoint_valid(folder, 1, {}))
            record.update(text_characters=4, prediction={"parsing_res_list": [{"block_content": "text"}]})
            ocr.atomic_json(folder / "page_0001.json", record)
            (folder / "page_0001.txt").write_text("text\n")
            (folder / "page_0001.md").write_text("text\n")
            self.assertTrue(ocr.checkpoint_valid(folder, 1, {}))
            (folder / "page_0001.txt").write_text("corrupt\n")
            self.assertFalse(ocr.checkpoint_valid(folder, 1, {}))

    def test_recovery_preserves_originals_and_verifies_hashes(self):
        with tempfile.TemporaryDirectory() as tmp:
            source, target = Path(tmp) / "original", Path(tmp) / "local"
            source.mkdir()
            target.mkdir()
            (source / "page.json").write_text("original checkpoint")
            controller.recover(source, target)
            self.assertEqual(controller.sha256(source / "page.json"), controller.sha256(target / "page.json"))
            self.assertEqual((source / "page.json").read_text(), "original checkpoint")
            self.assertEqual(controller.read_json(target / "recovery_copy.json")["file_count"], 1)

    def test_worker_failure_retries_smaller_ranges_and_skips_valid_pages(self):
        good, calls = {1, 2}, []
        def fake_worker(root, input_dir, doc, first, count):
            calls.append((first, count))
            if count <= 2:
                good.update(range(first, first + count))
        with patch.object(controller, "valid", side_effect=lambda root, doc, page, config: page in good), \
             patch.object(controller, "progress"), patch.object(controller, "worker", side_effect=fake_worker):
            controller.run_range(Path("local"), Path("input"), {"source_file": "example.pdf"}, 1, 10, {}, [])
            self.assertEqual(good, set(range(1, 11)))
            self.assertEqual(calls, [(1, 10), (3, 2), (5, 2), (7, 2), (9, 2)])

    def test_incomplete_corpus_cannot_trigger_delivery(self):
        with tempfile.TemporaryDirectory() as tmp:
            with patch.object(sys, "argv", ["controller", "--output-dir", tmp, "--verify-only"]), \
                 patch.object(controller.ocr, "output_lock"), \
                 patch.object(controller, "inventory", return_value=([], {})), \
                 patch.object(controller, "reconcile"), patch.object(controller, "progress"), \
                 patch.object(controller, "verify_complete", return_value={"complete": False, "failures": [{"page": 1}]}), \
                 patch.object(controller.logging, "basicConfig"), \
                 patch.object(controller.logging, "FileHandler"), \
                 patch.object(controller, "deliver") as deliver:
                self.assertEqual(controller.main(), 1)
                deliver.assert_not_called()

    def test_zip_delivery_preserves_local_and_matches_hash(self):
        with tempfile.TemporaryDirectory() as tmp:
            root, target = Path(tmp) / "local", Path(tmp) / "dropbox"
            root.mkdir()
            (root / "page_0001.txt").write_text("raw OCR\n")
            controller.deliver(root, target)
            report = controller.read_json(root / "delivery.json")
            self.assertEqual(controller.sha256(Path(report["local_zip"])), report["sha256"])
            self.assertEqual(controller.sha256(Path(report["dropbox_zip"])), report["sha256"])
            self.assertTrue((root / "page_0001.txt").exists())
            self.assertEqual(len(list(target.iterdir())), 1)

    def test_separate_columns_preserve_scores_and_coordinate_offsets(self):
        import numpy as np
        image = np.full((400, 600, 3), 255, dtype=np.uint8)
        image[40:360, 50:250] = 0
        image[40:360, 350:550] = 0
        class Structure:
            calls = 0
            def predict(self, input):
                self.calls += 1
                text = "left column" if self.calls == 1 else "right column"
                return [SimpleNamespace(json={"res": {
                    "parsing_res_list": [{"block_label": "text", "block_bbox": [10, 20, 30, 40],
                                          "block_content": text}],
                    "overall_ocr_res": {"rec_scores": [0.9], "rec_boxes": [[10, 20, 30, 40]]}}},
                    markdown={"markdown_texts": text})]
        result = ocr.ColumnPipeline(Structure(), None).predict(image)[0]
        payload = ocr.prediction_payload(result)
        self.assertTrue(250 < payload["gutter_x"] < 350)
        self.assertEqual(ocr.ordered_text(payload), "left column\n\nright column\n")
        right = payload["column_predictions"][1]
        self.assertEqual(right["prediction"]["overall_ocr_res"]["rec_scores"], [0.9])
        self.assertEqual(right["prediction"]["overall_ocr_res"]["rec_boxes"], [[10, 20, 30, 40]])
        self.assertEqual(payload["parsing_res_list"][1]["block_bbox"][0], right["x_offset"] + 10)
        with self.assertRaisesRegex(ValueError, "No clear two-column gutter"):
            ocr.column_gutter(np.zeros_like(image), np)

    def test_atomic_export_retries_transient_dropbox_lock(self):
        with tempfile.TemporaryDirectory() as tmp, patch.object(ocr.time, "sleep"):
            path = Path(tmp) / "checkpoint.json"
            replace = Path.replace
            calls = []
            def temporarily_locked(source, target):
                calls.append(target)
                if len(calls) == 1:
                    raise PermissionError("Dropbox has the destination open")
                return replace(source, target)
            with patch.object(Path, "replace", temporarily_locked):
                ocr.atomic_json(path, {"status": "ok"})
            self.assertEqual(json.loads(path.read_text()), {"status": "ok"})
            self.assertEqual(len(calls), 2)

    def test_gpu_memory_sample(self):
        with patch.object(ocr.subprocess, "check_output", return_value="1234\n") as query:
            self.assertEqual(ocr.gpu_memory("gpu:0"), 1234)
            self.assertEqual(ocr.gpu_memory("cpu"), None)
            self.assertEqual(query.call_count, 1)

    def test_select_missing_only_case_insensitive(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for name in ["16_C_400_600.pdf", "16_A_0_200.pdf", "16_A_0_200.DOCX",
                         "16_Index_300_400.pdf", "16_G_500_600.PDF"]:
                (root / name).touch()
            self.assertEqual([p.name for p in ocr.select_pdfs(root)],
                             ["16_C_400_600.pdf", "16_G_500_600.PDF"])
            with self.assertRaises(ValueError):
                ocr.select_pdfs(root, "16_Index_300_400.pdf")

    def test_ordered_text_does_not_interleave_columns(self):
        payload = {"parsing_res_list": [
            {"block_content": "left top"}, {"block_content": "left bottom"},
            {"block_content": "right top"}]}
        self.assertEqual(ocr.ordered_text(payload), "left top\n\nleft bottom\n\nright top\n")

    def test_failure_partial_checkpoint_resume_and_source_change(self):
        import numpy as np
        from PIL import Image

        class Bitmap:
            def to_pil(self):
                return Image.new("RGB", (10, 10), "white")
            def close(self):
                pass

        class Page:
            def render(self, scale):
                return Bitmap()
            def close(self):
                pass

        class Document:
            def __init__(self, path):
                pass
            def __len__(self):
                return 3
            def __getitem__(self, idx):
                return Page()
            def close(self):
                pass

        class Pipeline:
            calls = 0
            def predict(self, input):
                self.calls += 1
                if self.calls == 2:
                    raise RuntimeError("simulated page failure")
                return [SimpleNamespace(json={"res": {"parsing_res_list": [
                    {"block_content": "raw OCR", "block_bbox": [0, 0, 10, 10]}],
                    "overall_ocr_res": {"rec_scores": [0.9]}}},
                    markdown={"markdown_texts": "raw OCR"})]

        with tempfile.TemporaryDirectory() as tmp, patch.object(ocr, "gpu_memory", return_value=None):
            root = Path(tmp)
            pdf = root / "16_C_400_600.pdf"
            pdf.write_bytes(b"test source")
            args = SimpleNamespace(output_dir=root, dpi=300, device="cpu", resume=False,
                                   start_page=1, limit_pages=None, save_images=False)
            pipeline = Pipeline()
            config = {"dpi": 300}
            summary = ocr.run_pdf(pdf, pipeline, args, config, SimpleNamespace(PdfDocument=Document), np)
            self.assertEqual(summary["completed_pages"], 2)
            self.assertEqual(summary["failed_pages"], [2])
            folder = root / pdf.stem
            self.assertIn("PAGE NOT PROCESSED OR FAILED", (folder / "document.txt").read_text())
            # A txt without the final JSON marker must be reprocessed, not skipped.
            (folder / "page_0003.json").unlink()
            args.resume = True
            summary = ocr.run_pdf(pdf, pipeline, args, config, SimpleNamespace(PdfDocument=Document), np)
            self.assertEqual(summary["completed_pages"], 3)
            self.assertEqual(summary["failed_pages"], [])
            self.assertEqual(summary["this_run"], {"resumed": 1, "processed": 2})
            record = json.loads((folder / "page_0002.json").read_text())
            self.assertEqual(record["pdf_page_number"], 2)
            self.assertEqual(record["prediction"]["overall_ocr_res"]["rec_scores"], [0.9])
            self.assertNotIn("PAGE NOT PROCESSED OR FAILED", (folder / "document.txt").read_text())
            pdf.write_bytes(b"changed source")
            with self.assertRaisesRegex(ValueError, "Source/config changed"):
                ocr.run_pdf(pdf, pipeline, args, config, SimpleNamespace(PdfDocument=Document), np)


if __name__ == "__main__":
    unittest.main()

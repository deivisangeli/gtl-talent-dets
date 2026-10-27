"""Conservation and rejection checks for the saved-OCR paragraph repair."""
import copy
import importlib.util
import json
from pathlib import Path
import unittest


spec = importlib.util.spec_from_file_location("repair", Path(__file__).with_name("repair_ocr_paragraphs.py"))
repair = importlib.util.module_from_spec(spec)
spec.loader.exec_module(repair)


def fixture():
    column = dict(column_number=2, x_offset=100, prediction=dict(
        layout_det_res=dict(boxes=[
            dict(label="reference", coordinate=[0, 0, 90, 100]),
            dict(label="text", coordinate=[0, 0, 90, 40]),
            dict(label="reference_content", coordinate=[0, 50, 90, 100])]),
        overall_ocr_res=dict(rec_texts=["SAME", "first continuation", "SAME", "last  "],
                             rec_boxes=[[1, 5, 80, 15], [2, 20, 80, 30], [1, 60, 80, 70], [2, 80, 80, 90]],
                             rec_scores=[.99, .97, .99, .98])))
    parent = dict(block_label="reference", column_number=2, block_order=2,
                  block_bbox=[100, 0, 190, 100], block_content="collapsed")
    return parent, column


class RepairTests(unittest.TestCase):
    def test_duplicate_strings_retained_as_distinct_lines_with_provenance(self):
        parent, column = fixture()
        blocks, audit = repair.repair_reference(parent, column, "source.pdf", 67)
        self.assertEqual(audit["status"], "repaired")
        self.assertEqual([b["block_content"] for b in blocks], ["SAME\nfirst continuation", "SAME\nlast  "])
        lines = [line for b in blocks for line in b["paragraph_repair"]["lines"]]
        self.assertEqual([r["source_line_index"] for r in lines], [0, 1, 2, 3])
        self.assertEqual([r["text"] for r in lines], column["prediction"]["overall_ocr_res"]["rec_texts"])
        self.assertEqual(lines[0]["bbox"], [101, 5, 180, 15])
        self.assertEqual(lines[0]["pdf_page_number"], 67)
        self.assertEqual(lines[0]["confidence"], .99)

    def test_missing_paragraph_boxes_are_rejected(self):
        parent, column = fixture()
        column["prediction"]["layout_det_res"]["boxes"] = []
        blocks, audit = repair.repair_reference(parent, column, "source.pdf", 1)
        self.assertIsNone(blocks)
        self.assertEqual(audit["reason"], "fewer_than_two_paragraph_regions")

    def test_ambiguous_overlapping_regions_are_rejected(self):
        parent, column = fixture()
        column["prediction"]["layout_det_res"]["boxes"][1]["coordinate"][3] = 70
        blocks, audit = repair.repair_reference(parent, column, "source.pdf", 1)
        self.assertIsNone(blocks)
        self.assertEqual(audit["reason"], "paragraph_regions_not_vertically_separated")

    def test_uncovered_line_rejects_whole_reference(self):
        parent, column = fixture()
        column["prediction"]["overall_ocr_res"]["rec_boxes"][1] = [1, 42, 80, 48]
        blocks, audit = repair.repair_reference(parent, column, "source.pdf", 1)
        self.assertIsNone(blocks)
        self.assertEqual(audit["uncovered_line_indices"], [1])

    def test_bad_confidence_arrays_rejected(self):
        parent, column = fixture()
        column["prediction"]["overall_ocr_res"]["rec_scores"].pop()
        self.assertEqual(repair.repair_reference(parent, column, "source.pdf", 1)[1]["reason"],
                         "missing_or_mismatched_ocr_arrays")

    def test_export_preserves_unaffected_bytes_and_windows_newlines(self):
        before = b"HEADER\r\n\r\ncollapsed\r\n\r\nunchanged right column\r\n"
        after = repair.replace_export(before, [("collapsed", "A\n\nB")])
        self.assertEqual(after, b"HEADER\r\n\r\nA\r\n\r\nB\r\n\r\nunchanged right column\r\n")

    def test_end_of_file_strip_is_supported(self):
        self.assertEqual(repair.replace_export(b"header\n\nold\n", [("old  ", "last  ")]),
                         b"header\n\nlast  \n")

    def test_nonunique_export_content_leaves_reference_unrepaired(self):
        parent, column = fixture()
        record = dict(source_file="source.pdf", pdf_page_number=1,
                      prediction=dict(column_predictions=[column], parsing_res_list=[parent]))
        before_record = copy.deepcopy(record)
        original = b"collapsed\ncollapsed\n"
        blocks, audits, txt, md = repair.repair_page(record, original, original)
        self.assertEqual(record, before_record)
        self.assertEqual(blocks, [parent])
        self.assertEqual(txt, original)
        self.assertEqual(md, original)
        self.assertEqual(audits[0]["reason"], "export_content_mismatch")

    def test_control_with_no_reference_keeps_both_exports_identical(self):
        record = dict(source_file="source.pdf", pdf_page_number=1,
                      prediction=dict(column_predictions=[], parsing_res_list=[
                          dict(block_label="text", block_content="original", block_order=1)]))
        blocks, audits, txt, md = repair.repair_page(record, b"text\r\n", b"markdown \r\n")
        self.assertEqual(audits, [])
        self.assertEqual(txt, b"text\r\n")
        self.assertEqual(md, b"markdown \r\n")


class SavedPilotTests(unittest.TestCase):
    """Run the reported-page regressions when this machine's saved OCR exists."""
    root = Path.home() / "ocr_runs" / "amws_ed16"

    def source(self, name, page):
        base = self.root / name / f"page_{page:04d}"
        if not base.with_suffix(".json").exists():
            self.skipTest("Local AMWS saved-OCR fixture is not available")
        return (json.loads(base.with_suffix(".json").read_text(encoding="utf-8")),
                base.with_suffix(".txt").read_bytes(), base.with_suffix(".md").read_bytes())

    def test_all_three_reported_columns_have_complete_line_conservation(self):
        for page, column, paragraphs, line_count in [(1, 2, 8, 90), (67, 2, 9, 88), (81, 1, 9, 90)]:
            with self.subTest(page=page):
                record, txt, md = self.source("16_C_400_600", page)
                before = copy.deepcopy(record)
                blocks, audits, out_txt, out_md = repair.repair_page(record, txt, md)
                self.assertEqual(record, before)
                self.assertEqual(len(audits), 1)
                audit = audits[0]
                self.assertEqual(audit["status"], "repaired")
                self.assertEqual(audit["paragraph_count"], paragraphs)
                self.assertEqual(audit["source_line_count"], line_count)
                recovered = [b for b in blocks if "paragraph_repair" in b]
                indices = [i for b in recovered for i in b["paragraph_repair"]["source_line_indices"]]
                self.assertEqual(repair.Counter(indices), repair.Counter(audit["source_line_indices"]))
                original_lines = record["prediction"]["column_predictions"][column - 1]["prediction"]["overall_ocr_res"]["rec_texts"]
                for b in recovered:
                    expected = "\n".join(original_lines[i] for i in b["paragraph_repair"]["source_line_indices"])
                    self.assertEqual(b["block_content"], expected)
                inserted = "\r\n\r\n".join(b["block_content"].replace("\n", "\r\n") for b in recovered).encode("utf-8")
                self.assertEqual(out_txt.count(inserted), 1)
                self.assertEqual(out_md.count(inserted), 1)
                # Geometry and every unaffected block must survive the correction.
                unaffected = [b for b in blocks if "paragraph_repair" not in b]
                original = [b for b in record["prediction"]["parsing_res_list"] if b["block_label"] != "reference"]
                without_order = lambda b: {k: v for k, v in b.items() if k != "block_order"}
                self.assertEqual([without_order(b) for b in unaffected], [without_order(b) for b in original])

    def test_good_control_keeps_exports_and_blocks_identical(self):
        record, txt, md = self.source("16_G_500_600", 1)
        blocks, audits, out_txt, out_md = repair.repair_page(record, txt, md)
        self.assertEqual(blocks, record["prediction"]["parsing_res_list"])
        self.assertEqual(audits, [])
        self.assertEqual(out_txt, txt)
        self.assertEqual(out_md, md)


if __name__ == "__main__":
    unittest.main()

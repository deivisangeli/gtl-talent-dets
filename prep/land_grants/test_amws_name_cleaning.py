"""Meaningful guard tests for the frozen sample and explicit review ledger."""
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import amws_name_cleaning as cleaning


class NameCleaningTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.dest = Path(self.temp.name)
        self.patcher = patch.object(cleaning, "locations", return_value=(None, None, self.dest))
        self.patcher.start()
        self.addCleanup(self.patcher.stop)
        self.sample = [dict(sample_id="1", source_key="1986:1", name_raw="SMITH. ANN",
                            name_cleaned="SMITH, ANN", raw_text_adjusted="SMITH. ANN, b Boston, Mass; Educ: X."),
                       dict(sample_id="2", source_key="1986:2", name_raw="", name_cleaned="",
                            raw_text_adjusted="b Chicago, Ill; Educ: Y.")]
        cleaning.write_csv(self.dest / "sample_500.csv", self.sample, self.sample[0].keys())
        self.ledger = self.dest / "review_decisions.jsonl"
        self.ledger.touch()

    def test_sampling_is_order_independent_and_includes_missing_names(self):
        pool = [dict(source_key=f"1986:{i}", name_raw="" if i == 0 else "SMITH") for i in range(501)]
        drawn = cleaning.draw_sample(pool)
        self.assertEqual(drawn, cleaning.draw_sample(list(reversed(pool))))
        self.assertEqual(len({r["source_key"] for r in drawn}), 500)
        with patch.object(cleaning, "SAMPLE_SIZE", 501):
            self.assertEqual(sum(r["name_raw"] == "" for r in cleaning.draw_sample(pool)), 1)

    def test_no_reviews_are_inferred_for_unsubmitted_rows(self):
        cleaning.append_reviews([dict(sample_id=1, decision="no_change")])
        reviews = [json.loads(s) for s in self.ledger.read_text().splitlines()]
        self.assertEqual([r["sample_id"] for r in reviews], ["1"])

    def test_duplicate_submission_is_rejected_atomically(self):
        with self.assertRaises(ValueError):
            cleaning.append_reviews([dict(sample_id=1, decision="no_change"),
                                     dict(sample_id=1, decision="no_change")])
        self.assertEqual(self.ledger.read_text(), "")

    def test_proposal_requires_literal_evidence(self):
        with self.assertRaises(ValueError):
            cleaning.append_reviews([dict(sample_id=1, decision="proposed_fix", proposed_full_name="SMITH, ANNE",
                                          evidence="SMITH, ANNE", explanation="Unsupported spelling", error_pattern="test")])
        self.assertEqual(self.ledger.read_text(), "")

    def test_review_cannot_change_identity_fields(self):
        with self.assertRaises(ValueError):
            cleaning.append_reviews([dict(sample_id=1, source_key="1986:99", decision="no_change")])

    def test_unresolved_is_distinct_from_correction(self):
        cleaning.append_reviews([dict(sample_id=2, decision="unresolved", evidence="b Chicago, Ill",
                                     explanation="No name survives", error_pattern="missing_name")])
        self.assertEqual(json.loads(self.ledger.read_text())["decision"], "unresolved")
        with self.assertRaises(ValueError):
            cleaning.amend_review(2, dict(proposed_full_name="GUESS, NAME"), "Unjustified name")

    def test_amendment_preserves_original_review(self):
        cleaning.append_reviews([dict(sample_id=1, decision="no_change")])
        cleaning.amend_review(1, dict(decision="unresolved", explanation="Initial was unclear", error_pattern="ocr"), "Second look")
        history = json.loads((self.dest / "review_amendments.jsonl").read_text())
        self.assertEqual(history["previous"]["decision"], "no_change")
        self.assertEqual(history["replacement"]["decision"], "unresolved")

    def test_invalid_corroborating_evidence_is_rejected(self):
        with self.assertRaises(ValueError):
            cleaning.append_reviews([dict(sample_id=1, decision="no_change", supporting_source_key="1986:2",
                                          supporting_evidence="NOT IN SECOND BIOGRAPHY")])

    def test_completed_release_cannot_be_edited(self):
        (self.dest / "completion_manifest.json").write_text("{}")
        with self.assertRaises(ValueError):
            cleaning.append_reviews([dict(sample_id=1, decision="no_change")])
        with self.assertRaises(ValueError):
            cleaning.amend_review(1, {}, "Change")


if __name__ == "__main__":
    unittest.main()

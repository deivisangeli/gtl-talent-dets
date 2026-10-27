"""Edge cases for the regex engine and sealed calibration workflow."""
import tempfile
from pathlib import Path
import unittest
from unittest.mock import patch

import amws_name_regex_calibration as calibration
import amws_name_regex_rules as engine


class RegexTests(unittest.TestCase):
    def setUp(self):
        self.config = engine.candidate_configuration(["SMITH", "VAN DYKE", "VAN COUVERING", "FELKNER", "WALLACE"])

    def clean(self, name):
        return engine.apply(name, self.config)["name_regex_cleaned"]

    def test_compound_surnames_and_longest_boundary(self):
        self.assertEqual(self.clean("VAN DYKE KNOX"), "VAN DYKE, KNOX")
        self.assertEqual(self.clean("VAN COUVERING, JUDITH ANNE HARRIS"), "VAN COUVERING, JUDITH ANNE HARRIS")
        self.assertEqual(self.clean("DE LONG JOHN"), "DE LONG JOHN")

    def test_apostrophes_suffixes_and_expansions(self):
        self.assertEqual(self.clean("O'MALLEY,MATTHEW J(OSEPH),JR"), "O'MALLEY, MATTHEW J(OSEPH), JR")
        self.assertEqual(self.clean("REAVEY-CANTWELL, NELSON HENRY"), "REAVEY-CANTWELL, NELSON HENRY")

    def test_reviewed_joins_have_component_boundaries(self):
        self.assertEqual(self.clean("SMITH, LOW ELL LAWRENCE"), "SMITH, LOWELL LAWRENCE")
        self.assertEqual(self.clean("W ARNER, FRANCIS JAMES"), "WARNER, FRANCIS JAMES")
        self.assertEqual(self.clean("LAW RENCE, JOHN"), "LAW RENCE, JOHN")

    def test_no_guessing_damaged_glyphs_or_incomplete_names(self):
        for name in ["TAMBUR1NO, LOUIS", "KOPF, PETER VV", "JOHN, III", "SMITH, W ARREN ( RYDER"]:
            self.assertEqual(self.clean(name), name)
        self.assertIn("incomplete_suffix_only_name", engine.flags("JOHN, III"))

    def test_unknown_unseparated_name_stays_flagged(self):
        self.assertEqual(self.clean("WTTIIAM SANDS"), "WTTIIAM SANDS")
        self.assertIn("missing_surname_separator", engine.flags("WTTIIAM SANDS"))

    def test_missing_name_preserved_and_flagged(self):
        self.assertEqual(self.clean(""), "")
        self.assertEqual(engine.flags(""), ["missing_name"])

    def test_internal_surname_separator_is_not_reinterpreted(self):
        self.assertEqual(self.clean("STONF,MAN, DAVID MCNEEL"), "STONF, MAN, DAVID MCNEEL")
        self.assertIn("ambiguous_internal_separator", engine.flags("STONF,MAN, DAVID MCNEEL"))

    def test_prefix_requires_intact_audited_name(self):
        self.assertEqual(self.clean("UNIV, 77-FELKNER, IRA CECIL"), "FELKNER, IRA CECIL")
        self.assertEqual(self.clean("corrupt ORMAN, DONALD T"), "corrupt ORMAN, DONALD T")

    def test_idempotence_and_rule_trace(self):
        result = engine.apply("W ARNER,FRANCIS JAMES", self.config)
        self.assertEqual(self.clean(result["name_regex_cleaned"]), result["name_regex_cleaned"])
        self.assertEqual(result["rule_ids"], ["S01_comma_spacing", "S02_reviewed_word_joins"])
        self.assertEqual(result["trace"][0]["before"], "W ARNER,FRANCIS JAMES")

    def test_unresolved_changes_are_not_counted_as_repairs(self):
        label = dict(decision="unresolved", expected_name="")
        self.assertEqual(calibration.outcome("A,B", "A, B", label), "unresolved_format_only")
        self.assertEqual(calibration.outcome("A B", "A, B", label), "unconfirmed_change")


class ReviewSealTests(unittest.TestCase):
    def test_rules_participating_in_failed_test_changes_are_withheld(self):
        predictions = [dict(source_key="1986:1", outcome="incorrect_change", rule_ids='["S01", "S03"]'),
                       dict(source_key="1986:2", outcome="unresolved_format_only", rule_ids='["S02"]'),
                       dict(source_key="1986:3", outcome="unconfirmed_change", rule_ids='["S04"]')]
        self.assertEqual(set(calibration.withheld_rules(predictions)), {"S01", "S03", "S04"})

    def test_cached_test_evaluation_cannot_be_overwritten(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp)
            (path / "test_evaluation.json").write_text('{"evaluation_count": 1}')
            with patch.object(calibration, "destination", return_value=path):
                with self.assertRaisesRegex(ValueError, "already exists"):
                    calibration.evaluate_test()
            self.assertEqual((path / "test_evaluation.json").read_text(), '{"evaluation_count": 1}')

    def test_test_reviews_rejected_before_freeze(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp)
            row = dict(sample_id="1", source_key="1986:1", partition="test", name_cleaned="SMITH, ANN", raw_text_adjusted="SMITH, ANN, b A")
            calibration.base.write_csv(path/"sample_500.csv", [row], row.keys())
            (path/"manual_labels.jsonl").touch()
            with patch.object(calibration, "destination", return_value=path):
                with self.assertRaises(ValueError):
                    calibration.append_labels([dict(sample_id=1, decision="no_change")])
            self.assertEqual((path/"manual_labels.jsonl").read_text(), "")

    def test_sample_disjoint_and_duplicate_biographies_not_in_test(self):
        rows = [dict(source_key=f"1986:{i}", raw_text_adjusted=f"NAME{i}, b City{i}; Educ: school{i}") for i in range(520)]
        rows[1]["raw_text_adjusted"] = "DIFFERENT HEADING, b City0; Educ: school0"
        old = [rows[0]]
        sample = calibration.make_sample(rows, old)
        self.assertEqual(len(sample), 500)
        self.assertNotIn("1986:0", {r["source_key"] for r in sample})
        test = [r for r in sample if r["partition"] == "test"]
        self.assertEqual(len(test), 200)
        self.assertNotIn(calibration.body_key(old[0]), {r["biography_group"] for r in test})
        self.assertEqual(sample, calibration.make_sample(list(reversed(rows)), old))


if __name__ == "__main__":
    unittest.main()

import unittest

import build_postsecondary_release_v04 as build
import education_regex_v04 as parser
import postsecondary_qualifications_v04 as scope


class ScopeV04Tests(unittest.TestCase):
    def labels(self, text, birth=1930, edition=1986):
        return [(d["label_as_written"], d["family"], d["training_eligibility"]) for d in scope.parse(text, birth, edition)[1]]

    def test_board_certification_excluded(self):
        self.assertEqual(self.labels("Educ: Univ Alpha, MD, 47; Am Bd Internal Med, dipl, 57. Prof Exp: Lab."),
                         [("MD", "doctorate_other", "eligible"), ("dipl", None, "board_certification")])
        self.assertEqual(self.labels("Educ: Univ Alpha, BS, 47; Am Bd Indust Hyg, cert, 80. Prof Exp: Lab."),
                         [("BS", "ba_bs", "eligible"), ("cert", None, "board_certification")])
        # A school diploma keeps the postsecondary_v1 rules.
        self.assertEqual(self.labels("Educ: Univ Alpha, BS, 47; Alpha Polytech Inst, Dipl, 50. Prof Exp: Lab.")[-1],
                         ("Dipl", "postsecondary_credential", "eligible"))

    def test_extra_labels_under_v04(self):
        for label, family in (("DEng", "doctorate_other"), ("Kand", "postsecondary_credential"), ("Certificate", "postsecondary_credential")):
            ds = [d for d in scope.parse(f"Educ: Univ Alpha, {label}, 1960. Prof Exp: Engineer, 1961.", 1930)[1]
                  if parser.canon(d["label_as_written"]) == parser.canon(label)]
            self.assertEqual([(d["family"], d["training_eligibility"]) for d in ds], [(family, "eligible")], label)

    def test_extra_labels_use_v04_bounds(self):
        # A damaged heading is recovered for the extension labels as well.
        self.assertEqual(self.labels("SMITH, JO, b Town, Iowa, June 5, 39; m 62. ENGINEERING. E due: Alpha Univ, DEng, 61. Prof Exp: Lab, 70.", 1939),
                         [("DEng", "doctorate_other", "eligible")])

    def test_no_duplicate_spans(self):
        ds = scope.parse("Educ: Univ Alpha, BS, 50; Univ Beta, Dipl, 52; Univ Gamma, Diploma, 53. Prof Exp: Lab.", 1930)[1]
        spans = [(d["label_start"], d["label_end"]) for d in ds]
        self.assertEqual(len(spans), len(set(spans)))

    def test_reuse_key(self):
        published = dict(institution_as_written="Univ Alpha", year="1950", requested_family="ba_bs", training_eligibility="eligible")
        self.assertEqual(build.reuse_key(published), build.reuse_key(dict(published, year=1950)))
        for change in (dict(institution_as_written="Univ Alpha, Md"), dict(year=""), dict(requested_family="bachelors_other"),
                       dict(training_eligibility="unresolved_level")):
            self.assertNotEqual(build.reuse_key(published), build.reuse_key(dict(published, **change)), change)

    def test_wide_column_order(self):
        cols = ["phd_award_1_year", "ba_bs_award_10_year", "ba_bs_award_2_year", "postsecondary_credential_award_1_year"]
        self.assertEqual(sorted(cols, key=build.column_order),
                         ["ba_bs_award_2_year", "ba_bs_award_10_year", "phd_award_1_year", "postsecondary_credential_award_1_year"])


if __name__ == "__main__":
    unittest.main()

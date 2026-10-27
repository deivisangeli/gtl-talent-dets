import unittest

import build_postsecondary_release_v06 as v06

BASE = v06.normalise("SMITH, JOHN ALBERT, b Erie, Pa, Sept 3, 23; m 66. PHYSICAL CHEMISTRY. Educ: Gannon Col, BS, 45; "
                     "Pa State Univ, MS, 46; Univ Chicago, PhD(phys chem), 54. Prof Exp: Asst prof chem, Gannon Col, 46-49.")


class Links(unittest.TestCase):
    def test_kinds(self):
        self.assertEqual(v06.link_kind(BASE, BASE), "exact")
        self.assertEqual(v06.link_kind(BASE, BASE[:120]), "truncated_copy")
        noisy = BASE[:100] + "x" + BASE[101:]
        self.assertEqual(v06.link_kind(BASE, noisy), "ocr_variant")
        other = BASE[:70] + v06.normalise("Completely different career at another institution entirely, 1950 to 1980.") * 3
        self.assertIsNone(v06.link_kind(BASE, other))
        self.assertEqual(v06.link_kind(BASE, other, reviewed=True), "reviewed_borderline")

    def test_groups_chain_and_lookalikes(self):
        lookalike = v06.normalise("SMITH, JOHN BERNARD, b Boston, Mass, Jan 1, 23; m 50. GEOLOGY. Educ: Harvard Univ, AB, 44.")
        texts = {"1986:10": BASE, "1986:11": BASE[:120], "1986:40": BASE[:100] + "q" + BASE[101:],
                 "1986:90": lookalike, "1938:10": BASE}
        groups, edges = v06.find_groups(texts)
        self.assertEqual(groups, [["1986:10", "1986:11", "1986:40"]])   # chain joined; other edition and look-alike apart
        self.assertIn(edges["1986:11"][1], {"truncated_copy"})

    def test_reviewed_pair_links_and_distinct_does_not(self):
        other = BASE[:70] + v06.normalise("Completely different career at another institution entirely, 1950 to 1980.") * 3
        texts = {"1986:1": BASE, "1986:2": other}
        self.assertEqual(v06.find_groups(texts)[0], [])
        self.assertEqual(v06.find_groups(texts, frozenset({frozenset(("1986:1", "1986:2"))}))[0], [["1986:1", "1986:2"]])

    def test_short_texts_never_linked(self):
        self.assertEqual(v06.find_groups({"1955:1": "abc", "1955:2": "abc"})[0], [])


class Keeper(unittest.TestCase):
    def test_kept_flag_first_in_earlier_editions(self):
        k, why = v06.choose_keeper(["1955:10", "1955:2411"], {"1955:10": "FALSE", "1955:2411": "TRUE"}, {"1955:10": 9, "1955:2411": 9})
        self.assertEqual((k, why), ("1955:2411", "only_kept_true"))

    def test_kept_flag_beats_longer_text(self):
        k, _ = v06.choose_keeper(["1938:5", "1938:9"], {"1938:5": "TRUE", "1938:9": "FALSE"}, {"1938:5": 100, "1938:9": 400})
        self.assertEqual(k, "1938:5")

    def test_both_true_or_false_falls_to_length_then_line(self):
        self.assertEqual(v06.choose_keeper(["1955:1", "1955:2"], {"1955:1": "TRUE", "1955:2": "TRUE"}, {"1955:1": 5, "1955:2": 9}),
                         ("1955:2", "longest_text"))
        self.assertEqual(v06.choose_keeper(["1955:1", "1955:2"], {"1955:1": "FALSE", "1955:2": "FALSE"}, {"1955:1": 9, "1955:2": 9}),
                         ("1955:1", "lowest_line_id"))

    def test_1986_ignores_kept_and_keeps_complete_copy(self):
        k, why = v06.choose_keeper(["1986:3", "1986:4"], {"1986:3": "", "1986:4": "TRUE"}, {"1986:3": 1107, "1986:4": 1016})
        self.assertEqual((k, why), ("1986:3", "longest_text"))


if __name__ == "__main__":
    unittest.main()

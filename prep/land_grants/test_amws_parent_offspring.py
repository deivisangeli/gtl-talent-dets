"""Meaningful parsing, pairing, completeness and immutable-release checks."""
import collections
import random
import tempfile
from pathlib import Path
import unittest

import amws_parent_offspring as matching


def source(key="1986:1", name="SMITH, ANN", year="1920", last="", first="", edition=""):
    row = {field: "" for field in matching.RAW_FIELDS}
    row.update(source_key=key, edition=edition, name_raw=name, birth_year=year, last=last, first=first)
    return row


def derived(**kwargs):
    return matching.derive_record(source(**kwargs), 1, {}, {})


class NameTests(unittest.TestCase):
    def test_fold_case_accents_apostrophes_hyphens_and_all_components(self):
        for name, expected in [("VAN DYKE", "VANDYKE"), ("García-López", "GARCIALOPEZ"),
                               ("O’Neil", "ONEIL"), ("D'Angelo", "DANGELO"), ("St. John", "STJOHN"),
                               ("Garci\u0301a", "GARCIA"), ("Straße", "STRASSE"), ("Łódź", "ŁODZ")]:
            self.assertEqual(matching.surname_key(name), (expected, []))
            self.assertEqual(matching.surname_key(expected), (expected, []))

    def test_damaged_characters_are_not_deleted_into_names(self):
        for name in ["SM1TH", "D1«ERSpN", "Dâ\x80\x99AUNOY", "S(MITH)", "SMITH*", "SMITH/SMYTH", "\u0301SMITH"]:
            key, reasons = matching.surname_key(name)
            self.assertEqual(key, "")
            self.assertIn("unexpected_surname_character", reasons)

    def test_heading_contamination_and_single_initial(self):
        self.assertIn("possible_heading_contamination", matching.surname_key("SMITH DR JOHN")[1])
        self.assertIn("surname_initial_only", matching.surname_key("S")[1])
        self.assertEqual(matching.surname_key("DEAN"), ("DEAN", []))

    def test_1986_period_replacement_and_internal_punctuation_preserved(self):
        row = derived(name="O'MALLEY. JOHN P(AUL). JR")
        self.assertEqual(row["name_raw"], "O'MALLEY. JOHN P(AUL). JR")
        self.assertEqual(row["name_period_cleaned"], "O'MALLEY, JOHN P(AUL), JR")
        self.assertEqual(row["surname_key"], "OMALLEY")
        self.assertEqual(row["suffix_extracted"], "JR")
        self.assertEqual(row["eligible"], "true")

    def test_missing_and_ambiguous_headings_go_to_review(self):
        for name, reason in [("", "missing_name"), ("SMITH JOHN", "missing_surname_separator"),
                             ("JOHN, III", "suffix_only_heading"), ("STONF,MAN, DAVID", "ambiguous_surname_separator"),
                             ("SMITH, J(OHN", "unbalanced_heading_parentheses"), ("UNIV, 77-FELKNER, IRA", "ambiguous_surname_separator")]:
            row = derived(name=name)
            self.assertEqual(row["eligible"], "false")
            self.assertIn(reason, row["exclusion_reasons"])

    def test_prior_editions_use_last_and_source_key_edition(self):
        row = derived(key="1906:1", name="", year="1880", last="St. John", first="Prof. James", edition="")
        self.assertEqual(row["edition"], "1906")
        self.assertEqual(row["surname_key"], "STJOHN")
        self.assertEqual(row["name_derived"], "St. John, Prof. James")
        self.assertEqual(row["eligible"], "true")

    def test_explicit_override_is_bounded_to_record(self):
        row = source(name="yANGELL, ROBERT WALKER")
        corrected = matching.derive_record(row, 1, {"1986:1": "ANGELL, ROBERT WALKER"}, {})
        self.assertEqual(corrected["surname_key"], "ANGELL")
        self.assertEqual(corrected["name_raw"], row["name_raw"])
        self.assertEqual(corrected["manual_override_applied"], "true")
        untouched = matching.derive_record(source(key="1986:2", name=row["name_raw"]), 2, {"1986:1": "ANGELL, ROBERT WALKER"}, {})
        self.assertEqual(untouched["surname_key"], "YANGELL")
        self.assertEqual(untouched["manual_override_applied"], "false")

    def test_previously_unresolved_is_excluded_even_if_it_looks_valid(self):
        row = matching.derive_record(source(), 1, {}, {"1986:1": "unresolved"})
        self.assertEqual(row["eligible"], "false")
        self.assertIn("previously_reviewed_unresolved", row["exclusion_reasons"])

    def test_calibrated_word_join_rules_not_used(self):
        row = derived(name="W ARREN, LOW ELL LAWRENCE")
        self.assertEqual(row["name_derived"], "W ARREN, LOW ELL LAWRENCE")
        # Surname spacing folds as a general bucket policy; display spelling is preserved.
        self.assertEqual(row["surname_key"], "WARREN")

    def test_years_not_imputed_or_used_when_invalid(self):
        for text in ["", "19?0", "1920.0", "2000", "0", "-1", "١٩٢٠"]:
            self.assertEqual(derived(year=text)["eligible"], "false")
        self.assertEqual(matching.parse_year(" 1920 ", "1986"), ("1920", []))


class PairTests(unittest.TestCase):
    def test_19_vs_20_years_and_both_edition_combinations(self):
        rows = [derived(key="1906:1", last="Smith", first="John", year="1880"),
                derived(key="1986:2", year="1899"), derived(key="1986:3", year="1900"),
                derived(key="1986:4", year="1920")]
        pairs = list(matching.candidate_records(matching.buckets_for(rows)))
        keys = {(p["source_key"], c["source_key"]) for _,p,c in pairs}
        self.assertNotIn(("1906:1", "1986:2"), keys)
        self.assertIn(("1906:1", "1986:3"), keys)
        self.assertIn(("1986:3", "1986:4"), keys)
        self.assertEqual(len(keys), len(pairs))
        self.assertTrue(all(a != b for a,b in keys))

    def test_large_gap_retained_and_flagged(self):
        rows = [derived(key="1986:1", year="1800"), derived(key="1986:2", year="1900")]
        key,parent,child = next(matching.candidate_records(matching.buckets_for(rows)))
        self.assertEqual(matching.pair_row(1,key,parent,child)["gap_above_80"], "true")

    def test_complete_against_exhaustive_cartesian_fixture(self):
        rng = random.Random(10)
        rows = [derived(key=f"1986:{i}", name=f"{rng.choice(['SMITH', 'JONES', 'NG'])}, PERSON", year=str(rng.randrange(1800,1980)))
                for i in range(100)]
        rows += [derived(key="1986:200", name="CORRUPT1, ANN", year="1900")]
        actual = {(p["source_key"], c["source_key"]) for _,p,c in matching.candidate_records(matching.buckets_for(rows))}
        expected = {(p["source_key"], c["source_key"]) for p in rows for c in rows
                    if p["eligible"] == c["eligible"] == "true" and p["surname_key"] == c["surname_key"]
                    and int(c["birth_year"])-int(p["birth_year"]) >= 20}
        self.assertEqual(actual, expected)
        self.assertEqual(matching.pair_count(matching.buckets_for(rows)), len(expected))
        self.assertEqual(list(matching.candidate_records(matching.buckets_for(rows))),
                         list(matching.candidate_records(matching.buckets_for(list(reversed(rows))))))

    def test_duplicate_appearances_not_collapsed(self):
        a = derived(key="1906:1", last="Smith", first="John", year="1880")
        b = derived(key="1938:2", last="Smith", first="John", year="1880")
        c = derived(key="1986:3", year="1920")
        self.assertEqual(a["duplicate_signature"], b["duplicate_signature"])
        self.assertEqual(len(list(matching.candidate_records(matching.buckets_for([a,b,c])))),2)


class ReleaseTests(unittest.TestCase):
    def test_existing_directory_cannot_be_overwritten(self):
        with tempfile.TemporaryDirectory() as root:
            dest=matching.paths(root)[-1]
            dest.mkdir(parents=True)
            marker=dest/"keep.txt"
            marker.write_text("preserve")
            with self.assertRaisesRegex(ValueError,"already exists"):
                matching.build(root)
            self.assertEqual(marker.read_text(),"preserve")

    def test_inspection_sample_100_or_all_and_empty_pairs(self):
        rows=[derived(key=f"1986:{i}",year=str(1800+i)) for i in range(30)]
        with tempfile.TemporaryDirectory() as root:
            dest=Path(root)
            total,*_=matching.build_pair_files(dest,matching.buckets_for(rows))
            sample=matching.base.read_csv(dest/"inspection_sample_100.csv")
            self.assertEqual(len(sample), min(100,total))
            self.assertEqual(len({r['pair_index'] for r in sample}),len(sample))
            before=matching.base.digest(dest/"inspection_sample_100.csv")
            matching.build_pair_files(dest,matching.buckets_for(list(reversed(rows))))
            self.assertEqual(before,matching.base.digest(dest/"inspection_sample_100.csv"))
            matching.build_pair_files(dest,{})
            self.assertEqual(matching.base.read_csv(dest/"candidate_pairs.csv"),[])


if __name__ == "__main__":
    unittest.main()

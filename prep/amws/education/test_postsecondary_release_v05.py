import csv
import hashlib
import json
from pathlib import Path
import tempfile
import unittest

import build_postsecondary_release_v05 as v05

REFERENCE = {
    "I1": dict(oa_canonical_name="Flagship University", latitude="40.1", longitude="-88.2", city="Urbana", country="US"),
    "I2": dict(oa_canonical_name="Medical Campus", latitude="41.9", longitude="-87.6", city="Chicago", country="US"),
    "I3": dict(oa_canonical_name="Existing College", latitude="43.0", longitude="-76.1", city="Syracuse", country="US"),
}
SOURCES = dict(crosswalk="crosswalk.csv", override="overrides.csv")


def award(**kw):
    a = dict(source_key="1955:1", award_index="1", requested_family="phd", training_eligibility="eligible",
             fully_matched="False", oa_ids="[]", institution_as_written="Illinois")
    a.update(kw)
    return a


CROSSWALK = {
    "Illinois": dict(rank="1", openalex_id="I1", openalex_display_name="Flagship University", confidence="likely"),
    "Yankton Col": dict(rank="122", openalex_id="", openalex_display_name="", confidence="not_in_openalex"),
}
OVERRIDES = {("1955:1", "2"): dict(override_id="I2", confidence="clear", rule_id="uiuc_md")}


def write_csv(path, fieldnames, records):
    with open(path, "w", encoding="utf-8-sig", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        w.writerows(records)


class ManualAssignment(unittest.TestCase):
    def test_crosswalk_assigns_unmatched_award(self):
        out = v05.manual_assignment(award(), CROSSWALK, OVERRIDES, REFERENCE, SOURCES)
        self.assertEqual(json.loads(out["oa_ids"]), ["I1"])
        self.assertEqual(json.loads(out["oa_names"]), ["Flagship University"])
        self.assertEqual(json.loads(out["oa_latitudes"]), [40.1])
        self.assertEqual(out["fully_matched"], "True")
        self.assertEqual(out["manual_assignment"], "crosswalk")
        self.assertEqual(out["record_id"], "top500:1")
        self.assertEqual(json.loads(out["assignment_class"]), [v05.CROSSWALK_CLASS])

    def test_override_beats_crosswalk(self):
        out = v05.manual_assignment(award(award_index="2"), CROSSWALK, OVERRIDES, REFERENCE, SOURCES)
        self.assertEqual(json.loads(out["oa_ids"]), ["I2"])
        self.assertEqual(out["manual_assignment"], "override")
        self.assertEqual(out["record_id"], "uiuc_md")
        self.assertEqual(out["matching_source"], "overrides.csv")

    def test_blank_crosswalk_id_stays_unmatched(self):
        self.assertIsNone(v05.manual_assignment(award(institution_as_written="Yankton Col"), CROSSWALK, OVERRIDES, REFERENCE, SOURCES))

    def test_string_outside_crosswalk_stays_unmatched(self):
        self.assertIsNone(v05.manual_assignment(award(institution_as_written="Leander Clark"), CROSSWALK, OVERRIDES, REFERENCE, SOURCES))

    def test_fully_matched_award_is_untouched(self):
        a = award(fully_matched="True", oa_ids='["I3"]')
        self.assertIsNone(v05.manual_assignment(a, CROSSWALK, OVERRIDES, REFERENCE, SOURCES))

    def test_non_eligible_and_excluded_awards_are_untouched(self):
        for a in (award(training_eligibility="unresolved_level"), award(requested_family="")):
            self.assertIsNone(v05.manual_assignment(a, CROSSWALK, OVERRIDES, REFERENCE, SOURCES))

    def test_partial_award_takes_the_union(self):
        out = v05.manual_assignment(award(oa_ids='["I3"]'), CROSSWALK, OVERRIDES, REFERENCE, SOURCES)
        self.assertEqual(json.loads(out["oa_ids"]), ["I1", "I3"])
        self.assertEqual(json.loads(out["IDs_before"]), ["I3"])
        self.assertEqual(json.loads(out["additional_IDs"]), ["I1"])
        self.assertIn("manual_union_with_existing_ids", json.loads(out["matching_flags"]))

    def test_names_and_coordinates_stay_aligned_with_ids(self):
        out = v05.manual_assignment(award(oa_ids='["I3"]'), CROSSWALK, OVERRIDES, REFERENCE, SOURCES)
        ids = json.loads(out["oa_ids"])
        self.assertEqual(json.loads(out["oa_cities"]), [REFERENCE[i]["city"] for i in ids])
        self.assertEqual(json.loads(out["oa_longitudes"]), [float(REFERENCE[i]["longitude"]) for i in ids])


class Categories(unittest.TestCase):
    def test_unmatched_categories(self):
        cases = [
            (award(training_eligibility="unresolved_level"), "unresolved_level"),
            (award(institution_as_written=" "), "no_institution_text"),
            (award(oa_ids='["I3"]'), "partially_matched"),
            (award(institution_as_written="Yankton Col"), "not_in_openalex"),
            (award(institution_as_written="Leander Clark"), "outside_top500"),
        ]
        for a, expected in cases:
            self.assertEqual(v05.unmatched_category(a, CROSSWALK), expected)

    def test_normalisation_folds_ocr_variants(self):
        self.assertEqual(v05.normalized_name("Uniy Pittsburgh"), v05.normalized_name("Univ Pittsburgh"))
        self.assertEqual(v05.normalized_name("L'niv Calif"), v05.normalized_name("Univ Calif"))
        self.assertEqual(v05.normalized_name("IUlnois"), "illinois")
        self.assertEqual(v05.normalized_name("Univ 111"), v05.normalized_name("Univ Ill"))
        self.assertNotEqual(v05.normalized_name("Univ Tex, Dallas"), v05.normalized_name("Univ Tex"))


class PersonSummary(unittest.TestCase):
    def test_summary_counts(self):
        awards = [dict(oa_ids='["I1"]'), dict(oa_ids='["I1","I3"]'), dict(oa_ids="[]")]
        s = v05.person_summary(awards)
        self.assertEqual(json.loads(s["oa_ids"]), ["I1", "I3"])
        self.assertEqual((s["oa_id_count"], s["oa_award_count"], s["oa_matched_award_count"], s["oa_multi_institution_award_count"]), (2, 3, 2, 1))
        self.assertEqual(s["oa_has_id"], "True")
        self.assertEqual(v05.person_summary([])["oa_has_id"], "False")


class Loaders(unittest.TestCase):
    def setUp(self):
        self.dir = Path(tempfile.mkdtemp())

    def test_duplicate_crosswalk_string_rejected(self):
        p = self.dir / "c.csv"
        write_csv(p, ["institution_as_written", "openalex_id"], [dict(institution_as_written="A", openalex_id="I1")] * 2)
        with self.assertRaises(AssertionError):
            v05.load_crosswalk(p)

    def test_override_must_agree_with_crosswalk(self):
        p = self.dir / "o.csv"
        write_csv(p, ["source_key", "award_index", "institution_as_written", "crosswalk_id", "override_id"],
                  [dict(source_key="1955:1", award_index="2", institution_as_written="Illinois", crosswalk_id="I9", override_id="I2")])
        with self.assertRaises(AssertionError):
            v05.load_overrides(p, CROSSWALK)

    def test_missing_reference_coordinates_rejected(self):
        p = self.dir / "r.csv"
        write_csv(p, ["id", "name", "latitude", "longitude", "valid_coordinates"],
                  [dict(id="I1", name="A", latitude="40", longitude="-88", valid_coordinates="True"),
                   dict(id="I2", name="B", latitude="", longitude="", valid_coordinates="False")])
        self.assertIn("I1", v05.load_reference(p, {"I1"}))
        with self.assertRaises(ValueError):
            v05.load_reference(p, {"I1", "I2"})
        with self.assertRaises(ValueError):
            v05.load_reference(p, {"I7"})

    def test_physical_science_requires_matching_manifest(self):
        audit = self.dir / "amws_ed16_physical_science_audit.csv"
        write_csv(audit, ["source_key", "physical_science", "unresolved", "mixed"],
                  [dict(source_key="1986:5", physical_science="1", unresolved="0", mixed="1")])
        sha = hashlib.sha256(audit.read_bytes()).hexdigest()
        (self.dir / "manifest.json").write_text(json.dumps({"rows": 1, "output_checksums_sha256": {audit.name: sha}}), encoding="utf-8")
        got = v05.load_physical_science(self.dir)
        self.assertEqual(got, {"1986:5": dict(physical_science="1", physical_science_unresolved="0", physical_science_mixed="1")})
        (self.dir / "manifest.json").write_text(json.dumps({"rows": 1, "output_checksums_sha256": {audit.name: "0" * 64}}), encoding="utf-8")
        with self.assertRaises(AssertionError):
            v05.load_physical_science(self.dir)


if __name__ == "__main__":
    unittest.main()

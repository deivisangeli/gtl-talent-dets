import json
import unittest

import education_regex as legacy
import postsecondary_qualifications as post
import build_postsecondary_release as build


class ScopeTests(unittest.TestCase):
    def test_conditional_mapping_multiple_ids_and_missing_coordinates(self):
        rules, _ = build.load_condition_rules()
        mapping = dict(existing=[], additional=[], matches=[dict(openalex_id="I1", conditions={"all": [dict(field="year", op="lt", value=1956)]})],
            explicit=False, component_count=1, status="conditional", record_id="test", origin="test")
        ref = {f"I{k}": dict(oa_canonical_name=f"University {k}", latitude="1", longitude="2", city="City", country="US") for k in (1,2,3)}
        award = dict(training_eligibility="eligible", requested_family="doctorate_other", label_as_written="MD", year="1950", flags="[]")
        self.assertEqual(json.loads(build.assign(award, mapping, rules, ref, "")["oa_ids"]), ["I1"])
        for year, flags in (("", "[]"), ("1960", "[]"), ("1950", '["chronology_conflict"]')):
            a = dict(award, year=year, flags=flags)
            self.assertEqual(build.assign(a, mapping, rules, ref, "")["fully_matched"], "False")
        mapping.update(existing=["I1", "I2", "I3"], matches=[], explicit=True, component_count=3, status="fully_provisional_matched")
        result = build.assign(award, mapping, rules, ref, "")
        self.assertEqual(len(json.loads(result["oa_ids"])), 3)
        self.assertEqual(result["fully_matched"], "True")
        self.assertEqual(build.assign(award, mapping, rules, {"I1": ref["I1"]}, "")["fully_matched"], "False")
        award["training_eligibility"] = "unresolved_level"
        self.assertEqual(build.assign(award, mapping, rules, ref, "")["oa_ids"], "[]")

    def test_all_degree_types(self):
        cases = {"M.D.": "doctorate_other", "D.V.M.": "doctorate_other", "B.Ch.E.": "bachelors_other",
                 "C.E.": "postsecondary_credential", "E.E.": "postsecondary_credential",
                 "D.E.": "doctorate_other", "DEng": "doctorate_other", "Kand": "postsecondary_credential",
                 "MBA": "masters_other", "MPhil": "masters_other", "Dipl": "postsecondary_credential",
                 "Certificate": "postsecondary_credential", "Diploma": "postsecondary_credential"}
        for label, family in cases.items():
            text = f"Educ: Univ Alpha, {label}, 1960. Prof Exp: Engineer, 1961."
            ds = legacy.extract(text, 1930)["degrees"] + post.extra_awards(text, 1930)
            candidates = [d for d in ds if legacy.canon(d["label_as_written"]) == legacy.canon(label)]
            self.assertEqual(len(candidates), 1, label)
            self.assertEqual(post.qualification(candidates[0]), (family, "eligible"), label)

    def test_no_heading_repair_and_no_legacy_changes(self):
        original = legacy.extract("Educ: Univ Alpha, BA, 1960; Univ Beta, PhD, 1964.", 1930)
        post.extra_awards("Educ: Univ Alpha, BA, 1960; Univ Beta, DEng, 1964.", 1930)
        self.assertEqual(original, legacy.extract("Educ: Univ Alpha, BA, 1960; Univ Beta, PhD, 1964.", 1930))
        self.assertEqual(post.extra_awards("EdW Univ Alpha, DEng, 1964. Prof Exp: Scientist.", 1930), [])
        self.assertEqual(post.extra_awards("Educ: Univ de Paris, PhD, 1964. Prof Exp: Scientist.", 1930), [])

    def test_honorary_and_titles(self):
        ds = post.extra_awards("Educ: Univ Alpha, BA, 1960. Hon Degrees: DEng, Univ Beta, 1980.", 1930)
        self.assertEqual(post.qualification(ds[0]), (None, "honorary"))
        self.assertEqual(legacy.extract("Educ: Dr Smith, licensed engineer. Mem: Certificate Board.", 1930)["degrees"], [])
        self.assertEqual(post.extra_awards("Educ: Univ Alpha, BA, 1960. Prof Exp: Certificate Board, 1965.", 1930), [])

    def test_diploma_level_and_missing_year(self):
        award = dict(group="other_credential", label_as_written="Dipl", institution_as_written="High School", honorary=False)
        self.assertEqual(post.qualification(award), (None, "below_postsecondary"))
        award["institution_as_written"] = "Academy Alpha"
        self.assertEqual(post.qualification(award), ("postsecondary_credential", "unresolved_level"))
        d = post.extra_awards("Educ: Univ Alpha, Kand. Prof Exp: Researcher.", 1930)[0]
        self.assertIsNone(d["year"])
        self.assertEqual(post.qualification(d), ("postsecondary_credential", "eligible"))


if __name__ == "__main__":
    unittest.main()

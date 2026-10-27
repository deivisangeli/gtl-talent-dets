"""Independently audit amws_education_postsecondary_v04_20261019.

Re-parses every biography with multi_v04, re-derives each award's reuse or
reassignment decision from the 20261019 baseline and the published crosswalk,
and reconciles the long ledger with the wide US person view cell by cell.
Writes <release>_validation/independent_validation.json; the release is not touched.
"""
import argparse
import collections
import json
import os
from pathlib import Path
import re
import sys

import build_postsecondary_release as base
import build_postsecondary_release_v04 as build
import education_regex_v04 as parser
import enrich_event_study_sources as enrichment
import postsecondary_qualifications as v1
import postsecondary_qualifications_v04 as scope

rows, digest = base.rows, base.digest
WIDE_AWARD = re.compile(r"^(" + "|".join(scope.FAMILIES) + r")_award_(\d+)_award_index$")


def run(root):
    dest = root / "output/amws" / build.STAMP
    manifest = base.verify_release(dest)
    baseline, top = {}, collections.Counter()
    for r in rows(root / "output/amws" / build.BASELINE / "education_awards.csv"):
        baseline[v1.fingerprint(r)] = r
        top[r["source_key"]] = max(top[r["source_key"]], int(r["award_index"]))
    ledger = collections.defaultdict(list)
    for r in rows(dest / "education_awards.csv"):
        ledger[r["source_key"]].append(r)
    mappings = base.load_crosswalk(root / "output/amws/openalex_id_resolution_20261012/raw_entry_crosswalk.jsonl")
    rules, _ = base.load_condition_rules()
    reference = build.reference_coordinates(root, mappings, rules)
    published_reference = {r["openalex_id"]: r for r in rows(dest / "institution_reference_coordinates.csv")}
    checks = collections.Counter()
    included = {}
    for bio in rows(root / "output/amws/education_enriched_20261011_updated_final/source_biographies.csv"):
        key, text, ed = bio["source_key"], bio["raw_text_adjusted"], int(bio["edition"])
        _, parsed = scope.parse(text, enrichment.integer(bio["birth_year_used"]), ed)
        got = sorted(ledger.pop(key, []), key=lambda r: (int(r["label_start"]), int(r["label_end"])))
        assert len(got) == len(parsed), key
        region = parser.bounds(text, ed)[0]
        context = text[slice(*region)] if region else ""
        positions = collections.Counter()
        for d, r in zip(parsed, got):
            assert (int(r["label_start"]), int(r["label_end"]), r["label_as_written"]) == (d["label_start"], d["label_end"], d["label_as_written"]), key
            assert text[int(r["start"]):int(r["end"])] == r["evidence"] == d["evidence"], key
            assert r["institution_as_written"] == (d["institution_as_written"] or "") and r["year"] == build.year_text(d["year"]), key
            assert r["training_eligibility"] == d["training_eligibility"] and r["requested_family"] == (d["family"] or ""), key
            fp = v1.fingerprint(r)
            prior = baseline.get(fp)
            # Published identifiers are stable; new evidence numbers after them.
            assert r["award_index"] == prior["award_index"] if prior else int(r["award_index"]) > top.get(key, 0), key
            checks["awards"] += 1
            if not r["requested_family"]:
                assert r["assignment_reused"] == "False" and r["training_eligibility"] in (
                    "honorary", "below_postsecondary", "title", "not_a_qualification", "board_certification"), key
                checks["excluded"] += 1
                continue
            positions[r["requested_family"]] += 1
            assert int(r["family_award_index"]) == positions[r["requested_family"]], key
            reusable = prior is not None and prior["requested_family"] and build.reuse_key(prior) == build.reuse_key(r)
            assert (r["assignment_reused"] == "True") == bool(reusable), key
            if reusable:
                assert all(r[k] == prior.get(k, "") for k in build.ASSIGNMENT), key
                checks["reused"] += 1
            else:
                fresh = base.assign(dict(r), mappings.get(r["institution_as_written"] or None), rules, reference, context)
                assert all(r[k] == v for k, v in fresh.items()), (key, r["award_index"])
                checks["reassigned"] += 1
            ids = json.loads(r["oa_ids"])
            for i, lat in zip(ids, json.loads(r["oa_latitudes"])):
                assert abs(float(published_reference[i]["latitude"]) - float(lat)) < 1e-12, (key, i)
            included[(key, r["award_index"])] = r
    assert not ledger, "Ledger entries without a source biography"
    # Every published included award absent from the v04 included set is listed once as removed.
    removed = collections.Counter((r["source_key"], r["award_index"]) for r in rows(dest / "removed_awards.csv"))
    expected_removed = {(f[0], r["award_index"]) for f, r in baseline.items() if r["requested_family"] and (f[0], r["award_index"]) not in included}
    assert set(removed) == expected_removed and max(removed.values(), default=1) == 1
    checks["removed"] = len(removed)
    # Wide view: every non-education canonical cell unchanged; every award cell from the ledger.
    canonical = rows(root / "Data/processed/amws/amws_open_alex.csv")
    count, wide_awards = 0, 0
    for r, c in zip(rows(dest / "amws_open_alex.csv"), canonical):
        assert r["source_key"] == c["source_key"]
        for k, v in c.items():
            if not build.AWARD_COLUMN.match(k) and k not in enrichment.NEW_COLUMNS and k not in build.PERSON_META:
                assert r[k] == v, (c["source_key"], k)
        found = 0
        for k, v in r.items():
            m = WIDE_AWARD.match(k)
            if not m or not v:
                continue
            a = included[(r["source_key"], v)]
            assert (a["requested_family"], a["family_award_index"]) == (m.group(1), m.group(2)), (r["source_key"], k)
            for name, value in build.wide_values(a).items():
                assert r[name] == str(value), (r["source_key"], name)
            found += 1
        assert found == int(r["oa_award_count"]), r["source_key"]
        wide_awards += found
        count += 1
    assert count == 136489 == manifest["canonical_entries"]
    report = dict(passed=True, release=build.STAMP, release_manifest_sha256=digest(dest / "completion_manifest.json"),
                  validator_sha256=digest(Path(__file__)), canonical_entries=count, wide_awards_reconciled=wide_awards,
                  **{f"{k}_awards_checked": n for k, n in checks.items()},
                  reparsed_with=parser.RULE_VERSION, assignments_recomputed=True, coordinates_checked=True,
                  non_education_cells_unchanged=True)
    out = dest.parent / (build.STAMP + "_validation")
    out.mkdir(exist_ok=True)
    base.write_json(out / "independent_validation.json", report)
    print(json.dumps(report, indent=1))


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--data-root", type=Path, default=Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets"))))
    run(cli.parse_args().data_root.resolve())

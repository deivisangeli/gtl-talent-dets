"""Independent source/evidence, assignment and wide-export acceptance checks."""
import argparse
import collections
import csv
import itertools
import json
import os
from pathlib import Path
import sqlite3

import build_postsecondary_release as build
import postsecondary_qualifications as scope
import education_regex as legacy


def run(root):
    release = root / "output/amws" / build.STAMP
    manifest = build.verify_release(release)
    old = root / "output/amws/amws_education_openalex_coordinates_20261014"
    enriched = root / "output/amws/education_enriched_20261011_updated_final"
    work = Path(os.environ.get("AMWS_CHECKPOINT_ROOT", str(Path.home() / ".codex/amws_checkpoints"))) / build.STAMP
    db = sqlite3.connect(work / "independent_validation.sqlite")
    db.executescript("DROP TABLE IF EXISTS old; DROP TABLE IF EXISTS new; "
        "CREATE TABLE old(source_key TEXT,ls INTEGER,le INTEGER,label TEXT,data TEXT,PRIMARY KEY(source_key,ls,le,label)); "
        "CREATE TABLE new(source_key TEXT,award_index INTEGER,data TEXT,PRIMARY KEY(source_key,award_index));")
    old_count = 0
    for a in build.rows(old / "education_awards_openalex.csv"):
        db.execute("INSERT INTO old VALUES(?,?,?,?,?)", (*scope.fingerprint(a), build.dumps(a)))
        old_count += 1
    reference = {r["openalex_id"]: r for r in build.rows(release / "institution_reference_coordinates.csv")}
    mappings = build.load_crosswalk(root / "output/amws/openalex_id_resolution_20261012/raw_entry_crosswalk.jsonl")
    rules, _ = build.load_condition_rules()
    published = itertools.groupby(build.rows(release / "education_awards.csv"), key=lambda a: a["source_key"])
    current = next(published, (None, []))
    group_key, group_rows = current[0], list(current[1])
    baseline = itertools.groupby(build.rows(enriched / "education_awards.csv"), key=lambda a: a["source_key"])
    initial = next(baseline, (None, []))
    initial_key, initial_rows = initial[0], list(initial[1])
    full_count = checked_old = additional = header_missing = 0
    stats = collections.Counter()
    for bio in build.rows(enriched / "source_biographies.csv"):
        key, text = bio["source_key"], bio["raw_text_adjusted"]
        source_awards = initial_rows if initial_key == key else []
        if initial_key == key:
            nxt = next(baseline, (None, []))
            initial_key, initial_rows = nxt[0], list(nxt[1])
        actual = group_rows if group_key == key else []
        if group_key == key:
            nxt = next(published, (None, []))
            group_key, group_rows = nxt[0], list(nxt[1])
        by_id = {int(a["award_index"]): a for a in actual}
        assert len(by_id) == len(actual)
        for a in source_awards:
            b = by_id[int(a["award_index"])]
            for k in a:
                if k not in ("requested_family", "family_award_index"):
                    assert a[k] == b[k], (key, k)
        region, flags = legacy.bounds(text, int(bio["edition"]))
        context = text[slice(*region)] if region else ""
        if "education_heading_not_found" in flags:
            header_missing += 1
            assert not actual, key
        for a in actual:
            assert text[int(a["start"]):int(a["end"])] == a["evidence"]
            assert text[int(a["label_start"]):int(a["label_end"]) ] == a["label_as_written"]
            assert json.loads(a["evidence_key"]) == list(scope.fingerprint(a))
            full_count += 1
            if a["requested_family"] not in scope.FAMILIES:
                assert a["training_eligibility"] in ("honorary", "below_postsecondary", "title", "not_a_qualification")
                continue
            assert a["honorary"].lower() == "false"
            ids = json.loads(a["oa_ids"])
            assert len(ids) == len(set(ids))
            assert set(ids) <= reference.keys()
            for i, entity in enumerate(ids):
                assert abs(float(reference[entity]["latitude"]) - json.loads(a["oa_latitudes"])[i]) < 1e-12
                assert abs(float(reference[entity]["longitude"]) - json.loads(a["oa_longitudes"])[i]) < 1e-12
            original = db.execute("SELECT data FROM old WHERE source_key=? AND ls=? AND le=? AND label=?", scope.fingerprint(a)).fetchone()
            if a["legacy_award"] == "True":
                assert original is not None
                original = json.loads(original[0])
                assert all(a[k] == value for k, value in original.items()), key
                checked_old += 1
            else:
                additional += 1
                if a["training_eligibility"] != "eligible":
                    assert not ids and a["fully_matched"] == "False"
                else:
                    mapping = mappings.get(a["institution_as_written"])
                    expected, conditional, comps = set(), set(), collections.defaultdict(set)
                    if mapping:
                        expected.update(mapping["existing"] + mapping["additional"])
                        for match in mapping["matches"]:
                            condition = match.get("conditions")
                            if condition is None or rules.condition_applies(condition, dict(a, education_context=context)):
                                entity = rules.valid_id(match["openalex_id"])
                                comps[match.get("component_index", 0)].add(entity)
                                if condition is not None:
                                    conditional.add(entity)
                        conflict = any(len(v) > 1 for v in comps.values()) or (len(comps) > 1 and not mapping["explicit"])
                        if not conflict:
                            for values in comps.values():
                                expected.update(values)
                        else:
                            conditional.clear()
                        assert (a["condition_conflict"] == "True") == conflict
                        assert set(json.loads(a["conditional_IDs"])) == conditional & reference.keys()
                    assert set(ids) == expected & reference.keys(), key
            stats[(a["edition"], a["requested_family"])] += 1
            db.execute("INSERT INTO new VALUES(?,?,?)", (key, int(a["award_index"]), build.dumps(a)))
        if full_count % 20000 < 10:
            db.commit()
    assert group_key is None and initial_key is None
    assert checked_old == old_count == manifest["old_awards_preserved"]
    db.commit()
    unchanged = primary_columns = entries = 0
    canonical = root / "Data/processed/amws/amws_open_alex.csv"
    updated = release / "amws_open_alex.csv"
    metadata = {"oa_ids", "oa_id_count", "oa_has_id", "oa_award_count", "oa_matched_award_count", "oa_multi_institution_award_count", "oa_export_version"}
    for before, after in itertools.zip_longest(build.rows(canonical), build.rows(updated)):
        assert before is not None and after is not None
        assert before["source_key"] == after["source_key"]
        for k, value in before.items():
            if k not in metadata:
                assert after[k] == value, (before["source_key"], k)
                unchanged += 1
                primary_columns += k.startswith(("ba_bs_", "ma_", "ms_", "phd_"))
        awards = [json.loads(v[0]) for v in db.execute("SELECT data FROM new WHERE source_key=?", (before["source_key"],))]
        assert len(awards) == int(after["oa_award_count"])
        all_ids = set()
        for a in awards:
            family, position = a["requested_family"], a["family_award_index"]
            prefix = f"{family}_award_{position}_"
            assert after[prefix + "award_index"] == a["award_index"]
            assert after[prefix + "raw_institution"] == (a["institution_as_written"] or "")
            assert after[prefix + "degree_label"] == a["label_as_written"]
            ids = json.loads(a["oa_ids"])
            assert len(ids) == int(after[prefix + "oa_id_count"])
            assert after[prefix + "fully_matched"] == a["fully_matched"]
            assert after[prefix + "condition_conflict"] == a["condition_conflict"]
            for i, entity in enumerate(ids, 1):
                assert after[f"{prefix}institution_{i}_oa_id"] == entity
                assert abs(float(after[f"{prefix}institution_{i}_latitude"]) - json.loads(a["oa_latitudes"])[i-1]) < 1e-12
                assert abs(float(after[f"{prefix}institution_{i}_longitude"]) - json.loads(a["oa_longitudes"])[i-1]) < 1e-12
            all_ids.update(ids)
        assert set(json.loads(after["oa_ids"])) == all_ids
        entries += 1
    assert entries == 136489
    report = dict(passed=True, original_awards_and_assignments_unchanged=checked_old,
        added_earned_qualifications=additional, all_ledger_awards_evidence_verified=full_count,
        unchanged_original_cells=unchanged, unchanged_primary_family_cells=primary_columns,
        canonical_entries=entries, heading_missing_entries_unchanged=header_missing,
        conditional_assignments_recomputed=True, full_long_wide_reconciliation=True,
        validator_sha256=build.digest(Path(__file__)), release_manifest_sha256=build.digest(release / "completion_manifest.json"))
    target = root / "output/amws" / (build.STAMP + "_validation")
    target.mkdir(exist_ok=True)
    build.write_json(target / "independent_validation.json", report)
    import shutil
    shutil.copy2(Path(__file__), target / Path(__file__).name)
    db.close()
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--data-root", type=Path, default=Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets"))))
    run(cli.parse_args().data_root.resolve())

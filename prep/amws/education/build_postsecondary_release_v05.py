"""Publish the v05 AMWS education release: manual institution matches on top of v04.

v05 does not re-parse biographies. It reads the published v04 release and changes
only awards that v04 left without a full match:

- an eligible award whose institution text has an ID in the top-500 manual
  crosswalk (manual_institution_matches_top500_20261019) gets that ID;
- an award listed in medical_campus_overrides.csv gets the override ID instead
  (MD, dental and pharmacy degrees granted by a separate medical campus);
- an award that already held some IDs keeps them, with the manual ID added.

Awards v04 matched fully are never touched, even where the crosswalk names a
different institution; those disagreements go to manual_conflicts_review.csv.
Every other award, and every non-award person cell, is carried over unchanged.

The person view also gains the 1986 physical-science classification
(physical_science_dummy_20261020): physical_science, physical_science_unresolved
and physical_science_mixed, blank for the 1906, 1938 and 1955 editions.

Awards still unmatched are stored in unmatched_awards.csv and unmatched_strings.csv,
with an OCR-normalised candidate ID recorded for later review but not applied.
"""
import argparse
import collections
import csv
import json
import math
import os
from pathlib import Path
import re
import shutil
import sqlite3
import sys

import attach_amws_openalex_coordinates as coordinates
import build_postsecondary_release as base
import build_postsecondary_release_v04 as v04
import export_amws_openalex as export
import postsecondary_qualifications as v1
import postsecondary_qualifications_v04 as scope

STAMP = "amws_education_postsecondary_v05_20261020"
BASELINE = v04.STAMP
MANUAL = "manual_institution_matches_top500_20261019"
FIELDS_RELEASE = "physical_science_dummy_20261020"
VERSION = "postsecondary_v05"
FIELD_COLUMNS = ("physical_science", "physical_science_unresolved", "physical_science_mixed")
PERSON_COLUMNS = ("manual_assigned_awards",) + FIELD_COLUMNS
LEDGER_COLUMNS = ("manual_assignment", "manual_confidence")
CROSSWALK_CLASS, OVERRIDE_CLASS = "manual_top500_crosswalk", "manual_medical_campus_override"
CANDIDATE_AGREEMENT = 0.9
csv.field_size_limit(16 * 1024 * 1024)
rows, digest, dumps, table, write_json = base.rows, base.digest, base.dumps, base.table, base.write_json


def load_reference(path, ids):
    """Snapshot rows for the given IDs, rejecting duplicates and invalid coordinates."""
    reference, _ = coordinates.load_reference(path, set(ids))
    return reference


def load_crosswalk(path):
    """Institution text -> crosswalk row, for every row of the manual crosswalk."""
    out = {}
    for r in rows(path):
        name = r["institution_as_written"]
        assert name not in out, "Duplicate crosswalk string: " + name
        assert r["openalex_id"] == "" or re.fullmatch(r"I\d+", r["openalex_id"]), r["openalex_id"]
        out[name] = r
    return out


def load_overrides(path, crosswalk):
    """(source_key, award_index) -> override row; each must agree with the crosswalk."""
    out = {}
    for r in rows(path):
        key = (r["source_key"], r["award_index"])
        assert key not in out, "Duplicate override: " + str(key)
        assert re.fullmatch(r"I\d+", r["override_id"]), r["override_id"]
        assert crosswalk.get(r["institution_as_written"], {}).get("openalex_id") == r["crosswalk_id"], key
        out[key] = r
    return out


def load_physical_science(folder):
    """source_key -> the three field columns, checked against the field release manifest."""
    manifest = json.loads((folder / "manifest.json").read_text(encoding="utf-8"))
    audit = folder / "amws_ed16_physical_science_audit.csv"
    assert digest(audit) == manifest["output_checksums_sha256"][audit.name], "Field release changed"
    out = {}
    for r in rows(audit):
        assert r["source_key"].startswith("1986:") and r["source_key"] not in out, r["source_key"]
        assert {r["physical_science"], r["unresolved"], r["mixed"]} <= {"0", "1"}, r["source_key"]
        out[r["source_key"]] = dict(physical_science=r["physical_science"], physical_science_unresolved=r["unresolved"],
                                    physical_science_mixed=r["mixed"])
    assert len(out) == manifest["rows"]
    return out


def coordinate_row(snapshot, reference_path, reference_sha):
    """A row in the shape of institution_reference_coordinates.csv, from a snapshot row."""
    return dict(openalex_id=snapshot["id"], oa_canonical_name=snapshot["name"], reference_name=snapshot["name"],
                latitude=snapshot["latitude"], longitude=snapshot["longitude"], city=snapshot["city"],
                country=snapshot["country"], region=snapshot["region"], geonames_city_id=snapshot["geonames_city_id"],
                coordinate_kind="openalex_reference_location", historical_campus_accuracy_verified="False",
                reference_updated_date=snapshot["updated_date"], reference_source_file=snapshot["source_file"],
                reference_table=str(reference_path), reference_sha256=reference_sha)


def qualifies(award):
    """Whether v05 may assign this award: an included, eligible award v04 did not fully match."""
    return bool(award["requested_family"]) and award["training_eligibility"] == "eligible" and award["fully_matched"] != "True"


def manual_assignment(award, crosswalk, overrides, reference, sources):
    """The v05 assignment fields for one award, or None when the award keeps its v04 values."""
    if not qualifies(award):
        return None
    override = overrides.get((award["source_key"], award["award_index"]))
    row = crosswalk.get(award["institution_as_written"])
    if override is not None:
        entity, how, confidence, record = override["override_id"], "override", override["confidence"], override["rule_id"]
    elif row is not None and row["openalex_id"]:
        entity, how, confidence, record = row["openalex_id"], "crosswalk", row["confidence"], f'top500:{row["rank"]}'
    else:
        return None
    before = json.loads(award["oa_ids"])
    ids = sorted(set(before) | {entity})
    flags = ["manual_union_with_existing_ids"] if before else []
    return dict(oa_ids=dumps(ids), oa_names=dumps([reference[i]["oa_canonical_name"] for i in ids]),
                oa_latitudes=dumps([float(reference[i]["latitude"]) for i in ids]),
                oa_longitudes=dumps([float(reference[i]["longitude"]) for i in ids]),
                oa_cities=dumps([reference[i]["city"] for i in ids]), oa_countries=dumps([reference[i]["country"] for i in ids]),
                fully_matched="True", condition_conflict="False", conditional_IDs="[]",
                assignment_class=dumps([OVERRIDE_CLASS if how == "override" else CROSSWALK_CLASS]),
                matching_flags=dumps(flags), record_id=record, mapping_origin=MANUAL, raw_mapping_status=confidence,
                IDs_before=dumps(before), IDs_after=dumps(ids), additional_IDs=dumps(sorted(set(ids) - set(before))),
                matching_source=sources[how], decision_source=sources[how],
                manual_assignment=how, manual_confidence=confidence)


def normalized_name(text):
    """Institution text with common OCR and abbreviation variants folded together."""
    s = text.lower().replace("’", "'").replace("¬", "")
    s = re.sub(r"[^a-z0-9' ]", " ", s)
    for pattern, repl in ((r"\b(uniy|unix|umv|unlv|unis|l'niv|lniv|univ|uni)\b", "univ"),
                          (r"\b(111|iii|iil|ill|ii|iu|hi)\b", "ill"),
                          (r"\b(tcchnol|technoj|techno|tcch|tech)\b", "technol"),
                          (r"\b(col|coll|college)\b", "col")):
        s = re.sub(pattern, repl, s)
    s = re.sub(r"\b[il1hnu]{0,3}[il1]?[nm][o0][il1]s\b", "illinois", s)
    return " ".join(s.split())


def unmatched_category(award, crosswalk):
    """Why an included award has no full match in v05."""
    if award["training_eligibility"] != "eligible":
        return award["training_eligibility"]
    if not award["institution_as_written"].strip():
        return "no_institution_text"
    if json.loads(award["oa_ids"]):
        return "partially_matched"
    row = crosswalk.get(award["institution_as_written"])
    return row["confidence"] if row is not None else "outside_top500"


def person_summary(awards):
    """The person-level oa_* summaries, from that person's included awards."""
    all_ids = set()
    for a in awards:
        all_ids.update(json.loads(a["oa_ids"]))
    return dict(oa_ids=dumps(sorted(all_ids)), oa_id_count=len(all_ids), oa_has_id=str(bool(all_ids)),
                oa_award_count=len(awards), oa_matched_award_count=sum(bool(json.loads(a["oa_ids"])) for a in awards),
                oa_multi_institution_award_count=sum(len(json.loads(a["oa_ids"])) > 1 for a in awards),
                oa_export_version=scope.VERSION)


def run(root):
    dest = root / "output/amws" / STAMP
    if dest.exists():
        base.verify_release(dest)
        print("Verified existing v05 education release:", dest)
        return
    previous = root / "output/amws" / BASELINE
    manual = root / "output/amws" / MANUAL
    fields_release = root / "output/amws" / FIELDS_RELEASE
    reference_path = root / "output/amws/education_geolocated/reference_institutions.csv"
    print("Verifying the v04 release.", flush=True)
    base.verify_release(previous)
    here = Path(__file__)
    code = [here, Path(base.__file__), Path(v04.__file__), Path(v1.__file__), Path(scope.__file__), Path(export.__file__),
            Path(coordinates.__file__), here.with_name("test_postsecondary_release_v05.py")]
    previous_files = ["completion_manifest.json", "education_awards.csv", "education_records.csv", "amws_open_alex.csv",
                      "institution_reference_coordinates.csv", "schema.json"]
    inputs = [previous / f for f in previous_files] + [
        manual / "top500_manual_matches.csv", manual / "medical_campus_overrides.csv", manual / "medical_campus_rules.csv",
        fields_release / "manifest.json", fields_release / "amws_ed16_physical_science_audit.csv", reference_path, *code]
    hashes = {str(p.resolve()): digest(p) for p in inputs}
    sources = dict(crosswalk=f"output/amws/{MANUAL}/top500_manual_matches.csv",
                   override=f"output/amws/{MANUAL}/medical_campus_overrides.csv")

    crosswalk = load_crosswalk(manual / "top500_manual_matches.csv")
    overrides = load_overrides(manual / "medical_campus_overrides.csv", crosswalk)
    physical = load_physical_science(fields_release)
    published = {r["openalex_id"]: r for r in rows(previous / "institution_reference_coordinates.csv")}
    manual_ids = {r["openalex_id"] for r in crosswalk.values() if r["openalex_id"]} | {r["override_id"] for r in overrides.values()}
    snapshot = load_reference(reference_path, manual_ids)
    for r in crosswalk.values():
        if r["openalex_id"]:
            assert snapshot[r["openalex_id"]]["name"] == r["openalex_display_name"], r["institution_as_written"]
    for r in overrides.values():
        assert snapshot[r["override_id"]]["name"] == r["override_name"], r["rule_id"]
    reference = dict(published)
    reference_sha = hashes[str(reference_path.resolve())]
    for entity in manual_ids - published.keys():
        reference[entity] = coordinate_row(snapshot[entity], reference_path, reference_sha)
    print(f"Manual IDs: {len(manual_ids)}, of which {len(manual_ids - published.keys())} are new to v04.", flush=True)

    work = Path(os.environ.get("AMWS_CHECKPOINT_ROOT", str(Path.home() / ".codex/amws_checkpoints"))) / STAMP
    stage = work / "stage"
    if stage.exists():
        shutil.rmtree(stage)
    stage.mkdir(parents=True)
    db = sqlite3.connect(work / "awards.sqlite")
    db.executescript("DROP TABLE IF EXISTS expanded; "
                     "CREATE TABLE expanded(source_key TEXT,award_index INTEGER,data TEXT,PRIMARY KEY(source_key,award_index));")

    ledger_fields = list(next(rows(previous / "education_awards.csv"))) + list(LEDGER_COLUMNS)
    af, aw = table(stage / "education_awards_openalex.csv", ledger_fields)
    lf, lw = table(stage / "education_awards.csv", ledger_fields)
    mf, mw = table(stage / "manual_assignments.csv", [
        "source_key", "edition", "award_index", "requested_family", "label_as_written", "institution_as_written", "year",
        "old_oa_ids", "new_oa_ids", "new_oa_names", "old_fully_matched", "manual_assignment", "manual_confidence", "record_id"])
    conflicts_f, conflicts_w = table(stage / "manual_conflicts_review.csv", [
        "source_key", "edition", "award_index", "label_as_written", "institution_as_written", "year",
        "v04_oa_ids", "v04_oa_names", "crosswalk_id", "crosswalk_name", "crosswalk_confidence"])
    research = (stage / "institution_research_inventory.jsonl").open("w", encoding="utf-8")
    coverage, by_person = collections.Counter(), collections.defaultdict(lambda: collections.Counter())
    unresolved, used_ids, extra_fields, overrides_seen = collections.Counter(), set(), set(), set()
    max_awards, max_slots = collections.Counter(), collections.Counter()
    single_id = collections.defaultdict(collections.Counter)   # normalised text -> fully matched single IDs
    unmatched = []
    print("Applying manual matches to the v04 award ledger.", flush=True)
    for award in rows(previous / "education_awards.csv"):
        award.update(manual_assignment="", manual_confidence="")
        key = (award["source_key"], award["award_index"])
        if key in overrides:
            o = overrides[key]
            assert award["evidence_key"] == o["evidence_key"] and award["institution_as_written"] == o["institution_as_written"], key
            assert qualifies(award), ("override on an award v05 may not assign", key)
            overrides_seen.add(key)
        if not award["requested_family"]:
            lw.writerow(award)
            continue
        old_ids, old_full = award["oa_ids"], award["fully_matched"]
        change = manual_assignment(award, crosswalk, overrides, reference, sources)
        if change is not None:
            award.update(change)
            coverage[f"manual_{change['manual_assignment']}_awards"] += 1
            coverage[f"manual_confidence:{change['manual_confidence']}"] += 1
            by_person[award["source_key"]]["manual"] += 1
            mw.writerow(dict(source_key=award["source_key"], edition=award["edition"], award_index=award["award_index"],
                             requested_family=award["requested_family"], label_as_written=award["label_as_written"],
                             institution_as_written=award["institution_as_written"], year=award["year"], old_oa_ids=old_ids,
                             new_oa_ids=award["oa_ids"], new_oa_names=award["oa_names"], old_fully_matched=old_full,
                             manual_assignment=change["manual_assignment"], manual_confidence=change["manual_confidence"],
                             record_id=change["record_id"]))
        elif award["fully_matched"] == "True":
            row = crosswalk.get(award["institution_as_written"])
            if row is not None and row["openalex_id"] and json.loads(award["oa_ids"]) != [row["openalex_id"]]:
                coverage["manual_conflicts_left_on_v04"] += 1
                conflicts_w.writerow(dict(source_key=award["source_key"], edition=award["edition"], award_index=award["award_index"],
                                  label_as_written=award["label_as_written"], institution_as_written=award["institution_as_written"],
                                  year=award["year"], v04_oa_ids=award["oa_ids"], v04_oa_names=award["oa_names"],
                                  crosswalk_id=row["openalex_id"], crosswalk_name=row["openalex_display_name"],
                                  crosswalk_confidence=row["confidence"]))
        ids = json.loads(award["oa_ids"])
        assert set(ids) <= reference.keys(), (key, ids)
        used_ids.update(ids)
        ed, family = award["source_key"].split(":")[0], award["requested_family"]
        coverage[f"{ed}:{family}:awards"] += 1
        coverage[f"{ed}:{family}:matched"] += bool(ids)
        by_person[award["source_key"]]["awards"] += 1
        by_person[award["source_key"]]["matched"] += bool(ids)
        if award["fully_matched"] == "True" and len(ids) == 1 and award["institution_as_written"].strip():
            single_id[normalized_name(award["institution_as_written"])][ids[0]] += 1
        if award["fully_matched"] != "True":
            unresolved[(award["institution_as_written"], award["training_eligibility"], family)] += 1
            if v1.select_for_research(award):
                research.write(dumps({k: award[k] for k in ("source_key", "award_index", "requested_family", "institution_as_written",
                                                            "year", "evidence", "evidence_key", "flags", "matching_flags")}) + "\n")
            unmatched.append(dict(source_key=award["source_key"], edition=ed, award_index=award["award_index"],
                                  requested_family=family, label_as_written=award["label_as_written"],
                                  institution_as_written=award["institution_as_written"], year=award["year"],
                                  training_eligibility=award["training_eligibility"], oa_ids=award["oa_ids"],
                                  evidence=award["evidence"], unmatched_category=unmatched_category(award, crosswalk)))
        max_awards[family] = max(max_awards[family], int(award["family_award_index"]))
        max_slots[family] = max(max_slots[family], len(ids), 1)
        extra_fields.update(v04.wide_values(award))
        aw.writerow(award)
        lw.writerow(award)
        db.execute("INSERT INTO expanded VALUES(?,?,?)", (award["source_key"], int(award["award_index"]), dumps(award)))
    for f in (af, lf, mf, conflicts_f, research):
        f.close()
    db.commit()
    assert overrides_seen == overrides.keys(), "Overrides missing from the v04 ledger"
    print("Ledger pass:", {k: v for k, v in coverage.items() if k.startswith("manual")}, flush=True)

    # Unmatched store, with an OCR-normalised candidate recorded but not applied.
    candidates = {}
    for k, counts in single_id.items():
        entity, n = counts.most_common(1)[0]
        if n / sum(counts.values()) >= CANDIDATE_AGREEMENT:
            candidates[k] = (entity, round(n / sum(counts.values()), 3))
    names = {i: r["oa_canonical_name"] for i, r in reference.items()}
    snapshot_names = {}
    wanted = set()
    for u in unmatched:
        hit = candidates.get(normalized_name(u["institution_as_written"])) if u["unmatched_category"] == "outside_top500" else None
        u.update(normalization_candidate_id=hit[0] if hit else "", normalization_candidate_share=hit[1] if hit else "")
        if hit:
            wanted.add(hit[0])
    for r in rows(reference_path):
        if r["id"] in wanted:
            snapshot_names[r["id"]] = r["name"]
    for u in unmatched:
        u["normalization_candidate_name"] = names.get(u["normalization_candidate_id"]) or snapshot_names.get(u["normalization_candidate_id"], "")
    unmatched_fields = ["source_key", "edition", "award_index", "requested_family", "label_as_written", "institution_as_written", "year",
                        "training_eligibility", "oa_ids", "unmatched_category", "normalization_candidate_id",
                        "normalization_candidate_name", "normalization_candidate_share", "evidence"]
    uf, uw = table(stage / "unmatched_awards.csv", unmatched_fields)
    strings = {}
    for u in unmatched:
        uw.writerow(u)
        s = strings.setdefault(u["institution_as_written"], dict(institution_as_written=u["institution_as_written"], awards=0,
                               persons=set(), categories=collections.Counter(), editions=collections.Counter(),
                               normalization_candidate_id=u["normalization_candidate_id"],
                               normalization_candidate_name=u["normalization_candidate_name"],
                               normalization_candidate_share=u["normalization_candidate_share"]))
        s["awards"] += 1
        s["persons"].add(u["source_key"])
        s["categories"][u["unmatched_category"]] += 1
        s["editions"][u["edition"]] += 1
    uf.close()
    sf, sw = table(stage / "unmatched_strings.csv", ["institution_as_written", "awards", "persons", "unmatched_category",
                                                    "editions", "normalization_candidate_id", "normalization_candidate_name",
                                                    "normalization_candidate_share"])
    for s in sorted(strings.values(), key=lambda s: (-s["awards"], s["institution_as_written"])):
        sw.writerow(dict(s, persons=len(s["persons"]), unmatched_category=";".join(f"{k}:{n}" for k, n in s["categories"].most_common()),
                         editions=";".join(f"{k}:{n}" for k, n in sorted(s["editions"].items()))))
    sf.close()
    coverage["unmatched_awards"] = len(unmatched)
    coverage["unmatched_strings"] = len(strings)
    coverage["unmatched_with_normalization_candidate"] = sum(bool(u["normalization_candidate_id"]) for u in unmatched)
    for u in unmatched:
        coverage[f"unmatched_category:{u['unmatched_category']}"] += 1

    print("Writing the US person view.", flush=True)
    with (previous / "amws_open_alex.csv").open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        old_fields = reader.fieldnames
        old_award_fields = [k for k in old_fields if v04.AWARD_COLUMN.match(k)]
        non_award = [k for k in old_fields if not v04.AWARD_COLUMN.match(k)]
        assert non_award[-2:] == ["postsecondary_scope", "reassigned_awards"], non_award[-2:]
        award_fields = sorted(extra_fields | set(old_award_fields), key=v04.column_order)
        fields = non_award[:-2] + award_fields + non_award[-2:] + list(PERSON_COLUMNS)
        of, ow = table(stage / "amws_open_alex.csv", fields)
        count, seen_1986 = 0, set()
        for r in reader:
            key = r["source_key"]
            awards = [json.loads(a[0]) for a in db.execute("SELECT data FROM expanded WHERE source_key=? ORDER BY award_index", (key,))]
            out = {k: r[k] for k in non_award}
            wide = {}
            for a in awards:
                wide.update(v04.wide_values(a))
            out.update({k: wide.get(k, "") for k in award_fields})
            summary = person_summary(awards)
            out.update(summary)
            n_manual = by_person[key]["manual"]
            if n_manual == 0:
                # Untouched people must reproduce their v04 award cells and summaries exactly.
                assert all(str(out[k]) == r.get(k, "") for k in award_fields), key   # coordinates are floats until written
                assert all(str(out[k]) == r[k] for k in v04.PERSON_META), key
            out["manual_assigned_awards"] = n_manual
            field = physical.get(key)
            if key.startswith("1986:"):
                assert field is not None, key
                seen_1986.add(key)
            out.update(field or {k: "" for k in FIELD_COLUMNS})
            ow.writerow(out)
            count += 1
        of.close()
    assert count == 136489
    assert seen_1986 == physical.keys(), "Physical-science keys differ from the 1986 person rows"

    rf, rw = table(stage / "education_records.csv", list(next(rows(previous / "education_records.csv"))) + ["manual_awards"])
    for r in rows(previous / "education_records.csv"):
        p = by_person[r["source_key"]]
        assert int(r["awards"]) == p["awards"], r["source_key"]
        rw.writerow(dict(r, matched_awards=p["matched"], manual_awards=p["manual"]))
    rf.close()
    coord_fields = list(next(rows(previous / "institution_reference_coordinates.csv")))
    cf, cw = table(stage / "institution_reference_coordinates.csv", coord_fields)
    for entity in sorted(used_ids):
        cw.writerow(reference[entity])
    cf.close()
    nf, nw = table(stage / "unresolved_training_inventory.csv", ["institution_as_written", "eligibility", "family", "awards"])
    for (name, eligibility, family), n in sorted(unresolved.items()):
        nw.writerow(dict(institution_as_written=name, eligibility=eligibility, family=family, awards=n))
    nf.close()
    vf, vw = table(stage / "coverage_by_edition_family.csv", ["edition", "family", "metric", "count"])
    for k, n in coverage.items():
        bits = k.split(":")
        if len(bits) == 3:
            vw.writerow(dict(edition=bits[0], family=bits[1], metric=bits[2], count=n))
    vf.close()
    # Nothing is removed or re-parsed relative to v04; the empty logs keep the schema downstream readers expect.
    for name in ("removed_awards.csv", "changed_awards.csv"):
        header = list(next(csv.reader((previous / name).open(encoding="utf-8-sig", newline=""))))
        table(stage / name, header)[0].close()

    schema = json.loads((previous / "schema.json").read_text(encoding="utf-8"))
    for k in ("reused_awards", "reassigned_awards"):   # v04's counts relative to 20261019; they do not describe v05
        schema.pop(k)
    schema.update(version=VERSION, maximum_awards=dict(max_awards), maximum_institution_slots=dict(max_slots),
                  baseline_release=BASELINE, removed_awards=0,
                  manual_crosswalk=f"output/amws/{MANUAL}/top500_manual_matches.csv",
                  manual_overrides=f"output/amws/{MANUAL}/medical_campus_overrides.csv",
                  manual_crosswalk_awards=coverage["manual_crosswalk_awards"], manual_override_awards=coverage["manual_override_awards"],
                  manual_conflicts_left_on_v04=coverage["manual_conflicts_left_on_v04"],
                  field_classification=dict(release=f"output/amws/{FIELDS_RELEASE}", columns=list(FIELD_COLUMNS), editions=["1986"],
                                            zero_definition="No detected Physical match, including unresolved entries; not a verified non-Physical label."))
    write_json(stage / "schema.json", schema)
    write_json(stage / "coverage_report.json", dict(canonical_entries=count, counters=dict(coverage), used_institutions=len(used_ids),
                                                    new_institutions=len(used_ids - published.keys())))
    write_json(stage / "amws_open_alex_manifest.json", dict(rows=count, sha256=digest(stage / "amws_open_alex.csv"), schema="schema.json"))
    (stage / "DATA_DICTIONARY.txt").write_text(
        f"v05 = the v04 release ({BASELINE}) plus manual institution matches; biographies are not re-parsed.\n"
        "Families, award indices, labels, years and eligibility are unchanged from v04. Nothing is removed (removed_awards.csv and changed_awards.csv are empty).\n"
        f"An eligible award v04 did not fully match gets the ID of its institution text in {MANUAL}/top500_manual_matches.csv, "
        "or the award-level medical-campus override (medical_campus_overrides.csv) where one exists; existing partial IDs are kept and the manual ID added.\n"
        "Manual awards have assignment_class manual_top500_crosswalk or manual_medical_campus_override, raw_mapping_status = the manual confidence, "
        "and ledger columns manual_assignment and manual_confidence. manual_assignments.csv lists them.\n"
        "Awards v04 fully matched are never changed; manual_conflicts_review.csv lists those whose ID differs from the crosswalk.\n"
        f"amws_open_alex.csv adds manual_assigned_awards and the 1986 physical-science classification ({FIELDS_RELEASE}): physical_science, "
        "physical_science_unresolved, physical_science_mixed, blank for 1906, 1938 and 1955. physical_science = 0 means no Physical match detected, "
        "not a verified non-Physical label.\n"
        "unmatched_awards.csv and unmatched_strings.csv store every included award still without a full match, with its category and an "
        "OCR-normalised candidate ID that is recorded for review but not applied.\n",
        encoding="utf-8")
    snap = stage / "code_snapshot"
    snap.mkdir(exist_ok=True)
    for p in code:
        shutil.copy2(p, snap / p.name)
    for p, checksum in hashes.items():
        assert digest(p) == checksum, p
    checksums = {str(p.relative_to(stage)): digest(p) for p in stage.rglob("*") if p.is_file() and p.name != "completion_manifest.json"}
    write_json(stage / "completion_manifest.json", dict(status="complete", input_checksums=hashes, output_checksums=checksums,
                                                        canonical_entries=count, schema=schema))
    shutil.copytree(stage, dest)
    base.verify_release(dest)
    db.close()
    print("Published:", dest, dict(coverage), flush=True)


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--data-root", type=Path, default=Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets"))))
    run(cli.parse_args().data_root.resolve())

"""Publish the all-earned-postsecondary education release re-extracted with multi_v04.

Every source biography is re-parsed with education_regex_v04. An award keeps its
published identifier and institution assignment from amws_education_postsecondary_20261019
when its evidence key, institution text, resolved year, family and eligibility are
unchanged. Every other included award is assigned from the published crosswalk by
exact institution text, as build_postsecondary_release.assign does. Awards the v04
rules no longer report are listed in removed_awards.csv. All eight family blocks
of the US person view are rewritten from the new ledger; every other person cell
must be unchanged. Offline: no web research, fuzzy matching or new coordinates.
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

import build_postsecondary_release as base
import education_regex_v04 as parser
import enrich_event_study_sources as enrichment
import export_amws_openalex as export
import postsecondary_qualifications as v1
import postsecondary_qualifications_v04 as scope

STAMP = "amws_education_postsecondary_v04_20261019"
BASELINE = "amws_education_postsecondary_20261019"
AWARD_COLUMN = re.compile(r"^(?:" + "|".join(scope.FAMILIES) + r")_award_\d+_")
ASSIGNMENT = ("oa_ids", "oa_names", "record_id", "assignment_class", "conditional_IDs", "condition_conflict",
              "fully_matched", "matched_components", "component_count", "IDs_before", "IDs_after", "additional_IDs",
              "matching_flags", "mapping_origin", "raw_mapping_status", "education_source", "matching_source",
              "decision_source", "oa_latitudes", "oa_longitudes", "oa_cities", "oa_countries")
PERSON_META = ("oa_ids", "oa_id_count", "oa_has_id", "oa_award_count", "oa_matched_award_count",
               "oa_multi_institution_award_count", "oa_export_version")
csv.field_size_limit(16 * 1024 * 1024)
rows, digest, dumps, table, write_json = base.rows, base.digest, base.dumps, base.table, base.write_json


def year_text(value):
    return "" if value in (None, "") else str(value)


def reuse_key(award):
    """Fields that must be unchanged for a published assignment to carry over."""
    return (award["institution_as_written"] or "", year_text(award["year"]), award["requested_family"],
            award["training_eligibility"])


def wide_values(award):
    ids, names = json.loads(award["oa_ids"]), json.loads(award["oa_names"])
    result = export.award_values(dict(award, oa_ids=ids, oa_names=names))
    prefix = f'{award["requested_family"]}_award_{award["family_award_index"]}_'
    for k in ("training_eligibility", "assignment_reused", "evidence_key"):
        result[prefix + k] = award[k]
    for i in range(len(ids)):
        for suffix, key in (("latitude", "oa_latitudes"), ("longitude", "oa_longitudes"), ("city", "oa_cities"), ("country", "oa_countries")):
            result[f"{prefix}institution_{i+1}_{suffix}"] = json.loads(award[key])[i]
    return result


def column_order(name):
    m = re.match(r"^(" + "|".join(scope.FAMILIES) + r")_award_(\d+)_(.*)$", name)
    return (scope.FAMILIES.index(m.group(1)), int(m.group(2)), m.group(3))


def reference_coordinates(root, mappings, rules):
    """The coordinate table build_postsecondary_release used, rebuilt the same way."""
    reference = {r["openalex_id"]: r for r in rows(root / "output/amws/amws_education_openalex_coordinates_20261014/institution_reference_coordinates.csv")}
    wanted = set()
    for m in mappings.values():
        wanted.update(m["existing"] + m["additional"])
        wanted.update(rules.valid_id(v["openalex_id"]) for v in m["matches"])
    for r in rows(root / "output/amws/education_geolocated/reference_institutions.csv"):
        if r["id"] in wanted and r["id"] not in reference and r["valid_coordinates"].lower() == "true":
            try:
                lat, lon = float(r["latitude"]), float(r["longitude"])
            except ValueError:
                continue
            if math.isfinite(lat) and math.isfinite(lon) and abs(lat) <= 90 and abs(lon) <= 180:
                reference[r["id"]] = dict(openalex_id=r["id"], oa_canonical_name=r["name"], reference_name=r["name"],
                    latitude=r["latitude"], longitude=r["longitude"], city=r["city"], country=r["country"],
                    coordinate_kind="openalex_reference_location", historical_campus_accuracy_verified="False")
    return reference


def run(root):
    dest = root / "output/amws" / STAMP
    if dest.exists():
        base.verify_release(dest)
        print("Verified existing v04 education release:", dest)
        return
    bios = root / "output/amws/education_enriched_20261011_updated_final/source_biographies.csv"
    baseline = root / "output/amws" / BASELINE
    canonical = root / "Data/processed/amws/amws_open_alex.csv"
    crosswalk_path = root / "output/amws/openalex_id_resolution_20261012/raw_entry_crosswalk.jsonl"
    rules, rules_path = base.load_condition_rules()
    here = Path(__file__)
    code = [here, Path(scope.__file__), Path(parser.__file__), Path(base.__file__), Path(v1.__file__), Path(export.__file__),
            Path(enrichment.__file__), here.with_name("test_postsecondary_qualifications_v04.py"), rules_path]
    inputs = [bios, baseline / "education_awards.csv", baseline / "completion_manifest.json", canonical, crosswalk_path,
              root / "output/amws/amws_education_openalex_coordinates_20261014/institution_reference_coordinates.csv",
              root / "output/amws/education_geolocated/reference_institutions.csv", *code]
    hashes = {str(p.resolve()): digest(p) for p in inputs}
    work = Path(os.environ.get("AMWS_CHECKPOINT_ROOT", str(Path.home() / ".codex/amws_checkpoints"))) / STAMP
    stage = work / "stage"
    if stage.exists():
        shutil.rmtree(stage)
    stage.mkdir(parents=True)
    db = sqlite3.connect(work / "awards.sqlite")
    db.executescript("DROP TABLE IF EXISTS baseline; DROP TABLE IF EXISTS expanded; DROP TABLE IF EXISTS person; "
        "CREATE TABLE baseline(source_key TEXT,label_start INTEGER,label_end INTEGER,label TEXT,award_index INTEGER,data TEXT,PRIMARY KEY(source_key,label_start,label_end,label)); "
        "CREATE TABLE expanded(source_key TEXT,award_index INTEGER,data TEXT,PRIMARY KEY(source_key,award_index)); "
        "CREATE TABLE person(source_key TEXT PRIMARY KEY,data TEXT);")
    print("Indexing the published 20261019 award ledger.", flush=True)
    baseline_fields = None
    for r in rows(baseline / "education_awards.csv"):
        baseline_fields = baseline_fields or list(r)
        db.execute("INSERT INTO baseline VALUES(?,?,?,?,?,?)", (*v1.fingerprint(r), int(r["award_index"]), dumps(r)))
    db.commit()
    mappings = base.load_crosswalk(crosswalk_path)
    reference = reference_coordinates(root, mappings, rules)
    fields = [f for f in baseline_fields if f != "legacy_award"] + ["assignment_reused"]
    af, aw = table(stage / "education_awards_openalex.csv", fields)
    lf, lw = table(stage / "education_awards.csv", fields)
    rf, rw = table(stage / "education_records.csv", ["source_key", "edition", "awards", "matched_awards", "reused_awards",
                                                     "reassigned_awards", "removed_awards", "heading_missing", "heading_recovered"])
    removed_fields = ["source_key", "edition", "award_index", "requested_family", "label_as_written", "institution_as_written",
                      "year", "training_eligibility", "fully_matched", "evidence", "now"]
    xf, xw = table(stage / "removed_awards.csv", removed_fields)
    changed_fields = ["source_key", "edition", "award_index", "label_as_written"] + [
        f"{s}_{k}" for s in ("old", "new") for k in ("requested_family", "institution_as_written", "year", "training_eligibility", "fully_matched", "evidence")]
    cf, cw = table(stage / "changed_awards.csv", changed_fields)
    research_stream = (stage / "institution_research_inventory.jsonl").open("w", encoding="utf-8")
    coverage = collections.Counter()
    max_awards, max_slots = collections.Counter(), collections.Counter()
    extra_fields, used_ids, source_keys = set(), set(), set()
    unresolved = collections.Counter()
    for bio in rows(bios):
        key, text, ed = bio["source_key"], bio["raw_text_adjusted"], int(bio["edition"])
        assert key not in source_keys
        source_keys.add(key)
        old = {tuple(r[:4]): json.loads(r[5]) for r in db.execute(
            "SELECT source_key,label_start,label_end,label,award_index,data FROM baseline WHERE source_key=?", (key,))}
        next_id = max((int(a["award_index"]) for a in old.values()), default=0)
        by = enrichment.integer(bio["birth_year_used"])
        primary, parsed = scope.parse(text, by, ed)
        region = parser.bounds(text, ed)[0]
        context = text[slice(*region)] if region else ""
        positions = collections.Counter()
        kept, n_included, n_matched, n_reused, n_reassigned = set(), 0, 0, 0, 0
        for d in parsed:
            award = dict(source_key=key, edition=str(ed), group=d["group"], label_as_written=d["label_as_written"],
                         institution_as_written=d["institution_as_written"] or "", year_raw=d["year_raw"] or "",
                         year=year_text(d["year"]), honorary=str(d["honorary"]), evidence=d["evidence"],
                         evidence_source=d["evidence_source"], start=str(d["start"]), end=str(d["end"]),
                         label_start=str(d["label_start"]), label_end=str(d["label_end"]), rule_id=d["rule_id"],
                         flags=dumps(d["flags"]), training_eligibility=d["training_eligibility"])
            assert text[d["start"]:d["end"]] == d["evidence"] and text[d["label_start"]:d["label_end"]] == d["label_as_written"]
            fp = v1.fingerprint(award)
            previous = old.get(fp)
            if previous is not None:
                award["award_index"] = previous["award_index"]
            else:
                next_id += 1
                award["award_index"] = str(next_id)
                coverage["new_evidence_awards"] += 1
            award["evidence_key"] = dumps(fp)
            if d["family"] is None:
                award.update(requested_family="", family_award_index="", assignment_reused="False")
                lw.writerow(award)
                coverage[f"excluded:{d['training_eligibility']}"] += 1
                continue
            positions[d["family"]] += 1
            award.update(requested_family=d["family"], family_award_index=str(positions[d["family"]]))
            if previous is not None and previous["requested_family"] and reuse_key(previous) == reuse_key(award):
                award.update({k: previous.get(k, "") for k in ASSIGNMENT}, assignment_reused="True")
                n_reused += 1
            else:
                award.update({k: "" for k in ASSIGNMENT})
                award.update(base.assign(award, mappings.get(award["institution_as_written"] or None), rules, reference, context),
                             assignment_reused="False", education_source=parser.RULE_VERSION,
                             matching_source="exact_raw_name_crosswalk")
                n_reassigned += 1
                if previous is not None and previous["requested_family"]:
                    cw.writerow(dict(source_key=key, edition=ed, award_index=award["award_index"], label_as_written=award["label_as_written"],
                        **{f"{s}_{k}": src.get(k, "") for s, src in (("old", previous), ("new", award))
                           for k in ("requested_family", "institution_as_written", "year", "training_eligibility", "fully_matched", "evidence")}))
            award["oa_export_version"] = scope.VERSION
            kept.add(fp)
            ids = json.loads(award["oa_ids"])
            assert set(ids) <= reference.keys(), (key, ids)
            used_ids.update(ids)
            n_included += 1
            n_matched += bool(ids)
            coverage[f"{ed}:{d['family']}:awards"] += 1
            coverage[f"{ed}:{d['family']}:matched"] += bool(ids)
            if award["fully_matched"].lower() != "true":
                unresolved[(award["institution_as_written"], award["training_eligibility"], d["family"])] += 1
                if v1.select_for_research(award):
                    research_stream.write(dumps({k: award[k] for k in ("source_key", "award_index", "requested_family", "institution_as_written", "year", "evidence", "evidence_key", "flags", "matching_flags")}) + "\n")
            max_awards[d["family"]] = max(max_awards[d["family"]], positions[d["family"]])
            max_slots[d["family"]] = max(max_slots[d["family"]], len(ids), 1)
            extra_fields.update(wide_values(award))
            aw.writerow(award)
            lw.writerow(award)
            db.execute("INSERT INTO expanded VALUES(?,?,?)", (key, int(award["award_index"]), dumps(award)))
        n_removed = 0
        for fp, previous in old.items():
            if previous["requested_family"] and fp not in kept:
                now = next((f"excluded:{d['training_eligibility']}" for d in parsed if (d["label_start"], d["label_end"], d["label_as_written"]) == fp[1:]), "absent")
                xw.writerow(dict({k: previous.get(k, "") for k in removed_fields}, now=now))
                n_removed += 1
        projected, _ = enrichment.project(primary, parser)
        db.execute("INSERT INTO person VALUES(?,?)", (key, dumps(dict(projected, reassigned_awards=n_reassigned))))
        rw.writerow(dict(source_key=key, edition=ed, awards=n_included, matched_awards=n_matched, reused_awards=n_reused,
                         reassigned_awards=n_reassigned, removed_awards=n_removed,
                         heading_missing="education_heading_not_found" in primary["flags"],
                         heading_recovered=any(f in primary["flags"] for f in ("education_heading_ocr", "education_heading_inferred"))))
        coverage["reused_awards"] += n_reused
        coverage["reassigned_awards"] += n_reassigned
        coverage["removed_awards"] += n_removed
        if len(source_keys) % 20000 == 0:
            print("Processed biographies:", len(source_keys), flush=True)
            db.commit()
    for f in (af, lf, rf, xf, cf, research_stream):
        f.close()
    db.commit()
    assert len(source_keys) == 156139
    print("Writing the US person view:", dict(coverage), flush=True)
    with canonical.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        original_fields = reader.fieldnames
        education = set(enrichment.NEW_COLUMNS) | set(PERSON_META) | {k for k in original_fields if AWARD_COLUMN.match(k)}
        kept_fields = [k for k in original_fields if not AWARD_COLUMN.match(k)]
        fields = kept_fields + sorted(extra_fields, key=column_order) + ["postsecondary_scope", "reassigned_awards"]
        of, ow = table(stage / "amws_open_alex.csv", fields)
        count = 0
        for r in reader:
            person = db.execute("SELECT data FROM person WHERE source_key=?", (r["source_key"],)).fetchone()
            assert person is not None, r["source_key"]
            person = json.loads(person[0])
            out = {k: r[k] for k in kept_fields}
            awards = [json.loads(a[0]) for a in db.execute("SELECT data FROM expanded WHERE source_key=? ORDER BY award_index", (r["source_key"],))]
            all_ids = set()
            for a in awards:
                out.update(wide_values(a))
                all_ids.update(json.loads(a["oa_ids"]))
            out.update({k: person[k] for k in enrichment.NEW_COLUMNS})
            out.update(oa_ids=dumps(sorted(all_ids)), oa_id_count=len(all_ids), oa_has_id=str(bool(all_ids)),
                oa_award_count=len(awards), oa_matched_award_count=sum(bool(json.loads(a["oa_ids"])) for a in awards),
                oa_multi_institution_award_count=sum(len(json.loads(a["oa_ids"])) > 1 for a in awards),
                oa_export_version=scope.VERSION, postsecondary_scope="all_earned_postsecondary_multi_v04",
                reassigned_awards=person["reassigned_awards"])
            assert all(out[k] == r[k] for k in kept_fields if k not in education), r["source_key"]
            ow.writerow(out)
            count += 1
        of.close()
    assert count == 136489
    coord_fields = list(next(rows(baseline / "institution_reference_coordinates.csv")))
    kf, kw = table(stage / "institution_reference_coordinates.csv", coord_fields)
    for entity in sorted(used_ids):
        kw.writerow(reference[entity])
    kf.close()
    uf, uw = table(stage / "unresolved_training_inventory.csv", ["institution_as_written", "eligibility", "family", "awards"])
    for (name, eligibility, family), n in sorted(unresolved.items()):
        uw.writerow(dict(institution_as_written=name, eligibility=eligibility, family=family, awards=n))
    uf.close()
    vf, vw = table(stage / "coverage_by_edition_family.csv", ["edition", "family", "metric", "count"])
    for k, n in coverage.items():
        bits = k.split(":")
        if len(bits) == 3:
            vw.writerow(dict(edition=bits[0], family=bits[1], metric=bits[2], count=n))
    vf.close()
    schema = dict(version=scope.VERSION, parser=parser.RULE_VERSION, families=list(scope.FAMILIES), maximum_awards=dict(max_awards),
                  maximum_institution_slots=dict(max_slots), baseline_release=BASELINE,
                  reused_awards=coverage["reused_awards"], reassigned_awards=coverage["reassigned_awards"],
                  removed_awards=coverage["removed_awards"], board_certifications_excluded=True,
                  generic_credentials_require_postsecondary_evidence=True, headings_repaired=True)
    write_json(stage / "schema.json", schema)
    write_json(stage / "coverage_report.json", dict(source_biographies=len(source_keys), canonical_entries=count,
        counters=dict(coverage), used_institutions=len(used_ids)))
    write_json(stage / "amws_open_alex_manifest.json", dict(rows=count, sha256=digest(stage / "amws_open_alex.csv"), schema="schema.json"))
    (stage / "DATA_DICTIONARY.txt").write_text(
        "All-earned-postsecondary scope re-extracted with the multi_v04 parser (education_regex_v04.py); the frozen multi_v03 releases are unchanged.\n"
        "Families: ba_bs, ma, ms, phd, bachelors_other, masters_other, doctorate_other, postsecondary_credential. Family indices follow biography order.\n"
        "Specialty-board and society diplomas/certificates are excluded as licences (training_eligibility=board_certification), like titles and honorary awards.\n"
        "An award keeps its 20261019 award_index and assignment (assignment_reused=True) when evidence key, institution text, year, family and eligibility are unchanged.\n"
        "Other included awards are assigned from the published crosswalk by exact raw institution name; no web research, fuzzy matching or new coordinates.\n"
        "removed_awards.csv lists 20261019 awards that multi_v04 no longer reports; changed_awards.csv lists awards whose text, year, family or eligibility changed.\n"
        "amws_open_alex.csv keeps the canonical US person view's rows, order and non-education cells; its eight family blocks, education lists/status and oa_* summaries come from multi_v04.\n",
        encoding="utf-8")
    snap = stage / "code_snapshot"
    snap.mkdir(exist_ok=True)
    for p in code:
        shutil.copy2(p, snap / p.name)
    for p, checksum in hashes.items():
        assert digest(p) == checksum, p
    checksums = {str(p.relative_to(stage)): digest(p) for p in stage.rglob("*") if p.is_file() and p.name != "completion_manifest.json"}
    manifest = dict(status="complete", input_checksums=hashes, output_checksums=checksums, canonical_entries=count, schema=schema)
    write_json(stage / "completion_manifest.json", manifest)
    shutil.copytree(stage, dest)
    base.verify_release(dest)
    db.close()
    print("Published:", dest, flush=True)


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--data-root", type=Path, default=Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets"))))
    run(cli.parse_args().data_root.resolve())

"""Publish additive postsecondary awards and the US education view, offline."""
import argparse
import collections
import csv
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import shutil
import sqlite3
import sys

import postsecondary_qualifications as scope
import education_regex as legacy
import export_amws_openalex as export

STAMP = "amws_education_postsecondary_20261019"
csv.field_size_limit(16 * 1024 * 1024)


def rows(path):
    with Path(path).open(encoding="utf-8-sig", newline="") as f:
        yield from csv.DictReader(f)


def digest(path):
    with Path(path).open("rb") as f:
        return hashlib.file_digest(f, "sha256").hexdigest()


def write_json(path, value):
    Path(path).write_text(json.dumps(value, ensure_ascii=False, indent=2), encoding="utf-8")


def dumps(value):
    return json.dumps(value, ensure_ascii=False, separators=(",", ":"))


def table(path, fields):
    f = Path(path).open("w", encoding="utf-8-sig", newline="")
    writer = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
    writer.writeheader()
    return f, writer


def verify_release(path):
    m = json.loads((path / "completion_manifest.json").read_text(encoding="utf-8"))
    for relative, checksum in m["output_checksums"].items():
        assert digest(path / relative) == checksum, relative
    for p, checksum in m["input_checksums"].items():
        assert digest(p) == checksum, p
    return m


def load_condition_rules():
    path = Path.home() / ".codex/skills/recover-openalex-institution-id/scripts/research_batches.py"
    spec = importlib.util.spec_from_file_location("postsecondary_condition_rules", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module, path


def load_crosswalk(path):
    out = {}
    with path.open(encoding="utf-8-sig") as f:
        for line in f:
            r = json.loads(line)
            result = r.get("resolution_result") or {}
            matches = result.get("matches") or r.get("conditional_resolution_matches") or []
            out[r["institution_as_written"]] = dict(
                record_id=r["record_id"], status=r["status"], origin=r["origin"],
                existing=r.get("existing_unconditional_or_provisional_IDs", []),
                additional=r.get("additional_unconditional_provisional_IDs", []),
                matches=matches, explicit=result.get("explicit_co_named_institutions", False),
                component_count=result.get("component_count") or len(r.get("components", [])) or 1)
    return out


def assign(award, mapping, rules, reference, context):
    empty = dict(oa_ids="[]", oa_names="[]", conditional_IDs="[]", assignment_class="[]",
                 matching_flags="[]", condition_conflict="False", fully_matched="False",
                 oa_latitudes="[]", oa_longitudes="[]", oa_cities="[]", oa_countries="[]",
                 record_id="", mapping_origin="", raw_mapping_status="unmatched")
    if award["training_eligibility"] != "eligible" or mapping is None:
        if award["training_eligibility"] != "eligible":
            empty["matching_flags"] = dumps([award["training_eligibility"]])
        return empty
    selected = set(mapping["existing"]) | set(mapping["additional"])
    conditional, components = set(), collections.defaultdict(set)
    evaluation = dict(award, education_context=context)
    for match in mapping["matches"]:
        cond = match.get("conditions")
        if cond is None or rules.condition_applies(cond, evaluation):
            entity = rules.valid_id(match["openalex_id"])
            components[match.get("component_index", 0)].add(entity)
            if cond is not None:
                conditional.add(entity)
    conflict = any(len(ids) > 1 for ids in components.values()) or (len(components) > 1 and not mapping["explicit"])
    if not conflict:
        selected.update(set().union(*components.values()) if components else set())
    else:
        conditional.clear()
    missing_coordinates = selected - reference.keys()
    located = sorted(selected - missing_coordinates)
    base_complete = mapping["status"] in {"fully_provisional_matched", "reviewed_match", "resolved"}
    full = bool(selected) and not conflict and not missing_coordinates and (
        base_complete or len(components) >= mapping["component_count"])
    flags = (["reference_coordinates_unavailable"] if missing_coordinates else [])
    if conflict:
        flags.append("conditional_identity_conflict")
    return dict(empty, oa_ids=dumps(located), oa_names=dumps([reference[i]["oa_canonical_name"] for i in located]),
        conditional_IDs=dumps(sorted(conditional & set(located))), assignment_class=dumps(["reused_exact_documented_mapping"]),
        matching_flags=dumps(flags), condition_conflict=str(conflict), fully_matched=str(full),
        oa_latitudes=dumps([float(reference[i]["latitude"]) for i in located]),
        oa_longitudes=dumps([float(reference[i]["longitude"]) for i in located]),
        oa_cities=dumps([reference[i]["city"] for i in located]), oa_countries=dumps([reference[i]["country"] for i in located]),
        record_id=mapping["record_id"], mapping_origin=mapping["origin"], raw_mapping_status=mapping["status"])


def wide_values(award):
    # Keep the established award suffixes and any number of institution slots.
    ids = json.loads(award["oa_ids"])
    names = json.loads(award["oa_names"])
    converted = dict(award, oa_ids=ids, oa_names=names)
    converted.setdefault("flags", "[]")
    result = export.award_values(converted)
    prefix = f'{award["requested_family"]}_award_{award["family_award_index"]}_'
    for k in ("training_eligibility", "legacy_award", "evidence_key"):
        result[prefix + k] = award[k]
    for i in range(len(ids)):
        for suffix, key in (("latitude", "oa_latitudes"), ("longitude", "oa_longitudes"), ("city", "oa_cities"), ("country", "oa_countries")):
            result[f"{prefix}institution_{i+1}_{suffix}"] = json.loads(award[key])[i]
    return result


def run(root):
    dest = root / "output/amws" / STAMP
    if dest.exists():
        verify_release(dest)
        print("Verified existing expanded education release:", dest)
        return
    enriched = root / "output/amws/education_enriched_20261011_updated_final"
    old = root / "output/amws/amws_education_openalex_coordinates_20261014"
    canonical = root / "Data/processed/amws/amws_open_alex.csv"
    crosswalk_path = root / "output/amws/openalex_id_resolution_20261012/raw_entry_crosswalk.jsonl"
    rules, rules_path = load_condition_rules()
    code = [Path(__file__), Path(scope.__file__), Path(legacy.__file__), Path(export.__file__), Path(scope.enrichment.__file__),
            Path(__file__).with_name("test_postsecondary_qualifications.py"), rules_path]
    inputs = [enriched / "education_awards.csv", enriched / "source_biographies.csv", old / "education_awards_openalex.csv",
              old / "institution_reference_coordinates.csv", canonical, crosswalk_path,
              root / "output/amws/education_geolocated/reference_institutions.csv", *code]
    hashes = {str(p.resolve()): digest(p) for p in inputs}
    work = Path(os.environ.get("AMWS_CHECKPOINT_ROOT", str(Path.home() / ".codex/amws_checkpoints"))) / STAMP
    stage = work / "stage"
    stage.mkdir(parents=True, exist_ok=True)
    db = sqlite3.connect(work / "awards.sqlite")
    db.executescript("DROP TABLE IF EXISTS original; DROP TABLE IF EXISTS old; DROP TABLE IF EXISTS expanded; "
        "CREATE TABLE original(source_key TEXT,award_index INTEGER,data TEXT,PRIMARY KEY(source_key,award_index)); "
        "CREATE TABLE old(source_key TEXT,label_start INTEGER,label_end INTEGER,label TEXT,data TEXT,PRIMARY KEY(source_key,label_start,label_end,label)); "
        "CREATE TABLE expanded(source_key TEXT,award_index INTEGER,data TEXT,PRIMARY KEY(source_key,award_index));")
    print("Indexing frozen evidence and old assignments.", flush=True)
    for r in rows(inputs[0]):
        db.execute("INSERT INTO original VALUES(?,?,?)", (r["source_key"], int(r["award_index"]), dumps(r)))
    old_columns = None
    for r in rows(inputs[2]):
        old_columns = list(r)
        db.execute("INSERT INTO old VALUES(?,?,?,?,?)", (*scope.fingerprint(r), dumps(r)))
    db.commit()
    mappings = load_crosswalk(crosswalk_path)
    reference = {r["openalex_id"]: r for r in rows(old / "institution_reference_coordinates.csv")}
    wanted = set()
    for m in mappings.values():
        wanted.update(m["existing"] + m["additional"])
        wanted.update(rules.valid_id(v["openalex_id"]) for v in m["matches"])
    for r in rows(inputs[6]):
        if r["id"] in wanted and r["id"] not in reference and r["valid_coordinates"].lower() == "true":
            try:
                lat, lon = float(r["latitude"]), float(r["longitude"])
            except ValueError:
                continue
            if math.isfinite(lat) and math.isfinite(lon) and abs(lat) <= 90 and abs(lon) <= 180:
                reference[r["id"]] = dict(openalex_id=r["id"], oa_canonical_name=r["name"], reference_name=r["name"],
                    latitude=r["latitude"], longitude=r["longitude"], city=r["city"], country=r["country"],
                    coordinate_kind="openalex_reference_location", historical_campus_accuracy_verified="False")
    fields = list(dict.fromkeys(old_columns + ["training_eligibility", "legacy_award", "evidence_key"]))
    af, aw = table(stage / "education_awards_openalex.csv", fields)
    research_stream = (stage / "institution_research_inventory.jsonl").open("w", encoding="utf-8")
    lf, lw = table(stage / "education_awards.csv", fields)
    rf, rw = table(stage / "education_records.csv", ["source_key", "edition", "awards", "matched_awards", "additional_awards", "heading_missing"])
    coverage = collections.Counter()
    max_awards, max_slots = collections.Counter(), collections.Counter()
    extra_fields, used_ids = set(), set()
    unresolved = collections.Counter()
    source_keys = set()
    old_preserved = 0
    for bio in rows(enriched / "source_biographies.csv"):
        key, text, ed = bio["source_key"], bio["raw_text_adjusted"], int(bio["edition"])
        assert key not in source_keys
        source_keys.add(key)
        baseline = [json.loads(r[0]) for r in db.execute("SELECT data FROM original WHERE source_key=? ORDER BY award_index", (key,))]
        existing_spans = {(int(r["label_start"]), int(r["label_end"])) for r in baseline}
        next_id = max((int(r["award_index"]) for r in baseline), default=0)
        by = int(bio["birth_year_used"]) if bio["birth_year_used"] else None
        for new in scope.extra_awards(text, by, ed):
            if (new["label_start"], new["label_end"]) in existing_spans:
                continue
            next_id += 1
            baseline.append(dict(source_key=key, edition=str(ed), award_index=str(next_id), requested_family="", family_award_index="", **new))
            coverage["new_label_awards"] += 1
        family_positions = collections.Counter()
        original_primary = {int(r["award_index"]) for r in baseline if r.get("requested_family") in scope.FAMILIES[:4]
                            and r.get("family_award_index") and str(r["honorary"]).lower() == "false"}
        for a in baseline:
            if int(a["award_index"]) in original_primary:
                family_positions[a["requested_family"]] = max(family_positions[a["requested_family"]], int(a["family_award_index"]))
        total, matched, additional = 0, 0, 0
        region, boundary_flags = legacy.bounds(text, ed)
        context = text[slice(*region)] if region else ""
        # New-family positions follow biography order; existing primary positions stay exact.
        baseline.sort(key=lambda r: (int(r["label_start"]), int(r["award_index"])))
        for award in baseline:
            family, eligibility = scope.qualification(award)
            award["training_eligibility"] = eligibility
            award["legacy_award"] = str(int(award["award_index"]) in original_primary)
            award["evidence_key"] = dumps(scope.fingerprint(award))
            assert text[int(award["start"]):int(award["end"])] == award["evidence"]
            assert text[int(award["label_start"]):int(award["label_end"]) ] == award["label_as_written"]
            if isinstance(award.get("flags"), list):
                award["flags"] = dumps(award["flags"])
            if family is None:
                award["requested_family"] = ""
                lw.writerow(award)
                coverage[f"excluded:{eligibility}"] += 1
                continue
            was_legacy = award["legacy_award"] == "True"
            if not was_legacy:
                family_positions[family] += 1
                award["family_award_index"] = str(family_positions[family])
                award["requested_family"] = family
            old_match = db.execute("SELECT data FROM old WHERE source_key=? AND label_start=? AND label_end=? AND label=?", scope.fingerprint(award)).fetchone()
            if was_legacy:
                assert old_match is not None, scope.fingerprint(award)
                original = json.loads(old_match[0])
                for k in ("institution_as_written", "year", "label_as_written", "family_award_index", "award_index", "evidence"):
                    assert str(award[k] or "") == str(original[k] or ""), (key, k)
                # All old export values are retained verbatim, including conditional/provisional decisions.
                added = {k: award[k] for k in ("training_eligibility", "legacy_award", "evidence_key")}
                award = dict(original, **added)
                old_preserved += 1
            else:
                award.update(assign(award, mappings.get(award.get("institution_as_written")), rules, reference, context))
                additional += 1
            ids = json.loads(award["oa_ids"])
            used_ids.update(ids)
            total += 1
            matched += bool(ids)
            coverage[f"{ed}:{family}:awards"] += 1
            coverage[f"{ed}:{family}:matched"] += bool(ids)
            if award["fully_matched"].lower() != "true":
                unresolved[(award.get("institution_as_written") or "", eligibility, family)] += 1
                if scope.select_for_research(award):
                    research_stream.write(dumps({k: award[k] for k in ("source_key", "award_index", "requested_family", "institution_as_written", "year", "evidence", "evidence_key", "flags", "matching_flags")}) + "\n")
            max_awards[family] = max(max_awards[family], int(award["family_award_index"]))
            max_slots[family] = max(max_slots[family], len(ids), 1)
            extra_fields.update(wide_values(award))
            aw.writerow(award)
            lw.writerow(award)
            db.execute("INSERT INTO expanded VALUES(?,?,?)", (key, int(award["award_index"]), dumps(award)))
        rw.writerow(dict(source_key=key, edition=ed, awards=total, matched_awards=matched, additional_awards=additional,
                         heading_missing="education_heading_not_found" in boundary_flags))
        if len(source_keys) % 10000 == 0:
            print("Processed biographies:", len(source_keys), flush=True)
            db.commit()
    for f in (af, lf, rf, research_stream):
        f.close()
    db.commit()
    expected_preserved = db.execute("SELECT COUNT(*) FROM old").fetchone()[0]
    assert old_preserved == expected_preserved
    print("Preserved old awards:", old_preserved, "; exporting expanded US view.", flush=True)
    with canonical.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        original_fields = reader.fieldnames
        fields = original_fields + sorted(extra_fields - set(original_fields)) + ["postsecondary_scope", "postsecondary_additional_awards"]
        of, ow = table(stage / "amws_open_alex.csv", fields)
        count = 0
        changed_meta = ("oa_ids", "oa_id_count", "oa_has_id", "oa_award_count", "oa_matched_award_count", "oa_multi_institution_award_count", "oa_export_version")
        for r in reader:
            base = dict(r)
            awards = [json.loads(a[0]) for a in db.execute("SELECT data FROM expanded WHERE source_key=? ORDER BY award_index", (r["source_key"],))]
            all_ids = set()
            for a in awards:
                if a["legacy_award"] != "True":
                    r.update(wide_values(a))
                all_ids.update(json.loads(a["oa_ids"]))
            r.update(oa_ids=dumps(sorted(all_ids)), oa_id_count=len(all_ids), oa_has_id=str(bool(all_ids)),
                oa_award_count=len(awards), oa_matched_award_count=sum(bool(json.loads(a["oa_ids"])) for a in awards),
                oa_multi_institution_award_count=sum(len(json.loads(a["oa_ids"])) > 1 for a in awards),
                oa_export_version=scope.VERSION, postsecondary_scope="all_earned_postsecondary",
                postsecondary_additional_awards=sum(a["legacy_award"] != "True" for a in awards))
            assert all(r[k] == base[k] for k in original_fields if k not in changed_meta)
            ow.writerow(r)
            count += 1
        of.close()
    assert count == 136489
    coord_fields = list(next(rows(old / "institution_reference_coordinates.csv")))
    cf, cw = table(stage / "institution_reference_coordinates.csv", coord_fields)
    for entity in sorted(used_ids):
        cw.writerow(reference[entity])
    cf.close()
    uf, uw = table(stage / "unresolved_training_inventory.csv", ["institution_as_written", "eligibility", "family", "awards"])
    for (name, eligibility, family), n in sorted(unresolved.items()):
        uw.writerow(dict(institution_as_written=name, eligibility=eligibility, family=family, awards=n))
    uf.close()
    coverage_rows = []
    for k, n in coverage.items():
        bits = k.split(":")
        if len(bits) == 3:
            coverage_rows.append(dict(edition=bits[0], family=bits[1], metric=bits[2], count=n))
    vf, vw = table(stage / "coverage_by_edition_family.csv", ["edition", "family", "metric", "count"])
    vw.writerows(coverage_rows)
    vf.close()
    schema = dict(version=scope.VERSION, families=list(scope.FAMILIES), maximum_awards=dict(max_awards),
                  maximum_institution_slots=dict(max_slots), original_awards_preserved=old_preserved,
                  generic_credentials_require_postsecondary_evidence=True, headings_repaired=False)
    write_json(stage / "schema.json", schema)
    write_json(stage / "coverage_report.json", dict(source_biographies=len(source_keys), canonical_entries=count,
        old_awards_preserved=old_preserved, counters=dict(coverage), used_institutions=len(used_ids)))
    write_json(stage / "amws_open_alex_manifest.json", dict(rows=count, sha256=digest(stage / "amws_open_alex.csv"), schema="schema.json"))
    (stage / "DATA_DICTIONARY.txt").write_text(
        "Additive all-earned-postsecondary scope. Existing four-family award columns and source fields are unchanged.\n"
        "New families: bachelors_other, masters_other, doctorate_other, postsecondary_credential.\n"
        "Uncertain credential levels stay unresolved and cannot prove local or never-local training. Honorary awards, titles and known school-level credentials are excluded.\n"
        "Existing institution decisions and conditions are reused by exact raw institution name; no web research or fuzzy matching. Missing coordinates remain unmatched.\n"
        "New-label awards are appended after existing award IDs; original label offsets are stable evidence keys. Family indices follow biography order.\n"
        "No heading repairs, new person linkage or geographic corrections. Global awards cover the original biography pool; amws_open_alex.csv preserves the existing US person view and its row order.\n"
        "oa_* person metadata now summarizes all included qualifications; original four-family columns and legacy education_status remain unchanged.\n", encoding="utf-8")
    snap = stage / "code_snapshot"
    snap.mkdir(exist_ok=True)
    for p in code:
        shutil.copy2(p, snap / p.name)
    for p, checksum in hashes.items():
        assert digest(p) == checksum
    checksums = {str(p.relative_to(stage)): digest(p) for p in stage.rglob("*") if p.is_file() and p.name != "completion_manifest.json"}
    manifest = dict(status="complete", input_checksums=hashes, output_checksums=checksums,
                    old_awards_preserved=old_preserved, canonical_entries=count, schema=schema)
    write_json(stage / "completion_manifest.json", manifest)
    shutil.copytree(stage, dest)
    verify_release(dest)
    db.close()
    print("Published:", dest, flush=True)


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--data-root", type=Path, default=Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets"))))
    run(cli.parse_args().data_root.resolve())

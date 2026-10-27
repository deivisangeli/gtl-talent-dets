"""Independently validate the v05 AMWS education release against v04 and its manual inputs.

The manual assignment is re-derived here from the crosswalk, the overrides and the
snapshot coordinates without importing the v05 builder's assignment code. Checks:

1. both releases verify against their manifests;
2. every v05 award is the v04 award at the same position, identical outside the
   assignment fields; awards v05 may not assign are identical everywhere;
3. each assigned award carries old IDs plus the override (or else crosswalk) ID, with
   names and coordinates from the reference, fully_matched=True and the right source;
4. no award loses an ID or a full match;
5. the coordinate table agrees with the snapshot for every new ID and with v04 otherwise;
6. every person row: non-award cells as in v04, award cells and oa_* summaries rebuilt
   from the v05 ledger, physical-science cells equal to the field release (1986 only);
7. the unmatched store, the unresolved inventory and education_records reconcile with the ledger.
"""
import argparse
import collections
import csv
import json
import math
import os
from pathlib import Path
import sqlite3
import sys

import build_postsecondary_release as base
import build_postsecondary_release_v04 as v04

STAMP = "amws_education_postsecondary_v05_20261020"
MANUAL = "manual_institution_matches_top500_20261019"
csv.field_size_limit(16 * 1024 * 1024)
rows, dumps = base.rows, base.dumps
NEW_PERSON = ("manual_assigned_awards", "physical_science", "physical_science_unresolved", "physical_science_mixed")


def check(condition, message, failures):
    if not condition:
        failures.append(message)
        if len(failures) > 50:
            raise SystemExit("Too many failures:\n" + "\n".join(failures))


def run(root):
    failures = []
    new = root / "output/amws" / STAMP
    old = root / "output/amws" / v04.STAMP
    manual = root / "output/amws" / MANUAL
    base.verify_release(new)
    base.verify_release(old)

    crosswalk = {r["institution_as_written"]: r for r in rows(manual / "top500_manual_matches.csv")}
    overrides = {(r["source_key"], r["award_index"]): r for r in rows(manual / "medical_campus_overrides.csv")}
    snapshot = {}
    wanted = {r["openalex_id"] for r in crosswalk.values() if r["openalex_id"]} | {r["override_id"] for r in overrides.values()}
    for r in rows(root / "output/amws/education_geolocated/reference_institutions.csv"):
        if r["id"] in wanted:
            snapshot[r["id"]] = r
    check(wanted <= snapshot.keys(), "manual IDs missing from the snapshot", failures)
    published = {r["openalex_id"]: r for r in rows(old / "institution_reference_coordinates.csv")}

    def ref(entity):
        if entity in published:
            r = published[entity]
            return r["oa_canonical_name"], float(r["latitude"]), float(r["longitude"]), r["city"], r["country"]
        r = snapshot[entity]
        return r["name"], float(r["latitude"]), float(r["longitude"]), r["city"], r["country"]

    assignment = set(v04.ASSIGNMENT) | {"manual_assignment", "manual_confidence"}
    counts = collections.Counter()
    unmatched_expected, unresolved = {}, collections.Counter()
    per_person = collections.defaultdict(collections.Counter)
    work = Path(os.environ.get("AMWS_CHECKPOINT_ROOT", str(Path.home() / ".codex/amws_checkpoints"))) / (STAMP + "_validation")
    work.mkdir(parents=True, exist_ok=True)
    db = sqlite3.connect(work / "ledger.sqlite")
    db.executescript("DROP TABLE IF EXISTS expanded; CREATE TABLE expanded(source_key TEXT,award_index INTEGER,data TEXT);")
    for a, b in zip(rows(old / "education_awards.csv"), rows(new / "education_awards.csv"), strict=True):
        key = (a["source_key"], a["award_index"])
        check(key == (b["source_key"], b["award_index"]), f"ledger order differs at {key}", failures)
        check(all(a[k] == b[k] for k in a if k not in assignment), f"non-assignment field changed {key}", failures)
        may = bool(a["requested_family"]) and a["training_eligibility"] == "eligible" and a["fully_matched"] != "True"
        target = None
        if may and key in overrides:
            target, how = overrides[key]["override_id"], "override"
        elif may and crosswalk.get(a["institution_as_written"], {}).get("openalex_id"):
            target, how = crosswalk[a["institution_as_written"]]["openalex_id"], "crosswalk"
        if target is None:
            check(all(a[k] == b[k] for k in v04.ASSIGNMENT) and b["manual_assignment"] == b["manual_confidence"] == "",
                  f"award changed without a manual reason {key}", failures)
        else:
            ids = sorted(set(json.loads(a["oa_ids"])) | {target})
            names, lats, lons, cities, countries = zip(*[ref(i) for i in ids])
            check(json.loads(b["oa_ids"]) == ids, f"wrong IDs {key}", failures)
            check(json.loads(b["oa_names"]) == list(names) and json.loads(b["oa_cities"]) == list(cities)
                  and json.loads(b["oa_countries"]) == list(countries), f"wrong names/places {key}", failures)
            check(all(math.isclose(x, y) for x, y in zip(json.loads(b["oa_latitudes"]) + json.loads(b["oa_longitudes"]), lats + lons)),
                  f"wrong coordinates {key}", failures)
            check(b["fully_matched"] == "True" and b["condition_conflict"] == "False" and b["manual_assignment"] == how,
                  f"wrong match status {key}", failures)
            counts[f"manual_{how}"] += 1
        check(set(json.loads(a["oa_ids"] or "[]")) <= set(json.loads(b["oa_ids"] or "[]")), f"ID lost {key}", failures)
        check(not (a["fully_matched"] == "True" and b["fully_matched"] != "True"), f"full match lost {key}", failures)
        if b["requested_family"]:
            per_person[b["source_key"]]["awards"] += 1
            per_person[b["source_key"]]["matched"] += bool(json.loads(b["oa_ids"]))
            per_person[b["source_key"]]["manual"] += bool(b["manual_assignment"])
            db.execute("INSERT INTO expanded VALUES(?,?,?)", (b["source_key"], int(b["award_index"]), dumps(b)))
            if b["fully_matched"] != "True":
                unresolved[(b["institution_as_written"], b["training_eligibility"], b["requested_family"])] += 1
                if b["training_eligibility"] != "eligible":
                    cat = b["training_eligibility"]
                elif not b["institution_as_written"].strip():
                    cat = "no_institution_text"
                elif json.loads(b["oa_ids"]):
                    cat = "partially_matched"
                elif b["institution_as_written"] in crosswalk:
                    cat = crosswalk[b["institution_as_written"]]["confidence"]
                else:
                    cat = "outside_top500"
                unmatched_expected[key] = cat
    db.execute("CREATE INDEX person ON expanded(source_key, award_index)")
    db.commit()
    check(counts["manual_override"] == len(overrides), "not every override was applied", failures)

    coords = {r["openalex_id"]: r for r in rows(new / "institution_reference_coordinates.csv")}
    for entity, r in coords.items():
        name, lat, lon, city, country = ref(entity)
        check(r["oa_canonical_name"] == name and math.isclose(float(r["latitude"]), lat) and math.isclose(float(r["longitude"]), lon)
              and r["city"] == city and r["country"] == country, f"coordinate row differs {entity}", failures)
        if entity not in published:
            check(r["reference_updated_date"] and r["reference_source_file"] and r["reference_sha256"], f"provenance missing {entity}", failures)

    physical = {r["source_key"]: r for r in rows(root / "output/amws/physical_science_dummy_20261020/amws_ed16_physical_science_audit.csv")}
    n_people = 0
    with (old / "amws_open_alex.csv").open(encoding="utf-8-sig", newline="") as f_old, \
         (new / "amws_open_alex.csv").open(encoding="utf-8-sig", newline="") as f_new:
        for a, b in zip(csv.DictReader(f_old), csv.DictReader(f_new), strict=True):
            key = a["source_key"]
            check(key == b["source_key"], f"person order differs at {key}", failures)
            for k in a:
                if not v04.AWARD_COLUMN.match(k) and k not in v04.PERSON_META:
                    check(a[k] == b[k], f"person cell changed {key} {k}", failures)
            awards = [json.loads(x[0]) for x in db.execute("SELECT data FROM expanded WHERE source_key=? ORDER BY award_index", (key,))]
            wide = {}
            for w in awards:
                wide.update(v04.wide_values(w))
            for k in b:
                if v04.AWARD_COLUMN.match(k):
                    check(b[k] == str(wide.get(k, "")), f"award cell differs {key} {k}", failures)
            ids = set().union(*[set(json.loads(w["oa_ids"])) for w in awards]) if awards else set()
            check(b["oa_ids"] == dumps(sorted(ids)) and b["oa_id_count"] == str(len(ids)) and b["oa_award_count"] == str(len(awards))
                  and b["oa_matched_award_count"] == str(sum(bool(json.loads(w["oa_ids"])) for w in awards)), f"summary differs {key}", failures)
            check(b["manual_assigned_awards"] == str(per_person[key]["manual"]), f"manual count differs {key}", failures)
            p = physical.get(key)
            if key.startswith("1986:"):
                check(p is not None and (b["physical_science"], b["physical_science_unresolved"], b["physical_science_mixed"])
                      == (p["physical_science"], p["unresolved"], p["mixed"]), f"physical science differs {key}", failures)
                counts["physical_science_rows"] += 1
                counts["physical_science_ones"] += b["physical_science"] == "1"
            else:
                check(b["physical_science"] == b["physical_science_unresolved"] == b["physical_science_mixed"] == "", f"field set outside 1986 {key}", failures)
            n_people += 1
    check(n_people == 136489, "person row count", failures)
    check(counts["physical_science_rows"] == len(physical), "physical-science rows missing", failures)

    stored = {(r["source_key"], r["award_index"]): r["unmatched_category"] for r in rows(new / "unmatched_awards.csv")}
    check(stored == unmatched_expected, f"unmatched store differs ({len(stored)} vs {len(unmatched_expected)})", failures)
    strings = collections.Counter()
    for r in rows(new / "unmatched_strings.csv"):
        strings[r["institution_as_written"]] += int(r["awards"])
    check(sum(strings.values()) == len(unmatched_expected), "unmatched strings do not sum to the awards", failures)
    inventory = {(r["institution_as_written"], r["eligibility"], r["family"]): int(r["awards"]) for r in rows(new / "unresolved_training_inventory.csv")}
    check(inventory == dict(unresolved), "unresolved inventory differs", failures)
    for r in rows(new / "education_records.csv"):
        p = per_person[r["source_key"]]
        check((int(r["awards"]), int(r["matched_awards"]), int(r["manual_awards"])) == (p["awards"], p["matched"], p["manual"]),
              f"education record differs {r['source_key']}", failures)
    for name in ("removed_awards.csv", "changed_awards.csv"):
        check(sum(1 for _ in rows(new / name)) == 0, f"{name} should be empty", failures)
    db.close()

    report = dict(status="pass" if not failures else "fail", release=str(new), failures=failures, counts=dict(counts),
                  unmatched_awards=len(unmatched_expected), unmatched_by_category=dict(collections.Counter(unmatched_expected.values())),
                  people=n_people)
    out = root / "output/amws" / (STAMP + "_validation")
    out.mkdir(exist_ok=True)
    base.write_json(out / "independent_validation.json", report)
    print(json.dumps({k: v for k, v in report.items() if k != "failures"}, indent=1), "\nfailures:", len(failures))
    if failures:
        print("\n".join(failures[:20]))
        sys.exit(1)


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--data-root", type=Path, default=Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets"))))
    run(cli.parse_args().data_root.resolve())

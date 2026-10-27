"""Publish the v06 AMWS education release: v05 with duplicate entries removed.

Some AMWS entries were captured twice: a block of the 1955 source repeated 2,401
lines later, 1986 entries read twice around page and column breaks (often with one
copy truncated), and re-entered 1938 pages. The copies share name, birthplace, birth
date and degrees; they differ only in OCR noise or truncation.

Within each edition, person-view entries are blocked on the first 60 characters of
their normalised biography text (lowercase letters and digits) and linked when the
texts are identical, one is a prefix of the other, their first 400 characters are at
least 85% similar, or the pair is marked `duplicate` in the reviewed borderline file.
Linked entries form groups; one entry per group is kept:
  1. in 1906, 1938 and 1955, the member with kept=TRUE (the cross-edition flag);
  2. then the longest text (the complete copy rather than a truncated one);
  3. then the lowest line id.
Kept rows are byte-identical to v05; every other row of every v05 file is too. Removed
entries are listed, with their keeper, in duplicates_removed.csv.
"""
import argparse
import collections
import csv
from difflib import SequenceMatcher
import json
import os
from pathlib import Path
import re
import shutil
import sys

import build_postsecondary_release as base
import build_postsecondary_release_v05 as v05

STAMP = "amws_education_postsecondary_v06_20261021"
BASELINE = v05.STAMP
REVIEW = "amws_dedup_review_20261021"
BIOGRAPHIES = "education_enriched_20261011_updated_final/source_biographies.csv"
VERSION = "postsecondary_v06"
BLOCK_CHARS, COMPARE_CHARS, SIMILARITY, MIN_CHARS = 60, 400, 0.85, 60
KEPT_FLAG_EDITIONS = {"1906", "1938", "1955"}
csv.field_size_limit(16 * 1024 * 1024)
rows, digest, dumps, table, write_json = base.rows, base.digest, base.dumps, base.table, base.write_json


def normalise(text):
    return re.sub(r"[^a-z0-9]", "", text.lower())


def line_id(key):
    return int(key.split(":")[1])


def link_kind(a, b, reviewed=False):
    """How two normalised texts from the same block are linked, or None."""
    if a == b:
        return "exact"
    if a.startswith(b) or b.startswith(a):
        return "truncated_copy"
    similarity = SequenceMatcher(None, a[:COMPARE_CHARS], b[:COMPARE_CHARS]).ratio()
    if similarity >= SIMILARITY:
        return "ocr_variant"
    return "reviewed_borderline" if reviewed else None


def find_groups(texts, reviewed_pairs=frozenset()):
    """texts: source_key -> normalised text. Returns (groups, edges) with edges[key] = (partner, kind)."""
    parent = {k: k for k in texts}

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    blocks = collections.defaultdict(list)
    for k, t in texts.items():
        if len(t) >= MIN_CHARS:
            blocks[(k.split(":")[0], t[:BLOCK_CHARS])].append(k)
    edges = {}
    for members in blocks.values():
        if len(members) < 2:
            continue
        members.sort(key=line_id)
        for i, a in enumerate(members):
            for b in members[i + 1:]:
                kind = link_kind(texts[a], texts[b], frozenset((a, b)) in reviewed_pairs)
                if kind and find(a) != find(b):
                    parent[find(b)] = find(a)
                    edges.setdefault(b, (a, kind))
                    edges.setdefault(a, (b, kind))
    groups = collections.defaultdict(list)
    for k in texts:
        groups[find(k)].append(k)
    return [sorted(g, key=line_id) for g in groups.values() if len(g) > 1], edges


def choose_keeper(members, kept, lengths):
    """The member to keep: kept=TRUE first (earlier editions), then longest text, then lowest line id."""
    edition = members[0].split(":")[0]

    def rank(k):
        flag = edition in KEPT_FLAG_EDITIONS and kept.get(k, "").upper() == "TRUE"
        return (not flag, -lengths[k], line_id(k))

    keeper = min(members, key=rank)
    flag_used = edition in KEPT_FLAG_EDITIONS and sum(kept.get(k, "").upper() == "TRUE" for k in members) == 1
    if flag_used:
        reason = "only_kept_true"
    elif len({lengths[k] for k in members}) > 1:
        reason = "longest_text"
    else:
        reason = "lowest_line_id"
    return keeper, reason


def load_reviewed(path):
    pairs = set()
    for r in rows(path):
        assert r["decision"] in {"duplicate", "distinct"}, r
        if r["decision"] == "duplicate":
            pairs.add(frozenset((r["key_a"], r["key_b"])))
    return frozenset(pairs)


def filter_csv(src, dest, drop, key="source_key"):
    """Copy a CSV keeping header and row order, dropping rows whose key is in drop. Returns the kept rows' count."""
    n = 0
    with open(src, encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        out, writer = table(dest, reader.fieldnames)
        for r in reader:
            if r[key] not in drop:
                writer.writerow(r)
                n += 1
        out.close()
    return n


def run(root):
    dest = root / "output/amws" / STAMP
    if dest.exists():
        base.verify_release(dest)
        print("Verified existing v06 education release:", dest)
        return
    previous = root / "output/amws" / BASELINE
    biographies = root / "output/amws" / BIOGRAPHIES
    review = root / "output/amws" / REVIEW / "borderline_pairs_reviewed.csv"
    print("Verifying the v05 release.", flush=True)
    base.verify_release(previous)
    here = Path(__file__)
    code = [here, Path(base.__file__), Path(v05.__file__), here.with_name("test_postsecondary_release_v06.py")]
    v05_files = sorted(p for p in previous.iterdir() if p.is_file())
    hashes = {str(p.resolve()): digest(p) for p in [*v05_files, biographies, review, *code]}

    print("Reading person view and biography text.", flush=True)
    person = {}
    with open(previous / "amws_open_alex.csv", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        person_columns = [c for c in reader.fieldnames if not v05.v04.AWARD_COLUMN.match(c)]
        for r in reader:
            person[r["source_key"]] = {c: r[c] for c in person_columns}
    texts = {}
    for r in rows(biographies):
        if r["source_key"] in person:
            texts[r["source_key"]] = normalise(r["raw_text_adjusted"])
    assert texts.keys() == person.keys(), "Person-view entries without biography text"
    reviewed = load_reviewed(review)
    groups, edges = find_groups(texts, reviewed)
    lengths = {k: len(t) for k, t in texts.items()}
    kept_flags = {k: p["kept"] for k, p in person.items()}

    removed, records = set(), []
    counts = collections.Counter()
    for g in groups:
        keeper, reason = choose_keeper(g, kept_flags, lengths)
        edition = g[0].split(":")[0]
        if edition in KEPT_FLAG_EDITIONS and kept_flags[keeper].upper() == "TRUE" and lengths[keeper] < max(lengths[k] for k in g):
            counts[f"kept_flag_overrode_longer_text:{edition}"] += 1
        for k in g:
            if k == keeper:
                continue
            partner, kind = edges.get(k, (keeper, "chain"))
            differs = [c for c in person_columns if c not in ("source_key", "lineid", "entry_id", "key_lo", "key_hi")
                       and person[k][c] != person[keeper][c]]
            records.append(dict(removed_key=k, kept_key=keeper, edition=edition, group_size=len(g), link_kind=kind,
                                linked_to=partner, similarity_first400=round(SequenceMatcher(
                                    None, texts[k][:COMPARE_CHARS], texts[keeper][:COMPARE_CHARS]).ratio(), 3),
                                removed_kept_flag=kept_flags[k], keeper_kept_flag=kept_flags[keeper],
                                removed_text_chars=lengths[k], keeper_text_chars=lengths[keeper],
                                keeper_reason=reason, differing_person_columns=";".join(differs)))
            removed.add(k)
            counts[f"removed:{edition}"] += 1
            counts[f"removed:{edition}:{kind}"] += 1
    counts["groups"] = len(groups)
    counts["removed_total"] = len(removed)
    print("Duplicate groups:", len(groups), "removed:", len(removed),
          {k: v for k, v in counts.items() if k.startswith("removed:") and k.count(":") == 1}, flush=True)

    work = Path(os.environ.get("AMWS_CHECKPOINT_ROOT", str(Path.home() / ".codex/amws_checkpoints"))) / STAMP
    stage = work / "stage"
    if stage.exists():
        shutil.rmtree(stage)
    stage.mkdir(parents=True)
    rf, rw = table(stage / "duplicates_removed.csv", list(records[0]))
    for r in sorted(records, key=lambda r: (r["edition"], line_id(r["removed_key"]))):
        rw.writerow(r)
    rf.close()

    n_persons = filter_csv(previous / "amws_open_alex.csv", stage / "amws_open_alex.csv", removed)
    assert n_persons == len(person) - len(removed)
    for name in ("education_awards.csv", "education_awards_openalex.csv", "education_records.csv", "manual_assignments.csv",
                 "manual_conflicts_review.csv", "unmatched_awards.csv"):
        filter_csv(previous / name, stage / name, removed)
    with open(previous / "institution_research_inventory.jsonl", encoding="utf-8") as src, \
         open(stage / "institution_research_inventory.jsonl", "w", encoding="utf-8") as out:
        for line in src:
            if json.loads(line)["source_key"] not in removed:
                out.write(line)
    for name in ("removed_awards.csv", "changed_awards.csv"):
        shutil.copy2(previous / name, stage / name)

    # Inventories and coverage from the remaining included awards.
    coverage, unresolved, used = collections.Counter(), collections.Counter(), set()
    for a in rows(stage / "education_awards_openalex.csv"):
        ids = json.loads(a["oa_ids"])
        used.update(ids)
        ed = a["source_key"].split(":")[0]
        coverage[(ed, a["requested_family"], "awards")] += 1
        coverage[(ed, a["requested_family"], "matched")] += bool(ids)
        coverage["manual:" + a["manual_assignment"]] += bool(a["manual_assignment"])
        if a["fully_matched"] != "True":
            unresolved[(a["institution_as_written"], a["training_eligibility"], a["requested_family"])] += 1
    vf, vw = table(stage / "coverage_by_edition_family.csv", ["edition", "family", "metric", "count"])
    for k, n in coverage.items():
        if isinstance(k, tuple):
            vw.writerow(dict(edition=k[0], family=k[1], metric=k[2], count=n))
    vf.close()
    nf, nw = table(stage / "unresolved_training_inventory.csv", ["institution_as_written", "eligibility", "family", "awards"])
    for (name, eligibility, family), n in sorted(unresolved.items()):
        nw.writerow(dict(institution_as_written=name, eligibility=eligibility, family=family, awards=n))
    nf.close()
    strings, candidate = {}, {}
    for u in rows(stage / "unmatched_awards.csv"):
        s = strings.setdefault(u["institution_as_written"], dict(awards=0, persons=set(), categories=collections.Counter(),
                                                                 editions=collections.Counter()))
        s["awards"] += 1
        s["persons"].add(u["source_key"])
        s["categories"][u["unmatched_category"]] += 1
        s["editions"][u["edition"]] += 1
        candidate[u["institution_as_written"]] = (u["normalization_candidate_id"], u["normalization_candidate_name"],
                                                  u["normalization_candidate_share"])
    string_fields = list(next(rows(previous / "unmatched_strings.csv")))
    sf, sw = table(stage / "unmatched_strings.csv", string_fields)
    for name, s in sorted(strings.items(), key=lambda kv: (-kv[1]["awards"], kv[0])):
        cid, cname, share = candidate[name]
        sw.writerow(dict(institution_as_written=name, awards=s["awards"], persons=len(s["persons"]),
                         unmatched_category=";".join(f"{k}:{n}" for k, n in s["categories"].most_common()),
                         editions=";".join(f"{k}:{n}" for k, n in sorted(s["editions"].items())),
                         normalization_candidate_id=cid, normalization_candidate_name=cname, normalization_candidate_share=share))
    sf.close()
    coord_fields = list(next(rows(previous / "institution_reference_coordinates.csv")))
    cf, cw = table(stage / "institution_reference_coordinates.csv", coord_fields)
    for r in rows(previous / "institution_reference_coordinates.csv"):
        if r["openalex_id"] in used:
            cw.writerow(r)
    cf.close()

    schema = json.loads((previous / "schema.json").read_text(encoding="utf-8"))
    schema.update(version=VERSION, baseline_release=BASELINE, removed_awards=0,
                  manual_crosswalk_awards=coverage["manual:crosswalk"], manual_override_awards=coverage["manual:override"],
                  deduplication=dict(rule="edition x first 60 normalised characters; identical, prefix, >=0.85 similar over the "
                                          "first 400 characters, or reviewed duplicate; keep kept=TRUE (1906-1955), then longest "
                                          "text, then lowest line id",
                                     review_file=f"output/amws/{REVIEW}/borderline_pairs_reviewed.csv",
                                     groups=len(groups), removed=len(removed),
                                     removed_by_edition={k.split(":")[1]: v for k, v in counts.items()
                                                         if k.startswith("removed:") and k.count(":") == 1}))
    write_json(stage / "schema.json", schema)
    write_json(stage / "coverage_report.json", dict(canonical_entries=n_persons, v05_entries=len(person),
                                                    used_institutions=len(used), counters=dict(counts),
                                                    unmatched_awards=sum(s["awards"] for s in strings.values()),
                                                    unmatched_strings=len(strings)))
    write_json(stage / "amws_open_alex_manifest.json", dict(rows=n_persons, sha256=digest(stage / "amws_open_alex.csv"),
                                                            schema="schema.json"))
    (stage / "DATA_DICTIONARY.txt").write_text(
        (previous / "DATA_DICTIONARY.txt").read_text(encoding="utf-8")
        + f"v06 ({STAMP}) = v05 minus {len(removed)} duplicate entries ({len(groups)} groups): entries captured twice in the "
          "source (identical text, a truncated copy, or an OCR variant of the same biography). Every remaining row of every file "
          "is byte-identical to v05; duplicates_removed.csv lists each removed entry with the entry kept in its place.\n",
        encoding="utf-8")
    snap = stage / "code_snapshot"
    snap.mkdir(exist_ok=True)
    for p in code:
        shutil.copy2(p, snap / p.name)
    for p, checksum in hashes.items():
        assert digest(p) == checksum, p
    checksums = {str(p.relative_to(stage)): digest(p) for p in stage.rglob("*") if p.is_file() and p.name != "completion_manifest.json"}
    write_json(stage / "completion_manifest.json", dict(status="complete", input_checksums=hashes, output_checksums=checksums,
                                                        canonical_entries=n_persons, schema=schema))
    shutil.copytree(stage, dest)
    base.verify_release(dest)
    print("Published:", dest, dict(counts), flush=True)


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--data-root", type=Path, default=Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets"))))
    run(cli.parse_args().data_root.resolve())

"""Independently validate the v06 AMWS release (v05 minus duplicate entries).

The duplicate groups are re-derived here without importing the v06 builder. Checks:
1. v05 and v06 verify against their manifests;
2. the removed set equals the recomputed one, and each removed entry's keeper is in its group;
3. every filtered file equals the v05 file minus the removed keys, row for row and in order;
4. re-running detection on the remaining entries finds no duplicates;
5. the person count, manifest and coverage reconcile.
"""
import argparse
import collections
import csv
from difflib import SequenceMatcher
import json
import os
from pathlib import Path
import re
import sys

import build_postsecondary_release as base

STAMP = "amws_education_postsecondary_v06_20261021"
PREVIOUS = "amws_education_postsecondary_v05_20261020"
csv.field_size_limit(16 * 1024 * 1024)
rows = base.rows


def groups_of(texts, reviewed):
    parent = {k: k for k in texts}

    def root(x):
        while parent[x] != x:
            x = parent[x]
        return x

    blocks = collections.defaultdict(list)
    for k, t in texts.items():
        if len(t) >= 60:
            blocks[(k.split(":")[0], t[:60])].append(k)
    for members in blocks.values():
        for i in range(len(members)):
            for j in range(i + 1, len(members)):
                a, b = texts[members[i]], texts[members[j]]
                linked = (a == b or a.startswith(b) or b.startswith(a)
                          or SequenceMatcher(None, a[:400], b[:400]).ratio() >= 0.85
                          or frozenset((members[i], members[j])) in reviewed)
                if linked:
                    ra, rb = root(members[i]), root(members[j])
                    if ra != rb:
                        parent[rb] = ra
    out = collections.defaultdict(set)
    for k in texts:
        out[root(k)].add(k)
    return [g for g in out.values() if len(g) > 1]


def run(root):
    failures = []
    new, old = root / "output/amws" / STAMP, root / "output/amws" / PREVIOUS
    base.verify_release(new)
    base.verify_release(old)
    norm = lambda t: re.sub(r"[^a-z0-9]", "", t.lower())
    persons = [r["source_key"] for r in rows(old / "amws_open_alex.csv")]
    keyset = set(persons)
    texts = {r["source_key"]: norm(r["raw_text_adjusted"])
             for r in rows(root / "output/amws/education_enriched_20261011_updated_final/source_biographies.csv") if r["source_key"] in keyset}
    reviewed = frozenset(frozenset((r["key_a"], r["key_b"])) for r in rows(root / "output/amws/amws_dedup_review_20261021/borderline_pairs_reviewed.csv")
                         if r["decision"] == "duplicate")
    groups = groups_of(texts, reviewed)
    group_of = {k: i for i, g in enumerate(groups) for k in g}
    listed = list(rows(new / "duplicates_removed.csv"))
    removed = {r["removed_key"] for r in listed}
    if len(removed) != len(listed):
        failures.append("duplicate rows in duplicates_removed.csv")
    if removed != {k for g in groups for k in g} - {r["kept_key"] for r in listed}:
        failures.append("removed set differs from the recomputed groups")
    for r in listed:
        if group_of.get(r["removed_key"]) is None or group_of.get(r["removed_key"]) != group_of.get(r["kept_key"]):
            failures.append(f"keeper not in group: {r['removed_key']}")
            break
    keepers = {r["kept_key"] for r in listed}
    if keepers & removed or len(keepers) != len(groups):
        failures.append("keepers overlap removed entries or one keeper per group fails")
    remaining = {k: t for k, t in texts.items() if k not in removed}
    if groups_of(remaining, reviewed):
        failures.append("duplicates remain after removal")

    def same_minus(name, key="source_key"):
        a = (r for r in rows(old / name) if r[key] not in removed)
        for x, y in zip(a, rows(new / name), strict=True):
            if x != y:
                return f"{name}: row differs at {y.get(key)}"
        return None

    for name in ("amws_open_alex.csv", "education_awards.csv", "education_awards_openalex.csv", "education_records.csv",
                 "manual_assignments.csv", "manual_conflicts_review.csv", "unmatched_awards.csv"):
        try:
            problem = same_minus(name)
        except ValueError:
            problem = f"{name}: row count differs"
        if problem:
            failures.append(problem)
    inv_old = [l for l in open(old / "institution_research_inventory.jsonl", encoding="utf-8") if json.loads(l)["source_key"] not in removed]
    if inv_old != list(open(new / "institution_research_inventory.jsonl", encoding="utf-8")):
        failures.append("institution_research_inventory.jsonl differs")
    manifest = json.loads((new / "amws_open_alex_manifest.json").read_text(encoding="utf-8"))
    n_new = sum(1 for _ in rows(new / "amws_open_alex.csv"))
    if not (n_new == manifest["rows"] == len(persons) - len(removed)):
        failures.append("person count does not reconcile")
    by_edition = collections.Counter(k.split(":")[0] for k in removed)
    report = dict(status="pass" if not failures else "fail", failures=failures, groups=len(groups), removed=len(removed),
                  removed_by_edition=dict(by_edition), v05_persons=len(persons), v06_persons=n_new,
                  link_kinds=dict(collections.Counter(r["link_kind"] for r in listed)),
                  keeper_reasons=dict(collections.Counter(r["keeper_reason"] for r in listed)))
    out = root / "output/amws" / (STAMP + "_validation")
    out.mkdir(exist_ok=True)
    base.write_json(out / "independent_validation.json", report)
    print(json.dumps(report, indent=1))
    if failures:
        sys.exit(1)


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--data-root", type=Path, default=Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets"))))
    run(cli.parse_args().data_root.resolve())

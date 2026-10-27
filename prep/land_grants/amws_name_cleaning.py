"""Reproducible 1986 AMWS name cleaning and explicit biography-review ledger.

Run `python prep/land_grants/amws_name_cleaning.py prepare`, then enter reviewed
decisions with append_reviews(), and run `... validate --complete` after all 500.
The immutable v06 release is never rewritten; additional corrections are proposals.
"""
import argparse
import collections
import csv
import hashlib
import json
import os
from pathlib import Path
import random

RELEASE = "amws_name_cleaning_20261022"
SOURCE_RELEASE = "amws_education_postsecondary_v06_20261021"
SEED, SAMPLE_SIZE, EXPECTED_ROWS = 42, 500, 79073
LOOKUP_FIELDS = ["source_key", "entry_id", "source_row", "name_raw", "name_cleaned",
                 "period_replaced", "period_count", "raw_text_adjusted", "source_record_sha256"]
REVIEW_FIELDS = ["sample_id", "source_key", "name_raw", "name_cleaned", "decision",
                 "proposed_full_name", "proposed_surname", "proposed_given_names",
                 "proposed_suffix", "evidence", "explanation", "error_pattern", "full_bio_reviewed",
                 "supporting_source_key", "supporting_evidence"]
csv.field_size_limit(16 * 1024 * 1024)


def data_root():
    return Path(os.environ.get("TALENT_DETS_DATA_DIR") or os.environ.get("DET_DIR")
                or Path.home() / "Globtalent Dropbox" / "gtl_talent_dets")


def locations(root=None):
    root = Path(root) if root else data_root()
    source = root / "output/amws" / SOURCE_RELEASE
    return source / "amws_open_alex.csv", source / "amws_open_alex_manifest.json", root / "output/land_grants" / RELEASE


def digest(path):
    with Path(path).open("rb") as f:
        return hashlib.file_digest(f, "sha256").hexdigest()


def record_digest(row):
    return hashlib.sha256(json.dumps(row, ensure_ascii=False, sort_keys=True,
                                    separators=(",", ":")).encode("utf-8")).hexdigest()


def read_csv(path):
    with Path(path).open(encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def write_csv(path, rows, fields):
    with Path(path).open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def write_json(path, obj):
    Path(path).write_text(json.dumps(obj, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def load_source(root=None):
    source, manifest, destination = locations(root)
    expected = json.loads(manifest.read_text(encoding="utf-8"))
    checksum = digest(source)
    if checksum != expected["sha256"]:
        raise ValueError("v06 source checksum differs from its manifest")
    selected, total = [], 0
    with source.open(encoding="utf-8-sig", newline="") as f:
        for total, row in enumerate(csv.DictReader(f), 1):
            if row["source_key"].split(":")[0] != "1986":
                continue
            name = row["name_raw"]
            selected.append(dict(source_key=row["source_key"], entry_id=row["entry_id"],
                                 source_row=str(total), name_raw=name, name_cleaned=name.replace(".", ","),
                                 period_replaced=str("." in name).lower(), period_count=str(name.count(".")),
                                 raw_text_adjusted=row["raw_text_adjusted"], source_record_sha256=record_digest(row)))
    if total != expected["rows"] or len(selected) != EXPECTED_ROWS:
        raise ValueError("Unexpected source row counts")
    if len({r["source_key"] for r in selected}) != len(selected):
        raise ValueError("Duplicate source keys")
    if not all(r["raw_text_adjusted"] for r in selected):
        raise ValueError("Missing biography in 1986 pool")
    return selected, checksum, destination


def draw_sample(lookup):
    rows = random.Random(SEED).sample(sorted(lookup, key=lambda r: r["source_key"]), SAMPLE_SIZE)
    return [dict(sample_id=str(i), **r) for i, r in enumerate(rows, 1)]


def prepare(root=None):
    lookup, checksum, destination = load_source(root)
    if destination.exists():
        validate(root)
        return destination
    destination.mkdir(parents=True)
    sample = draw_sample(lookup)
    write_csv(destination / "names_1986.csv", lookup, LOOKUP_FIELDS)
    write_csv(destination / "period_replacement_log.csv", [r for r in lookup if r["period_replaced"] == "true"], LOOKUP_FIELDS)
    write_csv(destination / "sample_500.csv", sample, ["sample_id"] + LOOKUP_FIELDS)
    source, manifest, _ = locations(root)
    write_json(destination / "sample_manifest.json", dict(
        source_path=str(source), source_sha256=checksum, source_manifest_sha256=digest(manifest),
        source_release=SOURCE_RELEASE, population_rows=len(lookup), sample_size=SAMPLE_SIZE,
        seed=SEED, algorithm="random.Random(42).sample(sorted(rows, key=source_key), 500)",
        sample_ids=[r["source_key"] for r in sample], sample_sha256=digest(destination / "sample_500.csv"),
        name_rule="Every literal period in name_raw becomes a comma, including initials and suffixes",
        output_checksums={p: digest(destination / p) for p in ("names_1986.csv", "period_replacement_log.csv")}))
    (destination / "review_decisions.jsonl").touch()
    validate(root)
    return destination


def check_review(review, sample):
    row = sample[review["sample_id"]]
    if any(review[k] != row[k] for k in ("source_key", "name_raw", "name_cleaned")):
        raise ValueError("Review/source mismatch")
    if review["decision"] not in {"no_change", "proposed_fix", "unresolved"} or review["full_bio_reviewed"] != "true":
        raise ValueError("Invalid review")
    if not review["explanation"] or not review["evidence"] or review["evidence"] not in row["raw_text_adjusted"]:
        raise ValueError("Review lacks literal biography evidence")
    if review["decision"] == "proposed_fix":
        if not review["proposed_full_name"] or review["proposed_full_name"] == row["name_cleaned"] or not review["error_pattern"]:
            raise ValueError("Invalid correction proposal")
    elif any(review[k] for k in REVIEW_FIELDS if k.startswith("proposed_")):
        raise ValueError("Non-fix has proposed correction")
    support = review.get("supporting_source_key", "")
    evidence = review.get("supporting_evidence", "")
    if support or evidence:
        matches = [r for r in sample.values() if r["source_key"] == support and support != row["source_key"]]
        if len(matches) != 1 or not evidence or evidence not in matches[0]["raw_text_adjusted"]:
            raise ValueError("Invalid corroborating biography evidence")


def append_reviews(decisions, root=None):
    """Persist manually reviewed rows; never infer a review from absent decisions.

    Each item requires sample_id and decision. Proposed fixes additionally require
    proposed_full_name, evidence, explanation and error_pattern. Evidence must be a
    literal substring of the full original biography, without Unicode normalization.
    """
    _, _, destination = locations(root)
    if (destination / "completion_manifest.json").exists():
        raise ValueError("Completed review is immutable")
    sample = {r["sample_id"]: r for r in read_csv(destination / "sample_500.csv")}
    path = destination / "review_decisions.jsonl"
    old = [json.loads(line) for line in path.read_text(encoding="utf-8").splitlines() if line]
    seen = {r["sample_id"] for r in old}
    new = []
    for item in decisions:
        item = dict(item)
        key = str(item.pop("sample_id"))
        if key not in sample or key in seen:
            raise ValueError(f"Unknown or repeated review {key}")
        row = sample[key]
        decision = item.get("decision")
        if decision not in {"no_change", "proposed_fix", "unresolved"}:
            raise ValueError(f"Invalid decision for {key}")
        review = {field: "" for field in REVIEW_FIELDS}
        review.update({k: row[k] for k in ["sample_id", "source_key", "name_raw", "name_cleaned"]})
        if set(item) - set(REVIEW_FIELDS):
            raise ValueError("Unknown review fields")
        review.update(item)
        review["full_bio_reviewed"] = "true"
        if decision == "no_change":
            review["evidence"] = review["evidence"] or row["raw_text_adjusted"][:180]
            review["explanation"] = review["explanation"] or "Full biography reviewed; no additional name correction supported."
            review["error_pattern"] = review["error_pattern"] or "none"
        check_review(review, sample)
        new.append(review)
        seen.add(key)
    with path.open("a", encoding="utf-8", newline="\n") as f:
        for review in new:
            f.write(json.dumps(review, ensure_ascii=False) + "\n")
    return len(seen)


def amend_review(sample_id, changes, reason, root=None):
    """Amend a review after new evidence, preserving its original decision in a log."""
    _, _, destination = locations(root)
    if (destination / "completion_manifest.json").exists():
        raise ValueError("Completed review is immutable")
    if not reason or set(changes) - set(REVIEW_FIELDS):
        raise ValueError("Missing reason or unknown amendment fields")
    sample = {r["sample_id"]: r for r in read_csv(destination / "sample_500.csv")}
    path = destination / "review_decisions.jsonl"
    rows = [json.loads(s) for s in path.read_text(encoding="utf-8").splitlines() if s]
    positions = [i for i, r in enumerate(rows) if r["sample_id"] == str(sample_id)]
    if len(positions) != 1:
        raise ValueError("Amendment must refer to one existing review")
    pos = positions[0]
    previous = rows[pos]
    replacement = {**previous, **changes}
    if replacement["sample_id"] != previous["sample_id"]:
        raise ValueError("Cannot change sample ID")
    check_review(replacement, sample)
    rows[pos] = replacement
    with (destination / "review_amendments.jsonl").open("a", encoding="utf-8", newline="\n") as f:
        f.write(json.dumps(dict(sample_id=str(sample_id), reason=reason, previous=previous,
                                replacement=replacement), ensure_ascii=False) + "\n")
    temporary = path.with_suffix(".jsonl.tmp")
    temporary.write_text("".join(json.dumps(r, ensure_ascii=False) + "\n" for r in rows), encoding="utf-8")
    temporary.replace(path)


def validate(root=None, complete=False):
    source, manifest_path, destination = locations(root)
    manifest = json.loads((destination / "sample_manifest.json").read_text(encoding="utf-8"))
    if digest(source) != manifest["source_sha256"] or digest(manifest_path) != manifest["source_manifest_sha256"]:
        raise ValueError("Protected source changed")
    lookup, checksum, _ = load_source(root)
    if read_csv(destination / "names_1986.csv") != lookup:
        raise ValueError("Lookup disagrees with original source, biography, or cleaning rule")
    if read_csv(destination / "period_replacement_log.csv") != [r for r in lookup if r["period_replaced"] == "true"]:
        raise ValueError("Replacement log differs")
    for filename, expected in manifest["output_checksums"].items():
        if digest(destination / filename) != expected:
            raise ValueError(f"Changed frozen output: {filename}")
    sample = read_csv(destination / "sample_500.csv")
    if sample != draw_sample(lookup) or manifest["sample_ids"] != [r["source_key"] for r in sample]:
        raise ValueError("Sample is not reproducible")
    if digest(destination / "sample_500.csv") != manifest["sample_sha256"]:
        raise ValueError("Frozen sample changed")
    reviews = [json.loads(s) for s in (destination / "review_decisions.jsonl").read_text(encoding="utf-8").splitlines() if s]
    indexed = {r["sample_id"]: r for r in sample}
    if len({r["sample_id"] for r in reviews}) != len(reviews):
        raise ValueError("Repeated review decision")
    for r in reviews:
        check_review(r, indexed)
    result = dict(passed=True, population_rows=len(lookup), reviewed=len(reviews), source_sha256=checksum,
                  changed_names=sum(r["period_replaced"] == "true" for r in lookup),
                  replaced_periods=sum(int(r["period_count"]) for r in lookup),
                  decisions=dict(collections.Counter(r["decision"] for r in reviews)),
                  error_patterns=dict(collections.Counter(r["error_pattern"] for r in reviews if r["error_pattern"] != "none")))
    marker = destination / "completion_manifest.json"
    if marker.exists():
        saved = json.loads(marker.read_text(encoding="utf-8"))
        for filename, expected in saved["output_checksums"].items():
            if digest(destination / filename) != expected:
                raise ValueError(f"Completed release file changed: {filename}")
        if saved["status"] != "complete" or any(saved[k] != v for k, v in result.items()):
            raise ValueError("Completion marker differs from current validation")
        return result
    if complete:
        if len(reviews) != SAMPLE_SIZE or {r["sample_id"] for r in reviews} != set(indexed):
            raise ValueError("All 500 biographies must have explicit review decisions")
        if not (destination / "review_report.md").exists():
            raise ValueError("Missing written review report")
        reviews.sort(key=lambda r: int(r["sample_id"]))
        write_csv(destination / "review_decisions.csv", reviews, REVIEW_FIELDS)
        write_csv(destination / "proposed_name_fixes.csv", [r for r in reviews if r["decision"] == "proposed_fix"], REVIEW_FIELDS)
        write_csv(destination / "unresolved_names.csv", [r for r in reviews if r["decision"] == "unresolved"], REVIEW_FIELDS)
        write_json(destination / "validation_report.json", result)
        outputs = [p for p in destination.iterdir() if p.is_file() and p.name != "completion_manifest.json"]
        write_json(destination / "completion_manifest.json", dict(
            status="complete", **result, code_sha256=digest(Path(__file__)),
            further_fixes_applied=False, output_checksums={p.name: digest(p) for p in outputs}))
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["prepare", "review", "validate"])
    parser.add_argument("--root", type=Path)
    parser.add_argument("--reviews", type=Path, help="Explicit manually reviewed decisions as JSONL")
    parser.add_argument("--complete", action="store_true")
    args = parser.parse_args()
    if args.command == "prepare":
        print(prepare(args.root))
    elif args.command == "review":
        if not args.reviews:
            parser.error("review requires --reviews")
        decisions = [json.loads(s) for s in args.reviews.read_text(encoding="utf-8").splitlines() if s]
        print("Reviewed", append_reviews(decisions, args.root))
    else:
        print(json.dumps(validate(args.root, complete=args.complete), indent=2))


if __name__ == "__main__":
    main()

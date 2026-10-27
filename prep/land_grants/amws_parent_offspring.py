"""Exact normalized-surname / >=20-year AMWS record-pair candidates.

Build a new immutable Dropbox release; never modifies source or cleaning releases.
Use `build` once, then `validate` for read-only reconstruction and checksum checks.
"""
import argparse
from bisect import bisect_left
import collections
import csv
from datetime import datetime, timezone
import hashlib
import itertools
import json
from pathlib import Path
import random
import re
import unicodedata

import amws_name_cleaning as base

RELEASE = "amws_parent_offspring_candidates_20261023"
EDITIONS = ("1906", "1938", "1955", "1986")
MIN_GAP, LARGE_GAP, SEED = 20, 80, 46
SUFFIX = re.compile(r"(?:JR|SR|I|II|III|IV|V|VI|3RD|\(JR\)|\(SR\))", re.I)
SEPARATORS = set(" '’‘ʼ-‐‑‒–—.")
RAW_FIELDS = ["entry_id", "lineid", "name_raw", "raw_text_adjusted", "birth_place", "birth_city",
              "birth_state", "birth_country", "birthplace_orig", "city", "state", "geoid", "geo_geoid",
              "kept", "norm_last", "finit", "key_lo", "key_hi"]
LOOKUP_FIELDS = ["source_key", "source_row", "source_record_sha256", "edition", "edition_raw",
                 "last_raw", "first_raw", "birth_year_raw", *RAW_FIELDS, "name_original", "name_period_cleaned",
                 "name_derived", "surname_extracted", "given_names_extracted", "suffix_extracted", "surname_key",
                 "birth_year", "manual_override_applied", "manual_override_name", "review_status",
                 "review_flags", "exclusion_reasons", "eligible", "duplicate_signature", "duplicate_signature_size"]
ENDPOINT_FIELDS = ["source_key", "edition", "name_original", "name_derived", "surname_extracted", "birth_year",
                   "birth_place", "birth_city", "birth_state", "birth_country", "birthplace_orig", "city", "state",
                   "kept", "key_lo", "key_hi", "duplicate_signature", "duplicate_signature_size", "manual_override_applied"]
PAIR_FIELDS = ["pair_index", "surname_key", "age_gap", "gap_above_80", "within_edition"] + [
    f"{role}_{field}" for role in ("parent", "offspring") for field in ENDPOINT_FIELDS]
CONFIG = dict(editions=list(EDITIONS), edition_source="source_key prefix", minimum_birth_year_gap=MIN_GAP,
              maximum_birth_year_gap=None, flag_gap_above=LARGE_GAP, geography_filter=None, gender_filter=None,
              repeat_appearances="Retain source records; duplicate flags do not merge identities",
              inspection_seed=SEED, inspection_size=100,
              normalization="Validate Latin letters/combining accents and approved separators first; NFKD; remove combining marks; uppercase; remove whitespace, apostrophes, hyphens and periods; retain every surname component",
              overrides="Only original 14 explicitly reviewed source_key overrides; no calibrated dictionary or word-join rules",
              record_order="source_key lexical", pair_order="surname_key lexical; parent and offspring ordered by birth_year then source_key",
              year_policy="positive ASCII integer <= source_key edition; no imputation",
              duplicate_signature_policy="NFKC uppercase full derived name with whitespace collapsed + parsed birth year; exact spelling and punctuation retained; flags only")


def paths(root=None):
    root = Path(root) if root else base.data_root()
    source, manifest, prior = base.locations(root)
    return source, manifest, prior, root / "output/land_grants/amws_name_regex_calibration_20261023", root / "output/land_grants" / RELEASE


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


def protected_material(root=None):
    source, manifest, prior, calibration, _ = paths(root)
    expected = read_json(manifest)
    checks = {str(source): expected["sha256"], str(manifest): base.digest(manifest)}
    for directory in (prior, calibration):
        complete = directory / "completion_manifest.json"
        data = read_json(complete)
        checks[str(complete)] = base.digest(complete)
        checks.update({str(directory / name): checksum for name, checksum in data["output_checksums"].items()})
    verify_checksums(checks)
    return checks


def verify_checksums(checks):
    for name, expected in checks.items():
        if base.digest(Path(name)) != expected:
            raise ValueError(f"Protected file changed: {name}")


def review_material(root=None):
    _, _, prior, calibration, _ = paths(root)
    overrides = base.read_csv(prior / "proposed_name_fixes.csv")
    if len(overrides) != 14 or len({r["source_key"] for r in overrides}) != 14:
        raise ValueError("Expected exactly 14 unique approved overrides")
    statuses = {r["source_key"]: r["decision"] for r in base.read_csv(prior / "review_decisions.csv")}
    new = base.read_csv(calibration / "review_decisions.csv")
    if set(statuses) & {r["source_key"] for r in new}:
        raise ValueError("Review samples overlap")
    statuses.update({r["source_key"]: r["decision"] for r in new})
    return {r["source_key"]: r["proposed_full_name"] for r in overrides}, statuses, overrides


def surname_key(text):
    """Return (key, reasons). Never delete an unapproved glyph into a valid key."""
    if not text.strip():
        return "", ["missing_surname"]
    letter_seen = False
    for char in text:
        category = unicodedata.category(char)
        if category.startswith("L") and "LATIN" in unicodedata.name(char, ""):
            letter_seen = True
        elif category.startswith("M") and letter_seen:
            continue
        elif char in SEPARATORS or char.isspace():
            continue
        else:
            return "", ["unexpected_surname_character"]
    decomposed = unicodedata.normalize("NFKD", text)
    key = "".join(c for c in decomposed if not unicodedata.category(c).startswith("M")
                  and not c.isspace() and c not in SEPARATORS).upper()
    if not key:
        return "", ["missing_surname"]
    if len(key) < 2:
        return "", ["surname_initial_only"]
    tokens = re.findall(r"[A-Z]+", decomposed.upper())
    if len(tokens) > 1 and set(tokens) & {"DR", "PROF", "PRES", "REV", "MRS", "MISS", "UNIV", "DEPT", "HOSP"}:
        return "", ["possible_heading_contamination"]
    return key, []


def parse_heading(name):
    parts = [p.strip() for p in name.split(",")]
    reasons = []
    if not name.strip():
        return "", "", "", ["missing_name"]
    if len(parts) == 1:
        return "", "", "", ["missing_surname_separator"]
    surname, given = parts[:2]
    suffix = parts[2] if len(parts) == 3 else ""
    if len(parts) > 3 or (len(parts) == 3 and not SUFFIX.fullmatch(suffix)):
        reasons.append("ambiguous_surname_separator")
    if not given:
        reasons.append("missing_given_name")
    elif SUFFIX.fullmatch(given):
        reasons.append("suffix_only_heading")
    if name.count("(") != name.count(")"):
        reasons.append("unbalanced_heading_parentheses")
    return surname, given, suffix, reasons


def parse_year(text, edition):
    stripped = text.strip()
    if not stripped:
        return "", ["missing_birth_year"]
    if not re.fullmatch(r"[0-9]+", stripped) or not 0 < int(stripped) <= int(edition):
        return "", ["invalid_birth_year"]
    return str(int(stripped)), []


def derive_record(row, index, overrides, statuses):
    edition = row["source_key"].split(":")[0]
    if edition not in EDITIONS:
        raise ValueError(f"Unexpected edition: {edition}")
    original = row["name_raw"] if edition == "1986" else ", ".join(x for x in (row["last"], row["first"]) if x)
    period_cleaned = original.replace(".", ",") if edition == "1986" else original
    override = overrides.get(row["source_key"], "")
    derived = override or period_cleaned
    if edition == "1986" or override:
        surname, given, suffix, reasons = parse_heading(derived)
    else:
        surname, given, suffix = row["last"].strip(), row["first"].strip(), ""
        reasons = [] if original.strip() else ["missing_name"]
    key, name_reasons = surname_key(surname)
    reasons.extend(name_reasons)
    year, year_reasons = parse_year(row["birth_year"], edition)
    reasons.extend(year_reasons)
    status = statuses.get(row["source_key"], "unreviewed")
    if status == "unresolved":
        reasons.append("previously_reviewed_unresolved")
    flags = []
    if row["edition"] != edition:
        flags.append("source_edition_blank_or_disagrees")
    if given and any(unicodedata.category(c).startswith("N") or unicodedata.category(c).startswith("C") for c in given):
        flags.append("given_name_damage_not_repaired")
    if row["kept"].lower() in {"false", "f", "0"}:
        flags.append("legacy_duplicate_indicator")
    # A supported full-name proposal may leave this round's surname key unchanged.
    if status == "proposed_fix" and not override:
        flags.append("new_review_proposal_not_applied")
    reasons = sorted(set(reasons))
    signature_text = " ".join(unicodedata.normalize("NFKC", derived).upper().split())
    signature = hashlib.sha256((signature_text + "|" + year).encode("utf-8")).hexdigest() if signature_text and year else ""
    return dict(source_key=row["source_key"], source_row=str(index), source_record_sha256=base.record_digest(row),
                edition=edition, edition_raw=row["edition"], last_raw=row["last"], first_raw=row["first"], birth_year_raw=row["birth_year"],
                **{field: row[field] for field in RAW_FIELDS}, name_original=original, name_period_cleaned=period_cleaned,
                name_derived=derived, surname_extracted=surname, given_names_extracted=given, suffix_extracted=suffix,
                surname_key=key, birth_year=year, manual_override_applied=str(bool(override)).lower(), manual_override_name=override,
                review_status=status, review_flags=json.dumps(flags), exclusion_reasons=json.dumps(reasons), eligible=str(not reasons).lower(),
                duplicate_signature=signature, duplicate_signature_size="0")


def load_lookup(root=None):
    source, manifest, _, _, _ = paths(root)
    overrides, statuses, _ = review_material(root)
    records = []
    with source.open(encoding="utf-8-sig", newline="") as f:
        for index, row in enumerate(csv.DictReader(f), 1):
            records.append(derive_record(row, index, overrides, statuses))
    if len(records) != read_json(manifest)["rows"] or len({r["source_key"] for r in records}) != len(records):
        raise ValueError("Source row count or unique IDs changed")
    if not set(overrides) <= {r["source_key"] for r in records} or not set(statuses) <= {r["source_key"] for r in records}:
        raise ValueError("Reviewed IDs absent from source")
    signatures = collections.Counter(r["duplicate_signature"] for r in records if r["duplicate_signature"])
    for row in records:
        n = signatures.get(row["duplicate_signature"], 0)
        row["duplicate_signature_size"] = str(n)
        if n > 1:
            row["review_flags"] = json.dumps(json.loads(row["review_flags"]) + ["repeated_exact_name_year_signature"])
    return sorted(records, key=lambda r: r["source_key"])


def buckets_for(records):
    groups = collections.defaultdict(list)
    for row in records:
        if row["eligible"] == "true":
            groups[row["surname_key"]].append(row)
    return {key: sorted(value, key=lambda r: (int(r["birth_year"]), r["source_key"]))
            for key, value in sorted(groups.items())}


def candidate_records(buckets):
    """Only visit output-sized qualifying slices inside each sorted surname bucket."""
    for key, records in buckets.items():
        years = [int(r["birth_year"]) for r in records]
        for parent in records:
            start = bisect_left(years, int(parent["birth_year"]) + MIN_GAP)
            for i in range(start, len(records)):
                yield key, parent, records[i]


def pair_row(index, key, parent, offspring):
    gap = int(offspring["birth_year"]) - int(parent["birth_year"])
    result = dict(pair_index=str(index), surname_key=key, age_gap=str(gap), gap_above_80=str(gap > LARGE_GAP).lower(),
                  within_edition=str(parent["edition"] == offspring["edition"]).lower())
    result.update({f"{role}_{field}": row[field] for role, row in [("parent", parent), ("offspring", offspring)] for field in ENDPOINT_FIELDS})
    return result


def pair_count(buckets):
    return sum(len(records) - bisect_left(years, year + MIN_GAP)
               for records in buckets.values() for years in [[int(r["birth_year"]) for r in records]] for year in years)


def build_pair_files(dest, buckets):
    total = pair_count(buckets)
    chosen = set(random.Random(SEED).sample(range(total), min(100, total)))
    inspection = []
    summary = collections.Counter()
    gaps = collections.Counter()
    per_bucket = collections.Counter()
    with (dest / "candidate_pairs.csv").open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=PAIR_FIELDS)
        writer.writeheader()
        count = 0
        for index, (key, parent, offspring) in enumerate(candidate_records(buckets), 1):
            row = pair_row(index, key, parent, offspring)
            writer.writerow(row)
            if index-1 in chosen:
                inspection.append(row)
            summary[(parent["edition"], offspring["edition"])]+=1
            gaps[row["age_gap"]]+=1
            per_bucket[key]+=1
            count = index
    if count != total:
        raise ValueError("Analytical pair count differs from generated pairs")
    base.write_csv(dest / "inspection_sample_100.csv", inspection, PAIR_FIELDS)
    base.write_csv(dest / "edition_pair_summary.csv", [dict(parent_edition=a, offspring_edition=b,
                   record_pairs=summary[(a,b)]) for a in EDITIONS for b in EDITIONS], ["parent_edition", "offspring_edition", "record_pairs"])
    base.write_csv(dest / "age_gap_summary.csv", [dict(age_gap=gap, record_pairs=n) for gap, n in sorted(gaps.items(), key=lambda x:int(x[0]))],
                   ["age_gap", "record_pairs"])
    return total, summary, gaps, per_bucket


def bucket_summary(buckets, counts):
    rows = []
    for key, records in buckets.items():
        rows.append(dict(surname_key=key, eligible_records=str(len(records)),
                         surname_variants=json.dumps(sorted({r["surname_extracted"] for r in records}), ensure_ascii=False),
                         edition_count=str(len({r["edition"] for r in records})),
                         earliest_birth_year=records[0]["birth_year"], latest_birth_year=records[-1]["birth_year"],
                         candidate_record_pairs=str(counts[key])))
    return rows


def build(root=None):
    _, _, _, _, dest = paths(root)
    if dest.exists():
        raise ValueError("Release already exists; use validate instead of overwriting")
    protected = protected_material(root)
    records = load_lookup(root)
    buckets = buckets_for(records)
    dest.mkdir(parents=True)
    base.write_json(dest / "configuration.json", CONFIG)
    base.write_json(dest / "input_manifest.json", dict(input_checksums=protected, source_rows=len(records),
                    created_at_utc=datetime.now(timezone.utc).isoformat()))
    base.write_csv(dest / "name_lookup.csv", records, LOOKUP_FIELDS)
    excluded = [r for r in records if r["eligible"] == "false"]
    base.write_csv(dest / "excluded_record_review_queue.csv", excluded, LOOKUP_FIELDS)
    _, _, overrides = review_material(root)
    base.write_csv(dest / "manual_override_ledger.csv", overrides, overrides[0].keys())
    members = [dict(surname_key=key, source_key=r["source_key"], edition=r["edition"], birth_year=r["birth_year"])
               for key, rows in buckets.items() for r in rows]
    base.write_csv(dest / "surname_bucket_membership.csv", members, ["surname_key", "source_key", "edition", "birth_year"])
    total, editions, gaps, counts = build_pair_files(dest, buckets)
    summary = bucket_summary(buckets, counts)
    base.write_csv(dest / "surname_buckets.csv", summary, list(summary[0]))
    largest = sorted(summary, key=lambda r: (-int(r["eligible_records"]), r["surname_key"]))[:100]
    base.write_csv(dest / "largest_surname_buckets.csv", largest, list(summary[0]))
    frequencies = collections.Counter(int(r["eligible_records"]) for r in summary)
    frequency_rows = [dict(bucket_size=size, surname_buckets=n, eligible_records=size*n,
                           candidate_record_pairs=sum(counts[key] for key, rows in buckets.items() if len(rows)==size))
                      for size, n in sorted(frequencies.items())]
    base.write_csv(dest / "surname_frequency_summary.csv", frequency_rows,
                   ["bucket_size", "surname_buckets", "eligible_records", "candidate_record_pairs"])
    coverage = []
    for edition in EDITIONS:
        entries = [r for r in records if r["edition"] == edition]
        coverage.append(dict(edition=edition, source_records=len(entries), eligible_records=sum(r["eligible"] == "true" for r in entries),
                             excluded_records=sum(r["eligible"] == "false" for r in entries)))
    base.write_csv(dest / "edition_coverage.csv", coverage, list(coverage[0]))
    reasons = collections.Counter(reason for r in excluded for reason in json.loads(r["exclusion_reasons"]))
    base.write_csv(dest / "exclusion_reason_summary.csv", [dict(reason=k, records=v) for k,v in sorted(reasons.items())], ["reason", "records"])
    write_report(dest, records, summary, total, coverage, editions, gaps, reasons)
    validation = validate(root, complete=False)
    code = [Path(__file__), Path(base.__file__), Path(__file__).with_name("test_amws_parent_offspring.py")]
    base.write_json(dest / "completion_manifest.json", dict(completed_at_utc=datetime.now(timezone.utc).isoformat(),
                    validation=validation, code_checksums={str(p.resolve()): base.digest(p) for p in code},
                    output_checksums={p.name: base.digest(p) for p in sorted(dest.iterdir()) if p.is_file()}))
    return validation


def csv_matches(path, expected):
    """Streaming exact comparison, including all fields and row order."""
    sentinel = object()
    with Path(path).open(encoding="utf-8-sig", newline="") as f:
        for actual, wanted in itertools.zip_longest(csv.DictReader(f), expected, fillvalue=sentinel):
            if actual != wanted:
                raise ValueError(f"Reconstruction mismatch: {Path(path).name}")


def validate(root=None, complete=True):
    _, _, _, _, dest = paths(root)
    manifest = read_json(dest / "input_manifest.json")
    verify_checksums(manifest["input_checksums"])
    if read_json(dest / "configuration.json") != CONFIG:
        raise ValueError("Configuration changed")
    records = load_lookup(root)
    csv_matches(dest / "name_lookup.csv", records)
    excluded = [r for r in records if r["eligible"] == "false"]
    csv_matches(dest / "excluded_record_review_queue.csv", excluded)
    if len(records) != manifest["source_rows"] or len(records) != len(excluded)+sum(r["eligible"] == "true" for r in records):
        raise ValueError("Coverage does not reconcile")
    buckets = buckets_for(records)
    total = pair_count(buckets)
    sample_indices = set(random.Random(SEED).sample(range(total), min(100,total)))
    sampled = []
    editions = collections.Counter()
    gaps = collections.Counter()
    counts = collections.Counter()

    def expected_pairs():
        for index, (key, parent, offspring) in enumerate(candidate_records(buckets), 1):
            row = pair_row(index, key, parent, offspring)
            if (parent["source_key"] == offspring["source_key"] or parent["surname_key"] != offspring["surname_key"]
                    or int(row["age_gap"]) < MIN_GAP or any(r["eligible"] != "true" for r in (parent,offspring))):
                raise ValueError("Invalid pair")
            if index-1 in sample_indices:
                sampled.append(row)
            counts[key]+=1
            editions[(parent["edition"], offspring["edition"])]+=1
            gaps[row["age_gap"]]+=1
            yield row

    csv_matches(dest / "candidate_pairs.csv", expected_pairs())
    if sum(counts.values()) != total:
        raise ValueError("Missing candidate pairs")
    csv_matches(dest / "inspection_sample_100.csv", sampled)
    csv_matches(dest / "surname_bucket_membership.csv", (dict(surname_key=key, source_key=r["source_key"], edition=r["edition"], birth_year=r["birth_year"])
                  for key, rows in buckets.items() for r in rows))
    csv_matches(dest / "surname_buckets.csv", bucket_summary(buckets, counts))
    # Independent counting uses a birth-year histogram, not the pairing iterator.
    independent_count = 0
    for rows in buckets.values():
        years = collections.Counter(int(r["birth_year"]) for r in rows)
        independent_count += sum(n*m for a,n in years.items() for b,m in years.items() if b-a>=MIN_GAP)
    if independent_count != total:
        raise ValueError("Independent age-histogram pair count failed")
    _, _, manual = review_material(root)
    csv_matches(dest / "manual_override_ledger.csv", manual)
    csv_matches(dest / "edition_pair_summary.csv", (dict(parent_edition=a, offspring_edition=b, record_pairs=str(editions[(a,b)])) for a in EDITIONS for b in EDITIONS))
    csv_matches(dest / "age_gap_summary.csv", (dict(age_gap=gap, record_pairs=str(n)) for gap,n in sorted(gaps.items(), key=lambda x:int(x[0]))))
    if complete:
        completion = read_json(dest / "completion_manifest.json")
        verify_checksums({str(dest/name): checksum for name,checksum in completion["output_checksums"].items()})
        verify_checksums(completion["code_checksums"])
    return dict(source_records=len(records), eligible_records=len(records)-len(excluded), excluded_records=len(excluded),
                surname_buckets=len(buckets), candidate_record_pairs=total, inspection_sample=len(sampled),
                manual_overrides=len(manual), gaps_above_80=sum(n for gap,n in gaps.items() if int(gap)>LARGE_GAP),
                protected_inputs_unchanged=True, exact_source_and_pair_reconstruction=True,
                independent_histogram_count=True, reproducible_inspection_sample=True)


def write_report(dest, records, buckets, total, coverage, editions, gaps, reasons):
    lines = ["# AMWS exact-surname parent–offspring candidates", "", "Release: 2026-10-23.", "",
             f"The v06 source contains {len(records):,} edition-specific records. "
             f"{sum(r['eligible']=='true' for r in records):,} records are eligible for matching across {len(buckets):,} surname buckets, "
             f"producing **{total:,} directed candidate record pairs** with identical normalized surnames and birth-year gaps of at least 20 years.", "",
             "These are possible relationships, not verified family links. Repeated appearances are retained; counts are record pairs, not unique people or families. "
             "There is no gender, geography, maximum-age-gap or additional name-similarity restriction. "
             "Coverage is relative to this v06 source, not all people ever listed in AMWS.", "",
             "## Coverage", "", "| Edition | Source records | Eligible | Excluded |", "|---|---:|---:|---:|"]
    for row in coverage:
        lines.append(f"| {row['edition']} | {row['source_records']:,} | {row['eligible_records']:,} | {row['excluded_records']:,} |")
    lines += ["", "Every excluded source record is retained in excluded_record_review_queue.csv, including full source biography text where available. "
              "Reasons can overlap; eligibility partitions the source exactly.", "",
              "| Exclusion reason | Records |", "|---|---:|"]
    for reason,n in sorted(reasons.items(), key=lambda x:(-x[1],x[0])):
        lines.append(f"| {reason} | {n:,} |")
    lines += ["", "## Names and exact matching", "",
              "Edition comes from source_key; the source edition column is often blank. Older editions use the supplied last field. "
              "1986 first receives literal period-to-comma replacement, then surname extraction before a clear comma. "
              "Missing separators, ambiguous extra commas, incomplete/suffix-only headings and unbalanced parentheses are queued rather than guessed. "
              "A positive integer birth year no later than the edition year is required; no years are imputed.", "",
              "Surname characters are checked before normalization: Latin letters and combining accents plus spaces, apostrophes, "
              "hyphens and periods are allowed. Digits, broken encoding, parentheses and other unexpected surname characters are rejected; "
              "single-letter surname keys and surname fields containing clear title/institution contamination are also queued. "
              "NFKD decomposition and combining-mark removal fold accents; uppercase and separator removal produce the exact key. "
              "Every surname component is retained: VAN DYKE→VANDYKE, O’Neil→ONEIL, García-López→GARCIALOPEZ. "
              "Latin letters without a decomposition, such as Ł, are retained rather than silently dropped. "
              "Folding may combine distinct spellings; alphabetic OCR damage that looks like a name can still pass these checks.", "",
              "Only the original 14 approved source_key corrections are applied as manual overrides; "
              "manual_override_ledger.csv preserves their original evidence and the corroborated ANGELL exception. "
              "No calibrated word-join or audited-surname-dictionary rule is applied to the population. "
              "The seven newer name proposals are not promoted into manual overrides. Previously unresolved names from both review rounds are excluded. "
              "Given-name damage may be flagged without excluding a separately clear surname; given-name spelling does not enter matching.", "",
              "Records are paired only inside surname buckets sorted by birth year/source_key. "
              "Binary search finds the first offspring at least 20 years younger for each parent, then emits each qualifying pair once. "
              "The same record cannot pair with itself and equal birth years never qualify.", "",
              "## Candidate counts by edition", "", "Rows identify the older candidate's edition; columns identify the younger candidate's edition.", "",
              "| Parent edition | 1906 | 1938 | 1955 | 1986 |", "|---|---:|---:|---:|---:|"]
    for a in EDITIONS:
        lines.append("| " + a + " | " + " | ".join(f"{editions[(a,b)]:,}" for b in EDITIONS) + " |")
    large = sum(n for gap,n in gaps.items() if int(gap)>LARGE_GAP)
    lines += ["", f"**{large:,} pairs have gaps above 80 years** and are retained with gap_above_80=true. "
              "Large gaps can reflect distant generations, unrelated people or date errors. No pair is removed for this flag.", "",
              "## Largest surname buckets", "", "| Key | Eligible records | Candidate record pairs |", "|---|---:|---:|"]
    for row in sorted(buckets,key=lambda r:(-int(r["eligible_records"]),r["surname_key"]))[:20]:
        lines.append(f"| {row['surname_key']} | {int(row['eligible_records']):,} | {int(row['candidate_record_pairs']):,} |")
    lines += ["", "## Duplicate indicators and audit files", "",
              "Existing kept, key_lo and key_hi fields are preserved, including records previously marked as duplicates. "
              "A repeated exact NFKC/uppercase/whitespace-normalized full derived name plus parsed birth year is an additional indicator only; "
              "honorifics, parenthetical expansions and punctuation are not stripped for this signature. "
              "It can miss repeated appearances and can also group different people. No person identities are asserted or merged.", "",
              "name_lookup.csv retains source names, original surname/given fields, biographies, birth-year text, birthplace fields, "
              "source-row numbers and whole-source-record SHA256 digests alongside the derived fields. "
              "The source itself remains unchanged, preserving all other source values. surname_bucket_membership.csv and surname_buckets.csv "
              "provide membership, variants and counts. candidate_pairs.csv contains endpoints, birthplaces and duplicate indicators. "
              "inspection_sample_100.csv samples min(100, pair count) distinct pair indices with random.Random(46), saving them in canonical pair order. "
              "This inspection sample is not a completed relationship review or a precision estimate.", "",
              "edition_coverage.csv, exclusion_reason_summary.csv, edition_pair_summary.csv, age_gap_summary.csv, "
              "surname_frequency_summary.csv and largest_surname_buckets.csv describe coverage and concentration. "
              "configuration.json records all matching policies; input_manifest.json and completion_manifest.json freeze input/output and code checksums.", "",
              "## Validation", "",
              "The validator reconstructs every lookup row from v06, preserving original values and record digests, then reconstructs "
              "every candidate field in deterministic order. It verifies eligibility, exact surname equality, distinct IDs, >=20-year gaps, "
              "complete bucket membership and sample reproducibility. An independent birth-year histogram calculation checks the total pair count. "
              "Source and both earlier cleaning releases are verified against protected checksums. "
              "Unit tests compare the bucket algorithm to exhaustive pairing and cover normalization, ambiguous names, valid suffixes, "
              "invalid dates, 19/20-year boundaries, repeated records and both within/cross-edition pairs.", ""]
    (dest / "matching_report.md").write_text("\n".join(lines), encoding="utf-8")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["build", "validate"])
    parser.add_argument("--root", type=Path)
    args = parser.parse_args()
    print(json.dumps((build if args.command == "build" else validate)(args.root), indent=2))


if __name__ == "__main__":
    main()

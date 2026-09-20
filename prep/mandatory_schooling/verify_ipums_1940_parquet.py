"""Verify the Parquet tree written by convert_ipums_1940_fullcount.py.

Reads the written dataset back (never the source .dat.gz, except for the
round-trip check) and reports structural, arithmetic and domain checks, plus a
per-state population table to eyeball against published 1940 figures.

Run from any working directory:
    python prep/mandatory_schooling/verify_ipums_1940_parquet.py

Environment:
    IPUMS_1940_PARQUET_DIR    output root (default: D:\\ipums_1940_fullcount)
    IPUMS_1940_SOURCE         source .dat.gz, for the round-trip check
    IPUMS_1940_ROUNDTRIP_ROWS records to re-read from source (default: 1000000; 0 skips)
    IPUMS_1940_CHECK_HISTID   set to "true" to check HISTID uniqueness (~1 GB RAM)
"""

import json
import os
import sys
from pathlib import Path

import numpy as np
import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.dataset as ds

sys.path.insert(0, str(Path(__file__).resolve().parent))
import convert_ipums_1940_fullcount as converter  # noqa: E402

ROOT = Path(os.environ.get("IPUMS_1940_PARQUET_DIR", r"D:\ipums_1940_fullcount"))
ROUNDTRIP_ROWS = int(os.environ.get("IPUMS_1940_ROUNDTRIP_ROWS", "1000000"))
CHECK_HISTID = os.environ.get("IPUMS_1940_CHECK_HISTID", "").lower() in ("1", "true", "yes")

# Published 1940 resident population, U.S. Census Bureau.
CENSUS_1940_TOTAL = 132_164_569
STATE_POP_1940 = {
    1: 2832961, 4: 499261, 5: 1949387, 6: 6907387, 8: 1123296, 9: 1709242, 10: 266505,
    11: 663091, 12: 1897414, 13: 3123723, 16: 524873, 17: 7897241, 18: 3427796,
    19: 2538268, 20: 1801028, 21: 2845627, 22: 2363880, 23: 847226, 24: 1821244,
    25: 4316721, 26: 5256106, 27: 2792300, 28: 2183796, 29: 3784664, 30: 559456,
    31: 1315834, 32: 110247, 33: 491524, 34: 4160165, 35: 531818, 36: 13479142,
    37: 3571623, 38: 641935, 39: 6907612, 40: 2336434, 41: 1089684, 42: 9900180,
    44: 713346, 45: 1899804, 46: 642961, 47: 2915841, 48: 6414824, 49: 550310,
    50: 359231, 51: 2677773, 53: 1736191, 54: 1901974, 55: 3137587, 56: 250742,
}
STATE_NAME = {
    1: "AL", 2: "AK", 4: "AZ", 5: "AR", 6: "CA", 8: "CO", 9: "CT", 10: "DE", 11: "DC",
    12: "FL", 13: "GA", 15: "HI", 16: "ID", 17: "IL", 18: "IN", 19: "IA", 20: "KS",
    21: "KY", 22: "LA", 23: "ME", 24: "MD", 25: "MA", 26: "MI", 27: "MN", 28: "MS",
    29: "MO", 30: "MT", 31: "NE", 32: "NV", 33: "NH", 34: "NJ", 35: "NM", 36: "NY",
    37: "NC", 38: "ND", 39: "OH", 40: "OK", 41: "OR", 42: "PA", 44: "RI", 45: "SC",
    46: "SD", 47: "TN", 48: "TX", 49: "UT", 50: "VT", 51: "VA", 53: "WA", 54: "WV",
    55: "WI", 56: "WY", 72: "PR",
}

# Positions of 16 genuinely random hex digits in a v4 UUID
# (8-4-4-4-12 with dashes at 8/13/18/23, a constant '4' at 14 and the variant
# nibble at 19). Folding those fixed characters into the hash would collapse its
# entropy and manufacture collisions.
HISTID_RANDOM_NIBBLES = list(range(0, 8)) + list(range(9, 13)) + list(range(24, 28))

FAILURES = 0


def check(label, ok, detail=""):
    global FAILURES
    if not ok:
        FAILURES += 1
    print(f"  {'PASS' if ok else 'FAIL'}  {label}{('  -> ' + detail) if detail else ''}")


def report(label, value):
    print(f"  ....  {label}: {value}")


def main():
    manifest = json.loads((ROOT / "_manifest.json").read_text(encoding="utf-8"))
    person = ds.dataset(ROOT / "person", format="parquet", partitioning="hive")
    household = ds.dataset(ROOT / "household", format="parquet", partitioning="hive")

    print(f"Dataset: {ROOT}")
    print(f"Converted {manifest['converted_utc']} in {manifest['elapsed_minutes']} min\n")

    # ---- structure ---------------------------------------------------------
    print("Structure")
    check("layout was cross-checked against the IPUMS command file",
          manifest["layout_cross_checked_against_sps"])
    check("conversion was not a dry run", manifest["dry_run_max_records"] is None,
          f"max_records={manifest['dry_run_max_records']}")
    n_persons = person.count_rows()
    n_households = household.count_rows()
    check("person rows match the manifest", n_persons == manifest["persons"],
          f"{n_persons:,} vs {manifest['persons']:,}")
    check("household rows match the manifest", n_households == manifest["households"],
          f"{n_households:,} vs {manifest['households']:,}")
    check("sum(NUMPREC) equals the person count",
          manifest["numprec_sum"] == manifest["persons"],
          f"{manifest['numprec_sum']:,} vs {manifest['persons']:,}")
    check("NUMPREC equals NUMPERHH on every household",
          manifest["domain_counters"]["numprec_ne_numperhh"] == 0,
          f"{manifest['domain_counters']['numprec_ne_numperhh']:,} mismatches")
    report("persons", f"{n_persons:,}")
    report("households", f"{n_households:,}")
    report("published 1940 resident population", f"{CENSUS_1940_TOTAL:,} "
           f"(difference {n_persons - CENSUS_1940_TOTAL:+,}; universes differ, not asserted)")
    report("mean household size", f"{n_persons / n_households:.2f}")
    print()

    # ---- domains -----------------------------------------------------------
    # Accumulated batch by batch: the person table is 132 M rows and will not fit
    # in memory alongside the HISTID check.
    print("Domains")
    stats = scan_domains(person, ["SEX", "AGE", "YEAR", "RECTYPE", "birthyr_approx",
                                  "county_geoid", "BPL"])
    check("SEX in {1,2}", stats["SEX"]["values"] <= {1, 2}, str(sorted(stats["SEX"]["values"])))
    check("YEAR is 1940 everywhere", stats["YEAR"]["values"] == {1940})
    check("RECTYPE is 'P' everywhere", stats["RECTYPE"]["values"] == {"P"})
    age_max = stats["AGE"]["max"]
    check("AGE is a valid age or the 999 missing code", age_max <= 135 or age_max == 999,
          f"max {age_max}")
    report("birthyr_approx range",
           f"{stats['birthyr_approx']['min']}-{stats['birthyr_approx']['max']}")
    report("AGE missing (999)", f"{manifest['domain_counters']['age_missing']:,}")
    geoid_nulls = stats["county_geoid"]["nulls"]
    report("county_geoid null (county not identified)",
           f"{geoid_nulls:,} ({100 * geoid_nulls / n_persons:.2f}%)")
    report("distinct county_geoid", f"{len(stats['county_geoid']['values']):,}")
    report("distinct BPL codes", f"{len(stats['BPL']['values']):,}")
    hh_stats = scan_domains(household, ["CNTRY", "CPI99", "RECTYPE"])
    check("CNTRY is 840 everywhere", hh_stats["CNTRY"]["values"] == {840})
    check("CPI99 is 11986 everywhere", hh_stats["CPI99"]["values"] == {11986})
    check("RECTYPE is 'H' everywhere", hh_stats["RECTYPE"]["values"] == {"H"})
    print()

    # ---- per-state ---------------------------------------------------------
    print("Persons by state (vs published 1940 population)")
    print(f"  {'st':>3} {'':2} {'parquet':>12} {'published':>12} {'diff':>10} {'pct':>7}")
    worst = 0.0
    for state, count in sorted(manifest["persons_per_state"].items(), key=lambda kv: int(kv[0])):
        state = int(state)
        published = STATE_POP_1940.get(state)
        name = STATE_NAME.get(state, "??")
        if published is None:
            print(f"  {state:>3} {name:2} {count:>12,} {'-':>12} {'-':>10} {'-':>7}")
            continue
        diff = count - published
        pct = 100 * diff / published
        worst = max(worst, abs(pct))
        print(f"  {state:>3} {name:2} {count:>12,} {published:>12,} {diff:>+10,} {pct:>+6.2f}%")
    check("every state is within 2% of its published population", worst < 2.0,
          f"largest deviation {worst:.2f}%")
    print()

    # ---- HISTID ------------------------------------------------------------
    print("HISTID")
    if CHECK_HISTID:
        seen = np.empty(n_persons, dtype=np.uint64)
        filled = 0
        for batch in person.to_batches(columns=["HISTID"]):
            array = batch["HISTID"]
            # A batch array can be a slice into a larger buffer, so index through
            # its own offsets rather than assuming the buffer starts at row 0.
            offsets = np.frombuffer(array.buffers()[1], dtype=np.int32)
            offsets = offsets[array.offset:array.offset + len(array) + 1]
            data = np.frombuffer(array.buffers()[2], dtype=np.uint8)
            raw = data[offsets[0]:offsets[-1]].reshape(len(array), 36)
            raw = raw[:, HISTID_RANDOM_NIBBLES]
            packed = np.zeros(raw.shape[0], dtype=np.uint64)
            for k in range(16):
                digit = raw[:, k]
                value = np.where(digit <= 57, digit - 48, (digit & 0xDF) - 55).astype(np.uint64)
                packed = (packed << np.uint64(4)) | value
            seen[filled:filled + packed.size] = packed
            filled += packed.size
        seen = seen[:filled]
        seen.sort()
        candidates = int((seen[1:] == seen[:-1]).sum())
        check("HISTID 64-bit hashes are unique", candidates == 0,
              f"{candidates:,} candidate collisions (would need a full-string check)")
    else:
        report("uniqueness check", "skipped (set IPUMS_1940_CHECK_HISTID=true)")
    print()

    # ---- round-trip --------------------------------------------------------
    print("Round-trip against the source file")
    if ROUNDTRIP_ROWS <= 0:
        report("round-trip", "skipped")
    else:
        roundtrip(person, manifest)
    print()

    print("All checks passed." if FAILURES == 0 else f"{FAILURES} check(s) FAILED.")
    sys.exit(0 if FAILURES == 0 else 1)


def scan_domains(dataset, columns, distinct_cap=100_000):
    """Per-column min/max, null count and distinct values, accumulated batch by batch.

    Distinct tracking gives up past `distinct_cap` values so a high-cardinality
    column cannot blow up memory.
    """
    stats = {name: {"min": None, "max": None, "nulls": 0, "values": set(), "capped": False}
             for name in columns}
    for batch in dataset.to_batches(columns=columns):
        for name in columns:
            array = batch[name]
            entry = stats[name]
            entry["nulls"] += array.null_count
            if array.null_count == len(array):
                continue
            unique = pc.unique(array).drop_null()
            if not entry["capped"]:
                entry["values"].update(unique.to_pylist())
                if len(entry["values"]) > distinct_cap:
                    entry["values"] = set()
                    entry["capped"] = True
            if pa.types.is_integer(array.type):
                low, high = pc.min(array).as_py(), pc.max(array).as_py()
                entry["min"] = low if entry["min"] is None else min(entry["min"], low)
                entry["max"] = high if entry["max"] is None else max(entry["max"], high)
    return stats


def roundtrip(person, manifest):
    """Re-parse the first ROUNDTRIP_ROWS source records and compare to the Parquet."""
    source = converter.resolve_source()
    fields = converter.load_layout()
    compare = ["SERIAL", "PERNUM", "AGE", "SEX", "BPL", "BPLD", "SCHOOL", "HIGRADE",
               "EDUC", "OCC1950", "INCWAGE", "MIGCOUNTY", "SSENROLL"]

    statefip = converter.field_by_name(fields["H"], "STATEFIP")
    histid = converter.field_by_name(fields["P"], "HISTID")

    expected = {name: [] for name in compare}
    expected["HISTID"] = []
    first_state = None
    pending_state = None
    taken = 0
    # Compare within one partition so that file order and Parquet order agree.
    # A block can straddle the state boundary (Alaska, the first state in the
    # file, is smaller than one block), so keep that block's in-state persons
    # before stopping rather than discarding the whole block.
    for block, n_records in converter.iter_blocks(source, ROUNDTRIP_ROWS):
        buf, starts, is_h = converter.split_records(block, n_records)
        hh = converter.RecordView(buf, starts[is_h])
        pv = converter.RecordView(buf, starts[~is_h])
        states = hh.digits(statefip["start"], statefip["width"], np.int16)
        if first_state is None:
            if not states.size:
                continue
            first_state = int(states[0])

        previous = pending_state if pending_state is not None else first_state
        extended = np.concatenate((np.array([previous], dtype=states.dtype), states))
        keep = extended[np.cumsum(is_h)[~is_h]] == first_state
        if states.size:
            pending_state = states[-1]

        for name in compare:
            spec = converter.field_by_name(fields["P"], name)
            expected[name].append(pv.digits(spec["start"], spec["width"], np.int64)[keep])
        ids = pv.text(histid["start"], histid["width"]).to_pylist()
        expected["HISTID"].append([value for value, flag in zip(ids, keep) if flag])
        taken += int(keep.sum())

        if states.size and (states != first_state).any():
            break

    got = person.to_table(
        columns=compare + ["HISTID"],
        filter=(ds.field("statefip") == first_state)).slice(0, taken)
    report("records compared", f"{taken:,} (statefip={first_state:02d})")
    for name in compare:
        want = np.concatenate(expected[name])[:taken]
        have = np.asarray(got[name].to_numpy(zero_copy_only=False), dtype=np.int64)
        check(f"round-trip {name}", bool(np.array_equal(want, have)),
              "" if np.array_equal(want, have) else f"{int((want != have).sum()):,} differ")
    want_ids = [v for part in expected["HISTID"] for v in part][:taken]
    check("round-trip HISTID", want_ids == got["HISTID"].to_pylist())


if __name__ == "__main__":
    main()

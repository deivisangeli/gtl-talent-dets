"""Checks for convert_ipums_1940_fullcount.py.

Builds a small synthetic EXT1940USCB-shaped .dat.gz whose every field value is
known by construction, converts it, and reads the Parquet back. Each block of
checks names the failure mode it rules out.

Run from any working directory:
    python prep/mandatory_schooling/test_ext1940_parser.py
"""

import gzip
import os
import sys
import tempfile
from pathlib import Path

import pyarrow.dataset as ds

sys.path.insert(0, str(Path(__file__).resolve().parent))
import convert_ipums_1940_fullcount as converter  # noqa: E402

FAILURES = 0


def check(label, expr):
    global FAILURES
    try:
        ok = bool(expr())
    except Exception as exc:  # a raising check is a failing check
        print(f"    error: {type(exc).__name__}: {exc}")
        ok = False
    if not ok:
        FAILURES += 1
    print(f"  {'PASS' if ok else 'FAIL'}  {label}")


def check_raises(label, fn, fragment):
    global FAILURES
    try:
        fn()
    except ValueError as exc:
        ok = fragment.lower() in str(exc).lower()
        if not ok:
            print(f"    error: raised ValueError but message lacked {fragment!r}: {exc}")
    except Exception as exc:
        ok = False
        print(f"    error: expected ValueError, got {type(exc).__name__}: {exc}")
    else:
        ok = False
        print("    error: expected ValueError, nothing raised")
    if not ok:
        FAILURES += 1
    print(f"  {'PASS' if ok else 'FAIL'}  {label}")


# --------------------------------------------------------------------------- #
# Synthetic fixture
# --------------------------------------------------------------------------- #

FIELDS = converter.load_layout()
HH_DEFAULTS = {"RECTYPE": "H", "YEAR": 1940, "CPI99": converter.CPI99_1940,
               "CNTRY": converter.CNTRY_US, "DATANUM": 2}
P_DEFAULTS = {"RECTYPE": "P", "YEAR": 1940, "DATANUM": 2, "PERWT": 100}


def make_record(rectype, values):
    defaults = HH_DEFAULTS if rectype == "H" else P_DEFAULTS
    values = {**defaults, **values}
    buf = ["0"] * converter.RECORD_WIDTH[rectype]
    for field in FIELDS[rectype]:
        raw = values.get(field["name"], 0)
        if field["dtype"] == "string":
            text = str(raw).ljust(field["width"])[:field["width"]]
        else:
            text = str(int(raw)).zfill(field["width"])
            if len(text) != field["width"]:
                raise ValueError(f"{field['name']}={raw} does not fit {field['width']} digits")
        buf[field["start"] - 1:field["end"]] = list(text)
    return "".join(buf)


def histid(n):
    return f"{n:08d}-0000-4000-8000-{n:012d}"


def build_fixture():
    """Three households across two states, one with no identified county."""
    records = []
    expected = []

    households = [
        # serial, statefip, county, stateicp, region, persons [(pernum, age, sex, bpl)]
        (1, 1, 730, 41, 63, [(1, 42, 1, 1), (2, 39, 2, 13), (3, 7, 2, 1)]),
        (2, 1, 1210, 41, 63, [(1, 66, 1, 47)]),
        # COUNTY 0 -> county_geoid must be null
        (3, 1, 0, 41, 63, [(1, 25, 2, 12)]),
        (4, 6, 750, 71, 93, [(1, 999, 1, 53), (2, 31, 2, 6)]),
    ]
    for serial, statefip, county, stateicp, region, people in households:
        records.append(make_record("H", {
            "SERIAL": serial, "NUMPREC": len(people), "NUMPERHH": len(people),
            "STATEFIP": statefip, "STATEICP": stateicp, "COUNTY": county,
            "REGION": region, "URBAN": 1, "METRO": 0, "GQ": 1, "FARM": 2,
            "HHWT": 100,
        }))
        for pernum, age, sex, bpl in people:
            records.append(make_record("P", {
                "SERIAL": serial, "PERNUM": pernum, "AGE": age, "SEX": sex,
                "BPL": bpl, "BPLD": bpl * 100, "SCHOOL": 1, "HIGRADE": 9,
                "EDUC": 6, "HISTID": histid(len(expected) + 1), "SSENROLL": 1,
                "MIGCOUNTY": 9999,
            }))
            expected.append({
                "SERIAL": serial, "PERNUM": pernum, "AGE": age, "SEX": sex, "BPL": bpl,
                "HISTID": histid(len(expected) + 1),
                "hh_STATEFIP": statefip, "hh_COUNTY": county, "hh_STATEICP": stateicp,
                "county_geoid": None if county == 0 else f"{statefip:02d}{county // 10:03d}",
                "birthyr_approx": None if age >= 900 else 1940 - age,
            })
    return records, expected


def write_gz(path, records):
    with gzip.open(path, "wt", newline="") as handle:
        handle.write("".join(line + "\n" for line in records))


def run_conversion(tmp, records, chunk_bytes):
    source = tmp / "EXT1940USCB.dat.gz"
    write_gz(source, records)
    out = tmp / f"out_{chunk_bytes}"
    os.environ["IPUMS_1940_SOURCE"] = str(source)
    converter.OUTPUT_ROOT = out
    converter.CHUNK_BYTES = chunk_bytes
    converter.MAX_RECORDS = None
    converter.IPUMS_DOC_URLS = {}  # keep the test offline
    out.mkdir(parents=True, exist_ok=True)
    converter.convert()
    return out


def read_table(root, name):
    dataset = ds.dataset(root / name, format="parquet", partitioning="hive")
    return dataset.to_table().to_pylist()


def main():
    records, expected = build_fixture()
    print(f"Fixture: {len(records)} records, {len(expected)} persons\n")

    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)

        # A chunk of 400 bytes cuts mid-household repeatedly, so the household
        # geography carried onto persons must survive block boundaries. Rules out
        # the carry silently resetting at the start of each block.
        for chunk_bytes in (1 << 20, 400):
            print(f"--- chunk_bytes={chunk_bytes} ---")
            out = run_conversion(tmp, records, chunk_bytes)
            persons = sorted(read_table(out, "person"), key=lambda r: (r["SERIAL"], r["PERNUM"]))
            households = sorted(read_table(out, "household"), key=lambda r: r["SERIAL"])

            check("person row count", lambda: len(persons) == len(expected))
            check("household row count", lambda: len(households) == 4)
            check("sum(NUMPREC) equals person count",
                  lambda: sum(h["NUMPREC"] for h in households) == len(expected))

            for key in ("SERIAL", "PERNUM", "AGE", "SEX", "BPL", "HISTID"):
                check(f"person field round-trips: {key}",
                      lambda k=key: [p[k] for p in persons] == [e[k] for e in expected])

            # Rules out the household->person geography slipping by one record.
            for key in ("hh_STATEFIP", "hh_COUNTY", "hh_STATEICP"):
                check(f"household geography carried: {key}",
                      lambda k=key: [p[k] for p in persons] == [e[k] for e in expected])

            check("county_geoid derived, null where COUNTY==0",
                  lambda: [p["county_geoid"] for p in persons]
                  == [e["county_geoid"] for e in expected])
            check("birthyr_approx derived, null where AGE>=900",
                  lambda: [p["birthyr_approx"] for p in persons]
                  == [e["birthyr_approx"] for e in expected])
            check("statefip partition column matches hh_STATEFIP",
                  lambda: all(int(p["statefip"]) == p["hh_STATEFIP"] for p in persons))
            check("partition directories written",
                  lambda: sorted(d.name for d in (out / "person").iterdir())
                  == ["statefip=01", "statefip=06"])
            print()

        # Compact dtypes: a 1-char field must not land as int64.
        print("--- schema ---")
        out = run_conversion(tmp, records, 1 << 20)
        schema = ds.dataset(out / "person", format="parquet", partitioning="hive").schema
        check("SEX is int8", lambda: str(schema.field("SEX").type) == "int8")
        check("SERIAL is int32", lambda: str(schema.field("SERIAL").type) == "int32")
        check("PERWT is int64", lambda: str(schema.field("PERWT").type) == "int64")
        check("HISTID is string", lambda: str(schema.field("HISTID").type) == "string")
        check("birthyr_approx is int16",
              lambda: str(schema.field("birthyr_approx").type) == "int16")
        print()

        # The two failure modes that would otherwise corrupt every column silently.
        print("--- guards ---")
        corrupted = list(records)
        bad = list(corrupted[1])
        bad[73] = "X"  # AGE occupies 74-76 (1-based)
        corrupted[1] = "".join(bad)
        check_raises("non-digit in a numeric field is caught",
                     lambda: run_conversion(tmp, corrupted, 1 << 20), "misaligned")

        slipped = list(records)
        slipped[1] = make_record("P", {"SERIAL": 999, "PERNUM": 1, "AGE": 42, "SEX": 1,
                                       "HISTID": histid(1)})
        check_raises("person whose SERIAL does not match its household is caught",
                     lambda: run_conversion(tmp, slipped, 1 << 20), "slipped")

        truncated = "".join(records[0][:-5])
        check_raises("short record is caught",
                     lambda: run_conversion(tmp, [truncated] + records[1:], 1 << 20),
                     "unexpected length")

    print(f"\n{'All checks passed.' if FAILURES == 0 else f'{FAILURES} check(s) FAILED.'}")
    sys.exit(0 if FAILURES == 0 else 1)


if __name__ == "__main__":
    main()

"""Extract the inputs for the county x cohort attendance build from the linked census.

One streaming pass over usa_00001.csv.gz (12.2 GB gzipped, 220,979,023 rows) writing
two Parquet files:

  men_1940.parquet            the 1940 rows for white, US-born men in AGE 19-49,
                              i.e. birth cohorts 1890-1920 on yob = 1940 - AGE - 1,
                              carrying attainment (HIGRADED) and the 1935 migration
                              questions the non-migrant restriction needs
  childhood_pre1940.parquet   every pre-1940 row at AGE 0-18, carrying the state and
                              county the person lived in -- the build takes each
                              man's EARLIEST such record as his county of schooling

Unlike extract_linked_bite_inputs.py, the 1940 rows ARE filtered on SEX, RACE and BPL
here. This build is defined on white US-born men only, and the 1940 row is the
authoritative record of all three. The childhood rows are NOT filtered on sex or race:
a miscoded childhood row must not break a link the 1940 row vouches for.

The file is ordered by census year, 1900 first and 1940 last, so:

  1. Every column is read as string and cast explicitly. Type inference from the
     first block makes a 1940-only column (HIGRADED, MIGRATE5) null, and the cast
     then fails on its first real value 200 M rows later.
  2. The childhood rows cannot be restricted to the HIKs of the kept men, because
     the 1940 rows have not been seen yet. The join in
     build_county_attendance_cohort.R drops the rest.

AGE 0-4 rows are kept in the childhood table. They are not school age, but an
earliest record at age 3 still locates the county a man grew up in; the build
decides what to do with them, not the extractor.

Run from any working directory:
    python prep/mandatory_schooling/extract_linked_attendance_cohort_inputs.py

Environment:
    IPUMS_LINKED_CSV       path to the extract
                           (default: ~/OneDrive/Documentos/usa_00001.csv.gz)
    IPUMS_BITE_MAX_ROWS    stop after N rows (dry run; default: all)
    TALENT_DETS_DATA_DIR / DET_DIR    Dropbox project root

Outputs (output/mandatory_schooling/attendance_cohort/inputs/):
    men_1940.parquet
    childhood_pre1940.parquet
    extract_manifest.json
"""

import csv
import gzip
import hashlib
import json
import os
import time
from pathlib import Path

import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.parquet as pq
from pyarrow import csv as pacsv

# --------------------------------------------------------------------------- #
# Paths
# --------------------------------------------------------------------------- #

SOURCE = Path(os.environ.get(
    "IPUMS_LINKED_CSV",
    Path.home() / "OneDrive" / "Documentos" / "usa_00001.csv.gz"))
MAX_ROWS = int(os.environ.get("IPUMS_BITE_MAX_ROWS", "0")) or None


def resolve_data_root():
    """Same three-tier fallback the R prep scripts use."""
    for var in ("TALENT_DETS_DATA_DIR", "DET_DIR"):
        value = os.environ.get(var)
        if value and Path(value).is_dir():
            return Path(value)
    fallback = Path.home() / "Globtalent Dropbox" / "gtl_talent_dets"
    if fallback.is_dir():
        return fallback
    raise FileNotFoundError(
        "Could not locate the Dropbox data root. Set TALENT_DETS_DATA_DIR or DET_DIR.")


OUT_DIR = (resolve_data_root() / "output" / "mandatory_schooling"
           / "attendance_cohort" / "inputs")

# --------------------------------------------------------------------------- #
# Sample window and guards
# --------------------------------------------------------------------------- #

# yob = 1940 - AGE - 1, so AGE 19-49 is birth cohorts 1890-1920. Everyone is 19 or
# older in April 1940, so high-school completion is observable for the whole window.
MEN_AGE_MIN, MEN_AGE_MAX = 19, 49

# BPL 1-56 are the US states and DC; 90+ are territories and foreign countries.
BPL_US_MAX = 56

CHILD_AGE_MIN, CHILD_AGE_MAX = 0, 18

# Established by a full pass in profile_ipums_linked_census.py. The run fails if
# they no longer hold, which is how a swapped or truncated extract gets caught.
EXPECTED_TOTAL = 220_979_023
EXPECTED_BY_YEAR = {
    1900: 12_971_952, 1910: 23_984_383, 1920: 39_982_220,
    1930: 70_207_628, 1940: 73_832_840,
}

MEN_SCHEMA = pa.schema([
    ("HIK", pa.string()),
    ("AGE", pa.int16()),
    ("BPL", pa.int16()),
    ("HIGRADED", pa.int16()),
    ("EDUC", pa.int16()),
    ("SCHOOL", pa.int8()),
    ("STATEFIP", pa.int16()),
    ("COUNTYICP", pa.int32()),
    # 1 same house, 2 moved within state, 3 between states, 4 abroad, 0 N/A,
    # 9 unknown -- residence in 1935, the only migration question in the file.
    ("MIGRATE5", pa.int8()),
    ("MIGPLAC5", pa.int16()),
    ("GQ", pa.int8()),
])

CHILD_SCHEMA = pa.schema([
    ("HIK", pa.string()),
    ("YEAR", pa.int16()),
    ("AGE", pa.int16()),
    ("SEX", pa.int8()),
    ("RACE", pa.int8()),
    ("STATEFIP", pa.int16()),
    ("COUNTYICP", pa.int32()),
])

FILTER_ONLY = ["YEAR", "SEX", "RACE"]


def read_header(path):
    """Column names, without parsing the body."""
    with gzip.open(path, "rt") as handle:
        return next(csv.reader(handle))


def to_int(array, dtype):
    """Cast a string column, treating blank as null rather than as an error."""
    trimmed = pc.utf8_trim_whitespace(array)
    blanked = pc.if_else(pc.equal(trimmed, ""), pa.scalar(None, pa.string()), trimmed)
    return pc.cast(blanked, dtype)


def project(batch, schema):
    """Build a batch matching `schema`, casting everything but HIK."""
    columns = [batch.column(f.name) if f.name == "HIK" else to_int(batch.column(f.name), f.type)
               for f in schema]
    return pa.RecordBatch.from_arrays(columns, schema=schema)


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1 << 22), b""):
            digest.update(block)
    return digest.hexdigest()


def between(array, lo, hi):
    return pc.and_(pc.greater_equal(array, lo), pc.less_equal(array, hi))


def main():
    started = time.time()
    if not SOURCE.is_file():
        raise FileNotFoundError(f"Extract not found: {SOURCE}\nSet IPUMS_LINKED_CSV.")
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    header = read_header(SOURCE)
    needed = set(MEN_SCHEMA.names) | set(CHILD_SCHEMA.names) | set(FILTER_ONLY)
    missing = sorted(needed - set(header))
    if missing:
        raise ValueError(f"columns absent from the extract: {', '.join(missing)}")

    print(f"Source : {SOURCE}  ({SOURCE.stat().st_size / 1e9:.2f} GB gzipped)")
    print(f"Output : {OUT_DIR}")
    if MAX_ROWS:
        print(f"DRY RUN: stopping after {MAX_ROWS:,} rows")

    read_opts = pacsv.ReadOptions(block_size=64 << 20)
    convert_opts = pacsv.ConvertOptions(
        include_columns=sorted(needed),
        strings_can_be_null=False,
        column_types={name: pa.string() for name in needed})

    men_path = OUT_DIR / "men_1940.parquet"
    child_path = OUT_DIR / "childhood_pre1940.parquet"
    men_writer = pq.ParquetWriter(men_path, MEN_SCHEMA, compression="zstd")
    child_writer = pq.ParquetWriter(child_path, CHILD_SCHEMA, compression="zstd")

    rows_by_year = {}
    n_men = n_child = total = 0
    blank_age = 0

    try:
        with pa.CompressedInputStream(pa.OSFile(str(SOURCE), "rb"), "gzip") as stream:
            reader = pacsv.open_csv(stream, read_options=read_opts,
                                    convert_options=convert_opts)
            for batch in reader:
                year = to_int(batch.column("YEAR"), pa.int16())
                age = to_int(batch.column("AGE"), pa.int16())
                blank_age += pc.sum(pc.cast(pc.is_null(age), pa.int64())).as_py() or 0

                counted = zip(*[c.to_pylist() for c in pc.value_counts(year).flatten()])
                for value, count in counted:
                    if value is not None:
                        rows_by_year[value] = rows_by_year.get(value, 0) + count

                is_1940 = pc.fill_null(pc.equal(year, 1940), False)

                keep = pc.and_(
                    pc.and_(is_1940, between(age, MEN_AGE_MIN, MEN_AGE_MAX)),
                    pc.and_(
                        pc.and_(pc.equal(to_int(batch.column("SEX"), pa.int8()), 1),
                                pc.equal(to_int(batch.column("RACE"), pa.int8()), 1)),
                        between(to_int(batch.column("BPL"), pa.int16()), 1, BPL_US_MAX)))
                keep = pc.fill_null(keep, False)
                if pc.any(keep).as_py():
                    sub = batch.filter(keep)
                    men_writer.write_batch(project(sub, MEN_SCHEMA))
                    n_men += sub.num_rows

                keep = pc.fill_null(
                    pc.and_(pc.invert(is_1940), between(age, CHILD_AGE_MIN, CHILD_AGE_MAX)),
                    False)
                if pc.any(keep).as_py():
                    sub = batch.filter(keep)
                    child_writer.write_batch(project(sub, CHILD_SCHEMA))
                    n_child += sub.num_rows

                total += batch.num_rows
                if total % 20_000_000 < batch.num_rows:
                    print(f"  {total:,} rows | men {n_men:,} | childhood {n_child:,} | "
                          f"{(time.time() - started) / 60:.1f} min", flush=True)
                if MAX_ROWS and total >= MAX_ROWS:
                    break
    finally:
        men_writer.close()
        child_writer.close()

    elapsed = time.time() - started
    print(f"\nRead {total:,} rows in {elapsed / 60:.1f} min")
    print(f"  men        {n_men:,}  -> {men_path.name}")
    print(f"  childhood  {n_child:,}  -> {child_path.name}")

    if not MAX_ROWS:
        assert total == EXPECTED_TOTAL, f"row count {total:,} != {EXPECTED_TOTAL:,}"
        for year, expected in EXPECTED_BY_YEAR.items():
            assert rows_by_year.get(year) == expected, (
                f"{year}: {rows_by_year.get(year):,} != {expected:,}")
        assert blank_age == 0, f"{blank_age:,} rows have a blank AGE"
        print("Source assertions passed: row counts by year match the dictionary.")

    manifest = {
        "extracted_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "source": str(SOURCE),
        "source_bytes": SOURCE.stat().st_size,
        "source_sha256": sha256(SOURCE) if not MAX_ROWS else None,
        "elapsed_minutes": round(elapsed / 60, 2),
        "dry_run_max_rows": MAX_ROWS,
        "rows_read": total,
        "rows_by_year": {str(k): v for k, v in sorted(rows_by_year.items())},
        "men_rows": n_men,
        "childhood_rows": n_child,
        "men_filter": {"YEAR": 1940, "AGE": [MEN_AGE_MIN, MEN_AGE_MAX],
                       "yob": [1940 - MEN_AGE_MAX - 1, 1940 - MEN_AGE_MIN - 1],
                       "SEX": 1, "RACE": 1, "BPL": [1, BPL_US_MAX]},
        "childhood_filter": {"YEAR": "<1940", "AGE": [CHILD_AGE_MIN, CHILD_AGE_MAX]},
        "notes": [
            "1940 rows filtered to white US-born men; childhood rows carry SEX/RACE "
            "unfiltered, since the 1940 row is authoritative.",
            "Childhood rows are NOT restricted to the kept men's HIKs -- the file is "
            "year ordered and the 1940 rows arrive last. The build's join drops the rest.",
            "IPUMS codes are raw. COUNTYICP is the FIPS county code times 10 and is "
            "state-dependent; HIGRADED 000/999 = N/A; MIGRATE5 0 = N/A, 9 = unknown.",
        ],
    }
    (OUT_DIR / "extract_manifest.json").write_text(json.dumps(manifest, indent=2),
                                                   encoding="utf-8")
    print(f"Wrote {OUT_DIR / 'extract_manifest.json'}")


if __name__ == "__main__":
    main()

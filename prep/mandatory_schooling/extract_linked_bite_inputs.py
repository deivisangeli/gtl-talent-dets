"""Extract the two inputs the county bite measure needs from the linked census.

One streaming pass over usa_00001.csv.gz (12.2 GB gzipped, 220,979,023 rows) writing
two Parquet files:

  students_1940.parquet     the 1940 rows for birth cohorts 1885-1912 (AGE 27-54),
                            carrying attainment
  candidates_pre1940.parquet  the pre-1940 rows at school age (5-18), carrying the
                            county and state the child lived in, and SCHOOL --
                            contemporaneous attendance, which is the margin a
                            compulsory-attendance law actually operates on

The file is ordered by census year, 1900 first and 1940 last. That ordering forces
two decisions:

  1. Every column is read as string and cast explicitly. Type inference from the
     first block makes a 1940-only column (HIGRADED, EDUC) null, and the cast then
     fails on its first real value 200 M rows later.
  2. A single pass CANNOT restrict the candidate rows to HIKs that survive the
     student filter, because the 1940 rows have not been seen yet. So only the
     cheap filters above are applied here and the join in build_county_bite.R
     drops the rest.

Sample restrictions are deliberately NOT applied. SEX and RACE are carried as
columns so the CLS-comparable sample (white men) and the full population both come
out of one pass rather than two over 12 GB. MOMLOC, POPLOC and GQ are carried for
the same reason: the merge specification wants a co-resident parent because it is
approximating a BIRTH county, whereas this measure wants residence at SCHOOL age,
and institutionalised children are the population compulsory schooling most
concerns. Filtering them here would foreclose that choice.

Run from any working directory:
    python prep/mandatory_schooling/extract_linked_bite_inputs.py

Environment:
    IPUMS_LINKED_CSV       path to the extract
                           (default: ~/OneDrive/Documentos/usa_00001.csv.gz)
    IPUMS_BITE_MAX_ROWS    stop after N rows (dry run; default: all)
    TALENT_DETS_DATA_DIR / DET_DIR    Dropbox project root

Outputs (output/mandatory_schooling/county_bite/inputs/):
    students_1940.parquet
    candidates_pre1940.parquet
    extract_manifest.json
"""

import csv
import gzip
import hashlib
import json
import os
import sys
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


OUT_DIR = resolve_data_root() / "output" / "mandatory_schooling" / "county_bite" / "inputs"

# --------------------------------------------------------------------------- #
# Sample window and guards
# --------------------------------------------------------------------------- #

# yob = 1940 - AGE - 1, so AGE 27-54 is birth cohorts 1885-1912 -- the CLS
# estimation window. Bounded by this, not by reachability.
STUDENT_AGE_MIN, STUDENT_AGE_MAX = 27, 54

# The statutory windows run entry 6-10 to exit 12-18, so 5-18 covers every age at
# which any state's law could have applied. Narrowing happens downstream, where the
# window is known per state-year.
CAND_AGE_MIN, CAND_AGE_MAX = 5, 18

# Established by a full pass in profile_ipums_linked_census.py. The run fails if
# they no longer hold, which is how a swapped or truncated extract gets caught.
EXPECTED_TOTAL = 220_979_023
EXPECTED_BY_YEAR = {
    1900: 12_971_952, 1910: 23_984_383, 1920: 39_982_220,
    1930: 70_207_628, 1940: 73_832_840,
}

STUDENT_SCHEMA = pa.schema([
    ("HIK", pa.string()),
    ("AGE", pa.int16()),
    ("SEX", pa.int8()),
    ("RACE", pa.int8()),
    ("BPL", pa.int16()),
    ("HIGRADED", pa.int16()),
    ("EDUC", pa.int16()),
])

CANDIDATE_SCHEMA = pa.schema([
    ("HIK", pa.string()),
    ("YEAR", pa.int16()),
    ("AGE", pa.int16()),
    ("SEX", pa.int8()),
    ("RACE", pa.int8()),
    # SCHOOL is the attendance question, present in all five census years at 100%
    # fill. It is the margin a compulsory-ATTENDANCE law actually operates on,
    # observed contemporaneously on the childhood row rather than inferred from
    # attainment forty years later. Its reference period changes across censuses
    # (~12 months in 1900, ~4 in 1920, ~1 month in 1940), so it is comparable
    # WITHIN a census year and not across them.
    ("SCHOOL", pa.int8()),
    ("STATEFIP", pa.int16()),
    ("COUNTYICP", pa.int32()),
    ("MOMLOC", pa.int16()),
    ("POPLOC", pa.int16()),
    ("GQ", pa.int8()),
])


def read_header(path):
    """Column names, without parsing the body."""
    with gzip.open(path, "rt") as handle:
        return next(csv.reader(handle))


def to_int(array, dtype):
    """Cast a string column, treating blank as null rather than as an error."""
    trimmed = pc.utf8_trim_whitespace(array)
    blanked = pc.if_else(pc.equal(trimmed, ""), pa.scalar(None, pa.string()), trimmed)
    return pc.cast(blanked, dtype)


def project(batch, schema, ints):
    """Build a batch matching `schema`, casting the named integer columns."""
    columns = []
    for field in schema:
        column = batch.column(field.name)
        columns.append(to_int(column, field.type) if field.name in ints else column)
    return pa.RecordBatch.from_arrays(columns, schema=schema)


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1 << 22), b""):
            digest.update(block)
    return digest.hexdigest()


def main():
    started = time.time()
    if not SOURCE.is_file():
        raise FileNotFoundError(f"Extract not found: {SOURCE}\nSet IPUMS_LINKED_CSV.")
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    header = read_header(SOURCE)
    needed = set(STUDENT_SCHEMA.names) | set(CANDIDATE_SCHEMA.names)
    missing = sorted(needed - set(header))
    if missing:
        raise ValueError(f"columns absent from the extract: {', '.join(missing)}")

    print(f"Source : {SOURCE}  ({SOURCE.stat().st_size / 1e9:.2f} GB gzipped)")
    print(f"Output : {OUT_DIR}")
    if MAX_ROWS:
        print(f"DRY RUN: stopping after {MAX_ROWS:,} rows")

    student_ints = set(STUDENT_SCHEMA.names) - {"HIK"}
    candidate_ints = set(CANDIDATE_SCHEMA.names) - {"HIK"}

    read_opts = pacsv.ReadOptions(block_size=64 << 20)
    convert_opts = pacsv.ConvertOptions(
        strings_can_be_null=False,
        column_types={name: pa.string() for name in header})

    students_path = OUT_DIR / "students_1940.parquet"
    candidates_path = OUT_DIR / "candidates_pre1940.parquet"
    student_writer = pq.ParquetWriter(students_path, STUDENT_SCHEMA, compression="zstd")
    candidate_writer = pq.ParquetWriter(candidates_path, CANDIDATE_SCHEMA, compression="zstd")

    rows_by_year = {}
    n_students = n_candidates = total = 0
    age_out_of_range = 0

    try:
        with pa.CompressedInputStream(pa.OSFile(str(SOURCE), "rb"), "gzip") as stream:
            reader = pacsv.open_csv(stream, read_options=read_opts,
                                    convert_options=convert_opts)
            for batch in reader:
                year = to_int(batch.column("YEAR"), pa.int16())
                age = to_int(batch.column("AGE"), pa.int16())
                age_out_of_range += pc.sum(pc.cast(pc.is_null(age), pa.int64())).as_py() or 0

                counted = zip(*[c.to_pylist() for c in pc.value_counts(year).flatten()])
                for value, count in counted:
                    if value is not None:
                        rows_by_year[value] = rows_by_year.get(value, 0) + count

                is_1940 = pc.fill_null(pc.equal(year, 1940), False)

                keep = pc.and_(is_1940,
                               pc.and_(pc.greater_equal(age, STUDENT_AGE_MIN),
                                       pc.less_equal(age, STUDENT_AGE_MAX)))
                keep = pc.fill_null(keep, False)
                if pc.sum(pc.cast(keep, pa.int64())).as_py():
                    sub = batch.filter(keep)
                    student_writer.write_batch(project(sub, STUDENT_SCHEMA, student_ints))
                    n_students += sub.num_rows

                keep = pc.and_(pc.invert(is_1940),
                               pc.and_(pc.greater_equal(age, CAND_AGE_MIN),
                                       pc.less_equal(age, CAND_AGE_MAX)))
                keep = pc.fill_null(keep, False)
                if pc.sum(pc.cast(keep, pa.int64())).as_py():
                    sub = batch.filter(keep)
                    candidate_writer.write_batch(
                        project(sub, CANDIDATE_SCHEMA, candidate_ints))
                    n_candidates += sub.num_rows

                total += batch.num_rows
                if total % 20_000_000 < batch.num_rows:
                    print(f"  {total:,} rows | students {n_students:,} | "
                          f"candidates {n_candidates:,} | "
                          f"{(time.time() - started) / 60:.1f} min", flush=True)
                if MAX_ROWS and total >= MAX_ROWS:
                    break
    finally:
        student_writer.close()
        candidate_writer.close()

    elapsed = time.time() - started
    print(f"\nRead {total:,} rows in {elapsed / 60:.1f} min")
    print(f"  students   {n_students:,}  -> {students_path.name}")
    print(f"  candidates {n_candidates:,}  -> {candidates_path.name}")

    if not MAX_ROWS:
        assert total == EXPECTED_TOTAL, f"row count {total:,} != {EXPECTED_TOTAL:,}"
        for year, expected in EXPECTED_BY_YEAR.items():
            assert rows_by_year.get(year) == expected, (
                f"{year}: {rows_by_year.get(year):,} != {expected:,}")
        assert age_out_of_range == 0, f"{age_out_of_range:,} rows have a blank AGE"
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
        "students_rows": n_students,
        "candidates_rows": n_candidates,
        "student_filter": {"YEAR": 1940, "AGE": [STUDENT_AGE_MIN, STUDENT_AGE_MAX],
                           "yob": [1940 - STUDENT_AGE_MAX - 1, 1940 - STUDENT_AGE_MIN - 1]},
        "candidate_filter": {"YEAR": "<1940", "AGE": [CAND_AGE_MIN, CAND_AGE_MAX]},
        "notes": [
            "No sample restriction on SEX, RACE or BPL: applied downstream so one pass "
            "serves both the CLS-comparable and full-population variants.",
            "MOMLOC/POPLOC/GQ carried, not filtered: this measure wants residence at "
            "school age, not a proxy for birth county.",
            "Candidates are NOT restricted to student HIKs -- the file is ordered by "
            "census year and the 1940 rows arrive last. The join drops non-matches.",
            "IPUMS codes are raw. COUNTYICP is the FIPS county code times 10 and is "
            "state-dependent; HIGRADED 999 = N/A.",
        ],
    }
    (OUT_DIR / "extract_manifest.json").write_text(json.dumps(manifest, indent=2),
                                                   encoding="utf-8")
    print(f"Wrote {OUT_DIR / 'extract_manifest.json'}")


if __name__ == "__main__":
    main()

"""Convert the IPUMS 1940 full-count extract (EXT1940USCB.dat.gz) to Parquet.

The source is a gzipped hierarchical fixed-width file: household records ("H",
159 chars) each followed by their person records ("P", 273 chars). It ships with
no codebook, so the field layout lives in ext1940uscb_layout.csv next to this
script, transcribed from IPUMS' published EXT1940USCB.sps / .cbk for this exact
extract and cross-checked against the bytes.

Output is a Hive-partitioned Parquet tree (partitioned by STATEFIP) that
arrow::open_dataset() reads directly:

    <root>/person/statefip=01/part-000.parquet
    <root>/household/statefip=01/part-000.parquet
    <root>/_docs/, <root>/_manifest.json

Person rows carry their household's geography (hh_* columns) so the person table
is usable without a join, plus two derived helpers: county_geoid and
birthyr_approx. All IPUMS fields keep their raw codes - 9-fills are NOT nulled.

Run from any working directory:
    python prep/mandatory_schooling/convert_ipums_1940_fullcount.py

Environment:
    IPUMS_1940_PARQUET_DIR  output root (default: D:\\ipums_1940_fullcount)
    IPUMS_1940_SOURCE       explicit path to EXT1940USCB.dat.gz
    IPUMS_1940_MAX_RECORDS  stop after N records (dry run; default: all)
    IPUMS_1940_CHUNK_MB     decompressed chunk size in MB (default: 256)
"""

import csv
import gzip
import json
import os
import re
import shutil
import time
import urllib.request
from pathlib import Path

import numpy as np
import pyarrow as pa
import pyarrow.parquet as pq

LAYOUT_CSV = Path(__file__).resolve().parent / "ext1940uscb_layout.csv"

SOURCE_BASENAME = "EXT1940USCB.dat.gz"
IPUMS_DOC_URLS = {
    "EXT1940USCB.sps": "https://usa.ipums.org/usa/resources/1940CensusDASTestData/EXT1940USCB.sps",
    "EXT1940USCB.cbk": "https://usa.ipums.org/usa/resources/1940CensusDASTestData/EXT1940USCB.cbk",
}

OUTPUT_ROOT = Path(os.environ.get("IPUMS_1940_PARQUET_DIR", r"D:\ipums_1940_fullcount"))
MAX_RECORDS = int(os.environ.get("IPUMS_1940_MAX_RECORDS", "0")) or None
CHUNK_BYTES = int(os.environ.get("IPUMS_1940_CHUNK_MB", "256")) * 1024 * 1024

RECORD_WIDTH = {"H": 159, "P": 273}
# Household geography copied onto every person row.
CARRY_FIELDS = ["REGION", "STATEICP", "STATEFIP", "COUNTY", "URBAN", "METRO", "GQ", "FARM"]
ROWS_PER_FILE = 8_000_000
ROW_GROUP_SIZE = 500_000

# Sentinels that must hold on every record if the layout is aligned.
CENSUS_YEAR = 1940
CPI99_1940 = 11986
CNTRY_US = 840


# --------------------------------------------------------------------------- #
# Layout
# --------------------------------------------------------------------------- #

def load_layout():
    """Read the layout CSV into {'H': [field, ...], 'P': [...]} and re-check it."""
    fields = {"H": [], "P": []}
    with LAYOUT_CSV.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            fields[row["rectype"]].append({
                "name": row["varname"],
                "start": int(row["start"]),
                "end": int(row["end"]),
                "width": int(row["width"]),
                "dtype": row["dtype"],
                "implied_decimals": int(row["implied_decimals"]),
            })
    for rectype, spec in fields.items():
        expected = 1
        for field in spec:
            if field["start"] != expected:
                raise ValueError(
                    f"{rectype}/{field['name']}: gap or overlap at column {field['start']}")
            expected = field["end"] + 1
        if expected - 1 != RECORD_WIDTH[rectype]:
            raise ValueError(
                f"{rectype}: layout spans {expected - 1} chars, record is {RECORD_WIDTH[rectype]}")
    return fields


def field_by_name(spec, name):
    for field in spec:
        if field["name"] == name:
            return field
    raise KeyError(name)


def parse_sps_layout(text):
    """Extract {'H': [(name, start, end)], 'P': [...]} from an IPUMS SPSS command file."""
    blocks = {}
    pattern = r'record type\s+"([HP])"\.(.*?)(?=record type\s+"|\Z)'
    for match in re.finditer(pattern, text, re.S | re.I):
        rectype, body = match.group(1).upper(), match.group(2)
        blocks[rectype] = [
            (m.group(1), int(m.group(2)), int(m.group(3)))
            for m in re.finditer(r"^\s*([A-Z][A-Z0-9_]*)\s+(\d+)\s*-\s*(\d+)", body, re.M)
        ]
    return blocks


def cross_check_against_sps(fields, sps_path):
    """Fail loudly if the checked-in layout disagrees with IPUMS' own command file.

    A silent off-by-one in the spec is the one error that corrupts every column
    without raising anything, so it gets its own check.
    """
    blocks = parse_sps_layout(sps_path.read_text(encoding="utf-8", errors="replace"))
    if not blocks:
        print(f"  ! {sps_path.name} parsed to nothing - skipping cross-check")
        return False
    for rectype, spec in fields.items():
        ours = [(f["name"], f["start"], f["end"]) for f in spec]
        theirs = blocks.get(rectype, [])
        if ours != theirs:
            padded = zip(ours + [None] * len(theirs), theirs + [None] * len(ours))
            mismatch = next((a, b) for a, b in padded if a != b)
            raise ValueError(
                f"layout CSV disagrees with {sps_path.name} for record type {rectype}: "
                f"ours={mismatch[0]} theirs={mismatch[1]}")
    print(f"  layout cross-checked against {sps_path.name}: identical")
    return True


# --------------------------------------------------------------------------- #
# Paths
# --------------------------------------------------------------------------- #

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


def resolve_source():
    explicit = os.environ.get("IPUMS_1940_SOURCE")
    if explicit:
        path = Path(explicit)
        if not path.is_file():
            raise FileNotFoundError(f"IPUMS_1940_SOURCE does not exist: {path}")
        return path
    root = resolve_data_root()
    # The repo documents raw/ at the project root; this file sits under Data/raw/.
    candidates = [
        root / "Data" / "raw" / "mandatory_schooling" / SOURCE_BASENAME,
        root / "raw" / "mandatory_schooling" / SOURCE_BASENAME,
    ]
    for candidate in candidates:
        if candidate.is_file():
            return candidate
    raise FileNotFoundError(
        f"{SOURCE_BASENAME} not found. Looked in:\n  "
        + "\n  ".join(str(c) for c in candidates))


def fetch_docs(docs_dir):
    """Cache IPUMS' published command files alongside the output, for provenance."""
    docs_dir.mkdir(parents=True, exist_ok=True)
    for name, url in IPUMS_DOC_URLS.items():
        target = docs_dir / name
        if target.is_file():
            continue
        try:
            with urllib.request.urlopen(url, timeout=60) as response:
                target.write_bytes(response.read())
            print(f"  downloaded {name}")
        except Exception as exc:  # offline is survivable; the layout CSV is authoritative
            print(f"  ! could not download {name}: {exc}")


# --------------------------------------------------------------------------- #
# Fixed-width parsing
# --------------------------------------------------------------------------- #

def arrow_type(width):
    if width == 1:
        return pa.int8()
    if width <= 4:
        return pa.int16()
    if width <= 9:
        return pa.int32()
    return pa.int64()


NUMPY_FOR_ARROW = {
    pa.int8(): np.int8,
    pa.int16(): np.int16,
    pa.int32(): np.int32,
    pa.int64(): np.int64,
}


def numpy_type(width):
    return NUMPY_FOR_ARROW[arrow_type(width)]


class RecordView:
    """Column-wise reader over records at arbitrary offsets in a byte buffer.

    Gathers one character position at a time (buf[starts + k]) rather than
    materialising an (n_records x record_width) matrix, which for a 256 MB chunk
    would cost gigabytes of index array.
    """

    def __init__(self, buf, starts):
        self.buf = buf
        self.starts = starts
        self.n = starts.size
        self._idx = np.empty_like(starts)

    def char(self, offset):
        np.add(self.starts, offset, out=self._idx)
        return self.buf[self._idx]

    def digits(self, start, width, dtype):
        """Parse a zero-padded decimal field (1-based `start`) into `dtype`."""
        acc = np.zeros(self.n, dtype=np.int64 if width > 9 else np.int32)
        scratch = np.empty(self.n, dtype=acc.dtype)
        bad = None
        for offset in range(start - 1, start - 1 + width):
            column = self.char(offset)
            invalid = (column < 48) | (column > 57)
            if invalid.any():
                bad = invalid if bad is None else (bad | invalid)
            np.subtract(column, 48, out=scratch, dtype=acc.dtype, casting="unsafe")
            acc *= 10
            acc += scratch
        if bad is not None:
            raise ValueError(
                f"non-digit characters in {int(bad.sum())} record(s) at columns "
                f"{start}-{start + width - 1}; layout is misaligned")
        return acc.astype(dtype, copy=False)

    def text(self, start, width):
        """Gather a fixed-width character field into an Arrow string array."""
        matrix = np.empty((self.n, width), dtype=np.uint8)
        for k in range(width):
            matrix[:, k] = self.char(start - 1 + k)
        offsets = np.arange(0, (self.n + 1) * width, width, dtype=np.int32)
        return pa.Array.from_buffers(
            pa.string(), self.n,
            [None, pa.py_buffer(offsets), pa.py_buffer(matrix.tobytes())])


def geoid_array(statefip, county):
    """5-char FIPS GEOID from STATEFIP and IPUMS COUNTY (= FIPS county code x 10).

    Null where COUNTY is 0 (county not identified) or not a multiple of 10.
    """
    valid = (county > 0) & (county % 10 == 0)
    code = statefip.astype(np.int32) * 1000 + (county.astype(np.int32) // 10)
    n = code.size
    matrix = np.empty((n, 5), dtype=np.uint8)
    remainder = code.copy()
    for k in range(4, -1, -1):
        matrix[:, k] = 48 + (remainder % 10).astype(np.uint8)
        remainder //= 10
    matrix[~valid] = 48  # placeholder bytes sitting under the null mask
    offsets = np.arange(0, (n + 1) * 5, 5, dtype=np.int32)
    validity = np.packbits(valid, bitorder="little")
    return pa.Array.from_buffers(
        pa.string(), n,
        [pa.py_buffer(validity), pa.py_buffer(offsets), pa.py_buffer(matrix.tobytes())],
        null_count=int((~valid).sum()))


# --------------------------------------------------------------------------- #
# Schemas and writers
# --------------------------------------------------------------------------- #

def build_schemas(fields):
    household = [
        pa.field(f["name"], pa.string() if f["dtype"] == "string" else arrow_type(f["width"]))
        for f in fields["H"]
    ]
    household.append(pa.field("county_geoid", pa.string()))

    person = [
        pa.field(f["name"], pa.string() if f["dtype"] == "string" else arrow_type(f["width"]))
        for f in fields["P"]
    ]
    person += [
        pa.field(f"hh_{name}", arrow_type(field_by_name(fields["H"], name)["width"]))
        for name in CARRY_FIELDS
    ]
    person.append(pa.field("county_geoid", pa.string()))
    person.append(pa.field("birthyr_approx", pa.int16()))
    return pa.schema(household), pa.schema(person)


class PartitionedWriter:
    """One Parquet writer per statefip, rolling to a new part file past ROWS_PER_FILE."""

    def __init__(self, root, schema):
        self.root = root
        self.schema = schema
        self.writers = {}
        self.rows = {}
        self.parts = {}

    def write(self, statefip, table):
        key = int(statefip)
        if key in self.writers and self.rows[key] + table.num_rows > ROWS_PER_FILE:
            self.writers.pop(key).close()
        if key not in self.writers:
            part = self.parts.get(key, -1) + 1
            self.parts[key] = part
            directory = self.root / f"statefip={key:02d}"
            directory.mkdir(parents=True, exist_ok=True)
            self.writers[key] = pq.ParquetWriter(
                directory / f"part-{part:03d}.parquet", self.schema,
                compression="zstd", compression_level=3,
                # HISTID is a unique UUID per person: dictionary-encoding it would
                # build a dictionary as large as the column itself.
                use_dictionary=[n for n in self.schema.names if n != "HISTID"])
            self.rows[key] = 0
        self.writers[key].write_table(table, row_group_size=ROW_GROUP_SIZE)
        self.rows[key] += table.num_rows

    def close(self):
        for writer in self.writers.values():
            writer.close()
        self.writers.clear()


# --------------------------------------------------------------------------- #
# Conversion
# --------------------------------------------------------------------------- #

def iter_blocks(path, max_records=None):
    """Yield (bytes, n_records) blocks of whole records from the gzip stream."""
    remainder = b""
    seen = 0
    with gzip.open(path, "rb") as stream:
        while True:
            data = stream.read(CHUNK_BYTES)
            if not data:
                break
            data = remainder + data
            cut = data.rfind(b"\n")
            if cut == -1:
                remainder = data
                continue
            remainder = data[cut + 1:]
            block = data[:cut + 1]
            count = block.count(b"\n")
            if max_records is not None and seen + count >= max_records:
                keep = max_records - seen
                end = -1
                for _ in range(keep):
                    end = block.index(b"\n", end + 1)
                yield block[:end + 1], keep
                return
            seen += count
            yield block, count
    if remainder.strip():
        raise ValueError(f"file ended mid-record with {len(remainder)} trailing bytes")


def split_records(block, n_records):
    """Return (starts, is_household) for the records in a block, validating widths."""
    buf = np.frombuffer(block, dtype=np.uint8)
    newlines = np.flatnonzero(buf == 10)[:n_records]
    starts = np.empty(n_records, dtype=np.int64)
    starts[0] = 0
    starts[1:] = newlines[:-1] + 1
    lengths = newlines - starts

    first = buf[starts]
    if not np.isin(first, [ord("H"), ord("P")]).all():
        raise ValueError("record type other than H/P")
    is_h = first == ord("H")
    if not (lengths[is_h] == RECORD_WIDTH["H"]).all():
        raise ValueError("household record of unexpected length")
    if not (lengths[~is_h] == RECORD_WIDTH["P"]).all():
        raise ValueError("person record of unexpected length")
    return buf, starts, is_h


def convert():
    started = time.time()
    fields = load_layout()
    source = resolve_source()
    docs_dir = OUTPUT_ROOT / "_docs"

    print(f"Source : {source}  ({source.stat().st_size / 1e9:.2f} GB gzipped)")
    print(f"Output : {OUTPUT_ROOT}")
    if MAX_RECORDS:
        print(f"DRY RUN: stopping after {MAX_RECORDS:,} records")
    print("Documentation:")
    fetch_docs(docs_dir)
    sps = docs_dir / "EXT1940USCB.sps"
    checked = cross_check_against_sps(fields, sps) if sps.is_file() else False
    if not checked:
        print("  ! layout NOT cross-checked against the IPUMS command file")
    shutil.copy(LAYOUT_CSV, docs_dir / "layout.csv")

    hh_schema, person_schema = build_schemas(fields)
    hh_writer = PartitionedWriter(OUTPUT_ROOT / "household", hh_schema)
    person_writer = PartitionedWriter(OUTPUT_ROOT / "person", person_schema)
    histid = field_by_name(fields["P"], "HISTID")

    n_households = n_persons = 0
    numprec_total = 0
    state_persons, state_households = {}, {}
    domain = {"sex_out_of_range": 0, "county_unidentified": 0,
              "age_missing": 0, "numprec_ne_numperhh": 0}
    # Geography of the last household seen, for persons whose household record
    # sat in the previous block.
    pending_carry = None

    for block_index, (block, n_records) in enumerate(iter_blocks(source, MAX_RECORDS)):
        try:
            buf, starts, is_h = split_records(block, n_records)
        except ValueError as exc:
            raise ValueError(f"block {block_index}: {exc}") from exc

        hh_view = RecordView(buf, starts[is_h])
        person_view = RecordView(buf, starts[~is_h])

        hh_columns = {
            f["name"]: hh_view.digits(f["start"], f["width"], numpy_type(f["width"]))
            for f in fields["H"] if f["dtype"] != "string"
        }
        person_columns = {
            f["name"]: person_view.digits(f["start"], f["width"], numpy_type(f["width"]))
            for f in fields["P"] if f["dtype"] != "string"
        }

        if hh_view.n:
            for name, expected in (("YEAR", CENSUS_YEAR), ("CPI99", CPI99_1940),
                                   ("CNTRY", CNTRY_US)):
                if (hh_columns[name] != expected).any():
                    raise ValueError(f"block {block_index}: household {name} != {expected}")
            numprec_total += int(hh_columns["NUMPREC"].sum())
            domain["numprec_ne_numperhh"] += int(
                (hh_columns["NUMPREC"] != hh_columns["NUMPERHH"]).sum())
        if person_view.n and (person_columns["YEAR"] != CENSUS_YEAR).any():
            raise ValueError(f"block {block_index}: person YEAR != {CENSUS_YEAR}")

        # Map each person to its household: the most recent H record before it.
        # Index 0 of the extended array is the carry-over from the previous block.
        hh_index = np.cumsum(is_h)[~is_h]
        carry = {}
        for name in CARRY_FIELDS + ["SERIAL"]:
            previous = pending_carry[name] if pending_carry is not None else 0
            extended = np.concatenate((np.array([previous], dtype=hh_columns[name].dtype),
                                       hh_columns[name]))
            carry[name] = extended[hh_index]
        if person_view.n:
            slipped = int((carry["SERIAL"] != person_columns["SERIAL"]).sum())
            if slipped:
                raise ValueError(
                    f"block {block_index}: household geography slipped - carried SERIAL "
                    f"does not match person SERIAL on {slipped} row(s)")
        if hh_view.n:
            pending_carry = {name: hh_columns[name][-1] for name in CARRY_FIELDS + ["SERIAL"]}

        if hh_view.n:
            hh_arrays = {name: pa.array(values) for name, values in hh_columns.items()}
            hh_arrays["RECTYPE"] = hh_view.text(1, 1)
            hh_arrays["county_geoid"] = geoid_array(hh_columns["STATEFIP"], hh_columns["COUNTY"])
            for state in np.unique(hh_columns["STATEFIP"]):
                mask = pa.array(hh_columns["STATEFIP"] == state)
                table = pa.table({name: array.filter(mask) for name, array in hh_arrays.items()},
                                 schema=hh_schema)
                hh_writer.write(state, table)
                state_households[int(state)] = state_households.get(int(state), 0) + table.num_rows
            n_households += hh_view.n

        if person_view.n:
            person_arrays = {name: pa.array(values) for name, values in person_columns.items()}
            person_arrays["RECTYPE"] = person_view.text(1, 1)
            person_arrays["HISTID"] = person_view.text(histid["start"], histid["width"])
            for name in CARRY_FIELDS:
                person_arrays[f"hh_{name}"] = pa.array(carry[name])
            person_arrays["county_geoid"] = geoid_array(carry["STATEFIP"], carry["COUNTY"])

            age = person_columns["AGE"]
            age_known = age < 900
            person_arrays["birthyr_approx"] = pa.array(
                (CENSUS_YEAR - age.astype(np.int16)), mask=~age_known)

            domain["age_missing"] += int((~age_known).sum())
            domain["sex_out_of_range"] += int((~np.isin(person_columns["SEX"], [1, 2])).sum())
            domain["county_unidentified"] += int((carry["COUNTY"] == 0).sum())

            for state in np.unique(carry["STATEFIP"]):
                mask = pa.array(carry["STATEFIP"] == state)
                table = pa.table({name: array.filter(mask) for name, array in person_arrays.items()},
                                 schema=person_schema)
                person_writer.write(state, table)
                state_persons[int(state)] = state_persons.get(int(state), 0) + table.num_rows
            n_persons += person_view.n

        if block_index % 20 == 0:
            elapsed = (time.time() - started) / 60
            print(f"  block {block_index:5d} | {n_persons:12,} persons | "
                  f"{n_households:11,} households | {elapsed:6.1f} min", flush=True)

    hh_writer.close()
    person_writer.close()
    elapsed = time.time() - started

    manifest = {
        "source": str(source),
        "source_bytes": source.stat().st_size,
        "converted_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "elapsed_minutes": round(elapsed / 60, 2),
        "dry_run_max_records": MAX_RECORDS,
        "layout_cross_checked_against_sps": checked,
        "households": n_households,
        "persons": n_persons,
        "numprec_sum": numprec_total,
        "persons_per_state": dict(sorted(state_persons.items())),
        "households_per_state": dict(sorted(state_households.items())),
        "domain_counters": domain,
        "implied_decimals": {f["name"]: f["implied_decimals"]
                             for spec in fields.values() for f in spec
                             if f["implied_decimals"]},
        "notes": [
            "IPUMS codes are stored raw; 9-fills are not converted to nulls.",
            "COUNTY and MIGCOUNTY are FIPS county codes multiplied by 10.",
            "county_geoid is derived: STATEFIP*1000 + COUNTY/10, null where COUNTY==0.",
            "birthyr_approx = 1940 - AGE. Enumeration was 1 April 1940, so anyone with a "
            "birthday between 2 April and 31 December was in fact born the prior year.",
            "This extract has no date of birth and no county of birth: the 1940 schedule "
            "recorded age and state/country of birth only.",
        ],
    }
    (OUTPUT_ROOT / "_manifest.json").write_text(json.dumps(manifest, indent=2), encoding="utf-8")

    print(f"\nDone in {elapsed / 60:.1f} min")
    print(f"  households {n_households:,}   persons {n_persons:,}")
    print(f"  sum(NUMPREC) {numprec_total:,}  (should equal the person count)")
    print(f"  domain counters: {domain}")
    print(f"Wrote {OUTPUT_ROOT / '_manifest.json'}")


def main():
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    convert()


if __name__ == "__main__":
    main()

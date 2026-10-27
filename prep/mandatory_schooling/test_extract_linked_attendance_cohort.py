"""Checks for extract_linked_attendance_cohort_inputs.py.

Builds a small gzipped CSV shaped like the real linked extract -- same column names
and the same ordering, 1900 rows first and 1940 last -- runs the real extractor over
it, and reads both Parquet outputs back. Each block names the failure mode it rules
out. The ordering matters: it is what makes Arrow infer a 1940-only column as null
from the first block.

Run from any working directory:
    python prep/mandatory_schooling/test_extract_linked_attendance_cohort.py
"""

import gzip
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

import pyarrow.parquet as pq

HERE = Path(__file__).resolve().parent
EXTRACTOR = HERE / "extract_linked_attendance_cohort_inputs.py"

COLUMNS = ["YEAR", "HIK", "AGE", "SEX", "RACE", "BPL", "HIGRADED", "EDUC", "SCHOOL",
           "STATEFIP", "COUNTYICP", "MIGRATE5", "MIGPLAC5", "GQ", "LIT"]

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
    except Exception as exc:
        if fragment.lower() in str(exc).lower():
            print(f"  PASS  {label}")
            return
        print(f"    raised {type(exc).__name__}: {exc}")
    else:
        print("    did not raise")
    FAILURES += 1
    print(f"  FAIL  {label}")


def row(year, hik, age, **over):
    """One CSV row. 1940-only columns are blank before 1940, as in the real extract."""
    only_1940 = year < 1940
    values = {
        "YEAR": year, "HIK": hik, "AGE": age, "SEX": 1, "RACE": 1, "BPL": 1,
        "HIGRADED": "" if only_1940 else 110,
        "EDUC": "" if only_1940 else 2,
        "SCHOOL": 1,
        "STATEFIP": 1, "COUNTYICP": 310,
        "MIGRATE5": "" if only_1940 else 1,
        "MIGPLAC5": "" if only_1940 else 0,
        "GQ": 1, "LIT": "" if not only_1940 else 4,
    }
    values.update(over)
    return ",".join(str(values[c]) for c in COLUMNS)


def write_fixture(path, columns=COLUMNS, rows=None):
    rows = FIXTURE_ROWS if rows is None else rows
    with gzip.open(path, "wt", newline="") as handle:
        handle.write(",".join(columns) + "\n")
        for line in rows:
            handle.write(line + "\n")
    return path


FIXTURE_ROWS = [
    row(1900, "child-age0", 0),            # birth-year record, kept
    row(1900, "child-age18", 18),          # upper bound, kept
    row(1900, "drop-child-19", 19),        # an adult, dropped
    row(1910, "child-female", 9, SEX=2),   # sex not filtered on childhood rows
    row(1920, "child-black", 7, RACE=2),   # nor race
    row(1930, "child-1930", 12, STATEFIP=2, COUNTYICP=50),
    row(1940, "man-lo", 19),               # yob 1920
    row(1940, "man-hi", 49),               # yob 1890
    row(1940, "drop-man-18", 18),
    row(1940, "drop-man-50", 50),
    row(1940, "drop-female", 30, SEX=2),
    row(1940, "drop-black", 30, RACE=2),
    row(1940, "drop-foreign", 30, BPL=410),
    row(1940, "man-bpl56", 30, BPL=56),
    row(1940, "man-999", 30, HIGRADED=999),   # kept here, dropped in R
    row(1940, "drop-1940-child", 10),          # 1940 rows never enter the childhood table
    row(1940, "man-migrant", 30, MIGRATE5=3, MIGPLAC5=6),
]


def run_extractor(source, data_root):
    env = dict(os.environ)
    env["IPUMS_LINKED_CSV"] = str(source)
    env["TALENT_DETS_DATA_DIR"] = str(data_root)
    # A cap above the fixture size skips the real row-count assertions.
    env["IPUMS_BITE_MAX_ROWS"] = "1000000"
    env.pop("DET_DIR", None)
    result = subprocess.run([sys.executable, str(EXTRACTOR)], env=env,
                            capture_output=True, text=True)
    if result.returncode != 0:
        raise RuntimeError(result.stderr.strip() or result.stdout.strip())
    return result.stdout


def main():
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)
        data_root = tmp / "data"
        data_root.mkdir()
        run_extractor(write_fixture(tmp / "linked.csv.gz"), data_root)

        out = data_root / "output" / "mandatory_schooling" / "attendance_cohort" / "inputs"
        men = pq.read_table(out / "men_1940.parquet").to_pylist()
        child = pq.read_table(out / "childhood_pre1940.parquet").to_pylist()
        manifest = json.loads((out / "extract_manifest.json").read_text(encoding="utf-8"))
        m_hiks = {r["HIK"] for r in men}
        c_hiks = {r["HIK"] for r in child}

        print("\nMen (1940 rows)")
        check("AGE 19 and 49 are kept -- yob 1920 and 1890",
              lambda: {"man-lo", "man-hi"} <= m_hiks)
        check("AGE 18 and 50 are excluded",
              lambda: not {"drop-man-18", "drop-man-50"} & m_hiks)
        check("women and non-white men are excluded",
              lambda: not {"drop-female", "drop-black"} & m_hiks)
        check("foreign-born are excluded, BPL 56 is kept",
              lambda: "drop-foreign" not in m_hiks and "man-bpl56" in m_hiks)
        check("HIGRADED 999 is NOT dropped here -- that is the R step's job",
              lambda: "man-999" in m_hiks)
        check("migrants are NOT dropped here -- flags are built in R",
              lambda: "man-migrant" in m_hiks)
        check("exactly five men", lambda: len(men) == 5)
        check("the 1940-only columns cast despite being blank in every earlier row",
              lambda: all(isinstance(r["HIGRADED"], int) and isinstance(r["MIGRATE5"], int)
                          for r in men))
        check("MIGPLAC5 is carried", lambda: any(r["MIGPLAC5"] == 6 for r in men))

        print("\nChildhood rows")
        check("AGE 0 and AGE 18 are kept", lambda: {"child-age0", "child-age18"} <= c_hiks)
        check("AGE 19 is excluded", lambda: "drop-child-19" not in c_hiks)
        check("childhood rows are NOT filtered on sex or race",
              lambda: {"child-female", "child-black"} <= c_hiks)
        check("no 1940 row enters the childhood table",
              lambda: "drop-1940-child" not in c_hiks and {r["YEAR"] for r in child}
              == {1900, 1910, 1920, 1930})
        check("COUNTYICP is carried raw, still times ten",
              lambda: {r["COUNTYICP"] for r in child} == {310, 50})
        check("HIK stays a string", lambda: all(isinstance(r["HIK"], str) for r in child))

        print("\nManifest")
        check("rows read are recorded", lambda: manifest["rows_read"] == len(FIXTURE_ROWS))
        check("output counts match the tables",
              lambda: manifest["men_rows"] == len(men)
              and manifest["childhood_rows"] == len(child))
        check("the men filter records the cohort window it implies",
              lambda: manifest["men_filter"]["yob"] == [1890, 1920])

        print("\nGuards")
        drop = COLUMNS.index("MIGRATE5")
        bad = write_fixture(tmp / "missing.csv.gz",
                            columns=[c for c in COLUMNS if c != "MIGRATE5"],
                            rows=[",".join(r.split(",")[:drop] + r.split(",")[drop + 1:])
                                  for r in FIXTURE_ROWS])
        check_raises("a source missing a needed column is rejected",
                     lambda: run_extractor(bad, data_root), "MIGRATE5")
        check_raises("a source that does not exist is rejected",
                     lambda: run_extractor(tmp / "nope.csv.gz", data_root), "not found")

    print(f"\n{'All checks passed.' if FAILURES == 0 else f'{FAILURES} check(s) FAILED.'}")
    sys.exit(0 if FAILURES == 0 else 1)


if __name__ == "__main__":
    main()

"""Checks for extract_linked_bite_inputs.py.

Builds a small gzipped CSV shaped like the real linked extract -- same column
names, and crucially the same ordering, 1900 rows first and 1940 last -- runs the
real extractor over it, and reads both Parquet outputs back. Each block names the
failure mode it rules out.

The ordering is the point. It is what makes Arrow infer a 1940-only column as
null from the first block, and it is why a single pass cannot restrict the
candidate rows to students. A fixture in the wrong order would pass while the
real file failed.

Run from any working directory:
    python prep/mandatory_schooling/test_county_bite.py
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
EXTRACTOR = HERE / "extract_linked_bite_inputs.py"

COLUMNS = ["YEAR", "HIK", "AGE", "SEX", "RACE", "BPL", "HIGRADED", "EDUC",
           "SCHOOL", "STATEFIP", "COUNTYICP", "MOMLOC", "POPLOC", "GQ", "LIT"]

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
    """One CSV row. Attainment is blank before 1940, as in the real extract."""
    values = {
        "YEAR": year, "HIK": hik, "AGE": age, "SEX": 1, "RACE": 1, "BPL": 1,
        "HIGRADED": "" if year < 1940 else 110,
        "EDUC": "" if year < 1940 else 6,
        "SCHOOL": 2,   # attended; 1 = did not attend
        "STATEFIP": 1, "COUNTYICP": 310, "MOMLOC": 1, "POPLOC": 2, "GQ": 1,
        "LIT": 4,
    }
    values.update(over)
    return ",".join(str(values[c]) for c in COLUMNS)


def write_fixture(path, columns=COLUMNS, rows=None):
    if rows is None:
        rows = FIXTURE_ROWS
    with gzip.open(path, "wt", newline="") as handle:
        handle.write(",".join(columns) + "\n")
        for line in rows:
            handle.write(line + "\n")
    return path


# Ordered by census year, as the real file is. Ages are chosen so each filter is
# exercised at and just outside its boundary.
FIXTURE_ROWS = [
    row(1900, "keep-cand-lo", 5),      # candidate, lower bound
    row(1900, "keep-cand-hi", 18),     # candidate, upper bound
    row(1900, "drop-cand-young", 4),   # below the school-age window
    row(1900, "drop-cand-old", 19),    # above it
    row(1910, "keep-cand-mid", 12),
    row(1920, "keep-cand-mid2", 9, STATEFIP=2, COUNTYICP=50),
    row(1930, "keep-cand-1930", 7),
    row(1940, "keep-student-lo", 27),  # student, lower bound -> yob 1912
    row(1940, "keep-student-hi", 54),  # student, upper bound -> yob 1885
    row(1940, "drop-student-young", 26),
    row(1940, "drop-student-old", 55),
    row(1940, "keep-student-999", 40, HIGRADED=999),  # kept here, dropped in R
]


def run_extractor(source, data_root, extra_env=None):
    env = dict(os.environ)
    env["IPUMS_LINKED_CSV"] = str(source)
    env["TALENT_DETS_DATA_DIR"] = str(data_root)
    # A cap above the fixture size skips the real row-count assertions, which
    # describe the 221 M row source and cannot hold on a fixture.
    env["IPUMS_BITE_MAX_ROWS"] = "1000000"
    env.pop("DET_DIR", None)
    if extra_env:
        env.update(extra_env)
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
        source = write_fixture(tmp / "linked.csv.gz")

        run_extractor(source, data_root)
        out = data_root / "output" / "mandatory_schooling" / "county_bite" / "inputs"
        students = pq.read_table(out / "students_1940.parquet").to_pylist()
        candidates = pq.read_table(out / "candidates_pre1940.parquet").to_pylist()
        manifest = json.loads((out / "extract_manifest.json").read_text(encoding="utf-8"))
        s_hiks = {r["HIK"] for r in students}
        c_hiks = {r["HIK"] for r in candidates}

        print("\nStudent selection")
        check("1940 rows inside AGE 27-54 are kept",
              lambda: {"keep-student-lo", "keep-student-hi", "keep-student-999"} <= s_hiks)
        check("AGE 26 is excluded", lambda: "drop-student-young" not in s_hiks)
        check("AGE 55 is excluded", lambda: "drop-student-old" not in s_hiks)
        check("no pre-1940 row reaches the student table",
              lambda: not any(h.startswith("keep-cand") for h in s_hiks))
        check("exactly three students", lambda: len(students) == 3)
        check("HIGRADED 999 is NOT dropped here -- that is the R step's job",
              lambda: any(r["HIGRADED"] == 999 for r in students))

        print("\nCandidate selection")
        check("pre-1940 rows at ages 5-18 are kept",
              lambda: {"keep-cand-lo", "keep-cand-hi", "keep-cand-mid",
                       "keep-cand-mid2", "keep-cand-1930"} <= c_hiks)
        check("AGE 4 is excluded", lambda: "drop-cand-young" not in c_hiks)
        check("AGE 19 is excluded", lambda: "drop-cand-old" not in c_hiks)
        check("no 1940 row reaches the candidate table",
              lambda: not any(h.startswith("keep-student") for h in c_hiks))
        check("candidates are NOT restricted to student HIKs -- the file is year "
              "ordered, so they cannot be", lambda: len(c_hiks & s_hiks) == 0 and len(c_hiks) == 5)
        check("every candidate census year survives",
              lambda: {r["YEAR"] for r in candidates} == {1900, 1910, 1920, 1930})

        print("\nTyping and blanks")
        check("the 1940-only column casts despite being blank in every earlier row",
              lambda: all(isinstance(r["HIGRADED"], int) for r in students))
        check("integer columns arrive as integers, not strings",
              lambda: all(isinstance(r[c], int)
                          for r in candidates for c in ("YEAR", "AGE", "STATEFIP", "COUNTYICP")))
        check("HIK stays a string", lambda: all(isinstance(r["HIK"], str) for r in candidates))
        check("COUNTYICP is carried raw, still times ten",
              lambda: {r["COUNTYICP"] for r in candidates} == {310, 50})
        check("the parent pointers and GQ are carried, not filtered",
              lambda: all(r["MOMLOC"] == 1 and r["POPLOC"] == 2 and r["GQ"] == 1
                          for r in candidates))
        check("SCHOOL is carried on the childhood rows -- the attendance margin",
              lambda: all(r["SCHOOL"] == 2 for r in candidates))

        print("\nManifest")
        check("row counts are recorded", lambda: manifest["rows_read"] == len(FIXTURE_ROWS))
        check("per-year counts are recorded",
              lambda: manifest["rows_by_year"] == {"1900": 4, "1910": 1, "1920": 1,
                                                   "1930": 1, "1940": 5})
        check("output counts match the tables",
              lambda: manifest["students_rows"] == len(students)
              and manifest["candidates_rows"] == len(candidates))
        check("the student filter records the cohort window it implies",
              lambda: manifest["student_filter"]["yob"] == [1885, 1912])

        print("\nGuards")
        drop = COLUMNS.index("COUNTYICP")
        bad = write_fixture(tmp / "missing.csv.gz",
                            columns=[c for c in COLUMNS if c != "COUNTYICP"],
                            rows=[",".join(r.split(",")[:drop] + r.split(",")[drop + 1:])
                                  for r in FIXTURE_ROWS])
        check_raises("a source missing a needed column is rejected",
                     lambda: run_extractor(bad, data_root), "COUNTYICP")
        check_raises("a source that does not exist is rejected",
                     lambda: run_extractor(tmp / "nope.csv.gz", data_root), "not found")

    print(f"\n{'All checks passed.' if FAILURES == 0 else f'{FAILURES} check(s) FAILED.'}")
    sys.exit(0 if FAILURES == 0 else 1)


if __name__ == "__main__":
    main()

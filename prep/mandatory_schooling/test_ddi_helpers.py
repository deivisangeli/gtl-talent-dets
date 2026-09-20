"""Checks for ddi_helpers.py and the DDI-dependent parts of the profiler.

Builds a small synthetic DDI codebook whose every label and code is known by
construction, then exercises the reader, the two cross-checks, the value-table
renderer and the --labels-only reload. Each block names the failure mode it
rules out.

No real extract and no real codebook are needed, which is the point: the DDI for
usa_00001 has to be fetched by hand from IPUMS, and this has to be testable
before it arrives.

Run from any working directory:
    python prep/mandatory_schooling/test_ddi_helpers.py
"""

import csv
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ddi_helpers  # noqa: E402
import profile_ipums_linked_census as profiler  # noqa: E402

FAILURES = 0

# BPL gets more categories than VALUE_TABLE_CAP so truncation is exercised, and
# they are zero-padded so code_key has something to normalise.
BPL_CODES = {f"{n:03d}": f"Place {n}" for n in range(1, 61)}


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


def build_ddi(path, namespaced=True, sex_label="Sex"):
    ns = ' xmlns="ddi:codebook:2_5"' if namespaced else ""
    cats = "".join(
        f"<catgry><catValu>{code}</catValu><labl>{name}</labl></catgry>"
        for code, name in BPL_CODES.items())
    path.write_text(
        '<?xml version="1.0" encoding="UTF-8"?>'
        f'<codeBook{ns} version="2.5">'
        "<stdyDscr><citation>"
        "<titlStmt><titl>IPUMS USA test extract</titl></titlStmt>"
        "<prodStmt><producer>IPUMS</producer><prodDate>2026-09-20</prodDate></prodStmt>"
        "<notes>Synthetic fixture.</notes>"
        "</citation></stdyDscr>"
        "<dataDscr>"
        '<var ID="YEAR" name="YEAR"><labl>Census year</labl>'
        "<txt>Four-digit census year.</txt></var>"
        f'<var ID="SEX" name="SEX"><labl>{sex_label}</labl>'
        "<catgry><catValu>1</catValu><labl>Male</labl></catgry>"
        "<catgry><catValu>2</catValu><labl>Female</labl></catgry></var>"
        f'<var ID="BPL" name="BPL"><labl>Birthplace</labl>{cats}</var>'
        '<var ID="HIK" name="HIK"><labl>Historical index key</labl></var>'
        "</dataDscr></codeBook>",
        encoding="utf-8")
    return path


def make_labels(sex_label="Sex"):
    return {
        "YEAR": {"varname": "YEAR", "label": "Census year", "group": "identification",
                 "coding_note": "Census year of the record."},
        "SEX": {"varname": "SEX", "label": sex_label, "group": "demographics",
                "coding_note": ""},
        "BPL": {"varname": "BPL", "label": "Birthplace", "group": "birthplace",
                "coding_note": "US states carry their FIPS state code."},
        "HIK": {"varname": "HIK", "label": "Historical index key", "group": "linkage",
                "coding_note": "Cross-year person key."},
    }


def make_stats(observed):
    """{year: {col: ColumnStats}} from {col: {value: count}}, one year."""
    stats = {1940: {}}
    for name in ("YEAR", "SEX", "BPL", "HIK"):
        st = profiler.ColumnStats()
        counts = observed.get(name, {})
        if name in profiler.HIGH_CARDINALITY:
            st.add_opaque(100)
        else:
            st.update_counter(counts)
        stats[1940][name] = st
    return stats


def main():
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)
        ddi_path = build_ddi(tmp / "usa_test.xml")
        ddi = ddi_helpers.read_ddi(ddi_path)
        header = ["YEAR", "SEX", "BPL", "HIK"]

        print("\nDDI parsing")
        check("all four variables are read",
              lambda: sorted(ddi) == ["BPL", "HIK", "SEX", "YEAR"])
        check("variable label is read", lambda: ddi["SEX"]["label"] == "Sex")
        check("free-text description is read",
              lambda: ddi["YEAR"]["description"].startswith("Four-digit"))
        check("value labels are read", lambda: ddi["SEX"]["values"]["1"] == "Male")
        check("a variable with no categories has none",
              lambda: ddi["HIK"]["values"] == {})
        check("the un-namespaced variant parses identically",
              lambda: ddi_helpers.read_ddi(
                  build_ddi(tmp / "plain.xml", namespaced=False))["SEX"]["values"]["2"]
              == "Female")
        check_raises("a non-DDI XML is rejected",
                     lambda: ddi_helpers.read_ddi(
                         build_not_a_ddi(tmp / "bad.xml")), "dataDscr")

        print("\nCode normalisation")
        check("zero padding does not change a code",
              lambda: ddi_helpers.code_key("006") == ddi_helpers.code_key("6"))
        check("a trailing decimal does not change a code",
              lambda: ddi_helpers.code_key("6.0") == "6")
        check("non-numeric codes compare as upper case",
              lambda: ddi_helpers.code_key(" na ") == "NA")
        check("a padded DDI code is reachable by its bare form",
              lambda: ddi["BPL"]["keys"]["6"] == "Place 6")

        print("\nProvenance")
        meta = ddi_helpers.ddi_provenance(ddi_path)
        check("variable count is recorded", lambda: meta["n_vars"] == 4)
        check("sha256 is a full digest", lambda: len(meta["sha256"]) == 64)
        check("the digest is stable across reads",
              lambda: ddi_helpers.ddi_provenance(ddi_path)["sha256"] == meta["sha256"])
        check("the study title is recovered",
              lambda: meta["title"] == "IPUMS USA test extract")

        print("\nLabel cross-check")
        report = profiler.cross_check_labels(ddi, header, make_labels())
        check("a consistent set of labels passes",
              lambda: report["n_reworded"] == 0 and report["n_undocumented"] == 0)
        check_raises("a column absent from the codebook stops the run",
                     lambda: profiler.cross_check_labels(
                         ddi, header + ["GHOSTVAR"], make_labels()), "GHOSTVAR")
        check_raises("a documented variable the codebook lacks stops the run",
                     lambda: profiler.cross_check_labels(
                         ddi, header, dict(make_labels(),
                                           PHANTOM={"label": "x", "group": "y"})),
                     "PHANTOM")
        reworded = profiler.cross_check_labels(ddi, header, make_labels("Sex of person"))
        check("a reworded label is reported, not fatal",
              lambda: reworded["reworded"] == ["SEX"])
        check("punctuation and case alone are not a rewording",
              lambda: profiler.cross_check_labels(
                  ddi, header, make_labels("sex"))["n_reworded"] == 0)
        undocumented = profiler.cross_check_labels(
            ddi, header, {k: v for k, v in make_labels().items() if k != "BPL"})
        check("a column with no project note is reported",
              lambda: undocumented["undocumented"] == ["BPL"])

        print("\nCode cross-check")
        stats = make_stats({"YEAR": {"1940": 100}, "SEX": {"1": 60, "2": 39, "3": 1},
                            "BPL": {"6": 50, "25": 50}})
        codes = profiler.cross_check_codes(ddi, header, stats, [1940])
        check("a code the codebook does not list is flagged",
              lambda: codes["SEX"] == ["3"])
        check("an unpadded observation of a padded code is not flagged",
              lambda: "BPL" not in codes)
        check("a variable with no categories is skipped",
              lambda: "HIK" not in codes and "YEAR" not in codes)

        print("\nValue tables")
        sex_table = profiler.value_table(ddi["SEX"], "SEX", stats, [1940])
        check("a short code list is printed in full",
              lambda: sum(1 for line in sex_table if line.startswith("| 1 |")) == 1
              and any(line.startswith("| 2 |") for line in sex_table))
        bpl_table = profiler.value_table(ddi["BPL"], "BPL", stats, [1940])
        check("a long code list is cut to the observed codes",
              lambda: sum(1 for line in bpl_table if line.startswith("| 0")) == 2)
        check("the cut is declared rather than silent",
              lambda: any("60 codes in the codebook" in line for line in bpl_table))
        check("a variable with no categories gets no table",
              lambda: profiler.value_table(ddi["HIK"], "HIK", stats, [1940]) == [])
        check("top values are decoded",
              lambda: profiler.decode_code(ddi["SEX"], "1") == "`1` Male")
        check("an unlabelled code degrades to the bare code",
              lambda: profiler.decode_code(ddi["SEX"], "3") == "`3`")
        check("no codebook means no decoding and no crash",
              lambda: profiler.decode_code(None, "3") == "`3`")

        print("\nReload from the saved dictionary CSV")
        saved = tmp / "dictionary.csv"
        write_saved_csv(saved)
        original_csv = profiler.OUT_CSV
        profiler.OUT_CSV = saved
        try:
            columns, reloaded, rows_by_year = profiler.load_saved_profile()
            check("column order is preserved", lambda: columns == header)
            check("the row count is recovered from a fully-populated column",
                  lambda: rows_by_year[1940] == 100)
            check("fill counts survive the round trip",
                  lambda: reloaded[1940]["SEX"].n_nonnull == 99)
            check("top values survive the round trip",
                  lambda: reloaded[1940]["SEX"].values["1"] == 60)
            check("min and max are parsed back out of thousands separators",
                  lambda: reloaded[1940]["YEAR"].vmin == 1940.0)
            check("identifier columns are marked opaque again",
                  lambda: reloaded[1940]["HIK"].opaque)

            blank = tmp / "no_full_column.csv"
            write_saved_csv(blank, year_pct="99.0")
            profiler.OUT_CSV = blank
            check_raises("a CSV with no fully-populated column is rejected",
                         profiler.load_saved_profile, "row count cannot be recovered")

            profiler.OUT_CSV = tmp / "absent.csv"
            check_raises("a missing dictionary CSV is rejected",
                         profiler.load_saved_profile, "run a full profile once")
        finally:
            profiler.OUT_CSV = original_csv

        print("\nNo codebook available")
        check("cross-checks record their own absence",
              lambda: profiler.run_cross_checks(None, header, make_labels(), header,
                                                stats, [1940])["ddi_cross_checked"] is False)
        check("the Markdown says the labels are unverified",
              lambda: "unverified" in profiler._provenance_line(
                  None, {"ddi_cross_checked": False}))
        check("with a codebook it cites the digest instead",
              lambda: meta["sha256"][:16] in profiler._provenance_line(
                  meta, {"ddi_cross_checked": True}))

    print(f"\n{'All checks passed.' if FAILURES == 0 else f'{FAILURES} check(s) FAILED.'}")
    sys.exit(0 if FAILURES == 0 else 1)


def build_not_a_ddi(path):
    path.write_text('<?xml version="1.0"?><notACodebook><var name="SEX"/></notACodebook>',
                    encoding="utf-8")
    return path


def write_saved_csv(path, year_pct="100.0"):
    # year_pct below 100 stands for a dictionary in which no column is filled on
    # every row, so every other column has to drop below 100 with it.
    other_pct = "100.0" if year_pct == "100.0" else "99.0"
    rows = [
        ["YEAR", "Census year", "identification", 1940, 100, year_pct, 1, "False",
         "1,940", "1,940", "1940=100"],
        ["SEX", "Sex", "demographics", 1940, 99, "99.0", 3, "False", "1", "3",
         "1=60; 2=38; 3=1"],
        ["BPL", "Birthplace", "birthplace", 1940, 100, other_pct, 2, "False", "6", "25",
         "6=50; 25=50"],
        ["HIK", "Historical index key", "linkage", 1940, 100, other_pct, ">4000", "True",
         "-", "-", ""],
    ]
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(["varname", "label", "group", "year", "n_nonnull", "pct_nonnull",
                         "n_distinct", "distinct_capped", "min", "max", "top_values"])
        writer.writerows(rows)


if __name__ == "__main__":
    main()

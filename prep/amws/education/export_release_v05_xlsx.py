"""Export an AMWS education release (v05 by default, or v06 with --release) to one .xlsx workbook.

Sheets:
  README     what the release is, sheet contents, provenance (source sha256) and key caveats;
  Persons    one row per scientist entry (136,489): the person columns of amws_open_alex.csv,
             without the 705 wide award columns, which the Awards sheet holds in long form;
  Awards     one row per included postsecondary award (377,987), from education_awards_openalex.csv;
  Unmatched  the awards still without a full match (unmatched_awards.csv);
  Columns    a short dictionary of every exported column.

The release folder is checksum-locked, so the workbook is written to a sibling folder:
output/amws/amws_education_postsecondary_v05_20261020_xlsx/.
"""
import csv
import hashlib
import os
from pathlib import Path
import re
import sys

from openpyxl import Workbook
from openpyxl.cell import WriteOnlyCell
from openpyxl.styles import Font, PatternFill
from openpyxl.utils import get_column_letter
from openpyxl.utils.indexed_list import IndexedList

csv.field_size_limit(16 * 1024 * 1024)
ROOT = Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets")))
RELEASES = {"v05": ("amws_education_postsecondary_v05_20261020", "build_postsecondary_release_v05.py", "2026-10-20"),
            "v06": ("amws_education_postsecondary_v06_20261021", "build_postsecondary_release_v06.py", "2026-10-21")}
STAMP, BUILDER, BUILT = RELEASES["v05"]
RELEASE = ROOT / "output/amws" / STAMP
OUT = ROOT / "output/amws" / f"{STAMP}_xlsx"
AWARD_COLUMN = re.compile(r"^[a-z_]+_award_\d+_")

PERSON_NUMERIC = {"birth_year", "lat", "lon", "geo_lat", "geo_lon", "oa_id_count", "oa_award_count", "oa_matched_award_count",
                  "oa_multi_institution_award_count", "reassigned_awards", "manual_assigned_awards",
                  "physical_science", "physical_science_unresolved", "physical_science_mixed"}
AWARD_COLUMNS = ["source_key", "edition", "award_index", "requested_family", "family_award_index", "label_as_written",
                 "institution_as_written", "year", "honorary", "training_eligibility", "fully_matched", "condition_conflict",
                 "oa_ids", "oa_names", "oa_cities", "oa_countries", "oa_latitudes", "oa_longitudes", "assignment_class",
                 "raw_mapping_status", "mapping_origin", "manual_assignment", "manual_confidence", "matching_flags", "flags", "evidence"]
AWARD_NUMERIC = {"award_index", "family_award_index", "year"}
UNMATCHED_NUMERIC = {"award_index", "year", "normalization_candidate_share"}

DESCRIPTIONS = {
    "source_key": "Entry key, edition:line (e.g. 1986:12345). Joins Persons, Awards and Unmatched.",
    "edition": "AMWS edition (blank for 1986 rows in Persons; use the source_key prefix).",
    "birth_year": "Birth year used by the project.",
    "geoid": "Birth county FIPS (1906-1955 editions); text, keeps leading zeros.",
    "geo_geoid": "Birth county FIPS (1986 edition); text, keeps leading zeros.",
    "kept": "Earlier editions: entry retained after cross-edition deduplication.",
    "is_us_birth": "1986 edition: born in the United States.",
    "raw_text_adjusted": "Full biography text as parsed.",
    "education_status": "Outcome of the education parse for this entry.",
    "oa_ids": "JSON list of every OpenAlex institution ID across the entry's awards.",
    "oa_id_count": "Number of distinct OpenAlex institutions across the entry's awards.",
    "oa_award_count": "Number of included postsecondary awards.",
    "oa_matched_award_count": "Awards with at least one OpenAlex institution.",
    "manual_assigned_awards": "Awards matched by the v05 manual crosswalk or medical-campus overrides.",
    "physical_science": "1986 only: 1 = Physical-science field detected (incl. engineering, mathematics, mixed Physical+Biological); 0 = no Physical match detected, NOT a verified negative.",
    "physical_science_unresolved": "1986 only: 1 = field heading could not be classified (counted as physical_science = 0).",
    "physical_science_mixed": "1986 only: 1 = Physical and Biological field (counted as physical_science = 1).",
    "award_index": "Award position within the biography (stable across releases).",
    "requested_family": "Degree family: ba_bs, ma, ms, phd, bachelors_other, masters_other, doctorate_other, postsecondary_credential.",
    "family_award_index": "Order of the award within its family for this entry.",
    "label_as_written": "Degree label as printed (e.g. PhD, A.B, M.D).",
    "institution_as_written": "Institution text as printed in the biography.",
    "year": "Award year, resolved to four digits where possible.",
    "training_eligibility": "eligible, or the reason the award is not used (e.g. unresolved_level).",
    "fully_matched": "True when every institution named for the award has an OpenAlex ID with coordinates.",
    "oa_names": "JSON list of OpenAlex institution names, aligned with oa_ids.",
    "oa_cities": "JSON list of institution cities (OpenAlex reference location; historical campus not verified).",
    "oa_latitudes": "JSON list of institution latitudes, aligned with oa_ids.",
    "oa_longitudes": "JSON list of institution longitudes, aligned with oa_ids.",
    "assignment_class": "How the ID was assigned; manual_top500_crosswalk / manual_medical_campus_override mark v05 additions.",
    "raw_mapping_status": "Mapping status; for v05 manual awards, the manual confidence.",
    "manual_assignment": "v05 only: crosswalk or override when the award was matched manually, blank otherwise.",
    "manual_confidence": "v05 only: clear, likely or ambiguous for manually matched awards.",
    "evidence": "The text span the award was read from.",
    "unmatched_category": "Why the award has no full match: outside_top500, no_institution_text, unresolved_level, not_in_openalex, unresolved_split, no_institution, partially_matched.",
    "normalization_candidate_id": "OpenAlex ID an OCR-normalised version of the text matches; recorded for review, NOT applied.",
}


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def typed(value, numeric):
    if numeric and value != "":
        try:
            n = float(value)
            return int(n) if n.is_integer() and "." not in value else n
        except ValueError:
            return value
    return value


def sheet(wb, title, columns, records, numeric, widths, bold, fill):
    ws = wb.create_sheet(title)
    ws.freeze_panes = "A2"
    for i, c in enumerate(columns, 1):
        ws.column_dimensions[get_column_letter(i)].width = widths.get(c, 16)
    head = []
    for c in columns:
        cell = WriteOnlyCell(ws, value=c)
        cell.font, cell.fill = bold, fill
        head.append(cell)
    ws.append(head)
    n = 0
    for r in records:
        ws.append([typed(r.get(c, ""), c in numeric) for c in columns])
        n += 1
    ws.auto_filter.ref = f"A1:{get_column_letter(len(columns))}{n + 1}"
    return n


def rows(path):
    with open(path, encoding="utf-8-sig", newline="") as f:
        yield from csv.DictReader(f)


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    sources = {name: RELEASE / name for name in ("amws_open_alex.csv", "education_awards_openalex.csv", "unmatched_awards.csv",
                                                 "completion_manifest.json")}
    hashes = {name: sha256(p) for name, p in sources.items()}
    wb = Workbook(write_only=True)
    arial = Font(name="Arial", sz=10, family=2)
    wb._fonts = IndexedList([arial])                       # default font for every unstyled cell
    wb._named_styles["Normal"].font = arial
    bold = Font(name="Arial", sz=10, family=2, bold=True)
    fill = PatternFill("solid", fgColor="DDE6F0")

    with open(sources["amws_open_alex.csv"], encoding="utf-8-sig", newline="") as f:
        person_columns = [c for c in csv.DictReader(f).fieldnames if not AWARD_COLUMN.match(c)]
    unmatched_columns = list(next(rows(sources["unmatched_awards.csv"])))

    readme = wb.create_sheet("README")
    readme.column_dimensions["A"].width = 30
    readme.column_dimensions["B"].width = 120
    title = WriteOnlyCell(readme, value=f"AMWS education release {STAMP.split('_')[3]} ({STAMP})")
    title.font = Font(name="Arial", sz=13, family=2, bold=True)
    readme.append([title])

    counts = {}
    print("Writing Persons", flush=True)
    counts["Persons"] = sheet(wb, "Persons", person_columns, rows(sources["amws_open_alex.csv"]), PERSON_NUMERIC,
                              {"raw_text_adjusted": 60, "name_raw": 30, "birthplace_orig": 30, "oa_ids": 30}, bold, fill)
    print("Writing Awards", flush=True)
    counts["Awards"] = sheet(wb, "Awards", AWARD_COLUMNS, rows(sources["education_awards_openalex.csv"]), AWARD_NUMERIC,
                             {"institution_as_written": 30, "oa_names": 40, "evidence": 50}, bold, fill)
    print("Writing Unmatched", flush=True)
    counts["Unmatched"] = sheet(wb, "Unmatched", unmatched_columns, rows(sources["unmatched_awards.csv"]), UNMATCHED_NUMERIC,
                                {"institution_as_written": 30, "normalization_candidate_name": 35, "evidence": 50}, bold, fill)
    cols = wb.create_sheet("Columns")
    cols.freeze_panes = "A2"
    for letter, w in zip("ABC", (14, 34, 110)):
        cols.column_dimensions[letter].width = w
    head = []
    for v in ("sheet", "column", "description"):
        c = WriteOnlyCell(cols, value=v)
        c.font, c.fill = bold, fill
        head.append(c)
    cols.append(head)
    for name, columns in (("Persons", person_columns), ("Awards", AWARD_COLUMNS), ("Unmatched", unmatched_columns)):
        for c in columns:
            cols.append([name, c, DESCRIPTIONS.get(c, "See the release's DATA_DICTIONARY.txt and POSTSECONDARY.md.")])

    lines = [
        ("Release", f"output/amws/{STAMP}/ in the gtl_talent_dets Dropbox (built {BUILT} by prep/amws/education/{BUILDER}; independent validator passed)."),
        ("Institutions (v05)", "v05 is the v04 release plus manual institution matches: the top-500 crosswalk (52,752 awards) and award-level medical-campus overrides (653). 94.2% of 377,987 awards have an institution, against 80.0% in v04."),
        ("Persons", f"{counts['Persons']:,} rows. One row per scientist entry (an edition line). The 705 wide award columns of amws_open_alex.csv are omitted; the Awards sheet holds the same information, one row per award."),
        ("Awards", f"{counts['Awards']:,} rows. Every included postsecondary award, from education_awards_openalex.csv (selected columns). Join to Persons on source_key."),
        ("Unmatched", f"{counts['Unmatched']:,} rows. Awards still without a full match, with the reason and an OCR-normalised candidate ID that is NOT applied."),
        ("Columns", "Short description of every exported column."),
        ("JSON lists", "oa_ids, oa_names, oa_cities, oa_countries, oa_latitudes and oa_longitudes hold JSON lists aligned by position (most awards have one institution)."),
        ("physical_science", "1986 edition only; blank for 1906, 1938 and 1955. 0 means no Physical match detected, not a verified non-Physical scientist; use physical_science_unresolved to separate the two."),
        ("Duplicates", DUPLICATES_NOTE),
        ("Coordinates", "OpenAlex reference locations; historical campuses are not verified."),
        ("Source sha256", "; ".join(f"{k} {v}" for k, v in hashes.items())),
    ]
    for k, v in lines:
        label = WriteOnlyCell(readme, value=k)
        label.font = bold
        readme.append([label, v])

    path = OUT / f"{STAMP}.xlsx"
    print("Saving", path, flush=True)
    wb.save(path)
    print("Done:", counts, f"{path.stat().st_size / 1e6:.0f} MB")


if __name__ == "__main__":
    import argparse
    import json
    sys.stdout.reconfigure(encoding="utf-8")
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--release", choices=sorted(RELEASES), default="v05")
    STAMP, BUILDER, BUILT = RELEASES[cli.parse_args().release]
    RELEASE = ROOT / "output/amws" / STAMP
    OUT = ROOT / "output/amws" / f"{STAMP}_xlsx"
    dedup = json.loads((RELEASE / "schema.json").read_text(encoding="utf-8")).get("deduplication")
    DUPLICATES_NOTE = (f"Duplicate entries removed in this release: {dedup['removed']:,} rows in {dedup['groups']:,} groups "
                       f"(by edition: {dedup['removed_by_edition']}); duplicates_removed.csv in the release lists each with the entry kept."
                       if dedup else "Some biographies appear twice under different source_keys (mainly 1955 and 1986); rows are kept as released.")
    main()

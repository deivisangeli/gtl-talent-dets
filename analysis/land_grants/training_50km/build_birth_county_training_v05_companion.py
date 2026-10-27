"""Figures 13-16 companion for the Oct 21, 2026 report entry: birth-county training event studies.

One landscape page per figure: a caption strip above the original vector ggplot PDF from the
birth-county training results of the chosen education release (--release v05, the default, or v06,
which removes duplicate entries). Two table pages follow, computed from that education release's
coverage_by_edition_family.csv: award coverage by edition (Table 1) and by degree type (Table 2). Figure numbering continues the Oct 14 companion, which ended at
Figure 12. The file name stays the same for both releases so the Drive copy linked from the report
can be replaced in place.
"""
import argparse
import collections
import copy
import csv
import hashlib
import io
import os
from pathlib import Path
import sys

from pypdf import PdfReader, PdfWriter, Transformation
from reportlab.lib.colors import HexColor
from reportlab.pdfgen import canvas
from reportlab.platypus import Table, TableStyle

ROOT = Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets")))
RELEASES = {
    "v05": ("amws_birth_county_training_postsecondary_v05_20261020", "college_allocation_birth_county_training_v05_20261021",
            "AMWS education release v05.", "amws_education_postsecondary_v05_20261020"),
    "v06": ("amws_birth_county_training_postsecondary_v06_20261021", "college_allocation_birth_county_training_v06_20261021",
            "AMWS education release v06 (v05 with duplicate entries removed).", "amws_education_postsecondary_v06_20261021"),
}
TITLE = "College allocation — birth-county training outcomes (v05) — 2026-10-21"
FIGURES = [
    (13, "birth_county_training_relative_standard_decade.pdf", "relative-time-balanced sample, standard decades"),
    (14, "birth_county_training_relative_alternative_decade.pdf", "relative-time-balanced sample, shifted decades"),
    (15, "birth_county_training_calendar_standard_decade.pdf", "calendar-balanced sample, standard decades"),
    (16, "birth_county_training_calendar_alternative_decade.pdf", "calendar-balanced sample, shifted decades"),
]
NOTE = ("Scientists born in the county who ever, never or with unknown status trained at an institution inside the birth county (top row) "
        "or within 50 km of its centroid (bottom row), per 1,000 county births.",
        "Callaway-Sant'Anna with baseline births; reference e = 0; pointwise 95% intervals. {release}")
STRIP = 66  # points reserved above the figure for the caption
EDITIONS = ["1906", "1938", "1955", "1986"]
FAMILIES = [
    ("ba_bs", "Bachelor of Arts / Science (B.S., A.B., B.A., B.Sc.)"),
    ("bachelors_other", "Other bachelor's (Ph.B., B.Ch.E., B.E., M.B.)"),
    ("ma", "Master of Arts (M.A., A.M.)"),
    ("ms", "Master of Science (M.S., M.Sc., S.M.)"),
    ("masters_other", "Other master's (M.E., M.S.E., M.B.A., M.P.H.)"),
    ("phd", "Doctor of Philosophy (Ph.D.)"),
    ("doctorate_other", "Other doctorate (M.D., Sc.D., D.Sc., D.V.M.)"),
    ("postsecondary_credential", "Other credential (C.E., diploma, E.E., certificate)"),
]
TABLE_NOTE = ("Awards are the post-secondary degrees and credentials parsed from each entry; honorary degrees, titles and board "
              "certifications are excluded. An award is matched",
              "when an institution it names is linked to an OpenAlex institution ID (awards naming several institutions count as "
              "matched if any one is). {release}")


def caption_page(width, height, number, label, release_note):
    buf = io.BytesIO()
    c = canvas.Canvas(buf, pagesize=(width, height))
    c.setFont("Helvetica-Bold", 14)
    c.drawString(36, height - 28, f"Figure {number}.")
    c.setFont("Helvetica", 14)
    c.drawString(36 + c.stringWidth(f"Figure {number}. ", "Helvetica-Bold", 14), height - 28,
                 f"College allocation and training of county-born scientists: {label}.")
    c.setFont("Helvetica", 9)
    c.setFillColor(HexColor("#444444"))
    for i, line in enumerate(NOTE):
        c.drawString(36, height - 46 - 11 * i, line.format(release=release_note))
    c.showPage()
    c.save()
    return PdfReader(io.BytesIO(buf.getvalue())).pages[0]


def coverage_tables(education):
    counts = collections.Counter()
    with open(education / "coverage_by_edition_family.csv", encoding="utf-8-sig", newline="") as f:
        for r in csv.DictReader(f):
            counts[(r["edition"], r["family"], r["metric"])] += int(r["count"])
    entries = collections.Counter()
    with open(education / "amws_open_alex.csv", encoding="utf-8-sig", newline="") as f:
        for r in csv.DictReader(f):
            entries[r["source_key"].split(":")[0]] += 1
    families = [f for f, _ in FAMILIES]
    assert {f for _, f, _ in counts} == set(families) and {e for e, _, _ in counts} == set(EDITIONS), "unexpected coverage keys"

    def total(metric, eds=EDITIONS, fams=families):
        return sum(counts[(e, f, metric)] for e in eds for f in fams)

    def pct(a, b):
        return f"{100 * a / b:.1f}%"

    by_edition = [["Edition", "Entries", "Awards", "Matched", "Coverage"]]
    for e in EDITIONS + ["All"]:
        eds = EDITIONS if e == "All" else [e]
        a, m = total("awards", eds), total("matched", eds)
        by_edition.append([e, f"{sum(entries[x] for x in eds):,}", f"{a:,}", f"{m:,}", pct(m, a)])
    grand = total("awards")
    by_family = [["Degree type", "Awards", "Share of awards", "Matched", "Coverage"]]
    for f, label in FAMILIES + [("all", "All")]:
        fams = families if f == "all" else [f]
        a, m = total("awards", fams=fams), total("matched", fams=fams)
        by_family.append([label, f"{a:,}", pct(a, grand), f"{m:,}", pct(m, a)])
    return by_edition, by_family


def table_page(width, height, number, title, rows, col_widths, release_note):
    buf = io.BytesIO()
    c = canvas.Canvas(buf, pagesize=(width, height))
    c.setFont("Helvetica-Bold", 14)
    c.drawString(36, height - 28, f"Table {number}.")
    c.setFont("Helvetica", 14)
    c.drawString(36 + c.stringWidth(f"Table {number}. ", "Helvetica-Bold", 14), height - 28, title)
    c.setFont("Helvetica", 9)
    c.setFillColor(HexColor("#444444"))
    for i, line in enumerate(TABLE_NOTE):
        c.drawString(36, height - 46 - 11 * i, line.format(release=release_note))
    table = Table(rows, colWidths=col_widths)
    table.setStyle(TableStyle([
        ("FONT", (0, 0), (-1, -1), "Helvetica", 11),
        ("FONT", (0, 0), (-1, 0), "Helvetica-Bold", 11),
        ("FONT", (0, -1), (-1, -1), "Helvetica-Bold", 11),
        ("ALIGN", (1, 0), (-1, -1), "RIGHT"),
        ("LINEABOVE", (0, 0), (-1, 0), 1, HexColor("#000000")),
        ("LINEBELOW", (0, 0), (-1, 0), 0.6, HexColor("#000000")),
        ("LINEABOVE", (0, -1), (-1, -1), 0.6, HexColor("#000000")),
        ("LINEBELOW", (0, -1), (-1, -1), 1, HexColor("#000000")),
        ("TOPPADDING", (0, 0), (-1, -1), 5),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 5),
    ]))
    tw, th = table.wrapOn(c, width, height)
    table.drawOn(c, (width - tw) / 2, height - STRIP - 30 - th)
    c.showPage()
    c.save()
    return PdfReader(io.BytesIO(buf.getvalue())).pages[0]


def main(release):
    stamp, folder, release_note, education_stamp = RELEASES[release]
    EDUCATION = ROOT / "output/amws" / education_stamp
    SOURCE = ROOT / "results/land_grants/event_studies" / stamp
    OUT = ROOT / "results/land_grants/companions" / folder
    OUT.mkdir(parents=True, exist_ok=True)
    writer = PdfWriter()
    rows = []
    for number, name, label in FIGURES:
        src = SOURCE / name
        fig = PdfReader(src).pages[0]
        fw, fh = float(fig.mediabox.width), float(fig.mediabox.height)
        width, height = fw, fh + STRIP
        page = caption_page(width, height, number, label, release_note)
        page.merge_transformed_page(copy.copy(fig), Transformation().translate(0, 0))
        writer.add_page(page).compress_content_streams()
        rows.append((number, name, hashlib.sha256(src.read_bytes()).hexdigest()))
    by_edition, by_family = coverage_tables(EDUCATION)
    for number, title, table, widths in (
            (1, "Award coverage by AMWS edition.", by_edition, [90, 90, 90, 90, 90]),
            (2, "Award coverage by degree type.", by_family, [330, 80, 105, 80, 80])):
        writer.add_page(table_page(width, height, number, title, table, widths, release_note)).compress_content_streams()
    coverage = EDUCATION / "coverage_by_edition_family.csv"
    rows.append(("Tables 1-2", coverage.name, hashlib.sha256(coverage.read_bytes()).hexdigest()))
    writer.add_metadata({"/Title": TITLE, "/Author": "João Francisco Marques"})
    writer.compress_identical_objects()
    pdf = OUT / f"{TITLE}.pdf"
    with open(pdf, "wb") as f:
        writer.write(f)
    (OUT / "README.md").write_text(
        f"# {TITLE}\n\nFigures 13-16 and Tables 1-2 for the Oct 21, 2026 entry of 'Reports - João Francisco', continuing the Oct 14 "
        "companion's numbering. Built by analysis/land_grants/training_50km/build_birth_county_training_v05_companion.py "
        f"from {SOURCE} and {EDUCATION}.\n\n| Figure / table | Source | sha256 |\n|---|---|---|\n"
        + "".join(f"| {n} | {s} | {h} |\n" for n, s, h in rows), encoding="utf-8")
    print(pdf, pdf.stat().st_size, "bytes")


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--release", choices=sorted(RELEASES), default="v05")
    main(cli.parse_args().release)

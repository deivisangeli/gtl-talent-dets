"""Standalone PDF comparing two shifted-decade definitions for the birth-county training event studies (v06).

Outcome and treatment shifted is the v06 alternative_decade specification; outcome only shifted keeps the
allocation decade at g_std (run_birth_county_training_v06_outcome_shift.R). For each balanced sample, one page
overlays the two definitions and the next shows the standard-decade figure as the unshifted reference. Each page is
a caption strip above the original vector ggplot PDF.
"""
import copy
import hashlib
import io
import os
from pathlib import Path
import sys

from pypdf import PdfReader, PdfWriter, Transformation
from reportlab.lib.colors import HexColor
from reportlab.pdfgen import canvas

ROOT = Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets")))
RESULTS = ROOT / "results/land_grants/event_studies"
SHIFT = RESULTS / "amws_birth_county_training_v06_outcome_shift_20261022"
BOTH = RESULTS / "amws_birth_county_training_postsecondary_v06_20261021"
OUT = ROOT / "results/land_grants/companions/birth_county_training_decade_shift_comparison_20261022"
TITLE = "College allocation — shifted-decade definitions — 2026-10-22"
SAMPLES = {"relative": "relative-time-balanced sample", "calendar": "calendar-balanced sample"}
PAGES = [
    (SHIFT, "decade_shift_comparison_{b}.pdf", "{s}: outcome and treatment shifted vs outcome only shifted",
     ("Outcome and treatment shifted (blue): birth years and allocation years ending in 7-9 move to the next decade, as in the v06 shifted-decade figures.",
      "Outcome only shifted (orange): birth years shifted, allocation decade kept at the standard decade, on the standard-decade sample. {note}")),
    (BOTH, "birth_county_training_{b}_standard_decade.pdf", "{s}, standard decades (unshifted reference)",
     ("Reference: neither outcome nor treatment shifted. Same sample as the outcome-only definition on the previous page.",
      "Callaway-Sant'Anna with baseline births; reference e = 0; pointwise 95% intervals. AMWS education release v06.")),
]
NOTES = {"relative": "Shifting the treatment adds events 39, 40, 42 and 43 (20 vs 16), so this gap also reflects sample composition.",
         "calendar": "Both definitions use the same 15 events and 46 units, so the gap is the treatment shift alone."}
STRIP = 66  # points reserved above the figure for the caption


def caption_page(width, height, number, title, lines):
    buf = io.BytesIO()
    c = canvas.Canvas(buf, pagesize=(width, height))
    c.setFont("Helvetica-Bold", 14)
    c.drawString(36, height - 28, f"Figure {number}.")
    c.setFont("Helvetica", 14)
    c.drawString(36 + c.stringWidth(f"Figure {number}. ", "Helvetica-Bold", 14), height - 28,
                 f"College allocation and training of county-born scientists, {title}.")
    c.setFont("Helvetica", 9)
    c.setFillColor(HexColor("#444444"))
    for i, line in enumerate(lines):
        c.drawString(36, height - 46 - 11 * i, line)
    c.showPage()
    c.save()
    return PdfReader(io.BytesIO(buf.getvalue())).pages[0]


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    writer = PdfWriter()
    rows = []
    number = 0
    for balance, sample in SAMPLES.items():
        for folder, name, title, lines in PAGES:
            number += 1
            src = folder / name.format(b=balance)
            fig = PdfReader(src).pages[0]
            width, height = float(fig.mediabox.width), float(fig.mediabox.height) + STRIP
            text = [line.format(note=NOTES[balance]) for line in lines]
            page = caption_page(width, height, number, title.format(s=sample), text)
            page.merge_transformed_page(copy.copy(fig), Transformation().translate(0, 0))
            writer.add_page(page).compress_content_streams()
            rows.append((number, str(src.relative_to(ROOT)), hashlib.sha256(src.read_bytes()).hexdigest()))
    writer.add_metadata({"/Title": TITLE, "/Author": "João Francisco Marques"})
    writer.compress_identical_objects()
    pdf = OUT / f"{TITLE}.pdf"
    with open(pdf, "wb") as f:
        writer.write(f)
    (OUT / "README.md").write_text(
        f"# {TITLE}\n\nTwo shifted-decade definitions for the birth-county training event studies (education release v06). "
        "Built by analysis/land_grants/training_50km/build_decade_shift_comparison.py.\n\n| Figure | Source | sha256 |\n|---|---|---|\n"
        + "".join(f"| {n} | {s} | {h} |\n" for n, s, h in rows), encoding="utf-8")
    print(pdf, pdf.stat().st_size, "bytes")


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    main()

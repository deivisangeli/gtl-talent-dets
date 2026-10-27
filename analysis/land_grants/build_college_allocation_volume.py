"""Collect the college-allocation event studies into one PDF.

Sections:
  A. the main-text figures of project/main.tex (balanced window e in [-20, 70]);
  B. its appendix variants (less-restrictive sample, per cohort, 1860-1880 cohorts);
  C. the birth-county training outcomes on education release v05 (balanced and unbalanced);
  D. tables: the selected experiments, v04-vs-v05 training classes and overall ATTs.

Figures are copied from the results folders into project/figures/college_allocation_volume/
so the volume never points at the PNGs main.tex uses. The .tex is written to
project/college_allocation_event_studies.tex and compiled twice with pdflatex in
project/tmp/college_allocation/; the PDF is copied to project/ and to
results/land_grants/companions/college_allocation_event_studies_20261020/.
"""
import csv
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

REPO = Path(__file__).resolve().parents[2]
PROJECT = REPO / "project"
ROOT = Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets")))
ES = ROOT / "results/land_grants/event_studies"
BASE = "amws_county_pairs_temporal_support_1830_1950_baseline_pop"
TRAINING = "amws_birth_county_training_postsecondary_v05_20261020"
TRAINING_V04 = "amws_birth_county_training_postsecondary_v04_20261019"
PREP = ROOT / "output/land_grants"
NAME = "college_allocation_event_studies"
COMPANION = ROOT / "results/land_grants/companions" / f"{NAME}_20261020"
FIGDIR = "figures/college_allocation_volume"
PDFLATEX = shutil.which("pdflatex") or str(Path.home() / "AppData/Local/Programs/MiKTeX/miktex/bin/x64/pdflatex.exe")

OUTCOMES = [("n_amws", "AMWS scientists born in the county (count)"),
            ("amws_per_1000_births", "AMWS scientists per 1,000 estimated county births"),
            ("county_births_estimate", "Estimated county births (placebo)")]
TIMINGS = [("standard", "standard decades"), ("alternative", "shifted decades")]
MAIN_NOTE = ("Callaway--Sant'Anna estimator with never-treated runner-up counties as controls and births at $e=-10$ "
             "as covariate; reference $e=0$; pointwise 95\\% confidence intervals.")
FAMILIES = [
    ("A", "Main-text figures", [
        (f"{BASE}_balanced_event_time_max70", "ES_{o}_births_level_{t}_decade.png",
         "Fully balanced window, $e \\in [-20, 70]$ (main text)", "main_max70")]),
    ("B", "Appendix variants in main.tex", [
        (BASE, "ES_{o}_births_level_{t}_decade.png", "Less-restrictive sample (no event-time balancing)", "less_restrictive"),
        (f"{BASE}_per_cohort_max_e", "ES_{o}_{t}_decade.png", "Estimates by allocation cohort", "per_cohort"),
        (f"{BASE}_balanced_event_time_max70_min30_cohorts_1860_1870_1880",
         "ES_{o}_births_level_{t}_decade.png", "Allocation cohorts 1860--1880, balanced $e \\in [-30, 70]$", "cohorts_1860_1880")]),
]
TRAINING_FIGS = [
    (TRAINING, "birth_county_training_relative_{t}_decade.pdf", "Relative-time-balanced sample, $e \\in [-20, 70]$"),
    (TRAINING, "birth_county_training_calendar_{t}_decade.pdf", "Calendar-balanced sample, $e \\in [-50, 70]$"),
    (TRAINING.replace("_v05_", "_v05_unbalanced_"), "birth_county_training_unbalanced_{t}_decade.pdf",
     "Unbalanced sample (37/38 events), $e \\in [-30, 90]$"),
]


def tex_escape(s):
    return s.replace("_", "\\_").replace("&", "\\&").replace("%", "\\%")


def rows(path):
    with open(path, encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def copy(src, name):
    assert src.exists(), f"Missing figure: {src}"
    dest = PROJECT / FIGDIR / name
    shutil.copy2(src, dest)
    return f"{FIGDIR}/{name}"


def figure_pair(left, right, caption, label):
    return ("\\begin{figure}[H]\n\\centering\n"
            f"\\begin{{minipage}}{{0.49\\textwidth}}\\centering\\includegraphics[width=\\linewidth,height=0.62\\textheight,keepaspectratio]{{{left}}}\\\\[-2pt]\\small (a) Standard decades\\end{{minipage}}\\hfill\n"
            f"\\begin{{minipage}}{{0.49\\textwidth}}\\centering\\includegraphics[width=\\linewidth,height=0.62\\textheight,keepaspectratio]{{{right}}}\\\\[-2pt]\\small (b) Shifted decades\\end{{minipage}}\n"
            f"\\caption{{{caption}}}\\label{{{label}}}\n\\end{{figure}}\n\\clearpage\n")


def figure_page(path, caption, label):
    return ("\\begin{figure}[H]\n\\centering\n"
            f"\\includegraphics[width=\\textwidth,height=0.78\\textheight,keepaspectratio]{{{path}}}\n"
            f"\\caption{{{caption}}}\\label{{{label}}}\n\\end{{figure}}\n\\clearpage\n")


def class_table():
    """Training classes, v04 against v05, from the v05 prep release's transition summary."""
    t = rows(PREP / TRAINING / "classification_transition_summary.csv")
    out = []
    for geo, label in (("same_county", "Inside birth county"), ("50km", "Within 50 km")):
        old, new = {}, {}
        for r in t:
            if r["geography"] == geo:
                old[r["old_class"]] = old.get(r["old_class"], 0) + int(r["entries"])
                new[r["new_class"]] = new.get(r["new_class"], 0) + int(r["entries"])
        cells = " & ".join(f"{old.get(c, 0):,} & {new.get(c, 0):,}" for c in ("ever", "never", "unknown"))
        out.append(f"{label} & {cells} \\\\")
    return ("\\begin{table}[H]\\centering\\small\n\\caption{Training classes of the 12,360 eligible scientists, education release v04 against v05}\n"
            "\\label{tab:classes}\n\\begin{tabular}{lrrrrrr}\n\\toprule\n"
            " & \\multicolumn{2}{c}{Ever} & \\multicolumn{2}{c}{Never} & \\multicolumn{2}{c}{Unknown} \\\\\n"
            "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}\n"
            "Geography & v04 & v05 & v04 & v05 & v04 & v05 \\\\\n\\midrule\n" + "\n".join(out) +
            "\n\\bottomrule\n\\end{tabular}\n\n\\medskip\\parbox{0.85\\textwidth}{\\footnotesize v05 adds manual institution matches "
            "only to awards v04 left unmatched, so no scientist leaves the ever or never class; the change is unknown entries "
            "resolving.}\n\\end{table}\n")


def att_table():
    """Overall post-treatment ATT (e = 0..70), v04 against v05, relative-time-balanced sample."""
    t = [r for r in rows(ES / TRAINING / "v04_vs_v05_post_treatment_att.csv") if r["balance"] == "relative"]
    order = ["same_county__ever", "same_county__never", "same_county__unknown", "50km__ever", "50km__never", "50km__unknown", "total"]
    names = {"same_county__ever": "Ever, inside county", "same_county__never": "Never, inside county",
             "same_county__unknown": "Unknown, inside county", "50km__ever": "Ever, within 50 km",
             "50km__never": "Never, within 50 km", "50km__unknown": "Unknown, within 50 km", "total": "All county-born (total)"}
    lines = []
    for g in order:
        cells = []
        for timing in ("standard_decade", "alternative_decade"):
            r = next(x for x in t if x["group"] == g and x["timing"] == timing)
            cells.append(f"{float(r['estimate_v04']):.3f} ({float(r['se_v04']):.3f}) & {float(r['estimate_v05']):.3f} ({float(r['se_v05']):.3f})")
        lines.append(f"{names[g]} & {' & '.join(cells)} \\\\")
    return ("\\begin{table}[H]\\centering\\small\n\\caption{Overall post-treatment effect on scientists per 1,000 county births, "
            "$e = 0,\\dots,70$, relative-time-balanced sample}\n\\label{tab:att}\n\\begin{tabular}{lcccc}\n\\toprule\n"
            " & \\multicolumn{2}{c}{Standard decades} & \\multicolumn{2}{c}{Shifted decades} \\\\\n"
            "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\nOutcome & v04 & v05 & v04 & v05 \\\\\n\\midrule\n" + "\n".join(lines) +
            "\n\\bottomrule\n\\end{tabular}\n\n\\medskip\\parbox{0.85\\textwidth}{\\footnotesize Standard errors in parentheses. "
            "Same samples, denominators, estimator and seeds in both columns; within each geography the three classes sum to "
            "the total, which is identical across releases.}\n\\end{table}\n")


def main():
    (PROJECT / FIGDIR).mkdir(parents=True, exist_ok=True)
    body = []
    for letter, title, variants in FAMILIES:
        body.append(f"\\section*{{{letter}. {title}}}\\addcontentsline{{toc}}{{section}}{{{letter}. {title}}}\n")
        for folder, pattern, description, tag in variants:
            for o, outcome in OUTCOMES:
                pair = [copy(ES / folder / pattern.format(o=o, t=t), f"{tag}__{o}__{t}.png") for t, _ in TIMINGS]
                body.append(figure_pair(*pair, f"{outcome}. {description}. {MAIN_NOTE} Source: \\texttt{{{tex_escape(folder)}}}.",
                                        f"fig:{tag}:{o}"))
    body.append("\\section*{C. Birth-county training outcomes, education release v05}"
                "\\addcontentsline{toc}{section}{C. Birth-county training outcomes, v05}\n"
                "Six rates per 1,000 county births: scientists born in the county who ever, never or with unknown status trained "
                "at an institution inside the birth county or within 50 km of its centroid. All earned postsecondary degrees and "
                "credentials; institutions from education release v05 (v04 plus the top-500 manual crosswalk and medical-campus "
                "overrides, 94.2\\% of awards matched). Callaway--Sant'Anna with baseline births, reference $e=0$, pointwise 95\\% "
                "intervals.\n\\clearpage\n")
    for folder, pattern, description in TRAINING_FIGS:
        for t, timing in TIMINGS:
            name = pattern.format(t=t)
            path = copy(ES / folder / name, f"training_v05__{name}")
            body.append(figure_page(path, f"Training of county-born scientists. {description}; {timing}. "
                                          f"Source: \\texttt{{{tex_escape(folder)}}}.", f"fig:{Path(name).stem}"))
    body.append("\\section*{D. Tables}\\addcontentsline{toc}{section}{D. Tables}\n")
    body.append(class_table())
    body.append(att_table())
    body.append("\\clearpage\n\\input{tables/andrews_selected_events}\n")
    tex = ("% Generated by analysis/land_grants/build_college_allocation_volume.py -- do not edit by hand.\n"
           "\\documentclass[11pt]{article}\n\\usepackage[a4paper,landscape,margin=12mm]{geometry}\n"
           "\\usepackage{graphicx,float,caption,booktabs,longtable}\n\\usepackage{hyperref}\n"
           "\\hypersetup{hidelinks,pdftitle={College Allocation Event Studies}}\n"
           "\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{5pt}\n\\captionsetup{font=small,labelfont=bf,justification=raggedright,singlelinecheck=false}\n"
           "\\begin{document}\n\\begin{center}{\\LARGE\\bfseries College Allocation Event Studies}\\par\n"
           "{\\large Andrews (2023) selected versus runner-up counties}\\par 20 October 2026\\end{center}\n"
           "Sections A and B re-run every college-allocation figure in \\texttt{main.tex}: AMWS scientist counts, scientists "
           "per 1,000 births and the births placebo. These outcomes do not use the education release. Section C re-runs the "
           "birth-county training outcomes on education release v05, and Section D compares them with v04.\n"
           "\\tableofcontents\n\\clearpage\n" + "".join(body) + "\\end{document}\n")
    (PROJECT / f"{NAME}.tex").write_text(tex, encoding="utf-8")
    build = PROJECT / "tmp" / "college_allocation"
    build.mkdir(parents=True, exist_ok=True)
    for _ in range(2):
        result = subprocess.run([PDFLATEX, "-interaction=nonstopmode", "-halt-on-error", f"-output-directory={build}", f"{NAME}.tex"],
                                cwd=PROJECT, capture_output=True, text=True, errors="replace")
        if result.returncode:
            print(result.stdout[-4000:])
            raise SystemExit("pdflatex failed")
    log = (build / f"{NAME}.log").read_text(errors="replace")
    assert not re.search(r"File `[^']*' not found|undefined references", log), "Missing file or reference in LaTeX log"
    pdf = build / f"{NAME}.pdf"
    shutil.copy2(pdf, PROJECT / f"{NAME}.pdf")
    COMPANION.mkdir(parents=True, exist_ok=True)
    shutil.copy2(pdf, COMPANION / f"{NAME}.pdf")
    shutil.copy2(PROJECT / f"{NAME}.tex", COMPANION / f"{NAME}.tex")
    print("Wrote", PROJECT / f"{NAME}.pdf", "and", COMPANION)


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    main()

"""Independently reconcile state panels, estimate exports and PDF captions.

Requires pypdf. Use TALENT_DETS_DATA_DIR; writes only a QA JSON under tmp/pdfs.
"""
import csv
import json
import math
import os
import re
from collections import Counter, defaultdict
from pathlib import Path

from pypdf import PdfReader


def rows(path):
    with open(path, encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream))


def validate(root):
    out = root / "results/mandatory_schooling/compulsory_schooling_amws_yearly_rd_state_direct"
    panel = root / "output/mandatory_schooling/yearly_state_direct"
    summary = rows(out / "run_summary.csv")[0]
    primary = rows(out / "primary_state_estimates.csv")
    estimated = sorted((r for r in primary if r["status"] == "estimated"), key=lambda r: r["state"])
    coverage = rows(out / "all_state_dispositions.csv")
    points = rows(out / "state_yearly_plot_points.csv")
    included = [r for r in rows(panel / "scientist_record_dispositions.csv") if r["included"] == "TRUE"]
    annual = rows(panel / "state_amws_yearly_gapminder_1800_1960.csv")
    assert len(included) == int(summary["source_records"])
    record_keys = [(r["edition"], r["record_id"]) for r in included]
    assert len(record_keys) == len(set(record_keys))
    counts = Counter((r["state_fips"], r["year"]) for r in included)
    for r in annual:
        assert int(r["n_amws"]) == counts[(r["state_fips"], r["year"])]
        assert int(r["n_amws"]) == int(r["n_early"]) + int(r["n_1986"])
        if r["population"]:
            assert math.isclose(float(r["births"]), float(r["population"]) * float(r["us_birth_rate_year"]), rel_tol=1e-12)
        else:
            assert r["births"] == "" and r["rate"] == ""
    assert len(annual) == 49 * 161
    assert len(coverage) == 51 and len({r["state_fips"] for r in coverage}) == 51
    assert all(len(r["state_fips"]) == 2 for r in annual + coverage)
    grouped = defaultdict(list)
    for r in points:
        grouped[r["state_fips"]].append(r)
    for r in estimated:
        d = grouped[r["state_fips"]]
        assert len(d) == 20
        for side in (0, 1):
            selected = [z for z in d if int(z["post"]) == side]
            assert len(selected) == 10
            field = "pre_scientists" if side == 0 else "post_scientists"
            assert sum(int(z["n_amws"]) for z in selected) == int(r[field])
        for z in d:
            assert math.isclose(float(z["rate"]), 1000 * int(z["n_amws"]) / float(z["births"]), rel_tol=1e-12)
        assert not r["mixed_year_omitted"] or all(z["year"] != r["mixed_year_omitted"] for z in d)
    assert len(estimated) == int(summary["estimated_states"]) and len(points) == 20 * len(estimated)
    tex = (out / "yearly_state_rd_figures.tex").read_text(encoding="utf-8")
    references = re.findall(r"\\includegraphics\[[^\]]*\]\{([^}]+)\}", tex)
    expected = ["state_comparison_verified_monthly.png", "state_comparison_annual_approximation.png"]
    expected += [f"states/{r['state_abbr']}.png" for r in estimated]
    assert references == expected and len(set(references)) == len(estimated) + 2
    pdf = PdfReader(out / "yearly_state_rd_figures.pdf")
    image_counts = [len(page.images) for page in pdf.pages]
    assert image_counts == [0, 1, 1] + [2] * (len(estimated) // 2) + ([1] if len(estimated) % 2 else []) + [0]
    text = " ".join(" ".join(page.extract_text().split()) for page in pdf.pages)
    for r in estimated:
        prefix = rf"Figure \d+:\s*{re.escape(r['state'])}\. Birth cutoff:"
        assert len(re.findall(prefix, text)) == 1, r["state"]
        assert f"Birth cutoff: {r['cutoff_date']}" in text
        assert f"Jump: {float(r['estimate']):.4f};" in text
        assert f"[{float(r['ci_low']):.4f}, {float(r['ci_high']):.4f}]" in text
    appendix = " ".join(pdf.pages[-1].extract_text().split())
    for r in coverage:
        assert r["state"] in appendix
    manifest = rows(out / "protected_input_manifest.csv")
    assert all(r["unchanged"] == "TRUE" and r["md5_before"] == r["md5_after"] for r in manifest)
    return {
        "pages": len(pdf.pages),
        "state_plots": len(estimated),
        "coefficient_figures": 2,
        "jurisdictions_accounted_for": 51,
        "all_captions_match_exports": True,
        "all_figures_exactly_once": True,
        "all_state_counts_reconcile": True,
        "primary_plot_points": len(points),
        "original_files_verified_unchanged": len(manifest),
        "source_records": len(included),
        "added_state_only": sum(r["origin"] == "unmatched_early" for r in included),
    }


if __name__ == "__main__":
    result = validate(Path(os.environ["TALENT_DETS_DATA_DIR"]))
    scratch = Path(__file__).resolve().parents[2] / "tmp/pdfs/yearly_state_rd_state_direct"
    scratch.mkdir(parents=True, exist_ok=True)
    (scratch / "pdf_validation.json").write_text(json.dumps(result, indent=2), encoding="utf-8")
    print(json.dumps(result, indent=2))

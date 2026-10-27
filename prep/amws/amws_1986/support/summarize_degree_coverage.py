"""Validate manual AMWS degree coding and produce the reconciled audit outputs.

Run after sample_degree_coverage.R and manual review_A/B/C/D.jsonl coding.
Reconciled replacements come from explicitly reviewed second_review_* JSONs;
this script performs no degree inference or extraction from biographies.
"""
import collections
import csv
import hashlib
import json
import math
from pathlib import Path

from degree_audit_io import ROOT, load_sample


def md5(path):
    h = hashlib.md5()
    with Path(path).open("rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def write_csv(name, rows, fields):
    with (ROOT / name).open("w", encoding="utf-8-sig", newline="") as f:
        out = csv.DictWriter(f, fieldnames=fields)
        out.writeheader()
        out.writerows(rows)


def wilson(k, n):
    if not n:
        return None, None
    z = 1.959963984540054
    p = k / n
    den = 1 + z * z / n
    center = (p + z * z / (2 * n)) / den
    margin = z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / den
    return max(0, center - margin), min(1, center + margin)


def flags(degrees, groups):
    ds = [d for d in degrees if d["group"] in groups and not d["honorary"]]
    return dict(identified=bool(ds),
                institution=any(d["institution_as_written"] for d in ds),
                year=any(d["year"] is not None for d in ds),
                institution_and_year=any(d["institution_as_written"] and
                                         d["year"] is not None for d in ds))


def main():
    sample = load_sample()
    manifest = json.loads((ROOT / "sample_manifest.json").read_text(encoding="utf-8"))
    assert list(sample) == list(range(1, 1001))
    assert len({r["source_row"] for r in sample.values()}) == 1000
    assert len({(r["doc_id"], r["lineid"], r["entry_instance"])
                for r in sample.values()}) == 1000
    assert md5(manifest["source"]) == manifest["source_md5"]
    # Independently verify every saved source field with the standard CSV parser.
    selected = {int(r["source_row"]): r for r in sample.values()}
    matched = set()
    with Path(manifest["source"]).open(encoding="utf-8-sig", newline="") as f:
        for source_row, original in enumerate(csv.DictReader(f), 1):
            if source_row in selected:
                saved = selected[source_row]
                assert all(saved[k] == v for k, v in original.items()), source_row
                matched.add(source_row)
    assert len(matched) == 1000 and source_row == manifest["population_n"]
    rows = {}
    for index, letter in enumerate("ABCD"):
        p = ROOT / f"review_{letter}.jsonl"
        rs = [json.loads(x) for x in p.read_text(encoding="utf-8").splitlines()]
        assert sorted(r["sample_id"] for r in rs) == list(range(index * 250 + 1,
                                                               index * 250 + 251))
        for r in rs:
            assert r["reviewer"] == letter
            assert r["sample_id"] not in rows
            rows[r["sample_id"]] = r
    adjudications, second_ids = [], set()
    for pair in ("A_of_B", "B_of_C", "C_of_D", "D_of_A"):
        proposals = json.loads((ROOT / f"second_review_{pair}.json").read_text(encoding="utf-8"))
        log = json.loads((ROOT / f"second_review_{pair}_log.json").read_text(encoding="utf-8"))
        ids = (log["ids_reviewed"] if isinstance(log, dict)
               else [x["sample_id"] for x in log])
        second_ids.update(ids)
        for p in proposals:
            i = p["sample_id"]
            assert i in ids and p["replacement"]["sample_id"] == i
            adjudications.append(dict(sample_id=i, second_reviewer=pair[0],
                                      reason=p["reason"], before=rows[i],
                                      after=p["replacement"]))
            rows[i] = p["replacement"]
    ledger, reviewed, quote_repairs = [], [], []
    valid_groups = {"bachelors", "phd", "other_doctorate", "other_bachelor"}
    valid_status = {"complete", "partial", "not_reported", "unreadable", "cross_reference"}
    for i in range(1, 1001):
        r, source = rows[i], sample[i]
        assert r["full_review"] is True and r["status"] in valid_status
        for field in ("overlapping_biographies", "ocr_ambiguous"):
            assert isinstance(r[field], bool)
        for j, d in enumerate(r["degrees"], 1):
            # Original coding is retained unchanged in reviewer JSONL files.
            # Decode residual CSV quote escapes only if this yields an exact
            # substring of the independently reconstructed source text.
            text = source[d["evidence_source"]]
            for field in ("evidence", "label_as_written", "institution_as_written", "year_raw"):
                value = d[field]
                if not value or value in text:
                    continue
                candidate = value
                while '""' in candidate and candidate not in text:
                    candidate = candidate.replace('""', '"')
                if candidate in text:
                    quote_repairs.append(dict(sample_id=i, degree_id=j, field=field,
                                              before=value, after=candidate))
                    d[field] = candidate
            assert d["group"] in valid_groups and isinstance(d["honorary"], bool)
            assert d["evidence_source"] in ("raw_text", "raw_text_adjusted")
            assert d["evidence"] and d["evidence"] in source[d["evidence_source"]]
            for field in ("label_as_written", "institution_as_written", "year_raw"):
                assert not d[field] or d[field] in d["evidence"], (i, field, d[field])
            assert d["label_as_written"]
            assert d["year"] is None or (isinstance(d["year"], int) and
                                          1800 <= d["year"] <= 1986 and d["year_raw"])
            ledger.append(dict(sample_id=i, degree_id=j,
                               source_row=source["source_row"], doc_id=source["doc_id"],
                               lineid=source["lineid"], entry_instance=source["entry_instance"],
                               name_raw=source["name_raw"], **d))
        target = {g: flags(r["degrees"], {g}) for g in ("bachelors", "phd")}
        z = dict(source)
        z.update(reviewer=r["reviewer"], full_review=True, review_status=r["status"],
                 second_reviewed=i in second_ids,
                 overlapping_biographies=r["overlapping_biographies"],
                 ocr_ambiguous=r["ocr_ambiguous"], review_note=r["note"],
                 degrees_json=json.dumps(r["degrees"], ensure_ascii=False))
        for group, ff in target.items():
            for metric, value in ff.items():
                z[f"{group}_{metric}"] = bool(value)
        z["both_degrees_identified"] = all(target[g]["identified"] for g in target)
        z["both_degrees_complete"] = all(target[g]["institution_and_year"] for g in target)
        reviewed.append(z)
    coverage = []
    groups = {"bachelors": {"bachelors"}, "phd": {"phd"},
              "any_bachelor": {"bachelors", "other_bachelor"},
              "other_doctorate": {"other_doctorate"}}
    for group, members in groups.items():
        ff = {i: flags(rows[i]["degrees"], members) for i in rows}
        n_identified = sum(v["identified"] for v in ff.values())
        for metric in ("identified", "institution", "year", "institution_and_year"):
            k = sum(v[metric] for v in ff.values())
            for population, n in (("all_sampled_records", 1000),
                                   ("records_with_identified_degree", n_identified)):
                if metric == "identified" and population == "records_with_identified_degree":
                    continue
                lo, hi = wilson(k, n)
                coverage.append(dict(degree_group=group, metric=metric,
                                     denominator_population=population, numerator=k,
                                     denominator=n, percent=100 * k / n if n else None,
                                     ci95_lower_percent=100 * lo if lo is not None else None,
                                     ci95_upper_percent=100 * hi if hi is not None else None,
                                     ci_method="Wilson (binomial; no finite population correction)"))
    for metric, field in (("identified", "both_degrees_identified"),
                           ("institution_and_year", "both_degrees_complete")):
        k = sum(r[field] for r in reviewed)
        lo, hi = wilson(k, 1000)
        coverage.append(dict(degree_group="both_bachelors_and_phd", metric=metric,
                             denominator_population="all_sampled_records", numerator=k,
                             denominator=1000, percent=k / 10,
                             ci95_lower_percent=100 * lo, ci95_upper_percent=100 * hi,
                             ci_method="Wilson (binomial; no finite population correction)"))
    # Reconcile record counts independently to sets reconstructed from the ledger.
    for c in coverage:
        if c["degree_group"] not in groups:
            continue
        dd = [d for d in ledger if d["group"] in groups[c["degree_group"]]
              and not d["honorary"]]
        pred = {"identified": lambda d: True,
                "institution": lambda d: bool(d["institution_as_written"]),
                "year": lambda d: d["year"] is not None,
                "institution_and_year": lambda d: bool(d["institution_as_written"]) and
                                                    d["year"] is not None}[c["metric"]]
        assert c["numerator"] == len({d["sample_id"] for d in dd if pred(d)})
    write_csv("reviewed_sample_1000.csv", reviewed, list(reviewed[0]))
    write_csv("degree_extractions.csv", ledger, list(ledger[0]))
    write_csv("degree_coverage.csv", coverage, list(coverage[0]))
    (ROOT / "adjudication_log.json").write_text(json.dumps(adjudications, indent=2,
                                                         ensure_ascii=False), encoding="utf-8")
    validation = dict(sample_n=len(sample), reviewed_n=len(reviewed),
                      degree_extraction_n=len(ledger), all_sample_ids_distinct=True,
                      all_reviews_complete=True, exact_evidence_validated=True,
                      all_sample_source_fields_match=True,
                      source_checksum_unchanged=True,
                      source_md5=manifest["source_md5"],
                      ledger_counts_reconcile=True, independently_reread_n=len(second_ids),
                      adjudication_n=len(adjudications),
                      quote_escape_repairs_n=len(quote_repairs),
                      status_counts=dict(collections.Counter(r["status"] for r in rows.values())),
                      overlapping_biographies_n=sum(r["overlapping_biographies"] for r in rows.values()),
                      ocr_ambiguous_n=sum(r["ocr_ambiguous"] for r in rows.values()))
    assert md5(manifest["source"]) == manifest["source_md5"]
    (ROOT / "validation_report.json").write_text(json.dumps(validation, indent=2), encoding="utf-8")
    (ROOT / "evidence_serialization_repairs.json").write_text(
        json.dumps(quote_repairs, indent=2, ensure_ascii=False), encoding="utf-8")
    make_report(coverage, reviewed, ledger, validation, manifest)
    for c in coverage:
        if c["degree_group"] in ("bachelors", "phd", "both_bachelors_and_phd") and c["denominator_population"] == "all_sampled_records":
            print(c["degree_group"], c["metric"], c["numerator"], f"{c['percent']:.1f}%")
    print("Validation:", json.dumps(validation))


def make_report(coverage, reviewed, ledger, validation, manifest):
    def get(g, metric, pop="all_sampled_records"):
        return next(c for c in coverage if c["degree_group"] == g and
                    c["metric"] == metric and c["denominator_population"] == pop)
    lines = ["AMWS 1986 degree-coverage audit", "",
             f"Population: {manifest['population_n']:,} full-edition digitized records.",
             "Sample: 1,000 records, uniformly sampled without replacement using R seed 42.",
             "All sampled raw_text_adjusted biographies manually read and coded by Codex reviewers.",
             f"Independent second reading: {validation['independently_reread_n']} difficult records; "
             f"{validation['adjudication_n']} explicit corrections reconciled.", "",
             "Coverage among all 1,000 records (record has at least one qualifying degree):",
             "Metric                         BA/BS family        PhD/DPhil"]
    for metric, label in (("identified", "Degree identified"), ("institution", "Institution recovered"),
                           ("year", "Year recovered"), ("institution_and_year", "Institution and year")):
        values = [get(g, metric) for g in ("bachelors", "phd")]
        lines.append(f"{label:30} " + "    ".join(f"{c['numerator']:3} ({c['percent']:4.1f}%)" for c in values))
    lines += ["", "95% confidence intervals for full-sample proportions (Wilson):"]
    for g, label in (("bachelors", "BA/BS"), ("phd", "PhD/DPhil")):
        for metric in ("identified", "institution_and_year"):
            c = get(g, metric)
            lines.append(f"{label} {metric.replace('_',' ')}: {c['ci95_lower_percent']:.1f}%–{c['ci95_upper_percent']:.1f}%.")
    lines += ["", "Completeness conditional on identifying the degree:"]
    for g, label in (("bachelors", "BA/BS"), ("phd", "PhD/DPhil")):
        items = [get(g, m, "records_with_identified_degree") for m in
                 ("institution", "year", "institution_and_year")]
        lines.append(f"{label} (n={items[0]['denominator']}): institution {items[0]['percent']:.1f}%, "
                     f"year {items[1]['percent']:.1f}%, both {items[2]['percent']:.1f}%.")
    for m in ("identified", "institution_and_year"):
        c = get("both_bachelors_and_phd", m)
        lines.append(f"Both BA/BS and PhD {m.replace('_',' ')}: {c['numerator']} ({c['percent']:.1f}%).")
    broad = get("any_bachelor", "identified")
    lines += [f"Including other explicit bachelor credentials: {broad['numerator']} records ({broad['percent']:.1f}%).",
              "", "Equivalent labels preserved separately (award rows, not distinct records):"]
    variants = collections.Counter((d["group"], d["label_as_written"]) for d in ledger
                                   if not d["honorary"])
    for group, label in (("bachelors", "BA"), ("bachelors", "AB"), ("bachelors", "BS"),
                         ("bachelors", "BSc"), ("bachelors", "SB"), ("bachelors", "ScB"),
                         ("phd", "PhD"), ("phd", "DPhil")):
        lines.append(f"{label}: {variants[(group, label)]} explicit award rows.")
    lines += ["Additional specialized and OCR labels are retained in degree_extractions.csv.",
              "", "Definitions and limitations:",
              "BA/BS family includes BA/AB, BS/SB/ScB, BSc and explicit BS specializations (e.g., BSEE, BSA, BSPharm).",
              "Engineering/professional labels without explicit BA/BS family (BE, BChE, BEE, BAS, BPharm, etc.) are recorded separately.",
              "Medical MB BS is a medical bachelor qualification, not a BA/BS science degree.",
              "PhD includes DPhil and clearly readable OCR variants; ScD/DSc, MD and other doctorates are separate.",
              "Explicit honorary awards are preserved but excluded from primary and broad non-honorary counts.",
              "Other doctorate rows not marked honorary are not independently verified as earned awards; unknown cases have notes.",
              "Hons on a bachelor course is an honors degree, not an honorary award.",
              "Institution names remain as written, including abbreviations and readable OCR variants; canonical institution IDs were not assigned.",
              "Two-digit years expanded only when biographical/professional context supports the century. Missing/truncated dates not inferred from jobs.",
              "One literal bachelor year (sample 731: BSc 1941 with parsed birth 1926) is unusually early; retained with chronology caution.",
              "Multiple degrees retained; record coverage counts each person-row once. Complete coverage requires institution/year on the same degree.",
              "The full file contains damaged fragments and cross-references; these stay in the sample denominator.",
              "This estimates recoverability in digitized entries, not degree prevalence among scientists or extraction accuracy against scanned pages.",
              "No external sources used. Missing reporting does not imply no degree. Current panels/cleaned sources were not changed.",
              "Wilson intervals use a binomial approximation without finite-population correction; the sample fraction is about 1.05%.",
              "", "Text-quality diagnostics:"]
    for k, n in sorted(validation["status_counts"].items()):
        lines.append(f"Reviewer status {k}: {n} ({n/10:.1f}%).")
    lines += [f"Overlapping biographies flagged: {validation['overlapping_biographies_n']} ({validation['overlapping_biographies_n']/10:.1f}%).",
              f"Ambiguous education OCR flagged: {validation['ocr_ambiguous_n']} ({validation['ocr_ambiguous_n']/10:.1f}%).",
              "These flags overlap and are not mutually exclusive; statuses describe the biography as reviewed, not a degree-extraction error rate.",
              "", "Representative reviewed examples (sample ID; source key in reviewed CSV):"]
    example_notes = {751: "Clear BS 1966 at Univ Pittsburgh and PhD 1971 at Univ Wis-Madison.",
                     754: "BSc 1965 and PhD 1969 share Univ Western Ont; repeated institution inherited within the same clause.",
                     806: "Oxford DPhil 1979; no bachelor information supplied.",
                     755: "Univ Mich BS year reads Si; left unresolved. Univ Chicago PhD 1960 readable.",
                     797: "PhD 1966 readable, institution destroyed; no guessed university expansion.",
                     211: "Focal Rudolph see-previous stub followed by complete Camp education; Camp degrees excluded.",
                     860: "Earned BS/PhD retained; two explicitly honorary DSc awards kept separately.",
                     950: "Severely damaged possible doctoral label excluded as insufficiently certain."}
    for i, note in example_notes.items():
        source = reviewed[i - 1]
        ds = [d for d in ledger if d["sample_id"] == i]
        lines += [f"Sample {i}: {source['name_raw']}. {note}"]
        if ds:
            for evidence in dict.fromkeys(d["evidence"] for d in ds):
                lines.append("Evidence: " + evidence)
        else:
            lines.append("Evidence: " + source["raw_text_adjusted"])
    lines += ["", "Assessment:",
              "Education extraction is feasible across this edition. Once a target degree is recognizable, bachelor details are usually complete; PhD institution/year recovery has more OCR losses.",
              "A full-edition extraction should isolate the focal biography, parse education clauses with shared-institution context, preserve literal evidence, and route damaged/overlapping clauses for review.",
              "This pilot is a reviewed coverage estimate. A future automated extractor needs separate held-out evaluation of degree, institution and year attribution.",
              "", "Files:",
              "sample_1000.csv: immutable sampled source rows, including both raw text fields.",
              "reviewed_sample_1000.csv: reviewed records and degree-coverage flags.",
              "degree_extractions.csv: one row per explicit award with year, institution, supporting text and notes.",
              "degree_coverage.csv: unconditional/conditional counts, percentages and confidence intervals.",
              "sample_manifest.json, validation_report.json, adjudication_log.json: reproducibility and validation.",
              "review_A/B/C/D.jsonl and second-review logs: original manual coding and reconciliation provenance.",
              "serialization_repair.json and evidence_serialization_repairs.json: corrected residual CSV quote escapes; sample membership and degree classifications unchanged."]
    (ROOT / "findings_report.txt").write_text("\n".join(lines) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()

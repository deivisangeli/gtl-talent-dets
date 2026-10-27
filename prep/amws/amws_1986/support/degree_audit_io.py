"""I/O helpers for explicit, manually coded degree audits (no degree classifier)."""
import csv
import json
import os
from pathlib import Path

DATA_ROOT = Path(os.environ.get("TALENT_DETS_DATA_DIR", str(
    Path.home() / "Globtalent Dropbox" / "gtl_talent_dets")))
ROOT = DATA_ROOT / "output" / "amws" / "education_pilot"


def load_sample():
    with (ROOT / "sample_1000.csv").open(encoding="utf-8-sig", newline="") as f:
        return {int(r["sample_id"]): r for r in csv.DictReader(f)}


def show(start, end):
    for i, r in load_sample().items():
        if start <= i <= end:
            print(f"\n[{i}] {r['name_raw']} | birth={r['birth_year']}")
            print(r["raw_text_adjusted"])


def save_manual(reviewer, records):
    """Validate explicit decisions and append them to the reviewer's own file."""
    sample = load_sample()
    path = ROOT / f"review_{reviewer}.jsonl"
    prior = []
    if path.exists():
        prior = [json.loads(x) for x in path.read_text(encoding="utf-8").splitlines()]
    ids = {r["sample_id"] for r in prior}
    for r in records:
        assert r["sample_id"] not in ids
        assert r["sample_id"] in sample
        assert r["full_review"] is True and r["reviewer"] == reviewer
        for d in r["degrees"]:
            assert d["evidence"] in sample[r["sample_id"]][d["evidence_source"]]
        ids.add(r["sample_id"])
    with path.open("a", encoding="utf-8", newline="\n") as f:
        for r in records:
            f.write(json.dumps(r, ensure_ascii=False) + "\n")
    print(f"Saved {len(records)}; total {len(ids)} for {reviewer}")


class ManualBatch:
    def __init__(self, reviewer="D"):
        self.reviewer, self.records, self.sample = reviewer, [], load_sample()

    def row(self, sample_id, degrees=(), status=None, overlap=False, ocr=False, note=""):
        # Degree tuples: group, label, institution, raw year, resolved year,
        # optionally honorary and note. No content is classified by this helper.
        text = self.sample[sample_id]["raw_text_adjusted"]
        out = []
        for values in degrees:
            group, label, institution, year_raw, year = values[:5]
            honorary = values[5] if len(values) > 5 else False
            degree_note = values[6] if len(values) > 6 else ""
            # A verbatim context for auditing each explicit manual extraction.
            start = text.find(institution) if institution else text.find(label)
            if start < 0:
                start = text.find(label)
            if start < 0:
                start = 0
            label_pos = text.find(label, start)
            if label_pos < 0:
                label_pos = start
            end = label_pos + len(label)
            if year_raw:
                year_pos = text.find(year_raw, end)
                if year_pos >= 0:
                    end = year_pos + len(year_raw)
            evidence = text[start:max(end, start + 1)]
            out.append(dict(group=group, label_as_written=label,
                            institution_as_written=institution, year_raw=year_raw,
                            year=year, honorary=honorary, evidence=evidence,
                            evidence_source="raw_text_adjusted", note=degree_note))
        target = [d for d in out if d['group'] in ('bachelors', 'phd') and not d['honorary']]
        if status is None:
            status = ("complete" if target and all(d["institution_as_written"] and d["year"]
                                                   for d in target) else
                      "partial" if target else "not_reported")
        self.records.append(dict(sample_id=sample_id, reviewer=self.reviewer,
                                 full_review=True, status=status,
                                 overlapping_biographies=overlap,
                                 ocr_ambiguous=ocr, note=note, degrees=out))

    def save(self):
        save_manual(self.reviewer, self.records)


if __name__ == "__main__":
    import sys
    sys.stdout.reconfigure(encoding="utf-8")
    show(int(sys.argv[1]), int(sys.argv[2]))

# Frozen Physical rules: fresh 500-entry blind audit

This is a second evaluation of the unchanged `amws_ed16_science_v2` rules.
It uses 500 new 1986 entries from v06, independently of the earlier 100-entry
evaluation. It does not revise the classifier or the published population dummy.
See [the revision workflow](RULES_V2.md) for the vocabulary and OCR design.

## Sample and reference labels

The retained 1986 population has 79,073 keys. Excluding every key in the four
original review samples, the preceding fresh 100-entry evaluation and an existing
additional review manifest excludes 1,283 retained keys. The eligible pool is
77,790. The additional manifest contributes 19 keys beyond the other exclusions.
Use `random.Random(20261025).sample(sorted(eligible_keys), 500)` without replacement.
There is zero overlap with the known prior reviews. The source checksums,
classifier snapshots and ordered draw are frozen before reviewing biographies.

Review displays source biography text only, initially the discipline heading and
full text for ambiguous or damaged cases. Predictions, recovered fields and rule
evidence are not displayed. Every entry receives an explicit manual label, literal
evidence with zero-based, end-exclusive Unicode offsets, reasoning and confidence.
Manual labels are frozen and hashed before either model is evaluated. Difficult
entries are retained. Codex manual interpretation is the reference standard;
there is no independent human expert validation.

The broad Physical definition includes engineering, mathematics, statistics and
computing. A distinct Physical component alongside Biological work receives 1.
Biochemistry/biophysics alone remain 0. Full research and professional work may
establish a Physical component even when the heading is broad or Biological.
Degrees, memberships and affiliations alone do not suffice. Appended biographies
are excluded from the target's evidence. Substantive organic synthesis is
Physical even when applied to drugs; enzyme/protein/lipid biochemistry without
a separate Physical field is Biological. The before-prediction adjudication of
sample 23 is retained in `blind_review_amendments.json`.

## Paired results

There are 281 manual Physical, 218 manual non-Physical and one unassessable entry.
The latter is a truncated personal fragment with no usable field evidence.

| Metric | Original guarded rules | Frozen v2 |
|---|---:|---:|
| Precision | 236/242 = 97.52% | 240/243 = 98.77% |
| Recall | 236/281 = 83.99% | 240/281 = 85.41% |
| Correct / assessable | 448/499 = 89.78% | 455/499 = 91.18% |
| False positives / false negatives | 6 / 45 | 3 / 41 |
| Unresolved / sampled | 68/500 = 13.6% | 64/500 = 12.8% |

V2 95% Wilson intervals: precision **96.43–99.58%**, recall **80.80–89.06%**,
accuracy **88.37–93.37%**. Original precision is **94.70–98.86%** and recall is
**79.24–87.81%**. Metrics exclude the unclear reference, with explicit denominators.
Wilson intervals describe sampling uncertainty, not uncertainty in reference
judgments. The all-500 accuracy bounds are 89.6–89.8% for original and 91.0–91.2%
for v2, treating the unassessable case as incorrect versus correct.

All seven changed binary predictions are corrections: three biological-chemistry
false positives removed, two paleontology misses recovered, and two image
processing misses recovered. One paleontology match uses accent removal. Every
error, change, unassessable case and OCR-assisted match has a case explanation.
The remaining false positives are bioorganic, lipid and protein chemistry.
Misses include omitted specialties, severe OCR damage, Physical work visible
only in biography bodies and a recovered field that drops signal processing.
No rule or frozen reference label was changed after viewing predictions.

## Deliverables and reproduction

Separate output folder:
`output/amws/physical_science_rules_v2_audit_500_20261023/`.
It contains the 500-row audit CSV, source/sample manifests, original and v2
snapshots, frozen labels, paired predictions, case explanations, error summaries,
report, code snapshots, export verification and final validation/checksums.
The CSV preserves source text and provenance, manual evidence/offsets, both
predictions, explicit mixed/unresolved flags and input sources. A classifier
zero means no Physical detection; unresolved zeros remain identifiable.

From the repository root (Python and Node are the bundled runtimes):

```powershell
python -X utf8 prep/amws/fields/rules_v2_audit_500.py prepare
python -X utf8 prep/amws/fields/rules_v2_audit_500.py review --start 1 --end 50
python -X utf8 prep/amws/fields/rules_v2_audit_500.py review --start 76 --end 76 --full
python -X utf8 prep/amws/fields/rules_v2_audit_500.py add --labels '<manual batch.json>'
python -X utf8 prep/amws/fields/rules_v2_audit_500.py freeze-manual
python -X utf8 prep/amws/fields/rules_v2_audit_500.py evaluate
python -X utf8 prep/amws/fields/rules_v2_audit_500.py report
node prep/amws/fields/export_rules_v2_audit_500.mjs
python -X utf8 prep/amws/fields/rules_v2_audit_500.py validate
```

Set `TALENT_DETS_DATA_DIR` or pass `--data-root` to override the Dropbox root.
Archived manual labels and case notes are reproduction inputs; they cannot be
recreated automatically from predictions. Existing frozen labels are never
edited by the evaluator. Use the supplied artifact-tool dependencies for CSV
export; `AMWS_ARTIFACT_NODE_MODULES` can override their location.

Validation reconstructs the seeded draw, checks 500 distinct keys and zero
prior overlap, verifies exact-key source joins and literal evidence/repair spans,
recomputes both models and metrics, compares every CSV cell and verifies all
original input/release checksums. Mixed and unresolved predictions are distinct.
Both models retain their prior frozen code hashes and the population dummy is
preserved. Results apply to these retained 1986 entries and the stated broad
definition; they do not evaluate an automated cross-edition classifier.

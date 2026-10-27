# Fresh blind confirmation of earlier-edition source-field classification

This confirms the unchanged source-field adapter from the previous
[post-hoc diagnostic](CROSS_EDITION_BLIND_AUDIT.md) on a new untouched sample.
There are 50 new entries from each of 1906, 1938 and 1955 (150 total), excluding
every known prior review key, including the immediately preceding four-edition
test. Output:
`output/amws/physical_science_early_editions_blind_audit_50_20261023/`.

## Freeze and sampling

The adapter is byte-identical to the preceding diagnostic, SHA-256
`3e938d3e541a80ed7f5b21cee3afef20a2de80e20f26b9f0a77cdaa77cd40b39`.
It and all v2 rules/normalization dependencies are copied and checksum-frozen
before the new draw. No vocabulary, exception, OCR alias or input-handling
changes are made for these results.

Use one `random.Random(20261027)` generator and sample 50 sorted eligible keys
without replacement, chronologically: 1906, 1938, then 1955.

| Edition | Retained v06 population | Excluded reviewed keys | Eligible | Sample |
|---|---:|---:|---:|---:|
| 1906 | 3,555 | 177 | 3,378 | 50 |
| 1938 | 18,963 | 187 | 18,776 | 50 |
| 1955 | 32,720 | 208 | 32,512 | 50 |

Original fields are reread from workbook column 4 in `input/amws/amws_1906.xlsx`,
`field_legacy` in `amws_1938.dta` and `specialization` in `amws_1955.dta`. Join
by edition and original lineid, never row position. Full biographies are joined
by source_key to the existing biography ledger. Original source strings and
provenance are retained.

## Blind reference and frozen classifier

All 150 original fields and full available biographies were reviewed without
predictions or rule evidence displayed. Explicit manual labels, literal evidence,
Unicode offsets, reasoning and confidence were frozen before classification.
Labels can be 1, 0 or unclear; none of this sample required an unclear reference.
One blank 1906 biography still has an unambiguous original Medicine field.
Damaged and difficult entries are retained without replacement.

Physical includes chemistry, physics, astronomy, Earth sciences, all engineering,
mathematics, statistics and computing. A separate Physical component alongside
Biological work receives 1; biochemistry/biophysics alone remain Biological.
Source headings more specific than generic stored fields are used manually.
Substantive professional/research work can establish a component missing from
the field, while degrees, memberships and affiliations alone cannot. Brief
assistant duties without an independent specialty are not sufficient. Medium-
confidence boundary interpretations are recorded explicitly.

The frozen adapter classifies only the verified original field. It calls the
same frozen v2 API with an education terminator after that field. The terminator
provides no classification evidence; every excerpt maps to the literal field.
No full-biography keyword search or edition-16 heading-support requirement is
used. Missing/unusable fields remain unresolved. The binary equals Physical
match, including mixed Physical+Biological, and unresolved zeros are identifiable.

## Fresh results

| Edition | TP | TN | FP | FN | Accuracy | Physical precision | Physical recall |
|---|---:|---:|---:|---:|---|---|---|
| 1906 | 26 | 23 | 0 | 1 | 49/50 = 98% | 26/26 = 100% | 26/27 = 96.30% |
| 1938 | 31 | 18 | 0 | 1 | 49/50 = 98% | 31/31 = 100% | 31/32 = 96.88% |
| 1955 | 44 | 1 | 2 | 3 | 45/50 = 90% | 44/46 = 95.65% | 44/47 = 93.62% |

| Edition | Accuracy 95% Wilson CI | Precision 95% CI | Recall 95% CI | Classifier unresolved |
|---|---|---|---|---:|
| 1906 | 89.50–99.65% | 87.13–100% | 81.72–99.34% | 4/50 |
| 1938 | 89.50–99.65% | 88.97–100% | 84.26–99.45% | 1/50 |
| 1955 | 78.64–95.65% | 85.47–98.80% | 82.84–97.81% | 1/50 |

There are 143 correct, seven incorrect and zero unassessable references across
the equally allocated sample. Raw pooled sample accuracy is 143/150 = 95.33%,
which is not a population-weighted accuracy estimate. Small samples give wide
intervals. The 1955 reference contains only three non-Physical cases, two of
which are false positives, so overall accuracy alone is insufficient.

The 1906 miss is Physical geography behind a broad Geography field. The 1938
miss is the unrecognized Hydraulics specialty. For 1955, two false positives
have generic Chemistry fields hiding Biological/Physiological Chemistry in
their original biography headings. Three misses involve unrecognized
Metallography and separate prior Physical chemistry work behind Biochemistry.
The latter two manual interpretations are medium confidence and are preserved
with their evidence and reasoning. Every error is explained in the CSV/report.

These results are a fresh test of the pre-draw frozen adapter, unlike the prior
diagnostic. The reference remains prediction-blinded Codex manual judgment,
without independent human expert validation. Wilson intervals describe sampling
uncertainty, not uncertainty in the reference labels. No rules or labels were
tuned after viewing these predictions. No population dummy is replaced.

## Reproduction and validation

From the repository root, using bundled Python/Node and R with haven/readxl:

```powershell
python -X utf8 prep/amws/fields/early_editions_blind_audit.py prepare
python -X utf8 prep/amws/fields/early_editions_blind_audit.py review --start 1 --end 50 --full
python -X utf8 prep/amws/fields/early_editions_blind_audit.py add --labels '<manual batch.json>'
python -X utf8 prep/amws/fields/early_editions_blind_audit.py freeze-manual
python -X utf8 prep/amws/fields/early_editions_blind_audit.py evaluate
python -X utf8 prep/amws/fields/early_editions_blind_audit.py report
node prep/amws/fields/export_early_editions_blind_audit.mjs
python -X utf8 prep/amws/fields/early_editions_blind_audit.py validate
```

Use `TALENT_DETS_DATA_DIR` or `--data-root` and optionally `--rscript`.
Frozen manual labels and case explanations are reproduction inputs, not
regenerated from predictions. CSV export uses the bundled artifact tool and
verifies every cell before serializing.

Validation reconstructs the seeded draw, requires 50 distinct keys per edition
and zero previous overlap, rereads the original source files, checks all field,
name, biography and provenance joins, verifies every literal manual/model/OCR
span, recomputes metrics and checks every exported CSV cell. Model/adapter hashes
must match their pre-draw snapshots and the previous adapter. Source and prior
release checksums must remain unchanged. Mixed/unresolved flags remain distinct.
The release includes the source inventory, frozen sample, labels, predictions,
audit CSV, report, code/model snapshots, validation and checksum manifests.

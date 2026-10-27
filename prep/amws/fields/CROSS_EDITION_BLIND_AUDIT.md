# Fifty new blind references per edition

This separate audit draws 50 new entries from each of 1906, 1938, 1955 and
1986 in the v06 retained release. Output:
`output/amws/physical_science_cross_edition_blind_audit_50_20261023/`.
It preserves the source datasets, all earlier reviews and the population dummy.

## Frozen sample and labels

Use one `random.Random(20261026)` generator, sampling 50 sorted eligible keys
without replacement for each edition in chronological order. All known prior
review keys are excluded, including the latest 500-entry 1986 blind audit.

| Edition | Retained population | Previously reviewed/excluded | Eligible | New sample |
|---|---:|---:|---:|---:|
| 1906 | 3,555 | 127 | 3,428 | 50 |
| 1938 | 18,963 | 137 | 18,826 | 50 |
| 1955 | 32,720 | 158 | 32,562 | 50 |
| 1986 | 79,073 | 1,783 | 77,290 | 50 |

Rules and input hashes are frozen before drawing. Earlier fields are reread
from workbook column 4 (1906), `field_legacy` (1938) and `specialization` (1955),
joining by edition and original lineid. The 1986 fields reuse the keyed published
audit. Biographies join by source_key to the existing biography ledger.

All 200 records receive manual binary Physical labels, literal evidence and
Unicode offsets, reasoning and confidence. Review displays original source
fields for earlier editions and biography text, with full biographies for
ambiguous cases. Predictions and rule evidence stay hidden until every label
is frozen. Engineering, mathematics, statistics, computing and Physical plus
Biological work receive 1. Biochemistry and biophysics alone receive 0. Substantive
professional/research work can establish a separate Physical field; degrees,
memberships and affiliations alone cannot. Appended biographies are excluded.
These are Codex manual judgments, without independent human expert validation.

Manual counts are 34 Physical/16 other (1906), 26/24 (1938), 48/2 (1955), and
29/21 (1986). All 200 are assessable. The missing-name 1986 target retains a
readable professional mathematics career before unrelated appended biographies.
Interdisciplinary and damaged cases retain explicit medium-confidence reasoning.

## Original frozen pipeline transport result

The original and v2 binaries agree on this sample. The unmodified edition-16
pipeline has an input-format failure on earlier editions: it requires an
original field to appear in the extracted biography heading. 1906 and 1938
biography-ledger text lacks that heading, while 1955 places addresses and
discipline before birth information. Most real fields are therefore discarded.
Some accidental earlier positives instead use address/body words. These are
pipeline transport findings, not reliable use of the earlier source fields.

| Edition | Correct/50 | Accuracy | Physical precision | Physical recall | Unresolved/50 |
|---|---:|---:|---:|---:|---:|
| 1906 | 16 | 32% | Undefined (0/0) | 0/34 = 0% | 50 |
| 1938 | 26 | 52% | 2/2 = 100% | 2/26 = 7.69% | 48 |
| 1955 | 6 | 12% | 4/4 = 100% | 4/48 = 8.33% | 46 |
| 1986 | 44 | 88% | 23/23 = 100% | 23/29 = 79.31% | 9 |

## Post-hoc source-field diagnostic

After viewing that failure, freeze `cross_edition_field_adapter.py` and apply
the unchanged v2 API to each verified earlier-edition discipline field in
isolation, followed by a terminating education marker. The marker contains no
evidence. Every matched excerpt/offset maps to the literal original field.
Unusable fields are retained as unresolved. No vocabulary, compound precedence,
OCR rule or reference label changes. The 1986 branch is unchanged.

**This format adjustment was introduced after the original results were seen.**
These earlier-edition diagnostic results do not constitute an independent
held-out performance claim. A new untouched sample is needed to validate the
adapted pipeline. The manual labels themselves remain prediction-blinded.

| Edition | Correct/50 | Accuracy (95% Wilson CI) | Precision (95% CI) | Recall (95% CI) |
|---|---:|---|---|---|
| 1906 | 48 | 96% (86.54–98.90%) | 32/32 = 100% (89.28–100%) | 32/34 = 94.12% (80.91–98.37%) |
| 1938 | 49 | 98% (89.50–99.65%) | 25/25 = 100% (86.68–100%) | 25/26 = 96.15% (81.11–99.32%) |
| 1955 | 50 | 100% (92.87–100%) | 48/48 = 100% (92.59–100%) | 48/48 = 100% (92.59–100%) |
| 1986 | 44 | 88% (76.20–94.38%) | 23/23 = 100% (85.69–100%) | 23/29 = 79.31% (61.61–90.15%) |

No false positives appear in the diagnostic; 100% point estimates do not imply
perfect population precision. Unresolved counts are 3, 2, 0 and 9, respectively.
Mixed is distinct from unresolved. Small class denominators are particularly
important for 1955, which has only two manual non-Physical examples.

Remaining misses: stereotomy/mechanism vocabulary and agricultural chemistry
work behind an animal-nutrition field (1906); chemistry work behind pharmacy
(1938); and six 1986 cases involving pharmaceutical Physical chemistry,
missing mathematics heading, drug synthesis, damaged computer-aided design,
fuel materials/chemistry and physics behind an administration heading.
Every original error and diagnostic discrepancy is explained in the CSV/report.

## Reproduction and checks

From the repository root, with bundled Python/Node and R with haven/readxl:

```powershell
python -X utf8 prep/amws/fields/cross_edition_blind_audit.py prepare
python -X utf8 prep/amws/fields/cross_edition_blind_audit.py review --start 1 --end 50
python -X utf8 prep/amws/fields/cross_edition_blind_audit.py review --start 158 --end 158 --full
python -X utf8 prep/amws/fields/cross_edition_blind_audit.py add --labels '<manual batch.json>'
python -X utf8 prep/amws/fields/cross_edition_blind_audit.py freeze-manual
python -X utf8 prep/amws/fields/cross_edition_blind_audit.py evaluate
python -X utf8 prep/amws/fields/cross_edition_blind_audit.py diagnose-fields
python -X utf8 prep/amws/fields/cross_edition_blind_audit.py report
node prep/amws/fields/export_cross_edition_blind_audit.mjs
python -X utf8 prep/amws/fields/cross_edition_blind_audit.py validate
```

Use `TALENT_DETS_DATA_DIR` or `--data-root`; `--rscript` selects R. Frozen manual
labels and case notes are required reproduction inputs. They are not inferred
from predictions. The adapter freeze explicitly records its post-comparison
timestamp and the unchanged original evaluation checksum. Both analyses remain
separate and all literal evidence is preserved.

Validation reconstructs 50 distinct keys per edition with zero prior overlap,
rereads the three original source files, verifies exact biography/field joins,
checks every manual/model/OCR evidence offset, recomputes per-edition confusion
matrices/metrics and preserves every exported CSV cell. Hashes verify unchanged
rules, frozen labels, source inputs and previous output releases. Metrics include
explicit denominators and Wilson intervals, with undefined quantities left null.
Raw equal-allocation pooled results are not population-weighted estimates.

# AMWS science fields, Physical dummy and sample audits

## Fifty new blind references per edition (2026-10-23)

The [new four-edition audit](CROSS_EDITION_BLIND_AUDIT.md) reviews 50 new entries
per edition (200 total; seed `20261026`), excluding all known prior samples.
All manual labels were frozen before any predictions, and all 200 were assessable.
The original 1986-format pipeline fails to accept most earlier-edition source
fields: its accuracy is 32% (1906), 52% (1938), 12% (1955) and 88% (1986).
These earlier results expose a heading/layout compatibility failure.

A **post-hoc format diagnostic** applies the identical frozen vocabulary directly
to exact-key original earlier-edition field columns, without changing labels,
rules or OCR handling. The existing 1986 branch stays unchanged:

| Edition | Correct / 50 | Accuracy | Physical precision | Physical recall |
|---|---:|---:|---:|---:|
| 1906 | 48 | 96% | 32/32 = 100% | 32/34 = 94.12% |
| 1938 | 49 | 98% | 25/25 = 100% | 25/26 = 96.15% |
| 1955 | 50 | 100% | 48/48 = 100% | 48/48 = 100% |
| 1986 | 44 | 88% | 23/23 = 100% | 23/29 = 79.31% |

The source-field adjustment followed exposure to the original transport results;
the earlier-edition diagnostic is **not independent held-out validation**.
A fresh sample must validate the adapter. Zero observed false positives does
not establish perfect precision; small denominators give wide Wilson intervals.
The report documents both analyses, all original errors, remaining diagnostic
misses, exact evidence, provenance, timestamps and unchanged input checksums.
Output is `output/amws/physical_science_cross_edition_blind_audit_50_20261023/`.
Reference labels remain Codex interpretations without independent human validation.

## Second fresh v2 blind audit: 500 entries (2026-10-23)

The [500-entry workflow](RULES_V2_500.md) evaluates the same frozen v2 rules on
500 new retained 1986 entries. Seed `20261025` draws from 77,790 eligible v06 keys,
excluding 1,283 keys in all known earlier reviews. Source/rule hashes and the
draw were frozen first; manual labels were frozen before either prediction was
displayed. There is no prior-review overlap or replacement of difficult entries.

| Metric, excluding one unassessable biography | Original guarded | Frozen v2 |
|---|---:|---:|
| Physical precision | 236/242 = 97.52% | 240/243 = 98.77% |
| Physical recall | 236/281 = 83.99% | 240/281 = 85.41% |
| Correct / assessable | 448/499 = 89.78% | 455/499 = 91.18% |
| False positives / false negatives | 6 / 45 | 3 / 41 |
| Unresolved / all sampled | 68/500 | 64/500 |

V2 95% Wilson intervals are **96.43–99.58%** for precision and **80.80–89.06%**
for recall. It has 455 correct, 44 incorrect and one unassessable result out of
500. Eight v2 predictions are mixed Physical+Biological, distinct from the 64
unresolved cases. Resolution coverage is 436/500 = 87.2%, including substantive
non-Physical classifications. Among unresolved cases, 30 are manually Physical,
33 non-Physical and one unclear; zero is therefore not a confirmed negative.

All seven changed predictions are corrections: three biological-chemistry false
positives, two paleontology misses and two image-processing misses. One match
uses accent removal. Remaining v2 errors comprise 19 vocabulary misses, eight
OCR failures, 13 body-only Physical cases, one recovered-field omission, and
three Biological chemistry false positives. Manual labels use substantive work
and the broad Physical policy; interdisciplinary reasoning/confidence is recorded.

Outputs are separate under
`output/amws/physical_science_rules_v2_audit_500_20261023/`, with the 500-row CSV,
frozen labels/code, exact evidence/provenance, every error/change explanation,
reproducible runner, report and validation/checksums. No classifier or reference
labels were tuned after comparison. The reference remains prediction-blinded
Codex manual review, without independent human expert validation. Original data,
earlier audits and the published population dummy remain unchanged.

## Revised Physical vocabulary and OCR audit (2026-10-23)

The separate [v2 revision](RULES_V2.md) expands missing Physical specialties,
corrects biomedical-engineering/statistics precedence, protects biological
chemistry and adds allowlisted OCR repairs with literal evidence offsets.
The original classifier and full-population dummy are unchanged. V2 has been
applied only to the fresh evaluation sample, not published over the population.

The classifier was frozen before drawing **100 previously unreviewed 1986 keys**
from v06 with seed `20261024`. Excluding 1,164 previously reviewed keys leaves
77,909 eligible entries. All manual labels were frozen before either model's
predictions were displayed. One interleaved biography was unassessable.

| Metric on the same fresh sample | Original guarded rules | Revised v2 |
|---|---:|---:|
| Physical precision | 47/48 = 97.92% | 49/50 = 98.00% |
| Physical recall | 47/56 = 83.93% | 49/56 = 87.50% |
| Correct / assessable | 89/99 | 91/99 |
| False positives / false negatives | 1 / 9 | 1 / 7 |
| Classifier-unresolved / all sampled | 14/100 | 12/100 |

V2 precision's 95% Wilson interval is **89.50–99.65%**; recall's is
**76.37–93.81%**. The small sample does not establish a statistically reliable
population improvement. V2 recovered one biometry entry through vocabulary and
one `PHY SICS` entry through OCR normalization. Two entries had OCR-assisted
evidence, one without a binary prediction change. Biological oceanography
remained the false positive; seven Physical cases were still missed. Every
error, prediction change and OCR-assisted match is documented in the report.

Outputs are under `output/amws/physical_science_rules_v2_audit_20261023/`: the
100-row CSV, frozen code/rules, source/sample manifests, blinded manual labels,
comparisons, regression results, explanations, report and validation. Old exposed
sample results are development evidence only. The fresh benchmark is Codex manual
review, not independent human expert validation. No rules or labels were tuned
against these new evaluation results. See the workflow for reproduction commands.

## Cross-edition sample (2026-10-23)

The [cross-edition sample](CROSS_EDITION_SAMPLE.md) uses the latest v06 release
(134,311 retained entries) and seed `20261023` to draw **125 entries per edition**.
Original source fields exist for all retained 1906 and 1955 entries and for
18,827/18,963 retained 1938 entries. They are recovered from workbook column 4
(1906), `field_legacy` (1938) and `specialization` (1955), keyed by edition and
original `lineid`. 1986 reuses its existing keyed audit without rereading raw files.

All 375 sampled earlier-edition fields were usable. Of the 125 sampled 1986
fields, 118 were usable, six additional fields were recovered manually from
biographies and one truncated biography remains unresolved. **499/500 entries**
therefore have identified specific fields. Original field strings and all
manual evidence, confidence and reasoning are preserved.

| Edition | Sample | Physical dummy = 1 | Mixed | Existing/main unresolved |
|---|---:|---:|---:|---:|
| 1906 | 125 | 73 | 5 | 0 |
| 1938 | 125 | 59 | 5 | 0 |
| 1955 | 125 | 125 | 0 | 0 |
| 1986 | 125 | 68 | 2 | 17 |
| Total | 500 | 325 | 12 | 17 |

Earlier-edition flags are manual; all existing 1986 flags are preserved. Separate
`review_*` columns record the manual assessment: 332 Physical, 15 mixed and one
unresolved across the sample. Seven additional 1986 Physical cases are marked as
review disagreements without altering the old dummy. The 1955 sample result is
descriptive of this retained source; it does not establish universal edition coverage.

Population-weighted shares are 64.60% Physical using the primary flags and 67.90%
using manual review, compared with raw equal-allocation sample shares of 65.0%
and 66.4%. Weights use the edition populations, not an equal 25% per edition.
This supplies sample classifications and source coverage findings, not new
cross-edition automated-classifier accuracy estimates. Existing full-population
classification remains restricted to 1986.

Outputs are under `output/amws/field_classification_sample_20261023/`: the 500-row
CSV, original source inventory, frozen sample and manual decisions, summary,
report, reproducible code, validation and checksum manifests. Earlier datasets
and releases are preserved. See the linked workflow for commands and definitions.

## Current releases (2026-10-20)

The unchanged boundary-guarded classifier has been applied to all **79,658**
retained US-birth edition-16 entries in the October 19 `multi_v04` release.
The separate output is `output/amws/physical_science_dummy_20261020/`.
Its keyed CSV contains `source_key`, `entry_id`, `name_raw` and integer
`physical_science`. Merge on `source_key`.

- **39,461 ones**, including **1,685 mixed Physical + Biological** entries.
- **40,197 zeros**, including **10,028 unresolved** entries identified in the audit.

The dummy equals `int(physical_match)`. A zero means no detected Physical match,
not a confirmed negative. Mixed and unresolved are distinct flags. See
[the dummy workflow](PHYSICAL_DUMMY.md) for commands, provenance and validation.

A subsequent [independent sample audit](ACCURACY_AUDIT.md) drew **500 new entries**
from the remaining 79,158 entries, excluding every original pilot key, using
`random.Random(20261022).sample(sorted(eligible_keys), 500)`. Codex manual labels
were frozen before predictions were displayed or joined. This is prediction-blinded
manual review, not an independent human expert gold standard.

- **461 correct, 37 incorrect and 2 unassessable** out of 500.
- Assessable accuracy: **461/498 = 92.57%**, with 95% Wilson interval **89.93–94.56%**.
- Physical precision: **251/252 = 99.60%**. Recall: **251/287 = 87.46%**.
- Errors: **36 false negatives and 1 false positive**. All-500 accuracy bounds,
  treating unassessable labels as all wrong versus all right, are **92.20–92.60%**.
- Eight classifier-mixed entries: seven correct and one incorrect. Of 58
  classifier-unresolved entries, 27 are correct zeros, 29 are missed Physical
  entries and two are unassessable.

Outputs are under `output/amws/physical_science_accuracy_audit_20261020/`:
the 500-row CSV, frozen manual labels, comparisons, every error explanation,
`evaluation.json`, `report.txt`, validation and checksum manifests. All checks
passed. Inputs, prior pilot outputs, published dummy and classifier remain unchanged.

The audit uses the agreed broad Physical definition, including engineering,
mathematics, statistics and computing; Physical + Biological work receives 1.
Biochemistry and biophysics alone remain Biological. Manual review can use
substantive professional and research work beyond the heading. The preserved
classifier searches guarded headings/recovered fields and retains the original
vocabulary and compound-term exceptions. Known misses include biomedical
engineering, unrecognized Physical specialties, damaged headings and fields
visible only in the biography body. The false positive is biological chemistry
being treated as a Physical + Biological mixture. No rules were tuned to this audit.

The workflow guides and checksum-frozen release snapshots record the implementation
at publication. Current results and cross-release status are documented here.

## Original three-way pilot

The 2026-10-20 pilot samples 500 of the latest release's 79,658 US-birth 1986
entries. It preserves entry selection and all original data. Labels are
`physical_science`, `biological_science`, or `other`.

The broad Physical group includes engineering, mathematics and computing.
Biological includes life, agricultural/food, medical and veterinary science.
Named specialties such as biochemistry, biophysics, biostatistics, bioengineering,
medicinal/pharmaceutical chemistry and physical anthropology are Biological.
Separate Physical and Biological fields produce `other_reason=mixed`.
A science field plus economics, psychology or another non-target field retains
the science group. Pure non-target, mixed and unresolved Other are distinguished.
See the delivered report for the complete mapping conventions and limitations.

## Historical pilot performance

The frozen v1 classifier agrees with 171/200 manual held-out labels (85.5%).
Physical precision/recall are 92/93 (98.9%) and 92/102 (90.2%); Biological are
67/68 (98.5%) and 67/85 (78.8%). Thirty held-out cases remain unresolved.
Manual labels are Codex interpretations, not independent human-validated truth.

A subsequent scope audit found malformed headers could include employment.
`classifier_boundary_guard.py` is a conservative **post-hoc candidate**, with
unchanged discipline vocabulary. Its 174/200 agreement on the old test partition
is regression performance, not fresh held-out evidence. V1 remains frozen and
its genuine held-out results remain separate. The guarded version was subsequently
used for the full-population dummy and the separate fresh sample audit described
above. Any subsequent vocabulary expansion needs another untouched test sample.

## API and evidence

`classifier.classify_entry(field_raw, raw_text_adjusted, name_raw='')` exposes
the original v1. The published dummy imports the same function from
`classifier_boundary_guard`; its output carries a different version/status.

Returned fields include `science_group`, `other_reason`, `review_required`,
`physical_match`, `biological_match`, `input_source`, `selected_heading`,
`matched_rule_ids`, `evidence`, and `rules_version`. Mixed is true exactly when
both science evidence flags are true. Reasons `missing`, `damaged`, `unmatched`
are abstentions; `non_target` is a substantive Other. Mixed/abstentions require
review. Unmatched Other must not be used as a clean non-science control group.

Evidence offsets are zero-based Unicode code-point indices, exclusive at the
end, into the named `evidence_source` (`field_raw` or `raw_text_adjusted`).
The CSV includes complete source text and every exact provenance match. Original
field values are recovered by exact name and biography, never row position.
CSV unprefixed prediction columns are frozen v1; `guarded_*` are post-hoc.
Manual columns are independent of predictions and remain unchanged.

## Reproduction

From the repository root, set `TALENT_DETS_DATA_DIR` or pass `--data-root`:

```powershell
python prep/amws/fields/pilot.py prepare --data-root '<Dropbox project root>'
python prep/amws/fields/pilot.py review --split development --start 1 --end 50
python -m unittest discover -s prep/amws/fields -p 'test_*.py'
```

The frozen sample uses Python sample seed 20261020 and independent shuffle seed
20261021 for the 300/200 split. Inputs and sample files have hashes. Existing
sample files are checked rather than replaced. Manual decisions are delivered
as individual records plus `manual_development.py` and `manual_heldout.py`.
They were authored manually, not inferred from keyword rules. Review packets
show headings and research; `--full` exposes the complete biography for context.

`pilot.py freeze` requires 300 development labels and no held-out gold. It
archives the original code/rules. Opening held-out review packets requires that
freeze. After all 200 manual labels are recorded, evaluate the snapshot:

```powershell
python prep/amws/fields/evaluate.py
python prep/amws/fields/evaluate_boundary_guard.py
python prep/amws/fields/publish_report.py
node prep/amws/fields/export_csv.mjs
python prep/amws/fields/validate_pilot.py
```

`evaluate.py` always loads the immutable original classifier snapshot and checks
gold hashes. `evaluate_boundary_guard.py` evaluates the post-hoc candidate on
the already exposed partitions, without claiming held-out validation. The CSV
export uses the bundled artifact tool; `AMWS_ARTIFACT_NODE_MODULES` can override
its module location. The CSV path follows `TALENT_DETS_DATA_DIR`.

Outputs are separate under `output/amws/field_regex_pilot_20261020/`: audit CSV,
source packets, explicit manual gold, original and guarded predictions, metrics,
all discrepancies, report, frozen/current source snapshots, and validation.
Existing education releases, AMWS inputs, outcome panels and estimators are not
modified. Re-running manual label scripts after evaluation is intentionally
blocked; use a fresh working copy to reconstruct historical review artifacts.

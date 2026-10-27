# Cross-edition field sample (2026-10-23)

The v06 person release supplies 134,311 retained entries: 3,555 from 1906,
18,963 from 1938, 32,720 from 1955 and 79,073 from 1986. One Python
`random.Random(20261023)` generator samples 125 sorted `source_key` values per
edition in chronological order. The sample is not filtered by earlier samples.
All 500 keys and input hashes are frozen before manual review.

## Sources and judgments

Original fields are recovered by edition and original `lineid`, never row position:

| Edition | File in `input/amws/` | Field | Retained nonblank / total |
|---|---|---|---:|
| 1906 | `amws_1906.xlsx` | Column 4, no header | 3,555 / 3,555 |
| 1938 | `amws_1938.dta` | `field_legacy` | 18,827 / 18,963 |
| 1955 | `amws_1955.dta` | `specialization` | 32,720 / 32,720 |

1986 fields and flags are reused from the published Physical dummy audit, by
exact key. Raw 1986 sources are not searched again. Biographies for every edition
come from the existing keyed biography ledger, since earlier biography columns
are blank in the combined release.

Every sampled source field receives individual manual review and normalization.
Missing or unusable values use substantive biography evidence. Literal excerpts,
source columns and Unicode offsets, reasoning and confidence are preserved.
`field_raw` is never replaced. Minor readable OCR errors can remain usable.

For earlier editions, `physical_science`, `mixed` and `unresolved` are manual
decisions. The 1986 values are preserved exactly; `review_*` columns give separate
manual judgments and `review_disagreement` marks a binary difference. Thus the
main flags combine manual earlier-edition classifications with existing 1986
regex predictions. This is a descriptive sample, not new classifier accuracy
validation or a full-population extension. Manual labels are Codex judgments,
not an independent human expert benchmark.

Physical includes chemistry, physics, astronomy, Earth sciences, engineering,
mathematics, statistics and computing. Physical + Biological work receives 1.
Biochemistry and biophysics alone remain Biological. Biomedical/agricultural
engineering count as Physical. Social/non-target fields alone receive 0.
Unresolved cases receive 0 with an explicit flag and remain in the sample.
Degrees, affiliations and memberships alone do not establish a Physical field.

## Reproduction

From the repository root, set `TALENT_DETS_DATA_DIR` or pass `--data-root`.
Python, R with haven/readxl/readr/jsonlite, and the bundled artifact-tool Node
runtime are required. `--rscript` selects R during preparation; validation uses
the Rscript on PATH. `AMWS_ARTIFACT_NODE_MODULES` can override the bundled Node
dependency directory. No dependencies are installed into the repository.

```powershell
python -X utf8 prep/amws/fields/cross_edition_sample.py prepare
python -X utf8 prep/amws/fields/cross_edition_sample.py review --start 1 --end 50
python -X utf8 prep/amws/fields/cross_edition_sample.py review --start 385 --end 385 --full
python -X utf8 prep/amws/fields/cross_edition_sample.py add --labels '<manual batch.json>'
python -X utf8 prep/amws/fields/cross_edition_sample.py freeze
python -X utf8 prep/amws/fields/cross_edition_sample.py publish
node prep/amws/fields/export_cross_edition_sample.mjs
python -X utf8 prep/amws/fields/cross_edition_sample.py validate
```

Manual batches explicitly supply each decision. Compact form is
`[sample_id, normalized_field, physical_review, mixed_review]`; the original
field is copied as literal evidence. Extended form adds evidence source, literal
excerpt, reasoning, optional confidence and source-field usability. `unclear`
is allowed for the Physical review. `revise --labels` records an explicit revision
log before freeze; revisions after freeze are blocked. Frozen manual decisions
are inputs for reproduction, not regenerated from keyword rules.

Outputs are separate under `output/amws/field_classification_sample_20261023/`:
the 500-row CSV, source inventory, source packets, manual decisions, report,
summary, code snapshots, validation and manifests. Existing releases are intact.

Report both raw sample counts and population-weighted shares:
`sum_h (N_h / 134311) * (count_h / 125)`. Each row carries `population_weight`
(`N_h/N`) and `expansion_weight` (`N_h/125`). Equal sample sizes do not imply
equal population weights. `review_physical_science` weighted shares use manual
judgments; the primary weighted flag shares retain existing 1986 predictions.
Unknown manual labels contribute no Physical match and remain separately counted.

Validation reconstructs the seeded sample, rereads all three original source
files, verifies all 375 original field/name joins, checks all 500 biography joins
and evidence spans, preserves all 125 existing 1986 flag sets, reconciles counts
and weights, checks CSV cells and confirms unchanged source hashes.

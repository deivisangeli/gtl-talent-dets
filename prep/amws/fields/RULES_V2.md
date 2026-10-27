# Physical vocabulary/OCR revision and fresh 100-entry audit

This separate v2 classifier preserves the original classifier, rules and
full-population dummy. It is evaluated on 100 previously unreviewed 1986 entries
from the v06 retained release. Output is under
`output/amws/physical_science_rules_v2_audit_20261023/`.

## Classification changes

`classifier_v2.classify_entry(field_raw, raw_text_adjusted, name_raw='')` keeps the
original input interface and returns the same core fields plus `ocr_assisted`.
Each evidence record includes `ocr_repairs` with repair kind, original text,
literal offsets and replacement where applicable. Version is
`amws_ed16_science_v2`; vocabulary and normalization settings are in `rules_v2.json`.

Added Physical terms cover electromagnetic/scattering theory, fluid dynamics,
solar energy, astrodynamics, radiation pressure, paleontology/biostratigraphy,
physical geography, dielectrics/electrical insulation, aerosol science, industrial
chemicals, reactor/refinery design, pilot plants, carbon-black/rubber materials,
control/expert systems, image processing, biometry, biometeorology, crystallography,
metal cutting, static magnetic-field analysis and computer communication aids.

Biomedical engineering, bioengineering and biological statistics now retain a
Physical component under the broad engineering/statistics definition. Economic
statistics is Physical. Biological chemistry is protected as biochemistry;
biochemistry and biophysics alone remain Biological. Independent Physical and
Biological matches produce `other_reason=mixed` and `physical_match=true`.
The binary dummy remains `int(physical_match)`.

OCR normalization removes accents while mapping characters to original Unicode
positions. A finite allowlist permits at most two internal spacing or punctuation
breaks in recognized discipline words. Clause punctuation is never joined.
Explicit aliases are `ASTROPOHYSICS -> ASTROPHYSICS` and
`BJOCHEMISTRY -> BIOCHEMISTRY`. Repairs do not extend searches into degrees,
professional sections, memberships, names or addresses. No unrestricted fuzzy
matching or whole-biography keyword search is used. Damaged phrases outside
the allowlist remain unresolved; this protects precision at a cost to recall.

## Evaluation protocol

Rules/tests and exposed-label regression were completed before freezing.
The old 500-entry audit is development/regression evidence, not a fresh holdout.
The new sample excludes the union of all original pilot, independent audit,
100-entry field sample and cross-edition sample keys: 1,164 retained 1986 keys.
There are 77,909 eligible keys out of 79,073 in v06. Draw:
`random.Random(20261024).sample(sorted(eligible_keys),100)`.

All 100 biographies were manually reviewed without either model's predictions,
rule matches or recovered fields displayed. Full text was consulted for ambiguous
headings. Labels are `0`, `1` or `unclear`, with literal evidence, reasoning and
confidence. They are frozen before prediction joins. These are Codex manual
judgments, not independent human expert labels. All engineering is included by
the broad policy; genetic engineering is explicitly marked as a medium-confidence
interdisciplinary boundary case. Speleology as Earth science is also a medium-
confidence judgment. No labels or classifier rules were changed after evaluation.

Evaluate original and revised code on the same sample. Precision, recall,
confusion matrix, accuracy and Wilson intervals exclude unclear reference labels.
Unresolved prediction rates include all 100 entries. Undefined metrics stay null.
Report every error, changed prediction and OCR-assisted match, including repairs
that do not change the binary prediction. Small-sample intervals do not capture
reference-label uncertainty. The full-population dummy is not replaced.

## Reproduction

From the repository root, using Python and the bundled Node/artifact runtime:

```powershell
python -X utf8 prep/amws/fields/rules_v2_audit.py freeze
python -X utf8 prep/amws/fields/rules_v2_audit.py prepare
python -X utf8 prep/amws/fields/rules_v2_audit.py review --start 1 --end 50
python -X utf8 prep/amws/fields/rules_v2_audit.py review --start 3 --end 3 --full
python -X utf8 prep/amws/fields/rules_v2_audit.py add --labels '<manual batch.json>'
python -X utf8 prep/amws/fields/rules_v2_audit.py freeze-manual
python -X utf8 prep/amws/fields/rules_v2_audit.py evaluate
python -X utf8 prep/amws/fields/rules_v2_audit.py report
node prep/amws/fields/export_rules_v2_audit.mjs
python -X utf8 prep/amws/fields/rules_v2_audit.py validate
```

Python commands accept `--data-root`; Python and Node accept
`TALENT_DETS_DATA_DIR`. The exporter accepts `--data-root` and
`AMWS_ARTIFACT_NODE_MODULES`. Frozen manual labels are reproduction inputs;
they are not regenerated from regex predictions. `case_notes.json` documents
post-comparison explanations without changing reference labels. Classifier
snapshots/hashes, sample keys, exclusion inventory, source checksums, labels,
comparisons, CSV, report and validation are retained in the separate output.

Validation reconstructs the seeded draw, verifies no prior overlap, rereads keyed
source data, recomputes both models, checks every literal evidence/repair span,
reconciles metrics and CSV cells, and verifies unchanged original releases.

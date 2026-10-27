# Independent physical-science accuracy audit

This audit samples 500 new edition-16 entries with seed `20261022`, excluding
the original 500 pilot keys. It evaluates the published Physical-or-mixed dummy
against prediction-blinded Codex manual judgments. It is not a human expert
gold standard. Classifier rules and prior outputs are preserved.

The runner accepts `--data-root` or `TALENT_DETS_DATA_DIR`. Run from the repository
root with Python. Output is under Dropbox
`output/amws/physical_science_accuracy_audit_20261020/`.

```powershell
python -X utf8 prep/amws/fields/accuracy_audit.py prepare
python -X utf8 prep/amws/fields/accuracy_audit.py review --start 1 --end 50
python -X utf8 prep/amws/fields/accuracy_audit.py review --start 28 --end 28 --full
```

Review uses only source biographies. It displays the heading portion (through
Educ, or the first 420 characters), with full biography navigation when needed.
It does not import a classifier or display predictions, recovered fields or rules.
Manual annotation batches contain `[sample_id, label, literal_evidence,
reasoning, optional_confidence]` rows, where label is `0`, `1` or `unclear`.
No keyword-generated reference labels are permitted.

```powershell
python -X utf8 prep/amws/fields/accuracy_audit.py add --labels '<manual batch.json>'
python -X utf8 prep/amws/fields/accuracy_audit.py freeze
python -X utf8 prep/amws/fields/accuracy_audit.py evaluate
```

The shipped `manual_labels.json` and `labels_frozen.json` are the frozen inputs
for reproduction. Do not relabel after seeing evaluation results. Running
`evaluate` reconstructs the comparison and metrics without fitting rules. Record
post-comparison explanations in `error_notes.json` for every incorrect or
unassessable entry, leaving manual labels intact. Then run:

```powershell
python -X utf8 prep/amws/fields/accuracy_audit.py report
node prep/amws/fields/export_accuracy_audit.mjs
python -X utf8 prep/amws/fields/accuracy_audit.py validate
```

Export requires the bundled artifact-tool Node runtime. Override its dependency
directory through `AMWS_ARTIFACT_NODE_MODULES` if necessary. CSV authoring preserves
all cells exactly. Labels and predictions are numeric 0/1, except the explicit
manual `unclear` marker. Source identifiers remain text. Evidence offsets use
zero-based Unicode code points, with exclusive end, in `raw_text_adjusted`.

Primary results are correct/incorrect/unassessable counts out of 500; accuracy
and its Wilson interval exclude unassessable cases. All-500 accuracy bounds treat
unknown reference labels as all wrong versus all right. Precision and recall
also exclude unassessable cases. The confusion matrix uses manual rows and
predicted columns. Mixed and classifier-unresolved subsets are reported separately.

Validation verifies the seeded draw, zero pilot overlap, all source and previous
pilot checksums, frozen labels, literal evidence, prediction joins, CSV cells,
reconciled counts and metric calculations. Any later classifier revision requires
a separate evaluation sample.

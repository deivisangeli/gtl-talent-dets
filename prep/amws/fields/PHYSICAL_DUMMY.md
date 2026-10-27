# Physical-science dummy for retained edition 16 entries

This separate release applies the unchanged `amws_ed16_science_v1_1_boundary_guard`
classifier to the 79,658 retained US-birth edition-16 entries in the October 19,
2026 `multi_v04` combined release. No source datasets or historical pilot outputs
are replaced.

`physical_science` is integer 1 if `physical_match` is true, including separately
identified Physical + Biological mixed fields. It is 0 otherwise, including
unresolved entries. **Zero means no detected Physical match, not a verified
non-Physical entry.** Engineering, mathematics and computing retain their broad
Physical classification. Existing compound-term precedence is unchanged.

The primary CSV has `source_key`, `entry_id`, `name_raw`, `physical_science`, in
the source release's edition-16 row order. Merge on `source_key`, not names or
row position. The audit includes recovered field, mixed/unresolved flags, original
three-way classifier result, rule IDs, evidence and all exact source references.
`mixed=1` always has `physical_science=1`; `unresolved=1` always has 0. The two
flags are distinct. Mixed remains marked for review in the unchanged classifier.
Audit boolean flags are encoded as 0/1. Evidence offsets are zero-based Unicode
code-point indices with exclusive end, into the recovered `field_raw` or the
release's `raw_text_adjusted`, as named in each evidence record.

The existing pilot's boundary-guard evaluation is post-hoc. A full-population
application and successful reproducibility checks do not create new accuracy
validation. Classifier and rules hashes must agree with the preserved pilot.

## Run

From the repository root, using Python and the bundled Node/artifact runtime:

```powershell
python prep/amws/fields/build_physical_science_dummy.py --data-root '<Dropbox project root>'
```

`TALENT_DETS_DATA_DIR` is also supported. `AMWS_NODE` and
`AMWS_ARTIFACT_NODE_MODULES` override the bundled Node executable and dependency
directory on another machine. The Python runner is standard-library-only.

Subcommands `prepare`, `export`, and `validate` support checkpointed execution.
The default `all` executes all three. A completed release is verified rather
than overwritten. A partial preparation is resumed only when its source/code
hashes match. Failures in source joins or conflicting source field values stop
publication rather than dropping rows.

Outputs go to `output/amws/physical_science_dummy_20261020/`:

- `amws_ed16_physical_science_dummy.csv`: keyed dummy, 79,658 rows.
- `amws_ed16_physical_science_audit.csv`: same rows and order with evidence.
- `summary.json` and `report.txt`: flag counts, unresolved coverage and definitions.
- `manifest.json` and `validation.json`: source/code/output checksums and checks.
- `code/`: exact runner, classifier, rule and exporter snapshot.
- `_staging/`: reproducible authoring records and intermediate checksums.

Validation independently reconstructs the input selection and joins, recomputes
all classification results with the archived code, checks every evidence span,
and reproduces all 500 prior pilot predictions. It validates strict 0/1 values,
key uniqueness, source order, counts and unchanged input/pilot checksums. The
two CSVs are authored in bounded artifact-tool workbook batches and checked
cell-for-cell against the prepared records.

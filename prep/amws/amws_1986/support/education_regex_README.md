# AMWS1986 education regex pilot

The parser extracts earned bachelor’s, master’s and PhD/DPhil awards from
`raw_text_adjusted`, preserving literal text and offsets. It contains no sample
identifiers or person-specific extraction overrides. Institutions are returned
as written; dates are award years, not complete calendar dates.

`education_regex.extract(text, birth_year=None)` returns a record status, review
flags, and a list of award dictionaries. Each award carries the family, literal
label, institution, literal/resolved year, evidence offsets, rule ID and flags.
Offsets use Python Unicode code points in the unchanged input text. Partial
awards remain in the output. `no_match` means no target match was produced, not
that the person lacked a qualification.

The pipeline uses `TALENT_DETS_DATA_DIR`, falling back to the current user's
`Globtalent Dropbox/gtl_talent_dets`, consistently with the earlier Python audit
helpers. All pilot data is under `output/amws/education_pilot/regex_pilot/`.
No cleaned-data or analysis-panel file is updated.

## Reproduce the saved pilot

Use Python3 from the repository root. The manual reference ledgers must exist
in the Dropbox output directory; they are an input, not regex-generated labels.

```powershell
python prep/amws/amws_1986/support/test_education_regex.py
python prep/amws/amws_1986/support/education_pilot.py init
python prep/amws/amws_1986/support/education_pilot_report.py
```

The existing report uses the frozen v08 predictions and the separately versioned
reference_v2. The holdout has already been evaluated. The runner rejects a
second holdout evaluation and checks the frozen parser/reference hashes.

For a genuinely new pilot, the stages are `init`, `reference`, `develop VERSION`,
`freeze VERSION`, then `holdout`. Development snapshots cannot be overwritten
with different parser code. Later reference versions preserve originals and
record explicit adjudications. Changes informed by the existing holdout require
a new independent evaluation sample; they cannot be presented as fresh holdout
results on the same200 records.

## Evaluation

The split shuffles sorted IDs with `random.Random(42)` and reserves the final200
for evaluation. All1000 biographies were independently reviewed for master’s
degrees, including negatives. Degree families were informed by the earlier
audit; the200 records were held out from subsequent regex tuning, rather than
being newly collected blind observations.

Award metrics use one-to-one Counter/multiset matches within each record.
Degree detection counts occurrences by family; institution/year/tuple metrics
add the corresponding exact field to that key. Aggregated metrics sum the
family-specific counts. Duplicate predictions therefore count as errors.
Institutions are compared after case, punctuation and whitespace normalization;
university aliases are not reconciled. Year tokens and resolved years are
evaluated separately. Explicit honorary awards are excluded from earned metrics.
Record-level detection and coverage count each sampled record once.

Confidence intervals are Wilson95% intervals. Award-level intervals do not
account for several awards within one biography; small professional-degree
groups have substantial uncertainty. Neither development nor held-out recall
should be confused with the percentage of biographies reporting the degree.

The frozen parser achieves high precision on complete tuples, with remaining
misses from OCR, institution boundaries, overlapping biographies and conservative
century resolution. It supports an edition-wide candidate extraction with a
review queue; its outputs are not independently verified education histories.

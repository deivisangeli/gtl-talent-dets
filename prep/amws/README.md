# AMWS Prep Pipeline

Prepare American Men and Women of Science data for county-year outcomes.

## Fifty new blind references per edition (2026-10-23)

The [four-edition blind review](fields/CROSS_EDITION_BLIND_AUDIT.md) draws 50 new
entries per edition, excluding prior reviews, with seed `20261026`. All 200
manual labels were frozen before predictions. Applying the unmodified 1986
parser exposes an earlier-edition layout failure (accuracy 32%/52%/12% for
1906/1938/1955; 88% for 1986).

A separately recorded **post-hoc source-field diagnostic**, using unchanged
vocabulary directly on original field columns, yields 48/50, 49/50 and 50/50
correct for 1906/1938/1955. The unchanged 1986 result is 44/50. The adjustment
followed the initial comparison and needs a fresh validation sample; these
earlier diagnostic figures are not independent held-out estimates. Full metrics,
intervals and every error are under
`output/amws/physical_science_cross_edition_blind_audit_50_20261023/`.
Sources, previous audits, frozen rules/labels and the population dummy are preserved.

## Fresh 500-entry blind test of frozen v2 (2026-10-23)

The [new 500-entry audit](fields/RULES_V2_500.md) keeps the revised rules frozen
and samples new retained 1986 entries, excluding all 1,283 known prior-reviewed
keys (77,790 eligible; seed `20261025`). Manual labels and evidence were frozen
before displaying predictions. V2 precision is **240/243 = 98.77%** (95% Wilson
interval **96.43–99.58%**), and recall **240/281 = 85.41%** (**80.80–89.06%**).
It classifies 455 correctly, 44 incorrectly, and one biography is unassessable.
Original guarded precision/recall on the same sample are 97.52%/83.99%.

The separate output is
`output/amws/physical_science_rules_v2_audit_500_20261023/`, including the audit
CSV, frozen sources/labels/rules, every error explanation, report and checksums.
Eight mixed predictions remain distinct from 64 unresolved cases. Resolution
coverage is 87.2%; unresolved zeros are not confirmed non-Physical entries.
These remain Codex manual reference labels without independent human validation.
Rules, earlier outputs and the population dummy are preserved. See
[the fields overview](fields/README.md) for the paired counts and limitations.

## Revised Physical vocabulary and OCR sample audit (2026-10-23)

The separate [Physical rules v2 workflow](fields/RULES_V2.md) expands missing
specialties and adds conservative, allowlisted OCR repairs while preserving
literal evidence and heading boundaries. The original classifier, data releases
and full-population dummy remain unchanged. The revision was frozen before a
100-entry random sample from 77,909 previously unreviewed retained 1986 entries
(v06; seed `20261024`). All reference labels were frozen before predictions.

On the same 99 assessable entries, original versus revised precision is
**97.92% versus 98.00%**, and recall **83.93% versus 87.50%**. V2 has 91 correct,
seven false negatives and one false positive; one additional record is
unassessable. V2's 95% Wilson intervals are 89.50–99.65% for precision and
76.37–93.81% for recall. These are prediction-blinded Codex manual judgments,
not independent human validation; the sample is too small to establish a
reliable population improvement. Outputs and every error are under
`output/amws/physical_science_rules_v2_audit_20261023/`. See
[the fields overview](fields/README.md) for the paired results and limitations.

## Cross-edition field sample (2026-10-23)

The [500-entry field sample](fields/CROSS_EDITION_SAMPLE.md) draws 125 entries
from each of 1906, 1938, 1955 and 1986 in the latest v06 release (134,311 retained
entries), with seed `20261023`. Original source fields are available for all
3,555 retained 1906 entries, 18,827/18,963 retained 1938 entries and all 32,720
retained 1955 entries. They join by edition and original `lineid`; 1986 reuses
the existing field audit and dummy.

All 375 sampled earlier-edition fields were usable. The sample has specific
fields identified for **499/500 entries** after recovering six 1986 fields from
biographies; one truncated biography remains unresolved. Earlier Physical flags
are assigned manually. The unchanged 1986 flags yield **325 Physical entries**
overall (73/59/125/68 by edition), including 12 mixed cases. Separate manual
review columns identify seven additional 1986 Physical cases without replacing
existing values. Source text, evidence, uncertainty and population weights are
in the audit under `output/amws/field_classification_sample_20261023/`.

This is a sample classification exercise, not a full-population earlier-edition
classifier or a new automated accuracy estimate. The report distinguishes raw
sample counts from population-weighted shares. Existing inputs and releases
are preserved; see [the fields overview](fields/README.md) for results.

## Edition 16 Physical-science dummy and accuracy audit (2026-10-20)

The [fields pipeline](fields/README.md) publishes a separate Physical-science dummy
for all 79,658 retained edition-16 entries in the October 19 `multi_v04` release.
There are **39,461 ones**, including 1,685 mixed Physical + Biological entries,
and **40,197 zeros**, including 10,028 unresolved entries flagged separately.
Zero means no detected Physical match, not a verified negative. Merge the keyed
CSV on `source_key`. Outputs are under
`output/amws/physical_science_dummy_20261020/`; see
[the dummy workflow](fields/PHYSICAL_DUMMY.md).

The [fresh 500-entry audit](fields/ACCURACY_AUDIT.md) excludes the original pilot
sample and compares frozen predictions with prediction-blinded Codex manual
labels: **461 correct, 37 incorrect and two unassessable**. Accuracy among the
498 assessable entries is **92.57%** (95% Wilson interval 89.93–94.56%). Physical
precision is 99.60% and recall 87.46%. These are manual Codex judgments, not an
independent human expert benchmark. Results and every error explanation are under
`output/amws/physical_science_accuracy_audit_20261020/`.

The intended broad Physical definition includes engineering, mathematics,
statistics and computing, with Physical + Biological mixtures receiving 1.
The unchanged classifier retains known vocabulary and compound-term limitations;
the audit documents 36 missed Physical entries and one false positive. This
work preserves the source releases, prior pilot outputs and existing outcomes.

## Education enrichment and institution geolocation

The frozen four-edition education parser and event-study-source enrichment live
in [`education/`](education/README.md). The original five additive person-file copies in
`output/amws/education_enriched/` preserve the original rows, columns, ordering,
geographic candidates and inclusion decisions. Their earned BA/BS, MS, MA and
PhD institution/year columns are aligned JSON arrays. Reported dates are award
years; missing or century-ambiguous values remain null. Professional degrees,
other degree families and honorary awards remain in the long education ledger.

```text
python prep/amws/education/enrich_event_study_sources.py --data-root <Dropbox-root> --rscript <Rscript-path>
python -m unittest discover -s prep/amws/education -p "test_*.py"
```

### Current education and OpenAlex ID export (2026-10-12)

The completed identity export is
`output/amws/amws_education_openalex_20261012/`. Its main file,
`amws_all_editions_education_openalex.csv`, contains 156,139 distinct
edition-specific entries, including the updated 99,308-entry 1986 edition.
Existing OA assignments cover 132,555 entries and 273,958 earned-degree awards,
using 1,627 distinct IDs. Each award has numbered institution-name/ID columns;
all 67 awards with two assigned institutions retain both IDs. Conditional and
provisional assignment labels are preserved; coverage is not verified identity
accuracy.

The release also contains five source-specific copies, the earned-award table,
all 156,607 original rows in provenance, a data dictionary, coverage and validation
reports, and checksums. The combined file selects one row per edition-specific
entry without linking people across editions. Working SQLite files remain local.
See [the award-level export workflow](education/README.md#award-level-amws-person-export)
for commands, column definitions, duplicate precedence and validation. This
identity export does not change historical training locations or county outcomes.

### University reference coordinate attachment (2026-10-14)

`output/amws/amws_education_openalex_coordinates_20261014/` extends the ID
export with local OpenAlex reference latitude, longitude, city and country for
each numbered institution. All 1,627 assigned institutions have coordinates,
covering 132,555 entries and 273,958 awards, including all 67 two-ID awards.
The combined file and five source-specific files retain their previous filenames
inside the new release. Existing columns, birthplace coordinates, identity
decisions, row order and duplicate provenance are preserved.

The release includes an institution-coordinate lookup with snapshot provenance
and dates, aligned award-level coordinate arrays, documentation and validation.
Locations may be city coordinates; historical teaching-campus accuracy remains
unverified. See [coordinate attachment](education/README.md#university-reference-coordinate-attachment)
for the runner, field names and checks. Existing datasets, geolocation mappings
and county outcomes remain unchanged.

### College-allocation training-location outcomes (2026-10-14)

The separate [neighbouring-county extension](../../analysis/land_grants/training_50km/CATCHMENTS.md)
pools birth counties around both allocated and runner-up sites. Its geography
contains 622 counties before historical denominator screening. It preserves the
training classifications and frozen event/site observations, adds supported
neighbouring counties to each site's counts and birth denominator, and publishes
under `amws_training_50km_catchments_20261014`. It does not change this AMWS input
or the county-only release.

After timing-specific historical-denominator screening, the catchment analyses
include 202 counties and 6,781 edition-specific entries under standard timing,
and 234 counties and 8,103 entries under alternative timing. These are increases
of 2,858 and 3,579 entries relative to their corresponding frozen county-only
samples; they are not counts of newly extracted biographies. All eight models
and the independent reconstruction checks passed.

A separate sensitivity, `amws_training_50km_catchments_no_reuse_20261014`, removes
the seven cross-event shared counties from every catchment, including original
home memberships. It retains all frozen events, site units and observations and
uses 195/227 counties with 6,636/7,958 edition-specific entries. No entry is reused
across experiments in either estimation sample. The AMWS source, identity
assignments and training classifications are unchanged; only catchment outcome
aggregation and birth denominators are rebuilt. See the catchment guide for
commands, comparisons, validation and the warnings that remain.

The additional [calendar-balanced version](../../analysis/land_grants/training_50km/CALENDAR.md)
uses complete 1830–1950 calendar-decade labels and variable relative-time support.
It excludes eleven shared counties and the unsupported East Carolina experiment,
retaining 14 events, 43 sites and 154 member counties under both timings. Its
7,206 standard-timing and 7,198 alternative-timing entries differ because the
alternative 1960 bin is excluded. The release
`amws_training_50km_calendar_no_reuse_20261014` preserves this AMWS input, all
training classifications, previous releases and the Drive companion. All eight
models and independent checks passed; small-cohort and singular pre-test
covariance warnings are retained.

The canonical education input also feeds a separate downstream land-grants
pipeline. It classifies eligible scientists born in an Andrews event county as
ever trained within 50 km of that county's centroid, never trained within 50 km
with complete institution matching, or unknown training. BA/BS, MA, MS and PhD
are included. A known nearby assignment takes precedence over incomplete other
awards; undated unconditional assignments remain usable. No identity or location
assignments are changed.

Run `Rscript --vanilla prep/land_grants/02_build_amws_training_50km.R`, followed
by `Rscript --vanilla analysis/land_grants/training_50km/run_event_studies.R`,
from the repository root with `TALENT_DETS_DATA_DIR` set. Outputs are separate
under `output/land_grants/amws_training_50km_20261014/` and
`results/land_grants/event_studies/amws_training_50km_20261014/`.

The outcomes preserve the earlier-edition `kept` and 1986 contamination rules,
birth-year restrictions, and existing cross-edition counting convention. They
retain the canonical geography for `1955:46642`, documenting the resulting
one-entry difference from the legacy outcome. OpenAlex reference locations do
not establish historical campuses; incomplete matching can move records into
unknown training. The AMWS input, prior releases, and event definitions remain
unchanged. See [the preparation guide](../land_grants/README.md#amws-training-within-50-km-of-birth-county-2026-10-14)
and [the event-study guide](../../analysis/land_grants/training_50km/README.md).

The latest education extraction is `multi_v04`, published as
`output/amws/amws_education_postsecondary_v04_20261019/`. That is a new release;
the canonical `Data/processed/amws/amws_open_alex.csv` is unchanged. See
[education/POSTSECONDARY.md](education/POSTSECONDARY.md).

### Canonical OpenAlex analysis input

Use `Data/processed/amws/amws_open_alex.csv` as the combined person-level
education/OpenAlex input. It contains 136,489 edition-specific entries and 444
columns after restricting the 1986 component to `is_us_birth = TRUE` (case and
surrounding whitespace ignored). All 56,831 earlier-edition entries remain;
79,658 of the 99,308 1986 entries remain, excluding 19,650 false flags. False,
missing or unrecognized 1986 flags are excluded and reported separately. The
edition is read from `source_key`; earlier blank US-birth flags are retained.
This applies the existing birthplace classification, not a citizenship test.

The full 156,139-entry coordinate release remains intact. The canonical CSV is
now a filtered derivative, not a byte-identical copy of that full release.
`amws_open_alex_manifest.json` beside the CSV records source and destination
checksums, publication date, filter counts, retained-row validation, OA coverage
and coordinate qualification. Rebuild with
`python -B prep/amws/education/publish_amws_open_alex.py --data-root <Dropbox-root>`.

Source the repository's `paths.R` and read `AMWS_OPEN_ALEX`; its path is resolved
through `TALENT_DETS_DATA_DIR`. Existing provisional/conditional IDs, counting
conventions and reference coordinates are preserved. Historical-campus accuracy
remains unverified. Event-study integration and estimation are subsequent steps.

### Earlier institution geolocation release

Institution matching uses **GTAllocation's OpenAlex institution snapshots** as
the authoritative source of IDs and coordinates. GTAllocation's separate
university crosswalks supply candidates only; their aliases do not automatically
authorize matches. The new `output/amws/education_geolocated/` directory contains
the reference, institution inventory, candidates, manual decisions, versioned
rules, independent audit, unresolved inventory and award-location ledger. The
five `person_files/*_education_geolocated.csv` copies append
`{ba_bs,ms,ma,phd}_institution_geocodes`, with one object per existing award
position and zero, one or multiple reference locations inside each object.

See the education README for commands and the manual-review sequence.
The 99% independent-audit precision gate controls publication of automatic
results: if it fails, unaudited automatic matches remain candidates requiring
review, with empty accepted locations. Explicit audited decisions are retained.
Coordinates represent the current supplied reference location; historical
teaching campuses are not reconstructed. Ambiguous campuses, conflicting
historical names, absent reference institutions and invalid extractions remain
flagged. The geolocation report distinguishes descriptive coverage from audited
accuracy, and does not validate all original regex award associations.

Existing education/year lists, original AMWS sources and estimation panels are
unchanged. Analysis scripts have not been switched to the new copies.

The completed geolocation audit confirmed 476/500 automatic name mappings
(95.2% name precision; 98.57% award-weighted precision), below the 99% gate.
Publication therefore retains reviewed decisions and holds 1,355 unreviewed
name mappings as candidates. The released copies locate 153,386/286,848
institution-bearing awards (53.5%), connecting 1,470 literal strings to 709
snapshot institutions. See `output/amws/education_geolocated/findings.md` for
edition/degree coverage, accuracy intervals and remaining failures.

Run scripts from the repository root or from `prep/`; the scripts resolve the repository root from `--file`/`GTL_REPO` and load `paths.R`.

## Main Order

1. `cleaning_amws_1906.R`, `cleaning_amws_1938.R`, `cleaning_amws_1955.R`
2. `geocode_amws_edition.R` for 1906/1938 and `geocode_amws_1955_us.R` for 1955
3. Manual/audit helpers as needed: `prep_amws_edition_batches.R`, `aggregate_amws_edition_fix.R`, `prep_manual_fix_batches.R`, `aggregate_manual_fix.R`, `prep_audit_batches.R`, `aggregate_audit.R`
4. `dedup_amws_editions.R`
5. `build_amws_county_year.R` and `build_amws_combined_county_year.R`

## AMWS 1986 / Edition 16

Active cleaning scripts for the 1986 edition live in `amws_1986/`.
The old filenames in `prep/amws/` are compatibility wrappers that call the
new locations.

Main 1986 order:

1. `amws_1986/01_run_regex_all_docx.R`
2. `amws_1986/02_build_birth_year_city.R`
3. `amws_1986/03_expand_multi_entry_rows.R`
4. `amws_1986/04_apply_expanded_all_corrections.R`
5. `amws_1986/05_apply_location_format_manual_corrections.R`
6. `amws_1986/06_apply_birth_year_manual_corrections.R`
7. `amws_1986/07_geocode_us_birthplaces.R`
8. `amws_1986/08_build_county_year_panel.R`
9. `amws_1986/09_plot_births_by_year.R`

All-country post-geocode correction:

- `amws_1986/10_apply_piloted_regex_corrections_all_countries.R` applies the
  piloted regex corrections to
  `AMWS_OUTPUT/regex_all_docs/amws_ed16_all_countries_geocoded_us_only.csv`,
  preserving non-US rows and refreshing US geocodes only where corrected
  location keys changed. It also applies two explicit manual exclusions for
  implausible/corrupted birth-year parses and writes the final AMWS 1986
  distribution copies:
  - `Data/intermediary/amws/amws_ed86_full.csv` and `.xlsx`: full 41-column
    output, including parsing/geocoding diagnostics.
  - `Data/processed/amws/amws_ed86.csv` and `.xlsx`: 21-column processed
    version for analysis and sharing.
  - `Data/processed/amws/amws_ed86_filtered.csv` and `.xlsx`: processed
    version restricted to rows with nonempty `birth_city` and `birth_year`.

Auxiliary manual-review, audit, and diagnosis scripts are under
`amws_1986/support/`. Pilot and superseded scripts are under
`amws_1986/archive/`.

### Reviewed OCR supplement

The 14 edition-16 PDFs without Word transcriptions have a separate reviewed OCR
delivery at `output/amws/amws_ed16_paragraph_reviewed_data/` in the Dropbox
project. It contains 1,792 corrected page JSON files, 14 combined TXT and 14
combined Markdown documents, plus repair decisions, audits, review queues and
provenance. Each document export retains one-based PDF page markers.

Read `corrected_prediction.parsing_res_list` for the final paragraph structure;
`original_record` retains the original OCR. The append review identified the two
formerly unresolved blocks (`16_E_700_800.pdf` page 28 and `16_E_800_900.pdf`
page 94) as margin fragments and excludes them with scan evidence. Source OCR
remains immutable; corrections are explicit sidecar decisions.
The conservative 2026-10-11 append adds 19,731 records to the 1986 person dataset
(99,308 final rows) and creates the separate four-edition person stack (152,354
rows). There are 3,606 unresolved candidates still awaiting individual scan
review and excluded from final counts. Outcome panels have not been refreshed.

The append pipeline is `amws_1986/ocr_supplement.py`,
`amws_1986/11_parse_ocr_supplement.R`, and
`amws_1986/12_publish_ocr_supplement.py`. See
[`amws_1986/OCR_SUPPLEMENT.md`](amws_1986/OCR_SUPPLEMENT.md) for staging,
source-keyed scan decisions, audit requirements, stable final IDs, and publication.

The delivery README and manifest describe the selected files, while its report
distinguishes verified local copies from confirmed cloud synchronization.
Scan images, viewers and resume checkpoints remain in the complete local
corpus. See [PaddleOCR workflow and delivery](amws_1986/PADDLEOCR.md) for details.

## Inputs

- `AMWS_INPUT`: raw AMWS editions in Dropbox input.
- `DATA_INPUT`: geocoding references such as Census Gazetteer and GeoNames.

## Outputs

- `AMWS_OUTPUT`: cleaned, geocoded, audited, and deduplicated AMWS files.
- `DATA_OUTPUT/us_panel_county_amws_combined_year.csv`: county-year AMWS panel merged onto the yearly Wikipedia county panel.
- 1986 outputs keep the existing Dropbox paths, including
  `AMWS_OUTPUT/regex_all_docs/`, `AMWS_OUTPUT/amws_1986_county_year.csv`,
  and `DATA_OUTPUT/us_panel_county_amws_1986_year.csv`.

## Gender inference and annual counts

After the combined annual panel and the reviewed 100-record gender pilot exist,
run from the repository root:

```r
Sys.setenv(TALENT_DETS_DATA_DIR = "C:/Users/<username>/Globtalent Dropbox/gtl_talent_dets")
source("prep/amws/build_amws_gender_inference.R")
```

Alternatively, set `TALENT_DETS_DATA_DIR` in the shell and run
`Rscript prep/amws/build_amws_gender_inference.R`. The script requires installed
`data.table`, `ggplot2`, `gender`, and `genderdata` packages; the name-reference
lookups use local data and make no API requests.

This classifies the 112,291 US-born records counted by the current combined
1800–1960 panel, preserving its inclusion and deduplication rules. First names
are extracted conservatively, with the pilot's reviewed names retained. Names
are queried in birth-decade batches using IPUMS before 1930 and SSA from 1930.
A male/female label requires a reference proportion of at least 95%. Ambiguous,
unmatched, and unreadable records remain unresolved. Raw predictions are kept;
the documented correction for Comfort Avery Adams changes his final label to
male. Reference proportions and classification coverage are not validated
precision estimates.

New outputs are saved separately under `AMWS_OUTPUT/gender_inference/`:

- `amws_gender_records.csv`: record identifiers, names, extraction status,
  reference proportions, raw and final classifications, and correction evidence.
- `amws_gender_name_reference.csv`: name/window lookup results, including unmatched names.
- `amws_gender_overall_counts.csv`, `amws_gender_birth_year_counts.csv`, and
  `amws_gender_edition_coverage.csv`: counts and classification coverage. The
  annual table contains all 161 birth years, including zero-count years.
- `figures/`: overall and annual count charts in PNG and PDF formats. Annual
  men/women panels have separate, labeled count scales.
- `amws_gender_source_checksums.csv` and `amws_gender_run_metadata.csv`: source
  preservation checks and reproducibility details.

Counts describe records under the existing panel convention, not verified unique
people: 1986 has not been deduplicated against earlier editions. Recent birth
cohorts have incomplete career coverage in the directories. The script verifies
all county-year component counts, category and annual totals, pilot predictions,
and unchanged input/pilot checksums. It does not modify existing AMWS data or panels.

### Male and female flags at additional thresholds

After the full gender builder, run from the repository root with
`TALENT_DETS_DATA_DIR` set as above:

```r
source("prep/amws/add_amws_gender_threshold_flags.R")
```

The shell equivalent is `Rscript prep/amws/add_amws_gender_threshold_flags.R`.
This script needs only `data.table`. It reads the saved predictions and appends
eight integer columns to `AMWS_OUTPUT/gender_inference/amws_gender_records.csv`:
`male_95`, `male_90`, `male_85`, `male_80`, `female_95`, `female_90`, `female_85`,
and `female_80`. Each is 1 when its sex-specific reference proportion is at least
its cutoff, 0 when a usable prediction exists but falls below the cutoff, and
missing for unreadable or unmatched names. Zero does not imply the opposite sex.
Both flags can be 0 for an ambiguous prediction, and neither can be 1 at the
same cutoff. The documented Comfort Avery Adams correction sets all male flags
to 1 and all female flags to 0, preserving his original proportions and evidence.

The original columns, names, GEOID strings, reference proportions, record order,
and final 95% classification remain unchanged. The existing `threshold` column
still describes that primary classification. No name extraction or inference is
rerun. A staged CSV is read back and validated before replacement; an original
backup is retained until installation checks pass. Reruns regenerate all eight
flags, verify any existing male flags are unchanged, and append only missing
columns. The script must be rerun after rebuilding the original gender CSV.

Additional outputs in the same gender-inference folder are:

- `amws_gender_threshold_coverage.csv`: overall counts at each threshold.
- `amws_gender_threshold_edition_coverage.csv`: the corresponding edition counts.
- `amws_gender_threshold_flags_checksums.csv`: protected-file MD5 checks and the
  authorized record-CSV augmentation.
- `amws_gender_threshold_flags_validation.csv`: execution time, validation
  results, flag meanings, and denominator definitions.

Both coverage tables include male, female, ambiguous, unmatched, unreadable,
total, classified and unresolved counts, flag zeros and missing flags,
`male_flag_share` and `female_flag_share` (sex-specific flags divided by all
included records), and `coverage` (male plus female classifications divided by
all included records). The female columns `female_flag_zero`,
`female_flag_missing`, `female_flag_column`, and `female_flag_share` extend the
existing report columns without changing their values or definitions. Proportions
are stored as fractions. Female flags/counts exclude the documented male correction.
At 0.95, 0.90, 0.85 and 0.80, female counts are 5,143, 5,314, 5,420 and 5,480.
Every male and female flag has 10,533 missing values. The 95% flags reproduce the
existing final classifications; both sexes' flags are nested across cutoffs.
Adding female flags leaves the previously reported overall coverage unchanged.
The mandatory-schooling monthly and quarterly outcome builders consume
`male_80` and `female_80` directly, preserving these saved inference files and
the primary 95% classifications. They append male, female and unclassified
counts/rates using existing total-birth and population denominators. Matching
uses edition plus original source-row position, with source-checksum and
identifier/geography/year checks. The period panels also retain 1,694 records
outside this annual-panel-based inference sample as unclassified with the
distinct reason `no_saved_inference`; no additional name inference is performed.
See [the period-panel dataset guide](../mandatory_schooling/monthly_amws_panel.md#gender-specific-outcomes-at-the-08-threshold)
and run `Rscript prep/mandatory_schooling/test_amws_panel_gender.R --integration`
after rebuilding all four panels to validate their gender splits and preservation
of existing values using the full rebuild test. To validate already-built period panels through
DuckDB in R, use
`Rscript prep/mandatory_schooling/validate_amws_panel_gender_duckdb.R`.
Lower cutoffs increase
classification coverage; verified precision remains unmeasured. Existing
datasets, panels, pilot files, figures, and primary coverage tables are preserved.

## College-allocation county-target training outcomes (2026-10-14)

The `amws_county_training_location_20261014` analysis classifies all 130,730
eligible US-born AMWS entries by training within 50/100 km of each original
target county's centroid, regardless of birth county. The three categories use
that county's estimated birth denominator. No pooled catchments or birthplace
restrictions are applied. Source data, institution assignments and reference
coordinates remain unchanged. See the
[county-level outcome guide](../../analysis/land_grants/training_50km/COUNTY_TRAINING_LOCATION.md)
for definitions, counting, samples, 32 models and the corrected companion.

## Historical OCR comparison (2026-10-19)

The companion field extraction audit uses **18 fresh pages and every biography
starting on them (304 biographies)**, rather than character preservation as its
primary outcome. It applies the same frozen automated segmentation and field
parsers to complete Word and raw Paddle documents, then compares the predictions
with scan-derived names, full birthplaces, four-digit years and education
institution sets. References are agent-transcribed, without independent human
verification. See the [field audit protocol](amws_1986/PADDLEOCR.md#automated-field-extraction-audit-2026-10-19).
The audit is separate from production data and from the earlier comparison;
its archive destination is `output/amws/field_extraction_audit_20261019/`.

A separate nine-document edition 16 comparison reprocesses 1,360 PDF pages with
the adopted raw PaddleOCR procedure and compares them to the matching historical
Word transcripts. The seed-20261019 draw preserves the original six and adds
three from the remaining 31 pairs. Its scan-first audit accounts for 90 sampled
biographies on 18 pages; unreadable scan references remain explicitly unscored.
The workflow preserves production datasets and source files. See the
[PaddleOCR comparison procedure](amws_1986/PADDLEOCR.md#historical-word-versus-raw-paddleocr-comparison-2026-10-19)
for selection, source hashes, worker/resume commands, audit definitions and
verified delivery requirements. The delivery folder is
`output/amws/ocr_comparison_20261019/`; completion is established by its
`delivery_report.json`, not by the presence of this documentation.

### Manual Word/raw-Paddle entry audit

The `manual_entry_audit_20261020` delivery compares the entire printed text of 339
biographies on 20 fresh pages (seed `20261023`, no overlap with the prior 36 audit
pages). It reuses the nine PDF/Word pairs and completed raw OCR; no field extractor,
OCR rerun or paragraph repair is involved. References and judgments are agent
reviewed, without independent human verification.

The result is mixed: Paddle preserves more names and complete education institution
sets; Word preserves more full birthplaces and printed birth-year digits. Overall
judgments include paragraph structure and retain 117 uncertain decisions. See
`amws_1986/PADDLEOCR.md` for counts and interpretation. The archive includes a
portable scan/source/decision viewer, reference and review ledgers, per-page and
per-PDF metrics, source hashes and rechecks. It is copied to Dropbox
`output/amws/manual_entry_audit_20261020/`; previous deliveries and production data
are preserved. Word evidence includes generated OOXML numbering labels that the
previous XML snapshot omitted, documented separately from OCR/paragraph quality.

### Word/raw/repaired-Paddle full-document comparison

`paragraph_repaired_comparison_20261021` reprocesses all nine original PDF/Word
pairs (1,360 pages) using the established saved-OCR paragraph recovery and
scan-reviewed exception workflow. All 1,317 collapsed reference blocks have
recorded assignments; recognized wording and source detections are preserved.
This does not certify every biography boundary or repair character errors.

The frozen extractor returns 23,787 Word records with generated list labels,
21,908 raw Paddle records (identical to the historical predictions), and 23,077
repaired Paddle records. Complete-document records include possible fragments
and stubs. Accuracy is evaluated on the existing 304 adjudicated scan-reference
biographies, not on all records. Repair raises detection from 92.4% to 97.7% and
complete institution sets from 67.4% to 71.7%, but complete four-field records
rise only from 54.2% to 55.2%, compared with Word at 60.8%. Recognition, layout and
parser failures remain. The small paired audit is agent-reviewed without
independent human verification.

The separate archive, three-arm scan viewer, full prediction/linkage ledgers,
decisions and validation are delivered under
`output/amws/paragraph_repaired_comparison_20261021/`. Source documents, previous
archives and production datasets are preserved. See `amws_1986/PADDLEOCR.md` for
the workflow and reproducibility commands.

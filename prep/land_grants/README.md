# Land Grants Prep Pipeline

This pipeline builds the Andrews college site-selection county-pairs workbook used by the land-grants analyses and by the AMWS county-pairs robustness scripts.

## Input

- Dropbox raw file: `raw/andrews_2023_appendix_table_a1.xlsx`
- Source sheet: `table_a1`
- External reference: Census county gazetteer `2025_Gaz_counties_national.txt`

The script downloads the Census gazetteer to `raw/` if it is missing.

## Output

- Dropbox raw file: `raw/andrews_2023_county_pairs_long.xlsx`

The output keeps the historical `county_pairs` filename because existing analysis scripts consume it directly.

## Run

From the repo root:

```r
Rscript prep/land_grants/01_build_andrews_county_pairs_long.R
```

The output workbook contains:

- `county_pairs_long`: one selected county and one runner-up county per row.
- `source`: provenance metadata, matching rule, and row counts.

Runner-up counties are matched only within the row's state. Unmatched rows are kept with missing coordinates and `runner_up_match_status == "unmatched_not_in_state"`.

## Longitudinal faculty panel

The `faculty/` subdirectory expands the first-search opening/transition faculty
rosters into an annual instructional-faculty panel for the 57 selected Andrews
colleges through academic year 1950-51. It initializes 19 fixed research
batches of exactly three institutions, preserves the 338 legacy faculty rows,
validates independently staged research, and builds person-year and
college-year outputs only from batches accepted by the primary reviewer.

Run from the repository root with `TALENT_DETS_DATA_DIR` set:

```r
Rscript prep/land_grants/faculty/01_initialize_faculty_panel.R
Rscript prep/land_grants/faculty/03_prepare_legacy_seed.R
Rscript prep/land_grants/faculty/validate_faculty_scaffold.R
Rscript prep/land_grants/faculty/02_validate_and_merge_faculty_batches.R
Rscript prep/land_grants/faculty/04_build_faculty_person_year.R
```

See `faculty/research_protocol.md` for source priority, inclusion rules,
coverage statuses, and staged-agent handoff requirements.

## AMWS training within 50 km of birth county (2026-10-14)

For the expanded all-earned-postsecondary scope, run
`10_build_amws_birth_county_training_postsecondary.R`; see
[postsecondary extraction and outcome refresh](../amws/education/POSTSECONDARY.md).
The separate release is `amws_birth_county_training_postsecondary_20261019`,
with all six outcomes, 28 models and old-versus-expanded comparisons.

The latest refresh uses the `multi_v04` extractor. Run
`11_build_amws_birth_county_training_postsecondary_v04.R` to build
`amws_birth_county_training_postsecondary_v04_20261019`. It applies the same
rules to the v04 education release. An ever-local entry may be lost only
through an award the release lists as removed or changed, and none was. 50 km
unknown falls from 3,827 to 3,553. See the "multi_v04 release and
college-allocation refresh" section of
[POSTSECONDARY.md](../amws/education/POSTSECONDARY.md).

The newest refresh reads education release v05, which is v04 plus the top-500
manual institution crosswalk and the medical-campus overrides. Run
`12_build_amws_birth_county_training_postsecondary_v05.R` to build
`amws_birth_county_training_postsecondary_v05_20261020`. v05 only adds matches,
so the script requires that no entry leaves ever or never relative to v04, and
that every class change comes from an entry with a manual award.

| Class (12,360 entries) | Same county: v04 → v05 | 50 km: v04 → v05 |
|---|---:|---:|
| Ever | 2,661 → 3,270 | 3,571 → 4,289 |
| Never | 5,936 → 7,976 | 5,236 → 7,014 |
| Unknown | 3,763 → 1,114 | 3,553 → 1,057 |

The 2026-10-19 birth-origin extension adds exact-county training alongside the
50 km definition, producing six rates with the same county-born numerator pool
and own-county birth denominators. Run
`09_build_amws_birth_county_training.R`; see the
[birth-county training guide](../../analysis/land_grants/training_50km/BIRTH_COUNTY_TRAINING.md)
for commands, columns, all 28 models and independent validation. Its separate
release is `amws_birth_county_training_20261019`.

Run from the repository root with `TALENT_DETS_DATA_DIR` pointing to Dropbox:

```powershell
Rscript --vanilla prep/land_grants/test_amws_training_50km.R
Rscript --vanilla prep/land_grants/02_build_amws_training_50km.R
Rscript --vanilla analysis/land_grants/training_50km/run_event_studies.R
Rscript --vanilla analysis/land_grants/training_50km/validate_release.R
```

The builder reads the canonical US-born `Data/processed/amws/amws_open_alex.csv`
and its checksum manifest. It keeps the existing birth-year, county, earlier
edition `kept`, and 1986 see-previous exclusion rules. It does not link people
across editions: the current cross-edition counting convention remains in force.
The preparation pool is the frozen 57 Andrews events and 168 physical counties,
with birth years 1830–1959. This pool is larger than the final estimation samples.

BA/BS, MA, MS and PhD awards determine three exhaustive categories. **Ever
nearby** means any assigned institution lies at most 50,000 metres from the birth
county's centroid, even if other awards are unmatched. **Never nearby** requires
at least one reported award, every award fully matched without conflicts, and
every assigned location farther than 50,000 metres. **Unknown training** covers
the remainder, including incomplete matching and absent education. Missing years
do not invalidate unconditional assignments; existing satisfied conditional IDs
are carried through unchanged.

Centroids are geometric centroids of the cached 2020 county polygons in
EPSG:5070, transformed to longitude/latitude. Distances use `sf` with S2 enabled.
Institution coordinates are the attached OpenAlex reference locations; historical
teaching campuses remain unverified. Existing provisional identity assignments
remain provisional.

Prepared files are published separately at
`output/land_grants/amws_training_50km_20261014/`: entry classifications and
exclusions, award evidence, assigned-institution distances, county centroids,
county/institution distance lookup, annual counts, both decade panels, coverage
audits, source/code checksums and validation. Each rate is 1,000 times the category
count divided by the unchanged estimated county births. Zero counts remain zero;
unavailable or nonpositive denominators produce missing rates. Every pre-existing
panel column is preserved, including the legacy outcomes. Alternative timing
moves birth years ending in 7–9 into the next decade.

The 12,360 eligible records comprise 3,413 ever nearby, 5,275 never nearby and
3,672 unknown. Their total is one below the legacy count: `1955:46642` is assigned
to canonical county `42015` (outside this event pool), whereas its legacy source
variant used `17113`. Both timing panels differ only at `17113`, decade 1930.
No birthplace correction is made. All 136,489 source entries receive an audit
row, including those excluded from these outcomes.

See [the analysis documentation](../../analysis/land_grants/training_50km/README.md)
for the frozen manuscript specification, results, warnings and output definitions.

### Neighbouring birth-county catchments

`03_build_amws_training_catchments.R` expands both selected and runner-up sites
to counties with centroids within 50 km, retains original counties, and assigns
overlaps to the nearest site within each experiment. Added counties without
usable denominators throughout a site's frozen estimation rows are excluded
for all periods. Outcomes and births are summed into one fixed catchment per
original site; nearby training remains relative to birth-county centroids.

Run this builder, then
`analysis/land_grants/training_50km/run_catchment_event_studies.R` and
`analysis/land_grants/training_50km/validate_catchment_release.R`, with
`TALENT_DETS_DATA_DIR` set. The 622-county geographic pool includes 454 additions;
analysis membership is timing-specific after denominator screening. Separate
preparation/results releases use `amws_training_50km_catchments_20261014`.
See [the catchment guide](../../analysis/land_grants/training_50km/CATCHMENTS.md)
for allocation coordinates, denominator conventions, overlap rules and limitations.

The validated catchment runs retain 202 counties for standard timing and 234 for
alternative timing, adding respectively 150 and 172 counties to their original
analysis samples. They capture 6,781 and 8,103 distinct edition-specific AMWS
entries in the frozen observations, including 2,858 and 3,579 newly captured
entries. All eight models completed; site-unit and observation counts remain
52/650 and 62/834. The independent validator passes all release, assignment,
denominator, count, baseline and sample reconstruction checks.

For the sensitivity excluding all seven shared counties, run
`04_build_amws_training_no_reuse.R`, then
`analysis/land_grants/training_50km/run_no_reuse_event_studies.R` and
`analysis/land_grants/training_50km/validate_no_reuse_release.R`. It removes those
counties from every catchment, including two original runner-up home counties,
without reassignment. All site units and frozen observations survive. The new
release `amws_training_50km_catchments_no_reuse_20261014` contains 195/227 analysis
counties and 6,636/7,958 unique edition-specific entries, with no cross-event
county or entry reuse in either sample. All eight models and independent checks
pass; cohort-size and alternative nearby-model pre-test warnings remain.

For the report companion's calendar-only balance specification, run
`05_build_amws_training_calendar.R`, then
`analysis/land_grants/training_50km/run_calendar_event_studies.R` and
`analysis/land_grants/training_50km/validate_calendar_release.R`. It reproduces
the companion's initial 69 anchors / 15 events / 46 sites / 598 rows, freezes the
previous seven and four new North Carolina county exclusions, and drops the
empty East Carolina event. Both timings then use 14 events, 43 sites, 559 rows
and 154 fixed member counties, with no event-time balance filter. All eight
models and independent checks pass. Joint pre-trend Wald tests are unavailable
in all eight models; covariance-rank diagnostics are saved. See
[the calendar guide](../../analysis/land_grants/training_50km/CALENDAR.md) for
outputs under `amws_training_50km_calendar_no_reuse_20261014`, support counts,
results and limitations. The Drive companion is unchanged.

### County-target training location: revised definition, 2026-10-14

The new `amws_county_training_location_20261014` release classifies the full
eligible US-born AMWS cohort against each original county centroid, regardless
of birthplace. Rates use that county's births; no catchments are pooled. Both
50 km and 100 km versions use the original county samples, and the calendar
version restores 15 events including East Carolina. All 32 models and independent
classification/reconstruction checks pass. See the
[county-level guide](../../analysis/land_grants/training_50km/COUNTY_TRAINING_LOCATION.md).
Earlier releases remain separate and unchanged; the report's companion URL
points to the corrected county-level version after publication.

## AMWS names: cleaning and biography review (2026-10-22)

`amws_name_cleaning.py` builds the separate
`output/land_grants/amws_name_cleaning_20261022/` release from v06 education
data. All 79,073 edition-1986 records retain their original names and full
biographies; `name_cleaned` replaces every literal period with a comma.
11,854 names change (12,976 periods). The v06 source remains unchanged.

The frozen sample uses `random.Random(42).sample` on records sorted by
`source_key`, with no name-quality or analysis-sample restrictions. All 500
biographies were reviewed in full: 430 need no additional supported change,
14 have proposed corrections, and 56 remain unresolved. The proposals are
not applied. A clearly transcribed duplicate biography corroborates one
correction; its original unresolved decision is retained in an amendment log.

```powershell
python prep/land_grants/test_amws_name_cleaning.py
python prep/land_grants/amws_name_cleaning.py prepare
python prep/land_grants/amws_name_cleaning.py validate --complete
```

Use `TALENT_DETS_DATA_DIR` (or compatibility `DET_DIR`); `--root` also accepts
an explicit Dropbox project root. `prepare` validates an existing release
instead of overwriting it. An unfinished release accepts explicit reviewed
JSONL decisions through `review --reviews <file>`; no decisions are inferred
for unsubmitted records. Completed releases are immutable.

The release's `review_report.md` documents the 14 proposals, candidate next
rules and unresolved OCR cases. Review evidence is in `review_decisions.csv`,
`proposed_name_fixes.csv` and `unresolved_names.csv`; `sample_500.csv` preserves
all sampled biographies. Source hashes, whole-record digests, deterministic
sampling and one decision per sampled ID are validated. A comma is not yet a
validated surname boundary: internal surname punctuation and preceding-entry
spillover require further handling before parent-offspring matching.

## AMWS name regex calibration (2026-10-23)

`amws_name_regex_calibration.py` and `amws_name_regex_rules.py` create a separate
`output/land_grants/amws_name_regex_calibration_20261023/` release. Only sample
copies receive regex changes; all 14 original proposals are applied as explicit
overrides on the original sample copy and excluded from regex performance.

The new 500 IDs exclude the original 500. Selection uses the source-key-sorted
v06 1986 pool and `random.Random(43)`; seed 44 creates a reproducible 300/200
split grouped on normalized biography text to prevent duplicate leakage.
Development labels precede calibration; rules and audited surname vocabulary
are frozen before test biographies can be displayed or labeled. All 500 new
biographies were reviewed in full: 463 no-change, 7 supported fixes, 30 unresolved.

```powershell
python prep/land_grants/test_amws_name_regex_calibration.py
# New-release stages; completed releases cannot be overwritten:
python prep/land_grants/amws_name_regex_calibration.py prepare
python prep/land_grants/amws_name_regex_calibration.py show --partition development --start 1 --end 30
# Record explicit manual labels with append_labels(); never generate missing labels.
python prep/land_grants/amws_name_regex_calibration.py calibrate
python prep/land_grants/amws_name_regex_calibration.py freeze
# Read and explicitly label all test biographies only after freeze.
python prep/land_grants/amws_name_regex_calibration.py evaluate-test
python prep/land_grants/amws_name_regex_calibration.py publish
# Read-only verification of this completed release:
python prep/land_grants/amws_name_regex_calibration.py validate
```

Development repaired 6/6 supported errors and original-sample regression 13/14,
with no observed incorrect changes. The held-out test repaired 0/1, made no
incorrect changes, and formatted two unresolved names. The missed `ED WARD`
split was not used to retune the frozen rules; test repair precision is undefined.
These counts do not establish population accuracy. The corroborated ANGELL
repair remains a record-specific manual override. See `calibration_report.md`,
`review_decisions.csv`, `rule_catalog.json`, `performance_by_rule.csv` and
`review_queue.csv` in the release for evidence, traces, rule counts and remaining
cases. `TALENT_DETS_DATA_DIR`, compatibility `DET_DIR`, and `--root` work as in
the earlier cleaning workflow. Source and prior-release hashes are verified.

## AMWS exact-surname parent–offspring candidates (2026-10-23)

`amws_parent_offspring.py` builds the separate
`output/land_grants/amws_parent_offspring_candidates_20261023/` release from
all four v06 editions. Edition comes from `source_key`, including records with
a blank edition column. A candidate record pair requires an exact normalized
surname and an offspring birth year at least 20 years after the parent's.
Both within-edition and cross-edition pairs are included, with no maximum gap,
gender, geography or additional name-similarity filter. Gaps above 80 are flagged.

The release processes 134,311 records: 125,583 eligible and 8,728 excluded,
across 39,443 eligible surname buckets. It generates 1,761,760 candidate record
pairs, including 39,254 gaps above 80 years. SMITH alone contributes 380,341
pairs, illustrating the concentration of this broad first-stage screen.

Older editions use their `last` field; 1986 receives literal period-to-comma
replacement followed by conservative separator parsing. Surname keys fold
case, accents, whitespace, apostrophes, hyphens and periods, preserving every
surname component. Unexpected characters are rejected before folding. Missing
or ambiguous names, damaged surnames, invalid/missing years and previously
reviewed unresolved entries remain in an excluded-record queue. The original
14 approved corrections are explicit overrides; the calibrated word-join and
surname-vocabulary rules are not applied to the population.

```powershell
python prep/land_grants/test_amws_parent_offspring.py
python prep/land_grants/amws_parent_offspring.py build
python prep/land_grants/amws_parent_offspring.py validate
```

Use `TALENT_DETS_DATA_DIR`, compatibility `DET_DIR`, or `--root` for the Dropbox
project root. `build` refuses an existing release directory. `validate` is
read-only and reconstructs all lookup rows and candidate pairs, checks source
and prior-release hashes, and verifies an independent birth-year-histogram
pair count. Source files, earlier releases and analysis inputs are preserved.

`name_lookup.csv` retains original names, birth-year text, source record digests,
available biographies/birthplaces and all matching decisions. The release also
contains bucket membership/counts, `candidate_pairs.csv`, the original override
ledger, coverage/exclusion/edition-pair/age-gap/frequency summaries and the
largest surname buckets. `inspection_sample_100.csv` freezes 100 random pairs
(or all pairs if fewer), using seed 46; it is not a completed relationship review.
See `matching_report.md` for results and limitations.

Repeat appearances are retained. Existing duplicate indicators and repeated
exact full-name/birth-year signatures are flags only. All reported counts are
**record pairs, not unique people, families or verified relationships**.

## AMWS workplace-location feasibility audit (2026-10-23)

`amws_workplace_feasibility.py` and `read_amws_workplace_sources.R` produce the
separate `output/land_grants/amws_workplace_feasibility_20261023/` release.
One `random.Random(47)` generator draws 20 records per edition from each
source-key-sorted v06 pool in ascending edition order, without eligibility or
name-quality filters. Original name/address/biography fields are recovered by
record ID from the 1906 workbook, 1938 and 1955 Stata files, and 1986 original
CSV. Separate address fields and full entry headers are included.

All 80 entries were manually read in full. Current-as-reported employer names
are explicit in 70/80, university affiliations in 42/80, work states/territories
in 37/80, and work counties in 0/80. These are at-least-one-affiliation counts,
not normalized institution IDs, complete concurrent-workplace coverage or
population estimates. Dated earlier employment has usable employer context in
67/80 and partial evidence in another 9/80. Retired and emeritus status,
non-university employers, foreign/overseas locations and ambiguous OCR/spillover
remain explicit in the review.

```powershell
python prep/land_grants/test_amws_workplace_feasibility.py
python prep/land_grants/amws_workplace_feasibility.py prepare
python prep/land_grants/amws_workplace_feasibility.py show --start 1 --end 10
# Record explicit human decisions using append_reviews(); never infer missing reviews.
python prep/land_grants/amws_workplace_feasibility.py publish
python prep/land_grants/amws_workplace_feasibility.py validate
```

`prepare` and `publish` refuse existing/finalized outputs. R with haven/readr
and Python with openpyxl read the original sources; data paths use
`TALENT_DETS_DATA_DIR`, compatibility `DET_DIR`, or `--root`. No external
institution research or geocoding is performed. A postal address qualifies as
workplace evidence only when tied to the named employer/department; birthplace,
education, research areas and home/club addresses are not substitutes.

See `feasibility_report.md`, `review_decisions.csv`, `workplace_mentions.csv`,
`feasibility_by_edition.csv`, `history_by_edition.csv` and the frozen full-text
`sample_80.csv`. Literal evidence offsets and source/output checksums support
the 80 explicit decisions. Validation reproduces the random draw and original
joins, and protects v06, raw inputs, both name-cleaning releases and the
candidate-pair release. The workflow proposes a later historical institution,
campus and county-resolution stage; it does not modify matching outputs.

## AMWS university-affiliation regex calibration (2026-10-23)

`amws_university_affiliation_calibration.py` draws 500 development and 500
independent test records, 125 per edition per partition. Seeds are 49 and 50,
using one generator per partition across ascending editions and source-key-sorted
pools. All prior 80 workplace-review IDs are excluded. Test pools also exclude
nonempty normalized biography and exact full-name/birth-year fingerprints of
development and prior reviewed entries. This is conservative duplicate detection,
not person linkage.

The separate Dropbox release is
`output/land_grants/amws_university_affiliation_regex_20261023/`. Both full-text
samples and all 1,000 explicit reviews are preserved. The extractor interface is
`amws_university_affiliation_engine.extract_affiliations(entry, edition)`.
It uses generic headers, roles, dates, institution markers and bounded career
sequences, without institution dictionaries or person-specific corrections.
Literal evidence uses original source-field codepoint offsets, end exclusive.
Current, past, mixed (successive current/past roles), and uncertain timing are
distinguished. Canonical institutions, campuses and geocoding are outside scope.

```powershell
python -m unittest discover -s prep/land_grants -p test_amws_university_affiliation.py
# For a new release only; existing samples/rules/evaluation refuse replacement:
python prep/land_grants/amws_university_affiliation_calibration.py prepare
python prep/land_grants/amws_university_affiliation_calibration.py show --start 1 --end 25
# Supply explicit manual references using ManualBatch, then:
python prep/land_grants/amws_university_affiliation_calibration.py develop
python prep/land_grants/amws_university_affiliation_calibration.py freeze
python prep/land_grants/amws_university_affiliation_calibration.py show --partition test --start 501 --end 525
# Complete all 500 independent test references before:
python prep/land_grants/amws_university_affiliation_calibration.py evaluate-test
python prep/land_grants/amws_university_affiliation_calibration.py finish
python prep/land_grants/amws_university_affiliation_release_audit.py
```

All 500 development references preceded prediction evaluation; the final engine,
development references and four development experiments were frozen before test
biographies were opened. The single cached test evaluation followed all 500
test references. Strict institution precision is 81.1%, 88.2%, 76.8% and 57.8%
for 1906, 1938, 1955 and 1986, respectively. **The 95% target was not met in any
edition. This version is not recommended for population extraction.** Partial
institution forms and duplicates are not silently credited as correct; temporal
and conditional role/date metrics, review counts, rule-family results and errors
remain available. One post-test fixture documents a known expected failure for
an OCR section marker glued to a date; it does not change the frozen rules.
Source files, previous releases, candidate pairs and production inputs are
unchanged. See the release's `REPORT.md`, `validation.json`, frozen code,
references, version results and `test_evaluation/` error queue.

## Fresh three-edition university extraction: 150 development entries (2026-10-23)

`amws_university_regex_150.py` publishes the separate Dropbox release
`output/land_grants/amws_university_regex_150_20261023/`. One seed-52 generator
draws 50 fresh records each from 1906, 1938 and 1955, excluding all 1,080 prior
workplace/calibration records and conservative nonempty biography and
full-name/birth-year duplicates. Full original fields, including 1955 `original`,
`address` and `rest`, are frozen with provenance and checksums.

All 150 entries were read in full and manually referenced before any new
predictions were scored. `amws_university_150_manual_references.py` records these
literal human annotations; they are never extraction overrides. References and
qualifiers are frozen separately from subsequent rule development. The interface
is `amws_university_150_engine.extract_universities(entry, edition)`.

The edition-specific source boundaries and shared generic appointment grammar
extract current/past college or university employment, retaining appointments,
printed dates, temporal uncertainty, qualifiers and original evidence offsets.
Address-only affiliations are reported separately and never establish paid work.
No university dictionary, person overrides, spelling restoration, canonical IDs
or external geographic research is used. The preceding frozen extractor is
unchanged. These 150 records are **development data, not an independent test**.

```powershell
python -m unittest discover -s prep/land_grants -p test_amws_university_150.py
# For a new release only; completed outputs refuse replacement:
python prep/land_grants/amws_university_regex_150.py prepare
python prep/land_grants/amws_university_regex_150.py show --start 1 --end 25
# Read every complete entry; supply all manual references, then:
python prep/land_grants/amws_university_regex_150.py freeze-references
python prep/land_grants/amws_university_regex_150.py develop
python prep/land_grants/amws_university_regex_150.py finish
```

See `REPORT.md`, `COMPARISON.md`, `university_comparison.csv`, the full manual
reviews and `development_versions/` for edition/rule metrics and errors. Every
successive rule version has its own engine snapshot; replay that snapshot rather
than substitute later code. `validation.json` reconstructs the seed-52 draw and
all source joins, checks literal evidence and every saved version's deterministic
predictions, and verifies protected-release checksums. No production affiliations,
candidate pairs or analysis inputs are changed.

## Fresh 1986 university extraction: 150 development entries (2026-10-23)

`amws_university_regex_1986_150.py` publishes the separate Dropbox release
`output/land_grants/amws_university_regex_1986_150_20261023/`. It draws 150 records
from all 79,073 v06 1986 records with `random.Random(53)` after sorting eligible
IDs. Prior workplace, affiliation and name-review IDs, plus nonempty normalized
biography and full-name/birth-year duplicates, are excluded. Fingerprints use the
same original-source representation; detection is conservative, not person linkage.

Every full original biography was manually reviewed before prediction scoring.
All original 1986 CSV fields, complete biographies and source checksums are
preserved. `amws_university_1986_150_manual_references.py` records literal human
references and predeclared alternatives; it is never imported by the engine and
does not provide extraction overrides. References are frozen before calibration.

The separate interface is
`amws_university_1986_150_engine.extract_universities(entry, edition="1986")`,
accepting a frozen sample entry or `raw_text_adjusted`. It bounds `Prof Exp` and
`Concurrent Pos` separately, including labels glued to dates, and stops at
memberships, honors, research narratives, addresses and neighboring entries.
Employment roles, generic institutional markers and bounded local carry support
literal affiliations; uncertain hospital/institute ties and damaged names are
flagged. Address-only evidence cannot establish employment or reopen a closed
appointment. No university dictionary, person overrides, spelling restoration,
canonical identities or geographic lookup is used.

```powershell
python -m unittest discover -s prep/land_grants -p test_amws_university_1986_150.py
# New release only; completed outputs refuse replacement:
python prep/land_grants/amws_university_regex_1986_150.py prepare
python prep/land_grants/amws_university_regex_1986_150.py show --start 1 --end 15
# Read every complete biography and enter all 150 manual references, then:
python prep/land_grants/amws_university_regex_1986_150.py freeze-references
python prep/land_grants/amws_university_regex_1986_150.py develop
python prep/land_grants/amws_university_regex_1986_150.py finish
```

The release includes `REPORT.md`, `COMPARISON.md`, `university_comparison.csv`,
manual decisions, appointments, separate address reports, rule traces, version
snapshots, error queues and source/output checksums. Replay each earlier version
with its own engine snapshot. The final development result is 272/272 correct
institution predictions against 278 reference affiliations (100% precision,
97.8% recall), with 250/272 correct temporal classifications. Six institutions
and 22 temporal classifications remain errors. **These 150 are development
records; the result is not independent test or population accuracy.** A later
independent evaluation requires a new sample. Eighteen structural fixtures cover
boundaries, education/employment separation, carry, qualifiers, training, funding,
retirement/emeritus, OCR damage, neighboring records, evidence and determinism.
Source reconstruction and protected-release checks preserve v06, prior extractors,
cleaning releases, candidate pairs and production inputs.

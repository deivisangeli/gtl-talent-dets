# Monthly and quarterly AMWS county panels

This separate panel counts scientists by their recorded birth month and divides
NHGIS annual **recorded live births by place of occurrence** into twelve equal
monthly denominators. It estimates no within-year birth seasonality. The output
is a data preparation product; it does not assign monthly schooling treatment or
estimate effects. Existing annual panels and analyses are unchanged.
The default range is 1915-1950. The Gapminder variant also supports the full
1800-1960 range available in the consolidated annual AMWS input.
A separate quarterly Gapminder panel aggregates the validated full-period
monthly counts into calendar quarters, using annual county birth estimates
divided by four.

## Run

From the repository root (absolute script paths also work from any directory):

```powershell
Rscript prep/mandatory_schooling/download_nhgis_births.R
Rscript prep/mandatory_schooling/build_amws_county_month.R
Rscript prep/mandatory_schooling/test_monthly_amws.R --integration
```

For the separate Gapminder variant:

```powershell
Rscript prep/mandatory_schooling/build_amws_county_month.R --births-source=gapminder
Rscript prep/mandatory_schooling/test_monthly_gapminder_amws.R --integration
```

Omitting `--births-source` retains NHGIS. `--births-source=nhgis` is equivalent.
The Gapminder option uses the annual panel's existing
`county_births_estimate_year`, divided by twelve without rounding. Those annual
estimates equal county population times `us_birth_rate_year`; the rate equals
Gapminder US annual births divided by the upstream annual panel's `us_pop_year`.
The builder preserves that national population universe instead of recomputing
it from the smaller monthly sample. It verifies the annual panel against the
USA row of `input/new_births_total_number_estimated.csv` and stops on disagreement.

This assumes the same national annual birth rate for every county and equal
births across the twelve months. It captures annual fertility changes but no
monthly seasonality or county-specific fertility differences. County-years with
missing population/annual estimates remain missing; nonpositive estimates yield
missing rates. Scientist month selection and completeness are identical to the
NHGIS version. NHGIS birth extracts and API credentials are unnecessary for a
Gapminder build; the existing nominal population reference still supplies
geography cautions. NHGIS-only NYC aggregation cautions are not applied to the
Gapminder denominator.

Gapminder outputs in `output/mandatory_schooling/` are:

- `county_amws_monthly_gapminder_1915_1950.csv`: the same 1,335,744 county-month
  keys, source counts and completeness, with `county_births_estimate_year`,
  `county_births_estimate_month`, `monthly_allocated_births`,
  `amws_per_1000_births`, `amws_per_1000_pop`, national births/population/rate,
  county population and explicit source/allocation provenance. The two monthly
  denominator columns contain the same annual estimate divided by twelve.
- `county_gapminder_annual_births_1915_1950.csv`: the county-year denominator
  spine, scientist totals/completeness, population and national inputs.
- `monthly_gapminder_1915_1950_audits/`: separate scientist and geography audits,
  annual/monthly reconciliation, national source verification, input hashes, and
  county-denominator coverage by state/year. Coverage columns distinguish
  `observed_estimated_births`, `usable_denominators`, zeros and missing values.

The national verification also supplies `gapminder_us_births`, independently read
from the raw file, alongside the upstream `us_births_year`; they must agree.
Usable county coverage is 2,894 in 1915-1919, 3,051 in 1920-1929, 3,084 in
1930-1949, and 3,088 in 1950. Every year still contains the 3,092-county grid.
Integration checks compare scientist counts and completeness against the existing
NHGIS panel, verify allocation and rates, rerun both branches from outside the
repo, and confirm all existing NHGIS/annual/source files remain identical by hash.

### Full-period Gapminder panel: 1800-1960

```powershell
Rscript prep/mandatory_schooling/build_amws_county_month.R --births-source=gapminder --years=all
Rscript prep/mandatory_schooling/test_monthly_gapminder_all_years.R --integration
```

`--years=all` derives the inclusive year range from the consolidated annual AMWS
input, currently 1800-1960, and validates Gapminder national births for each of
the 161 years. This option requires the Gapminder source. Commands without it
retain 1915-1950 and the existing output filenames.

The full dataset has **5,973,744 rows**, with all 3,092 counties and 1,932 months
per county. It uses exactly the same annual county birth estimates divided by
twelve, scientist selection, date parser and no-cross-dedup rule. A positive
monthly birth estimate with no assigned scientist births yields a **zero** rate;
a missing or nonpositive estimate yields a missing rate. Scientists with
unassigned months remain in the audits and are not distributed across months.
The initial build assigns 111,701 scientist births to months and audits 2,284
unassigned records within the county universe.

The separate full-period outputs under `output/mandatory_schooling/` are:

- `county_amws_monthly_gapminder_1800_1960.csv`: the full county-month panel,
  approximately 2.23 GB as an uncompressed CSV, with the same outcome and source
  fields as the shorter Gapminder version.
- `county_gapminder_annual_births_1800_1960.csv`: all 497,812 county-year keys,
  including missing denominators, annual scientist totals and completeness.
- `monthly_gapminder_1800_1960_audits/`: full-period scientist assignments and
  exclusions, denominator/scientist reconciliation, national birth verification,
  geography flags, input hashes and annual/state coverage summaries.

Usable-denominator coverage varies with historical county population availability:

| Year | Counties with positive estimated births |
|---|---:|
| 1800 | 384 |
| 1820 | 702 |
| 1850 | 1,530 |
| 1900 | 2,756 |
| 1960 | 3,092 |

Missing population years are not extrapolated or filled. The upstream national
population denominator is preserved. Geography review flags inspect nominal
names over the full selected census range; `late_nominal_coverage` now refers
to coverage starting after 1800. This remains a caution system rather than a
complete historical boundary harmonization.

The builder writes sorted batches of at most 100 counties to limit memory use,
validates each batch, and then verifies totals across the complete grid. Full
integration tests require unique keys, complete month grids, zero/missing-rate
behavior, annual/monthly reconciliation, identical counts/completeness and
denominators/rates for the 1915-1950 overlap, and byte-identical cached reruns.
All earlier panels, audits and scientist sources are protected by hash checks.
The AMWS selection and censoring limitations described below continue to apply;
extending the year grid does not create additional source coverage.

### Quarterly Gapminder panel: 1800-1960

Build the full-period monthly Gapminder panel first, then run:

```powershell
Rscript prep/mandatory_schooling/build_amws_county_quarter.R
Rscript prep/mandatory_schooling/test_quarterly_gapminder_amws.R --integration
```

The quarterly builder runs from any working directory and takes no arguments.
It uses the existing full-period monthly panel and its normalized annual table;
it does not repeat scientist selection, date parsing or deduplication. Calendar
quarters are January-March, April-June, July-September and October-December.
Scientist counts are the sum of the three monthly counts, separately for
`n_amws_1906_1955_dedup` and `n_amws_1986`, with their sum in `n_amws`.
The grid retains all **3,092 counties**, **161 years**, and **1,991,248 rows**,
or **644 quarters per county**, including county-years without denominators.

The output is `output/mandatory_schooling/county_amws_quarterly_gapminder_1800_1960.csv`.
It contains `quarter` (1-4) and `quarter_start` (January 1, April 1, July 1 or
October 1). The identical `quarterly_allocated_births` and
`county_births_estimate_quarter` columns equal `county_births_estimate_year / 4`,
without rounding. `quarterly_allocation` is `annual_estimate_divided_by_4`.
Monthly time/allocation fields are omitted.

`amws_per_1000_births` is `1000 * n_amws / quarterly_allocated_births`.
Positive estimated births and no assigned scientist births produce zero rates.
Missing or nonpositive birth estimates produce missing rates; missing estimates
are not converted to zeros. County `population` and national births, population
and birth rate are carried unchanged from the annual table. `amws_per_1000_pop`
uses the quarterly count divided by that annual county population, with the
same missing/nonpositive-rate rules. Rates are recomputed from quarterly counts.

Annual source totals, provenance and geography flags are carried unchanged into
each quarter. `birth_month_completeness`, `birth_month_complete`,
`early_month_completeness`, `late_month_completeness`, `n_unassigned_month` and
source-specific assigned/unassigned counts remain **county-year diagnostics**;
they are repeated rather than summed, and do not measure completeness of an
individual quarter. Scientists lacking an assigned month are also unassigned to
quarters. The initial quarterly build retains **111,701 assigned births** and
**2,284 unassigned records** within the county universe.

Separate audits under `output/mandatory_schooling/quarterly_gapminder_1800_1960_audits/` are:

- `quarterly_annual_reconciliation.csv`: quarterly sums, annual birth estimates,
  source-specific assigned/unassigned counts and reconciliation to the original
  annual AMWS panel wherever observations exist.
- `scientists_unassigned_quarter.csv`: a byte-identical copy of the existing
  unassigned-month records and parsing reasons. Its original schema is retained;
  every record remains unassigned to any quarter. Copying the audit verbatim
  preserves OCR text containing unusual runs of quotation marks.
- `coverage_year.csv` and `coverage_state_year.csv`: annual and state/year
  coverage, scientist totals, missing/zero denominators, total quarter rows and
  usable quarter denominators. County-year coverage is identical to the monthly
  variant; each usable county-year contributes four usable quarters.
- `county_geography_flags.csv`, `gapminder_national_birth_verification.csv` and
  `input_manifest.csv`: inherited historical geography cautions, national source
  verification and input-file hashes.

The builder writes sorted batches of at most 100 counties. Unit tests cover
calendar boundaries, source aggregation, positive-denominator zeros, missing
and nonpositive denominators, and invalid monthly keys/counts. Integration
checks require complete quarterly grids, exact source aggregation, unchanged
annual metadata, correct rates and denominator sums, source reconciliation,
matching year/state coverage, deterministic cached reruns and unchanged existing
monthly, NHGIS, annual and scientist-source files.

Quarterly allocation assumes the same national annual birth rate across counties
and equal births across the four quarters; it models no within-year seasonality.
No population or scientist dates are imputed. Existing historical geography,
AMWS source coverage and censoring limitations remain. This is a separate data
preparation output without quarterly treatment assignment or estimation.

Dependencies are `ipumsr`, `data.table` and `stringr`. `paths.R` reads
`TALENT_DETS_DATA_DIR`; `IPUMS_API_KEY` or `IPUMS_KEY` authorizes uncached NHGIS
downloads. Credentials are never written to the manifest. Matching completed
archives work offline; unfinished requests resume by their saved extract number.

## Gender-specific outcomes at the 0.8 threshold

All four panels and the three annual denominator/support tables include gender
splits using the saved `male_80` and `female_80` flags from
`output/amws/gender_inference/amws_gender_records.csv`. Run the AMWS gender
builder and threshold-flag builder before rebuilding these panels. The panel
builders require both flags and never repeat name inference or derive female
labels from a zero male flag. Existing commands rebuild the augmented monthly
panels; rebuild the full-period monthly variant before the quarterly variant.
Validate the complete set with:

```powershell
Rscript prep/mandatory_schooling/test_amws_panel_gender.R --integration
```

For faster validation of already-built panels, use DuckDB in R:

```powershell
Rscript prep/mandatory_schooling/validate_amws_panel_gender_duckdb.R
```

This checks all rows using SQL, including source/gender counts, annual
reconciliation, formulas, missing denominators, common-period equality and
monthly-to-quarterly sums. Add `--rerun-quarterly` to also verify a byte-identical
cached quarterly rebuild. It uses the installed `duckdb` and `DBI` R packages
and a temporary on-disk database with a 4 GB memory limit. Setting
`AMWS_PANEL_VALIDATION_ENGINE=duckdb` selects DuckDB for the builders' staged
original-column comparison; the default bounded R comparison remains available.

`male_80 == 1` assigns male, `female_80 == 1` assigns female, and neither flag
equaling 1 assigns unclassified. Both-zero flags are ambiguous predictions;
missing flags arise from unreadable or unmatched names. Records absent from the
saved inference sample are also unclassified, with the distinct audit reason
`no_saved_inference`. Both flags equaling 1 is an error. The existing Comfort
Avery Adams correction is encoded as male = 1 and female = 0. The primary
95% classifications, saved proportions and all gender-inference files are
preserved. The threshold is a reference-proportion cutoff, not a verified
prediction-accuracy measure.

The period panels append 49 fields, using suffixes `_male_80`, `_female_80` and
`_unclassified_80` on the following existing bases:

| Role | Gender-specific bases |
|---|---|
| Period counts | `n_amws`, `n_amws_1906_1955_dedup`, `n_amws_1986` |
| Period rates | `amws_per_1000_births`, `amws_per_1000_pop` |
| Annual source totals | `n_early_year`, `n_1986_year` |
| Annual assigned/unassigned counts | `n_early_assigned`, `n_1986_assigned`, `n_early_unassigned`, `n_1986_unassigned`, `n_unassigned_month` |
| Annual completeness | `birth_month_completeness`, `birth_month_complete`, `early_month_completeness`, `late_month_completeness` |

For example, `n_amws_female_80` counts female-classified scientists assigned to
that month or quarter, and `amws_per_1000_births_female_80` divides that count by
the same total-birth denominator used by the combined outcome. Each table also
contains `gender_inference_threshold = 0.8`. Annual support tables append the
annual counts/completeness and threshold (34 fields). Annual diagnostics remain
county-year quantities repeated across periods. Completeness ratios are missing
when that gender/source group has no records; completeness flags are true when
there are no unassigned months, following the existing convention.

Gender counts sum to the original combined/source counts. Gender rates use the
existing total births or total resident population, so they sum to the original
rate wherever the denominator is positive. Empty gender groups receive zero
counts and zero rates with a positive denominator. Missing/nonpositive
denominators yield missing rates for every group. Quarterly gender counts sum
the three monthly counts; quarterly rates are recomputed with annual births
divided by four. No sex-specific birth or population denominators are introduced.

The saved gender sample contains 112,291 records, while the full monthly
pipeline retains 113,985 records. The additional 1,694 were excluded before name
inference because their county-years are absent from the consolidated annual
AMWS input, whose row availability depends on historical population coverage.
They span 1,119 county-years and 454 counties: 187 records from 1906, 706 from
1938, 580 from 1955 and 221 from 1986. Their lack of inference does not establish
that their names are ambiguous or unreadable. All have missing Gapminder
population/birth denominators, but 25 have positive NHGIS birth denominators.
They remain unclassified rather than being dropped or receiving new inference.
Full-period totals, including unassigned months, are 94,369 male, 5,480 female
and 14,136 unclassified records.

Matching uses edition plus the original source-row position, checked against
source identifiers, county, birth year and recorded source checksums. This
avoids duplicate identifiers in the early files. Dedicated gender audits in
each panel's existing audit directory contain:

- `scientist_gender_assignments.csv`: stable source keys, saved flags, exhaustive
  gender labels/reasons, original date text and month-assignment status.
- `scientists_no_saved_gender_inference.csv` and
  `no_saved_inference_denominator_coverage.csv`: the absent-inference records and
  their county-year population/birth-denominator coverage.
- `gender_classification_coverage.csv` and `gender_coverage_state_year.csv`:
  record counts by edition/reason and by state/year.
- Monthly audits: `scientists_unassigned_month_by_gender.csv`,
  `gender_annual_reconciliation.csv` and `gender_monthly_annual_reconciliation.csv`.
- Quarterly audits: `scientists_unassigned_quarter_by_gender.csv` and
  `gender_quarterly_annual_reconciliation.csv`, plus inherited classification and
  denominator-coverage audits from the full monthly panel.

Legacy scientist-assignment/unassigned audits retain their original schema and
OCR text. Builders write county batches into staging directories, verify every
original panel/support-table column before installation, and retain recoverable
backups until installation succeeds. Tests cover mutually exclusive flags,
absent inference, stable-row joins, assignment reconciliation, additive rates,
monthly-to-quarterly aggregation, cached reruns and unchanged inference/raw
sources and original annual analysis panels. Adding columns changes panel files;
their existing values, grids and denominator coverage remain unchanged.

## Inputs and birth definitions

- Existing `output/us_panel_county_amws_combined_year.csv` supplies the county
  universe, annual population and reconciliation targets.
- `output/amws/amws_combined_us_geocoded.csv` supplies retained early-edition
  records. `Data/processed/amws/amws_ed86_final.csv` supplies US-born geocoded
  1986 records. The existing see-previous-edition contamination exclusion is
  repeated. No additional deduplication is performed against 1986.
- `input/nhgis0005_ts_nominal_county.csv` supplies the explicit historical
  GISJOIN-to-project-GEOID crosswalk.
- NHGIS `1915_1941_cVS`, NT001, AF1U001 supplies 1915-1938. NHGIS
  `1939_1959_cVS`, NT001, AGVA001 supplies 1939-1950. Both county and state
  extracts, metadata, manifests and original codebooks are cached under
  `raw/nhgis_births/{early,late}/`. Early 1939-1941 is retained only for auditing.
  Late-vintage missing values never fall back to early values.

The early table's title says "births". The original Census
[1935 report](https://www.cdc.gov/nchs/data/vsushistorical/birthstat_1935.pdf),
Table B, printed page 4 (PDF page 8), covers 1915-1935 and explicitly excludes
stillbirths. Its 1915 and 1935 totals exactly match sums of NHGIS state NT001:
776,304 and 2,155,105. Later metadata explicitly calls NT001 live births and
separately identifies stillbirths. The early/later 1939-1941 state totals agree.
The source PDF and inspected page are cached in `raw/nhgis_births/source_verification/`.
See [NHGIS source documentation](https://www.nhgis.org/tabular-data-sources).

The [birth registration area](https://www.cdc.gov/nchs/hus/sources-definitions/registration-area.htm)
expanded from ten states and DC in 1915 to all states in 1933. Recorded births
are not a correction for underregistration. Missing county values remain missing;
recorded zeros remain zero and yield missing rates. State tables audit coverage
only and do not allocate births to counties.

## Scientist months and geography

The early-edition parser stops at semicolons or education/marriage markers in
`birthplace_orig`; 1986 uses only `birth_date`. Named months, a valid day, and an
explicit two- or four-digit year must agree with the canonical `birth_year`.
Periods and commas and compact strings such as `Feb17,43` are accepted. OCR
letter substitutions, missing dates, invalid days, multiple possible dates and
year conflicts remain unassigned. Nothing is inferred from education dates.
Record-level assigned and unassigned audits preserve the text and exclusion reason.

All 3,092 counties in the existing annual panel's contiguous-US-plus-DC universe
receive 432 rows. Population stays missing for county-years absent from that
annual panel; scientist counts still come from the valid source records. Those
years cannot be reconciled to an absent annual observation and are flagged by
`annual_panel_present`. Counts of zero mean no *assigned* scientist records;
check completeness before interpreting them.

Matches use exact GISJOINs in the nominal population crosswalk; no fuzzy matching,
aggregation or population reallocation is used. Ambiguous source or target
identifiers are rejected and retained in audits. Important geography cautions:

- New York City is an aggregate, not Manhattan. In early 1939-1941 it is stored
  under G3600610, while the later source uses G3609610. Their identical totals
  explain the six raw overlap differences. The city aggregate is rejected for
  borough denominators; available borough observations remain separate.
- [Fulton County's 1932 merger](https://fultoncountyga.gov/Inside-Fulton-County/About-Fulton-County/History)
  and [Princess Anne's incorporation into modern Virginia Beach](https://www.lva.virginia.gov/collections/ccmf/VA/VA229)
  are explicitly flagged. Historical units not in the county universe remain
  unmatched. This is not a complete inventory of boundary changes.
- `late_nominal_coverage` identifies counties whose population series starts
  after 1915; it does not assert a legal formation date. Every row warns that
  historical boundaries have not been harmonized to modern scientist geocodes.

## Outputs and checks

In `output/mandatory_schooling/`:

- `county_amws_monthly_1915_1950.csv`: one county-month row with GEOID, state ID,
  year, month, month-start date, counts by source, combined counts, annual births,
  fractional monthly births, and `amws_per_1000_births` and `amws_per_1000_pop`.
  Annual source totals, unassigned counts and completeness by source accompany
  each row, along with NHGIS provenance and geography/coverage flags.
- `county_nhgis_annual_births_1915_1950.csv`: normalized county-year spine with
  births, population, source counts, provenance and annual completeness.
- `monthly_1915_1950_audits/`: crosswalk and unmatched counties, raw vintage
  overlap, state births and county/state coverage differences, source record
  assignments, contamination exclusions, out-of-universe records, unassigned
  months, annual/monthly reconciliation, state/year coverage, live-birth
  definition checks and input hashes.

The builder fails on duplicate keys, invalid count values, failed annual-source
reconciliation or monthly/annual sum discrepancies. Tests exercise ambiguous and
conflicting dates, misleading education/place text, leap days, geography ambiguity,
missing versus zero births and vintage precedence. Integration runs from outside
the repo, compares every generated CSV's hash across cached reruns, and confirms
existing annual outputs and scientist sources are unchanged.

AMWS combines selected editions with different inclusion and censoring patterns;
1986 is not deduplicated against early editions. This monthly construction
preserves those limitations. Rates use assigned-month counts, never redistribute
unassigned scientists, and do not correct scientist selection or birth seasonality.

NHGIS citation: Jonathan Schroeder et al. IPUMS National Historical Geographic
Information System: Version 21.0 [dataset]. Minneapolis, MN: IPUMS, 2026.
https://doi.org/10.18128/D050.V21.0. Original extract codebooks retain the complete
citation and use terms. Data stay in the project's Dropbox tree.

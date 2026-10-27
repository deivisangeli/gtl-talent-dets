# Mandatory Schooling Prep Pipeline

The separate [monthly AMWS panel](monthly_amws_panel.md) builds county-month
outcomes for 1915-1950 using recorded scientist birth months and NHGIS annual
occurrence births divided by twelve. Its downloader, builder and tests preserve
the existing annual panels.
Use `--births-source=gapminder` for a separate variant that divides the existing
Gapminder county annual birth estimates by twelve; the NHGIS default remains available.
Add `--years=all` to the Gapminder build to cover the full 1800-1960 annual AMWS
range, producing a separate panel and audits while preserving the shorter versions.

The [quarterly Gapminder panel](monthly_amws_panel.md#quarterly-gapminder-panel-1800-1960)
uses the validated full-period monthly counts and annual county birth estimates.
Run `Rscript prep/mandatory_schooling/build_amws_county_quarter.R` from the repo
root (or use an absolute script path from any directory), followed by
`Rscript prep/mandatory_schooling/test_quarterly_gapminder_amws.R --integration`.
It writes `county_amws_quarterly_gapminder_1800_1960.csv` and separate quarterly
audits in Dropbox `output/mandatory_schooling/`: 1,991,248 rows for 3,092 counties,
with four calendar quarters per year, summed scientist counts and annual birth
estimates divided by four. Positive denominators with zero counts yield zero
rates; missing/nonpositive denominators yield missing rates. Existing panels
remain unchanged.

All four period panels and their annual support tables now include male, female
and unclassified scientist counts, rates and annual birth-month completeness
using saved `male_80`/`female_80` flags. Existing total-birth and population
denominators are retained. Rebuild with the commands above after the AMWS gender
and threshold-flag steps, then run
`Rscript prep/mandatory_schooling/test_amws_panel_gender.R --integration`.
For SQL validation of existing outputs, run
`Rscript prep/mandatory_schooling/validate_amws_panel_gender_duckdb.R`;
add `--rerun-quarterly` to check a cached quarterly rebuild with DuckDB.
See [the gender-specific definitions and audits](monthly_amws_panel.md#gender-specific-outcomes-at-the-08-threshold),
including the 1,694 records outside the saved inference sample.

The current pipeline is manual. The canonical treatment dates are from
[Lingwall (2014), dissertation Table 6.2](https://www.cmu.edu/sites/default/files/cmu-tepper-site-files/2025-06/2014-joint-phd-program-economics-and-public-policy-lingwall-dissertation.pdf),
and record the first year a compulsory attendance law was in effect in every
county of each state:

1. Maintain `DATA_INPUT/compulsory_schooling_laws.csv` in Dropbox. The earlier
   input is archived as `compulsory_schooling_laws_legacy_pre_lingwall.csv`, and
   the source-specific copy is `compulsory_schooling_laws_lingwall_statewide.csv`.
   The parallel `compulsory_and_child_labor_laws_lingwall.csv` reproduces both
   Table 6.2 law series and their separate session-law references. Its child-labor
   date is the first restriction on employment across a broad range of industries;
   it is a source artifact and does not change the analysis treatment.
2. Run `analysis/mandatory_schooling/analysis_compulsory_schooling.R`, which reads the manual CSV directly and assigns treatment by state law decade.

The AMWS-outcome analysis uses the consolidated county-decade panel shared
with the USA world's-fairs pipeline. Its default profile is unbalanced. To run
the event-time-balanced `e = -40,...,+50` profile in PowerShell:

```powershell
$env:COMPULSORY_AMWS_PROFILE = "balanced_m40_p50"
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws.R
```

The balanced profile requires every retained county at every reported event
time, but keeps each retained county's other available decades in estimation
so the `e = +50` coefficient remains identified.

The balanced run also writes treatment-cohort event studies to
`results/mandatory_schooling/compulsory_schooling_amws_balanced_m40_p50/per_cohort/`.
These figures show the unaggregated Sun-Abraham cohort-by-event-time cells for
all four AMWS outcomes in levels and cohort-linear-detrended form. All seven law
cohorts are shown. Collinear boundary cells are left blank and recorded in the
support audit (`g = 1850` at `e = -40,-30,-20`; `g = 1910` at `e = +50`). The
balanced sample has no never-treated states, so this is the appropriate analogue
of the college-allocation per-cohort CSDID figures rather than a literal reuse
of their never-treated runner-up design.

### AMWS county maps for selected birth cohorts

To map AMWS scientists born per 1,000 estimated births for the inclusive
1800-1850 and 1920-1950 birth-year windows, run from the repository root:

```powershell
Rscript analysis/mandatory_schooling/plot_amws_per_1000_births_by_period.R
```

The script computes each county-period rate as the ratio of period totals,
`1,000 * sum(n_amws) / sum(county_births_estimate_year)`, and places the two
CONUS maps in one figure with a shared scale. It writes PNG and PDF versions to
`results/mandatory_schooling/amws_county_birth_maps/` in Dropbox.

For the separate variant restricted to counties in the 16 states whose first
compulsory-schooling law took effect strictly after 1900, run:

```powershell
Rscript analysis/mandatory_schooling/plot_amws_per_1000_births_late_treated_by_period.R
```

This version maps the inclusive 1890-1900 and 1920-1930 windows using a fixed
common sample: both panels retain only the 1,281 counties with a positive,
finite estimated-birth denominator in 1890-1900. It retains the full CONUS
outline for context: counties outside the restricted states are light grey,
while eligible counties outside the common sample are darker grey. Its output
filenames include
`treated_after_1900_1890_1900_1920_1930` so they do not replace the all-county
figure.

This is separate from `prep/elite_schools`: mandatory schooling is a broad state-level compulsory attendance law treatment, while elite schools are selective secondary-school openings.

## Law enforcement: school attendance

A compulsory attendance law on the books is not a law enforced. **School
attendance is the direct first stage** — the law mandates exactly that
school-age children attend school — and unlike literacy it is measured
alongside the 1850–1880 law dates rather than decades later.

The series is built once for the whole repository in
`prep/build_county_demographics_panel.R` and read against the law dates by
`prep/mandatory_schooling/build_schooling_enforcement.R`. No NHGIS extract and
no API key are needed: each year's numerator and its school-age denominator
were already in Dropbox, having arrived with extracts downloaded for other
domains.

| Year | Level | Attending | School-age denominator | Rate |
|---|---|---|---:|---|
| 1850 | county | `AD7001` (also by race `AD5`, race×sex `AD4`, nativity `AD6`) | free persons 5–19, from `AEL` | 56.0% |
| 1880 | **state** | `AQQ001` pupils during the year; `AQT001` average daily attendance | — | 0.631 daily/pupil |
| 1890 | county | `AVK005-008` common-school pupils, race×sex | `AUN001-006` persons 5–20 | 56.8% |
| 1920 | county | `A7N001-004` by age band | `A7M002-005` same bands | 68.1% (7–20) |

**1860 and 1900 have no county school table in NHGIS at all.** For 1860 this is
a hard gap, not a download oversight: `1860_cPAX` has 41 tables and none is
school-related, and the extract already requested all 41. For 1900 all 55
tables were downloaded and none is school-related. 1880 exists only for states
(`1880_sPbSMX`), which is still directly usable because the laws are
state-level.

### Two 1920 rates, because they answer different questions

`school_attendance_rate` is **broad** — 1920 sums all its age bands (7–20) so
the figure lines up with the 1850 and 1890 totals. `attendance_rate_7_13` is
the **compulsory-attendance age band** and is the enforcement-relevant one. The
bands fall away exactly as expected, which is itself informative about where
the laws bound:

| Band | 1920 rate |
|---|---:|
| 7–13 | 90.7% |
| 14–15 | 80.0% |
| 16–17 | 43.0% |
| 18–20 | 14.9% |

The 7–13 figure matches the published national rate of about 90.5%, which is
the check that the `A7N`/`A7M` bands are aligned. They are easy to misalign:
`A7N` has four bands starting at 7–13 while `A7M` has five, the first being
"Under 7", so pairing by index would divide attendance for 7–13 by the
population under 7. `prep/test_nhgis_education_helpers.R` pins this.

### Universes are not comparable across years

1850 counts anyone attending any institution, colleges and academies included;
1890 counts only pupils on the rolls of *common* schools; 1920 counts persons
attending school within an age band. Pooling them in one event study would
confound the universe change with the treatment. `attendance_universe` and
`attendance_age_span` carry this per row — read them before comparing years.

### The 1850 denominator excludes enslaved children

The 1850 attendance universe is "White and Free Colored Persons Attending
School", so the denominator is free persons aged 5–19, built from the `AEL`
race-by-sex-by-age table. Counting enslaved children, who were largely barred
from schooling, would depress the rate in slave states for a reason that is
slavery rather than weak schooling. `AEL` is used in preference to `AEO001`,
the published 5–19 total, because it decomposes by race so the total and the
race-specific rates share one universe; `AEO001` is itself already free-only
(0.94–1.00 of the `AEL` free total, against 0.42–0.63 of the all-persons total
in slave states) and is retained in `school_age_population_published` as an
independent check.

State ordering in 1850 matches the historical record: Maine 87%, Vermont 85%
and New Hampshire 85% at the top, Virginia 28% and Florida 26% at the bottom,
and Gold Rush California last at 10%.

### Outputs

`output/mandatory_schooling/enforcement/`:
`county_schooling_enforcement.csv` (county-year, law year, `years_since_law`,
attendance and literacy together), `state_schooling_by_year.csv`
(denominator-weighted state rates), `state_attendance_1880.csv`, and
`coverage_by_year.csv`.

`daily_attendance_per_pupil` in the 1880 file is average daily attendance over
pupils enrolled. It needs no population denominator and reads as enforcement
*intensity* rather than enrollment reach — arguably the sharpest single
enforcement measure available for the law window.

### Did the laws bite? Attendance by law decade

```powershell
Rscript analysis/mandatory_schooling/summarise_attendance_by_law_decade.R
```

Distribution statistics for the county attendance rate, grouped by the decade
the state's law took effect. Outputs go to
`results/mandatory_schooling/attendance_law_decade/`:
`attendance_by_law_decade.csv`, `attendance_by_law_decade_region.csv` and
`notes.txt`.

The table is built on **1920**, not 1930. 1930 attendance is not in Dropbox —
the county panel stops at 1920 — and it is still unverified whether NHGIS
publishes a 1930 county attendance table at all. 1920 is the closest available
year and the only one with an age-graded rate, so `attendance_rate_7_13`
covers exactly the compulsory-attendance ages. The script is parameterised by
year and picks 1930 up with no code change once the extract lands.
`MS_ATTENDANCE_YEARS` selects years, or `all` to add 1850 and 1890.

1920, ages 7–13, percent:

| Law decade | Counties | States | Southern | Mean | SD | Min | p25 | Median | p75 | Max |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| All | 3,062 | 49 | 17 | 88.6 | 9.3 | 34.7 | 85.1 | 91.9 | 95.0 | 99.2 |
| 1850 | 14 | 1 | 0 | 95.9 | 0.9 | 94.5 | 95.3 | 95.9 | 96.7 | 97.4 |
| 1860 | 15 | 2 | 1 | 93.6 | 1.6 | 89.7 | 93.2 | 93.8 | 94.4 | 95.7 |
| 1870 | 600 | 13 | 0 | 94.1 | 3.2 | 64.0 | 93.0 | 94.7 | 96.1 | 98.4 |
| 1880 | 601 | 10 | 0 | 93.4 | 4.5 | 61.7 | 92.0 | 94.5 | 96.2 | 99.2 |
| 1890 | 406 | 7 | 2 | 90.8 | 7.0 | 39.2 | 87.8 | 92.7 | 95.5 | 98.9 |
| 1900 | 318 | 5 | 3 | 91.6 | 6.3 | 66.2 | 89.7 | 93.9 | 95.8 | 98.1 |
| 1910 | 1,026 | 10 | **10** | 81.4 | 10.2 | 34.7 | 76.8 | 83.8 | 88.7 | 97.9 |
| 1920 | 82 | 1 | 1 | 80.3 | 11.4 | 35.6 | 76.9 | 83.5 | 87.9 | 93.2 |

Mean attendance falls and dispersion rises as adoption gets later, which looks
like bite.

**It mostly is not.** Law-adoption decade is almost perfectly collinear with
region: all ten states adopting in the 1910s are Southern, and the 1870s (13
states) and 1880s (10 states) cohorts contain none. Holding region fixed, the
gradient largely disappears:

| Law decade | Non-South mean | South mean |
|---|---:|---:|
| 1850 | 95.9 | — |
| 1860 | 93.6 | 93.5 |
| 1870 | 94.1 | — |
| 1880 | 93.4 | — |
| 1890 | 92.9 | 87.9 |
| 1900 | 93.8 | 87.1 |
| 1910 | — | 81.4 |
| 1920 | — | 80.3 |

Within the non-South, attendance is flat at roughly 93–96% regardless of when
the law passed, and the 1900 adopters sit *above* the 1880 adopters. Within
the South there is a gradient, but only across 1890–1920 and confounded with
everything else distinguishing those states. The two regions barely overlap in
adoption timing, so the raw table is largely a North–South comparison wearing a
law-timing label.

Two further reasons to read this as descriptive only:

- By 1920 every state had a law, so there is no untreated comparison group.
  Variation across groups is in *length of exposure*, not treatment status.
- Statistics are unweighted across counties, which is what a distribution
  asks for, but means a small county counts as much as a city.
- `counties_over_100` flags counties whose rate exceeds 100%. Only the
  age-banded measures restrict the numerator to the denominator's ages, so
  1850 (2 counties) and 1890 (17, max 186%) can overshoot. The 1920 banded
  rates have none, which is a further check that the `A7N`/`A7M` bands line
  up.

For a causal answer, use the annual Callaway–Sant'Anna design with
not-yet-treated controls documented above. This table is the descriptive first
look that motivates it.

### Still unknown

1840, 1870, 1880 (county) and 1910 have never had more than three tables pulled
from their datasets, so whether they carry an attendance table is unresolved.
They are included in the pending education extract
(`prep/download_county_data.r`); a miss there is reported, not fatal, and the
candidate-tables audit will settle it.

## Law enforcement: county literacy

A compulsory attendance law on the books is not a law enforced. County literacy
is the first proxy for enforcement. The series itself is built once, for the
whole repository, in `prep/build_county_demographics_panel.R`; it is shared with
the land-grants and inventor-determinants analyses, so it is not duplicated
here. This pipeline only reads it against the law dates:

```powershell
Rscript prep/build_county_demographics_panel.R    # run from prep/
Rscript prep/mandatory_schooling/build_schooling_enforcement.R
```

Outputs land in `output/mandatory_schooling/enforcement/`:
`county_schooling_enforcement.csv` (county-year, law year, `years_since_law`),
`state_schooling_by_year.csv` (denominator-weighted state rates), and
`coverage_by_year.csv`.

**Coverage is set by what the census asked, not by what we pulled.** The
decennial literacy question ("able to read and write") ran 1840-1930 and was
dropped afterwards; from 1940 the census asked years of school completed
instead. NHGIS has no county literacy table for 1860, 1880 or 1890.

| Census years | Measure | Columns |
|---|---|---|
| 1840, 1850, 1870, 1900, 1910, 1920, 1930 | Direct literacy | `literacy_rate`, `illiterate_share_10plus`, `illiterate_share_total_population` |
| 1940, 1950 | Attainment proxy | `low_education_share_25plus`, `no_schooling_share_25plus`, `elementary_complete_share_25plus`, `median_school_years_25plus` |
| 1860, 1880, 1890 | None | no county table in NHGIS |

`literacy_measure_type` marks every row as `direct_literacy` or
`attainment_proxy`. **1940 and 1950 carry no `literacy_rate`.** There is no
1950 literacy rate to compute: `low_education_share_25plus` is the share of
persons 25 and over with fewer than five years of school completed, which is
the Census Bureau's own functional-illiteracy convention for 1950-1970, not a
measured literacy rate. The two are different constructs and the panel keeps
them in different columns on purpose.

Universes differ across the direct-literacy years and are recorded per year in
`output/county_nhgis_demographics_variable_map.csv`: 1840 covers white persons
over 20, 1850 adults, 1870 persons who cannot write, and 1900-1930 persons 10
and over. Only 1910-1930 carry a published 10-and-over denominator, so
`literacy_rate` is defined only for those years; earlier years normalise on
total population instead. Rates are not comparable across universes without
saying so.

The 1930-1950 variable codes are resolved from the NHGIS codebooks at build
time rather than hand-listed, because they are split by sex and vary by
dataset. Every resolved code and the description it matched are written to
`output/county_nhgis_education_code_resolution.csv`. Resolution refuses to
guess: an ambiguous total, an unrecognised attainment bin, a missing bin, or
more than one candidate table stops the build instead of silently summing a
subtotal into its parent or treating an unmatched bin as zero. Run the checks
with:

```powershell
Rscript prep/test_nhgis_education_helpers.R
```

Validation against published national figures: illiteracy among persons 10 and
over comes out at 7.63% for 1910 and 5.96% for 1920, against published 7.7% and
6.0%. State ordering matches the historical record, with Louisiana, South
Carolina, Mississippi and Alabama highest in 1920 and Iowa, Nebraska and Oregon
lowest.

### Prerequisites and a caveat

1930-1950 need their own NHGIS extract, which needs `IPUMS_API_KEY` (or the
existing `IPUMS_KEY`). Until it is downloaded the panel stops at 1920; set
`SKIP_NHGIS_EDUCATION=true` to build the 1790-1920 years deliberately rather
than by accident.

Adding the 1930-1950 rows changes which census vintage the land-grants control
scripts pick, because they take the latest available pre-treatment literacy
year. After the extract lands, diff `illiterate_share_control_year` and
`illiterate_share_year_distance_to_treatment` in
`analysis/land_grants/analysis_amws_county_pairs_all_colleges_controls.R`,
`..._from_1900.R` and `analysis_county_pairs_inventor_rates_hyde_pre1900_controls.R`
against their current results, and report any unit whose vintage moves.

Timing is the real limitation of this measure, not coverage. The early-adopter
cohorts this pipeline studies have law years of 1850-1880, while 1950 adults
were born in 1925 or earlier. 1930 is the best-timed direct-literacy year for
these cohorts: its adults were born roughly 1860-1910, inside the exposure
window. Using literacy as an enforcement moderator is a different estimand from
using it as a pre-treatment matching covariate, and the matching vintage rule
(latest source year at or before `g-15`) rejects 1930 and 1950 for every
1850-1880 cohort. This is deliberately not wired into the synth-matching specs.

## Annual AMWS Callaway–Sant'Anna studies

Run both annual profiles from the repository root:

```powershell
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_annual_csdid.R
```

To select one profile, set `COMPULSORY_AMWS_ANNUAL_PROFILE` to `pre20` or `pre40`.
The default is `both`. Set `TALENT_DETS_DATA_DIR` to override the Dropbox root.

| Profile | Annual event window | Balanced treated sample |
|---|---|---|
| `pre20` | -20 through +20 | 322 counties; 11 states plus DC |
| `pre40` | -40 through +20 | 195 counties; 8 states plus DC |

Targets are jurisdictions with a recorded first compulsory-law year during
1850–1880, inclusive. Each retained target county must have every event year
and valid values for all six outcomes. Other years
are retained for estimation. Later adopters supply controls before exposure;
later early-adopter cohorts can also serve as controls while unexposed. Counties
with no observed unexposed year cannot supply controls and are excluded from
both levels and detrended fits. Missing law dates are excluded, not classified
as never treated. Alaska and Hawaii have explicit missing dates because Table
6.2 excludes them; they are excluded from estimation rather than treated as
never treated.

The AMWS outcomes are indexed by **birth year**; county population and estimated
births are indexed by calendar year. Event time zero is the year the law took
statewide effect for every outcome.
The agreed exposure assumption allows children born up to 14 years before
passage to be exposed: `did::att_gt(anticipation=14, base_period="universal",
control_group="notyettreated")`. The reference birth cohort is **-15**, and
birth cohorts -14 through -1 are shaded as potentially exposed. Consequently,
the two profiles contain **6 and 26 unexposed pre-law years**, respectively,
including the reference. These are not windows of 20 and 40 wholly unexposed
years. The 14-year allowance is a modeling assumption, not a verified statement
about each state's historical school-age rules.

The six outcomes are AMWS counts, log(1+counts), AMWS births per 1,000 population,
AMWS births per 1,000 estimated births, county population, and estimated county
births. Each is estimated in levels and after
subtracting an adoption-year-cohort linear trend fitted only over -pre through
-15. Inference for detrended outcomes is conditional on the estimated trends.
Annual scientist counts use recorded birth years; county population is linearly
interpolated between censuses and county birth denominators are estimated.

Dynamic aggregation includes only retained early-adopter cohorts, with weights
proportional to their county counts. It preserves joint influence functions and
uncertainty in cohort shares. No requested event year is silently reweighted
because one target cohort is missing. Confidence intervals are 95% pointwise,
using a state-clustered multiplier bootstrap with 1,000 draws. Base seed
`20260905` has deterministic profile/model offsets recorded in the exports.

Outputs are in
`results/mandatory_schooling/compulsory_schooling_amws_annual_csdid/`, under
`pre20_post20_a14/` and `pre40_post20_a14/`: pooled and per-cohort PNG/PDF figures,
coefficient CSVs, input/sample/support audits, trend fits, estimator warnings,
direct-DiD validation, and fitted RDS models. Previous decennial outputs are
preserved. Each model RDS includes the complete fit and the target-only
aggregation, so joint inference can be reproduced.

Every pooled and multi-cohort figure has one compact upper-right summary box.
It reports distinct treated counties and states/DC, the fixed later-adopter
control pool, and events counted as distinct treated adoption-year cohorts.
Control eligibility still varies by comparison; the box does not imply a fixed
control group at every event year. Counts are read from the sample audit,
treated-county list, and cohort weights, including when regenerating saved plots.

The supplied calendar panel extends through 1960. The `did` package internally
truncates estimation when no not-yet-exposed comparison remains (1903 in these
runs). `sample_summary.csv` reports supplied rows; `model_metadata.csv` reports
the processed rows and years. Every requested target event year is earlier than
that cutoff. The final `validation_summary.csv` checks sample counts, estimates,
aggregation, and cluster alignment. To verify saved models and regenerate figures
without fitting the models again:

```powershell
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_annual_csdid.R --verify-outputs
```

Run the focused synthetic estimator checks from the repository root:

```powershell
Rscript analysis/mandatory_schooling/test_annual_csdid.R
```

### Population-controlled annual versions

Run both additional controls for both annual profiles:

```powershell
$env:COMPULSORY_AMWS_ANNUAL_CONTROL = "both"
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_annual_csdid.R
```

The control selector also accepts `baseline_log_population`,
`preperiod_population_cagr`, or `none` (the default uncontrolled execution).
The existing `COMPULSORY_AMWS_ANNUAL_PROFILE` selector still applies.
Use `--verify-outputs` with the same selectors to validate saved controlled fits
and regenerate their figures. `--resume` skips completed models only when input
and analysis-code hashes match the current run.

Each control adds all six outcomes in levels and cohort-linear-detrended form
for each profile: 48 additional specifications in total. Results are saved in
the corresponding control-named subdirectory of each existing profile folder.

For a focal law cohort `g`, baseline population is `log(population[g - 15])`.
Population CAGR is `100 * ((population[g - 15] / population[g - pre])^
(1 / (pre - 15)) - 1)`: percent annual growth over 5 years for pre20 and 25 years
for pre40. Both endpoints use the existing interpolated population series.
The two controls enter separate models. Every treated and donor county uses
the **same focal cohort calendar years**; the covariate is fixed across time
within each fit. Pre-event coefficients are conditional comparisons using these
fixed summaries, which include population through the reference year -15.

The controlled versions use C&S **outcome regression** (`est_method="reg"`),
not doubly robust estimation. This retains the single-county DC cohort, whose
treated-group covariate regressions would be singular under the package's
unbalanced-panel doubly robust estimator. Treated samples and county-count
cohort weights remain unchanged. Donors missing the required population
endpoints are excluded separately for each focal cohort. Lingwall statewide-effect law dates,
not-yet-treated eligibility, the 14-year exposure allowance and reference -15
are preserved. Detrending uses the original profile's cohort trend fits before
applying the covariate exclusions.

Cohort-specific estimates are pooled using county-aligned influence functions,
rescaled from each fitted sample to the common original profile county universe.
The aggregation adds cohort-share estimation uncertainty. All cohort and pooled
effects share 1,000 state-level multiplier draws; intervals are 95% pointwise.
Inference remains conditional on fitted detrending paths. Compact fitted RDS
files retain requested cohort effects, county influence functions, ids, sample
normalizations, settings, checks and seeds for reproducible joint inference.

The covariate audit reports source years, population values, control values and
exclusion reasons. Comparison support includes control-regression rank and
treated counties outside the control covariate range. Range extrapolation is
reported, without trimming or silently changing the target estimand. Missing
target effects halt estimation rather than silently reweighting the pooled curve.
The figure box reports the union of eligible later-adopter counties/states
across cohort fits; the comparison-specific control pool can be smaller.

Run the additional estimator checks with:

```powershell
Rscript analysis/mandatory_schooling/test_annual_population_controls.R
```

## Five-year-bin AMWS Callaway–Sant'Anna studies

The same C&S design on five-year bins instead of single years. Annual AMWS
births are sparse — roughly four county-years in five are zero — so the annual
cells are noisy. Binning trades event-time resolution for precision and leaves
the annual pipeline and its results untouched.

Build the panels first, then run from the repository root:

```powershell
Rscript prep/amws/build_amws_combined_county_period5.R
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_period5_csdid.R
```

### Two binning conventions

Both are run by default. They are the decade conventions of the land-grants and
world-fairs papers carried over to five-year blocks, and the rule is applied to
the outcome, its denominators **and** the statewide law year alike:

| Convention | Rule | Bin `B` collects |
|---|---|---|
| `standard` | `5 * floor(y / 5)` | `B, B+1, B+2, B+3, B+4` (leading) |
| `alt` | `5 * floor(y / 5) + 5 * (y %% 5 >= 3)` | `B-2, B-1, B, B+1, B+2` (centred) |

They differ on exactly the years ending 3, 4, 8 and 9. The rule lives once, in
`prep/amws/period5_bins.R`, and is shared by the builder and the runner.

**The convention moves whole states between cohorts, so the two samples are not
nested.** `cohort_bin_mapping.csv` in every output directory records the raw
Lingwall law year and both bins for all 51 jurisdictions. The consequential
moves are:

| State(s) | Law year | `standard` | `alt` |
|---|---:|---:|---:|
| DC | 1864 | 1860 | 1865 |
| NV, WY, CA, KS, NJ | 1873–1874 | 1870 | 1875 |
| WA, WI | 1878–1879 | 1875 | **1880** |
| IL, MT, ND, RI, SD | 1883 | **1880** | 1885 |

The 1883 adopters therefore enter the 1850–1880 target window under `standard`
but not under `alt`, and DC has no cohort of its own under `alt`. Cohort
membership is defined on the **binned** law year, not the raw one: under
`standard` a raw 1881 law would otherwise collide with the 1880 target cohort
while being classified as a later-adopter donor.

### Outcomes are annual averages

`prep/amws/build_amws_combined_county_period5.R` divides each bin total by the
number of contributing years and takes the bin mean of population, so counts
stay on an annual scale and remain comparable with the annual runs. Rates are
recomputed from the aggregated numerator and denominator; a rate is never itself
averaged. Totals reconcile only after reweighting each bin by `n_years`, which
the reader asserts (112,291 AMWS births; 49,587 and 62,704 by edition).

Partial bins are written with `complete_period = FALSE` and dropped by the
reader, so a three-year average is never pooled with five-year ones. Under
`standard` that is only the 1960 bin; under `alt` it is the 1800 and 1960 edges
plus the first bin of any county whose series starts on a decade boundary —
6,213 rows in all, reported as `dropped_partial_bin_rows`.

### The 14-year allowance is unchanged

`did` applies `anticipation` in the units of the time variable, not in period
indices, so the annual value of 14 carries over without modification:

- the universal base period is the latest bin strictly before `g - 14`, which on
  a five-year grid is the bin at **`e = -15`**, exactly as in the annual design;
- a not-yet-treated donor needs `g_donor > max(t_base, t_event) + 14`, and since
  every bin is a multiple of five that means its law bin starts at least 15 years
  after the comparison bin — again the annual condition.

Bins `e = -10` and `e = -5` are inside the anticipation window and are shaded,
not read as placebo leads. The `direct_did_validation.csv` export re-derives
every reported cell with an independent 2×2 unconditional DiD and must agree to
`1e-7`; that check is what pins this mapping, and it fails loudly if the base
period or the donor rule moves.

### Profiles and specifications

| Profile | Event window | Bins | Unexposed bins incl. reference | Specifications |
|---|---|---:|---:|---|
| `pre20` | -20 … +20 | 9 | 2 | levels |
| `pre40` | -40 … +20 | 13 | 6 | levels, detrended |

The cohort-linear trend is fitted only on `e ∈ [-pre, -15]`. In five-year bins
that leaves pre20 with two points, and a two-point line interpolates exactly —
it would force the detrended estimate to zero at both pre-bins rather than
estimate anything. **pre20 therefore runs levels only**, recorded per run in
`specification_status.csv`; the three-point guard in `annual_detrend` is kept.

Balanced target samples, after calibration:

| Convention | pre20 counties / jurisdictions | pre40 counties / jurisdictions |
|---|---:|---:|
| `standard` | 429 / 14 | 287 / 11 |
| `alt` | 310 / 12 | 193 / 9 |

These exceed the annual samples (322 / 12 and 195 / 9) because balance over 9 or
13 bins is a weaker requirement than over 41 or 61 years. Under `alt` the pre40
1880 cohort (WA, WI) drops out entirely: those counties cannot cover `e = -40`,
i.e. the 1840 bin.

### Selectors and outputs

`COMPULSORY_AMWS_PERIOD5_BINNING` accepts `both` (default), `standard` or `alt`;
`COMPULSORY_AMWS_PERIOD5_HORIZON` accepts `both` (default), `post20` or `post30`;
`COMPULSORY_AMWS_PERIOD5_PROFILE` accepts `both`, `pre20` or `pre40`;
`COMPULSORY_AMWS_PERIOD5_CONTROL` accepts `none` (default), `both`,
`baseline_log_population` or `preperiod_population_cagr`. `--verify-outputs`
validates saved fits and regenerates figures without refitting, and `--resume`
skips completed controlled models whose input and code hashes still match.

```powershell
$env:COMPULSORY_AMWS_PERIOD5_CONTROL = "both"
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_period5_csdid.R
Remove-Item Env:COMPULSORY_AMWS_PERIOD5_CONTROL
```

Results are in
`results/mandatory_schooling/compulsory_schooling_amws_period5_csdid/<convention>/<profile>_post<horizon>_ref15/`,
with the controlled variants in a further `baseline_log_population/` or
`preperiod_population_cagr/` subdirectory. Each directory carries the same
exports as the annual runs — pooled and per-cohort figures, coefficient CSVs,
sample and support audits, trend fits, estimator warnings, the direct-DiD
validation and fitted RDS models — plus `cohort_bin_mapping.csv` and
`specification_status.csv`. Each horizon has its own seed base — `20260916` for
post20 and `20260946` for post30, plus one for the controlled runs — with
deterministic convention, profile and model offsets recorded in the exports.

Run the focused checks with:

```powershell
Rscript analysis/mandatory_schooling/test_period5_csdid.R
```

They cover the bin rules, total-preserving annual averaging, the cadence
mapping, the horizon table and its `1905` ceiling, the cap semantics (a capped
cohort must become a donor, not an exclusion), and — on a purpose-built isolated
panel where the ATT is zero at exactly one event time — that `did` places its
base period on `e = -15` under both the annual and the five-year cadence. The
post20 directory suffix and seed base are pinned by the tests, because
renumbering either would orphan or overwrite published results.

### Extended +30 horizon

The default horizon is +20. A second horizon follows the treated cohorts a decade
further, to **+30**, in its own `_post30_ref15` directories. The +20 runs are
untouched.

```powershell
$env:COMPULSORY_AMWS_PERIOD5_HORIZON = "post30"
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_period5_csdid.R
$env:COMPULSORY_AMWS_PERIOD5_CONTROL = "both"
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_period5_csdid.R
Remove-Item Env:COMPULSORY_AMWS_PERIOD5_CONTROL, Env:COMPULSORY_AMWS_PERIOD5_HORIZON
```

`COMPULSORY_AMWS_PERIOD5_HORIZON` accepts `both` (default), `post20` or `post30`,
and composes with the existing binning, profile and control selectors.

#### Why +30 is the limit, and what it costs

**Every state had a compulsory-attendance law by 1920, so the not-yet-treated
donor pool is finite.** A comparison at calendar bin `t` needs a donor whose law
bin starts at least 15 years later, and the latest law bin is 1920 (MS, raw
1920). The last usable comparison bin is therefore **1905** — `did` finds this on
its own and truncates its processed panel there, which `model_metadata.csv`
records as `processed_max_year = 1905`. A cohort `g` can be followed only to
event time `1905 - g`:

| cohort `g` | 1850 | 1860 | 1865 | 1870 | 1875 | 1880 |
|---|---|---|---|---|---|---|
| max event time | +55 | +45 | +40 | +35 | +30 | +25 |

Reaching +30 therefore requires capping the target window at binned law year
**`g <= 1875`**. A capped cohort is **not discarded**: it becomes an eligible
not-yet-treated donor, which is exactly what it is, so the donor pool grows
(1745 to 1852 counties under `standard`). The cap, and the cohorts it moved, are
recorded per run in `sample_summary.csv` as `target_max_cohort` and
`cohorts_excluded_by_cap`.

The balanced-target requirement never binds at longer horizons — `standard/pre20`
retains 429 counties at both +20 and +25. **The whole cost of the longer horizon
is the dropped cohort.**

| | `standard` pre20 / pre40 | `alt` pre20 / pre40 |
|---|---|---|
| +20 (`g <= 1880`) | 429 / 14, 287 / 11 | 310 / 12, 193 / 9 |
| +30 (`g <= 1875`) | 322 / 12, 195 / 9 | 280 / 11, 193 / 9 |

(counties / jurisdictions)

#### The cap costs the two conventions different things

Under `standard` the 1880 bin is exactly the raw-1883 adopters (IL, MT, ND, RI,
SD), which the annual design never treated as targets at all. Capping at 1875
therefore restores the annual paper's own target set — and it does so **county
for county**, not merely in count: the `standard` +30 treated sample is identical
to `pre20_post20_a14` / `pre40_post20_a14` in the annual pipeline (322 and 195
counties, same GEOIDs, same states). The +30 run under `standard` studies exactly
the annual paper's counties, grouped into coarser cohorts and followed a decade
further.

Under `alt` the 1880 bin is WA (1878) and WI (1879), whose raw law years are
*inside* the 1850-1880 window. There the cap is a genuine loss of 30 counties and
one jurisdiction from `pre20`; `alt/pre40` never had an 1880 cohort, so it is
unaffected.

#### The terminal coefficient is the weak point

The last bin of an extended run always lands on calendar 1905, whose donors must
have a law bin of 1920 or later. Under `standard` that is **MS alone — one
control state, 75 counties**. Under `alt` it is four (FL, MS, SC, VA).
`sample_summary.csv` reports this as `terminal_control_states` and
`terminal_control_counties`, and `notes.txt` repeats it. State-clustered
inference at `e = +30` rests on a single cluster under `standard`; read that
coefficient with more caution than the rest of the curve.

#### Why not two decades

+35 and +40 are feasible in the same sense but not worth running. Their caps
(`g <= 1870` and `g <= 1865`) leave 133 / 56 and **29 / 26 counties in three
jurisdictions** (MA, DC, VT) respectively, which is not a credible
state-clustered event study. `PERIOD5_LAST_COMPARISON_BIN` and the
`target_max <= 1905 - post` bound in `period5_profiles_for()` make adding such a
horizon a two-line change if it is ever wanted.

### Shared code

The two cadences share their estimation primitives. `csdid_cadence()` in
`annual_csdid_helpers.R` carries the step, the anticipation allowance, the
resulting reference event, the figure wording and the output-directory suffix;
its defaults are the annual design unchanged. `csdid_figure_helpers.R` holds the
plotting code both runners use. Editing either changes the annual code manifest,
so an annual `--resume` will refit rather than reuse saved controlled models;
the saved results themselves are unaffected, and
`analysis_compulsory_schooling_amws_annual_csdid.R --verify-outputs` reproduces
the annual coefficient exports byte for byte.

## Exporting figures to the paper

`project/mandatory_schooling.tex` is a **standalone figure volume** — 56 full-page
figures, no captions, compiled on its own:

```powershell
Rscript analysis/mandatory_schooling/export_mandatory_schooling_project_artifacts.R
cd project; pdflatex mandatory_schooling.tex   # 56 pages
```

It is not part of `main.tex`, and the figure paths are relative, so `pdflatex`
must run from `project/`.

### Why the export script exists

The volume's figures were originally hand-copied from the results tree, which
flattened the results directory structure into the filename by a rule that lived
nowhere in code. **Re-running the analysis therefore left the volume silently
stale**, and it did: the committed C&S figures were byte-identical to the
pre-Lingwall vintage in
`results/mandatory_schooling/compulsory_schooling_amws_annual_csdid_legacy_pre_lingwall/`
long after the Lingwall (2014) law dates became canonical.

The script closes that gap. It **copies** the PDFs the analysis already renders —
it does not re-plot, because these figures carry a summary header box that
`csdid_figure_helpers.R` builds — and refuses to run unless every source checks out.

### What it manages

48 figures, from a single data-driven spec table:

| kind | source under `results/mandatory_schooling/` | destination in `project/figures/` |
|---|---|---|
| annual | `compulsory_schooling_amws_annual_csdid/<profile>_post20_a14/[<control>/]pooled_<outcome>.pdf` | `mandatory_schooling_csdid_<profile>_<outcome>_<variant>.pdf` |
| five-year | `compulsory_schooling_amws_period5_csdid/<conv>/<profile>_post30_ref15/[<control>/]pooled_<outcome>.pdf` | `mandatory_schooling_csdid_5yr_<conv>_<profile>_post30_<outcome>_<variant>.pdf` |

`variant` is `unadjusted` for the run in the profile root, and the control
directory name otherwise. Annual covers all six outcomes × two profiles × three
variants (36); five-year covers `amws_per_1000_births` × two conventions × two
profiles × three variants (12).

### Four guards, all before anything is copied

1. Every source path exists.
2. **The Lingwall guard** — each source run's `notes.txt` must cite
   `Lingwall (2014)`. This is the check that catches exporting a superseded
   vintage, and it is worth keeping: pointed at the legacy tree it aborts with
   the offending directories named.
3. Each run's `validation_summary.csv` reports `validation_passed == TRUE`.
4. **Tex cross-check** — every `\figurepage{figures/...}` path in the volume must
   either be exported by this script or appear in its `UNMANAGED_FIGURES` vector.
   A figure referenced but not exported, or exported but not referenced, stops the
   run, so the volume and the script cannot drift apart.

It then writes `project/figures/mandatory_schooling_figure_manifest.csv`
(destination, kind, source, source md5, source mtime, export time) and deletes any
superseded managed figure it no longer emits. Re-running is idempotent.

### What it does not manage

The **eight synthetic-DiD figures** are declared unmanaged in `UNMANAGED_FIGURES`
and left untouched. Their vintage is unresolved: the synth-matching and
state-placebo trees carry no Lingwall attribution, and the committed PDFs no longer
match the obvious current source
(`synth_matching/pre40/amws_per_1000_births/pooled_comparison.pdf` has a different
checksum). They are still the hand-copied files. Resolving them means tracing each
back to its source run and confirming which law dates it used.

## Synthetic-control matching extension

The county synthetic-control workflow has separate `original` and `richer`
matching specifications for the annual `pre20` and `pre40` profiles. Existing
C&S outputs are unaffected. Run from any working directory:

```powershell
Rscript prep/mandatory_schooling/download_matching_covariates.R
# Re-run the download command if the saved NHGIS extract is still processing.
Rscript prep/mandatory_schooling/build_matching_covariates.R
Rscript analysis/mandatory_schooling/test_annual_synth_matching.R
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_synth_matching.R
Rscript analysis/mandatory_schooling/validate_annual_synth_matching.R
```

The downloader uses `IPUMS_API_KEY` or the existing `IPUMS_KEY`. It reuses its
saved extract request and downloads county tables from `1850_cPAX`, `1860_cPAX`,
`1850_cAg`, and `1860_cAg`, plus the Historical Census of Manufactures
county-industry tabulations. Raw data are stored separately in
`raw/mandatory_schooling_matching`; constructed inputs and provenance audits
are in `output/mandatory_schooling/matching`.

### Four added matching variables

| Variable | Construction |
|---|---|
| Manufacturing intensity | `log1p(manufacturing output / census population)`; same census year; non-manufacturing industries excluded |
| Agricultural intensity | `log1p(improved farmland acres / census population)`; **land use, not output or farm property value** |
| Illiteracy | 1850 `AEC001` divided by white plus free nonwhite adults in the published 20+ age bins; excludes enslaved persons and counties with unknown free-population ages |
| Foreign-born share | Foreign-born persons divided by free population; 1850 `AEM002/(AE6001+AE6002)` and 1860 `AH6002/AH7001` |

The literacy numerator is historically described as adults/over twenty; the
available published age bins begin at 20. This is an adult literacy measure
using those census bins, not a total-population rate or a modern age-15+ rate.
The 1860 NHGIS county collection does not contain the needed literacy count;
1850 literacy is therefore retained for later cohorts. Unknown-age counts are
retained in the panel and required to equal zero before accepting the literacy
denominator. This conservative requirement materially reduces the sample.

For each focal law cohort and variable, choose the latest nationally available
source year at or before `g-15`, before county-level filtering. Treated counties,
donors, and local placebos use the same year and universe. There is no
county-specific older-year fallback, post-exposure interpolation, imputation,
or silent removal of a missing matching variable. Massachusetts and DC have
cutoffs before 1850 and cannot enter the richer specification.

Manufacturing uses the CMF 1850 broad groups except `not mfg`; 1860 uses broad
manufacturing categories, excluding the aggregate `TOTAL` that can include
mining and fisheries. Missing source components remain missing in our sums.
**Source limitation:** the 1850 CMF reconstruction lacks Georgia and Louisiana
and its authors imputed zero for unreadable digits. Those upstream imputations
cannot be recovered from the published tabulations and are explicitly recorded
in the manufacturing construction metadata. Missing GA/LA counties are never
filled with zeros. Fractional historical CMF county identifiers are excluded,
not truncated into modern FIPS identifiers.

NHGIS geography uses the repository's nominal GISJOIN-to-FIPS crosswalk.
Unmatched/ambiguous joins are audited and excluded. This does not harmonize
historical county boundaries. Density uses the existing 2020 Census county
land areas, preserving the panel's old Connecticut county identifiers.

### Matching, estimation, and diagnostics

Original features are three nonoverlapping means spanning all clean AMWS
pre-exposure years, baseline log population, log population growth, and log
density. Outcome-bin means use `log1p`, except the already logged outcome.
The richer specification appends the four variables above. These are
standardized with eligible-donor means and standard deviations and enter an
equal-weight Euclidean distance. Select the nearest 30 counties, breaking ties
by county id. Constant donor features are recorded; a different treated value
causes an explicit support failure. Missing target covariates exclude the
target only from the affected specification. Range extrapolation is reported
in the balance audit, not hidden by automatic donor replacement.

Targets retain the existing balanced annual windows, law years 1850–1880,
and `-15` reference. Donors must have the full outcome window and a law year
strictly later than `focal_g+34`; same-state donors are excluded. Synthetic-DiD
unit weights are fitted using all 6 (`pre20`) or 26 (`pre40`) clean pre-exposure
years. The potentially exposed window starts at `-14`. Saved SDID ATTs cover
`-14..20`; the separately saved event effect averages reference-normalized
gaps over law-relative years `0..20`. A numerical noise floor is pre-data-only.

Each selected donor becomes a local pseudo-treated event and is rematched
against the broad eligible pool, excluding its entire state. Identical timing,
missingness and matching rules apply. Repeated identical placebo fits are
reused within a cohort/specification/outcome. Every requested placebo retains
an estimated or failed status. Zero pre-RMSPE events have an undefined
standardized effect rather than an infinite test statistic.

**Inference limitation:** these local county placebo tail ranks are diagnostics,
not state-randomization p-values. Pooled paths are descriptive county-weighted
means, with no independent-county standard errors or pooled p-values. Joint
state-level placebo inference remains a separate methodological extension.
No pre-fit trimming is applied in this matching extension.

Results are saved under `results/mandatory_schooling/synth_matching`, separately
by profile and outcome. Outputs include:

- Cohort features, census vintages, geography/eligibility audits, and sample counts.
- Event paths, donor weights, covariate balance before/after selection and after weighting.
- Thirty-placebo diagnostics and per-event RDS records containing placebo paths,
  weights, balance and source signatures.
- A multipage raw-fit PDF with one page per fitted county/specification and a
  compact treated/control county/state count box.
- Pooled CSV/PDF comparisons: original full sample, original common sample,
  and richer common sample. The last two contain exactly the same treated counties.

`MS_MATCHING_ONLY=true` runs the coverage/matching audit without estimation.
`MS_SYNTH_PROFILES` and `MS_SYNTH_OUTCOMES` accept comma-separated subsets;
defaults run both profiles and all four annual outcomes. `MS_SYNTH_PLACEBOS`
defaults to 30; smaller pilot runs go to separate `pilot_placebosN` directories.
Event caches are reused only when code/input signatures and placebo counts
match. Data remain in Dropbox; no downloaded data are added to git.

Sources: [IPUMS NHGIS](https://www.nhgis.org/overview-nhgis-datasets),
[Historical Census of Manufactures](https://cmfdata.org/county_ind_tab), and
the [1850 Census questionnaire](https://www.census.gov/programs-surveys/decennial-census/technical-documentation/questionnaires.1850_Census.html).

Production matching sample (all four outcomes):

| Profile | Original counties / jurisdictions | Richer counties / states |
|---|---:|---:|
| pre20 | 322 / 12 | 113 / 10 |
| pre40 | 195 / 9 | 52 / 6 |

Original jurisdiction counts include DC. The richer sample excludes MA and DC
because their cutoffs precede the available covariates. Most further exclusions
reflect the strict adult-literacy denominator requirement; some counties also
lack manufacturing data. Six additional pre20 targets fail the constant-donor
outcome-feature support check. See the county eligibility and model-status
audits for the complete accounting.

### Attributes-only donor selection

Set `MS_SYNTH_MATCHING_SET=attributes_only` to select the 30 donors using only
log population, annualized log population growth, log population density,
manufacturing intensity, agricultural intensity, illiteracy, and foreign-born
share. AMWS lag bins are excluded from matching distances, feature-completeness
checks, and constant-feature support checks. The balanced outcome panel and all
timing, geographic exclusion, covariate transformation and vintage rules stay
the same. Donor lists are identical across the four outcomes for a given target
and profile.

**Pre-period AMWS still determines the synthetic-DiD weights and regularization.**
This is an alternative donor-selection specification, not an attribute-only
weighting estimator. Placebos use the same seven-variable rematching rule.

PowerShell, from the repository root:

```powershell
$env:MS_SYNTH_MATCHING_SET = 'attributes_only'
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_synth_matching.R
Rscript analysis/mandatory_schooling/validate_annual_synth_matching.R
Rscript analysis/mandatory_schooling/compare_annual_synth_attributes.R
Remove-Item Env:MS_SYNTH_MATCHING_SET
```

The default without this flag remains the original/richer pair. The new run
has an independent output and cache namespace:
`results/mandatory_schooling/synth_matching_attributes_only`.
It retains **119 counties in 10 states for pre20**, including the six targets
previously excluded by constant AMWS donor features, and **52 counties in six
states for pre40**. Each of four outcomes has 30 local placebo attempts per fit.

The comparison script reads the existing richer artifacts without modifying
them. It checks that the data hashes agree while preserving historical code
hashes. Its paired samples contain 113 counties for pre20 and 52 for pre40;
the full attributes-only sample is reported separately. Run the existing default
pipeline first if richer artifacts are absent on a new machine.

Additional outputs in the new namespace:

- `comparison_summary.csv` and `comparison_paired_summary.csv`: attribute
  distances/balance, pre-RMSPE, donor activity, zero donor-year cells, support
  for positive treated years, donor overlap, and improved/worsened fit counts.
  Both the full diagnostic population and nonzero-pre-outcome subgroup are shown.
- Per profile/outcome, `comparison_event_diagnostics.csv`,
  `comparison_paired_counties.csv`, and `newly_eligible_counties.csv` provide the
  county-level accounting.
- `pooled_richer_comparison.csv` / `.pdf` compare richer-common,
  attributes-only-common, and attributes-only-full descriptive paths.
- `validation.csv`, `comparison_validation.csv`, and provenance manifests record
  validation and the historical baseline version. The validator checks actual
  and placebo rematching and exact cross-outcome donor-list identity.

Distances and balance for both specifications use the same eligible donor pool
and the same seven attribute standard deviations. Nonzero-pre restrictions are
diagnostic summaries only; they do not trim the main fits or pooled samples.
Local placebo ranks remain diagnostics, not state-randomization p-values.

### Joint state-level placebos

The state-placebo extension assigns the observed law-date schedule jointly to
distinct eligible control states. It covers richer and attributes-only matching,
both annual windows, and all four outcomes. Earlier county placebos remain
historical diagnostics; they do not reproduce state-level law assignment.

Each assignment excludes every placebo-assigned state from the relevant donor
pools and rebuilds county eligibility and nearest-30 matching. Actual law dates
must remain later than the focal year plus 34. All eligible, successfully matched
counties within each assigned state contribute; paired comparisons use their
intersection across the two matching specifications. AMWS history remains in
SDID weighting and regularization. No additional pre-fit trimming is applied.

County paths are averaged within states, then pooled using the corresponding
observed treated state's fixed county share. Common-sample and attributes-only
full-sample weights are kept separate. The observed aggregation reproduces the
existing county-weighted estimates.

The primary statistic is the mean pooled normalized gap over event years 0–20,
with reference −15. Its two-sided state-placebo tail rank is
`(1 + number of equally or more extreme draws) / (1 + number of draws)`.
Zero-pre-RMSPE cases remain in this calculation. The pointwise 95% placebo
envelope is a benchmark distribution, **not a confidence interval**. Inference
is conditional on the specified state-assignment benchmark.

Run from the repository root:

```powershell
Rscript analysis/mandatory_schooling/test_state_placebos.R
Rscript analysis/mandatory_schooling/prepare_compulsory_schooling_state_placebos.R
Rscript analysis/mandatory_schooling/estimate_compulsory_schooling_state_placebos.R
Rscript analysis/mandatory_schooling/report_compulsory_schooling_state_placebos.R
Rscript analysis/mandatory_schooling/validate_compulsory_schooling_state_placebos.R
Rscript analysis/mandatory_schooling/audit_state_placebo_design.R
```

`MS_STATE_PROFILES` accepts `pre20,pre40`; `MS_STATE_OUTCOMES` can select outcomes
for estimation/reporting/validation. Preparation always checks support jointly
across all four outcomes. Production defaults use seed `20260906`, sample 999
unique admissible pre20 assignments uniformly, and exhaust pre40 assignments.
The sampler counts feasible completions rather than greedily assigning states.
Rejections depend only on matching support, never on post-treatment effects;
numerical estimation errors stop execution rather than triggering redraws.

`MS_STATE_PREP_WORKERS` controls parallel matching workers (default four).
`MS_STATE_COHORTS` optionally selects comma-separated law years for an estimation
worker; workers for distinct cohorts may run concurrently. Cohort workers save
separate audit/summary CSVs and share identical provenance and individual-context
fit caches. Archived preparation/estimator sources preserve the version used
for a completed run when orchestration code subsequently changes.

Structural support is limited: pre20 has 99,792 feasible assignments from 14
candidate states; pre40 has only 54 assignments, spanning 21 distinct selected
state sets. Every pre40 assignment replaces Ohio with Georgia and New Jersey
with Louisiana. Matching-support checks may reduce these counts further.

Results live separately under `results/mandatory_schooling/synth_state_placebos`:

- Profile-level assignment schedules, rejection audits, state weights, candidate
  coverage, matching records, and preparation provenance.
- Outcome-level resumable county-fit records containing raw paths and donor
  weights, with county fit diagnostics and estimator provenance.
- Complete state paths, county membership, draw-to-state links, pooled placebo
  paths, scalar statistics, tail ranks, and pointwise envelopes.
- `pooled_state_placebos.pdf`, `state_placebo_distribution.pdf`, and representative
  state-average raw-fit books. Every county fit remains available in the RDS
  records; observed county raw-fit books remain in their original directories.
- Validation checks assignment eligibility and uniqueness, excluded-state donor
  rules, matching equivalence, source-outcome path reconstruction, state means,
  fixed-weight pooling, and the final tail-rank calculation.

## 1940 full-count census microdata

`EXT1940USCB.dat.gz` (7.44 GB gzipped, ~41 GB uncompressed) is the IPUMS USA
v8.0 extract of the 1940 census prepared for the Census Bureau Disclosure
Avoidance System test data — the **complete count**, 100 % density, `PERWT` = 1.00
on every record. It lives in Dropbox at
`Data/raw/mandatory_schooling/EXT1940USCB.dat.gz`.

It is a hierarchical fixed-width file: each household record (`RECTYPE` = `H`,
159 chars) is followed by its person records (`RECTYPE` = `P`, 273 chars). **It
ships with no codebook.** The field layout is checked in here as
`ext1940uscb_layout.csv`, transcribed from IPUMS' published command files for
this exact extract — [`EXT1940USCB.sps`](https://usa.ipums.org/usa/resources/1940CensusDASTestData/EXT1940USCB.sps)
and [`EXT1940USCB.cbk`](https://usa.ipums.org/usa/resources/1940CensusDASTestData/EXT1940USCB.cbk),
which agree field-for-field — and re-verified against the bytes. 53 household
fields, 102 person fields.

### Converting to Parquet

```powershell
python prep/mandatory_schooling/test_ext1940_parser.py           # seconds
python prep/mandatory_schooling/convert_ipums_1940_fullcount.py  # ~25 min
python prep/mandatory_schooling/verify_ipums_1940_parquet.py     # ~5 min
```

The converter streams the gzip once and writes a Hive-partitioned tree, by
default to `D:\ipums_1940_fullcount` (override with `IPUMS_1940_PARQUET_DIR`).
This is the one dataset in the project that does **not** live in Dropbox: at
~6 GB it is too large, and it is reproducible from the Dropbox source in 25
minutes. Read it with the same `arrow` idiom the rest of the prep pipeline uses:

```r
library(arrow)
person <- open_dataset("D:/ipums_1940_fullcount/person")
person |>
  filter(hh_STATEFIP == 1L, birthyr_approx %in% 1880:1900) |>
  select(county_geoid, BPL, birthyr_approx, SCHOOL, HIGRADE, EDUC) |>
  collect()
```

On first run the converter downloads IPUMS' `.sps`/`.cbk` into
`<root>/_docs/` and **asserts that the checked-in layout matches the `.sps`
field-for-field**. A silent off-by-one in the layout is the one error that would
corrupt every column without raising anything, so it gets its own check; the
result is recorded as `layout_cross_checked_against_sps` in `_manifest.json`.

### What the person table contains

All 102 IPUMS person fields keep their **raw codes** — 9-fills (`999`, `9999`)
are *not* converted to nulls, because the right recoding differs by variable and
is a downstream decision. Implied decimals are likewise not applied: `HHWT`,
`PERWT` and `SLWT` carry 2, `CPI99` carries 3, and `PRESGL`, `ERSCOR50`,
`EDSCOR50`, `NPBOSS50` carry 1. They are listed in `_manifest.json`.

Beyond the IPUMS fields, each person row carries its household's geography, so
the person table is usable without a join:

| Added column | Meaning |
|---|---|
| `hh_REGION`, `hh_STATEICP`, `hh_STATEFIP`, `hh_COUNTY`, `hh_URBAN`, `hh_METRO`, `hh_GQ`, `hh_FARM` | copied from the person's household record |
| `county_geoid` | derived: 5-char FIPS GEOID, null where the county is not identified |
| `birthyr_approx` | derived: `1940 - AGE`, null where `AGE` is the 999 missing code |
| `statefip` | Hive partition column |

**`COUNTY` is the FIPS county code multiplied by 10** (Alabama: 0010, 0030,
0050…), which is where `county_geoid` comes from. `MIGCOUNTY` — county of
residence in 1935 — uses the same coding.

`birthyr_approx` is approximate by ±1 year and cannot be sharpened. Enumeration
was 1 April 1940, so a person aged 30 was born between 2 April 1909 and 1 April
1910; anyone whose birthday fell between 2 April and 31 December was in fact
born the year before `1940 - AGE`.

### No date of birth, and no county of birth

This is a property of the 1940 census, not of the extract, and it constrains any
design built on this file:

- **No date of birth.** The schedule asked age, not DOB. The file has `AGE`
  (cols 74–76) and `AGEMONTH` (77–78) and no `BIRTHYR`/`BIRTHQTR`.
- **No county of birth.** Birthplace was recorded at state/country level only:
  `BPL`/`BPLD` for the person, `MBPL`/`FBPL` for the parents. There is no
  county-level birth geography anywhere in the 1940 census.

`county_geoid` is county of **residence in 1940**; `MIGCOUNTY` is county of
residence in **1935**. Neither is a birth county, and for cohorts born decades
earlier the gap between them is exactly the selective internal migration a
county-of-birth design would be trying to avoid.

Recovering a birth or childhood county requires linking on `HISTID` (cols
235–270, a UUID unique to the person) to an earlier full count — 1910, 1920 or
1930 — where the same person appears as a child in their county of upbringing.
Those files are not in this project and would have to be obtained from IPUMS
separately. Scope the match rates before committing to such a design.

### Using it for compulsory schooling

The standard design this file supports is **state of birth × birth cohort**:
`BPL` gives state of birth, `birthyr_approx` gives the cohort, and Lingwall's
state law dates give exposure. Outcomes are `SCHOOL` (attendance),
`HIGRADE`/`HIGRADED` (highest grade completed) and `EDUC`/`EDUCD`.

This is a different unit from the rest of the mandatory-schooling pipeline,
which is county-decade on NHGIS aggregates. Individual-level state-of-birth
exposure is not contaminated by migration in the way a county-of-residence
measure is, so it is a genuine complement to the existing county panel rather
than a finer-grained version of it.

### Age heaping

`birthyr_approx` inherits whatever rounding respondents applied to `AGE`. Over
ages 20–79 in the converted data, ages ending in 0 take 12.40 % of the mass
against 10.0 % under no heaping, and ages ending in 5 take 10.49 %; the Whipple
index over ages 23–62 is **109.6**, which is the UN's "relatively accurate"
band (100 = none, >175 = rough). So heaping is real but mild.

It still matters for a birth-cohort design, because it is concentrated exactly
on the round birth years that a law-adoption cutoff is likely to fall near. If a
compulsory-schooling cutoff lands on a cohort ending in 0 or 5, the treated and
control cells are not the same size for a substantive reason. Prefer multi-year
cohort bins over single birth years, and check that results survive shifting the
bin edges off round years.

### Verification run

`verify_ipums_1940_parquet.py` on the full conversion (2026-09-17):

- 132,404,766 persons in 38,492,963 households; `sum(NUMPREC)` equals the person
  count exactly, and `NUMPREC == NUMPERHH` on every household.
- Total is +240,197 (+0.18 %) above the published 1940 resident population of
  132,164,569, spread evenly: every state lands within **0.48 %** of its
  published figure. IPUMS' universe differs slightly from the published
  tabulation, so this is reported, not asserted.
- 3,105 distinct `county_geoid`; 0.02 % of persons have no identified county.
  153 distinct `BPL` codes. No missing ages; `AGE` max 120.
- All 132.4 M `HISTID` values unique (no 64-bit hash collision).
- Alaska (72,665 persons — the whole partition) re-parsed from the source and
  compared field-for-field against the Parquet, including `HISTID`: identical.

Conversion takes ~20 min and produces 6.2 GB (5.5 GB person, 706 MB household).

## CLS detailed compulsory-schooling laws (Clay–Lingwall–Stephens)

The pipeline above codes compulsory schooling as **one adoption date per state**
(`input/compulsory_schooling_laws.csv`, Lingwall 2014 Table 6.2). That is the
extensive margin only, and the extensive margin turns out to be the weak part of
the variation. This section covers the intensive margin.

Source: Clay, Lingwall & Stephens Jr (2021), *Laws, educational outcomes, and
returns to schooling: evidence from the first wave of U.S. state compulsory
attendance laws*, **Labour Economics 68, 101935**. Replication package ICPSR
145122 v1.1, in Dropbox as `Data/raw/mandatory_schooling/replication_CLS.zip`,
extracted alongside it as `ICPSR_145122-V1.1/`.

```powershell
Rscript prep/mandatory_schooling/build_cls_schooling_laws.R
Rscript prep/mandatory_schooling/test_cls_schooling_laws.R   # expect: All checks passed.
```

Outputs land in `output/mandatory_schooling/cls/`, each as `.csv` and `.parquet`:

| File | Grain | Rows |
|---|---|---|
| `cls_state_age_limits_1880_1930` | state × year, 1880–1930 | 2,499 |
| `cls_cohort_requirements_1875_1912` | **`bpl` × `yob`** | 1,862 |
| `cls_law_onset_by_state` | state | 49 |

### What the law panel holds

| Variable | Definition (CLS Data Appendix, verbatim) |
|---|---|
| `entryage` | The age at which children had to enter school |
| `exitage` | The age at which all children could stop attending school |
| `earlyyrs` | The years of completed schooling that allowed an exemption to `exitage` |
| `earlyyrs_condition` | The earliest age at which an `earlyyrs` exemption could be used |
| `workage` | The age at which employed children could stop attending school |
| `workyrs` | The schooling required for a `workage` exception to apply |
| `contage` | The age working children stopped attending continuation school |
| `contyrs` | The schooling required to be exempt from continuation school |
| `reference_agelimit` | Session-law citation, e.g. `1915 Ala. Acts 534` (220 of 2,499 rows) |
| `reference_courseofstudy` | Source for typical primary-course length (32 rows) |

The build adds `*_decoded` / `*_code_type` siblings, `has_law` / `has_workage` /
`has_contage`, `law_in_force` / `law_spell_id`, the two exemption routes
`work_exit_age` / `early_exit_age` with `binding_exemption` naming which governs,
and `effective_leaving_age` / `required_window_nominal` / `required_window_effective`.

`binding_exemption` over the 1,808 law state-years: **none 45.3%** (the stated
exit age binds), **work_permit 28.7%**, **early_dropout 21.7%**, **both 4.3%** —
so an exemption opened an earlier route in 989 of them.

### Three traps, all handled in the build

**`0` means "no law", not missing.** Every cell is populated. A naive
`mean(entryage)` averages in the states that had no law. Use `has_law`.

**Sentinel codes.** `99` means the law required *literacy* rather than a year
count; `99X` means "completion of primary school" where X is the typical
primary-course length. The build writes `*_decoded` (99→4, 996→6, 997→7, 998→8,
9910→10) and keeps the originals, because **99 → 4 is CLS's assumption, not a
measurement**. `*_code_type` marks which construct each row holds. CLS's own
do-file overwrites in place; this build deliberately does not.
Genuine 12-year requirements exist (Nebraska `earlyyrs`, California `contyrs`),
so a decoded value of 12 is real, not a leaked sentinel.

**Arizona and Michigan repealed.** Arizona has a law 1880–1882, **none
1883–1898**, then 1899–1930; Michigan has a one-year gap. A monotone "treated
from year g" coding gets Arizona wrong for 19 years. Use `law_in_force` /
`law_spell_id`. No other state has a gap.

### The policy timeline this reveals

Adoption is **bimodal** — median 1889, IQR 1875–1907. 23 states (47%) adopt in
the 1870s–80s; then a trickle; then 10 more in the 1910s, all Southern.
**16 states first adopt in 1900 or later, 11 of them the entire CLS-coded South**
(1913–1920, eight of those in 1916–1920).

Then four stages, which map exactly onto the `ca_years*` construction:

1. **1870s–80s — adoption.** Entry ~8, exit ~14. (`ca_years1`)
2. **1900s–10s — exit ages raised.** 38 states raised theirs, 59 upward changes,
   median 1911, mean 14.25 → 16.20. Meanwhile **entry age is near-static: 24 of
   49 states never changed it and no state ever raised it** (mean 7.94 → 7.31).
3. **Simultaneously — exemptions give half of it back.** (`ca_years2`, `ca_years3`)
   Nominal required window 6.31 → 8.90 years; **effective window only
   6.31 → 7.59**. By 1930 an exemption opened a route out before the stated exit
   age in **89.8%** of states with a law, and the effective leaving age had
   barely moved off 14 since 1880 (14.25 → 14.90). Two routes are at work: the
   child-labour work permit (`workage`/`workyrs`) and the early-dropout
   exemption (`earlyyrs`/`earlyyrs_condition`), the latter being the more common
   of the two.
4. **1917 onward — continuation school claws part of it back.** (`ca_years4`)
   3 states in 1910 → 25 by 1920 → 29 by 1930, mean `contage` ~16.9.

This is the arithmetic behind CLS's own line that *"the laws were unlikely to
have been binding for most children"* — and the reason to use
`ca_years_combined` rather than raw age limits. Coding nominal limits alone
would record a +2.6-year tightening where the real one was +1.4.

### Cohort requirements

`ca_years*` are built by CLS's `cohort_requirements_oct_2016.do`, which walks
each state × cohort through ages 5–17 and looks up the law in force in
`yob + age`:

| Variable | Counts |
|---|---|
| `ca_years1` | age limits only, no exemptions |
| `ca_years2` | minus years lost once the **work-permit** exemption binds |
| `ca_years3` | minus years lost once the **early-dropout** exemption binds |
| `ca_years4` | **additional** continuation-school years (22 states ever > 0) |
| `ca_years` | `min(ca_years2, ca_years3)` — regular school |
| `ca_years5` | `ca_years2 + ca_years4` |
| **`ca_years_combined`** | `min(ca_years3, ca_years5)` — **headline** |

The exemptions are **not controls** in the paper. They are stopping rules baked
into the treatment, which then enters non-parametrically as bins
(`0 / 1-5 / 6 / 7 / 8 / 9+`, zero omitted) and instruments for schooling in a
wage equation, with `i.bpl i.region#i.yob` fixed effects clustered on
`yob × bpl`.

### CLS design facts worth not re-deriving

- **The paper uses no record linkage.** Treatment is assigned on *state of
  birth* × year of birth, both recorded in the 1940 census itself. That is why
  they get a ~19 M sample with no crosswalk and no match-rate selection. Any
  county-level design introduces linkage selection that this paper never had.
- **`yob = 1940 - age - 1`**, not `1940 - age`. Enumeration was 1 April 1940, so
  most people had not yet had their birthday. This differs from `birthyr_approx`
  in the 1940 parquet — **joining `ca_years_combined` on `birthyr_approx` is off
  by one for ~75% of people.**
- **Sample:** native-born white men aged 27–54 in 1940, i.e. **yob 1885–1912**.
  The cohort file's 1875 start is wider than the estimation sample.
- **There is no "years of schooling" variable.** CLS construct it from
  `HIGRADED`: divide by 10, then `0` if ≤3, `g-3` if 4–20, `17` if 21–23, and
  drop `999`. Pre-1940 censuses have no attainment variable at all — `SCHOOL`
  (attendance) runs back to 1850, but `HIGRADE` starts in 1940.

### Against the project's Lingwall dates

15 states are **left-censored** (law predates the 1880 panel start). Of the 34
comparable states, **32 match Lingwall Table 6.2 exactly**. Two disagree:

- **Arizona** — CLS show a law in force 1880–1882 (the 1875 statute, repealed
  1883); Lingwall's 1899 is the re-implementation date.
- **Washington** — Lingwall 1878, CLS 1886. The 1877 law "did not apply to small
  localities"; 1886 is when it applied statewide. WA is already a state that
  moves between cohorts under the two five-year binning conventions.

### Joining to the 1940 full count

```r
laws <- arrow::read_parquet(file.path(DATA_OUTPUT, "mandatory_schooling/cls",
                                      "cls_cohort_requirements_1875_1912.parquet"))
arrow::open_dataset("D:/ipums_1940_fullcount/person") |>
  dplyr::filter(AGE >= 27L, AGE <= 54L, HIGRADED != 999L) |>
  dplyr::mutate(yob = 1940L - AGE - 1L,
                g   = HIGRADED %/% 10L,
                yrs_sch = dplyr::case_when(g <= 3L ~ 0L, g <= 20L ~ g - 3L,
                                           g <= 23L ~ 17L)) |>
  dplyr::inner_join(laws, by = c("BPL" = "bpl", "yob"))
```

Note `yob`, not `birthyr_approx`.

Verified: 1,372 cells (49 states × 28 cohorts), 21.4 M men aged 27–54. Raw mean
years of schooling by required-years bin — 0: 7.04, 1–5: 7.99, 6: 8.80, 7: 9.18,
8: 9.74, 9+: 10.27. That raw gradient is **not** the effect; CLS's estimates net
of state and region×cohort fixed effects run 0.041–0.244 years.

### Policy-environment descriptive tables

```powershell
Rscript analysis/mandatory_schooling/describe_cls_policy_environment.R
```

Writes to `results/mandatory_schooling/cls_policy_descriptives/` — five tidy CSVs,
six `booktabs` `.tex` fragments (one per panel plus a combined
`cls_policy_environment.tex`), and the four sidecars this pipeline expects
(`notes.txt`, `session_info.txt`, `input_manifest.csv`, `validation_summary.csv`).

| Panel | Content |
|---|---|
| **A** | Adoption of the first law, by decade, with the Southern share |
| **B** | Entry / exit / effective leaving age, work-permit escape rate, nominal vs effective required window, continuation-school diffusion, 1880–1930 |
| **C** | Within-state changes while a law was in force — entry age vs exit age |
| **D** | Required years facing the CLS cohorts, **person-weighted**, against CLS Table 1; plus mean requirement by cohort decade × region |
| **E** | How the requirement was expressed — plain years vs literacy vs primary-school completion |

Panels B, C and E are restricted to state-years with `law_in_force`, so the
Arizona and Michigan repeal gaps do not register as age changes.

**Panel D is also an end-to-end regression guard.** Weighting the requirement
distribution by the CLS sample — native-born white men aged 27–54 in 1940, i.e.
`BPL <= 56, RACE == 1, SEX == 1, HIGRADED != 999`, `yob = 1940 - AGE - 1` —
reproduces the paper's published Table 1 to **0.0 pp on every bin**
(13.5 / 9.2 / 19.1 / 16.9 / 33.4 / 7.9) with mean age 38.93 against their 38.9.
The script `stopifnot()`s a 0.1 pp tolerance, so it fails loudly if the 1940
conversion, the `yob` convention or the `HIGRADED` recode ever drifts. Our N is
19,185,307 against their 19,055,381 (+0.7%), consistent with the IPUMS full count
running ~0.2% above the published 1940 population.

Two paper artifacts are generated from these CSVs by
`analysis/mandatory_schooling/export_cls_policy_table_to_project.R`:

| Path | What |
|---|---|
| `project/tables/mandatory_schooling_laws.tex` | 2-panel `booktabs` float (age limits, within-state changes), ready to `\input` |
| `project/mandatory_schooling_laws_stats.tex` | standalone 5-panel volume; compile directly, **run pdflatex twice** for the cross-references |

The exporter applies the same guards the figure exporter does — sources present,
`notes.txt` carrying the Lingwall (2014) attribution, `validation_passed` all TRUE —
plus assertions on the specific numbers each artifact publishes, so a changed
upstream run fails the export rather than silently shipping different figures.

The `.tex` fragments in the results tree are bare `tabular` environments with no float wrapper —
wrap them in `\begin{table}` yourself. Verified to compile under `booktabs` with
zero overfull boxes. They are **not** wired into `project/`: that needs a separate
export script carrying the Lingwall-attribution and `validation_passed` guards
that `export_mandatory_schooling_project_artifacts.R` applies to figures, and the
`\input` would go in `project/main.tex` rather than the figures-only
`mandatory_schooling.tex`.

Caveat worth repeating from `notes.txt`: `effective_leaving_age` is the earliest
age at which **any** statutory route out opens, with both the age floor and the
schooling condition enforced for each route — `max(workage, entryage + workyrs)`
for the work permit, `max(earlyyrs_condition, entryage + earlyyrs)` for early
dropout, then the minimum against `exitage`. Accumulated schooling at age *a* is
taken as `a - entryage`, i.e. continuous attendance from the entry age, which is
CLS's own idealisation (their `ca_years2` counter increments once per year while
`entryage <= age < exitage`). It is therefore not an upper bound, but it does
assume no repetition, absence or late start. Note also that the decoded sentinels
feed this arithmetic, so the `99 -> 4` literacy stand-in propagates into an age.

### IPUMS MLP linked census extract

`usa_00001.csv.gz` (13 GB gzipped, in OneDrive) is an IPUMS Multigenerational Longitudinal
Panel **linked** extract. Despite the filename it is not a two-year 1900–1940 link: it
stacks all five census years, one row per person per census in which they appear, with only
linked cases included — **220,979,023 rows**, 58 columns.

| 1900 | 1910 | 1920 | 1930 | 1940 |
|---|---|---|---|---|
| 12,971,952 | 23,984,383 | 39,982,220 | 70,207,628 | 73,832,840 |

It carries `SCHOOL`, `SEX` and `RACE` on the childhood rows as of 2026-09-21, for the attendance measure. **The extract is built from 1940 outward:** a 1940 person is included only if linked to at least one of 1900–1930, so everyone in it appears in 2–5 censuses and 1940 people found in no other census are absent (73.8 M of ~132 M). Among white men aged 17–49 in 1940, 30.0% appear in 2 censuses, 36.2% in 3, 22.4% in 4 and 11.4% in all 5; 9.7% have a gap (e.g. 1920 and 1940 but not 1930). It was downloaded as a bare CSV — **no DDI, codebook or command file came with it**, which
is why everything below exists. The DDI has since been fetched separately and now backs the
value labels; see *The DDI codebook* below. Four documents cover the extract:

| File | What |
|---|---|
| `ipums_linked_1900_1940_dictionary.md` | Every variable: definition, availability by census year, fill rate, distinct count, range, top values, and the code → meaning table wherever the DDI supplies one |
| `ipums_linked_1900_1940_provenance.json` | Run manifest: source identity, row counts, the DDI's sha256, `ddi_cross_checked`, and which variables still have no value labels |
| `merge_cls_to_linked_census.md` | How to join the CLS cohort requirements onto it, following what the authors actually did |
| `county_bite_from_linked_census.md` | Build spec **and results** for the county-level bite measure: observed completed schooling against what the law required, nominal and net of exemptions |
| `county_attendance_from_linked_census.md` | Build spec **and results** for the county-level ATTENDANCE measure -- the share of legally-required children not in school, measured contemporaneously. Supersedes the bite measure, since the statute names attendance |
| `county_attendance_from_fullcount.md` | Steps to rebuild the attendance measure on the **unlinked** 1900-1930 full counts, which the project does not yet hold. Removes the linkage selection entirely, since attendance needs no link |
| `county_attendance_cohort.md` | Build spec **and results** for county × birth-cohort schooling (ever attended, meeting the law's minimum years, gap to it) around the 13 first laws of 1907–1920, white non-migrant men born 1890–1920, placed at their earliest linked record at age ≤ 18. County levels carry signal; county before/after contrasts do not (split-half r 0.04–0.20) |

```powershell
python prep/mandatory_schooling/profile_ipums_linked_census.py                 # ~16 min, full pass
python prep/mandatory_schooling/profile_ipums_linked_census.py --labels-only   # seconds, re-renders
python prep/mandatory_schooling/test_ddi_helpers.py                            # no extract needed
```

#### Where each part of the dictionary comes from

Three sources with three different authorities, and it matters which is which:

| Layer | Source | Verified against |
|---|---|---|
| Fill rates, distinct counts, ranges, top values | one full streaming pass over the extract | row-count and structural assertions |
| Value labels (code → meaning) | IPUMS' DDI codebook | it *is* the authority |
| Variable labels and coding notes | `ipums_variable_labels.csv`, hand-maintained | the DDI, on every run |

`--labels-only` re-reads the saved `ipums_linked_1900_1940_dictionary.csv` instead of the
13 GB extract, so relabelling costs seconds rather than 16 minutes. The numeric columns pass
through untouched — if a `git diff` after a `--labels-only` run shows any of them moving,
something is wrong. One caveat: the saved dictionary keeps each column's top eight values,
so on a long code list the re-render cuts the table to the most common codes rather than to
every observed one, and says so. A full run is the authoritative render.

#### The DDI codebook

`Data/raw/mandatory_schooling/usa_00001.xml` (497 KB, fetched by hand from IPUMS USA →
My Data Extracts on 2026-09-20; override the location with `IPUMS_LINKED_DDI`). It
describes exactly the extract's **58 variables** — no surplus, no shortfall — and gives
value labels to **45** of them. The remaining 13 split into two groups:

- genuinely unlabelled, because they are continuous or are identifiers: `BIRTHYR`, `HIK`,
  `HISTID`, `HRSWORK1`, `INCWAGE`, `MIGCOUNTYICP5`, `MOMLOC`, `PERNUM`, `POPLOC`, `SERIAL`,
  `WKSWORK1`;
- **`COUNTYICP` and `OCCSCORE`**, where the codebook enumerates codes (293 and 58) but
  names none of them. For `COUNTYICP` that is not an omission: ICPSR county codes are
  state-dependent and mean nothing without `STATEFIP`/`STATEICP`.

The cross-check fails loudly if the codebook does not describe the extract — the same
guarantee `convert_ipums_1940_fullcount.py` gets from checking its layout against IPUMS'
published `.sps`. Without a codebook the run still completes: value labels are omitted, the
Markdown header says the labels are unverified, and the manifest records
`"ddi_cross_checked": false`.

Do **not** overwrite `ipums_variable_labels.csv` with the DDI's labels. Its `coding_note`
column carries analyst knowledge the DDI does not have — the `HIGRADED` years-of-schooling
construction, the `COUNTYICP` ×10 trap, which variables are 1940-only. 36 of the 58 labels
are deliberately reworded from IPUMS' phrasing and the check reports them without
complaining.

**What the cross-check caught on its first real run.** Four project labels said less than
IPUMS' own, and all four are now corrected in `ipums_variable_labels.csv`:

| Variable | Was | IPUMS says |
|---|---|---|
| `QSCHOOL` | "flag for SCHOOL"; "0 = reported" | flags **SCHOOL *and* SCHLTYPE**; `0` = original entry **OR inapplicable (not in universe)** — so `0` does not mean "reported" |
| `QAGE` | "non-zero = allocated or edited" | `4` = allocated, `1`/`8` = failed edit, `2`/`5`/`7` = illegible, `3`/`6` = missing — non-zero is not uniformly allocation |
| `COUNTYICP` | "County of residence (ICPSR code)" | codes are **state-dependent**; boundaries are as-of-year with no uniform county system, so the same code is not necessarily the same area in 1900 and 1940 |
| `HIGRADE`/`HIGRADED` | "Highest grade attended" | "Highest grade **of schooling**" |

The `COUNTYICP` one matters for the childhood-county extension in
`merge_cls_to_linked_census.md`. Note also that IPUMS' "identifiable counties only" caveat
does **not** bite here: all counties are identified in the 1850–1950 full counts, and the
column is 100% populated with no `0` codes.

The code-mismatch report caught a fifth, and it is the one most likely to produce silently
wrong numbers. `PRESGL`, `ERSCOR50` and `EDSCOR50` are flagged because the DDI codes them
as integers with an implied decimal (`0001` = 0.1) while the extract's values carry the
decimal already — `40.7`, not `407`. **The 1940 full-count Parquet does the opposite**: its
manifest records the implied decimal as *unapplied*, so the same three variables are 10×
larger there. Anything comparing occupational scores across the two datasets has to
rescale. While checking this, `PRESGL`'s N/A code also turned out to be `0`, not the
`999.9` the project note claimed — its observed maximum is 81.5, so `999.9` never occurs.
`ERSCOR50` and `EDSCOR50` do use `999.9`.

#### The extract's specification

The DDI records it, so the extract can be rebuilt without the original request:

| | |
|---|---|
| Project | IPUMS USA |
| Samples | the **1900, 1910, 1920, 1930 and 1940 100% databases** — full counts, not samples |
| Density | 100% of each; the extract applies no sub-sampling |
| Restriction | MLP-**linked cases only**, which is why 221 M rows sit below full-count size |
| Variables | the 58 listed in `ipums_variable_labels.csv` |
| DDI produced | 2026-09-18, sha256 in `ipums_linked_1900_1940_provenance.json` |

Two gaps remain. Nothing in this repo *downloads* the extract — the only IPUMS API calls in
the project, in `prep/download_county_data.r` and `download_matching_covariates.R`, fetch
NHGIS *aggregate* data, so rebuilding means submitting the above by hand through the IPUMS
USA web interface. And the CSV still sits in OneDrive rather than under `DET_DIR` like
every other input.

**The headline point of the merge document:** CLS use **no record linkage**. Treatment is
assigned on state of birth × birth cohort, both recorded in the 1940 cross-section, which is
why they get a ~19 M sample with no crosswalk and no match-rate selection. Reproduce that
join first (1940 rows only, `yob = 1940 - AGE - 1`); the childhood-county extension the
linked file enables is something the paper never did, and it carries selection their design
does not.

### Linked 1900–1950 extract: birthplace-partitioned Parquet

The standalone [dataset guide](ipums_linked_1900_1950.md) describes its
contents, linkage and partition rules, scientist flag, and provenance.

`D:/usa_00003.csv.gz` is the six-census IPUMS extract (1900–1950). Its companion
`D:/usa_00003_ddi.xml` supplies the 60 variable names, classes, descriptions, and
category labels. Convert both with DuckDB through R:

```powershell
Rscript prep/mandatory_schooling/convert_ipums_linked_1950.R
Rscript prep/mandatory_schooling/test_convert_ipums_linked_1950.R
```

The converter uses 12 DuckDB threads and a 20 GB memory limit. Override its default
paths with `IPUMS_LINKED_1950_SOURCE`, `IPUMS_LINKED_1950_DDI`, and
`IPUMS_LINKED_1950_PARQUET_DIR`. The final output path must not exist when it starts.
The default is `D:/ipums_linked_1900_1950/`, outside Dropbox. It contains
`birth_bpl=<code>/*.parquet`, `dictionary.md`, `variable_dictionary.csv`,
`value_labels.csv`, `validation_year_bpl.csv`, and `_manifest.json`.

`birth_bpl` is a **1950-anchored birthplace code**, not the row's residence
`STATEFIP`. Every 1950 row uses its own `BPL`, even when `HIK` is blank. An earlier
row uses the `BPL` on its unique 1950 `HIK` match. If no unique 1950 match exists,
it goes in `birth_bpl=unknown`; its original `BPL` remains in the data. The
partition includes every birthplace code, including territories and foreign
countries. `unknown` means missing 1950 linkage, distinct from any IPUMS `BPL`
code for unknown birthplace. The raw `BPL` can disagree across a person's census
rows, so this rule gives linked rows a consistent 1950 birthplace assignment.

The added Boolean `scientist_occ1950` marks the occupation on **that census
row**. It is true only for `OCC1950` codes `007` (chemists), `061`
(agricultural scientists), `062` (biological scientists), `063` (geologists and
geophysicists), `067` (mathematicians), `068` (physicists), and `069` (miscellaneous
natural scientists). Other recorded codes are false; blank `OCC1950` is null.
The DDI calls `069` “Misc. natural scientists.” This flag does not mean a person
ever held one of these occupations across linked years.

The 2026-09-25 build passed source-to-Parquet counts by year and original `BPL`
and checked every partition assignment. It contains **463,770,248 rows** in
**146 Parquet files** (19.69 GiB), with **203,835 scientist-position rows**.
There are **77,229,397 unique 1950 HIK anchors**. Among pre-1950 rows,
**171,024,163** (49.6%) have no unique 1950 match and therefore occupy the
`unknown` partition. This partition describes linkage availability, not a
missing birthplace on the original census row.

The source fields remain available through Hive partitioning. For example:

```r
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbGetQuery(con, paste0(
  "SELECT YEAR, birth_bpl, count(*) AS n FROM read_parquet(",
  "'D:/ipums_linked_1900_1950/birth_bpl=*/*.parquet', ",
  "hive_partitioning=true) GROUP BY ALL"))
DBI::dbDisconnect(con, shutdown = TRUE)
```

### 1950 sample-line linked dataset

The separate [1950 sample-line linked dataset](ipums_linked_1950_sample_line.md)
retains every 1950 sample-line respondent and adds a reciprocal 1940 linked row
when available. It enriches the existing census Parquet with row-specific
`SLWT`, `SLREC`, `VETWWI`, and `VETWWII`, and writes to
`D:/ipums_linked_1950_sample_line/`. Run
`prep/mandatory_schooling/test_build_linked_1950_sample_line.R` followed by
`prep/mandatory_schooling/build_linked_1950_sample_line.R` from the repo root.

### 1950 scientist occupations by birth state

Run `Rscript analysis/mandatory_schooling/describe_linked_1950_scientists_by_state.R`
from any working directory. It reads the Parquet above with R/DuckDB (12 threads,
20 GB memory limit) and writes `state_summary.csv`, `state_occupation.csv`, and
`notes.txt` under the Dropbox
`results/mandatory_schooling/linked_1950_scientists/` directory. Use
`IPUMS_LINKED_1950_PARQUET_DIR` and `TALENT_DETS_DATA_DIR` to override the
default source and Dropbox locations. Run
`Rscript analysis/mandatory_schooling/test_describe_linked_1950_scientists_by_state.R`
for the small synthetic-data test.

The denominator is every 1950 white male age 18 or older with a state/DC `BPL`
equal to his 1950 `STATEFIP`, regardless of whether an occupation was recorded.
The numerator is the count with `scientist_occ1950 == TRUE`. The state table
reports that count, each of the seven `OCC1950` component counts, scientists per
100,000 eligible men, and the percentage of all eligible U.S. scientists in
each state. The long table gives each occupation's within-state percentage;
that percentage is blank when the state has zero scientists. State and
occupation labels come from the DDI-derived `value_labels.csv`.

This is a **birth-state residence** comparison, not a confirmed never-migrant
sample. A man who left his birth state and returned by 1950 qualifies. The
1950 records have no populated `MIGRATE5` field to tighten that definition.

### Schooling IV for 1950 scientist occupation

This analysis adapts the first-wave compulsory-schooling-law design of Clay,
Lingwall, and Stephens (2021) to ask whether schooling changes the probability
of holding a scientist occupation in the 1950 census. It combines schooling
reported in 1940 with the occupation on the same person's linked 1950 record.
The [linked-census dataset guide](ipums_linked_1900_1950.md) documents the
Parquet conversion and scientist flag; the
[CLS law guide](merge_cls_to_linked_census.md) explains the state-by-cohort
requirements. The executable specification is
[`estimate_linked_1940_schooling_1950_scientist_iv.R`](../../analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_iv.R).

#### Inputs and run order

Start with the converted six-census extract at
`D:/ipums_linked_1900_1950/birth_bpl=*/*.parquet` and the CLS cohort file at
`<Dropbox project>/output/mandatory_schooling/cls/cls_cohort_requirements_1875_1912.parquet`.
If the Parquet extract is absent, follow the dataset guide to run
`convert_ipums_linked_1950.R` on `D:/usa_00003.csv.gz` and its DDI; that
conversion preserves the source variables and adds `scientist_occ1950`.
Set `IPUMS_LINKED_1950_PARQUET_DIR` for another Parquet location and
`TALENT_DETS_DATA_DIR` for another Dropbox root. The estimator uses DuckDB
with eight threads and a 20 GB memory limit.

Run these commands **from the repository root** (or pass absolute script paths):

```powershell
Rscript analysis/mandatory_schooling/test_linked_1940_schooling_1950_scientist_iv.R
Rscript analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_iv.R
```

#### Cleaning, law assignment, and linkage

1. Restrict the CLS file to birth years 1885--1912. The script requires one
   row per `bpl` × `yob` and checks for 49 states × 28 birth years = 1,372 law
   cells. Its treatment is `ca_years_combined`, the combined compulsory
   attendance, child-labor, and continuation-school requirement.
2. Select `YEAR == 1940`, male (`SEX == 1`), white (`RACE == 1`) census rows
   aged 27--54. Define `yob = 1940 - AGE - 1` and join laws on the **1940
   row's `BPL`** and `yob`. This state of birth determines exposure even if
   the 1950 `BPL` disagrees. Neither current residence `STATEFIP` nor the
   Parquet partition key `birth_bpl` substitutes for this join key.
3. Convert 1940 `HIGRADED` to completed years: discard blank, `000`, and
   `999`; let `g = floor(HIGRADED / 10)`; map `g <= 3` to zero, `4 <= g <= 20`
   to `g - 3`, and higher valid grades to 17. This follows the CLS schooling
   recode and yields the endogenous regressor `school_years`.
4. Retain rows with `LINK1950 == 1` and a nonblank `HIK`, the cross-census
   person key. Require unique eligible 1940 `HIK`s. Join to `YEAR == 1950`
   on `HIK` while requiring the reciprocal `LINK1940 == 1`, and assert a
   one-to-one match. Audit 1950 birthplace, sex, race, and age changes;
   disagreements remain in the main sample.
5. Read the **1950 row's** `scientist_occ1950` flag, derived from `OCC1950`
   codes `007`, `061`, `062`, `063`, `067`, `068`, and `069`. Drop rows whose
   occupation flag is null. All other recorded occupation codes, including
   nonoccupational codes, are zero. Check that the flag agrees with `OCC1950`.

The saved 2026-09-25 run has this auditable flow (`sample_flow.csv`):

| Restriction | Men |
|:--|--:|
| CLS-eligible 1940 men | 16,219,098 |
| Valid 1940 schooling | 15,880,601 |
| Valid grade and flagged 1950 link | 12,285,411 |
| Unique 1940--1950 pairs | 12,285,411 |
| Pairs with observed 1950 occupation | 12,285,411 |
| Scientist occupations in 1950 | 22,743 |

#### IV specification and inference

Bin `ca_years_combined` as 1--5, 6, 7, 8, or 9+ required years; zero is
the omitted category. For man $i$, birth state $s$, and birth year $c$, the
first stage and second stage are:

$$
S_{isc}=\alpha^S_s+\lambda^S_{r(s)c}
       +\sum_{b\in\{1\text{--}5,6,7,8,9+\}}\pi_b Z^b_{sc}+u_{isc},
$$

$$
Y_{isc}=\alpha^Y_s+\lambda^Y_{r(s)c}
       +\beta\widehat{S}_{isc}+\varepsilon_{isc}.
$$

Here $S$ is completed schooling in 1940, $Y$ is the binary 1950 scientist
occupation, $Z^b$ indicates a law-requirement bin, $\alpha_s$ denotes
birth-state fixed effects, and $\lambda_{r(s)c}$ denotes CLS-region ×
birth-year fixed effects. The reduced form replaces $S$ in the first equation
with $Y$. An OLS regression of $Y$ on observed $S$ with the same fixed effects
is reported for comparison. Since $Y$ is binary, the second stage is a linear
probability model; multiply $\beta$ by 100,000 to report additional scientist
occupations per 100,000 men per completed school year.

The script groups identical `bpl`/`yob`/region/law-bin/schooling/outcome
records and weights each cell by its person count, preserving the individual
regression cross-products. It reports state × birth-year and birth-state
clustered standard errors and joint first-stage $F$ tests. `K.adj = FALSE`
removes the finite-observation correction so clustering is invariant to the
cell collapse. The Anderson--Rubin $p$-value for zero effect is the joint
reduced-form Wald test of the five instruments with birth-state clustering.
The 95% Anderson--Rubin confidence set inverts state-clustered tests over a
grid of $\beta$ values from -0.05 to 0.05 probability units; a blank endpoint
means the accepted set reached the grid boundary, not that it is bounded.

#### Sensitivities, outputs, and interpretation

The stricter-link sensitivity requires matching 1940 and 1950 birth state,
male and white codes in 1950, and an age increase of 9--11 years. A separate
1950 same-record sensitivity uses 1950 `HIGRADED` and occupation for eligible
men asked the sample-line grade question. It is unweighted because this
extract does not contain `SLWT`.

Results are written to
`<Dropbox project>/results/mandatory_schooling/linked_1940_schooling_1950_scientist_iv/`.
`sample_flow.csv`, `linkage_audit.csv`, and `law_bin_counts.csv` audit
selection and treatment; `main_cells.csv`, `quality_link_cells.csv`, and
`sensitivity_1950_grade_cells.csv` hold grouped regression inputs.
`first_stage.csv`, `reduced_form.csv`, `estimates.csv`, and `diagnostics.csv`
report model results. `anderson_rubin_grid.csv` and
`anderson_rubin_intervals.csv` record weak-instrument-robust inference;
`models.rds` and `notes.txt` retain fitted models and run provenance.

The outcome is an occupation observed in 1950, not the date a man first
became a scientist. Linking 1940 to 1950 selects people non-randomly, so the
main estimate does not represent all 1950 men. A causal schooling
interpretation requires relevant law-induced schooling changes and, beyond
the included fixed effects, valid exclusion, monotonicity, and comparable
state-cohort law timing. The state-clustered first stage and Anderson--Rubin
results should be read alongside conventional 2SLS estimates.

## CLS IV for BA-equivalent completion

The separate runner
`analysis/mandatory_schooling/estimate_linked_1940_ba_1950_scientist_iv.R`
uses `1(school_years >= 16)` as the endogenous treatment, including
postgraduate schooling. It keeps everyone with valid schooling and retains
the original five CLS requirement bins, fixed effects, scientist outcome,
cell-count weights, clustering conventions, and three sensitivity samples.
The measure is BA-equivalent completed schooling, not an observed degree award.

Run from the repo root (or any directory using the script's absolute path):

```powershell
Rscript analysis/mandatory_schooling/estimate_linked_1940_ba_1950_scientist_iv.R
Rscript analysis/mandatory_schooling/test_linked_1940_ba_1950_scientist_iv.R
Rscript analysis/mandatory_schooling/test_linked_1940_schooling_1950_scientist_iv.R
```

The runner reads the existing schooling IV's three saved cell CSVs and sample
audits, without rescanning census microdata. It validates the state-cohort
mapping and sample totals and re-estimates the schooling-years baseline to
check the saved coefficients, clustered inference, and diagnostics. The BA
reduced form and AR test at zero must match the saved baseline.

Results go to Dropbox
`results/mandatory_schooling/linked_1940_ba_1950_scientist_iv/`.
Override the saved-cell input, result, and note directories with
`MS_BA_IV_SOURCE_DIR`, `MS_BA_IV_RESULTS_DIR`, and `MS_BA_IV_PROJECT_DIR`.
The input and result directories must differ. Standard data-root configuration
still uses `TALENT_DETS_DATA_DIR` and `paths.R`.

First-stage CSVs contain probability-unit and percentage-point coefficients.
The BA effect column is `effect_per_100k_per_ba_completed`: scientist
occupations per 100,000 men for a change from noncompletion to completion.
`completion_shares.csv` reports the denominator and completion count for each
sample. `input_provenance.csv`, `baseline_validation.csv`, `session_info.txt`,
sample audits, fitted models, and run notes retain reproducibility information.

The main sample's state-clustered 95% Anderson--Rubin set is inverted over
2,001 probability-unit values in `[-1, 1]`, refining crossings to `1e-8`.
All detected accepted intervals are retained. Explicit boundary flags mark
search truncation rather than proven infinite endpoints; a finite grid may
miss accepted regions narrower than its 0.001 spacing.

The runner also renders the self-contained note
`project/mandatory_schooling_ba_scientist_iv.tex` with inline tables and a
comparison to the existing schooling-years specification. Open and compile
the note using the built-in Codex LaTeX editor. The original results and note
are preserved. The two coefficients have different units and estimands.
Re-render the note without re-estimating by running
`analysis/mandatory_schooling/export_linked_1940_ba_1950_scientist_iv_to_project.R`;
`note_provenance.csv` records the note, renderer, and result-input checksums.

A BA-specific causal interpretation additionally requires law-induced
changes at other schooling margins to have no separate effect on scientist
occupation. The outcome remains occupation observed in 1950, and selected
census links do not represent all men.

## WWII veteran status and scientist occupation

`build_linked_1950_veteran_scientist.R` creates one row per eligible 1950
sample-line man with reciprocal 1940 records, linked father characteristics,
and full-count household homeownership. The separate output is
`D:/ipums_linked_1950_veteran_scientist`.
`analysis/mandatory_schooling/estimate_linked_1950_veteran_scientist.R` estimates
weighted linear probability models for WWII veteran status and scientist work,
covering the paper's two cohorts and the earlier overlapping comparison cohorts.
Results and sample audits go to Dropbox
`results/mandatory_schooling/linked_1950_sample_line_veteran_scientist/`.
See [the dataset and analysis documentation](linked_1950_veteran_scientist.md)
for joins, definitions, tests, controls, and environment overrides.

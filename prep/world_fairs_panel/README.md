# World Fairs Panel Pipeline

Scripts in this folder collect the Wikipedia world's fairs list, attach the
previously researched enrichment fields, and build the panels used by the world
fairs event studies.

These scripts resolve repository files through the `GTL_REPO` environment
variable, which should point to the repository root. They can be run from this
directory or from the repository root.

Run scripts from this directory:

```powershell
cd prep/world_fairs_panel
```

Recommended order:

```powershell
Rscript 00_download_arcgis_english_parishes_1911.R
Rscript 01_scrape_worlds_fairs.R
Rscript 02_build_worlds_fairs_enriched.R
Rscript 03a_build_greater_london_1911_parish_crosswalk.R
Rscript 03c_build_ukds_1911_registration_lgd_crosswalk.R
Rscript 03d_build_ukds_1911_population_weighted_lgd_rd_crosswalk.R
Rscript 03e_build_ukds_historical_lgd_population_1851_1901.R
Rscript 03_build_uk_historical_urban_unit_population_1801_1961.R
Rscript 04_build_uk_historical_urban_units_us_county_panel.R
```

The active world's-fairs enrichment stage is now intentionally compact:

1. `01_scrape_worlds_fairs.R` scrapes the Wikipedia list to
   `input/worlds_fairs_wikipedia.xlsx`.
2. `02_build_worlds_fairs_enriched.R` reads the scrape, the previously extracted
   host geocodes, the curated 1790-1910 visits/venues file, and the researched
   1911-1960 batches. It writes the canonical consolidated file
   `input/worlds_fairs/worlds_fairs_1790_1960_with_visits_venues.csv` and the
   matching XLSX/summary outputs. It does not perform new internet searches.

`00_download_arcgis_english_parishes_1911.R` downloads the public Great Britain
Historical GIS English parish boundaries for 1911 from ArcGIS. It paginates the
13,416-feature service into retained GeoJSON source chunks, writes the pipeline
GeoPackage under `input/worlds_fairs/city_census/GBR/raw/`, preserves source
metadata and attribution, and validates identifiers, geometry, and coverage of
the existing 1911 Liverpool CB polygon. Existing valid downloads are reused;
pass `--force` to refresh every source file.

The `fair_enrichment/` subfolder keeps the older intermediate enrichment
scripts for traceability: geocoding, post-1911 additions export, research-batch
preparation, batch consolidation, and the former final-combine step. These
scripts are not part of the main run order.

The current UK panel builder is
`03_build_uk_historical_urban_unit_population_1801_1961.R`. It builds the
harmonized 1921 urban-unit geography, attaches Law-Robson and Nomis population,
adds Wikipedia scientist/inventor outcomes from geocoded birth points, and
attaches the 1801 population density and agriculture, trade, and
other-occupation shares from the Caprettini-Voth `swing-cross.dta` replication
data. It also reads `swing-panel.dta` to recover parish-density population
knots for 1801, 1811, 1821, and 1831. The demographic variables are
joined to `Parishes1851.shp` by `PARISH_ID`, allocated to the fixed 1921 target
units by polygon intersection, and weighted by implied 1801 parish population.
Population density is expressed as persons per square kilometre and aggregated
as allocated 1801 population divided by allocated source area. All variables
are time-invariant baseline characteristics named with the `_1801` suffix;
missing and partial source coverage is retained rather than imputed. The source files live
under `Data/raw/world_fairs/data_c_voth/` in Dropbox. The
old numbered scripts from `03` through `21`, including the LAU/GISCO outcomes
workflow, are archived in `legacy/numbered_03_21/` for traceability and are not
part of the main run order.

An optional post-processing script,
`03b_attach_ukds_1831_socioeconomic_controls.R`, adds the housing, family,
sex, occupation, and servant variables from UK Data Service study 4961 to a
separately named copy of the canonical UK panel. It reconstructs the current
1921 target geometry, links each 1831 record to its 1851 UKDS source geometry,
and assigns the record's full counts only when at least 60% of that source area
falls inside one target. Records below the threshold remain in the assignment
audit and are not imputed. Outputs are written under
`Data/processed/worlds_fairs/alternative_ukds/`. The canonical panel,
population series, outcomes, and analysis inputs are not changed. Run it after
the main UK panel exists:

```powershell
Rscript prep/world_fairs_panel/03b_attach_ukds_1831_socioeconomic_controls.R
```

The occupation-share stage writes a unit-level file, a parish-to-target
crosswalk, and a QC summary under `Data/processed/`. The annual UK panel carries
`agri_share_1801`, `trade_share_1801`, `other_share_1801`,
`occupation_share_coverage_1801`, `population_density_1801`, and
`population_density_area_coverage_1801`. It also carries
`population_implied_1801`, the target-unit population implied by parish density
and spatially allocated parish area. For population years 1801, 1811, 1821,
and 1831, an uncalibrated Swing knot is used only when the harmonized census
population is missing, at least 95% of the real target geometry and its valid
density area are covered, and the sequential change relative to the last
accepted Swing knot is no greater than a factor of five per decade. Observed
population always takes precedence. Annual population is then linearly
interpolated in levels between observed or accepted Swing knots, without
extrapolation. The full candidate/usage/exclusion audit is written to
`Data/processed/uk_historical_urban_units_swing_population_1801_1831_audit.csv`.
Script `04` propagates these fields to the combined UK-US panel and leaves the
Swing-specific fields missing for US counties. Current analysis scripts do not
use the new fields as regression controls.

`03a_build_greater_london_1911_parish_crosswalk.R` is a diagnostic/refinement
step for London. It reads the BBCE 1911 parish list added under
`input/worlds_fairs/city_census/GBR/raw/`, matches those parish names to the
downloaded ArcGIS English parish boundaries, reconstructs a
`GREATER_LONDON_1911_PARISHES` geometry, and allocates that geometry and 1911
population to the Nomis/ONS 1921 district boundaries. It writes match audits,
the district crosswalk, and a GeoPackage back to the ArcGIS parish input folder
in Dropbox. It does not currently overwrite the main panel.

`03c_build_ukds_1911_registration_lgd_crosswalk.R` overlays the UKDS 1911
registration-district and local-government-district boundaries. Script `03d`
matches published 1911 population margins to both geographies and uses
area-seeded iterative proportional fitting to distribute population across the
overlap cells. Observed LGD populations are exact calibration targets, so an
LGD's reconstructed total equals its published total by construction; this is
an accounting identity, not an out-of-sample population prediction. The
substantive crosswalk output is the calibrated composition of each LGD across
registration districts. The LGD-level fit, contributing RD shares, excluded
sliver counts, and RD-margin adjustments are written to
`Data/processed/worlds_fairs/ukds_1911_boundary_crosswalk/ukds_1911_lgd_calibrated_population_audit.csv`.

`03e_build_ukds_historical_lgd_population_1851_1901.R` carries the calibrated
1911 RD-by-LGD relative-density surface back to the historical RD systems. It
matches each year's RD populations to the corresponding UKDS boundaries,
spatially adapts the 1911 weights for RD splits and mergers, and reconstructs
population on fixed 1911 LGDs for 1851, 1861, 1871, 1891, and 1901. The 1881
LGD population is the linear-in-levels midpoint of 1871 and 1891; the available
raw 1881 RD file is intentionally not used. The stage writes the balanced LGD
panel, historical RD-to-LGD weights, population-match audit, and QC table under
`Data/processed/worlds_fairs/ukds_1911_boundary_crosswalk/`. Absolute historical
density is rescaled to each observed RD total, while the 1911 within-RD relative
density pattern is assumed to remain fixed.

The same stage also constructs fixed 1851 baseline covariates from the UKDA
8613 registration-subdistrict file. It first population-weights `HC1`--`HC5`,
`SC1`--`SC8`, and `C_TEACHER` from subdistricts to their 1851 registration
district, then applies the 1851 historical RD-to-LGD population contributions
used in the population backcast. Thus the HC and SC fields remain percentages,
while `C_TEACHER` remains a children-per-teacher ratio. Effective source
population coverage is tracked through both aggregation steps. All 14 values
are set to missing for an LGD when coverage is below 95 percent. The stage
writes the 1,834-LGD baseline file
`ukds_1911_lgd_baseline_covariates_1851.csv`, the RD aggregation audit
`ukds_1851_rd_baseline_covariate_audit.csv`, and a baseline QC table alongside
the historical-population outputs.

`03f_build_ukds_1911_lgd_population_1851_1961.R` extends that fixed-1911 LGD
population series to the 1921, 1931, 1951, and 1961 census systems. Each later
LGD is intersected with the calibrated 1911 density cells; its observed census
population is then allocated across the 1,834 analytical 1911 LGDs in
proportion to the inherited density seed. The resulting eleven population
knots are linearly interpolated in levels to a balanced 1851--1961 annual
panel. Annual England-and-Wales births come from the registration vital series
through 1910 and the LGD vital series thereafter. They are distributed using
each fixed LGD's population share, so estimated LGD births sum exactly to the
national total in every year. The script also writes the dissolved analytical
1911 geometry and detailed later-boundary weights, match audits, and QC files
under `Data/processed/worlds_fairs/ukds_1911_boundary_crosswalk/`.

`03g_build_ukds_1911_lgd_world_fairs_outcomes_1851_1961.R` attaches the
Wikipedia Discovery/Science outcomes, STEM classification, AMWS outcomes, and
full-history fair treatment to that annual panel. It also appends the fixed
1851 baseline covariates and their coverage/provenance fields to every annual
and decade observation; these fields do not vary over time within an LGD. AMWS
1906, 1938, and 1955
records are deduplicated across and within editions by normalized surname,
first initial, birth year (plus or minus one), and England/Wales scope; the
1986 edition is added without cross-edition deduplication, following the US
AMWS convention. UK place assignment is deliberately conservative: only an
exact, unique GeoNames populated-place match is accepted, while broad or
ambiguous locations such as generic London are excluded and retained in an
audit. The 1986 `see previous edition` contamination rule is also carried
over. AMWS outcomes are supported through birth year 1960, so their 1961
annual values remain missing rather than being coded as zero.

Treatment is defined at the fixed-LGD level using each LGD's own first matched
fair in the complete fair history. First fairs strictly after 1870 define the
treated cohort; LGDs first exposed by 1870 are marked pretreated and excluded
from the synthetic-DiD donor pool; never-host LGDs remain eligible controls.
The reviewed Crystal Palace 1851 venue coordinate is set to Hyde Park
(`-0.17049`, `51.50241`). The stage writes the balanced annual panel and two
balanced decade panels: standard calendar decades and the existing alternative
definition that shifts birth years ending in 7--9 to the following decade.

Run the two extension stages from the repository root after `03e`:

```powershell
Rscript prep/world_fairs_panel/03f_build_ukds_1911_lgd_population_1851_1961.R
Rscript prep/world_fairs_panel/03g_build_ukds_1911_lgd_world_fairs_outcomes_1851_1961.R
```

The first analysis-stage use of these panels is
`analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R`.
It constructs a separate 30-LGD donor pool for every post-1870 treated LGD
under both decade definitions. Candidates are restricted to never-host LGDs
whose fixed polygon is at least 50 km from the treated unit's first-fair venue.
They are ranked by equal-weight Euclidean distance in standardized log-one-plus
pooled pre-treatment Discovery/Science births per 100,000 population per year
and log population density in the completed decade before treatment. The
pooled rate is `100000 * sum(n_inventors) / sum(population * n_years_in_bin)`
over bins strictly before treatment. Raw
scientist counts and population do not enter the selection distance; they are
retained with the pooled person-year denominator in the pair and pool balance
audits. The script writes selected pairs, the full candidate audit, pool summaries,
standard-versus-alternative overlap, and QC files under
`results/worlds_fair/synthdid/ukds_1911_lgd_wikipedia_donor_pools/`. Defaults
can be changed for sensitivity checks with `SYNTHDID_DONOR_COUNT` and
`SYNTHDID_DONOR_MIN_DISTANCE_KM`. `SYNTHDID_COHORT_LEAD_YEARS` moves exposure
to cohorts born before the fair and rebuilds matching covariates at the shifted
pre-treatment cutoff.

```powershell
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
```

The corresponding estimation stage is
`analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R`.
It estimates one synthetic-DiD model for every treated LGD and decade
definition using its fixed 30-LGD donor pool and all twelve 1850--1960 bins.
The Wikipedia Discovery/Science count is annualized as
`100000 * n_inventors / (population * n_years_in_bin)`. Inference uses 200
seeded placebo replications by default. The current inputs yield 18 estimated
models; Willesden and Penge are explicitly reported as infeasible under both
definitions because the treated unit and selected donors have no informative
pre-treatment outcome variation. The stage writes model status, ATT and dynamic
effects, placebo draws, control and time weights, fit paths, serialized model
objects, model-specific figures, a forest plot, and QC under
`results/worlds_fair/synthdid/ukds_1911_lgd_wikipedia_donor_pools/models_wikipedia_rate/`.
The replication count and base seed can be changed with
`SYNTHDID_PLACEBO_REPLICATIONS` and `SYNTHDID_BASE_SEED`.

```powershell
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R
```

The 20-year cohort-lead sensitivity assumes exposure begins permanently for
birth cohorts starting at `first_fair_year - 20`. The shifted year is assigned
to the standard or alternative decade using the same binning functions as the
outcome panel. Donor pools are rebuilt rather than reused, preventing outcomes
that are post-treatment under the sensitivity timing from entering the match.
Models with fewer than two pre-treatment bins remain in the status audit but
are not estimated because `synthdid` cannot fit them. This removes standard
Willesden (no pre-period), standard Liverpool, Newcastle upon Tyne, and Penge,
and alternative Liverpool, Penge, and Willesden (one pre-period each). The
remaining 15 models and all supporting artifacts are written under
`ukds_1911_lgd_wikipedia_donor_pools/cohort_lead_20y/`; the zero-lead outputs
are unchanged.

```powershell
$env:SYNTHDID_COHORT_LEAD_YEARS = "20"
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R
```

`analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_synthdid.R`
adds an equal-treated-LGD pooled estimate for the host design. Because the
fairs have staggered treatment dates, each treated LGD and its 30 selected
donors form an event-time stack. The stacks are aligned at event time zero and
combined into one simultaneous-adoption `synthdid` matrix for each combination
of standard/alternative decades and zero/20-year cohort lead. The zero-lead
models exclude Lambeth and use the `-20, -10, 0, +10, +20, +30, +40` window,
leaving ten treated LGDs under each decade definition. The 20-year-lead models
retain Lambeth and use the `-20, -10, 0, +10, +20, +30` window; they contain
seven standard and eight alternative timing-eligible treated LGDs. A physical
donor selected for multiple treated LGDs remains a separate stack-control copy
because each copy represents a different calendar alignment.

Inference uses 200 seeded within-stack block placebos. Each replication selects
one donor per stack as placebo-treated, removes the actual treated LGD, and
uses the other 29 donors in that stack as controls. Outputs include the combined
stacked panel, ATT and dynamic effects, placebo draws and assignments, stack
and physical-LGD control weights, time weights, fit paths, figures, serialized
models, and QC under `ukds_1911_lgd_wikipedia_donor_pools/pooled_event_time/`.

```powershell
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_synthdid.R
```

With the current data, the pooled zero-lead estimates are -0.078 under standard
decades (95% interval -0.396 to 0.240; block-placebo p = 0.602) and -0.023 under
alternative decades (95% interval -0.302 to 0.256; p = 0.896). The 20-year-lead
estimates are 0.205 under standard decades (95% interval -0.034 to 0.443;
p = 0.109) and 0.192 under alternative decades (95% interval -0.031 to 0.415;
p = 0.114). All four intervals span zero.

## Lagged-outcome donor pools and Dube-Zipperer pooling

The donor-pool builder supports a second matching specification, selected with
`SYNTHDID_MATCHING_SPEC` and defined in
`analysis/world_fairs/synthdid/ukds_1911_lgd_matching_spec.R`:

- `pooled_rate_density` (default) is the original two-variable match described
  above: one scalar pooled pre-treatment Wikipedia rate and log baseline
  population density. It writes to the historical paths, so leaving the
  variable unset reproduces the existing outputs unchanged.
- `lagged_rate_growth_density` replaces the pooled scalar with the whole
  pre-treatment outcome path and adds population growth. The matching columns
  are one `log1p` annualized Wikipedia Discovery/Science rate per pre-treatment
  decade bin, `log1p` of the annualized pre-treatment population growth rate,
  and `log` baseline population density. All outputs are isolated under
  `match_v2_lagged/`.

The population growth rate is a compound annual rate over the pre-treatment
window, `(P_last / P_first)^(1 / (t_last - t_first)) - 1`, where the first bin
is 1850 and the last is the completed decade before treatment. Both
specifications z-score every column on the eligible-donor mean and standard
deviation, standardize the treated unit with those same donor moments, and take
an equal-weight Euclidean distance, so with the lagged specification the
outcome path's share of the distance rises with the number of pre-treatment
bins. Donor eligibility, the 50 km venue ring, and the 30-donor cut are
unchanged. A matching column with no variance across eligible donors is dropped
from the distance and recorded rather than aborting the run.

Two extra audits accompany the lagged specification:
`donor_pool_matching_columns.csv` holds one row per treated LGD, decade
definition and matching column with its donor moments, treated value, treated
z-score and used/dropped status; `donor_pool_matching_components.csv` holds the
per-column z-gaps for every selected donor and is verified to reproduce
`match_distance` exactly. The distance identity check becomes three-component
(`outcome_lag`, `growth`, `density`), and `donor_pool_qc.csv` gains
`matching_specification_id`, the used-column range, the dropped-column count,
the component row count and the component identity error.

With the current data the lagged specification builds all 22 zero-lead pools
and the same 15 timing-eligible pools at the 20-year lead, uses between 4 and 12
columns per pool (always `available_pre_periods + 2`), drops no columns, and
retains on average 8 of the original 30 donors per zero-lead pool (mean
Jaccard 0.17, range 0 to 18 donors) relative to `pooled_rate_density`.

Two consequences of the rebuilt pools are worth recording. First, feasibility
improves: at the zero lead 21 of 22 models are estimated instead of 18, because
alternative Penge, alternative Willesden and standard Penge now have donors with
informative pre-treatment outcome variation. Only standard Willesden, which has
two pre-periods, remains infeasible. At the 20-year lead all 15 timing-eligible
models are estimated with no zero-variation failures.

Second, and against expectation, the richer predictor set does **not** uniformly
improve pre-treatment fit. Across the 18 pools comparable to the old
specification, only 7 have a lower pre-treatment RMSPE and the median rises from
0.056 to 0.067; the mean falls only because Wembley's already very poor fit
improves. The likely mechanism is that the old rule put half its weight on a
single pooled rate, which is a smoothed and relatively low-noise quantity,
whereas the new rule spreads most of its weight across individual decade bins
whose rates are sparse and often zero, so z-scoring them amplifies noise.
Several event-level estimates move materially as a result (for example
alternative Bristol from 0.002 to -0.106 and standard Wolverhampton from -0.009
to -0.139). The lagged specification should therefore be read as a different,
more demanding match rather than as a strictly better one, and the pre-fit
diagnostics below are the place to judge it.

```powershell
$env:SYNTHDID_MATCHING_SPEC = "lagged_rate_growth_density"

$env:SYNTHDID_COHORT_LEAD_YEARS = "0"
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R

$env:SYNTHDID_COHORT_LEAD_YEARS = "20"
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R
```

The per-event estimator now resolves its donor and results directories through
the same matching specification and refuses to run when the donor pools on disk
were built under a different one. The 20-year-lead outputs therefore live at
`match_v2_lagged/cohort_lead_20y/models_wikipedia_rate/`.

`analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_dube_zipperer.R`
implements `docs/generic_pooled_synthetic_control_plan.md`. It estimates one
synthetic control per treated event on that event's own pool and pools the
event-level estimates afterwards, which is a different estimator from the
stacked `pooled_event_time` design rather than a replacement replication of it.
Two consequences are worth stating explicitly. First, the event effect is the
mean treated-minus-synthetic gap over all available post decades, which is not
the same quantity as `synthdid`'s own lambda-weighted ATT; both are reported
side by side in every output. Second, no common event window is needed, so
Lambeth is retained under the zero cohort lead whereas the stacked design drops
it.

The stage recomputes each treated event and requires the refit to reproduce the
per-event stage's ATT, mean post gap and pre-treatment RMSPE before continuing.
It then computes pre-treatment RMSPE and a mean-absolute-pre-outcome normalized
NRMSPE, the standardized effect `S_e = tau_e / RMSPE_pre`, the post/pre RMSPE
ratio, and a full in-space placebo distribution in which every one of the 30
donors takes a turn as placebo-treated, is removed from its own pool, and is
fitted against the remaining 29 with the real treated LGD dropped. Placebo
p-values are two-sided on `|S|`; percentile ranks are directional. Placebo-fit
restrictions on the pre-treatment MSPE ratio are reported at cutoffs 2, 5, 10
and 20 alongside the unrestricted set.

Pooling is equal-weighted per treated LGD for economic magnitude, and the mean
placebo percentile rank is the pre-specified pooled inference statistic. The
baseline retains every estimated event; progressive trimming on NRMSPE is
reported as robustness rather than used as a screen. Joint randomization
inference permutes unit identities jointly: one permutation of the pooled donor
universe is walked and each event takes the first still-unused unit eligible for
it, which preserves treatment timing, donor eligibility, donor-pool overlap and
the repeated use of the same physical LGD, and prevents one LGD from being
placebo-treated for two events in the same draw. Because a drawn unit's
placebo was already fitted with that unit treated, the real treated LGD removed
and the other 29 donors as controls, the draw reuses that fit rather than
refitting; the pre-fit trimming rule is reproduced inside each draw. Note that
the observed rank divides by 31 while a placebo rank divides by 30, which is
the usual asymmetry of the in-space placebo construction.

Two scale and centring issues surfaced when this stage was first run and are
handled explicitly. Penge and Willesden have an identically zero treated
pre-treatment outcome path, so their pre-treatment RMSPE is near zero and the
standardized effect `S_e` explodes to between 8 and 12, dominating the mean
`S-bar`. This is the scale problem the plan warns about in section 4, and the
plan's own normalization is undefined here because the treated pre-period mean
is the zero denominator. The normalized fit measure therefore divides by the
mean absolute pre-treatment outcome of the whole event panel, treated unit plus
its 30 donors, which stays positive and uses no post-treatment information; the
treated-only version is retained beside it as
`nrmspe_pre_treated_scale`. In addition an `informative_pre_treatment_path`
variant drops events whose treated pre-period outcome is identically zero and is
reported alongside the baseline, with the same screen reproduced inside the
randomization draws. Because the pre-specified pooled statistic is the mean
percentile rank, which is bounded, the headline result does not depend on the
standardized-effect blow-up; both the mean and the median `S` are reported so the
influence is visible.

The mean percentile rank also does not have a null expectation of exactly 0.5.
An observed rank divides by `J + 1 = 31` while a placebo rank divides by 30, so
under exchangeability both centre on `16/31 = 0.516`. The observed joint
randomization nulls land at 0.521 to 0.552, consistent with that value, so the
two-sided p-values are centred on the empirical null mean of the draws rather
than on 0.5; the one-sided p-value is reported as well because it does not
depend on the centring. `dz_qc.csv` records the null mean, the theoretical
`16/31` centre, and the deviation between them.

Robustness covers progressive trimming, leave-one-event-out, leave-shared-donor-out
for the most influential donors by cumulative weight, placebo-in-time at minus
20 years, and the placebo-fit restrictions. Donor sharing is diagnosed rather
than prohibited: the stage writes the number of synthetic controls using each
donor, cumulative donor weights, and the pairwise weighted overlap
`sum_j min(w_ej, w_e'j)` for every pair of treated events.

Outputs land under
`results/worlds_fair/synthdid/ukds_1911_lgd_wikipedia_donor_pools/match_v2_lagged/pooled_dz/`
as `dz_event_effects.csv`, `dz_event_gap_paths.csv`, `dz_placebo_events.csv`,
`dz_placebo_gap_paths.csv`, `dz_placebo_fit_restrictions.csv`,
`dz_pooled_att.csv`, `dz_pooled_dynamic_effects.csv`,
`dz_progressive_trimming.csv`, `dz_leave_one_event_out.csv`,
`dz_leave_donor_out.csv`, `dz_placebo_in_time.csv`, `dz_donor_usage.csv`,
`dz_donor_overlap_matrix.csv`, `dz_randomization_draws.csv`, the four
presentation tables, `dz_pre_analysis_decisions.csv`, `dz_qc.csv`,
`dz_models.rds`, and six figures per specification under
`figures/<model_id>/`. The four specifications are `lead0_standard`,
`lead0_alternative`, `lead20_standard` and `lead20_alternative`.

```powershell
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_dube_zipperer.R
```

Defaults can be changed with `SYNTHDID_DZ_RANDOMIZATION_DRAWS` (1,000),
`SYNTHDID_DZ_INFLUENTIAL_DONORS` (5) and `SYNTHDID_BASE_SEED`. The stage also
runs unchanged against the original `pooled_rate_density` pools, which serves as
the plan's section 18C alternative-donor-pool comparison; those outputs live in
the sibling `pooled_dz/` directory under the donor-pool root.

The event effect was subsequently changed from the plan's raw mean
post-treatment gap to the synthdid ATT, for the reason set out under "The
estimand" below, and every tree was re-estimated on that basis. The
consolidated results for all three trees are in the "Results" table further
down. Do not read pre-correction numbers from this stage's history: they were
computed on the superseded estimand.

The stage also runs unchanged against the original `pooled_rate_density` pools,
which serves as the plan's section 18C alternative-donor-pool comparison; those
outputs live in the sibling `pooled_dz/` directory under the donor-pool root.

## Corrected balanced 3-pre / 4-treated event window (preferred)

The preferred UK specification implements the intended lead-20 interpretation:
the effective treatment cohort is the fair cohort minus twenty years, treatment
starts at event time `e = 0`, and the fair occurs at `e = +20`. Its seven-bin
window has untreated observations at `e = -30, -20, -10` and treated
observations at `e = 0, +10, +20, +30`. The final untreated reference is
therefore `e = -10`, and the event effect is

```text
g_eh  = gap_eh - gap_e,-10
tau_e = mean(g_eh for h in {0, 10, 20, 30}).
```

This geometry is registered as `balanced_3pre_4post`. Its matching companion is
`balanced3_rate_growth_density_e0`, written under `match_v4_balanced_e0/`. The
five matching variables remain the outcome at `e = -30, -20, -10`, population
CAGR from `e = -30` to `e = -10`, and log density at `e = -10`. Hence all
matching inputs are untreated. The 30 selected donor IDs, ranks, and distances
are exactly unchanged from the corresponding `match_v3_balanced` pools; the
builder asserts that equality and records
`legacy_balanced_pool_ids_ranks_distances_match = TRUE` in donor-pool QC.

The complete window leaves 29 timing-eligible model-event cases: 10 and 9 for
the lead-0 alternative and standard models, and 5 under each lead-20 definition.
The unchanged informative-pre-panel gate excludes Wembley in both lead-20
models because its three untreated outcome observations are identically zero.
Thus the preferred lead-20 pooled event studies each retain **four events**;
the preferred four-model pooled run estimates 27 of 29 eligible cases and all
810 requested in-space placebo fits.

With 1,000 joint randomization draws, the preferred lead-20 results are:

| Decades | Events | Pooled effect | Mean rank | Rank p | Any-treated effect p | Any-treated rank p |
|---|---:|---:|---:|---:|---:|---:|
| Standard | 4 | -0.056 | 0.161 | 0.008 | 0.294 | 0.085 |
| Alternative | 4 | -0.053 | 0.347 | 0.213 | 0.508 | 0.386 |

The standard-decade rank rejection is negative. Neither definition rejects the
joint effect-scale test across the four treated horizons, neither has an
unusual corrected pre-horizon, and neither supports a positive fair effect.
Progressive-trimming figures stop after two deletions so every displayed stage
retains at least two events. Hammersmith is removed first in both definitions;
the next removal is Islington under standard timing and Bristol under
alternative timing.

For design decomposition, the corrected window imposed on the original
`match_v2_lagged` pools retains five lead-20 events per definition and produces
effects of 0.169 (standard) and 0.185 (alternative). Rebuilding from the fixed
five-variable match makes Wembley infeasible and changes both preferred effects
to about -0.05. This is reported as donor-pool sensitivity, not as evidence for
a positive effect. The legacy full-window outputs remain untouched for audit.

```powershell
# Rebuilt five-variable pools and per-event estimates.
$env:SYNTHDID_MATCHING_SPEC = "balanced3_rate_growth_density_e0"
$env:SYNTHDID_COHORT_LEAD_YEARS = "0"
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R
$env:SYNTHDID_COHORT_LEAD_YEARS = "20"
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_dube_zipperer.R

# Corrected window on the original lagged donor pools.
$env:SYNTHDID_MATCHING_SPEC = "lagged_rate_growth_density"
$env:SYNTHDID_EVENT_WINDOW = "balanced_3pre_4post"
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_dube_zipperer.R

# Recreate the manuscript composites and generated tables.
Rscript analysis/world_fairs/render_worlds_fairs_synthdid_paper_figures.R
```

Preferred pooled outputs live under
`match_v4_balanced_e0/pooled_dz/`; corrected-window/original-pool robustness
outputs live under `match_v2_lagged/pooled_dz_balanced_3p4/`. The renderer reads
those two trees plus the archived full-window results and writes the three UK
lead-20 manuscript figures and the two synthetic-DiD tables.

## Legacy balanced 4-pre / 3-post window (superseded, retained for audit)

The material below documents the earlier `match_v3_balanced` interpretation.
It treated `e = 0` as untreated and began treatment at `e = +10`, so it is not
the lead-20 specification reported in the paper. Its code paths and outputs are
retained to make the correction auditable; do not use its estimates or timing
language as the current design.

The full-window design estimates every event on all twelve 1850-1960 decade
bins, so `T_0` runs 3 to 10 and `T_1` runs 2 to 9. Two things follow that the
balanced window fixes. The event effect is the mean gap over all available post
decades, so Liverpool contributes nine post bins and Wembley four, and the
dynamic `ATT_h` thins out at the tails until the extreme horizons rest on one or
two events. Separately, with one matched lag per available pre bin the outcome's
share of the equal-per-column matching distance is set by column count, so it
varies from 3 of 5 columns to 10 of 12 across events, and the population CAGR
spans 20 years for one treated unit and 90 for another.

The balanced window, selected with `SYNTHDID_EVENT_WINDOW=balanced_4pre_3post`,
uses four pre bins at offsets -30, -20, -10 and 0 decades and three post bins at
+10, +20 and +30, relative to the fair decade `T`. The fair decade is the **last
pre-treatment period**, so the treatment indicator turns on at `T+10`. That
requires `T` in `[1880, 1920]`, since `T-30` must reach no earlier than the 1850
bin and `T+30` no later than 1950. This keeps the 9-year 1850 bin, which is
comparable because the outcome is annualized per person-year, and excludes the
2-year 1960 bin. All geometry is defined once in
`analysis/world_fairs/synthdid/ukds_1911_lgd_matching_spec.R`
(`wf_event_window_spec` and friends) and read from there by the builder, the
per-event estimator and the pooled stage.

Of the 36 estimated full-window events, **29** survive: 10 and 9 at the zero
lead, 5 and 5 at the 20-year lead. Lambeth drops at both leads because its post
window would run past 1950 (`T` = 1950 and 1930), and the `T` = 1870 cases drop
for want of three pre-fair bins (Cardiff under both definitions and alternative
Newcastle upon Tyne). Liverpool is retained: its first fair is 1886, so `T` =
1880 with pre bins 1850/1860/1870, and the balanced window adds 1880 as a
fourth. Liverpool is absent at the 20-year lead for an unrelated reason that
already applies in the full-window run, namely that its effective cohort year
1866 leaves a single pre bin.

The matching specification `balanced3_rate_growth_density`, written under
`match_v3_balanced/`, is the fixed-dimension companion to that window. It
matches on exactly three strictly pre-fair lags at `T-30`, `T-20` and `T-10`, a
uniform 20-year population CAGR over that same span, and log density at `T-10`
(which is the existing baseline decade). That is five z-scored columns for every
treated unit, so the outcome's share of the distance is 3/5 everywhere and the
growth covariate is the same construct across events. The fair decade is
deliberately excluded from donor selection even though estimation treats it as
pre, so no matching covariate is contaminated by post-fair births. The
specification implies the balanced window and refuses to run on any other.

Three trees are produced so the window change and the matching change can be
told apart:

| Tree | Pools | Window | Directory |
|---|---|---|---|
| Full window | `lagged_rate_growth_density`, 4-12 columns | all 12 bins | `match_v2_lagged/pooled_dz/` |
| Balanced, same pools | `lagged_rate_growth_density`, 4-12 columns | 4 pre / 3 post | `match_v2_lagged/pooled_dz_balanced_4p3/` |
| Balanced, rebuilt pools | `balanced3_rate_growth_density`, 5 columns | 4 pre / 3 post | `match_v3_balanced/` |

The per-event stage was re-run only for the rebuilt-pool tree, which therefore
also has forest figures and 200-replication `placebo_vcov` intervals. The
middle tree is pooled-only, which is sufficient because the pooled stage
estimates every event itself.

```powershell
# rebuilt 5-column pools, balanced window, per-event stage and pooled stage
$env:SYNTHDID_MATCHING_SPEC = "balanced3_rate_growth_density"
$env:SYNTHDID_COHORT_LEAD_YEARS = "0"
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R
$env:SYNTHDID_COHORT_LEAD_YEARS = "20"
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_dube_zipperer.R

# existing pools, balanced window, pooled stage only
$env:SYNTHDID_MATCHING_SPEC = "lagged_rate_growth_density"
$env:SYNTHDID_EVENT_WINDOW = "balanced_4pre_3post"
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_pooled_dube_zipperer.R
```

The pooled stage no longer depends on the per-event stage. It applies the same
zero-pre-period-variance feasibility gate itself and derives its event list from
the donor pools and the timing manifest, so a window with no per-event run can
still be pooled; `dz_event_feasibility.csv` records the verdict per event. When
the per-event outputs exist for the same window the refit is still reconciled
against them, and `dz_qc.csv` records which mode was used. On the full-window
lagged tree the independently-derived gate reproduces the per-event stage
exactly, agreeing that standard Willesden is the single infeasible event.

### Progressive trimming now carries inference at every step

The Dube-Zipperer baseline is the full treated-event sample, with progressive
trimming reported alongside rather than used as a screen; picking a single fit
cutoff after seeing results is what section 23 warns against. The trimming path
ranks events by normalized pre-treatment RMSPE and drops the worst first, and
the joint randomization p-value is now computed at **every** step rather than
only the first two, with the screen reproduced inside each of the 1,000 draws as
section 17 requires. `dz_progressive_trimming.csv` gained
`randomization_p_rank`, `randomization_p_att`, `randomization_null_mean` and the
usable-draw count per step, and table 4 carries the trim p-values.

### The estimand: why it is the synthdid ATT, not the raw post gap

The plan defines the event effect as the mean post-treatment gap `Y - Yhat(0)`,
which presumes a counterfactual whose pre-treatment gap is essentially zero.
Classical synthetic control delivers that; synthetic DiD does not. `synthdid`
deliberately permits a level offset between the treated unit and its synthetic
control and removes it inside its own ATT through the lambda time weights. On
this panel that offset is large: the pooled pre-treatment gap runs about +0.03
to +0.09, driven mostly by Wembley (+0.42 and +0.48 at the zero lead). The raw
mean post gap therefore reports the standing offset as though it were a change
at treatment, and at both zero-lead specifications it disagrees in sign with the
estimator's own ATT.

Every statistic in this stage consequently uses

```text
tau_e = (mean post treated - mean post synthetic)
        - sum_t lambda_t (pre-treatment gap at t)
```

which is exactly the contrast `synthdid` optimises. The raw mean post gap, the
mean pre gap and the lambda-weighted pre gap are all retained beside it as
diagnostics in `dz_event_effects.csv`. The event-time path is reported as the
gap less that same offset, so the pre-treatment path is centred by construction
and averaging the adjusted path over the post periods reproduces `tau_e`
exactly. `dz_qc.csv` asserts both identities, and under the balanced window it
additionally asserts that the pooled path integrates to the pooled ATT. That
last identity holds only when every event contributes the same post periods,
which is one more thing the balanced window buys.

A related labelling bug was fixed at the same time: the pooled dynamic table had
been treating event time 0 as post-treatment, which is wrong under the balanced
window where the fair decade is the last pre-treatment period. The period label
and every event-study boundary line now follow the treatment start.

### Event studies at every trim stage

`dz_pooled_dynamic_by_trim.csv` and
`figures/<model_id>/dz_fig7_trim_event_studies.png` give the pooled event-time
path recomputed at each progressive-trim stage, one facet per stage, with a band
showing the across-event dispersion rather than an inferential interval. Because
the balanced window fixes the event-time grid, every stage is directly
comparable; under the full window the grid is legitimately ragged, and the QC
table records that rather than enforcing a common grid.

### Results

With 1,000 joint randomization draws, no specification in any tree comes close
to its joint null, and after the estimand correction most point estimates are
slightly negative:

| Specification | Tree | Events | Pooled ATT | Mean rank | p |
|---|---|---:|---:|---:|---:|
| `lead0_standard` | full window | 10 | -0.056 | 0.526 | 0.828 |
| `lead0_standard` | balanced, same pools | 9 | -0.046 | 0.498 | 0.744 |
| `lead0_standard` | balanced, rebuilt pools | 9 | -0.017 | 0.520 | 0.977 |
| `lead0_alternative` | full window | 11 | -0.056 | 0.528 | 0.774 |
| `lead0_alternative` | balanced, same pools | 10 | -0.050 | 0.465 | 0.483 |
| `lead0_alternative` | balanced, rebuilt pools | 10 | -0.024 | 0.561 | 0.733 |
| `lead20_standard` | full window | 7 | 0.027 | 0.553 | 0.765 |
| `lead20_standard` | balanced, same pools | 5 | 0.083 | 0.484 | 0.747 |
| `lead20_standard` | balanced, rebuilt pools | 5 | -0.036 | 0.387 | 0.331 |
| `lead20_alternative` | full window | 8 | -0.006 | 0.520 | 0.918 |
| `lead20_alternative` | balanced, same pools | 5 | 0.171 | 0.503 | 0.867 |
| `lead20_alternative` | balanced, rebuilt pools | 5 | 0.194 | 0.471 | 0.756 |

Every baseline mean rank now sits between 0.39 and 0.56, within a few
hundredths of the 16/31 null centre, and no baseline p-value falls below 0.33.
The mild positive tilt visible before the estimand correction was the
pre-treatment level offset rather than a treatment effect.

That statement is about the baseline all-events samples only, and the trimming
paths deserve a separate sentence because they are not uniformly quiet. Nine
trim steps across the three trees return p below 0.05, and every one of them is
**negative**, with pooled ATTs from -0.08 to -0.14. Most sit at two or three
retained events, where nothing can be concluded. The two exceptions are on the
full-window tree at `lead0_standard`, which gives -0.070 with p = 0.029 after
dropping the five worst-fitting of ten events and -0.073 with p = 0.039 after
dropping six; `lead0_alternative` there reaches p = 0.051 at six retained
events. The balanced trees show the same sign but never get below p = 0.33 at
five or more retained events.

The consistent direction is worth recording: removing poorly-fitted events
pushes the estimate negative rather than positive, in every tree. It is not a
finding. It rests on half the sample or less, it is strongest on the tree with
the worst pre-treatment fit and a ragged event-time grid, and it is not the
pre-specified baseline. What it does rule out is any reading in which trimming
to the credible events reveals a positive effect of hosting a fair.

The one remaining positive point estimate of any size is `lead20_alternative` at
+0.194, and the trim-stage event studies show exactly what it is: a single +0.53
spike at event time +10 that falls to +0.009 as soon as Wembley is dropped.
Wembley is simultaneously the largest effect and the worst-fitting case, carrying
a normalized pre-RMSPE of 15.0 at `lead20_standard`. With five events and a mean
rank of 0.471 there is nothing there to interpret.

This is consistent with the broader-determinants framing: hosting a world's fair
does not produce a detectable effect on local Discovery/Science birth rates under
any of the three donor-pool and window combinations.

### Method in brief

The sections below were written as each stage was built, and two estimands were
superseded along the way. This is the pipeline as it now stands, in order. Every
detail here is expanded in one of the subsections that follow.

The design question is that roughly thirty LGDs host their first fair at
staggered dates between 1880 and 1920. A single pooled regression would impose
one common counterfactual on all of them, and a single stacked `synthdid` would
let one global donor-weight simplex serve every treated unit. Dube-Zipperer
instead builds a bespoke counterfactual for each event and pools the *results*.
The price of that choice is that the events are not independent, because they
draw donors from overlapping pools, and that is what dictates the form of the
inference at step 8.

1. **Geometry and eligibility, fixed first.** Four pre-treatment decade bins at
   offsets -30, -20, -10 and 0 from the fair decade `T`, and three post bins at
   +10, +20 and +30. Treatment turns on at `T+10`, so **the fair decade is the
   last pre-treatment period**. That requires `T` in `[1880, 1920]` and retains
   29 of 36 events. An event is dropped if its pre-treatment block carries no
   informative variation.

2. **Donor pools, from pre-treatment information only.** Candidates must be
   never-host, at least 50 km from the treated unit's fair venue, and have valid
   covariates, which leaves roughly 1,600 to 1,740 per event. They are ranked by
   equal-weight Euclidean distance over five z-scored columns — three lagged
   outcome bins at `T-30`, `T-20`, `T-10`, the 20-year population growth rate,
   and log baseline density — and the nearest 30 form the pool. Pools are fixed
   before estimation touches anything post-treatment.

3. **One synthetic DiD fit per event.** Each event is a 31-by-7 matrix: its 30
   donors plus itself, over the seven window bins. The event study is *derived*
   from that fit as the gap path `Y - Yhat(0)` in event time; it is not
   estimated as a separate regression.

4. **The event effect is the synthdid ATT**, that is the mean post-treatment gap
   *less* the lambda-weighted pre-treatment gap — **not** the raw post-treatment
   gap. Synthetic DiD deliberately permits a level offset between treated unit
   and synthetic control and removes it inside its own ATT, where classical
   synthetic control would have driven that offset to zero. On this panel the
   pooled pre-treatment gap runs about +0.03 to +0.09, and using the raw gap
   reversed the sign at both zero-lead specifications.

5. **`S_e = tau_e / RMSPE_pre_e` is a standardized effect, not an elasticity.**
   Numerator and denominator share units so it is dimensionless, but it measures
   signal against pre-treatment prediction error rather than any ratio of
   proportional changes. The denominator is a **fit** measure — the root mean
   squared pre-treatment gap — not a standard error, so `S_e` has no `t`
   reference distribution and is read only against its placebo distribution.

6. **In-space placebos are exhaustive and deterministic, not a bootstrap.** Each
   of the 30 donors takes a turn as the treated unit, is removed from its own
   pool, and is fitted against the remaining 29 with the real treated unit
   dropped. That yields exactly 30 placebo values per event, so
   `r_e = (1 + #{S_placebo <= S_e}) / 31`.

7. **Pooling has two separate halves, and conflating them is the easiest
   mistake to make.** Plan §12 is explicit about the split. *Magnitude* is the
   equal-weighted mean of `tau_e` across events, in outcome units, and ranks
   play no part in it. *Evidence* is the mean percentile rank `r-bar`, which is
   the pre-specified inference statistic precisely because it is bounded and
   scale-free, so no single outlying event can move it.

8. **Inference is a joint randomization, not combined per-event p-values.**
   Because donor pools overlap, the event estimates are dependent and §23 rules
   out combining their p-values under independence. Instead 1,000 draws each
   permute unit identities *jointly* across events, preserving treatment timing,
   donor eligibility, pool overlap and the repeated use of the same physical
   LGD. `r-bar` is compared against that empirical null, whose centre sits near
   0.52 rather than 0.5 because ties inflate ranks under the `<=` indicator.

9. **Screening, dynamics and robustness.** The baseline retains every estimated
   event; progressive trimming on normalized pre-treatment fit is reported with
   a p-value at each step and reproduced inside every draw. The dynamic layer
   gives `ATT_h` and `r-bar_h` at each of the seven horizons with their own null
   bands, which is what the main figure shows. Robustness covers
   leave-one-event-out, leave-shared-donor-out, placebo-in-time, and
   placebo-fit restrictions, and donor sharing is diagnosed rather than
   prohibited.

In one sentence: build a bespoke synthetic control per fair from pre-fair data
only, divide each event's ATT by its own pre-treatment fit to get a scale-free
statistic, rank that against 30 in-space placebos, then pool the *effects* for
magnitude and the *ranks* for evidence, testing the latter against a joint
permutation that preserves donor sharing across events.

`dz_pre_analysis_decisions.csv` in each output directory records all of these
choices in the plan's §26 format, derived from the active configuration rather
than restated, so it describes the run it belongs to.

### Which figure is the result

`figures/<model_id>/dz_fig3_pooled_event_time.png` is the main reported outcome.
It has two stacked panels, keeping the plan's own separation of pooled magnitude
from pooled evidence:

- **Top panel, plan §12.1.** The equal-weighted pooled effect `ATT_h` in outcome
  units, at each of the seven event times, with a joint-randomization null band
  and a two-sided p per horizon.
- **Bottom panel, plan §12.2.** The mean placebo percentile rank `r̄_h` per
  horizon, bounded and scale-free, with its own null band and a reference line
  at the exchangeable centre `16/31`.

Both shaded ribbons are **null bands**, showing where the statistic would fall
under no effect, from the 2.5th to 97.5th percentile of the 1,000 joint
randomization draws (§15, §16). They are not confidence intervals and the
subtitle says so. The dashed vertical line is the treatment start, at event time
`+10` under the balanced window because the fair decade is the last
pre-treatment period.

The per-horizon statistics mirror the scalar definitions exactly. Each unit's
adjusted gap is divided by its **own** pre-treatment RMSPE, so
`S_{e,h} = g̃_{e,h} / RMSPE_pre_e` for the treated unit and
`S^P_{ej,h} = g̃^P_{ej,h} / RMSPE_pre_ej` for placebo `j`; ranks then use the
same `(1 + #{S^P <= S}) / (J + 1)` convention as `r_e`. `dz_qc.csv` asserts the
reduction: averaging `S_{e,h}` over an event's post horizons reproduces its
scalar `S_e` to machine precision, which is the strongest single check that the
horizon layer is wired to the same apparatus.

Supporting figures, in the order they are usually needed:

| Figure | Role |
|---|---|
| `dz_fig5_trimming.png` | Pooled ATT against the number of worst-fitting events dropped, with a p-value at every step |
| `dz_fig6_randomization.png` | The pooled joint-randomization distribution against the observed mean rank |
| `dz_fig4_placebo.png` | Observed versus placebo standardized effects, per event |
| `dz_fig1_paths.png`, `dz_fig2_gaps.png` | Per-event treated-versus-synthetic trajectories and gaps |
| `dz_fig7_trim_event_studies.png` | The event study recomputed at every trim stage — a **robustness diagnostic** |

`dz_fig7` should not be presented as the estimate. It is a sensitivity sweep, so
elevating it would invert the plan's logic: §24 step 3 reports the full treated
sample and §5 warns against promoting a chosen fit cutoff. Its deep facets also
rest on two or three events while taking the same visual weight as the
informative ones. It now carries the same null bands as the main figure, so
nothing on it reads as a confidence interval.

### Reading the per-horizon p-values

Seven horizons are not seven independent tests. The per-horizon p-values in
`dz_pooled_dynamic_effects.csv` are uncorrected, and the multiplicity-aware
statistic is the joint "any post-treatment horizon" test in `dz_pooled_att.csv`,
reported on both scales as `randomization_p_any_post_horizon` and
`randomization_p_any_post_horizon_rank`. It compares the observed
`max |Q_h − c|` across post horizons with the same statistic in every draw.

Two properties of the null are worth knowing before reading the bands.

First, the rank null does not sit exactly at `16/31`. Observed per-horizon rank
nulls run from about 0.51 to 0.63. Two features of the design push them up: ties
among placebo statistics, which the plan's `<=` indicator counts in full, and
the joint assignment, which is deliberately non-uniform so that donor sharing is
preserved (§16). Ties are the larger driver and are heaviest at early horizons,
where many donor LGDs record no births at all — a mean of 6 to 10 of the 30
placebo values are tied at the earliest horizon, and the per-horizon tie count
correlates 0.74 with the rank null mean. This is harmless because every p-value
is centred on its own empirical null rather than on a theoretical constant;
`dz_qc.csv` records the deviation and the tie count instead of asserting them.

Second, the `ATT_h` null mean need not be zero at individual pre-treatment
horizons, because the lambda-weighted pre gap is zero per event rather than
horizon by horizon. It is reported as a diagnostic.

### What the per-horizon results show

With the corrected estimand, the two panels separate cleanly, and the
separation is itself the finding.

Both zero-lead specifications are quiet everywhere. In `match_v3_balanced` no
horizon, pre or post, reaches p < 0.05 on either scale; the joint any-horizon
p-values are 0.59 and 0.89 on effects and 0.20 and 0.14 on ranks. The observed
path sits inside the null band at all seven horizons in both panels.

The two 20-year-lead specifications show apparent effect-scale significance that
the rank panel does not corroborate, and the pre-treatment horizons give the
reason. `lead20_alternative` has one post-treatment horizon below 0.05 on the
effect scale (event time +10, the Wembley spike) and a joint effect p of 0.001,
against a joint rank p of 0.65 and no significant horizon on the rank scale.
`lead20_standard` is starker: **three of its four pre-treatment horizons** fall
below 0.05 on the effect scale, alongside two post-treatment horizons, with a
joint effect p of 0.005 — while the rank scale flags nothing anywhere, pre or
post, and returns a joint rank p of 0.38.

Significant pre-treatment horizons cannot be a treatment effect. They say the
effect-scale inference is not trustworthy for those two specifications, which
have only five events each and include Wembley, whose adjusted gap dwarfs the
others. This is exactly the failure mode the bounded rank statistic exists to
catch, and it is why §12.2 makes the mean percentile rank the pooled inference
statistic rather than the ATT.

On the rank scale, **both balanced trees have no significant horizon at all**,
pre or post, in any of their four specifications: 0 of 28 cells each. The effect
scale flags 6 of 28 on the rebuilt-pool tree, all inside the two five-event
20-year-lead specifications, and none on the same-pools tree.

The full-window tree is the exception, and it is instructive. It flags 7 of its
74 horizon cells on the rank scale and 13 on the effect scale, but six of the
seven rank flags sit at event times -100, -90, -80 and -60, where the pooled
path rests on a **single event**, and the seventh at +80 rests on three. Those
are the ragged tails the balanced window exists to remove: with one event the
pooled rank is simply that event's own rank, so the pooled statistic is not
pooling anything. This is the clearest single argument for reporting the
balanced window rather than the full one, and it is why the verification suite
asserts the no-significant-horizon property only for the balanced trees while
reporting the full-window flags with their event counts attached.

### Two caveats recorded in the outputs

**Pre-treatment fit improves partly for mechanical reasons.** Four pre periods
against 30 donors is a very wide simplex, so fit improves without necessarily
meaning better counterfactuals. The pattern confirms this reading: the median
pre-RMSPE barely moves between trees (0.043 to 0.084 across all twelve
specification-tree cells), while the maximum falls sharply, from 1.66-2.37 under
the full window to 0.08-0.74 under the balanced window. The balanced window
mostly repairs the extreme cases rather than improving typical fit. Because the
in-space placebos face the same overfitting, the percentile ranks and joint
p-values remain the right yardstick, and raw pre-RMSPE comparisons against the
full-window run should not be read as progress.

**Donor turnover behaves exactly as the rule predicts**, which is a useful
correctness check. Comparing `match_v3_balanced` with `match_v2_lagged`, every
event that had exactly three available pre bins keeps an identical 30-donor pool
(Jaccard 1.000: Liverpool, Penge and Willesden at the zero lead, Wolverhampton
and Hammersmith at the 20-year lead), because for those events "all pre bins"
and "the last three pre bins" are the same set. Turnover then rises with the
number of bins the old rule used, up to almost complete replacement for Wembley
(3 of 30 retained at the zero lead, 1 of 30 at the 20-year lead), whose pool
change is what moves its fit so much. Overall 20.4 of 30 donors are retained on
average, Jaccard 0.588 - far less churn than the v1-to-v2 change, as expected
since both rules use lags, growth and density and differ only in the lag count.

The degenerate-path picture also improves. Only one of the 29 balanced events
has an identically zero treated pre-treatment path (alternative Willesden),
against three of 36 under the full window, because the fair decade contributes
a non-zero outcome for Penge and Wembley. Five events are identically zero
across the three *matched* lags (Penge under both definitions, alternative
Willesden, and Wembley under both definitions at the 20-year lead); their lag
columns are weakly informative rather than degenerate, since donor variance is
unaffected.

Under the balanced window the placebo-in-time test cannot preserve the 4/3
geometry, since shifting treatment back inevitably shortens both sides. It is
run at a 20-year shift on a reduced 2-pre / 2-post window and every row records
`window_note` describing the reduction, so it is never mistaken for the main
design.

`analysis/world_fairs/augsynth/estimate_ukds_1911_lgd_wikipedia_multisynth.R`
provides a separate pooled augmented-synthetic-control analysis using
`augsynth::multisynth`. It estimates the same four host-treatment timing and
decade specifications, retaining the lead-0 Lambeth exclusion and the lead-20
event window through +30. The outcome is annualized Wikipedia
Discovery/Science births per 100,000 population. All 14 fixed-1851 HC, SC, and
children-per-teacher fields enter the native multisynth auxiliary balance
problem; the package's unit/time fixed-effects augmentation remains active.
LGDs below 95-percent baseline coverage are excluded, while donor eligibility
follows multisynth's native staggered-adoption rule rather than the earlier
30-LGD pools.

The pipeline pins `augsynth` to commit
`7a90ea48877fae7925a72cb50bc03a315bc7c042`. Install it once with:

```r
remotes::install_github(
  "ebenmichael/augsynth@7a90ea48877fae7925a72cb50bc03a315bc7c042",
  dependencies = TRUE,
  upgrade = "never"
)
```

Then run:

```powershell
Rscript analysis/world_fairs/augsynth/estimate_ukds_1911_lgd_wikipedia_multisynth.R
```

Wild-bootstrap inference uses 1,000 replications by default and can be changed
with `AUGSYNTH_BOOTSTRAP_REPLICATIONS`; `AUGSYNTH_BASE_SEED` controls the
deterministic model-specific seeds. Results, donor weights, pre-outcome and
auxiliary-covariate balance, serialized models, QC, and figures are isolated
under `results/worlds_fair/augsynth/ukds_1911_lgd_wikipedia_multisynth/` and do
not overwrite the pooled synthetic-DiD outputs.

The 50 km radius extension changes exposure from hosting to proximity. An LGD
is exposed when the minimum distance from its fixed-1911 polygon to a reviewed
UK fair venue is at most 50 km; venues inside a polygon therefore have distance
zero. The first such fair sets treatment timing. First exposure through 1870
is pre-treatment contamination and is excluded, while LGDs never within 50 km
of any fair form the donor universe. The current venue history partitions the
1,834 LGDs into 293 post-1870 treated, 644 pretreated, and 897 never-exposed
units. When rebuilt, the donor pools use the active 30-neighbor pooled
Wikipedia-scientist-rate and population-density ranking. Radius outputs are
isolated under `radius_50km/`; the 20-year timing version is nested under
`radius_50km/cohort_lead_20y/`. The existing radius outputs were intentionally
left unchanged during the host-first rate revision and therefore retain the
earlier cumulative-scientist-count matching rule until that extension is
rebuilt.

```powershell
$env:SYNTHDID_TREATMENT_MODE = "radius"
$env:SYNTHDID_TREATMENT_RADIUS_KM = "50"
$env:SYNTHDID_COHORT_LEAD_YEARS = "0"
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R

$env:SYNTHDID_COHORT_LEAD_YEARS = "20"
Rscript analysis/world_fairs/synthdid/build_ukds_1911_lgd_wikipedia_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_ukds_1911_lgd_wikipedia_synthdid.R
```

The radius estimator writes a separate forest figure for each first-exposure
year. Because the unit-level radius design is much larger, it also supports
deterministic checkpoint-only process shards with `SYNTHDID_SHARD_COUNT`,
`SYNTHDID_SHARD_INDEX`, and `SYNTHDID_CHECKPOINT_ONLY=TRUE`. After all shards
finish, rerun once with the default single shard and checkpoint-only disabled;
the script resumes completed models and assembles the validated combined
outputs. Checkpoints are reused only when the treatment configuration, seeds,
replication count, panels, and donor inputs match exactly.

With the current data and 200 placebo replications, the zero-lead radius run
estimates 150 of 586 requested LGD/definition models; the remaining 436 have
zero informative pre-period outcome variation. The 20-year lead has 403
timing-eligible models, estimates 79, classifies 324 as zero-variation
infeasible, and records 183 timing skips (five without a pre-period and 178
with only one). Neither run has an estimation failure, and every estimated
model completes all 200 dynamic placebo replications.

The notes below describe archived helper and diagnostic scripts in
`legacy/numbered_03_21/`. They document how the current inputs were assembled,
but they are not active pipeline steps.

The census downloader stores original and normalized France/United Kingdom
files under `input/worlds_fairs/city_census/` in Dropbox. It keeps only
observed census years through 1960, does not interpolate missing years, and
records Dublin and Cork as deferred until the Ireland collection is added.

The UK urban-population comparison script reads the manually supplied
Law-Robson-Bennett population and Settlement Points CSVs in the Dropbox
`city_census/GBR/` folder. It builds a geocoded 1801-1911 town panel and
compares the 1851-1911 overlap with Populations Past registration
sub-districts using reviewed name and historic-county matches.

The Nomis extension script downloads the official CR03 population tables and
metadata for the 1921, 1931, 1951, and 1961 censuses of England and Wales. It
matches urban administrative districts to the same 934-city universe, retains
ambiguous and unmatched cases in an audit file, and writes a combined
1801-1961 geocoded panel. There was no census in 1941.

The spatial matching script downloads historical district polygons from the
ONS Open Geography Portal and UK Data Service study 9321. It assigns Settlement
Points to census-year districts, links the polygons to Nomis populations, and
writes a separate spatially validated panel and full textual-versus-spatial
audit. It does not overwrite the earlier name-based panel.

The 1911-1921 audit uses the comparison population for 1911 published inside
the 1921 Nomis CR03 table. It decomposes each observed Law-Robson-to-Nomis
change into a 1911 source/geography difference and population growth within
the Nomis comparison geography. The audit is diagnostic and does not remove
or replace any existing match.

The UK historical urban-unit harmonization script uses 1921 Urban Districts,
Municipal Boroughs, and County Boroughs as the fixed target geography, with a
synthetic Greater London unit replacing fragmented London geographies. It
assigns Law-Robson settlements by point-in-polygon for 1801-1911 and allocates
Nomis district populations for 1921-1961 by polygon intersections weighted by
1911 target density. It also reads the raw cross-verified Wikipedia people
database, classifies STEM with `prep/stem_labels.R`, assigns UK birth points to
the same target polygons, and writes the processed inventor/scientist outcome
panel under `Data/processed/`.

The ukgeog parish feasibility script checks whether the current UK panel can
be rebuilt at parish level using `ukgeog`. Nomis metadata do contain
parish-like population units for 1921-1951 and parish/enumeration-district
units for 1961, but `ukgeog` does not expose parish boundary levels in its
metadata. The script therefore writes an audit report and lower-unit counts,
but it does not create a parish panel without an additional parish boundary
source.

The Nomis parish script extracts the observed primary `Civil Parish`/`Parish`
population rows from CR03 for 1921, 1931, 1951, and 1961, and writes related
parish/common-land/intersection units separately. This is useful for inspecting
the finest population units available in Nomis, but it is not a harmonized
geographic panel: `ukgeog` has been installed and checked, and it still does
not provide parish polygons.

For interactive inspection in RStudio, source the following script. It loads
the original Law-Robson table, the original Nomis CR03 tables, matched panels,
crosswalk, audit, coverage summary, and a direct 1911-1921 comparison:

```r
source("prep/world_fairs_panel/legacy/numbered_03_21/07_open_uk_nomis_law_robson_data.R")
```

Outputs continue to be written under the Dropbox paths defined in
`file.path(Sys.getenv("GTL_REPO"), "paths.R")`.

## US county AMWS pooled synthetic-DiD

The US pipeline pools the six validated single-fair county case studies under
standard decades and the same six under alternative decades. The pooled stage
uses only the corrected fixed specification: `e = [-30,+50]`, three untreated
periods, six treated periods, and `e = -10` as the reference. The outcome is
AMWS scientists born per 1,000 estimated county births. The maximum-preperiod
variant remains a raw-fit robustness exercise and is not pooled.

Each observed fair uses 30 donors selected from 875--1,836 event-specific
eligible never-host counties. The original 30 donors also define the 30
placebo-treated identities, but the comparison pool is rebuilt around every
placebo. For each pseudo-treated county, the pipeline excludes that county,
restandardizes the same three untreated outcome lags, population growth, and
population density on the remaining broad eligible universe, and selects a
fresh 30 controls. The focal fair's complete-window and 50 km venue exclusions
remain in force. The output records all 10,800 event-placebo-control mappings.

The primary effect is the equal-weighted mean reference-normalized gap. The
primary evidence statistic is the mean event-specific percentile rank of the
post-effect divided by pre-RMSPE. Pooled inference uses 1,000 joint assignments
per decade definition, prevents duplicate placebo-treated counties within a
draw, and repeats normalized-pre-RMSPE trimming inside each draw. Trimming
reports samples retaining six through two events.

Twenty-four rematched placebos have exact zero pre-RMSPE. A `1e-8` solver noise
floor recovers weights in those otherwise unidentified cases; their nonzero
post effects retain the mathematically corresponding signed infinite
standardized statistic and extreme percentile rank. They are not discarded.

| Decades | Events | Pooled effect | Mean rank | Rank p | Effect p |
|---|---:|---:|---:|---:|---:|
| Standard | 6 | -0.038 | 0.398 | 0.295 | 0.390 |
| Alternative | 6 | -0.020 | 0.505 | 0.960 | 0.730 |

The pooled stage passes all 12 validation checks: 12 observed fits, 360
successful rematched placebos, 30 unique controls and rank statistics per
event-placebo design, valid venue exclusions and weights, complete event-time
grids, 1,000 joint draws per definition, and progressive trimming from six to
two events. Results are isolated under:

```text
results/worlds_fair/synthdid/
  us_county_amws_figure5_lead20_e0_nonpooled/
    pooled_dz/
```

Run the fixed and maximum-preperiod case studies first, then pool and export:

```powershell
$env:US_WF_SYNTHDID_VARIANT = "fixed_m30"
Rscript analysis/world_fairs/synthdid/build_us_county_amws_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_us_county_amws_synthdid.R

$env:US_WF_SYNTHDID_VARIANT = "max_pre"
Rscript analysis/world_fairs/synthdid/build_us_county_amws_donor_pools.R
Rscript analysis/world_fairs/synthdid/estimate_us_county_amws_synthdid.R
Remove-Item Env:US_WF_SYNTHDID_VARIANT -ErrorAction SilentlyContinue

$env:US_WF_DZ_RANDOMIZATION_DRAWS = "1000"
$env:US_WF_DZ_CORES = "8"
Rscript analysis/world_fairs/synthdid/estimate_us_county_amws_pooled_dube_zipperer.R
Remove-Item Env:US_WF_DZ_RANDOMIZATION_DRAWS -ErrorAction SilentlyContinue
Remove-Item Env:US_WF_DZ_CORES -ErrorAction SilentlyContinue

Rscript analysis/world_fairs/export_us_amws_synthdid_project_artifacts.R
```

The exporter owns `project/chapters/us_synthdid_world_fairs.tex` and the
World-Fairs-only compatibility wrapper `project/wf_us_synthdid_figures.tex`.
It deliberately does not rewrite the general master `project/synth_dids.tex`.
That master uses one chapter per experiment and currently contains commented
hooks for Elite High Schools and Mandatory Schooling. Compile the general
figure-only volume with:

```powershell
Push-Location project
pdflatex -interaction=nonstopmode -halt-on-error synth_dids.tex
pdflatex -interaction=nonstopmode -halt-on-error synth_dids.tex
Pop-Location
```

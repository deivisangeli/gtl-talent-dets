# College-allocation outcomes with neighbouring birth counties

The separate [calendar-balanced extension](CALENDAR.md) follows Appendix A of the
Drive companion, using complete calendar decades rather than full event-time
coverage. It has 14 events, 43 sites and 154 member counties per timing after all
eleven shared-county exclusions. Both catchment versions below remain intact.

An additional [sensitivity below](#sensitivity-excluding-the-seven-shared-counties)
excludes all seven counties reused in the estimation samples. The original
catchment release described first remains unchanged.

This 2026-10-14 extension pools nearby birth counties around both allocated-college
and runner-up sites. It keeps the county-only release and all source datasets
unchanged. One original comparison site remains one analysis unit.

## Run

Set `TALENT_DETS_DATA_DIR` to the Dropbox project root, then run from the repository
root with installed `sf`, `readxl`, `data.table`, `digest`, `jsonlite`, `did`,
`dplyr`, `readr` and `ggplot2` packages:

```powershell
Rscript --vanilla prep/land_grants/03_build_amws_training_catchments.R
Rscript --vanilla analysis/land_grants/training_50km/run_catchment_event_studies.R
Rscript --vanilla analysis/land_grants/training_50km/validate_catchment_release.R
```

The builder and estimator use local staging outside Dropbox. Completed releases
are verified on rerun and are not overwritten. No downloads, identity research,
APIs, new geocoding or cross-edition person linkage are performed.

## Geography and assignment

All 57 experiments and 168 original sites are mapped before sample selection.
Sites use the selected and runner-up town coordinates in the existing Andrews
county-pairs workbook, including its existing county-level averages for multiple
runner-up towns. These are allocation-site reference coordinates, not newly
verified historical campus locations.

Birth-county centroids are geometric centroids of cached 2020 polygons projected
to EPSG:5070 and transformed to EPSG:4326. S2 great-circle distances determine
eligibility at an inclusive 50,000 metres. Each original county remains with its
original site, including four cases whose centroid lies outside that radius.
Every other eligible county is assigned once within each experiment to its
nearest site; exact ties use the stable site identifier. Original ownership has
priority over proximity. Assignments are fixed before denominator filtering;
excluded counties are not reassigned to a different site.

The full geographic union contains **622 counties**, including **454 additions**
to the original 168. The 851 candidate site/county memberships reduce to 652
unique experiment/county assignments. Counties can occur in different
experiments. They cannot contribute to two sites within the same experiment.

## Outcomes and denominators

The original AMWS eligibility, birth-year window 1830–1959, earlier-edition `kept`,
1986 contamination and cross-edition counting rules are preserved. County
geography, including canonical `1955:46642`, is not corrected. All 136,489 input
entries receive an eligibility audit; the original 12,360 eligible entries retain
their classifications.

BA/BS, MA, MS and PhD awards define ever nearby, never nearby with complete
conflict-free matching, and unknown training exactly as in the county-only
release. Nearby training is measured from **each scientist's own birth-county
centroid**, not from the allocation site. A known nearby institution takes
precedence over other incomplete awards. Unconditional IDs with missing degree
years remain usable. Existing conditional assignments, multiple institutions,
flags and provisional qualifications are preserved.

The geographic catchment is filtered separately for each timing. An added county
is excluded for every period at a site if any original frozen estimation row or
baseline period lacks a finite positive county birth denominator. Original
counties remain included. Catchment membership is constant over time within each
timing; it can differ between standard and alternative timing.

Existing county-decade denominators are reused exactly for every original county.
New counties use the existing NHGIS/manual population knots, linear interpolation
between observed knots without extrapolation, and the same national annual birth
rates. Decade birth estimates preserve the legacy sum over available annual
values, remaining missing when all years are unavailable. This convention can
produce partial annual coverage, which is explicitly audited. No HYDE values or
new extrapolation fill gaps. Alternative timing shifts years ending in 7–9
forward one decade.

Each site's count is the sum of member-county counts. Its rate is **1,000 times
the summed count divided by summed estimated births**; county rates are not
averaged. Baseline births are the pooled denominator at the existing g−10
baseline. Annual panels display missing pooled births if any member's annual
birth estimate is missing; decade models retain the legacy aggregation convention.

## Estimation, validation and interpretation

Eight CSDID models use the frozen county-only event/site/observation identifiers:
standard timing **16 events, 52 sites, 650 rows**; alternative timing **20 events,
62 sites, 834 rows**. The selected regression estimator, never-treated controls,
default inference conventions, balanced −20 to +70 support and influence-function
normalization to e = 0 are unchanged. Counts and the baseline-birth covariate now
describe catchments. No additional observations are admitted by newly available
pooled data. The separate simple ATT retains the manuscript's default
pre-treatment reference and is not an average of the normalized dynamic series.

Tests cover radius boundaries, home priority, nearest assignment and ties,
overlapping controls, cross-event reuse, fixed denominator support and pooled
rates. The independent validator reconstructs assignments, denominators, counts,
baseline controls, frozen samples and effect decomposition. Standard errors and
confidence limits are never added. Source/output checksums and copied files are
verified before completion markers are written last.

Cross-event county reuse can induce dependence between site outcomes. This
extension preserves the requested inference conventions and does not introduce
an additional adjustment for that dependence. County-centroid inclusion assigns
the whole county, including residents farther than 50 km from a site. Historical
campus accuracy of OpenAlex reference locations remains unverified; matching
incompleteness, provisional identities, career censoring and the existing
cross-edition counting convention also limit interpretation.

## Releases and audit fields

All eight models completed and passed the decomposition checks. The maximum
absolute discrepancy between the sum of category effects and the benchmark is
4.997e-16. The independent release validator also passed. Final analysis coverage
after fixed denominator screening is:

| Timing | Original counties | Catchment counties | Original unique entries | Expanded unique entries | Newly captured entries |
|---|---:|---:|---:|---:|---:|
| Standard | 52 | 202 | 3,923 | 6,781 | 2,858 |
| Alternative | 62 | 234 | 4,524 | 8,103 | 3,579 |

Entry counts refer to edition-specific source keys actually appearing in frozen
estimation periods, not cross-edition distinct people. The standard geographic
assignments initially cover 261 counties for retained sites; 63 added site/county
memberships fail support, leaving 209 memberships over 202 counties. Alternative
assignments initially cover 299 counties; 69 memberships fail support, leaving
241 memberships over 234 counties. Each timing reuses seven counties across
experiments. Counties excluded here remain in the geographic audit and are not
reassigned.

The full 622-county geographic preparation pool, before analysis support and
event restrictions, includes 45,081 eligible entries: 14,314 ever nearby, 18,289
never nearby and 12,478 unknown. This larger pool must not be confused with either
estimation sample.

At event time +60, the e = 0 normalized effects per 1,000 estimated births are:

| Outcome | Standard estimate [pointwise 95% CI] | Alternative estimate [pointwise 95% CI] |
|---|---|---|
| Ever nearby | 0.154 [0.031, 0.276] | 0.171 [0.076, 0.267] |
| Never nearby | 0.065 [−0.006, 0.137] | 0.026 [−0.056, 0.107] |
| Unknown training | 0.064 [−0.011, 0.139] | 0.095 [0.016, 0.174] |
| Total | 0.283 [0.122, 0.444] | 0.292 [0.127, 0.457] |

The corresponding county-only ever-nearby estimates were 0.242 and 0.253; total
estimates were 0.613 and 0.603. Expansion changes both outcome geography and the
baseline-birth covariate, so this comparison does not isolate a spillover effect.
These are selected dynamic coefficients, not overall average effects. Consult
the complete series and pre-treatment estimates; the pointwise intervals do not
provide a simultaneous test over event times.

All models retain warnings about small treatment cohorts. The alternative
ever-nearby model cannot return a pre-trend Wald statistic because its covariance
matrix is singular. Warnings are saved without changing the estimator or sample.

Preparation: `output/land_grants/amws_training_50km_catchments_20261014/`.
Results: `results/land_grants/event_studies/amws_training_50km_catchments_20261014/`.

- `allocation_sites.csv`, `catchment_candidates_and_decisions.csv`, geographic
  membership and cross-event reuse tables describe the 57-event geographic pool.
- `catchment_membership_<timing>.csv` identifies included and excluded members;
  `denominator_support_rows_<timing>.csv` records every screening observation.
- County and catchment annual/decade tables retain counts and denominators.
  In pooled panels `GEOID` identifies the original **anchor** county; the full
  geography is given by `site_id` and the member table, not by `GEOID` alone.
- `analysis_entry_membership_<timing>.csv` provides scientist-to-site provenance.
  `analysis_coverage_summary.csv` distinguishes unique entries, original-site
  entries, entries appearing in added counties, and entries only in added
  counties. Unique counts across these categories can overlap across experiments;
  use source-key unions rather than adding them.
- Matching coverage by edition, cohort and role, annual denominator completeness,
  exclusions, preserved original classifications and training evidence are saved.
- Results include model objects, coefficients, uncertainties, warnings, status,
  frozen samples, pooled baseline comparisons, county-only versus catchment
  dynamic coefficients, validation and PDF/PNG figures for both timings.

## Sensitivity excluding the seven shared counties

This separate release removes Franklin GA (`13119`), De Witt IL (`17039`),
Tazewell IL (`17179`), Spencer IN (`18147`), Warrick IN (`18173`), Daviess KY
(`21059`) and Henderson KY (`21101`) from **every** catchment. It removes both
appearances of each county, including original home memberships. Warrick and
Daviess were original runner-up counties; their site units survive using their
remaining surrounding counties. Anchor county identifiers remain unchanged as
site labels and do not imply that an excluded home county contributes outcomes.

Run from the repository root with `TALENT_DETS_DATA_DIR` set:

```powershell
Rscript --vanilla prep/land_grants/04_build_amws_training_no_reuse.R
Rscript --vanilla analysis/land_grants/training_50km/run_no_reuse_event_studies.R
Rscript --vanilla analysis/land_grants/training_50km/validate_no_reuse_release.R
```

Existing geographic assignments, denominator support decisions, county values,
training classifications and AMWS counting conventions are inherited unchanged.
No counties are reassigned. Counts, birth denominators, rates and baseline births
are recomputed after the exclusions. The same event/site/observation identifiers,
model settings, inference conventions, e = 0 reference and recorded model seeds
are retained. All eight models completed, and independent validation passed.

| Timing | Counties before → after | Unique AMWS entries before → after | Events / sites / observations |
|---|---:|---:|---|
| Standard | 202 → 195 | 6,781 → 6,636 | 16 / 52 / 650 |
| Alternative | 234 → 227 | 8,103 → 7,958 | 20 / 62 / 834 |

Each timing excludes 145 distinct edition-specific entries and 14 site/county
memberships. No county or entry contributes to different experiments in either
estimation sample. The full 57-event geographic preparation pool falls from 622
to 615 counties; other geographic overlaps outside these estimation samples can
remain. The input dataset and both earlier releases are unchanged.

At e = +60, effects per 1,000 estimated births are:

| Outcome | Standard estimate [pointwise 95% CI] | Alternative estimate [pointwise 95% CI] |
|---|---|---|
| Ever nearby | 0.148 [0.025, 0.272] | 0.165 [0.068, 0.262] |
| Never nearby | 0.057 [−0.014, 0.127] | 0.021 [−0.059, 0.102] |
| Unknown training | 0.067 [−0.007, 0.141] | 0.101 [0.023, 0.180] |
| Total | 0.272 [0.114, 0.429] | 0.288 [0.126, 0.449] |

The corresponding ever-nearby estimates in the shared-county version were 0.154
and 0.171. These selected coefficients describe one event time, not an overall
average. Full dynamic and simple-ATT comparisons are saved separately; the
simple ATT retains its default pre-treatment reference. Category effects sum to
the total within 7.217e-16; standard errors and confidence limits are not summed.

Small-treatment-cohort warnings remain in all eight models. The alternative
ever-nearby model still cannot return the joint pre-trend Wald test because of a
singular covariance matrix. Removing mechanical reuse does not resolve those
warnings. The OpenAlex reference-location and historical-campus qualifications
also remain unchanged.

New preparation and results folders use
`amws_training_50km_catchments_no_reuse_20261014` under the same respective Dropbox
parents. They include the excluded county list, removed memberships and entry
coverage, unchanged source classifications with explicit sensitivity eligibility
flags, rebuilt panels, model objects, sample audits, warnings and figures.
`shared_vs_no_reuse_dynamic.csv` and `shared_vs_no_reuse_simple.csv` compare the
two catchment versions without treating the difference as an independent estimate.
Publication checks verify prior releases and new copied-file checksums, writing
the completion marker last. The independent validator reconstructs the exclusions,
pooled outcomes, denominator and baseline sums, unchanged samples and decomposition.

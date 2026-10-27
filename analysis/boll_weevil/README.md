# Boll weevil, AMWS scientist generation, and county-level CS DID

## Run

From the repository root in PowerShell:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
Rscript analysis/boll_weevil/test_csdid_helpers.R
Rscript analysis/boll_weevil/analysis_boll_weevil_amws_csdid.R
```

Scripts resolve the repository through their own location and source `paths.R`;
absolute script paths work from other directories. Dependencies: R packages
`data.table`, `did`, and `ggplot2`. The initial run uses `did` 2.5.1.
Sources, other analyses, and previously built treatment/panel files are not
modified. Re-running replaces outputs only in this analysis's results folder.

## Inputs and measurement

Under the Dropbox project root:

- `output/boll_weevil/boll_weevil_counties_1920.csv`: Baker treatment timing,
  retreat restrictions, and checked nominal county identifiers.
- `output/us_panel_county_amws_combined_year.csv`: county birth-cohort counts,
  estimated county births, and scientists per 1,000 estimated births.

The outcome is `1000 * n_amws / county_births_estimate_year`. Genuine zero
scientist counts remain zero. Nonpositive/missing denominators or invalid
outcomes do not become zero; counties lacking complete coverage are excluded.
Estimated births allocate national births using county population shares,
rather than observing historical county births directly.

We preserve the combined county-geocoded AMWS numerator and reconcile its
early-edition and 1986 components. Per the user's information, 1986 entries
already appearing in earlier editions generally lack county information,
preventing them from contributing again to this county numerator. We do not
perform or independently certify a new all-edition person deduplication.
There is no additional sex or discipline restriction beyond the existing panel.

County of birth is explicitly used as the proxy for schooling location.
Migration can misclassify exposure. Verified nominal GEOIDs do not harmonize
historical boundaries to modern ones; county formation and boundary changes
also motivate the explicit coverage audit.

## Exposure, target, and reference

Let `G` be the midpoint infestation year and `t` an individual's birth year.

| Quantity | Definition |
| --- | --- |
| Age at infestation | `G - t` |
| First potentially exposed birth cohort `g` | `G - 18` |
| C&S event time `e` | `t - g = 18 - age` |
| Universal reference | `t = g - 1 = G - 19`, age 19 |
| Primary ages 4-9 | `t = G - 9, ..., G - 4`, event times +9 through +14 |
| Ages 4-6 | Event times +12 through +14 |
| Ages 7-9 | Event times +9 through +11 |
| Pre-exposure diagnostics, ages 20-40 | Event times -2 through -22 |

Ages 10-18 are potentially affected and are never treated as untreated
comparison cohorts. Post-onset effects can differ freely by age. Plotting the
birth-cohort panel does not track the same scientists over historical time.
The reference is age 19, whereas Baker's individual regression pools ages
19-30. Unaffected scientist generation at age 19 is a maintained assumption,
not a consequence of Baker's evidence about completed schooling.

## Sample

Restrict to the prepared Baker-rule-eligible Southern counties (permanent full
infestation, no full retreat, arrival-to-completion interval at most four years,
excluding Missouri), with verified nominal GEOIDs. No cotton-intensity or
positive-cotton-acreage requirement is imposed.

Require valid data for every year in the common 1853-1903 window. The balanced
panel contains 450 counties and 22,950 county-birth-year observations. Targets
are the 86 counties with midpoint infestation in 1893-1907: 59 Texas,
21 Louisiana, five Arkansas, and one Mississippi. The remaining 364 counties
are later-infested candidates for comparison. Some targets can also serve as
not-yet-exposed comparisons for earlier targets.

A control for comparison `(g,t)` must have its own exposure-onset birth cohort
strictly after `max(g-1,t)`. The common panel ensures that the identical control
counties are observed in both periods; their eligibility can still change as
their own exposure begins. At least 33 control counties support every reported
target comparison. The tightest comparison uses 31 in North Carolina and two
in Tennessee. The six-year age-4-9 estimand has 516 county-age observations.

The 1907 cap is determined by untreated donor support, not an outcome search:
with the last midpoint infestation in 1922, the last unexposed donor birth
cohort is 1903. Observing age four therefore requires target infestation at
most 1907. Missing infestation dates are excluded, never recoded as never
treated. `did` may internally label exposure occurring after the sample ends
as untreated within that observation window; original dates are preserved in
all sample exports.

## Estimation, aggregation, and inference

`did::att_gt` uses a balanced county panel, not-yet-treated controls, a universal
base, `anticipation = 0` after shifting `g`, `xformla = ~1`, and
`est_method = "dr"`. With an intercept only, ATT cells equal direct
unconditional two-period DiD comparisons. No county trends, cotton controls,
population controls, or other covariates are added in this initial analysis.

All counties have equal sampling weight. Only the 86 target counties' effects
are aggregated. `aggte(type="dynamic")` is applied to a copy of the fitted
object with non-target/non-reported ATT cells masked; the original sample,
influence functions, and metadata are preserved. No target group is silently
dropped at a particular reported age. Within each age, infestation groups are
weighted by their number of target counties. Pooled windows apply
`aggte(type="simple")` to their selected cells, giving equal county and equal
annual-age weights, rather than birth-population weights.

Inference clusters at county and uses 1,000 multiplier-bootstrap draws with
seed `20261025`, reset independently before estimation and aggregation so
upstream random-number consumption cannot change aggregation results. Annual
results include both pointwise 95% intervals and 95%
simultaneous bands across all 37 reported ages. Pooled intervals use
`ATT +/- 1.96 * bootstrap SE`. Standard errors are never averaged across ages:
joint influence functions retain covariance and uncertainty in estimated
group-share weights. Analytical covariance matrices are also exported for
inspection; their diagonals need not equal bootstrap variance estimates.

The runner checks that actual simultaneous inference was returned; it does
not silently relabel pointwise intervals as simultaneous bands. Very small
infestation groups (including one with a single county) are preserved and
their warnings retained. County clustering does not additionally accommodate
dependence across different counties or common state shocks.

In the initial sample there are only three scientist births in the 86 age-19
reference county-cohorts, versus 46 in the 516 target county-cohorts. The
package's full pre-treatment Wald test is unavailable because of a singular
covariance matrix; it is not recorded as a passed test. The requested
simultaneous-band diagnostics are reported separately.

Method references:
[att_gt](https://bcallaway11.github.io/did/reference/att_gt.html),
[aggte](https://bcallaway11.github.io/did/reference/aggte.html).

## Outputs and validation

Results go to `results/boll_weevil/amws_county_csdid/` in Dropbox:

- `results_summary.md`: numerical results, identification/measurement limits,
  pre-exposure diagnostics, and the main figure.
- `event_study_age_at_infestation.png`, `pooled_age_windows.png`: annual
  profile and pooled effects.
- `event_study_coefficients.csv`, `group_time_coefficients.csv`,
  `pooled_age_window_coefficients.csv`: all requested estimates and inference.
- `sample_inclusion_audit.csv`, `estimation_counties.csv`,
  `treated_counties.csv`, `estimation_panel.csv`, `state_composition.csv`,
  `sample_summary.csv`: exact sample and exclusions.
- `comparison_support.csv`, `target_cohort_weights.csv`,
  `aggregation_weights.csv`: comparison availability and explicit weights.
- `outcome_descriptives.csv`, `pre_exposure_diagnostics.csv`: raw outcomes,
  edition counts, and whether pre-exposure simultaneous bands exclude zero.
- `direct_did_validation.csv`, `dynamic_weight_validation.csv`,
  `pooled_weight_and_influence_validation.csv`, `validation_checks.csv`:
  numerical validation.
- `event_study_analytic_covariance.csv`, `pooled_analytic_covariance.csv`,
  `models/att_gt.rds`, `models/aggregations_and_influence.rds`: covariance and
  reproducible fitted/aggregated objects.
- `input_and_code_manifest.csv`, `estimation_warnings.csv`,
  `session_info.txt`: provenance, warnings, software, and random seed.

Helper tests cover cohort alignment, the untreated baseline, excluding an
already-exposed control with a large artificial outcome jump, and averaging
correlated influence functions correctly. On the real data, all 555 target
group-age cells are compared with direct DiD, every annual aggregation is
checked against explicit county weights, and pooled influence functions are
checked against averages of the annual influence functions. Input hashes are
verified unchanged at completion.

Do not interpret a confidence interval containing zero as proof of no effect.
Any causal conclusion requires the birth-cohort comparison assumptions to
hold for scientist generation, not simply for schooling. Results pertain to
the selected early-infested counties and should not be presented as effects
for all affected Southern counties.

## County and birth-year fixed-effects alternative

```powershell
Rscript analysis/boll_weevil/test_twfe_helpers.R
Rscript analysis/boll_weevil/analysis_boll_weevil_amws_twfe.R
```

The additional dependency is `fixest`. This runner preserves all CS DID files
and writes to `results/boll_weevil/amws_county_twfe/` in Dropbox. It estimates
OLS with county and birth-year fixed effects and Baker-style exposure-age bins
4-6, 7-9, 10-12, 13-15, and 16-18, omitting ages 19-30. Each included county
must contribute all 27 annual cohorts aged 4-30 at midpoint infestation.

Two samples are clearly separated: the original 86 CS DID target counties
(2,322 observations), and a broader 590 eligible counties (15,930 observations)
whose complete age windows are available in the AMWS panel. The first holds
target counties fixed; the second additionally changes geographic and cohort
coverage. Neither requires an unaffected later donor in every comparison.
The broader model is an additional sample sensitivity, rather than an isolated
estimator comparison.

Each observation has equal weight. Standard errors cluster by county with
`ssc(K.adj=TRUE, K.fixef="nonnested", G.adj=TRUE, G.df="min", t.df="min")`.
Confidence intervals use a t critical value with counties minus one degrees
of freedom. The age-4-9 coefficient averages the 4-6 and 7-9 coefficients,
using their full joint covariance for inference. The five individual bins
and the pooled coefficient are exported.

The summary and `comparison_with_csdid.csv` explicitly distinguish the CS DID
single-age-19 reference from the FE pooled-age-19-30 reference. TWFE can use
already-exposed cohorts in its comparisons; under heterogeneous effects, its
regression coefficients need not equal the previous CS DID estimand.

The runner exports models, full adjusted coefficient covariance matrices,
exact estimation samples, state/county identifiers, age-specific raw summaries,
exclusion audits, and hashes. An independent alternating-projection FWL
implementation checks every coefficient, residual, and unadjusted
county-clustered covariance matrix in each specification. Helper tests check
age bins, the omitted group, correlated contrasts, and residualization on a
ragged county-calendar panel. Original data and CS DID files are checksummed
before and after the run.

## Paired CS DID using the broader FE county roster

```powershell
Rscript analysis/boll_weevil/test_paired_csdid_helpers.R
Rscript analysis/boll_weevil/analysis_boll_weevil_amws_csdid_fe_counties.R
```

This separate run writes to
`results/boll_weevil/amws_county_csdid_fe_counties/`. It uses the exact 590-county
roster saved by the broader FE model, and preserves all prior CS DID/FE results.
Every prior result file is checksummed before and after estimation.

The complete ages-4-9 effect remains identifiable only for 133 counties with
midpoint infestation in 1893-1907. The other counties can supply comparisons
while their cohorts are unexposed. Ages 4-30 are reported so all 133 targets
have every own outcome and the reference. This is a shorter diagnostic window
than the original age-40 run. There are 798 target county-age observations
in the pooled ages-4-9 result. The FE roster, actual contributing sample, and
target-effect population are distinguished explicitly.

The annual AMWS panel supplies all available observations for those counties
in 1863-1903, including older birth cohorts absent from the FE regression's
age-trimmed sample. A comparison requires the same county to be observed at
the target outcome year and the age-19 reference year. Target counties are
observed in every comparison. Control eligibility requires exposure onset
`g = infestation_year - 18` to be strictly later than both comparison years.
At least 35 paired controls support every requested cell. 588 roster counties
appear in at least one nonreference comparison. Durham (37063) and Vance
(37181), North Carolina, lack the required earlier baseline observations and
therefore do not contribute; they remain in the roster and receive zero
influence, without imputed outcomes.

Every nonreference group-age cell is estimated through public `did::att_gt`
on a two-period balanced panel with an intercept-only specification and no
within-cell bootstrap. Pair controls are coded zero only inside that
two-period problem; they are not classified as historically never treated.
For pre-reference diagnostics the returned estimate and influence function
are both reversed in sign. Age 19 is the exact zero reference, not an
estimated two-period change. Per-group public model objects are saved.

For a cell with `n_pair` counties, its `att_gt` influence is multiplied by
`590/n_pair` and aligned by county ID to the common roster. Annual aggregation
weights groups by their number of the 133 target counties. It includes the
influence of estimated target-group shares. Pooled windows average the annual
influences, preserving all covariance. Group-age point estimates and embedded
influences are verified against independent closed-form paired DiD formulas.

All annual and pooled effects use the same 1,000 county-level Mammen
multipliers, seed `20261025`. The multiplier takes `(1-sqrt(5))/2` with
probability `(sqrt(5)+1)/(2*sqrt(5))`, and `(1+sqrt(5))/2` otherwise. Each
bootstrap perturbation is `sum(multiplier_i * influence_i)/590`. Standard
errors use the bootstrap interquartile scale, matching the `did` convention.
Pooled intervals use the normal critical value 1.96. Annual simultaneous
bands use the 95th percentile of the joint standardized supremum over the
26 nonreference ages, floored at 1.96 for conservatism. Both raw and used
critical values and empirical bootstrap coverage are exported. This is
explicit joint influence aggregation of public CS cells, rather than a
single globally unbalanced `att_gt` call.

The supporting FE regression uses the same 133 target counties, all 27 ages
4-30 (3,591 rows), Baker's five age bins, omitted ages 19-30, and county and
birth-year fixed effects. Its reference and comparison structure still differ
from CS DID. Existing CS86 and FE590 estimates are retained only as context.

Outputs include `results_summary.md`, the annual figure and same-target
comparison figure, coefficient tables, comparison memberships/support, county
roles, coverage, exact observed data, weights, joint analytic covariance,
supporting FE models/validation, and saved cell/aggregation/influence/bootstrap
objects under `models/`. `county_mammen_high_draws.csv.gz` records each shared
multiplier exactly: columns are sorted county IDs, rows are bootstrap draws,
and 1 denotes the positive Mammen value. `prior_results_protection.csv` records
unchanged checksums for the full previous-result directories.

The optional independent Python check reconstructs all cells, paired
membership, aggregated influences and covariance, multiplier draws, scales,
bands, and intervals using only CSVs and numpy/pandas:

```powershell
python analysis/boll_weevil/verify_paired_csdid_outputs.py
```

It writes `independent_python_validation.json` beside the results. Helper tests
also check sign reversal, nonsequential county IDs, sample-size scaling,
estimated group-share terms, and perfectly correlated bootstrap effects.
Matching the roster or requiring paired observations does not establish
parallel untreated cohort trends or the unaffected age-19 reference assumption
for scientist generation.

## Flexible FE analysis of exposure-age windows

```powershell
Rscript analysis/boll_weevil/test_fe_age_window_helpers.R
Rscript analysis/boll_weevil/analysis_boll_weevil_amws_fe_age_windows.R
```

This FE-only extension writes to
`results/boll_weevil/amws_county_fe_age_windows/`. It loads the frozen
`amws_county_twfe/estimation_panel_broader_590.csv` directly: all 590 counties,
15,930 observations, and the ages-19-30 reference remain identical. No new
CS DID estimation is run. Prior FE and CS result directories are checksummed
and preserved. Inputs include the earlier saved broader FE model and county
roster, so the earlier FE run must already exist.

Before estimating anything new, the runner reproduces the original five-bin
regression and its adjusted covariance. It then replaces those five indicators
with separate indicators for all 15 exposure ages 4-18, retaining county and
birth-year fixed effects. Ages 19-30 have all indicators equal to zero and
remain the pooled omitted category. The 15-indicator within design has full
rank. The model does not estimate separate older-age effects or an unrestricted
age/cohort profile that would require additional normalizations.

Five equal-annual-age averages and one difference are fixed in advance:

| Contrast | Role |
| --- | --- |
| 4-9 | Early childhood benchmark |
| 10-13 | Intermediate childhood |
| 14-18 | Primary high-school window |
| 13-18 | Broader adolescent sensitivity |
| 16-18 | Later high-school sensitivity |
| Mean effect at 14-18 minus mean effect at 4-9 | High school versus early childhood |

"High school" denotes age at infestation rather than observed attendance.
No sample, weights, outcome, denominator, controls, or trends are changed.
Annual-age effects can now vary inside the old three-year bins, so a new
window average need not reproduce the restricted old grouped-model estimate.
All contrasts use the full joint coefficient covariance, including overlapping
windows. The difference uses the difference of mean coefficients, not the
difference of their significance labels.

All observations have equal weight. Standard errors use the same county
clustering, nonnested-FE finite-sample adjustment, and 589 t degrees of freedom
as the broader FE specification. Results report pointwise 95% intervals,
two-sided p-values, and Holm-adjusted p-values across the six contrasts as one
family. The annual-age plot has descriptive pointwise intervals; it does not
have simultaneous coverage and is not included in the six-test family.

Outputs include `results_summary.md`, `annual_age_profile.png`,
`age_window_comparison.png`, annual coefficients, all six tests, exact contrast
weights and covariance, the estimation panel and county roster, age/window
descriptives, benchmark reproduction, saved models, validation, session
information, and input/code/protected-result hashes. `model_summary.csv`
records the design rank, reference and primary-window counts, 589 cluster
degrees of freedom, adjusted parameter count, and covariance correction.

Validation holds all frozen input values and row keys fixed. Independent FWL
calculations reproduce all new coefficients, residuals, and both unadjusted
and finite-sample-adjusted county-clustered covariance. Unit tests verify
age boundaries, the older reference, equal weights, overlapping contrasts,
perfectly correlated averages, differences, and the six-test Holm procedure.
The run stops on a changed benchmark, dropped observation/indicator, rank
failure, or failed numerical/protection check.

## Extended annual FE profile: ages 4-40

Run `analysis_boll_weevil_amws_fe_ages_4_40.R` from the repo root, or invoke
it by its absolute path. It resolves `paths.R` itself. Set
`TALENT_DETS_DATA_DIR` to the local Dropbox project root when needed.
Results go to `results/boll_weevil/amws_county_fe_ages_4_40/` in Dropbox.

This extension retains 555 of the frozen 590 FE counties: each must have valid
annual outcomes for all 37 ages 4-40, yielding 20,535 observations. The source
AMWS outcome and denominator are checked against all 15,930 frozen FE rows.
County exclusions and every missing county-age observation are exported.

There are 25 annual indicators for ages 4-18 and 31-40; ages 19-30 remain
the pooled reference. The figure orders ages from 40 down to four, highlights
ages 14-18, and marks the reference without coefficient points. Intervals are
pointwise 95% county-clustered t intervals with 554 degrees of freedom and
the same nonnested-FE small-sample adjustment. All observations are equally
weighted. No covariates, trends, or CS DID models are added.

The same six window contrasts and Holm family are exported, with explicit
zero weights on ages 31-40 and full joint covariance. A separate ages-4-30
annual model on the same 555 counties separates the county restriction from
the age-window extension; prior 590-county annual results provide context.
Independent FWL calculations validate both new regressions and their clustered
covariance. `verify_fe_ages_4_40_outputs.py` independently verifies exported
sample keys, age coding, reference, weights, covariance, intervals, Holm values,
exclusions, and source/prior-result checksums. The runner preserves all earlier
results and saves model objects, provenance, warnings, and session information.

Older-age estimates are relative to ages 19-30; they do not establish that
adult exposure leaves scientist generation unaffected. Birth county proxies
schooling location, and the denominator remains estimated county births.

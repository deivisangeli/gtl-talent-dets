# Linked-census cohort cutoff analysis

Run `Rscript analysis/mandatory_schooling/estimate_linked_1940_schooling_1950_scientist_rd.R`
from the repo root after the companion IV script has produced `main_cells.csv`.
The synthetic check is
`Rscript analysis/mandatory_schooling/test_linked_1940_schooling_1950_scientist_rd.R`.
Outputs go to Dropbox `results/mandatory_schooling/linked_1940_schooling_1950_scientist_rd/`.

The analysis uses **exactly the main IV population**: linked 1940–1950 native-born
white men born 1885–1912, 1940 schooling from `HIGRADED`, and the 1950
scientist occupation flag. The RD selects cohorts from that population. Law
exposure comes from the 1940 state of birth and the CLS cohort requirements.
The sample audit checks the IV cell totals against its flow and diagnostics
tables before estimation.

## Primary design

For states whose first law took effect after 1900, the cutoff is the first
birth cohort with positive CLS required years after a zero-requirement cohort.
The primary sample has six annual cohorts on either side of the cutoff. Fourteen
states have complete support. The running variable is birth year relative to
the first exposed cohort; the threshold is between `k=-1` and `k=0`.

The fuzzy RD instruments years of completed schooling with `1(k>=0)`. A pooled
local linear regression uses state fixed effects, state-specific slopes on
each side, triangular distance weights times IV cell counts, and state-clustered
standard errors. It reports the legal-requirement jump, schooling first stage,
scientist-occupation reduced form, and local schooling effect. The
Anderson–Rubin confidence set inverts the state-clustered reduced-form test of
`scientist - beta * schooling`. A 95% interval touching the evaluated grid edge
has an open endpoint.

The primary fitted jump is evaluated at the midpoint between adjacent annual
cohorts (`k=-0.5`). Because birth year is discrete, this is an interpolation:
the fitted required-years jump need not equal the observed one-year change from
`k=-1` to `k=0`. `adjacent_cohort_contrast.csv` reports those observed changes.
`window_and_state_sensitivity.csv` also evaluates the fitted lines at `k=0`
while holding observations and weights fixed.

This is a **local use of the same instrument**, not independent law variation.
The broad IV uses all 49 jurisdictions and multiple requirement bins; the RD
uses the first-exposure cutoff and nearby cohorts. As CLS exposure rises
gradually after first adoption, a first-stage jump must be checked rather than
presumed. The design requires smooth counterfactual cohort outcomes, exclusion,
and monotonicity. Birth year is inferred as `1940 - AGE - 1` and thus also has
measurement error near the threshold.

## Outputs and checks

- `primary_estimates.csv`, `anderson_rubin_intervals.csv`, and `sample_summary.csv`
  hold the main estimates and sample counts.
- `discontinuity_raw.*` and `discontinuity_adjusted.*` plot required years,
  completed schooling, and scientists per 100,000. Each annual cohort is a
  point with a descriptive, state-bootstrap 95% interval. Raw fitted lines are
  descriptive; adjusted fitted lines come from the primary model.
- `first_law_by_state.png` shows raw person-weighted means and separate
  descriptive fits for all three outcomes in each of the 14 first-law states.
  Each card names the law-effective year and first exposed birth cohort; it
  uses six annual cohorts on either side of that cohort cutoff.
- `calonico_fuzzy_sensitivity.csv` uses `rdrobust` with a fixed six-year
  bandwidth. Its conventional and robust bias-corrected results are a
  sensitivity check: the running variable has only six distinct values on
  each side, limiting continuous-score inference.
- `window_and_state_sensitivity.csv` varies the window, drops each state
  in turn, and checks the cutoff location. `prelaw_placebo.csv` splits the six unexposed cohorts into two
  groups of three at a false cutoff.
- `later_reform_inventory.csv` screens actual post-1900 policy changes. The
  script reproduces CLS's cohort requirements from the state-year law panel
  before computing each reform's incremental requirements under a
  prior-policy counterfactual. `later_reform_exploratory.csv` reports only
  descriptive first-stage and reduced-form contrasts for isolated candidates.
  It does not report unstable reform-specific schooling-effect ratios.

The 1950 outcome measures occupation at enumeration, not ever working in
science. The linked extract is selected and does not represent the full census.

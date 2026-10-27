# WWII service IV and scientist occupation in the 1960 census

Run from any working directory (use absolute script paths outside the repo):

```powershell
Rscript analysis/mandatory_schooling/test_wwii_service_scientist_1960_iv.R
Rscript analysis/mandatory_schooling/estimate_wwii_service_scientist_1960_iv.R
Rscript analysis/mandatory_schooling/test_wwii_service_scientist_1960_iv.R --integration
```

Packages: DBI, duckdb, data.table, fixest, xml2, ggplot2. The analysis reads the
compressed CSV directly into individual-level census records and fits all models
at that level. Aggregated cohorts are used only for descriptive outputs/plots.

`paths.R`/`TALENT_DETS_DATA_DIR` select the Dropbox project root, with `DET_DIR`
and the current user's conventional Dropbox directory as compatibility fallbacks.
Inputs default to `Data/raw/mandatory_schooling/usa_00007.csv.gz` and
`usa_00007.xml`; the ordinary `raw/mandatory_schooling` layout is also supported.
`IPUMS_1960_SOURCE` and `IPUMS_1960_DDI` override individual inputs.

Results default to `results/mandatory_schooling/wwii_service_scientist_1960_iv`
under the Dropbox project root. `WWII_1960_RESULTS_DIR` overrides that path.
The analysis refuses to overwrite an existing results directory; set that
variable to a new directory for a repeated run. Results are first written to a
staging directory and promoted only after all embedded checks and input-hash
verification pass. Failed staging is retained for inspection.

## Main sample and estimator

US-born civilian men (BPL 1-56, 90, 99), 1925-1930, valid BIRTHQTR 1-4,
VETWWII 1/2, positive PERWT, nonblank OCC1950. Exclude current armed forces
using OCC1950=595, EMPSTATD=13-15, or CLASSWKRD=26. Exclude Korean-only veterans
(`VETKOREA=2 & VETWWII=1`); retain WWII-plus-Korea veterans and veterans reporting
neither period. This removes 75,364 men from 321,247, leaving 245,883 men and
1,189 scientists. Use supplied BIRTHYR without another birth-quarter correction.

Scientist status uses the existing IV definition: OCC1950 7,61,62,63,67,68,69.
Other nonblank occupation codes, including 997/999, receive zero, exactly as in
the linked census IV classifier; blanks remain missing. Retain nonworkers and
students. Unknown 979/997 codes are excluded only in a sensitivity check.

The original design note is
`Lit/Compulsory Attendance/abramitzky_wwii_gi_bill_empirical_strategy.md`.
Define x=4*(BIRTHYR-1927)+BIRTHQTR-4 and z=1{x<=0}. First stage and reduced form
use intercept, z, x*z, x*(1-z). Weighted 2SLS instruments VETWWII=2 with z and
includes both trends. No additional controls enter the baseline. The contrast
is earlier minus later at x=0, rather than at the plotted boundary x=0.5.

PERWT=20 throughout the primary sample, so weighted and unweighted coefficients
coincide. Cluster by national birth-year x quarter. SSC: K.adj=TRUE,
K.fixef=nonnested, G.adj=TRUE, G.df=min, t.df=min. Use G-1 degrees of freedom,
with 24 primary clusters. First-stage F is the square of the clustered t test.
The main Anderson-Rubin 95% set solves the acceptance quadratic over the entire
real line, permitting empty, unbounded, and disjoint sets. Its saved grid is
diagnostic only. Cross-covariance comes from the first/reduced-form models and
the model for their summed outcome, using identical regressors and SSC.

## Robustness and outputs

Ten specifications: primary; unrestricted US-born benchmark; 1923-1930;
1926-1929; omit 1928; all birthplaces; exclude 979/997; QAGE=0;
quarter-of-year effects; birthplace-plus-race effects. All but the unrestricted
benchmark exclude Korean-only veterans. Each specification saves three models,
coefficients, cohort summaries, sample summaries, and first-stage diagnostics.

Outputs include estimates.csv, model_diagnostics.csv, specifications.csv,
sample_exclusions.csv, sample_summaries.csv, service_groups_before_exclusion.csv,
cohort_summaries.csv, fitted_cohort_trends.csv, Anderson-Rubin components and
intervals, DDI/code/age/occupation audits, extract key audit, input_manifest.csv,
models/*.rds, figures/*.png and *.pdf, session_info.txt, and results_note.md.
The Korean service regression and retention plot use the unrestricted sample.

The primary filter selects on subsequent service and may invalidate the original
IV exclusion assumptions; interpret alongside the unrestricted benchmark.
The outcome is scientist occupation in 1960, not first entry or lifetime status.
Service effects bundle veteran benefits. Discrete trend approximation and
national cohort serial dependence are not solved by conventional clustering.
The college-entry-before-1952 check is unavailable.

No Vietnam-era exclusion applies: the IPUMS era is August 1964-April 1975,
after this outcome; VETVIETN is absent from 1960. Earlier Vietnam service is
not separately identifiable. See
[IPUMS](https://usa.ipums.org/usa-action/variables/VETVIETN).

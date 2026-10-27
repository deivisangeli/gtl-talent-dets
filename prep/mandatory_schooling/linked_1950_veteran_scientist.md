# WWII veteran status and scientist occupation

This analysis adapts equation (2) in Collins and Zimran's *World War II Service
and the GI Bill* to scientist occupations in 1950. It estimates adjusted
veteran-minus-non-veteran differences, not a separately identified GI Bill effect.

## Run from the repository root

```powershell
Rscript prep/mandatory_schooling/test_build_linked_1950_veteran_scientist.R
Rscript analysis/mandatory_schooling/test_estimate_linked_1950_veteran_scientist.R
Rscript prep/mandatory_schooling/build_linked_1950_veteran_scientist.R
Rscript analysis/mandatory_schooling/estimate_linked_1950_veteran_scientist.R
```

The builder requires the existing 1950 sample-line, broader linked census, and
1940 full-count Parquet datasets. It creates a separate dataset at
`D:/ipums_linked_1950_veteran_scientist`; it refuses to overwrite an existing
output. Validation precedes staging promotion. Failed staging is retained for
inspection. Existing IV results and the Overleaf document are not changed.

| Environment variable | Default |
|---|---|
| `IPUMS_1950_SAMPLE_LINE_PARQUET_DIR` | `D:/ipums_linked_1950_sample_line` |
| `IPUMS_LINKED_1950_PARQUET_DIR` | `D:/ipums_linked_1900_1950` |
| `IPUMS_1940_PARQUET_DIR` | `D:/ipums_1940_fullcount` |
| `IPUMS_VETERAN_SCIENTIST_PARQUET_DIR` | `D:/ipums_linked_1950_veteran_scientist` |
| `VETERAN_SCIENTIST_RESULTS_DIR` | Dropbox `results/mandatory_schooling/linked_1950_sample_line_veteran_scientist` |

Set `TALENT_DETS_DATA_DIR` for the Dropbox project root; the scripts also support
`DET_DIR` and the current user's conventional Dropbox path.

## Data and joins

Every eligible 1950 man has one row, including men excluded from regressions.
Eligibility uses `YEAR=1950`, `SEX=1`, `SLREC=2`, positive `SLWT`, `VETWWII` 1 or 2,
and inclusive `BIRTHYR` 1910-1927. The production count is 2,345,225.

Respondents' 1940 records require reciprocal `HIK` links. Father records come
from the broader linked extract, using 1940 `SERIAL` and `PERNUM=POPLOC`.
Full-count person enrichment uses trimmed, uppercase `HISTID`. Household
serials differ between extracts: ownership is joined using the **full-count
person's** `SERIAL`, never the linked-extract serial. Reported father's
birthplace comes from full-count `FBPL`, so it is available without a
coresident father. Source-version disagreements are counted rather than used
to overwrite linked fields.

Prefixes distinguish `y1950_`, `y1940_`, `father1940_`, `fc1940_`, and `hh1940_`
raw values. `field_dictionary.csv` documents every output column.
`exclusion_reason` records all applicable reasons; `regression_eligible` is true
only when this string is empty. Raw and derived missingness remain in the file.

## Models and inference

Scientist work uses the inherited seven-occupation flag: `OCC1950` 007, 061,
062, 063, 067, 068, 069. The control for the respondent's own 1940 scientist
status uses the same definition. WWII veterans have `VETWWII=2`; code 1 defines
non-veterans. Weighting always uses the respondent's **1950** `SLWT`.

Main cohorts are 1922-1927 and 1910-1921; comparison cohorts are 1921-1927 and
1910-1927. Background is 1950 race crossed with reported father's 1940 origin.
The pooled analysis includes all eligible backgrounds. Subgroup analyses use
US-born white men with US-born, northern/western European, or southern/eastern
European fathers, and US-born Black men with US-born fathers. US birthplace
codes are 1-56, 90, 99. NW codes are 400-429 and 453; SE codes are 430-440,
450-452, 454-457, 459-465. Ambiguous origins remain outside those four groups
but are retained for pooled analysis.

Weighted linear probability models successively include:

1. Background-by-birth-year fixed effects.
2. Additionally background-by-birthplace and background-by-1940-county effects.
3. Additionally prewar covariates, with slopes varying by background.

The full specification uses weighted Frisch-Waugh-Lovell projection separately
within each background, followed by a pooled residual regression. This fits the
same explicit interactions while avoiding numerical problems with iterative
varying slopes. Covariate rank is restored in finite-sample covariance and
degrees-of-freedom adjustments. Fixtures compare coefficients and both inference
methods against direct regressions with explicit background interactions.

Common prewar controls are own schooling years, college completion, school
attendance, scientist status, urban/farm residence, and homeownership.
For 1910-1921 births, labor controls describe the man himself; for 1922-1927,
they describe his father and add father's schooling years/college completion.
Labor controls are log wages, a valid-zero-wage indicator, occupational income
score, a zero-score indicator, white-collar occupations 0-299, and nonfarm
unskilled occupations 900-970. Inactive age-specific terms are zeroed only
inside modeling; missing active controls trigger exclusion. Completed years
are capped at 17. College completion means 16+ completed years (`HIGRADED>=190`),
not the earlier descriptive measure of any college attendance (`HIGRADED>=151`).

All three specifications use the same complete-case sample and retain fixed
effect singletons. Current armed forces are excluded using `OCC1950=595`,
`EMPSTATD=13-15`, or `CLASSWKRD=26`. Main models retain students, unemployed
men and men outside the labor force, and impose no postwar schooling or income
restriction. Strict-link sensitivity requires agreement in race, birthplace,
and census year minus age across the linked 1940/1950 records.

County-1940 clustered standard errors are primary; HC1 inference is reported
separately. Coefficients, standard errors and intervals are stored in both
probability units and percentage points. Fixed-effect collinearity, convergence
warnings, and unestimable groups are documented in `model_diagnostics.csv`.
Group/cohort tests are not adjusted for multiple comparisons.

## Outputs and interpretation

The dataset includes `data.parquet`, `field_dictionary.csv`, `_manifest.json`,
`validation_cohorts.csv`, and `source_version_audit.csv`. Results include
`coefficients.csv`, `scientist_rates.csv`, `sample_selection.csv`,
`exclusion_reasons.csv`, `model_diagnostics.csv`, `model_definitions.txt`,
and a PNG/PDF coefficient plot. The builder reconciles raw scientist counts and
shares against the earlier veteran descriptive summaries when they are present.

The 2026-09-30 build passed validation and retains all 2,345,225 eligible men.
The Parquet file is approximately 419 MB. Every one of the 2,118,285 reciprocal
1940 records matched the full count, and all 1,124,956 positive father pointers
matched father records in the broader extract. The full-count source has
different household serial numbers for every matched respondent; disagreements
in age (33,626), race (5,197), and birthplace (14,203) are recorded in the audit.

| Cohort | Eligible men | Complete cases | Strict-link complete cases |
|---|---:|---:|---:|
| 1922-1927 | 742,139 | 534,733 | 396,971 |
| 1910-1921 | 1,603,086 | 1,289,556 | 862,151 |
| 1921-1927 | 866,793 | 631,054 | 467,280 |
| 1910-1927 | 2,345,225 | 1,824,289 | 1,259,122 |

This is an adaptation of the paper's strategy: father birthplace comes from
1940, and the sample-line weights are not corrected for selection into linkage.
Unobserved selection may remain, and military service cannot be separated from
GI Bill eligibility. Weighted totals represent the available extract.

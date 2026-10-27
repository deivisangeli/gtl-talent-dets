# AMWS WWII/GI Bill birth-cohort reduced forms

This analysis implements the project strategy note's December 1927 / January
1928 and 1927Q4 / 1928Q1 cohort comparisons. The inferred-male AMWS outcome is
primary. Female and unclassified counts are placebo diagnostics. All outcomes
use **total estimated births**, not sex-specific births, as denominators.

## Run

Set the canonical Dropbox root on machines whose path differs from `paths.R`:

```powershell
$env:TALENT_DETS_DATA_DIR = "C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets"
Rscript analysis/mandatory_schooling/test_wwii_gi_bill_amws_cutoff.R
Rscript analysis/mandatory_schooling/estimate_wwii_gi_bill_amws_cutoff.R
Rscript analysis/mandatory_schooling/test_wwii_gi_bill_amws_cutoff.R --integration
Rscript analysis/mandatory_schooling/export_wwii_gi_bill_amws_cutoff_to_project.R
Rscript analysis/mandatory_schooling/test_export_wwii_gi_bill_amws_cutoff.R
```

Scripts accept absolute script paths from any directory and resolve the repo
before sourcing `paths.R`. Packages: `data.table`, `fixest`, and `ggplot2`.
`--render-only` regenerates plots and audits from the saved 60-model run without
refitting; it requires an unchanged-input manifest. No input panels are modified.

## Inputs and specifications

The monthly and quarterly `county_amws_*_gapminder_1800_1960.csv` panels and
`county_gapminder_annual_births_1800_1960.csv` annual support table live under
Dropbox `output/mandatory_schooling/`. Only 1923-1930 is read into analysis
memory after selecting monthly/quarterly columns. Saved gender flags use the
0.8 threshold; no classification is rerun.

Each window requires a balanced monthly county sample with positive finite
denominators and valid nonnegative integer counts for all outcomes. Zeros stay;
unassigned birth months never enter regression numerators. Quarterly counts and
denominators are summed from these monthly cells and checked against the
existing extended quarterly panel before rates are calculated.

The birth-weighted baseline is 1925-1930 with county and seasonal fixed effects
and separate linear trends. Running variables are
`12*(year-1927)+month-12` and `4*(year-1927)+quarter-4`. The earlier-born indicator
is `x <= 0`. The reported coefficient is the earlier-born minus later-born
fitted rate **at x=0**, in scientists per 1,000 total estimated births. It is not
a raw adjacent-bin difference or a fitted contrast evaluated at x=0.5.
Resolution-specific centering can change the coefficient after aggregation.

Standard errors use two separate dimensions: county and national birth cohort.
The explicit `fixest::ssc` settings are `K.adj=TRUE`, `K.fixef="nonnested"`,
`G.adj=TRUE`, `G.df="min"`, and `t.df="min"`. County fixed effects are nested in
county clusters; seasonal effects are not nested in both clustering dimensions.
Confidence intervals and p-values use `min(county clusters, cohort clusters)-1`
degrees of freedom. Collinearity, dropped observations, or unavailable cutoff
inference stop estimation; other model warnings are recorded in diagnostics.

There are 60 fits: four windows, two sample-support definitions, two resolutions,
and three outcomes (48); equal weights and removal of seasonality are separate
baseline checks at both resolutions for all outcomes (12). The other windows
are 1923-1930, 1926-1929, and 1925-1930 omitting 1928. Window-specific and
common-county results remain separately labelled even when identical. Equal
observation weights give equal county influence on a balanced panel.

## Results and report

Outputs live in Dropbox `results/mandatory_schooling/wwii_gi_bill_amws_cutoff/`:

- `estimates.csv`, `specifications.csv`, saved `models/*.rds`, and cohort/fitted-line CSVs.
- County exclusions/common sample, sample summaries, quarter reconciliation,
  annual/gender/December-January/geography audits, input hashes and session information.
- PNG/PDF cutoff and measurement plots, diagnostics, and `results_note.md`.

The exporter generates `project/wwii_gi_bill_amws_reduced_form.tex` with main
male results, female/unclassified placebos, and an appendix containing all core
robustness, raw cohort plots, sample retention and measurement diagnostics.
The source inlines tables and PGFPlots coordinates, allowing single-file
compilation. Reusable tables and inline plot snippets are also saved separately;
rendered PNG/PDF figures are copied to project figure files, all using the report
prefix. A manifest records each source path/hash and destination. The existing
`wwII_gi_bill.tex` is untouched. Generated estimates and prose should be updated
by rerunning the exporter, not by editing numbers manually.

Open the new `.tex` in Codex and use its compiler first. If that compiler is
unavailable on the host, an existing local TeX installation can compile the
self-contained source from `project/` using `pdflatex` twice. Do not install a
compiler merely to use the built-in editor. The source requires PGFPlots 1.18.

## Interpretation

The design has 72 monthly or 24 quarterly baseline national cohorts, despite
many county observations. Conventional two-way clustering does not correct
serially correlated national cohort shocks or discrete-trend approximation
bias. Saved lag-one correlations of national cohort residual means are descriptive,
not alternative inference. Excluding 1928 increases extrapolation.

Annual births divided by twelve can create December/January changes. Directory
selection, career censoring, inferred gender, missing month assignment, geography
changes, and non-deduplication of 1986 against early editions constrain
interpretation. Unclassified records can include men. No service first stage is
estimated: effects cannot be expressed per veteran, and exposure bundles service
with GI Bill and other veteran benefits rather than isolating education subsidies.

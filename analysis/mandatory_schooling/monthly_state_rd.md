# State-by-state monthly AMWS compulsory-schooling RD

This is a separate analysis of reduced-form birth-cohort discontinuities around
the oldest nominal school-age cohort covered by each jurisdiction's first
statewide compulsory-attendance law. It uses the existing full-period monthly
Gapminder/AMWS panel, 1800–1960. It does not replace the existing event studies.

## Run

From the repository root in PowerShell:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' analysis/mandatory_schooling/test_monthly_state_rd.R
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' analysis/mandatory_schooling/estimate_compulsory_schooling_amws_monthly_rd.R
```

The runner resolves `paths.R` independently of the working directory and requires
`data.table`, `ggplot2` and `sandwich`. Set the data-root variable to the local
Dropbox path on another machine. The input panel must already be built; this
analysis does not run prep or rewrite input data.

## Inputs and research

Paths below are relative to the configured Dropbox project root:

- `output/mandatory_schooling/county_amws_monthly_gapminder_1800_1960.csv`
- `output/mandatory_schooling/county_gapminder_annual_births_1800_1960.csv`
- `output/mandatory_schooling/monthly_gapminder_1800_1960_audits/scientist_birth_month_assignments.csv`
- `input/compulsory_schooling_laws.csv`
- `output/mandatory_schooling/cls/cls_state_age_limits_1880_1930.csv`
- `output/mandatory_schooling/cls/cls_law_onset_by_state.csv`

`monthly_state_rd_law_research.R` supplies explicit researched overrides for
pre-1880 age limits, implementation dates, repeals, partial coverage and source
disagreements. It initializes
`input/mandatory_schooling_monthly_rd/law_research_overrides.csv` **only if absent**.
Subsequent runs preserve manual edits. Changes to the seed do not silently
overwrite an existing research file: update that file deliberately to incorporate
new source findings. The derived sourced inventory is written both there as
`first_statewide_law_cutoff_inventory.csv` and to the results directory.

The inventory covers 48 contiguous states plus DC and explicitly excludes Alaska
and Hawaii. Original-year CLS age rules are used only when the first statewide law
is observed in the CLS panel. The panel's left-censored 1880 entries never stand
in for older laws. Earlier ages are researched separately. Unknown age rules
remain unresolved without an estimated cutoff.

Principal sources are the [CLS replication data and appendix](https://doi.org/10.3886/E145122V1),
[CLS 2012 Appendix A](https://www.nber.org/system/files/working_papers/w18477/w18477.pdf),
and [Gateway historical US policy narratives](https://g2aging-media.s3.amazonaws.com/bilhdkmj/bywhymrq.pdf),
cross-checked against available original statutes and historical government
reports. State-specific links and statutory citations appear in the inventory.
Cached source PDFs and extraction notes live under
`raw/mandatory_schooling_monthly_rd/sources/`. The file
`attendance_laws_1914.pdf` is a legacy cache label for the **1935** government
attendance-law report, not a report from 1914.

Month-specific statutory timing is established for Alabama (October 1, 1917),
Georgia (January 1, 1917), and Michigan's first school year (September 4, 1871).
Other cutoffs remain annual approximations. “Verified monthly” describes the
sourced nominal timing, not an observed individual schooling first stage.
School-calendar reference dates were not verified for every district or state;
approval dates alone are not treated as implementation dates.

First episodes include repealed laws in Arizona (1875), Texas (1870, with
1870/1871 implementation uncertainty) and Washington (1872, with early timing
disagreement). Later reintroductions are supplementary, never substitutes for
the earliest episode. New Jersey uses sourced 1875 rather than canonical 1874;
Oregon uses effective 1890 rather than enactment 1889. Earlier geographically
limited or opt-in laws are flagged. Louisiana's 1914 provision covered selected
cities, so statewide 1916 is retained. Maryland and Missouri's oldest nominal
coverage is conditional on employment. Contemporaneous CLS exemption and
continuation codes are retained; individual exemptions are unobserved.

## Estimator

For every state, source outcome and bandwidth, retain the counties with complete
monthly observations and finite positive estimated birth denominators throughout
the window. County sets are fixed within a window but may differ between the
three-, six- and ten-year windows. Preserve GEOIDs as five-character strings.
Aggregate counts and denominators to states, then form scientists per 1,000 births.
Annual births are divided by twelve; observed monthly birth seasonality is absent.

Use cohort-month midpoints relative to the cutoff. A verified within-month
eligibility boundary excludes its straddling month from fitting while still
requiring denominator coverage in that month. The fitted equation is:

```text
rate = intercept + jump * post + pre_slope * x + slope_change * post * x
       + calendar_month_fixed_effects
weight = estimated_monthly_births * (1 - abs(x) / bandwidth_months)
```

A verified cutoff subtracts the exclusive nominal leaving age from the effective
date. An inclusive endpoint is converted to the next birthday. Annual cutoffs
use January 1 of `law_year - exclusive_leaving_age + 1`, explicitly labeled
approximate. This annual age convention is not a claim of a January legal rule.

Six years per side and HAC 12 are primary. Three and ten years use HAC 12; the
six-year specification also uses HAC 6 and 24. Standard errors use Bartlett
Newey–West scores, without prewhitening and with `n/(n-k)` adjustment. Actual
calendar gaps are preserved instead of compressing a dropped straddling month.
On contiguous months this is numerically equivalent to
[`sandwich::NeweyWest(..., prewhite=FALSE, adjust=TRUE)`](https://sandwich.r-forge.r-project.org/reference/NeweyWest.html).
Normal 95% intervals and two-sided p-values are reported. BH-adjusted primary
p-values are supplemental and calculated separately by date-precision family.

Combined counts are primary. The early editions preserve their existing
1906/1938/1955 deduplication; 1986 is added without deduplicating against those
editions. Source-only estimates are sensitivities. All-zero outcomes have no
estimate, confidence interval or p-value. Fewer than 20 scientists on either
side is flagged as sparse. Post-1880 overlapping age/exemption changes,
source-coverage boundaries, repeals and earlier partial coverage are flagged.
The policy-change audit is not an exhaustive reconstruction of pre-1880 reforms.

## Results and validation

Outputs are isolated under
`results/mandatory_schooling/compulsory_schooling_amws_monthly_rd/`.
Start with `results_summary.md`, which contains coverage, edition completeness,
monthly-cutoff estimates, bandwidth sensitivity, all estimated states, exclusions,
figures and an output-file guide. Machine-readable exports include all
specifications, the sourced inventory, county coverage, plot points, source and
date diagnostics, overlapping policies, reintroductions, and preservation checks.

The completed run estimates 34 jurisdictions: three month-specific cutoffs and
31 annual approximations. Fifteen have no balanced county denominator coverage
in the primary window. Earlier editions have birth-month assignment rates of
98.7% (1906), 96.2% (1938), and 98.7% (1955), versus 98.1% (1986). All 34 primary
windows have informative early-edition outcomes; only ten have informative
1986-only outcomes. The 1986 retained sample starts with births in 1886.

The estimates have mixed signs. Michigan's positive monthly-cutoff estimate is
sparse and disappears at narrower and wider bandwidths. Georgia's negative
estimate weakens at ten years; Alabama also varies with bandwidth. Interpret all
estimates as reduced-form cohort breaks: legal exposure can ramp gradually,
birth state can differ from schooling state, and there is no schooling first stage.

### LaTeX figure document

To reproduce the figure collection without rerunning estimation:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' analysis/mandatory_schooling/render_monthly_state_rd_latex.R
```

This writes `monthly_state_rd_figures.tex` and its compiled PDF in the existing
monthly-RD results directory. It includes both coefficient figures, 34 primary
state plots alphabetically arranged two per portrait page, and coverage for all
51 jurisdictions. Captions read the cutoff, timing precision, estimate and
interval directly from the primary export. Relative PNG references permit
compilation from the results directory. Use `--tex-only` to generate source
without compiling; default compilation requires local `pdflatex`. Compiler logs
and preservation checks go to `tmp/pdfs/monthly_state_rd/` in the repository.
The renderer hashes all pre-existing monthly-RD results before and after rendering.

Synthetic tests check ages, inclusive endpoints, January/year and leap transitions,
within-month boundaries, missing dates, unresolved left censoring, repeals,
leading-zero GEOIDs, source reconciliation, fixed counties, denominators and
zero-versus-missing outcomes. A simulated seasonal series with separate slopes
recovers a known jump, and a contiguous-month comparison validates the HAC
implementation against `sandwich::NeweyWest`.

The full run checks the 5,973,744-row monthly grid, source-count reconciliation
to assigned annual counts, and denominator aggregation to the annual estimates.
It hashes consumed inputs, original AMWS datasets and existing mandatory-schooling
analysis files before and after execution. Existing result-directory metadata
is checked at the top level; this is not a recursive content-hash audit. All
writes stay inside the new research and results directories.

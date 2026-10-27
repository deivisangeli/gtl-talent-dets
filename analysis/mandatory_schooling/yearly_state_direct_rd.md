# Direct-state yearly AMWS compulsory-schooling RDs

Run from any working directory, with `TALENT_DETS_DATA_DIR` pointing to the local
Dropbox project root:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
Rscript analysis/mandatory_schooling/test_yearly_state_direct.R
Rscript analysis/mandatory_schooling/estimate_compulsory_schooling_amws_yearly_rd_state_direct.R
```

The separate builder `build_amws_state_year_direct.R` can also be run alone.
It writes `output/mandatory_schooling/yearly_state_direct/` in Dropbox.
The estimator writes `results/mandatory_schooling/compulsory_schooling_amws_yearly_rd_state_direct/`,
including an editable LaTeX source and compiled PDF. Local `pdflatex` is required.
`render_yearly_state_direct_latex.R` also runs independently to rebuild the document
from saved estimates and PNGs without estimating regressions again.
`validate_yearly_state_direct_outputs.py` uses Python with `pypdf` to independently
reconcile all record-level state counts, plotted points, PDF captions, image
inclusions, 51-jurisdiction coverage and the original-file checksum manifest.

Scientists are counted by explicit birth-state fields, never selected by county
population availability. Valid birth years do not require birth months. Original
early-edition deduplication decisions are fixed. Additional unmatched records
are joined to canonical years and names, checked against all retained early
records and one another using normalized surname, first initial, birth year
(plus or minus one), and state, with earliest additions considered first.
Existing records cannot be displaced. Missing name keys are retained and flagged,
matching the original deduplication convention. The 1986 see-previous exclusions
remain; 1986 is not deduplicated against earlier editions. County/state conflicts
are audited and explicit birth states determine assignment. Three existing 1955
identifiers have retained candidates in two different states; these six rows are
excluded as unresolved state assignments, with the original kept flags preserved
in the audit. Identical repetitions of unmatched candidate rows count only once;
conflicting unmatched candidates stop the builder. Alaska/Hawaii remain
scope exclusions; unresolved state/year records are audited rather than imputed.

State population comes from Forstall (1996), Part II, Census Bureau:
https://www2.census.gov/programs-surveys/decennial/tables/1990/population-of-states-and-counties-us-1790-1990/population_partii.xls
The Census historical series reconstruct state boundaries where possible, including
pre-statehood territories; historical underenumeration remains. Annual values use
linear interpolation between observed census anchors, never extrapolation. The
population-source audit links the Census boundary documentation. The national
Gapminder birth rate is inherited unchanged, including its existing county-derived
national population denominator. State births are statewide population times this
rate; they are estimates, not state vital-registration observations.

Primary: 10 complete pre and 10 complete post years, omitting the entire mixed
birth year at a within-year cutoff. Use annual midpoints, separate linear trends,
birth-weighted triangular kernels, and calendar-aware Bartlett/Newey-West HAC-1
normal intervals with finite-sample adjustment and no prewhitening. Sensitivities:
6/20 years per side, HAC 0/2, and early-only/1986-only counts. Missing population
anywhere in the window fails the state window; counties are never selected.
All-zero outcomes are uninformative. Fewer than 20 scientists on a side is sparse.

Only informative primary states receive plots. The PDF contains the two precision-
specific coefficient figures and all primary state plots once, ordered alphabetically,
two per portrait page, plus all 51 jurisdiction dispositions and available years.
The comparison exports hold the existing county-restricted specification at ten
years per side; changes combine added scientists with a statewide denominator.
Interpret estimates as reduced-form cohort discontinuities, not an IV effect of
schooling. Neither monthly analyses nor old yearly results are rewritten; MD5
manifests verify this. Compilation/render intermediates are under
`tmp/pdfs/yearly_state_rd_state_direct/` in the repository.

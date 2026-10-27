# Yearly-outcome state RD parallel

Run from the repo root:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' analysis/mandatory_schooling/test_yearly_state_rd.R
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' analysis/mandatory_schooling/estimate_compulsory_schooling_amws_yearly_rd.R
```

The default is ten complete pre-years and ten complete post-years, as selected
by the user. Set `ANNUAL_RD_YEARS_PER_SIDE` to 6, 10 or 20 to change the primary
document. All three windows are exported, with separate early-only and 1986-only
outcomes. The primary window also exports HAC 0/1/2 checks. Requires `data.table`,
`ggplot2`, `sandwich` and local `pdflatex`.

Inputs are the existing annual Gapminder/AMWS support table, monthly-law cutoff
inventory, source record totals and CLS policy panel. No monthly regression is
rerun and no existing monthly result or input is rewritten. Outputs live under
Dropbox `results/mandatory_schooling/compulsory_schooling_amws_yearly_rd/`.

Annual counts include scientists with a known birth year even if their month is
unknown: 113,985 records, versus 111,701 assigned-month records in the full source
panel. Combined counts preserve early-edition deduplication and the absence of
1986 cross-deduplication. Aggregate annual counts and denominators over a fixed
county set, then calculate scientists per 1,000 estimated births.

For non-January cutoffs, omit the entire mixed-exposure birth year, retaining
positive county denominators through that year. Select exactly the requested
number of complete cohorts on either side. Use annual midpoints relative to the
original cutoff; separate linear slopes and a treatment indicator; no seasonal
fixed effects. A triangular kernel ends half a year beyond the outermost selected
midpoint on each side and multiplies annual births. Thus the calendar-distance
bandwidth can differ slightly across sides when a mixed year is omitted.

Annual Newey-West inference uses Bartlett scores, actual calendar-year gaps,
one annual lag, no prewhitening, and `n/(n-4)` correction. All-zero outcomes are
uninformative; fewer than 20 scientists on a side is sparse. Policy overlaps and
source-coverage boundaries are flagged. Estimates are reduced-form cohort breaks
and AMWS provides no schooling first stage. Annual aggregation does not make
the small scientist samples larger.

Available-year audits find contiguous stretches of positive county denominators
covering both sides of each cutoff. Max pre and max post may correspond to
different county sets; max balanced uses one county throughout both sides.
These are technical coverage limits, not recommended bandwidths. All 34 monthly
estimated jurisdictions support ten years per side; only 28 support twenty.
Minnesota's ten pre-years limit the common symmetric window. California has 11,
Michigan/Oregon 17, West Virginia 14 and Wisconsin 15. Nebraska and the earliest
Texas episode have only three pre-years. Scientist source coverage and later
policy changes constrain the interpretation of longer spans.

The standalone `yearly_state_rd_figures.tex` and compiled PDF contain two
coefficient comparisons, annual state plots alphabetically arranged two per page,
methods and available years for all 51 jurisdictions. Relative PNG references
allow compilation from the output directory. CSVs retain all estimates, annual
points, exclusions, source reconciliation, available-year spells and before/after
hashes of annual inputs and all original monthly results.

Synthetic checks cover year boundaries, mixed-year omission, complete county
coverage, leading-zero GEOIDs, inclusion of unknown-month records, recovery of
a known annual discontinuity, all-zero handling, contiguous support spans and
agreement with `sandwich::NeweyWest` when calendar years are contiguous.

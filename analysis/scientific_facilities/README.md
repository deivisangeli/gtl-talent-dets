# Scientific-facilities analyses

## Annual combined-AMWS companion

`analysis_amws_scientific_facilities_annual.R` uses the current
`output/us_panel_county_amws_combined_year.csv` without decade aggregation.
It leaves the existing decennial script and results intact.

Run from the repository root, after setting `TALENT_DETS_DATA_DIR` to the
Dropbox project root:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
Rscript analysis/scientific_facilities/analysis_amws_scientific_facilities_annual.R
```

The full facility list defines first county exposure, using the existing
50 km buffers in EPSG:3857 and 2020 county polygons. Targets have first
exposure before 1944. Annual outcomes cover 1866–1949; event time zero is
the coded facility year, and the reference is −1. Facilities opening in
1944–1949 are donor-only. A later facility in an already-exposed county
does not create a new treatment event.

Five outcomes (counts, log counts, population-normalized and
birth-normalized rates, and population in persons) each receive four figures:

| Curve | Controls | Relative years |
|---|---|---|
| Unbalanced | Never-treated | −10 through maximum supported +73 |
| Unbalanced | Not-yet-treated | −10 through maximum supported +73 |
| Fixed opening-year cohorts | Never-treated | −10 through +7 |
| Fixed opening-year cohorts | Not-yet-treated | −10 through +7 |

Never-treated means unexposed through 1949, including counties first
exposed after 1949. Not-yet-treated additionally admits future-treated
counties while they remain unexposed in both the comparison and reference
years. Donor-only cohort effects are excluded from reported aggregation.

The unbalanced curve's cohort composition can change with event time.
The balanced curve retains only cohorts with estimable effects throughout
the entire requested window; the 1943 cohort cannot enter because +7
would require 1950. Balance is by opening-year cohort, not by county:
counties contributing within a cohort can vary. The estimator permits
incomplete panels; annual missing observations are never filled with zero.
Observed and paired county counts are reported separately because incomplete
panel handling does not force the same counties at both comparison dates.

Aggregation uses cohort county counts, checked against an explicit weighted
calculation. Post-treatment summaries average eligible group-time ATT cells
with cohort-size weights, both over the displayed post horizon and over
0 through +7 for the unbalanced curve. Longer-followed cohorts contribute
more cells to the full-horizon summary. All inference uses joint influence
functions, a 1,000-repetition multiplier bootstrap, county clustering, and
pointwise 95% intervals. The current source panel adds the 1986 edition
without deduplicating it against the earlier editions.

## Outputs and checks

Results are written to a new timestamped directory under:

```text
results/scientific_facilities/event_studies/
  amws_scientific_facilities_annual_pre1944_end1949/<run_tag>/
```

Outputs include 20 PNG figures, dynamic and cohort-time ATTs,
post-treatment summaries, cohort membership, annual coverage, exposure
assignments, missing-data/support audits, and input hashes/package/run
manifests. A pre-existing run directory is never overwritten.

Set `FACILITIES_ANNUAL_RESULTS_TAG` for an explicit new run tag.
`FACILITIES_ANNUAL_BITERS` changes the bootstrap repetitions; use its
1,000 default for reported results.

```powershell
Rscript analysis/scientific_facilities/analysis_amws_scientific_facilities_annual.R --preflight-only
Rscript analysis/scientific_facilities/test_amws_scientific_facilities_annual.R
```

Preflight runs without loading `did`. The tests check donor contamination,
cutoff boundaries, missing observations, fixed cohort composition, and
weighting; when `did` loads they also recover known synthetic effects under
both control groups and both curve versions. Tests explicitly report when
estimator checks cannot run.

If an estimator dependency fails to load, the runner saves the completed
preflight and marks all specifications `blocked_dependency`. It produces no
estimated figures or replacement-estimator results in that case. If an
individual fit or aggregation fails, inspect `model_status.csv`; the run
is complete only when all specifications and figures succeed.

## Expanded complete-decade companion

`analysis_amws_scientific_facilities_decennial_expanded.R` leaves the legacy
decennial analysis intact. It reads the same combined annual panel, then
builds complete calendar decades from 1860–1869 through 1940–1949. Scientist
counts and estimated births are summed; population is averaged; log counts
and rates are recomputed from these aggregates. These are the legacy outcome
definitions, not averages of the annual rates. Each outcome's required
components must contain ten valid annual observations. Incomplete blocks
remain missing; counties need not appear in every decade.

Both legacy timing conventions are produced: standard decade flooring and
shifting opening years ending in 7–9 to the next decade. Exact opening dates
determine target eligibility before rounding (through 1943). The reference is
the preceding assigned decade, e = −10. Shifted reference blocks can contain
exposed years; `timing_exposure_audit.csv` documents this by county. Thus the
shifted curve can measure changes relative to a partially exposed baseline.

Unbalanced curves display −30 through maximum supported +70 in decade steps;
cohort membership varies with support. Balanced curves require estimable
−10, 0, and +10 cells and keep assigned-decade cohorts fixed. The 1940 cohort
is excluded from balanced curves because +10 would require the 1950s.

Never-treated counties remain unexposed through 1949. Not-yet-treated
donors must remain unexposed through the end of both comparison blocks;
an opening decade cannot supply controls, even under shifted timing. The
complete facility list defines donor exposure. Counties opening in 1944–1949
remain donor-only, even when rounding places them in the same decade as a
target county.

Each target cohort–time comparison uses an explicit two-period C&S
`did::att_gt` fit (DR, intercept only, no anticipation, universal base).
The reference maps to period 1 and the comparison to period 2, including
pre-treatment contrasts. Eligible donors map to the comparison group after
actual-date screening. This separates target membership from donor exposure
and avoids contamination from shifted or colliding cohort labels. County
influence functions are divided by each fit's sample count, aligned across
fits, and aggregated with cohort-size weight derivatives. Common county
normal multiplier draws preserve covariance across cells. Standard errors
use the standard deviation of 1,000 bootstrap draws and pointwise 95%
intervals. Post summaries weight cohort-time cells by cohort county counts.

```powershell
Rscript analysis/scientific_facilities/analysis_amws_scientific_facilities_decennial_expanded.R --preflight-only
Rscript analysis/scientific_facilities/test_amws_scientific_facilities_decennial.R
Rscript analysis/scientific_facilities/analysis_amws_scientific_facilities_decennial_expanded.R
```

`FACILITIES_DECENNIAL_RESULTS_TAG` chooses a new run directory;
`FACILITIES_DECENNIAL_BITERS` defaults to 1,000. Results live under
`results/scientific_facilities/event_studies/amws_scientific_facilities_decennial_pre1944_end1949/<run_tag>/`.
Each run contains 40 figures, complete-block outcomes/audits, exposure/timing
assignments, support estimates, county/donor contributions, summaries,
annotations, and manifests. Figure filenames use compact timing/control
codes to stay within Windows path limits; `figure_manifest.csv` maps each
filename to its full specification. Tests recover known effects including a very
large donor-only effect, and compare cell estimates, influence functions,
and joint aggregation against ordinary `att_gt`/`aggte` where definitions
coincide. Failed estimator loading produces a blocked preflight, not a
replacement estimator or uncomputed figures.

## Pre-1920 decennial targets with thirty-year balance

`analysis_amws_scientific_facilities_decennial_pre1920.R` runs the same five
outcomes, two timing conventions and two control groups, using exact first
exposure through 1919. Outcomes still end in 1949. All later exposures remain
in the full exposure history and are donor-only through the observation cutoff.
Balanced curves hold assigned-decade cohorts fixed from -10 through +30;
unbalanced curves use -30 through the maximum supported +70, without a +30 cap.
Both versions export 0:+30 post summaries, plus the unbalanced full horizon.
The 1917 event maps to 1910 under standard timing and can reach +30; it maps
to 1920 under shifted timing and is excluded from that balanced cohort set.
Balance refers to cohorts, not a fixed county panel. Results go to a separate
`amws_scientific_facilities_decennial_pre1920_end1949/<run_tag>` directory.

```powershell
Rscript analysis/scientific_facilities/analysis_amws_scientific_facilities_decennial_pre1920.R
```

## Pre-1920 annual birth-normalized outcome

`analysis_amws_scientific_facilities_annual_pre1920.R` uses only scientists
per 1,000 estimated births, exact first exposure through 1919, and annual
outcomes from 1866 through 1949. It produces four figures: fixed opening-year
cohorts over -10:+30 and changing-support curves over -10:+73, each with
never-treated and not-yet-treated controls. Later exposures through 1949
remain donor-only, and the full facility list determines donor eligibility.
The reference is -1 and the visual separator is -0.5. No annual observations
are averaged or missing values filled. Both versions report 0:+30 summaries;
unbalanced versions also report their full supported post horizon.

Results, support audits and annotations go to a new run under
`amws_scientific_facilities_annual_pre1920_end1949`. The original annual
runner still defaults to all five outcomes and the pre-1944/+7 specification.
Expected figure counts are derived from the specification registry.

```powershell
Rscript analysis/scientific_facilities/analysis_amws_scientific_facilities_annual_pre1920.R
Rscript analysis/scientific_facilities/validate_amws_scientific_facilities_outputs.R <annual_run_directory> - <annual_input_csv>
```

## Annotations in both companions

Every new figure has an upper-right box reporting unique treated counties,
unique control counties, and a pooled baseline outcome mean. Counts cover
the union of actual contributors across the displayed comparisons; counties
may contribute in both roles at different times. Balanced annotations use
only retained cohorts. Baselines pool equally weighted, distinct
county-reference observations, including treated counties and eligible
controls. Reusing an observation does not increase its weight; a county at
different reference dates contributes distinct observations. Annual
references are −1; decade references are −10. Log-outcome baselines average
the log observations. Population uses the interpolated panel series.

`plot_annotation_summary.csv`, `plot_baseline_observations.csv`, and
`county_contributions.csv` make the labels reproducible. Event-specific
control availability remains in support tables; expanded decade runs also
export donor IDs for every displayed comparison.

After estimation, validate the saved outputs against the annual source:

```powershell
Rscript analysis/scientific_facilities/validate_amws_scientific_facilities_outputs.R <annual_run_directory> <decennial_run_directory> <annual_input_csv>
```

Successful validation writes `validation_report.csv` to each run directory.

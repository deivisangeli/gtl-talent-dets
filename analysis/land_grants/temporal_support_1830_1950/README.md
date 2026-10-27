# AMWS land-grants event studies: temporal support 1830-1950

This pipeline keeps Andrews experiments conducted in 1850-1920 and balances
the count outcomes on calendar decades rather than requiring every event to
have the full relative-time window.

Run from the repository root, in order:

```r
Rscript analysis/land_grants/temporal_support_1830_1950/01_build_temporal_support_panel.R
Rscript analysis/land_grants/temporal_support_1830_1950/02_run_main_event_studies.R
Rscript analysis/land_grants/temporal_support_1830_1950/03_run_baseline_population_controls.R
```

Required environment variables are `DET_DIR` and `GTL_REPO`.

The specification uses calendar decades 1830-1950, event time -20 to +90,
and reference period -10. Population is observed from NHGIS/manual knots or
interpolated only between valid knots. Missing population never removes a unit
from the count outcomes.

Prepared panels are written to Dropbox `output/land_grants/`. Model outputs
are written to Dropbox `results/land_grants/event_studies/` in separate main
and baseline-population folders.

The separate [training-location outcome pipeline](../training_50km/README.md)
uses the current manuscript's pooled CSDID regression specification with baseline
estimated births, balanced event time −20 to +70, and displayed reference e = 0.
It reuses this directory's sample helpers and verifies the saved manuscript rate
samples (16 events/52 units/650 observations standard; 20/62/834 alternative).
It preserves these legacy panels, estimators and results.

Unbalanced versions of the `main.tex` college figures use four pre-periods
(e = −40..+70) with `BALANCE_EVENT_TIME=FALSE MIN_EVENT_TIME=-40`, and
`MAX_EVENT_TIME=70` for script 03:

- 03 pooled: `..._baseline_pop_max70_min40/`, 37/38 events.
- 07 per cohort: `..._baseline_pop_per_cohort_max_e_balnone_min40/`.
- 03 with `COHORTS=1860,1870,1880`: `..._baseline_pop_max70_min40_cohorts_1860_1870_1880/`, 19 events.

These outcomes are AMWS counts and rates, not training classes, so the education
extractor version does not affect them.

## 08: Could professors' children explain the result?

`08_run_professor_channel.R` asks whether the AMWS effect could be a
composition channel. A college imports professors, and their children, born
in the county, are unusually likely to become scientists.

**Setup**

- **Input.** It reads professors' children per county × birth decade from
  `output/occupational_fertility/college_channel/`, built by
  `prep/occupational_fertility/build_professor_children_event_panel.R` from
  the 1850–1910 and 1940 full counts.
- **Sample and estimator.** It uses the same events, balance filter and
  CS-DID as 03: `births_baseline`, universal base period, normalised to
  e = 0.
- **Reproduction.** It first reproduces the `main.tex` estimates exactly.

**What it computes**

- **Break-even rate.** p\* = ATT_amws / ATT_prof.
- **Joint inference.** On a common sample the regression-adjusted DiD is
  linear in the outcome, so ATT(n_amws − p·prof_births) = ATT_amws −
  p·ATT_prof exactly, influence functions included. That gives adjusted
  ATTs for any p and a Fieller interval for p\*.
- **Checks needing no value of p:**
  - per-event dose-response
  - dropping the five largest professor importers
  - college type
- **National benchmark.** The share of all AMWS scientists that p\* would
  require.

**Output and runtime.** Output goes to `results/land_grants/professor_channel/`
(`memo.md` first). It runs in about 20 s. Faculty births are observed to
1949 (the 1850–1950 full counts), so the channel loses events only where
1950s births would be needed: standard +70 (10 of 16), alternative +60 and
+70 (15 and 10 of 20). The last birth year comes from the panel's
`build_manifest.json`, and figure captions follow it.

**Two channel measures, both plotted.**
- **`faculty` (scaled):** `faculty_births`, children of 010–029 fathers
  rescaled per census to estimated total male college faculty
  (`prep/occupational_fertility/build_professor_coverage.R`).
- **`raw` (unscaled):** the same 010–029 count with no factor
  (`prof_births_plus_deans`; before 2026-10-26 it was 012–029). The two
  differ only by the factor.

**Results at standard e = +50.** Break-even p\* is 9.3% [5.5, 15.3] scaled
and 21.0% unscaled, falling to 4.5% and 9.1% at +60. The verdict and the
county-coding caveat are in `results/land_grants/professor_channel/memo.md`.

**Headline outcome and figures.** The headline outcome is AMWS per 1,000
births (`main.tex` Figure 2). Scaled files have no suffix; unscaled ones end
in `_unscaled`.

- `fig_channel_paths_per1000_*.png`: panel A reproduces Figure 2 exactly.
- `fig_figure2_adjusted_*.png`: Figure 2 minus p × faculty children.
- `fig_breakeven_p*.png`: break-even p\* per 1,000 births.

Count versions are `*_count*.png`. `*_pdf.pdf` are the title-free copies
that `09_build_faculty_offspring_companion.py` puts into the six-figure
report companion `companion/faculty_offspring_channel.pdf` (Figures 1–3
scaled, 4–6 unscaled).

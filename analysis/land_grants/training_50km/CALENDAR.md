# Calendar-balanced AMWS training-location event studies

This separate release implements Appendix A of the [Google Drive report
companion](https://drive.google.com/file/d/1gvX6r51DtH2ymHyKVBzy5mwCb_ER5cWP/view)
for the training-location outcomes and 50 km county catchments. It balances
calendar decades, permits different cohorts to contribute at different event
times, and excludes every shared county identified in the selected sample.
The Drive file and all earlier preparation and analysis releases are unchanged.

## Commands and source selection

From the repository root, set `TALENT_DETS_DATA_DIR` to the Dropbox project root:

```powershell
Rscript --vanilla prep/land_grants/05_build_amws_training_calendar.R
Rscript --vanilla analysis/land_grants/training_50km/run_calendar_event_studies.R
Rscript --vanilla analysis/land_grants/training_50km/validate_calendar_release.R
```

The extension needs the same installed local R packages as the catchment pipeline.
It performs no downloads, rematching, new geocoding or Drive writes. It reuses
the full 57-event geographic assignments and county outcomes from the original
catchment release, rather than limiting itself to the earlier 16/20-event
event-time-balanced samples.

The builder reproduces the companion's original county-only selection from the
existing panels and sample helpers: **69 calendar-complete anchor counties**, with
**15 events, 46 site units and 598 observations** under each timing. It checks
county and site/event identifiers against the saved `calendar_appendix_20261005`
audits. The reference local runner is `analysis/report_companion_calendar_appendices.R`.

## Calendar coverage, exclusions and final sample

Every included member county must have usable nonnegative outcomes and positive
estimated births in every decade labelled **1830, 1840, …, 1950**, under both
standard and alternative binning. Membership is the common complete set across
timings and is fixed across all periods. Existing within-experiment nearest-site
assignments remain unchanged. No event-time balance filter is applied.

The previous seven exclusions remain frozen: Franklin GA; De Witt and Tazewell
IL; Spencer and Warrick IN; Daviess and Henderson KY. Four additional counties
are shared by the NC State and East Carolina comparisons: **Craven (`37049`),
Greene (`37079`), Lenoir (`37107`) and Pitt (`37147`), North Carolina**. All eleven
counties are removed from every catchment, including original home memberships.
They are not reassigned or restored after event selection.

East Carolina University's event **50 (1907)** then loses its entire treated
catchment. The event and both of its runner-up units are dropped, following the
requirement that an event retain its own treated site and at least one runner-up.
No other initial companion event is dropped. Site and stack identifiers of all
surviving units remain unchanged.

| Timing | Events | Site units | Observations | Member counties | AMWS entries |
|---|---:|---:|---:|---:|---:|
| Standard | 14 | 43 | 559 | 154 | 7,206 |
| Alternative | 14 | 43 | 559 | 154 | 7,198 |

Each site has exactly 13 calendar observations. Event/site/decade membership is
identical across timings, while treatment cohorts, outcomes and baseline births
follow each timing's original definitions. No member county or edition-specific
AMWS entry is reused across retained experiments. Counts refer to source keys,
not verified distinct people across editions.

The source birth-year window remains 1830–1959. Alternative binning moves years
ending in 7–9 forward; its 1960 bin is excluded here. Eight entries born in
1957–1959 therefore appear only in standard timing. The binning audit makes these
exclusions explicit. The original first/last-bin coverage and partial-annual
birth-denominator aggregation conventions are retained and audited.

## Outcomes and model specification

The original three training classifications and all identity qualifications are
preserved: ever nearby, never nearby with fully matched conflict-free awards,
and unknown training. BA/BS, MA, MS and PhD are included. Nearby training remains
relative to each scientist's **own birth-county centroid**, not the allocation
site. Existing AMWS eligibility, earlier `kept`, 1986 contamination and cross-edition
counting rules remain unchanged.

Counts and estimated births are summed across included member counties. Rates
equal **1,000 × summed count / summed estimated births**. The baseline covariate
is the catchment's estimated births at the existing **g−10** decade.

Eight models estimate the three category rates and their total under two timings.
They retain CSDID regression adjustment, never-treated controls, the universal
base period and the manuscript's inference conventions. The same call permits an
unbalanced panel; `did` detects the complete calendar grid and automatically uses
its balanced-panel path. All actual fitting parameters remain in the saved model
objects. Seeds are recorded, starting with base seed 20261014 plus model index.

Dynamic estimates use supported event times **−50 through +70**. Estimates and
influence functions are normalized to **e = 0**, with pointwise 95% intervals.
The separate simple ATT retains its default pre-treatment reference and is not
an average of these normalized dynamic coefficients.

Calendar balance does not guarantee full relative-time coverage. For standard
timing, the native ATT draws on 8 events at e = −50, 14 at e = 0, and 9 at e = +70;
alternative timing draws on 8, 14 and 8, respectively. Counts in figure captions
and `event_time_support.csv` refer to the underlying ATT **before** subtracting
the common e = 0 estimate, which draws on all 14 events. They do not describe the
entire influence-function contribution to each normalized point. Universal-base
reference cells are explicitly identified; unsupported comparisons are not
imputed as zero.

## Results, warnings and validation

All eight models completed. Category effects sum to the total benchmark to
maximum absolute error **6.662e-16**; standard errors and confidence limits are
never summed. At e = +60, normalized effects per 1,000 estimated births are:

| Outcome | Standard estimate [pointwise 95% CI] | Alternative estimate [pointwise 95% CI] |
|---|---|---|
| Ever nearby | 0.214 [0.049, 0.378] | 0.234 [0.076, 0.392] |
| Never nearby | 0.071 [−0.026, 0.167] | 0.024 [−0.062, 0.109] |
| Unknown training | −0.016 [−0.145, 0.113] | 0.118 [−0.016, 0.252] |
| Total | 0.268 [0.017, 0.519] | 0.375 [0.125, 0.625] |

These are selected dynamic coefficients, not overall effects. This version changes
the event set, geographic sample, observation support and event-time aggregation
relative to the earlier balanced-window analyses, so differences do not isolate
the effect of any one sample restriction.

All models retain warnings about small treatment cohorts. **None of the eight
joint pre-trend Wald tests is available**: each has 27 pre-treatment restrictions,
with covariance rank 19–21 at diagnostic QR tolerance 1e-10; reciprocal condition
numbers are below the package's machine-epsilon threshold. No replacement test
or generalized-inverse p-value is substituted. Individual coefficients and
pointwise uncertainty are retained. Missing joint tests do not establish parallel
trends. Diagnostic summaries, tested coefficient indices and covariance matrices
are saved for every model.

The independent validator passed checks of protected previous releases,
companion selection, eleven fixed exclusions, declared event removal, unchanged
assignments and classifications, common calendar membership, pooled counts and
denominators, baseline controls, actual model samples, event-time support,
pre-test diagnostics and coefficient decomposition. Source and copied-file
checksums are verified before completion markers are written last.

The locations remain OpenAlex reference locations with unverified historical
campus accuracy. Original allocation-site coordinates, provisional identity
assignments, career censoring and the retained cross-edition counting convention
continue to qualify interpretation.

## Published outputs

Preparation: `output/land_grants/amws_training_50km_calendar_no_reuse_20261014/`.
Results: `results/land_grants/event_studies/amws_training_50km_calendar_no_reuse_20261014/`.

Preparation includes reproduced companion samples, anchor/member calendar audits,
the eleven-county list, fixed common membership and exclusion reasons, event-drop
audit, annual and decade panels, source-entry classifications and membership,
birth-year binning, denominator completeness and matching coverage.

Results include `dynamic_att.csv`, `post_treatment_att.csv`, model objects,
sample membership, warnings, model status, `event_time_support.csv`,
`cohort_event_time_support.csv`, `pretrend_test_diagnostics.csv`,
`pretrend_coefficients.csv`, and per-model covariance objects.
`training_outcomes_standard_decade.{pdf,png}` and
`training_outcomes_alternative_decade.{pdf,png}` show all three outcomes on a
common vertical scale, with native support counts beneath each timing's figure.
Code snapshots, session details, timing, validation and completion manifests
support replication. Staging files remain local outside Dropbox.

# College-allocation event studies: AMWS training proximity

The all-earned-postsecondary extension adds other bachelor/master/doctoral
qualifications, engineering credentials, diplomas and certificates. See
[expanded scope and refresh commands](../../../prep/amws/education/POSTSECONDARY.md)
for `amws_birth_county_training_postsecondary_20261019` and comparisons with the
earlier four-family release below.

The newest results read education release v05, which adds manual institution matches:

- **Balanced samples:** `run_birth_county_training_postsecondary_v05.R` writes `amws_birth_county_training_postsecondary_v05_20261020`, with `v04_vs_v05_*` comparisons. `validate_birth_county_training_postsecondary_v05.R` checks it independently.
- **Unbalanced sample:** `run_birth_county_training_postsecondary_v05_unbalanced.R` writes `..._v05_unbalanced_20261020`, with `v04_vs_v05_unbalanced_*` comparisons.
- **One PDF:** `../build_college_allocation_volume.py` collects these figures with every college-allocation figure in `main.tex` into `project/college_allocation_event_studies.pdf`.

The previous results use the `multi_v04` extractor:

- **Event-time-balanced and calendar-balanced samples:** `run_birth_county_training_postsecondary_v04.R` and its validator write `amws_birth_county_training_postsecondary_v04_20261019`, with v03-versus-v04 comparisons. See [POSTSECONDARY.md](../../../prep/amws/education/POSTSECONDARY.md).
- **Unbalanced sample:** `run_birth_county_training_postsecondary_v04_unbalanced.R` writes `amws_birth_county_training_postsecondary_v04_unbalanced_20261019`. It has 37/38 events and no balancing; see [BIRTH_COUNTY_TRAINING.md](BIRTH_COUNTY_TRAINING.md#unbalanced-sample-multi_v04-outcomes).

The latest birth-origin extension builds six outcomes: ever/never/unknown
training inside the birth county and within 50 km of its centroid, always
counting only scientists born in that county and dividing by its births. See
[six birth-county training outcomes](BIRTH_COUNTY_TRAINING.md) for preparation,
all 28 models, validation and comparison figures.

The earlier county-target definition uses **training location regardless of birth
county**, with one original county per analysis unit. See the
[county-level training-location guide](COUNTY_TRAINING_LOCATION.md) for the
`amws_county_training_location_20261014` preparation, 32 models and corrected
figure-only companion. Calendar balancing restores the original 15-event sample.
The birth-origin and pooled catchment releases below are preserved separately.

For the additional Appendix A specification balancing calendar decades 1830–1950
and permitting variable event-time support, see [calendar-balanced catchments](CALENDAR.md).
It excludes eleven shared counties, drops the unsupported East Carolina event,
and uses 14 events, 43 sites, 559 observations and 154 member counties per timing.

For the separate extension pooling neighbouring birth counties around selected
and runner-up sites, see [50 km county catchments](CATCHMENTS.md). The county-only
release described below remains unchanged.

The catchment guide also documents the separate sensitivity excluding all seven
cross-event shared counties. It retains the original event/site observations,
uses 195 and 227 counties under standard/alternative timing, and leaves both
earlier versions intact.

This separate 2026-10-14 analysis holds the Andrews quasi-random college-allocation
design fixed and decomposes the AMWS birth outcome by training proximity. It uses
existing OpenAlex assignments, including their provisional qualifications, and
reference coordinates. Historical teaching-campus accuracy remains unverified.

## Commands and inputs

From the repository root, set `TALENT_DETS_DATA_DIR` to the Dropbox project root:

```powershell
Rscript --vanilla prep/land_grants/test_amws_training_50km.R
Rscript --vanilla prep/land_grants/02_build_amws_training_50km.R
Rscript --vanilla analysis/land_grants/training_50km/run_event_studies.R
Rscript --vanilla analysis/land_grants/training_50km/validate_release.R
```

Required local packages are `data.table`, `jsonlite`, `digest`, `sf`, `did`,
`dplyr`, `readr` and `ggplot2`. The runner sets `DET_DIR` to the resolved data root
for compatibility with the existing sample helper. No downloads or APIs are used.
Completed releases are checksum verified on rerun rather than overwritten.

Inputs are `Data/processed/amws/amws_open_alex.csv`, its manifest, the frozen
`andrews_event_county_units_1850_1920.csv`, the standard and alternative temporal
support panels, cached 2020 county polygons, and saved manuscript sample audits.
Source and code checksums are recorded in each release's completion manifest.
The canonical input and existing panels/results are preserved.

## Outcomes and counting

For scientists born in county c, use every extracted BA/BS, MA, MS and PhD award:

| Category | Rule |
|---|---|
| Ever nearby | Any assigned institution is within 50 km, inclusive, of c's centroid. |
| Never nearby | At least one reported award; every award fully matched to usable locations without conflicts; every location outside 50 km. |
| Unknown training | Every other case, including missing awards or incomplete matching. |

Ever nearby takes precedence even when another award is unmatched. Missing years
do not invalidate unconditional IDs; conditional assignments are inherited from
the published export without reevaluation. Multiple institutions and their
qualifications remain separate. The categories partition every eligible entry.
They measure reference-location proximity, not attendance at the allocated college.

Birth years 1830–1959, existing county and earlier-edition `kept` rules, and the
1986 US-birth and see-previous contamination rules are retained. No additional
cross-edition deduplication is performed. Counts are edition-specific directory
entries under the existing convention, rather than verified distinct people.
The 57-event, 168-county preparation pool has 12,360 eligible entries: 3,413 ever
nearby, 5,275 never nearby and 3,672 unknown (29.7%). Unknown reasons are 598 without
a qualifying award, 116 without institution text, 1,076 without an assigned
location, 1,878 with an unmatched award, and four with a partially matched award.
Known nearby records can also have unmatched other awards.

County centroids use geometric centroids in EPSG:5070, transformed to EPSG:4326;
`sf` S2 great-circle distances are in metres. Each annual category count is binned
using the legacy timing rules. Standard timing floors to the decade; alternative
timing moves years ending in 7–9 forward. Existing population, births, controls
and missingness are retained. Rates equal 1,000 times counts / estimated county
births. Genuine zero counts remain zero; unavailable/nonpositive denominators
yield missing rates. The category rates sum to the updated-total benchmark.

The legacy panel contains one additional record in county `17113`, decade 1930,
under each timing. Canonical `1955:46642` uses county `42015` outside the event
pool, whereas the legacy selected source variant used `17113`. The reconciliation
documents this birthplace-selection difference without changing geography.

## Frozen manuscript specification

`run_event_studies.R` reuses `temporal_support_1830_1950/_sample_helpers.R` and
the manuscript's `03_run_baseline_population_controls.R` estimation conventions:

- `did::att_gt`, regression adjustment (`est_method="reg"`), never-treated
  controls, universal base period, baseline estimated births as the covariate,
  `allow_unbalanced_panel=TRUE`, and the existing default inference settings.
- Existing event assignments, runner-up controls, stack identifiers, baseline
  control timing, and event selection remain fixed. Balance is evaluated over
  event time −20 to +70 using the existing support rule. All calendar observations
  retained by that rule enter estimation, including those outside the plotted
  window. The panel can therefore be unbalanced in calendar time.
- Standard timing: **16 events, 52 stack units, 650 observations**. Alternative:
  **20 events, 62 stack units, 834 observations**. Every outcome has exactly the
  same timing-specific identifiers, observations and baseline control values as
  the regenerated legacy rate sample. Saved event and model audits are checked.
- Four rates under each timing give eight models: three categories and the
  updated total. Base seed 20261014; model seeds are recorded individually.
- Dynamic coefficients and influence functions are renormalized to e = 0 using
  the manuscript formula. Figures show pointwise 95% intervals, estimate ±1.96 SE.
  `post_treatment_att.csv` retains the manuscript's separate simple ATT call and
  its default pre-treatment reference; it is not the e = 0 normalized dynamic
  series and should not be interpreted as its average.

## Results and limitations

All eight models completed. The standard and alternative dynamic decompositions
pass, with maximum absolute error 1.12e-15. Standard errors and confidence limits
are calculated for each model and are never summed. At e = +60, normalized
effects on scientists per 1,000 births are:

| Outcome | Standard estimate [95% CI] | Alternative estimate [95% CI] |
|---|---|---|
| Ever nearby | 0.242 [0.027, 0.458] | 0.253 [0.096, 0.410] |
| Never nearby | 0.197 [0.049, 0.346] | 0.179 [0.040, 0.317] |
| Unknown training | 0.173 [−0.001, 0.348] | 0.172 [0.058, 0.285] |
| Updated total | 0.613 [0.275, 0.952] | 0.603 [0.284, 0.923] |

These are selected dynamic coefficients, not overall effects. See the complete
series before drawing conclusions. The alternative never-nearby e = −10
coefficient has a pointwise interval excluding zero, so the plots do not support
an unqualified claim of flat pre-treatment dynamics. Individual pointwise
intervals do not provide a simultaneous test across event times.

`did` warns that some treatment cohorts have few observations. It cannot return
the pre-trend Wald statistic for ever-nearby models because of singular covariance
matrices, and flags an unreliable simultaneous critical value for the alternative
ever-nearby model. All warnings are retained in `warnings.csv`; no estimator or
sample is changed in response. The displayed intervals follow the manuscript's
pointwise influence-function convention, rather than that simultaneous critical
value. Matching incompleteness, provisional IDs, reference-location uncertainty,
career censoring and the retained cross-edition convention limit interpretation.

## Published files

Preparation: `output/land_grants/amws_training_50km_20261014/`.
See [the preparation guide](../../../prep/land_grants/README.md) for its files.

Results: `results/land_grants/event_studies/amws_training_50km_20261014/`:

- `dynamic_att.csv`, `post_treatment_att.csv`, `models/*.rds`, `model_status.csv`
  and `warnings.csv` retain estimates, inference, objects, seeds and diagnostics.
- `frozen_sample_*.csv`, `frozen_sample_audit.csv` and
  `estimation_sample_membership.csv` freeze the exact timing-specific samples.
- `main_sample_matching_coverage.csv` audits edition, allocation cohort, role,
  training class and missingness for stacked sample observations. A physical
  county or person may appear in several stacks; these counts must not be
  interpreted as distinct scientists without taking a union of source keys.
- `effect_decomposition_check.csv`, `validation_report.json`, source/output
  checksums, code snapshots, session/package details and timing support replication.
- `training_outcomes_standard_decade.{pdf,png}` and
  `training_outcomes_alternative_decade.{pdf,png}` show all three categories with
  the same vertical scale and a separate figure for each timing.

Local staging is outside Dropbox. Files are copied and their checksums verified
before `completion_manifest.json` is written last. The independent validator
checks both release manifests and reconstructs key count, rate, distance and
sample relationships without modifying the published data.

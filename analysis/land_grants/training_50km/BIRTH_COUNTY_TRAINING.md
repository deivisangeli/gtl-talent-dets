# Six birth-county training outcomes

This guide describes the original four-family release. For the expanded
postsecondary release, updated counts and latest zero-extraction audit, see
[postsecondary qualifications](../../../prep/amws/education/POSTSECONDARY.md).
That guide also covers the `multi_v04` re-extraction and its refreshed outcomes
and event studies. The unbalanced-sample event studies on those outcomes are
described [at the end of this guide](#unbalanced-sample-multi_v04-outcomes).

The `amws_birth_county_training_20261019` release decomposes scientists **born in
county c** by where they earned their recorded degrees. Both geographic versions
use births in county c as the denominator. This differs from the earlier
birthplace-independent county-target release and does not pool neighboring birth
counties.

## Definitions

For each eligible edition-specific AMWS entry, include every recorded earned
BA/BS, MA, MS and PhD award. Graduation years are not required. Existing
conditional and provisional institution assignments remain in force.

| Category | Classification |
|---|---|
| Ever | Any assigned institution qualifies as local, including when other awards are unresolved. |
| Never | At least one reported award; all awards completely matched to valid locations without conflicts; all locations outside the geography. |
| Unknown | All remaining entries, including absent education and incomplete institution matching. |

The exact-county version uses the cached 2020 polygon for the birth county,
including boundary points. A point touching multiple county boundaries qualifies
for each touching county, but an entry contributes only to its own birth county.
The 50 km version uses an inclusive 50,000 metre S2 great-circle distance from
the geometric birth-county centroid (computed in EPSG:5070 and transformed to
longitude/latitude). A nearby institution may be across a county border. These
geographies are not necessarily nested, and unknown classifications can differ.

For each geography, the three categories partition the same county-born total.
Rates equal `1000 * category_count / county_births_estimate` for the birth cohort.
Zero counts remain zero when births are valid; missing or nonpositive births
produce missing rates. Timing is birth cohort, rather than graduation cohort.

| Geography | Rate columns |
|---|---|
| Exact county | `amws_born_ever_trained_same_county_per_1000_births`, `amws_born_never_trained_same_county_per_1000_births`, `amws_born_training_unknown_same_county_per_1000_births` |
| 50 km | `amws_born_ever_trained_50km_per_1000_births`, `amws_born_never_trained_50km_per_1000_births`, `amws_born_training_unknown_50km_per_1000_births` |

Counts have the same stems prefixed with `n_` and without `_per_1000_births`.
The shared benchmark is `amws_born_total_per_1000_births` / `n_amws_born_total`.
`outcome_dictionary.csv` gives the complete column mapping.

## Run

From the repository root in PowerShell:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
Rscript --vanilla prep/land_grants/test_amws_birth_county_training.R
Rscript --vanilla prep/land_grants/09_build_amws_birth_county_training.R
Rscript --vanilla analysis/land_grants/training_50km/run_birth_county_training.R
Rscript --vanilla analysis/land_grants/training_50km/validate_birth_county_training.R
```

On another machine, set the data root to the local Dropbox project folder.
Use the same R packages as the existing training pipeline (`data.table`,
`jsonlite`, `digest`, `sf`, `did`, and `ggplot2`). No new network research or
downloads are required. Existing source data, polygon cache, birth-origin 50 km
release and frozen county samples must be present. Completed releases are
checksum-verified on rerun rather than overwritten. Checkpoints use
`AMWS_CHECKPOINT_ROOT`, defaulting to `~/.codex/amws_checkpoints/`.

## Outputs and estimation

Preparation is under `output/land_grants/amws_birth_county_training_20261019/`:
all-source eligibility/classification audit, award evidence, institution
coordinates and polygon/distance membership, county centroids, annual counts,
both decade panels, frozen samples, coverage, legacy reconciliation, code
snapshots and checksum manifest. The annual file contains counts; cohort rates
are in `county_decade_standard_decade.csv` and
`county_decade_alternative_decade.csv`. Existing panel columns are preserved.

Results are under
`results/land_grants/event_studies/amws_birth_county_training_20261019/`.
There are 24 requested models and four county-born total benchmarks:

| Design | Timing | Events | Stack units | Observations |
|---|---|---:|---:|---:|
| Relative-time balance | Standard | 16 | 52 | 650 |
| Relative-time balance | Shifted | 20 | 62 | 834 |
| Calendar balance | Standard | 15 | 46 | 598 |
| Calendar balance | Shifted | 15 | 46 | 598 |

The runner preserves `did::att_gt` regression adjustment, never-treated
controls, universal base period, own-county births at allocation decade minus
10 as the covariate, unbalanced-panel allowance, and default package inference.
Relative dynamics span -20 to +70; calendar dynamics span -50 to +70. Dynamic
estimates and influence functions are shifted to reference e=0; overall ATT
retains its original pre-treatment reference. Calendar samples retain East
Carolina. Standard timing floors birth years to decades; shifted timing moves
years ending in 7-9 forward.

Exports include model RDS files, analysis panels and membership, dynamic and
overall ATT with uncertainty, warnings, pre-trend covariance/rank diagnostics,
native event-time support, category-effect decomposition checks, and four
six-panel figures in both PNG and PDF. Each figure has exact-county and 50 km
rows and ever/never/unknown columns, with a common vertical scale.

The independent validator re-reads canonical eligibility, awards and assigned
coordinates, reverses the spatial predicate to polygon covers, recalculates
individual distances and aggregates, checks frozen samples and birth controls,
and reconstructs influence functions and confidence intervals. Its report is
separate from the immutable release, at
`results/land_grants/event_studies/amws_birth_county_training_20261019_validation/independent_validation.json`.

## Coverage and interpretation

The 136,489 canonical source entries yield 12,360 eligible entries born in the
168 allocation counties, using birth years 1830-1959, earlier-edition `kept`
flags, and the existing 1986 US-birth and see-previous exclusions.

| Training geography | Ever | Never | Unknown | Total |
|---|---:|---:|---:|---:|
| Exact county | 2,562 | 5,940 | 3,858 | 12,360 |
| 50 km | 3,413 | 5,275 | 3,672 | 12,360 |

The 50 km classifications reproduce the earlier birth-origin release exactly.
All 28 models completed. Maximum error when summing category effects to the
total is 1.45e-15 for dynamic estimates and 3.06e-16 for overall ATT. Small-cohort
warnings are retained. Joint pre-trend Wald tests are unavailable because of
singular covariance in four relative-time models (the ever category under each
geography/timing) and all 14 calendar models; their covariance diagnostics are
exported rather than treating unavailable tests as passes. Other pre-trend
statistics and p-values are saved in `pretrend_test_diagnostics.csv`.

Both panels retain the known one-entry difference from legacy counts in county
17113, decade 1930: canonical entry `1955:46642` is assigned to county 42015,
outside this pool. This release documents that discrepancy without correcting
birthplace data.

Counts preserve edition-specific source keys and existing inclusion rules, not
newly deduplicated people. Institution locations are attached OpenAlex reference
coordinates; historical teaching campuses and historical county boundaries are
not reconstructed. Training measures refer to any included institution, rather
than exclusively to the allocated college.

## Unbalanced sample (multi_v04 outcomes)

`run_birth_county_training_postsecondary_v04_unbalanced.R` drops both balancing
rules. A unit enters when its county has positive estimated births in the decade
before allocation, which is its baseline covariate, and at least one usable
decade from the allocation on. An event needs its treated county and at least one
runner-up. Stack identifiers and baselines follow `build_stack()`, and every unit
of the event-time-balanced sample appears with the same identifiers and covariate.
The span is 1830–1950 under standard decades and 1830–1960 under shifted decades.

| Timing | Events | Units | Observations |
|---|---:|---:|---:|
| Standard | 37 | 116 | 1,390 |
| Shifted | 38 | 118 | 1,528 |

Twenty events (nineteen under shifted decades) are still excluded.
`excluded_events.csv` lists them:

- **17 (16 shifted):** the treated county has no births in the decade before allocation. These are mostly frontier counties not yet enumerated.
- **3:** no runner-up qualifies.

The same 7 outcomes and CSDID settings run under both timings: 14 models,
dynamic window −30 to +90 and overall ATT over e = 0..70. Outputs go to
`results/land_grants/event_studies/amws_birth_county_training_postsecondary_v04_unbalanced_20261019/`.

**Composition varies by horizon.** All events contribute from −10 to +40. The
count falls after that, as late allocations run out of follow-up at the end of
the panel: 32/25/20/14/8 events at +50 to +90 under standard decades.
`event_time_support.csv` records the support, and the figures print it. Read the
late horizons as effects for early allocations only.
`unbalanced_vs_balanced_*.csv` compares the estimates with the
event-time-balanced v04 results.

The v05 sibling, `run_birth_county_training_postsecondary_v05_unbalanced.R`, uses
the same sample and expected sizes on education release v05. It writes
`amws_birth_county_training_postsecondary_v05_unbalanced_20261020/`, which adds
`v04_vs_v05_unbalanced_*.csv`. That comparison asserts the county-born totals are
unchanged.

## Shifted-decade definitions compared (v06)

The v06 shifted-decade models (`alternative_decade`) shift both the outcome
(birth years ending in 7-9 move to the next decade) and the treatment
(`g_shift`). `run_birth_county_training_v06_outcome_shift.R` estimates the
second definition, which shifts the outcome only: the standard-decade frozen
samples with treatment at `g_std`, outcomes from
`county_decade_alternative_decade.csv`, and baseline births from the shifted
decade before allocation. It runs 14 models with the same estimator, windows and
seeds, and writes `amws_birth_county_training_v06_outcome_shift_20261022/`,
with `shift_definition_comparison_*.csv` and two overlay figures.
`build_decade_shift_comparison.py` collects them, with the standard-decade
reference figures, into a four-page PDF under
`results/land_grants/companions/birth_county_training_decade_shift_comparison_20261022/`.
The calendar samples are identical under both definitions (15 events, 46 units),
so that comparison isolates the treatment shift. The relative sample gains
events 39, 40, 42 and 43 when the treatment is shifted (20 against 16), so its
comparison also reflects sample composition.

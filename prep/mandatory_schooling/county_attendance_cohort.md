# County × cohort schooling around the first compulsory-schooling laws, 1907–1920

For each of the 13 states whose first durable compulsory-attendance law took effect in
1907–1920, this build compares the schooling of birth cohorts too old to be covered with
cohorts the law covered, county by county. The sample is white, US-born, non-migrant men
born 1890–1920, located in their county of schooling through the IPUMS MLP linked extract.

| Script | Does |
|---|---|
| `extract_linked_attendance_cohort_inputs.py` | One streaming pass over `usa_00001.csv.gz`, ~4 min |
| `build_county_attendance_cohort.R` | Law table, person panel, county × cohort, county contrast, diagnostics, ~2 min |
| `test_extract_linked_attendance_cohort.py` | 20 synthetic-fixture checks on the extractor |
| `test_county_attendance_cohort.R` | 37 synthetic-fixture checks, driving the real build through `ATT_COHORT_IN_DIR` / `ATT_COHORT_OUT_DIR` |

Outputs go to `output/mandatory_schooling/attendance_cohort/`, with the extractor's Parquet
files in `inputs/`.

This is a parallel build. It does not replace `county_bite_from_linked_census.md` (other
cohorts, other placement rule, other requirement bases) or the attendance bite.

---

## Design

| Choice | Rule |
|---|---|
| Laws | First year `T` of the state's **last** law spell in `cls_state_age_limits_1880_1930`, kept if 1907 ≤ `T` ≤ 1920. Using the last spell handles Arizona's repeal. Amendments to existing laws are not events. |
| Cohorts | `yob = 1940 - AGE - 1`, from 1890 to 1920 (`AGE` 19–49). Everyone is 19 or older in April 1940, so high-school completion is observable. |
| Sample | 1940 `SEX == 1`, `RACE == 1`, `BPL` 1–56 |
| Exposure | **Unexposed** if the man's age at `T` was at least `exitage + 1`, i.e. `yob <= T - exitage - 1`. Otherwise **exposed**. For an 8–14 law passed in 1917, the 1902 cohort (15) is unexposed and 1903 (14) is exposed. |
| County | The man's **earliest** linked `COUNTYICP`, used only if he was **18 or younger** in that record. Otherwise he is unplaced: counted in the funnel, never assigned. |
| Non-migrant (headline) | `BPL` = 1940 `STATEFIP`, `MIGRATE5` ∈ {1 same house, 2 moved within state}, and the earliest-record state = `BPL` |
| Non-migrant (loose) | `BPL` = 1940 `STATEFIP` only. Carried as `nonmig_loose`, not used in the tables. |

### Outcomes, from 1940 `HIGRADED`

| Variable | Definition |
|---|---|
| `ever_attended` | `HIGRADED >= 040`, i.e. completed 1st grade. 010 = none. Nursery school and kindergarten (011–032) are 17 men of 21.6 M, so the cut-off does not matter. |
| `years` | CLS construction: `g = HIGRADED %/% 10`; `g <= 3` → 0; `g <= 20` → `g - 3`; `g <= 23` → 17. Codes 000/999 are dropped. |
| `years_hs` | `min(years, 12)`, the high-school cap |
| `law_min` | `exitage - entryage` of the law as passed |
| `law_min_effective` | `effective_leaving_age - entryage`, net of the work-permit and early-dropout exemptions |
| `meets_min` | `years >= law_min` |
| `gap_hs` | `years_hs - law_min`, averaged per county. `gap` is the uncapped version and `gap_hs_effective` uses the effective minimum. |
| `hs_complete` | `years >= 12` |

**`law_min` is held fixed across every cohort in the state, exposed or not.** The cohort's
own requirement is 0 before the law and ramps up after it. A gap measured against that
would mostly record the yardstick changing, not the children (C7 in
`county_bite_from_linked_census.md`). Against a fixed minimum, the before/after change in
`gap_hs` equals the change in `years_hs`.

### Exposure variables carried with every row

| Variable | Meaning |
|---|---|
| `rel_cohort` | `yob - (T - exitage)`. The last unexposed cohort is −1 and the first exposed is 0. |
| `full_exposure` | `yob >= T - entryage`: covered for the law's whole age range |
| `years_exposed` | Ages in the CLS window `[entryage, exitage)` that the cohort reached in or after `T` |

**`rel_cohort` 0 is exposed with `years_exposed` 0.** The first exposed cohort is exactly
`exitage` at `T`. Under the CLS convention (`entryage <= age < exitage`) the window is
already closed for it, but the user's rule counts it exposed. Partial exposure then ramps
up over `exitage - entryage` cohorts (4–9 here). No sharp jump should be expected at the
boundary; the `full` contrast below exists for that reason.

---

## The 13 law states

| State | `T` | Ages | `law_min` | Effective | Last unexposed | First full exposure |
|---|---|---|---|---|---|---|
| Delaware | 1907 | 7–14 | 7 | 7 | 1892 | 1900 |
| Oklahoma | 1908 | 8–16 | 8 | 8 | 1891 | 1900 |
| North Carolina | 1913 | 8–12 | 4 | 4 | 1900 | 1905 |
| Tennessee | 1913 | 8–16 | 8 | 6 | 1896 | 1905 |
| Louisiana | 1916 | 7–15 | 8 | 8 | 1900 | 1909 |
| Texas | 1916 | 8–14 | 6 | 6 | 1901 | 1908 |
| Alabama | 1917 | 8–16 | 8 | 7 | 1900 | 1909 |
| Arkansas | 1917 | 7–16 | 9 | 7 | 1900 | 1910 |
| Georgia | 1917 | 8–14 | 6 | 4 | 1902 | 1909 |
| Virginia | 1918 | 8–12 | 4 | 4 | 1905 | 1910 |
| Florida | 1919 | 7–16 | 9 | 8 | 1902 | 1912 |
| South Carolina | 1919 | 8–14 | 6 | 6 | 1904 | 1911 |
| Mississippi | 1920 | 7–15 | 8 | 8 | 1904 | 1913 |

- 11 of the 13 are Southern, which is the region collinearity CLAUDE.md already documents
  for this pipeline.
- **Delaware and Oklahoma have only 3 and 2 unexposed cohorts** (1890–1892, 1890–1891).
  Their contrasts are flagged `short_pre_window` and rest on few men: Oklahoma's unexposed
  side is 1,006 men, against 6,459 exposed.
- Oklahoma became a state in 1907, one year before its law. Its 1890–1891 cohorts grew up
  in Indian and Oklahoma Territories.
- **Benchmark:** every state whose first law predates 1907 already covers all of 1890–1920.
  `state_cohort.csv` carries these states, and a pooled `benchmark_pooled` series, as the
  secular trend to read the law states against.

---

## Outputs

| File | Grain | Notes |
|---|---|---|
| `law_events.csv` | state | All 49 states with `role` ∈ {`law_state`, `benchmark`, `later_law`} |
| `students.parquet` | `HIK` | Every man in the window, with every flag. Filter on `has_attainment AND placed AND nonmig AND county_geoid IS NOT NULL AND in_law_state` for the analysis sample. `half` is a reproducible random split. |
| `county_cohort.parquet` / `.csv` | county × `yob` | Law states, analysis sample. Shares and means with standard errors, the exposure variables, `share_placed_from_19xx`, `mean_age_at_placement`, `below_min_cell` (n < 25). |
| `county_exposure_contrast.csv` | county × `window_k` ∈ {3, 5} × `exposed_def` ∈ {`ramp`, `full`} | The unexposed side is always `rel_cohort` −k…−1. `ramp` takes the exposed side as `rel_cohort` 0…k−1; `full` takes it as the first k fully exposed cohorts. For each outcome: `unexp_`, `exp_`, `diff_` (exposed − unexposed) and `se_`. Flags `below_min_cell` and `short_pre_window`. |
| `state_cohort.csv` | state × `yob` | Law states, benchmark states, and the pooled benchmark |
| `sample_funnel.csv` | `yob` | White US-born men → with attainment → placed → non-migrant → with county → in a law state, plus the outcomes of unplaced men |
| `signal_diagnostics.csv` | outcome × scope | Within state × cohort SD of county cells against the binomial noise floor, and split-half reliability of the k=5 `ramp` county contrast |

---

## What the build produced

First full run, 2026-09-22. It reproduced the planning run exactly: an ever-attended cell
mean of 0.9888 and a signal ratio of 1.46 over all states.

### Sample

- White, US-born men born 1890–1920 in the extract: **18,313,078**.
- **2,484,283** in the analysis sample in the 13 law states, spread over 36,391 county ×
  cohort cells.

| `yob` | Men | With attainment | Placed | Placed, non-migrant | In a law state |
|---|---|---|---|---|---|
| 1890 | 427,749 | 418,194 | 69.1% | 47.8% | 36,812 |
| 1900 | 557,305 | 546,164 | 79.5% | 55.1% | 63,627 |
| 1910 | 610,637 | 599,863 | 90.8% | 67.3% | 84,812 |
| 1920 | 810,649 | 796,196 | 100.0% | 80.9% | 141,032 |

Placement reaches 100% from 1915 on. For those cohorts the only earlier censuses find them
as children, and the extract only holds 1940 men linked to at least one earlier census.
The gradient is the linkage gradient: older cohorts must be found in 1900 or 1910. Men who
cannot be placed report slightly less schooling (1890: 98.1% ever attended against 98.8%),
so placement selects mildly upward.

### State-level contrast, k = 5, exposed = `rel_cohort` 0–4

Exposed minus unexposed, pooled over counties and weighted by men:

| State | Ever attended (pp) | Meets `law_min` (pp) | `gap_hs` (years) |
|---|---|---|---|
| Alabama | +0.77 | +1.44 | +0.21 |
| Arkansas | +0.44 | +2.74 | +0.17 |
| Delaware | +0.52 | +2.26 | +0.28 |
| Florida | +0.36 | +3.40 | +0.23 |
| Georgia | +0.48 | +1.63 | +0.21 |
| Louisiana | +1.10 | +2.17 | +0.23 |
| Mississippi | +0.45 | +2.97 | +0.34 |
| North Carolina | +1.16 | +2.60 | +0.34 |
| Oklahoma | +0.99 | +8.30 | +0.55 |
| South Carolina | +1.18 | +4.04 | +0.40 |
| Tennessee | +0.51 | +2.02 | +0.15 |
| Texas | +0.19 | +2.29 | +0.30 |
| Virginia | +0.81 | +1.75 | +0.38 |

- **Every state is positive on all three**, but none of this nets out the secular trend.
- Over the same birth years the benchmark states rise **+0.04 pp** in ever attended and
  **+1.37 pp** in grade 6 or more (planning run).
- **Oklahoma's +8.3 rests on 1,006 unexposed men** in two cohorts born before statehood.
  Do not read it.

### County level: the levels have signal, the before/after contrast does not

Within state × cohort, the county spread of each cell-level outcome is larger than sampling
noise alone would produce. Cells have n ≥ 25; the law-state column covers 27,429 cells.

| Outcome | Law-state mean | Cells at 0 or 1 | Signal ratio |
|---|---|---|---|
| `ever_attended` | 0.980 | **39.1%** | 1.59 |
| `meets_min` | 0.720 | 1.5% | 2.06 |
| `hs_complete` | 0.235 | 0.3% | 1.83 |
| `gap_hs` / `years_hs` | 1.51 / 8.14 | — | 2.37 |

But the **county contrast is not reliable**. Split the men of each county into two random
halves and recompute its k=5 `ramp` difference in each; over the 971 counties with ≥ 25 men
on both sides in both halves, the two halves correlate:

| `diff_` | r | Spearman–Brown |
|---|---|---|
| `ever_attended` | 0.197 | 0.33 |
| `meets_min` | 0.040 | 0.08 |
| `gap_hs` | 0.053 | 0.10 |

So a county's own before/after change is **almost entirely sampling noise**. This is the
same finding as C7 in the bite document (ratio 1.07), reached by a different route. The
county median differences are small and positive: `gap_hs` +0.26 under `ramp` and +0.64
under `full` at k=5, and `ever_attended` +0.4 and +0.7 pp. They are usable as a pooled
mean. **They are not usable as a county ranking of bite.**

What the county dimension does support is **levels**: county attainment among the
unexposed cohorts (`unexp_*`), i.e. how much room the law had to move. That is the analogue
of the attendance bite's pre-law measure, and it passes the signal test.

---

## Traps

| Trap | Consequence |
|---|---|
| Ranking counties by `diff_*` | Split-half r is 0.04–0.20; the ranking is noise |
| Reading `ever_attended` as a county measure | 39% of law-state cells sit at exactly 0 or 1 (almost all at 1); it cannot register change where there is no room |
| Reading a `diff_*` as the law's effect | No trend adjustment; the benchmark states rise too (+1.4 pp at grade 6 or more over the same years) |
| Expecting a jump at `rel_cohort` 0 | That cohort is exposed with `years_exposed` 0; coverage ramps over 4–9 cohorts |
| Comparing `gap_hs` across states | `law_min` runs from 4 (North Carolina, Virginia) to 9 (Arkansas, Florida); compare within state, or use `years_hs` |
| Reading Delaware or Oklahoma contrasts | 2–3 unexposed cohorts (`short_pre_window`); Oklahoma's predate statehood |
| Treating `placed` as random | It runs 69% → 100% across cohorts, and unplaced men have less schooling |
| Joining `county_geoid` on a padded string | It is an integer, `STATEFIP * 1000 + COUNTYICP / 10`; codes ending in 5 are NULL |
| Pooling Southern and non-Southern evidence | 11 of 13 law states are Southern; the benchmark is mostly not |

# A county-level bite measure for the compulsory-attendance laws

How to build, for each county, the gap between the schooling students actually completed and
the schooling the law required of them — nominal (age limits only) and effective (net of the
work-permit, early-dropout and continuation-school exemptions). Inputs are the IPUMS MLP
linked extract and the CLS law data. The variable-by-variable reference is
[`ipums_linked_1900_1940_dictionary.md`](ipums_linked_1900_1940_dictionary.md); the
state-level join is [`merge_cls_to_linked_census.md`](merge_cls_to_linked_census.md), which
this document assumes you have read.

---

## Start here: the county variation is in attainment, not in the law

Required years vary on **state × cohort**. Observed schooling varies on the individual. So
in `mean(observed - required)`, the county-to-county variation is county mean attainment and
nothing else — the required term enters only through cohort composition. Any specification
with state × cohort fixed effects absorbs the whole measure. That is the warning already
made at `merge_cls_to_linked_census.md:250-255`, and it applies in full force here.

Two of the four measures below survive it, because they are **not linear** in attainment:

- **`shortfall_rate`** — the share of students finishing below the legal minimum. Depends on
  where the county's attainment distribution sits relative to a threshold, which is not
  recoverable from the county mean.
- **`gap_censored`** — the mean deficit below the minimum, ignoring schooling beyond it.
  Same argument.

`gap_mean` and the percentiles are reported as the descriptive benchmark they are.

**This is a descriptive measure of whether the law bound, not a source of exogenous
variation.** It answers "where was the law binding on the children who lived there", not
"what did the law cause". `merge_cls_to_linked_census.md:255-259` pre-committed to building
bite from county literacy in `county_nhgis_demographics_panel.csv` with **no linkage at
all**. That route is still the right one for anything causal. This document takes the other
route — actual completed attainment, compared to the threshold person by person — and pays
for it in linkage selection. **Build both and correlate them.** If they disagree, the
linkage is the first suspect.

---

## Part A — the student-level panel

### A1. Take observed schooling from the 1940 rows only

Attainment exists in one census year. `HIGRADE`, `HIGRADED`, `EDUC` and `EDUCD` sit in the
dictionary's "1940 only" group and read `0.0%` fill in 1900, 1910, 1920 and 1930.

**Do not substitute `SCHOOL`.** It is present in all five years at 100.0% fill, and it is
not comparable across them: the reference period runs ~12 months in 1900, ~7.5 in 1910, ~4
in 1920, ~6 in 1930 and ~1 month in 1940. It measures enrolment on a shifting window, not
completed years.

`LIT` runs 1900–1930 at 100.0% and is `0.0%` in 1940 — literacy stops exactly where
attainment begins, so there is no year in which both are observed.

### A2. Build the cohort key

```r
mutate(yob = 1940L - AGE - 1L)
```

`BIRTHYR` equals `1940 - AGE` for **100.0%** of the 73,832,840 1940 rows — the convention
CLS reject. See `merge_cls_to_linked_census.md` A2 for why; do not re-derive it here.

### A3. Construct years of schooling

There is no years-of-schooling variable in IPUMS. Use CLS's construction from `HIGRADED`,
which `analysis/mandatory_schooling/describe_cls_policy_environment.R:235-242` already runs
and validates against their published Table 1:

```r
filter(HIGRADED != 999L) |>
  mutate(g = HIGRADED %/% 10L,
         observed_years = dplyr::case_when(g <= 3L ~ 0L,
                                           g <= 20L ~ g - 3L,
                                           g <= 23L ~ 17L))
```

`999` is not-reported and covers ~7% of rows. **Do not use `EDUC`/`EDUCD`** — a different
IPUMS recode with different bin edges, not a year count.

### A4. Fix the sample

Default to the CLS-comparable restriction, so the state-level measure can be checked against
their published moments before the county dimension is trusted:

```r
filter(AGE >= 27L, AGE <= 54L,   # yob 1885-1912, the CLS estimation window
       SEX == 1L, RACE == 1L,    # white men, as CLS
       BPL <= 56L)               # US-born
```

Then build a **full-population variant** with `SEX` and `RACE` released, and report the two
side by side. Bite among white men is not bite among everyone, and the difference is a
result rather than a robustness check. State which variant every table uses.

---

## Part B — locating the student's school-age county

### B1. Why the linkage is needed

No census recorded county of birth, and `COUNTYICP` is county of *residence*. See
`merge_cls_to_linked_census.md` B1.

What this document needs is narrower than a birth county and easier to justify: the county
the student lived in **while the law applied to them**. Residence at school age is the right
object here, not a proxy for a birth county.

### B2. Assemble the candidate observations

`HIK` is the cross-year person key. `HISTID` is not — it identifies a record within one
census year.

Every earlier row in this extract links to 1940, so the `LINK19xx` counts on the 1940 rows
are exact and equal the earlier years' row totals:

| Candidate year | 1940 rows reachable | Share of 73,832,840 |
|---|---|---|
| 1900 | 12,971,952 | **17.57%** |
| 1910 | 23,984,383 | **32.48%** |
| 1920 | 39,982,220 | **54.15%** |
| 1930 | 70,207,628 | 95.09% — unusable, these cohorts are adults by then |

```r
candidates <- linked |>
  filter(YEAR < 1940L) |>
  semi_join(students, by = "HIK") |>
  transmute(HIK, cand_year = YEAR, cand_statefip = STATEFIP,
            cand_countyicp = COUNTYICP, cand_age_observed = AGE)
```

### B3. Choose the year by the statutory window

The window depends on the state, and the state is only known per candidate. So each
candidate is scored against **its own** observed state — two passes, not one, and not
circular.

```r
windows <- laws |>                                    # cls_state_age_limits_1880_1930
  filter(year %in% c(1900L, 1910L, 1920L, 1930L), law_in_force) |>
  transmute(statefip, year, window_mid = (entryage + exitage) / 2)

scored <- candidates |>
  left_join(students[, c("HIK", "yob")], by = "HIK") |>
  mutate(cand_age = cand_year - yob) |>
  left_join(windows, by = c("cand_statefip" = "statefip", "cand_year" = "year")) |>
  mutate(no_law_at_candidate = is.na(window_mid),
         target_age = dplyr::coalesce(window_mid, 11.5),
         selection_distance = abs(cand_age - target_age))

assigned <- scored |>
  group_by(HIK) |>
  arrange(selection_distance, cand_year, .by_group = TRUE) |>
  slice_head(n = 1L) |>
  ungroup()
```

Ties break to the **earlier** year, which is closer to school entry. The `11.5` fallback for
state-years with no law in force is the median window midpoint over all 1,808 law-in-force
state-years (mean 11.48, median 11.5). Flag those rows via `no_law_at_candidate` and report
their share.

**Keep `selection_distance` on every row.** It is the only measure of how well the assigned
county represents where the student actually went to school, and B5 shows it varies
systematically with cohort. Every result must be shown to survive restricting on it.

`cand_age` is `cand_year - yob`, consistent with the treatment key. Carry
`cand_age_observed` — the `AGE` recorded on that row — as a cross-check. The two will
disagree for some students because enumeration dates differ by census (1900 June, 1910
April, 1920 **January**, 1930 and 1940 April) while `yob` is a single April-1940 convention.

### B4. Build the county key

```r
mutate(county_geoid = cand_statefip * 1000L + cand_countyicp %/% 10L)
```

**`COUNTYICP` is the FIPS county code multiplied by 10** — Alabama's counties are `0010,
0030, 0050`. **And the codes are state-dependent**: ~280 distinct values per census year is
the within-state county sequence, not 280 counties, so `cand_statefip` is not optional.
`COUNTYICP` is 100% populated with no `0` codes in all five years, so the "identifiable
counties only" caveat in the IPUMS label does not bite on full-count files.

### B5. Cohort coverage — and why the rule does less work than it looks

Candidate years are those where `5 <= cand_age <= 18`:

| `yob` | Candidates (year: age) | Assigned from |
|---|---|---|
| 1885–1891 | 1900: 15 → 9 | 1900 only |
| 1892–1895 | 1900: 8 → 5, **and** 1910: 18 → 15 | the rule decides |
| 1896–1901 | 1910: 14 → 9 | 1910 only |
| 1902–1905 | 1910: 8 → 5, **and** 1920: 18 → 15 | the rule decides |
| 1906–1911 | 1920: 14 → 9 | 1920 only |
| 1912 | 1920: 8, **and** 1930: 18 | the rule decides |

**Nineteen of the 28 cohorts have exactly one candidate year**, so for them the selection
rule is inert — it records `selection_distance` and nothing else. For the nine with two
candidates the rule does real work: the age pairs are `(8,18)`, `(7,17)`, `(6,16)`, `(5,15)`,
whose indifference midpoints are 13, 12, 11 and 10, and all four sit inside the observed
range of window midpoints (**9.5 to 13.5**). So the state's own window genuinely flips the
choice for those cohorts.

Note also that state-specific midpoints span only 9.5–13.5 while the competing ages within a
pair differ by 10 years. The rule is therefore close to "nearest age 11.5" in practice. It is
kept in its state-specific form because it costs nothing and is the defensible framing, not
because it buys much.

**The school-age window recovers cohorts the obvious rule loses.** `merge_cls_to_linked_census.md`
B3 assigns childhood county at `AGE <= 9` and therefore loses the 1885–1889 cohorts
completely, because the 1890 census burned. Here they are reachable in 1900 at ages 11–15 —
inside the compulsory window for most states, since `exitage` is 14 or above in 1,762 of
1,808 law state-years. The cost is that they are observed near the *end* of their window
rather than the start, which is exactly what `selection_distance` records.

---

## Part C — required years under the residence state's law

### C1. Join the cohort table on residence state

```r
students <- assigned |>
  inner_join(cohorts, by = c("cand_statefip" = "bpl", "yob")) |>
  rename(resid_statename = statename, resid_region = region,
         resid_region_label = region_label)
```

This looks like a misuse of the CLS file and is not. `cls_cohort_requirements_1875_1912` is
keyed on `bpl` × `yob`, but `bpl` holds a plain state FIPS code, and the algorithm that
produced the table — `cohort_requirements_oct_2016.do`, which walks each state × cohort
through ages 5–17 and reads the law in force in `yob + age` — contains nothing about birth.
Re-keying it to residence state yields required years under the law that actually applied,
with no re-derivation.

**The assumption this carries: the student remained in that state across ages 5–17.** State
it as an assumption. It is the same class of assumption CLS make about state of birth, and a
weaker one, since residence at school age is nearer to the schooling state than birth is.

**Rename `statename`, `region` and `region_label` on arrival.** They are residence-state
fields after this join and will be misread as birth-state fields otherwise. CLS's regions are
their own coding, not the census's: MD and DC sit in the Northeast, KY and WV in the Midwest,
OK in the West.

### C2. Nominal versus effective

Both versions come from the same table:

| Column | Definition | Use here |
|---|---|---|
| `ca_years1` | age limits only — `entryage <= age < exitage` | **`required_nominal`** |
| `ca_years_combined` | `min(ca_years3, ca_years5)`, exemptions and continuation school | **`required_effective`** — headline |
| `ca_years` | `min(ca_years2, ca_years3)`, regular school only | effective, continuation excluded — robustness |
| `ca_years2` / `ca_years3` | work-permit / early-dropout exemption separately | decomposition |

The exemptions are not a rounding detail. In `cls_state_age_limits_1880_1930` an exemption
governs the leaving age in **989 of 1,808** law state-years — `work_permit` 518,
`early_dropout` 393, `both` 78 — and across the panel they give back about half of the
tightening in exit ages. The effective version is the one that answers the question; the
nominal version is there to show how much the exemptions matter.

### C3. Carry the birth-state measure too

```r
left_join(cohorts[, c("bpl", "yob", "ca_years_combined")] |>
            rename(required_effective_bpl = ca_years_combined),
          by = c("BPL" = "bpl", "yob"))
```

The difference between `required_effective` and `required_effective_bpl` is the migration
bias in CLS's own treatment key, measured rather than assumed. Report its distribution — it
is a result in its own right.

**Take `BPL` from the 1940 row.** Across a 1-in-60 sample of `HIK`s, **4.9%** (113,538 of
2,309,382) carry more than one coded birthplace across their linked records.

### C4. The sentinel assumption propagates into every effective measure

Where a law required literacy rather than a year count, CLS recode `99` to **4 years**; `99X`
becomes X. That is an assumption, not a measurement, and it feeds the exemption arithmetic:
`workyrs` is coded `99` in **166** state-years and `earlyyrs` in **29**.

Filter on `workyrs_code_type` and `earlyyrs_code_type` in `cls_state_age_limits_1880_1930`
and report `required_effective` with those state-years dropped. If the measure moves, say so.

---

## Part D — the four county measures

### D1. Definitions

Over students `i` in county `c`, computed twice — once with `required_nominal`, once with
`required_effective`:

| Measure | Definition | Reads as |
|---|---|---|
| `shortfall_rate` | `mean(observed < required)` | **headline** — the non-compliance rate |
| `gap_censored` | `mean(pmin(observed, required) - required)` | mean deficit below the legal minimum |
| `gap_mean` | `mean(observed - required)` | descriptive only — **absorbed by state × cohort FE** |
| `gap_p10`, `gap_p25`, `gap_p50` | percentiles of `observed - required` | counties clearing the law at the median but not the bottom decile |

```r
county <- students |>
  group_by(county_geoid, required_basis) |>
  summarise(n_students     = n(),
            shortfall_rate = mean(observed_years < required_years),
            gap_censored   = mean(pmin(observed_years, required_years) - required_years),
            gap_mean       = mean(observed_years - required_years),
            gap_p10        = quantile(observed_years - required_years, 0.10),
            .groups = "drop")
```

### D2. Diagnostics that belong on every output row

Without these the measure cannot be read, because Part E's problems vary by county:

- `n_students`, and `n_students` broken out by `cand_year` — a county whose students come
  mostly from 1900 is not comparable to one drawn from 1920.
- `mean_selection_distance` and its p90.
- `share_no_law_at_candidate`.
- `mean_cand_age`, since B5 shows it moves with cohort.
- The same four measures on `required_effective_bpl`, for the C3 comparison.

### D3. Minimum cell size

Require **25 students**. Write smaller counties to the output with a
`below_min_cell` flag rather than dropping them — a silently missing county looks like a
county with no bite.

---

## Part E — what this measure cannot do

### E1. Assignment year is collinear with cohort

This is the sharpest limitation, and it follows from B5 rather than from anything fixable.
Nineteen of 28 cohorts have a single candidate census year, so the census a student is
located from is very nearly a deterministic function of their birth cohort: 1885–1895 come
from 1900, 1896–1905 from 1910, 1906–1912 from 1920.

The linkage rates differ by a factor of three across those years — **17.57%**, **32.48%**,
**54.15%**. So cohort, linkage selection and county-boundary vintage move together and
cannot be separated. A cohort gradient in the measure has at least three candidate
explanations, and the design cannot tell them apart. **Do not interpret cohort trends.**
Within-cohort, cross-county comparison is the only comparison this measure supports.

Worse, `cand_age` also trends within an assignment year: the 1900-assigned students are
observed at 15 if born 1885 and at 5 if born 1895. Assignment quality is therefore a
function of cohort too.

### E2. The raw gap is absorbed by the obvious specification

Restated from the top, because it is the error most likely to be made downstream: with state
× cohort fixed effects, `gap_mean` reduces to county mean attainment. Use `shortfall_rate`
or `gap_censored`, or drop the fixed effects and accept a descriptive reading.

### E3. County boundaries are as-of-year

IPUMS imposes no uniform county system: the same `COUNTYICP` is not necessarily the same
area in 1900 and 1920. Since different cohorts are assigned from different censuses (E1), a
single county's measure mixes boundary vintages, and the mixture is itself cohort-dependent.
Report the measure separately by `cand_year` before pooling.

### E4. Required years are not linear

CLS enter required attendance in bins, not linearly; the effect is not linear in required
years. Do not regress on `required_effective` as a continuous covariate without checking the
binned version.

### E5. Linkage selection is unmeasured

No match-rate-by-characteristic table exists for this extract yet.
`merge_cls_to_linked_census.md:241-242` prescribes it — match rate by 1940 `SEX`, `RACE`,
`BPL` and `EDUC` — and it has not been computed. It is a prerequisite for interpreting
anything here, not a robustness appendix.

Related and also undone: comparing the county assigned from 1900 against the one assigned
from 1910 for the same `HIK` would measure how fast childhood residence drifts. Only the nine
two-candidate cohorts can support it.

---

## Implementation

Two scripts, specified here and not yet written:

| Script | Covers |
|---|---|
| `prep/mandatory_schooling/build_county_bite.R` | Parts A–C; writes the student-level parquet and the county aggregate |
| `analysis/mandatory_schooling/describe_county_bite.R` | Part D's tables and maps, Part E's diagnostics |

Both inherit the patterns already established in
`analysis/mandatory_schooling/describe_cls_policy_environment.R`: the `repo_root` /
`data_root` resolution idiom, the named-input manifest with an existence gate and
`tools::md5sum` (lines 78–92), and the `expect()` validation accumulator that writes
`validation_summary.csv` and hard-fails at the end (lines 103–110, 513–515). Reuse its CLS
Table 1 reproducibility guard — `CLS_TABLE1_PCT`, mean age 38.9 — which will catch any drift
in the `yob` or `HIGRADED` conventions before the county dimension is added.

Outputs to `output/mandatory_schooling/county_bite/` and
`results/mandatory_schooling/county_bite/`.

**On reading the extract.** The linked CSV is the only source for Parts A–B, it is 13 GB
gzipped, and it is **not** under `DET_DIR` — it sits at
`~/OneDrive/Documentos/usa_00001.csv.gz`, resolved through `IPUMS_LINKED_CSV`. Stream it once
with `arrow`, selecting only the columns these steps need — `YEAR`, `HIK`, `AGE`, `SEX`,
`RACE`, `BPL`, `HIGRADED`, `STATEFIP`, `COUNTYICP`, `MOMLOC`, `POPLOC`, `GQ` — and write the
student-level intermediate so Part D never re-reads it. Read everything as string and cast
explicitly: the file is ordered by census year, so type inference from the first block makes
every 1940-only column null. See `profile_ipums_linked_census.py` for the working pattern.

---

## Traps, collected

| Trap | Consequence |
|---|---|
| Reading `gap_mean` as identifying variation | It is county mean attainment; state × cohort FE absorb it |
| Interpreting a cohort trend | Cohort, linkage rate and boundary vintage are collinear (E1) |
| Using `SCHOOL` as observed schooling | Reference period runs ~12 months in 1900 to ~1 month in 1940 |
| Expecting attainment before 1940 | `HIGRADE`/`EDUC` start in 1940; `LIT` stops in 1930 |
| Joining the cohort table on `bpl` and calling it the law that applied | That is the birth state; re-key to residence state (C1) |
| Forgetting to rename `statename`/`region` after the C1 join | They become residence fields and will be misread as birth fields |
| Treating `COUNTYICP` as FIPS, or without `STATEFIP` | GEOIDs wrong by a factor of 10; codes are state-dependent |
| Pooling counties across `cand_year` | Mixes county boundary vintages and linkage rates |
| Taking `BPL` from a pre-1940 row | 4.9% of `HIK`s carry more than one coded birthplace |
| Forgetting `HIGRADED == 999` | ~7% of rows carry a not-reported code as data |
| Using `ca_years_combined` linearly | CLS use bins; the effect is not linear in required years |
| Ignoring `*_code_type` | The `99 → 4` literacy stand-in is an assumption in 166 + 29 state-years |
| Dropping small counties silently | A missing county reads as a county with no bite; flag instead (D3) |
| Averaging raw law columns | In `cls_state_age_limits_1880_1930`, `0` means *no law*, not missing |

## Sources

- Clay, K., Lingwall, J. & Stephens, M. Jr (2021). "Laws, educational outcomes, and returns
  to schooling: evidence from the first wave of U.S. state compulsory attendance laws."
  *Labour Economics* **68**, 101935. Replication package ICPSR 145122 v1.1.
- `cohort_requirements_oct_2016.do` in that package — the definition of the `ca_years*`
  age-walk, and the authority for the re-keying argument in C1.
- IPUMS USA Multigenerational Longitudinal Panel, linked extract `usa_00001.csv.gz`, with
  its DDI at `Data/raw/mandatory_schooling/usa_00001.xml`.
- [`ipums_linked_1900_1940_dictionary.md`](ipums_linked_1900_1940_dictionary.md) — every
  fill rate and coding note quoted above.
- [`merge_cls_to_linked_census.md`](merge_cls_to_linked_census.md) — the state-level join,
  and the no-linkage bite route this document deliberately departs from.

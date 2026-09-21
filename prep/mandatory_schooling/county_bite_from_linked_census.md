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

Required years vary on **state × birth cohort**. Observed schooling varies on the individual.
So in `mean(observed - required)`, the county-to-county variation is county mean attainment
and nothing else — the required term enters only through cohort composition. State × cohort
fixed effects absorb the **required term**, not the measure: the county variation lives in
attainment and survives. Within-state comparison of counties is therefore entirely available;
what changes is which measure carries information. Within a state × cohort cell the
requirement is a constant `r`, so:

| Measure | Within-cell form | Within-state ranking |
|---|---|---|
| `gap_mean` | `mean(obs) - r` | identical to ranking by mean attainment |
| `gap_p10/p25/p50` | `quantile(obs) - r` | identical to ranking by that quantile |
| `shortfall_rate` | `F_c(r)` — the county CDF at the threshold | **not recoverable from the mean** |
| `gap_censored` | `-mean(max(r - obs, 0))` | **not recoverable from the mean** |

This sharpens rather than contradicts `merge_cls_to_linked_census.md:250-255`. That warning
is about a county measure **of the law**, which does collapse to the state measure. This is a
county measure of **attainment against** the law, which does not.

Two of the four measures below carry information the county mean does not, because they are
**not linear** in attainment:

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

## Definitions used throughout

**Cohort always means birth cohort**, `yob = 1940 - AGE - 1`, which is the CLS key. The
sample is `yob` **1885–1912, 28 cohorts**, and that bound comes from the `AGE 27–54`
restriction in 1940 rather than from reachability. The **assignment stratum** — which census
a student was located in — is a different thing that happens to be nearly determined by birth
cohort, which is why E1 below matters. `cohort_decade = (yob %/% 10L) * 10L` is a birth
decade, used only to join county panels indexed the same way. There is no enrolment or grade
cohort anywhere in this data: 1940 records the highest grade completed as a single number and
never grade progression.

**Required years is not one number.** Six bases, each a threshold the same four measures are
computed against:

| | Actual — `0` where no law bound | Counterfactual — state plateau imputed for unexposed cohorts |
|---|---|---|
| **Nominal**, `ca_years1`, age limits only | `required_nominal` | `required_nominal_cf` |
| **Effective**, `ca_years_combined`, net of exemptions | `required_effective` | **`required_effective_cf`** |

plus `required_fixed6` and `required_fixed8`, constants, because `shortfall_rate` is `F_c(r)`
and county distributions cross — a ranking must be shown to survive a threshold that owes
nothing to the state's own choice. `required_effective` says what actually bound a student;
`required_effective_cf` is the basis for anything comparing across cohorts. Part C5 explains
the counterfactual.

Carried for comparison, never as bases: `required_effective_regular` (`ca_years`,
continuation school excluded) and `required_effective_bpl` (`ca_years_combined` on `BPL`,
CLS's birth-state key — its difference from `required_effective` is the migration bias in
their treatment assignment).

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

**The realized strata are narrower than the candidate ranges above**, which overlap precisely
because they are candidates. Running the rule with the 11.5 fallback target puts each cohort
in exactly one stratum:

| Stratum | Cohorts | n | Observed at ages |
|---|---|---|---|
| 1900-assigned | 1885–1893 | 9 | 15 down to 7 |
| 1910-assigned | 1894–1903 | 10 | 16 down to 7 |
| 1920-assigned | 1904–1912 | 9 | 16 down to 8 |

**The strata are an output of the rule, not an input.** For the boundary cohorts — 1893/1894
and 1903/1904 — the choice turns on the state's own window, so a cohort can sit in different
strata in different states. Any table keyed on stratum must therefore be read as a property
of the assignment, not of the cohort.

`selection_distance` runs 0.5 to 4.5 *within* each stratum, a sawtooth peaking at the cohort
observed nearest 11.5. So assignment quality is not collinear with stratum and can be used as
a within-stratum control or restriction.

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

### C5. The counterfactual bases, and the cohorts no law bound

`ca_years_combined == 0` means **no law bound that cohort** - 231 of the 1,372 sample cells,
16.8%. There the measures collapse by construction rather than by finding: `shortfall_rate`
is `P(observed < 0)` = 0 identically, `gap_censored` is 0 identically, and `gap_mean` is
plain attainment with the law contributing nothing. Two consequences.

**Partition, do not pool.** Carry `law_bound_cohort = required_nominal > 0L` and report the
actual bases only on law-bound students. Pooling unexposed students into a county cell drags
its `shortfall_rate` toward zero in proportion to how many were unexposed - a composition
artefact indistinguishable from low bite.

**Impute the future standard for the unexposed**, which is what
`merge_cls_to_linked_census.md:255-259` already committed to: how far the county's
counterfactual education sat below what the law would later require, *measured
pre-treatment*. Pre-treatment is the whole point. Bite measured *during* a law regime is
contaminated - if the law worked, attainment rose and measured bite fell, making it an
outcome rather than a covariate. Against a future standard on pre-law cohorts it is a usable
baseline.

```r
plateau <- cohorts |>
  group_by(bpl) |>
  summarise(plateau_nominal   = max(ca_years1),
            plateau_effective = max(ca_years_combined))
```

The plateau is the requirement once the law is fully phased in, equivalently the first
fully-exposed cohort's value. **Build it on each basis separately**, because the two differ:
across the 17 imputation states the nominal plateau spans **6-10 years** and the effective
**6-8**, and they diverge in **10 of the 17** by up to 2 years - Delaware, Iowa, Maryland and
Oklahoma run nominal 10 against effective 8; Alabama 8 against 6. The other seven have
exemptions that do not bind at the plateau. Keep both counterfactual columns beside the
actual ones, never overwriting them, under one shared `required_is_imputed` flag - the same
231 cells are imputed on either basis.

**Imputation is concentrated in the South.** 201 of the 231 imputed cells (**87%**) fall in
the 11 CLS-coded South states: Virginia 22 unexposed cohorts, Mississippi and South Carolina
21, Georgia and Florida 19, Texas 18, Alabama/Arkansas/Louisiana/North Carolina 17, Tennessee
13, against a thin non-South remainder (Arizona 1, Maryland 2, Iowa 3, Missouri 7, Oklahoma
8, Delaware 9). Report every table with and without imputed cells. **Arizona repealed** (no
law 1883-1898), so "the first law" is ill-defined there; `max()` handles it mechanically but
the state should be flagged.

There is also **no never-treated state** in this window: zero of 49 are unexposed across all
28 cohorts and 32 are exposed throughout. Any exposed-versus-unexposed contrast is therefore
built inside those 17 states, which is the region collinearity `CLAUDE.md` already documents
for this pipeline.

---

## Part D — the four county measures

### D1. Definitions

Over students `i` in county `c`, computed once per basis - all six from *Definitions*
above. The actual bases run on `law_bound_cohort` students only; the counterfactual and fixed
bases run on everyone (C5):

| Measure | Definition | Reads as |
|---|---|---|
| `shortfall_rate` | `mean(observed < required)` | **headline** — the non-compliance rate |
| `gap_censored` | `mean(pmin(observed, required) - required)` | mean deficit below the legal minimum |
| `gap_mean` | `mean(observed - required)` | descriptive — within a cell this ranks counties exactly as attainment does |
| `gap_p10`, `gap_p25`, `gap_p50` | percentiles of `observed - required` | counties clearing the law at the median but not the bottom decile |

```r
county <- students |>
  group_by(county_geoid, assignment_stratum, required_basis) |>
  summarise(n_students     = n(),
            shortfall_rate = mean(observed_years < required_years),
            shortfall_se   = sqrt(shortfall_rate * (1 - shortfall_rate) / n_students),
            gap_censored   = mean(pmin(observed_years, required_years) - required_years),
            gap_mean       = mean(observed_years - required_years),
            gap_p10        = quantile(observed_years - required_years, 0.10),
            .groups = "drop")
```

**The cohort must stay in the grouping.** Collapsing it makes the fixed-effects panel and any
per-state cohort plot impossible. Four grains are written:

| Table | Grain | Purpose |
|---|---|---|
| `bite_students.parquet` | one row per `HIK` | everything downstream |
| `bite_county_stratum` | county x stratum x basis | **primary**, ~9,000 cells - what the 25-student minimum is sized for |
| `bite_county_cohort` | county x `yob` x basis | thin, ~84,000 cells; fixed effects and thinness diagnostics |
| `bite_state_cohort` | state x `yob` x basis | mean required against mean realized, by state and cohort |

### D2. Diagnostics that belong on every output row

Without these the measure cannot be read, because Part E's problems vary by county:

- `n_students`, and `n_students` broken out by `cand_year` — a county whose students come
  mostly from 1900 is not comparable to one drawn from 1920.
- `mean_selection_distance` and its p90.
- `share_no_law_at_candidate`.
- `mean_cand_age`, since B5 shows it moves with cohort.
- `share_imputed`, so a cell resting mostly on the counterfactual standard is visible.
- `shortfall_se`, the binomial standard error, so a ranking is read against its own sampling
  error rather than as a clean order.
- `coverage` - see D4.
- The same four measures on `required_effective_bpl`, for the C3 comparison.

### D4. Per-county linkage coverage

The main threat to a within-state ranking is that **linkage is not random across counties**:
match rates depend on urbanisation, literacy and name commonality. No match-rate table exists
for this extract - `merge_cls_to_linked_census.md:241-242` prescribes one and it has never
been built. Cheap guard: divide each county's linked student count by an independent count of
its children.

```r
coverage <- county_decade |>
  left_join(births, by = c("county_geoid" = "GEOID", "cohort_decade" = "decade")) |>
  mutate(coverage = n_students / county_births_estimate_decade)
```

`county_births_estimate_decade` is already in `output/us_panel_county_stem_1800.csv` for
decades 1880-1910, 3,143 counties each. Two traps in that file: the column carries **294
literal `NA` strings**, so a reader types it as text and a plain numeric cast throws; and
`GEOID` is typed inconsistently depending on the data — text on the real file, which holds
leading zeros, but a number wherever every value looks numeric. **Join on integers on both
sides**, not on a zero-padded string, or the join silently matches nothing in one of the two
cases. Report per state the within-state dispersion of `coverage` and its rank correlation
with `shortfall_rate`. **If those correlate, the ranking is measuring linkage, not bite.**

### D3. Minimum cell size

Require **25 students**. Write smaller counties to the output with a
`below_min_cell` flag rather than dropping them — a silently missing county looks like a
county with no bite.

---

## Part E — what this measure cannot do

### E1. Assignment year is collinear with cohort

This is the sharpest limitation, and it follows from B5 rather than from anything fixable.
Nineteen of 28 cohorts have a single candidate census year, so the census a student is
located from is very nearly a deterministic function of their birth cohort. The realized
strata are 1885–1893 from 1900, 1894–1903 from 1910, 1904–1912 from 1920.

The linkage rates differ by a factor of three across those years — **17.57%**, **32.48%**,
**54.15%**. So cohort, linkage selection and county-boundary vintage move together and
cannot be separated. A cohort gradient in the measure has at least three candidate
explanations, and the design cannot tell them apart. **Do not interpret cohort trends.**

What this bars is cohort *trends*, not within-state work. Stratifying on assignment year
holds the linkage regime and the boundary vintage fixed, and within a stratum the
cross-county comparison is clean on both. The cost is that the requirement is then close to a
state constant: inside the realized strata only 25, 28 and 22 of 49 states vary in
`ca_years_combined` at all, with a median within-state spread of 1, 1 and 0 years. So the
variation being used is cross-county attainment against a common legal minimum - which is
what `shortfall_rate` measures and is a real quantity, but is **not** variation in the law
and must not be described as such.

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

Three scripts. The first two are built; the third is not.

| Script | Covers |
|---|---|
| `prep/mandatory_schooling/extract_linked_bite_inputs.py` | One streaming pass over the extract, writing `students_1940.parquet` and `candidates_pre1940.parquet` |
| `prep/mandatory_schooling/build_county_bite.R` | Parts A–D: the selection rule, the six bases, the four grains |
| `analysis/mandatory_schooling/describe_county_bite.R` | *Not yet written* — the within-state tables and maps, and Part E's diagnostics |

Tested by `test_county_bite.py` (extractor, 25 checks) and `test_county_bite.R` (builder, 42
checks against a synthetic fixture with hand-computed answers). Both run without the 12 GB
source. `COUNTY_BITE_IN_DIR` and `COUNTY_BITE_OUT_DIR` let the R test drive the real build
script over that fixture, including its own law panel, so the year-selection rule can be
checked on a boundary cohort that flips on the state's window.

Both inherit the patterns already established in
`analysis/mandatory_schooling/describe_cls_policy_environment.R`: the `repo_root` /
`data_root` resolution idiom, the named-input manifest with an existence gate and
`tools::md5sum` (lines 78–92), and the `expect()` validation accumulator that writes
`validation_summary.csv` and hard-fails at the end (lines 103–110, 513–515). Reuse its CLS
Table 1 reproducibility guard — `CLS_TABLE1_PCT`, mean age 38.9 — which will catch any drift
in the `yob` or `HIGRADED` conventions before the county dimension is added.

Outputs to `output/mandatory_schooling/county_bite/` and
`results/mandatory_schooling/county_bite/`.

**On reading the extract.** The linked CSV is the only source for Parts A–B, it is 12.2 GB
gzipped, and it is **not** under `DET_DIR` — it sits at
`~/OneDrive/Documentos/usa_00001.csv.gz`, resolved through `IPUMS_LINKED_CSV`. It is streamed
once, in Python, selecting only `YEAR`, `HIK`, `AGE`, `SEX`, `RACE`, `BPL`, `HIGRADED`,
`EDUC`, `STATEFIP`, `COUNTYICP`, `MOMLOC`, `POPLOC`, `GQ`, and the two intermediates mean the
R build never touches it again.

Two properties of the file drive that design. Everything is read as string and cast
explicitly, because the rows are ordered by census year and type inference from the first
block makes every 1940-only column null, failing 200 M rows later on its first real value.
And **the 1940 rows arrive last**, so a single pass cannot restrict the candidate rows to
HIKs that survive the student filter — only the cheap age filters are applied there and the
join drops the rest. `profile_ipums_linked_census.py` is the working precedent for both.

---

## What the build produced

First full run, 2026-09-21. The extraction reads all 220,979,023 rows in 9.6 minutes; the
build takes about two.

| | |
|---|---|
| 1940 rows in `AGE` 27-54 | 33,046,638 |
| with attainment (`HIGRADED != 999`) | 32,367,477 |
| **assigned a school-age county** | **15,472,169** (47.8%) |
| county x stratum x basis rows | 68,854 |
| state x cohort x basis rows | 7,774 |

Mean `shortfall_rate` across county-stratum cells of at least 25 students, which is the share
of a county's students finishing below the threshold:

| Basis | Mean | p10 | p90 | `gap_mean` | `gap_censored` |
|---|---|---|---|---|---|
| `fixed6` | 0.153 | 0.019 | 0.359 | 2.78 | -0.39 |
| `effective` (law-bound only) | 0.181 | 0.036 | 0.400 | 2.41 | -0.44 |
| `effective_cf` | **0.222** | 0.056 | 0.438 | 1.90 | -0.59 |
| `nominal` (law-bound only) | 0.273 | 0.047 | 0.579 | 1.86 | -0.65 |
| `fixed8` | 0.305 | 0.079 | 0.598 | 0.78 | -0.91 |
| `nominal_cf` | 0.321 | 0.084 | 0.623 | 1.33 | -0.83 |

Monotone in the threshold, as it has to be: `fixed6` < `effective_cf` < `fixed8`, and the
nominal bases sit above their effective counterparts because exemptions lower the bar. On the
headline basis the law was **slack on average** — students finished about 1.9 years beyond
what it required — and `shortfall_rate` exceeds 0.5 in only 5.8% of cells.

**Three diagnostics on the within-state ranking**, over the 135 state-strata with at least 10
qualifying counties, excluding the 1930 stratum:

- **The ranking is not an artefact of linkage.** Median Spearman correlation between
  `coverage` and `shortfall_rate` is **0.121** (p10 -0.43, p90 0.50) — no systematic
  relationship. Coverage itself does vary within state (median CV 0.38), so carry it as a
  control, but it is not producing the ranking.
- **`shortfall_rate` and `gap_mean` order counties differently.** Median Spearman **-0.874**,
  and |rho| < 0.9 in **78 of the 135** state-strata. So the threshold does real work in most
  states, which is the case for preferring `shortfall_rate` over the raw gap.
- **The ranking is well resolved.** Within-state IQR of `shortfall_rate` is 0.076 against a
  median within-cell standard error of 0.011 — about 6.6 to 1.

**A fourth stratum exists and should usually be dropped.** 189,062 students (1.22%) are
assigned from 1930. They are **entirely cohort 1912 observed at age 18**, which is at or past
every statutory exit age, and every one has `selection_distance >= 5` — the worst-assigned
group in the file, and a single cohort, so maximally confounded. `selection_distance >= 5`
isolates it.

Two data facts the build surfaced that were not known before:

- **`COUNTYICP` is not always the FIPS code times ten.** 4,944 of 15,472,169 rows (0.032%)
  end in 5 — Oregon 605, Virginia 1875, Tennessee 875, South Dakota 1055. Flooring them would
  silently merge them into a neighbouring county, so they carry a NULL `county_geoid` and a
  `countyicp_standard` flag, and drop out of the county grains while still counting in the
  state ones.
- **The linked records' ages disagree, and +1 is the consistent case.** Comparing
  `cand_age = YEAR - yob` against the age recorded at that census, the modal difference is
  **+1 (57.0%)**, then +2 (17.3%), then 0 (15.0%). That is arithmetic, not error:
  `yob = 1940 - AGE - 1` sits a year below the earlier censuses' own arithmetic. The tails are
  genuine misreporting, and 3.6% of candidate rows fall outside the school-age window on the
  cohort-consistent basis and are dropped. `age_consistency` carries the difference per row.

---

## Traps, collected

| Trap | Consequence |
|---|---|
| Reading `gap_mean` as identifying variation | Within a cell it ranks counties exactly as attainment does; use `shortfall_rate` |
| Interpreting a cohort trend | Cohort, linkage rate and boundary vintage are collinear (E1) |
| Reading a within-stratum result as variation in the law | Inside a stratum the requirement is near a state constant; only 25/28/22 of 49 states vary (E1) |
| Pooling unexposed students into a bite cell | `shortfall_rate` is 0 by construction where no law bound; it drags the cell toward zero (C5) |
| Overwriting `required_effective` with the counterfactual | The actual and imputed standards answer different questions; keep both columns (C5) |
| Imputing the plateau on one basis only | Nominal and effective plateaus differ in 10 of the 17 states, by up to 2 years (C5) |
| Joining `county_geoid` to `GEOID` without a zero-padded cast | One is an integer, the other a 5-character string; the join silently returns nothing (D4) |
| Reading a county ranking without `coverage` | Differential linkage across counties can produce the ranking on its own (D4) |
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

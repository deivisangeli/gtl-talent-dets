# Merging the CLS schooling-law data onto the linked census

How to join `cls_cohort_requirements_1875_1912` (Clay, Lingwall & Stephens 2021, converted
by `prep/mandatory_schooling/build_cls_schooling_laws.R`) onto the IPUMS MLP linked extract
`usa_00001.csv.gz`.

The variable-by-variable reference is
[`ipums_linked_1900_1940_dictionary.md`](ipums_linked_1900_1940_dictionary.md).

---

## Start here: CLS used no record linkage

This is the single most important fact for getting the merge right, and it is easy to miss
now that a linked file is sitting on disk.

Clay, Lingwall and Stephens assign treatment on **state of birth × year of birth**. Both are
recorded in the 1940 census itself, so their entire design runs on a single cross-section.
From the paper's first stage:

$$\text{Educ}_{isc} = \pi_0 + \pi_1 \text{LawYears}_{sc} + \theta_s + \tau_{rc} + u_{isc}$$

where `LawYears_sc` is required attendance for someone **born in state *s* in cohort *c***,
`θ_s` is state-of-birth fixed effects and `τ_rc` is region × birth-cohort fixed effects.
That is how they get a ~19 million sample with no crosswalk, no match-rate selection, and
nothing to defend about who links to whom.

So this document has two halves that must not be confused:

- **Part A** reproduces the CLS join exactly. It uses only the `YEAR == 1940` rows and
  ignores the linkage entirely.
- **Part B** is the extension the linked file makes possible — childhood county — which CLS
  never did, and which carries costs their design does not.

Do Part A first. If a result appears only once Part B's linkage is introduced, the linkage
is a candidate explanation for it.

---

## Part A — the CLS join, reproduced

### A1. Restrict to 1940

```r
census_1940 <- linked |> dplyr::filter(YEAR == 1940L)
```

The extract stacks all five census years, one row per person per census. Every other year is
irrelevant to Part A.

### A2. Build the cohort key — **not** `BIRTHYR`

```r
mutate(yob = 1940L - AGE - 1L)
```

The 1940 census was enumerated on **1 April**, so roughly three-quarters of each age bin had
not yet had their birthday and were born the *previous* calendar year. CLS say so directly
(footnote 15): *"Year of birth is calculated as 1940-age-1, because the census was taken in
April and most individuals had not yet had their birthday. The 1940 Census does not include
quarter of birth information that can be used to more accurately compute year of birth in
other Censuses."*

Three traps here:

- **Do not use `BIRTHYR`.** Measured over all 73,832,840 of the 1940 rows in this extract,
  `BIRTHYR` equals `1940 - AGE` for **100.0%** of them — i.e. exactly the convention CLS
  reject. Joining on it does not misfile a few people; it shifts **every** person's cohort
  by one year relative to the key the law measure was built on. (`BIRTHYR` is self-reported
  only in 1900, the one census that asked date of birth; elsewhere it is derived.)
- Both conventions are defensible about the *truth* — `1940 - AGE` is right for the ~25%
  born January to March, `1940 - AGE - 1` for the ~75% born April to December. CLS take the
  modal case. What matters for the join is matching their key, not picking the better one.
- **Do not use `birthyr_approx`** from `D:\ipums_1940_fullcount` either. It is the same
  `1940 - AGE` quantity as `BIRTHYR`, so it carries the same uniform one-year shift.
- The cohort file covers **`yob` 1875–1912** and the estimation sample **1885–1912**. Rows
  outside that simply will not match.

### A3. Apply the sample restrictions

```r
filter(SEX == 1L,            # men
       RACE == 1L,           # white
       BPL <= 56L,           # native-born (US state codes only)
       AGE >= 27L, AGE <= 54L,
       HIGRADED != 999L)     # attainment reported
```

CLS restrict to **native-born white men aged 27–54**. This is deliberate and they explain
why (footnote 2): Jim Crow, differential school quality for Black students, Rosenwald
schools, and gendered norms about work outside the home would all confound the comparison,
so the sample is narrowed rather than the estimate adjusted. Treat any widening as a
different exercise, not a robustness check.

### A4. Construct years of schooling

**There is no years-of-schooling variable in IPUMS.** The 1940 census recorded highest grade
completed. CLS build it:

```r
mutate(g       = HIGRADED %/% 10L,
       yrs_sch = dplyr::case_when(g <= 3L  ~ 0L,
                                  g <= 20L ~ g - 3L,
                                  g <= 23L ~ 17L))
```

`HIGRADED == 999` is not-reported and is dropped at A3, about 7% of rows. Do not substitute
`EDUC`/`EDUCD`: those are IPUMS's own attainment recode with different bin edges, and CLS go
to `HIGRADED` precisely to get a clean year count.

### A5. Join

```r
laws <- arrow::read_parquet(file.path(DATA_OUTPUT, "mandatory_schooling/cls",
                                      "cls_cohort_requirements_1875_1912.parquet"))
merged <- census_1940 |> dplyr::inner_join(laws, by = c("BPL" = "bpl", "yob"))
```

`bpl` in the cohort file is the state FIPS code, which is what `BPL` carries for US-born
people, so the join is direct. The file also brings `region`, needed for the fixed effects.

**Take `BPL` from the 1940 row.** Across a 1-in-60 sample of `HIK`s, **4.9%** (113,538 of
2,309,382) carry more than one coded birthplace across their linked records. Birthplace is
time-invariant in principle but not in the coding, so the join needs a stated rule, and the
1940 record is the one the CLS design reads.

Expected: 49 states × 28 cohorts = **1,372 cells** for `yob` 1885–1912.

### A6. Specify as CLS do

Treatment enters as **bins, never linearly** — the effect of one more required year is not
assumed constant:

```r
cacont1_cacont5 = ca_years_combined >= 1 & ca_years_combined <= 5
cacont6         = ca_years_combined == 6
cacont7         = ca_years_combined == 7
cacont8         = ca_years_combined == 8
cacont9plus     = ca_years_combined >= 9
```

with `ca_years_combined == 0` (no law) as the omitted category. Then:

```stata
reg      yrs_sch  $bins i.bpl i.region#i.yob, vce(cluster yob_bpl)     // first stage
ivregress 2sls lnwkwage (yrs_sch = $bins) i.bpl i.region#i.yob, vce(cluster yob_bpl)
```

- Fixed effects: **state of birth** and **region × year of birth**. The region-by-cohort
  terms are what handle the fact that laws were passed as educational attainment was already
  rising; states are compared against others in their own region.
- Cluster on `yob_bpl = yob * 100 + bpl` — birth-year × birth-state, the level at which
  treatment actually varies.
- `region` is the CLS coding, where region 3 ("South") is **11 states** and places MD, DC,
  KY, WV and OK elsewhere. It is narrower than a census South.

Which `ca_years*` to use:

| Variable | Counts |
|---|---|
| `ca_years1` | age limits only, no exemptions |
| `ca_years2` | minus years lost to the work-permit exemption |
| `ca_years3` | minus years lost to the early-dropout exemption |
| `ca_years4` | additional continuation-school years |
| `ca_years` | `min(ca_years2, ca_years3)` — binding requirement, regular school |
| **`ca_years_combined`** | `min(ca_years3, ca_years2 + ca_years4)` — **the headline** |

The exemptions are **not controls**. They are stopping rules already built into these
counts; `workage`, `workyrs`, `earlyyrs` and `earlyyrs_condition` never appear on the
right-hand side of any CLS regression.

### A7. Check the join before trusting it

Person-weighting `ca_years_combined` over this sample should reproduce the published Table 1
distribution: **13.5 / 9.2 / 19.1 / 16.9 / 33.4 / 7.9** across the bins `0 / 1-5 / 6 / 7 / 8
/ 9+`, with mean age 38.9 and mean years of schooling 9.2.

On the **full count** this reproduces to within 0.1 pp on every bin — that check is already
automated in `analysis/mandatory_schooling/describe_cls_policy_environment.R`, Panel D.

On **this linked extract it will not match**, and that is expected, not a bug: the 1940 rows
here are 73.8 M against a full count of 132 M, because only linked cases are included. The
gap between the two distributions *is* the linkage selection, and is worth reporting rather
than explaining away.

---

## Part B — childhood county, which CLS did not do

### B1. Why the linkage is needed at all

No US census ever recorded **county of birth**. Birthplace was asked at state/country level
only — `BPL`/`BPLD` — in every year from 1850 to 1950. `COUNTYICP` is county of *residence*;
`MIGCOUNTYICP5` is county of residence in *1935*. Neither is a birth county.

The only route is to observe the person as a young child in an earlier census and read the
county they lived in then.

### B2. The join

`HIK` is the cross-year person key. `HISTID` is **not** — it identifies a record within a
census year and is not shared across years by the same person. Verified on this extract:
`HISTID` is unique within a census year (3,000,000 distinct over the first 3,000,000 rows),
`HIK` appears at most once per person per year, and every sampled `HIK` spans more than one
census year.

```r
childhood <- linked |>
  filter(YEAR < 1940L, AGE <= 9L, MOMLOC > 0L | POPLOC > 0L) |>
  group_by(HIK) |> slice_min(YEAR, with_ties = FALSE) |> ungroup() |>
  transmute(HIK,
            childhood_year   = YEAR,
            childhood_age    = AGE,
            childhood_geoid  = sprintf("%02d%03d", STATEFIP, COUNTYICP %/% 10L))

merged <- merged |> left_join(childhood, by = "HIK")
```

`AGE <= 9` keeps the residence close to birth; `MOMLOC`/`POPLOC` confirm the child is with a
parent rather than institutionalised.

**`COUNTYICP` is the FIPS county code multiplied by 10** — Alabama's counties are
`0010, 0030, 0050`, not `001, 003, 005`. Dividing by 10 before building a GEOID is not
optional. `MIGCOUNTYICP5` uses the same coding.

### B3. Cohort coverage

| Birth years | Caught at age ≤ 9 in | Age in 1940 |
|---|---|---|
| **1885–1889** | **nothing — the 1890 census burned** | 50–54 |
| 1890–1899 | 1900 | 41–50 |
| 1900–1909 | 1910 | 31–40 |
| 1910–1912 | 1920 | 28–30 |

Twenty-three of the 28 CLS cohorts are reachable. The 1885–1889 cohorts are not, at any age
under 10, because the 1890 microdata was destroyed by fire — no extract can fix this.

### B4. What Part B costs

**Linkage selection.** Match rates correlate with name commonness, literacy and migration.
CLS carry none of this. Report match rate by 1940 `SEX`, `RACE`, `BPL` and `EDUC` before
interpreting anything.

**Proxy decay, which is measurable here.** Someone aged 0–4 in 1900 is 10–14 in 1910, and
this extract has both. Comparing the county assigned from 1900 against the one assigned from
1910 for the same `HIK` measures directly how fast childhood residence drifts from birth
county. Do this — it is the number a referee will ask for.

**A state-level law gives almost no within-county variation.** `ca_years_combined` varies on
(state of birth, cohort). If childhood county sits in the birth state — true for nearly
everyone — a county-level exposure measure collapses to the state measure, varying only
through cohort composition and cross-state child migration, and any specification with
state × cohort fixed effects absorbs it entirely.

The county dimension is therefore useful through **bite**, not through the law: how far the
county's counterfactual education sat below what the law would later require. That varies
within state, is measured pre-treatment, and can be built from
`county_nhgis_demographics_panel.csv` (county literacy 1840–1930), already in the project,
with no linkage at all.

---

## Traps, collected

| Trap | Consequence |
|---|---|
| Joining on `BIRTHYR` or `birthyr_approx` | Cohort key off by one for ~75% of people |
| Using `EDUC` instead of `HIGRADED` | Different bin edges; not a year count |
| Forgetting `HIGRADED == 999` | ~7% of rows carry a not-reported code as data |
| Treating `COUNTYICP` as FIPS | Every GEOID wrong by a factor of 10 |
| Joining on `HISTID` across years | No matches; `HISTID` is within-year |
| One row per person-year | Person-level joins silently fan out; take `BPL` from the 1940 row |
| Comparing `SCHOOL` across years | Reference period runs ~12 months in 1900 to ~1 month in 1940 |
| Expecting attainment before 1940 | `HIGRADE`/`EDUC` start in 1940; `LIT` stops in 1930 |
| Entering `ca_years_combined` linearly | CLS use bins; the effect is not linear in required years |
| Averaging raw law columns | In the source panel `0` means *no law*, not missing |

## Sources

- Clay, K., Lingwall, J. & Stephens, M. Jr (2021). "Laws, educational outcomes, and returns
  to schooling: evidence from the first wave of U.S. state compulsory attendance laws."
  *Labour Economics* **68**, 101935. Replication package ICPSR 145122 v1.1.
- Lingwall, J. (2014). Dissertation Table 6.2 — statewide compulsory attendance law dates.
- IPUMS USA Multigenerational Longitudinal Panel; IPUMS 1940 complete count.

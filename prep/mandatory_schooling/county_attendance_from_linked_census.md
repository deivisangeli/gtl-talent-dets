# A county-level attendance measure for the compulsory-attendance laws

How to build, for each county and census year, the share of children the law required to be
in school who were not. Inputs are the IPUMS MLP linked extract and the CLS law panel. The
variable reference is [`ipums_linked_1900_1940_dictionary.md`](ipums_linked_1900_1940_dictionary.md);
the attainment-based sibling is
[`county_bite_from_linked_census.md`](county_bite_from_linked_census.md), which this document
supersedes as the primary measure for the reason in the next section.

---

## Start here: the law names attendance, not attainment

A compulsory-**attendance** law obliges a child to be in school between two ages. It says
nothing about years completed. So an attainment-based measure is a downstream proxy, and it
is blind in exactly the place the law operates: **if enforcement pulls marginal children into
school who still leave at the exit age, completed attainment barely moves.** An attainment
shortfall reads that as no bite. Worse, if the children drawn in are the ones who would have
finished fewest years, the composition can make measured attainment *fall*, so a working law
looks like a failing one.

`SCHOOL` records attendance **contemporaneously**, on the childhood census row, for
**42,407,004 of 42,408,792** school-age observations — 100.00% coverage, with 1,788 rows
carrying the unknown code. Three structural gains follow:

1. **No 1940 linkage is needed for the outcome.** The sample is every school-age childhood
   row — **42.4 M** — rather than the 15.5 M that also needed a usable 1940 attainment record
   and an age-consistent match. The extract is still linked-cases-only, so selection does not
   vanish; it stops compounding.
2. **No `yob` convention.** The cohort is census year minus age. Nothing joins the CLS cohort
   file; only the state × year law panel is used, so the `1940 - AGE - 1` trap does not arise.
3. **1930 becomes usable.** Its children are born 1912–1925, outside the CLS cohort window,
   which is irrelevant when the outcome is contemporaneous.

Two costs, both handled below: `SCHOOL`'s reference period changes across censuses, and
attendance varies enormously with age.

---

## Definitions used throughout

The grain is **county × census year**, for 1900, 1910, 1920 and 1930. Geography comes from
the childhood row: `county_geoid = STATEFIP * 1000 + COUNTYICP / 10`, with a NULL geoid and a
`countyicp_standard` flag for the 0.027% of rows whose `COUNTYICP` does not divide by ten.

`SCHOOL` is `1` = did not attend, `2` = attended, with `0`/`8`/`9` uninformative.
`attendance_known` gates every rate on `SCHOOL IN (1, 2)`.

| Measure | Universe | Reads as |
|---|---|---|
| `not_attending_required` | `entryage <= age < exitage`, law in force | **headline** — non-compliance with the statute as written |
| `not_attending_effective` | `entryage <= age < effective_leaving_age` | net of the work-permit and early-dropout exemptions |
| `not_attending_std_1013` | ages 10–13, standardised | **comparable across states**, see B2 |
| `not_attending_above` | `exitage <= age < exitage + 2` | voluntary attendance: a within-county benchmark |
| `exit_discontinuity` | the jump at the exit age | see B3; only interpretable against a no-law benchmark |

---

## Part A — the sample

### A1. Children, not students

One row per childhood census observation, at ages 5–18, from
`candidates_pre1940.parquet`. Nothing is joined from 1940. The row carries its own outcome.

```r
mutate(attended = SCHOOL == 2L,
       attendance_known = SCHOOL %in% c(1L, 2L),
       birth_year_approx = census_year - age)
```

`birth_year_approx` is the census year minus the recorded age and is **not** the CLS cohort
key. Nothing here needs it to be; it is carried for description only.

### A2. What selection remains

The extract holds only MLP-linked cases, so these are not all children — the 1900 rows are
13.0 M against a full count near 76 M. That selection is unchanged from the attainment
measure. What is removed is the *compounding*: no 1940 attainment record is required, no age
consistency between two censuses, and no 47.8% assignment loss.

**`coverage` is the guard.** Linked children over the county's population that decade, from
`county_population.csv`. School-age children cannot plausibly exceed 30% of a population, so
`coverage_implausible` flags anything above that. On the real build it fires on **15 of
11,927** cells, against 76 with no denominator at all and 99 below the 25-child minimum,
leaving **11,746 usable**.

---

## Part B — the two things attendance costs

### B1. The reference period changes, so census years never pool

`SCHOOL` asks whether the child attended within a window that is **not the same window** in
each census: roughly 12 months in 1900, 7.5 in 1910, 4 in 1920 — enumerated 1 January, so
mid-winter — and 6 in 1930. Every output is therefore keyed on census year and **nothing is
pooled or differenced across them.**

The built numbers show why this is not a pedantic caveat. Child-weighted non-attendance in
the required window runs **0.211 (1900), 0.063 (1910), 0.095 (1920), 0.065 (1930)**. Read
naively that is a collapse, a relapse and a recovery. The 1900→1910 fall is far too large for
the reference period to explain and is mostly real growth in attendance; the 1920 uptick is
what the four-month midwinter window predicts. Neither reading can be separated from the
other with this variable, which is precisely why the comparison is not offered.

### B2. Attendance swings with age, so county rates must be standardised

The age profile in 1900: **12.3% attending at age 5, 69.0% at 8, 85–87% at 10–12, 74.5% at
14, 49.7% at 16, 24.2% at 18.** A county whose compulsory window skews old therefore looks
non-compliant when it is not, and states with different exit ages are not comparable at all.

`not_attending_std_1013` is a direct standardisation: the unweighted mean of the age-specific
rates over ages **10–13**, which sit inside the compulsory window for 1,762 of the 1,808 law
state-years, since entry never exceeds 10 and exit is 14 or above in all but 46. It is NULL
where any age in the band carries fewer than five children.

**The standardisation is not cosmetic.** New York's Seneca County in 1900 has a raw rate of
0.122 against a standardised 0.017 — a factor of seven — because its required window reaches
into ages where attendance is voluntary in practice. Georgia's Heard County runs the other
way, 0.567 raw against 0.629 standardised. Rank on the standardised rate.

### B3. The exit-age discontinuity, and why it needs a benchmark

Within a county and census year, children just below the exit age are legally required to
attend and those just above are not — same county, same reference period, two years apart.
That is the sharpest contrast this data offers.

```r
not_attending_below_exit = mean(!attended[age %in% (exitage - 2):(exitage - 1)])
not_attending_above_exit = mean(!attended[age %in% exitage:(exitage + 1)])
exit_discontinuity       = above - below
```

**The raw jump is mostly the age gradient, not the law.** Attendance falls with age whether
or not a statute says anything, so the jump has to be read against state-years with **no law
in force**, where the same calculation is done at the state's counterfactual exit age. From
`attendance_state_year.csv`:

| Census year | Law in force | States | Below exit | Above exit | Jump |
|---|---|---|---|---|---|
| 1900 | no | 16 | 0.352 | 0.529 | **0.177** |
| 1900 | yes | 33 | 0.164 | 0.355 | **0.191** |
| 1910 | no | 11 | 0.166 | 0.303 | **0.137** |
| 1910 | yes | 38 | 0.099 | 0.324 | **0.226** |
| 1920 | yes | 49 | 0.196 | 0.523 | 0.328 |
| 1930 | yes | 49 | 0.147 | 0.442 | 0.295 |

In 1900 the jump is **the same in law and no-law states** (0.191 against 0.177), so nothing
is attributable to the statute. In 1910 law states jump more, by 0.089. And note the levels:
no-law states sit at 0.352 below the exit age where law states sit at 0.164, so the two
groups are not comparable places and the difference is not a causal estimate.

**After 1910 there is no benchmark at all.** All 49 jurisdictions have a law by 1920, so the
1920 and 1930 jumps of 0.328 and 0.295 cannot be separated from the age gradient. They are
reported, and should not be interpreted.

---

## Part C — what the build produced

First run, 2026-09-21, over **42,408,792** childhood observations. 11,927 county × year cells,
**11,746 usable**; 196 state × year rows.

Non-attendance, child-weighted over usable cells:

| Census year | Counties | Children | Required window | Standardised 10–13 | Above the exit age |
|---|---|---|---|---|---|
| 1900 | 2,737 | 4,883,827 | 0.211 | 0.181 | 0.426 |
| 1910 | 2,911 | 7,366,006 | 0.063 | 0.060 | 0.401 |
| 1920 | 3,028 | 10,448,756 | 0.095 | 0.072 | 0.543 |
| 1930 | 3,070 | 19,556,576 | 0.065 | 0.046 | 0.407 |

Across county cells the **median** non-attendance is **0.093 inside the required window
against 0.403 above the exit age** — a four-fold gap, and the basic evidence that the window
is being applied to the right children.

### The county tails, 1900, standardised, usable cells at 200+ required children

10,807 cells are rankable. Highest non-attendance:

| State | County | n | Raw | Standardised |
|---|---|---|---|---|
| Tennessee | Stewart | 610 | 0.846 | **0.837** |
| Georgia | Wilcox | 220 | 0.791 | 0.780 |
| Kentucky | Leslie | 240 | 0.779 | 0.722 |
| New Mexico | San Miguel | 719 | 0.672 | 0.698 |
| Arkansas | Izard | 715 | 0.676 | 0.659 |
| Louisiana | Acadia | 736 | 0.652 | 0.620 |

Appalachian Kentucky and Tennessee, the Ozarks, rural Georgia, Hispano New Mexico and
Acadiana Louisiana. Lowest: Indiana's Union (0.008) and Noble (0.016), California's Kings
(0.016), Pennsylvania's Fulton (0.017), New York's Seneca (0.017) — Midwestern and
north-eastern farming counties at near-universal attendance.

So the county range runs from under 1% to 84% of legally-required children out of school,
against a 0.056–0.815 range on the attainment measure's shortfall rate. Within Ohio in 1900,
where entry and exit ages are constant statewide, 88 counties run from **Champaign at 0.023 to
Vinton at 0.103**. Vinton also topped Ohio's attainment-based ranking, which is the one piece
of cross-validation between the two measures.

---

## Traps, collected

| Trap | Consequence |
|---|---|
| Comparing non-attendance across census years | The reference period runs ~12 months in 1900 to ~4 in 1920; the series is not a time series (B1) |
| Ranking on the raw window rate | Age composition drives it — New York's Seneca is 0.122 raw against 0.017 standardised (B2) |
| Reading the exit-age jump as the law's effect | No-law states jump 0.177 in 1900 against law states' 0.191; the gradient does the work (B3) |
| Interpreting the 1920 or 1930 discontinuity | Every state has a law by then, so there is no benchmark left (B3) |
| Using attainment to measure a compulsory-attendance law | It is blind to the enrolment margin, which is where the law operates (Start here) |
| `coalesce(exitage, ...)` for no-law state-years | The CLS panel stores `0`, not NULL, so coalesce never fires and the benchmark comes back empty |
| Treating `SCHOOL` 0/8/9 as non-attendance | They are uninformative; gate on `attendance_known` |
| Ranking cells without `cell_usable` | 15 cells have impossible coverage and 76 have no denominator |
| Treating `COUNTYICP` as FIPS, or without `STATEFIP` | Codes are state-dependent and are the FIPS code times ten |

## Sources

- Clay, K., Lingwall, J. & Stephens, M. Jr (2021). "Laws, educational outcomes, and returns
  to schooling." *Labour Economics* **68**, 101935. Replication package ICPSR 145122 v1.1 —
  the source of the state × year entry, exit and exemption ages.
- IPUMS USA Multigenerational Longitudinal Panel, linked extract `usa_00001.csv.gz`.
- [`ipums_linked_1900_1940_dictionary.md`](ipums_linked_1900_1940_dictionary.md) — `SCHOOL`'s
  coding and its reference-period warning.
- [`county_bite_from_linked_census.md`](county_bite_from_linked_census.md) — the
  attainment-based measure this one supersedes.

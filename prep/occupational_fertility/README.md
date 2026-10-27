# Occupational fertility, 1910 full count

The fertility of US college professors in 1910, against other occupations,
counted two ways:

1. **Mother-reported, wife-linked.** `CHSURV` (children surviving) and `CHBORN`
   (children ever born) were asked only of ever-married women. Every man is 00
   (N/A), so a male professor's count is his wife's, reached through `SPLOC`.
   These count children wherever they live: completed fertility.
2. **Father-reported, direct.** Persons whose `POPLOC` points at the man. This
   needs no wife and covers widowers, but sees only co-resident children, so
   the headline counts are children under 5 and under 10: recent fertility.

## Source

| File | What it is |
|---|---|
| `D:/ipums_1910_birth_occupation_fullcount.gz` | IPUMS extract **usa_00009**, 1910 100% database (`SAMPLE` 191004), gzipped CSV, 32 variables, 92,043,618 persons |
| `D:/ipums_1910_birth_occupation_ddi.xml` | Its DDI (sha256 `ad693727…`) |

`D:/usa_00003_ddi.xml` is **not** this extract's codebook. It describes the
1900–1950 linked extract read by
`prep/mandatory_schooling/convert_ipums_linked_1950.R`, and it has no `CHSURV`,
`CHBORN`, `SPLOC` or `MARST`.

## Scripts

| Script | Purpose |
|---|---|
| `convert_ipums_1910_fertility.R` | CSV.gz → Parquet at `D:/ipums_1910_fullcount_fertility`, partitioned by `STATEFIP`; ~1.5 min. The conversion checks: the 32 names match the DDI; the DDI labels for professor and fertility codes are as expected; everything is the 1910 100% sample; no man has a fertility code; `HISTID` is unique; source and Parquet counts agree by state. Codes are kept raw. |
| `build_professor_fertility.R` | Decodes and links: couples, men's own-children counts, female professors, diagnostics → `<Dropbox>/output/occupational_fertility/`; ~2.5 min |
| `test_professor_fertility.R` | 31 synthetic-fixture checks for the build and the standardisation helper |
| `../../analysis/occupational_fertility/professor_fertility_1910.R` | Way 1 (a)–(c), Poisson IRRs, way 2, reconciliation, female professors → `<Dropbox>/results/occupational_fertility/` |

## Coding traps

- **`CHBORN` and `CHSURV` are offset by one.** 00 = N/A, 01 = none, k+1 = k
  children. `CHBORN` 97/98/99 are unknown, illegible and missing. The 1910 file
  has none of these, and no woman has `CHSURV` > `CHBORN`.
- **DuckDB identifiers are case-insensitive.** A decoded column called
  `chborn` next to the raw `CHBORN` makes later references read the raw code.
  The build uses `dec_*` names internally for this reason.
- **`DURMARR` is years in the current marital status.** 98 = illegible and
  99 = missing/NA (never-married people carry 99). For a remarried wife it
  times the current marriage, while her `CHBORN`/`CHSURV` count children by
  every husband. `MARRNO` is not in the extract, so first marriages cannot be
  isolated.
- **Professors** are `OCC1950` 012–029. In 1910 nearly all are 029 (subject not
  specified), so there is no split by subject. Presidents and deans (010) are a
  separate group and are pooled only as `professor_plus_deans`.

## Build facts (2026-10-23)

- **Couples:** 16,797,244 `SPLOC` couples, every link reciprocal and
  male–female; no `MARST`=1 man lacks `SPLOC`.
- **Professors:** 11,444 male professors, of whom 6,663 are married with the
  spouse present, every wife with known `CHSURV`/`CHBORN`. 412 of them (6.2%)
  are not household heads (boarders and subfamilies), which household-head
  pairing would have missed.
- **Own children:** with step-children included and capped at 9, the
  own-children count equals `NCHILD` for every one of the 25.9 M men aged
  20–64.
- **Female professors:** 4,698, which is 29% of all professors and high for
  college faculty. `OCC1950` 029 probably includes "professors" of music,
  dancing and the like. Read `female_professors_profile.csv` before
  interpreting the female line.

## Decade trend, 1850-1910, on NCHILD

`CHBORN`/`CHSURV` exist only in 1900 and 1910, so the trend uses `NCHILD`
(own children of any age in the household, step-children included, capped at
9+). `NCHILD` is filled for every person in all six surviving censuses.

### Source

| File | What it is |
|---|---|
| `D:/ipums_1850_1910_fullcount_fertility.gz` | IPUMS extract **usa_00010**: 100% databases for 1850, 1860, 1870, 1880, 1900 and 1910 (1890 burned in 1921). 303,956,856 persons, 33 variables: usa_00009's 32 plus the contemporary `OCC` |
| `D:/ipums_1850_1910_fullcount_ddi.xml` | Its DDI (sha256 `66c251cd…`) |

Variables a census did not ask are blank (NULL in the Parquet), not 0:

| Variable | Blank in |
|---|---|
| `MARST`, `DURMARR` | 1850–1870 |
| `CHBORN`, `CHSURV` | before 1900 |
| `OCC` | 1900–1910 |

### Scripts

| Script | Purpose |
|---|---|
| `convert_ipums_1850_1910_fertility.R` | CSV.gz → Parquet at `D:/ipums_1850_1910_fullcount_fertility` (7.4 GB), partitioned by `YEAR`. It streams `COPY` straight to Parquet (304 M rows run out of memory as a table) and checks the result per year: sample codes, row counts, men's fertility codes, and unique `SERIAL`×`PERNUM`. The 1910 partition is cross-checked against the 1910 extract (identical rows, professors and Σ`NCHILD`). ~4 min |
| `build_professor_nchild_decades.R` | One census at a time: men aged 20–64 aggregated to cells (`occ_group × AGE × married_sp × in_household × us_born × race3 × urban × STATEFIP`, with n and sums of `NCHILD`, `NCHILD`² and own children under 5 and under 10) → `output/occupational_fertility/decades/`. Also writes every professor, the 1900/1910 couple calibration cells and the diagnostics. ~2 min |
| `test_professor_nchild_decades.R` | 20 checks on a two-census fixture: blank `MARST` in 1850, blank `OCC1950`, step-children, calibration decoding, and the fixed standard |
| `ipums_convert_helpers.R`, `occupation_groups.R` | Shared by both converters and both builders |
| `../../analysis/occupational_fertility/professor_nchild_by_decade.R` | Age-standardised means on one pooled standard, Poisson IRRs by year fitted on cells with offset log(n), calibration, definition audit, figure → `results/occupational_fertility/decades/` (`summary.md` first) |

### Results (2026-10-24)

Male professors aged 25–54 in households, mean `NCHILD`, age-standardised.

| Year | Professors | Ratio to all men | Ratio to other professionals | Poisson IRR vs all other men |
|---|---:|---:|---:|---|
| 1850 | 464 | 0.77 | 1.07 | 0.84 [0.76, 0.92] |
| 1860 | 608 | 0.78 | 1.05 | 0.81 [0.74, 0.89] |
| 1870 | 612 | 0.74 | 1.06 | 0.81 [0.72, 0.90] |
| 1880 | 1,677 | 0.76 | 1.09 | 0.84 [0.78, 0.91] |
| 1900 | 2,828 | 0.67 | 1.09 | 0.73 [0.70, 0.77] |
| 1910 | 8,023 | 0.64 | 1.04 | 0.70 [0.66, 0.74] |

- **Against all men,** professors had about three-quarters of the
  co-resident children from 1850 to 1880. The gap widens to about two-thirds
  in 1900–1910.
- **Against other professionals,** they are level or slightly above
  throughout.
- **Calibration** (wives aged 40–49, professor/all):

  | Year | Husband's `NCHILD` | Wife's `CHSURV` | Wife's `CHBORN` |
  |---|---:|---:|---:|
  | 1900 | 0.74 | 0.66 | 0.62 |
  | 1910 | 0.70 | 0.60 | 0.56 |

  `NCHILD` understates the professor gap in completed fertility by about
  0.08–0.10.

### Traps

- **No marital status before 1880.** `MARST` is blank 1850–1870, so
  "married" is `SPLOC` > 0 in every year.
- **Imputed relationships before 1880.** The census did not record
  relationship to head, so IPUMS imputes the family pointers, and with them
  `NCHILD`.
- **`HISTID` is not unique in 1870.** 331 `HISTID`s are each carried by two
  distinct person records. Key persons on `SERIAL`×`PERNUM`.
- **The professor code is not stable over time.**
  - 1850–1880 are almost all contemporary `OCC` 54.
  - 1910 has 3.3× the professors of 1900.
  - The female share rises from 6–7% to 12% in 1900 and 29% in 1910.
  - `professor_definition_audit.csv` profiles each year.

## County × census dataset, 1850-1910

The same `NCHILD` proxy, built for each county and census edition (standing
in for the decade) and broken into 10-year age bins (20–29 … 60–69).

- **Men only.**
- **Two universes side by side in every row:**
  - men in households (`GQ` 1/2), any marital status
  - married men (`_married`, wife in the household: `SPLOC` > 0)

### Scripts

| Script | Purpose |
|---|---|
| `build_professor_nchild_decades.R` | Also writes `decades/county_cells/YEAR=*/part.parquet`: men aged 20–69, grouped by `STATEFIP × COUNTYICP × age bin × occ_group × married_sp × in_household` (1.9 M cells, 81.7 M men). Its other outputs are unchanged; two of its CSVs gained a full `ORDER BY` and are now byte-stable across runs |
| `build_professor_county_nchild.R` | Shapes those cells into `output/occupational_fertility/county_decade/`, with `README_county_decade.md` as the data dictionary; seconds |

### Outputs

| File | Rows | What it is |
|---|---:|---|
| `professor_nchild_county_age.csv` / `.parquet` | 71,295 | One row per year × county × age bin, for every county with any man in the bin. A county with no professor appears with `n_prof = 0` |
| `professor_nchild_county.csv` | 14,316 | One row per year × county: an indirectly standardised summary, defined below |
| `county_codes_unmatched.csv` | | County codes that are not in the NHGIS crosswalk |
| `coverage_by_year.csv` | | Counties, professors and crosswalk misses per census |

**`professor_nchild_county_age`** holds, for each group (`prof` = `OCC1950`
012–029, `pres_dean`, `prof_other` = other professional codes, `allmen`) and
each universe:

- n, mean, sum and sum of squares of `NCHILD`
- own children under 5
- `ratio_prof_to_allmen`
- `low_n_prof` (fewer than 5 professors)
- `n_prof_gq`: professors in group quarters, counted but kept out of the
  means

**`professor_nchild_county`** compares each county's professors' summed
`NCHILD` (observed) with what the same professors would have at their own
county's all-men rates by age bin (`ratio_local`), and at national rates
(`ratio_national`). It includes an approximate SE.

**Keys:**

- `GISJOIN` = `G` + `%02d` state + `0` + `%04d` `COUNTYICP`. It follows
  NHGIS, historical counties ending in 5 included.
- 99%+ of counties match `input/nhgis0001_ts_nominal_county.csv`. The rest
  (DC's 1870 sub-units, an unidentified 9999, Oregon 605, …) are kept with
  `in_nhgis_crosswalk = FALSE`.
- `county_geoid` follows the project convention: NULL for codes not ending
  in 0.

### Verification (2026-10-25)

- **33 fixture checks.** These cover `GISJOIN` for a historical code,
  age-bin edges at 29/30 and 70, zero-professor counties, married via
  `SPLOC` in 1850, and the indirect ratio by hand.
- **County vs national cells.** County cells summed over counties equal the
  national `men_cells` at ages 20–59 in all 456 year × group × married ×
  household cells.
- **Professor totals.** Professor totals per age bin equal a direct Parquet
  query in all 30 year × bin cells.
- **Spot check.** Philadelphia 1880 recomputes by hand.

### Results

Pooled over counties, professors' observed / expected `NCHILD`, with expected
taken from all men at the same ages:

| Census | Counties with a professor | Professors | Vs own county's men | Vs national rates |
|---|---:|---:|---:|---:|
| 1850 | 171 | 545 | 0.89 | 0.80 |
| 1860 | 282 | 720 | 0.86 | 0.83 |
| 1870 | 254 | 763 | 0.82 | 0.79 |
| 1880 | 482 | 2,159 | 0.87 | 0.84 |
| 1900 | 665 | 3,453 | 0.76 | 0.70 |
| 1910 | 1,081 | 9,642 | 0.71 | 0.67 |

Professors lived in lower-fertility, mostly urban counties, so part of the
national gap is location.

Most counties have 1–3 professors (`counties_prof_ge_5`: 30 in 1850, 380 in
1910). Weight by `n_prof`, or pool, before reading a county's ratio.

## The 1850–1950 full counts (usa_00011)

`convert_ipums_1850_1950_fertility.R` converts
`D:/ipums_1850_1950_fullcount_fertility.gz` (27 GB, every 100% census
1850–1950 except 1890, the same 33 variables as usa_00010) to Parquet at
`D:/ipums_1850_1950_fullcount_fertility`, partitioned by `YEAR`. It took
13 min on 2026-10-26 for 816,654,828 persons.

**Codebook.** The extract came without an XML DDI, so it is read with
usa_00010's (`D:/ipums_1850_1910_fullcount_ddi.xml`). The converter checks
that DDI's 33 names and labels against the extract's own codebook page
(`D:/ipums_1850_1950_ddi.html`) and records both in `_manifest.json`.

**Checks.**
- Each year is one 100% sample.
- 1850–1910 match usa_00010 exactly (rows, professors, Σ`NCHILD`).
- 1940 has 131,902,398 persons against 132,404,766 in the 1940 full count
  (`D:/ipums_1940_fullcount`), and codes no presidents or deans separately.
- The 1950 database is an IPUMS preliminary release.
- `coverage_by_year.csv` shows what each census holds: `OCC` blank in
  1900–1930, `MARST` from 1880, `CHSURV` 1900–1910 only.

## Professors' children in the college-allocation event counties

`build_professor_children_event_panel.R` counts professors' children for
the 168 Andrews event counties, by birth decade 1830s–1940s.

**Counting rule.** Surviving children with a `POPLOC` father coded
`OCC1950` 012–029 (`prof_births`) or 010–029 (`prof_births_plus_deans`,
and `faculty_births` once scaled). Each birth year is read from the census
that sees it youngest, at ages 0–20. With the 1850–1950 extract (below)
every census but 1890 is held:

| Birth years | Source |
|---|---|
| 1830–1880 | 1850–1880 censuses, ages 0–9 except 1830s–40s from 1850 |
| 1881–1889 | 1900 census, ages 11–19 (no 1890 census) |
| 1890–1949 | the next census, ages 0–10 |

Nothing is interpolated, and the window ends the year before the latest
census (1949). Without that extract the builder falls back to the 1850–1910
extract plus the 1940 full count, interpolates 1911–1919 and ends in 1939.

Years are binned under both the standard and alternative decade
conventions.

**Which census reads a cohort matters.** County professor counts swing far
more than real faculty between censuses, for example Montgomery VA (VPI):
12, 18, 8 and 173 coded professors in 1910–1940. A cohort's count therefore
depends on the census it is read from: VPI's 1920s-born faculty children are
69 read from 1940 and 2 read from 1930. A uniform national factor cannot fix
this; see `results/land_grants/professor_channel/memo.md`.

**Missing vs zero.** A county no census enumerated is NA, not 0.

**Outputs** (in `output/occupational_fertility/college_channel/`):

- `professor_children_county_{year,decade,decade_alt}.csv`
- `professor_stock_county_census.csv`
- the national series `national_professor_children_{year,decade}.csv`

**Runtime and tests.** About 1 min on all ten censuses. Fixture tests:
`test_professor_children_event_panel.R` (46 checks, including the
single-extract path and the 1930 coverage estimate).

**Trap: DuckDB join plan.** A non-equality term in a DuckDB `LEFT JOIN … ON`
clause (here `c.POPLOC > 0`) silently replaces the hash join with a
blockwise nested loop. The first run took more than six hours without
finishing; the fixed one takes seconds. Keep join conditions to equalities.

**Analysis.** The event-study analysis is
`analysis/land_grants/temporal_support_1830_1950/08_run_professor_channel.R`.
See `results/land_grants/professor_channel/memo.md`.

## Comparable professor counts across censuses (coverage factors)

`OCC1950` professors cover a different share of real college faculty in
each census. Coverage is measured as census men coded 010–029 divided by
NCES male professional staff (`nces_faculty_benchmark.csv`, NCES 93-442,
Table 26):

| Census | Coverage | `faculty_factor` | Note |
|---|---:|---:|---|
| 1870 | 0.244 | 4.10 | also used for 1850 and 1860 |
| 1880 | 0.424 | 2.36 | |
| 1900 | 0.240 | 4.16 | |
| 1910 | 0.404 | 2.47 | |
| 1920 | 0.426 | 2.35 | |
| 1930 | 0.453 | 2.21 | NCES benchmark estimated (`nces_estimated`) |
| 1940 | 0.502 | 1.99 | 1.94 on the 1940 full count used before |
| 1950 | 0.626 | 1.60 | |

NCES reports no 1929–30 professional staff, only instructional staff
(82,386). The 1930 benchmark scales that by the 1931–32 ratio of male
professional to instructional staff (71,680 / 88,172), giving 66,976 men.
Counts come from the 1850–1950 extract when its Parquet exists.

**Why rescaling is valid.** For men the jumps are coverage, not
composition: 1910 professors are more concentrated in college counties
than 1900's, with the same age and marriage profile.

**What 010–029 covers.** Presidents and deans (010) are included because
NCES counts all staff. In 1940 the census folds them into 029 anyway.

**`build_professor_coverage.R`** writes
`output/occupational_fertility/professor_coverage_factors.csv`. Rescaled
census counts reproduce the NCES totals exactly in every benchmark year.

**Where the factor is used:**

- **`build_professor_children_event_panel.R`** adds `faculty_births`:
  children of 010–029 fathers × the factor of the census the birth year is
  read from (1911–1919 interpolated between harmonised ends only in the
  fallback build). It also
  writes the national series and `n_faculty` in the stock file. Raw columns
  are unchanged.
- **`build_professor_county_nchild.R`** adds `n_prof_deans_all`,
  `faculty_factor` and `n_faculty_equiv` to the county summary.

**What it leaves alone.** The `NCHILD` means and ratios are within-census
comparisons and need no factor.

**Scope and caveats.** The factor rescales counts; it does not reclassify
anyone. NCES counts all professional staff, so the faculty scale is an
upper bound. A person-level fix needs `OCCSTR` occupation strings in a new
extract.

## Professors' fertility in American Men of Science 1956

`analysis/occupational_fertility/professor_fertility_mos1956.R` measures
professors' fertility without the census occupation codes, from the
digitised American Men of Science 1956 in Kim & Moser's replication package
(*Women in Science: Lessons from the Baby Boom*, Econometrica;
`D:/ECMA_MS-22741_Replication.zip`).

**Source handling.** Only two files are extracted, to
`<Dropbox>/raw/kim_moser_mos1956/` with SHA-256 provenance:
- the cleaned `01_MoS 1956.csv`: 82,094 scientists;
- the career-title dictionary.

None of the package's code is run. The script fails if the row count or the
`child` distribution differs from the file as profiled on 2026-10-26.

**Definitions.**
- **Professor:** ever held Assistant, Associate or Full Professor (medical
  included), Department Head, Dean or Professor Emeritus.
- **Comparison:** every other scientist, with the never-academic subset
  reported too.
- **Children:** `child`, blank = 0, as Kim & Moser treat it.
- **Married:** year of marriage reported.
- **Completed fertility:** born 1905 or earlier.

**Results (men, 2026-10-26).**
- **Sample.** 35,294 of 65,914 men are professors.
- **Completed cohorts.** Professors have 1.59 children against 1.67 for other scientists (1.88 vs 1.82 among the married). They are more often childless (30% vs 25%) and less often married (77% vs 84%).
- **Poisson IRR with birth-year FE:**
  - all men: 0.99 [0.98, 1.00];
  - married men: 1.03 [1.02, 1.04];
  - with field FE: 1.02.

So science professors have the fertility of other scientists. Any shortfall
comes from marrying less, not from fewer children within marriage.

**Caveats.**
- AMS lists scientists only, so this is science faculty.
- Only scientists alive and listed in 1955–56 are covered.
- 16% of married men have a blank `child`, which may hide unreported
  children.

## AMS-predicted faculty children (`build_professor_birth_schedule.R`, `build_professor_ams_channel.R`)

The census-observed channel reads each birth cohort's faculty children from
one census, so it inherits that census's coding. These two scripts predict
them instead.

**Shared loading.** `mos1956_helpers.R` holds the MoS 1956 extraction,
validation and professor definition, shared with
`analysis/occupational_fertility/professor_fertility_mos1956.R`.

**Birth schedule** (`build_professor_birth_schedule.R`).
- **Sample.** 14,756 male professors born 1870–1905: 1.59 children, 30%
  childless, median marriage age 28.
- **Timing.** Births at marriage + 1, then every 2.5 years. The variant
  `uniform10` spreads them over the 10 years after marriage.
- **Output.** Single-year rates per professor, and ten-year windows, in
  `college_channel/professor_birth_schedule.csv`. The rates integrate to
  1.59 by construction (checked).

**County predictions** (`build_professor_ams_channel.R`).
- **Stock.** Men coded 010–029 by event county × census × single-year age,
  from the 1850–1950 full counts.
- **Prediction.** The stock times the rates. Each decade averages:
  - the forward view from the census that opens it;
  - the backward view from the census that closes it.

  The 1880s, 1890s and 1950s have one end only; the 1830s repeat the 1840s.
- **Versions.** Scaled by each census's factor, and unscaled.
- **Fit.** Predicted is 36–81% of the census-observed count in the event
  counties, with correlation 0.92–0.99 across counties from the 1890s on. The
  check is written to `ams_vs_observed_decade.csv`.

**Analysis.** `08_run_professor_channel.R` runs these as channels
`ams_faculty` and `ams_raw`. See
`results/land_grants/professor_channel/memo_ams.md`.

## Census-measured fertility for the predicted channel (`build_professor_census_fertility_schedule.R`)

The same prediction, with births per professor measured in the census from
professors' wives, linked through `SPLOC` and decoded as `value − 1`.

**Two eras.**

| Era | Measure | Couples (professor aged 20–69) | Applied to censuses |
|---|---|---:|---|
| 1900 + 1910 | CHSURV, children surviving | 9,512 | 1850–1920 |
| 1940 | CHBORN, children ever born; sample line only, CHSURV not asked | 1,608 | 1930–1950 |

**Schedule.**
- **Cumulative curve.** By husband's 5-year age, m(a) = share ever married
  × mean count per couple. Unmarried professors count 0.
- **Monotone fit.** A couple-weighted isotonic fit keeps m from falling at
  older ages; it only binds in the small 1940 bins.
- **Rates.** The yearly increments of m.
- **Lifetime totals.** 2.32 surviving children per professor (1900–1910) and
  1.96 ever born (1940).
- **Output.** `professor_birth_schedule_census.csv`, with a `census` column,
  plus the curve and a manifest.

**Channel builder.** `build_professor_ams_channel.R` now takes any schedule:
- `OCC_BIRTH_SCHEDULE` sets the schedule file;
- `OCC_CHANNEL_PREFIX` sets the column and file prefix;
- a `census` column in the schedule gives each census its own rates.

Its defaults reproduce the AMS outputs byte for byte. The census version is
built with `OCC_CHANNEL_PREFIX=chsurv`. Predicted counts are 75–92% of the
census-observed count in the event counties from the 1880s to the 1930s,
with correlation 0.93–0.98 across counties from the 1890s on.

**Results.** See `results/land_grants/professor_channel/memo_chsurv.md`.

## County coverage factors (`build_professor_county_coverage.R`)

A county-level alternative to the national coverage factor.

**Source.** The Education by the Numbers faculty panel
(`Data/processed/faculty_data/county_faculty_panel_estimated_interpolated.csv`):
corrected and FTE-converted headcounts, gaps interpolated, 2020 counties.

**The factor.**
- **Census professors.** Persons of both sexes coded 010–029 in each county.
- **Rule.** Factor = faculty headcount / census professors where the
  headcount is larger, else 1. It never scales down.
- **Year matching.** Academic years 1909–10 to 1949–50 for the 1910–1950
  censuses; the same year before 1910. 1949–50 covers land-grant colleges
  only.
- **Output.** `professor_county_coverage_factors.csv`, with a `reason` per
  cell (`scaled` / `headcount_le_census` / `no_headcount` /
  `no_census_professors`), and a summary by census for the event counties.

**Where it is used.**
- `build_professor_children_event_panel.R` adds `county_faculty_births`.
- `build_professor_ams_channel.R` adds `<prefix>_county_faculty_births`.
- Both read `OCC_COUNTY_COVERAGE_FACTORS` and leave their other columns
  unchanged.

**Tests.** `test_professor_children_event_panel.R` pins the factor file to a
fixture and checks both cases: the factor applies, and a cell without a
factor is not scaled.

**Caution.** College counties the census coded with 1–5 professors get
factors of 30–68. These dominate the noise; see
`results/land_grants/professor_channel/memo_county_scale.md`.

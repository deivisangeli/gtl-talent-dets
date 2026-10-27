# Building the attendance measure on the full-count censuses

The steps to rebuild the county attendance measure from the **unlinked** 1900–1930 full
counts instead of the MLP linked extract. The measure itself is specified in
[`county_attendance_from_linked_census.md`](county_attendance_from_linked_census.md) and does
not change; only its source does, and that removes its one remaining selection problem.

Nothing here is built yet. Part A is a manual IPUMS request and gates everything else.

---

## Start here: the linkage buys nothing for this measure

The attendance measure reads five variables off a single childhood census row — `YEAR`,
`AGE`, `SCHOOL`, `STATEFIP`, `COUNTYICP`. It needs no `HIK`, no cross-year link and no 1940
record. So the linked extract is the wrong source: the linkage contributes nothing to the
measure and costs the only selection problem it still carries.

**What that selection costs, measured on the built linked version.** `coverage` is linked
school-age children over the county's population, and children aged 5–18 were roughly 26% of
the population in this era:

| Census year | Median coverage | Share of a county's children the linkage holds |
|---|---|---|
| 1900 | 0.069 | **~27%** |
| 1910 | 0.087 | ~33% |
| 1920 | 0.107 | ~41% |
| 1930 | 0.170 | **~65%** |

Two problems in one table. Between two thirds and a third of children are missing — and the
share missing **changes by a factor of 2.4 across the four census years**, tracking the
`LINK19xx` gradient (17.57%, 32.48%, 54.15%, 95.09% of 1940 rows). So the census years are
selected to very different degrees, which compounds the reference-period problem: at present
neither the level nor the change between years reads cleanly. On the full counts both become
interpretable, and `coverage` turns from a selection warning into an ordinary denominator
check that should sit near 0.26.

There is a plain power gain too. Ages 5–18 across the four full counts is roughly **107 M
children against the 42.4 M now held** — about 2.5× — so the standardisation band's
five-per-age floor would almost never bind and small counties become estimable.

---

## Part A — obtain the extract

### A1. What to request

The linked extract's DDI confirms both that the samples exist and that this account has
access: it was itself drawn from the **1900, 1910, 1920, 1930 and 1940 100% databases**.

IPUMS USA → *Create an extract*:

| | |
|---|---|
| Samples | 1900, 1910, 1920, 1930 **100% databases** |
| Variables | `YEAR`, `AGE`, `SEX`, `RACE`, `SCHOOL`, `STATEFIP`, `COUNTYICP`, `SERIAL`, `PERNUM`, `MOMLOC`, `POPLOC`, `GQ` |
| Select cases | **`AGE` 5–18** |

1890 is not requested because the microdata burned. 1940 is not requested because the project
already holds it in a better form, and because the attendance measure needs childhood rows
rather than the 1940 cross-section.

**Apply the age restriction through *Select cases*.** It is the difference between roughly
398 M rows and 107 M, and IPUMS applies it server-side, so it costs nothing to download and
nothing to filter later.

### A2. What to expect

Twelve columns over ~107 M rows should come to roughly **1.5–2.5 GB gzipped** — smaller than
the 12.2 GB linked extract despite holding 2.5× the children, because that one carries 58
columns and every age.

### A3. Download the DDI as well

Take the **DDI codebook (XML)** and the extract description, and put both beside the data in
`Data/raw/mandatory_schooling/`. This is the lesson from the linked extract, which arrived as
a bare CSV and needed its codebook fetched separately a year later. With the DDI in place,
`ddi_helpers.py` can cross-check the columns on every run, exactly as it now does for
`usa_00001.xml`.

---

## Part B — extract to Parquet

### B1. `prep/mandatory_schooling/extract_fullcount_attendance_inputs.py` (new)

A sibling to `extract_linked_bite_inputs.py`, reusing its proven pattern: read every column
as `pa.string()`, cast explicitly, stream with `pacsv.open_csv` over a
`pa.CompressedInputStream`, and write Parquet partitioned by `YEAR`.

Simpler than the linked extractor in two ways — one output table rather than two, and no
year-ordering trap, since nothing here depends on seeing 1940 last. **Keep the explicit
casting regardless**: a full-count CSV will still type a sparse column wrongly from its first
block, and that failure surfaces 100 M rows in.

```python
SCHEMA = pa.schema([
    ("YEAR", pa.int16()), ("AGE", pa.int16()), ("SEX", pa.int8()), ("RACE", pa.int8()),
    ("SCHOOL", pa.int8()), ("STATEFIP", pa.int16()), ("COUNTYICP", pa.int32()),
    ("SERIAL", pa.int64()), ("PERNUM", pa.int16()),
    ("MOMLOC", pa.int16()), ("POPLOC", pa.int16()), ("GQ", pa.int8()),
])
```

`STATEFIP` must be widened to `int32` before any `* 1000` arithmetic. The linked build hit
this: `48 * 1000` overflows an `int16` and DuckDB raises rather than wrapping, which is the
good outcome, but only because a fixture happened to carry Texas.

### B2. What to assert

- Row counts per census year, against the numbers in the extract description. Record them as
  constants so a swapped or truncated file fails the next run, as `EXPECTED_BY_YEAR` does for
  the linked extract.
- School-age totals near **27%** of each census's published population (~76 M, ~92 M,
  ~106 M, ~123 M for 1900–1930).
- `SCHOOL` domain is `{0, 1, 2, 8, 9}` — `1` not in school, `2` in school, the rest
  uninformative.
- The share of `COUNTYICP` values not divisible by ten, which was **0.027%** on the linked
  extract. A materially different figure means a different county coding.

---

## Part C — the measure, which does not change

### C1. Geography is contemporaneous, so there is no proxy

This is the conceptual gain and it is worth stating plainly, because it is the difference
between this measure and the attainment one.

In the attainment measure the county is a **proxy**: schooling is observed in 1940, so the
county has to be recovered from a childhood census, which needs the linkage, a candidate-year
choice and a `selection_distance` that runs up to 8.5 years between the observation and the
middle of the compulsory window.

Here the child is observed **at** school age, and `COUNTYICP` is their residence on that same
row, from that same enumeration, on the same date as the attendance answer. There is no
assignment step, no candidate year, no `selection_distance` and no assignment strata — so the
collinearity between cohort, linkage regime and boundary vintage that limits the attainment
measure does not arise.

Two residual mismatches remain and should not be papered over:

- **County of residence is not county of schooling.** A child could live one side of a county
  line and attend school the other. District schooling makes this small, but it concentrates
  near boundaries and on urban fringes, and nothing in these variables measures it.
- **The attendance question covers a window preceding enumeration**, while residence is
  recorded at enumeration, so a family that moved within the window has part of its
  attendance attributable to another county.

The second is boundable, and the linked extract is the right tool for bounding it. Of the
**7,100,615** children observed at school age in two censuses a decade apart:

| Span | Children | Same county after 10 years | Same state |
|---|---|---|---|
| 1900→1910 | 1,626,737 | 0.784 | 0.910 |
| 1910→1920 | 2,301,213 | 0.772 | 0.910 |
| 1920→1930 | 3,172,665 | 0.776 | 0.912 |
| **all** | **7,100,615** | **0.776** | **0.911** |

So 22% change county per decade. Scaled to a 12-month reference window that is roughly 2% of
children, and under 1% for 1920's four-month window — an order of magnitude below the
attainment measure's proxy error. Note the direction runs opposite to the reference-period
problem: the long 1900 window is the worst for residence mismatch and the best for capturing
attendance.

And usefully, **91% are in the same state after a decade against 78% in the same county**. The
state determines which law applies, so treatment assignment is more stable than unit
assignment — within a reference window it is right for well over 99% of children.

### C2. Everything downstream is unchanged

`build_county_attendance.R` needs one new input and no new logic. Add a
`COUNTY_ATTENDANCE_SOURCE` switch taking `fullcount` or `linked`, defaulting to `fullcount`
once the extract lands. The window flags, the counterfactual window for state-years with no
law, the ages 10–13 standardisation, the above-exit-age benchmark, the exit-age discontinuity
and the usability flags all carry over untouched.

**Keep the linked path working.** It is the only way to measure the linkage selection rather
than assert it, which is Part D.

### C3. `COVERAGE_MAX` becomes a real check

On the full counts coverage should sit near **0.26** rather than 0.07–0.17, so the current
one-sided `> 0.30` flag starts doing useful work. Make it two-sided: materially above means a
wrong denominator, materially below means missing children. Record the observed distribution
so the band is set from data rather than from the 30% ceiling argument alone.

---

## Part D — the comparison that validates the change

Computing county attendance both ways and correlating them is the most valuable by-product
here: a direct estimate of how much the MLP linkage distorts a county measure, which is a
number this project does not have.

1. Join the two county × census-year tables and report the median absolute difference in
   `not_attending_std_1013`, the Spearman correlation, and both against `coverage`.
2. Read the outcome honestly:
   - **They agree closely** → the selection was benign for this measure, the already-committed
     linked results stand, and that is worth recording as a licence for the linked extract in
     other work.
   - **They diverge, and the divergence tracks `coverage`** → the linked numbers in
     `output/mandatory_schooling/county_attendance/` need reissuing, and the divergence is
     itself a finding about MLP linkage selection at county level.
3. Median coverage on the full counts should sit near 0.26 in every census year **with the
   1900→1930 gradient gone.** Its disappearance is the test that the selection has been
   removed rather than relabelled.

---

## Part E — what the full counts do not fix

- **The reference period still changes across censuses** — ~12 months in 1900, ~7.5 in 1910,
  ~4 in 1920 enumerated 1 January, ~6 in 1930. The four years remain not a time series, and
  nothing may be pooled or differenced across them.
- **Attendance still swings 12% to 87% across ages 5–18**, so county rates still need
  standardising over ages 10–13.
- **County boundaries still change between censuses**, so a `county_geoid` is not the same
  area in 1900 and 1930.
- **Ages are still misreported.** With no 1940 row to compare against, the internal age check
  that the linked build performs is unavailable, so age heaping at multiples of five cannot be
  diagnosed the same way.
- **The exit-age discontinuity still needs its no-law benchmark**, and still runs out after
  1910 when all 49 jurisdictions have a law.

---

## Traps, collected

| Trap | Consequence |
|---|---|
| Downloading the full samples without *Select cases* | ~398 M rows instead of ~107 M, for no gain |
| Omitting the DDI again | The linked extract's codebook had to be fetched a year later; `ddi_helpers.py` cannot cross-check without it |
| `STATEFIP` left as `int16` | `48 * 1000` overflows; widen before any GEOID arithmetic |
| Letting the reader infer column types | A sparse column types wrongly from the first block and fails 100 M rows in |
| Reading county of residence as county of schooling | It is not; the mismatch is small but real, and unmeasurable here (C1) |
| Comparing non-attendance across census years | The reference period changes; the gradient is not a trend (E) |
| Dropping the linked path once the full counts land | It is the only way to measure the linkage selection (D) |
| Expecting the full counts to fix the age gradient | They do not; standardisation is still required (E) |

## Sources

- IPUMS USA 100% databases, 1900–1930. The linked extract's DDI,
  `Data/raw/mandatory_schooling/usa_00001.xml`, records these as its own samples.
- Clay, K., Lingwall, J. & Stephens, M. Jr (2021), *Labour Economics* **68**, 101935,
  replication package ICPSR 145122 v1.1 — the state × year entry, exit and exemption ages.
- [`county_attendance_from_linked_census.md`](county_attendance_from_linked_census.md) — the
  measure itself, and the results from the linked build this one replaces.
- [`county_bite_from_linked_census.md`](county_bite_from_linked_census.md) — the
  attainment-based measure, which still needs the linkage and so keeps the proxy.

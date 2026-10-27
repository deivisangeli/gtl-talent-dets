# County compulsory-schooling bite, from attendance

Built by `build_county_attendance_bite.R` into
`output/mandatory_schooling/county_attendance_bite/`. Checked by
`test_county_attendance_bite.R` (22 synthetic-fixture checks, runnable without the
776 MB attendance parquet).

This is the **third** bite-shaped object in the project and the only one measured on
attendance. Its siblings are `county_bite_from_linked_census.md` (attainment shortfall
among 1940 students) and the descriptive attendance measures in
`county_attendance_from_linked_census.md`, which this builds on.

---

## What it is

**Among the children a state's compulsory-schooling law would later require to attend,
the share who were not attending before the law took effect.**

One row per county, for the 16 states adopting after 1900. Pre-law by construction —
the build asserts `baseline_year < compulsory_law_year` on every row.

Compulsory-schooling law varies only at the **state** level. County-level treatment
intensity therefore has to come from how far each county sat from compliance before
the statute arrived: a county already at near-universal attendance had nothing to gain,
a county far below had a great deal. That is the whole idea, and it is what lets a
design carry state × period fixed effects and still identify something — which matters
here, because the state-level law/no-law contrast cannot. Eleven of the 16 late
adopters are the CLS-coded South, every state with a law in 1900 is non-South, and the
exposed/unexposed attendance gap is a flat 2.5–3.5pp level difference that is present
even at age 6, where no statute applies.

## Why attendance rather than attainment

`build_county_bite.R` compares completed schooling against what the law required. That
is a downstream proxy, blind to the margin the statute actually names: if enforcement
draws in marginal children who still leave at the exit age, attainment barely moves and
a shortfall measure reads that as no bite. Its own spec reports the consequence — the
county before/after contrast has **no signal**, an SD of 0.0484 against a binomial noise
floor of 0.0454, a ratio of **1.07**.

The attendance bite runs at a ratio of **5.40** pooled and **4.25** within state, with a
split-half reliability of **r = 0.897** across 1,270 counties. It carries real county
information; the attainment version does not.

## How much was already there

Most of it. `build_county_attendance.R:262-267` forms `not_attending_required` over
`in_window OR in_window_cf` — the actual statutory window where a law was in force, and
the state's **modal law-in-force window** where none was. Its comment at `:162-167`
names the second case as "the pre-law baseline". This build extracts that, adds a
cross-state-comparable band, applies the usability guards and measures signal.

### The trap it exists to document

On pre-law rows in `attendance_county_year.csv`, `entryage` and `exitage` are **0**.
That is the raw CLS convention for "provision absent", carried through by `any_value()`
— **not** the window the rate was computed over, which was `cf_entryage`/`cf_exitage`.
Reading those two columns and concluding the pre-law rate is degenerate is wrong, and
is an error this session actually made before checking the builder. The fixture test
`the pre-law window convention does not empty the cf rate` pins it.

---

## The three bases

| Column | Ages | Reads as |
|---|---|---|
| `bite_std_8_14` | 8–14, standardised | **headline** — comparable across states, spans the edges where the law binds |
| `bite_std_1013` | 10–13, standardised | the existing band; comparable but compressed |
| `bite_cf_window` | the state's own eventual window | statutorily apt, **not** comparable across states |

All standardisation is **direct with uniform weights** — the unweighted mean of the
single-age non-attendance rates over the band — so a county whose children skew old
cannot look non-compliant on composition alone. NULL if any age in the band carries
fewer than 5 children.

### Why 8–14 rather than 10–13

Two reasons, one statutory and one empirical.

Post-adoption windows run entry 7–8 with an effective leaving age of 14–15 (Alabama
8–16, Mississippi 7–17, Texas 8–14, Maryland 7–17, Iowa 7–15), so 8–14 is the band the
law actually covers.

And 10–13 sits on the **attendance peak**. The 1900 age profile is 12.3% attending at
age 5, 69.0% at 8, 85–87% at 10–12, 74.5% at 14, 49.7% at 16, 24.2% at 18. Measuring
the bite at 10–13 measures it where there is least room for a law to move anything.
Empirically the wider band is larger for **89.8%** of counties, median difference
**+0.0198**, and the widening holds within each baseline year (1900: 98.9%, median
+0.0554; 1910: 87.5%, median +0.0160).

The existing 10–13 band remains the right choice for the *descriptive* attendance
comparisons it was built for — it is the widest band inside the required window in
1,762 of 1,808 law state-years. It is the wrong choice for a dose.

---

## Coverage

Pre-law censuses exist only for the 16 post-1900 adopters: 1900 for all 16, 1910 for
the 11 adopting 1913 or later. 1920 and 1930 have none — every jurisdiction has a law
by 1920.

| | counties |
|---|---:|
| in a post-1900-adopting state | 1,456 |
| with a usable pre-law cell | **1,312** |
| with both a usable pre-law and post-law cell | 1,309 |
| rows written | 1,341 |

The 138 counties with only post-law observations are overwhelmingly OK (55), TX (31)
and FL (20) — counties that did not exist or were unpopulated in 1900/1910, not data
failures.

### Bite by state, latest pre-law baseline, usable counties

| state | law | baseline | counties | bite | SD |
|---|---:|---:|---:|---:|---:|
| IA | 1902 | 1900 | 99 | 0.124 | 0.033 |
| MD | 1902 | 1900 | 23 | 0.270 | 0.090 |
| MO | 1905 | 1900 | 114 | 0.214 | 0.076 |
| DE | 1907 | 1900 | 3 | 0.233 | 0.050 |
| OK | 1908 | 1900 | 18 | 0.231 | 0.100 |
| NC | 1913 | 1910 | 98 | 0.125 | 0.044 |
| TN | 1913 | 1910 | 95 | 0.120 | 0.060 |
| LA | 1916 | 1910 | 60 | 0.267 | 0.088 |
| TX | 1916 | 1910 | 200 | 0.088 | 0.066 |
| AL | 1917 | 1910 | 67 | 0.181 | 0.050 |
| AR | 1917 | 1910 | 75 | 0.148 | 0.057 |
| GA | 1917 | 1910 | 142 | 0.171 | 0.056 |
| VA | 1918 | 1910 | 109 | 0.124 | 0.055 |
| FL | 1919 | 1910 | 45 | 0.137 | 0.091 |
| SC | 1919 | 1910 | 43 | 0.161 | 0.048 |
| MS | 1920 | 1910 | 79 | 0.125 | 0.056 |

---

## Guards

Flags, never filters — a silently missing county looks like a county with no bite.

| Flag | Rule |
|---|---|
| `cell_usable` | carried from the attendance build: `n_required >= 25`, `coverage <= 0.30`, denominator present |
| `bite_thin` | `n_band < 100` — the attenuation floor |
| `bite_rankable` | `n_band >= 200` |
| `bite_usable` | `cell_usable` and a non-NULL band rate and not thin |

`MIN_CELL` 25 makes a cell estimable; 100 makes it usable as a regressor; 200 makes it
rankable. The three are not the same. At n = 100 the standard error on a rate near 0.11
is about 0.03 against a cross-county SD near 0.075, which is why the regression floor
sits there.

1900 is the thin year and it is exactly where the pre-law cells live — across all county
× year cells, 97 fall below 25 in 1900 against 8 in 1930.

---

## Traps, collected

| Trap | Why it bites |
|---|---|
| Reading `entryage`/`exitage` on a pre-law row | They are 0, the CLS "provision absent" convention, not the window used |
| Differencing attendance across census years | `SCHOOL`'s reference period is ~12 months in 1900, ~7.5 in 1910, ~4 in 1920 (enumerated 1 January), ~6 in 1930. The state series 0.211 → 0.063 → 0.095 → 0.065 is mostly that |
| Comparing `bite_std_8_14` across states without checking `baseline_year` | It is 1900 for 268 counties and 1910 for 1,073. Use the `bite_*_1900` columns for anything cross-state |
| Using the bite where `bite_usable` is FALSE | Thin cells produce extreme rates that look like strong bite |
| Treating the bite as exogenous | It is pre-law non-compliance, correlated with everything that makes a county poor and rural |
| Using a noisy baseline as treatment intensity | Measurement error in the baseline mechanically produces mean reversion in the outcome |

## What it may be used for

**May:** a cross-sectional treatment intensity for an outcome measured elsewhere —
bite × post on `amws_per_1000_births`, with state × cohort fixed effects and within-state
county variation. That never differences `SCHOOL`.

**May not:** the basis of a pre/post regression on attendance itself. A pre-law versus
post-law attendance contrast is by construction a cross-census difference, and the
attendance spec's rule — nothing is pooled or differenced across census years — is not
negotiable here.

## Selection, still unmeasured

The source is the MLP linked extract. Median linkage `coverage` is **0.057 in 1900** and
**0.078 in 1910** against a true school-age share near 0.26, so the bite rests on roughly
a fifth to a third of each county's children, and the composition of who appears changes
across censuses. `county_attendance_from_fullcount.md` specifies the rebuild on the
unlinked 1900–1930 full counts that removes this; it should happen before the bite
carries causal weight.

The attainment spec asks for both measures to be built and correlated. That correlation
is now possible and has not been run.

## Outputs

```text
bite_county.csv / .parquet        one row per county            (PRIMARY)
bite_county_band_age.parquet      county x age, the band's inputs
bite_signal_by_state.csv          SD against the noise floor, per state
input_manifest.csv                paths, sizes, md5
validation_summary.csv            18 checks
notes.txt                         which column to use, and the traps
```

## Sources

- `prep/mandatory_schooling/county_attendance_from_linked_census.md` — the attendance
  measures this builds on
- `prep/mandatory_schooling/county_attendance_from_fullcount.md` — the rebuild that
  removes the linkage selection
- `prep/mandatory_schooling/county_bite_from_linked_census.md` — the attainment-based
  sibling this supersedes as a dose
- Lingwall (2014), Table 6.2, via `input/compulsory_schooling_laws.csv`; the build
  asserts the vintage rather than trusting the file

# IPUMS MLP linked census extract — variable dictionary

Generated 2026-09-18 by `prep/mandatory_schooling/profile_ipums_linked_census.py`.

Source: `C:\Users\megaj\OneDrive\Documentos\usa_00001.csv.gz` — 13.08 GB gzipped, **220,979,023 rows**, 58 columns. Profiled in 16 minutes.

## What this file is

An IPUMS Multigenerational Longitudinal Panel **linked** extract. Despite the filename it is not a two-year 1900–1940 link: it stacks **all five census years**, one row per person per census in which they appear. Row counts sit far below full-count size because only linked cases are included.

| Census year | Rows | Share |
|---|---|---|
| 1900 | 12,971,952 | 5.9% |
| 1910 | 23,984,383 | 10.9% |
| 1920 | 39,982,220 | 18.1% |
| 1930 | 70,207,628 | 31.8% |
| 1940 | 73,832,840 | 33.4% |

**Total: 220,979,023**

## Keys

- **`HISTID`** identifies a person *record* within a census year. It is stable across IPUMS data versions but **is not shared by the same person across years**.
- **`HIK`** is the cross-year person key, assigned only to people the MLP could link. To follow someone through the censuses, group by `HIK`; to identify a single record, use `HISTID`.
- `LINK1900`…`LINK1940` flag which censuses a person links to.
- Household is `(YEAR, SERIAL)`; person within household is `PERNUM`.

## Findings the merge depends on

**`BIRTHYR` vs the CLS cohort key on 1940 rows.** Comparing `BIRTHYR` against `1940 - AGE - 1` over 73,832,840 rows: **0.0% agree exactly**. Distribution of `BIRTHYR - (1940 - AGE - 1)`:

| Difference | Rows | Share |
|---|---|---|
| +1 | 73,832,840 | 100.0% |

Clay, Lingwall and Stephens compute year of birth as `1940 - age - 1` because enumeration was on 1 April and most respondents had not yet had their birthday. Use their convention when joining their cohort file, whatever `BIRTHYR` says, so the treatment key matches the one the law measure was built on.

**`BPL` within a person.** Of 2,309,382 sampled `HIK`s (those beginning `a`, roughly 1/60 of the file), **113,538 carry more than one `BPL`** across their linked records. Where birthplace disagrees, take it from the 1940 record, which is the one the CLS design reads.

**`HIK` spans years.** 2,309,382 of the sampled 2,309,382 appear in more than one census year, confirming `HIK` as the across-year key.

## Availability at a glance

Which census years each variable is populated in. **Empty means the question was not asked that year**, not that the value is zero — this is the first thing to check before using any column.

| Variable | 1900 | 1910 | 1920 | 1930 | 1940 |
|---|---|---|---|---|---|
| `YEAR` | yes | yes | yes | yes | yes |
| `SERIAL` | yes | yes | yes | yes | yes |
| `STATEICP` | yes | yes | yes | yes | yes |
| `STATEFIP` | yes | yes | yes | yes | yes |
| `COUNTYICP` | yes | yes | yes | yes | yes |
| `URBAN` | yes | yes | yes | yes | yes |
| `GQ` | yes | yes | yes | yes | yes |
| `FARM` | yes | yes | yes | yes | yes |
| `PERNUM` | yes | yes | yes | yes | yes |
| `MOMLOC` | yes | yes | yes | yes | yes |
| `MOMRULE_HIST` | yes | yes | yes | yes | yes |
| `POPLOC` | yes | yes | yes | yes | yes |
| `POPRULE_HIST` | yes | yes | yes | yes | yes |
| `RELATE` | yes | yes | yes | yes | yes |
| `RELATED` | yes | yes | yes | yes | yes |
| `SEX` | yes | yes | yes | yes | yes |
| `AGE` | yes | yes | yes | yes | yes |
| `BIRTHYR` | yes | yes | yes | yes | yes |
| `RACE` | yes | yes | yes | yes | yes |
| `RACED` | yes | yes | yes | yes | yes |
| `BPL` | yes | yes | yes | yes | yes |
| `BPLD` | yes | yes | yes | yes | yes |
| `SCHOOL` | yes | yes | yes | yes | yes |
| `HIGRADE` | — | — | — | — | yes |
| `HIGRADED` | — | — | — | — | yes |
| `EDUC` | — | — | — | — | yes |
| `EDUCD` | — | — | — | — | yes |
| `LIT` | yes | yes | yes | yes | — |
| `EMPSTAT` | — | yes | — | yes | yes |
| `EMPSTATD` | — | yes | — | yes | yes |
| `LABFORCE` | — | yes | yes | yes | yes |
| `CLASSWKR` | — | yes | yes | yes | yes |
| `CLASSWKRD` | — | yes | yes | yes | yes |
| `OCC1950` | yes | yes | yes | yes | yes |
| `IND1950` | yes | yes | yes | yes | yes |
| `WKSWORK1` | — | — | — | — | yes |
| `HRSWORK1` | — | — | — | — | yes |
| `INCWAGE` | — | — | — | — | yes |
| `OCCSCORE` | yes | yes | yes | yes | yes |
| `SEI` | yes | yes | yes | yes | yes |
| `PRESGL` | yes | yes | yes | yes | yes |
| `ERSCOR50` | yes | yes | yes | yes | yes |
| `EDSCOR50` | yes | yes | yes | yes | yes |
| `MIGRATE5` | — | — | — | — | yes |
| `MIGRATE5D` | — | — | — | — | yes |
| `MIGPLAC5` | — | — | — | — | yes |
| `MIGCOUNTYICP5` | — | — | — | — | yes |
| `VERSIONHIST` | yes | yes | yes | yes | yes |
| `HISTID` | yes | yes | yes | yes | yes |
| `QAGE` | yes | yes | yes | yes | yes |
| `QSCHOOL` | yes | yes | yes | yes | yes |
| `HIK` | yes | yes | yes | yes | yes |
| `LINK1900` | yes | yes | yes | yes | yes |
| `LINK1910` | yes | yes | yes | yes | yes |
| `LINK1920` | yes | yes | yes | yes | yes |
| `LINK1930` | yes | yes | yes | yes | yes |
| `LINK1940` | yes | yes | yes | yes | yes |
| `VERSIONHIK` | yes | yes | yes | yes | yes |

Grouped:

- **All five years** (41): `AGE`, `BIRTHYR`, `BPL`, `BPLD`, `COUNTYICP`, `EDSCOR50`, `ERSCOR50`, `FARM`, `GQ`, `HIK`, `HISTID`, `IND1950`, `LINK1900`, `LINK1910`, `LINK1920`, `LINK1930`, `LINK1940`, `MOMLOC`, `MOMRULE_HIST`, `OCC1950`, `OCCSCORE`, `PERNUM`, `POPLOC`, `POPRULE_HIST`, `PRESGL`, `QAGE`, `QSCHOOL`, `RACE`, `RACED`, `RELATE`, `RELATED`, `SCHOOL`, `SEI`, `SERIAL`, `SEX`, `STATEFIP`, `STATEICP`, `URBAN`, `VERSIONHIK`, `VERSIONHIST`, `YEAR`
- **1900, 1910, 1920, 1930 only** (1): `LIT`
- **1910, 1920, 1930, 1940 only** (3): `CLASSWKR`, `CLASSWKRD`, `LABFORCE`
- **1910, 1930, 1940 only** (2): `EMPSTAT`, `EMPSTATD`
- **1940 only** (11): `EDUC`, `EDUCD`, `HIGRADE`, `HIGRADED`, `HRSWORK1`, `INCWAGE`, `MIGCOUNTYICP5`, `MIGPLAC5`, `MIGRATE5`, `MIGRATE5D`, `WKSWORK1`

The three that matter most for the mandatory-schooling work: attainment (`HIGRADE`, `HIGRADED`, `EDUC`, `EDUCD`) exists **only in 1940**; literacy (`LIT`) runs to **1930 and stops**, so the two never overlap; and `SCHOOL` is present in every year but its reference period changes from ~12 months in 1900 to ~1 month in 1940, so it is not comparable across years untreated.

## Reading the table

`Fill` is the share of that year's rows with a non-empty value. **Empty means the question was not asked that year**, which is the single most important thing to check before using a variable: attainment (`HIGRADE`, `HIGRADED`, `EDUC`, `EDUCD`) exists only from 1940, while literacy (`LIT`) runs to 1930 and stops. `SCHOOL` appears in every year but its reference period changes from ~12 months in 1900 to ~1 month in 1940 and is not comparable across years untreated.

## Identification

### `YEAR` — Census year

Census year of the record. This extract stacks 1900-1940; one row per person per census in which they appear.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | 1,900 | 1,900 | `1900` |
| 1910 | 100.0% | 1 | 1,910 | 1,910 | `1910` |
| 1920 | 100.0% | 1 | 1,920 | 1,920 | `1920` |
| 1930 | 100.0% | 1 | 1,930 | 1,930 | `1930` |
| 1940 | 100.0% | 1 | 1,940 | 1,940 | `1940` |

### `SERIAL` — Household serial number

Unique within YEAR only. Household key is (YEAR SERIAL).

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1910 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1920 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1930 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1940 | 100.0% | ~1 per row | - | - | *(identifier)* |

### `PERNUM` — Person number in household

Sequence within (YEAR SERIAL). Person key is (YEAR SERIAL PERNUM).

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 60 | 1 | 60 | `3`, `2`, `4` |
| 1910 | 100.0% | 60 | 1 | 60 | `2`, `1`, `3` |
| 1920 | 100.0% | 60 | 1 | 60 | `2`, `1`, `3` |
| 1930 | 100.0% | 60 | 1 | 60 | `2`, `1`, `3` |
| 1940 | 100.0% | 60 | 1 | 60 | `1`, `2`, `3` |

## Geography

### `STATEICP` — State (ICPSR code)

ICPSR state code. Differs from STATEFIP; Alabama is 41 in ICPSR and 1 in FIPS.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 49 | 1 | 98 | `14`, `13`, `21` |
| 1910 | 100.0% | 49 | 1 | 98 | `13`, `14`, `21` |
| 1920 | 100.0% | 49 | 1 | 98 | `13`, `14`, `21` |
| 1930 | 100.0% | 49 | 1 | 98 | `13`, `14`, `21` |
| 1940 | 100.0% | 49 | 1 | 98 | `13`, `14`, `21` |

### `STATEFIP` — State (FIPS code)

Standard 2-digit state FIPS. Use this for GEOIDs.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 49 | 1 | 56 | `42`, `36`, `17` |
| 1910 | 100.0% | 49 | 1 | 56 | `36`, `42`, `17` |
| 1920 | 100.0% | 49 | 1 | 56 | `36`, `42`, `17` |
| 1930 | 100.0% | 49 | 1 | 56 | `36`, `42`, `17` |
| 1940 | 100.0% | 49 | 1 | 56 | `36`, `42`, `17` |

### `COUNTYICP` — County of residence (ICPSR code)

ICPSR county code = 3-digit FIPS county code times 10. GEOID = STATEFIP*1000 + COUNTYICP/10. Do NOT join raw against a 5-digit GEOID.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 278 | 10 | 9,270 | `310`, `610`, `30` |
| 1910 | 100.0% | 279 | 10 | 8,400 | `310`, `610`, `30` |
| 1920 | 100.0% | 280 | 10 | 8,400 | `310`, `30`, `610` |
| 1930 | 100.0% | 282 | 10 | 8,400 | `310`, `30`, `470` |
| 1940 | 100.0% | 282 | 10 | 8,400 | `310`, `30`, `370` |

### `URBAN` — Urban/rural status

1 = rural; 2 = urban.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1910 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1920 | 100.0% | 2 | 1 | 2 | `2`, `1` |
| 1930 | 100.0% | 2 | 1 | 2 | `2`, `1` |
| 1940 | 100.0% | 2 | 1 | 2 | `2`, `1` |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | Rural |
| 2 | Urban |

## Household

### `GQ` — Group quarters status

1 = household; 2 = additional household; 3-4 = group quarters; 5 = additional GQ. Values above 2 indicate institutional or collective living.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 4 | 1 | 4 | `1`, `2`, `4` |
| 1910 | 100.0% | 3 | 1 | 4 | `1`, `2`, `4` |
| 1920 | 100.0% | 4 | 1 | 4 | `1`, `4`, `2` |
| 1930 | 100.0% | 4 | 1 | 4 | `1`, `2`, `4` |
| 1940 | 100.0% | 5 | 1 | 6 | `1`, `4`, `3` |

| Code | Meaning |
|---|---|
| 0 | Vacant unit |
| 1 | Household |
| 2 | Additional household |
| 3 | Group quarters |
| 4 | Fragment |
| 5 | Additional group quarters |

### `FARM` — Farm status

1 = non-farm; 2 = farm.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1910 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1920 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1930 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1940 | 100.0% | 2 | 1 | 2 | `1`, `2` |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | Non-farm |
| 2 | Farm |

## Family Pointer

### `MOMLOC` — Mother's location in household

PERNUM of the person's mother in the same household; 0 = mother not present.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 59 | 0 | 59 | `2`, `0`, `1` |
| 1910 | 100.0% | 59 | 0 | 58 | `0`, `2`, `1` |
| 1920 | 100.0% | 59 | 0 | 58 | `0`, `2`, `1` |
| 1930 | 100.0% | 59 | 0 | 59 | `0`, `2`, `1` |
| 1940 | 100.0% | 59 | 0 | 58 | `0`, `2`, `1` |

### `MOMRULE_HIST` — Rule used to link mother

Records which matching rule attached MOMLOC. 0 = no link.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 8 | 0 | 7 | `1`, `0`, `2` |
| 1910 | 100.0% | 8 | 0 | 7 | `1`, `0`, `2` |
| 1920 | 100.0% | 6 | 0 | 7 | `0`, `1`, `2` |
| 1930 | 100.0% | 6 | 0 | 7 | `0`, `1`, `2` |
| 1940 | 100.0% | 8 | 0 | 7 | `0`, `1`, `2` |

### `POPLOC` — Father's location in household

PERNUM of the person's father in the same household; 0 = father not present.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 56 | 0 | 58 | `1`, `0`, `3` |
| 1910 | 100.0% | 59 | 0 | 60 | `0`, `1`, `3` |
| 1920 | 100.0% | 57 | 0 | 57 | `0`, `1`, `3` |
| 1930 | 100.0% | 56 | 0 | 55 | `0`, `1`, `3` |
| 1940 | 100.0% | 60 | 0 | 59 | `0`, `1`, `3` |

### `POPRULE_HIST` — Rule used to link father

Records which matching rule attached POPLOC. 0 = no link.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 6 | 0 | 7 | `1`, `0`, `2` |
| 1910 | 100.0% | 6 | 0 | 7 | `0`, `1`, `3` |
| 1920 | 100.0% | 6 | 0 | 7 | `0`, `1`, `2` |
| 1930 | 100.0% | 6 | 0 | 7 | `0`, `1`, `2` |
| 1940 | 100.0% | 6 | 0 | 7 | `0`, `1`, `2` |

## Family

### `RELATE` — Relationship to household head (general)

1 = head; 2 = spouse; 3 = child; 4 = child-in-law; 5 = parent; 6 = parent-in-law; 7 = sibling; 8 = sibling-in-law; 9 = grandchild; 10 = other relative; 11 = partner/friend/visitor; 12 = other non-relative; 13 = institutional inmates.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 13 | 1 | 13 | `3`, `2`, `1` |
| 1910 | 100.0% | 13 | 1 | 13 | `3`, `2`, `1` |
| 1920 | 100.0% | 13 | 1 | 13 | `3`, `2`, `1` |
| 1930 | 100.0% | 13 | 1 | 13 | `3`, `1`, `2` |
| 1940 | 100.0% | 13 | 1 | 13 | `3`, `1`, `2` |

| Code | Meaning |
|---|---|
| 1 | Head/householder |
| 2 | Spouse |
| 3 | Child |
| 4 | Child-in-law |
| 5 | Parent |
| 6 | Parent-in-law |
| 7 | Sibling |
| 8 | Sibling-in-law |
| 9 | Grandchild |
| 10 | Other relative |
| 11 | Partner/friend/visitor |
| 12 | Other non-relative |
| 13 | Institutional inmate |

### `RELATED` — Relationship to household head (detailed)

4-digit elaboration of RELATE.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 61 | 101 | 1,301 | `301`, `201`, `101` |
| 1910 | 100.0% | 62 | 101 | 1,301 | `301`, `201`, `101` |
| 1920 | 100.0% | 60 | 101 | 1,301 | `301`, `201`, `101` |
| 1930 | 100.0% | 62 | 101 | 1,301 | `301`, `101`, `201` |
| 1940 | 100.0% | 62 | 101 | 1,301 | `301`, `101`, `201` |

## Demographics

### `SEX` — Sex

1 = male; 2 = female.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1910 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1920 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1930 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1940 | 100.0% | 2 | 1 | 2 | `1`, `2` |

| Code | Meaning |
|---|---|
| 1 | Male |
| 2 | Female |

### `AGE` — Age in years

Age at the census date. 1940 was enumerated 1 April, so most respondents had not yet had their birthday that year.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 120 | 0 | 125 | `2`, `3`, `4` |
| 1910 | 100.0% | 82 | 0 | 83 | `2`, `0`, `3` |
| 1920 | 100.0% | 92 | 0 | 91 | `0`, `1`, `2` |
| 1930 | 100.0% | 107 | 0 | 109 | `5`, `6`, `3` |
| 1940 | 100.0% | 117 | 0 | 119 | `16`, `15`, `14` |

### `BIRTHYR` — Year of birth

Self-reported in 1900, the only census that asked date of birth. Derived in other years. Check against 1940-AGE-1 before using as a cohort key.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 72 | 1,829 | 9,999 | `1898`, `1896`, `1897` |
| 1910 | 100.0% | 82 | 1,827 | 1,910 | `1908`, `1910`, `1907` |
| 1920 | 100.0% | 92 | 1,829 | 1,920 | `1920`, `1919`, `1918` |
| 1930 | 100.0% | 107 | 1,821 | 1,930 | `1925`, `1924`, `1927` |
| 1940 | 100.0% | 117 | 1,821 | 1,940 | `1924`, `1925`, `1926` |

### `RACE` — Race (general)

1 = White; 2 = Black/African American; 3 = American Indian/Alaska Native; 4 = Chinese; 5 = Japanese; 6 = other Asian/Pacific Islander; 7 = other; 8-9 = two or more major races.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 5 | 1 | 5 | `1`, `2`, `3` |
| 1910 | 100.0% | 6 | 1 | 6 | `1`, `2`, `3` |
| 1920 | 100.0% | 6 | 1 | 6 | `1`, `2`, `3` |
| 1930 | 100.0% | 6 | 1 | 6 | `1`, `2`, `3` |
| 1940 | 100.0% | 7 | 1 | 7 | `1`, `2`, `3` |

| Code | Meaning |
|---|---|
| 1 | White |
| 2 | Black/African American |
| 3 | American Indian/Alaska Native |
| 4 | Chinese |
| 5 | Japanese |
| 6 | Other Asian or Pacific Islander |
| 7 | Other race |
| 8 | Two major races |
| 9 | Three or more major races |

### `RACED` — Race (detailed)

3-digit elaboration of RACE; 100 = White; 200 = Black.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 5 | 100 | 500 | `100`, `200`, `300` |
| 1910 | 100.0% | 9 | 100 | 672 | `100`, `200`, `210` |
| 1920 | 100.0% | 10 | 100 | 632 | `100`, `200`, `210` |
| 1930 | 100.0% | 10 | 100 | 630 | `100`, `200`, `140` |
| 1940 | 100.0% | 25 | 100 | 700 | `100`, `200`, `300` |

## Birthplace

### `BPL` — Birthplace (general)

State or country of birth. US states carry their FIPS state code (1 = Alabama, 6 = California, 36 = New York); codes above 99 are foreign countries. There is NO county of birth in any US census.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 121 | 1 | 900 | `42`, `36`, `17` |
| 1910 | 100.0% | 125 | 1 | 900 | `42`, `36`, `17` |
| 1920 | 100.0% | 140 | 1 | 900 | `42`, `36`, `17` |
| 1930 | 100.0% | 141 | 1 | 900 | `36`, `42`, `17` |
| 1940 | 100.0% | 143 | 1 | 900 | `36`, `42`, `17` |

| Code | Meaning |
|---|---|
| 1 | Alabama |
| 6 | California |
| 13 | Georgia |
| 25 | Massachusetts |
| 36 | New York |
| 42 | Pennsylvania |
| 47 | Tennessee |

*US states carry their FIPS state code; codes above 99 are foreign countries.*

### `BPLD` — Birthplace (detailed)

5-digit elaboration, generally BPL*100. Adds territorial-era distinctions (e.g. Indian Territory vs Oklahoma), never sub-state geography.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 228 | 100 | 90,022 | `4200`, `3600`, `1700` |
| 1910 | 100.0% | 265 | 100 | 90,021 | `4200`, `3600`, `1700` |
| 1920 | 100.0% | 355 | 100 | 90,022 | `4200`, `3600`, `1700` |
| 1930 | 100.0% | 371 | 100 | 90,022 | `3600`, `4200`, `1700` |
| 1940 | 100.0% | 412 | 100 | 90,022 | `3600`, `4200`, `1700` |

## Education

### `SCHOOL` — School attendance

1 = did not attend; 2 = attended. Available 1850-1940 but the reference period changes drastically: ~12 months in 1900, ~7.5 in 1910, ~4 in 1920 (enumerated 1 January), ~6 in 1930, ~1 month in 1940. Not comparable across years untreated.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 3 | 1 | 8 | `1`, `2`, `8` |
| 1910 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1920 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1930 | 100.0% | 2 | 1 | 2 | `1`, `2` |
| 1940 | 100.0% | 2 | 1 | 2 | `1`, `2` |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | Did not attend |
| 2 | Attended |
| 9 | Missing |

### `HIGRADE` — Highest grade attended (general)

1940 onward only. The census recorded highest grade completed; there is no years-of-schooling variable.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 23 | 1 | 99 | `11`, `15`, `10` |

*Grade index. yrs_sch is constructed from HIGRADED, not from this.*

### `HIGRADED` — Highest grade attended (detailed)

1940 onward only. Codes are multiples of 10; divide by 10 to get the grade index. 999 = N/A. Years of schooling is CONSTRUCTED: g = HIGRADED %/% 10, then 0 if g<=3, g-3 if 4<=g<=20, 17 if 21<=g<=23.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 23 | 10 | 999 | `110`, `150`, `100` |

### `EDUC` — Educational attainment (general)

1940 onward only. IPUMS recode of HIGRADE into attainment bins; different bin edges from a raw year count.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 13 | 0 | 99 | `2`, `6`, `1` |

*IPUMS attainment recode; different bin edges from a raw year count.*

### `EDUCD` — Educational attainment (detailed)

1940 onward only. Detailed elaboration of EDUC.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 23 | 2 | 999 | `26`, `60`, `25` |

### `LIT` — Literacy

Ability to read and write. The census asked this 1840-1930 and dropped it afterwards, so it is absent in 1940 exactly where HIGRADE begins.

**Available:** 1900, 1910, 1920, 1930 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 5 | 0 | 4 | `4`, `0`, `1` |
| 1910 | 100.0% | 5 | 0 | 4 | `4`, `0`, `1` |
| 1920 | 100.0% | 5 | 0 | 4 | `4`, `0`, `1` |
| 1930 | 100.0% | 3 | 0 | 4 | `4`, `0`, `1` |
| 1940 | — | | | | |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | No, illiterate |
| 2 | Cannot read or write |
| 3 | Cannot read |
| 4 | Yes, literate |

## Labour

### `EMPSTAT` — Employment status (general)

1 = employed; 2 = unemployed; 3 = not in labour force; 0 = N/A.

**Available:** 1910, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 3 | 1 | 3 | `3`, `1`, `2` |
| 1920 | — | | | | |
| 1930 | 100.0% | 3 | 1 | 3 | `3`, `1`, `2` |
| 1940 | 100.0% | 4 | 0 | 3 | `1`, `3`, `0` |

### `EMPSTATD` — Employment status (detailed)

2-digit elaboration of EMPSTAT.

**Available:** 1910, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 4 | 10 | 30 | `30`, `10`, `20` |
| 1920 | — | | | | |
| 1930 | 100.0% | 4 | 10 | 30 | `30`, `10`, `20` |
| 1940 | 100.0% | 12 | 0 | 34 | `10`, `31`, `33` |

### `LABFORCE` — Labour force status

1 = not in labour force; 2 = in labour force; 0 = N/A.

**Available:** 1910, 1920, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 3 | 0 | 2 | `0`, `2`, `1` |
| 1920 | 100.0% | 3 | 0 | 2 | `2`, `0`, `1` |
| 1930 | 100.0% | 3 | 0 | 2 | `0`, `2`, `1` |
| 1940 | 100.0% | 3 | 0 | 2 | `2`, `1`, `0` |

### `CLASSWKR` — Class of worker (general)

1 = self-employed; 2 = works for wages; 0 = N/A.

**Available:** 1910, 1920, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 3 | 0 | 2 | `0`, `2`, `1` |
| 1920 | 100.0% | 3 | 0 | 2 | `0`, `2`, `1` |
| 1930 | 100.0% | 3 | 0 | 2 | `0`, `2`, `1` |
| 1940 | 100.0% | 4 | 0 | 9 | `0`, `2`, `1` |

### `CLASSWKRD` — Class of worker (detailed)

2-digit elaboration of CLASSWKR.

**Available:** 1910, 1920, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 4 | 0 | 20 | `0`, `20`, `12` |
| 1920 | 100.0% | 4 | 0 | 20 | `0`, `20`, `12` |
| 1930 | 100.0% | 5 | 0 | 29 | `0`, `20`, `12` |
| 1940 | 100.0% | 8 | 0 | 98 | `0`, `22`, `12` |

### `OCC1950` — Occupation (1950 basis)

Occupation recoded to the 1950 classification for cross-year comparability. Manual non-farm groups: craftsmen 500-595, operatives 600-690, labourers 910-970; service 700-790 and farm 100/123/810-840 are usually kept separate. 999 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 261 | 0 | 999 | `999`, `983`, `820` |
| 1910 | 100.0% | 271 | 0 | 999 | `999`, `100`, `820` |
| 1920 | 100.0% | 274 | 0 | 999 | `999`, `100`, `820` |
| 1930 | 100.0% | 275 | 0 | 999 | `999`, `100`, `970` |
| 1940 | 100.0% | 223 | 1 | 999 | `999`, `100`, `690` |

*Manual non-farm groups: craftsmen 500-595, operatives 600-690, labourers 910-970. 999 = N/A.*

### `IND1950` — Industry (1950 basis)

Industry recoded to the 1950 classification. 0 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 147 | 0 | 997 | `0`, `105`, `997` |
| 1910 | 100.0% | 154 | 0 | 997 | `0`, `105`, `246` |
| 1920 | 100.0% | 151 | 0 | 997 | `0`, `105`, `506` |
| 1930 | 100.0% | 151 | 0 | 997 | `0`, `105`, `246` |
| 1940 | 100.0% | 128 | 0 | 997 | `0`, `105`, `246` |

### `WKSWORK1` — Weeks worked last year

1940 onward. Reported weeks; 0 = none or N/A.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 53 | 0 | 52 | `0`, `52`, `50` |

### `HRSWORK1` — Hours worked last week

1940 onward. Reported hours; 0 = none or N/A.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 99 | 0 | 98 | `0`, `40`, `48` |

### `INCWAGE` — Wage and salary income

1940 onward, for the prior calendar year (1939). Top-coded; 999999 = N/A.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | >4000 | 0 | 999,999 | `0`, `999999`, `999998` |

### `OCCSCORE` — Occupational income score

Constructed score assigned to OCC1950; 0 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 51 | 0 | 80 | `0`, `9`, `14` |
| 1910 | 100.0% | 51 | 0 | 80 | `0`, `14`, `9` |
| 1920 | 100.0% | 52 | 0 | 80 | `0`, `14`, `9` |
| 1930 | 100.0% | 52 | 0 | 80 | `0`, `14`, `20` |
| 1940 | 100.0% | 50 | 0 | 80 | `0`, `14`, `23` |

### `SEI` — Duncan socioeconomic index

Constructed index assigned to OCC1950; 0 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 84 | 0 | 96 | `0`, `6`, `14` |
| 1910 | 100.0% | 86 | 0 | 96 | `0`, `14`, `6` |
| 1920 | 100.0% | 87 | 0 | 96 | `0`, `14`, `6` |
| 1930 | 100.0% | 87 | 0 | 96 | `0`, `14`, `8` |
| 1940 | 100.0% | 83 | 0 | 96 | `0`, `14`, `18` |

### `PRESGL` — Siegel occupational prestige score

Constructed score with one implied decimal; 999.9 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 188 | 0 | 81.5 | `0.0`, `18.4`, `40.7` |
| 1910 | 100.0% | 196 | 0 | 81.5 | `0.0`, `40.7`, `18.4` |
| 1920 | 100.0% | 197 | 0 | 81.5 | `0.0`, `40.7`, `18.4` |
| 1930 | 100.0% | 197 | 0 | 81.5 | `0.0`, `40.7`, `17.5` |
| 1940 | 100.0% | 170 | 0 | 81.5 | `0.0`, `40.7`, `28.4` |

### `ERSCOR50` — Occupational earnings score (1950 basis)

Constructed score with one implied decimal; 999.9 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 168 | 0.8 | 999.9 | `999.9`, `4.8`, `9.9` |
| 1910 | 100.0% | 174 | 0.6 | 999.9 | `999.9`, `9.9`, `4.8` |
| 1920 | 100.0% | 174 | 0.6 | 999.9 | `999.9`, `9.9`, `4.8` |
| 1930 | 100.0% | 174 | 0.6 | 999.9 | `999.9`, `9.9`, `39.7` |
| 1940 | 100.0% | 155 | 0.6 | 999.9 | `999.9`, `9.9`, `41.4` |

### `EDSCOR50` — Occupational education score (1950 basis)

Constructed score with one implied decimal; 999.9 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 171 | 0 | 999.9 | `999.9`, `2.8`, `4.6` |
| 1910 | 100.0% | 178 | 0 | 999.9 | `999.9`, `4.6`, `2.8` |
| 1920 | 100.0% | 179 | 0 | 999.9 | `999.9`, `4.6`, `2.8` |
| 1930 | 100.0% | 179 | 0 | 999.9 | `999.9`, `4.6`, `3.0` |
| 1940 | 100.0% | 154 | 0 | 999.9 | `999.9`, `4.6`, `3.2` |

## Migration

### `MIGRATE5` — Migration status 5 years ago (general)

1940 only. 1 = same house; 2 = same county/different house; 3 = different county same state; 4 = different state; 0 = N/A (under 5); 9 = unknown.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 5 | 1 | 9 | `1`, `2`, `3` |

| Code | Meaning |
|---|---|
| 0 | N/A (under 5) |
| 1 | Same house |
| 2 | Same county, different house |
| 3 | Different county, same state |
| 4 | Different state or abroad |
| 9 | Unknown |

### `MIGRATE5D` — Migration status 5 years ago (detailed)

1940 only. 2-digit elaboration of MIGRATE5.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 9 | 10 | 90 | `10`, `21`, `22` |

### `MIGPLAC5` — State or country of residence 5 years ago

1940 only. Residence in 1935. 990 = same state; 999 = unknown.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 122 | 1 | 999 | `990`, `36`, `17` |

### `MIGCOUNTYICP5` — County of residence 5 years ago (ICPSR)

1940 only. County of residence in 1935, same FIPS-times-10 coding as COUNTYICP. 9999 = unknown or N/A. This is residence, not birthplace.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 286 | 10 | 9,999 | `9999`, `310`, `10` |

## Linkage

### `VERSIONHIST` — HISTID version

Version of the HISTID assignment this record carries.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | 2 | 2 | `2` |
| 1910 | 100.0% | 1 | 2 | 2 | `2` |
| 1920 | 100.0% | 1 | 2 | 2 | `2` |
| 1930 | 100.0% | 1 | 2 | 2 | `2` |
| 1940 | 100.0% | 1 | 3 | 3 | `3` |

### `HISTID` — Historical unique identifier

36-character UUID identifying a person RECORD within a census year. Stable across IPUMS data versions, NOT shared by the same person across census years.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1910 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1920 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1930 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1940 | 100.0% | ~1 per row | - | - | *(identifier)* |

### `HIK` — Historical Index Key

21-character identifier that IS shared by the same person across census years. This is the cross-year person key; HISTID is not. Assigned only to persons the MLP could link.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1910 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1920 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1930 | 100.0% | ~1 per row | - | - | *(identifier)* |
| 1940 | 100.0% | ~1 per row | - | - | *(identifier)* |

### `LINK1900` — Links to the 1900 census

1 = this person has a linked record in 1900; 0 = not linked.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | 1 | 1 | `1` |
| 1910 | 100.0% | 2 | 0 | 1 | `0`, `1` |
| 1920 | 100.0% | 2 | 0 | 1 | `0`, `1` |
| 1930 | 100.0% | 2 | 0 | 1 | `0`, `1` |
| 1940 | 100.0% | 2 | 0 | 1 | `0`, `1` |

| Code | Meaning |
|---|---|
| 0 | No linked 1900 record |
| 1 | Links to 1900 |

### `LINK1910` — Links to the 1910 census

1 = this person has a linked record in 1910; 0 = not linked.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 0 | 1 | `1`, `0` |
| 1910 | 100.0% | 1 | 1 | 1 | `1` |
| 1920 | 100.0% | 2 | 0 | 1 | `1`, `0` |
| 1930 | 100.0% | 2 | 0 | 1 | `0`, `1` |
| 1940 | 100.0% | 2 | 0 | 1 | `0`, `1` |

| Code | Meaning |
|---|---|
| 0 | No linked 1910 record |
| 1 | Links to 1910 |

### `LINK1920` — Links to the 1920 census

1 = this person has a linked record in 1920; 0 = not linked.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 0 | 1 | `1`, `0` |
| 1910 | 100.0% | 2 | 0 | 1 | `1`, `0` |
| 1920 | 100.0% | 1 | 1 | 1 | `1` |
| 1930 | 100.0% | 2 | 0 | 1 | `1`, `0` |
| 1940 | 100.0% | 2 | 0 | 1 | `1`, `0` |

| Code | Meaning |
|---|---|
| 0 | No linked 1920 record |
| 1 | Links to 1920 |

### `LINK1930` — Links to the 1930 census

1 = this person has a linked record in 1930; 0 = not linked.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 0 | 1 | `1`, `0` |
| 1910 | 100.0% | 2 | 0 | 1 | `1`, `0` |
| 1920 | 100.0% | 2 | 0 | 1 | `1`, `0` |
| 1930 | 100.0% | 1 | 1 | 1 | `1` |
| 1940 | 100.0% | 2 | 0 | 1 | `1`, `0` |

| Code | Meaning |
|---|---|
| 0 | No linked 1930 record |
| 1 | Links to 1930 |

### `LINK1940` — Links to the 1940 census

1 = this person has a linked record in 1940; 0 = not linked.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | 1 | 1 | `1` |
| 1910 | 100.0% | 1 | 1 | 1 | `1` |
| 1920 | 100.0% | 1 | 1 | 1 | `1` |
| 1930 | 100.0% | 1 | 1 | 1 | `1` |
| 1940 | 100.0% | 1 | 1 | 1 | `1` |

| Code | Meaning |
|---|---|
| 0 | No linked 1940 record |
| 1 | Links to 1940 |

### `VERSIONHIK` — HIK version

Version of the MLP linking that produced HIK, e.g. "2.0-2.0".

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | - | - | `2.0-2.0` |
| 1910 | 100.0% | 1 | - | - | `2.0-2.0` |
| 1920 | 100.0% | 1 | - | - | `2.0-2.0` |
| 1930 | 100.0% | 1 | - | - | `2.0-2.0` |
| 1940 | 100.0% | 1 | - | - | `3.0-2.0` |

## Quality

### `QAGE` — Data quality flag for AGE

0 = reported; non-zero = allocated or edited. The EXT1940USCB full-count extract does not carry these flags.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 0 | 4 | `0`, `4` |
| 1910 | 100.0% | 2 | 0 | 4 | `0`, `4` |
| 1920 | 100.0% | 2 | 0 | 4 | `0`, `4` |
| 1930 | 100.0% | 2 | 0 | 4 | `0`, `4` |
| 1940 | 100.0% | 2 | 0 | 4 | `0`, `4` |

### `QSCHOOL` — Data quality flag for SCHOOL

0 = reported; non-zero = allocated or edited.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | 0 | 0 | `0` |
| 1910 | 100.0% | 2 | 0 | 4 | `0`, `4` |
| 1920 | 100.0% | 1 | 0 | 0 | `0` |
| 1930 | 100.0% | 2 | 0 | 4 | `0`, `4` |
| 1940 | 100.0% | 2 | 0 | 4 | `0`, `4` |


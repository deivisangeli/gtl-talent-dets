# IPUMS MLP linked census extract — variable dictionary

Generated 2026-09-20 by `prep/mandatory_schooling/profile_ipums_linked_census.py`.

Source: `C:\Users\megaj\OneDrive\Documentos\usa_00001.csv.gz` — 13.08 GB gzipped, **220,979,023 rows**, 58 columns. Profiled in 16 minutes.

Value labels and the label cross-check come from IPUMS' DDI codebook `usa_00001.xml` (sha256 `44e7efb67c2f86b2…`, 58 variables). Variable labels and coding notes are the project's own, in `ipums_variable_labels.csv`, checked against it.

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

Which census years each variable is populated in. **Empty means the question was not asked that year**, not that the value is zero.

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
| `HIGRADE` | --- | --- | --- | --- | yes |
| `HIGRADED` | --- | --- | --- | --- | yes |
| `EDUC` | --- | --- | --- | --- | yes |
| `EDUCD` | --- | --- | --- | --- | yes |
| `LIT` | yes | yes | yes | yes | --- |
| `EMPSTAT` | --- | yes | --- | yes | yes |
| `EMPSTATD` | --- | yes | --- | yes | yes |
| `LABFORCE` | --- | yes | yes | yes | yes |
| `CLASSWKR` | --- | yes | yes | yes | yes |
| `CLASSWKRD` | --- | yes | yes | yes | yes |
| `OCC1950` | yes | yes | yes | yes | yes |
| `IND1950` | yes | yes | yes | yes | yes |
| `WKSWORK1` | --- | --- | --- | --- | yes |
| `HRSWORK1` | --- | --- | --- | --- | yes |
| `INCWAGE` | --- | --- | --- | --- | yes |
| `OCCSCORE` | yes | yes | yes | yes | yes |
| `SEI` | yes | yes | yes | yes | yes |
| `PRESGL` | yes | yes | yes | yes | yes |
| `ERSCOR50` | yes | yes | yes | yes | yes |
| `EDSCOR50` | yes | yes | yes | yes | yes |
| `MIGRATE5` | --- | --- | --- | --- | yes |
| `MIGRATE5D` | --- | --- | --- | --- | yes |
| `MIGPLAC5` | --- | --- | --- | --- | yes |
| `MIGCOUNTYICP5` | --- | --- | --- | --- | yes |
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

## Reading the table

`Fill` is the share of that year's rows with a non-empty value. **Empty means the question was not asked that year**, which is the single most important thing to check before using a variable: attainment (`HIGRADE`, `HIGRADED`, `EDUC`, `EDUCD`) exists only from 1940, while literacy (`LIT`) runs to 1930 and stops. `SCHOOL` appears in every year but its reference period changes from ~12 months in 1900 to ~1 month in 1940 and is not comparable across years untreated.

## Identification

### `YEAR` — Census year

Census year of the record. This extract stacks 1900-1940; one row per person per census in which they appear.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | 1,900 | 1,900 | `1900` 1900 |
| 1910 | 100.0% | 1 | 1,910 | 1,910 | `1910` 1910 |
| 1920 | 100.0% | 1 | 1,920 | 1,920 | `1920` 1920 |
| 1930 | 100.0% | 1 | 1,930 | 1,930 | `1930` 1930 |
| 1940 | 100.0% | 1 | 1,940 | 1,940 | `1940` 1940 |

| Code | Meaning |
|---|---|
| 1850 | 1850 |
| 1860 | 1860 |
| 1870 | 1870 |
| 1880 | 1880 |
| 1900 | 1900 |
| 1910 | 1910 |
| 1920 | 1920 |
| 1930 | 1930 |
| 1940 | 1940 |
| 1950 | 1950 |
| 1960 | 1960 |
| 1970 | 1970 |
| 1980 | 1980 |
| 1990 | 1990 |
| 2000 | 2000 |
| 2001 | 2001 |
| 2002 | 2002 |
| 2003 | 2003 |
| 2004 | 2004 |
| 2005 | 2005 |
| 2006 | 2006 |
| 2007 | 2007 |
| 2008 | 2008 |
| 2009 | 2009 |
| 2010 | 2010 |
| 2011 | 2011 |
| 2012 | 2012 |
| 2013 | 2013 |
| 2014 | 2014 |
| 2015 | 2015 |
| 2016 | 2016 |
| 2017 | 2017 |
| 2018 | 2018 |
| 2019 | 2019 |
| 2020 | 2020 |
| 2021 | 2021 |
| 2022 | 2022 |
| 2023 | 2023 |
| 2024 | 2024 |

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

### `PERNUM` — Person number in sample unit

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
| 1900 | 100.0% | 49 | 1 | 98 | `14` Pennsylvania, `13` New York, `21` Illinois |
| 1910 | 100.0% | 49 | 1 | 98 | `13` New York, `14` Pennsylvania, `21` Illinois |
| 1920 | 100.0% | 49 | 1 | 98 | `13` New York, `14` Pennsylvania, `21` Illinois |
| 1930 | 100.0% | 49 | 1 | 98 | `13` New York, `14` Pennsylvania, `21` Illinois |
| 1940 | 100.0% | 49 | 1 | 98 | `13` New York, `14` Pennsylvania, `21` Illinois |

| Code | Meaning |
|---|---|
| 03 | Massachusetts |
| 13 | New York |
| 14 | Pennsylvania |
| 21 | Illinois |
| 22 | Indiana |
| 23 | Michigan |
| 24 | Ohio |
| 31 | Iowa |
| 34 | Missouri |
| 49 | Texas |
| 71 | California |

*55 codes in the codebook; the 11 shown are those most common in this extract. See the DDI for the full list.*

### `STATEFIP` — State (FIPS code)

Standard 2-digit state FIPS. Use this for GEOIDs.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 49 | 1 | 56 | `42` Pennsylvania, `36` New York, `17` Illinois |
| 1910 | 100.0% | 49 | 1 | 56 | `36` New York, `42` Pennsylvania, `17` Illinois |
| 1920 | 100.0% | 49 | 1 | 56 | `36` New York, `42` Pennsylvania, `17` Illinois |
| 1930 | 100.0% | 49 | 1 | 56 | `36` New York, `42` Pennsylvania, `17` Illinois |
| 1940 | 100.0% | 49 | 1 | 56 | `36` New York, `42` Pennsylvania, `17` Illinois |

| Code | Meaning |
|---|---|
| 06 | California |
| 17 | Illinois |
| 18 | Indiana |
| 19 | Iowa |
| 25 | Massachusetts |
| 26 | Michigan |
| 29 | Missouri |
| 36 | New York |
| 39 | Ohio |
| 42 | Pennsylvania |
| 48 | Texas |

*62 codes in the codebook; the 11 shown are those most common in this extract. See the DDI for the full list.*

### `COUNTYICP` — County of residence (ICPSR code, identifiable counties only)

ICPSR county code = 3-digit FIPS county code times 10. GEOID = STATEFIP*1000 + COUNTYICP/10. Do NOT join raw against a 5-digit GEOID. Codes are STATE-DEPENDENT and mean nothing without STATEFIP/STATEICP: ~280 distinct codes per year is the within-state county sequence, not 280 counties. The 'identifiable only' caveat in the IPUMS label does not bite here - all counties are identified in the 1850-1950 full counts, and this extract is 100% populated with no 0 codes. Boundaries are as-of-year; IPUMS imposes no uniform county system, so the same code is not necessarily the same area in 1900 and 1940.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 278 | 10 | 9,270 | `310`, `610`, `30` |
| 1910 | 100.0% | 279 | 10 | 8,400 | `310`, `610`, `30` |
| 1920 | 100.0% | 280 | 10 | 8,400 | `310`, `30`, `610` |
| 1930 | 100.0% | 282 | 10 | 8,400 | `310`, `30`, `470` |
| 1940 | 100.0% | 282 | 10 | 8,400 | `310`, `30`, `370` |

*The codebook enumerates 293 codes for this variable but gives none of them a label.*

### `URBAN` — Urban/rural status

1 = rural; 2 = urban.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 1 | 2 | `1` Rural, `2` Urban |
| 1910 | 100.0% | 2 | 1 | 2 | `1` Rural, `2` Urban |
| 1920 | 100.0% | 2 | 1 | 2 | `2` Urban, `1` Rural |
| 1930 | 100.0% | 2 | 1 | 2 | `2` Urban, `1` Rural |
| 1940 | 100.0% | 2 | 1 | 2 | `2` Urban, `1` Rural |

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
| 1900 | 100.0% | 4 | 1 | 4 | `1` Households under 1970 defin…, `2` Additional households under…, `4` Other group quarters |
| 1910 | 100.0% | 3 | 1 | 4 | `1` Households under 1970 defin…, `2` Additional households under…, `4` Other group quarters |
| 1920 | 100.0% | 4 | 1 | 4 | `1` Households under 1970 defin…, `4` Other group quarters, `2` Additional households under… |
| 1930 | 100.0% | 4 | 1 | 4 | `1` Households under 1970 defin…, `2` Additional households under…, `4` Other group quarters |
| 1940 | 100.0% | 5 | 1 | 6 | `1` Households under 1970 defin…, `4` Other group quarters, `3` Group quarters--Institutions |

| Code | Meaning |
|---|---|
| 0 | Vacant unit |
| 1 | Households under 1970 definition |
| 2 | Additional households under 1990 definition |
| 3 | Group quarters--Institutions |
| 4 | Other group quarters |
| 5 | Additional households under 2000 definition |
| 6 | Fragment |

### `FARM` — Farm status

1 = non-farm; 2 = farm.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 1 | 2 | `1` Non-Farm, `2` Farm |
| 1910 | 100.0% | 2 | 1 | 2 | `1` Non-Farm, `2` Farm |
| 1920 | 100.0% | 2 | 1 | 2 | `1` Non-Farm, `2` Farm |
| 1930 | 100.0% | 2 | 1 | 2 | `1` Non-Farm, `2` Farm |
| 1940 | 100.0% | 2 | 1 | 2 | `1` Non-Farm, `2` Farm |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | Non-Farm |
| 2 | Farm |
| 9 | Blank/missing |

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
| 1900 | 100.0% | 8 | 0 | 7 | `1` Unambiguous mother link, `0` No mother link, `2` Daughter/grandchild link |
| 1910 | 100.0% | 8 | 0 | 7 | `1` Unambiguous mother link, `0` No mother link, `2` Daughter/grandchild link |
| 1920 | 100.0% | 6 | 0 | 7 | `0` No mother link, `1` Unambiguous mother link, `2` Daughter/grandchild link |
| 1930 | 100.0% | 6 | 0 | 7 | `0` No mother link, `1` Unambiguous mother link, `2` Daughter/grandchild link |
| 1940 | 100.0% | 8 | 0 | 7 | `0` No mother link, `1` Unambiguous mother link, `2` Daughter/grandchild link |

| Code | Meaning |
|---|---|
| 0 | No mother link |
| 1 | Unambiguous mother link |
| 2 | Daughter/grandchild link |
| 3 | Preceding female (no intervening person) |
| 4 | Preceding female (surname similarity) |
| 5 | Daughter/grandchild (child surviving status) |
| 6 | Preceding female (child surviving status) |
| 7 | Spouse of father becomes stepmother |

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
| 1900 | 100.0% | 6 | 0 | 7 | `1` Unambiguous father link, `0` No father link, `2` Son/granchild link |
| 1910 | 100.0% | 6 | 0 | 7 | `0` No father link, `1` Unambiguous father link, `3` Preceding male (no interven… |
| 1920 | 100.0% | 6 | 0 | 7 | `0` No father link, `1` Unambiguous father link, `2` Son/granchild link |
| 1930 | 100.0% | 6 | 0 | 7 | `0` No father link, `1` Unambiguous father link, `2` Son/granchild link |
| 1940 | 100.0% | 6 | 0 | 7 | `0` No father link, `1` Unambiguous father link, `2` Son/granchild link |

| Code | Meaning |
|---|---|
| 0 | No father link |
| 1 | Unambiguous father link |
| 2 | Son/granchild link |
| 3 | Preceding male (no intervening person) |
| 4 | Preceding male (surname similarity) |
| 7 | Husband of mother becomes stepfather |

## Family

### `RELATE` — Relationship to household head (general)

1 = head; 2 = spouse; 3 = child; 4 = child-in-law; 5 = parent; 6 = parent-in-law; 7 = sibling; 8 = sibling-in-law; 9 = grandchild; 10 = other relative; 11 = partner/friend/visitor; 12 = other non-relative; 13 = institutional inmates.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 13 | 1 | 13 | `3` Child, `2` Spouse, `1` Head/Householder |
| 1910 | 100.0% | 13 | 1 | 13 | `3` Child, `2` Spouse, `1` Head/Householder |
| 1920 | 100.0% | 13 | 1 | 13 | `3` Child, `2` Spouse, `1` Head/Householder |
| 1930 | 100.0% | 13 | 1 | 13 | `3` Child, `1` Head/Householder, `2` Spouse |
| 1940 | 100.0% | 13 | 1 | 13 | `3` Child, `1` Head/Householder, `2` Spouse |

| Code | Meaning |
|---|---|
| 01 | Head/Householder |
| 02 | Spouse |
| 03 | Child |
| 04 | Child-in-law |
| 05 | Parent |
| 06 | Parent-in-Law |
| 07 | Sibling |
| 08 | Sibling-in-Law |
| 09 | Grandchild |
| 10 | Other relatives |
| 11 | Partner, friend, visitor |
| 12 | Other non-relatives |
| 13 | Institutional inmates |

### `RELATED` — Relationship to household head (detailed)

4-digit elaboration of RELATE.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 61 | 101 | 1,301 | `301` Child, `201` Spouse, `101` Head/Householder |
| 1910 | 100.0% | 62 | 101 | 1,301 | `301` Child, `201` Spouse, `101` Head/Householder |
| 1920 | 100.0% | 60 | 101 | 1,301 | `301` Child, `201` Spouse, `101` Head/Householder |
| 1930 | 100.0% | 62 | 101 | 1,301 | `301` Child, `101` Head/Householder, `201` Spouse |
| 1940 | 100.0% | 62 | 101 | 1,301 | `301` Child, `101` Head/Householder, `201` Spouse |

| Code | Meaning |
|---|---|
| 0101 | Head/Householder |
| 0201 | Spouse |
| 0301 | Child |
| 0401 | Child-in-law |
| 0501 | Parent |
| 0601 | Parent-in-Law |
| 0701 | Sibling |
| 0901 | Grandchild |
| 1031 | Nephew, Niece |
| 1202 | Boarders |
| 1203 | Lodgers |

*93 codes in the codebook; the 11 shown are those most common in this extract. See the DDI for the full list.*

## Demographics

### `SEX` — Sex

1 = male; 2 = female.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 1 | 2 | `1` Male, `2` Female |
| 1910 | 100.0% | 2 | 1 | 2 | `1` Male, `2` Female |
| 1920 | 100.0% | 2 | 1 | 2 | `1` Male, `2` Female |
| 1930 | 100.0% | 2 | 1 | 2 | `1` Male, `2` Female |
| 1940 | 100.0% | 2 | 1 | 2 | `1` Male, `2` Female |

| Code | Meaning |
|---|---|
| 1 | Male |
| 2 | Female |
| 9 | Missing/blank |

### `AGE` — Age in years

Age at the census date. 1940 was enumerated 1 April, so most respondents had not yet had their birthday that year.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 120 | 0 | 125 | `2` 2, `3` 3, `4` 4 |
| 1910 | 100.0% | 82 | 0 | 83 | `2` 2, `0` Less than 1 year old, `3` 3 |
| 1920 | 100.0% | 92 | 0 | 91 | `0` Less than 1 year old, `1` 1, `2` 2 |
| 1930 | 100.0% | 107 | 0 | 109 | `5` 5, `6` 6, `3` 3 |
| 1940 | 100.0% | 117 | 0 | 119 | `16` 16, `15` 15, `14` 14 |

| Code | Meaning |
|---|---|
| 000 | Less than 1 year old |
| 001 | 1 |
| 002 | 2 |
| 003 | 3 |
| 004 | 4 |
| 005 | 5 |
| 006 | 6 |
| 007 | 7 |
| 008 | 8 |
| 011 | 11 |
| 012 | 12 |
| 013 | 13 |
| 014 | 14 |
| 015 | 15 |
| 016 | 16 |
| 017 | 17 |
| 018 | 18 |
| 030 | 30 |

*138 codes in the codebook; the 18 shown are those most common in this extract. See the DDI for the full list.*

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
| 1900 | 100.0% | 5 | 1 | 5 | `1` White, `2` Black/African American, `3` American Indian or Alaska N… |
| 1910 | 100.0% | 6 | 1 | 6 | `1` White, `2` Black/African American, `3` American Indian or Alaska N… |
| 1920 | 100.0% | 6 | 1 | 6 | `1` White, `2` Black/African American, `3` American Indian or Alaska N… |
| 1930 | 100.0% | 6 | 1 | 6 | `1` White, `2` Black/African American, `3` American Indian or Alaska N… |
| 1940 | 100.0% | 7 | 1 | 7 | `1` White, `2` Black/African American, `3` American Indian or Alaska N… |

| Code | Meaning |
|---|---|
| 1 | White |
| 2 | Black/African American |
| 3 | American Indian or Alaska Native |
| 4 | Chinese |
| 5 | Japanese |
| 6 | Other Asian or Pacific Islander |
| 7 | Other race, nec |
| 8 | Two major races |
| 9 | Three or more major races |

### `RACED` — Race (detailed)

3-digit elaboration of RACE; 100 = White; 200 = Black.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 5 | 100 | 500 | `100` White, `200` Black/African American, `300` American Indian/Alaska Nati… |
| 1910 | 100.0% | 9 | 100 | 672 | `100` White, `200` Black/African American, `210` Mulatto |
| 1920 | 100.0% | 10 | 100 | 632 | `100` White, `200` Black/African American, `210` Mulatto |
| 1930 | 100.0% | 10 | 100 | 630 | `100` White, `200` Black/African American, `140` Mexican (1930) |
| 1940 | 100.0% | 25 | 100 | 700 | `100` White, `200` Black/African American, `300` American Indian/Alaska Nati… |

| Code | Meaning |
|---|---|
| 100 | White |
| 140 | Mexican (1930) |
| 200 | Black/African American |
| 210 | Mulatto |
| 300 | American Indian/Alaska Native |
| 360 | Mexican American Indian |
| 400 | Chinese |
| 500 | Japanese |
| 600 | Filipino |
| 620 | Korean |
| 672 | Asian, not specified |

*368 codes in the codebook; the 11 shown are those most common in this extract. See the DDI for the full list.*

## Birthplace

### `BPL` — Birthplace (general)

State or country of birth. US states carry their FIPS state code (1 = Alabama, 6 = California, 36 = New York); codes above 99 are foreign countries. There is NO county of birth in any US census.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 121 | 1 | 900 | `42` Pennsylvania, `36` New York, `17` Illinois |
| 1910 | 100.0% | 125 | 1 | 900 | `42` Pennsylvania, `36` New York, `17` Illinois |
| 1920 | 100.0% | 140 | 1 | 900 | `42` Pennsylvania, `36` New York, `17` Illinois |
| 1930 | 100.0% | 141 | 1 | 900 | `36` New York, `42` Pennsylvania, `17` Illinois |
| 1940 | 100.0% | 143 | 1 | 900 | `36` New York, `42` Pennsylvania, `17` Illinois |

| Code | Meaning |
|---|---|
| 017 | Illinois |
| 018 | Indiana |
| 019 | Iowa |
| 026 | Michigan |
| 029 | Missouri |
| 036 | New York |
| 039 | Ohio |
| 042 | Pennsylvania |
| 048 | Texas |
| 055 | Wisconsin |

*164 codes in the codebook; the 10 shown are those most common in this extract. See the DDI for the full list.*

### `BPLD` — Birthplace (detailed)

5-digit elaboration, generally BPL*100. Adds territorial-era distinctions (e.g. Indian Territory vs Oklahoma), never sub-state geography.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 228 | 100 | 90,022 | `4200` Pennsylvania, `3600` New York, `1700` Illinois |
| 1910 | 100.0% | 265 | 100 | 90,021 | `4200` Pennsylvania, `3600` New York, `1700` Illinois |
| 1920 | 100.0% | 355 | 100 | 90,022 | `4200` Pennsylvania, `3600` New York, `1700` Illinois |
| 1930 | 100.0% | 371 | 100 | 90,022 | `3600` New York, `4200` Pennsylvania, `1700` Illinois |
| 1940 | 100.0% | 412 | 100 | 90,022 | `3600` New York, `4200` Pennsylvania, `1700` Illinois |

| Code | Meaning |
|---|---|
| 01700 | Illinois |
| 01800 | Indiana |
| 01900 | Iowa |
| 02600 | Michigan |
| 02900 | Missouri |
| 03600 | New York |
| 03900 | Ohio |
| 04200 | Pennsylvania |
| 04800 | Texas |
| 05500 | Wisconsin |

*544 codes in the codebook; the 10 shown are those most common in this extract. See the DDI for the full list.*

## Education

### `SCHOOL` — School attendance

1 = did not attend; 2 = attended. Available 1850-1940 but the reference period changes drastically: ~12 months in 1900, ~7.5 in 1910, ~4 in 1920 (enumerated 1 January), ~6 in 1930, ~1 month in 1940. Not comparable across years untreated.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 3 | 1 | 8 | `1` No, not in school, `2` Yes, in school, `8` Unknown |
| 1910 | 100.0% | 2 | 1 | 2 | `1` No, not in school, `2` Yes, in school |
| 1920 | 100.0% | 2 | 1 | 2 | `1` No, not in school, `2` Yes, in school |
| 1930 | 100.0% | 2 | 1 | 2 | `1` No, not in school, `2` Yes, in school |
| 1940 | 100.0% | 2 | 1 | 2 | `1` No, not in school, `2` Yes, in school |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | No, not in school |
| 2 | Yes, in school |
| 8 | Unknown |
| 9 | Missing |

### `HIGRADE` — Highest grade of schooling (general)

1940 onward only. The census recorded highest grade completed; there is no years-of-schooling variable.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 23 | 1 | 99 | `11` 8th grade, `15` 12th grade, `10` 7th grade |

| Code | Meaning |
|---|---|
| 00 | N/A (or None, 1980) |
| 01 | None |
| 02 | Nursery school |
| 03 | Kindergarten |
| 04 | 1st grade |
| 05 | 2nd grade |
| 06 | 3rd grade |
| 07 | 4th grade |
| 08 | 5th grade |
| 09 | 6th grade |
| 10 | 7th grade |
| 11 | 8th grade |
| 12 | 9th grade |
| 13 | 10th grade |
| 14 | 11th grade |
| 15 | 12th grade |
| 16 | 1st year |
| 17 | 2nd year |
| 18 | 3rd year |
| 19 | 4th year |
| 20 | 5th year or more (40-50) |
| 21 | 6th year or more (60,70) |
| 22 | 7th year |
| 23 | 8th year or more |
| 99 | Missing |

### `HIGRADED` — Highest grade of schooling (detailed)

1940 onward only. Codes are multiples of 10; divide by 10 to get the grade index. 999 = N/A. Years of schooling is CONSTRUCTED: g = HIGRADED %/% 10, then 0 if g<=3, g-3 if 4<=g<=20, 17 if 21<=g<=23.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 23 | 10 | 999 | `110` 8th grade, `150` 12th grade, `100` 7th grade |

| Code | Meaning |
|---|---|
| 070 | 4th grade |
| 080 | 5th grade |
| 090 | 6th grade |
| 100 | 7th grade |
| 110 | 8th grade |
| 120 | 9th grade |
| 130 | 10th grade |
| 150 | 12th grade |

*69 codes in the codebook; the 8 shown are those most common in this extract. See the DDI for the full list.*

### `EDUC` — Educational attainment (general)

1940 onward only. IPUMS recode of HIGRADE into attainment bins; different bin edges from a raw year count.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 13 | 0 | 99 | `2` Grade 5, 6, 7, or 8, `6` Grade 12, `1` Nursery school to grade 4 |

| Code | Meaning |
|---|---|
| 00 | N/A or no schooling |
| 01 | Nursery school to grade 4 |
| 02 | Grade 5, 6, 7, or 8 |
| 03 | Grade 9 |
| 04 | Grade 10 |
| 05 | Grade 11 |
| 06 | Grade 12 |
| 07 | 1 year of college |
| 08 | 2 years of college |
| 09 | 3 years of college |
| 10 | 4 years of college |
| 11 | 5+ years of college |
| 99 | Missing |

### `EDUCD` — Educational attainment (detailed)

1940 onward only. Detailed elaboration of EDUC.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 23 | 2 | 999 | `26` Grade 8, `60` Grade 12, `25` Grade 7 |

| Code | Meaning |
|---|---|
| 017 | Grade 4 |
| 022 | Grade 5 |
| 023 | Grade 6 |
| 025 | Grade 7 |
| 026 | Grade 8 |
| 030 | Grade 9 |
| 040 | Grade 10 |
| 060 | Grade 12 |

*44 codes in the codebook; the 8 shown are those most common in this extract. See the DDI for the full list.*

### `LIT` — Literacy

Ability to read and write. The census asked this 1840-1930 and dropped it afterwards, so it is absent in 1940 exactly where HIGRADE begins.

**Available:** 1900, 1910, 1920, 1930 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 5 | 0 | 4 | `4` Yes, literate (reads and wr…, `0` N/A, `1` No, illiterate (cannot read… |
| 1910 | 100.0% | 5 | 0 | 4 | `4` Yes, literate (reads and wr…, `0` N/A, `1` No, illiterate (cannot read… |
| 1920 | 100.0% | 5 | 0 | 4 | `4` Yes, literate (reads and wr…, `0` N/A, `1` No, illiterate (cannot read… |
| 1930 | 100.0% | 3 | 0 | 4 | `4` Yes, literate (reads and wr…, `0` N/A, `1` No, illiterate (cannot read… |
| 1940 | — | | | | |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | No, illiterate (cannot read or write) |
| 2 | Cannot read, can write |
| 3 | Cannot write, can read |
| 4 | Yes, literate (reads and writes) |
| 9 | Unknown, illegible or blank |

## Labour

### `EMPSTAT` — Employment status (general)

1 = employed; 2 = unemployed; 3 = not in labour force; 0 = N/A.

**Available:** 1910, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 3 | 1 | 3 | `3` Not in labor force, `1` Employed, `2` Unemployed |
| 1920 | — | | | | |
| 1930 | 100.0% | 3 | 1 | 3 | `3` Not in labor force, `1` Employed, `2` Unemployed |
| 1940 | 100.0% | 4 | 0 | 3 | `1` Employed, `3` Not in labor force, `0` N/A |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | Employed |
| 2 | Unemployed |
| 3 | Not in labor force |
| 9 | Unknown/Illegible |

### `EMPSTATD` — Employment status (detailed)

2-digit elaboration of EMPSTAT.

**Available:** 1910, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 4 | 10 | 30 | `30` Not in Labor Force, `10` At work, `20` Unemployed |
| 1920 | — | | | | |
| 1930 | 100.0% | 4 | 10 | 30 | `30` Not in Labor Force, `10` At work, `20` Unemployed |
| 1940 | 100.0% | 12 | 0 | 34 | `10` At work, `31` NILF, housework, `33` NILF, school |

| Code | Meaning |
|---|---|
| 00 | N/A |
| 10 | At work |
| 11 | At work, public emerg |
| 12 | Has job, not working |
| 13 | Armed forces |
| 14 | Armed forces--at work |
| 15 | Armed forces--not at work but with job |
| 20 | Unemployed |
| 21 | Unemp, exper worker |
| 22 | Unemp, new worker |
| 30 | Not in Labor Force |
| 31 | NILF, housework |
| 32 | NILF, unable to work |
| 33 | NILF, school |
| 34 | NILF, other |
| 99 | Unknown/Illegible |

### `LABFORCE` — Labour force status

1 = not in labour force; 2 = in labour force; 0 = N/A.

**Available:** 1910, 1920, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 3 | 0 | 2 | `0` N/A, `2` Yes, in the labor force, `1` No, not in the labor force |
| 1920 | 100.0% | 3 | 0 | 2 | `2` Yes, in the labor force, `0` N/A, `1` No, not in the labor force |
| 1930 | 100.0% | 3 | 0 | 2 | `0` N/A, `2` Yes, in the labor force, `1` No, not in the labor force |
| 1940 | 100.0% | 3 | 0 | 2 | `2` Yes, in the labor force, `1` No, not in the labor force, `0` N/A |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | No, not in the labor force |
| 2 | Yes, in the labor force |
| 9 | Unclassifiable (employment status unknown) |

### `CLASSWKR` — Class of worker (general)

1 = self-employed; 2 = works for wages; 0 = N/A.

**Available:** 1910, 1920, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 3 | 0 | 2 | `0` N/A, `2` Works for wages, `1` Self-employed |
| 1920 | 100.0% | 3 | 0 | 2 | `0` N/A, `2` Works for wages, `1` Self-employed |
| 1930 | 100.0% | 3 | 0 | 2 | `0` N/A, `2` Works for wages, `1` Self-employed |
| 1940 | 100.0% | 4 | 0 | 9 | `0` N/A, `2` Works for wages, `1` Self-employed |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | Self-employed |
| 2 | Works for wages |
| 9 | Unknown |

### `CLASSWKRD` — Class of worker (detailed)

2-digit elaboration of CLASSWKR.

**Available:** 1910, 1920, 1930, 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | 100.0% | 4 | 0 | 20 | `0` N/A, `20` Works for wages, `12` Working on own account |
| 1920 | 100.0% | 4 | 0 | 20 | `0` N/A, `20` Works for wages, `12` Working on own account |
| 1930 | 100.0% | 5 | 0 | 29 | `0` N/A, `20` Works for wages, `12` Working on own account |
| 1940 | 100.0% | 8 | 0 | 98 | `0` N/A, `22` Wage/salary, private, `12` Working on own account |

| Code | Meaning |
|---|---|
| 00 | N/A |
| 10 | Self-employed |
| 11 | Employer |
| 12 | Working on own account |
| 13 | Self-employed, not incorporated |
| 14 | Self-employed, incorporated |
| 20 | Works for wages |
| 21 | Works on salary (1920) |
| 22 | Wage/salary, private |
| 23 | Wage/salary at non-profit |
| 24 | Wage/salary, government |
| 25 | Federal govt employee |
| 26 | Armed forces |
| 27 | State govt employee |
| 28 | Local govt employee |
| 29 | Unpaid family worker |
| 98 | Illegible |
| 99 | Unknown |

### `OCC1950` — Occupation (1950 basis)

Occupation recoded to the 1950 classification for cross-year comparability. Manual non-farm groups: craftsmen 500-595, operatives 600-690, labourers 910-970; service 700-790 and farm 100/123/810-840 are usually kept separate. 999 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 261 | 0 | 999 | `999` N/A (blank), `983` At school/student, `820` Farm laborers, wage workers |
| 1910 | 100.0% | 271 | 0 | 999 | `999` N/A (blank), `100` Farmers (owners and tenants), `820` Farm laborers, wage workers |
| 1920 | 100.0% | 274 | 0 | 999 | `999` N/A (blank), `100` Farmers (owners and tenants), `820` Farm laborers, wage workers |
| 1930 | 100.0% | 275 | 0 | 999 | `999` N/A (blank), `100` Farmers (owners and tenants), `970` Laborers (nec) |
| 1940 | 100.0% | 223 | 1 | 999 | `999` N/A (blank), `100` Farmers (owners and tenants), `690` Operative and kindred worke… |

| Code | Meaning |
|---|---|
| 100 | Farmers (owners and tenants) |
| 290 | Managers, officials, and proprietors (nec) |
| 390 | Clerical and kindred workers (n.e.c.) |
| 490 | Salesmen and sales clerks (nec) |
| 690 | Operative and kindred workers (nec) |
| 820 | Farm laborers, wage workers |
| 970 | Laborers (nec) |
| 979 | Not yet classified |
| 983 | At school/student |
| 999 | N/A (blank) |

*283 codes in the codebook; the 10 shown are those most common in this extract. See the DDI for the full list.*

### `IND1950` — Industry (1950 basis)

Industry recoded to the 1950 classification. 0 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 147 | 0 | 997 | `0` N/A or none reported, `105` Agriculture, `997` Nonclassifiable |
| 1910 | 100.0% | 154 | 0 | 997 | `0` N/A or none reported, `105` Agriculture, `246` Construction |
| 1920 | 100.0% | 151 | 0 | 997 | `0` N/A or none reported, `105` Agriculture, `506` Railroads and railway |
| 1930 | 100.0% | 151 | 0 | 997 | `0` N/A or none reported, `105` Agriculture, `246` Construction |
| 1940 | 100.0% | 128 | 0 | 997 | `0` N/A or none reported, `105` Agriculture, `246` Construction |

| Code | Meaning |
|---|---|
| 000 | N/A or none reported |
| 105 | Agriculture |
| 216 | Coal mining |
| 246 | Construction |
| 506 | Railroads and railway |
| 636 | Food stores, except dairy |
| 646 | General merchandise |
| 699 | Not specified retail trade |
| 826 | Private households |
| 888 | Educational services |
| 979 | Not yet specified |
| 982 | Housework at home |
| 997 | Nonclassifiable |

*162 codes in the codebook; the 13 shown are those most common in this extract. See the DDI for the full list.*

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

*The codebook enumerates 58 codes for this variable but gives none of them a label.*

### `SEI` — Duncan socioeconomic index

Constructed index assigned to OCC1950; 0 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 84 | 0 | 96 | `0` 0, `6` 6, `14` 14 |
| 1910 | 100.0% | 86 | 0 | 96 | `0` 0, `14` 14, `6` 6 |
| 1920 | 100.0% | 87 | 0 | 96 | `0` 0, `14` 14, `6` 6 |
| 1930 | 100.0% | 87 | 0 | 96 | `0` 0, `14` 14, `8` 8 |
| 1940 | 100.0% | 83 | 0 | 96 | `0` 0, `14` 14, `18` 18 |

| Code | Meaning |
|---|---|
| 14 | 14 |
| 68 | 68 |
| 44 | 44 |
| 47 | 47 |
| 08 | 8 |
| 19 | 19 |
| 18 | 18 |
| 06 | 6 |
| 00 | 0 |

*87 codes in the codebook; the 9 shown are those most common in this extract. See the DDI for the full list.*

### `PRESGL` — Siegel occupational prestige score

Constructed score assigned to OCC1950. In THIS extract the decimal is already applied - values read 40.7, not 407 - whereas the 1940 full-count Parquet leaves the implied decimal in place (see its _manifest.json), so the same variable is 10x larger there. Do not mix the two without rescaling. N/A is 0, NOT 999.9: the observed maximum is 81.5.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 188 | 0 | 81.5 | `0.0` N/A, `18.4`, `40.7` |
| 1910 | 100.0% | 196 | 0 | 81.5 | `0.0` N/A, `40.7`, `18.4` |
| 1920 | 100.0% | 197 | 0 | 81.5 | `0.0` N/A, `40.7`, `18.4` |
| 1930 | 100.0% | 197 | 0 | 81.5 | `0.0` N/A, `40.7`, `17.5` |
| 1940 | 100.0% | 170 | 0 | 81.5 | `0.0` N/A, `40.7`, `28.4` |

| Code | Meaning |
|---|---|
| 000 | N/A |

*197 codes in the codebook; the 1 shown are those most common in this extract. See the DDI for the full list.*

### `ERSCOR50` — Occupational earnings score (1950 basis)

Constructed score assigned to OCC1950. In THIS extract the decimal is already applied - values read 40.7, not 407 - whereas the 1940 full-count Parquet leaves the implied decimal in place (see its _manifest.json), so the same variable is 10x larger there. Do not mix the two without rescaling. 999.9 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 168 | 0.8 | 999.9 | `999.9`, `4.8`, `9.9` |
| 1910 | 100.0% | 174 | 0.6 | 999.9 | `999.9`, `9.9`, `4.8` |
| 1920 | 100.0% | 174 | 0.6 | 999.9 | `999.9`, `9.9`, `4.8` |
| 1930 | 100.0% | 174 | 0.6 | 999.9 | `999.9`, `9.9`, `39.7` |
| 1940 | 100.0% | 155 | 0.6 | 999.9 | `999.9`, `9.9`, `41.4` |

| Code | Meaning |
|---|---|
| 0000 | 0 |
| 0001 | 0.1 |
| 0002 | 0.2 |
| 0003 | 0.3 |
| 0004 | 0.4 |
| 0005 | 0.5 |
| 0006 | 0.6 |
| 0007 | 0.7 |
| 0008 | 0.8 |
| 0009 | 0.9 |
| 0010 | 1 |
| 0011 | 1.1 |
| 0012 | 1.2 |
| 0013 | 1.3 |
| 0014 | 1.4 |
| 0015 | 1.5 |
| 0016 | 1.6 |
| 0017 | 1.7 |
| 0018 | 1.8 |
| 0019 | 1.9 |
| 0020 | 2 |
| 0021 | 2.1 |
| 0022 | 2.2 |
| 0023 | 2.3 |
| 0024 | 2.4 |
| 0025 | 2.5 |
| 0026 | 2.6 |
| 0027 | 2.7 |
| 0028 | 2.8 |
| 0029 | 2.9 |
| 0030 | 3 |
| 0031 | 3.1 |
| 0032 | 3.2 |
| 0033 | 3.3 |
| 0034 | 3.4 |
| 0035 | 3.5 |
| 0036 | 3.6 |
| 0037 | 3.7 |
| 0038 | 3.8 |
| 0039 | 3.9 |

*First 40 of 1002 codes in the codebook. **No value in this extract matched any of them** — expected for a continuous variable whose codebook lists only special codes, but worth checking that the codebook belongs to this extract.*

### `EDSCOR50` — Occupational education score (1950 basis)

Constructed score assigned to OCC1950. In THIS extract the decimal is already applied - values read 40.7, not 407 - whereas the 1940 full-count Parquet leaves the implied decimal in place (see its _manifest.json), so the same variable is 10x larger there. Do not mix the two without rescaling. 999.9 = N/A.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 171 | 0 | 999.9 | `999.9`, `2.8`, `4.6` |
| 1910 | 100.0% | 178 | 0 | 999.9 | `999.9`, `4.6`, `2.8` |
| 1920 | 100.0% | 179 | 0 | 999.9 | `999.9`, `4.6`, `2.8` |
| 1930 | 100.0% | 179 | 0 | 999.9 | `999.9`, `4.6`, `3.0` 0.3 |
| 1940 | 100.0% | 154 | 0 | 999.9 | `999.9`, `4.6`, `3.2` |

| Code | Meaning |
|---|---|
| 0003 | 0.3 |

*1002 codes in the codebook; the 1 shown are those most common in this extract. See the DDI for the full list.*

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
| 1940 | 100.0% | 5 | 1 | 9 | `1` Same house, `2` Moved within state, `3` Moved between states |

| Code | Meaning |
|---|---|
| 0 | N/A |
| 1 | Same house |
| 2 | Moved within state |
| 3 | Moved between states |
| 4 | Abroad five years ago |
| 8 | Moved (place not reported) |
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
| 1940 | 100.0% | 9 | 10 | 90 | `10` Same house, `21` Different house, moved with…, `22` Different house, moved with… |

| Code | Meaning |
|---|---|
| 00 | N/A |
| 10 | Same house |
| 20 | Same state (migration status within state unknown) |
| 21 | Different house, moved within county |
| 22 | Different house, moved within state, between counties |
| 23 | Different house, moved within state, within PUMA |
| 24 | Different house, moved within state, between PUMAs |
| 25 | Different house, unknown within state |
| 30 | Different state (general) |
| 31 | Moved between contiguous states |
| 32 | Moved between non-contiguous states |
| 33 | Unknown between states |
| 40 | Abroad five years ago |
| 80 | Moved, but place was not reported |
| 90 | Unknown |

### `MIGPLAC5` — State or country of residence 5 years ago

1940 only. Residence in 1935. 990 = same state; 999 = unknown.

**Available:** 1940 only

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | — | | | | |
| 1910 | — | | | | |
| 1920 | — | | | | |
| 1930 | — | | | | |
| 1940 | 100.0% | 122 | 1 | 999 | `990` Same house, `36` New York, `17` Illinois |

| Code | Meaning |
|---|---|
| 006 | California |
| 017 | Illinois |
| 036 | New York |
| 039 | Ohio |
| 042 | Pennsylvania |
| 048 | Texas |
| 990 | Same house |
| 999 | Missing/unknown |

*199 codes in the codebook; the 8 shown are those most common in this extract. See the DDI for the full list.*

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
| 1900 | 100.0% | 1 | 2 | 2 | `2` Public release version 2 |
| 1910 | 100.0% | 1 | 2 | 2 | `2` Public release version 2 |
| 1920 | 100.0% | 1 | 2 | 2 | `2` Public release version 2 |
| 1930 | 100.0% | 1 | 2 | 2 | `2` Public release version 2 |
| 1940 | 100.0% | 1 | 3 | 3 | `3` Public release version 3 |

| Code | Meaning |
|---|---|
| 01 | Public release version 1 |
| 02 | Public release version 2 |
| 03 | Public release version 3 |

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
| 1900 | 100.0% | 1 | 1 | 1 | `1` Link |
| 1910 | 100.0% | 2 | 0 | 1 | `0` No link, `1` Link |
| 1920 | 100.0% | 2 | 0 | 1 | `0` No link, `1` Link |
| 1930 | 100.0% | 2 | 0 | 1 | `0` No link, `1` Link |
| 1940 | 100.0% | 2 | 0 | 1 | `0` No link, `1` Link |

| Code | Meaning |
|---|---|
| 0 | No link |
| 1 | Link |

### `LINK1910` — Links to the 1910 census

1 = this person has a linked record in 1910; 0 = not linked.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |
| 1910 | 100.0% | 1 | 1 | 1 | `1` Link |
| 1920 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |
| 1930 | 100.0% | 2 | 0 | 1 | `0` No link, `1` Link |
| 1940 | 100.0% | 2 | 0 | 1 | `0` No link, `1` Link |

| Code | Meaning |
|---|---|
| 0 | No link |
| 1 | Link |

### `LINK1920` — Links to the 1920 census

1 = this person has a linked record in 1920; 0 = not linked.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |
| 1910 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |
| 1920 | 100.0% | 1 | 1 | 1 | `1` Link |
| 1930 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |
| 1940 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |

| Code | Meaning |
|---|---|
| 0 | No link |
| 1 | Link |

### `LINK1930` — Links to the 1930 census

1 = this person has a linked record in 1930; 0 = not linked.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |
| 1910 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |
| 1920 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |
| 1930 | 100.0% | 1 | 1 | 1 | `1` Link |
| 1940 | 100.0% | 2 | 0 | 1 | `1` Link, `0` No link |

| Code | Meaning |
|---|---|
| 0 | No link |
| 1 | Link |

### `LINK1940` — Links to the 1940 census

1 = this person has a linked record in 1940; 0 = not linked.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | 1 | 1 | `1` Link |
| 1910 | 100.0% | 1 | 1 | 1 | `1` Link |
| 1920 | 100.0% | 1 | 1 | 1 | `1` Link |
| 1930 | 100.0% | 1 | 1 | 1 | `1` Link |
| 1940 | 100.0% | 1 | 1 | 1 | `1` Link |

| Code | Meaning |
|---|---|
| 0 | No link |
| 1 | Link |

### `VERSIONHIK` — HIK version

Version of the MLP linking that produced HIK, e.g. "2.0-2.0".

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | - | - | `2.0-2.0` Full count version 2, HIK v… |
| 1910 | 100.0% | 1 | - | - | `2.0-2.0` Full count version 2, HIK v… |
| 1920 | 100.0% | 1 | - | - | `2.0-2.0` Full count version 2, HIK v… |
| 1930 | 100.0% | 1 | - | - | `2.0-2.0` Full count version 2, HIK v… |
| 1940 | 100.0% | 1 | - | - | `3.0-2.0` Full count version 3, HIK v… |

| Code | Meaning |
|---|---|
| 1.0-1.1 | Full count version 1, HIK version 1.1 |
| 2.0-1.1 | Full count version 2, HIK version 1.1 |
| 3.0-1.1 | Full count version 3, HIK version 1.1 |
| 1.0-2.0 | Full count version 1, HIK version 2.0 |
| 2.0-2.0 | Full count version 2, HIK version 2.0 |
| 3.0-2.0 | Full count version 3, HIK version 2.0 |

## Quality

### `QAGE` — Data quality flag for AGE

0 = entered as written. 1 and 8 = failed edit, 2/5/7 = illegible, 3/6 = missing, 4 = allocated - so non-zero is not uniformly 'allocated'. The EXT1940USCB full-count extract does not carry these flags.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 2 | 0 | 4 | `0` Entered as written, `4` Allocated |
| 1910 | 100.0% | 2 | 0 | 4 | `0` Entered as written, `4` Allocated |
| 1920 | 100.0% | 2 | 0 | 4 | `0` Entered as written, `4` Allocated |
| 1930 | 100.0% | 2 | 0 | 4 | `0` Entered as written, `4` Allocated |
| 1940 | 100.0% | 2 | 0 | 4 | `0` Entered as written, `4` Allocated |

| Code | Meaning |
|---|---|
| 0 | Entered as written |
| 1 | Failed edit |
| 2 | Illegible |
| 3 | Missing |
| 4 | Allocated |
| 5 | Illegible |
| 6 | Missing |
| 7 | Original entry illegible |
| 8 | Original entry missing or failed edit |

### `QSCHOOL` — Data quality flag for SCHOOL and SCHLTYPE

Flags SCHOOL and SCHLTYPE, not SCHOOL alone. 0 = original entry OR inapplicable (not in universe) - the two are NOT distinguished, so 0 does not mean 'reported'. 1 and 8 = failed edit, 2/5/7 = illegible, 3/6 = missing, 4 = allocated.

**Available:** all years

| Year | Fill | Distinct | Min | Max | Most common |
|---|---|---|---|---|---|
| 1900 | 100.0% | 1 | 0 | 0 | `0` Original entry or Inapplica… |
| 1910 | 100.0% | 2 | 0 | 4 | `0` Original entry or Inapplica…, `4` Allocated |
| 1920 | 100.0% | 1 | 0 | 0 | `0` Original entry or Inapplica… |
| 1930 | 100.0% | 2 | 0 | 4 | `0` Original entry or Inapplica…, `4` Allocated |
| 1940 | 100.0% | 2 | 0 | 4 | `0` Original entry or Inapplica…, `4` Allocated |

| Code | Meaning |
|---|---|
| 0 | Original entry or Inapplicable (not in universe) |
| 1 | Failed edit |
| 2 | Illegible |
| 3 | Missing |
| 4 | Allocated |
| 5 | Illegible |
| 6 | Missing |
| 7 | Original entry illegible |
| 8 | Original entry missing or failed edit |


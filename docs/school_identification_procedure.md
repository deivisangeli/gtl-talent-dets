# School Identification Procedure

## Overview

This document explains, step by step, how we identified the universe of elite high schools used in the analysis, and why the set of high-access treated schools is defensible as complete within our definition.

---

## 1. What We Are Trying to Identify

We want schools that, historically, provided:
- **Free or near-free secondary education** (no tuition barrier)
- **Selective admission** (merit, examination, or demonstrated ability)
- **Sufficient scale** (≥ 50 students, so the catchment area meaningfully widens the talent pool)
- **Durability** (operated ≥ 20 years, long enough to affect multiple birth cohorts)

This definition excludes:
- Private tuition schools (access barrier too high)
- Open-admission neighbourhood schools (no quality signal, no peer effects)
- Vocational or military schools (different human capital target)
- Schools that closed within a decade (not long enough to identify effects)

The combination of tuition-free + selective is deliberately restrictive. It targets institutions that simultaneously eliminate the cost barrier and impose a merit filter — the mechanism we believe drives talent: the brightest students from low-income families gain access to rigorous instruction they could not otherwise afford, and are sorted into cohorts of similarly able peers.

---

## 2. Universe Search

We searched across four sources, in priority order:

### 2a. Wikipedia lists and reference histories
- "List of oldest schools in the United States" (Wikipedia)
- "List of magnet schools and programmes" (Wikipedia and state DOE equivalents)
- Individual school Wikipedia articles, cross-checked for founding year and admission model
- "List of specialized high schools in New York City" (the most prominent known cluster)

### 2b. NAIS and school association directories
- National Association of Independent Schools (NAIS) historical membership lists
- State associations of independent schools (e.g. NYSAIS, AISNE, PAIS)
- These cover private schools; public schools required a separate search

### 2c. Published historical and economic work
- Goldin and Katz (1999) — "Human Capital and Social Capital: The Rise of Secondary Schooling in America" — documents state-level HS enrollment and identifies prominent public schools
- Goldin (2001) — "The Human Capital Century and American Economic Growth" — city-level schooling histories
- Tyack (1974) — "The One Best System" — history of US urban public schooling

### 2d. State DOE and city school district records
- For each state with large cities, we searched historical surveys of selective/magnet schools
- Most valuable for identifying public exam schools (Central High Philadelphia, Lowell SF, Boston Latin, Baltimore City College, Walnut Hills Cincinnati, NYC specialized schools)

### 2e. Cross-verification against alumni notable-people lists
- Schools with known clusters of Wikipedia-notable alumni were flagged for inclusion
- This step only adds precision; it does not define the sample (circularity risk)

---

## 3. Inclusion/Exclusion Criteria Applied

Each school in the database is coded on 7 binary criteria. A school is **high-access** (`crit_high_access_strict = "yes"`) if and only if it passes ALL 7:

| Criterion | Column | Decision rule |
|-----------|--------|---------------|
| Secondary school | `crit_secondary_school` | Must be a secondary-level institution (grades ~9–12 or equivalent). Excludes primary schools, colleges, vocational-only programmes. |
| In frame | `crit_in_frame_1800_1940` | Founded/operating within 1800–1940. Pre-1800 schools are recorded but excluded from the treatment set (their counties are "always treated" in our window). Schools after 1940 are excluded (too recent; right-censoring of talent outcomes). |
| Active ≥ 20 years | `crit_active_20yr` | Must have operated continuously (or with reorganisation of the same lineage) for at least 20 years. Schools that closed within a decade cannot credibly affect multiple birth cohorts. |
| Size ≥ 50 | `crit_first_decade_size_ge_50` | At least 50 enrolled students (assessed at or near founding). Smaller schools cannot materially widen the local talent pool. |
| Tuition-free historical | `crit_tuition_free_historical` | Must have been tuition-free at the time of founding. Private schools with scholarship programmes but dominant tuition revenue are coded as tuition-dominant (fail this criterion). |
| Selective historical | `crit_selective_historical` | Must have used merit or examination-based admission at the time of founding. Open-admission public schools (neighbourhood, lottery) fail this criterion. |
| Not special model | `crit_not_special_model` | Must not be a school with a special-population mission (military academy, naval prep, prison school, reform school, teacher training model school). These schools serve a pre-selected population not representative of the local talent pool. |

### Edge cases and resolutions

**Pre-1800 schools**: Boston Latin (1635), Phillips Andover (1778), Collegiate School NYC (1628), Trinity School NYC (1709), Roxbury Latin (1645), Phillips Exeter (1781), Newark Academy (1774), Hopkins School (1660). All are recorded in the database. They fail `crit_in_frame`. Their counties (Suffolk MA, Essex MA, Rockingham NH, New York County NY, New Haven County CT) are **excluded from the control group** because they had elite-school access long before our panel begins. This is conservative: it prevents contamination of the control group by "always-treated" counties.

**Reorganised/renamed schools**: treated as continuous if the school served the same community with the same educational mission throughout. Founding year is the earliest documented predecessor.  
Example: Dunbar High School DC — roots as "Preparatory High School for Colored Youth" (1870), renamed M Street School (1891), renamed Dunbar (1916). We use 1870 as the founding year and code it as continuous.

**Later mergers** (`continuity_status = "later_merger_use_roots"`): we use the founding year of the predecessor institution but flag these schools as `core_with_caution`. They are included in the main event study but excluded in a robustness check.

**Segregation-era Black schools**: Dunbar DC (1870), Sumner MO (1875), Howard DE (1869), Pearl TN (1883), BTW GA (1924) are included in the database. Dunbar passes `crit_high_access_strict` (selective, tuition-free, secondary). The others are open-admission within the Black community (fail `crit_selective_historical`) and are coded as low-access. They are **kept as a separate group** for a planned stratified analysis.

**Vermont sending-town academies** (Burr & Burton, St. Johnsbury): these operated on a town-tuition model — the town paid for students to attend, effectively creating a tuition-free local secondary school. They pass `crit_tuition_free_historical`. However, they fail `crit_selective_historical` (admission was open to all local students, not merit-screened). Coded as high-access-public but NOT high-access-strict.

**Lane Tech Chicago (1908)**: founded as a vocational/manual-training school. Today it is selective (grades + test). Coded as `historically_unclear` for selectivity. Excluded from the strict high-access treatment group. This is the largest potential omission: if Lane Tech was effectively selective from early on, Cook County (Chicago) would be treated earlier than we assume. The bias from this omission is toward zero (underpowers the test) rather than toward a false positive.

---

## 4. Coverage Assessment

### By state/city

| Key city | County | Qualifying school | In high-access set? |
|----------|--------|------------------|---------------------|
| Boston, MA | Suffolk | Boston Latin (1635) | No — pre-1800 |
| New York, NY (Manhattan) | New York | Hunter HS (1869), Stuyvesant (1904), Regis (1914) | Yes |
| Brooklyn, NY | Kings | Brooklyn Tech (1922) | Yes |
| Bronx, NY | Bronx | Bronx Science (1938) | Yes |
| Philadelphia, PA | Philadelphia | Central High (1836) | Yes |
| Baltimore, MD | Baltimore city | Baltimore City College (1839), Baltimore Poly (1883) | Yes |
| Baltimore, MD | Baltimore city | McDonogh School (1873) | Yes (historically free for poor boys) |
| Washington, DC | DC | Dunbar (1870) | Yes (core_with_caution) |
| Cincinnati, OH | Hamilton | Walnut Hills (1895) | Yes |
| San Francisco, CA | San Francisco | Lowell HS (1856) | Yes |
| Chicago, IL | Cook | Lane Tech (1908) | No — historically unclear selectivity |
| Detroit, MI | Wayne | None identified | No qualifying school |
| Pittsburgh, PA | Allegheny | None identified | No qualifying school |
| Cleveland, OH | Cuyahoga | None identified | No qualifying school |
| Minneapolis, MN | Hennepin | None identified | No qualifying school |
| St. Louis, MO | St. Louis city | Sumner HS (1875) | No — open admission |
| Nashville, TN | Davidson | Pearl HS (1883) | No — open admission |

### Notable confirmed absences

The cities **without** a qualifying pre-1930 selective public high school are not a sampling failure — the historical record confirms these cities did not have an exam-admit free public HS in this period:

- **Chicago**: The Chicago selective-enrollment programme formalised in the 1980s. Pre-1980, Lane Tech and similar schools operated as vocational or broadly open schools. There was no equivalent of NYC's specialized high schools.
- **Detroit**: Cass Technical HS (1907) was a vocational school; selective enrollment came much later.
- **Pittsburgh**: No pre-1930 exam-admit free public HS. Shady Side Academy (private, 1883) serves the elite but fails tuition-free criterion.
- **Cleveland**: Public high schools expanded broadly but without examination-based admission before 1930.

These absences are historically plausible: the exam-admit selective public school model was most developed in cities with very large immigrant populations where a meritocratic gateway was politically salient (NYC, Boston, Philadelphia, Baltimore).

---

## 5. What Could We Be Missing?

The most plausible omissions, by category:

### 5a. Public exam schools we have not found

We believe the set of pre-1930 exam-admit free public high schools in the US is close to complete. These were major civic institutions that generated extensive newspaper coverage and government reporting. Known examples:
- **Stuyvesant, Bronx Science, Brooklyn Tech** (NYC): in our list ✓
- **Lowell HS** (San Francisco): in our list ✓
- **Central High** (Philadelphia): in our list ✓
- **Baltimore City College, Baltimore Polytechnic** (Baltimore): in our list ✓
- **Boston Latin** (Boston): in our list, pre-1800 excluded ✓
- **Walnut Hills** (Cincinnati): in our list ✓
- **Boston English HS** (Boston, 1821): open-admission, not exam-based — fails selectivity
- **Cass Tech, Denby Tech** (Detroit): vocational, open-admission — fail selectivity

If there are additional cities with pre-1930 exam-admit free public high schools, they are likely small cities where the Wikipedia-notable person data is too sparse to identify any effect anyway.

### 5b. Private schools with large scholarship programmes

Some 19th-century private schools offered free places to a large share of students (essentially operating as subsidised local schools). We code these conservatively as tuition-dominant unless the school's founding documents explicitly describe a free-access mission (as McDonogh does). More research could upgrade a small number of these schools. The bias from under-inclusion is toward zero.

### 5c. Schools outside the continental US

Hawaii and Alaska are excluded (territories in most of our study period, very limited Wikipedia coverage). Puerto Rico, the Philippines, and other territories are excluded.

---

## 6. Identification Assumption and Bias Direction

Our high-access school set likely **under-counts** rather than over-counts qualifying schools (given the conservative approach to ambiguous cases). Under-counting has a clear bias direction: it mis-classifies some treated counties as never-treated controls, which:

1. Attenuates the estimated ATT (bias toward zero).
2. If the mis-classified counties are typical treated counties, this generates classical attenuation bias.
3. If the mis-classified counties are unusual (e.g., Chicago is larger and more industrial than the typical treated county), the bias direction is less clear.

For interpretation: any positive result we find in the main analysis is a **lower bound** on the true causal effect of high-access elite schools on STEM talent production.

---

## 7. Planned Validation Checks

1. **Compare against Goldin-Katz (1999) Appendix**: their state-by-state HS data documents which cities had academically prominent public schools before 1940. Cross-reference our list against cities they flag.

2. **Compare against NHGIS school enrollment data**: counties in our treated set should show differential trends in school enrollment relative to the timing of the school opening. If they do not, the school was not large enough to affect county-level enrollment (supporting the `crit_size_ge_50` exclusion).

3. **Sensitivity to Lane Tech**: run a robustness specification treating Cook County (Chicago) as treated from 1910 (Lane Tech founding). If including Lane Tech does not change the result, the omission is inconsequential.

4. **Pre-period test for pre-1800 contamination**: for the counties we excluded because of pre-1800 schools (Suffolk MA, Essex MA, etc.), verify that they show significantly higher STEM rates in 1800 and earlier relative to matched counties. This justifies their exclusion from the control group.

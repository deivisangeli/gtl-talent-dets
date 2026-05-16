# International Replication Plan

## Overview

The US analysis identifies a positive causal effect of selective, tuition-free high schools on STEM talent production. Replicating this finding internationally serves two purposes:

1. **External validity**: does the mechanism generalise beyond the US context?
2. **Historical depth**: in countries with longer elite school histories, we can study effects over 200+ years rather than ~100.

The cross-verified database (Laouan et al.) is global, covering notable Wikipedia people from all countries with birth coordinates. The HYDE population rasters are also global. We therefore have the outcome and exposure data; we need country-specific school lists with founding dates and geocoordinates.

---

## Priority Countries

### 1. United Kingdom (England and Wales)

**Why priority**: UK grammar schools are the best-studied example of selective secondary education with natural experiments in their ABOLITION (1965 comprehensive reform), providing an additional identification strategy.

**School type**: Grammar schools — selective secondary schools admitting on the 11+ exam. Many date to the Tudor period (16th century), providing long pre-treatment periods.

**Identification strategy A — founding**:
- Same as US: staggered ETWFE on county founding dates of grammar schools.
- Treatment unit: historic county (or local education authority, LEA).
- Challenge: many grammar schools were founded pre-1800 (well before our panel), making them "always treated" for most of the series.

**Identification strategy B — abolition (preferred)**:
- The 1965 DES Circular 10/65 requested (but did not require) LEAs to submit plans for comprehensive reorganisation. Different LEAs complied at different times, 1965–1988.
- Treatment: decade an LEA abolished its grammar schools (i.e., became comprehensive).
- Outcome: STEM talent born in that LEA, measured ~15–20 years after treatment (cohorts most affected by the reform).
- This is a clean, well-documented staggered experiment. Selection into compliance timing is partially driven by local politics, not local talent.
- Prior work: Manning and Pischke (2006), Galindo-Rueda and Vignoles (2004), Burgess et al. (2014).

**Data needed**:
- Grammar school list with county-level assignment: **DGSE (Department for Education) historical databases; schools in England register (gov.uk)**. Also: Tapper and Salter (1978), Simon (1991).
- LEA-level comprehensive reform dates: **Comprehensive school database (Bellamy and Greenaway 2005); Galindo-Rueda and Vignoles (2004) use this.** Data may be obtainable from ISER Essex or from the original NCES-UK equivalent.
- Historic county population: HYDE covers UK. More precise: ENGLAND CENSUS 1801–2001 (Office for National Statistics historical series).
- Wikipedia birth coordinates: already in the cross-verified database; subset to UK (citizenship = United Kingdom, Great Britain, or iso3 = GBR).

**Pre-existing datasets to look for**:
- Galindo-Rueda & Vignoles (2004): LEA reform dates are in their appendix.
- Clark (2010) "Educating the Masses": reviews UK selective secondary education.

---

### 2. France

**Why priority**: France has a highly centralised, hierarchical secondary system with clearly ranked institutions (lycées ≫ autres). The écoles normales supérieures (ENS) and grandes écoles pipeline is well-documented historically.

**School type**: Lycées — government secondary schools, especially the grandes lycées in Paris and departmental capitals. The most selective are the "lycées d'excellence" (Louis-le-Grand, Henri-IV, Condorcet, etc.) which host classes préparatoires (CPGE) feeding into the grandes écoles.

**Identification strategy**:
- Treatment: département receives its first lycée (or first selective lycée with classes préparatoires).
- Napoleon established the first imperial lycées in 1802 in major cities; the system expanded through the 19th century to smaller cities and departments.
- The staggered rollout of lycées with classes préparatoires across départements provides within-France variation.

**Data needed**:
- List of lycées with founding dates and département: **Ministère de l'Éducation Nationale historical records; "Recueil des lois et règlements concernant l'instruction publique" (annual reports 1808–1940).**
- Historical département population: HYDE covers France. More precise: INSEE historical census data (1801–2000).
- Wikipedia birth data: subset cross-verified database to France (citizenship = France, iso3 = FRA).

**Key reference**: Brezis and Crouzet (2004) "The Role of Higher Education Institutions: Recruitment of Elites and Economic Growth" discuss the grandes écoles system.

---

### 3. Germany

**Why less tractable**:
- Gymnasium (selective academic secondary) was nearly universal in German cities by the mid-19th century.
- Little within-Germany staggered variation in when cities got their first Gymnasium (most pre-date 1800).
- The relevant variation is in QUALITY and selectivity of the Gymnasium across states (Länder), not timing.
- Possible alternative: use the Prussian Abitur pass rate as a proxy for effective selectivity. High Abitur pass rate ≈ easier access to university (less selective); low rate ≈ more selective elite. This would require time-series data on Abitur rates by district.

**Tentative status**: Low priority. Include only if UK and France replications are successful.

---

### 4. Japan

**Why interesting**:
- Pre-war Japan had a very clear elite-school system: First Higher School (一高, Tokyo), with regional higher schools in major cities. These were explicitly selective, tuition-subsidised, and produced a disproportionate share of Japan's scientists, prime ministers, and Nobel Prize winners.
- 33 higher schools existed by 1939; they were abolished in 1950 (post-war education reform under US occupation).
- The staggered FOUNDING of regional higher schools provides treatment variation.

**School type**:旧制高等学校 (old higher schools), 1886–1950. These were post-secondary preparatory institutions (age 17–20) feeding directly into imperial universities, but functionally equivalent to elite secondary schools in terms of talent identification.

**Identification strategy**:
- Treatment: prefecture gets its first higher school.
- Founding dates: First Higher School (1886 Tokyo), Second HS (1887 Sendai), Third (1894 Kyoto), Fourth (1901 Kanazawa), Fifth (1901 Kumamoto), Sixth (1908 Okayama), Seventh (1908 Kagoshima), Eighth (1908 Nagoya), etc.
- Post-1930, many more were added (up to 33). This gives a multi-wave staggered treatment.
- Outcome: Wikipedia-notable Japanese scientists and scholars born in each prefecture.

**Data needed**:
- Higher school list with founding dates and prefecture: **well-documented in Japanese historical education literature; Amano (1990) "Education and Examination in Modern Japan"**.
- Prefecture population: HYDE covers Japan. More precise: Japanese Census Bureau historical data (国勢調査).
- Wikipedia birth data: subset cross-verified database to Japan (iso3 = JPN). The database likely has many entries for Japan given its strong Wikipedia culture for historical figures.

---

## Common Data Architecture

For any country extension, we need the same three-layer structure as the US analysis:

```
Country-level school list CSV:
  school_name, city, region_code, founding_year, type, 
  crit_secondary, crit_selective, crit_free, crit_active_20yr,
  crit_high_access_strict, lat, lon, region_geoid

Regional population panel:
  region_geoid, decade, population, source

Wikipedia talent panel:
  region_geoid, decade, n_notable, n_stem, stem_per_1000_pop

```

The `analysis_elite_school_high_access_estimators_1800.R` script is designed around the US county; it will need a generalised version that accepts arbitrary region units. The helpers in `etwfe_high_access_helpers.R` are already generic and can be reused directly.

---

## Data Collection Priority

Phase 1 (immediate):
- [ ] UK LEA grammar school abolition dates (Galindo-Rueda & Vignoles appendix, or DfE archives)
- [ ] France lycée founding dates by département (Ministère records, 1802–1940)
- [ ] Count of Wikipedia-notable people by UK county and French département from cross-verified database

Phase 2 (after Phase 1):
- [ ] Japan higher school founding dates by prefecture
- [ ] Historical prefecture population series (Japan)
- [ ] Test whether cross-verified database has sufficient Japan coverage

Phase 3 (if Phases 1–2 succeed):
- [ ] Germany Gymnasium data (much harder to operationalise)
- [ ] Australia state selective schools (more recent, 1950s–present)

---

## Quick Coverage Check (to run before investing in data collection)

```r
# Check cross-verified database coverage by country
cv <- read_csv("prep/input/cross-verified-database.csv")
cv %>%
  filter(!is.na(bpla1), !is.na(bplo1)) %>%
  count(citizenship_1_b) %>%
  arrange(desc(n)) %>%
  head(20)

# For UK specifically
cv %>%
  filter(!is.na(bpla1), !is.na(bplo1),
         str_detect(citizenship_1_b, "United Kingdom|Great Britain")) %>%
  mutate(decade = (birth %/% 10) * 10) %>%
  count(decade) %>%
  print(n = 30)
```

Run this before committing to the UK replication. If there are < 50 STEM-coded UK births per decade in the relevant period, the analysis will be underpowered.

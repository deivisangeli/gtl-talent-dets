# CLAUDE.md — GTL Talent Determinants

## Project Overview

Two papers live in this repo:

1. **Elite Schools paper** — causal effect of elite, selective, tuition-free high schools on local STEM talent production in the US (1800-2000). Main identification: staggered DiD exploiting county-decade of first qualifying school opening. Estimator: Wooldridge ETWFE / Sun-Abraham (`fixest::sunab`).

2. **Broader determinants paper** — accumulated null results across treatments (mandatory schooling laws, entering the USSR, exposure to particle accelerators / big scientific projects). Framing: a paper on *what does and doesn't drive top talent*, organized around precise nulls. The elite schools finding is the candidate positive result.

All project data lives in Dropbox, not in the git repo. The canonical current data root is:

```text
C:\Users\<username>\Globtalent Dropbox\gtl_talent_dets\
```

Path variables are defined in `paths.R` at the repo root. Prep and analysis scripts that use the current convention source `../paths.R`.

## Repository Layout

```text
paths.R         Shared data-path config for the current convention.
                Override with env var TALENT_DETS_DATA_DIR for a different machine.

prep/           Data preparation — run scripts from within this folder
  input/        Empty or manual-only in repo; project data lives in Dropbox
  output/       Empty or manual-only in repo; project outputs live in Dropbox
  land_grants/ Andrews college site-selection / land-grants county-pairs prep

analysis/       Econometric analysis — run scripts from within this folder
  elite_schools/ Elite-school event studies and validation analyses
  land_grants/ Andrews college site-selection / land-grants analyses
  mandatory_schooling/ Compulsory-schooling-law event study
  scientific_facilities/ Scientific-infrastructure and broader-determinants analyses
  world_fairs/ World-fairs event studies
  results/      Legacy/local non-event outputs; event-study outputs live in Dropbox results/<pipeline>/

docs/           School-list documentation, revision workflow, facility spreadsheet
```

Some newer scripts use the remote path helper `prep/raw_paths.R`, which reads `DET_DIR`. Keep that helper available for those scripts, but treat `paths.R`/`TALENT_DETS_DATA_DIR` as the primary documented convention for this resolved merge.

## Path Configuration

### Current canonical convention: `paths.R`

Set `TALENT_DETS_DATA_DIR` to the local Dropbox project folder when running on a machine whose Dropbox path differs from the default.

```r
TALENT_DETS_DATA_DIR <- Sys.getenv("TALENT_DETS_DATA_DIR",
                             unset = "C:/Users/<username>/Globtalent Dropbox/gtl_talent_dets")
DATA_INPUT   <- file.path(TALENT_DETS_DATA_DIR, "input")
DATA_OUTPUT  <- file.path(TALENT_DETS_DATA_DIR, "output")
AMWS_INPUT   <- file.path(DATA_INPUT,  "amws")
SCHOOLS_INPUT  <- file.path(DATA_INPUT,  "elite_schools")
AMWS_OUTPUT    <- file.path(DATA_OUTPUT, "amws")
SCHOOLS_OUTPUT <- file.path(DATA_OUTPUT, "elite_schools")
```

Source from scripts as:

```r
# prep/ scripts
source("../paths.R")

# analysis/ scripts
source("../paths.R")
```

Coauthors on a different machine should set `TALENT_DETS_DATA_DIR` in their own R environment to their local Dropbox path.

### Compatibility convention: `prep/raw_paths.R`

Some scripts introduced on the remote side use `prep/raw_paths.R`, which requires these variables in `~/.Renviron`:

```r
GTL_REPO="C:/Users/<username>/repos/gtl_talent_dets"
DET_DIR="C:/Users/<username>/Globtalent Dropbox/gtl_talent_dets"
```

- **`GTL_REPO`** — path to the repo root for scripts that need to resolve repo files from any working directory.
- **`DET_DIR`** — path to the shared Dropbox folder for scripts using `prep/raw_paths.R`.

Dropbox layout used by both conventions:

```text
<Dropbox project root>/
├── raw/         Auto-downloaded raw data (Wikipedia, Gapminder, AMO PDFs, caches)
├── input/       Manual raw inputs (HYDE rasters, NHGIS, AMWS, school lists)
├── output/      Prep pipeline outputs (CSVs, intermediate panels)
└── results/     Analysis pipeline outputs (figures, tables)
```

## Data Sources

**Input** (`file.path(TALENT_DETS_DATA_DIR, "input")`):

- `cross-verified-database.csv` — Laouan et al., Wikipedia notable people (1.1 GB). Download: `prep/download_data.R`.
- `nhgis0005_ts_nominal_county.csv` — NHGIS county population time-series 1790-2020.
- `nhgis0001_ts_nominal_county.csv` — older NHGIS file used by legacy scripts.
- `new_births_total_number_estimated.csv`, child mortality files — Gapminder.
- `elite_schools/` — hand-curated school lists and revision files.
- `amws/` — American Men and Women of Science raw files (1906, 1938, 1955).
- `hyde_pop_asc/`, `hyde/` — HYDE 3.x rasters (download from PBL Netherlands before running HYDE scripts).
- `compulsory_schooling_laws.csv` — state-level compulsory schooling law dates.
- `geonames_US.txt`, `2024_Gaz_place_national.txt` — US geocoding reference.
- `facilities_us.csv`, `facilities_us_alt.csv` — scientific facility lists.

**Raw cache** (`file.path(TALENT_DETS_DATA_DIR, "raw")`, or `file.path(Sys.getenv("DET_DIR"), "raw")` for `raw_paths.R` scripts):

- Auto-downloaded Wikipedia, Gapminder, AMO, geodata, and tigris files.

**Output** (`file.path(TALENT_DETS_DATA_DIR, "output")`):

- `elite_schools/` — processed school lists.
- `amws/` — geocoded AMWS files.
- Panel CSVs: `us_panel_county_stem_1800.csv`, `county_population.csv`, `us_panel_county.csv`, etc.

## Script Execution

Working directory still matters for many scripts. Run prep scripts from `prep/` and analysis scripts from `analysis/` unless the script explicitly resolves paths through `GTL_REPO`.

## Prep Pipeline

Run all scripts from `prep/`. Order matters within each group.

### Core Wikipedia pipeline

| Step | Script | Output |
|------|--------|--------|
| 1 | `download_data.R` | `cross-verified-database.csv` in Dropbox input/raw, depending on path helper used |
| 2 | `cleaning.R` | `data_final.csv` |
| 3 | `cleaning_stem.R` + `stem_labels.R` | `data_final_stem.csv`, `crossverified_with_stem.csv` |
| 4 | `build_elite_high_schools_national.py` | `elite_schools/elite_high_schools_core_1800_1930.csv` |
| 5 | `build_county_population.R` | `county_population.csv` (NHGIS + HYDE unified — preferred) |
| 5b | `build_county_hyde_population.R` | `county_hyde_population.csv` (HYDE-only — keep as alternative) |
| 6 | `cleaning_county_stem.R` | `us_panel_county_stem.csv` |
| 7 | `cleaning_county_1800.R` | `us_panel_county_stem_1800.csv` |

**Population pipeline note:** Two scripts build the county population series. `build_county_population.R` (NHGIS + HYDE) is the canonical version; `build_county_hyde_population.R` (HYDE-only) is retained as an alternative until a decision is made on which harmonization approach to use.

### AMWS pipeline (American Men and Women of Science)

| Script | Purpose |
|--------|---------|
| `cleaning_amws_1906.R`, `_1938.R`, `_1955.R` | Parse raw AMWS editions |
| `geocode_amws_edition.R`, `geocode_amws_1955_us.R` | Geocode birth locations |
| `dedup_amws_editions.R` | Remove duplicates across editions |
| `build_amws_county_year.R`, `build_amws_combined_county_year.R` | Aggregate to county-year panel |

### Land grants pipeline

| Script | Purpose |
|--------|---------|
| `land_grants/01_build_andrews_county_pairs_long.R` | Build `raw/andrews_2023_county_pairs_long.xlsx` from Andrews Appendix Table A1 and Census county gazetteer |

### Legacy / broader determinants pipeline

| Script | Purpose |
|--------|---------|
| `download_amo_results.R` | Scrape AMO results page and download winner PDFs by year/grade |
| `cleaning_county.R` | County-level Wikipedia panel (older) |
| `cleaning_us.R` | US HYDE cleaning, facilities panel |
| `coding_hyde.R`, `cleaning_hyde.R`, `hyde_data.R` | HYDE raster processing |
| `coding_city.R`, `new_cleaning.R` | Alternative geocoding |

## Analysis Pipeline

Run from `analysis/`.

AMWS is an outcome source, not a standalone analysis pipeline. AMWS outcome scripts live under the treatment pipeline that uses them, currently `elite_schools/`, `land_grants/`, or `scientific_facilities/`. Standalone legacy AMWS analyses are archived under `analysis/archive/amws_standalone/`.

### Elite schools paper (main)

| Script | Purpose |
|--------|---------|
| `elite_schools/analysis_elite_school_high_access_estimators_1800.R` | **Main analysis** — Wooldridge ETWFE + CS DID |
| `elite_schools/etwfe_high_access_helpers.R` | Helper: Sun-Abraham runner, detrending, plotting |
| `elite_schools/analysis_elite_school_1800.R` | Reduced-form event study (raw cohort means) |
| `elite_schools/analysis_elite_school_high_vs_low_1800.R` | High-access vs low-access counties |
| `elite_schools/analysis_elite_school_stem_share.R` | STEM share event study |
| `elite_schools/analysis_elite_school_radius_stem.R` | Spatial treatment by radius |
| `mandatory_schooling/analysis_compulsory_schooling.R` | Null: compulsory schooling laws |
| `elite_schools/analysis_event_study_yearly_1860_1910.R` | Annual-level event study |
| `elite_schools/analysis_continuous_treatment_panel.R` | Continuous treatment (school count) |
| `elite_schools/analysis_synthetic_control_yearly_1860_1910.R` | Synthetic control |
| `elite_schools/analysis_elite_school_year_amws_wiki.R` | AMWS vs Wikipedia comparison |
| `elite_schools/analysis_high_access_etwfe_synthetic_validation.R` | Monte Carlo validation |
| `elite_schools/analysis_synthetic_bjs_wooldridge_validation.R` | BJS vs Wooldridge comparison |

### Robustness env-var flags (main script)

```r
ELITE_MERGE_NYC=TRUE Rscript elite_schools/analysis_elite_school_high_access_estimators_1800.R
ELITE_DROP_COHORT=1920 Rscript ...
ELITE_DROP_STATES=NY Rscript ...
```

Event-study results land in the Dropbox results tree, under `results/<pipeline>/...`: `elite_schools/`, `land_grants/`, `mandatory_schooling/`, `scientific_facilities/`, and `worlds_fair/` for world-fairs compatibility.

### Broader determinants / null results

| Script | Purpose |
|--------|---------|
| `analysis_main.R` | USSR talent analysis (TWFE + staggered DiD) |
| `land_grants/analysis_amws_county_pairs_all_colleges*.R` | AMWS outcome variants for Andrews selected vs runner-up county-pairs / land-grants event studies |
| `land_grants/amws_twfe_event_study_helpers.R` | Helper for AMWS county-pairs event studies |
| `land_grants/analysis_county_pairs_es.R` | Andrews selected vs runner-up county-pairs event study |
| `land_grants/analysis_county_pairs_map.R` | Map Andrews selected and runner-up counties |
| `land_grants/analysis_county_pairs_inventor_rates_hyde_pre1900.R` | Land-grants/county-pairs event study using HYDE inventor rates |
| `land_grants/analysis_county_pairs_inventor_rates_hyde_pre1900_controls.R` | Controlled HYDE inventor-rate land-grants/county-pairs event study |
| `scientific_facilities/analysis_amws_scientific_facilities.R` | AMWS outcome event study using scientific-facility treatment timing |
| `scientific_facilities/analysis_hyde.R` | HYDE event studies at 100km and 200km radii |
| `scientific_facilities/analysis_jan26.R`, `scientific_facilities/analysis_jan26_stem.R` | Jan 2026 analysis (STEM variant) |
| `scientific_facilities/analysis_country.R` | Country-level analysis |
| `scientific_facilities/analysis_county.R`, `scientific_facilities/analysis_county_stem.R` | County-level DiD (facilities) |
| `scientific_facilities/analysis_border.R` | County border discontinuity |
| `scientific_facilities/analysis_hyde_us.R` | US HYDE state-level analysis |
| `scientific_facilities/analysis_stem.R`, `compare_stem_allsci.R` | STEM vs all-science comparisons |

## Key Definitions (Elite Schools Paper)

**Treatment**: county treated in decade `g` = first qualifying school opened in that decade. Qualifying = `crit_high_access_strict == "yes"`.

**13 high-access schools in 9 counties**: Lowell (SF), Dunbar (DC), BCC + Western HS (Baltimore city), McDonogh (Baltimore Co), Hunter + Stuyvesant + Regis (NYC Manhattan), Brooklyn Tech (Kings), Bronx Science (Bronx), Walnut Hills (Hamilton OH), Central High + Girls' High (Philadelphia).

**Outcome**: `stem_per_1000_births` or `stem_per_1000_pop` — STEM-coded Wikipedia births per county per decade, normalized by estimated county births or population.

**STEM definition**: Level-3 occupation "hard STEM" = Discovery/Science fields excluding Humanities, Social Sciences, and Medicine. Mapping in `prep/stem_labels.R`.

**Reference period**: `e = -10`. Event time `e = 0` = decade school opened. `e = +10` = first fully-exposed birth cohort.

**Estimators**: Wooldridge ETWFE (`fixest::sunab`), Callaway-Sant'Anna (`did::att_gt + aggte`), CS DID with log 1820 county population covariate.

**Canonical spec**: boroughs-separate, drops Bronx (no pre-1914 NHGIS data) and SF (no meaningful pre-1848 baseline) → 7 treated counties. NYC-merge spec: 5 boroughs → synthetic GEOID "36000" treated at g=1860, includes Bronx, drops SF → 6 treated units.

## Research Status

### What we know

- **Elite high schools**: positive effect on STEM talent production — the main finding.
- **Mandatory schooling laws**: null.
- **USSR entry**: null.
- **Scientific facilities**: null.

The elite schools result is positive and the rest are precise nulls — together they make a coherent paper about what drives top talent.

### Near-term broader-determinants lead

The frontier-exposure map (counties with historical frontier exposure) shows a suggestive positive relationship with talent production. County-level covariates worth collecting and testing include:

1. **Frontier exposure** — historical frontier county status by decade from Census/NHGIS.
2. **Demographics by date** — county-level race, immigrant share, education, and related composition over time from NHGIS or Census IPUMS.
3. **Selective high schools** — proxy for access to free, ability-selected secondary education.

### Team

- **Deivis Angeli** — PI
- **João** (jotaprox on GitHub) — RA

### Taxonomy of US-born scientists (from `taxonomy_us_scientists.R`)

- 353k US-born individuals (1800-1999): 74k in 1800s, 279k in 1900s
- ~40% of "Discovery/Science" are genuinely hard STEM
- Peak birth decade: 1940s (6,638 individuals); sharp drop-off after due to right-censoring

## Key Packages

- `fixest` — `sunab()` for Sun-Abraham ETWFE, fast TWFE
- `did` — Callaway & Sant'Anna staggered DiD
- `synthdid` — synthetic DiD
- `tidyverse` — data manipulation
- `sf`, `terra` — spatial operations
- `dataverse`, `R.utils` — data download

## Known Issues / Limitations

- HYDE rasters are not tracked in git. Download from PBL Netherlands before running `build_county_hyde_population.R`, `cleaning_us.R`, `coding_hyde.R`, `cleaning_hyde.R`, or `hyde_data.R`.
- `cross-verified-database.csv` is not tracked. Run `prep/download_data.R` first.
- `analysis/scientific_facilities/analysis_hyde_us.R`: requires `us_panel_fixed.csv` from `cleaning_us.R`, which needs HYDE rasters.
- NHGIS file name changed across scripts: older scripts use `nhgis0001_ts_nominal_county.csv`, newer use `nhgis0005_ts_nominal_county.csv`. Both should be present in Dropbox input.
- `prep/scientific_facilities/cleaning_county.R`: still requires the manual NHGIS file `nhgis0001_ts_nominal_county.csv`.
- Population pipeline: `build_county_population.R` (NHGIS+HYDE) and `build_county_hyde_population.R` (HYDE-only) both exist; a decision on which to use as canonical is pending.
- Schools with `historically_unclear == "yes"` are flagged but included in main spec.

# CLAUDE.md — GTL Talent Determinants

## Project Overview

Two papers live in this repo:

1. **Elite Schools paper** — causal effect of elite, selective, tuition-free high schools on local STEM talent production in the US (1800–2000). Main identification: staggered DiD exploiting county-decade of first qualifying school opening. Estimator: Wooldridge ETWFE / Sun-Abraham (`fixest::sunab`).

2. **Broader determinants paper** — accumulated null results across treatments (mandatory schooling laws, entering the USSR, exposure to particle accelerators / big scientific projects). Framing: a paper on *what does and doesn't drive top talent*, organized around precise nulls. The elite schools finding is the candidate positive result.

All data lives in `C:\Users\deivi\Globtalent Dropbox\gtl_talent_dets\` (shared with coauthors). Path variables are defined in `paths.R` at the repo root — all scripts source `../paths.R`.

## Repository Layout

```
paths.R         Shared data-path config — all scripts source this
                Override with env var TALENT_DETS_DATA_DIR for a different machine.

prep/           Data preparation — run all scripts from within this folder
  input/        EMPTY in repo — all data in GTL Dropbox
  output/       EMPTY in repo — all outputs in GTL Dropbox

analysis/       Econometric analysis — run all scripts from within this folder
  results/      Figures and tables (committed)

docs/           School-list documentation, revision workflow, facility spreadsheet
```

## Data Sources

**Input** (`C:\Users\deivi\Globtalent Dropbox\gtl_talent_dets\input\`):
- `cross-verified-database.csv` — Laouan et al., Wikipedia notable people (1.1 GB). Download: `prep/download_data.R`.
- `nhgis0005_ts_nominal_county.csv` — NHGIS county population time-series 1790–2020.
- `nhgis0001_ts_nominal_county.csv` — older NHGIS file used by legacy scripts.
- `new_births_total_number_estimated.csv`, child mortality files — Gapminder.
- `elite_schools/` — hand-curated school lists and revision files.
- `amws/` — American Men and Women of Science raw files (1906, 1938, 1955).
- `hyde_pop_asc/`, `hyde/` — HYDE 3.x rasters (download from PBL Netherlands before running HYDE scripts).
- `compulsory_schooling_laws.csv` — state-level compulsory schooling law dates.
- `geonames_US.txt`, `2024_Gaz_place_national.txt` — US geocoding reference.
- `facilities_us.csv`, `facilities_us_alt.csv` — scientific facility lists.

**Output** (`C:\Users\deivi\Globtalent Dropbox\gtl_talent_dets\output\`):
- `elite_schools/` — processed school lists.
- `amws/` — geocoded AMWS files.
- Panel CSVs: `us_panel_county_stem_1800.csv`, `county_population.csv`, etc.

## paths.R

```r
TALENT_DETS_DATA_DIR <- Sys.getenv("TALENT_DETS_DATA_DIR",
                             unset = "C:/Users/deivi/Globtalent Dropbox/gtl_talent_dets")
DATA_INPUT   <- file.path(TALENT_DETS_DATA_DIR, "input")
DATA_OUTPUT  <- file.path(TALENT_DETS_DATA_DIR, "output")
AMWS_INPUT   <- file.path(DATA_INPUT,  "amws")
SCHOOLS_INPUT  <- file.path(DATA_INPUT,  "elite_schools")
AMWS_OUTPUT    <- file.path(DATA_OUTPUT, "amws")
SCHOOLS_OUTPUT <- file.path(DATA_OUTPUT, "elite_schools")
```

Coauthors on a different machine: set the `TALENT_DETS_DATA_DIR` environment variable to their local Dropbox path.

## Prep Pipeline

Run all scripts from `prep/`. Order matters within each group.

### Core Wikipedia pipeline

| Step | Script | Output |
|------|--------|--------|
| 1 | `download_data.R` | `input/cross-verified-database.csv` |
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

### Legacy / broader determinants pipeline

| Script | Purpose |
|--------|---------|
| `cleaning_county.R` | County-level Wikipedia panel (older) |
| `cleaning_us.R` | US HYDE cleaning, facilities panel |
| `coding_hyde.R`, `cleaning_hyde.R`, `hyde_data.R` | HYDE raster processing |
| `coding_city.R`, `new_cleaning.R` | Alternative geocoding |

## Analysis Pipeline

Run from `analysis/`.

### Elite schools paper (main)

| Script | Purpose |
|--------|---------|
| `analysis_elite_school_high_access_estimators_1800.R` | **Main analysis** — Wooldridge ETWFE + CS DID |
| `etwfe_high_access_helpers.R` | Helper: Sun-Abraham runner, detrending, plotting |
| `analysis_elite_school_1800.R` | Reduced-form event study (raw cohort means) |
| `analysis_elite_school_high_vs_low_1800.R` | High-access vs low-access counties |
| `analysis_elite_school_stem_share.R` | STEM share event study |
| `analysis_elite_school_radius_stem.R` | Spatial treatment by radius |
| `analysis_compulsory_schooling.R` | Null: compulsory schooling laws |
| `analysis_event_study_yearly_1860_1910.R` | Annual-level event study |
| `analysis_continuous_treatment_panel.R` | Continuous treatment (school count) |
| `analysis_synthetic_control_yearly_1860_1910.R` | Synthetic control |
| `analysis_elite_school_year_amws_wiki.R` | AMWS vs Wikipedia comparison |
| `analysis_high_access_etwfe_synthetic_validation.R` | Monte Carlo validation |
| `analysis_synthetic_bjs_wooldridge_validation.R` | BJS vs Wooldridge comparison |

### Robustness env-var flags (main script)

```r
ELITE_MERGE_NYC=TRUE Rscript analysis_elite_school_high_access_estimators_1800.R
ELITE_DROP_COHORT=1920 Rscript ...
ELITE_DROP_STATES=NY Rscript ...
```

Results land in `analysis/results/elite_school_event_studies/<spec>/`.

### Broader determinants / null results

| Script | Purpose |
|--------|---------|
| `analysis_main.R` | USSR talent analysis (TWFE + staggered DiD) |
| `analysis_hyde.R` | HYDE event studies at 100km and 200km radii |
| `analysis_jan26.R`, `analysis_jan26_stem.R` | Jan 2026 analysis (STEM variant) |
| `analysis_country.R` | Country-level analysis |
| `analysis_county.R`, `analysis_county_stem.R` | County-level DiD (facilities) |
| `analysis_border.R` | County border discontinuity |
| `analysis_hyde_us.R` | US HYDE state-level analysis |
| `analysis_stem.R`, `compare_stem_allsci.R` | STEM vs all-science comparisons |

## Key Definitions (Elite Schools Paper)

**Treatment**: county treated in decade `g` = first qualifying school opened in that decade. Qualifying = `crit_high_access_strict == "yes"`.

**13 high-access schools in 9 counties**: Lowell (SF), Dunbar (DC), BCC + Western HS (Baltimore city), McDonogh (Baltimore Co), Hunter + Stuyvesant + Regis (NYC Manhattan), Brooklyn Tech (Kings), Bronx Science (Bronx), Walnut Hills (Hamilton OH), Central High + Girls' High (Philadelphia).

**Outcome**: `stem_per_1000_births` or `stem_per_1000_pop` — STEM-coded Wikipedia births per county per decade, normalized by estimated county births or population.

**STEM definition**: Level-3 occupation "hard STEM" = Discovery/Science fields excluding Humanities, Social Sciences, and Medicine. Mapping in `prep/stem_labels.R`.

**Reference period**: `e = −10`. Event time `e = 0` = decade school opened. `e = +10` = first fully-exposed birth cohort.

**Estimators**: Wooldridge ETWFE (`fixest::sunab`), Callaway-Sant'Anna (`did::att_gt + aggte`), CS DID with log 1820 county population covariate.

**Canonical spec**: boroughs-separate, drops Bronx (no pre-1914 NHGIS data) and SF (no meaningful pre-1848 baseline) → 7 treated counties. NYC-merge spec: 5 boroughs → synthetic GEOID "36000" treated at g=1860, includes Bronx, drops SF → 6 treated units.

## Research Status

### What we know
- **Elite high schools**: positive effect on STEM talent production — the main finding.
- **Mandatory schooling laws**: null.
- **USSR entry**: null.
- **Scientific facilities**: null.

The elite schools result is positive and the rest are precise nulls — together they make a coherent paper about what drives top talent.

### Team
- **Deivis Angeli** — PI
- **João** (jotaprox on GitHub) — RA

### Taxonomy of US-born scientists (from `taxonomy_us_scientists.R`)
- 353k US-born individuals (1800–1999): 74k in 1800s, 279k in 1900s
- ~40% of "Discovery/Science" are genuinely hard STEM
- Peak birth decade: 1940s (6,638 individuals); sharp drop-off after due to right-censoring

## Key Packages

- `fixest` — `sunab()` for Sun-Abraham ETWFE, fast TWFE
- `did` — Callaway & Sant'Anna staggered DiD
- `tidyverse` — data manipulation
- `sf`, `terra` — spatial operations
- `dataverse`, `R.utils` — data download

## Known Issues / Limitations

- HYDE rasters not tracked in git. Download from PBL Netherlands before running `build_county_hyde_population.R`, `cleaning_us.R`, `coding_hyde.R`.
- `cross-verified-database.csv` not tracked. Run `prep/download_data.R` first.
- `analysis_hyde_us.R`: requires `us_panel_fixed.csv` from `cleaning_us.R`, which needs HYDE rasters.
- NHGIS file name changed across scripts: older scripts use `nhgis0001_ts_nominal_county.csv`, newer use `nhgis0005_ts_nominal_county.csv`. Both should be present in Dropbox input.
- Population pipeline: `build_county_population.R` (NHGIS+HYDE) and `build_county_hyde_population.R` (HYDE-only) both exist; a decision on which to use as canonical is pending.
- Schools with `historically_unclear == "yes"` are flagged but included in main spec.

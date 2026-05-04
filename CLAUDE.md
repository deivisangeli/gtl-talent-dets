# CLAUDE.md — GTL Talent Determinants

## Project Overview

This project studies the causal effect of scientific infrastructure (particle accelerators, research labs, etc.) on local talent production, using historical data on notable people from Wikipedia matched to HYDE population rasters.

Main identification strategy: staggered difference-in-differences and synthetic DiD, exploiting variation in when scientific facilities were constructed and their geographic proximity to birth locations of notable people.

## Repository Layout

```
prep/           Data preparation — run scripts from here
  input/        Raw data (large files gitignored)
  output/       Intermediate processed datasets (committed)

analysis/       Econometric analysis — run scripts from here
  results/      Figures and model output (committed)

docs/           Reference PDFs and facility spreadsheet
```

## Script Execution

**Working directory matters.** Scripts use relative paths and must be run from within their parent folder.

### Prep pipeline (run from `prep/`)

| Script | Purpose | Key output |
|--------|---------|------------|
| `download_data.R` | Download cross-verified database from SciencesPo | `input/cross-verified-database.csv` |
| `cleaning.R` | Clean Wikipedia notable people data | `output/data_final.csv`, `output/data_final_new.csv` |
| `new_cleaning.R` | Alternative cleaning pass | `output/` |
| `coding_city.R` | Geocode birth cities | `output/` |
| `coding_hyde.R` | Match people to HYDE grid cells | `output/agg_hyde.csv` |
| `hyde_data.R` | Extract HYDE .asc rasters from zip files | `input/hyde_pop_asc/` |
| `cleaning_hyde.R` | Additional HYDE cleaning | `output/` |
| `cleaning_us.R` | US-level HYDE cleaning | `output/facilities_us.csv`, `output/us_panel_county.csv` |
| `cleaning_county.R` | County-level cleaning | `output/` |

### Analysis pipeline (run from `analysis/`)

| Script | Purpose | Key output |
|--------|---------|------------|
| `analysis_main.R` | Main USSR talent analysis (TWFE + staggered DiD) | `results/` |
| `analysis_hyde.R` | HYDE event studies at 100km and 200km radii | `results/ES_all_100.png`, `results/ES_all_200.png`, etc. |
| `analysis_jan26.R` | Jan 2026 analysis | `results/` |
| `analysis_country.R` | Country-level analysis | `results/` |
| `analysis_hyde_us.R` | US state-level HYDE analysis | `results/` |
| `analysis_county.R` | US county-level DiD | `results/` |
| `analysis_border.R` | County border discontinuity | `results/` |

## Research Status & Next Steps

### Where things stand

The project has accumulated several **null results** across different treatments:
- Mandatory schooling laws
- Entering the USSR
- Exposure to big scientific projects (facilities)

These nulls are substantively interesting in combination — they cut against common intuitions about what drives top talent. The potential framing is a paper on *determinants of top talent* organized around a set of precise nulls, ideally paired with at least one thing that works.

Lucas redid the county-level US analysis and is finishing work on identifying **candidate locations for big projects** in the US (to construct better control groups for the facilities analysis).

### Team

- **Deivis Angeli** — PI
- **João** (jotaprox on GitHub) — taking over from Lucas

Lucas has left the project. João is likely taking it over.

### Taxonomy of US-born scientists in the database

Key findings from `analysis/taxonomy_us_scientists.R`:

- **353k US-born individuals** (1800–1999): 74k in 1800s, 279k in 1900s
- **Discovery/Science share** is stable: ~13% in 1800s, ~11% in 1900s
- **Level 2 is too coarse** — "Academia" lumps historians and physicists together. Level 3 is needed for STEM identification.
- **Within Discovery/Science (Level 3 classification):**

| Field | 1800s | 1900s |
|-------|-------|-------|
| Hard STEM | 43% | 39% |
| Humanities/Social Science | 28% | 35% |
| Medicine/Health | 14% | 6% |
| Social Science | 5% | 11% |
| Other/Unclassified | 10% | 10% |

- So roughly **40% of "Discovery/Science" are genuinely hard STEM**; the rest are historians, philosophers, social scientists, medical professionals.
- 1800s notable occupations: engineers, physicians, botanists, historians, inventors, chemists, zoologists — natural history was huge.
- Gender is remarkably stable: ~83% male in both centuries.
- The peak birth decade for US scientists is the **1940s** (6,638 individuals), then sharp drop-off (right-censoring: 1960s+ born haven't had time to accumulate Wikipedia entries).
- Top by Wikipedia visibility: Edison, Kaczynski (!!), Armstrong, Chomsky, Nash, Wozniak — the list is a reminder that "notable" ≠ "scientist."

### Priority next step

**Download county-level covariates and test what predicts Wikipedia appearances in the US.**

The frontier-exposure map (counties with historical frontier exposure) shows a suggestive positive relationship with talent production — this is the most promising lead. Specific things to collect and test:

1. **Frontier exposure** — historical frontier county status by decade (from Census/NHGIS). The frontier-talent pattern in the map is striking and worth a proper regression.
2. **Demographics by date** — county-level demographic composition over time (race, immigrant share, education). Available from NHGIS or Census IPUMS.
3. **Selective high schools** — proxy for access to free, ability-selected secondary education. NYC specialized high schools (Stuyvesant, Bronx Science, etc.) have produced a disproportionate number of Nobelists. Could build a city-level panel of enrollment capacity at schools that admit solely on test scores. This might be the "one thing that works."

### Data already in hand

- `prep/output/us_panel_county.csv` — county × decade panel of inventor rates (from NHGIS + cross-verified database)
- `prep/output/facilities_us.csv` — scientific facilities with coordinates and opening dates
- `prep/input/nhgis0001_ts_nominal_county.csv` — NHGIS time-series data (check what variables are included)

## Key Packages

- `did` — Callaway & Sant'Anna staggered DiD (`att_gt`, `aggte`)
- `fixest` — fast TWFE regressions (`feols`)
- `synthdid` — synthetic DiD
- `sf`, `terra` — spatial operations
- `dataverse`, `R.utils` — data download

## Known Issues / Limitations

- `analysis_hyde_us.R`: requires `prep/output/us_panel_fixed.csv`, a HYDE cell-level US panel generated by `prep/cleaning_us.R`. This file needs HYDE rasters (`prep/input/hyde_pop_asc/`) which must be downloaded manually. Script cannot run without it.
- `prep/cleaning_us.R`, `prep/cleaning_hyde.R`, `prep/hyde_data.R`, `prep/coding_hyde.R`: all require HYDE rasters or zip files not tracked in git. Must be downloaded from [PBL Netherlands](https://www.pbl.nl/en/image/links/hyde) before running.
- `prep/new_cleaning.R`, `prep/coding_city.R`: download raw data from external URLs at runtime.

## Data Sources

- **Cross-verified database**: Laouan et al., notable people from Wikipedia with birth coordinates. Downloaded automatically by `prep/download_data.R` from SciencesPo Dataverse (DOI: 10.7910/DVN/EEA236).
- **HYDE**: Historical Database of the Global Environment — decadal population rasters at ~10km resolution, 1800–2000. Must be downloaded manually.
- **Scientific facilities**: `docs/scientific_facilities.xlsx` — hand-curated list of facility construction dates and coordinates. Also at `prep/output/scientific_facilities.csv`.

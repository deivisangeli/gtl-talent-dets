# GTL — Talent Determinants

Analysis of the impact of scientific infrastructure on talent and innovation outcomes, using the cross-verified Wikipedia database of notable people (Laouan et al.) matched to HYDE historical population data.

## Repository Structure

```
prep/           Data download and cleaning
├── input/      Raw data (large files gitignored — see below)
└── output/     Processed intermediate datasets

analysis/       Econometric analysis
└── results/    Figures and tables

docs/           Reference materials and reports
```

## How to Run

### 1. Data preparation (`prep/`)

Run from within the `prep/` directory:

```r
source("download_data.R")   # Download cross-verified database (~250MB)
source("cleaning.R")        # Clean Wikipedia notable people data
source("coding_hyde.R")     # Match people to HYDE population grid
source("hyde_data.R")       # Extract HYDE population data from zip files
source("cleaning_hyde.R")   # Clean HYDE data
source("cleaning_us.R")     # US-level HYDE cleaning
source("cleaning_county.R") # County-level cleaning
```

The HYDE zip files (`prep/input/2016_beta_release/`) must be downloaded manually from the [HYDE database](https://www.pbl.nl/en/image/links/hyde).

### 2. Analysis (`analysis/`)

Run from within the `analysis/` directory:

```r
source("analysis_main.R")      # Main USSR/talent analysis
source("analysis_hyde.R")      # HYDE event studies (100km and 200km radii)
source("analysis_jan26.R")     # Jan 2026 analysis
source("analysis_country.R")   # Country-level analysis
source("analysis_hyde_us.R")   # US state-level HYDE analysis
source("analysis_county.R")    # US county-level analysis
source("analysis_border.R")    # County border discontinuity analysis
```

## Large Files (gitignored)

| File | How to get it |
|------|---------------|
| `prep/input/cross-verified-database.csv` | Run `source("prep/download_data.R")` |
| `prep/input/hyde_pop_asc/` | Extract from HYDE zip files (see HYDE database) |
| `prep/input/2016_beta_release/` | Download from HYDE database |

## References

- Cross-verified database: [Laouan et al., SciencesPo Dataverse](https://data.sciencespo.fr/dataset.xhtml?persistentId=doi:10.7910/DVN/EEA236)
- HYDE historical population data: [PBL Netherlands](https://www.pbl.nl/en/image/links/hyde)

Working document: https://docs.google.com/document/d/13-O7SAJiJtIyxD62EpvNY_wsFK0t5yJsRz_sOzjJ1os/edit?usp=sharing

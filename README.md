# GTL — Talent Determinants

Analysis of the impact of scientific infrastructure on talent and innovation outcomes, using the cross-verified Wikipedia database of notable people (Laouan et al.) matched to HYDE historical population data.

## Repository Structure

```
prep/           Data download and cleaning
├── input/      Manual raw inputs (HYDE and NHGIS)
└── output/     Processed intermediate datasets

analysis/       Econometric analysis
└── results/    Figures and tables

docs/           Reference materials and reports
```

Auto-downloadable raw files are stored outside the repo via `DET_DIR`, in
`file.path(Sys.getenv("DET_DIR"), "raw")`.

## Setup

Add to your `~/.Renviron`:

```r
GTL_REPO="C:/Users/megaj/repos/gtl_talent_dets"
DET_DIR="C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets"
```

- **`GTL_REPO`** — path to the repo root. All scripts resolve paths from here,
  so they can be run from any working directory.
- **`DET_DIR`** — shared Dropbox folder. All data (raw inputs, prep outputs,
  analysis results) lives here:
  - `raw/` — auto-downloaded raw inputs
  - `input/` — manual raw inputs (HYDE, NHGIS)
  - `output/` — prep pipeline outputs
  - `results/` — analysis pipeline outputs

Both variables are required.

## How to Run

### 1. Data preparation (`prep/`)

Run from within the `prep/` directory:

```r
source("download_data.R")   # Download cross-verified database (~250MB)
source("cleaning.R")        # Clean Wikipedia notable people data
source("new_cleaning.R")    # Alternative country-level cleaning with cached births
source("coding_city.R")     # Region-level enrichment with cached geodata
source("coding_hyde.R")     # Match people to HYDE population grid
source("hyde_data.R")       # Extract HYDE population data from zip files
source("cleaning_hyde.R")   # Clean HYDE data
source("cleaning_us.R")     # US-level HYDE cleaning
source("cleaning_county.R") # County-level cleaning
```

The HYDE zip files (`prep/input/2016_beta_release/`) must be downloaded manually from the [HYDE database](https://www.pbl.nl/en/image/links/hyde).
The NHGIS county time-series file (`prep/input/nhgis0001_ts_nominal_county.csv`) also remains a manual input.

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

## Data Inputs

| File | How to get it |
|------|---------------|
| `file.path(Sys.getenv("DET_DIR"), "raw", "cross-verified-database.csv")` | Run `source("prep/download_data.R")` |
| `file.path(Sys.getenv("DET_DIR"), "raw", "new_births_total_number_estimated.csv")` | Created automatically by `new_cleaning.R`, `coding_city.R`, or `cleaning.R` |
| `file.path(Sys.getenv("DET_DIR"), "raw", "geodata")` | Filled automatically when `coding_city.R` runs |
| `file.path(Sys.getenv("DET_DIR"), "raw", "tigris")` | Filled automatically when `cleaning_county.R` runs |
| `prep/input/hyde_pop_asc/` | Extract manually from HYDE zip files |
| `prep/input/2016_beta_release/` | Download manually from HYDE database |
| `prep/input/nhgis0001_ts_nominal_county.csv` | Download manually from NHGIS |

## References

- Cross-verified database: [Laouan et al., SciencesPo Dataverse](https://data.sciencespo.fr/dataset.xhtml?persistentId=doi:10.7910/DVN/EEA236)
- HYDE historical population data: [PBL Netherlands](https://www.pbl.nl/en/image/links/hyde)

Working document: https://docs.google.com/document/d/13-O7SAJiJtIyxD62EpvNY_wsFK0t5yJsRz_sOzjJ1os/edit?usp=sharing

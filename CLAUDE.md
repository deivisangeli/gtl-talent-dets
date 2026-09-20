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
| `build_amws_combined_county_period5.R` | Aggregate the county-year panel to five-year bins under both binning conventions (`period5_bins.R`); outcomes are annual averages within a bin |

### County literacy and schooling pipeline

| Script | Purpose |
|--------|---------|
| `download_county_data.r` | Download NHGIS manufacturing, farming, demographics and education domains to `raw/nhgis_*` (needs `IPUMS_API_KEY` or `IPUMS_KEY`) |
| `build_county_demographics_panel.R` | Build `county_nhgis_demographics_panel.csv` — slavery, literacy 1840-1930, attainment 1940-1950 |
| `nhgis_education_helpers.R` | Resolve 1930-1950 variable codes from the shipped codebooks |
| `test_nhgis_education_helpers.R` | Checks for the codebook parser and code resolvers |
| `mandatory_schooling/build_schooling_enforcement.R` | Read county attendance and literacy against compulsory-schooling law dates |
| `mandatory_schooling/convert_ipums_1940_fullcount.py` | Convert the 1940 full-count microdata (`EXT1940USCB.dat.gz`) to Hive-partitioned Parquet on `D:` |
| `mandatory_schooling/verify_ipums_1940_parquet.py` | Verify that Parquet tree (row counts, domains, per-state populations, round-trip) |
| `mandatory_schooling/test_ext1940_parser.py` | Synthetic-fixture checks for the 1940 fixed-width parser |
| `mandatory_schooling/build_cls_schooling_laws.R` | Convert the Clay–Lingwall–Stephens detailed compulsory-attendance / child-labour / continuation-school law data to CSV + parquet |
| `mandatory_schooling/test_cls_schooling_laws.R` | Checks for that conversion (sentinels, repeal gaps, cross-check vs Lingwall) |
| `mandatory_schooling/profile_ipums_linked_census.py` | Profile the IPUMS MLP linked extract (`usa_00001.csv.gz`, 221 M rows, 5 census years) and write `ipums_linked_1900_1940_dictionary.md` + `.csv` + `_provenance.json`; `--labels-only` re-renders from the saved numbers without re-reading the extract |
| `mandatory_schooling/ddi_helpers.py` | Read an IPUMS DDI 2.5 codebook (variable labels, value labels, sha256 provenance); `--describe <file.xml>` summarises one |
| `mandatory_schooling/test_ddi_helpers.py` | Synthetic-fixture checks for the DDI reader, both cross-checks and the `--labels-only` reload |

**CLS detailed schooling laws:** `input/compulsory_schooling_laws.csv` gives one
adoption *date* per state — the extensive margin only. The Clay–Lingwall–Stephens
replication package (`Data/raw/mandatory_schooling/replication_CLS.zip`, extracted
as `ICPSR_145122-V1.1/`) adds the intensive margin: entry/exit ages, the
work-permit and early-dropout exemptions, and continuation-school requirements,
1880–1930 for 49 states — plus `ca_years_combined`, years of required attendance
by `bpl` × `yob` (1875–1912), which joins straight onto the 1940 parquet with **no
record linkage**. Outputs in `output/mandatory_schooling/cls/`. See
`prep/mandatory_schooling/README.md`.

**1940 full-count microdata:** `Data/raw/mandatory_schooling/EXT1940USCB.dat.gz`
is the IPUMS USA v8.0 complete-count 1940 extract — hierarchical fixed-width,
household (`H`, 159 chars) followed by its persons (`P`, 273 chars), ~132 M
person records. It ships with **no codebook**; the layout is checked in as
`prep/mandatory_schooling/ext1940uscb_layout.csv` (53 household + 102 person
fields) and the converter asserts it against IPUMS' published `.sps` on every
run. Output defaults to `D:\ipums_1940_fullcount` (`IPUMS_1940_PARQUET_DIR`),
partitioned by `statefip`, ~6 GB — the one project dataset not kept in Dropbox,
since it rebuilds from the Dropbox source in under an hour. Read with
`arrow::open_dataset()`. See `prep/mandatory_schooling/README.md`.

**School attendance coverage note:** county attendance rates exist for 1850,
1890 and 1920 only, all from extracts already in Dropbox. 1880 is state-level
only; 1860 and 1900 have no county school table in NHGIS at all. Universes
differ by year (1850 any institution, 1890 common schools only, 1920 by age
band), so `attendance_universe` and `attendance_age_span` must be read before
comparing years. For 1920, `school_attendance_rate` is the broad 7-20 rate for
cross-year comparability and `attendance_rate_7_13` is the compulsory-age band.

**Literacy coverage note:** the census literacy question ran 1840-1930 and was
dropped afterwards; 1940 and 1950 asked years of school completed instead, so
those years carry an attainment proxy (`low_education_share_25plus`) and no
`literacy_rate`. NHGIS has no county literacy table for 1860, 1880 or 1890.
`literacy_measure_type` marks which construct each row holds. Set
`SKIP_NHGIS_EDUCATION=true` to build only 1790-1920 without the education extract.

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
| `mandatory_schooling/analysis_compulsory_schooling_amws_annual_csdid.R` | Annual C&S event studies, `pre20`/`pre40` profiles |
| `mandatory_schooling/analysis_compulsory_schooling_amws_period5_csdid.R` | Same design on five-year bins, run under the `standard` and `alt` binning conventions |
| `mandatory_schooling/export_mandatory_schooling_project_artifacts.R` | Copy the C&S figures into `project/figures/` for the standalone figure volume `project/mandatory_schooling.tex`; refuses to export a non-Lingwall vintage |
| `mandatory_schooling/summarise_attendance_by_law_decade.R` | Descriptive: attendance distribution by law decade (see caveat below) |
| `mandatory_schooling/describe_cls_policy_environment.R` | Descriptive: five-panel policy-environment table (adoption timing, age limits, within-state changes, person-weighted required years, requirement coding) → `results/mandatory_schooling/cls_policy_descriptives/`, CSV + booktabs `.tex` |
| `mandatory_schooling/export_cls_policy_table_to_project.R` | Export those descriptives to the paper: a 2-panel `booktabs` float at `project/tables/mandatory_schooling_laws.tex` and a standalone 5-panel volume at `project/mandatory_schooling_laws_stats.tex` (compile directly, run pdflatex twice). Guards on Lingwall attribution and `validation_passed` before writing |
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
- Five-year-bin C&S (`analysis_compulsory_schooling_amws_period5_csdid.R`): the `standard` and `alt` samples are NOT nested. Binning moves whole states between cohorts — the raw-1883 adopters bin to 1880 under `standard` and so enter the 1850-1880 target window, but to 1885 under `alt`; WA and WI move the other way. Read `cohort_bin_mapping.csv` before comparing the two.
- `project/mandatory_schooling.tex` is a standalone figure volume, not part of `main.tex`; compile it from `project/`. Its figures are managed by `export_mandatory_schooling_project_artifacts.R` — never hand-copy into `project/figures/`, which is how the volume silently went stale on the pre-Lingwall vintage. The eight synthetic-DiD figures in it are still unmanaged and of unresolved vintage.
- Five-year-bin horizons: the not-yet-treated donor pool runs out at calendar bin **1905** (every state had a law by 1920, and a donor must start 15+ years after the comparison bin), so a cohort `g` can be followed only to event time `1905 - g`. The +30 horizon therefore caps targets at binned law year `g <= 1875`; the capped cohort becomes a donor, not an exclusion. Its terminal coefficient rests on 1 control state under `standard` and 4 under `alt`. Horizons beyond +30 collapse the sample to 3 jurisdictions and are not run.
- Five-year `pre20` runs levels only. Its trend window leaves two unexposed bins, and a two-point line interpolates exactly, so a detrended estimate would be zero at both pre-bins by construction rather than estimated.
- `did` applies `anticipation` in the units of the time variable, not in period indices. The 14-year allowance is therefore unchanged on the five-year grid; do not rescale it to periods.
- Attendance by law decade (`summarise_attendance_by_law_decade.R`) is DESCRIPTIVE only: law-adoption decade is nearly collinear with region (all ten 1910s adopters are Southern, the 1870s/1880s cohorts none), and within the non-South the gradient is flat. Do not read it as bite.
- County school attendance exists for 1850, 1890 and 1920 only; 1880 is state-level; NHGIS has no 1860 or 1900 county school table. Attendance universes differ by year and are not poolable without noting the change.
- County literacy has no 1860, 1880 or 1890 table in NHGIS, and no 1940/1950 literacy at all — the census stopped asking. 1940-1950 carry an attainment proxy in separate columns; never read them as `literacy_rate`.
- `prep/build_county_demographics_panel.R` needs the `raw/nhgis_education` extract for 1930-1950. Without `IPUMS_API_KEY`/`IPUMS_KEY` it stops; `SKIP_NHGIS_EDUCATION=true` builds 1790-1920 only.
- Adding the 1930-1950 rows shifts which census vintage the land-grants control scripts select (they take the latest pre-treatment literacy year). Diff `illiterate_share_control_year` against current results after the education extract lands.
- `paths.R` still defaults to a coauthor's home directory. Set `TALENT_DETS_DATA_DIR`, or rely on the scripts that fall back to `DET_DIR`.
- The 1940 full count has **no date of birth and no county of birth** — a property of the census, not the extract. It records `AGE`/`AGEMONTH`, and birthplace only at state/country level (`BPL`/`BPLD`, `MBPL`/`FBPL`). `birthyr_approx` (`1940 - AGE`) is inherently ±1 year: enumeration was 1 April 1940, so anyone with a birthday after 2 April was born the prior year. `county_geoid` is county of **residence** in 1940 and `MIGCOUNTY` county of residence in **1935**; neither is a birth county. Recovering one means linking `HISTID` to the 1910/1920/1930 full counts, which the project does not hold.
- IPUMS `COUNTY` and `MIGCOUNTY` are FIPS county codes **multiplied by 10**. The 1940 Parquet carries a derived `county_geoid` for this reason; do not join raw `COUNTY` against a 5-digit GEOID.
- The 1940 Parquet keeps IPUMS codes raw: 9-fills (`999`, `9999`) are not nulled, and implied decimals (`HHWT`/`PERWT`/`SLWT` 2, `CPI99` 3, `PRESGL`/`ERSCOR50`/`EDSCOR50`/`NPBOSS50` 1) are not applied. Both are listed in `_manifest.json`.
- **Year of birth: CLS use `yob = 1940 - AGE - 1`, not `1940 - AGE`.** Enumeration was 1 April 1940, so most people had not yet had their birthday and the modal birth year is one lower. The 1940 Parquet's `birthyr_approx` takes the minority case, so **joining `ca_years_combined` on `birthyr_approx` is off by one for ~75% of people**, against a treatment that varies by cohort. Compute `yob` explicitly for anything touching the CLS law data.
- There is no "years of schooling" variable in IPUMS. CLS construct it from `HIGRADED`: divide by 10, then `0` if ≤3, `g-3` if 4–20, `17` if 21–23, dropping `999` (~7% of rows). `EDUC`/`EDUCD` are a different IPUMS recode with different bins — not a substitute.
- Attainment starts in 1940. `SCHOOL` (attendance) exists 1850–1940, but `HIGRADE`/`EDUC` do not exist before 1940, so in a linked extract those columns are legitimately blank on the pre-1940 rows. `SCHOOL`'s reference period also changes drastically (12 months in 1850–1900, ~4 months in 1920, ~1 month in 1940), so attendance rates are not comparable across census years without addressing it.
- CLS law data, three encoding traps (all handled by `build_cls_schooling_laws.R`, never read the raw `.dta` without them): (1) `0` means **no law**, not missing; (2) `earlyyrs`/`workyrs`/`contyrs` carry sentinels — `99` = literacy requirement, `99X` = primary-school completion of length X — and the `99 → 4` recode is CLS's **assumption**, flagged in `*_code_type`; (3) **Arizona has no law 1883–1898** and Michigan has a one-year gap, so a monotone "treated from year g" coding is wrong for Arizona by 19 years.
- The MLP linked extract's CSV shipped with **no DDI**, and the CSV still lives in OneDrive (`~/OneDrive/Documentos/usa_00001.csv.gz`), not under `DET_DIR` like every other input. The DDI was fetched separately and sits at `Data/raw/mandatory_schooling/usa_00001.xml`: it now supplies value labels for 45 of 58 variables and `profile_ipums_linked_census.py` hard-fails if it stops describing the extract. `COUNTYICP` and `OCCSCORE` have codes but no names in the codebook — ICPSR county codes are state-dependent and meaningless without `STATEFIP`. Use `--labels-only` to re-render (seconds); the full pass is only for re-deriving the numbers. Nothing in the repo downloads the extract, so rebuilding it means re-submitting the spec recorded in `prep/mandatory_schooling/README.md` by hand.
- Occupational scores are scaled differently in the project's two IPUMS datasets. `PRESGL`/`ERSCOR50`/`EDSCOR50` carry the decimal **already applied** in the linked extract (`40.7`), but the 1940 full-count Parquet leaves the implied decimal in place (`407`) — a silent 10x gap if the two are compared. N/A codes also differ by variable: `ERSCOR50`/`EDSCOR50` use `999.9`, but **`PRESGL` uses `0`** (its observed max is 81.5).
- The MLP linked extract `usa_00001.csv.gz` is **not** a two-year 1900–1940 link: it stacks all five census years (1900 13.0M, 1910 24.0M, 1920 40.0M, 1930 70.2M, 1940 73.8M = 220,979,023 rows), one row per person per census, linked cases only. `HIK` is the cross-year person key; **`HISTID` is not** — it identifies a record within a year. See `prep/mandatory_schooling/merge_cls_to_linked_census.md` before joining anything.
- The CLS join is on **state of birth × birth cohort using the 1940 rows only** — that paper uses no record linkage at all. Childhood county via `HIK` is an extension it never did, and carries match-rate selection the original design does not.
- CLS vs Lingwall: 15 states are left-censored by the 1880 panel start; of the 34 comparable, 32 match exactly. Arizona (CLS 1880 in force vs Lingwall 1899) and **Washington (Lingwall 1878, CLS 1886 statewide)** disagree.
- Compulsory-schooling policy shape, which argues against coding adoption dates alone: entry age is near-static (24 of 49 states never changed it, **none ever raised it**) while exit age rose in 38 states — but exemptions gave back about half the tightening (nominal required window 6.31 → 8.90 years, effective 6.31 → 7.59). `effective_leaving_age` is the earliest age any statutory route out opens — the child-labour work permit (`max(workage, entryage + workyrs)`) or the early-dropout exemption (`max(earlyyrs_condition, entryage + earlyyrs)`), whichever comes first, capped at `exitage`; `binding_exemption` records which governs. It assumes continuous attendance from the entry age, matching CLS's own `ca_years2` counter. Use `ca_years_combined`, not `exitage - entryage`.
- Post-1900 adoption variation is almost entirely Southern: 16 states first adopt in 1900 or later and 11 are the CLS-coded South, clustered 1913–1920. Combined with the region collinearity already noted for attendance, this makes Southern adoption a weak identification base — CLS's own `region!=3` robustness moves the IV estimate only from 0.067–0.077 to 0.064.

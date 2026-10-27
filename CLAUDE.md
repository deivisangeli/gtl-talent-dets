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
| `education/build_postsecondary_release_v06.py` | Newest education release `output/amws/amws_education_postsecondary_v06_20261021/`: v05 minus 2,178 duplicate entries (biographies captured twice: 1955 1,472 — one source block repeated 2,401 lines later; 1986 585 — the same entry read twice around page breaks, often truncated; 1938 119; 1906 2). Rows are otherwise byte-identical to v05; `duplicates_removed.csv` maps each removal to its keeper (kept=TRUE first for 1906-1955, then longest text, then lowest line id). 134,311 person rows. Validator `validate_postsecondary_release_v06.py`, tests `test_postsecondary_release_v06.py`; xlsx export `export_release_v05_xlsx.py --release v06` |
| `education/build_postsecondary_release_v05.py` | Previous education release `output/amws/amws_education_postsecondary_v05_20261020/`: v04 plus the top-500 manual institution crosswalk and award-level medical-campus overrides (`output/amws/manual_institution_matches_top500_20261019/`), 80.0% → 94.2% of awards matched; never alters v04 full matches (201 disagreements in `manual_conflicts_review.csv`). Adds the 1986-only `physical_science`/`_unresolved`/`_mixed` columns to the person view (blank for other editions; 0 is not a verified negative) and stores the 22,204 still-unmatched awards in `unmatched_awards.csv`/`unmatched_strings.csv`. Validator `validate_postsecondary_release_v05.py`, tests `test_postsecondary_release_v05.py`. Land-grants training outcomes on v05: `prep/land_grants/12_...v05.R` |

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
| `mandatory_schooling/extract_linked_bite_inputs.py` | One streaming pass over the linked extract writing `students_1940.parquet` (33.0 M) and `candidates_pre1940.parquet` (42.4 M, carrying `SCHOOL`/`SEX`/`RACE`) for the county bite and attendance measures; ~11 min |
| `mandatory_schooling/build_county_attendance.R` | Build the county ATTENDANCE measures from the childhood rows: non-attendance among legally-required children, age-standardised, with the exit-age discontinuity and its no-law benchmark |
| `mandatory_schooling/test_county_attendance.R` | Synthetic-fixture checks (32) for the attendance builder, including that two counties with identical age-specific rates but opposite composition standardise to the same value |
| `mandatory_schooling/build_county_bite.R` | Build the county compulsory-schooling ATTAINMENT bite measures: six requirement bases x four statistics at four grains, plus linkage-coverage diagnostics |
| `mandatory_schooling/build_county_attendance_bite.R` | Build the county ATTENDANCE bite — pre-law non-attendance among the children the law would later require, for the 16 post-1900 adopters; three bases, the usability guards, and the signal-to-noise diagnostic |
| `mandatory_schooling/test_county_attendance_bite.R` | Synthetic-fixture checks (22) for the attendance bite, including that age composition cannot move a standardised rate and that a thin age inside the band nulls it |
| `mandatory_schooling/extract_linked_attendance_cohort_inputs.py` | One ~4 min pass over the linked extract writing `men_1940.parquet` (18.3 M white US-born men, `AGE` 19-49) and `childhood_pre1940.parquet` (62.1 M rows at `AGE` 0-18) for the cohort attendance build |
| `mandatory_schooling/build_county_attendance_cohort.R` | County x birth-cohort schooling around the 13 first laws of 1907-1920: ever attended, meeting the law's minimum, gap to it; exposed/unexposed contrast per county |
| `mandatory_schooling/test_extract_linked_attendance_cohort.py`, `test_county_attendance_cohort.R` | Synthetic-fixture checks for that extractor (20) and builder (37) |
| `mandatory_schooling/test_county_bite.py`, `test_county_bite.R` | Synthetic-fixture checks for the extractor (24) and the bite builder (51), both runnable without the 12 GB source |
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

**County-level attainment bite measure (built; the *analysis* script is not):** `prep/mandatory_schooling/county_bite_from_linked_census.md` specifies how to compare each 1940 student's completed schooling against what the law required of them — nominal (`ca_years1`) and effective (`ca_years_combined`, net of exemptions) — aggregated to the county they lived in at school age, recovered through the MLP linkage. Key points: the CLS cohort table is re-keyed on **residence** state, not `bpl`; the raw gap is absorbed by state × cohort FE so `shortfall_rate` is the headline; and assignment census year is collinear with birth cohort (19 of 28 cohorts have one candidate year), so cohort trends are uninterpretable. `prep/mandatory_schooling/build_county_bite.R` is written and its 2026-09-21 outputs are described below; `analysis/mandatory_schooling/describe_county_bite.R` is still unwritten.

**County x cohort schooling around the 1907-1920 first laws** (`output/mandatory_schooling/attendance_cohort/`, built 2026-09-22). 13 states (DE 1907, OK 1908, NC/TN 1913, LA/TX 1916, AL/AR/GA 1917, VA 1918, FL/SC 1919, MS 1920; 11 Southern), white US-born non-migrant men born 1890-1920, unexposed if aged `exitage + 1` or more when the law passed, county = earliest linked `COUNTYICP` if taken at age <= 18. Outcomes from 1940 `HIGRADED`: `ever_attended` (>= 040), `meets_min` and `gap_hs` against `law_min = exitage - entryage` held **fixed** across cohorts. 2,484,283 men in the law states. Every state's exposed-minus-unexposed change is positive (ever attended +0.2 to +1.2 pp), but nothing nets out the trend (benchmark states +0.04 pp). **County levels carry signal (ratios 1.6-2.4) but county before/after differences do not: split-half r is 0.20 for ever attended and 0.04-0.05 for `meets_min`/`gap_hs`** - never rank counties by `diff_*`. `ever_attended` sits at the ceiling (39% of law-state cells at a bound). Spec: `prep/mandatory_schooling/county_attendance_cohort.md`.

**County-level ATTENDANCE bite measure** (`output/mandatory_schooling/county_attendance_bite/`, built 2026-09-22) — **prefer this over the attainment bite as a dose.** For each of the 16 states adopting after 1900, the share of children the law *would later require* who were not attending *before* it took effect: the room the law had to move. Pre-law by construction, 1,341 counties, 1,312 with a usable cell. Headline `bite_std_8_14` — ages 8–14, direct standardisation with uniform weights — rather than the descriptive `not_attending_std_1013`, because 10–13 sits on the attendance peak and compresses the bite (the wider band is larger for 89.8% of counties, median +0.020). Crucially it passes the project's own signal test that the attainment bite fails: cross-county SD against the binomial noise floor runs **5.40 pooled and 4.25 within state**, against **1.07** for the attainment bite's county before/after contrast, with split-half reliability **r = 0.897**. Use it as a cross-sectional treatment intensity for an outcome measured elsewhere; never difference attendance across census years. `baseline_year` is 1900 for 268 counties and 1910 for 1,073, so cross-state comparisons must use the `bite_*_1900` columns. Spec: `prep/mandatory_schooling/county_attendance_bite.md`.

**Reading `attendance_county_year.csv` directly, one trap:** on pre-law rows `entryage` and `exitage` are **0** — the raw CLS "provision absent" convention carried through by `any_value()`, **not** the window the rate was computed over. `not_attending_required` is formed over `in_window OR in_window_cf` (`build_county_attendance.R:262-267`), so pre-law cells use `cf_entryage`/`cf_exitage`, the state's modal law-in-force window. Concluding from the zeroes that the pre-law rate is degenerate is wrong.

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
| `land_grants/13_build_amws_birth_county_training_postsecondary_v06.R` | Newest birth-county training outcomes, from education release v06 (duplicates removed): 40 eligible duplicates drop out (12,360 → 12,320), every remaining entry keeps its v05 class, effects move by at most 0.002 overall / 0.021 dynamic. Runners `run_..._v06.R`, `..._v06_unbalanced.R`, validator `validate_..._v06.R`. The Oct 21 report companion (Figures 13-16) is built from these by `build_birth_county_training_v05_companion.py --release v06` |
| `analysis/land_grants/training_50km/run_birth_county_training_v06_outcome_shift.R` | Second shifted-decade definition on v06: outcome shifted, treatment kept at `g_std`, standard-decade samples, baseline births from the shifted decade (14 models). Compared with the v06 shifted-decade models (outcome and treatment shifted) in `shift_definition_comparison_*.csv`; `build_decade_shift_comparison.py` writes the four-page comparison PDF. Calendar samples are identical under both definitions; the relative sample gains 4 events under the treatment shift |
| `land_grants/12_build_amws_birth_county_training_postsecondary_v05.R` | Previous birth-county training outcomes, from education release v05 (`output/land_grants/amws_birth_county_training_postsecondary_v05_20261020/`). Same rules as 11; relative to v04 no entry may leave ever or never, and every class change needs a manual award. 50 km unknown 3,553 → 1,057. Runners `analysis/land_grants/training_50km/run_birth_county_training_postsecondary_v05.R` (28 models) and `..._v05_unbalanced.R` (14); validator `validate_..._v05.R`. `analysis/land_grants/build_college_allocation_volume.py` collects these figures and every college-allocation figure in `main.tex` (re-run 2026-10-20, byte-identical) into `project/college_allocation_event_studies.pdf` |
| `land_grants/11_build_amws_birth_county_training_postsecondary_v04.R` | Birth-county training outcomes (ever/never/unknown, same county and 50 km) from the multi_v04 education release `output/amws/amws_education_postsecondary_v04_20261019/` (built by `prep/amws/education/build_postsecondary_release_v04.py`); event studies in `analysis/land_grants/training_50km/run_birth_county_training_postsecondary_v04.R` (balanced samples) and `..._v04_unbalanced.R` (no balancing, 37/38 events). The extractor is `prep/amws/education/education_regex_v04.py`; the frozen `education_regex.py` (multi_v03) is hash-locked and must not be edited. See `prep/amws/education/POSTSECONDARY.md` |
| `occupational_fertility/build_professor_children_event_panel.R` + `analysis/land_grants/temporal_support_1830_1950/08_run_professor_channel.R` | Professors'-children channel for the College Allocation result: children of 010-029 fathers per event county x birth decade, each birth year 1830-1949 read from the census that sees it youngest in the 1850-1950 full counts (usa_00011; no interpolation), run through the paper's CS-DID (reproduces main.tex exactly). Two versions: scaled (`faculty_births`, census coverage factor 1.60-4.16 vs NCES staff, `build_professor_coverage.R`) and unscaled (`raw` = 010-029 count, no factor; was 012-029 before 2026-10-26); figures for the unscaled one end in `_unscaled`. On Figure 2's outcome at standard e=+50: faculty children +7.05 per 1,000 births scaled (+3.14 unscaled); break-even p* = 9.3% [5.5, 15.3] scaled, 21.0% unscaled; effect significant to p ~5.5% scaled; p* falls to 4.5% at +60 and 3.2% at +70. Events at +60/+70: standard 16/10, alternative 15/10. County professor coding swings between censuses (VPI's county: 8 coded professors in 1930, 173 in 1940), which moved the +50 ATT from 10.6 to 7.05 when the 1920 and 1930 censuses were added. `analysis/.../09_build_faculty_offspring_companion.py` builds the six-figure report companion. `results/land_grants/professor_channel/memo.md`. Also channels `ams_faculty`/`ams_raw` (figures `*_ams_scaled`/`*_ams_unscaled`, own common sample, all 16 standard events to +70): faculty children predicted as census faculty stock by age x AMS 1956 births per professor; scaled break-even p* 13.3% [8.1, 21.8] at +50, 4.4% at +70; significant to p ~8.2%; `memo_ams.md`. And `chsurv_faculty`/`chsurv_raw` (figures `*_chsurv_scaled`/`*_chsurv_unscaled`): the same prediction with census fertility (wives' CHSURV 1900-1910 for faculty to 1920, CHBORN 1940 sample line for 1930-1950); scaled p* 11.0% [6.9, 17.8] at +50, 3.8% at +70; significant to p ~7.0%; `memo_chsurv.md` |
| `occupational_fertility/build_professor_county_coverage.R` | County coverage factors from the Education by the Numbers faculty panel (`Data/processed/faculty_data/county_faculty_panel_estimated_interpolated.csv`, 2020 counties): factor = faculty headcount / census persons coded 010-029 (both sexes) where the headcount is larger, else 1 (never scaled down); panel year 1909/1919/1929/1939/1949 for the 1910-1950 censuses, same year before. 1949 is land-grant only. Largest factors (30-68) come from college counties the census coded with 1-5 professors → `output/occupational_fertility/professor_county_coverage_factors.csv`. The children panel and the predicted-channel builder add county-scaled columns (`county_faculty_births`, `<prefix>_county_faculty_births`); 08 runs them as `county_faculty`, `ams_county`, `chsurv_county` (figures `*_county_scaled`, `*_ams_county_scaled`, `*_chsurv_county_scaled`). Standard +50: p* 6.5% [2.9, 166] census-observed, 10.6% AMS, 9.2% CHSURV; significant only to p ~2.9-5.0%; `memo_county_scale.md` |
| `occupational_fertility/build_professor_census_fertility_schedule.R` | Census birth schedule for the predicted channel: by husband's 5-year age, m(a) = share ever married x mean wife's count per couple (CHSURV 1900+1910, 9,512 couples; CHBORN 1940 sample line, 1,608 couples; decoded value - 1), couple-weighted isotonic fit, single-year increments as rates. Lifetime 2.32 surviving (1900-1910), 1.96 ever born (1940). `census` column maps 1900-1910 rates to the 1850-1920 censuses and 1940 rates to 1930-1950 → `college_channel/professor_birth_schedule_census*.csv`. Run the channel builder with `OCC_BIRTH_SCHEDULE=<that file> OCC_CHANNEL_PREFIX=chsurv` |
| `occupational_fertility/build_professor_birth_schedule.R`, `build_professor_ams_channel.R` | AMS-predicted faculty children. The schedule: 14,756 male AMS 1956 professors born 1870-1905 (1.59 children, 30% childless, median marriage age 28), births at marriage + 1 then every 2.5 years (uniform-over-10-years variant), single-year rates per professor. The channel: men coded 010-029 by county x census x age (1850-1950 full counts) times those rates; each decade averages the forward view from the opening census and the backward view from the closing one (1880s/1890s/1950s one end only; 1830s repeat the 1840s). Predicted is 36-81% of census-observed in event counties (corr 0.92-0.99 from 1890) → `college_channel/ams_*` and `professor_birth_schedule.csv`. Shared MoS loading in `mos1956_helpers.R` |

### Occupational fertility pipeline (1910 full count)

| Script | Purpose |
|--------|---------|
| `occupational_fertility/convert_ipums_1910_fertility.R` | IPUMS extract usa_00009 (`D:/ipums_1910_birth_occupation_fullcount.gz` + `_ddi.xml`, 1910 100%, 92,043,618 persons) → Parquet at `D:/ipums_1910_fullcount_fertility`, by `STATEFIP`, raw codes; ~1.5 min |
| `occupational_fertility/build_professor_fertility.R` | 16,797,244 reciprocal `SPLOC` couples with the wife's decoded `CHBORN`/`CHSURV`, `POPLOC` own-children counts for 25.9 M men aged 20-64, female professors → `output/occupational_fertility/`; ~2.5 min |
| `occupational_fertility/test_professor_fertility.R` | 31 synthetic-fixture checks (offset decoding, boarder couples, step-children, standardisation) |
| `analysis/occupational_fertility/professor_fertility_1910.R` | Professors (`OCC1950` 012-029) against other occupations: wife-linked `CHSURV`/`CHBORN` at 40-49, age- and duration-standardised, Poisson IRRs; own children under 10 (way 2); reconciliation; female professors → `results/occupational_fertility/` (`summary.md` first) |
| `occupational_fertility/convert_ipums_1850_1950_fertility.R` | Extract usa_00011 (`D:/ipums_1850_1950_fullcount_fertility.gz`, every 100% census 1850-1950 except 1890, 816,654,828 persons, same 33 variables as usa_00010, 1950 a preliminary release) → Parquet at `D:/ipums_1850_1950_fullcount_fertility` by `YEAR`; ~13 min. Read with usa_00010's XML DDI, checked against the extract's own HTML codebook (`D:/ipums_1850_1950_ddi.html`). Coverage and children builders use it automatically when present |
| `occupational_fertility/convert_ipums_1850_1910_fertility.R`, `build_professor_nchild_decades.R` | Decade trend on `NCHILD`: extract usa_00010 (`D:/ipums_1850_1910_fullcount_fertility.gz`, 1850-1880 + 1900-1910 100%, 303,956,856 persons) → Parquet at `D:/ipums_1850_1910_fullcount_fertility` by `YEAR`; men 20-64 aggregated to cells → `output/occupational_fertility/decades/`; tests `test_professor_nchild_decades.R` (20) |
| `analysis/occupational_fertility/professor_nchild_by_decade.R` | Professors' age-standardised `NCHILD` (men 25-54, one pooled standard) by census, IRRs on cells, 1900/1910 calibration against `CHSURV`, figure → `results/occupational_fertility/decades/`. Ratio to all men 0.77/0.78/0.74/0.76 (1850-1880), 0.67/0.64 (1900/1910); level with other professionals throughout |
| `analysis/occupational_fertility/professor_fertility_mos1956.R` | Professors' fertility from American Men of Science 1956, scientist by scientist (Kim & Moser replication package `D:/ECMA_MS-22741_Replication.zip`; only the cleaned `01_MoS 1956.csv` and the career-title dictionary are extracted, to `raw/kim_moser_mos1956/` with SHA-256 provenance; none of its code is run). Professor = ever held Assistant/Associate/Full Professor (medical incl.), Department Head, Dean or Emeritus; blank `child` = 0 as in Kim & Moser (16% of married men are blank). 35,294 of 65,914 men are professors. Professors vs other scientists, birth-year FE: IRR 0.99 [0.98, 1.00] all men, 1.03 married; completed cohorts (born <= 1905) 1.59 vs 1.67 children, 30% vs 25% childless, 77% vs 84% married. So professors' fertility is that of other scientists; their lower count comes from marrying less. Science faculty only → `results/occupational_fertility/mos1956/` (`summary.md` first) |
| `occupational_fertility/build_professor_county_nchild.R` | County × census × 10-year age bin (20-29 ... 60-69) dataset of male professors' `NCHILD`, from `decades/county_cells` (written by `build_professor_nchild_decades.R`) → `output/occupational_fertility/county_decade/`: 71,295 county-age rows with professors, presidents/deans, other professionals and all men, each for men in households and married men (`_married`, `SPLOC` > 0); 14,316 county × census rows with an indirectly standardised `ratio_local` (vs the county's own men by age) and `ratio_national`. Keys `GISJOIN` (NHGIS, historical codes kept) and `county_geoid`. Dictionary `README_county_decade.md` |

Results, run on 2026-10-23, for the 6,663 professor couples:

- **Wives aged 40-49:** 2.15 surviving children against 3.61 for all married
  men, and 2.57 ever born against 4.61.
- **Poisson IRR against all other married men:** 0.77 surviving and 0.73
  ever born, with wife age, marriage duration and state fixed effects.
- **Against other professionals:** no difference (1.03, CI 0.99-1.08).
- **Way 2 (own children under 10, men 25-54):** 0.66 of all men.
- **Late marriage:** duration standardisation narrows the ratio to all men
  from 0.57 to 0.68.

See `prep/occupational_fertility/README.md`.

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
| `mandatory_schooling/analysis_compulsory_schooling_amws_state_csdid.R` | Same C&S designs (annual pre20/pre40; five-year standard/alt × post20/post30) with the **state** as the unit: AMWS births, population and births summed over all valid county-periods, rates recomputed, states unweighted. Helpers in `state_csdid_helpers.R`, checks in `test_state_csdid.R`; env vars `COMPULSORY_AMWS_STATE_{WINDOW,CADENCE,PROFILE,BINNING,HORIZON}`. `WINDOW=early` (default): 1850-1880 laws, balanced grid, output `results/mandatory_schooling/compulsory_schooling_amws_state_csdid/{annual,period5/<conv>}/`. `WINDOW=all_events`: every adopter up to the donor horizon on a **ragged** grid, output `..._state_csdid_all_events/` — no state is never-treated and a donor must adopt 15+ years after the comparison year, so nothing after 1905 has any donor and 1905 has only MS; a one-donor cell has no standard error, so the horizon is the last period with ≥ 2 donor states (**1904 annual, 1900 standard bins, 1905 alt bins**) and cohort g is identified only to e = horizon − g. Pooled points average the cohorts identified there (`n_cohorts`/`n_units` columns; 22 target states annual pre20 falling to 14 at +20), later adopters stay donors, and late post cells rest on the (Southern) 1916-1920 adopters |
| `mandatory_schooling/analysis_compulsory_schooling_amws_region_csdid.R` | The volume's county C&S designs (annual pre20/pre40; five-year standard/alt pre20/pre40 at `post30`; unadjusted + both population controls) with **region × birth-year effects**: CLS South (region 3) vs the rest. Every target is non-Southern, so this compares with non-Southern donors only, which is the C&S region × time estimator. The panel is cut at the donor horizon, the last period with ≥ 3 non-Southern donor states (1887 annual, 1885 bins), and the pooled curve is **ragged**. Helpers in `region_csdid_helpers.R`, checks in `test_region_csdid.R`; env vars `COMPULSORY_AMWS_REGION_{CADENCE,PROFILE,BINNING,CONTROL}`; output `results/mandatory_schooling/compulsory_schooling_amws_region_csdid/{annual,period5/<conv>}/` |
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
- **Region × birth-year C&S** (`analysis_compulsory_schooling_amws_region_csdid.R`) means *non-Southern donors only*, because every 1850-1880 target is non-Southern under the CLS coding (DC counts as Northeast). Outside the South the last laws are IA/MD 1902, MO 1905 and DE 1907, so donors run out early. Comparisons stop at the last period with **≥ 3** donor states: 1887 annually, the 1885 bin on the five-year grid. Two states are not enough, because a single-state target cohort's IF sums to zero within its cluster and the two donor clusters' sums cancel, so the multiplier-bootstrap SE becomes a coin flip (seen at 1890 with DE + MO). Late points rest on few early adopters: annual e=+20 is MA, DC and VT (29 of 322 counties), and five-year e=+30 is MA alone. Read `pooled_composition.csv`. A census four-region split is infeasible: NE targets lose NE donors by e ≈ +5, and CA has no Western donor after e = 0.
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
- **County ATTENDANCE measure** (`output/mandatory_schooling/county_attendance/`, built 2026-09-21) — **prefer this over the bite measure**. A compulsory-*attendance* law obliges a child to be in school between two ages and says nothing about years completed, so attainment is a downstream proxy that is blind to the enrolment margin: if enforcement draws in marginal children who still leave at the exit age, attainment barely moves and a shortfall measure reads that as no bite. `SCHOOL` records attendance contemporaneously for **42,407,004 of 42,408,792** childhood rows, needs no 1940 linkage for the outcome (so 42.4 M observations, not 15.5 M), needs no `yob` convention, and makes 1930 usable. Non-attendance among legally-required children: **0.211 (1900), 0.063 (1910), 0.095 (1920), 0.065 (1930)**, against 0.403 above the exit age. Spec: `prep/mandatory_schooling/county_attendance_from_linked_census.md`.
- **The attendance measure should be rebuilt on the unlinked full counts, which the project does not hold.** It reads five variables off one childhood row and needs no linkage, so the MLP extract costs its only selection problem for nothing: median `coverage` runs **0.069 / 0.087 / 0.107 / 0.170** across 1900-1930 against a true school-age share near 0.26, so the linkage holds ~27% of a county's children in 1900 rising to ~65% in 1930 — a **2.4x gradient** that compounds the reference-period problem. Full counts would also give ~107 M children against 42.4 M. Needs a manual IPUMS request (1900-1930 100% databases, `AGE` 5-18 via Select cases, ~1.5-2.5 GB). Steps: `prep/mandatory_schooling/county_attendance_from_fullcount.md`.
- Attendance geography is **contemporaneous, not a proxy** — unlike the attainment measure, `COUNTYICP` is read off the same row as the outcome, so there is no candidate-year choice, no `selection_distance` and no assignment strata. Two residual mismatches: county of residence is not county of schooling, and the attendance question covers a window preceding enumeration. The second is bounded — of 7,100,615 children seen at school age in two censuses a decade apart, **77.6% are in the same county and 91.1% in the same state**, so within-window moves are ~2% for 1900's 12-month window. Treatment assignment (state) is more stable than unit assignment (county).
- Two things the attendance measure must respect. **`SCHOOL`'s reference period changes across censuses** (~12 months 1900, ~7.5 1910, ~4 1920 enumerated 1 January, ~6 1930), so the four years are NOT a time series and nothing is pooled or differenced across them. And **attendance swings 12% to 87% across ages 5-18**, so county rates must be age-standardised — use `not_attending_std_1013` (ages 10-13, inside the window for 1,762 of 1,808 law state-years), never the raw window rate. New York's Seneca County is 0.122 raw against 0.017 standardised.
- **The exit-age discontinuity needs the no-law benchmark**, which the attendance build carries in `attendance_state_year.csv`. Attendance falls with age regardless of any law: in 1900 the jump at the exit age is **0.191 in law states and 0.177 in no-law states**, so essentially none of it is the statute. In 1910 law states jump 0.226 against 0.137, a gap of 0.089, but the two groups differ in level (0.099 vs 0.166 below the exit age) so it is not causal. **After 1910 all 49 jurisdictions have a law and there is no benchmark at all**, so the 1920 and 1930 jumps of 0.328 and 0.295 must not be interpreted.
- **County bite measure** (`output/mandatory_schooling/county_bite/`, built 2026-09-21): for each 1940 student, completed schooling against what the law required, aggregated to the county they lived in at school age. **15,472,169 students** of 32.4 M with attainment get a county (47.8%). Six requirement bases - nominal/effective x actual/counterfactual, plus fixed 6 and 8 - because `ca_years == 0` means *no law bound that cohort* (231 of 1,372 cells), where `shortfall_rate` is 0 by construction; the actual bases are reported on law-bound students only and the counterfactual ones impute the state's later plateau. Headline `effective_cf` mean `shortfall_rate` **0.222**. Rank counties by `shortfall_rate`, not `gap_mean`: within a state x cohort cell the latter is attainment with a constant subtracted. Spec: `prep/mandatory_schooling/county_bite_from_linked_census.md`.
- **`ca_years > 0` is not exposure**, and treating it as the first exposed cohort is the easiest way to get a spurious result out of the bite measure. Across all 17 states with unexposed cohorts the first cohort with a positive requirement faces a mean of **0.94 years** against a mean plateau of **7.24**, and in 17 of 17 it is 0 or 1 year — behaviourally a pre-law cohort. Full exposure needs the child at or below the entry age when the law passed, `yob = T - entryage`; that equals the plateau cohort in only **9 of 17** states, since later tightening moves the plateau (Maryland +15, Missouri +8) and Arizona's repeal breaks both. There is no sharp cutoff — the ramp runs 8-13 cohorts — so use `required_effective` as a dose, not a boundary.
- **Never compare a counterfactual basis across the law's introduction.** `required_effective_cf` means the plateau before and the actual requirement after, so the yardstick swaps: for Alabama 1901 vs 1902 the threshold falls 6 -> 1 and `shortfall_rate` 0.282 -> 0.029, a 25-point "improvement" from a 0.08-year change in attainment. Hold the threshold fixed (`fixed6`/`fixed8`) or compare `mean_observed_years`. The county-level before/after contrast has no signal regardless: over 609 county pairs the SD of the change is 0.0484 against a 0.0454 binomial noise floor, a ratio of 1.07. See C6-C7 in `prep/mandatory_schooling/county_bite_from_linked_census.md`.
- Three traps the bite build surfaced. **`COUNTYICP` is not always the FIPS code times ten** - 0.032% of rows end in 5 (Oregon 605, Virginia 1875) and get a NULL `county_geoid` rather than being merged into a neighbour. **Linked ages disagree by +1 as the consistent case**, since `yob = 1940 - AGE - 1` sits a year below the earlier censuses' arithmetic; the modal `cand_age - recorded age` is +1 (57%), and 3.6% of candidates fall outside the school-age window on the cohort basis. **The 1930 assignment stratum is entirely cohort 1912 at age 18** (1.22% of students, all `selection_distance >= 5`) and should normally be dropped.
- **Never rank bite cells without `cell_usable`.** Four things make a cell unreadable and all are flagged, not filtered: `below_min_cell` (n < 25), `coverage_implausible` (**coverage > 1, arithmetically impossible** — the county was settled after the birth decade, so its births denominator is near zero while its students are school-age in-migrants), `coverage_thin` (< 0.05), `coverage_missing`. On the `effective_cf` county-stratum grain, of 8,770 estimable cells **193 have impossible coverage** (up to 17.7) and 93 are thin, leaving **7,933** usable at the 200-student ranking threshold. Unguarded, the ten lowest-bite cells are all settlement artefacts. `MIN_CELL` = 25 makes a cell estimable; `MIN_CELL_FOR_RANKING` = 200 makes it rankable — the two are not the same.
- `county_births_estimate_decade` in `us_panel_county_stem_1800.csv` carries **294 literal `NA` strings**, so readers type the column as text - cast with `TRY_CAST`. `GEOID` is typed inconsistently (text on the real file, numeric on any all-numeric subset), so join it as an integer on both sides, never as a zero-padded string.
- The MLP linked extract's CSV shipped with **no DDI**, and the CSV still lives in OneDrive (`~/OneDrive/Documentos/usa_00001.csv.gz`), not under `DET_DIR` like every other input. The DDI was fetched separately and sits at `Data/raw/mandatory_schooling/usa_00001.xml`: it now supplies value labels for 45 of 58 variables and `profile_ipums_linked_census.py` hard-fails if it stops describing the extract. `COUNTYICP` and `OCCSCORE` have codes but no names in the codebook — ICPSR county codes are state-dependent and meaningless without `STATEFIP`. Use `--labels-only` to re-render (seconds); the full pass is only for re-deriving the numbers. Nothing in the repo downloads the extract, so rebuilding it means re-submitting the spec recorded in `prep/mandatory_schooling/README.md` by hand.
- Occupational scores are scaled differently in the project's two IPUMS datasets. `PRESGL`/`ERSCOR50`/`EDSCOR50` carry the decimal **already applied** in the linked extract (`40.7`), but the 1940 full-count Parquet leaves the implied decimal in place (`407`) — a silent 10x gap if the two are compared. N/A codes also differ by variable: `ERSCOR50`/`EDSCOR50` use `999.9`, but **`PRESGL` uses `0`** (its observed max is 81.5).
- The MLP linked extract `usa_00001.csv.gz` is **not** a two-year 1900–1940 link: it stacks all five census years (1900 13.0M, 1910 24.0M, 1920 40.0M, 1930 70.2M, 1940 73.8M = 220,979,023 rows), one row per person per census, linked cases only. `HIK` is the cross-year person key; **`HISTID` is not** — it identifies a record within a year. See `prep/mandatory_schooling/merge_cls_to_linked_census.md` before joining anything.
- The CLS join is on **state of birth × birth cohort using the 1940 rows only** — that paper uses no record linkage at all. Childhood county via `HIK` is an extension it never did, and carries match-rate selection the original design does not.
- CLS vs Lingwall: 15 states are left-censored by the 1880 panel start; of the 34 comparable, 32 match exactly. Arizona (CLS 1880 in force vs Lingwall 1899) and **Washington (Lingwall 1878, CLS 1886 statewide)** disagree.
- Compulsory-schooling policy shape, which argues against coding adoption dates alone: entry age is near-static (24 of 49 states never changed it, **none ever raised it**) while exit age rose in 38 states — but exemptions gave back about half the tightening (nominal required window 6.31 → 8.90 years, effective 6.31 → 7.59). `effective_leaving_age` is the earliest age any statutory route out opens — the child-labour work permit (`max(workage, entryage + workyrs)`) or the early-dropout exemption (`max(earlyyrs_condition, entryage + earlyyrs)`), whichever comes first, capped at `exitage`; `binding_exemption` records which governs. It assumes continuous attendance from the entry age, matching CLS's own `ca_years2` counter. Use `ca_years_combined`, not `exitage - entryage`.
- Post-1900 adoption variation is almost entirely Southern: 16 states first adopt in 1900 or later and 11 are the CLS-coded South, clustered 1913–1920. Combined with the region collinearity already noted for attendance, this makes Southern adoption a weak identification base — CLS's own `region!=3` robustness moves the IV estimate only from 0.067–0.077 to 0.064.
- **1910 fertility extract: `D:/usa_00003_ddi.xml` is not its codebook** — that DDI belongs to the 1900–1950 linked extract. Use `D:/ipums_1910_birth_occupation_ddi.xml` (usa_00009). `CHBORN`/`CHSURV` are **offset by one** (00 = N/A, 01 = none, k+1 = k children) and asked of ever-married women only, so a man's count is his wife's via `SPLOC`; her count includes children by every husband and `MARRNO` is not in the extract. Female "professors" (4,698, 29% of the code) are 87% never married and 22% under 25 — `OCC1950` 029 likely sweeps in music and dancing teachers, so do not read the female line as college faculty. In DuckDB, never name a decoded column `chborn` next to raw `CHBORN`: identifiers are case-insensitive and the raw code is read silently.
- **1850-1910 `NCHILD` extract (usa_00010):** `MARST` is blank before 1880 (married = `SPLOC` > 0 in every year), `CHBORN`/`CHSURV` before 1900, `OCC` in 1900-1910; relationships before 1880 are IPUMS-imputed, so `NCHILD` is too; 1890 does not exist. **`HISTID` is not unique in 1870** (331 IDs on two distinct records each): key on `SERIAL`×`PERNUM`. The professor code is not stable: 1910 has 3.3x the professors of 1900 and a 29% female share. `NCHILD` understates the professor gap in completed fertility by ~0.08-0.10 (1900/1910 calibration against the wife's `CHSURV`).
- **County professor-fertility dataset (`county_decade/`):** counties are contemporaneous, not harmonised across censuses; most counties with any professor have 1-3 (only 30 counties with 5+ in 1850, 380 in 1910), so weight by `n_prof` or pool before reading a county ratio. `GISJOIN = G + %02d state + 0 + %04d COUNTYICP` keeps historical counties (code ending in 5) that `county_geoid` drops; 7-25 county codes per census are not in `nhgis0001_ts_nominal_county.csv` and are flagged `in_nhgis_crosswalk = FALSE`, not dropped. Against their own county's men professors sit higher (0.71-0.89) than against national rates (0.67-0.84): part of the national gap is that professors lived in low-fertility counties.
- **DuckDB join-plan trap:** a non-equality predicate in a `LEFT JOIN ... ON` clause (e.g. `AND c.POPLOC > 0`) makes DuckDB use a blockwise nested-loop join instead of a hash join; on a full census the professor-children build ran 6+ hours instead of seconds. Keep `ON` clauses to equalities and filter in `WHERE`.
- **Census professor counts are not comparable across years** (`OCC1950` 012-029): coverage of real college faculty is 0.24 (1870), 0.42 (1880), 0.24 (1900), 0.40 (1910), 0.43 (1920), 0.45 (1930), 0.50 (1940), 0.63 (1950) against NCES male staff, so counts jump with coding, not growth. NCES has no 1929-30 professional staff: the 1930 benchmark is estimated from instructional staff (`nces_estimated`). The coverage is **not uniform across counties** either: a county's coded professors can swing 8 -> 173 between censuses, so the national factor fixes levels but not county noise. For men it is coverage, not composition. Use `faculty_factor` from `output/occupational_fertility/professor_coverage_factors.csv` (by the census a count is read from) for any count compared across censuses; NCHILD means need no factor. Presidents/deans (010) are folded into 029 in 1940, so count 010-029. `IND1950` cannot reclassify (888 for ~99% of professors in every year).

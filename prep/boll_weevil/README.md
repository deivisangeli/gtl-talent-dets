# Boll weevil county treatment data

Builds county treatment timing and baseline cotton intensity from the supplied
Baker, Blanchette, and Eriksson replication files. This recovers deposited
county data; it does not reconstruct the linked census-person sample or merge
GTL talent outcomes.

## Run

From the repository root in PowerShell:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
Rscript prep/boll_weevil/build_boll_weevil.R
```

The script resolves the repository from its own `--file` path and sources
`paths.R`. It can therefore run from other working directories using an absolute
script path. Dependencies: `haven`, `readr`, `dplyr` (1.1 or later), and `tidyr`.
When sourcing interactively, set `GTL_REPO` or use the repository root as the
working directory. The script does not edit `paths.R` or any input.

## Inputs and provenance

Relative to `TALENT_DETS_DATA_DIR`:

| Input | Use |
| --- | --- |
| `Data/raw/boll weevil/BollWeevilByCensusYear.dta` | 4,204 county-vintage records: 971 in 1900, 1,047 in 1910, 1,083 in 1920, and 1,103 in 1930 |
| `Data/raw/boll weevil/1889AGCensusCottonData.dta` | 1,028 baseline agricultural records |
| `Data/raw/boll weevil/rose_exp.dta` | Rosenwald exposure; preserved and checksummed, not used |
| `input/nhgis0001_ts_nominal_county.csv` | Existing project reference for checking county codes and names |

Archive citation: Baker, Richard B., John Blanchette, and Katherine Eriksson.
*Long-run Impacts of Agricultural Shocks on Educational Attainment: Evidence
from the Boll Weevil (Data)*, deposited December 2, 2019.
[DOI: 10.3886/E115906V1](https://doi.org/10.3886/E115906V1).

The working-paper reference is NBER Working Paper 25400 (December 2018),
available locally at `Lit/Schooling/baker_2018.pdf`. Sections 3.1 and 4.2 give
the timing and cotton-intensity definitions. The files were supplied locally
by the user. Their original download dates and identity against the archive
were not independently verified; the manifest deliberately leaves retrieval
dates missing. MD5 checksums identify the exact local inputs used.

## Outputs

All outputs are CSVs under `output/boll_weevil/`. Missing values are written as
`NA`. Read `GEOID`, `candidate_GEOID`, `statefp`, `countyfp`, and
`source_county_id` as character columns to preserve leading zeros and identifiers.

| File | Contents |
| --- | --- |
| `boll_weevil_counties_1920.csv` | Canonical table: 1,083 counties, including excluded counties and missing treatment dates |
| `boll_weevil_county_year_1892_1922.csv` | Balanced 1,083 x 31 panel: 33,573 county-years |
| `boll_weevil_source_all_geographies.csv` | All 4,204 infestation source rows with their distinct geography vintages |
| `boll_weevil_data_dictionary.csv` | Definitions and R storage types for every field in the data and match tables |
| `boll_weevil_source_variable_map.csv` | Original Stata labels and field mappings |
| `boll_weevil_source_manifest.csv` | Input paths, sizes, modification timestamps, checksums, and provenance limitations |
| `boll_weevil_geographic_match_audit.csv` | Candidate/verified codes, reference names, and match decisions for every county-vintage row |
| `boll_weevil_agricultural_match_audit.csv` | Agricultural joins, original geography flags, acreage, and missing ratios for 1920 counties |
| `boll_weevil_missingness_audit.csv` | Missing-value counts for every field in the data and match tables |
| `boll_weevil_sample_count_reconciliation.csv` | Deposited-data counts versus reported/implied 2018 working-paper counts |
| `boll_weevil_validation_checks.csv` | Build-time checks with observed values, expected values, and pass/fail |

Re-running replaces these generated files. Validation occurs before output
writing; source hashes are checked again before publishing outputs.

## Treatment definitions

- `bw_first_arrival_year`: deposited first partial or full infestation date.
- `bw_final_through_year`: deposited permanent full-infestation date through 1922.
- `bw_average_year`: deposited rounded-up midpoint of those two dates. All
  available source midpoint dates equal `ceiling((first + final) / 2)`.
- `bw_full_retreat` and `bw_part_retreat`: deposited flags, unchanged.
- `cotton_intensity_1889`: `cottonac / acimp`, where `acimp` is **improved**
  farm acreage. Total farm acreage (`farmarea`) is retained but is not the
  denominator. Nonpositive denominators, negative cotton acreage, or missing
  inputs produce a missing ratio. Genuine zero cotton acreage remains zero.

The annual panel contains `event_time_* = year - treatment_date` and
`derived_post_* = 1(year >= treatment_date)`. Missing dates produce missing
event times and indicators, rather than labeling those counties never treated.
These are absorbing treatment indicators, including for counties with retreat
flags. They are **not observed annual free/partial/full infestation status**.
The supplied files do not contain that underlying annual history, so it cannot
be reconstructed from the timing summaries alone.

`bd`, `cga1890_1920`, and `cga1890_1920_nm` are preserved verbatim. Their precise
construction is undocumented in the supplied files. Fractional acreage in
source records is preserved; the builder does not recompute historical acreage
allocations or use these flags as additional sample restrictions.

## County identifiers and geography

The source `fipscode` includes an offset of 100,000: for example, 101001 yields
candidate code `01001`. The source value remains available as
`source_fipscode` and the character key `source_county_id`.

A candidate becomes `GEOID` only after its code, state, and normalized county
name match the project's NHGIS reference. Normalization ignores case,
punctuation, common jurisdiction suffixes, and Saint/St spelling. Two explicit
Louisiana variants are documented in the code and audit:

- `22095`: deposited `ST JOHN THE BAPTI` truncates St. John the Baptist Parish.
- `22113`: deposited `VERMILLION` corresponds to Vermilion Parish.

For 1920, 1,079 rows pass ordinary normalization, two pass these explicit name
variants, and two remain unverified: Campbell and Milton, Georgia. Their source
identifiers and candidate codes (`13041` and `13203`) remain in the table, but
their `GEOID` values are missing. They are not reassigned to Fulton County.

Verified codes identify a nominal county correspondence; **they do not
harmonize 1920 boundaries to modern boundaries**. Later GTL outcome merges must
account for geographic changes. Other source geography vintages remain separate
and are never pooled into the canonical 1920 table. The all-vintage source
includes one missing source code for Day, Oklahoma, in 1900; that record is
preserved and flagged, not assigned an invented code.

The agricultural join uses the original source code. It matches 1,019 of the
1,083 canonical counties, with exact state/county name agreement on all matches.
All 64 unmatched canonical rows are in Oklahoma; the agricultural file also
contains nine records outside the canonical 1920 county universe. Missing
agriculture is never filled from another county or another geography vintage.

## Sample flags and the working-paper discrepancy

These are transparent county-level rule flags, not claims to reproduce the
authors' actual linked-person estimation sample.

| Stage | Deposited files, 1920 geography | 2018 working paper |
| --- | ---: | ---: |
| Complete infestation date recorded | 843 | 844 |
| Exclude full retreats among those counties | 120 | 120 |
| Remaining after that exclusion | 723 | 724 (implied) |
| Exclude arrival-to-completion gaps over four years | 38 | 38 |
| `eligible_timing` | 685 | 686 (implied) |
| Exclude Missouri: `eligible_baker_2018_rules` | 681 | Not directly reported at this stage |
| Also require cotton intensity | 677 | Not directly reported at this stage |

The paper's 13-Southern-state restriction excludes Missouri. The deposited
data contain 14 states, including Missouri. The 38 long-gap exclusions comprise
30 counties in Texas, six in Arkansas, and two in Oklahoma, agreeing with the
paper. The deposited universe and the paper's stated Cotton Belt universe are
not identical; the supplied files do not establish why one fewer fully infested
county appears in the deposited 1920 geography. Counts are not forced to match.

Four Southern timing-eligible counties lack usable cotton intensity: Dimmit and
Webb, Texas (missing cotton acres), and Mayes and Rogers, Oklahoma (unmatched
agriculture). These remain in the county table with missing ratios.

## Validation

The builder checks source schema/key integrity, 1920 and panel row counts,
date ranges and order, all source midpoint calculations, agricultural matches
and names, sample counts, ratio bounds, and missing-date/indicator behavior.
The validation CSV records 16 checks. Invalid or changed source data cause the
build to stop before rewriting outputs.

Initial execution also passed an independent Python verification of all
deposited 1920 treatment dates and retreat flags, every annual event time and
treatment indicator, cotton ratios, eligibility flags, unique keys, balanced
panel coverage, and unchanged input checksums.

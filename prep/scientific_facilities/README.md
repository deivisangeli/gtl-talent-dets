# Scientific Facilities Prep Pipeline

Prepare the legacy county/HYDE panels used by scientific-infrastructure analyses.

## Kantor-Whalley (2019) station inventory

`build_kantor_whalley_2019_stations.R` rebuilds the 23 stations in appendix Table A1
(PDF page 35 / printed page 71) with sourced station/campus reference coordinates
and independently validated 2020 county FIPS codes. It does not impute opening
dates or reproduce the authors' county-seat-based distance variables.

From any working directory, set the local Dropbox project root explicitly:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
Rscript C:/Users/megaj/repos/gtl_talent_dets/prep/scientific_facilities/build_kantor_whalley_2019_stations.R
```

Dependencies: R packages `sf` and `data.table`. Inputs are the immutable appendix
in `Data/raw/scientific_facilities/`, reviewed transcription/geography/snapshot
files in `Data/intermediary/scientific_facilities/`, the cached 2020 county
shapefile under `raw/tigris/`, `output/national_county2020.txt`, and the existing
combined annual AMWS panel. Rebuilding is offline; the curated inputs remain in
Dropbox. To revise a geographic decision, edit the lookup and its evidence notes,
then rerun the builder; numeric GeoNames changes also require the source snapshot.

Outputs live in `Data/processed/scientific_facilities/`:
`kantor_whalley_2019_stations.csv`, its field dictionary, validation report,
input-checksum manifest, and methodology README. Read `GEOID` as a string.
The published county is retained separately: Mountain Grove is reported as Texas
County but independently located in Wright County (`29229`). Coordinates are
campus/station-scale historical proxies, with precision and source notes per row.

These scripts are separated because they are tied to the broader-determinants scientific facilities exercises. Shared population scripts that are not specific to this pipeline remain in `prep/`.

## University-associated stations: June 30, 1910 snapshot

`build_usda_1910_stations.R` rebuilds the national confirmed inventory, candidate
audit, historical-site geography, 51-jurisdiction coverage checks, and allocation
matches for the full roster, curated pool, and saved standard/shifted main samples.
It preserves the Kantor-derived dataset above. It does not change any analysis.

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' 'C:/Users/megaj/repos/gtl_talent_dets/prep/scientific_facilities/build_usda_1910_stations.R'
```

The script resolves the repository root, sources `paths.R`, and runs offline with
`sf` and `data.table`. Research inputs are under Dropbox
`Data/intermediary/scientific_facilities/usda_1910/`; unchanged USDA scans and
research source captures are under `Data/raw/scientific_facilities/usda_1910/`.
Outputs begin `usda_1910_university_agricultural_stations` in
`Data/processed/scientific_facilities/`. The generated README documents every
output, evidence rules, location/date caveats, and percentage denominators.

This release is a **confirmed inventory with explicitly incomplete branch
coverage**, not a certified exhaustive census. Read `_jurisdiction_coverage.csv`,
`_unresolved_candidates.csv`, and `_unresolved_geography.csv` before coding
historical absence. All pool percentages are labeled **with a qualifying station
in 1910**, confirmed matches only; missing matches are not verified zeros.
Dates retain source precision, and organization under the Hatch Act is kept
separate from legal establishment and first operation. County FIPS use 2020
geography and must be read as text.

## Station finances in founding decades

`build_station_founding_decade_finances.R` publishes the separate financial
research files for the 15 confirmed station identities at the 10 universities
in the main event-study samples. It reads curated records and archived source
tables offline and preserves the two inventories and all analyses above.

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' 'C:/Users/megaj/repos/gtl_talent_dets/prep/scientific_facilities/build_station_founding_decade_finances.R'
```

Requires `data.table`. Curated inputs and visually reviewed financial pages live
under `Data/intermediary/scientific_facilities/station_finances/`; archived PDFs
live under the corresponding `Data/raw/` directory. Outputs begin
`station_founding_decade_finances` in `Data/processed/scientific_facilities/`.

**This is an initial archival audit, not a completed budget-share dataset.**
The current inputs contain no fully reconciled annual station/university pair.
The annual audit retains reported amounts, source pages, fiscal periods,
capital/carryover/transfer classifications, and specific missing-data reasons.
University medians and ranges therefore remain missing. Missing shares do not
mean zero spending. Three provisional search slots per station document the
requested collection; they do not establish that each year's accounts were
available or completely reviewed.

Do not use the 1889 federal directory's annual funding rates as realized
full-year receipts, divide a biennial appropriation by two, or align university
and station accounts by report year alone. New Hampshire's reported college
income explicitly includes station funds; Illinois records an internal station
transfer and different fiscal year ends; LSU combines networks and fiscal
periods; Clemson Coast's continuing-identity founding decade remains unresolved.
The generated methodology README gives account-specific evidence and the
remaining research work. `pair_adjustments_curated.csv` is initially empty:
future accepted shares require sourced line-level consolidation terms and
documented reconciliation, followed by distinct earliest/midpoint/latest
selection. Expenditure and current-revenue shares stay separately labeled.

### Annual funding components and reduced financial file

After rebuilding the financial audit, run the exporter with the same explicitly
configured `TALENT_DETS_DATA_DIR`:

```powershell
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' 'C:/Users/megaj/repos/gtl_talent_dets/prep/scientific_facilities/export_station_finance_key_columns.R'
```

This offline step publishes `station_founding_decade_finances_annual_funding.csv`
(nine 1889 funding accounts), a field dictionary and methodology, and updates
`station_founding_decade_finances_annual_key_columns.csv` with separate federal,
state-appropriation, fertilizer-tax and farm-proceeds fields. It uses the curated
`annual_funding_components_curated.csv` lookup and reconciles every component to
the source lines and existing reported totals. Additional reported funding was
already included in those totals; it must not be added twice. Blank categories
mean not reported, not zero. The table's footnote e identifies Purdue's extra
$2,000 as farm proceeds; the corrected source label is recorded in
`funding_source_corrections_curated.csv`. Source pages were reviewed again on
2026-10-23. Existing numerator/denominator values and unresolved-share decisions
are preserved; Cornell's 1889 funding remains outside its founding decade.

## University expenditure in station founding decades

`build_university_founding_decade_expenditures.R` publishes a separate expenditure
dataset for the same 10 universities and 15 station identities. It sources
`paths.R` and runs offline with `data.table` and `digest`.

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' 'C:/Users/megaj/repos/gtl_talent_dets/prep/scientific_facilities/build_university_founding_decade_expenditures.R'
```

Curated transcriptions, signed calculation terms and visually checked pages are
under Dropbox `Data/intermediary/scientific_facilities/university_expenditures/`.
New source documents and original Purdue page images are under the matching
`Data/raw/` directory; previously archived PDFs are reused unchanged from
`station_finances/`. Outputs start `university_founding_decade_expenditures` in
`Data/processed/scientific_facilities/`.

**Coverage is incomplete.** Purdue FY1889 has a reconstructed whole-university
expenditure total. Illinois, Texas and Clemson currently have verified spending
for explicitly limited university accounts. Use `total_university_expenditure_usd`
for a whole-university denominator; `reported_account_expenditure_usd` may be
partial. Operating-only university expenditure stays missing when the records
do not support a complete split. Read the coverage table and methodology before
using these data. The station crosswalk has three selection slots per station,
with missing slots and reasons preserved; no multiyear total is annualized.
This builder preserves the existing station-finance outputs, station inventories
and all event-study analyses, and does not calculate station shares.

## Main Order

1. HYDE raster helpers as needed:
   - `download_hyde.R`
   - `hyde_data.R`
   - `coding_hyde.R`
   - `cleaning_hyde.R`
2. County panels:
   - `cleaning_county.R`
   - `cleaning_county_stem.R`
3. HYDE cell panel:
   - `cleaning_us.R`

## Inputs

- `DATA_INPUT/cross-verified-database.csv`
- `DATA_INPUT/nhgis0001_ts_nominal_county.csv`
- `DATA_INPUT/hyde_pop_asc/`
- `prep/raw_paths.R` for legacy scripts that use `DET_DIR`

## Outputs

- `DATA_OUTPUT/us_panel_county.csv`
- `DATA_OUTPUT/us_panel_county_stem.csv`
- `DATA_OUTPUT/us_panel.csv`
- `DATA_OUTPUT/us_panel_fixed.csv`

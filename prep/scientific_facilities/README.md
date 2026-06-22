# Scientific Facilities Prep Pipeline

Prepare the legacy county/HYDE panels used by scientific-infrastructure analyses.

These scripts are separated because they are tied to the broader-determinants scientific facilities exercises. Shared population scripts that are not specific to this pipeline remain in `prep/`.

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

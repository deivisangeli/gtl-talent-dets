# AMWS Prep Pipeline

Prepare American Men and Women of Science data for county-year outcomes.

Run scripts from the repository root or from `prep/`; the scripts resolve the repository root from `--file`/`GTL_REPO` and load `paths.R`.

## Main Order

1. `cleaning_amws_1906.R`, `cleaning_amws_1938.R`, `cleaning_amws_1955.R`
2. `geocode_amws_edition.R` for 1906/1938 and `geocode_amws_1955_us.R` for 1955
3. Manual/audit helpers as needed: `prep_amws_edition_batches.R`, `aggregate_amws_edition_fix.R`, `prep_manual_fix_batches.R`, `aggregate_manual_fix.R`, `prep_audit_batches.R`, `aggregate_audit.R`
4. `dedup_amws_editions.R`
5. `build_amws_county_year.R` and `build_amws_combined_county_year.R`

## Inputs

- `AMWS_INPUT`: raw AMWS editions in Dropbox input.
- `DATA_INPUT`: geocoding references such as Census Gazetteer and GeoNames.

## Outputs

- `AMWS_OUTPUT`: cleaned, geocoded, audited, and deduplicated AMWS files.
- `DATA_OUTPUT/us_panel_county_amws_combined_year.csv`: county-year AMWS panel merged onto the yearly Wikipedia county panel.

# AMWS Prep Pipeline

Prepare American Men and Women of Science data for county-year outcomes.

Run scripts from the repository root or from `prep/`; the scripts resolve the repository root from `--file`/`GTL_REPO` and load `paths.R`.

## Main Order

1. `cleaning_amws_1906.R`, `cleaning_amws_1938.R`, `cleaning_amws_1955.R`
2. `geocode_amws_edition.R` for 1906/1938 and `geocode_amws_1955_us.R` for 1955
3. Manual/audit helpers as needed: `prep_amws_edition_batches.R`, `aggregate_amws_edition_fix.R`, `prep_manual_fix_batches.R`, `aggregate_manual_fix.R`, `prep_audit_batches.R`, `aggregate_audit.R`
4. `dedup_amws_editions.R`
5. `build_amws_county_year.R` and `build_amws_combined_county_year.R`

## AMWS 1986 / Edition 16

Active cleaning scripts for the 1986 edition live in `amws_1986/`.
The old filenames in `prep/amws/` are compatibility wrappers that call the
new locations.

Main 1986 order:

1. `amws_1986/01_run_regex_all_docx.R`
2. `amws_1986/02_build_birth_year_city.R`
3. `amws_1986/03_expand_multi_entry_rows.R`
4. `amws_1986/04_apply_expanded_all_corrections.R`
5. `amws_1986/05_apply_location_format_manual_corrections.R`
6. `amws_1986/06_apply_birth_year_manual_corrections.R`
7. `amws_1986/07_geocode_us_birthplaces.R`
8. `amws_1986/08_build_county_year_panel.R`
9. `amws_1986/09_plot_births_by_year.R`

All-country post-geocode correction:

- `amws_1986/10_apply_piloted_regex_corrections_all_countries.R` applies the
  piloted regex corrections to
  `AMWS_OUTPUT/regex_all_docs/amws_ed16_all_countries_geocoded_us_only.csv`,
  preserving non-US rows and refreshing US geocodes only where corrected
  location keys changed. It also applies two explicit manual exclusions for
  implausible/corrupted birth-year parses and writes the final AMWS 1986
  distribution copies:
  - `Data/intermediary/amws/amws_ed86_full.csv` and `.xlsx`: full 41-column
    output, including parsing/geocoding diagnostics.
  - `Data/processed/amws/amws_ed86.csv` and `.xlsx`: 21-column processed
    version for analysis and sharing.
  - `Data/processed/amws/amws_ed86_filtered.csv` and `.xlsx`: processed
    version restricted to rows with nonempty `birth_city` and `birth_year`.

Auxiliary manual-review, audit, and diagnosis scripts are under
`amws_1986/support/`. Pilot and superseded scripts are under
`amws_1986/archive/`.

## Inputs

- `AMWS_INPUT`: raw AMWS editions in Dropbox input.
- `DATA_INPUT`: geocoding references such as Census Gazetteer and GeoNames.

## Outputs

- `AMWS_OUTPUT`: cleaned, geocoded, audited, and deduplicated AMWS files.
- `DATA_OUTPUT/us_panel_county_amws_combined_year.csv`: county-year AMWS panel merged onto the yearly Wikipedia county panel.
- 1986 outputs keep the existing Dropbox paths, including
  `AMWS_OUTPUT/regex_all_docs/`, `AMWS_OUTPUT/amws_1986_county_year.csv`,
  and `DATA_OUTPUT/us_panel_county_amws_1986_year.csv`.

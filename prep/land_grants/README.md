# Land Grants Prep Pipeline

This pipeline builds the Andrews college site-selection county-pairs workbook used by the land-grants analyses and by the AMWS county-pairs robustness scripts.

## Input

- Dropbox raw file: `raw/andrews_2023_appendix_table_a1.xlsx`
- Source sheet: `table_a1`
- External reference: Census county gazetteer `2025_Gaz_counties_national.txt`

The script downloads the Census gazetteer to `raw/` if it is missing.

## Output

- Dropbox raw file: `raw/andrews_2023_county_pairs_long.xlsx`

The output keeps the historical `county_pairs` filename because existing analysis scripts consume it directly.

## Run

From the repo root:

```r
Rscript prep/land_grants/01_build_andrews_county_pairs_long.R
```

The output workbook contains:

- `county_pairs_long`: one selected county and one runner-up county per row.
- `source`: provenance metadata, matching rule, and row counts.

Runner-up counties are matched only within the row's state. Unmatched rows are kept with missing coordinates and `runner_up_match_status == "unmatched_not_in_state"`.

## Longitudinal faculty panel

The `faculty/` subdirectory expands the first-search opening/transition faculty
rosters into an annual instructional-faculty panel for the 57 selected Andrews
colleges through academic year 1950-51. It initializes 19 fixed research
batches of exactly three institutions, preserves the 338 legacy faculty rows,
validates independently staged research, and builds person-year and
college-year outputs only from batches accepted by the primary reviewer.

Run from the repository root with `TALENT_DETS_DATA_DIR` set:

```r
Rscript prep/land_grants/faculty/01_initialize_faculty_panel.R
Rscript prep/land_grants/faculty/03_prepare_legacy_seed.R
Rscript prep/land_grants/faculty/validate_faculty_scaffold.R
Rscript prep/land_grants/faculty/02_validate_and_merge_faculty_batches.R
Rscript prep/land_grants/faculty/04_build_faculty_person_year.R
```

See `faculty/research_protocol.md` for source priority, inclusion rules,
coverage statuses, and staged-agent handoff requirements.

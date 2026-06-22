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

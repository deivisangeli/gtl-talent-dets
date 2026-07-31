# Elite Schools Prep Pipeline

Build the curated elite high school treatment files.

This pipeline is separate from mandatory schooling. Elite schools are selective, historically accessible secondary schools; mandatory schooling is a state-law treatment.

## Main Order

1. `build_elite_high_schools_national.py`
2. Optional enrollment research helpers:
   - `prep_enrollment_research_batches.R`
   - `aggregate_enrollment_research.R`

## Inputs

- `SCHOOLS_INPUT`: manual school list, revision additions, revision decisions, review batches, and queue files.
- `SCHOOLS_INPUT/elite_high_schools_founding_county_overrides.csv`: audited overlay for founding geography. The builder applies this file after reading the manual school lists, so the original raw/manual files are never edited. Rows marked `exclude_from_core` remain auditable in the national and robustness outputs but are excluded from the core output.
- `DATA_OUTPUT/national_county2020.txt`
- `DATA_OUTPUT/us_panel_county.csv`

## Outputs

- `SCHOOLS_OUTPUT/elite_high_schools_national_1800_1930.csv`
- `SCHOOLS_OUTPUT/elite_high_schools_core_1800_1930.csv`
- `SCHOOLS_OUTPUT/elite_high_schools_expanded_1800_1930.csv`
- `SCHOOLS_OUTPUT/elite_high_schools_robustness_only_1800_1930.csv`
- `SCHOOLS_OUTPUT/elite_high_schools_state_coverage_1800_1930.csv`

The output keeps both the school-list geography (`school_city`, `school_county_name`) and the audited treatment geography (`founding_city`, `founding_county_name`, `founding_county_geoid`). `county_geoid` is retained as a backward-compatible alias of `founding_county_geoid` for analysis scripts.

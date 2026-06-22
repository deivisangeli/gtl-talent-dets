# National Elite High Schools Draft

This note documents the national school list and the current admissions/access coding now attached to it.

## Outputs

- [elite_high_schools_national_1800_1930.csv](/abs/path/C:/Users/deivi/github/gtl-talent-dets/prep/output/elite_high_schools_national_1800_1930.csv)
- [elite_high_schools_state_coverage_1800_1930.csv](/abs/path/C:/Users/deivi/github/gtl-talent-dets/prep/output/elite_high_schools_state_coverage_1800_1930.csv)
- [elite_high_schools_national_manual.csv](/abs/path/C:/Users/deivi/github/gtl-talent-dets/prep/input/elite_high_schools_national_manual.csv)
- [build_elite_high_schools_national.py](/abs/path/C:/Users/deivi/github/gtl-talent-dets/prep/elite_schools/build_elite_high_schools_national.py)

## What Changed

- Expanded the search from the original 10-state seed to all 50 states.
- Added merge-ready county fields:
  - `county_name`
  - `county_geoid`
  - `lat_county`
  - `lon_county`
- Added continuity flags so later mergers are not mistaken for continuous 19th-century institutions.
- Replaced the earlier admissions/access placeholders with usable first-pass coding fields for:
  - current selectivity model
  - whether admissions are test-based
  - current access for lower-income applicants
  - historical access for talented but poor students
  - evidence strength and source URLs

## Interpretation

This is a national draft, not a final census.

- `continuous` and `continuous_renamed` rows are the cleanest cases.
- `reorganized_continuity` rows preserve long institutional continuity but require care when interpreting the "founding" year.
- `later_merger_use_roots` rows are included because their elite lineage clearly predates 1930, but the current institution is a later merger.

## Admissions/Access Coding

The output now includes these working variables:

- `admission_selectivity_current`
- `test_based_admissions_current`
- `poor_access_current`
- `poor_access_historical`
- `historical_access_note`
- `historical_access_evidence_level`
- `current_admissions_source_url`
- `current_access_source_url`
- `historical_access_source_url`
- `admissions_access_coding_status`

The logic is intentionally transparent rather than pretending to be fully hand-coded.

- `admission_selectivity_current` distinguishes `exam_only`, `exam_plus`, `grades_test_combo`, `lottery`, `open_access`, `sending_town_open`, and `holistic_private`.
- `test_based_admissions_current` is `yes` only when an exam or explicit test screen is part of the current gatekeeping model.
- `poor_access_current` is a coarse current-period access code:
  - `high` for tuition-free public/selective schools and Vermont-style town-tuition academies
  - `medium` only for cases with stronger direct evidence of unusual affordability within an otherwise tuition-charging model
  - `low` for the default elite private tuition model, even when some ordinary need-based aid exists
- `poor_access_historical` is the main historical access code for whether talented but poor students had a realistic route in:
  - `high` for public tuition-free models, tuition-free scholarship schools such as Regis, and schools explicitly founded for poor boys or with town-tuition/public missions
  - `medium` for mixed public-tuition models that still left substantial price barriers
  - `low` for standard elite private pay schools
- `historical_access_evidence_level` distinguishes direct source-backed cases from broader rule-based coding.
- `admissions_access_coding_status` shows whether a row is fully source-checked, partly source-checked, or mostly rule-based.

This means the file is usable now, but not all rows have the same evidentiary weight.

- Schools with official admissions and aid pages linked directly in the CSV are the cleanest current-period cases.
- Public schools are often coded as historically accessible by rule because tuition barriers were low, even though sex, race, religion, geography, and exam filters still mattered.
- Private boarding/day schools are usually coded as historically low-access unless there is direct evidence of a tuition-free or poor-serving mission.

For empirical use, the safest pattern is to treat `poor_access_historical` and `test_based_admissions_current` as usable draft variables while retaining `historical_access_evidence_level` or `admissions_access_coding_status` for robustness checks.

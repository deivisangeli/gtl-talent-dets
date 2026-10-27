# County-level training-location outcomes: current requested definition

The 2026-10-14 `amws_county_training_location_20261014` release uses a target
county c and birth cohort. It classifies the full eligible US-born AMWS sample
by training distance to c's centroid, regardless of birthplace. The previous
birth-origin outcomes and pooled catchment releases are preserved as separate
analyses; they do not implement this revised definition.

An entry is ever nearby when any assigned BA/BS, MA, MS or PhD institution is
within the inclusive 50 km (or 100 km) radius. Never nearby requires at least
one reported award, complete conflict-free matching of every reported award,
and all locations outside the radius. All remaining entries are unknown.
Known nearby takes precedence over other unmatched awards. Conditional identity
assignments and unconditional assignments with missing graduation years remain
unchanged. Time is birth cohort, not graduation cohort.

Each entry counts once in each target county. The categories partition the same
full eligible cohort in every county. The rate is `1000 * count / county births`.
There are no pooled catchments or shared-county exclusions. The national cohort
numerator is common across counties for the total benchmark, so its county
variation arises from the birth denominator. Entries recur across county
outcomes by construction; original inference conventions are retained.

The source has 136,489 entries; 130,730 satisfy the retained year, `kept`, US-birth
and contamination rules. Of these, 118,370 were born outside the 168 original
allocation counties. All have birth-county codes in this source, but a missing
birth-county code would not exclude an otherwise eligible entry. Evidence
contains 286,464 qualifying awards and 1,234 distinct assigned institutions.

Both radii use exactly the original county samples. Relative-time standard:
16 events, 52 units, 650 rows; alternative: 20 events, 62 units, 834 rows.
Calendar standard and alternative: 15 events, 46 units, 598 rows each, including
East Carolina, with 13 complete calendar decades 1830–1950. Relative support is
balanced −20 to +70; calendar support varies over −50 to +70. Displayed estimates
and influence functions are shifted to e=0. Baseline adjustment uses own-county
estimated births at g−10. All 32 models estimated successfully; maximum category
effect decomposition error is 2.5012e−12. Total effects and inference coincide
across radii. Warnings, joint pre-trend tests, rank diagnostics and native support
are saved without estimator changes. Reference locations do not verify historical
teaching campuses, and existing provisional institution identities are retained.

Run from the repository root with `TALENT_DETS_DATA_DIR` set:

```powershell
Rscript --vanilla prep/land_grants/08_build_amws_county_training_location.R
Rscript --vanilla analysis/land_grants/training_50km/run_county_training_location.R
Rscript --vanilla analysis/land_grants/training_50km/validate_county_training_location.R
python analysis/land_grants/training_50km/build_county_training_companion.py
```

Preparation is in `output/land_grants/amws_county_training_location_20261014/`.
Models, coefficients, sample panels, inference, support and warnings are in
`results/land_grants/event_studies/amws_county_training_location_20261014/`.
The 12-page figure-only companion, rendered pages, numerical validation and
publication audit are in
`results/land_grants/companions/amws_county_training_location_20261014/`.
Checkpoint files remain under `~/.codex/amws_checkpoints/`.

`scientist_county_classifications.rds` contains explicit source-key and target
county vectors and integer matrices for both radii: 1=ever, 2=never, 3=unknown.
An independent sparse scientist/institution incidence calculation validates all
43,925,280 category assignments, annual/decade counts, original denominators,
frozen samples, model influence functions and unchanged previous releases.
Figures use category-specific scales held constant across radii to keep the
nearby category readable next to the much larger national comparison categories.

# AMWS land-grants event studies: temporal support 1830-1950

This pipeline keeps Andrews experiments conducted in 1850-1920 and balances
the count outcomes on calendar decades rather than requiring every event to
have the full relative-time window.

Run from the repository root, in order:

```r
Rscript analysis/land_grants/temporal_support_1830_1950/01_build_temporal_support_panel.R
Rscript analysis/land_grants/temporal_support_1830_1950/02_run_main_event_studies.R
Rscript analysis/land_grants/temporal_support_1830_1950/03_run_baseline_population_controls.R
```

Required environment variables are `DET_DIR` and `GTL_REPO`.

The specification uses calendar decades 1830-1950, event time -20 to +90,
and reference period -10. Population is observed from NHGIS/manual knots or
interpolated only between valid knots. Missing population never removes a unit
from the count outcomes.

Prepared panels are written to Dropbox `output/land_grants/`. Model outputs
are written to Dropbox `results/land_grants/event_studies/` in separate main
and baseline-population folders.

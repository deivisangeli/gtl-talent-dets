# 50 km and 100 km figure companion

The 2026-10-14 companion reuses the completed 50 km no-reuse results and adds
sixteen new 100 km CSDID regression-adjusted models. Both the county catchment
radius and birth-county-centroid training radius change to 100 km. It preserves
all source eligibility and assigned institution identities. Reference coordinates
do not verify historical campuses.

Run from the repository root with `TALENT_DETS_DATA_DIR` set to the Dropbox root:

```powershell
Rscript --vanilla prep/land_grants/06_build_amws_training_100km_geography.R
Rscript --vanilla prep/land_grants/07_build_amws_training_100km_samples.R
Rscript --vanilla analysis/land_grants/training_50km/run_100km_event_studies.R
Rscript --vanilla analysis/land_grants/training_50km/validate_100km_companion.R
python analysis/land_grants/training_50km/build_radius_companion.py
```

The new geographic pool has 1,574 counties and 1,955 experiment/county
memberships across 57 experiments. Training classification includes 83,007
eligible edition-specific AMWS entries in that pool; all 45,081 eligible entries
in the previous 50 km pool pass classification monotonicity checks.

The relative-time sample removes the original seven exclusions plus 92 new
shared counties, frozen across timings. The Macon County, Illinois runner-up
`7_17115_runner_up` becomes empty; its experiment retains other controls.
Standard timing has 16 events, 51 units, 637 observations, 399 counties and
10,587 included AMWS entries. Alternative timing has 20 events, 61 units,
820 observations, 464 counties and 22,224 entries. Counties are never reassigned
after screening or exclusion.

Calendar balancing starts from the latest 14-event, 43-unit sample, retaining
the eleven prior exclusions and adding 28 shared counties. The common fixed
membership has 368 counties, 559 observations and 13 calendar decades under
both timings. The standard/alternative samples include 12,994/12,984 AMWS
entries; the alternative 1960 decade bin is excluded. East Carolina remains
outside this version. Event-time support is allowed to vary from −50 to +70.

All sixteen estimations and numerical effect-decomposition checks pass.
Small-treatment-cohort warnings apply. The relative-time 100 km models have
available joint pre-trend tests; all eight calendar 100 km models have unavailable
joint tests because of singular covariance. These are retained in the model,
warning and covariance diagnostics, without estimator or sample substitutions.

Preparation releases live under `output/land_grants/`; analysis releases live
under `results/land_grants/event_studies/`, with stems
`amws_training_100km_catchments_no_reuse_20261014` and
`amws_training_100km_calendar_no_reuse_20261014`. The underlying geographic
release is `amws_training_100km_geography_20261014`. Completion manifests record
protected input and verified output checksums. Local checkpoints remain under
`~/.codex/amws_checkpoints/`. Previous releases are immutable.

The twelve-page figure-only PDF, source-derived notes, validation, rendered
pages and comparison figures are under
`results/land_grants/companions/amws_training_radius_companion_20261014/`.
It contains eight category pages and four total-benchmark comparison pages;
all document text is headings or notes below figures. Radius comparisons also
reflect changes in county composition; they are not constant-sample contrasts.

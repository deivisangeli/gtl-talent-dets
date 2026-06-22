# Fair Enrichment Intermediate Scripts

This folder preserves the intermediate scripts that were used to build the
world's-fairs enrichment files: host geocoding, post-1911 additions extraction,
manual research-batch preparation, researched-batch consolidation, and the old
final combine step.

The active pipeline entrypoints are now one level up:

```powershell
Rscript prep/world_fairs_panel/01_scrape_worlds_fairs.R
Rscript prep/world_fairs_panel/02_build_worlds_fairs_enriched.R
```

The scripts in this folder are kept for traceability and for rebuilding the
intermediate research inputs if needed. The active `02_build_worlds_fairs_enriched.R`
does not perform new internet searches; it reads the already researched
visits/venues/geocodes and writes the canonical 1790-1960 enriched file.

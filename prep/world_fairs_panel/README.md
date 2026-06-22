# World Fairs Panel Pipeline

Scripts in this folder collect the Wikipedia world's fairs list, attach the
previously researched enrichment fields, and build the panels used by the world
fairs event studies.

These scripts resolve repository files through the `GTL_REPO` environment
variable, which should point to the repository root. They can be run from this
directory or from the repository root.

Run scripts from this directory:

```powershell
cd prep/world_fairs_panel
```

Recommended order:

```powershell
Rscript 01_scrape_worlds_fairs.R
Rscript 02_build_worlds_fairs_enriched.R
Rscript 03_build_uk_historical_urban_unit_population_1801_1961.R
Rscript 03a_build_greater_london_1911_parish_crosswalk.R
```

The active world's-fairs enrichment stage is now intentionally compact:

1. `01_scrape_worlds_fairs.R` scrapes the Wikipedia list to
   `input/worlds_fairs_wikipedia.xlsx`.
2. `02_build_worlds_fairs_enriched.R` reads the scrape, the previously extracted
   host geocodes, the curated 1790-1910 visits/venues file, and the researched
   1911-1960 batches. It writes the canonical consolidated file
   `input/worlds_fairs/worlds_fairs_1790_1960_with_visits_venues.csv` and the
   matching XLSX/summary outputs. It does not perform new internet searches.

The `fair_enrichment/` subfolder keeps the older intermediate enrichment
scripts for traceability: geocoding, post-1911 additions export, research-batch
preparation, batch consolidation, and the former final-combine step. These
scripts are not part of the main run order.

The current UK panel builder is
`03_build_uk_historical_urban_unit_population_1801_1961.R`. It builds the
harmonized 1921 urban-unit geography, attaches Law-Robson and Nomis population,
and adds Wikipedia scientist/inventor outcomes from geocoded birth points. The
old numbered scripts from `03` through `21`, including the LAU/GISCO outcomes
workflow, are archived in `legacy/numbered_03_21/` for traceability and are not
part of the main run order.

`03a_build_greater_london_1911_parish_crosswalk.R` is a diagnostic/refinement
step for London. It reads the BBCE 1911 parish list added under
`input/worlds_fairs/city_census/GBR/raw/`, matches those parish names to the
downloaded ArcGIS English parish boundaries, reconstructs a
`GREATER_LONDON_1911_PARISHES` geometry, and allocates that geometry and 1911
population to the Nomis/ONS 1921 district boundaries. It writes match audits,
the district crosswalk, and a GeoPackage back to the ArcGIS parish input folder
in Dropbox. It does not currently overwrite the main panel.

The notes below describe archived helper and diagnostic scripts in
`legacy/numbered_03_21/`. They document how the current inputs were assembled,
but they are not active pipeline steps.

The census downloader stores original and normalized France/United Kingdom
files under `input/worlds_fairs/city_census/` in Dropbox. It keeps only
observed census years through 1960, does not interpolate missing years, and
records Dublin and Cork as deferred until the Ireland collection is added.

The UK urban-population comparison script reads the manually supplied
Law-Robson-Bennett population and Settlement Points CSVs in the Dropbox
`city_census/GBR/` folder. It builds a geocoded 1801-1911 town panel and
compares the 1851-1911 overlap with Populations Past registration
sub-districts using reviewed name and historic-county matches.

The Nomis extension script downloads the official CR03 population tables and
metadata for the 1921, 1931, 1951, and 1961 censuses of England and Wales. It
matches urban administrative districts to the same 934-city universe, retains
ambiguous and unmatched cases in an audit file, and writes a combined
1801-1961 geocoded panel. There was no census in 1941.

The spatial matching script downloads historical district polygons from the
ONS Open Geography Portal and UK Data Service study 9321. It assigns Settlement
Points to census-year districts, links the polygons to Nomis populations, and
writes a separate spatially validated panel and full textual-versus-spatial
audit. It does not overwrite the earlier name-based panel.

The 1911-1921 audit uses the comparison population for 1911 published inside
the 1921 Nomis CR03 table. It decomposes each observed Law-Robson-to-Nomis
change into a 1911 source/geography difference and population growth within
the Nomis comparison geography. The audit is diagnostic and does not remove
or replace any existing match.

The UK historical urban-unit harmonization script uses 1921 Urban Districts,
Municipal Boroughs, and County Boroughs as the fixed target geography, with a
synthetic Greater London unit replacing fragmented London geographies. It
assigns Law-Robson settlements by point-in-polygon for 1801-1911 and allocates
Nomis district populations for 1921-1961 by polygon intersections weighted by
1911 target density. It also reads the raw cross-verified Wikipedia people
database, classifies STEM with `prep/stem_labels.R`, assigns UK birth points to
the same target polygons, and writes the processed inventor/scientist outcome
panel under `Data/processed/`.

The ukgeog parish feasibility script checks whether the current UK panel can
be rebuilt at parish level using `ukgeog`. Nomis metadata do contain
parish-like population units for 1921-1951 and parish/enumeration-district
units for 1961, but `ukgeog` does not expose parish boundary levels in its
metadata. The script therefore writes an audit report and lower-unit counts,
but it does not create a parish panel without an additional parish boundary
source.

The Nomis parish script extracts the observed primary `Civil Parish`/`Parish`
population rows from CR03 for 1921, 1931, 1951, and 1961, and writes related
parish/common-land/intersection units separately. This is useful for inspecting
the finest population units available in Nomis, but it is not a harmonized
geographic panel: `ukgeog` has been installed and checked, and it still does
not provide parish polygons.

For interactive inspection in RStudio, source the following script. It loads
the original Law-Robson table, the original Nomis CR03 tables, matched panels,
crosswalk, audit, coverage summary, and a direct 1911-1921 comparison:

```r
source("prep/world_fairs_panel/legacy/numbered_03_21/07_open_uk_nomis_law_robson_data.R")
```

Outputs continue to be written under the Dropbox paths defined in
`file.path(Sys.getenv("GTL_REPO"), "paths.R")`.

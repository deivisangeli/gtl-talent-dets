# Education by the Numbers faculty panels

Builds a conservative college-year teaching-staff panel and source-observed
county-year subtotals for 1850–1950. Raw data and all outputs live in Dropbox.
This pipeline is separate from the manually researched 20-college faculty-count
release under `prep/land_grants/faculty_counts/`.

## Corrected and estimated release

The complete rebuild includes reviewed units/dates, the separate FTE-to-headcount
model, both county interpolation variants, comparisons and validation:

```powershell
python prep/faculty_data/run_faculty_release.py --data-root 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
```

The protected pre-correction snapshot lives in Dropbox `Data/faculty_data_releases/`.
The raw archive is never edited. Additional primary-source evidence is saved in
`Data/raw/faculty_data_source_reports/`, separate from the original archive.

Federal surveys first collected FTE in 1931–32. The 1935–36 and 1939–40 sources
default to FTE with reviewed whole-row or sex-specific headcount exceptions.
Faculty-specific/whole-row earlier-period footnotes change faculty years; notes
only about students or degrees do not. Almanac instructor counts concern
undergraduates. Source scope remains explicit and can be narrower or broader than
teaching-only faculty.

1949–50 resident instruction is headcount; extension and broader institutional
staff are separate. Calibration uses valid total-staff FTE/headcount pairs,
excluding unconverted or internally inconsistent pairs. Earlier FTE is divided by
the college-specific ratio when reviewed identity/locality/boundaries agree;
otherwise use the ratio of pooled valid totals. This assumes stable part-time
intensity across decades and produces a professional-staff proxy, not an observed
teaching-only count. Fractions are retained. Original reported counts are unchanged.

Additional outputs are `college_faculty_panel_estimated.csv` in the intermediary
directory, and `county_faculty_panel_estimated.csv` plus
`county_faculty_panel_estimated_interpolated.csv` in the processed directory.
Use `faculty_headcount_best_available` for source-year values or
`faculty_headcount_annual` for the annual estimated/interpolated series, always
retaining the corresponding status. Reported and modeled contributions are separate.
County interpolation fills all bounded gaps, does not extrapolate, and retains
endpoint values/years/statuses. The 1949 endpoint covers land-grant colleges only;
interpolation cannot correct changes in source coverage.

`ESTIMATION_README.md`, the estimated dictionaries, calibration crosswalk, unit/date
ledger, table audit, coverage/reconciliation files, unresolved cases and release
manifest accompany the outputs. See the explicit review depth in the table audit:
all archived headers/notes plus relevant original methodology/headings were checked;
this does not claim a cell-by-cell re-transcription of every printed report page.

Run from any working directory using an absolute script path, or from the repo root:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
python prep/faculty_data/build_faculty_panels.py
python prep/faculty_data/test_faculty_panels.py
python prep/faculty_data/validate_saved_faculty_panels.py --data-root $env:TALENT_DETS_DATA_DIR
```

The Python build uses the standard library and invokes `Rscript` with `sf` and
`data.table` for point-in-polygon checks. It resolves the data root from
`--data-root`, then `TALENT_DETS_DATA_DIR`, then the current user's standard
Dropbox project path. It does not change the shared `paths.R` configuration.

Inputs are the archived `Data/raw/faculty_data` download, saved GeoNames US data,
the national 2020 county roster/polygons and the existing OpenAlex institution
reference. The rebuild is offline. Historical-review decisions are preserved in
`Data/intermediary/faculty_data/curation/`; these files are required to reproduce
the reviewed release and are checksummed in the build manifest.

`source_table_mapping.json` is the explicit table/column/year specification.
`make_table_mapping.py --raw <archive-folder>` regenerates it from the archived
schemas and the reviewed mapping rules. Regeneration is needed only when mapping
rules or the source archive change. Regeneration does not modify raw data.

Outputs:

- `Data/intermediary/faculty_data/college_faculty_panel.csv`: observed college-year
  records, separate headcount/FTE/departmental counts, source IDs and review flags.
- `Data/processed/faculty_data/county_faculty_panel.csv`: 2020 county equivalents
  × 1850–1950; missing years remain missing, never filled with zero.
- Both directories contain interpretation notes and the data dictionary.
  Intermediate files preserve original rows, calculation terms, identity/location
  crosswalks and unresolved cases. Processed files include coverage, reconciliation,
  validation and checksummed build metadata.

County values are observed subtotals, not complete county censuses. FTE and
headcounts are never interchanged. Components with potentially overlapping
teachers are not summed into an invented total. Some institutional totals span
several counties; locality ambiguity and inseparable totals remain unallocated.
The location method uses historical reported localities, not modern university
coordinates as historical evidence. Conservative IDs do not claim complete
historical entity resolution; supported renamings/relocations and same-name
distinct institutions are recorded in reviewed overrides. See the output README
and coverage files before using these data in an analysis.

## Optional annual county interpolation

After building the observed panel, run:

```powershell
python prep/faculty_data/interpolate_county_faculty.py --data-root $env:TALENT_DETS_DATA_DIR
python prep/faculty_data/test_county_interpolation.py
python prep/faculty_data/validate_county_interpolation.py --data-root $env:TALENT_DETS_DATA_DIR
```

This standard-library step creates
`Data/processed/faculty_data/county_faculty_panel_interpolated.csv` without
changing either observed panel. Every original column remains unchanged;
`faculty_headcount_annual` contains original values plus linear interpolation
between observations within a county. `faculty_headcount_status` identifies
observed, interpolated and missing values. Endpoint years, original endpoint
values and their year distance accompany every interpolated value.

All bounded gaps are filled, with no length cap. No values are extrapolated
before the first observation or after the last; counties with zero or one
observation receive no new values. Explicit zeros are valid anchors. Estimates
retain fractions rather than being rounded to integers; calculations use 50
significant decimal digits. FTE counts remain unchanged and never supply
headcount anchors.

The new series interpolates partial source totals, not an exhaustive county
faculty census. Source reporting counts and coverage flags continue to describe
observations. Sidecars named `county_interpolation_*` provide coverage by
county/year, gap lengths, field definitions, checksums, and independent export
validation. The former 25,648-cell count applied to the old, uncorrected input;
corrected inputs receive their independently calculated interior-gap count.

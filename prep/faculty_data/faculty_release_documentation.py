"""Documentation and comparisons for the corrected/estimated faculty release."""
import collections,json
from pathlib import Path
from build_faculty_panels import read,write,digest
from panel_documentation import describe

DEFINITIONS={
 'faculty_headcount_estimated':'College: converted total, including retained reported sex components in mixed-unit rows; blank where reported total is preferred. County: modeled contribution only. Use best_available for combined totals.',
 'faculty_headcount_best_available':'Reported headcount if available; otherwise complete FTE-converted or mixed-sex source-year estimate. Fractional values retained. Partial source totals, not a complete census.',
 'faculty_headcount_best_status':'observed, estimated, mixed, or missing. Mixed means both reported and converted contributions. County status refers to contributing colleges.',
 'headcount_reported_contribution':'Reported headcount portion of best_available; includes reported sex component in a mixed-unit record.',
 'headcount_modeled_contribution':'FTE-converted portion of best_available; never overwrites reported source headcounts.',
 'headcount_conversion_ratio_fte_per_person':'1949 FTE/headcount ratio. Divide FTE by this factor; equal to calibration_FTE/calibration_headcount.',
 'headcount_conversion_multiplier':'Inverse ratio, calibration_headcount/calibration_FTE.',
 'headcount_calibration_method':'college_specific when reviewed exact identity/locality and no identified boundary conflict; pooled otherwise.',
 'headcount_calibration_observation':'Original 1949 source observation ID for institution-specific calibration; blank for pooled calibration.',
 'headcount_calibration_headcount':'Reported 1949 total-staff headcount or sum across usable calibration institutions.',
 'headcount_calibration_fte':'Reported 1949 total-staff FTE or sum across usable calibration institutions.',
 'headcount_calibration_fallback_reason':'Why an institution-specific ratio was unavailable or rejected.',
 'headcount_conversion_source_observation':'Source observation ID whose complete FTE/sex components were converted.',
 'headcount_conversion_formula':'Calculation identifying retained headcounts and converted FTE components. Exact source cells in faculty_calculation_terms.csv.',
 'headcount_estimate_scope_proxy':'True for modeled professional-staff proxy or observed source with broader professional-staff scope. Conversion cannot isolate teaching staff.',
 'faculty_headcount_best_GEOID':'County of actual selected reported/converted source. Blank for unresolved or inseparable multicounty location.',
 'faculty_headcount_best_definition':'Definition and limitations of selected reported or converted measure.',
 'faculty_headcount_annual':'Source-year best_available headcount or linear estimate between bounded county endpoints; no extrapolation or integer rounding.',
 'faculty_headcount_status':'observed, estimated, mixed, interpolated, or missing. Endpoint status remains available for interpolations.',
 'faculty_headcount_gap_years':'Later endpoint year minus earlier endpoint year; blank for noninterpolated rows.',
 'headcount_source_scope':'Observed and modeled partial source totals; no assumption of complete county coverage.',
}

def publish(root,college,county,manifest):
    inter=root/'Data/intermediary/faculty_data';out=root/'Data/processed/faculty_data'
    entries=[]
    for dataset,rows in [('college_faculty_panel_estimated',college),('county_faculty_panel_estimated',county)]:
        for k in dict.fromkeys(k for row in rows for k in row):
            definition=DEFINITIONS.get(k,describe(k))
            if k.startswith('n_colleges_') and k not in ['n_colleges_reported']:definition='Count of colleges in the stated reporting/estimation/coverage category; does not impute reporting in missing years.'
            entries.append(dict(dataset=dataset,column=k,description=definition,missing_value='Empty CSV cell, never automatically zero'))
    for k in ['faculty_headcount_annual','faculty_headcount_status','faculty_headcount_left_year','faculty_headcount_left_value','faculty_headcount_right_year','faculty_headcount_right_value','faculty_headcount_gap_years','faculty_headcount_left_status','faculty_headcount_right_status']:
        entries.append(dict(dataset='county_faculty_panel_estimated_interpolated',column=k,description=DEFINITIONS.get(k,'Year, value or status of the original source-year endpoint supporting this interpolation; blank otherwise.'),missing_value='Empty CSV cell'))
    entries += [dict(r,dataset='county_faculty_panel_estimated_interpolated') for r in entries if r['dataset']=='county_faculty_panel_estimated']
    for destination in [inter,out]:write(destination/'estimated_data_dictionary.csv',entries)
    snapshots=sorted((root/'Data/faculty_data_releases').glob('before_corrections_*'))
    snapshot=snapshots[0] if snapshots else None
    comparison=[]
    if snapshot:
        for dataset,current,path,keys in [('college',read(inter/'college_faculty_panel.csv'),'Data/intermediary/faculty_data/college_faculty_panel.csv',['college_id','year']),
            ('county',read(out/'county_faculty_panel.csv'),'Data/processed/faculty_data/county_faculty_panel.csv',['GEOID','year'])]:
            before={tuple(r[k] for k in keys):r for r in read(snapshot/path)};after={tuple(r[k] for k in keys):r for r in current}
            for key in sorted(set(before)|set(after)):
                a=before.get(key,{});b=after.get(key,{})
                for field in ['faculty_headcount','faculty_fte','GEOID']:
                    if a.get(field,'')!=b.get(field,''):
                        comparison.append(dict(dataset=dataset,key=json.dumps(key),field=field,before=a.get(field,''),after=b.get(field,''),change_type='modified' if a and b else 'added' if b else 'removed'))
    write(out/'corrections_before_after.csv',comparison,['dataset','key','field','before','after','change_type'])
    notes=f'''# Corrected faculty headcounts and model-assisted annual county series

The original archive is unchanged. Previous outputs and code are preserved in `{snapshot}`. `corrections_before_after.csv` lists changes to reported headcounts, FTEs, geography and college-year keys.

## Which files to use

- `college_faculty_panel.csv` and `county_faculty_panel.csv`: corrected source observations only, with headcounts/FTE separate.
- `college_faculty_panel_estimated.csv`: all observed college-panel columns unchanged, plus source-year modeled and best-available headcounts.
- `county_faculty_panel_estimated.csv`: all observed county-panel columns unchanged, plus reported/modeled contributions and best-available totals.
- `county_faculty_panel_interpolated.csv`: corrected observed-only bounded interpolation.
- `county_faculty_panel_estimated_interpolated.csv`: best-available observed/converted county anchors with all bounded annual gaps interpolated. Use `faculty_headcount_annual` and `faculty_headcount_status`.

## Estimation

For an accepted 1949 pair r = total institutional staff FTE / total institutional staff headcount. Earlier FTE / r estimates headcounts. Reported headcounts are copied unchanged; for a mixed-sex source only its FTE component is converted. Both components must be available. No missing component is treated as zero. Values retain fractions using 50-digit decimal arithmetic.

Usable calibration pairs: {manifest['valid_calibration_pairs']}; excluded: {manifest['excluded_calibration_pairs']}. Pooled staff headcount = {manifest['pooled_headcount']}; pooled FTE = {manifest['pooled_fte']}; fallback r = {manifest['pooled_ratio']}. The pooled ratio is a ratio of sums, not the mean of institution ratios.

Institution-specific factors require exact reviewed institutional identity (or exact name/state/locality), identical historical locality, and no identified multicampus boundary conflict. The crosswalk records acceptance or fallback for every eligible FTE source observation. No fuzzy matching or inferred merger joins are used. A constant ratio over decades is an explicit modeling assumption, not an observed fact.

Calibration uses TOTAL professional institutional staff, including research/administration/extension. Earlier 1935/1939 faculty FTE can also include noninstructional professional staff. These estimates are therefore professional-staff headcount proxies, not identified teaching-only faculty counts. Unit conversion cannot isolate teaching staff or correct incomplete coverage. Almanac headcounts have undergraduate-instructor-only scope.

## Corrections and geography

1935–36 and 1939–40 default to FTE, with whole-row/sex-specific headcount exceptions. Faculty dates follow whole-row or faculty-specific earlier-period footnotes; student/degree/financial-only notes do not alter faculty time. The website's 1910 tables use 1909–10 (starting year 1909), and its 1916 table belongs to the original 1914–15 chapter (starting year 1914). The state-university sample for the year ended June 1916 uses 1915–16. Single-year prior statistics without an academic period remain explicit single-year observations. Research-sample values matching nationwide cells inherit the original footnoted unit; conflicting notes/values remain preserved. Source table period remains separate from source-row corrected period.

1949–50 resident instruction is headcount, separate from extension and total staff. Extension and resident functions can overlap and are not summed. Historical localities are checked against the contemporary directory and earlier archive. The directory is one year later, so continuity remains an explicit limitation; possible multicampus totals, unresolved sites and ambiguous campus boundaries remain unallocated. Virginia State's Ettrick campus uses Chesterfield County rather than its Petersburg postal label. Geography uses fixed 2020 counties.

## Interpolation and coverage

Only source-year reported or converted values are endpoints. All bounded gaps are filled regardless of length, preserving anchors and reported zeros exactly; no extrapolation. Endpoint years, values and observed/estimated/mixed statuses are retained. 1949 is land-grant-only coverage. Interpolating toward it does not reconstruct missing non-land-grant colleges and may interpolate changes in reporting coverage. All county figures are partial source totals.

College statuses: {json.dumps(manifest['college_status'],sort_keys=True)}. Conversion methods: {json.dumps(manifest['conversion_methods'],sort_keys=True)}. Annual county statuses: {json.dumps(manifest['county_annual_status'],sort_keys=True)}.

## Audit and reproduction

`table_unit_audit.csv` covers all 76 included archived tables and explicitly states review depth: complete archived headers/notes plus original methodology and relevant printed headings, not a line-by-line re-transcription of all printed pages. `unit_date_review_ledger.csv`, original source observations/cell terms, calibration tables/crosswalk, historical-location curation, conflicting reports, large-change diagnostics, unresolved cases and coverage by year/type/state retain the evidence and limitations.

`estimated_reconciliation_by_year.csv` checks college totals = county allocated + unallocated separately for reported/modeled/combined values. `estimated_validation.json` independently checks conversion/interpolation using rational arithmetic. `estimated_build_manifest.json` records input/output/code checksums. The previous 25,648 interpolated-cell target is obsolete after source correction.

Run the documented commands in the repository faculty README. No network is required for rebuilding after source reports and curation are saved. Do not change calibration or curation files without retaining reviewed evidence and rebuilding all downstream files.
'''
    for destination in [inter,out]:(destination/'ESTIMATION_README.md').write_text(notes,encoding='utf-8')

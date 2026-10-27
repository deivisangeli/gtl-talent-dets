"""Publish field definitions and interpretation notes alongside the data."""
import csv
from pathlib import Path

DESCRIPTIONS={
 'college_id':'Stable conservative institutional-series ID. Reviewed overrides join proven renamings/relocations and separate distinct historical entities. Unverified aliases remain separate.',
 'college_name_original':'Institution name exactly as displayed in the selected source row.',
 'college_name_core':'Source institution name with explicit parenthetical department qualifiers separated for deduplication.',
 'college_name_standardized':'Reference-supported or manually reviewed name; otherwise the source name.',
 'identity_method':'reviewed_override; exact_reference_alias_same_reported_locality; or exact_source_name_state_locality. Exact matching is not an exhaustive historical continuity audit.',
 'reference_id':'OpenAlex reference identifier, when an exact canonical/alternative name agrees with the reported state and locality. Not used as proof of historical campus geography.',
 'year':'Reported single year or academic-year starting year; bounded 1850-1950. Almanac volume years remain flagged as observation-date unverified.',
 'reported_period':'Full source period, retained independently of the numeric year.',
 'year_basis':'Evidence used to label the year; distinguishes source academic periods, single-year reports, survey citations, and unverified Almanac volume years.',
 'publication_year':'Publication year recovered from the bibliographic source citation; blank when not established.',
 'reported_opening_year':'Opening/founding date printed in the source. Definitions can differ across reports; conflicts are review flags, not automatic identity changes.',
 'city_original':'Unmodified locality in the source; blank when the source gives none.',
 'state_original':'Unmodified state in the source; blank when the source gives none.',
 'city':'Locality used for matching; any recovery from independent historical reports is explicitly flagged and sourced.',
 'state':'Two-letter state/territory abbreviation. Blank when the source state is absent or unresolved.',
 'GEOID':'Five-character fixed-2020 county FIPS. Blank for unresolved locations or inseparable multiple-county totals. Keep as text.',
 'county_name':'Official county-equivalent name from the saved 2020 Census roster.',
 'county_geography_year':'2020; this is fixed modern county geography, not the county boundaries in the historical observation year.',
 'geography_method':'Assignment or unresolved reason. Most assignments use exact historical reported locality plus saved GeoNames populated-place points checked against 2020 polygons.',
 'geography_evidence':'Historical source/locality and GeoNames identifier or manually reviewed source URL supporting the county decision.',
 'geography_precision':'Explicitly distinguishes locality-based assignments from reviewed historical-site geography. A locality centroid is not an exact historical campus coordinate.',
 'locality_latitude':'Latitude of the matched GeoNames locality point; not an assertion of a historical campus coordinate.',
 'locality_longitude':'Longitude of the matched GeoNames locality point; not an assertion of a historical campus coordinate.',
 'locality_support_observation':'Independent source observation supplying a research-sample locality in the same year or in bracketing historical reports.',
 'locality_support_method':'Same-year independent report or agreement between reports on both sides of the year. No single-sided extrapolation and no interpolation of faculty counts.',
 'inclusion_status':'included or excluded according to the agreed institution scope.',
 'institution_type':'College/university, junior college, or teachers college; excluded normal/professional schools have explicit exclusion classifications.',
 'source_institution_type':'Original institution-type label where supplied by the source.',
 'faculty_headcount':'Reported faculty/instructor headcount, with actual source scope retained. Later survey exceptions may cover broader professional staff and are flagged. Never filled with converted FTE, interpolations, or missing components treated as zero.',
 'faculty_fte':'Reported faculty full-time equivalents. Separate from headcounts; 1933/1935/1937/1939 survey measures may include noninstructional professional roles and are flagged. 1949 broader institutional staff FTE is stored separately.',
 'faculty_male':'Reported male teaching headcount from the same observation selected for the total.',
 'faculty_female':'Reported female teaching headcount from the same observation selected for the total.',
 'faculty_fte_male':'Reported male teaching FTE from the same observation selected for the total.',
 'faculty_fte_female':'Reported female teaching FTE from the same observation selected for the total.',
 'faculty_preparatory':'Reported preparatory-department teachers; this can overlap with other departments and is not automatically added to the all-teaching total.',
 'faculty_collegiate':'Reported collegiate-department teachers; this can overlap with other departments and is not automatically added to the all-teaching total.',
 'faculty_professional':'Reported professional-department teachers within an included college; not standalone professional schools.',
 'faculty_resident_instruction_fte':'Legacy field retained blank: original 1949 functional counts are headcounts, not FTE.',
 'faculty_extension_instruction_fte':'Legacy field retained blank: original 1949 functional counts are headcounts, not FTE.',
 'faculty_resident_instruction_headcount':'1949 full- and part-time resident instruction headcount; no extension/research/administration added.',
 'faculty_extension_instruction_headcount':'1949 extension-function headcount, separate from resident instruction because overlap is possible; not assigned to main campus.',
 'institutional_staff_headcount':'1949 reported total institutional/professional staff headcount, retained for calibration only; not a teaching count.',
 'institutional_staff_fte':'1949 reported total institutional/professional staff FTE, retained for calibration only; source-note usability flag must also be checked.',
 'institutional_staff_fte_usable':'False when the source says FTE was not fully converted; excluded from all ratio calibration.',
 'faculty_male_unit':'Reviewed unit of original male faculty cell: headcount or fte. A mixed-sex source has no reported combined total.',
 'faculty_female_unit':'Reviewed unit of original female faculty cell: headcount or fte.',
 'source_catalog_year':'Table-level year before any faculty-specific or whole-row date footnote is applied.',
 'source_catalog_period':'Table-level period before row-level date correction.',
 'table_period_evidence':'Primary-source chapter or report evidence for table-specific academic-year starting labels; distinct from publication year.',
 'unit_review_evidence':'Primary-source heading, methodology or comparative-table evidence supporting unit classification.',
 'unit_review_status':'Explicit level of unit evidence; complete archival transcription is not claimed.',
 'reported_faculty_components_json':'All reported faculty component cells for all competing observations in this college-year, keyed by observation ID; preserves raw strings and parsed numeric values.',
 'components_json':'Original faculty component headers and their raw/parsed values for a source observation.',
 'raw_row_json':'Entire original source CSV row, preserving all values and original column labels.',
 'observation_id':'Deterministic source-row ID based on original CSV path and record number.',
 'source_csv':'Relative path to the archived raw table CSV.',
 'source_row':'Original CSV record number including the single header record; first data record is 2.',
 'source_url':'Original dataset web page URL.',
 'source_heading':'Original table heading.',
 'source_citation':'Original bibliographic citation to the historical publication, when printed on the archived web page.',
 'source_family':'national_institution_table, land_grant, or research_sample.',
 'source_priority':'Nationwide institution tables first, then land-grant tables, then the research-university sample, among equivalent institutional boundaries and periods.',
 'source_notes':'Original source-row notes.',
 'measure_basis':'headcount, fte, or mixed_sex_units after reviewed original headings and exception footnotes. See scope flags for broader professional staff.',
 'measure_definition':'Description of the source count and any calculation.',
 'department_coverage':'Reported all-teaching total, unspecified preparatory coverage, or department components whose unduplicated union is not identified.',
 'department_only':'True when the reported institution label explicitly limits the observation to a department.',
 'flags':'JSON list of source, identity, chronology, geography, conflict, or quarantine review flags.',
 'county_eligible':'False when possible duplicate institutional reports must be resolved before county aggregation. A nonblank measure-specific GEOID is also required.',
 'all_observation_ids':'JSON list of all source rows competing for this college-year; observations are selected, never added together across reports.',
 'n_competing_observations':'Number of source reports represented by this college-year.',
 'n_colleges_reported':'Number of college-year rows assigned to this county, including rows whose faculty count is unavailable.',
 'n_colleges_duplicate_review_excluded':'Potential duplicate college-year rows excluded from county sums pending review.',
 'reporting_scope':'sample_only, institution_tables_available, or no_source_observations. This describes available source families, not completeness of a county census.',
 'coverage_complete':'Always False: the archive does not establish an exhaustive county college/faculty universe.',
 'coverage_status':'observed_source_sum_partial_universe or unobserved_not_zero. County numbers are source-observed subtotals.',
}


def describe(field):
    if field in DESCRIPTIONS: return DESCRIPTIONS[field]
    for prefix in ['faculty_headcount','faculty_fte','faculty_preparatory','faculty_collegiate','faculty_professional','faculty_resident_instruction_fte','faculty_extension_instruction_fte']:
        if field==prefix+'_source_observation': return f'Observation ID supporting the selected {prefix}; blank without an observed value.'
        if field==prefix+'_definition': return f'Definition of the source selected for {prefix}.'
        if field==prefix+'_scope': return f'Departmental coverage of the selected {prefix}.'
        if field==prefix+'_department_only': return f'Whether selected {prefix} represents only a named department.'
        if field==prefix+'_GEOID': return f'County assignment of the actual source chosen for {prefix}; blank means the value is not county-allocated.'
        if field==prefix+'_geography_method': return f'Geography evidence/method of the selected {prefix} observation.'
        if field==prefix+'_source_family': return f'Source family supporting the selected {prefix}.'
    if field.startswith('n_colleges_with_'): return 'Number of deduplicated, county-eligible colleges contributing an observed value to this measure.'
    if field.startswith('n_colleges_missing_'): return 'Number of county-eligible reported colleges lacking this measure; not a count of all absent/nonreporting institutions.'
    if field.startswith('n_department_only_'): return 'Number of contributing colleges whose selected count covers only a named department.'
    if field.endswith('_terms'): return 'JSON array of source column labels used in the observation calculation; empty when no complete usable recipe exists.'
    if field.endswith('_source_observation'): return 'Observation ID supporting this reported sex-specific count; a partial breakdown can be retained even when the total is missing.'
    return 'Auxiliary audit/provenance field retained from the selected source observation; see source observations and review ledgers.'


def publish_documentation(inter,out,panel,county_panel,build):
    rows=[]
    for dataset,data in [('college_faculty_panel',panel),('county_faculty_panel',county_panel)]:
        fields=list(dict.fromkeys(k for row in data for k in row))
        for field in fields:
            rows.append(dict(dataset=dataset,column=field,description=describe(field),missing_value='Empty CSV cell; never implicitly zero'))
    for destination in [inter,out]:
        with (destination/'data_dictionary.csv').open('w',encoding='utf-8-sig',newline='') as f:
            w=csv.DictWriter(f,fieldnames=['dataset','column','description','missing_value']);w.writeheader();w.writerows(rows)
    notes=f'''# Observed college and county faculty, 1850–1950

Built from the saved Education by the Numbers archive. {build['source_observations']:,} source observations in {build['included_tables']} tables produce {build['college_years']:,} college-years for {build['unique_colleges']:,} conservative institutional-series IDs. The county grid has {build['county_years']:,} rows: {build['counties']:,} saved 2020 county equivalents × 101 years.

## Interpretation

- `faculty_headcount` is a reported faculty/instructor headcount with source scope retained. `faculty_fte` is a separate full-time-equivalent measure. Never combine their observed values as though they had the same units; modeled conversion is available only in separately labeled files.
- Counts cover only institutions and staff reported in these sources. County sums are **observed subtotals, not exhaustive totals**. They can include teacher appointments shared across institutions; they are not a deduplicated census of individual people.
- An empty cell is missing. No interpolation, carry-forward, extrapolation of counts, or assumption that an unreported county has zero faculty is used. A numeric zero comes from explicit reported zeros.
- Included: colleges, universities, junior colleges and degree-granting teachers colleges, according to source classification. Standalone normal, clinical, theological, legal, commercial and other identified professional schools are excluded. Historical classification is not a modern accreditation audit.
- Broad reported teaching totals retain preparatory teaching where included. Department/rank components and sex breakdowns remain available. Preparatory, collegiate and professional staff may overlap; their counts are not added unless an unduplicated total or a demonstrably disjoint recipe exists. Some early land-grant years therefore have components but no usable all-teaching total.
- Department-only institutional reports remain flagged and can contribute only their reported partial count. Missing teachers at other departments/institutions cannot be reconstructed.
- Academic years use their start: 1919–20 → 1919; 1925–26 → 1925; 1929–30 → 1929; 1935–36 → 1935; 1939–40 → 1939; 1949–50 → 1949. Almanac entries retain the volume year, with an explicit flag that their observation date is unverified. Single-year snapshots retain their reported year.
- Federal surveys first collected FTE in 1931–32 (ED543644, printed p. 11). Earlier faculty counts are headcounts. The 1935–36 and 1939–40 faculty tables default to FTE, with reviewed headcount exceptions for whole rows or one sex. Mixed-sex totals are missing in observed panels; sex components remain available. The research sample inherits units from matching nationwide cells, retaining conflicts in its notes.
- The 1949–50 resident-instruction fields are full- and part-time headcounts. Extension and total institutional staff are retained separately. Functional counts are not added because the same person may occur in several functions. Total institutional staff FTE is calibration metadata, not teaching FTE; four flagged unconverted pairs are excluded from estimation.
- Almanac instructors are those connected with undergraduates. Later survey faculty can include broader professional staff, including administration/research/extension. Unit conversion does not remove these scope differences. These observations and their derived estimates are explicitly flagged as proxies.
- Whole-row or faculty-specific earlier-period footnotes change faculty dates; footnotes about students/degrees alone do not. The raw catalog year/period and publication date remain separate. Original single-year labels without independently established academic periods remain flagged.
- The website's 1910 faculty tables report the year ended June 1910 and use 1909–10 (year 1909). The table labeled 1916 belongs to the original chapter explicitly titled 1914–15 and uses year 1914. The state-university sample's 1915 table cites the report for the year ended June 1916 and uses 1915–16. Whole-row prior-period exceptions remain separate.

## Identity and geography

IDs use exact source name/state/locality, or exact OpenAlex canonical/alternative names with the same reported state/locality. Manual reviews override proven renamings, relocations and same-name distinct institutions. Unverified historical aliases remain separate: this conservative college panel is not an exhaustive historical entity-resolution database. In particular the old University of Chicago is separate from the institution founded in 1890; the pre-1930 Catholic Brooklyn College label remains unresolved and separate from CUNY Brooklyn College.

Most county assignments use the **historically reported locality**, exact saved GeoNames populated-place names/aliases, and a point-in-polygon check against saved 2020 counties. This is locality-level evidence, not a precise historical-campus coordinate. Multiple county candidates, “near” locations, missing sites and possible multi-county institutional totals remain unallocated unless a reviewed historical source resolves them. Fixed 2020 geography retains original Connecticut counties. Modern university coordinates alone never establish a historical site.

The research sample's missing localities may be supported by an independent report of the exact institution in the same year, or matching localities in reports bracketing that year. These recoveries are flagged; no single-sided location extrapolation is used.

County aggregations require a valid source-specific county and resolved duplicate status. Nationwide institution reports have priority over land-grant reports, then the research sample, when periods and institutional boundaries are equivalent. Competing reports are never summed. Seven reviewed 1870 internal scientific/agricultural school labels are linked to their reported parent colleges, retaining their component counts rather than adding them to the parent report. These are source-observed parent subtotals where exact coverage cannot be proved, not a reconstruction of the union of all teachers.

## Files and review records

The intermediate directory contains `college_faculty_panel.csv`, complete source observations (including excluded rows), calculation terms with original cells, table mappings, identity/location crosswalks, competing values, unresolved cases, large changes, founding-date conflicts and curation files. `reported_faculty_components_json` retains all source faculty components for each college-year.

The processed directory contains `county_faculty_panel.csv`, coverage by year/type/state, year-by-measure reconciliation, validation results, the build manifest and this dictionary. Every observed measure traces to source observation IDs and cell-level calculation terms. Raw rows and quarantined values remain preserved.

Large changes are diagnostic flags: many span long reporting gaps, altered institutional scopes or historical reorganizations. Values are not automatically winsorized or corrected. The demonstrably anomalous 1919–20 Utah Agricultural College female-teacher entry is quarantined without inventing a replacement.

The 1890–91 HBCU table labeled colleges/universities is an exact copy of the normal-school table on the same archived page. That mislabeled table is excluded rather than treated as verified college faculty data. The misplaced K–12 table under 1930 public-college expenditures is also excluded. The table-mapping ledger records both decisions and the unmodified originals remain in the raw archive.

`reconciliation_by_year.csv` verifies: college observed total = county allocated total + explicitly unallocated total, separately for headcount and FTE. `build_manifest.json` records raw/reference/curation/code checksums. All validation checks passed at publication. See the saved independent-validation report for checks on exported files.
'''
    (inter/'README.md').write_text(notes,encoding='utf-8')
    (out/'README.md').write_text(notes,encoding='utf-8')

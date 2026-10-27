"""Interpolate bounded gaps in county faculty headcounts; never change inputs.

Standard-library, two-pass CSV pipeline. Academic-year and county definitions
are inherited unchanged from the observed panel. No headcount/FTE substitution.
"""
from __future__ import annotations
import argparse,bisect,collections,csv,hashlib,json,os
from datetime import datetime,timezone
from decimal import Decimal,localcontext
from pathlib import Path

HERE=Path(__file__).resolve().parent
NEW_FIELDS=['faculty_headcount_annual','faculty_headcount_status',
            'faculty_headcount_left_year','faculty_headcount_left_value',
            'faculty_headcount_right_year','faculty_headcount_right_value',
            'faculty_headcount_gap_years']
PLANNED_INPUT_SHA256='29141a994119d1d96c508475e3ba4f85b6c83e2e7820b38f66f3fbaca13f50e8'


def sha256(path):
    h=hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda:f.read(2**20),b''): h.update(block)
    return h.hexdigest()


def count(value):
    number=Decimal(value)
    if not number.is_finite() or number<0: raise ValueError(f'Invalid observed headcount: {value}')
    return number


def interpolate_row(row,points):
    """Copy one row; points are sorted (year, original value) observations only."""
    result=dict(row)
    result.update({field:'' for field in NEW_FIELDS})
    if row['faculty_headcount']!='':
        count(row['faculty_headcount'])
        result['faculty_headcount_annual']=row['faculty_headcount']
        result['faculty_headcount_status']='observed'
        return result
    result['faculty_headcount_status']='missing'
    year=int(row['year'])
    position=bisect.bisect_left(points,(year,''))
    if position==0 or position==len(points): return result
    left_year,left_value=points[position-1];right_year,right_value=points[position]
    if not left_year<year<right_year: raise ValueError('Observation grid/anchor mismatch')
    with localcontext() as context:
        context.prec=50
        value=count(left_value)+(count(right_value)-count(left_value))*Decimal(year-left_year)/Decimal(right_year-left_year)
        text=format(value.normalize(),'f')
    result.update(faculty_headcount_annual=text,faculty_headcount_status='interpolated',
                  faculty_headcount_left_year=str(left_year),faculty_headcount_left_value=left_value,
                  faculty_headcount_right_year=str(right_year),faculty_headcount_right_value=right_value,
                  faculty_headcount_gap_years=str(right_year-left_year))
    return result


def write_csv(path,rows,fields):
    with path.open('w',encoding='utf-8-sig',newline='') as f:
        writer=csv.DictWriter(f,fieldnames=fields);writer.writeheader();writer.writerows(rows)


def build(root):
    out=root/'Data/processed/faculty_data'
    source=out/'county_faculty_panel.csv'
    college=root/'Data/intermediary/faculty_data/college_faculty_panel.csv'
    protected={str(path.relative_to(root)):sha256(path) for path in [source,college]}
    points=collections.defaultdict(list);masks={};meta={};nrows=0
    full_mask=(1<<101)-1
    with source.open(encoding='utf-8-sig',newline='') as f:
        reader=csv.DictReader(f);original_fields=list(reader.fieldnames)
        if set(NEW_FIELDS)&set(original_fields): raise ValueError('Input is not the original observed panel')
        for row in reader:
            geoid=row['GEOID'];year=int(row['year']);nrows+=1
            if len(geoid)!=5 or not geoid.isdigit() or not 1850<=year<=1950:
                raise ValueError('Invalid county/year key')
            bit=1<<(year-1850)
            if masks.get(geoid,0)&bit: raise ValueError(f'Duplicate county-year: {geoid} {year}')
            masks[geoid]=masks.get(geoid,0)|bit
            meta[geoid]={key:row[key] for key in ['GEOID','county_name','state']}
            if row['faculty_headcount']!='':
                count(row['faculty_headcount']);points[geoid].append((year,row['faculty_headcount']))
    if not all(mask==full_mask for mask in masks.values()): raise ValueError('Expected complete 1850-1950 county grid')
    for anchors in points.values(): anchors.sort()
    expected_filled=sum(b[0]-a[0]-1 for anchors in points.values() for a,b in zip(anchors,anchors[1:]))
    expected_observed=sum(map(len,points.values()))
    if protected[str(source.relative_to(root))]==PLANNED_INPUT_SHA256 and expected_filled!=25648:
        raise AssertionError('Current planned input should yield exactly 25,648 interpolated cells')
    target=out/'county_faculty_panel_interpolated.csv'
    county_counts=collections.defaultdict(collections.Counter);year_counts=collections.defaultdict(collections.Counter)
    gaps=collections.Counter();statuses=collections.Counter()
    with source.open(encoding='utf-8-sig',newline='') as incoming,target.open('w',encoding='utf-8-sig',newline='') as outgoing:
        writer=csv.DictWriter(outgoing,fieldnames=original_fields+NEW_FIELDS);writer.writeheader()
        for row in csv.DictReader(incoming):
            result=interpolate_row(row,points[row['GEOID']]);writer.writerow(result)
            status=result['faculty_headcount_status'];statuses[status]+=1
            county_counts[row['GEOID']][status]+=1;year_counts[int(row['year'])][status]+=1
            if status=='interpolated': gaps[int(result['faculty_headcount_gap_years'])]+=1
    assert statuses['observed']==expected_observed and statuses['interpolated']==expected_filled
    assert sum(statuses.values())==nrows
    # Separate implementation reparses the export using exact rational arithmetic.
    from validate_county_interpolation import validate_export
    validation=validate_export(source,target)
    assert validation['interpolated_cells']==expected_filled
    assert all(sha256(root/relative)==value for relative,value in protected.items()),'Observed inputs changed'
    county_summary=[]
    for geoid in sorted(meta):
        anchors=points[geoid];tally=county_counts[geoid]
        county_summary.append(dict(**meta[geoid],observed=tally['observed'],interpolated=tally['interpolated'],missing=tally['missing'],
            first_observed_year=anchors[0][0] if anchors else '',last_observed_year=anchors[-1][0] if anchors else '',
            maximum_endpoint_gap_years=max((b[0]-a[0] for a,b in zip(anchors,anchors[1:])),default='')))
    county_coverage=out/'county_interpolation_coverage_by_county.csv'
    year_coverage=out/'county_interpolation_coverage_by_year.csv'
    gap_coverage=out/'county_interpolation_gap_summary.csv'
    write_csv(county_coverage,county_summary,list(county_summary[0]))
    write_csv(year_coverage,[dict(year=y,**{k:year_counts[y][k] for k in ['observed','interpolated','missing']}) for y in range(1850,1951)],
              ['year','observed','interpolated','missing'])
    write_csv(gap_coverage,[dict(endpoint_gap_years=gap,interpolated_cells=n) for gap,n in sorted(gaps.items())],
              ['endpoint_gap_years','interpolated_cells'])
    dictionary=out/'county_interpolation_data_dictionary.csv'
    definitions={
        'faculty_headcount_annual':'Original observed headcount or linear estimate between two observations in the same county. No integer rounding; repeating decimals use 50-significant-digit arithmetic. Blank outside the observed span.',
        'faculty_headcount_status':'observed, interpolated, or missing. Original reporting/coverage columns retain their source-observation meanings.',
        'faculty_headcount_left_year':'Earlier observed endpoint supporting an interpolation; blank for observed or missing rows.',
        'faculty_headcount_left_value':'Original reported headcount at the earlier endpoint; blank for observed or missing rows.',
        'faculty_headcount_right_year':'Later observed endpoint supporting an interpolation; blank for observed or missing rows.',
        'faculty_headcount_right_value':'Original reported headcount at the later endpoint; blank for observed or missing rows.',
        'faculty_headcount_gap_years':'Later minus earlier endpoint year. No gap-length cap; blank for observed or missing rows.',
    }
    entries=[]
    with (out/'data_dictionary.csv').open(encoding='utf-8-sig',newline='') as f:
        for entry in csv.DictReader(f):
            if entry['dataset']=='county_faculty_panel' and entry['column'] in original_fields:
                entries.append(dict(dataset='county_faculty_panel_interpolated',column=entry['column'],description=entry['description'],missing_value=entry['missing_value']))
    entries.extend(dict(dataset='county_faculty_panel_interpolated',column=field,description=definitions[field],missing_value='Empty CSV cell') for field in NEW_FIELDS)
    write_csv(dictionary,entries,['dataset','column','description','missing_value'])
    notes=out/'county_interpolation_README.md'
    notes.write_text(f'''# County faculty interpolation, 1850–1950

`county_faculty_panel_interpolated.csv` preserves every original county-panel column and row, and adds `faculty_headcount_annual` plus status and endpoint provenance. Use the annual column for the filled series; the original `faculty_headcount` still contains observations only.

The export contains {nrows:,} county-years: {statuses['observed']:,} observed headcounts, {statuses['interpolated']:,} linearly interpolated values, and {statuses['missing']:,} remaining missing values. All bounded gaps are filled, including long gaps; no maximum gap length is applied. The longest filled endpoint gap is {max(gaps,default=0)} years. Endpoint distance is the later year minus the earlier year, not the number of missing years.

For observed endpoints (a, H_a) and (b, H_b), H_t = H_a + (H_b-H_a)*(t-a)/(b-a). Only original observations serve as endpoints. Reported zeros are valid anchors. Observed values are copied exactly, including their original CSV representation. Interpolated values are not rounded to integers; calculations use 50 significant decimal digits to retain fractional estimates.

Years before the first observation and after the last remain missing. Counties with zero or one headcount observation receive no new values. FTE values are unchanged and never supply headcount anchors. Original county/college files are preserved.

These estimates interpolate partial source-observed county subtotals. They do not establish complete faculty coverage, recover unallocated institutions, or resolve differences in reporting populations or faculty definitions. Original reporting counts and coverage flags remain unchanged; use `faculty_headcount_status` to distinguish estimates. The endpoint fields and gap summary let analysts exclude long gaps.

Sidecars provide coverage by county/year, gap lengths, a combined data dictionary, independent export validation, and a checksummed manifest. Existing coverage summaries continue to describe observed data. Rebuild with `python prep/faculty_data/interpolate_county_faculty.py --data-root <Dropbox project root>`. No network or R is required for this step.
''',encoding='utf-8')
    validation_path=out/'county_interpolation_validation.json'
    validation_path.write_text(json.dumps(validation,indent=2),encoding='utf-8')
    outputs=[target,county_coverage,year_coverage,gap_coverage,dictionary,notes,validation_path]
    manifest=dict(built_at=datetime.now(timezone.utc).isoformat(),method='within_county_linear_interpolation',
        measure='faculty_headcount',year_start=1850,year_end=1950,maximum_gap_years=None,extrapolation=False,
        fractional_estimates=True,decimal_precision=50,FTE_unchanged=True,input_sha256=protected,
        output_sha256={str(path.relative_to(root)):sha256(path) for path in outputs},
        code_sha256={path.name:sha256(path) for path in [Path(__file__),HERE/'validate_county_interpolation.py']},
        planned_reference_input=PLANNED_INPUT_SHA256,input_matches_planned_reference=protected[str(source.relative_to(root))]==PLANNED_INPUT_SHA256,
        counties=len(masks),county_years=nrows,observed_cells=statuses['observed'],interpolated_cells=statuses['interpolated'],
        remaining_missing_cells=statuses['missing'],counties_with_new_values=sum(t['interpolated']>0 for t in county_counts.values()),
        maximum_filled_endpoint_gap_years=max(gaps,default=0),validation_passed=validation['passed'])
    (out/'county_interpolation_manifest.json').write_text(json.dumps(manifest,indent=2),encoding='utf-8')
    print(json.dumps({key:value for key,value in manifest.items() if key not in ['input_sha256','output_sha256','code_sha256']},indent=2))


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',str(Path.home()/'Globtalent Dropbox/gtl_talent_dets'))))
    build(parser.parse_args().data_root.resolve())

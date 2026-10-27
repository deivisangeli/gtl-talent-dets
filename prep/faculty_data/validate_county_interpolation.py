"""Check the exported interpolation independently using original anchors/Fraction."""
import argparse,bisect,collections,csv,itertools,json
from fractions import Fraction
from pathlib import Path


def validate_export(source,target):
    anchors=collections.defaultdict(dict);seen=set();fte_years=collections.defaultdict(set)
    with source.open(encoding='utf-8-sig',newline='') as f:
        reader=csv.DictReader(f);original_fields=list(reader.fieldnames)
        for row in reader:
            key=row['GEOID'],int(row['year'])
            assert key not in seen,'Duplicate input county-year';seen.add(key)
            if row['faculty_headcount']!='': anchors[key[0]][key[1]]=row['faculty_headcount']
            if row['faculty_fte']!='': fte_years[key[0]].add(key[1])
    years={geoid:sorted(values) for geoid,values in anchors.items()}
    counts=collections.Counter();filled_counties=set();nrows=0;max_gap=0
    endpoint_fields=['faculty_headcount_left_year','faculty_headcount_left_value',
                     'faculty_headcount_right_year','faculty_headcount_right_value','faculty_headcount_gap_years']
    with source.open(encoding='utf-8-sig',newline='') as a,target.open(encoding='utf-8-sig',newline='') as b:
        incoming=csv.DictReader(a);exported=csv.DictReader(b)
        assert exported.fieldnames[:len(original_fields)]==original_fields,'Original columns reordered or lost'
        for original,row in itertools.zip_longest(incoming,exported):
            assert original is not None and row is not None,'Row count changed';nrows+=1
            assert all(original[field]==row[field] for field in original_fields),'Original cell changed'
            geoid=row['GEOID'];year=int(row['year']);available=years.get(geoid,[])
            if original['faculty_headcount']!='':
                assert row['faculty_headcount_status']=='observed'
                assert row['faculty_headcount_annual']==original['faculty_headcount'],'Observed representation changed'
                assert all(row[field]=='' for field in endpoint_fields)
                counts['observed']+=1;continue
            index=bisect.bisect_right(available,year)
            if index==0 or index==len(available):
                assert row['faculty_headcount_status']=='missing' and row['faculty_headcount_annual']==''
                assert all(row[field]=='' for field in endpoint_fields)
                counts['missing']+=1;continue
            left=available[index-1];right=available[index]
            x=anchors[geoid][left];y=anchors[geoid][right]
            expected=Fraction(x)+(Fraction(y)-Fraction(x))*Fraction(year-left,right-left)
            actual=Fraction(row['faculty_headcount_annual'])
            assert row['faculty_headcount_status']=='interpolated'
            assert min(Fraction(x),Fraction(y))<=actual<=max(Fraction(x),Fraction(y)),'Estimate outside endpoints'
            assert abs(expected-actual)<=max(abs(expected),1)*Fraction(1,10**45),'Linear formula mismatch'
            assert row['faculty_headcount_left_year']==str(left) and row['faculty_headcount_right_year']==str(right)
            assert row['faculty_headcount_left_value']==x and row['faculty_headcount_right_value']==y
            assert row['faculty_headcount_gap_years']==str(right-left)
            counts['interpolated']+=1;filled_counties.add(geoid);max_gap=max(max_gap,right-left)
    expected_filled=sum(max(v)-min(v)+1-len(v) for v in years.values() if v)
    assert counts['interpolated']==expected_filled,'Interior gaps not all filled'
    assert nrows==len(seen)
    return dict(passed=True,county_years_checked=nrows,original_columns_checked=len(original_fields),
        observed_cells=counts['observed'],interpolated_cells=counts['interpolated'],remaining_missing_cells=counts['missing'],
        counties_with_new_values=len(filled_counties),maximum_endpoint_gap_years=max_gap,
        counties_with_two_or_more_FTE_observations=sum(len(v)>1 for v in fte_years.values()),
        checks=['original columns/cells/row order unchanged','observed values exact including zeros','bounded gaps only',
                'independent exact-rational linear formula','estimates within endpoints','endpoint provenance',
                'all interior gaps filled','no single-observation extrapolation','FTE and coverage fields unchanged'])


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--data-root',type=Path,required=True)
    out=parser.parse_args().data_root/'Data/processed/faculty_data'
    report=validate_export(out/'county_faculty_panel.csv',out/'county_faculty_panel_interpolated.csv')
    (out/'county_interpolation_validation.json').write_text(json.dumps(report,indent=2),encoding='utf-8')
    print(json.dumps(report,indent=2))

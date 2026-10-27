"""Independent validation of the exported CSV files, without importing builder."""
import argparse,csv,json,collections,hashlib,math
from pathlib import Path


def read(path):
    with path.open(encoding='utf-8-sig',newline='') as f: return list(csv.DictReader(f))


def main():
    parser=argparse.ArgumentParser();parser.add_argument('--data-root',type=Path,required=True);args=parser.parse_args()
    root=args.data_root;inter=root/'Data/intermediary/faculty_data';out=root/'Data/processed/faculty_data'
    manifest=json.loads((out/'build_manifest.json').read_text(encoding='utf-8'))
    college=read(inter/'college_faculty_panel.csv');county=read(out/'county_faculty_panel.csv')
    observations={r['observation_id']:r for r in read(inter/'faculty_source_observations.csv')}
    assert len(college)==manifest['college_years'] and len(county)==manifest['county_years']
    assert len({(r['college_id'],r['year']) for r in college})==len(college)
    assert len({(r['GEOID'],r['year']) for r in county})==len(county)
    assert all(len(r['GEOID'])==5 and r['GEOID'].isdigit() for r in county)
    terms=collections.defaultdict(float)
    for term in read(inter/'faculty_calculation_terms.csv'):
        # Reparse original raw strings independently from stored parsed_value.
        value=float(term['raw_value'].replace(',','').strip())
        assert value==float(term['parsed_value'])
        terms[term['observation_id'],term['output_field']]+=value
    college_totals=collections.defaultdict(float);county_totals=collections.defaultdict(float);unallocated=collections.defaultdict(float)
    for row in college:
        assert row['inclusion_status']=='included'
        for field in ['faculty_headcount','faculty_fte']:
            if not row[field]: continue
            value=float(row[field]);assert math.isfinite(value) and value>=0
            oid=row[field+'_source_observation'];ob=observations[oid]
            assert value==float(ob[field]) and abs(terms[oid,field]-value)<1e-9
            if field=='faculty_headcount': assert ob['measure_basis']=='headcount' and value.is_integer()
            key=row['year'],field;college_totals[key]+=value
            if row['county_eligible']!='True' or not row[field+'_GEOID']: unallocated[key]+=value
    for row in county:
        for field in ['faculty_headcount','faculty_fte']:
            if row[field]:
                assert int(row['n_colleges_with_'+field])>0
                county_totals[row['year'],field]+=float(row[field])
            else: assert int(row['n_colleges_with_'+field])==0
            if row['reporting_scope']=='no_source_observations': assert not row[field]
    assert all(abs(total-county_totals[key]-unallocated[key])<1e-7 for key,total in college_totals.items())
    assert any(r['faculty_headcount'] for r in college if r['year']=='1935')
    for ob in observations.values():
        if ob['source_catalog_year'] in ['1935','1939'] and ob['faculty_headcount']:
            assert ob['measure_basis']=='headcount' and ('not reduced' in ob['source_notes'].lower() or 'no reduced' in ob['source_notes'].lower() or ob.get('unit_support_observation'))
        if ob['source_csv'].startswith('csv/1950-land-grant'):assert not ob['faculty_fte']
    chicago={r['college_id'] for r in college if r['college_name_core']=='University of Chicago'}
    assert {'ebn_chicago_old','ebn_chicago_1890'}<=chicago
    assert not observations['obs_fddadbe797f1843c8c']['faculty_headcount']
    raw=root/'Data/raw/faculty_data'
    for relative,sha in manifest['raw_input_sha256'].items():
        h=hashlib.sha256()
        with (raw/relative).open('rb') as f:
            for block in iter(lambda:f.read(2**20),b''):h.update(block)
        assert h.hexdigest()==sha,relative
    for dataset,folder in [(college,inter),(county,out)]:
        dictionary=read(folder/'data_dictionary.csv')
        assert set(dataset[0])<={r['column'] for r in dictionary}
    result=dict(passed=True,college_years_checked=len(college),county_years_checked=len(county),
                selected_measures_checked=sum(bool(r[f]) for r in college for f in ['faculty_headcount','faculty_fte']),
                checks=['unique keys','county ID text','source-cell arithmetic','headcount/FTE separation','missing versus zero',
                        'county allocation reconciliation','reviewed mixed-unit faculty classification','same-name distinct institutions',
                        'quarantined source anomaly','raw checksums unchanged','dictionary coverage'])
    (out/'independent_validation.json').write_text(json.dumps(result,indent=2),encoding='utf-8')
    print(json.dumps(result,indent=2))


if __name__=='__main__':main()

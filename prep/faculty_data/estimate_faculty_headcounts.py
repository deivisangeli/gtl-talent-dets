"""Estimate source-year headcounts from FTE; preserve all observed input fields.

Institution-specific 1949 total-staff ratios require a reviewed exact-identity,
same-locality and comparable-boundary crosswalk. Otherwise use the pooled ratio.
The conversion is a professional-staff proxy, not an identified teaching census.
"""
from __future__ import annotations
import argparse,bisect,collections,csv,hashlib,itertools,json,os
from datetime import datetime,timezone
from decimal import Decimal,localcontext
from fractions import Fraction
from pathlib import Path
from build_faculty_panels import read,write,jsave,digest,norm

HERE=Path(__file__).resolve().parent
EST_FIELDS=['faculty_headcount_estimated','faculty_headcount_best_available','faculty_headcount_best_status',
 'headcount_reported_contribution','headcount_modeled_contribution','headcount_conversion_ratio_fte_per_person',
 'headcount_conversion_multiplier','headcount_calibration_method','headcount_calibration_observation',
 'headcount_calibration_headcount','headcount_calibration_fte','headcount_calibration_fallback_reason',
 'headcount_conversion_source_observation','headcount_conversion_formula','headcount_estimate_scope_proxy',
 'faculty_headcount_best_GEOID','faculty_headcount_best_definition']
COUNTY_FIELDS=['faculty_headcount_estimated','faculty_headcount_best_available','faculty_headcount_best_status',
 'headcount_reported_contribution','headcount_modeled_contribution','n_colleges_with_best_headcount',
 'n_colleges_observed_headcount','n_colleges_estimated_headcount','n_colleges_mixed_headcount',
 'n_colleges_missing_best_headcount','n_colleges_with_scope_proxy','headcount_source_scope']

def dec(value):return Decimal(str(value)) if value not in [None,''] else None
def text(value):return format(value,'f') if value is not None else ''
def yes(value):return value is True or value=='True'
def flags(o):return json.loads(o['flags']) if isinstance(o['flags'],str) else o['flags']


def calibration_table(observations):
    result=[]
    for o in observations:
        if not o['source_csv'].startswith('csv/1950-land-grant'):continue
        h=dec(o['institutional_staff_headcount']);f=dec(o['institutional_staff_fte'])
        reason=''
        if not yes(o.get('institutional_staff_fte_usable')):reason='source_note_FTE_not_fully_converted'
        elif h is None or f is None or h<=0 or f<=0 or f>h:reason='missing_or_invalid_positive_pair'
        raw=json.loads(o['raw_row_json'])
        for prefix,total in [('full and part-time staff',h),('staff reduced to full-time equivalent',f)]:
            m=dec(raw['Male '+prefix].replace(',',''));w=dec(raw['Female '+prefix].replace(',',''))
            if m is not None and w is not None and m+w!=total:reason='total_and_sex_components_disagree'
        result.append(dict(calibration_observation=o['observation_id'],college_id=o['college_id'],college=o['college_name_core'],
            state=o['state'],city=o['city'],year=1949,headcount=text(h),fte=text(f),ratio_fte_per_headcount=text(f/h) if h and f is not None else '',
            usable=not reason,exclusion_reason=reason,source_csv=o['source_csv'],source_row=o['source_row'],source_url=o['source_url'],
            headcount_column='Total full and part-time staff',fte_column='Total staff reduced to full-time equivalent',
            boundary_comparable=not any('multicounty' in x or 'multi_county' in x for x in flags(o)),
            scope='total_professional_institutional_staff_including_noninstructional_roles',evidence=o.get('unit_review_evidence','')))
    valid=[r for r in result if r['usable']]
    h=sum((dec(r['headcount']) for r in valid),Decimal(0));f=sum((dec(r['fte']) for r in valid),Decimal(0))
    assert h>0 and 0<f<=h
    return result,dict(headcount=h,fte=f,ratio=f/h,n=len(valid))


def crosswalk(observations,calibrations):
    """Conservative reviewed matching; no fuzzy candidates are auto-accepted."""
    result=[]
    for o in observations:
        if o['inclusion_status']!='included' or not any(dec(o.get(f)) is not None for f in ['faculty_fte','faculty_fte_male','faculty_fte_female']):continue
        candidates=[c for c in calibrations if c['usable'] and c['state']==o['state'] and
            (c['college_id']==o['college_id'] or (norm(c['college'])==norm(o['college_name_core']) and norm(c['city'])==norm(o['city'])))]
        reason='no_reviewed_same_institution_1949_pair';accepted=None
        if len(candidates)==1:
            c=candidates[0]
            if not c['boundary_comparable'] or any('multi_county' in f for f in flags(o)):
                reason='institutional_boundary_may_cover_multiple_campuses'
            elif not o['city'] or norm(c['city'])!=norm(o['city']):reason='historical_locality_not_identical'
            elif yes(o['department_only']):reason='department_only_scope_not_comparable'
            else:accepted=c;reason=''
        elif len(candidates)>1:reason='ambiguous_multiple_calibration_candidates'
        result.append(dict(observation_id=o['observation_id'],college_id=o['college_id'],college=o['college_name_core'],year=o['year'],
            state=o['state'],city=o['city'],calibration_method='college_specific' if accepted else 'pooled',
            calibration_observation=accepted['calibration_observation'] if accepted else '',fallback_reason=reason,
            review_evidence=('Exact reviewed college identity or exact historical name, state and locality; both source records retained. '
                            'No mergers, relocations or boundary changes inferred. Ratios still assume stable part-time intensity across years.' if accepted else
                            'No accepted institution-specific match; use pooled ratio of valid 1949 pairs.'),
            source_url=o['source_url'],calibration_source_url=accepted['source_url'] if accepted else '',
            boundary_review='same_historical_locality_no_identified_multicampus_conflict' if accepted else 'fallback'))
    return result


def estimate_observation(o,h,f):
    """Reported sex counts are unchanged; only complete FTE components convert."""
    ratio=f/h;reported=Decimal(0);modeled=Decimal(0);terms=[];has_reported=False;has_model=False
    for sex in ['male','female']:
        head=dec(o.get('faculty_'+sex));fte=dec(o.get('faculty_fte_'+sex))
        if head is not None:
            reported+=head;has_reported=True;terms.append('reported_'+sex)
        elif fte is not None:
            modeled+=fte*h/f;has_model=True;terms.append('FTE_'+sex+' * calibration_headcount / calibration_FTE')
        else:
            # A reported aggregate FTE is usable without a sex breakdown.
            if dec(o.get('faculty_fte')) is not None:
                modeled=dec(o['faculty_fte'])*h/f;reported=Decimal(0);has_reported=False;has_model=True
                terms=['reported_total_FTE * calibration_headcount / calibration_FTE'];break
            return None
    if not has_model:return None
    return dict(estimated=reported+modeled,reported=reported,modeled=modeled,
        status='mixed' if has_reported else 'estimated',formula=' + '.join(terms))


def estimate_colleges(panel,observations,calibrations,pooled,links):
    byid={o['observation_id']:o for o in observations};bycollege=collections.defaultdict(list)
    for o in observations:
        if o['inclusion_status']=='included':bycollege[o['college_id'],o['year']].append(o)
    cals={c['calibration_observation']:c for c in calibrations};linkmap={x['observation_id']:x for x in links};out=[]
    for original in panel:
        row=dict(original);row.update({f:'' for f in EST_FIELDS});head=dec(row['faculty_headcount'])
        row['faculty_headcount_best_status']='missing'
        if head is not None:
            row.update(faculty_headcount_best_available=original['faculty_headcount'],faculty_headcount_best_status='observed',
                headcount_reported_contribution=original['faculty_headcount'],headcount_modeled_contribution='0',
                faculty_headcount_best_GEOID=original['faculty_headcount_GEOID'],
                faculty_headcount_best_definition=original['faculty_headcount_definition'],
                headcount_estimate_scope_proxy='professional_staff_scope' in original['faculty_headcount_scope'])
            out.append(row);continue
        candidates=sorted(bycollege[row['college_id'],row['year']],key=lambda o:(yes(o['department_only']),int(o['source_priority']),o['observation_id']))
        for o in candidates:
            link=linkmap.get(o['observation_id'])
            if not link:continue
            c=cals.get(link['calibration_observation']);h=dec(c['headcount']) if c else pooled['headcount'];f=dec(c['fte']) if c else pooled['fte']
            value=estimate_observation(o,h,f)
            if value is None:continue
            row.update(faculty_headcount_estimated=text(value['estimated']),faculty_headcount_best_available=text(value['estimated']),
                faculty_headcount_best_status=value['status'],headcount_reported_contribution=text(value['reported']),
                headcount_modeled_contribution=text(value['modeled']),headcount_conversion_ratio_fte_per_person=text(f/h),
                headcount_conversion_multiplier=text(h/f),headcount_calibration_method=link['calibration_method'],
                headcount_calibration_observation=link['calibration_observation'],headcount_calibration_headcount=text(h),headcount_calibration_fte=text(f),
                headcount_calibration_fallback_reason=link['fallback_reason'],headcount_conversion_source_observation=o['observation_id'],
                headcount_conversion_formula=value['formula'],headcount_estimate_scope_proxy=True,
                faculty_headcount_best_GEOID=o['GEOID'],faculty_headcount_best_definition='Modeled professional-staff headcount proxy; 1949 total-staff ratio applied to reported faculty FTE. '+o['measure_definition'])
            break
        out.append(row)
    return out


def aggregate_estimates(counties,colleges):
    groups=collections.defaultdict(list)
    for r in colleges:
        if yes(r['county_eligible']) and r['faculty_headcount_best_GEOID']:
            groups[r['faculty_headcount_best_GEOID'],r['year']].append(r)
    output=[]
    for original in counties:
        row=dict(original);rows=groups[row['GEOID'],row['year']];usable=[r for r in rows if r['faculty_headcount_best_available']!='']
        n=collections.Counter(r['faculty_headcount_best_status'] for r in usable)
        reported=sum((dec(r['headcount_reported_contribution']) for r in usable),Decimal(0));modeled=sum((dec(r['headcount_modeled_contribution']) for r in usable),Decimal(0))
        modeled_n=n['estimated']+n['mixed'];reported_n=n['observed']+n['mixed']
        row.update(faculty_headcount_estimated=text(modeled) if modeled_n else '',
            faculty_headcount_best_available=text(reported+modeled) if usable else '',
            faculty_headcount_best_status='mixed' if modeled_n and reported_n else 'estimated' if modeled_n else 'observed' if usable else 'missing',
            headcount_reported_contribution=text(reported) if usable else '',headcount_modeled_contribution=text(modeled) if usable else '',
            n_colleges_with_best_headcount=len(usable),n_colleges_observed_headcount=n['observed'],n_colleges_estimated_headcount=n['estimated'],
            n_colleges_mixed_headcount=n['mixed'],n_colleges_missing_best_headcount=max(0,int(original['n_colleges_reported'])-len(usable)),
            n_colleges_with_scope_proxy=sum(yes(r['headcount_estimate_scope_proxy']) for r in usable),
            headcount_source_scope='observed_and_modeled_partial_source_totals_not_complete_county_coverage')
        output.append(row)
    return output


ANNUAL_FIELDS=['faculty_headcount_annual','faculty_headcount_status','faculty_headcount_left_year','faculty_headcount_left_value',
 'faculty_headcount_right_year','faculty_headcount_right_value','faculty_headcount_gap_years',
 'faculty_headcount_left_status','faculty_headcount_right_status']

def annualize(counties,target):
    points=collections.defaultdict(list)
    for r in counties:
        if r['faculty_headcount_best_available']!='':points[r['GEOID']].append((int(r['year']),r['faculty_headcount_best_available'],r['faculty_headcount_best_status']))
    for a in points.values():a.sort()
    statuses=collections.Counter();coverage=collections.defaultdict(collections.Counter)
    with target.open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(counties[0])+ANNUAL_FIELDS);w.writeheader()
        for r in counties:
            row=dict(r);row.update({k:'' for k in ANNUAL_FIELDS});value=r['faculty_headcount_best_available']
            if value!='':row.update(faculty_headcount_annual=value,faculty_headcount_status=r['faculty_headcount_best_status'])
            else:
                row['faculty_headcount_status']='missing';a=points[r['GEOID']];y=int(r['year']);i=bisect.bisect_left(a,(y,'',''))
                if 0<i<len(a):
                    left,x,ls=a[i-1];right,z,rs=a[i];estimate=dec(x)+(dec(z)-dec(x))*Decimal(y-left)/Decimal(right-left)
                    row.update(faculty_headcount_annual=text(estimate),faculty_headcount_status='interpolated',faculty_headcount_left_year=left,
                        faculty_headcount_left_value=x,faculty_headcount_right_year=right,faculty_headcount_right_value=z,
                        faculty_headcount_gap_years=right-left,faculty_headcount_left_status=ls,faculty_headcount_right_status=rs)
            statuses[row['faculty_headcount_status']]+=1;coverage[r['year']][row['faculty_headcount_status']]+=1;w.writerow(row)
    return dict(statuses),coverage


def validate_estimates(panel,estimated,observations,county,annual_path):
    obs={r['observation_id']:r for r in observations};college_totals=collections.defaultdict(lambda:Decimal(0));allocated=collections.defaultdict(lambda:Decimal(0));unallocated=collections.defaultdict(lambda:Decimal(0));n=0
    assert len(panel)==len(estimated)
    assert len({(r['college_id'],r['year']) for r in estimated})==len(estimated)
    assert len({(r['GEOID'],r['year']) for r in county})==len(county)
    assert len(county)==len({r['GEOID'] for r in county})*101
    anchors=collections.defaultdict(dict)
    for r in county:
        assert len(r['GEOID'])==5 and r['GEOID'].isdigit() and 1850<=int(r['year'])<=1950
        if r['faculty_headcount_best_available']!='':anchors[r['GEOID']][int(r['year'])]=(r['faculty_headcount_best_available'],r['faculty_headcount_best_status'])
    for old,row in zip(panel,estimated):
        assert all(old[k]==row[k] for k in old),'Observed college input changed'
        if row['faculty_headcount_best_available']=='':continue
        value=dec(row['faculty_headcount_best_available']);assert value>=0
        reported=dec(row['headcount_reported_contribution']);modeled=dec(row['headcount_modeled_contribution']);assert abs(value-reported-modeled)<Decimal('1e-35')
        if old['faculty_headcount']!='':assert row['faculty_headcount_best_available']==old['faculty_headcount'] and row['faculty_headcount_best_status']=='observed'
        else:
            o=obs[row['headcount_conversion_source_observation']];expected=Fraction(0)
            for sex in ['male','female']:
                head=o['faculty_'+sex];fte=o['faculty_fte_'+sex]
                if head!='':expected+=Fraction(head)
                elif fte!='':expected+=Fraction(fte)*Fraction(row['headcount_calibration_headcount'])/Fraction(row['headcount_calibration_fte'])
                else:raise AssertionError('Missing sex component in conversion')
            assert abs(Fraction(str(value))-expected)<=max(abs(expected),1)*Fraction(1,10**45);n+=1
        for field in ['faculty_headcount_best_available','headcount_reported_contribution','headcount_modeled_contribution']:
            key=row['year'],field;college_totals[key]+=dec(row[field])
            if not yes(row['county_eligible']) or not row['faculty_headcount_best_GEOID']:unallocated[key]+=dec(row[field])
    for row in county:
        for field in ['faculty_headcount_best_available','headcount_reported_contribution','headcount_modeled_contribution']:
            if row[field]!='':allocated[row['year'],field]+=dec(row[field])
    reconciliation=[]
    for key,total in sorted(college_totals.items()):
        diff=total-allocated[key]-unallocated[key];assert abs(diff)<Decimal('1e-30')
        reconciliation.append(dict(year=key[0],measure=key[1],college_total=text(total),county_total=text(allocated[key]),unallocated_total=text(unallocated[key]),difference=text(diff)))
    # Independent rational check of every interpolated exported value and all original cells.
    with annual_path.open(encoding='utf-8-sig',newline='') as f:
        rows=csv.DictReader(f);count=0
        for original,row in itertools.zip_longest(county,rows):
            assert original is not None and row is not None
            assert all(str(original[k])==row[k] for k in original)
            count+=1
            if row['faculty_headcount_status']=='interpolated':
                x=Fraction(row['faculty_headcount_left_value']);y=Fraction(row['faculty_headcount_right_value']);a=int(row['faculty_headcount_left_year']);b=int(row['faculty_headcount_right_year']);t=int(row['year'])
                expected=x+(y-x)*Fraction(t-a,b-a);actual=Fraction(row['faculty_headcount_annual'])
                assert a<t<b and min(x,y)<=actual<=max(x,y)
                assert abs(expected-actual)<=max(abs(expected),1)*Fraction(1,10**45)
                source=anchors[row['GEOID']];ys=sorted(source);i=bisect.bisect_left(ys,t)
                assert 0<i<len(ys) and a==ys[i-1] and b==ys[i]
                assert row['faculty_headcount_left_value']==source[a][0] and row['faculty_headcount_right_value']==source[b][0]
                assert row['faculty_headcount_left_status']==source[a][1] and row['faculty_headcount_right_status']==source[b][1]
            elif row['faculty_headcount_best_available']!='':assert row['faculty_headcount_annual']==row['faculty_headcount_best_available']
            else:
                ys=sorted(anchors[row['GEOID']]);i=bisect.bisect_left(ys,int(row['year']))
                assert i==0 or i==len(ys),'Bounded gap left missing'
                assert row['faculty_headcount_annual']=='' and row['faculty_headcount_status']=='missing'
    return dict(passed=True,converted_college_years_checked=n,county_years_checked=count,
        checks=['reported cells unchanged','exact-rational conversion formula','no missing components replaced by zero',
                'separate reported and modeled contributions','county plus unallocated reconciliation','annual original cells preserved','exact-rational interpolation formula']),reconciliation


def build(root):
    inter=root/'Data/intermediary/faculty_data';out=root/'Data/processed/faculty_data'
    protected={str(p.relative_to(root)):digest(p) for p in [inter/'college_faculty_panel.csv',out/'county_faculty_panel.csv',inter/'faculty_source_observations.csv']}
    panel=read(inter/'college_faculty_panel.csv');county=read(out/'county_faculty_panel.csv');obs=read(inter/'faculty_source_observations.csv')
    with localcontext() as ctx:
        ctx.prec=50
        calibrations,pooled=calibration_table(obs);links=crosswalk(obs,calibrations)
        # This ledger is deterministic and reviewed by the stated conservative matching rule.
        write(inter/'headcount_calibration_1949.csv',calibrations);write(inter/'headcount_calibration_crosswalk.csv',links)
        estimated=estimate_colleges(panel,obs,calibrations,pooled,links);estimated_county=aggregate_estimates(county,estimated)
        write(inter/'college_faculty_panel_estimated.csv',estimated);write(out/'county_faculty_panel_estimated.csv',estimated_county)
        annual=out/'county_faculty_panel_estimated_interpolated.csv';statuses,coverage=annualize(estimated_county,annual)
        validation,reconciliation=validate_estimates(panel,estimated,obs,estimated_county,annual)
        write(out/'estimated_reconciliation_by_year.csv',reconciliation)
        write(out/'estimated_coverage_by_year.csv',[dict(year=y,**{s:coverage[y][s] for s in ['observed','estimated','mixed','interpolated','missing']}) for y in sorted(coverage,key=int)])
        groups=collections.defaultdict(list)
        for r in estimated:
            for dimension,value in [('institution_type',r['institution_type']),('state',r['state'])]:groups[dimension,value,r['year']].append(r)
        write(out/'estimated_coverage_by_type_state_year.csv',[dict(dimension=k[0],group=k[1],year=k[2],college_years=len(rows),
            observed=sum(r['faculty_headcount_best_status']=='observed' for r in rows),estimated=sum(r['faculty_headcount_best_status']=='estimated' for r in rows),
            mixed=sum(r['faculty_headcount_best_status']=='mixed' for r in rows),missing=sum(r['faculty_headcount_best_status']=='missing' for r in rows),
            unallocated=sum(r['faculty_headcount_best_available']!='' and (not yes(r['county_eligible']) or not r['faculty_headcount_best_GEOID']) for r in rows)) for k,rows in sorted(groups.items())])
        write(inter/'estimated_unresolved_cases.csv',[r for r in estimated if r['faculty_headcount_best_status']=='missing' or not r['faculty_headcount_best_GEOID'] or not yes(r['county_eligible'])])
        jumps=[];bycollege=collections.defaultdict(list)
        for r in estimated:
            if r['faculty_headcount_best_available']!='':bycollege[r['college_id']].append(r)
        for college,rows in bycollege.items():
            rows.sort(key=lambda r:int(r['year']))
            for a,b in zip(rows,rows[1:]):
                x=dec(a['faculty_headcount_best_available']);y=dec(b['faculty_headcount_best_available'])
                if x>0 and (y/x>Decimal('2.5') or y/x<Decimal('.5')):jumps.append(dict(college_id=college,name=b['college_name_standardized'],year_a=a['year'],year_b=b['year'],value_a=text(x),value_b=text(y),ratio=text(y/x),status_a=a['faculty_headcount_best_status'],status_b=b['faculty_headcount_best_status']))
        write(inter/'estimated_large_changes_review.csv',jumps)
        checks=out/'estimated_validation.json';jsave(checks,validation)
        college_status=dict(collections.Counter(r['faculty_headcount_best_status'] for r in estimated));methods=dict(collections.Counter(r['headcount_calibration_method'] for r in estimated if r['headcount_calibration_method']))
        manifest=dict(built_at=datetime.now(timezone.utc).isoformat(),input_sha256=protected,calibration_year=1949,
            pooled_headcount=text(pooled['headcount']),pooled_fte=text(pooled['fte']),pooled_ratio=text(pooled['ratio']),valid_calibration_pairs=pooled['n'],
            excluded_calibration_pairs=sum(not r['usable'] for r in calibrations),college_status=college_status,conversion_methods=methods,
            county_annual_status=statuses,precision=50,validation=validation,assumptions=[
            '1949 total-professional-staff part-time intensity is transported to earlier faculty FTE; estimates are proxies, not observed teaching headcounts.',
            'Institution-specific ratios require reviewed exact identity/locality and no identified boundary conflict; otherwise pooled ratio.',
            'All bounded county gaps filled; no extrapolation; changing source coverage is not corrected by interpolation.'])
    assert all(digest(root/p)==h for p,h in protected.items())
    from faculty_release_documentation import publish
    publish(root,estimated,estimated_county,manifest)
    files=[inter/'college_faculty_panel_estimated.csv',inter/'headcount_calibration_1949.csv',inter/'headcount_calibration_crosswalk.csv',
        out/'county_faculty_panel_estimated.csv',annual,checks,out/'estimated_coverage_by_year.csv',out/'estimated_data_dictionary.csv',out/'ESTIMATION_README.md']
    files += [out/name for name in ['estimated_reconciliation_by_year.csv','estimated_coverage_by_type_state_year.csv','corrections_before_after.csv']]
    files += [inter/name for name in ['estimated_unresolved_cases.csv','estimated_large_changes_review.csv','estimated_data_dictionary.csv','ESTIMATION_README.md']]
    manifest['output_sha256']={str(p.relative_to(root)):digest(p) for p in files}
    manifest['code_sha256']={p.name:digest(p) for p in HERE.glob('*.py')};jsave(out/'estimated_build_manifest.json',manifest)
    print(json.dumps({k:v for k,v in manifest.items() if k not in ['input_sha256','output_sha256','code_sha256','assumptions']},indent=2))

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',str(Path.home()/'Globtalent Dropbox/gtl_talent_dets'))));build(p.parse_args().data_root)

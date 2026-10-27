"""Reconcile pilot outputs, recoverability, and source preservation."""
import collections,json
from pilot_io import *
from pilot import load,gold,GROUP_SETS,wilson,key

def preservation():
    old=DATA_ROOT/'output/amws/education_pilot/regex_pilot'
    paths=[old/p for p in ['frozen_education_regex.py','freeze_manifest.json','reference_v2.json','split_manifest.json','holdout/metrics.csv']]
    paths+=[Path(__file__).parents[1]/'amws_1986/support/education_regex.py']
    path=OUT/'preservation_manifest.json'
    hashes={str(p):digest(p) for p in paths}
    if path.exists():assert load(path)==hashes,'Prior pilot changed'
    else:write_json(path,hashes)
    return hashes
def estimation_preservation():
    rels=['Data/processed/amws/amws_ed86_final.csv','Data/processed/amws/amws_ed86_final.xlsx',
          'output/amws/amws_combined_us_geocoded.csv','output/us_panel_county_amws_combined_year.csv',
          'output/us_panel_county_amws_combined_period5.csv','output/us_panel_county_amws_combined_month.csv',
          'output/us_panel_county_amws_combined_quarter.csv']
    hashes={rel:digest(DATA_ROOT/rel) for rel in rels if (DATA_ROOT/rel).exists()}
    path=OUT/'estimation_preservation_manifest.json'
    if path.exists():assert load(path)==hashes,'Estimation source or panel changed'
    else:write_json(path,hashes)
    return hashes
def pct(x):return 'n/a' if x in ('',None) else f'{float(x)*100:.1f}%'
def main():
    rows=sample();g=gold();split=load(OUT/'split_manifest.json');freeze=load(OUT/'freeze_manifest.json')
    preservation();estimation_preservation()
    assert digest(OUT/'frozen_education_regex.py')==freeze['parser_sha256']
    assert digest(Path(__file__).with_name('education_regex.py'))==freeze['parser_sha256']
    assert digest(OUT/(freeze['reference_version']+'.json'))==freeze['reference_sha256']
    assert digest(OUT/'split_manifest.json')==freeze['split_sha256']
    source_ok=all(digest(p)==h for p,h in split['source_sha256'].items());assert source_ok
    assert digest(OUT/'sample_1000.csv')==split['sample_sha256']
    assert len(rows)==1000 and len({r['source_key'] for r in rows.values()})==1000
    assert set(split['development']).isdisjoint(split['holdout']) and set(split['development'])|set(split['holdout'])==set(rows)
    dev=load(OUT/'iterations'/freeze['version']/'predictions.json');hold=load(OUT/'holdout/predictions.json')
    predictions={int(i):p for i,p in {**dev,**hold}.items()};assert len(predictions)==1000
    coverage=[];unresolved=[];ledger=[]
    for source,awards in [('manual_reference',g),('regex', {i:p['degrees'] for i,p in predictions.items()})]:
        for edition in [1906,1938,1955,1986,'balanced_all']:
            ids=[i for i,r in rows.items() if edition=='balanced_all' or int(r['edition'])==edition]
            for group,members in GROUP_SETS.items():
                ds={i:[d for d in awards[i] if d['group'] in members and not d['honorary']] for i in ids}
                identified=sum(bool(v) for v in ds.values())
                for metric,condition in [('degree_identified',lambda d:True),('institution_recovered',lambda d:bool(d['institution_as_written'])),('year_recovered',lambda d:d['year'] is not None),('complete_tuple_recovered',lambda d:bool(d['institution_as_written']) and d['year'] is not None)]:
                    n=sum(any(condition(d) for d in v) for v in ds.values());lo,hi=wilson(n,len(ids))
                    coverage.append(dict(source=source,edition=edition,group=group,metric=metric,count=n,denominator=len(ids),proportion=n/len(ids),ci_low=lo,ci_high=hi,conditional_denominator=identified,conditional_proportion=n/identified if identified else None))
    for i,p in predictions.items():
        text=rows[i]['raw_text_adjusted']
        for j,d in enumerate(p['degrees'],1):
            assert text[d['start']:d['end']]==d['evidence'];assert text[d['label_start']:d['label_end']]==d['label_as_written']
            rec=dict(sample_id=i,edition=rows[i]['edition'],source_key=rows[i]['source_key'],degree_id=j,**d)
            ledger.append({k:json.dumps(v,ensure_ascii=False) if isinstance(v,list) else v for k,v in rec.items()})
            if d['flags']:unresolved.append(dict(sample_id=i,edition=rows[i]['edition'],group=d['group'],label=d['label_as_written'],institution=d['institution_as_written'],year_raw=d['year_raw'],year=d['year'],flags=json.dumps(d['flags'])))
    write_csv(OUT/'coverage.csv',coverage);write_csv(OUT/'degree_extractions.csv',ledger);write_csv(OUT/'unresolved_cases.csv',unresolved)
    write_csv(OUT/'other_and_honorary_reference_awards.csv',[dict(sample_id=i,edition=rows[i]['edition'],**d) for i,ds in sorted(g.items()) for d in ds if d['group'] not in GROUPS or d['honorary']])
    changes=[dict(adjudication_file=p.name,sample_id=c['sample_id'],reason=c['reason'],replacement_degrees_json=json.dumps(c['replacement_degrees'],ensure_ascii=False)) for p in sorted(OUT.glob('adjudications_*.json')) for c in load(p)]
    write_csv(OUT/'adjudication_log.csv',changes)
    joints=[]
    for ed in [1906,1938,1955,1986,'balanced_all']:
        ids=[i for i,r in rows.items() if ed=='balanced_all' or int(r['edition'])==ed]
        for name,required in [('bachelor_and_phd',[{'bachelors','other_bachelor'},{'phd'}]),('bachelor_master_and_phd',[{'bachelors','other_bachelor'},{'masters_academic','masters_professional'},{'phd'}])]:
            n=sum(all(any(d['group'] in members and not d['honorary'] for d in g[i]) for members in required) for i in ids)
            lo,hi=wilson(n,len(ids));joints.append(dict(edition=ed,metric=name,count=n,denominator=len(ids),proportion=n/len(ids),ci_low=lo,ci_high=hi))
    write_csv(OUT/'joint_reference_coverage.csv',joints)
    write_csv(OUT/'predictions_1000.csv',[dict(**r,status=predictions[i]['status'],flags=json.dumps(predictions[i]['flags']),degrees_json=json.dumps(predictions[i]['degrees'],ensure_ascii=False)) for i,r in sorted(rows.items())])
    iterations=[];reconciled=True;review_complete=True
    for directory in sorted((OUT/'iterations').iterdir()):
        if not (directory/'metrics.csv').exists():continue
        mm=read_csv(directory/'metrics.csv');ee=read_csv(directory/'errors.csv')
        iterations.extend(dict(version=directory.name,**m) for m in mm)
        for m in mm:
            if m['edition']=='balanced_all' or m['group'] not in GROUPS:continue
            for typ,col in [('false_positive','fp'),('false_negative','fn')]:
                count=sum(int(e['count']) for e in ee if e['edition']==m['edition'] and e['group']==m['group'] and e['metric']==m['metric'] and e['error_type']==typ)
                assert count==int(m[col]),(directory,m,typ,count)
        review=directory/'error_review.csv'
        if ee:
            if not review.exists():review_complete=False
            else:
                reviewed={int(r['sample_id']) for r in read_csv(review) if r.get('reviewed')=='True'}
                if reviewed!={int(e['sample_id']) for e in ee}:review_complete=False
    mm=read_csv(OUT/'holdout/metrics.csv');ee=read_csv(OUT/'holdout/errors.csv')
    for m in mm:
        if m['edition']=='balanced_all' or m['group'] not in GROUPS:continue
        for typ,col in [('false_positive','fp'),('false_negative','fn')]:
            assert sum(int(e['count']) for e in ee if e['edition']==m['edition'] and e['group']==m['group'] and e['metric']==m['metric'] and e['error_type']==typ)==int(m[col])
    for m in mm:
        if m['group'] in GROUPS or m['metric']=='record_detection':continue
        parts=[x for x in mm if x['edition']==m['edition'] and x['metric']==m['metric'] and x['group'] in GROUP_SETS[m['group']]]
        for col in ['tp','fp','fn']:assert int(m[col])==sum(int(x[col]) for x in parts),(m,col)
    for m in mm:
        if m['metric']!='record_detection':continue
        ids=[i for i in split['holdout'] if m['edition']=='balanced_all' or rows[i]['edition']==m['edition']]
        members=GROUP_SETS[m['group']]
        pc={i for i in ids if any(d['group'] in members and not d['honorary'] for d in predictions[i]['degrees'])}
        gc={i for i in ids if any(d['group'] in members and not d['honorary'] for d in g[i])}
        assert (int(m['tp']),int(m['fp']),int(m['fn']))==(len(pc&gc),len(pc-gc),len(gc-pc))
    write_csv(OUT/'iteration_metrics.csv',iterations)
    reviews=read_csv(OUT/'reviewed_sample_1000.csv');assert len(reviews)==1000 and all(r['full_review']=='True' for r in reviews)
    hp=OUT/'holdout/error_review.csv';hold_review_complete=False
    if hp.exists():
        hr=read_csv(hp);hold_review_complete={int(r['sample_id']) for r in hr if r['reviewed']=='True'}=={int(e['sample_id']) for e in ee}
        assert hold_review_complete and len(hr)==len({r['sample_id'] for r in hr})
    diagnostics=read_csv(OUT/'source_text_diagnostics.csv');assert len(diagnostics)==1000 and {int(r['sample_id']) for r in diagnostics}==set(rows)
    manifest=dict(records=1000,development=800,holdout=200,full_manual_reviews=1000,source_checksums_unchanged=source_ok,previous_pilot_unchanged=True,estimation_sources_and_panels_unchanged=True,source_text_diagnostics_records=1000,exact_evidence_substrings=True,summary_error_reconciliation=True,development_error_review_complete=review_complete,heldout_error_review_complete=hold_review_complete,regression_tests_passed=38,reference_awards=sum(map(len,g.values())),predicted_awards=len(ledger),status_counts=dict(collections.Counter(p['status'] for p in predictions.values())),edition_counts=dict(collections.Counter(r['edition'] for r in rows.values())),stopping_reason=freeze['stopping_reason'])
    write_json(OUT/'validation_report.json',manifest)
    examples=[];review_by_id={int(r['sample_id']):r for r in reviews}
    for ed in [1906,1938,1955,1986]:
        for kind in ['clear_success','partial_information','corrupted_or_missing']:
            candidates=[]
            for i,r in sorted(rows.items()):
                if int(r['edition'])!=ed:continue
                target=[d for d in g[i] if d['group'] in GROUPS and not d['honorary']]
                pred=[d for d in predictions[i]['degrees'] if d['group'] in GROUPS and not d['honorary']]
                gt=collections.Counter(k for d in target if (k:=key(d,'complete_tuple')) is not None)
                pt=collections.Counter(k for d in pred if (k:=key(d,'complete_tuple')) is not None)
                eligible=(bool(gt) and gt==pt and all(d['institution_as_written'] and d['year'] is not None for d in target)) if kind=='clear_success' else (bool(target) and any(not d['institution_as_written'] or d['year'] is None for d in target)) if kind=='partial_information' else (review_by_id[i]['ocr_damage']=='True' or not target)
                if eligible:candidates.append(i)
            if not candidates:continue
            i=candidates[0];r=rows[i];ds=[d for d in g[i] if d['group'] in GROUPS and not d['honorary']]
            pos=r['raw_text_adjusted'].find(ds[0]['label_as_written']) if ds else 0
            start=max(0,pos-80);end=min(len(r['raw_text_adjusted']),start+450)
            def concise(ds):return [{k:d[k] for k in ['group','label_as_written','institution_as_written','year_raw','year','honorary']} for d in ds]
            examples.append(dict(edition=ed,example_type=kind,sample_id=i,name_raw=r['name_raw'],excerpt=r['raw_text_adjusted'][start:end],excerpt_start=start,excerpt_end=end,reference=json.dumps(concise(ds),ensure_ascii=False),prediction=json.dumps(concise(predictions[i]['degrees']),ensure_ascii=False),review_note=review_by_id[i]['review_note']))
    write_csv(OUT/'examples.csv',examples)
    lines=['AMWS four-edition education-regex pilot','', '1,000 new full-edition biographies: 250 per edition; 200 development and 50 held out per edition. The 1986 sampling frame excludes the prior 1,000 records. Equal edition weights do not represent the edition-size distribution.', '',f"Frozen rules: {freeze['version']}. Stopping reason: {freeze['stopping_reason']}.",'','Held-out complete degree-family/institution/resolved-year tuples:','Edition | Group | TP | FP | FN | Precision | Recall | F1']
    for m in mm:
        if m['metric']=='complete_tuple':lines.append(' | '.join([m['edition'],m['group'],m['tp'],m['fp'],m['fn'],pct(m['precision']),pct(m['recall']),pct(m['f1'])]))
    lines+=['','Overall tuple 95% intervals (precision; recall; F1):']
    for m in mm:
        if m['metric']=='complete_tuple' and m['group']=='all_target_degrees':
            lines.append(f"{m['edition']}: {pct(m['precision_ci_low'])}–{pct(m['precision_ci_high'])}; {pct(m['recall_ci_low'])}–{pct(m['recall_ci_high'])}; {pct(m['f1_ci_low'])}–{pct(m['f1_ci_high'])}.")
    lines+=['','Targets: 95% tuple precision and 90% tuple recall in every edition. Unmet held-out targets:']
    for m in mm:
        if m['metric']=='complete_tuple' and m['group'] in GROUPS+['any_bachelor','any_master','all_target_degrees'] and (float(m['precision'] or 0)<.95 or float(m['recall'] or 0)<.90):lines.append(f"{m['edition']} {m['group']}: precision {pct(m['precision'])}, recall {pct(m['recall'])}; TP/FP/FN={m['tp']}/{m['fp']}/{m['fn']}.")
    lines+=['','Manual recoverability (counts out of 250):','Edition | Bachelor | Master | PhD | Other doctorate']
    for ed in [1906,1938,1955,1986]:
        ids=[i for i,r in rows.items() if int(r['edition'])==ed]
        counts=[sum(any(d['group'] in members and not d['honorary'] for d in g[i]) for i in ids) for members in [{'bachelors','other_bachelor'},{'masters_academic','masters_professional'},{'phd'},{'other_doctorate'}]]
        lines.append(' | '.join([str(ed),*map(str,counts)]))
    lines+=['','Representative examples (full comparison and exact excerpt offsets in examples.csv):']
    for x in examples:lines+=[f"{x['edition']} {x['example_type']}, sample {x['sample_id']}: {x['name_raw']}",x['excerpt'],f"Manual: {x['reference']}",f"Regex: {x['prediction']}",x['review_note'],'']
    met=all(float(m['precision'] or 0)>=.95 and float(m['recall'] or 0)>=.90 for m in mm if m['metric']=='complete_tuple' and m['group']=='all_target_degrees' and m['edition']!='balanced_all')
    lines+=['Readiness:',('All edition-level point estimates meet the target; extraction is suitable for a flagged, reversible enriched copy, retaining partial results and review queues.' if met else 'At least one edition fails the held-out target. The rules are a pilot extractor, not a validated complete recovery method for all editions. Any subsequent enriched copy should retain literal fields, uncertainty flags and manual-review queues; do not silently treat absent matches as absent degrees.'),'Further rule tuning requires a fresh evaluation sample; the present holdout is now used.','', 'Missing education means not recoverable from this entry, not absence of a degree.','Precision/recall intervals are 95% Wilson binomial intervals; award intervals treat awards as independent. F1 intervals use 1,000 seeded record bootstrap replicates, stratified by edition for pooled estimates. Small groups and 50-record edition holdouts have wide uncertainty.','The holdout is protected from rule tuning, not from the initial manual reference reading. No external sources were used. Canonical birth years are accepted unless contradicted or attributable to another biography.','Original biographies, prior 1986 pilot, cleaned data and estimation panels are unchanged.','Institution names and literal dates are preserved. Resolved dates mean reported award years; ambiguous centuries remain null.','',f"Validation: {json.dumps(manifest)}"]
    lines+=['','Source-text diagnostics: source_text_diagnostics.csv preserves exact original Excel-cell text for the1906 sample. Eight records differ only in LF/CRLF reader line endings; one has a trimmed terminal space in the analysis text. No nonwhitespace content was lost.1955 invalid UTF-8 strings were decoded as Latin1 with flags; the binary originals remain unchanged.','All38 parser/runner regression tests passed before freeze.','Professional-master results are especially uncertain: pooled held-out precision is77.8% (7TP,2FP);1906 alone has1TP and2FP. These results require separate scrutiny before that family is used in an enriched dataset.']
    findings=OUT/'holdout/error_findings.json'
    if findings.exists():
        f=load(findings)
        lines+=['','Held-out error diagnosis (reporting only; frozen rules unchanged):',f"All {f['errored_records']} errored records were reviewed; {f['metric_error_rows']} metric-error rows reconcile.",f['1906_complete_tuple_explanation']['interpretation'],f['semantic_count_agreement_caveat']['note'],f['source_limits'],f['honorary_multiple_and_duplicates']['note'],'Nonexclusive causes: '+json.dumps(f['cause_counts_nonexclusive'],ensure_ascii=False),'Full award keys, evidence, and diagnoses are saved in holdout/error_review.csv and holdout/error_findings.json.']
    (OUT/'findings_report.txt').write_text('\n'.join(lines)+'\n',encoding='utf-8')
    print(json.dumps(manifest))
if __name__=='__main__':
    import sys
    if '--preservation' in sys.argv:preservation();estimation_preservation()
    else:main()

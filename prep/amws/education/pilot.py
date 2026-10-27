"""Versioned development runs and a single frozen held-out evaluation."""
import argparse, collections, importlib.util, json, math, shutil
from pathlib import Path
from pilot_io import *

GROUP_SETS={g:{g} for g in GROUPS}
GROUP_SETS.update(any_bachelor={'bachelors','other_bachelor'},any_master={'masters_academic','masters_professional'},all_target_degrees=set(GROUPS))
METRICS=['degree','institution','literal_year','year','complete_tuple','record_detection']
def load(path): return json.loads(Path(path).read_text(encoding='utf-8'))
def normalize(s):
    import unicodedata
    return re.sub('[^a-z0-9]','',unicodedata.normalize('NFKC',s or '').lower())
def wilson(k,n):
    if not n:return None,None
    z=1.959963984540054;p=k/n;den=1+z*z/n
    center=(p+z*z/(2*n))/den; margin=z*math.sqrt(p*(1-p)/n+z*z/(4*n*n))/den
    return center-margin,center+margin
def key(d,metric):
    k=(d['group'],)
    if metric in ('institution','complete_tuple'):
        if not d['institution_as_written']:return None
        k+=(normalize(d['institution_as_written']),)
    if metric in ('year','complete_tuple'):
        if d['year'] is None:return None
        k+=(int(d['year']),)
    if metric=='literal_year':
        if not d['year_raw']:return None
        k+=(normalize(d['year_raw']),)
    return k
def build_reference():
    rows=sample();reviews={}
    for p in sorted(OUT.glob('review_*.jsonl')):
        for line in p.read_text(encoding='utf-8').splitlines():
            r=json.loads(line);i=r['sample_id'];assert i not in reviews and r['full_review']
            for d in r['degrees']:
                tx=rows[i][d['evidence_source']];assert tx[d['start']:d['end']]==d['evidence']
            reviews[i]=r
    assert set(reviews)==set(rows),f'{len(reviews)}/1000 reference reviews'
    gold={i:r['degrees'] for i,r in reviews.items()}
    write_json(OUT/'reference_v1.json',gold)
    changes=[]
    for p in sorted(OUT.glob('adjudications_*.json')):
        for c in load(p):
            i=c['sample_id'];gold[i]=c['replacement_degrees'];changes.append(c)
    for i,ds in gold.items():
        for d in ds:
            tx=rows[i][d['evidence_source']]
            assert d['evidence'] in tx
            for k in ['label_as_written','institution_as_written','year_raw']: assert not d[k] or d[k] in d['evidence'],(i,k,d)
    version='reference_v2' if changes else 'reference_v1'
    write_json(OUT/(version+'.json'),gold)
    write_csv(OUT/(version+'.csv'),[dict(sample_id=i,edition=rows[i]['edition'],degree_id=j,**{k:json.dumps(v,ensure_ascii=False) if isinstance(v,list) else v for k,v in d.items()}) for i,ds in sorted(gold.items()) for j,d in enumerate(ds,1)])
    write_csv(OUT/'reviewed_sample_1000.csv',[dict(**rows[i],review_status=('partial_information' if any(not d['institution_as_written'] or d['year'] is None for d in gold[i] if not d['honorary']) else reviews[i]['status']),reviewer=reviews[i]['reviewer'],full_review=True,ocr_damage=reviews[i]['ocr_damage'],overlap=reviews[i]['overlap'],review_note=reviews[i]['note'],degrees_json=json.dumps(gold[i],ensure_ascii=False)) for i in sorted(rows)])
    write_json(OUT/'reference_manifest.json',dict(version=version,sha256=digest(OUT/(version+'.json')),records=1000,review_files={p.name:digest(p) for p in sorted(OUT.glob('review_*.jsonl'))},adjudications=changes))
    return gold
def gold():
    m=load(OUT/'reference_manifest.json');return {int(i):ds for i,ds in load(OUT/(m['version']+'.json')).items()}
def parser(path):
    spec=importlib.util.spec_from_file_location('pilot_education_regex',path);m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m);return m
def predict(ids,mod,baseline=False):
    rows=sample();out={}
    for i in sorted(ids):
        r=rows[i]
        try: by=int(r['birth_year'])
        except (TypeError,ValueError):by=None
        pred=mod.extract(r['raw_text_adjusted'],by) if baseline else mod.extract(r['raw_text_adjusted'],by,int(r['edition']))
        for d in pred['degrees']:
            assert r['raw_text_adjusted'][d['start']:d['end']]==d['evidence'],(i,d)
            assert r['raw_text_adjusted'][d['label_start']:d['label_end']]==d['label_as_written'],(i,d)
            for k in ['institution_as_written','year_raw']:assert not d[k] or d[k] in d['evidence'],(i,k,d)
        out[i]=pred
    return out
def evaluate(ids,pred,g):
    rows=sample();metrics=[];errors=[]
    for edition in [1906,1938,1955,1986,'balanced_all']:
        selected=sorted(i for i in ids if edition=='balanced_all' or int(rows[i]['edition'])==edition)
        for group,members in GROUP_SETS.items():
            for metric in METRICS:
                tp=fp=fn=0; record_counts=[]
                for i in sorted(selected):
                    pp=[d for d in pred[i]['degrees'] if d['group'] in members and not d['honorary']]
                    gg=[d for d in g[i] if d['group'] in members and not d['honorary']]
                    if metric=='record_detection':
                        pc=collections.Counter({group:1}) if pp else collections.Counter();gc=collections.Counter({group:1}) if gg else collections.Counter()
                    else:
                        pc=collections.Counter(k for d in pp if (k:=key(d,metric)) is not None)
                        gc=collections.Counter(k for d in gg if (k:=key(d,metric)) is not None)
                    # Exact-key multiset intersection is a one-to-one matching;
                    # every duplicate occurrence beyond the reference is an FP.
                    ct=(sum((pc&gc).values()),sum((pc-gc).values()),sum((gc-pc).values()))
                    record_counts.append(ct);tp+=ct[0];fp+=ct[1];fn+=ct[2]
                    if edition!='balanced_all' and group in GROUPS:
                        for typ,vals in [('false_positive',pc-gc),('false_negative',gc-pc)]:
                            for k,n in vals.items():
                                errors.append(dict(sample_id=i,edition=edition,group=group,metric=metric,error_type=typ,key=json.dumps(k),count=n,
                                                   parser_flags=json.dumps(pred[i]['flags']),gold=json.dumps(gg,ensure_ascii=False),predicted=json.dumps(pp,ensure_ascii=False)))
                pr=tp/(tp+fp) if tp+fp else None;rc=tp/(tp+fn) if tp+fn else None
                pl,pu=wilson(tp,tp+fp);rl,ru=wilson(tp,tp+fn)
                import numpy as np
                rng=np.random.default_rng(42);arr=np.asarray(record_counts,dtype=int)
                totals=np.zeros((1000,3),dtype=int)
                # F1 uncertainty uses record resampling, stratified for pooled results.
                for ed in sorted({int(rows[i]['edition']) for i in selected}):
                    ix=[j for j,i in enumerate(selected) if int(rows[i]['edition'])==ed]
                    weights=rng.multinomial(len(ix),np.full(len(ix),1/len(ix)),size=1000)
                    totals+=weights@arr[ix]
                den=2*totals[:,0]+totals[:,1]+totals[:,2]
                boot=2*totals[den>0,0]/den[den>0]
                fl,fu=(map(float,np.quantile(boot,[.025,.975])) if len(boot) else (None,None))
                metrics.append(dict(edition=edition,group=group,metric=metric,records=len(selected),tp=tp,fp=fp,fn=fn,
                                    precision=pr,recall=rc,f1=2*tp/(2*tp+fp+fn) if 2*tp+fp+fn else None,
                                    precision_ci_low=pl,precision_ci_high=pu,recall_ci_low=rl,recall_ci_high=ru,f1_ci_low=fl,f1_ci_high=fu))
    return metrics,errors
def run(version):
    assert not (OUT/'freeze_manifest.json').exists(),'Rules frozen: cannot run further development'
    split=load(OUT/'split_manifest.json');p=OUT/'iterations'/version;p.mkdir(parents=True,exist_ok=True)
    source=DATA_ROOT/'output/amws/education_pilot/regex_pilot/frozen_education_regex.py' if version=='baseline_v08' else Path(__file__).with_name('education_regex.py')
    target=p/'education_regex.py'
    if target.exists():assert digest(source)==digest(target),'Changed rules need a new version'
    else:shutil.copyfile(source,target)
    mod=parser(target);pred=predict(split['development'],mod,version=='baseline_v08')
    write_json(p/'predictions.json',pred)
    metrics,errors=evaluate(split['development'],pred,gold())
    write_csv(p/'metrics.csv',metrics);write_csv(p/'errors.csv',errors)
    write_json(p/'rules.json',dict(version=version,parser_sha256=digest(target),reference_sha256=load(OUT/'reference_manifest.json')['sha256'],split_sha256=digest(OUT/'split_manifest.json'),regexes={k:v.pattern for k,v in vars(mod).items() if isinstance(v,re.Pattern)}))
    for m in metrics:
        if m['metric']=='complete_tuple' and m['group']=='all_target_degrees': print(m['edition'],'P',m['precision'],'R',m['recall'],'F1',m['f1'])
def freeze(version):
    p=OUT/'iterations'/version;metrics=read_csv(p/'metrics.csv')
    byedition=[m for m in metrics if m['metric']=='complete_tuple' and m['group']=='all_target_degrees' and m['edition']!='balanced_all']
    history=[]
    for d in (OUT/'iterations').iterdir():
        if d.name=='baseline_v08' or not (d/'metrics.csv').exists():continue
        mm=read_csv(d/'metrics.csv');ff=[float(m['f1']) for m in mm if m['group']=='all_target_degrees' and m['metric']=='complete_tuple' and m['edition']!='balanced_all'];history.append((d.name,sum(ff)/4))
    history.sort();gate=all(float(m['precision'] or 0)>=.95 and float(m['recall'] or 0)>=.90 for m in byedition)
    plateau=len(history)>=3 and all(history[j][1]-history[j-1][1]<.005 for j in [-1,-2])
    assert gate or plateau,dict(gate=gate,plateau=plateau,history=history)
    mf=dict(version=version,parser_sha256=digest(p/'education_regex.py'),reference_sha256=load(OUT/'reference_manifest.json')['sha256'],reference_version=load(OUT/'reference_manifest.json')['version'],split_sha256=digest(OUT/'split_manifest.json'),development_quality_gate_passed=gate,stopping_reason='targets_met_in_each_edition' if gate else 'two_consecutive_macro_F1_improvements_below_0.5pp',development_history=history)
    if (OUT/'freeze_manifest.json').exists():assert load(OUT/'freeze_manifest.json')==mf
    else:shutil.copyfile(p/'education_regex.py',OUT/'frozen_education_regex.py');write_json(OUT/'freeze_manifest.json',mf)
def holdout():
    mf=load(OUT/'freeze_manifest.json');split=load(OUT/'split_manifest.json');path=OUT/'holdout';assert not path.exists(),'Held-out evaluation already saved'
    assert digest(OUT/'frozen_education_regex.py')==mf['parser_sha256'] and digest(OUT/'split_manifest.json')==mf['split_sha256']
    assert digest(OUT/(mf['reference_version']+'.json'))==mf['reference_sha256']
    rm=load(OUT/'reference_manifest.json')
    assert rm['version']==mf['reference_version'] and rm['sha256']==mf['reference_sha256'],'Live reference manifest differs from frozen reference'
    frozen_gold={int(i):ds for i,ds in load(OUT/(mf['reference_version']+'.json')).items()}
    pred=predict(split['holdout'],parser(OUT/'frozen_education_regex.py'));metrics,errors=evaluate(split['holdout'],pred,frozen_gold)
    path.mkdir();write_json(path/'predictions.json',pred);write_csv(path/'metrics.csv',metrics);write_csv(path/'errors.csv',errors)
    write_json(path/'evaluation_manifest.json',dict(**mf,records=200,predictions_sha256=digest(path/'predictions.json'),metrics_sha256=digest(path/'metrics.csv')))
    for m in metrics:
        if m['metric']=='complete_tuple' and m['group']=='all_target_degrees':print(m['edition'],'P',m['precision'],'R',m['recall'],'F1',m['f1'])
if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('action',choices=['reference','develop','freeze','holdout']);p.add_argument('version',nargs='?');a=p.parse_args()
    if a.action=='reference':build_reference()
    elif a.action=='develop':run(a.version)
    elif a.action=='freeze':freeze(a.version)
    else:holdout()

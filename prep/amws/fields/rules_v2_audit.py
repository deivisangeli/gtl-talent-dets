"""Freeze v2, draw 100 untouched 1986 entries, and evaluate blind manual labels."""
import argparse
import collections
import csv
import importlib
import json
import math
import os
from pathlib import Path
import random
import shutil
import subprocess
import sys
from pilot import DEFAULT_ROOT, now, rows, read, sha, write

NAME='physical_science_rules_v2_audit_20261023'
RELEASE='output/amws/amws_education_postsecondary_v06_20261021/amws_open_alex.csv'
BIO='output/amws/education_enriched_20261011_updated_final/source_biographies.csv'
OLD='output/amws/physical_science_dummy_20261020'
CLASS_FILES=['classifier.py','classifier_boundary_guard.py','rules_v1.json','classifier_v2.py','rules_v2.json','test_classifier_v2.py']
csv.field_size_limit(16*1024*1024)


def input_paths(root):
    paths=[root/RELEASE,root/BIO,root/'Data/processed/amws/amws_ed86.csv']
    for folder in ['field_regex_pilot_20261020','physical_science_accuracy_audit_20261020',
                   'field_of_work_sample_20261020','field_classification_sample_20261023',
                   'physical_science_dummy_20261020']:
        paths.extend(p for p in (root/'output/amws'/folder).rglob('*') if p.is_file() and '__pycache__' not in p.parts)
    return sorted(set(paths))


def checks(root): return {str(p):sha(p) for p in input_paths(root)}


def exclusion_keys(root):
    base=root/'output/amws'
    old=set(read(base/'field_regex_pilot_20261020/sample_manifest.json')['sample_keys_in_draw_order'])
    for file in ['physical_science_accuracy_audit_20261020/blind_sample.json',
                 'field_classification_sample_20261023/sample_records.json',
                 'field_of_work_sample_20261020/sample_biographies.json']:
        old.update(r['source_key'] for r in read(base/file))
    return old


def load(out=None):
    if out: sys.path.insert(0,str(out/'frozen_classifier'))
    return importlib.import_module('classifier_boundary_guard'),importlib.import_module('classifier_v2')


def count_metrics(records,model):
    matrix={str(g):{str(p):0 for p in [0,1]} for g in [0,1]}; unknown=0; unresolved=0
    for r in records:
        g=r['manual']['physical_science']; p=int(r[model]['physical_match'])
        unresolved+=r[model]['other_reason'] in ['missing','damaged','unmatched']
        if g=='unclear': unknown+=1
        else: matrix[str(g)][str(p)]+=1
    tp=matrix['1']['1']; tn=matrix['0']['0']; fp=matrix['0']['1']; fn=matrix['1']['0']
    correct=tp+tn
    return dict(total=len(records),assessable=len(records)-unknown,correct=correct,incorrect=fp+fn,
        unassessable=unknown,confusion_matrix_rows_manual_columns_predicted=matrix,
        precision=proportion(tp,tp+fp),recall=proportion(tp,tp+fn),accuracy=proportion(correct,len(records)-unknown),
        unresolved=unresolved,unresolved_rate=proportion(unresolved,len(records)))


def proportion(k,n):
    if not n:return dict(numerator=k,denominator=n,value=None,wilson_95_interval=None)
    z=1.959963984540054; p=k/n; d=1+z*z/n
    mid=(p+z*z/(2*n))/d; half=z*math.sqrt(p*(1-p)/n+z*z/(4*n*n))/d
    return dict(numerator=k,denominator=n,value=p,wilson_95_interval=[mid-half,mid+half])


def freeze(root,out):
    if (out/'rules_frozen.json').exists():
        f=read(out/'rules_frozen.json')
        assert all(sha(p)==h for p,h in f['input_checksums_sha256'].items())
        for n,h in f['classifier_sha256'].items():
            assert sha(out/'frozen_classifier'/n)==sha(Path(__file__).with_name(n))==h
        print('Existing classifier freeze verified.'); return
    before=checks(root)
    subprocess.run([sys.executable,'-X','utf8','-m','unittest','test_classifier','test_boundary_guard','test_classifier_v2'],
                   cwd=Path(__file__).parent,check=True)
    baseline,candidate=load()
    old=read(root/'output/amws/physical_science_accuracy_audit_20261020/comparisons.json')
    records=[]
    for r in old:
        records.append(dict(source_key=r['source_key'],manual=dict(physical_science=r['manual_physical_science']),
            baseline=baseline.classify_entry(r['recovered_field'],r['raw_text_adjusted'],r['name_raw']),
            revised=candidate.classify_entry(r['recovered_field'],r['raw_text_adjusted'],r['name_raw'])))
    reg=dict(status='development_regression_only_exposed_labels',old_500_audit=dict(
        baseline=count_metrics(records,'baseline'),revised=count_metrics(records,'revised')),
        changed_keys=[r['source_key'] for r in records if r['baseline']['physical_match']!=r['revised']['physical_match']],
        test_suite='28 tests passed; tests include vocabulary subcases and adversarial examples')
    write(out/'development_regression.json',reg)
    dest=out/'frozen_classifier'; dest.mkdir(exist_ok=True)
    for n in CLASS_FILES: shutil.copyfile(Path(__file__).with_name(n),dest/n)
    assert checks(root)==before
    write(out/'rules_frozen.json',dict(frozen_at_utc=now(),version=candidate.VERSION,
        classifier_sha256={n:sha(dest/n) for n in CLASS_FILES},input_checksums_sha256=before,
        regression_sha256=sha(out/'development_regression.json'),sample_not_yet_drawn=True),True)
    print(json.dumps(reg['old_500_audit'],indent=2))


def prepare(root,out):
    frozen=read(out/'rules_frozen.json')
    assert all(sha(p)==h for p,h in frozen['input_checksums_sha256'].items())
    for n,h in frozen['classifier_sha256'].items(): assert sha(out/'frozen_classifier'/n)==sha(Path(__file__).with_name(n))==h
    if (out/'sample_manifest.json').exists():
        m=read(out/'sample_manifest.json'); assert sha(out/'blind_sample.json')==m['blind_sample_sha256'];return
    previous=exclusion_keys(root)
    population={r['source_key']:r for r in rows(root/RELEASE) if r['source_key'].startswith('1986:')}
    assert len(population)==79073
    pool=sorted(set(population)-previous)
    assert len(pool)==77909 and len(set(population)&previous)==1164
    keys=random.Random(20261024).sample(pool,100); wanted=set(keys)
    bio={r['source_key']:r for r in rows(root/BIO) if r['source_key'] in wanted}
    fields={r['source_key']:r for r in rows(root/OLD/'amws_ed16_physical_science_audit.csv') if r['source_key'] in wanted}
    assert set(bio)==set(fields)==wanted
    sample=[]
    for i,k in enumerate(keys,1):
        r=population[k]; b=bio[k]; f=fields[k]
        assert r['raw_text_adjusted']==b['raw_text_adjusted'] and r['name_raw']==f['name_raw']
        sample.append(dict(sample_id=i,source_key=k,name_raw=r['name_raw'],field_raw=f['field_raw'],
            raw_text_adjusted=b['raw_text_adjusted'],source_references_json=f['source_references_json'],
            biography_source_file=b['source_file'],biography_source_column=b['source_column']))
    write(out/'blind_sample.json',sample,True)
    write(out/'sample_manifest.json',dict(sampled_at_utc=now(),population_size=79073,eligible_size=77909,
        previous_reviewed_in_release=1164,seed=20261024,sample_size=100,sample_keys_in_draw_order=keys,
        exclusion_keys=sorted(set(population)&previous),blind_sample_sha256=sha(out/'blind_sample.json'),
        classifier_freeze_sha256=sha(out/'rules_frozen.json'),
        method='random.Random(20261024).sample(sorted(v06_1986_keys - previously_reviewed_keys),100)'),True)
    print('100 fresh keys frozen after classifier freeze. No predictions displayed.')


def review(out,start,end,full):
    import re
    for r in read(out/'blind_sample.json'):
        if start<=r['sample_id']<=end:
            text=r['raw_text_adjusted']
            cut=re.search(r'\bEduc\b',text,re.I)
            display=text if full else text[:min(cut.start() if cut else len(text),420)]
            print(f"[{r['sample_id']}] {r['source_key']} {display}")


def add(out,path):
    assert not (out/'manual_frozen.json').exists()
    sample={r['sample_id']:r for r in read(out/'blind_sample.json')}
    manual=read(out/'manual_labels.json') if (out/'manual_labels.json').exists() else []
    seen={r['sample_id'] for r in manual}
    for i,label,evidence,reason,*extra in read(path):
        assert i not in seen and label in [0,1,'unclear']
        s=sample[i]; start=s['raw_text_adjusted'].find(evidence)
        assert evidence and start>=0,(i,evidence)
        manual.append(dict(sample_id=i,source_key=s['source_key'],physical_science=label,evidence=evidence,
            evidence_start=start,evidence_end=start+len(evidence),reasoning=reason,
            confidence=extra[0] if extra else 'high'))
        seen.add(i)
    write(out/'manual_labels.json',sorted(manual,key=lambda r:r['sample_id']))
    print(f'{len(manual)}/100 blind manual labels recorded.')


def freeze_manual(out):
    sample=read(out/'blind_sample.json'); gold=read(out/'manual_labels.json')
    assert len(gold)==100 and [r['source_key'] for r in sample]==[r['source_key'] for r in gold]
    for s,g in zip(sample,gold): assert s['raw_text_adjusted'][g['evidence_start']:g['evidence_end']]==g['evidence']
    write(out/'manual_frozen.json',dict(frozen_at_utc=now(),manual_sha256=sha(out/'manual_labels.json'),
        labels_recorded_before_predictions=True,benchmark='Prediction-blinded Codex manual review; not independent human expert validation.'),True)
    print('100 labels frozen before evaluation.')


def check_evidence(pred,source):
    for e in pred['evidence']:
        text=source[e['evidence_source']]
        assert text[e['start']:e['end']]==e['evidence']
        if e['evidence_source']=='raw_text_adjusted': assert e['end']<=pred['scope_guard_offset']
        for repair in e.get('ocr_repairs',[]):
            assert text[repair['start']:repair['end']]==repair['original']


def evaluate(root,out):
    frozen=read(out/'rules_frozen.json'); manualfreeze=read(out/'manual_frozen.json')
    assert sha(out/'manual_labels.json')==manualfreeze['manual_sha256']
    assert all(sha(p)==h for p,h in frozen['input_checksums_sha256'].items())
    for n,h in frozen['classifier_sha256'].items(): assert sha(out/'frozen_classifier'/n)==sha(Path(__file__).with_name(n))==h
    baseline,revised=load(out); records=[]
    for s,g in zip(read(out/'blind_sample.json'),read(out/'manual_labels.json'),strict=True):
        assert s['source_key']==g['source_key']
        b=baseline.classify_entry(s['field_raw'],s['raw_text_adjusted'],s['name_raw'])
        v=revised.classify_entry(s['field_raw'],s['raw_text_adjusted'],s['name_raw'])
        check_evidence(b,s); check_evidence(v,s)
        records.append(dict(source=s,manual=g,baseline=b,revised=v,
            prediction_changed=b['physical_match']!=v['physical_match']))
    metrics=dict(evaluated_at_utc=now(),baseline=count_metrics(records,'baseline'),revised=count_metrics(records,'revised'),
        prediction_changes=sum(r['prediction_changed'] for r in records),
        revised_ocr_assisted_entries=sum(r['revised']['ocr_assisted'] for r in records),
        classifier_unchanged_since_freeze=True,holdout_tuning=False,reference_standard=manualfreeze['benchmark'])
    write(out/'comparisons.json',records); write(out/'evaluation.json',metrics)
    flat=[]
    for r in records:
        s,g,b,v=r['source'],r['manual'],r['baseline'],r['revised']; label=g['physical_science']
        outcome=lambda p:'unassessable' if label=='unclear' else 'correct' if int(p['physical_match'])==label else 'incorrect'
        flat.append(dict(sample_id=s['sample_id'],source_key=s['source_key'],name_raw=s['name_raw'],
            manual_physical_science=label,baseline_physical_science=int(b['physical_match']),revised_physical_science=int(v['physical_match']),
            baseline_outcome=outcome(b),revised_outcome=outcome(v),prediction_changed=int(r['prediction_changed']),
            ocr_assisted=int(v['ocr_assisted']),manual_evidence=g['evidence'],manual_reasoning=g['reasoning'],
            manual_confidence=g['confidence'],manual_evidence_start=g['evidence_start'],manual_evidence_end=g['evidence_end'],
            baseline_reason=b['other_reason'],revised_reason=v['other_reason'],field_raw=s['field_raw'],
            baseline_heading=b['selected_heading'],revised_heading=v['selected_heading'],
            baseline_rule_ids=';'.join(b['matched_rule_ids']),revised_rule_ids=';'.join(v['matched_rule_ids']),
            baseline_evidence_json=json.dumps(b['evidence'],ensure_ascii=False),revised_evidence_json=json.dumps(v['evidence'],ensure_ascii=False),
            source_references_json=s['source_references_json'],raw_text_adjusted=s['raw_text_adjusted']))
    write(out/'audit_records.json',flat)
    cols=list(flat[0]);write(out/'audit_payload.json',dict(columns=cols,rows=[[r[c] for c in cols] for r in flat]))
    write(out/'errors_changes_ocr.json',[r for r in records if r['prediction_changed'] or r['revised']['ocr_assisted']
        or r['manual']['physical_science']=='unclear' or any(int(r[m]['physical_match'])!=r['manual']['physical_science'] for m in ['baseline','revised'])])
    print(json.dumps(metrics,indent=2))


def report(out):
    metrics=read(out/'evaluation.json'); records=read(out/'audit_records.json')
    notes=read(out/'case_notes.json'); by_key={r['source_key']:r for r in notes}
    required={r['source_key'] for r in records if r['prediction_changed'] or r['ocr_assisted'] or r['baseline_outcome']!='correct' or r['revised_outcome']!='correct'}
    assert set(by_key)==required
    lines=['PHYSICAL RULES V2: FRESH 100-ENTRY 1986 EVALUATION','',
        'Frozen v2 vocabulary/OCR revision versus original v1.1 boundary-guard classifier.',
        'Population: 79,073 v06 retained 1986 entries; 1,164 previously reviewed keys',
        'excluded, leaving 77,909 eligible keys. Seed 20261024, random sample of 100',
        'without replacement from sorted eligible keys. Rules frozen before the draw.',
        'All 100 manual labels frozen before either prediction was displayed.',
        'Reference labels are Codex manual judgments, not an independent human standard.',
        'Unknown labels excluded from precision/recall/accuracy, retained in total counts.',
        'Wilson intervals represent binomial sampling uncertainty, not reference-label error.','']
    lines+=['Fresh sample results (same entries for both versions):',
        'Model       Precision       Recall          Correct / assessable']
    for model in ['baseline','revised']:
        m=metrics[model]
        fmt=lambda x:'undefined' if x['value'] is None else f"{x['numerator']}/{x['denominator']} ({x['value']:.2%})"
        lines.append(f"{model:10s}  {fmt(m['precision']):17s} {fmt(m['recall']):17s} {m['correct']}/{m['assessable']}")
    lines+=['', 'The revision recovers two additional Physical entries on this sample.',
        'The small sample gives wide intervals; the result does not establish a',
        'statistically reliable improvement across the full population.', '', 'Detailed metrics:']
    for model in ['baseline','revised']:
        m=metrics[model]; lines+=[model.upper(),json.dumps(m,indent=2),'']
    lines+=['Changes: '+str(metrics['prediction_changes']),
        'Revised OCR-assisted entries: '+str(metrics['revised_ocr_assisted_entries']),'',
        'Every error, unknown, changed prediction or OCR-assisted result:']
    for r in records:
        if r['source_key'] in required:
            lines+=[f"{r['source_key']} {r['name_raw']}: manual={r['manual_physical_science']}, baseline={r['baseline_physical_science']}, revised={r['revised_physical_science']}.",
                    'Evidence: '+r['manual_evidence'],by_key[r['source_key']]['explanation'],'']
    lines+=['Development regression uses exposed earlier labels; those results are not',
        'fresh accuracy validation. Only this untouched 100-entry sample is the new',
        'evaluation. No rules or reference labels were changed after evaluation.',
        'OCR support is allowlisted: accent removal, at most two within-word spacing/',
        'punctuation breaks and explicit aliases. Evidence retains literal offsets.',
        'No arbitrary fuzzy matching or whole-biography keyword classification.',
        'Some Physical work visible only in biography bodies can still be missed.',
        'The sample size is small; compare precision/recall with their intervals.',
        'This release does not update the full-population dummy.', '']
    (out/'report.txt').write_text('\n'.join(lines),encoding='utf8')
    snap=out/'code';snap.mkdir(exist_ok=True)
    for n in ['rules_v2_audit.py','export_rules_v2_audit.mjs','pilot.py','RULES_V2.md']:
        shutil.copyfile(Path(__file__).with_name(n),snap/n)


def validate(root,out):
    f=read(out/'rules_frozen.json'); m=read(out/'sample_manifest.json'); g=read(out/'manual_frozen.json')
    assert checks(root)==f['input_checksums_sha256']
    assert sha(out/'blind_sample.json')==m['blind_sample_sha256'] and sha(out/'manual_labels.json')==g['manual_sha256']
    assert f['frozen_at_utc']<m['sampled_at_utc']<g['frozen_at_utc']<read(out/'evaluation.json')['evaluated_at_utc']
    population={r['source_key']:r for r in rows(root/RELEASE) if r['source_key'].startswith('1986:')}
    pool=sorted(set(population)-exclusion_keys(root)); keys=random.Random(20261024).sample(pool,100)
    records=read(out/'comparisons.json'); flat=read(out/'audit_records.json')
    exported=list(rows(out/'amws_physical_rules_v2_audit_100.csv'))
    assert len(keys)==len(set(keys))==len(records)==len(exported)==100 and len(pool)==77909
    assert keys==m['sample_keys_in_draw_order']==[r['source']['source_key'] for r in records]
    wanted=set(keys)
    bios={r['source_key']:r for r in rows(root/BIO) if r['source_key'] in wanted}
    field={r['source_key']:r for r in rows(root/OLD/'amws_ed16_physical_science_audit.csv') if r['source_key'] in wanted}
    assert set(bios)==set(field)==wanted
    baseline,revised=load(out)
    for r,a,c in zip(records,flat,exported,strict=True):
        s,man=r['source'],r['manual']; k=s['source_key']
        assert s['raw_text_adjusted']==bios[k]['raw_text_adjusted']==population[k]['raw_text_adjusted']
        assert s['field_raw']==field[k]['field_raw'] and s['source_references_json']==field[k]['source_references_json']
        assert s['raw_text_adjusted'][man['evidence_start']:man['evidence_end']]==man['evidence']
        assert all(c[key]==str(value) for key,value in a.items())
        for model,mod in [('baseline',baseline),('revised',revised)]:
            p=mod.classify_entry(s['field_raw'],s['raw_text_adjusted'],s['name_raw'])
            assert p==r[model];check_evidence(p,s)
    ev=read(out/'evaluation.json')
    for model in ['baseline','revised']: assert count_metrics(records,model)==ev[model]
    for model in ['baseline','revised']:
        known=[r for r in records if r['manual']['physical_science']!='unclear']
        tp=sum(r['manual']['physical_science']==1 and r[model]['physical_match'] for r in known)
        predicted=sum(r[model]['physical_match'] for r in known)
        actual=sum(r['manual']['physical_science']==1 for r in known)
        assert (ev[model]['precision']['numerator'],ev[model]['precision']['denominator'])==(tp,predicted)
        assert (ev[model]['recall']['numerator'],ev[model]['recall']['denominator'])==(tp,actual)
    for n,h in f['classifier_sha256'].items(): assert sha(out/'frozen_classifier'/n)==sha(Path(__file__).with_name(n))==h
    assert sha(out/'development_regression.json')==f['regression_sha256']
    for name in ['rules_v2_audit.py','export_rules_v2_audit.mjs','pilot.py','RULES_V2.md']:
        assert sha(out/'code'/name)==sha(Path(__file__).with_name(name))
    assert read(out/'export_validation.json')['artifact_tool_cell_preservation_passed']
    write(out/'validation.json',dict(status='PASS',validated_at_utc=now(),unique_keys=100,previous_review_overlap=0,
        source_joins_verified=True,all_evidence_offsets_verified=True,classifier_and_labels_frozen=True,
        predictions_recomputed=True,metrics_reconciled=True,csv_cells_preserved=True,original_releases_unchanged=True))
    products={str(p.relative_to(out)):sha(p) for p in out.rglob('*') if p.is_file() and p.name!='manifest.json' and '__pycache__' not in p.parts}
    write(out/'manifest.json',dict(status='complete',completed_at_utc=now(),input_checksums_sha256=f['input_checksums_sha256'],
        output_checksums_sha256=products,seed=20261024,version='amws_ed16_science_v2',
        reproduction='rules_v2_audit.py freeze/prepare; use frozen manual labels; evaluate; report; export_rules_v2_audit.mjs; validate'))
    print('PASS: 100 fresh keys, frozen rules/labels, exact joins/evidence, recomputed metrics and unchanged originals.')


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('command',choices=['freeze','prepare','review','add','freeze-manual','evaluate','report','validate'])
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--start',type=int,default=1);p.add_argument('--end',type=int,default=100)
    p.add_argument('--full',action='store_true');p.add_argument('--labels',type=Path)
    a=p.parse_args();out=a.data_root/'output/amws'/NAME
    if a.command=='freeze':freeze(a.data_root,out)
    elif a.command=='prepare':prepare(a.data_root,out)
    elif a.command=='review':review(out,a.start,a.end,a.full)
    elif a.command=='add':add(out,a.labels)
    elif a.command=='freeze-manual':freeze_manual(out)
    elif a.command=='evaluate':evaluate(a.data_root,out)
    elif a.command=='report':report(out)
    elif a.command=='validate':validate(a.data_root,out)

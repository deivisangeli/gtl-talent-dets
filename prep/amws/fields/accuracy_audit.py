"""Blind manual audit of 500 new edition-16 entries; never fits classifier rules."""
import argparse
import collections
import csv
import json
import math
import os
import random
import re
import shutil
from pathlib import Path
from pilot import DEFAULT_ROOT, RELEASE, PROCESSED, now, read, rows, sha, write

NAME = 'physical_science_accuracy_audit_20261020'
PILOT = 'output/amws/field_regex_pilot_20261020'
DUMMY = 'output/amws/physical_science_dummy_20261020'
csv.field_size_limit(16*1024*1024)


def inputs(root):
    paths = [root/RELEASE, root/PROCESSED, root/PILOT/'sample_manifest.json',
             root/DUMMY/'amws_ed16_physical_science_dummy.csv',
             root/DUMMY/'amws_ed16_physical_science_audit.csv', root/DUMMY/'manifest.json']
    paths += [Path(__file__).with_name(n) for n in ['classifier.py','classifier_boundary_guard.py','rules_v1.json']]
    # Preserve the complete prior pilot, not just its sampled keys.
    paths += [p for p in (root/PILOT).rglob('*') if p.is_file() and '__pycache__' not in p.parts]
    return {str(p): sha(p) for p in paths}


def check_sources(root, out):
    m = read(out/'sample_manifest.json')
    assert inputs(root) == m['input_checksums_sha256'], 'Prior input changed'
    assert sha(out/'blind_sample.json') == m['blind_sample_sha256']
    return m


def prepare(root, out):
    if (out/'sample_manifest.json').exists():
        check_sources(root,out); print('Frozen sample already exists.'); return
    before = inputs(root)
    old = set(read(root/PILOT/'sample_manifest.json')['sample_keys_in_draw_order'])
    assert len(old)==500
    population = [r for r in rows(root/RELEASE) if r['source_key'].startswith('1986:')]
    assert len(population)==len({r['source_key'] for r in population})==79658
    assert all(r['is_us_birth'].strip().lower()=='true' for r in population)
    pool = sorted(r['source_key'] for r in population if r['source_key'] not in old)
    assert len(pool)==79158
    keys = random.Random(20261022).sample(pool,500)
    lookup = {r['source_key']:r for r in population}
    sample = [dict(sample_id=i, source_key=k, entry_id=lookup[k]['entry_id'],
                   name_raw=lookup[k]['name_raw'], raw_text_adjusted=lookup[k]['raw_text_adjusted'])
              for i,k in enumerate(keys,1)]
    write(out/'blind_sample.json',sample,True)
    write(out/'sample_manifest.json',dict(created_at_utc=now(),seed=20261022,
        population_size=79658,eligible_population_size=79158,sample_size=500,
        excluded_pilot_size=500,sampled_keys_in_draw_order=keys,
        method='random.Random(20261022).sample(sorted(edition16_keys - prior_pilot_keys),500)',
        input_checksums_sha256=before,blind_sample_sha256=sha(out/'blind_sample.json'),
        sampling_unit='edition-specific source_key',
        benchmark='Codex manual interpretation, prediction-blinded; not an independent human gold standard.'),True)
    assert inputs(root)==before
    print('Frozen 500 new keys from 79,158 eligible entries; no predictions displayed.')


def review(out,start,end,full):
    # Navigation only: no classifier imports, recovered fields or predictions.
    for r in read(out/'blind_sample.json'):
        if start <= r['sample_id'] <= end:
            t=r['raw_text_adjusted']
            if full:
                display=t
            else:
                boundary=re.search(r'\bEduc(?:ation)?\b',t,re.I)
                display=t[:min(boundary.start() if boundary else len(t),420)]
            print(f"[{r['sample_id']}] {r['source_key']} {display}")


def add_labels(out,path):
    assert not (out/'labels_frozen.json').exists(), 'Labels already frozen'
    sample={r['sample_id']:r for r in read(out/'blind_sample.json')}
    gold=read(out/'manual_labels.json') if (out/'manual_labels.json').exists() else []
    existing={r['sample_id'] for r in gold}
    for a in read(path):
        # Every judgment and excerpt is supplied explicitly by the manual reviewer.
        i,label,evidence,reason,*rest=a
        assert i not in existing and label in [0,1,'unclear']
        s=sample[i]; t=s['raw_text_adjusted']; start=t.find(evidence)
        assert evidence and start>=0, (i,evidence)
        gold.append(dict(sample_id=i,source_key=s['source_key'],manual_physical_science=label,
            evidence=evidence,evidence_start=start,evidence_end=start+len(evidence),reasoning=reason,
            confidence=rest[0] if rest else 'high'))
        existing.add(i)
    write(out/'manual_labels.json',sorted(gold,key=lambda r:r['sample_id']))
    print(f'Manual labels recorded: {len(gold)}/500')


def freeze(root,out):
    check_sources(root,out)
    sample=read(out/'blind_sample.json'); gold=read(out/'manual_labels.json')
    assert len(gold)==500 and [r['source_key'] for r in gold]==[r['source_key'] for r in sample]
    for s,g in zip(sample,gold):
        assert s['raw_text_adjusted'][g['evidence_start']:g['evidence_end']]==g['evidence']
    write(out/'labels_frozen.json',dict(frozen_at_utc=now(),manual_labels_sha256=sha(out/'manual_labels.json'),
        blind_sample_sha256=sha(out/'blind_sample.json'),count=500,
        predictions_displayed_during_review=False,
        review_method='Individual manual reading of biography headings; complete biographies consulted for ambiguous or damaged headings. No keyword-generated manual labels.'),True)
    print('All 500 manual labels frozen before prediction join.')


def proportion(k,n):
    if not n: return dict(numerator=k,denominator=n,value=None,wilson_95_interval=None)
    p=k/n; z=1.959963984540054; d=1+z*z/n
    mid=(p+z*z/(2*n))/d; half=z*math.sqrt(p*(1-p)/n+z*z/(4*n*n))/d
    return dict(numerator=k,denominator=n,value=p,wilson_95_interval=[mid-half,mid+half])


def evaluate(root,out):
    check_sources(root,out)
    freeze=read(out/'labels_frozen.json')
    assert freeze['manual_labels_sha256']==sha(out/'manual_labels.json')
    sample=read(out/'blind_sample.json'); gold=read(out/'manual_labels.json')
    keys={r['source_key'] for r in sample}
    predicted={r['source_key']:r for r in rows(root/DUMMY/'amws_ed16_physical_science_dummy.csv') if r['source_key'] in keys}
    audit={r['source_key']:r for r in rows(root/DUMMY/'amws_ed16_physical_science_audit.csv') if r['source_key'] in keys}
    assert set(predicted)==set(audit)==keys
    joined=[]; matrix={'0':{'0':0,'1':0},'1':{'0':0,'1':0}}
    for s,g in zip(sample,gold):
        k=s['source_key']; p=predicted[k]; a=audit[k]; label=g['manual_physical_science']; flag=int(p['physical_science'])
        assert p['name_raw']==s['name_raw'] and a['physical_science']==p['physical_science']
        assert s['raw_text_adjusted'][g['evidence_start']:g['evidence_end']]==g['evidence']
        result='unassessable' if label=='unclear' else 'correct' if label==flag else 'incorrect'
        if label!='unclear': matrix[str(label)][str(flag)]+=1
        joined.append(dict(**s,**{k:v for k,v in g.items() if k not in s},physical_science=flag,
            outcome=result,mixed=int(a['mixed']),classifier_unresolved=int(a['unresolved']),
            classifier_reason=a['other_reason'],recovered_field=a['field_raw'],
            classifier_heading=a['selected_heading'],matched_rules=a['matched_rule_ids'],
            classifier_evidence_json=a['evidence_json'],source_references_json=a['source_references_json']))
    counts=collections.Counter(r['outcome'] for r in joined)
    correct=counts['correct']; unclear=counts['unassessable']; n=500-unclear
    tp=matrix['1']['1']; fp=matrix['0']['1']; fn=matrix['1']['0']
    def subset(flag):
        selected=[r for r in joined if r[flag]]
        return dict(n=len(selected),outcomes=dict(collections.Counter(r['outcome'] for r in selected)),
                    manual_labels=dict(collections.Counter(str(r['manual_physical_science']) for r in selected)))
    metrics=dict(evaluated_at_utc=now(),sample_size=500,correct=correct,incorrect=counts['incorrect'],
        unassessable=unclear,confusion_matrix_rows_manual_columns_predicted=matrix,
        accuracy_assessable=proportion(correct,n),physical_precision=proportion(tp,tp+fp),
        physical_recall=proportion(tp,tp+fn),accuracy_bounds_all_500=[correct/500,(correct+unclear)/500],
        mixed_subset=subset('mixed'),classifier_unresolved_subset=subset('classifier_unresolved'),
        rules_changed=False,benchmark='Prediction-blinded Codex manual labels, not independent human validation.',
        inference_population='79,158 edition-16 retained entries excluding the prior 500-entry pilot')
    write(out/'evaluation.json',metrics)
    write(out/'comparisons.json',joined)
    write(out/'errors_and_unassessable.json',[r for r in joined if r['outcome']!='correct'])
    first=['sample_id','source_key','name_raw','manual_physical_science','physical_science','outcome',
           'confidence','evidence','reasoning','mixed','classifier_unresolved']
    columns=first+[c for c in joined[0] if c not in first]
    write(out/'audit_payload.json',dict(columns=columns,rows=[[r[c] for c in columns] for r in joined]))
    print(json.dumps(metrics,indent=2))


def report(root,out):
    """Attach post-comparison explanations without altering frozen manual labels."""
    ev=read(out/'evaluation.json'); records=read(out/'comparisons.json')
    notes=read(out/'error_notes.json'); by_id={n['sample_id']:n for n in notes}
    assert len(by_id)==len(notes)==ev['incorrect']+ev['unassessable']
    assert set(by_id)=={r['sample_id'] for r in records if r['outcome']!='correct'}
    for r in records:
        n=by_id.get(r['sample_id'])
        r['error_cause']=n['cause'] if n else ''
        r['error_explanation']=n['explanation'] if n else ''
    write(out/'comparisons.json',records)
    write(out/'errors_and_unassessable.json',[r for r in records if r['outcome']!='correct'])
    payload=read(out/'audit_payload.json')
    first=['sample_id','source_key','name_raw','manual_physical_science','physical_science','outcome',
           'confidence','evidence','reasoning','mixed','classifier_unresolved']
    columns=first+[c for c in payload['columns'] if c not in first+['error_cause','error_explanation']]
    columns+=['error_cause','error_explanation']
    write(out/'audit_payload.json',dict(columns=columns,rows=[[r[c] for c in columns] for r in records]))
    correct=ev['correct']; incorrect=ev['incorrect']; unknown=ev['unassessable']; n=500-unknown
    ci=ev['accuracy_assessable']['wilson_95_interval']
    cause_counts=collections.Counter(n['cause'] for n in notes if n['cause']!='unassessable_ocr')
    confident=[r for r in records if r['confidence']=='high' and r['outcome']!='unassessable']
    matrix=ev['confusion_matrix_rows_manual_columns_predicted']
    lines=[
        'AMWS EDITION 16: INDEPENDENT 500-ENTRY PHYSICAL-SCIENCE AUDIT','',
        f'Correct: {correct}/500. Incorrect: {incorrect}/500. Unassessable: {unknown}/500.',
        f'Accuracy among assessable entries: {correct}/{n} = {correct/n:.2%}.',
        f'95% Wilson interval for assessable accuracy: {ci[0]:.2%} to {ci[1]:.2%}.',
        f'Accuracy bounds across all 500: {correct/500:.2%} to {(correct+unknown)/500:.2%}.',
        'The bounds treat the two unassessable cases as all incorrect versus all correct;',
        'they describe unresolved reference labels, not a confidence interval.',
        f'Physical precision: {ev["physical_precision"]["value"]:.2%}.',
        f'Physical recall: {ev["physical_recall"]["value"]:.2%}.','',
        'Confusion matrix (manual rows, published dummy columns; excludes unassessable):',
        '                         Predicted 0   Predicted 1',
        f'Manual non-Physical 0    {matrix["0"]["0"]:12d}  {matrix["0"]["1"]:12d}',
        f'Manual Physical 1        {matrix["1"]["0"]:12d}  {matrix["1"]["1"]:12d}','',
        'Sampling: 500 new edition-specific entries drawn without replacement from',
        '79,158 retained edition-16 entries after excluding all 500 original pilot keys.',
        'Seed: 20261022. Python random.Random(seed).sample(sorted(eligible_keys), 500).',
        'The sample is not stratified by prediction. Keys and input hashes were frozen',
        'before manual review. Difficult entries were retained without replacement.','',
        'Reference standard: Codex manually interpreted every sampled biography heading,',
        'consulting complete biographies for ambiguous or damaged headings. Predictions,',
        'recovered fields and classifier evidence were hidden during labeling. All 500',
        'labels were frozen and hashed before predictions were joined. This is a new',
        'prediction-blinded sample audit, not independent human expert validation.','',
        'Physical includes chemistry, physics, astronomy, Earth sciences, engineering,',
        'mathematics, statistics and computing. Engineering includes agricultural and',
        'biomedical engineering. Physical + Biological mixed work receives 1.',
        'Biochemistry and biophysics alone remain Biological. Clinical chemistry is',
        'medical diagnostic work. A biological application alone does not remove',
        'separately established organic/inorganic chemistry or engineering work.',
        'Degrees and memberships alone do not establish a Physical field.','',
        'Manual work can use substantive career/research evidence beyond the heading.',
        'The existing classifier intentionally limits its search to recovered fields or',
        'guarded headings. This audit measures the complete published dummy against',
        'the agreed scientific-work definition, including losses from that restriction.',
        'No classifier, rule, existing prediction or manual label was revised after',
        'evaluation. Frozen manual judgments include confidence and explicit reasoning.',
        f'High-confidence manual subset: {sum(r["outcome"]=="correct" for r in confident)}/{len(confident)} correct.',
        'Several interdisciplinary judgments have medium confidence. They are retained',
        'in the primary estimate and can be inspected in the audit.','',
        'Mixed classifier subset: 8 entries, 7 correct and 1 incorrect (biological chemistry).',
        'Classifier-unresolved subset: 58 entries, 27 correct zeros, 29 missed Physical',
        'entries and 2 unassessable. Unresolved zeros are not confirmed negatives.','',
        'Error causes (descriptive categories assigned after comparison):']
    lines += [f'{k}: {v}' for k,v in sorted(cause_counts.items())]
    lines += ['', 'Every incorrect or unassessable case:']
    for r in records:
        if r['sample_id'] in by_id:
            n=by_id[r['sample_id']]
            lines += [f"{r['sample_id']}. {r['source_key']} {r['name_raw']}",
                f"   Manual={r['manual_physical_science']}; published={r['physical_science']}; {r['outcome']}.",
                f"   Evidence: {r['evidence']}",f"   {n['explanation']}"]
    lines += ['', 'Reproducibility and limits:',
        'Merge using source_key. Evidence offsets are zero-based Unicode code points,',
        'end-exclusive, into raw_text_adjusted. The CSV retains full source biographies,',
        'manual evidence/reasoning, predictions, audit flags and source references.',
        'Manifest and validation files record checksums, seeded draw and preservation.',
        'Inference applies directly to the 79,158 entries outside the original pilot.',
        'The confidence interval reflects sampling uncertainty among assessable records;',
        'it does not account for mistakes or judgment uncertainty in manual labels.',
        'Any later rule changes need a separate evaluation sample.', '']
    (out/'report.txt').write_text('\n'.join(lines),encoding='utf-8')
    snapshot=out/'code'; snapshot.mkdir(exist_ok=True)
    for name in ['accuracy_audit.py','export_accuracy_audit.mjs','pilot.py','ACCURACY_AUDIT.md']:
        shutil.copyfile(Path(__file__).with_name(name),snapshot/name)
    print('Report includes every error and unassessable case. Frozen labels unchanged.')


def validate(root,out):
    m=check_sources(root,out); sample=read(out/'blind_sample.json'); gold=read(out/'manual_labels.json')
    old=set(read(root/PILOT/'sample_manifest.json')['sample_keys_in_draw_order'])
    all_rows={r['source_key']:r for r in rows(root/RELEASE) if r['source_key'].startswith('1986:')}
    expected=random.Random(20261022).sample(sorted(set(all_rows)-old),500)
    assert [r['source_key'] for r in sample]==expected==m['sampled_keys_in_draw_order']
    assert len(set(expected))==500 and not set(expected)&old
    assert sha(out/'manual_labels.json')==read(out/'labels_frozen.json')['manual_labels_sha256']
    for s,g in zip(sample,gold,strict=True):
        assert s['raw_text_adjusted']==all_rows[s['source_key']]['raw_text_adjusted']
        assert s['raw_text_adjusted'][g['evidence_start']:g['evidence_end']]==g['evidence']
    comparisons=read(out/'comparisons.json'); data=list(rows(out/'amws_physical_science_accuracy_audit_500.csv'))
    assert len(data)==len(comparisons)==500
    assert read(out/'labels_frozen.json')['frozen_at_utc'] < read(out/'evaluation.json')['evaluated_at_utc']
    for a,b in zip(data,comparisons,strict=True):
        assert all(a[k]==str(v) for k,v in b.items())
    ev=read(out/'evaluation.json'); counts=collections.Counter(r['outcome'] for r in comparisons)
    assert all(ev[k]==counts[k] for k in ['correct','incorrect','unassessable'])
    assert sum(counts.values())==500
    matrix={str(g):{str(p):sum(r['manual_physical_science']==g and r['physical_science']==p for r in comparisons)
                      for p in [0,1]} for g in [0,1]}
    assert matrix==ev['confusion_matrix_rows_manual_columns_predicted']
    assert sum(matrix[str(i)][str(i)] for i in [0,1])==ev['correct']
    assert ev['physical_precision']==proportion(matrix['1']['1'],sum(matrix[g]['1'] for g in ['0','1']))
    assert ev['physical_recall']==proportion(matrix['1']['1'],sum(matrix['1'].values()))
    assert ev['accuracy_assessable']==proportion(ev['correct'],500-ev['unassessable'])
    assert ev['accuracy_bounds_all_500']==[ev['correct']/500,(ev['correct']+ev['unassessable'])/500]
    export=read(out/'export_validation.json')
    assert export['rows']==500 and export['artifact_tool_cell_preservation_passed']
    for name in ['accuracy_audit.py','export_accuracy_audit.mjs','pilot.py','ACCURACY_AUDIT.md']:
        assert sha(out/'code'/name)==sha(Path(__file__).with_name(name))
    write(out/'validation.json',dict(status='PASS',validated_at_utc=now(),sample_keys_unique=500,
        prior_pilot_overlap=0,seeded_draw_verified=True,evidence_spans_verified=500,
        original_sources_and_prior_outputs_unchanged=True,csv_cells_preserved=True,
        labels_frozen_before_prediction_join=True,counts_reconciled=True))
    checks={str(p.relative_to(out)):sha(p) for p in out.rglob('*') if p.is_file() and p.name!='manifest.json'}
    write(out/'manifest.json',dict(status='complete',created_at_utc=now(),seed=20261022,
        input_checksums_sha256=m['input_checksums_sha256'],output_checksums_sha256=checks,
        runner_sha256=sha(__file__),reproduction='accuracy_audit.py prepare; review + add; freeze; evaluate; export_accuracy_audit.mjs; validate',
        manual_labels_are_frozen_inputs=True))
    print('Validation PASS: 500 keys, zero pilot overlap, all evidence and CSV cells preserved.')


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('command',choices=['prepare','review','add','freeze','evaluate','report','validate'])
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--start',type=int,default=1); p.add_argument('--end',type=int,default=500)
    p.add_argument('--full',action='store_true'); p.add_argument('--labels',type=Path)
    a=p.parse_args(); out=a.data_root/'output/amws'/NAME
    if a.command=='prepare': prepare(a.data_root,out)
    elif a.command=='review': review(out,a.start,a.end,a.full)
    elif a.command=='add': add_labels(out,a.labels)
    elif a.command=='freeze': freeze(a.data_root,out)
    elif a.command=='evaluate': evaluate(a.data_root,out)
    elif a.command=='report': report(a.data_root,out)
    elif a.command=='validate': validate(a.data_root,out)

"""Evaluate the frozen rule snapshot; never refit from held-out labels."""
import argparse
import collections
import importlib.util
import json
import math
import os
from pathlib import Path
from pilot import DEFAULT_ROOT, NAME, LABELS, now, read, write, sha


def proportion(k, n):
    if not n:
        return dict(value=None, numerator=k, denominator=n, wilson_95_interval=None)
    p, z = k/n, 1.959963984540054
    d = 1+z*z/n
    middle = (p+z*z/(2*n))/d
    half = z*math.sqrt(p*(1-p)/n+z*z/(4*n*n))/d
    return dict(value=p,numerator=k,denominator=n,wilson_95_interval=[max(0,middle-half),min(1,middle+half)])


def metrics(records):
    matrix={g:{p:0 for p in LABELS} for g in LABELS}
    for r in records:
        matrix[r['gold']['science_group']][r['prediction']['science_group']]+=1
    per_class={}
    for c in LABELS:
        tp=matrix[c][c];actual=sum(matrix[c].values());predicted=sum(matrix[g][c] for g in LABELS)
        per_class[c]=dict(gold_count=actual,predicted_count=predicted,
            precision=proportion(tp,predicted),recall=proportion(tp,actual))
    correct=sum(matrix[c][c] for c in LABELS)
    abstentions=[r for r in records if r['prediction']['other_reason'] in ('missing','damaged','unmatched')]
    accepted=[r for r in records if not r['prediction']['review_required']]
    return dict(n=len(records), confusion_matrix_rows_gold_columns_predicted=matrix,
        accuracy=proportion(correct,len(records)), per_class=per_class,
        unresolved_count=len(abstentions), unresolved_rate=proportion(len(abstentions),len(records)),
        review_required_count=sum(r['prediction']['review_required'] for r in records),
        predicted_other_reasons=dict(collections.Counter(r['prediction']['other_reason'] for r in records if r['prediction']['science_group']=='other')),
        gold_other_reasons=dict(collections.Counter(r['gold']['other_reason'] for r in records if r['gold']['science_group']=='other')),
        accepted_without_review_accuracy=proportion(sum(r['prediction']['science_group']==r['gold']['science_group'] for r in accepted),len(accepted)),
        mixed_flag_precision=proportion(sum(r['gold']['other_reason']=='mixed' and r['prediction']['other_reason']=='mixed' for r in records),sum(r['prediction']['other_reason']=='mixed' for r in records)),
        mixed_flag_recall=proportion(sum(r['gold']['other_reason']=='mixed' and r['prediction']['other_reason']=='mixed' for r in records),sum(r['gold']['other_reason']=='mixed' for r in records)))


def run(out):
    freeze=read(out/'rules_frozen.json')
    for name,digest in freeze['code_sha256'].items():
        assert sha(out/'frozen_code'/name)==digest, name
    assert sha(out/'gold_development.json')==freeze['development_gold_sha256']
    gold_checks={str(out/f'gold_{split}.json'):sha(out/f'gold_{split}.json') for split in ('development','heldout')}
    if not (out/'gold_labels_frozen.json').exists():
        assert not (out/'evaluation.json').exists()
        write(out/'gold_labels_frozen.json',dict(created_at_utc=now(),gold_checksums=gold_checks,
            manual_gold_recorded_before_prediction=True,
            method='Individual manual interpretation of discipline headings and research excerpts; full biographies consulted when needed. No classifier predictions displayed during held-out labeling.'),True)
    assert read(out/'gold_labels_frozen.json')['gold_checksums']==gold_checks
    spec=importlib.util.spec_from_file_location('amws_frozen_classifier',out/'frozen_code/classifier.py')
    classifier=importlib.util.module_from_spec(spec);spec.loader.exec_module(classifier)
    records=[]
    for split in ('development','heldout'):
        sample=read(out/f'{split}_sample.json');gold=read(out/f'gold_{split}.json')
        assert len(sample)==len(gold)==(300 if split=='development' else 200)
        for r,g in zip(sample,gold):
            assert r['source_key']==g['source_key']
            assert r['raw_text_adjusted'][g['evidence_start']:g['evidence_end']]==g['evidence']
            pred=classifier.classify_entry(r['field_raw'],r['raw_text_adjusted'],r['name_raw'])
            for ev in pred['evidence']:
                assert r[ev['evidence_source']][ev['start']:ev['end']]==ev['evidence']
            assert (pred['other_reason']=='mixed')==(pred['physical_match'] and pred['biological_match'])
            records.append(dict(source=r,gold=g,prediction=pred))
    records.sort(key=lambda r:r['source']['sample_id'])
    evaluation=dict(evaluated_at_utc=now(), rules_version=classifier.RULES['version'],
        holdout_rule_tuning=False,
        benchmark='Codex manual labels; not an independent human-validated gold standard.',
        development=metrics([r for r in records if r['source']['split']=='development']),
        heldout=metrics([r for r in records if r['source']['split']=='heldout']),
        heldout_high_confidence_gold_only=metrics([r for r in records if r['source']['split']=='heldout' and r['gold']['confidence']=='high']),
        all_500_descriptive=metrics(records))
    write(out/'predictions.json',records)
    errors=[]
    for r in records:
        g,p,s=r['gold'],r['prediction'],r['source']
        if g['science_group']!=p['science_group'] or g['other_reason']!=p['other_reason']:
            errors.append(dict(sample_id=s['sample_id'],review_id=s['review_id'],source_key=s['source_key'],split=s['split'],
                name_raw=s['name_raw'],field_raw=s['field_raw'],selected_heading=p['selected_heading'],
                gold=g['science_group'],gold_reason=g['other_reason'],gold_confidence=g['confidence'],gold_evidence=g['evidence'],
                prediction=p['science_group'],predicted_reason=p['other_reason'],
                class_error=g['science_group']!=p['science_group'],manual_note=g['note'],
                predicted_evidence=p['evidence']))
    write(out/'errors.json',errors)
    write(out/'evaluation.json',evaluation)
    audit=[]
    for r in records:
        s,g,p=r['source'],r['gold'],r['prediction']
        audit.append(dict(sample_id=s['sample_id'],source_key=s['source_key'],split=s['split'],review_id=s['review_id'],
            name_raw=s['name_raw'],birth_year=s['birth_year'],field_raw=s['field_raw'],
            manual_group=g['science_group'],manual_other_reason=g['other_reason'],manual_confidence=g['confidence'],
            manual_evidence=g['evidence'],manual_evidence_start=g['evidence_start'],manual_evidence_end=g['evidence_end'],
            manual_reasoning=g['reasoning'],science_group=p['science_group'],other_reason=p['other_reason'],
            mixed=p['other_reason']=='mixed',physical_match=p['physical_match'],biological_match=p['biological_match'],
            review_required=p['review_required'],input_source=p['input_source'],selected_heading=p['selected_heading'],
            matched_rule_ids='; '.join(p['matched_rule_ids']),
            regex_evidence_json=json.dumps(p['evidence'],ensure_ascii=False),
            agrees_with_manual_group=p['science_group']==g['science_group'],
            source_references_json=json.dumps(s['source_references'],ensure_ascii=False),
            raw_text_adjusted=s['raw_text_adjusted'],rules_version=p['rules_version']))
    write(out/'audit_payload.json',dict(columns=list(audit[0]),rows=[list(r.values()) for r in audit]))
    print(json.dumps(dict(heldout=evaluation['heldout'],errors=[e for e in errors if e['split']=='heldout']),ensure_ascii=True))


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--output',type=Path)
    a=p.parse_args();run(a.output or a.data_root/'output/amws'/NAME)

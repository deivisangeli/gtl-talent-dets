"""Post-hoc regression evaluation of a scope guard, not new held-out evidence."""
import argparse
import os
from pathlib import Path
from classifier_boundary_guard import classify_entry, VERSION
from evaluate import metrics
from pilot import DEFAULT_ROOT, NAME, read, write, now, sha


def run(out):
    original=read(out/'predictions.json')
    candidate=[]
    changed=[]
    for old in original:
        s=old['source']
        pred=classify_entry(s['field_raw'],s['raw_text_adjusted'],s['name_raw'])
        for ev in pred['evidence']:
            assert s[ev['evidence_source']][ev['start']:ev['end']]==ev['evidence']
        candidate.append(dict(source=s,gold=old['gold'],prediction=pred))
        if (pred['science_group'],pred['other_reason']) != (old['prediction']['science_group'],old['prediction']['other_reason']):
            changed.append(dict(source_key=s['source_key'],split=s['split'],sample_id=s['sample_id'],
                original_group=old['prediction']['science_group'],original_reason=old['prediction']['other_reason'],
                guarded_group=pred['science_group'],guarded_reason=pred['other_reason'],
                gold_group=old['gold']['science_group'],guard_trigger=pred['scope_guard_trigger']))
    write(out/'predictions_boundary_guard_posthoc.json',candidate)
    write(out/'evaluation_boundary_guard_posthoc.json',dict(evaluated_at_utc=now(),rules_version=VERSION,
        evaluation_status='posthoc_no_fresh_holdout',
        reason='Code audit of malformed section boundaries after initial holdout evaluation; vocabulary unchanged. Original held-out results remain evaluation.json.',
        original_evaluation_sha256=sha(out/'evaluation.json'),
        original_development_partition=metrics([r for r in candidate if r['source']['split']=='development']),
        original_heldout_partition_posthoc=metrics([r for r in candidate if r['source']['split']=='heldout']),
        changed=changed))
    payload=read(out/'audit_payload.json')
    additions=['guarded_science_group','guarded_other_reason','guarded_mixed','guarded_review_required',
               'guarded_physical_match','guarded_biological_match','guarded_input_source',
               'guarded_evidence_json','guarded_rules_version','guarded_evaluation_status']
    import json
    payload['columns']+=additions
    assert len(payload['rows'])==len(candidate)
    for row,r in zip(payload['rows'],candidate):
        p=r['prediction']
        row += [p['science_group'],p['other_reason'],p['other_reason']=='mixed',p['review_required'],
                p['physical_match'],p['biological_match'],p['input_source'],json.dumps(p['evidence'],ensure_ascii=False),
                p['rules_version'],p['evaluation_status']]
    write(out/'audit_payload_with_guard.json',payload)
    print(json.dumps(dict(changed=changed,posthoc_accuracy=metrics([r for r in candidate if r['source']['split']=='heldout'])['accuracy'])))


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--output',type=Path)
    a=p.parse_args();run(a.output or a.data_root/'output/amws'/NAME)

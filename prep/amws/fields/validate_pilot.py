"""Independently validate sampling, source joins, frozen predictions and CSV."""
import argparse
import collections
import csv
import importlib.util
import json
import os
import random
from pathlib import Path
from pilot import DEFAULT_ROOT, NAME, RELEASE, PROCESSED, LABELS, rows, read, write, sha, now


def main(root,out):
    manifest=read(out/'sample_manifest.json');freeze=read(out/'rules_frozen.json')
    evaluation=read(out/'evaluation.json');guard_eval=read(out/'evaluation_boundary_guard_posthoc.json')
    gold_freeze=read(out/'gold_labels_frozen.json')
    assert not freeze['holdout_labels_present_at_freeze']
    assert freeze['frozen_at_utc'] < gold_freeze['created_at_utc'] < evaluation['evaluated_at_utc']
    assert not evaluation['holdout_rule_tuning']
    for p,digest in manifest['source_checksums'].items():
        assert sha(p)==digest, f'Input changed: {p}'
    for filename,digest in freeze['code_sha256'].items():
        assert sha(out/'frozen_code'/filename)==digest
        assert sha(Path(__file__).with_name(filename))==digest
    assert sha(out/'gold_development.json')==freeze['development_gold_sha256']
    assert all(sha(p)==h for p,h in gold_freeze['gold_checksums'].items())
    sample=[]
    for split,n in [('development',300),('heldout',200)]:
        s=read(out/f'{split}_sample.json');g=read(out/f'gold_{split}.json')
        assert len(s)==len(g)==n
        assert [r['source_key'] for r in s]==manifest[f'{split}_keys']
        assert sha(out/f'{split}_sample.json')==manifest[f'{split}_sample_sha256']
        for r,m in zip(s,g):
            assert r['source_key']==m['source_key']
            assert m['manual_review_complete']
            assert r['raw_text_adjusted'][m['evidence_start']:m['evidence_end']]==m['evidence']
        sample+=s
    sample.sort(key=lambda r:r['sample_id'])
    keys=[r['source_key'] for r in sample]
    assert len(keys)==len(set(keys))==500
    assert [r['sample_id'] for r in sample]==list(range(1,501))
    selected={r['source_key']:r for r in sample}
    pool=[]
    for i,r in enumerate(rows(root/RELEASE),1):
        if r['source_key'].startswith('1986:'):
            assert r['is_us_birth'].strip().lower()=='true'
            pool.append(r['source_key'])
        if r['source_key'] in selected:
            original=selected[r['source_key']]
            assert original['release_row']==i
            for c in ['name_raw','birth_year','raw_text_adjusted','entry_id']:
                assert original[c]==r[c]
    assert len(pool)==len(set(pool))==79658
    assert keys==random.Random(20261020).sample(sorted(pool),500)
    shuffled=keys.copy();random.Random(20261021).shuffle(shuffled)
    assert shuffled[:300]==manifest['development_keys']
    assert shuffled[300:]==manifest['heldout_keys']
    lookup=collections.defaultdict(list)
    wanted={(r['name_raw'],r['raw_text_adjusted']) for r in sample}
    for i,r in enumerate(rows(root/PROCESSED),1):
        ident=r['name_raw'],r['raw_text_adjusted']
        if ident in wanted:
            lookup[ident].append(dict(processed_row=i,field=r['field'],doc_id=r['doc_id'],
                source_file=r['source_file'],lineid=r['lineid'],entry_instance=r['entry_instance']))
    for r in sample:
        refs=lookup[(r['name_raw'],r['raw_text_adjusted'])]
        assert refs==r['source_references'] and refs
        assert {ref['field'].strip() for ref in refs}=={r['field_raw']}
    spec=importlib.util.spec_from_file_location('amws_original_validation',out/'frozen_code/classifier.py')
    original=importlib.util.module_from_spec(spec);spec.loader.exec_module(original)
    from classifier_boundary_guard import classify_entry as guarded_classify
    records=read(out/'predictions.json');guarded=read(out/'predictions_boundary_guard_posthoc.json')
    assert len(records)==len(guarded)==500
    evidence_count=0
    for r,v in zip(records,guarded):
        s=r['source']
        assert s['source_key']==v['source']['source_key']
        assert original.classify_entry(s['field_raw'],s['raw_text_adjusted'],s['name_raw'])==r['prediction']
        assert guarded_classify(s['field_raw'],s['raw_text_adjusted'],s['name_raw'])==v['prediction']
        for rec in (r,v):
            p=rec['prediction']
            assert p['science_group'] in LABELS
            assert (p['other_reason']=='mixed')==(p['physical_match'] and p['biological_match'])
            assert (p['science_group']=='other')==bool(p['other_reason'])
            assert p['review_required']==(p['other_reason'] in ['mixed','missing','damaged','unmatched'])
            for ev in p['evidence']:
                assert s[ev['evidence_source']][ev['start']:ev['end']]==ev['evidence']
                if rec is v and ev['evidence_source']=='raw_text_adjusted':
                    assert ev['end']<=p['scope_guard_offset']
                evidence_count+=1
    def check_metrics(data,expected):
        matrix={g:{p:0 for p in LABELS} for g in LABELS}
        for r in data:matrix[r['gold']['science_group']][r['prediction']['science_group']]+=1
        assert matrix==expected['confusion_matrix_rows_gold_columns_predicted']
        assert expected['n']==len(data)
        assert expected['accuracy']['numerator']==sum(matrix[g][g] for g in LABELS)
        assert expected['accuracy']['denominator']==len(data)
        for g in LABELS:
            m=expected['per_class'][g]
            assert m['precision']['numerator']==matrix[g][g]
            assert m['precision']['denominator']==sum(matrix[label][g] for label in LABELS)
            assert m['recall']['numerator']==matrix[g][g]
            assert m['recall']['denominator']==sum(matrix[g].values())
        assert expected['unresolved_count']==sum(r['prediction']['other_reason'] in ['missing','damaged','unmatched'] for r in data)
    for split in ['development','heldout']:
        check_metrics([r for r in records if r['source']['split']==split],evaluation[split])
    check_metrics(records,evaluation['all_500_descriptive'])
    check_metrics([r for r in guarded if r['source']['split']=='heldout'],guard_eval['original_heldout_partition_posthoc'])
    assert guard_eval['evaluation_status']=='posthoc_no_fresh_holdout'
    assert sha(out/'evaluation.json')==guard_eval['original_evaluation_sha256']
    payload=read(out/'audit_payload_with_guard.json')
    with (out/'amws_field_regex_pilot_500.csv').open(encoding='utf-8-sig',newline='') as f:
        csv_rows=list(csv.reader(f))
    def to_csv(v):
        if isinstance(v,bool):return str(v).lower()
        return '' if v is None else str(v)
    assert csv_rows[0]==payload['columns'] and len(csv_rows)==501
    assert csv_rows[1:]==[[to_csv(v) for v in row] for row in payload['rows']]
    error_rows=read(out/'errors.json')
    heldout_errors=[r for r in error_rows if r['split']=='heldout']
    assert sum(r['class_error'] for r in heldout_errors)==29
    assert len(heldout_errors)==32
    report=(out/'report.txt').read_text(encoding='utf-8')
    assert all(f'Sample {r["sample_id"]} {r["source_key"]}' in report for r in heldout_errors)
    assert 'POST-HOC REGRESSION AGREEMENT, NOT A FRESH HELD-OUT' in report
    pub=read(out/'publication_metadata.json');assert pub['tests_passed']==22
    for p in Path(__file__).parent.iterdir():
        if p.is_file() and p.suffix in ('.py','.json','.md','.mjs'):
            assert sha(p)==sha(out/'candidate_code'/p.name)
    checksums={str(p.relative_to(out)):sha(p) for p in out.rglob('*')
               if p.is_file() and p.name!='validation.json' and '__pycache__' not in p.parts}
    write(out/'validation.json',dict(status='PASS',validated_at_utc=now(),source_checksums_unchanged=True,
        population_entries=79658,distinct_sample_keys=500,development=300,heldout=200,
        exact_source_joins=500,manual_labels_with_literal_evidence=500,
        regex_evidence_spans_verified=evidence_count,csv_rows=500,csv_columns=len(payload['columns']),
        frozen_code_unchanged=True,frozen_evaluation_preserved=True,posthoc_results_separately_labeled=True,
        behavioral_tests_passed=22,seeded_sampling_and_split_reproduced=True,
        confusion_matrices_and_metric_denominators_verified=True,output_checksums_sha256=checksums,
        limitations='Validation checks reproducibility/provenance, not independent correctness of manual scientific judgments.'))
    print(json.dumps(dict(status='PASS',sample_entries=500,manual_labels=500,evidence_spans=evidence_count,
                         source_checksums_unchanged=True,csv_columns=len(payload['columns']))))


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--output',type=Path)
    a=p.parse_args();main(a.data_root,a.output or a.data_root/'output/amws'/NAME)

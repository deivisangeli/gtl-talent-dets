"""Second fresh v2 evaluation: 500 new 1986 entries, unchanged frozen classifier."""
import argparse
import collections
import csv
import json
import os
from pathlib import Path
import random
import shutil
import sys
from pilot import DEFAULT_ROOT,now,read,rows,sha,write
import rules_v2_audit as prior

NAME='physical_science_rules_v2_audit_500_20261023'
PRIOR='output/amws/'+prior.NAME
SEED=20261025
csv.field_size_limit(16*1024*1024)


def input_checks(root):
    paths=prior.input_paths(root)
    paths.extend(p for p in (root/PRIOR).rglob('*') if p.is_file() and '__pycache__' not in p.parts)
    extra=root/'output/amws/_tmp_agent_audit_v2/sample_manifest.csv'
    if extra.exists(): paths.append(extra)
    paths.extend(Path(__file__).with_name(n) for n in prior.CLASS_FILES)
    return {str(p):sha(p) for p in sorted(set(paths))}


def excluded(root):
    keys=prior.exclusion_keys(root)
    keys.update(r['source_key'] for r in read(root/PRIOR/'blind_sample.json'))
    extra=root/'output/amws/_tmp_agent_audit_v2/sample_manifest.csv'
    if extra.exists():keys.update('1986:'+str(int(float(r['lineid']))) for r in rows(extra))
    return keys


def prepare(root,out):
    if (out/'sample_manifest.json').exists():
        m=read(out/'sample_manifest.json')
        assert input_checks(root)==read(out/'rules_frozen.json')['input_checksums_sha256']
        assert sha(out/'blind_sample.json')==m['blind_sample_sha256'];print('Prepared sample verified.');return
    original=read(root/PRIOR/'rules_frozen.json'); before=input_checks(root)
    dest=out/'frozen_classifier';dest.mkdir(parents=True,exist_ok=True)
    for name,digest in original['classifier_sha256'].items():
        assert sha(root/PRIOR/'frozen_classifier'/name)==sha(Path(__file__).with_name(name))==digest
        shutil.copyfile(root/PRIOR/'frozen_classifier'/name,dest/name)
    write(out/'rules_frozen.json',dict(frozen_at_utc=now(),original_freeze_sha256=sha(root/PRIOR/'rules_frozen.json'),
        version=original['version'],classifier_sha256=original['classifier_sha256'],
        input_checksums_sha256=before,changes_since_100_entry_evaluation=False),True)
    population={r['source_key']:r for r in rows(root/prior.RELEASE) if r['source_key'].startswith('1986:')}
    assert len(population)==79073
    exclusion=excluded(root); pool=sorted(set(population)-exclusion)
    keys=random.Random(SEED).sample(pool,500); wanted=set(keys)
    assert not wanted&exclusion
    bios={r['source_key']:r for r in rows(root/prior.BIO) if r['source_key'] in wanted}
    fields={r['source_key']:r for r in rows(root/prior.OLD/'amws_ed16_physical_science_audit.csv') if r['source_key'] in wanted}
    assert set(bios)==set(fields)==wanted
    sample=[]
    for i,k in enumerate(keys,1):
        s=population[k];f=fields[k];bio=bios[k]
        assert s['raw_text_adjusted']==bio['raw_text_adjusted'] and s['name_raw']==f['name_raw']
        sample.append(dict(sample_id=i,source_key=k,name_raw=s['name_raw'],field_raw=f['field_raw'],
            raw_text_adjusted=bio['raw_text_adjusted'],source_references_json=f['source_references_json'],
            biography_source_file=bio['source_file'],biography_source_column=bio['source_column']))
    write(out/'blind_sample.json',sample,True)
    write(out/'sample_manifest.json',dict(sampled_at_utc=now(),seed=SEED,population_size=len(population),
        excluded_population_keys=len(set(population)&exclusion),eligible_size=len(pool),sample_size=500,
        excluded_keys=sorted(set(population)&exclusion),sample_keys_in_draw_order=keys,
        blind_sample_sha256=sha(out/'blind_sample.json'),classifier_freeze_sha256=sha(out/'rules_frozen.json'),
        method='random.Random(20261025).sample(sorted(v06_1986_keys - all_previous_review_keys),500)',
        exclusion_policy='All four prior field-review samples, prior fresh v2 100, plus existing additional review manifest.'),True)
    assert input_checks(root)==before
    print(f'500 fresh keys frozen; {len(pool):,} eligible, {len(set(population)&exclusion):,} excluded. Classifier unchanged.')


def add(out,path):
    assert not (out/'manual_frozen.json').exists()
    sample={r['sample_id']:r for r in read(out/'blind_sample.json')}
    gold=read(out/'manual_labels.json') if (out/'manual_labels.json').exists() else []
    seen={r['sample_id'] for r in gold}
    for item in read(path):
        i,label,evidence,*rest=item
        assert i not in seen and label in [0,1,'unclear']
        s=sample[i]; start=s['raw_text_adjusted'].find(evidence)
        assert evidence and start>=0,(i,evidence)
        reason=rest[0] if rest else 'Stated discipline: '+evidence+'. '+('Physical under the agreed broad definition.' if label==1 else 'Biological, medical or non-target work without a separate Physical field.' if label==0 else 'Source insufficient.')
        gold.append(dict(sample_id=i,source_key=s['source_key'],physical_science=label,evidence=evidence,
            evidence_start=start,evidence_end=start+len(evidence),reasoning=reason,confidence=rest[1] if len(rest)>1 else 'high'))
        seen.add(i)
    write(out/'manual_labels.json',sorted(gold,key=lambda r:r['sample_id']))
    print(f'{len(gold)}/500 blind manual labels recorded.')


def freeze(out):
    sample=read(out/'blind_sample.json');gold=read(out/'manual_labels.json')
    assert len(gold)==500 and [r['source_key'] for r in gold]==[r['source_key'] for r in sample]
    for s,g in zip(sample,gold):assert s['raw_text_adjusted'][g['evidence_start']:g['evidence_end']]==g['evidence']
    write(out/'manual_frozen.json',dict(frozen_at_utc=now(),manual_sha256=sha(out/'manual_labels.json'),
        labels_recorded_before_predictions=True,benchmark='Prediction-blinded Codex manual review, not independent human expert validation.'),True)
    print('500 manual judgments frozen before any predictions.')


def report(out):
    m=read(out/'sample_manifest.json');ev=read(out/'evaluation.json');flat=read(out/'audit_records.json')
    records=read(out/'comparisons.json')
    notes=read(out/'case_notes.json');note={r['source_key']:r for r in notes}
    required={r['source_key'] for r in flat if r['prediction_changed'] or r['ocr_assisted'] or r['baseline_outcome']!='correct' or r['revised_outcome']!='correct'}
    assert set(note)==required
    summary={}
    for model in ['baseline','revised']:
        unresolved=[r for r in records if r[model]['other_reason'] in ['missing','damaged','unmatched']]
        summary[model]=dict(mixed=sum(r[model]['other_reason']=='mixed' for r in records),
            unresolved=len(unresolved),resolved=len(records)-len(unresolved),
            unresolved_manual_labels=dict(collections.Counter(str(r['manual']['physical_science']) for r in unresolved)),
            error_categories=dict(collections.Counter(note[r['source_key']]['category'] for r in flat if r[model+'_outcome']=='incorrect')),
            accuracy_bounds_all_500=[ev[model]['correct']/500,(ev[model]['correct']+ev[model]['unassessable'])/500])
    write(out/'error_summary.json',summary)
    for a,r in zip(flat,records,strict=True):
        for model in ['baseline','revised']:
            a[model+'_mixed']=int(r[model]['other_reason']=='mixed')
            a[model+'_unresolved']=int(r[model]['other_reason'] in ['missing','damaged','unmatched'])
            a[model+'_input_source']=r[model]['input_source']
        a['review_case_category']=note.get(a['source_key'],{}).get('category','')
        a['review_case_explanation']=note.get(a['source_key'],{}).get('explanation','')
    write(out/'audit_records.json',flat)
    columns=list(flat[0]);write(out/'audit_payload.json',dict(columns=columns,rows=[[a[c] for c in columns] for a in flat]))
    lines=['PHYSICAL RULES V2: SECOND FRESH EVALUATION, 500 ENTRIES','',
        '1986 only; same frozen v2 classifier as the prior 100-entry evaluation.',
        f"Population: {m['population_size']:,}; excluded reviewed keys: {m['excluded_population_keys']:,}; eligible: {m['eligible_size']:,}.",
        'Seed 20261025; random.Random(seed).sample(sorted(eligible_keys),500), no replacement.',
        'Exclusions cover the original pilot, original 500-entry audit, 100-entry',
        'field-of-work sample, cross-edition sample, prior fresh v2 100-entry sample',
        'and the existing additional review manifest. No overlap or difficult-case replacement.',
        'Classifiers copied byte-for-byte from the prior frozen release before drawing.',
        'All 500 biographies manually reviewed without predictions, recovered fields or',
        'matched rules displayed. Full biographies consulted for ambiguous headings.',
        'Manual labels and evidence frozen before either model was evaluated.',
        'Reference standard: Codex manual interpretation, not independent human validation.',
        'The broad policy includes engineering, mathematics, statistics and computing;',
        'Physical+Biological fields count as 1. Biochemistry/biophysics alone remain Biological.',
        'Unknown reference labels remain in the sample but are excluded from metrics.',
        'Wilson intervals express sampling uncertainty, not uncertainty in manual judgments.',
        'The current sample has 281 manual Physical, 218 manual non-Physical and one unclear.',
        'Medicinal chemistry with explicit organic synthesis was adjudicated as Physical',
        'during the blind review, before freezing (blind_review_amendments.json).',
        'Biochemical enzyme/protein/lipid work without a separate Physical specialty is 0.','',
        'Paired results on this new sample:']
    fmt=lambda v:'undefined' if v['value'] is None else f"{v['numerator']}/{v['denominator']} = {v['value']:.2%}"
    for model in ['baseline','revised']:
        e=ev[model];lines += [model.upper(), 'Precision: '+fmt(e['precision']), 'Recall: '+fmt(e['recall']),
            'Accuracy: '+fmt(e['accuracy']),f"Correct {e['correct']}; incorrect {e['incorrect']}; unassessable {e['unassessable']}.",
            'Full metrics and confusion matrix (manual rows / prediction columns):',json.dumps(e,indent=2),'']
    lines += [f"Changed binary predictions: {ev['prediction_changes']}",
              f"OCR-assisted revised entries: {ev['revised_ocr_assisted_entries']}",
              'All seven changes are corrections against the frozen manual reference.',
              'Mixed predictions, unresolved cases, error causes and all-500 accuracy bounds:',
              json.dumps(summary,indent=2),'',
              'Every error, unassessable case, changed prediction and OCR-assisted match:']
    for r in flat:
        if r['source_key'] in note:
            lines += [f"{r['sample_id']}. {r['source_key']} {r['name_raw']}",
                f"Manual={r['manual_physical_science']}; original={r['baseline_physical_science']}; revised={r['revised_physical_science']}.",
                'Evidence: '+r['manual_evidence'],note[r['source_key']]['explanation'],'']
    lines += ['No rules or manual labels were tuned to this sample. Prior samples and',
        'their reported results remain intact. This evaluates only the existing frozen',
        'v2, and does not replace the full-population dummy. Heading-only scope and',
        'allowlisted OCR repair can still miss specialties or Physical work in the body.',
        'Interdisciplinary judgments are documented with confidence and reasoning.',
        'Reproduction: rules_v2_audit_500.py prepare/review/add/freeze-manual/evaluate;',
        'then report, export_rules_v2_audit_500.mjs and validate. Frozen manual labels',
        'are reproduction inputs, not generated from regex predictions.','']
    (out/'report.txt').write_text('\n'.join(lines),encoding='utf8')
    dest=out/'code';dest.mkdir(exist_ok=True)
    for name in ['rules_v2_audit_500.py','rules_v2_audit.py','export_rules_v2_audit_500.mjs','pilot.py','RULES_V2_500.md']:
        shutil.copyfile(Path(__file__).with_name(name),dest/name)


def validate(root,out):
    f=read(out/'rules_frozen.json');m=read(out/'sample_manifest.json');g=read(out/'manual_frozen.json')
    assert input_checks(root)==f['input_checksums_sha256']
    assert sha(out/'manual_labels.json')==g['manual_sha256'] and sha(out/'blind_sample.json')==m['blind_sample_sha256']
    assert f['frozen_at_utc']<m['sampled_at_utc']<g['frozen_at_utc']<read(out/'evaluation.json')['evaluated_at_utc']
    population={r['source_key']:r for r in rows(root/prior.RELEASE) if r['source_key'].startswith('1986:')}
    excluded_set=excluded(root);pool=sorted(set(population)-excluded_set)
    keys=random.Random(SEED).sample(pool,500)
    sample=read(out/'blind_sample.json');records=read(out/'comparisons.json');flat=read(out/'audit_records.json')
    exported=list(rows(out/'amws_physical_rules_v2_audit_500.csv'))
    assert len(keys)==len(set(keys))==len(sample)==len(records)==len(exported)==500
    assert not set(keys)&excluded_set and keys==m['sample_keys_in_draw_order']==[r['source_key'] for r in sample]
    wanted=set(keys); bios={r['source_key']:r for r in rows(root/prior.BIO) if r['source_key'] in wanted}
    fields={r['source_key']:r for r in rows(root/prior.OLD/'amws_ed16_physical_science_audit.csv') if r['source_key'] in wanted}
    assert set(bios)==set(fields)==wanted
    baseline,revised=prior.load(out)
    for r,a,c in zip(records,flat,exported,strict=True):
        s,manual=r['source'],r['manual'];k=s['source_key']
        assert s['raw_text_adjusted']==bios[k]['raw_text_adjusted']==population[k]['raw_text_adjusted']
        assert s['field_raw']==fields[k]['field_raw'] and s['source_references_json']==fields[k]['source_references_json']
        assert s['raw_text_adjusted'][manual['evidence_start']:manual['evidence_end']]==manual['evidence']
        assert all(c[key]==str(v) for key,v in a.items())
        for model,mod in [('baseline',baseline),('revised',revised)]:
            p=mod.classify_entry(s['field_raw'],s['raw_text_adjusted'],s['name_raw'])
            assert p==r[model];prior.check_evidence(p,s)
    ev=read(out/'evaluation.json')
    for model in ['baseline','revised']:
        assert ev[model]==prior.count_metrics(records,model)
        known=[r for r in records if r['manual']['physical_science']!='unclear']
        tp=sum(r['manual']['physical_science']==1 and r[model]['physical_match'] for r in known)
        assert ev[model]['precision']['numerator']==ev[model]['recall']['numerator']==tp
        assert ev[model]['precision']['denominator']==sum(r[model]['physical_match'] for r in known)
        assert ev[model]['recall']['denominator']==sum(r['manual']['physical_science']==1 for r in known)
    old=read(root/PRIOR/'rules_frozen.json')
    assert f['classifier_sha256']==old['classifier_sha256']
    for name,digest in old['classifier_sha256'].items():assert sha(out/'frozen_classifier'/name)==sha(Path(__file__).with_name(name))==digest
    for name in ['rules_v2_audit_500.py','rules_v2_audit.py','export_rules_v2_audit_500.mjs','pilot.py','RULES_V2_500.md']:
        assert sha(out/'code'/name)==sha(Path(__file__).with_name(name))
    assert read(out/'export_validation.json')['artifact_tool_cell_preservation_passed']
    write(out/'validation.json',dict(status='PASS',validated_at_utc=now(),unique_sample_keys=500,prior_review_overlap=0,
        unchanged_frozen_classifier=True,all_manual_and_model_evidence_offsets_verified=True,
        source_joins_verified=True,metrics_recomputed=True,csv_cells_preserved=True,prior_releases_unchanged=True))
    outputs={str(p.relative_to(out)):sha(p) for p in out.rglob('*') if p.is_file() and p.name!='manifest.json' and '__pycache__' not in p.parts}
    write(out/'manifest.json',dict(status='complete',completed_at_utc=now(),seed=SEED,sample_size=500,
        version=f['version'],input_checksums_sha256=f['input_checksums_sha256'],output_checksums_sha256=outputs,
        reproduction='rules_v2_audit_500.py prepare; frozen manual labels; evaluate; report; export_rules_v2_audit_500.mjs; validate'))
    print('PASS: 500 fresh keys, frozen unchanged classifiers/labels, source/evidence joins, metrics and original checksums.')


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('command',choices=['prepare','review','add','freeze-manual','evaluate','report','validate'])
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--start',type=int,default=1);p.add_argument('--end',type=int,default=500)
    p.add_argument('--full',action='store_true');p.add_argument('--labels',type=Path)
    a=p.parse_args();out=a.data_root/'output/amws'/NAME
    if a.command=='prepare':prepare(a.data_root,out)
    elif a.command=='review':prior.review(out,a.start,a.end,a.full)
    elif a.command=='add':add(out,a.labels)
    elif a.command=='freeze-manual':freeze(out)
    elif a.command=='evaluate':prior.evaluate(a.data_root,out)
    elif a.command=='report':report(out)
    elif a.command=='validate':validate(a.data_root,out)

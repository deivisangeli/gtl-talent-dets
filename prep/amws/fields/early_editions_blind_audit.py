"""Fresh confirmation: frozen source-field adapter, 50 entries per early edition."""
import argparse
import collections
import csv
import importlib
import json
import os
from pathlib import Path
import random
import shutil
import subprocess
from pilot import DEFAULT_ROOT,now,read,rows,sha,write
import cross_edition_blind_audit as prior
import rules_v2_audit as metric

NAME='physical_science_early_editions_blind_audit_50_20261023'
SEED=20261027
EDITIONS=['1906','1938','1955']
SIZES={ed:prior.source.SIZES[ed] for ed in EDITIONS}
ADAPTER='cross_edition_field_adapter.py'
CODE=['early_editions_blind_audit.py','cross_edition_blind_audit.py','cross_edition_blind_sources.R',
      'cross_edition_sample.py','rules_v2_audit.py','rules_v2_audit_500.py','pilot.py',
      'export_early_editions_blind_audit.mjs','EARLY_EDITIONS_BLIND_AUDIT.md']
csv.field_size_limit(16*1024*1024)


def excluded(root):
    keys=prior.exclusions(root)
    keys.update(r['source_key'] for r in read(root/'output/amws'/prior.NAME/'sample_records.json'))
    return keys


def checks(root):
    result=prior.checks(root)
    paths=[p for p in (root/'output/amws'/prior.NAME).rglob('*') if p.is_file() and '__pycache__' not in p.parts]
    paths += [Path(__file__).with_name(ADAPTER),Path(__file__).with_name('cross_edition_blind_sources.R')]
    result.update({str(p):sha(p) for p in paths})
    return dict(sorted(result.items()))


def draw(root):
    pools=collections.defaultdict(list)
    for r in rows(root/prior.source.RELEASE):
        ed=r['source_key'].split(':')[0]
        if ed in EDITIONS:pools[ed].append(r['source_key'])
    assert {ed:len(v) for ed,v in pools.items()}==SIZES
    assert all(len(v)==len(set(v)) for v in pools.values())
    ex=excluded(root);eligible={ed:sorted(set(pools[ed])-ex) for ed in EDITIONS};rng=random.Random(SEED)
    return [k for ed in EDITIONS for k in rng.sample(eligible[ed],50)],{ed:len(eligible[ed]) for ed in EDITIONS}


def prepare(root,out,rscript):
    if (out/'sample_records_frozen.json').exists():
        assert checks(root)==read(out/'rules_frozen.json')['input_checksums_sha256']
        assert sha(out/'sample_records.json')==read(out/'sample_records_frozen.json')['sha256']
        print('Existing frozen sample verified.');return
    previous=root/'output/amws'/prior.NAME;f=read(previous/'rules_frozen.json');before=checks(root)
    dest=out/'frozen_classifier';dest.mkdir(parents=True,exist_ok=True)
    modelhashes=dict(f['classifier_sha256']);modelhashes[ADAPTER]=read(previous/'field_adapter_frozen.json')['adapter_sha256']
    for name,digest in modelhashes.items():
        origin=previous/('code' if name==ADAPTER else 'frozen_classifier')/name
        assert sha(origin)==sha(Path(__file__).with_name(name))==digest
        shutil.copyfile(origin,dest/name)
    write(out/'rules_frozen.json',dict(frozen_at_utc=now(),classifier_sha256=modelhashes,
        input_checksums_sha256=before,version=f['version'],
        adapter_unchanged_from_prior_diagnostic=True,adapter_frozen_before_new_draw=True),True)
    keys,eligible=draw(root);assert len(set(keys))==150 and not set(keys)&excluded(root)
    write(out/'sample_manifest.json',dict(sampled_at_utc=now(),seed=SEED,sample_size=150,sample_per_edition=50,
        sampled_keys_in_order=keys,population_sizes=SIZES,eligible_sizes=eligible,
        excluded_retained_counts={ed:SIZES[ed]-eligible[ed] for ed in EDITIONS},
        excluded_keys=sorted(excluded(root)),
        method='One random.Random(20261027); sample(sorted(eligible_keys),50) per edition, 1906 then 1938 then 1955.'),True)
    subprocess.run([rscript,str(Path(__file__).with_name('cross_edition_blind_sources.R')),str(root),str(out)],check=True)
    early={r['source_key']:r for r in read(out/'early_source_records.json')}
    bios={r['source_key']:r for r in rows(root/prior.source.BIOS) if r['source_key'] in set(keys)}
    assert set(early)==set(bios)==set(keys)
    records=[]
    for i,k in enumerate(keys,1):
        s,b=early[k],bios[k]
        records.append(dict(sample_id=i,source_key=k,edition=k.split(':')[0],name_raw=s['name_raw'],
            field_raw=s['field_raw'],raw_text_adjusted=b['raw_text_adjusted'],
            source_provenance={key:s[key] for key in ['source_file','source_column','lineid']},
            biography_source_file=b['source_file'],biography_source_column=b['source_column']))
    write(out/'sample_records.json',records,True)
    write(out/'sample_records_frozen.json',dict(frozen_at_utc=now(),sha256=sha(out/'sample_records.json')),True)
    assert checks(root)==before
    print('150 fresh records frozen; 50 per edition. Eligible:',eligible)


def review(out,start,end,full):
    for r in read(out/'sample_records.json'):
        if start<=r['sample_id']<=end:
            print(f"[{r['sample_id']}] {r['source_key']} {r['name_raw']} | SOURCE FIELD: {r['field_raw']}")
            print('BIO: '+(r['raw_text_adjusted'] if full else r['raw_text_adjusted'][:700]))


def add(out,file):
    assert not (out/'manual_frozen.json').exists() and not (out/'evaluation.json').exists()
    sample={r['sample_id']:r for r in read(out/'sample_records.json')}
    gold=read(out/'manual_labels.json') if (out/'manual_labels.json').exists() else [];seen={r['sample_id'] for r in gold}
    for i,label,esrc,evidence,reason,*extra in read(file):
        assert i not in seen and label in [0,1,'unclear'] and esrc in ['field_raw','raw_text_adjusted']
        s=sample[i];lo=s[esrc].find(evidence);assert evidence and lo>=0,(i,evidence)
        gold.append(dict(sample_id=i,source_key=s['source_key'],edition=s['edition'],physical_science=label,
            evidence_source=esrc,evidence=evidence,evidence_start=lo,evidence_end=lo+len(evidence),
            reasoning=reason,confidence=extra[0] if extra else 'high'))
        seen.add(i)
    write(out/'manual_labels.json',sorted(gold,key=lambda r:r['sample_id']))
    print(f'{len(gold)}/150 blind labels saved.')


def freeze(out):
    sample=read(out/'sample_records.json');gold=read(out/'manual_labels.json')
    assert len(gold)==150 and [r['source_key'] for r in gold]==[r['source_key'] for r in sample]
    for s,g in zip(sample,gold):assert s[g['evidence_source']][g['evidence_start']:g['evidence_end']]==g['evidence']
    write(out/'manual_frozen.json',dict(frozen_at_utc=now(),manual_sha256=sha(out/'manual_labels.json'),
        labels_recorded_before_predictions=True,benchmark='Prediction-blinded Codex manual review; not independent human expert validation.'),True)
    print('150 manual labels frozen before predictions.')


def load(out):
    _,v2=metric.load(out);adapter=importlib.import_module('cross_edition_field_adapter')
    assert Path(adapter.__file__).resolve()==(out/'frozen_classifier'/ADAPTER).resolve()
    return v2,adapter


def evaluate(root,out):
    f=read(out/'rules_frozen.json');g=read(out/'manual_frozen.json')
    assert checks(root)==f['input_checksums_sha256'] and sha(out/'manual_labels.json')==g['manual_sha256']
    for name,digest in f['classifier_sha256'].items():assert sha(out/'frozen_classifier'/name)==sha(Path(__file__).with_name(name))==digest
    model,adapter=load(out);records=[];flat=[]
    for s,gold in zip(read(out/'sample_records.json'),read(out/'manual_labels.json'),strict=True):
        assert s['source_key']==gold['source_key']
        p=adapter.classify_verified_field(model,s['field_raw']);metric.check_evidence(p,s)
        records.append(dict(source=s,manual=gold,prediction=p))
        label=gold['physical_science'];outcome='unassessable' if label=='unclear' else 'correct' if int(p['physical_match'])==label else 'incorrect'
        flat.append(dict(sample_id=s['sample_id'],source_key=s['source_key'],edition=s['edition'],name_raw=s['name_raw'],
            manual_physical_science=label,physical_science=int(p['physical_match']),outcome=outcome,
            mixed=int(p['other_reason']=='mixed'),unresolved=int(p['other_reason'] in ['missing','damaged','unmatched']),
            science_group=p['science_group'],other_reason=p['other_reason'],review_required=int(p['review_required']),
            input_source=p['input_source'],matched_rule_ids=';'.join(p['matched_rule_ids']),
            ocr_assisted=int(p['ocr_assisted']),evidence_json=json.dumps(p['evidence'],ensure_ascii=False),
            field_raw=s['field_raw'],manual_evidence_source=gold['evidence_source'],manual_evidence=gold['evidence'],
            manual_evidence_start=gold['evidence_start'],manual_evidence_end=gold['evidence_end'],
            manual_reasoning=gold['reasoning'],manual_confidence=gold['confidence'],
            source_provenance_json=json.dumps(s['source_provenance'],ensure_ascii=False),raw_text_adjusted=s['raw_text_adjusted']))
    metrics=dict(evaluated_at_utc=now(),per_edition={ed:metric.count_metrics([r for r in records if r['source']['edition']==ed],'prediction') for ed in EDITIONS},
        raw_equal_allocation_sample=metric.count_metrics(records,'prediction'),
        pooled_warning='Equal allocation; pooled sample metrics are not population-weighted estimates.',
        holdout_tuning=False,adapter_and_vocabulary_frozen_before_draw=True,reference_standard=g['benchmark'])
    write(out/'comparisons.json',records);write(out/'audit_records.json',flat);write(out/'evaluation.json',metrics)
    print(json.dumps(metrics['per_edition'],indent=2))


def report(out):
    ev=read(out/'evaluation.json');m=read(out/'sample_manifest.json');flat=read(out/'audit_records.json')
    notes=read(out/'case_notes.json');by={r['source_key']:r for r in notes}
    required={r['source_key'] for r in flat if r['outcome']!='correct' or r['ocr_assisted']}
    assert set(by)==required
    for r in flat:r['case_explanation']=by.get(r['source_key'],{}).get('explanation','')
    write(out/'audit_records.json',flat);columns=list(flat[0]);write(out/'audit_payload.json',dict(columns=columns,rows=[[r[c] for c in columns] for r in flat]))
    fmt=lambda p:'undefined' if p['value'] is None else f"{p['numerator']}/{p['denominator']} = {p['value']:.2%}"
    lines=['AMWS EARLIER EDITIONS: FRESH BLIND CONFIRMATION','',
        '150 new entries: 50 each from 1906, 1938 and 1955. Seed 20261027.',
        'One random.Random generator samples sorted eligible source keys chronologically, without replacement.',
        'All known previous review samples are excluded, including the immediately preceding four-edition test.',
        'Population, exclusion and eligible counts: '+json.dumps({k:m[k] for k in ['population_sizes','excluded_retained_counts','eligible_sizes']}),'',
        'The same source-field adapter and v2 vocabulary from the prior diagnostic are frozen before this draw.',
        'No rule, alias, compound precedence, input handling or manual label is tuned to these new results.',
        'Original source fields are joined by edition:lineid; biographies join by source_key.',
        'Review uses original discipline fields and substantive research/professional text, without predictions or rule evidence.',
        'All 150 manual labels and literal evidence are frozen before classification.',
        'Reference standard: prediction-blinded Codex interpretation, without independent human expert validation.',
        'Physical includes engineering, mathematics, statistics, computing and Physical plus Biological mixtures.',
        'Biochemistry/biophysics alone remain Biological. Degrees/memberships/affiliations alone do not establish a field.',
        'Unclear references are retained and excluded from accuracy/precision/recall. Undefined quantities remain null.',
        'Wilson 95% intervals describe sampling uncertainty, not reference-label uncertainty.',
        'Equal allocation does not imply equal edition population weights. Raw pooled sample metrics are not population estimates.','',
        'Edition | Correct | Incorrect | Unassessable | Accuracy | Precision | Recall']
    for ed in EDITIONS:
        e=ev['per_edition'][ed]
        lines.append(f"{ed} | {e['correct']} | {e['incorrect']} | {e['unassessable']} | {fmt(e['accuracy'])} | {fmt(e['precision'])} | {fmt(e['recall'])}")
    lines+=['','Detailed confusion matrices (manual rows / predicted columns), intervals and unresolved rates:',json.dumps(ev,indent=2),'',
        'Every error, unassessable case and OCR-assisted match:']
    for r in flat:
        if r['source_key'] in by:lines += [f"{r['sample_id']}. {r['source_key']} {r['name_raw']}: manual={r['manual_physical_science']}, predicted={r['physical_science']}",
            'Evidence: '+r['manual_evidence'],by[r['source_key']]['explanation'],'']
    lines+=['Full-population classifications are not replaced. Existing sources, earlier samples and outputs are preserved.',
        'No subsequent tuning was performed. Small samples and class imbalance limit conclusions.',
        'Reproduce: early_editions_blind_audit.py prepare/review/add/freeze-manual/evaluate/report;',
        'export_early_editions_blind_audit.mjs; early_editions_blind_audit.py validate.',
        'Frozen manual labels and case notes are reproduction inputs, not generated from regex predictions.','']
    (out/'report.txt').write_text('\n'.join(lines),encoding='utf-8')
    dest=out/'code';dest.mkdir(exist_ok=True)
    for name in CODE:shutil.copyfile(Path(__file__).with_name(name),dest/name)


def validate(root,out,rscript):
    f=read(out/'rules_frozen.json');m=read(out/'sample_manifest.json');g=read(out/'manual_frozen.json')
    assert checks(root)==f['input_checksums_sha256']
    assert sha(out/'manual_labels.json')==g['manual_sha256'] and sha(out/'sample_records.json')==read(out/'sample_records_frozen.json')['sha256']
    assert f['frozen_at_utc']<m['sampled_at_utc']<g['frozen_at_utc']<read(out/'evaluation.json')['evaluated_at_utc']
    keys,eligible=draw(root);assert keys==m['sampled_keys_in_order'] and eligible==m['eligible_sizes'] and not set(keys)&excluded(root)
    sample=read(out/'sample_records.json');gold=read(out/'manual_labels.json');records=read(out/'comparisons.json');flat=read(out/'audit_records.json');export=list(rows(out/'amws_early_editions_blind_audit_150.csv'))
    assert len(keys)==len(set(keys))==len(sample)==len(gold)==len(records)==len(flat)==len(export)==150
    assert [s['source_key'] for s in sample]==keys and dict(collections.Counter(s['edition'] for s in sample))=={ed:50 for ed in EDITIONS}
    subprocess.run([rscript,str(Path(__file__).with_name('cross_edition_blind_sources.R')),str(root),str(out),'verify'],check=True)
    early={r['source_key']:r for r in read(out/'early_source_records.json')};bios={r['source_key']:r for r in rows(root/prior.source.BIOS) if r['source_key'] in set(keys)}
    assert set(early)==set(bios)==set(keys);model,adapter=load(out)
    for s,goldrow,r,a,c in zip(sample,gold,records,flat,export,strict=True):
        assert s==r['source'] and goldrow==r['manual'] and a['source_key']==c['source_key']==s['source_key']
        original=early[s['source_key']]
        assert s['field_raw']==original['field_raw'] and s['name_raw']==original['name_raw']
        assert s['source_provenance']=={k:original[k] for k in ['source_file','source_column','lineid']}
        assert s['raw_text_adjusted']==bios[s['source_key']]['raw_text_adjusted']
        assert s[goldrow['evidence_source']][goldrow['evidence_start']:goldrow['evidence_end']]==goldrow['evidence']
        pred=adapter.classify_verified_field(model,s['field_raw']);assert pred==r['prediction'];metric.check_evidence(pred,s)
        assert a['physical_science']==int(pred['physical_match']) and a['mixed']==int(pred['other_reason']=='mixed')
        assert a['unresolved']==int(pred['other_reason'] in ['missing','damaged','unmatched']) and not(a['mixed'] and a['unresolved'])
        assert all(c[k]==str(value) for k,value in a.items())
    ev=read(out/'evaluation.json')
    for ed in EDITIONS:assert ev['per_edition'][ed]==metric.count_metrics([r for r in records if r['source']['edition']==ed],'prediction')
    assert ev['raw_equal_allocation_sample']==metric.count_metrics(records,'prediction')
    for name,digest in f['classifier_sha256'].items():assert sha(out/'frozen_classifier'/name)==sha(Path(__file__).with_name(name))==digest
    assert f['classifier_sha256'][ADAPTER]==read(root/'output/amws'/prior.NAME/'field_adapter_frozen.json')['adapter_sha256']
    for name in CODE:assert sha(out/'code'/name)==sha(Path(__file__).with_name(name))
    assert read(out/'export_validation.json')['artifact_tool_cell_preservation_passed']
    write(out/'validation.json',dict(status='PASS',validated_at_utc=now(),sample_size=150,unique_per_edition=50,
        prior_review_overlap=0,adapter_and_vocabulary_frozen_before_sample=True,manual_labels_frozen_before_predictions=True,
        original_source_joins_and_literal_evidence_verified=True,metrics_recomputed=True,csv_cells_verified=True,
        earlier_releases_and_inputs_unchanged=True))
    write(out/'manifest.json',dict(status='complete',completed_at_utc=now(),seed=SEED,
        input_checksums_sha256=f['input_checksums_sha256'],output_checksums_sha256={str(p.relative_to(out)):sha(p) for p in out.rglob('*') if p.is_file() and p.name!='manifest.json' and '__pycache__' not in p.parts}))
    print('PASS: 150 new keys, pre-draw frozen adapter/rules, blind labels, source joins, evidence, metrics, CSV and preserved inputs.')


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('command',choices=['prepare','review','add','freeze-manual','evaluate','report','validate'])
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)));p.add_argument('--rscript',default='Rscript')
    p.add_argument('--start',type=int,default=1);p.add_argument('--end',type=int,default=150);p.add_argument('--full',action='store_true');p.add_argument('--labels',type=Path)
    a=p.parse_args();out=a.data_root/'output/amws'/NAME
    if a.command=='prepare':prepare(a.data_root,out,a.rscript)
    elif a.command=='review':review(out,a.start,a.end,a.full)
    elif a.command=='add':add(out,a.labels)
    elif a.command=='freeze-manual':freeze(out)
    elif a.command=='evaluate':evaluate(a.data_root,out)
    elif a.command=='report':report(out)
    elif a.command=='validate':validate(a.data_root,out,a.rscript)

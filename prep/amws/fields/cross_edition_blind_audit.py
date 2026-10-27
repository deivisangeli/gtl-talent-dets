"""Frozen v2 transport evaluation: 50 fresh manual references per AMWS edition."""
import argparse
import collections
import csv
import json
import os
from pathlib import Path
import random
import shutil
import subprocess
from pilot import DEFAULT_ROOT,now,read,rows,sha,write
import cross_edition_sample as source
import rules_v2_audit as metric
import rules_v2_audit_500 as latest

NAME='physical_science_cross_edition_blind_audit_50_20261023'
SEED=20261026
EDITIONS=list(source.SIZES)
csv.field_size_limit(16*1024*1024)


def exclusions(root):
    keys=latest.excluded(root)
    keys.update(r['source_key'] for r in read(root/'output/amws'/latest.NAME/'blind_sample.json'))
    return keys


def checks(root):
    paths=list(source.paths(root))
    for folder in [metric.NAME,latest.NAME,source.NAME,'field_regex_pilot_20261020',
                   'physical_science_accuracy_audit_20261020','field_of_work_sample_20261020',
                   'physical_science_dummy_20261020']:
        paths.extend(p for p in (root/'output/amws'/folder).rglob('*') if p.is_file() and '__pycache__' not in p.parts)
    extra=root/'output/amws/_tmp_agent_audit_v2/sample_manifest.csv'
    if extra.exists():paths.append(extra)
    paths.extend(Path(__file__).with_name(n) for n in metric.CLASS_FILES)
    return {str(p):sha(p) for p in sorted(set(paths))}


def draw(root):
    pools=collections.defaultdict(list)
    for r in rows(root/source.RELEASE):pools[r['source_key'].split(':')[0]].append(r['source_key'])
    assert {ed:len(v) for ed,v in pools.items()}==source.SIZES
    assert all(len(v)==len(set(v)) for v in pools.values())
    excluded=exclusions(root);rng=random.Random(SEED)
    eligible={ed:sorted(set(pools[ed])-excluded) for ed in EDITIONS}
    return [k for ed in EDITIONS for k in rng.sample(eligible[ed],50)],{ed:len(eligible[ed]) for ed in EDITIONS}


def prepare(root,out,rscript):
    if (out/'sample_records_frozen.json').exists():
        assert checks(root)==read(out/'rules_frozen.json')['input_checksums_sha256']
        assert sha(out/'sample_records.json')==read(out/'sample_records_frozen.json')['sha256']
        print('Existing frozen sample verified.');return
    frozen=read(root/'output/amws'/latest.NAME/'rules_frozen.json');before=checks(root)
    dest=out/'frozen_classifier';dest.mkdir(parents=True,exist_ok=True)
    for name,digest in frozen['classifier_sha256'].items():
        p=root/'output/amws'/latest.NAME/'frozen_classifier'/name
        assert sha(p)==sha(Path(__file__).with_name(name))==digest
        shutil.copyfile(p,dest/name)
    write(out/'rules_frozen.json',dict(frozen_at_utc=now(),classifier_sha256=frozen['classifier_sha256'],
        input_checksums_sha256=before,version=frozen['version'],changes_since_previous_audit=False),True)
    keys,eligible=draw(root);assert len(set(keys))==200 and not set(keys)&exclusions(root)
    write(out/'sample_manifest.json',dict(sampled_at_utc=now(),seed=SEED,sample_size=200,sample_per_edition=50,
        sampled_keys_in_order=keys,population_sizes=source.SIZES,eligible_sizes=eligible,
        excluded_retained_counts={ed:source.SIZES[ed]-eligible[ed] for ed in EDITIONS},
        exclusion_keys=sorted(exclusions(root)),
        method='One random.Random(20261026); sample(sorted(eligible_keys),50) per edition in chronological order.'),True)
    subprocess.run([rscript,str(Path(__file__).with_name('cross_edition_blind_sources.R')),str(root),str(out)],check=True)
    early={r['source_key']:r for r in read(out/'early_source_records.json')}
    wanted=set(keys);bios={r['source_key']:r for r in rows(root/source.BIOS) if r['source_key'] in wanted}
    old={r['source_key']:r for r in rows(root/source.OLD/'amws_ed16_physical_science_audit.csv') if r['source_key'] in wanted}
    assert len(early)==150 and set(bios)==wanted and set(old)=={k for k in keys if k.startswith('1986:')}
    sample=[]
    for i,k in enumerate(keys,1):
        ed=k.split(':')[0];s=old[k] if ed=='1986' else early[k];b=bios[k]
        provenance=dict(source_file=str(root/source.OLD/'amws_ed16_physical_science_audit.csv'),source_column='field_raw',source_references_json=s['source_references_json']) if ed=='1986' else {c:s[c] for c in ['source_file','source_column','lineid']}
        sample.append(dict(sample_id=i,source_key=k,edition=ed,name_raw=s['name_raw'],field_raw=s['field_raw'],
            raw_text_adjusted=b['raw_text_adjusted'],biography_source_file=b['source_file'],
            biography_source_column=b['source_column'],source_provenance=provenance))
    write(out/'sample_records.json',sample,True)
    write(out/'sample_records_frozen.json',dict(frozen_at_utc=now(),sha256=sha(out/'sample_records.json')),True)
    assert checks(root)==before
    print('Frozen 200 new entries, 50 per edition. Eligible pools:',eligible)


def review(out,start,end,full):
    for r in read(out/'sample_records.json'):
        if start<=r['sample_id']<=end:
            print(f"[{r['sample_id']}] {r['source_key']} {r['name_raw']}")
            if r['edition']!='1986':print('ORIGINAL SOURCE FIELD: '+r['field_raw'])
            text=r['raw_text_adjusted']
            print('BIO: '+(text if full else text.split('Educ')[0][:500]))


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
    print(f'{len(gold)}/200 blind labels saved.')


def freeze(out):
    sample=read(out/'sample_records.json');gold=read(out/'manual_labels.json')
    assert len(gold)==200 and [r['source_key'] for r in gold]==[r['source_key'] for r in sample]
    for s,g in zip(sample,gold):assert s[g['evidence_source']][g['evidence_start']:g['evidence_end']]==g['evidence']
    write(out/'manual_frozen.json',dict(frozen_at_utc=now(),manual_sha256=sha(out/'manual_labels.json'),
        labels_recorded_before_predictions=True,benchmark='Prediction-blinded Codex manual review; not independent human expert validation.'),True)
    print('All 200 manual labels frozen before evaluating either classifier.')


def evaluate(root,out):
    f=read(out/'rules_frozen.json');g=read(out/'manual_frozen.json')
    assert checks(root)==f['input_checksums_sha256'] and sha(out/'manual_labels.json')==g['manual_sha256']
    for name,digest in f['classifier_sha256'].items():assert sha(out/'frozen_classifier'/name)==digest
    base,v2=metric.load(out);records=[];flat=[]
    for s,gold in zip(read(out/'sample_records.json'),read(out/'manual_labels.json'),strict=True):
        assert s['source_key']==gold['source_key']
        b=base.classify_entry(s['field_raw'],s['raw_text_adjusted'],s['name_raw'])
        v=v2.classify_entry(s['field_raw'],s['raw_text_adjusted'],s['name_raw'])
        metric.check_evidence(b,s);metric.check_evidence(v,s)
        records.append(dict(source=s,manual=gold,baseline=b,revised=v))
        outcome=lambda p:'unassessable' if gold['physical_science']=='unclear' else 'correct' if int(p['physical_match'])==gold['physical_science'] else 'incorrect'
        a=dict(sample_id=s['sample_id'],source_key=s['source_key'],edition=s['edition'],name_raw=s['name_raw'],
            manual_physical_science=gold['physical_science'],manual_evidence_source=gold['evidence_source'],
            manual_evidence=gold['evidence'],manual_evidence_start=gold['evidence_start'],manual_evidence_end=gold['evidence_end'],
            manual_reasoning=gold['reasoning'],manual_confidence=gold['confidence'])
        for model,pred in [('baseline',b),('revised',v)]:
            a.update({model+'_physical_science':int(pred['physical_match']),model+'_outcome':outcome(pred),
                model+'_mixed':int(pred['other_reason']=='mixed'),model+'_unresolved':int(pred['other_reason'] in ['missing','damaged','unmatched']),
                model+'_input_source':pred['input_source'],model+'_reason':pred['other_reason'],model+'_heading':pred['selected_heading'],
                model+'_rule_ids':';'.join(pred['matched_rule_ids']),model+'_evidence_json':json.dumps(pred['evidence'],ensure_ascii=False)})
        a.update(prediction_changed=int(b['physical_match']!=v['physical_match']),ocr_assisted=int(v['ocr_assisted']),
            field_raw=s['field_raw'],source_provenance_json=json.dumps(s['source_provenance'],ensure_ascii=False),
            raw_text_adjusted=s['raw_text_adjusted'])
        flat.append(a)
    ev=dict(evaluated_at_utc=now(),per_edition={ed:{model:metric.count_metrics([r for r in records if r['source']['edition']==ed],model) for model in ['baseline','revised']} for ed in EDITIONS},
        raw_equal_allocation_sample={model:metric.count_metrics(records,model) for model in ['baseline','revised']},
        overall_warning='Raw equally allocated sample statistics are not pooled-population estimates.',
        holdout_tuning=False,reference_standard=read(out/'manual_frozen.json')['benchmark'])
    write(out/'comparisons.json',records);write(out/'audit_records.json',flat);write(out/'evaluation.json',ev)
    print(json.dumps(ev['per_edition'],indent=2))


def report(out):
    ev=read(out/'evaluation.json');flat=read(out/'audit_records.json');m=read(out/'sample_manifest.json')
    notes=read(out/'case_notes.json');by={r['source_key']:r for r in notes}
    required={r['source_key'] for r in flat if r['baseline_outcome']!='correct' or r['revised_outcome']!='correct' or r['prediction_changed'] or r['ocr_assisted']}
    assert set(by)==required
    for r in flat:r['case_explanation']=by.get(r['source_key'],{}).get('explanation','')
    write(out/'audit_records.json',flat);cols=list(flat[0]);write(out/'audit_payload.json',dict(columns=cols,rows=[[r[c] for c in cols] for r in flat]))
    lines=['AMWS: 50 NEW BLIND REFERENCES PER EDITION','',
        '200 entries: 50 from each of 1906, 1938, 1955 and 1986. Seed 20261026.',
        'Latest v06 retained release. Previous known review keys excluded.',
        'One Python random.Random generator samples sorted eligible keys in chronological edition order.',
        'The same frozen v2 rules were applied to all editions, without adaptation or tuning.',
        'This tests transport to original earlier-edition source fields; heading fallback remains edition-16 logic.',
        'Original guarded predictions are also reported as a paired comparison.',
        'Earlier editions use exact edition:lineid joins to original input field columns.',
        '1986 reuses its exact-key recovered field audit. All biographies join by source_key.',
        'Manual evidence uses original source fields and substantive professional/research text.',
        'No predictions, rule matches or classifier evidence displayed before all manual labels were frozen.',
        'Physical includes engineering, mathematics, statistics, computing and Physical+Biological work.',
        'Biochemistry/biophysics alone remain Biological. Degrees or memberships alone do not establish a field.',
        'Reference: prediction-blinded Codex manual interpretation, without independent human expert validation.',
        'Unclear references are retained and excluded from precision, recall and accuracy.',
        'Wilson 95% intervals describe sampling uncertainty, not reference-label uncertainty.',
        'Small samples and class imbalance limit conclusions. Undefined metrics remain null.',
        'Equal sample sizes do not imply equal edition population weights. Raw pooled sample results',
        'are descriptive of the 200-entry sample and are not pooled-population estimates.','',
        'Population and eligible sizes: '+json.dumps({k:m[k] for k in ['population_sizes','eligible_sizes','excluded_retained_counts']}),'',
        'Edition   Correct   Incorrect   Unassessable   Precision       Recall          Accuracy (v2)']
    fmt=lambda p:'undefined' if p['value'] is None else f"{p['numerator']}/{p['denominator']}={p['value']:.2%}"
    for ed in EDITIONS:
        e=ev['per_edition'][ed]['revised']
        lines.append(f"{ed}      {e['correct']:3d}        {e['incorrect']:3d}           {e['unassessable']:3d}       {fmt(e['precision']):18s} {fmt(e['recall']):18s} {fmt(e['accuracy'])}")
    diag=read(out/'field_diagnostic_metrics.json')
    lines+=['','FORMAT DIAGNOSTIC (POST-HOC; NOT AN INDEPENDENT HELD-OUT CLAIM)',
        'The original transport run rejected almost all earlier fields because it requires',
        'support in an edition-16-style biography heading. 1906/1938 biographies lack that',
        'layout; 1955 places address/discipline before birth information. Some original',
        'earlier-edition positives accidentally use body/address words. Those pipeline',
        'numbers diagnose failed transport, not successful use of verified source fields.',
        'After seeing this failure, an adapter was frozen that isolates the exact-key',
        'original earlier-edition discipline column. It calls the same frozen v2 API',
        'with an education section terminator; no keyword, exception or OCR rule changes.',
        'All evidence remains literal text from field_raw. It does not search biography',
        'bodies. 1986 is unchanged. Labels remain frozen from before the first predictions.',
        'Because this adjustment followed exposure, report these earlier-edition results',
        'as a post-hoc format diagnostic, requiring a separate fresh test for confirmation.','',
        'Edition   Correct   Incorrect   Precision        Recall           Accuracy (field diagnostic)']
    for ed in EDITIONS:
        e=diag['per_edition'][ed]
        lines.append(f"{ed}      {e['correct']:3d}        {e['incorrect']:3d}        {fmt(e['precision']):18s} {fmt(e['recall']):18s} {fmt(e['accuracy'])}")
    lines+=['','Diagnostic confusion matrices/intervals/unresolved rates:',json.dumps(diag,indent=2),'']
    lines+=['','Detailed per-edition confusion matrices, denominators, Wilson intervals and unresolved rates:',json.dumps(ev,indent=2),'',
        'All errors, unassessable cases, changes and OCR-assisted matches:']
    for r in flat:
        if r['source_key'] in by:lines += [f"{r['sample_id']}. {r['source_key']} {r['name_raw']}: manual={r['manual_physical_science']}, original={r['baseline_physical_science']}, v2={r['revised_physical_science']}",
            'Evidence: '+r['manual_evidence'],by[r['source_key']]['explanation'],'']
    lines+=['No labels or rules were tuned after comparison. Existing releases and the population dummy are preserved.',
        'Evidence offsets: zero-based Unicode code points, exclusive end, into the named source.',
        'Reproduce: cross_edition_blind_audit.py prepare/review/add/freeze-manual/evaluate/diagnose-fields/report;',
        'export_cross_edition_blind_audit.mjs; cross_edition_blind_audit.py validate.',
        'Frozen manual labels and case notes are required reproduction inputs.','']
    (out/'report.txt').write_text('\n'.join(lines),encoding='utf-8')
    code=out/'code';code.mkdir(exist_ok=True)
    for name in ['cross_edition_blind_audit.py','cross_edition_blind_sources.R','cross_edition_sample.py',
                 'cross_edition_field_adapter.py','rules_v2_audit.py','rules_v2_audit_500.py','pilot.py','export_cross_edition_blind_audit.mjs','CROSS_EDITION_BLIND_AUDIT.md']:
        shutil.copyfile(Path(__file__).with_name(name),code/name)


def diagnose(out):
    from cross_edition_field_adapter import classify_verified_field
    ev=read(out/'evaluation.json');f=read(out/'manual_frozen.json')
    assert sha(out/'manual_labels.json')==f['manual_sha256']
    adapter=Path(__file__).with_name('cross_edition_field_adapter.py')
    if not (out/'field_adapter_frozen.json').exists():
        write(out/'field_adapter_frozen.json',dict(frozen_at_utc=now(),adapter_sha256=sha(adapter),
            original_evaluation_sha256=sha(out/'evaluation.json'),posthoc=True,
            reason='Original ed16 heading-support check rejected earlier-edition layouts. Diagnostic trusts exact-key original discipline columns without changing vocabulary.'),True)
    frozen=read(out/'field_adapter_frozen.json');assert sha(adapter)==frozen['adapter_sha256']
    assert sha(out/'evaluation.json')==frozen['original_evaluation_sha256']
    _,v2=metric.load(out);records=read(out/'comparisons.json');flat=read(out/'audit_records.json');diagnostic=[]
    for r,a in zip(records,flat,strict=True):
        s=r['source'];p=classify_verified_field(v2,s['field_raw']) if s['edition']!='1986' else r['revised']
        metric.check_evidence(p,s)
        diagnostic.append(dict(source=s,manual=r['manual'],diagnostic=p))
        label=r['manual']['physical_science'];outcome='unassessable' if label=='unclear' else 'correct' if int(p['physical_match'])==label else 'incorrect'
        a.update(field_diagnostic_physical_science=int(p['physical_match']),field_diagnostic_outcome=outcome,
            field_diagnostic_mixed=int(p['other_reason']=='mixed'),
            field_diagnostic_unresolved=int(p['other_reason'] in ['missing','damaged','unmatched']),
            field_diagnostic_input_source=p['input_source'],field_diagnostic_reason=p['other_reason'],
            field_diagnostic_rule_ids=';'.join(p['matched_rule_ids']),field_diagnostic_evidence_json=json.dumps(p['evidence'],ensure_ascii=False))
    result=dict(evaluated_at_utc=now(),posthoc_format_diagnostic=True,classifier_vocabulary_unchanged=True,
        independent_holdout_claim=False,per_edition={ed:metric.count_metrics([r for r in diagnostic if r['source']['edition']==ed],'diagnostic') for ed in EDITIONS})
    write(out/'field_diagnostic_records.json',diagnostic);write(out/'field_diagnostic_metrics.json',result);write(out/'audit_records.json',flat)
    print(json.dumps({ed:{k:result['per_edition'][ed][k] for k in ['correct','incorrect','precision','recall']} for ed in EDITIONS},indent=2))


def validate(root,out,rscript):
    f=read(out/'rules_frozen.json');m=read(out/'sample_manifest.json');g=read(out/'manual_frozen.json')
    assert checks(root)==f['input_checksums_sha256'] and sha(out/'manual_labels.json')==g['manual_sha256']
    assert sha(out/'sample_records.json')==read(out/'sample_records_frozen.json')['sha256']
    assert f['frozen_at_utc']<m['sampled_at_utc']<g['frozen_at_utc']<read(out/'evaluation.json')['evaluated_at_utc']
    keys,eligible=draw(root);assert keys==m['sampled_keys_in_order'] and eligible==m['eligible_sizes']
    sample=read(out/'sample_records.json');records=read(out/'comparisons.json');flat=read(out/'audit_records.json');export=list(rows(out/'amws_cross_edition_blind_audit_200.csv'))
    assert len(keys)==len(set(keys))==len(sample)==len(records)==len(flat)==len(export)==200
    assert [r['source_key'] for r in sample]==keys and not set(keys)&exclusions(root)
    assert dict(collections.Counter(r['edition'] for r in sample))=={ed:50 for ed in EDITIONS}
    subprocess.run([rscript,str(Path(__file__).with_name('cross_edition_blind_sources.R')),str(root),str(out),'verify'],check=True)
    bios={r['source_key']:r for r in rows(root/source.BIOS) if r['source_key'] in set(keys)}
    early={r['source_key']:r for r in read(out/'early_source_records.json')}
    old={r['source_key']:r for r in rows(root/source.OLD/'amws_ed16_physical_science_audit.csv') if r['source_key'] in set(keys)}
    b,v=metric.load(out)
    for s,r,a,c in zip(sample,records,flat,export,strict=True):
        assert s==r['source'] and a['source_key']==c['source_key']==s['source_key']
        assert s['raw_text_adjusted']==bios[s['source_key']]['raw_text_adjusted']
        original=old[s['source_key']] if s['edition']=='1986' else early[s['source_key']]
        assert s['field_raw']==original['field_raw'] and s['name_raw']==original['name_raw']
        manual=r['manual'];assert s[manual['evidence_source']][manual['evidence_start']:manual['evidence_end']]==manual['evidence']
        assert manual==next(gold for gold in read(out/'manual_labels.json') if gold['source_key']==s['source_key'])
        for model,mod in [('baseline',b),('revised',v)]:
            assert r[model]==mod.classify_entry(s['field_raw'],s['raw_text_adjusted'],s['name_raw']);metric.check_evidence(r[model],s)
            assert a[model+'_physical_science']==int(r[model]['physical_match'])
            assert a[model+'_mixed']==int(r[model]['other_reason']=='mixed')
            assert a[model+'_unresolved']==int(r[model]['other_reason'] in ['missing','damaged','unmatched'])
            assert not (a[model+'_mixed'] and a[model+'_unresolved'])
        assert all(c[k]==str(value) for k,value in a.items())
    ev=read(out/'evaluation.json')
    for ed in EDITIONS:
        rs=[r for r in records if r['source']['edition']==ed]
        for model in ['baseline','revised']:assert ev['per_edition'][ed][model]==metric.count_metrics(rs,model)
    from cross_edition_field_adapter import classify_verified_field
    diag=read(out/'field_diagnostic_records.json');dm=read(out/'field_diagnostic_metrics.json');df=read(out/'field_adapter_frozen.json')
    assert df['posthoc'] and not dm['independent_holdout_claim']
    assert ev['evaluated_at_utc']<df['frozen_at_utc']<dm['evaluated_at_utc']
    assert sha(out/'evaluation.json')==df['original_evaluation_sha256']
    assert sha(Path(__file__).with_name('cross_edition_field_adapter.py'))==df['adapter_sha256']
    for r,d,a in zip(records,diag,flat,strict=True):
        s=r['source'];assert d['source']==s and d['manual']==r['manual']
        expected=classify_verified_field(v,s['field_raw']) if s['edition']!='1986' else r['revised']
        assert d['diagnostic']==expected;metric.check_evidence(expected,s)
        assert a['field_diagnostic_physical_science']==int(expected['physical_match'])
    for ed in EDITIONS:assert dm['per_edition'][ed]==metric.count_metrics([d for d in diag if d['source']['edition']==ed],'diagnostic')
    for name,digest in f['classifier_sha256'].items():assert sha(out/'frozen_classifier'/name)==sha(Path(__file__).with_name(name))==digest
    for p in (out/'code').iterdir():assert sha(p)==sha(Path(__file__).with_name(p.name))
    assert read(out/'export_validation.json')['artifact_tool_cell_preservation_passed']
    write(out/'validation.json',dict(status='PASS',validated_at_utc=now(),unique_sample_keys=200,
        unique_keys_per_edition=50,prior_review_overlap=0,source_joins_and_literal_offsets_verified=True,
        frozen_labels_and_rules_verified=True,per_edition_metrics_recomputed=True,csv_cells_verified=True,
        original_sources_and_releases_unchanged=True))
    write(out/'manifest.json',dict(status='complete',completed_at_utc=now(),seed=SEED,
        input_checksums_sha256=f['input_checksums_sha256'],output_checksums_sha256={str(p.relative_to(out)):sha(p) for p in out.rglob('*') if p.is_file() and p.name!='manifest.json' and '__pycache__' not in p.parts}))
    print('PASS: 50 fresh keys per edition, blind freeze, source/evidence, metrics, CSV and preserved checksums.')


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('command',choices=['prepare','review','add','freeze-manual','evaluate','diagnose-fields','report','validate'])
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--rscript',default='Rscript');p.add_argument('--start',type=int,default=1);p.add_argument('--end',type=int,default=200)
    p.add_argument('--full',action='store_true');p.add_argument('--labels',type=Path)
    a=p.parse_args();out=a.data_root/'output/amws'/NAME
    if a.command=='prepare':prepare(a.data_root,out,a.rscript)
    elif a.command=='review':review(out,a.start,a.end,a.full)
    elif a.command=='add':add(out,a.labels)
    elif a.command=='freeze-manual':freeze(out)
    elif a.command=='evaluate':evaluate(a.data_root,out)
    elif a.command=='diagnose-fields':diagnose(out)
    elif a.command=='report':report(out)
    elif a.command=='validate':validate(a.data_root,out,a.rscript)

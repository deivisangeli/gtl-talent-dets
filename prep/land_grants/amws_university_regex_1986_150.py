"""Fresh, manually referenced 1986 university employment development exercise."""
import argparse
import collections
import csv
from datetime import datetime,timezone
import importlib.util
import json
from pathlib import Path
import random
import subprocess
import sys

import amws_name_cleaning as base
import amws_name_regex_calibration as names
import amws_workplace_feasibility as workplace
import amws_university_affiliation_calibration as old
import amws_university_regex_150 as previous

RELEASE='amws_university_regex_1986_150_20261023'
FIELDS=['sample_id','partition','biography_fingerprint','person_fingerprint']+[f for f in workplace.SAMPLE_FIELDS if f!='sample_id']+['original_fields_json']

def dest(root=None):return workplace.root_path(root)/'output/land_grants'/RELEASE
def sample(root=None):return base.read_csv(dest(root)/'sample_150.csv')
def refs(root=None):return previous.read_jsonl(dest(root)/'manual_reviews.jsonl')

def prior_files(root=None):
    return [workplace.destination(root)/'sample_80.csv',old.destination(root)/'sample_1000.csv',
        previous.dest(root)/'sample_150.csv',base.locations(root)[2]/'sample_500.csv',names.destination(root)/'sample_500.csv']

def source_frame(root=None):
    frame=old.source_frame(root)
    assert sum(r['edition']=='1986' for r in frame)==79073
    raw={}
    with workplace.raw_paths(root)['1986'].open(encoding='utf-8-sig',newline='') as f:
        for r in csv.DictReader(f):
            k='1986:'+str(int(r['entry_id']))
            if k in raw:raise ValueError('Duplicate original entry ID')
            raw[k]=r
    for r in frame:
        if r['edition']=='1986':r['original_fields_json']=json.dumps(raw[r['source_key']],ensure_ascii=False)
    return frame

def draw(frame,root=None):
    sources={}
    for p in prior_files(root):
        for r in base.read_csv(p):sources.setdefault(r['source_key'],[]).append(str(p))
    prior=[r for r in frame if r['source_key'] in sources]
    assert len(prior)==len(sources),'Previously reviewed ID missing from v06'
    bios={r['biography_fingerprint'] for r in prior}-{''};people={r['person_fingerprint'] for r in prior}-{''}
    pool=sorted((r for r in frame if r['edition']=='1986' and r['source_key'] not in sources
        and (not r['biography_fingerprint'] or r['biography_fingerprint'] not in bios)
        and (not r['person_fingerprint'] or r['person_fingerprint'] not in people)),key=lambda r:r['source_key'])
    rows=[dict(sample_id=str(i),partition='development',**r) for i,r in enumerate(random.Random(53).sample(pool,150),1)]
    exclusions=[dict(source_key=r['source_key'],edition=r['edition'],biography_fingerprint=r['biography_fingerprint'],person_fingerprint=r['person_fingerprint'],review_sources=json.dumps(sources[r['source_key']])) for r in prior]
    return rows,exclusions,len(pool)

def protections(root=None):
    checks=previous.protections(root)
    folder=previous.dest(root);checks[str(folder/'completion_manifest.json')]=base.digest(folder/'completion_manifest.json')
    for name,value in old.read_json(folder/'completion_manifest.json')['output_checksums'].items():checks[str(folder/name)]=value
    for name in ['amws_university_regex_150.py','amws_university_150_engine.py','amws_university_150_manual_references.py','read_amws_university_150_sources.R']:
        p=Path(__file__).with_name(name).resolve();checks[str(p)]=base.digest(p)
    return checks

def prepare(root=None):
    d=dest(root)
    if d.exists():raise ValueError('Release exists; refuse to resample')
    checks=protections(root);workplace.verify_checksums(checks)
    rows,exclusions,pool=draw(source_frame(root),root)
    d.mkdir(parents=True)
    base.write_csv(d/'sample_150.csv',rows,FIELDS)
    base.write_csv(d/'prior_review_exclusions.csv',exclusions,['source_key','edition','biography_fingerprint','person_fingerprint','review_sources'])
    (d/'manual_reviews.jsonl').touch()
    base.write_json(d/'sample_manifest.json',dict(seed=53,population_1986=79073,sample_size=150,eligible_pool=pool,
        prior_unique_reviewed_ids=len(exclusions),prior_1986_ids=sum(r['edition']=='1986' for r in exclusions),
        selection='random.Random(53).sample(source_key-sorted eligible 1986 records,150), without replacement; no name or biography quality filters',
        duplicate_detection='Nonempty NFKC/casefold/whitespace-normalized complete original biography and exact full v06 name/birth-year signature. All fingerprints reconstructed from the same original-source frame. Conservative, not complete person linkage.',
        prior_samples={str(p):base.digest(p) for p in prior_files(root)},input_checksums=checks,
        sample_sha256=base.digest(d/'sample_150.csv'),exclusions_sha256=base.digest(d/'prior_review_exclusions.csv'),
        prepared_at_utc=datetime.now(timezone.utc).isoformat()))
    return d

def show(start,end,root=None):
    for r in sample(root):
        if start<=int(r['sample_id'])<=end:print(f"\n[{r['sample_id']}] {r['source_key']} NAME: {r['raw_name']}\nFULL ORIGINAL BIOGRAPHY: {r['biography_text']}")

class ManualBatch(old.ManualBatch):
    def __init__(self,root=None):self.root=root;self.rows={int(r['sample_id']):r for r in sample(root)}
    def block(self,*args,qualifiers=(),**kwargs):
        value=super().block(*args,**kwargs);value['qualifiers']=[self.ev(args[0],q) for q in qualifiers];return value
    def affiliation(self,*args,qualifiers=(),**kwargs):
        value=super().affiliation(*args,**kwargs);value['qualifiers']=[self.ev(args[0],q) for q in qualifiers];return value
    def entry(self,i,*args,addresses=(),**kwargs):
        value=super().entry(i,*args,**kwargs)
        value['reported_address_affiliations']=[self.ev(i,q) for q in addresses];return value
    def save(self,items):
        d=dest(self.root)
        if (d/'references_frozen.json').exists():raise ValueError('Manual references frozen before predictions')
        seen={r['sample_id'] for r in refs(self.root)}
        for r in items:
            if r['sample_id'] in seen:raise ValueError('Duplicate manual review')
            old.check_review(r,self.rows[int(r['sample_id'])]);previous.validate_evidence(self.rows[int(r['sample_id'])],r);seen.add(r['sample_id'])
        with (d/'manual_reviews.jsonl').open('a',encoding='utf-8',newline='\n') as f:
            for r in items:f.write(json.dumps(r,ensure_ascii=False)+'\n')
        return len(seen)

def freeze_references(root=None):
    d=dest(root);rows=sample(root);reviews=refs(root)
    assert len(reviews)==150 and {r['sample_id'] for r in rows}=={r['sample_id'] for r in reviews}
    for r in reviews:
        row=next(x for x in rows if x['sample_id']==r['sample_id']);old.check_review(r,row);previous.validate_evidence(row,r)
    if (d/'references_frozen.json').exists():raise ValueError('References already frozen')
    base.write_json(d/'references_frozen.json',dict(references_sha256=base.digest(d/'manual_reviews.jsonl'),sample_sha256=base.digest(d/'sample_150.csv'),
        completed_full_entry_reviews=150,predictions_displayed=False,frozen_at_utc=datetime.now(timezone.utc).isoformat()))
    return d/'references_frozen.json'

def evaluate(rows,refs,version):
    from amws_university_1986_150_engine import extract_universities,norm
    references={r['sample_id']:r for r in refs};predictions=[];errors=[];counts={}
    def counter(ed,family='all'):
        return counts.setdefault((ed,family),collections.Counter())
    for row in rows:
        ref=references[row['sample_id']];groups=old.reference_groups(ref)
        result=extract_universities(row,row['edition'],version)
        predictions.append(dict(**{k:row[k] for k in ('sample_id','source_key','edition','partition')},version=version,**result))
        ed=row['edition'];total=counter(ed);total['entries']+=1;total['reference_institutions']+=len(groups)
        total['unresolved_entries']+=bool(ref['unresolved_evidence']);total['flagged_entries']+=bool(result['review_flags'])
        total['negative_entries']+=ref['decision']=='negative'
        unmatched=set(range(len(groups)));seen=set()
        for pred in result['affiliations']:
            fam=counter(ed,pred['rule_family']);key=norm(pred['institution']['text'])
            duplicate=key in seen;seen.add(key)
            for ct in (total,fam):ct['predicted_institutions']+=1;ct['duplicates']+=duplicate
            candidates=[i for i in sorted(unmatched) if key in groups[i]['forms']]
            if candidates and not duplicate:
                j=candidates[0];unmatched.remove(j);group=groups[j]
                for ct in (total,fam):
                    ct['true_positive']+=1;ct['temporal_denominator']+=1
                    ct['temporal_correct']+=pred['temporal_status']==old.expected_temporal(group)
                for field in ('roles','dates'):
                    gold={norm(ev['text']) for a in group['blocks'] for ev in a[field]}
                    proposed={norm(ev['text']) for ev in pred[field]}
                    for ct in (total,fam):
                        ct[field+'_reference']+=len(gold);ct[field+'_predicted']+=len(proposed);ct[field+'_matched']+=len(gold&proposed)
                if pred['temporal_status']!=old.expected_temporal(group):
                    errors.append(dict(sample_id=row['sample_id'],source_key=row['source_key'],edition=ed,partition=row['partition'],kind='temporal_mismatch',rule_family=pred['rule_family'],prediction=pred,reference=group['blocks']))
            else:
                for ct in (total,fam):ct['false_positive']+=1;ct['predictions_on_unresolved']+=bool(ref['unresolved_evidence'])
                errors.append(dict(sample_id=row['sample_id'],source_key=row['source_key'],edition=ed,partition=row['partition'],kind='duplicate_prediction' if duplicate else 'unsupported_or_literal_mismatch',rule_family=pred['rule_family'],prediction=pred,reference=ref))
        for j in sorted(unmatched):
            total['false_negative']+=1
            errors.append(dict(sample_id=row['sample_id'],source_key=row['source_key'],edition=ed,partition=row['partition'],kind='missed_reference_institution',rule_family='',prediction=None,reference=groups[j]['blocks']))
    metrics=[]
    for (ed,family),ct in sorted(counts.items()):
        tp=ct['true_positive'];fp=ct['false_positive'];fn=ct['false_negative']
        precision=tp/(tp+fp) if tp+fp else None
        recall=tp/(tp+fn) if family=='all' and tp+fn else None
        metrics.append(dict(edition=ed,rule_family=family,**dict(ct),precision=precision,recall=recall,
                            f1=2*precision*recall/(precision+recall) if precision is not None and recall is not None and precision+recall else 0 if family=='all' else None,
                            temporal_accuracy=ct['temporal_correct']/ct['temporal_denominator'] if ct['temporal_denominator'] else None,
                            precision_target_met=precision>=.95 if precision is not None else False))
    return dict(version=version,metrics=metrics,predictions=predictions,errors=errors)




def develop(root=None):
    import amws_university_1986_150_engine as engine
    d=dest(root);frozen=old.read_json(d/'references_frozen.json')
    assert base.digest(d/'manual_reviews.jsonl')==frozen['references_sha256']
    assert base.digest(d/'sample_150.csv')==frozen['sample_sha256']
    if (d/'completion_manifest.json').exists():raise ValueError('Completed release immutable')
    for version in engine.VERSIONS:
        folder=d/'development_versions'/version
        if folder.exists():continue
        old.save_evaluation(folder,evaluate(sample(root),refs(root),version))
        (folder/'engine.py').write_bytes(Path(engine.__file__).read_bytes())
        base.write_json(folder/'rule_specification.json',dict(**engine.rule_specification(),selected_version=version,
            engine_sha256=base.digest(Path(engine.__file__)),references_sha256=frozen['references_sha256'],
            evaluated_at_utc=datetime.now(timezone.utc).isoformat(),partition='development'))
    return d/'development_versions'

def validate(root=None):
    import amws_university_1986_150_engine as engine
    d=dest(root);manifest=old.read_json(d/'sample_manifest.json');frozen=old.read_json(d/'references_frozen.json')
    workplace.verify_checksums(manifest['input_checksums'])
    for path,value in manifest['prior_samples'].items():assert base.digest(Path(path))==value
    assert base.digest(d/'sample_150.csv')==manifest['sample_sha256']==frozen['sample_sha256']
    assert base.digest(d/'prior_review_exclusions.csv')==manifest['exclusions_sha256']
    assert base.digest(d/'manual_reviews.jsonl')==frozen['references_sha256']
    rows=sample(root);reviews=refs(root);by={r['source_key']:r for r in rows}
    assert len(rows)==len(by)==len(reviews)==150
    assert {r['edition'] for r in rows}=={'1986'} and {r['partition'] for r in rows}=={'development'}
    assert [r['sample_id'] for r in rows]==list(map(str,range(1,151)))
    assert {r['sample_id'] for r in rows}=={r['sample_id'] for r in reviews}
    for r in reviews:old.check_review(r,by[r['source_key']]);previous.validate_evidence(by[r['source_key']],r)
    frame=source_frame(root);redrawn,exclusions,pool=draw(frame,root)
    assert rows==[{k:r.get(k,'') for k in FIELDS} for r in redrawn],'Frozen original values or sampling differ'
    assert exclusions==base.read_csv(d/'prior_review_exclusions.csv')
    assert pool==manifest['eligible_pool']
    prior_ids={r['source_key'] for r in exclusions};bios={r['biography_fingerprint'] for r in exclusions}-{''};people={r['person_fingerprint'] for r in exclusions}-{''}
    assert not set(by)&prior_ids
    assert not ({r['biography_fingerprint'] for r in rows}-{''})&bios
    assert not ({r['person_fingerprint'] for r in rows}-{''})&people
    duplicate_exclusions=[]
    for r in frame:
        if r['edition']!='1986' or r['source_key'] in prior_ids:continue
        reasons=[]
        if r['biography_fingerprint'] and r['biography_fingerprint'] in bios:reasons.append('normalized_original_biography')
        if r['person_fingerprint'] and r['person_fingerprint'] in people:reasons.append('normalized_full_name_birth_year')
        if reasons:duplicate_exclusions.append(dict(source_key=r['source_key'],reason=' | '.join(reasons),
            matching_prior_keys=json.dumps([p['source_key'] for p in exclusions if
                r['biography_fingerprint'] and r['biography_fingerprint']==p['biography_fingerprint'] or
                r['person_fingerprint'] and r['person_fingerprint']==p['person_fingerprint']])))
    versions=[]
    for folder in sorted((d/'development_versions').iterdir()):
        if not folder.is_dir():continue
        spec=old.read_json(folder/'rule_specification.json')
        assert base.digest(folder/'engine.py')==spec['engine_sha256'] and spec['references_sha256']==frozen['references_sha256']
        module_spec=importlib.util.spec_from_file_location('snapshot_1986_'+folder.name,folder/'engine.py')
        module=importlib.util.module_from_spec(module_spec);module_spec.loader.exec_module(module)
        predictions=previous.read_jsonl(folder/'predictions.jsonl');assert len(predictions)==150
        for p in predictions:
            row=by[p['source_key']];previous.validate_evidence(row,p)
            result=module.extract_universities(row,'1986',folder.name)
            assert result=={k:p[k] for k in result},'Saved result differs from exact engine snapshot'
            assert result==module.extract_universities(row,'1986',folder.name),'Nondeterministic extraction'
            keys=[engine.norm(a['institution']['text']) for a in result['affiliations']]
            assert len(keys)==len(set(keys)),'Duplicate institution scoring'
        versions.append(folder.name)
    result=dict(v06_records=len(frame),v06_1986_population=79073,sample_entries=150,unique_source_ids=150,
        completed_full_entry_manual_reviews=len(reviews),all_entries_development=True,reproducible_seed_53_selection=True,
        prior_unique_reviewed_ids=len(exclusions),prior_1986_ids=sum(r['edition']=='1986' for r in exclusions),
        additional_duplicate_exclusions=len(duplicate_exclusions),eligible_sampling_pool=pool,
        source_id_overlap=0,nonempty_biography_overlap=0,nonempty_name_birth_year_overlap=0,
        original_source_joins_and_all_fields_equal=True,literal_reference_prediction_and_qualifier_offsets_valid=True,
        protected_source_and_release_checksums_unchanged=True,deterministic_snapshot_replays=versions,duplicate_predictions=0,
        within_sample_nonempty_duplicate_biographies=sum(c-1 for k,c in collections.Counter(r['biography_fingerprint'] for r in rows if r['biography_fingerprint']).items()),
        within_sample_nonempty_duplicate_name_year_signatures=sum(c-1 for k,c in collections.Counter(r['person_fingerprint'] for r in rows if r['person_fingerprint']).items()))
    if not (d/'completion_manifest.json').exists():
        base.write_csv(d/'additional_duplicate_exclusions.csv',duplicate_exclusions,['source_key','reason','matching_prior_keys'])
        base.write_json(d/'validation.json',result)
    else:
        complete=old.read_json(d/'completion_manifest.json')
        for name,value in complete['output_checksums'].items():assert base.digest(d/name)==value
        assert result==old.read_json(d/'validation.json')
    return result


def export_comparisons(root,version):
    import amws_university_1986_150_engine as engine
    d=dest(root);rows=sample(root);by={r['source_key']:r for r in rows};reviews={r['source_key']:r for r in refs(root)}
    predictions=previous.read_jsonl(d/'development_versions'/version/'predictions.jsonl')
    comparison=[];appointments=[];traces=[];addresses=[];queue=[];temporal=collections.Counter()
    for result in predictions:
        row=by[result['source_key']];review=reviews[result['source_key']];groups=old.reference_groups(review);unmatched=set(range(len(groups)))
        identity={k:row[k] for k in ('sample_id','source_key','edition')};identity['scientist']=row['raw_name'];identity['review_decision']=review['decision']
        def texts(items):return ' | '.join(dict.fromkeys(x['text'] for x in items))
        for p in result['affiliations']:
            matches=[j for j in sorted(unmatched) if engine.norm(p['institution']['text']) in groups[j]['forms']]
            group=groups[matches[0]] if matches else None
            if matches:unmatched.remove(matches[0])
            blocks=group['blocks'] if group else [];expected=old.expected_temporal(group) if group else ''
            if group:temporal[(expected,p['temporal_status'])]+=1
            comparison.append(dict(**identity,university=p['institution']['text'],reference_university=blocks[0]['institution']['text'] if blocks else '',comparison='matched' if group else 'unsupported_or_literal_mismatch',
                role=texts(p['roles']),reference_roles=texts([e for a in blocks for e in a['roles']]),printed_dates=texts(p['dates']),reference_dates=texts([e for a in blocks for e in a['dates']]),
                temporal_status=p['temporal_status'],reference_temporal_status=expected,appointment_types=' | '.join(p['appointment_types']),
                qualifiers=texts(p['qualifiers']),reference_qualifiers=texts([e for a in blocks for e in a.get('qualifiers',[])]),
                evidence=texts(p['contexts']),reference_evidence=texts([e for a in blocks for e in a['contexts']]),
                source_field='raw_text_adjusted',frozen_field=p['institution']['field'],institution_start=p['institution']['start'],institution_end=p['institution']['end'],
                review_flags=' | '.join(f['reason'] for f in result['review_flags'])))
            for a in p['appointments']:appointments.append(dict(**identity,**a))
            for t in p['trace']:traces.append(dict(**identity,institution=p['institution']['text'],**t))
        for j in sorted(unmatched):
            g=groups[j];blocks=g['blocks'];a=blocks[0]
            comparison.append(dict(**identity,university='',reference_university=a['institution']['text'],comparison='missed_reference',
                role='',reference_roles=texts([e for a in blocks for e in a['roles']]),printed_dates='',reference_dates=texts([e for a in blocks for e in a['dates']]),temporal_status='',reference_temporal_status=old.expected_temporal(g),appointment_types='',qualifiers='',reference_qualifiers=texts([e for a in blocks for e in a.get('qualifiers',[])]),
                evidence='',reference_evidence=texts([e for a in blocks for e in a['contexts']]),source_field='raw_text_adjusted',frozen_field='biography_text',institution_start=a['institution']['start'],institution_end=a['institution']['end'],review_flags='recoverable_manual_reference_missed'))
        if not groups and not result['affiliations']:
            comparison.append(dict(**identity,university='',reference_university='',comparison=review['decision'],role='',reference_roles='',printed_dates='',reference_dates='',temporal_status='',reference_temporal_status='',appointment_types='',qualifiers='',reference_qualifiers='',evidence='',reference_evidence=texts(review['negative_evidence']+review['unresolved_evidence']),source_field='raw_text_adjusted',frozen_field='biography_text',institution_start='',institution_end='',review_flags=review['explanation']))
        for a in result['reported_address_affiliations']:addresses.append(dict(**identity,**a))
        if review['unresolved_evidence'] or result['review_flags']:queue.append(dict(**identity,manual_unresolved_evidence=review['unresolved_evidence'],prediction_flags=result['review_flags'],explanation=review['explanation']))
    fields=['sample_id','source_key','edition','scientist','review_decision','university','reference_university','comparison','role','reference_roles','printed_dates','reference_dates','temporal_status','reference_temporal_status','appointment_types','qualifiers','reference_qualifiers','source_field','frozen_field','institution_start','institution_end','evidence','reference_evidence','review_flags']
    base.write_csv(d/'university_comparison.csv',comparison,fields)
    base.write_csv(d/'temporal_confusion.csv',[dict(reference_status=g,predicted_status=p,count=c) for (g,p),c in sorted(temporal.items())],['reference_status','predicted_status','count'])
    for name,items in [('appointments.jsonl',appointments),('rule_traces.jsonl',traces),('reported_address_affiliations.jsonl',addresses),('review_queue.jsonl',queue)]:old.write_jsonl(d/name,items)
    summary=dict(entries=150,decisions=dict(collections.Counter(r['decision'] for r in reviews.values())),
        reference_institutions=sum(len(old.reference_groups(r)) for r in reviews.values()),
        all_reference_roles=sum(len({engine.norm(e['text']) for a in g['blocks'] for e in a['roles']}) for r in reviews.values() for g in old.reference_groups(r)),
        all_reference_dates=sum(len({engine.norm(e['text']) for a in g['blocks'] for e in a['dates']}) for r in reviews.values() for g in old.reference_groups(r)),
        predicted_institutions=sum(len(r['affiliations']) for r in predictions),address_reports=len(addresses),review_queue_entries=len(queue))
    base.write_json(d/'review_summary.json',summary)
    cell=lambda value:str(value).replace('|',' / ').replace('\n',' ')
    lines=['# 1986 university employment — 150 development entries','','All 150 entries appear, including negative/unresolved decisions and missed institutions. Address-only evidence stays separate.','',
        '| Source ID | Scientist | Predicted university | Manual reference | Result | Role | Printed dates | Status / reference | Evidence |','|---|---|---|---|---|---|---|---|---|']
    for r in comparison:lines.append('| '+' | '.join(cell(v) for v in [r['source_key'],r['scientist'],r['university'],r['reference_university'],r['comparison'],r['role'],r['printed_dates'],r['temporal_status']+' / '+r['reference_temporal_status'],r['evidence'] or r['reference_evidence']])+' |')
    (d/'COMPARISON.md').write_text('\n'.join(lines)+'\n',encoding='utf-8')
    return summary


def finish(root=None,version='v07_role_spans'):
    import amws_university_1986_150_engine as engine
    d=dest(root)
    if (d/'completion_manifest.json').exists():raise ValueError('Completed release immutable')
    folder=d/'development_versions'/version
    assert base.digest(folder/'engine.py')==base.digest(Path(engine.__file__)),'Selected snapshot and default engine differ'
    validation=validate(root)
    tests=subprocess.run([sys.executable,'-m','unittest','discover','-s',str(Path(__file__).parent),'-p','test_amws_university_1986_150.py','-v'],capture_output=True,text=True,encoding='utf-8')
    (d/'structural_test_results.txt').write_text(tests.stdout+tests.stderr,encoding='utf-8');assert tests.returncode==0,'Structural fixtures failed'
    summary=export_comparisons(root,version)
    metrics=old.read_json(folder/'metrics.json');m=next(r for r in metrics if r['rule_family']=='all')
    code=d/'frozen_code';code.mkdir()
    for name in ['amws_university_regex_1986_150.py','amws_university_1986_150_engine.py','amws_university_1986_150_manual_references.py','test_amws_university_1986_150.py']:
        (code/name).write_bytes(Path(__file__).with_name(name).read_bytes())
    checks={str(p.relative_to(d)):base.digest(p) for p in [*code.iterdir(),*(d/'development_versions').rglob('*')] if p.is_file()}
    base.write_json(d/'frozen_rules.json',dict(selected_version=version,frozen_at_utc=datetime.now(timezone.utc).isoformat(),
        engine_sha256=base.digest(Path(engine.__file__)),references_sha256=base.digest(d/'manual_reviews.jsonl'),
        frozen_checksums=checks,all_entries_development=True,independent_test_entries=0,precision_target=.95,target_met=m['precision_target_met']))
    report=['# 1986 university extraction: 150 fresh development entries','',
        'A separate 1986 strategy was developed on 150 complete original biographies. Every entry was manually read and referenced before new predictions were displayed or scored. references_frozen.json records that boundary. All 150 records are development data; these are calibration results, not independent test accuracy or population accuracy.','',
        '## Final results','',
        '| Measure | Result |','|---|---|',
        f"| Correct / predicted institutions | {m.get('true_positive',0)} / {m.get('predicted_institutions',0)} |",
        f"| Reference institutions | {m['reference_institutions']} |",
        f"| Institution precision | {m['precision']:.1%} |",f"| Institution recall | {m['recall']:.1%} |",f"| Institution F1 | {m['f1']:.1%} |",
        f"| 95% precision target | {'met' if m['precision_target_met'] else 'not met'} |",
        f"| Temporal correct / matched institutions | {m.get('temporal_correct',0)} / {m.get('temporal_denominator',0)} ({m['temporal_accuracy']:.1%}) |",
        f"| Missed reference institutions | {m.get('false_negative',0)} |",f"| Unsupported or mismatched predictions | {m.get('false_positive',0)} |",
        f"| Unsupported predictions on entries with unresolved evidence | {m.get('predictions_on_unresolved',0)} |",f"| Duplicate predictions | {m.get('duplicates',0)} |",'',
        'Institution scoring is one-to-one within each record using only frozen literal forms and manually predeclared alternatives. Successive appointments are retained separately and do not inflate institution counts. Supported affiliations on partly unresolved entries can score, but unresolved evidence itself never earns credit. No canonical identity equivalence or spelling restoration is inferred.','',
        f"All 150 entries remain in the review denominator. Decisions: {summary['decisions']}. {m.get('unresolved_entries',0)} entries contain unresolved evidence, including partially positive records. Original names and biographies were not filtered for quality. Institution counts refer to record-level literal affiliations, not unique scientists or canonical university identities.",'',
        '## Fresh sample and provenance','',
        f"The sampling frame is all 79,073 immutable v06 1986 records. Prior workplace, affiliation and name-review samples contain {validation['prior_unique_reviewed_ids']:,} distinct v06 IDs across editions, including {validation['prior_1986_ids']:,} from 1986. These IDs and {validation['additional_duplicate_exclusions']} additional detectable duplicates were excluded, leaving {validation['eligible_sampling_pool']:,} eligible 1986 records. One random.Random(53) draw selects 150 without replacement from source_key-sorted records.",'',
        'Prior fingerprints were recomputed from the same complete original-source frame. Nonempty NFKC/casefold/whitespace-normalized biographies and exact normalized full v06 name/birth-year signatures are conservative duplicate detection, not complete person linkage. prior_review_exclusions.csv and additional_duplicate_exclusions.csv retain the exclusion provenance. No sampled entry was replaced.','',
        'The original entry_id joins to source_key, and sample_150.csv preserves name_raw, complete raw_text_adjusted as biography_text, all original CSV columns in original_fields_json, v06 row checksums and source provenance. Evidence offsets are zero-based Python Unicode codepoints, end exclusive, in the unchanged raw_text_adjusted text; biography_text is its frozen exact alias. They are not byte or scanned-page coordinates.','',
        '## Structural strategy and development versions','',
        'Prof Exp and Concurrent Pos are bounded separately, including labels glued directly to preceding dates. Memberships, honors, research narratives, mailing addresses and neighboring entries cannot supply employers. Bounded OCR variants of section headings are recognized without modifying source text or repairing institution spelling. Missing or ambiguous headings are flagged; missing Prof Exp does not trigger a broad search through education.','',
        'Academic, research, technical and administrative employment roles support named university/college institutions, including technological institutes and clearly university-linked units. Generic role/date sequences handle institutions appearing before or after successive ranks, ampersand-separated employers and limited role carry. New incompatible employers, unrecognized role prefixes, governance, honorary posts and clinical training stop carry. Explicit faculty appointments in a clause also containing training remain extractable.','',
        'Medical-school, laboratory and recoverable campus qualifiers are separate from university names. Ambiguous constituent/parent relationships, damaged names and at-department updates without an appointment remain flagged. The engine has no institution-name dictionary, person override, sample-specific spelling correction, canonical IDs or geographic lookup.','',
        'Mailing addresses are always separate from employment. An exact literal university already supported by a job can be corroborated by an address, but address evidence cannot create a job or reopen a closed appointment. Unresolved address institution boundaries retain their full original address span. Postal endpoints prevent neighboring material being attached to the address where a postal code provides a clear boundary.','',
        '| Version | Correct / predicted | Precision | Recall | F1 | Temporal correct / matched |','|---|---|---|---|---|---|']
    for v in engine.VERSIONS:
        x=next(r for r in old.read_json(d/'development_versions'/v/'metrics.json') if r['rule_family']=='all')
        report.append(f"| {v} | {x.get('true_positive',0)} / {x.get('predicted_institutions',0)} | {x['precision']:.1%} | {x['recall']:.1%} | {x['f1']:.1%} | {x.get('temporal_correct',0)} / {x.get('temporal_denominator',0)} |")
    report+=['','Every version retains its original executable engine, patterns, processing order, predictions, rule metrics and error queue. Later code is not substituted when replaying earlier versions. Baseline errors include membership leakage, college-named campuses treated as employers, honorary fellowships, university-relations phrases, damaged names and unit/campus fragments. Subsequent general guards address these structures. Earlier frozen test records are not a new evaluation set; all new calibration uses only these 150 development references.','',
        '## Remaining failures and appointment detail','',
        'Six recoverable institutions remain missed: Iowa State in 1986:68862 has an unrecognized damaged research-role token; the Miami fellowship in 1986:99172 is withheld by a clause also naming a national laboratory; 1986:17861 lacks a professional-experience heading and has three readable but damaged/unbounded university appointments; 1986:58778 has a damaged school-of-mines name. These are false negatives, not successful unresolved extractions. Missing headings are not repaired by interpreting degree institutions as employers.','',
        f"There are {m.get('temporal_denominator',0)-m.get('temporal_correct',0)} temporal mismatches among correctly matched institutions. Closed printed ranges are past; valid open ranges support current status; combined spells may be mixed. Undated posts, damaged dates and terminal single years can remain uncertain. A later dated appointment can provide historical context for a single-year earlier job. No years are expanded or imputed: resolved_years is empty. Exact temporal mismatches and their evidence are retained in temporal_confusion.csv and the selected version error queue.",'',
        f"Exact normalized role/date set comparisons conditional on a matched institution: roles {m.get('roles_matched',0)} matched / {m.get('roles_predicted',0)} predicted / {m.get('roles_reference',0)} reference; dates {m.get('dates_matched',0)} / {m.get('dates_predicted',0)} / {m.get('dates_reference',0)}. Across all manual institutions, including misses, there are {summary['all_reference_roles']} reference role forms and {summary['all_reference_dates']} date forms. Role/date figures measure literal recovery, not complete or independently validated timeline accuracy.",'',
        'Appointments preserve printed roles, dates, visiting/consulting/fellowship/emeritus types and supporting contexts, but dense clauses can pool roles or dates across successive appointments or funding statements. Some reference qualifier splits are richer than the regex output. Institution precision does not imply complete role, campus or temporal accuracy. No salary, primary-employer or historical campus identity is inferred.','',
        '## Validation and files','',
        'Eighteen structural fixtures passed: glued and damaged section labels, missing headings, education exclusion, concurrent sections, carry termination, late institution names and multiple employers, university versus unit/campus, honorary/governance/secondary-school exclusions, clinical training versus faculty, funding-only mentions, retirement/emeritus, undated/single years, OCR damage, neighboring entries, literal addresses, evidence offsets and deterministic extraction. Validation reconstructs the draw and every original field, checks all 150 reviews, verifies each saved executable version, checks evidence offsets and confirms protected sources/releases are unchanged.','',
        '- sample_150.csv, sample_manifest.json and prior/additional exclusion ledgers: frozen source data, sampling and checksums.','- manual_reviews.jsonl and references_frozen.json: 150 full-entry manual decisions, literal forms, alternatives, roles, dates, qualifiers and unresolved evidence recorded before predictions. pre_prediction_review_revisions.jsonl retains the second-reading changes made before freeze.','- university_comparison.csv and COMPARISON.md: every scientist, predicted/reference university, role, printed dates, temporal status, qualifiers, evidence and review flags.','- development_versions/: baseline and all successive engine snapshots, predictions, rule specifications, metrics by rule family and error queues.','- appointments.jsonl and rule_traces.jsonl: appointment details and original-field rule traces. reported_address_affiliations.jsonl stays separate from employment.','- review_queue.jsonl, review_summary.json and temporal_confusion.csv: unresolved cases, denominator counts and temporal comparisons.','- structural_test_results.txt, validation.json, frozen_code/, frozen_rules.json and completion_manifest.json: verification, code and checksums.','',
        'Workflow: prepare → read all complete biographies and enter manual references → freeze-references → develop → finish. Completed releases refuse replacement. extract_universities(entry, edition="1986") accepts a frozen sample entry or raw_text_adjusted. Existing sources, first-three-edition extractor, earlier affiliation/name releases, parent–offspring candidates and analysis inputs remain unchanged. No population affiliation data was modified. Any independent performance claim requires a new test sample.','']
    (d/'REPORT.md').write_text('\n'.join(report),encoding='utf-8')
    base.write_json(d/'completion_manifest.json',dict(completed_at_utc=datetime.now(timezone.utc).isoformat(),selected_version=version,validation=validation,
        output_checksums={str(p.relative_to(d)):base.digest(p) for p in d.rglob('*') if p.is_file() and p.name!='completion_manifest.json'}))
    return d/'REPORT.md'

def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('command',choices=['prepare','show','freeze-references','develop','validate','finish'])
    p.add_argument('--start',type=int,default=1);p.add_argument('--end',type=int,default=15);a=p.parse_args()
    if a.command=='prepare':print(prepare())
    elif a.command=='show':show(a.start,a.end)
    elif a.command=='freeze-references':print(freeze_references())
    elif a.command=='develop':print(develop())
    elif a.command=='validate':print(validate())
    elif a.command=='finish':print(finish())

if __name__=='__main__':main()

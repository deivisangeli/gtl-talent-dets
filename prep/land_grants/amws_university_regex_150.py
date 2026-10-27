"""Fresh three-edition, 150-entry university extraction development release."""
import argparse
import collections
from datetime import datetime,timezone
import json
from pathlib import Path
import random
import shutil
import subprocess
import tempfile
import importlib.util

import amws_university_affiliation_calibration as old
import amws_name_cleaning as base
import amws_workplace_feasibility as workplace

EDITIONS=('1906','1938','1955')
RELEASE='amws_university_regex_150_20261023'
FIELDS=['sample_id','biography_fingerprint','person_fingerprint']+[k for k in workplace.SAMPLE_FIELDS if k!='sample_id']+['rest_text','original_fields_json']


def dest(root=None):return workplace.root_path(root)/'output/land_grants'/RELEASE
def read_jsonl(path):return [json.loads(s) for s in Path(path).read_text(encoding='utf-8').splitlines() if s]
def refs(root=None):return read_jsonl(dest(root)/'manual_reviews.jsonl')
def sample(root=None):return base.read_csv(dest(root)/'sample_150.csv')


def prior(root=None):
    return base.read_csv(workplace.destination(root)/'sample_80.csv')+base.read_csv(old.destination(root)/'sample_1000.csv')


def person(row):
    return old.fingerprint(row['v06_name']+'|'+row['v06_birth_year']) if row['v06_name'].strip() and row['v06_birth_year'].strip() else ''


def draw(frame,previous):
    ids={r['source_key'] for r in previous}
    bios={old.fingerprint(r['biography_text']) for r in previous}-{''}
    people={person(r) for r in previous}-{''}
    rng=random.Random(52);out=[];pools={}
    for ed in EDITIONS:
        pool=sorted((r for r in frame if r['edition']==ed and r['source_key'] not in ids
                     and (not r['biography_fingerprint'] or r['biography_fingerprint'] not in bios)
                     and (not r['person_fingerprint'] or r['person_fingerprint'] not in people)),key=lambda r:r['source_key'])
        pools[ed]=len(pool)
        offset=len(out)
        out.extend(dict(**r,sample_id=str(offset+i+1)) for i,r in enumerate(rng.sample(pool,50)))
    return out,pools


def extra_fields(rows,root=None):
    import openpyxl
    extras={};ids={r['source_key'] for r in rows}
    wb=openpyxl.load_workbook(workplace.raw_paths(root)['1906'],read_only=True,data_only=True)
    for row in wb.worksheets[0].iter_rows(values_only=True):
        if row[0] is None:continue
        key=f'1906:{int(row[0])}'
        if key in ids:extras[key]=dict(rest_text='',original_fields_json=json.dumps({'workbook_cells':list(row)},ensure_ascii=False,default=str))
    wb.close()
    with tempfile.TemporaryDirectory(prefix='amws_uni150_') as tmp:
        ip=Path(tmp)/'ids.csv';op=Path(tmp)/'extra.csv'
        base.write_csv(ip,[dict(source_key=k) for k in sorted(ids)],['source_key'])
        p=subprocess.run([shutil.which('Rscript'),str(Path(__file__).with_name('read_amws_university_150_sources.R')),
            str(workplace.root_path(root)),str(ip),str(op)],capture_output=True,text=True,encoding='utf-8',errors='replace')
        if p.returncode:raise ValueError(p.stderr)
        for row in base.read_csv(op):extras[row.pop('source_key')]=row
    assert set(extras)==ids
    return [dict(**r,**extras[r['source_key']]) for r in rows]


def protections(root=None):
    checks=workplace.protected_checksums(root)
    for folder in [workplace.destination(root),old.destination(root)]:
        checks[str(folder/'completion_manifest.json')]=base.digest(folder/'completion_manifest.json')
        for name,value in old.read_json(folder/'completion_manifest.json')['output_checksums'].items():checks[str(folder/name)]=value
    for name in ['amws_university_affiliation_calibration.py','amws_university_affiliation_engine.py','amws_workplace_feasibility.py','read_amws_workplace_sources.R']:
        p=Path(__file__).with_name(name).resolve();checks[str(p)]=base.digest(p)
    return checks


def prepare(root=None):
    d=dest(root)
    if d.exists():raise ValueError('Release already exists; refuse resampling')
    checks=protections(root);workplace.verify_checksums(checks)
    rows,pools=draw(old.source_frame(root),prior(root));rows=extra_fields(rows,root)
    d.mkdir(parents=True)
    base.write_csv(d/'sample_150.csv',rows,FIELDS)
    (d/'manual_reviews.jsonl').touch()
    base.write_json(d/'sample_manifest.json',dict(seed=52,editions=list(EDITIONS),records_per_edition=50,pool_counts=pools,
        prior_records_excluded=1080,sampling='Single random.Random(52), ascending editions, source_key-sorted eligible pools, without replacement',
        duplicate_detection='NFKC/casefold/whitespace-normalized biography and exact nonempty full-name/birth-year signature; conservative, not person linkage',
        sample_sha256=base.digest(d/'sample_150.csv'),input_checksums=checks,created_at_utc=datetime.now(timezone.utc).isoformat()))
    return d


def show(start,end,root=None):
    for r in sample(root):
        if start<=int(r['sample_id'])<=end:
            print(f"\n[{r['sample_id']}] {r['source_key']} NAME {r['raw_name']}\nADDRESS: {r['address_text']}\nFIELD: {r['field_text']}\nBIRTHPLACE: {r['birthplace_text']}\nBIO: {r['biography_text']}\nREST: {r['rest_text']}")


class ManualBatch(old.ManualBatch):
    def __init__(self,root=None):
        self.root=root;self.rows={int(r['sample_id']):dict(partition='development',**r) for r in sample(root)}

    def affiliation(self,*args,qualifiers=(),**kwargs):
        value=super().affiliation(*args,**kwargs)
        i=args[0];value['qualifiers']=[self.ev(i,*q) for q in qualifiers]
        return value

    def entry(self,i,*args,headers=(),**kwargs):
        r=super().entry(i,*args,**kwargs)
        r['reported_header_affiliations']=[self.ev(i,*h) for h in headers]
        return r

    def save(self,items):
        d=dest(self.root)
        if (d/'references_frozen.json').exists():raise ValueError('References frozen before predictions')
        seen={r['sample_id'] for r in refs(self.root)}
        for r in items:
            if r['sample_id'] in seen:raise ValueError('Duplicate manual review')
            old.check_review(r,self.rows[int(r['sample_id'])]);seen.add(r['sample_id'])
            for ev in [*r['reported_header_affiliations'],*[ev for a in r['affiliations'] for ev in a.get('qualifiers',[])]]:
                assert self.rows[int(r['sample_id'])][ev['field']][ev['start']:ev['end']]==ev['text']
        with (d/'manual_reviews.jsonl').open('a',encoding='utf-8',newline='\n') as f:
            for r in items:f.write(json.dumps(r,ensure_ascii=False)+'\n')
        return len(seen)


def freeze_references(root=None):
    rows=sample(root);reviews=refs(root);d=dest(root)
    if len(reviews)!=150 or {r['sample_id'] for r in rows}!={r['sample_id'] for r in reviews}:raise ValueError('All 150 full manual references required')
    for r in reviews:old.check_review(r,dict(partition='development',**next(x for x in rows if x['sample_id']==r['sample_id'])))
    if (d/'references_frozen.json').exists():raise ValueError('References already frozen')
    base.write_json(d/'references_frozen.json',dict(references_sha256=base.digest(d/'manual_reviews.jsonl'),sample_sha256=base.digest(d/'sample_150.csv'),
        frozen_at_utc=datetime.now(timezone.utc).isoformat(),complete_reviews=150,predictions_displayed=False))
    return d/'references_frozen.json'


def evaluate(rows,refs,version):
    from amws_university_150_engine import extract_universities,norm
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
    import amws_university_150_engine as engine
    d=dest(root);frozen=old.read_json(d/'references_frozen.json')
    assert base.digest(d/'manual_reviews.jsonl')==frozen['references_sha256']
    assert base.digest(d/'sample_150.csv')==frozen['sample_sha256']
    if (d/'completion_manifest.json').exists():raise ValueError('Completed release immutable')
    rows=[dict(partition='development',**r) for r in sample(root)]
    for version in engine.VERSIONS:
        folder=d/'development_versions'/version
        if folder.exists():continue
        result=evaluate(rows,refs(root),version);old.save_evaluation(folder,result)
        (folder/'engine.py').write_bytes(Path(engine.__file__).read_bytes())
        base.write_json(folder/'rule_specification.json',dict(**engine.rule_specification(),selected_version=version,
            engine_sha256=base.digest(Path(engine.__file__)),references_sha256=frozen['references_sha256'],
            evaluated_at_utc=datetime.now(timezone.utc).isoformat(),partition='development'))
    return d/'development_versions'


def validate_evidence(row,obj):
    if isinstance(obj,dict):
        if {'field','start','end','text'}<=obj.keys():
            assert obj['field'] in row and row[obj['field']][obj['start']:obj['end']]==obj['text'],'Nonliteral evidence'
        for value in obj.values():validate_evidence(row,value)
    elif isinstance(obj,list):
        for value in obj:validate_evidence(row,value)


def validate(root=None):
    import amws_university_150_engine as engine
    d=dest(root);manifest=old.read_json(d/'sample_manifest.json');frozen=old.read_json(d/'references_frozen.json')
    workplace.verify_checksums(manifest['input_checksums'])
    assert base.digest(d/'sample_150.csv')==manifest['sample_sha256']==frozen['sample_sha256']
    assert base.digest(d/'manual_reviews.jsonl')==frozen['references_sha256']
    stored=sample(root);reviews=refs(root);previous=prior(root)
    assert len(stored)==len({r['source_key'] for r in stored})==len(reviews)==150
    assert collections.Counter(r['edition'] for r in stored)=={e:50 for e in EDITIONS}
    assert [r['sample_id'] for r in stored]==list(map(str,range(1,151)))
    assert {r['sample_id'] for r in reviews}=={r['sample_id'] for r in stored}
    by={r['source_key']:r for r in stored}
    for ref in reviews:
        row=by[ref['source_key']];old.check_review(ref,dict(partition='development',**row));validate_evidence(row,ref)
    assert not ({r['source_key'] for r in stored}&{r['source_key'] for r in previous})
    for key in ('biography_fingerprint','person_fingerprint'):
        previous_values={old.fingerprint(r['biography_text']) if key=='biography_fingerprint' else person(r) for r in previous}-{''}
        assert not ({r[key] for r in stored}-{''})&previous_values
    frame=old.source_frame(root);redrawn,pools=draw(frame,previous);redrawn=extra_fields(redrawn,root)
    assert pools==manifest['pool_counts']
    assert stored==[{k:r.get(k,'') for k in FIELDS} for r in redrawn],'Original sources or sample differ'
    versions=[]
    for folder in sorted((d/'development_versions').iterdir()):
        if not folder.is_dir():continue
        spec=old.read_json(folder/'rule_specification.json')
        assert base.digest(folder/'engine.py')==spec['engine_sha256']
        assert spec['references_sha256']==frozen['references_sha256']
        module_spec=importlib.util.spec_from_file_location('snapshot_'+folder.name,folder/'engine.py')
        module=importlib.util.module_from_spec(module_spec);module_spec.loader.exec_module(module)
        predictions=read_jsonl(folder/'predictions.jsonl');assert len(predictions)==150
        for prediction in predictions:
            row=by[prediction['source_key']];validate_evidence(row,prediction)
            result=module.extract_universities(row,row['edition'],folder.name)
            assert result=={k:prediction[k] for k in result},'Saved prediction differs from exact version snapshot'
            assert result==module.extract_universities(row,row['edition'],folder.name),'Nondeterministic extraction'
            keys=[engine.norm(p['institution']['text']) for p in result['affiliations']]
            assert len(keys)==len(set(keys)),'Duplicate institution scoring'
        versions.append(folder.name)
    result=dict(source_records=len(frame),sample_entries=150,edition_counts={e:50 for e in EDITIONS},
        complete_full_entry_manual_reviews=len(reviews),unique_source_ids=150,prior_reviewed_entries=1080,
        source_id_overlap=0,nonempty_biography_overlap=0,nonempty_name_birth_year_overlap=0,
        reproducible_seed_52_selection=True,original_source_joins_and_all_frozen_fields_equal=True,
        literal_reference_and_prediction_offsets_valid=True,protected_checksums_unchanged=True,
        deterministic_version_replays=versions,duplicate_predictions=0,all_records_development=True)
    base.write_json(d/'validation.json',result)
    return result


def finish(root=None,version='v09_medical_unit_forms'):
    import amws_university_150_engine as engine
    d=dest(root)
    if (d/'completion_manifest.json').exists():raise ValueError('Completed release immutable')
    folder=d/'development_versions'/version
    assert base.digest(folder/'engine.py')==base.digest(Path(engine.__file__)),'Selected snapshot must match current extractor'
    validation=validate(root)
    tests=subprocess.run([__import__('sys').executable,'-m','unittest','discover','-s',str(Path(__file__).parent),'-p','test_amws_university_150.py','-v'],capture_output=True,text=True,encoding='utf-8')
    (d/'structural_test_results.txt').write_text(tests.stdout+tests.stderr,encoding='utf-8')
    assert tests.returncode==0,'Structural fixtures failed'
    rows=sample(root);by={r['source_key']:r for r in rows};reviews={r['source_key']:r for r in refs(root)}
    predictions=read_jsonl(folder/'predictions.jsonl');metrics=old.read_json(folder/'metrics.json')
    comparison=[];appointments=[];traces=[];reported=[];unresolved=[];temporal=collections.Counter();summary=[]
    for result in predictions:
        row=by[result['source_key']];review=reviews[result['source_key']];groups=old.reference_groups(review)
        unmatched=set(range(len(groups)))
        identity={k:row[k] for k in ('sample_id','source_key','edition')};identity['scientist']=row['raw_name'];identity['review_decision']=review['decision']
        for p in result['affiliations']:
            matches=[j for j in sorted(unmatched) if engine.norm(p['institution']['text']) in groups[j]['forms']]
            group=groups[matches[0]] if matches else None
            if matches:unmatched.remove(matches[0])
            expected=old.expected_temporal(group) if group else ''
            if group:temporal[(row['edition'],expected,p['temporal_status'])]+=1
            comparison.append(dict(**identity,university=p['institution']['text'],reference_university=group['blocks'][0]['institution']['text'] if group else '',
                comparison='matched' if group else 'unsupported_or_literal_mismatch',role=' | '.join(dict.fromkeys(x['text'] for x in p['roles'])),
                printed_dates=' | '.join(dict.fromkeys(x['text'] for x in p['dates'])),temporal_status=p['temporal_status'],reference_temporal_status=expected,
                appointment_types=' | '.join(p['appointment_types']),qualifiers=' | '.join(dict.fromkeys(x['text'] for x in p['qualifiers'])),
                evidence=' | '.join(dict.fromkeys(x['text'] for x in p['contexts'])),source_field=p['institution']['field'],institution_start=p['institution']['start'],institution_end=p['institution']['end'],
                review_flags=' | '.join(f['reason'] for f in result['review_flags'])))
            for appointment in p['appointments']:appointments.append(dict(**identity,**appointment))
            for trace in p['trace']:traces.append(dict(**identity,institution=p['institution']['text'],**trace))
        for j in sorted(unmatched):
            g=groups[j];blocks=g['blocks']
            comparison.append(dict(**identity,university='',reference_university=blocks[0]['institution']['text'],comparison='missed_reference',
                role=' | '.join(dict.fromkeys(x['text'] for a in blocks for x in a['roles'])),printed_dates=' | '.join(dict.fromkeys(x['text'] for a in blocks for x in a['dates'])),
                temporal_status='',reference_temporal_status=old.expected_temporal(g),appointment_types=' | '.join(sorted({t for a in blocks for t in a['appointment_types']})),
                qualifiers=' | '.join(dict.fromkeys(x['text'] for a in blocks for x in a.get('qualifiers',[]))),evidence=' | '.join(dict.fromkeys(x['text'] for a in blocks for x in a['contexts'])),
                source_field=blocks[0]['institution']['field'],institution_start=blocks[0]['institution']['start'],institution_end=blocks[0]['institution']['end'],review_flags='recoverable_manual_reference_missed'))
        if not result['affiliations'] and not groups:
            comparison.append(dict(**identity,university='',reference_university='',comparison=review['decision'],role='',printed_dates='',temporal_status='',reference_temporal_status='',appointment_types='',qualifiers='',
                evidence=' | '.join(e['text'] for e in review['negative_evidence']+review['unresolved_evidence']),source_field='',institution_start='',institution_end='',review_flags=review['explanation']))
        for h in result['reported_header_affiliations']:reported.append(dict(**identity,**h))
        if review['unresolved_evidence'] or result['review_flags']:
            unresolved.append(dict(**identity,manual_unresolved_evidence=review['unresolved_evidence'],prediction_review_flags=result['review_flags'],explanation=review['explanation']))
    fields=['sample_id','source_key','edition','scientist','review_decision','university','reference_university','comparison','role','printed_dates','temporal_status','reference_temporal_status','appointment_types','qualifiers','source_field','institution_start','institution_end','evidence','review_flags']
    base.write_csv(d/'university_comparison.csv',comparison,fields)
    base.write_csv(d/'temporal_confusion.csv',[dict(edition=e,reference_status=g,predicted_status=p,count=c) for (e,g,p),c in sorted(temporal.items())],['edition','reference_status','predicted_status','count'])
    for name,items in [('appointments.jsonl',appointments),('rule_traces.jsonl',traces),('reported_header_affiliations.jsonl',reported),('review_queue.jsonl',unresolved)]:old.write_jsonl(d/name,items)
    for ed in EDITIONS:
        r=[x for x in reviews.values() if x['edition']==ed]
        summary.append(dict(edition=ed,entries=50,**dict(collections.Counter(x['decision'] for x in r)),
            reference_institutions=sum(len(old.reference_groups(x)) for x in r),
            all_reference_roles=sum(len({engine.norm(v['text']) for a in g['blocks'] for v in a['roles']}) for x in r for g in old.reference_groups(x)),
            all_reference_dates=sum(len({engine.norm(v['text']) for a in g['blocks'] for v in a['dates']}) for x in r for g in old.reference_groups(x))))
    base.write_json(d/'review_summary.json',summary)
    # Standalone readable table, including every sampled entry and misses.
    def cell(s):return str(s).replace('|',' / ').replace('\n',' ')
    lines=['# University extraction comparison — 150 development entries','','Every entry appears. Blank predictions and missed references are retained. Address-only affiliations are separate in reported_header_affiliations.jsonl.','',
        '| ID | Edition | Scientist | Predicted university | Manual reference | Result | Role | Printed dates | Status / reference | Evidence |','|---|---|---|---|---|---|---|---|---|---|']
    for r in comparison:lines.append('| '+' | '.join(cell(x) for x in [r['source_key'],r['edition'],r['scientist'],r['university'],r['reference_university'],r['comparison'],r['role'],r['printed_dates'],r['temporal_status']+' / '+r['reference_temporal_status'],r['evidence']])+' |')
    (d/'COMPARISON.md').write_text('\n'.join(lines)+'\n',encoding='utf-8')
    snapshots=d/'frozen_code';snapshots.mkdir()
    for name in ['amws_university_regex_150.py','amws_university_150_engine.py','amws_university_150_manual_references.py','read_amws_university_150_sources.R','test_amws_university_150.py']:
        (snapshots/name).write_bytes(Path(__file__).with_name(name).read_bytes())
    frozen_checksums={str(p.relative_to(d)):base.digest(p) for p in snapshots.iterdir()}
    frozen_checksums.update({str(p.relative_to(d)):base.digest(p) for p in (d/'development_versions').rglob('*') if p.is_file()})
    base.write_json(d/'frozen_rules.json',dict(selected_version=version,frozen_at_utc=datetime.now(timezone.utc).isoformat(),
        engine_sha256=base.digest(Path(engine.__file__)),references_sha256=base.digest(d/'manual_reviews.jsonl'),
        frozen_checksums=frozen_checksums,partition='development',independent_test_entries=0))
    report=['# AMWS university extraction: 150 fresh development entries','',
        '50 entries each from 1906, 1938 and 1955, drawn from immutable v06 using one random.Random(52) generator across ascending editions and source_key-sorted eligible pools. All 150 complete original entries were read and manually referenced before predictions were displayed or scored. references_frozen.json records that boundary. All 150 are development records; these results are calibration performance, not independent test accuracy or population accuracy.','',
        'Previously reviewed 80 workplace entries and 1,000 affiliation-calibration entries were excluded, together with nonempty normalized biography and full-name/birth-year matches. NFKC/casefold/whitespace normalization is conservative duplicate detection, not complete person linkage. No exclusions were based on missing names, damaged text or non-university work. No replacement of sampled entries.','',
        '## Final institution results','',
        '| Edition | Correct / predicted | Reference institutions | Precision | Recall | F1 | Temporal correct / matched | 95% precision target |','|---|---|---|---|---|---|---|---|']
    for m in metrics:
        if m['rule_family']=='all':report.append(f"| {m['edition']} | {m.get('true_positive',0)} / {m.get('predicted_institutions',0)} | {m['reference_institutions']} | {m['precision']:.1%} | {m['recall']:.1%} | {m['f1']:.1%} | {m.get('temporal_correct',0)} / {m.get('temporal_denominator',0)} | {'met' if m['precision_target_met'] else 'not met'} |")
    report+=['','Institution scoring is one-to-one within each source record, using only literal manual forms and alternatives documented before scoring. Successive appointment blocks do not inflate institution totals. Unresolved evidence earns no successful-extraction credit; positive references on partially unresolved entries remain separately supported assertions. Unsupported predictions on unresolved entries are errors, reported by edition/rule. No such errors were observed in the final version. Address-only affiliations are not included in employment scoring.','',
        '## Development trajectory','',
        '| Version | 1906 precision / recall | 1938 precision / recall | 1955 precision / recall |','|---|---|---|---|']
    for v in engine.VERSIONS:
        ms=[m for m in old.read_json(d/'development_versions'/v/'metrics.json') if m['rule_family']=='all']
        report.append('| '+v+' | '+' | '.join(f"{m['precision']:.1%} / {m['recall']:.1%}" for m in ms)+' |')
    report+=['','The marker-only baseline misses most bare institutional abbreviations. Broad role and carry rules raise recall but introduce geographic qualifiers, honorary-degree institutions and neighboring material as false positives. Later development versions restrict institution slots, terminate carry at nonacademic roles and awards, split dated appointment sentences and separate medical-school qualifiers. All baseline and successive engine snapshots, patterns, predictions, traces, metrics and errors are retained; no earlier result was overwritten.','',
        '## Source-specific strategies','',
        '- **1906:** original address column supplies separately labeled institutional affiliation; the biography supplies dated instructor/professor/tutor and assistant appointments. Semicolon sequences and dated boundaries support limited role carry.','- **1938:** address_legacy is separate from a. Career sequences retain successive ranks and abbreviated employers, with role and institutional carry stopped by new employers, incompatible roles, memberships and neighboring-person headings.','- **1955:** complete original is primary. Specialty markup and the original birthplace segment bound the header/body; address and rest are preserved for comparison. Header titles do not start career parsing. Dated clauses are parsed within career text.','',
        'The shared grammar uses generic academic roles, institutional markers, dates and structural boundaries. There are no university dictionaries, person-specific overrides or sample-specific spelling repairs. Printed university names, dates and OCR forms remain literal. College/university names are separated from recoverable medical-school, department, laboratory and campus qualifiers; uncertain constituent/parent relationships are withheld. Qualifiers are not canonical institution identifiers. Institution carry uses local original text, not outside knowledge.','',
        '## Remaining failures and limits','',
        'Missed references remain recoverable errors. Frequent causes are a named institute without a clear higher-education marker, damaged ranks or institution letters, an institution embedded in an unsplit role/field phrase, compound constituent/parent structures and summer or undated appointments. Broadly restoring spelling or interpreting every geographic label as a university would increase unsupported extraction. Ambiguous fellowships in degree sequences, independent institutes and hospital relationships remain in the review queue.','',
        'Closed appointments are past; open printed date ranges support current status; combined spells can be mixed. Unreadable or absent dates remain uncertain. Dates are not expanded to canonical years (resolved_years is empty). Temporal errors remain even when the institution is correct; temporal_confusion.csv and the selected error queue give their denominators. Role/date matches in version metrics are exact normalized literal sets conditional on a matched institution, not timeline accuracy. review_summary.json additionally supplies totals across all manual institutions, including misses.','',
        'No duplicate predictions were scored. Twelve structural fixtures cover education/employment separation, abbreviated names and carry termination, multiple appointments, departments/medical schools, retirement and emeritus status, OCR damage, secondary-school and military resets, neighboring-entry contamination, literal offsets and determinism. validation.json records exact sample/source reconstruction and protected-release checksums.','',
        '## Files and reproduction','',
        '- sample_150.csv: all original fields, source provenance, selected IDs and fingerprints. Original 1955 entry is biography_text; rest_text and original_fields_json preserve comparison fields.','- manual_reviews.jsonl: all 150 decisions, literal affiliations, roles/dates, qualifiers, negative/unresolved evidence and predeclared alternatives.','- university_comparison.csv and COMPARISON.md: readable scientist/university/appointment/evidence comparisons, including missed institutions and negative/unresolved entries.','- development_versions/: every engine snapshot, ordered rule specification, predictions, edition/rule metrics and error queue.','- appointments.jsonl and rule_traces.jsonl: appointment details and original-field rule evidence; reported_header_affiliations.jsonl stays separate from employment.','- review_queue.jsonl, review_summary.json and temporal_confusion.csv: unresolved evidence, manual counts and temporal errors.','- frozen_rules.json, sample_manifest.json, references_frozen.json and completion_manifest.json: configuration and checksums.','',
        'Reproduce in order: prepare → full manual reference entry → freeze-references → develop → finish. Completed releases refuse overwriting. To replay a past rule version, load its own development_versions/<version>/engine.py snapshot; later engine code is not substituted for an earlier snapshot. Default extract_universities(entry, edition) exposes the selected strategy. Existing v06, the previous frozen extractor, name-cleaning releases, parent–offspring candidates and analysis inputs remain unchanged. No population affiliation data or external location research was produced. A future independent performance claim requires a new test sample.','']
    (d/'REPORT.md').write_text('\n'.join(report),encoding='utf-8')
    base.write_json(d/'completion_manifest.json',dict(completed_at_utc=datetime.now(timezone.utc).isoformat(),selected_version=version,
        validation=validation,output_checksums={str(p.relative_to(d)):base.digest(p) for p in d.rglob('*') if p.is_file() and p.name!='completion_manifest.json'}))
    return d/'REPORT.md'


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('command',choices=['prepare','show','freeze-references','develop','validate','finish'])
    p.add_argument('--start',type=int,default=1);p.add_argument('--end',type=int,default=25);a=p.parse_args()
    if a.command=='prepare':print(prepare())
    elif a.command=='show':show(a.start,a.end)
    elif a.command=='freeze-references':print(freeze_references())
    elif a.command=='develop':print(develop())
    elif a.command=='validate':print(validate())
    elif a.command=='finish':print(finish())


if __name__=='__main__':main()

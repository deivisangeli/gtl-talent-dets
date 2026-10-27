"""Evaluation only: frozen predictions are never edited or fed references."""
from __future__ import annotations
import argparse
from collections import Counter, defaultdict
from difflib import SequenceMatcher
import html
import json
from pathlib import Path
import re
import shutil
import zipfile
import numpy as np
import compare_ocr_fields as audit

METHODS=['word','paddle']
FIELDS=['name','birthplace','birth_year','city']
MEASURES=FIELDS+['institution_recall','institution_precision','institution_exact_set','complete_record','biography_detection']

def load_predictions(root,available=False):
    result={}
    for doc in audit.read(root/'manifest.json')['documents']:
        for method in METHODS:
            path=root/'prediction'/method/Path(doc['source_file']).stem/'predictions.json'
            if path.exists() or not available:result[doc['source_file'],method]=audit.read(path)
    return result

def head_name(row):
    # Used for alignment proposals only; no extracted field is replaced.
    s=row['raw_text_adjusted'][:170]
    return re.split(r',?\s+\bb\s+|\bEduc\s*[:.]|\bsee previous',s,flags=re.I)[0].strip(' ,.;:')

def propose(root,available=False):
    audit.check_freeze(root);predictions=load_predictions(root,available);refs=audit.read(root/'references.json')
    results=[]
    for (file,method),rows in predictions.items():
        group=[r for r in refs if r['source_file']==file]
        labels=[(audit.canon(p['name_raw']),audit.canon(head_name(p))) for p in rows]
        for ref in group:
            name=audit.canon(ref['fields']['name']['value']);surname=audit.canon(ref['fields']['name']['value'].split(',')[0])
            candidates=[]
            exact_indices=[i for i,(a,b) in enumerate(labels) if name in (a,b)]
            if exact_indices:
                candidates=[(1.0,i) for i in exact_indices]
            for i,pair in enumerate(labels):
                if exact_indices:continue
                a,b=pair
                if not a and not b:continue
                # First surname letter is merely a speed filter, not a match rule.
                if name[0] not in (a[:1],b[:1]) and surname not in b[:len(name)+8]:continue
                sim=max(SequenceMatcher(None,name,a,autojunk=False).ratio(),SequenceMatcher(None,name,b[:len(name)+8],autojunk=False).ratio())
                exact=(name==a or name==b)
                if name and b.startswith(name):sim=max(sim,.995)
                candidates.append((1.0 if exact else sim,i))
            candidates.sort(reverse=True)
            top=[]
            for similarity,i in candidates[:4]:
                p=rows[i]
                top.append(dict(source_key=p['source_key'],name_raw=p['name_raw'],head=head_name(p),similarity=round(similarity,4),
                    pdf_page=p['pdf_page'],raw_start=p['raw_text_adjusted'][:700],
                    previous=head_name(rows[i-1]) if i else '',next=head_name(rows[i+1]) if i+1<len(rows) else ''))
            results.append(dict(reference_id=ref['id'],source_file=file,page_key=ref['page_key'],
                reference_name=ref['fields']['name']['value'],method=method,candidates=top))
    audit.write(root/'alignment_proposals.json',results)
    print('Alignment proposals:',len(results))

def validate_alignment_rows(rows,expected=608):
    assert len(rows)==expected,'Incomplete alignment review'
    assert all(r['reviewed'] and r['review_reason'] for r in rows),'Unreviewed identity decision'
    identities=[(r['reference_id'],r['method']) for r in rows]
    assert len(identities)==len(set(identities)),'Duplicate reference/method decision'
    used=[(r['method'],r['source_key']) for r in rows if r.get('source_key')]
    assert len(used)==len(set(used)),'One prediction cannot be assigned to two people'
    duplicate=[(r['method'],key) for r in rows for key in r.get('duplicate_keys',[])]
    assert not (set(used)&set(duplicate)),'Matched record also labelled duplicate'
    assert len(duplicate)==len(set(duplicate)),'Duplicate attributed to two people'

def align(root):
    """Apply separately reviewed identity decisions, never improve field values."""
    proposals=audit.read(root/'alignment_proposals.json');decisions=audit.read(root/'alignment_review.json')
    indexed={(r['reference_id'],r['method']):r for r in decisions['decisions']}
    validate_alignment_rows(decisions['decisions'])
    assert len(indexed)==608 and decisions['reviewed_all'] is True
    records=load_predictions(root);refs={r['id']:r for r in audit.read(root/'references.json')}
    used=set();out=[]
    for p in proposals:
        d=indexed[p['reference_id'],p['method']];key=d.get('source_key')
        if key:
            assert (p['method'],key) not in used,'One prediction cannot be assigned to two people'
            used.add((p['method'],key))
            assert any(r['source_key']==key for r in records[p['source_file'],p['method']])
        assert d['review_reason'] and d['reviewed'] is True
        out.append(dict(reference_id=p['reference_id'],method=p['method'],source_key=key,
            source_file=p['source_file'],page_key=p['page_key'],reviewed=True,
            review_reason=d['review_reason'],merged_into=d.get('merged_into'),
            duplicate_keys=d.get('duplicate_keys',[]),uncertain=d.get('uncertain',False)))
    audit.write(root/'alignments.json',out)
    audit.write(root/'alignment_freeze.json',dict(created_at=audit.now(),alignments_sha256=audit.sha(root/'alignments.json'),
        review_sha256=audit.sha(root/'alignment_review.json'),predictions=[dict(path=str(p.relative_to(root)),sha256=audit.sha(p))
            for p in sorted((root/'prediction').rglob('predictions.json'))]))

def institution_scores(reference,predicted,aliases):
    reference=audit.distinct(reference,aliases);predicted=audit.distinct(predicted,aliases)
    used=set();matches=[];missing=[]
    for ref in reference:
        options=[i for i,p in enumerate(predicted) if i not in used and audit.equivalent(ref,p,'institutions',aliases)]
        if options:
            i=options[0];used.add(i);matches.append(dict(reference=ref,predicted=predicted[i]))
        else:missing.append(ref)
    extras=[p for i,p in enumerate(predicted) if i not in used]
    return dict(correct=len(matches),reference_count=len(reference),predicted_count=len(predicted),
        matches=matches,missing=missing,extras=extras,exact=not missing and not extras)

def field_score(reference,predicted,field,aliases,detected=True,wrong_person=False):
    if reference['status']!='readable':return reference['status']
    if not detected or not predicted:return 'missing'
    if audit.equivalent(reference['value'],predicted,field,aliases):return 'correct'
    return 'wrong_person_attribution' if wrong_person else 'incorrect'

def wrong_owner(value,row,ref,all_refs,field):
    if not value or field=='birth_year':return False # common years do not identify an owner
    raw=audit.canon(row['raw_text_adjusted']);value_key=audit.canon(value)
    for other in all_refs:
        if other['id']==ref['id'] or other['source_file']!=ref['source_file']:continue
        f=other['fields'][field]
        if f['status']!='readable' or audit.canon(f['value'])!=value_key:continue
        n=audit.canon(other['fields']['name']['value']);pos=raw.find(n)
        if pos>=0 and raw.find(value_key,pos+len(n))>=0:return True
    return False

def evaluation_references(root,adjudicated=True):
    refs=audit.read(root/'references.json')
    if adjudicated and (root/'reference_adjudications.json').exists():
        decisions=audit.read(root/'reference_adjudications.json')
        assert decisions['reviewed'] and decisions['occurred_after_prediction_review']
        indexed={r['id']:r for r in refs}
        for d in decisions['decisions']:
            field=indexed[d['reference_id']]['fields'][d['field']]
            assert field['value']==d['old_value'] and d['source_evidence'] and d['reason']
            field['value']=d['new_value']
            indexed[d['reference_id']]['notes']+=' Reference correction after prediction review: '+d['reason']
    return refs

def evaluate(root,original=False):
    audit.check_freeze(root);freeze=audit.read(root/'alignment_freeze.json')
    assert audit.sha(root/'alignments.json')==freeze['alignments_sha256']
    for item in freeze['predictions']:assert audit.sha(root/item['path'])==item['sha256']
    refs=evaluation_references(root,not original);aliases=audit.read(root/'aliases.json')
    preds={p['source_key']:p for rows in load_predictions(root).values() for p in rows}
    aligns={(r['reference_id'],r['method']):r for r in audit.read(root/'alignments.json')}
    ledger=[];institution_ledger=[]
    for ref in refs:
        for method in METHODS:
            alignment=aligns[ref['id'],method];p=preds.get(alignment['source_key']);detected=p is not None
            values=dict(name=p['name_raw'],birthplace=p['birth_place'],birth_year=p['birth_year'],city=p['birth_city']) if p else dict.fromkeys(FIELDS,'')
            item=dict(reference_id=ref['id'],source_file=ref['source_file'],page_key=ref['page_key'],
                method=method,source_key=alignment['source_key'],detected=detected,
                duplicates=len(alignment['duplicate_keys']),uncertain_alignment=alignment['uncertain'],
                reference=ref['fields'],predicted=values,raw_text=p['raw_text_adjusted'] if p else '',
                alignment_reason=alignment['review_reason'],merged_into=alignment['merged_into'],scores={},causes={})
            for field in FIELDS:
                wrong=wrong_owner(values[field],p,ref,refs,field) if p else False
                state=field_score(ref['fields'][field],values[field],field,aliases,detected,wrong)
                item['scores'][field]=state
                if state in {'correct','absent','source_unreadable'}:cause=state
                elif not detected:cause='segmentation_missed_or_merged_biography'
                elif wrong:cause='segmentation_or_parser_wrong_person'
                else:
                    key=audit.canon(ref['fields'][field]['value']);raw=audit.canon(p['raw_text_adjusted'])
                    # This is a conservative diagnostic, not an alternate accuracy score.
                    cause='parser_field_selection' if key and key in raw else 'recognition_or_reading_order_or_parser; not uniquely separable'
                item['causes'][field]=cause
            institutions=ref['fields']['institutions']
            pp=audit.distinct(p['institutions_raw'],aliases) if p else []
            rr=institutions['value'] if institutions['status']=='readable' else []
            inst=institution_scores(rr,pp,aliases)
            duplicate_institutions=[v for key in alignment['duplicate_keys'] for v in audit.distinct(preds[key]['institutions_raw'],aliases)]
            inst['duplicate_extras']=duplicate_institutions
            inst['predicted_count']+=len(duplicate_institutions)
            inst['extras']+=duplicate_institutions
            inst['exact']=inst['exact'] and not alignment['duplicate_keys']
            inst['reference_status']=institutions['status'];item['institutions']=inst
            item['predicted']['institutions']=pp+duplicate_institutions
            item['scores']['institutions']='source_unreadable' if institutions['status']=='source_unreadable' else (
                'absent' if institutions['status']=='absent' else ('correct' if inst['exact'] and detected else ('missing' if not pp else 'incorrect')))
            item['complete_eligible']=all(ref['fields'][f]['status']=='readable' for f in ['name','birthplace','birth_year','institutions'])
            item['complete_correct']=item['complete_eligible'] and all(item['scores'][f]=='correct' for f in ['name','birthplace','birth_year','institutions'])
            item['education_flags']=p['education']['flags'] if p else []
            item['education_awards']=p['education']['awards'] if p else []
            for state,values2 in [('correct',inst['matches']),('missing',inst['missing']),('incorrect',inst['extras'])]:
                for val in values2:
                    rv=val['reference'] if isinstance(val,dict) else val if state=='missing' else ''
                    pv=val['predicted'] if isinstance(val,dict) else val if state=='incorrect' else ''
                    institution_ledger.append(dict(reference_id=ref['id'],method=method,page_key=ref['page_key'],
                        reference=rv,predicted=pv,status=state,
                        cause='correct' if state=='correct' else 'segmentation_missed_biography' if not p else
                        'parser_institution_scope_or_selection' if audit.canon(rv or pv) in audit.canon(p['raw_text_adjusted']) else
                        'recognition_or_reading_order_or_parser; not uniquely separable'))
            ledger.append(item)
    # Unmatched fragments are evaluated as additional page-level false positives,
    # never stitched into or credited to an aligned person's institution set.
    scope=audit.read(root/'unmatched_predictions_review.json')
    assert scope['reviewed_all'] and all(d['reviewed'] for d in scope['decisions'])
    page_rows={}
    for item in ledger:
        item['unmatched_institution_prediction_count']=0
        page_rows.setdefault((item['page_key'],item['method']),item)
    for d in scope['decisions']:
        assert d['source_key'] not in {r['source_key'] for r in ledger if r['source_key']}
        if d['decision']!='extra_fragment_of_sampled_biography':continue
        values=audit.distinct(preds[d['source_key']]['institutions_raw'],aliases)
        page_rows[d['page_key'],d['method']]['unmatched_institution_prediction_count']+=len(values)
        for value in values:
            institution_ledger.append(dict(reference_id='',method=d['method'],page_key=d['page_key'],
                reference='',predicted=value,status='incorrect_unmatched_fragment',cause='segmentation_unattributed_fragment; no stitching or recovery credit'))
    suffix='_frozen_references' if original else ''
    audit.write(root/f'scored{suffix}.json',ledger);audit.csv_write(root/f'institutions{suffix}.csv',institution_ledger)
    if not original:audit.write(root/'references_adjudicated.json',refs)
    scalar=[]
    for r in ledger:
        for f in FIELDS:
            scalar.append(dict(reference_id=r['reference_id'],method=r['method'],page_key=r['page_key'],field=f,
                reference=r['reference'][f]['value'],predicted=r['predicted'][f],status=r['scores'][f],cause=r['causes'][f]))
    audit.csv_write(root/f'fields{suffix}.csv',scalar)
    audit.csv_write(root/f'biographies{suffix}.csv',[dict(reference_id=r['reference_id'],method=r['method'],page_key=r['page_key'],
        source_key=r['source_key'],detected=r['detected'],duplicates=r['duplicates'],
        complete_eligible=r['complete_eligible'],complete_correct=r['complete_correct'],
        institutions_correct=r['institutions']['correct'],institutions_reference=r['institutions']['reference_count'],
        institutions_predicted=r['institutions']['predicted_count'],institutions_exact=r['scores']['institutions']=='correct') for r in ledger])
    metrics=all_metrics(ledger);audit.write(root/f'metrics{suffix}.json',metrics)
    audit.csv_write(root/f'metrics{suffix}.csv',metrics['estimates'])
    print(json.dumps(metrics['pooled'],indent=2))

def counts(rows,measure):
    if measure in FIELDS:return sum(r['scores'][measure]=='correct' for r in rows),sum(r['reference'][measure]['status']=='readable' for r in rows)
    if measure=='institution_recall':return sum(r['institutions']['correct'] for r in rows if r['institutions']['reference_status']=='readable'),sum(r['institutions']['reference_count'] for r in rows if r['institutions']['reference_status']=='readable')
    if measure=='institution_precision':return sum(r['institutions']['correct'] for r in rows if r['institutions']['reference_status']!='source_unreadable'),sum(r['institutions']['predicted_count'] for r in rows if r['institutions']['reference_status']!='source_unreadable')+sum(r.get('unmatched_institution_prediction_count',0) for r in rows)
    if measure=='institution_exact_set':return sum(r['scores']['institutions']=='correct' for r in rows),sum(r['reference']['institutions']['status']=='readable' for r in rows)
    if measure=='complete_record':return sum(r['complete_correct'] for r in rows),sum(r['complete_eligible'] for r in rows)
    if measure=='biography_detection':return sum(r['detected'] for r in rows),len(rows)
    raise ValueError(measure)

def all_metrics(ledger):
    rng=np.random.default_rng(20261022);estimates=[];pooled={};groups=[('pooled','all',ledger)]
    for level,column in [('pdf','source_file'),('page','page_key')]:
        groups.extend((level,key,[r for r in ledger if r[column]==key]) for key in sorted({r[column] for r in ledger}))
    for level,key,rows in groups:
        for measure in MEASURES:
            stats={m:counts([r for r in rows if r['method']==m],measure) for m in METHODS}
            percentages={m:100*n/d if d else None for m,(n,d) in stats.items()}
            difference=percentages['paddle']-percentages['word'] if all(v is not None for v in percentages.values()) else None
            for method in METHODS:
                n,d=stats[method];estimates.append(dict(level=level,group=key,measure=measure,method=method,
                    correct=n,denominator=d,percent=percentages[method],paddle_minus_word_pp=difference))
            if level=='pooled':
                # Paired pages; independent strata = PDFs, two page draws per stratum.
                strata=sorted({r['source_file'] for r in rows});totals=np.zeros((10000,2,2),dtype=float)
                for file in strata:
                    pages=sorted({r['page_key'] for r in rows if r['source_file']==file});assert len(pages)==2
                    a=np.array([[counts([r for r in rows if r['page_key']==page and r['method']==m],measure) for m in METHODS] for page in pages])
                    draws=rng.integers(0,2,size=(10000,2));totals+=a[draws].sum(axis=1)
                rates=np.divide(100*totals[:,:,0],totals[:,:,1],out=np.full((10000,2),np.nan),where=totals[:,:,1]>0)
                differences=rates[:,1]-rates[:,0]
                pooled[measure]=dict(methods={m:dict(correct=stats[m][0],denominator=stats[m][1],percent=percentages[m],
                    bootstrap_95_percentile_ci=np.nanpercentile(rates[:,i],[2.5,97.5]).tolist()) for i,m in enumerate(METHODS)},
                    paddle_minus_word_pp=difference,paired_bootstrap_95_percentile_ci=np.nanpercentile(differences,[2.5,97.5]).tolist())
    # Also bootstrap the within-PDF difference for each PDF using its two paired pages.
    per_pdf_intervals=[]
    for file in sorted({r['source_file'] for r in ledger}):
        pages=sorted({r['page_key'] for r in ledger if r['source_file']==file})
        for measure in MEASURES:
            a=np.array([[counts([r for r in ledger if r['page_key']==p and r['method']==m],measure) for m in METHODS] for p in pages])
            totals=a[rng.integers(0,2,size=(10000,2))].sum(axis=1)
            rates=np.divide(100*totals[:,:,0],totals[:,:,1],out=np.full((10000,2),np.nan),where=totals[:,:,1]>0)
            per_pdf_intervals.append(dict(source_file=file,measure=measure,paired_bootstrap_95_percentile_ci=np.nanpercentile(rates[:,1]-rates[:,0],[2.5,97.5]).tolist()))
    status_counts={m:{f:dict(Counter(r['scores'][f] for r in ledger if r['method']==m)) for f in FIELDS+['institutions']} for m in METHODS}
    return dict(pooled=pooled,estimates=estimates,per_pdf_intervals=per_pdf_intervals,status_counts=status_counts,
        detection={m:dict(biographies=sum(r['method']==m for r in ledger),detected=sum(r['detected'] for r in ledger if r['method']==m),
            duplicate_records=sum(r['duplicates'] for r in ledger if r['method']==m),unmatched_fragment_institutions=sum(r.get('unmatched_institution_prediction_count',0) for r in ledger if r['method']==m),uncertain_alignments=sum(r['uncertain_alignment'] for r in ledger if r['method']==m)) for m in METHODS},
        bootstrap=dict(seed=20261022,draws=10000,strata='PDF',unit='paired page',interval='percentile 95%; only two pages per stratum; no estimable interval for a single page'))

def main():
    p=argparse.ArgumentParser();p.add_argument('phase',choices=['propose','propose-available','align','score','score-original']);p.add_argument('--root',type=Path,default=audit.ROOT)
    args=p.parse_args();{'propose':propose,'propose-available':lambda r:propose(r,True),'align':align,'score':evaluate,'score-original':lambda r:evaluate(r,True)}[args.phase](args.root)
if __name__=='__main__':main()

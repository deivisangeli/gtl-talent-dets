"""Evaluate frozen three-arm predictions and describe full-document agreement."""
import argparse,copy,json,re
from collections import Counter,defaultdict
from difflib import SequenceMatcher
from itertools import combinations
from functools import lru_cache
from pathlib import Path
import compare_ocr_fields as norm
import score_ocr_fields as oldscore
import repaired_ocr_comparison as a
from repaired_comparison_extract import METHODS,csv_write

# Pure memoization: fixed normalization/alias rules and values are unchanged.
# Large merged records otherwise repeat the same institutional comparisons.
norm.canon=lru_cache(maxsize=131072)(norm.canon)
norm.alias_normalize=lru_cache(maxsize=131072)(norm.alias_normalize)

def predictions(root,available=False):
    return {(d['source_file'],m):a.read(root/'prediction'/m/Path(d['source_file']).stem/'predictions.json') for d in a.read(root/'manifest.json')['documents'] for m in METHODS if not available or (root/'prediction'/m/Path(d['source_file']).stem/'complete.json').exists()}
def propose(root,available=False):
    assert available or (root/'prediction/complete.json').exists()
    preds=predictions(root,available);refs=a.read(root/'references_adjudicated.json');old={(r['reference_id'],r['method']):r for r in a.read(a.FIELD/'alignments.json')}
    proposals=[]
    for ref in refs:
        file=ref['source_file'];name=norm.canon(ref['fields']['name']['value'])
        for method in METHODS:
            if (file,method) not in preds:continue
            rows=preds[file,method];history=old.get((ref['id'],method));lookup={r['source_key']:r for r in rows}
            transferred=None
            if history and history.get('source_key') in lookup:transferred=history['source_key']
            candidates=[]
            for i,row in enumerate(rows):
                if method!='word' and abs(int(row['pdf_page'])-ref['pdf_page'])>1:continue
                first=oldscore.head_name(row);keys=[norm.canon(row['name_raw']),norm.canon(first)]
                sim=max(SequenceMatcher(None,name,key[:len(name)+8],autojunk=False).ratio() for key in keys)
                if name in keys:sim=1.0
                if name and keys[1].startswith(name):sim=max(sim,.995)
                candidates.append((sim,i))
            candidates.sort(reverse=True)
            top=[]
            for sim,i in candidates[:4]:
                row=rows[i];top.append(dict(source_key=row['source_key'],similarity=round(sim,4),head=oldscore.head_name(row),raw_start=row['raw_text_adjusted'][:450],
                    previous=oldscore.head_name(rows[i-1]) if i else '',next=oldscore.head_name(rows[i+1]) if i+1<len(rows) else ''))
            proposals.append(dict(reference_id=ref['id'],reference_name=ref['fields']['name']['value'],page_key=ref['page_key'],source_file=file,method=method,
                historical_alignment=history,transferred_primary=transferred,candidates=top))
    a.write(root/'alignment_proposals.json',proposals)
    print(json.dumps(dict(proposals=len(proposals),transferred=sum(bool(p['transferred_primary']) for p in proposals),repaired_exact=sum(p['method']=='paddle_repaired' and p['candidates'][0]['similarity']==1 for p in proposals))))
def evaluate(root):
    refs=a.read(root/'references_adjudicated.json');aliases=a.read(root/'aliases.json');preds=predictions(root)
    bykey={r['source_key']:r for rows in preds.values() for r in rows}
    review=a.read(root/'alignment_review_new.json');decisions=review['decisions'];assert review['reviewed_all'] and len(decisions)==912
    oldscore.validate_alignment_rows(decisions,expected=912)
    indexed={(r['reference_id'],r['method']):r for r in decisions};ledger=[]
    for ref in refs:
        for method in METHODS:
            alignment=indexed[ref['id'],method];p=bykey.get(alignment.get('source_key'));detected=p is not None
            values=dict(name=p['name_raw'],birthplace=p['birth_place'],birth_year=p['birth_year'],city=p['birth_city']) if p else dict.fromkeys(oldscore.FIELDS,'')
            item=dict(reference_id=ref['id'],source_file=ref['source_file'],page_key=ref['page_key'],method=method,source_key=alignment.get('source_key'),detected=detected,
                duplicates=len(alignment.get('duplicate_keys',[])),uncertain_alignment=alignment.get('uncertain',False),reference=ref['fields'],predicted=values,
                raw_text=p['raw_text_adjusted'] if p else '',alignment_reason=alignment['review_reason'],merged_into=alignment.get('merged_into'),scores={},causes={})
            for field in oldscore.FIELDS:
                wrong=oldscore.wrong_owner(values[field],p,ref,refs,field) if p else False
                state=oldscore.field_score(ref['fields'][field],values[field],field,aliases,detected,wrong);item['scores'][field]=state
                if state in ('correct','absent','source_unreadable'):cause=state
                elif not detected:cause='segmentation_missed_or_merged_biography'
                elif wrong:cause='segmentation_or_parser_wrong_person'
                else:
                    key=norm.canon(ref['fields'][field]['value']);raw=norm.canon(p['raw_text_adjusted'])
                    cause='parser_field_selection' if key and key in raw else 'recognition_or_reading_order_or_parser; not uniquely separable'
                item['causes'][field]=cause
            inst_ref=ref['fields']['institutions'];pp=norm.distinct(p['institutions_raw'],aliases) if p else []
            inst=oldscore.institution_scores(inst_ref['value'] if inst_ref['status']=='readable' else [],pp,aliases)
            duplicate_values=[v for key in alignment.get('duplicate_keys',[]) for v in norm.distinct(bykey[key]['institutions_raw'],aliases)]
            inst.update(reference_status=inst_ref['status'],duplicate_extras=duplicate_values)
            inst['predicted_count']+=len(duplicate_values);inst['extras']+=duplicate_values;inst['exact']=inst['exact'] and not alignment.get('duplicate_keys')
            item['institutions']=inst;item['predicted']['institutions']=pp+duplicate_values
            item['scores']['institutions']='source_unreadable' if inst_ref['status']=='source_unreadable' else 'absent' if inst_ref['status']=='absent' else 'correct' if inst['exact'] and detected else 'missing' if not pp else 'incorrect'
            item['complete_eligible']=all(ref['fields'][f]['status']=='readable' for f in ('name','birthplace','birth_year','institutions'))
            item['complete_correct']=item['complete_eligible'] and all(item['scores'][f]=='correct' for f in ('name','birthplace','birth_year','institutions'))
            item['unmatched_institution_prediction_count']=0;ledger.append(item)
    scope=a.read(root/'unmatched_predictions_review_new.json');assert scope['reviewed_all']
    used={r['source_key'] for r in ledger if r['source_key']};page_rows={(r['page_key'],r['method']):r for r in ledger}
    for d in scope['decisions']:
        assert d['reviewed'] and d['source_key'] not in used
        if d['decision']=='extra_fragment_of_sampled_biography':page_rows[d['page_key'],d['method']]['unmatched_institution_prediction_count']+=len(norm.distinct(bykey[d['source_key']]['institutions_raw'],aliases))
    a.write(root/'scored.json',ledger)
    metrics={};flat=[]
    for left,right in combinations(METHODS,2):
        pair=[dict(r,method='word' if r['method']==left else 'paddle') for r in ledger if r['method'] in (left,right)]
        result=oldscore.all_metrics(pair);metrics[left+'__'+right]=dict(arm_a=left,arm_b=right,**result)
        for x in result['estimates']:
            row=dict(x);row['method']=left if row['method']=='word' else right;row['comparison']=left+'__'+right;row['difference_pp_b_minus_a']=row.pop('paddle_minus_word_pp');flat.append(row)
    a.write(root/'metrics.json',metrics);csv_write(root/'metrics.csv',flat)
    csv_write(root/'biographies.csv',[dict(reference_id=r['reference_id'],page_key=r['page_key'],method=r['method'],source_key=r['source_key'],detected=r['detected'],duplicates=r['duplicates'],complete_eligible=r['complete_eligible'],complete_correct=r['complete_correct'],institutions_correct=r['institutions']['correct'],institutions_reference=r['institutions']['reference_count'],institutions_predicted=r['institutions']['predicted_count'],institutions_exact=r['scores']['institutions']=='correct') for r in ledger])
    csv_write(root/'fields.csv',[dict(reference_id=r['reference_id'],page_key=r['page_key'],method=r['method'],field=f,reference=r['reference'][f]['value'],predicted=r['predicted'][f],status=r['scores'][f],cause=r['causes'][f]) for r in ledger for f in oldscore.FIELDS])
    institution_rows=[dict(reference_id=r['reference_id'],page_key=r['page_key'],method=r['method'],status=status,reference=v['reference'] if isinstance(v,dict) else v if status=='missing' else '',predicted=v['predicted'] if isinstance(v,dict) else v if status=='extra' else '') for r in ledger for status,values in [('correct',r['institutions']['matches']),('missing',r['institutions']['missing']),('extra',r['institutions']['extras'])] for v in values]
    for d in scope['decisions']:
        if d['decision']=='extra_fragment_of_sampled_biography':
            institution_rows.extend(dict(reference_id='',page_key=d['page_key'],method=d['method'],status='extra_unmatched_fragment',reference='',predicted=value) for value in norm.distinct(bykey[d['source_key']]['institutions_raw'],aliases))
    csv_write(root/'institutions.csv',institution_rows)
    for method in METHODS:
        rows=[r for r in ledger if r['method']==method]
        print(method,{measure:oldscore.counts(rows,measure) for measure in oldscore.MEASURES})
def full_comparison(root):
    preds=predictions(root);aliases=a.read(root/'aliases.json');totals=[];links=[];unmatched=[];duplicates=[]
    def index(rows,fields):
        result=defaultdict(list)
        for r in rows:
            key=tuple(norm.canon(r[f]) for f in fields)
            if all(key):result[key].append(r)
        return result
    for (file,method),rows in preds.items():
        totals.append(dict(source_file=file,method=method,records=len(rows),name_present=sum(bool(r['name_raw'].strip()) for r in rows),birthplace_present=sum(bool(r['birth_place'].strip()) for r in rows),birth_year_present=sum(bool(re.fullmatch(r'\d{4}',r['birth_year'])) for r in rows),education_institutions_present=sum(bool(r['institutions_raw']) for r in rows)))
        for key,group in index(rows,('name_raw','birth_year')).items():
            if len(group)>1:
                for row in group:duplicates.append(dict(source_file=file,method=method,source_key=row['source_key'],name_raw=row['name_raw'],birth_year=row['birth_year'],group_size=len(group),status='duplicate_candidate_not_scan_verified'))
    for file in sorted({f for f,m in preds}):
        for left,right in combinations(METHODS,2):
            lrows,rrows=preds[file,left],preds[file,right];used_l=set();used_r=set()
            for fields in [('name_raw','birth_year'),('name_raw','birth_place')]:
                li,ri=index(lrows,fields),index(rrows,fields)
                for key in sorted(li.keys()&ri.keys()):
                    if len(li[key])!=1 or len(ri[key])!=1:continue
                    l,r=li[key][0],ri[key][0]
                    if l['source_key'] in used_l or r['source_key'] in used_r:continue
                    if l['birth_year'] and r['birth_year'] and l['birth_year']!=r['birth_year']:continue
                    used_l.add(l['source_key']);used_r.add(r['source_key'])
                    inst_l=norm.distinct(l['institutions_raw'],aliases);inst_r=norm.distinct(r['institutions_raw'],aliases)
                    inst=oldscore.institution_scores(inst_l,inst_r,aliases)
                    links.append(dict(source_file=file,comparison=left+'__'+right,arm_a=left,arm_b=right,key_a=l['source_key'],key_b=r['source_key'],link_rule='unique exact normalized '+'+'.join(fields),
                        name_a=l['name_raw'],name_b=r['name_raw'],birthplace_a=l['birth_place'],birthplace_b=r['birth_place'],birth_year_a=l['birth_year'],birth_year_b=r['birth_year'],
                        institutions_a=json.dumps(inst_l,ensure_ascii=False),institutions_b=json.dumps(inst_r,ensure_ascii=False),
                        name_agreement=norm.equivalent(l['name_raw'],r['name_raw'],'name',aliases),birthplace_agreement=bool(l['birth_place'] and r['birth_place']) and norm.equivalent(l['birth_place'],r['birth_place'],'birthplace',aliases),
                        birth_year_agreement=bool(l['birth_year'] and r['birth_year']) and l['birth_year']==r['birth_year'],institution_set_agreement=inst['exact'],agreement_is_not_accuracy=True))
            for method,rows,used in [(left,lrows,used_l),(right,rrows,used_r)]:
                for r in rows:
                    if r['source_key'] not in used:unmatched.append(dict(source_file=file,comparison=left+'__'+right,method=method,source_key=r['source_key'],name_raw=r['name_raw'],birth_year=r['birth_year'],status='unmatched_or_ambiguous; no forced linkage'))
    csv_write(root/'full_document_counts.csv',totals);csv_write(root/'full_document_comparison.csv',links);csv_write(root/'full_document_unmatched.csv',unmatched)
    csv_write(root/'duplicate_candidates.csv',duplicates)
    summary=dict(totals={m:sum(x['records'] for x in totals if x['method']==m) for m in METHODS},matched_pairs=dict(Counter(x['comparison'] for x in links)),unmatched=dict(Counter(x['comparison'] for x in unmatched)),duplicate_candidates=len(duplicates),policy='Conservative pairwise, one-to-one exact identity proposals; no fuzzy auto-linking or reference inputs. Agreement is not scan-verified accuracy. Identical-name/year records are candidates, not confirmed duplicates.')
    a.write(root/'full_document_summary.json',summary);print(json.dumps(summary))
    agreement_summary(root)
def agreement_summary(root):
    with (root/'full_document_comparison.csv').open(encoding='utf-8-sig',newline='') as f:
        import csv
        links=list(csv.DictReader(f))
    result=[]
    for pair in sorted({r['comparison'] for r in links}):
        paired=[r for r in links if r['comparison']==pair]
        groups=[('pooled','all',paired)]+[('pdf',file,[r for r in paired if r['source_file']==file]) for file in sorted({r['source_file'] for r in paired})]
        for scope,file,rs in groups:
            for field in ('name','birthplace','birth_year','institutions'):
                present=lambda r,arm:bool(json.loads(r['institutions_'+arm])) if field=='institutions' else bool(r[field+'_'+arm].strip())
                flag='institution_set_agreement' if field=='institutions' else field+'_agreement'
                eligible=[r for r in rs if present(r,'a') or present(r,'b')]
                matches=sum(r[flag]=='True' for r in eligible)
                result.append(dict(comparison=pair,scope=scope,source_file=file,field=field,agreement=matches,denominator_either_present=len(eligible),
                    percent=100*matches/len(eligible) if eligible else None,linked_records=len(rs),both_present=sum(present(r,'a') and present(r,'b') for r in rs),both_missing=len(rs)-len(eligible),
                    interpretation='Agreement conditional on conservative identity linkage; not scan-verified accuracy. Name agreement is required by linkage.'))
    csv_write(root/'full_document_agreement.csv',result)
    a.write(root/'full_document_agreement.json',dict(policy='Denominator: linked pairs with either source value present; both-missing pairs reported separately. Name agreement is tautological under exact-name linkage. Institution agreement requires complete sets under the frozen aliases.',estimates=result))
    return result
def main():
    p=argparse.ArgumentParser();p.add_argument('phase',choices=['propose','propose-available','score','full','agreement']);p.add_argument('--root',type=Path,default=a.ROOT);args=p.parse_args()
    {'propose':propose,'propose-available':lambda r:propose(r,True),'score':evaluate,'full':full_comparison,'agreement':agreement_summary}[args.phase](args.root)
if __name__=='__main__':main()

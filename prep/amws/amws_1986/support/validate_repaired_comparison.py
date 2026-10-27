"""Independent delivery checks for the complete repaired OCR comparison."""
import argparse,csv,json,zipfile,hashlib
from pathlib import Path
from itertools import combinations
from collections import Counter
import repaired_ocr_comparison as a
import repaired_comparison_score as scoring
from repaired_comparison_extract import METHODS

def rows(p):
    with p.open(encoding='utf-8-sig',newline='') as f:return list(csv.DictReader(f))
def validate_agreements(root):
    links=rows(root/'full_document_comparison.csv');summaries=a.read(root/'full_document_agreement.json')['estimates']
    assert len(summaries)==120
    for row in summaries:
        rs=[r for r in links if r['comparison']==row['comparison'] and (row['scope']=='pooled' or r['source_file']==row['source_file'])]
        field=row['field'];flag='institution_set_agreement' if field=='institutions' else field+'_agreement'
        present=lambda r,arm:bool(json.loads(r['institutions_'+arm])) if field=='institutions' else bool(r[field+'_'+arm].strip())
        eligible=[r for r in rs if present(r,'a') or present(r,'b')]
        assert row['agreement']==sum(r[flag]=='True' for r in eligible)
        assert row['denominator_either_present']==len(eligible) and row['both_missing']==len(rs)-len(eligible)
        assert row['both_present']==sum(present(r,'a') and present(r,'b') for r in rs)
        assert row['percent']==100*row['agreement']/len(eligible)
    return dict(agreement_rows_verified=len(summaries),full_document_agreement_summary_verified=True)
def validate(root,out=None):
    m=a.read(root/'manifest.json');repair,corpus,exceptions=a.modules(root)
    reviews=a.read(root/'paragraph_decisions.json');contexts=a.read(root/'exception_contexts.json');by_page={}
    assert reviews['all_exception_blocks_reviewed'] and len(reviews['decisions'])==122
    for c in contexts:
        exceptions.decision_valid(c,reviews['decisions'][c['block_id']]);by_page.setdefault((c['source_file'],c['pdf_page_number']),[]).append(c)
    for x in m['sources']+m['raw_inputs']:assert a.sha(x['path'])==x['sha256']
    for x in m['frozen_parser']:assert a.sha(root/'frozen/repo'/x['path'])==x['sha256']
    for x in m['frozen_repair']:assert a.sha(root/x['path'])==x['sha256']
    for x in m['previous_deliveries']:assert a.sha(x['path'])==x['sha256']
    with zipfile.ZipFile(a.CHAT/'outputs/amws_ocr_comparison_20261019.zip') as prior:
        for x in m['raw_inputs']:
            relative=Path(x['path']).relative_to(a.OLD/'ocr').as_posix()
            assert hashlib.sha256(prior.read('amws_ocr_comparison_20261019/ocr/'+relative)).hexdigest()==x['sha256']
    page_count=0
    for d in m['documents']:
        for page in range(1,d['page_count']+1):
            file=d['source_file'];src=corpus.page_paths(a.OLD/'ocr',file,page);first=corpus.page_paths(root/'first_pass',file,page)
            original=a.read(src['json']);txt=src['txt'].read_bytes();md=src['md'].read_bytes()
            blocks,audits,t,mk=repair.repair_page(original,txt,md);first_j=a.read(first['json'])
            assert first_j['original_record']==original and first_j['corrected_prediction']['parsing_res_list']==blocks
            assert first['txt'].read_bytes()==t and first['md'].read_bytes()==mk and first_j['repair_audit']==audits
            derived,exports,_=exceptions.derive(root/'first_pass',file,page,by_page.get((file,page),[]),reviews['decisions'])
            for target in ([root] if out is None else [root,out]):
                actual=corpus.page_paths(target/'repaired',file,page)
                assert a.read(actual['json'])==derived
                for ext in ('txt','md'):assert actual[ext].read_bytes()==exports[ext]
            page_count+=1
    assert page_count==1360
    predictions=scoring.predictions(root);assert len(predictions)==27
    for (file,method),rs in predictions.items():
        stage=root/'prediction'/method/Path(file).stem;cp=a.read(stage/'complete.json')
        assert cp['records']==len(rs)
        for name,digest in cp['outputs'].items():assert a.sha(stage/name)==digest
        if method=='paddle':assert cp['historical_raw_predictions_identical'] and rs==a.read(a.FIELD/'prediction/paddle'/Path(file).stem/'predictions.json')
        if out is not None:assert a.read(out/'prediction'/method/Path(file).stem/'predictions.json')==rs
    refs=a.read(root/'references_adjudicated.json');assert len(refs)==304
    for name in ('references.json','references_adjudicated.json','reference_adjudications.json','aliases.json'):
        assert a.sha(root/name)==a.sha(a.FIELD/name)
    review=a.read(root/'alignment_review_new.json');assert review['reviewed_all'];scoring.oldscore.validate_alignment_rows(review['decisions'],912)
    ledger=a.read(root/'scored.json');assert Counter(r['method'] for r in ledger)==Counter({m:304 for m in METHODS})
    metrics=a.read(root/'metrics.json')
    for left,right in combinations(METHODS,2):
        pair=[dict(r,method='word' if r['method']==left else 'paddle') for r in ledger if r['method'] in (left,right)]
        expected={k:v for k,v in metrics[left+'__'+right].items() if k not in ('arm_a','arm_b')}
        assert scoring.oldscore.all_metrics(pair)==expected,'Metrics are not reproducible'
    # The unchanged raw predictions/alignments must reproduce all historical point estimates.
    old=a.read(a.FIELD/'metrics.json')
    for measure in scoring.oldscore.MEASURES:
        assert metrics['word__paddle']['pooled'][measure]['methods']['paddle']['correct']==old['pooled'][measure]['methods']['paddle']['correct']
        assert metrics['word__paddle']['pooled'][measure]['methods']['paddle']['denominator']==old['pooled'][measure]['methods']['paddle']['denominator']
    links=rows(root/'full_document_comparison.csv');unmatched=rows(root/'full_document_unmatched.csv')
    for left,right in combinations(METHODS,2):
        pair=left+'__'+right;ls=[r for r in links if r['comparison']==pair]
        assert len({r['key_a'] for r in ls})==len({r['key_b'] for r in ls})==len(ls)
        for arm in (left,right):
            n=sum(len(rs) for (file,method),rs in predictions.items() if method==arm)
            assert len(ls)+sum(r['comparison']==pair and r['method']==arm for r in unmatched)==n
    result=dict(complete=True,validated_at=a.now(),source_files_unchanged=len(m['sources']),raw_files_unchanged=len(m['raw_inputs']),
        pages_independently_recomputed=page_count,extraction_arms=3,extraction_documents=27,raw_predictions_reproduce_history=True,
        references=304,reviewed_alignments=912,metrics_pairs_recomputed=3,all_metric_groups_per_pair=28,
        references_and_aliases_unchanged=True,parser_and_repair_hashes_unchanged=True,previous_archives_unchanged=True,
        full_linkage_one_to_one=True,unmatched_accounting_complete=True,test_suites=dict(paragraph_repair=33,field_scoring_and_continuations=21,three_arm_regressions=8,manual_word_evidence_regressions=7),
        independent_human_verification=False,browser_visual_preview='Unavailable: local browser use disabled in Work settings')
    result.update(validate_agreements(root))
    a.write((out or root)/'delivery_validation.json',result);print(json.dumps(result),flush=True)
def main():
    p=argparse.ArgumentParser();p.add_argument('--root',type=Path,default=a.ROOT);p.add_argument('--out',type=Path);args=p.parse_args();validate(args.root,args.out)
if __name__=='__main__':main()

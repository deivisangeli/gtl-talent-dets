"""Full-document extraction; never reads audit references or identity decisions."""
import argparse,csv,hashlib,json,re,shutil,subprocess,sys
from collections import Counter
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import repaired_ocr_comparison as a

METHODS=('word','paddle','paddle_repaired')
def contract_hash(value):return hashlib.sha256(json.dumps(value,sort_keys=True).encode('utf8')).hexdigest()
def csv_write(path,rows):
    with path.open('w',encoding='utf-8-sig',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
def complete_valid(stage,inputs):
    p=stage/'complete.json'
    if not p.exists():return False
    c=a.read(p)
    return c['input_manifest']==inputs and all((stage/k).exists() and a.sha(stage/k)==v for k,v in c['outputs'].items())
def word_inputs(root,manifest,folder):
    import manual_word_evidence as w
    folder.mkdir(parents=True,exist_ok=True)
    for d in manifest['documents']:
        source=Path(d['baseline']['path']);assert a.sha(source)==d['docx_sha256']
        ps,bad,unused=w.read_word(source);assert not bad and not unused,(source,bad,unused)
        old=a.read(a.OLD/'baseline'/f"{Path(d['source_file']).stem}.json")['paragraphs']
        assert len(ps)==len(old) and all(p['literal_text']==q['text'] for p,q in zip(ps,old))
        dest=folder/f"{source.stem}.json"
        a.write(dest,dict(source=str(source),source_sha256=d['docx_sha256'],reader_sha256=a.sha(Path(w.__file__)),paragraphs=ps,
            literal_runs_conserved=True,generated_labels_included=True,unsupported_labels=bad,label_only_paragraphs=unused))
        a.put(folder/f'{source.stem}.txt','\n\n'.join(f"[Original Word paragraph {p['index']}]\n{p['text']}" for p in ps).encode('utf8'))
def predict(root,paddle_input_dir=None,word_input_dir=None,workers=4):
    manifest=a.read(root/'manifest.json');freeze=a.read(root/'repair_freeze.json')
    assert a.sha(root/'paragraph_decisions.json')==freeze['decisions_sha256']
    assert a.sha(root/'visual_review.json')==freeze['visual_review_sha256']
    assert len(freeze['repaired_pages'])==1360
    for x in freeze['repaired_pages']:assert a.sha(root/x['path'])==x['sha256']
    frozen=root/'frozen/repo'
    for item in manifest['frozen_parser']:assert a.sha(frozen/item['path'])==item['sha256']
    paddle_input_dir=Path(paddle_input_dir or root/'repaired');word_input_dir=Path(word_input_dir or root/'word_inputs')
    word_inputs(root,manifest,word_input_dir)
    sys.path.insert(0,str(frozen/'prep/amws/education'))
    import education_regex as education,postsecondary_qualifications as additional
    driver=root/'frozen/repair/extract_ocr_audit.R'
    contract=dict(adapter_version=1,adapter_sha256=a.sha(Path(__file__)),word_reader_sha256=a.sha(Path(__file__).with_name('manual_word_evidence.py')),
        driver_sha256=a.sha(driver),parser=manifest['frozen_parser'],repair_decisions_sha256=freeze['decisions_sha256'],
        references_used=False,manual_prediction_corrections=False,paragraph_repair_frozen_before_extraction=True,
        body_labels=['text','paragraph_title','reference','reference_content'])
    saved=root/'extraction_contract.json'
    if saved.exists():assert a.read(saved)==contract,'Extractor changed: create an explicitly versioned run'
    else:a.write(saved,contract)
    adapters=root/'frozen/adapters';adapters.mkdir(exist_ok=True)
    for file in (Path(__file__),Path(__file__).with_name('manual_word_evidence.py')):shutil.copy2(file,adapters/file.name)
    def one(d,method):
        stem=Path(d['source_file']).stem;stage=root/'prediction'/method/stem;stage.mkdir(parents=True,exist_ok=True)
        blocks=[];inputs=[]
        if method=='word':
            path=word_input_dir/f'{stem}.json';baseline=a.read(path);inputs.append(dict(path=str(path),sha256=a.sha(path)))
            for p in baseline['paragraphs']:blocks.append(dict(method=method,source_file=d['source_file'],pdf_page='',block_index=p['index'],block_status='body',text=p['text'],label='word_paragraph',bbox=''))
        else:
            base=a.OLD/'ocr' if method=='paddle' else paddle_input_dir
            for page in range(1,d['page_count']+1):
                path=base/stem/f'page_{page:04d}.json';j=a.read(path);inputs.append(dict(path=str(path),sha256=a.sha(path)))
                assert j['source_file']==d['source_file'] and j['pdf_page_number']==page
                prediction=j['prediction'] if method=='paddle' else j['corrected_prediction']
                for idx,b in sorted(enumerate(prediction['parsing_res_list']),key=lambda x:(x[1].get('block_order',x[0]),x[0])):
                    text=b.get('block_content','');label=b.get('block_label','');body=label in contract['body_labels'] and bool(text.strip())
                    blocks.append(dict(method=method,source_file=d['source_file'],pdf_page=page,block_index=f'{page}:{idx}',block_status='body' if body else 'excluded_layout',text=text,label=label,bbox=json.dumps(b.get('block_bbox'))))
        identity=dict(inputs=inputs,contract=contract)
        if complete_valid(stage,identity):return dict(method=method,source_file=d['source_file'],records=a.read(stage/'complete.json')['records'],resumed=True)
        a.write(stage/'inputs.json',identity);csv_write(stage/'blocks.csv',blocks)
        parsed_contract=stage/'parsed_checkpoint.json';input_hash=contract_hash(identity)
        valid_parsed=parsed_contract.exists() and a.read(parsed_contract).get('input_hash')==input_hash and (stage/'parsed.csv').exists() and a.sha(stage/'parsed.csv')==a.read(parsed_contract)['sha256']
        if not valid_parsed:
            pending=stage/'parsed.pending.csv'
            with (stage/'extract.log').open('w',encoding='utf8') as log:
                subprocess.run([r'C:\Program Files\R\R-4.5.2\bin\Rscript.exe',str(driver),str(frozen),str(stage/'blocks.csv'),str(pending)],stdout=log,stderr=subprocess.STDOUT,check=True)
            pending.replace(stage/'parsed.csv');pending.with_name(pending.name+'.corrections.csv').replace(stage/'parsed.csv.corrections.csv')
            a.write(parsed_contract,dict(input_hash=input_hash,sha256=a.sha(stage/'parsed.csv')))
        csv.field_size_limit(10_000_000)
        with (stage/'parsed.csv').open(encoding='utf-8-sig',newline='') as f:records=list(csv.DictReader(f))
        for row in records:
            year=int(row['birth_year']) if re.fullmatch(r'\d{4}',row['birth_year']) else None
            e=education.extract(row['raw_text_adjusted'],year,1986);awards=e['degrees']+additional.extra_awards(row['raw_text_adjusted'],year,1986)
            row['education']=dict(status=e['status'],flags=e['flags'],awards=awards,rule_version=education.RULE_VERSION,additional_version=additional.VERSION)
            row['institutions_raw']=[x['institution_as_written'] for x in awards if x['institution_as_written']]
        a.write(stage/'predictions.json',records)
        historical_equal=None
        if method=='paddle':
            old=a.read(a.FIELD/'prediction/paddle'/stem/'predictions.json');historical_equal=records==old
            assert historical_equal,'Raw arm did not reproduce historical extraction: '+stem
        a.write(stage/'complete.json',dict(input_manifest=identity,records=len(records),historical_raw_predictions_identical=historical_equal,
            outputs={p:a.sha(stage/p) for p in ('predictions.json','parsed.csv','blocks.csv')},completed_at=a.now()))
        return dict(method=method,source_file=d['source_file'],records=len(records),resumed=False,historical_raw_predictions_identical=historical_equal)
    jobs=[(d,m) for d in manifest['documents'] for m in METHODS]
    result=[]
    with ThreadPoolExecutor(max_workers=workers) as pool:
        futures=[pool.submit(one,*job) for job in jobs]
        for future in futures:
            row=future.result();result.append(row);print(json.dumps(row),flush=True)
    a.write(root/'prediction/complete.json',dict(completed_at=a.now(),documents=result,totals=dict(Counter({m:sum(x['records'] for x in result if x['method']==m) for m in METHODS}))))
def main():
    p=argparse.ArgumentParser();p.add_argument('--root',type=Path,default=a.ROOT);p.add_argument('--paddle-input-dir',type=Path);p.add_argument('--word-input-dir',type=Path);p.add_argument('--workers',type=int,default=4)
    args=p.parse_args();predict(args.root,args.paddle_input_dir,args.word_input_dir,args.workers)
if __name__=='__main__':main()

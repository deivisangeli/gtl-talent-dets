"""Run the AMWS supplement stages; manual scan ledgers are required to publish."""
import argparse,json,os,subprocess,sys
from pathlib import Path

HERE=Path(__file__).resolve().parent;REPO=HERE.parents[2]
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('phase',choices=['prepare','stage','publish'])
p.add_argument('--rscript',default='Rscript');p.add_argument('--xlsx-python',default=sys.executable)
p.add_argument('--stage',type=Path);a=p.parse_args()
expression='source(commandArgs(TRUE)[1]);cat(jsonlite::toJSON(list(root=TALENT_DETS_DATA_DIR,input=AMWS_OCR_REVIEWED_INPUT,stage=AMWS_OCR_SUPPLEMENT_STAGE),auto_unbox=TRUE))'
config=json.loads(subprocess.check_output([a.rscript,'-e',expression,str(REPO/'paths.R')],text=True))
stage=a.stage or Path(config['stage']);root=Path(config['root'])
def run(args):subprocess.run([str(x) for x in args],check=True,env=os.environ.copy())
def python(script,command):run([sys.executable,HERE/script,command,'--data-root',root,'--stage',stage])
if a.phase=='prepare':
    python('12_publish_ocr_supplement.py','baseline')
    python('ocr_supplement.py','prepare')
    run([a.rscript,HERE/'11_parse_ocr_supplement.R',REPO,stage,'patterns'])
if a.phase in ('prepare','stage'):
    python('ocr_supplement.py','segment')
    run([a.rscript,HERE/'11_parse_ocr_supplement.R',REPO,stage])
    run([a.rscript,HERE/'13_normalize_ocr_dates.R',REPO,stage])
    python('12_publish_ocr_supplement.py','build')
    python('ocr_supplement.py','audit')
    print('Staged CSVs and review queues are ready. Complete explicit scan/duplicate decisions before publish.')
else:
    reviews=json.loads((stage/'audit_reviews.json').read_text(encoding='utf-8'))
    assert len(reviews)==200,'Complete the 200-biography scan audit before publication'
    python('12_publish_ocr_supplement.py','build')
    summary=json.loads((stage/'summary.json').read_text(encoding='utf-8'))
    assert summary['pending_duplicates']==0,'Resolve duplicate candidates before publication'
    sys.path.insert(0,str(HERE));from importlib import import_module
    publisher=import_module('12_publish_ocr_supplement')
    for rel in publisher.FILES+[publisher.COMBINED]:
        csv_path=stage/'candidates'/rel;reference=stage/'baseline'/Path(rel).with_suffix('.xlsx')
        args=[a.xlsx_python,HERE/'support/export_ocr_workbooks_streaming.py',csv_path,csv_path.with_suffix('.xlsx'),csv_path.stem]
        if reference.exists():args.extend(['--reference',reference])
        run(args)
    python('12_publish_ocr_supplement.py','validate')
    python('12_publish_ocr_supplement.py','publish')

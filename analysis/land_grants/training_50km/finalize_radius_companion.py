"""Finalize local provenance only after successful numerical, visual and Drive checks."""
from pathlib import Path
import os,json,hashlib,shutil,datetime
import pandas as pd
root=Path(os.environ['TALENT_DETS_DATA_DIR'])
repo=Path(__file__).resolve().parents[3]
dest=root/'results/land_grants/companions/amws_training_radius_companion_20261014'
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
v=json.loads((dest/'independent_validation.json').read_text())
p=json.loads((dest/'page_notes_and_validation.json').read_text(encoding='utf-8'))
drive=json.loads((dest/'drive_publication_audit.json').read_text(encoding='utf-8'))
assert v['passed'] and p['passed'] and p['page_count']==12
assert all(drive[k] for k in ('inserted_only_url','date_chip_preserved','surrounding_content_preserved'))
assert sha(dest/'college_allocation_training_50km_100km_companion_20261014.pdf')==p['pdf_sha256']
snap=dest/'code_snapshot';snap.mkdir(exist_ok=True)
source_files=[repo/'prep/land_grants'/n for n in ('06_build_amws_training_100km_geography.R','07_build_amws_training_100km_samples.R')]
source_files += [Path(__file__).parent/n for n in ('run_100km_event_studies.R','validate_100km_companion.R','build_radius_companion.py','finalize_radius_companion.py','RADIUS_COMPANION.md')]
for path in source_files:shutil.copy2(path,snap/path.name)
shutil.copy2(Path(__file__).parent/'RADIUS_COMPANION.md',dest/'README.md')
tables={k:[] for k in ('model_status','warnings','dynamic_att','pretrend_test_diagnostics','event_time_support')}
for balance in ('relative','calendar'):
  for radius in (50,100):
    stem=f'amws_training_{radius}km_{"catchments" if balance=="relative" else "calendar"}_no_reuse_20261014'
    result=root/'results/land_grants/event_studies'/stem
    for name in tables:
      path=result/f'{name}.csv'
      if not path.exists():continue
      z=pd.read_csv(path);z.insert(0,'radius_km',radius);z.insert(0,'balance',balance);tables[name].append(z)
for name,frames in tables.items():pd.concat(frames,ignore_index=True).to_csv(dest/f'combined_{name}.csv',index=False)
status=pd.read_csv(dest/'combined_model_status.csv')
assert len(status)==32 and status.ok.all()
checks={str(path.relative_to(dest)).replace('\\','/'):sha(path) for path in dest.rglob('*') if path.is_file() and path.name!='completion_manifest.json'}
marker={'status':'complete','published_at_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'pdf_sha256':p['pdf_sha256'],'reused_50km_models':16,'new_100km_models':16,'numerical_validation_passed':True,'all_12_pages_rendered_with_poppler_and_visually_reviewed':True,'text_policy':'Section/subsection titles and descriptive notes below figures only; essential labels inside figures','drive':drive,'source_manifests':p['source_checksums'],'code_checksums':{str(f):sha(f) for f in source_files},'output_checksums':checks}
(dest/'completion_manifest.json').write_text(json.dumps(marker,indent=2,ensure_ascii=False),encoding='utf-8')
print(json.dumps({'status':'complete','pdf_sha256':p['pdf_sha256'],'pages':12,'models':32,'url':drive['pdf_url']}))

"""Write completion marker last after numerical, visual and Drive verification."""
from pathlib import Path
import os,json,hashlib,shutil,datetime
root=Path(os.environ['TALENT_DETS_DATA_DIR'])
repo=Path(__file__).resolve().parents[3]
stamp='amws_county_training_location_20261014'
dest=root/'results/land_grants/companions'/stamp
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
numerical=json.loads((dest/'independent_validation.json').read_text())
pages=json.loads((dest/'page_notes_and_validation.json').read_text(encoding='utf-8'))
drive=json.loads((dest/'drive_publication_audit.json').read_text(encoding='utf-8'))
assert numerical['passed'] and pages['passed'] and pages['page_count']==12
assert drive['our_requests_changed_only_companion_url'] and drive['date_chip_preserved'] and drive['link_and_placement_verified']
pdf=dest/'college_allocation_county_training_location_50km_100km_20261014.pdf'
assert sha(pdf)==pages['pdf_sha256']==drive['pdf_sha256'] and pdf.stat().st_size==drive['pdf_bytes']
source_files=[repo/'prep/land_grants'/n for n in ('08_build_amws_county_training_location.R','amws_training_destination_helpers.R')]
source_files += [Path(__file__).parent/n for n in ('run_county_training_location.R','validate_county_training_location.R','build_county_training_companion.py','finalize_county_training_companion.py','COUNTY_TRAINING_LOCATION.md')]
snap=dest/'code_snapshot';snap.mkdir(exist_ok=True)
for path in source_files:shutil.copy2(path,snap/path.name)
shutil.copy2(Path(__file__).parent/'COUNTY_TRAINING_LOCATION.md',dest/'README.md')
res=root/'results/land_grants/event_studies'/stamp
model_validation=json.loads((res/'validation_report.json').read_text())
assert model_validation['passed'] and model_validation['successful_models']==32
files=[p for p in dest.rglob('*') if p.is_file() and p.name!='completion_manifest.json']
checks={str(p.relative_to(dest)).replace('\\','/'):sha(p) for p in files}
marker={'status':'complete','published_at_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),'unit':'original county c × birth cohort','numerator_universe':'Full eligible US-born AMWS pool, regardless of birthplace','source_entries_eligible':130730,'target_counties':168,'models_estimated':32,'all_12_pages_poppler_rendered_and_visually_reviewed':True,'text_policy':pages['text_policy'],'numerical_validation':numerical,'model_validation':model_validation,'pdf_sha256':sha(pdf),'drive_publication':drive,'source_manifests':pages['source_manifests'],'code_checksums':{str(p):sha(p) for p in source_files},'output_checksums':checks}
(dest/'completion_manifest.json').write_text(json.dumps(marker,indent=2,ensure_ascii=False),encoding='utf-8')
print(json.dumps({'status':'complete','models':32,'pdf_url':drive['pdf_url'],'county_level':True,'all_checks_passed':True}))

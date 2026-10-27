"""Rebuild corrected observations, conversions and bounded annual county panels."""
import argparse,json,os,subprocess,sys,hashlib
from pathlib import Path
from datetime import datetime,timezone
HERE=Path(__file__).resolve().parent

def sha(path):
    h=hashlib.sha256()
    with path.open('rb') as f:
        for b in iter(lambda:f.read(2**20),b''):h.update(b)
    return h.hexdigest()

def main():
    p=argparse.ArgumentParser();p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',str(Path.home()/'Globtalent Dropbox/gtl_talent_dets'))));root=p.parse_args().data_root.resolve()
    raw=root/'Data/raw/faculty_data';initial={str(f.relative_to(raw)):sha(f) for f in raw.rglob('*') if f.is_file()}
    snapshots=sorted((root/'Data/faculty_data_releases').glob('before_corrections_*'))
    if not snapshots:raise RuntimeError('A protected pre-correction release snapshot is required before replacing observed outputs.')
    snapshot=json.loads((snapshots[0]/'snapshot_manifest.json').read_text(encoding='utf-8'))
    assert initial==snapshot['raw_input_sha256'],'Raw archive differs from pre-correction snapshot'
    commands=[['make_table_mapping.py','--raw',str(raw)],['build_faculty_panels.py','--data-root',str(root)],
        ['validate_saved_faculty_panels.py','--data-root',str(root)],['interpolate_county_faculty.py','--data-root',str(root)],
        ['estimate_faculty_headcounts.py','--data-root',str(root)],['test_faculty_panels.py'],['test_faculty_estimation.py'],['test_county_interpolation.py']]
    for cmd in commands:
        print('Running',cmd[0],flush=True);subprocess.run([sys.executable,str(HERE/cmd[0]),*cmd[1:]],check=True)
    assert initial=={str(f.relative_to(raw)):sha(f) for f in raw.rglob('*') if f.is_file()}
    inter=root/'Data/intermediary/faculty_data';out=root/'Data/processed/faculty_data'
    outputs=[p for folder in [inter,out] for p in folder.rglob('*') if p.is_file() and p.name!='release_manifest.json']
    reports=root/'Data/raw/faculty_data_source_reports'
    manifest=dict(completed_at=datetime.now(timezone.utc).isoformat(),passed=True,raw_inputs_unchanged_from_snapshot=True,
        original_snapshot=str(snapshots[0]),raw_input_sha256=initial,output_sha256={str(p.relative_to(root)):sha(p) for p in outputs},
        source_evidence_sha256={str(p.relative_to(root)):sha(p) for p in reports.rglob('*') if p.is_file()},
        code_sha256={p.name:sha(p) for p in HERE.iterdir() if p.suffix in ['.py','.R','.json','.md']},
        commands=commands,limitations=['Source audit is headers, notes, original methodology and relevant pages, not a full re-transcription.',
            '1949 total-staff ratios assume comparable part-time intensity in earlier professional faculty measures.',
            'Source coverage is partial and varies over years; 1949 is land-grant-only; interpolation does not correct coverage.'])
    (out/'release_manifest.json').write_text(json.dumps(manifest,indent=2),encoding='utf-8')
    print('COMPLETE: corrected observations, estimated panels and both interpolations validated; raw archive unchanged.',flush=True)

if __name__=='__main__':main()

import importlib,json
from pathlib import Path
import sys,tempfile,unittest
from unittest.mock import patch
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
publisher=importlib.import_module('12_publish_ocr_supplement')

class PublicationTests(unittest.TestCase):
    def test_rollback_and_idempotent_install(self):
        with tempfile.TemporaryDirectory() as td:
            root=Path(td)/'official';stage=Path(td)/'stage';root.mkdir();stage.mkdir()
            csvs=publisher.FILES+[publisher.COMBINED]
            files=csvs+[p.replace('.csv','.xlsx') for p in csvs]
            prior={};manifest=[]
            for rel in files:
                candidate=stage/'candidates'/rel;candidate.parent.mkdir(parents=True,exist_ok=True);candidate.write_text('new '+rel)
                target=root/rel;target.parent.mkdir(parents=True,exist_ok=True)
                if 'all_editions' not in rel:
                    target.write_text('old '+rel);prior[rel]=target.read_bytes()
                    manifest.append(dict(path=rel,sha256=publisher.sha(target)))
            publisher.write_json(stage/'baseline_manifest.json',manifest)
            checked=[dict(path=rel,csv_sha256=publisher.sha(stage/'candidates'/rel),
                xlsx_sha256=publisher.sha((stage/'candidates'/rel).with_suffix('.xlsx'))) for rel in csvs]
            publisher.write_json(stage/'validation.json',dict(status='passed',files=checked))
            original_replace=publisher.os.replace;calls=[]
            def fail_install(source,target):
                calls.append(target)
                if len(calls)==3:raise OSError('simulated install failure')
                original_replace(source,target)
            with patch.object(publisher.os,'replace',side_effect=fail_install):
                with self.assertRaisesRegex(OSError,'simulated'):
                    publisher.publish(root,stage)
            for rel,content in prior.items():self.assertEqual((root/rel).read_bytes(),content)
            self.assertFalse((root/publisher.COMBINED).exists())
            self.assertFalse(list(root.rglob('*.ocr-installing')))
            publisher.publish(root,stage)
            installed={rel:(root/rel).read_bytes() for rel in files}
            publisher.publish(root,stage)
            for rel,content in installed.items():self.assertEqual((root/rel).read_bytes(),content)

if __name__=='__main__':unittest.main()

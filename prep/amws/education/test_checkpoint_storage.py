import json
import os
from pathlib import Path
import tempfile
import unittest
from unittest import mock

import checkpoint_storage as storage


class StorageTests(unittest.TestCase):
    def test_migration_backup_resolution_and_checksum_rejection(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);release=root/'dropbox'/'release';source=release/'query_checkpoints'/'q.json'
            source.parent.mkdir(parents=True);source.write_text('{"result":1}',encoding='utf-8')
            checksum=storage.raw_digest(source)
            with mock.patch.dict(os.environ,{'AMWS_CHECKPOINT_ROOT':str(root/'local')}):
                storage.registry.cache_clear()
                result=storage.migrate(release,['query_checkpoints'])
                self.assertEqual(result['files'],1)
                self.assertEqual(storage.verify_release(release),1)
                local=storage.resolve(source);self.assertNotEqual(local,source)
                source.unlink();source.parent.rmdir()
                storage.verify_checksum(source,checksum)
                local.unlink()
                self.assertEqual(storage.restore(release)['restored_files'],1)
                storage.verify_checksum(source,checksum)
                local.write_text('changed',encoding='utf-8')
                with self.assertRaises(ValueError):storage.verify_release(release)

    def test_historical_executed_code_and_new_local_checkpoints(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder);code=root/'runner.py';code.write_text('original',encoding='utf-8')
            checksum=storage.raw_digest(code)
            with mock.patch.dict(os.environ,{'AMWS_CHECKPOINT_ROOT':str(root/'local')}):
                storage.registry.cache_clear();storage.archive_code([code]);code.write_text('new version',encoding='utf-8')
                storage.verify_checksum(code,checksum)
                stage=root/'dropbox'/'.release.staging';stage.mkdir(parents=True)
                target=storage.checkpoint_dir(stage,'raw_entry_checkpoints')/'r.json'
                target.parent.mkdir(parents=True);target.write_text('{}',encoding='utf-8')
                self.assertFalse((stage/'raw_entry_checkpoints').exists())
                storage.describe_new_checkpoints(stage,['raw_entry_checkpoints'])
                self.assertEqual(storage.verify_release(stage),1)
                self.assertEqual(storage.resolve(root/'dropbox/release/raw_entry_checkpoints/r.json'),target)


if __name__=='__main__':unittest.main()

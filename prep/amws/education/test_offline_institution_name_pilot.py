import hashlib
from pathlib import Path
import tempfile
import unittest
from unittest import mock
import offline_institution_name_pilot as p


class OfflinePilotTests(unittest.TestCase):
    def test_transient_windows_destination_lock_is_retried(self):
        with tempfile.TemporaryDirectory() as run:
            path=Path(run)/'checkpoint.json';original_replace=Path.replace;attempts=[]
            def transient(source,target):
                attempts.append(source)
                if len(attempts)==1:raise PermissionError('Temporary sharing lock')
                return original_replace(source,target)
            with mock.patch.object(Path,'replace',transient),mock.patch.object(p.time,'sleep') as sleep:
                p.write(path,{'saved':True})
            self.assertEqual(p.read(path),{'saved':True})
            self.assertEqual(len(attempts),2)
            sleep.assert_called_once_with(0.1)

    def fixture(self,run):
        original=dict(record_id='institution-1',institution_as_written='Harvard Uhiv')
        path=p.batch_folder(run,1)/'input.json';p.write(path,[original])
        p.write(Path(run)/'sample_manifest.json',dict(batch_numbers=[1],selected_ids=['institution-1'],immutable_files={'batches/batch_001/input.json':hashlib.sha256(path.read_bytes()).hexdigest()}))
        result=dict(original,standard_names=['Harvard University'],status='clear',note='')
        return original,result

    def test_checkpoint_restart_and_versioned_correction(self):
        with tempfile.TemporaryDirectory() as run:
            original,result=self.fixture(run)
            with self.assertRaises(ValueError):p.save(run,1,result)
            timer=p.begin_batch(run,1);self.assertEqual(p.begin_batch(run,1),timer)
            p.save(run,1,result);self.assertEqual(p.save(run,1,result),result)
            revised=dict(result,status='likely',note='Provisional OCR correction')
            with self.assertRaises(ValueError):p.save(run,1,revised)
            p.save(run,1,revised,revision_reason='Coordinator review')
            self.assertEqual(p.read(p.batch_folder(run,1)/'revisions/institution-1/001.json')['original_decision'],result)
            self.assertTrue(p.aggregate(run)['complete'])
            self.assertEqual(p.finish_batch(run,1),p.finish_batch(run,1))

    def test_input_and_missing_result_integrity(self):
        with tempfile.TemporaryDirectory() as run:
            self.fixture(run);p.begin_batch(run,1)
            self.assertEqual(p.aggregate(run)['outstanding'],1)
            with self.assertRaises(ValueError):p.finish_batch(run,1)
            p.write(p.batch_folder(run,1)/'input.json',[])
            with self.assertRaises(ValueError):p.aggregate(run)

    def test_unresolved_names_and_forbidden_research_fields(self):
        with tempfile.TemporaryDirectory() as run:
            original,result=self.fixture(run)
            for update in [dict(record_id='other'),dict(status='unresolved'),dict(source_urls=['https://college.edu']),dict(standard_names=['Harvard University','Harvard University'])]:
                with self.assertRaises(ValueError):p.validate(dict(result,**update),original)
            p.validate(dict(result,status='unresolved',standard_names=[],note='Too damaged'),original)


if __name__=='__main__':unittest.main()

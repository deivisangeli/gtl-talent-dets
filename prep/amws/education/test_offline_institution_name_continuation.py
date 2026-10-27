import hashlib
from pathlib import Path
import tempfile
import unittest
import offline_institution_name_continuation as c


class ContinuationTests(unittest.TestCase):
    def test_import_preservation_and_exact_dedup(self):
        with tempfile.TemporaryDirectory() as temp:
            run=Path(temp)
            original=dict(record_id='new-1',institution_as_written='Raw')
            path=c.p.batch_folder(run,1)/'input.json';c.p.write(path,[original])
            prior=dict(record_id='old-1',institution_as_written='Old raw',standard_names=['Example University'],status='clear',note='')
            c.p.write(run/'preserved/offline_pilot_results.json',[prior])
            c.p.write(run/'sample_manifest.json',dict(batch_numbers=[1],selected_ids=['new-1'],final_offline_names=2,accounted_prior_groups={},immutable_files={'batches/batch_001/input.json':c.digest(path),'preserved/offline_pilot_results.json':c.digest(run/'preserved/offline_pilot_results.json')}))
            c.p.begin_batch(run,1);c.p.save(run,1,dict(original,standard_names=['Example University'],status='likely',note='Provisional'))
            c.p.finish_batch(run,1)
            self.assertEqual(c.validate_batch(run,1)['records'],1)
            report=c.aggregate(run)
            self.assertEqual(report['offline_entries'],2)
            self.assertEqual(report['distinct_proposed_names'],1)
            self.assertEqual(c.p.read(run/'results.json')[0],prior)
            self.assertEqual(c.aggregate(run)['offline_entries'],2)
            c.p.write(run/'preserved/offline_pilot_results.json',[])
            with self.assertRaises(ValueError):c.aggregate(run)


if __name__=='__main__':unittest.main()

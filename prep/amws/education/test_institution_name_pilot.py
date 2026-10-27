import unittest
import tempfile
import hashlib
from pathlib import Path
import institution_name_pilot as pilot


class PilotValidation(unittest.TestCase):
    def setUp(self):
        self.original = dict(record_id='institution-1',institution_as_written='College',award_refs=[dict(source_key='1986:1')])
        self.result = dict(record_id='institution-1',institution_as_written='College',status='resolved',reason='Identity reviewed',
                           institutions=[dict(standard_name='College',identity_type='continuing',identity_evidence='Education explicitly names this college',conditions=None)],
                           source_keys=['1986:1'],source_urls=['https://college.edu/history'],queries=[],opened_sources=[])

    def test_identity_and_network_budgets(self):
        pilot.validate(self.result,self.original)
        self.result['institution_as_written']='Altered'
        with self.assertRaises(ValueError):pilot.validate(self.result,self.original)
        self.result['institution_as_written']='College'
        self.result['queries']=['a','b','c']
        with self.assertRaises(ValueError):pilot.validate(self.result,self.original)

    def test_no_openalex_and_no_unsupported_award(self):
        self.result['source_urls']=['https://api.openalex.org/institutions/I1']
        with self.assertRaises(ValueError):pilot.validate(self.result,self.original)
        self.result['source_urls']=['https://college.edu']
        self.result['source_keys']=['1986:2']
        with self.assertRaises(ValueError):pilot.validate(self.result,self.original)

    def test_conditional_scope_required_and_ambiguous_has_no_accepted_name(self):
        self.result['status']='conditional'
        with self.assertRaises(ValueError):pilot.validate(self.result,self.original)
        self.result['institutions'][0]['conditions']={'all':[dict(field='year',op='lt',value=1950)]}
        pilot.validate(self.result,self.original)
        self.result['status']='ambiguous'
        with self.assertRaises(ValueError):pilot.validate(self.result,self.original)
        self.result['institutions']=[]
        pilot.validate(self.result,self.original)

    def test_checkpoint_resume_preserves_revision_history_and_immutable_inputs(self):
        with tempfile.TemporaryDirectory() as temp:
            run=Path(temp)
            pilot.batches.atomic_json(run/'input.json',[self.original])
            pilot.batches.atomic_json(run/'sample_manifest.json',dict(immutable_files={'input.json':hashlib.sha256((run/'input.json').read_bytes()).hexdigest()}))
            saved=pilot.save(run,self.result)
            self.assertEqual(pilot.save(run,dict(saved)),saved)
            self.assertTrue(pilot.aggregate(run)['complete'])
            updated=dict(saved,reason='Corrected evidence scope')
            with self.assertRaises(ValueError):pilot.save(run,updated)
            pilot.save(run,updated,revision_reason='Independent coordinator review')
            self.assertEqual(pilot.read(run/'revisions/institution-1/001.json'),saved)
            pilot.batches.atomic_json(run/'input.json',[])
            with self.assertRaises(ValueError):pilot.aggregate(run)


if __name__=='__main__':unittest.main()

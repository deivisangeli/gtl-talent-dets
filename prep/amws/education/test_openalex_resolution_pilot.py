import copy
import tempfile
import unittest
from pathlib import Path

import openalex_resolution_pilot as pilot
import finalize_openalex_resolution_pilot as final


class Conditions(unittest.TestCase):
    def test_reliable_year_and_legacy_operator(self):
        m=[dict(openalex_id='I1',conditions={'all':[dict(field='year',op='gte',value=1900)]})]
        self.assertEqual(pilot.condition_ids(m,dict(year='1901',flags='[]')),{'I1'})
        for a in (dict(year='',flags='[]'),dict(year='1901',flags='["chronology_conflict"]'),dict(year='1901',flags='["century_ambiguous"]')):
            self.assertFalse(pilot.condition_ids(m,a))

    def test_missing_context_fails_and_text_can_match_without_year(self):
        m=[dict(openalex_id='I1',conditions={'all':[dict(field='education_context',op='regex',value='Seattle')]})]
        self.assertFalse(pilot.condition_ids(m,dict(year='1900')))
        self.assertEqual(pilot.condition_ids(m,dict(year='',education_context='Washington (Seattle)')),{'I1'})

    def test_conflicting_conditions_withhold_award(self):
        m=[dict(openalex_id=i,conditions={'all':[dict(field='edition',op='eq',value=1955)]}) for i in ('I1','I2')]
        self.assertEqual(final.evaluated_ids(dict(status='conditional',matches=m),dict(edition='1955')),(set(),True))

    def test_co_named_resolved_keeps_both_ids(self):
        m=[dict(openalex_id=i,conditions=None) for i in ('I1','I2')]
        self.assertEqual(final.evaluated_ids(dict(status='resolved',matches=m),{}),({'I1','I2'},False))


class SamplingAndPersistence(unittest.TestCase):
    def fixture(self):
        return ([dict(record_id=f'c{i:03}',institution_as_written=f'C{i}',origin='oa_research_preserved',status='conditional') for i in range(20)]
               +[dict(record_id=f'l{i:03}',institution_as_written=f'L{i}',origin='offline_cleanup',status='candidate_only',cleanup_status='likely',
                      components=[dict(exact_status='eligible_unique_exact')],prior_conditions=[],accepted_provisional_ids=[]) for i in range(20)]
               +[dict(record_id=f'w{i:03}',institution_as_written=f'W{i}',origin='offline_cleanup',status='ambiguous',accepted_provisional_ids=[]) for i in range(30)])

    def test_sampling_order_independent_and_disjoint(self):
        rows=self.fixture();batches,pools=pilot.select(rows);again=pilot.select(list(reversed(rows)))
        self.assertEqual((batches,pools),again)
        self.assertEqual([len(b) for b in batches],[17,17,16])
        self.assertEqual(len({r['record_id'] for b in batches for g,r in b}),50)
        for b in batches:
            self.assertEqual(sum(g=='conditional' for g,r in b),5)
            self.assertEqual(sum(g=='likely_exact' for g,r in b),5)

    def test_checkpoint_resume_and_conflicting_write(self):
        with tempfile.TemporaryDirectory() as directory:
            p=Path(directory)/'result.json';pilot.immutable(p,dict(id=1));before=p.read_bytes()
            pilot.immutable(p,dict(id=1));self.assertEqual(before,p.read_bytes())
            with self.assertRaises(ValueError):pilot.immutable(p,dict(id=2))

    def test_review_keeps_original(self):
        orig=dict(record_id='a',institution_as_written='A');decision=dict(orig,status='resolved')
        replacement=dict(orig,status='ambiguous',matches=[])
        r=dict(orig,verdict='withheld',reason='Unsupported identity',replacement_decision=replacement)
        result=final.reviewed(orig,decision,r)
        self.assertEqual(result['original_agent_decision'],decision);self.assertEqual(result['status'],'ambiguous')

    def test_unsupported_id_and_changed_name_rejected(self):
        original=dict(record_id='a',institution_as_written='A',group='likely_exact')
        decision=dict(original,status='resolved',reason='evidence',research_date='2026-10-12',matches=[dict(openalex_id='I999',canonical_name='A',conditions=None,evidence='e',source_urls=['local'])])
        with self.assertRaises(KeyError):pilot.validate_decision(decision,original,{})
        changed=dict(decision,institution_as_written='B')
        with self.assertRaises(AssertionError):pilot.validate_decision(changed,original,{})


if __name__=='__main__':unittest.main()

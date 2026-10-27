import copy
import tempfile
import unittest
from unittest.mock import patch
from pathlib import Path

import openalex_id_resolution as r
import match_cleaned_names_snapshot as b


class Conditions(unittest.TestCase):
    def test_missing_and_conflicting_years(self):
        matches=[dict(openalex_id='I1',conditions={'all':[dict(field='year',op='lt',value=1957)]})]
        self.assertEqual(r.apply_matches(matches,dict(year='1956',flags=[])),({'I1'},False))
        for year,flags in [('',[]),('1956',['chronology_conflict']),('1956',['century_ambiguous'])]:
            self.assertEqual(r.apply_matches(matches,dict(year=year,flags=flags)),(set(),False))

    def test_overlap_is_conflict_but_conamed_components_can_both_match(self):
        same=[dict(openalex_id=i,conditions=None) for i in ('I1','I2')]
        self.assertEqual(r.apply_matches(same,{}),(set(),True))
        separate=[dict(m,component_index=k) for k,m in enumerate(same)]
        self.assertEqual(r.apply_matches(separate,{},True),({'I1','I2'},False))
        self.assertEqual(r.apply_matches(separate,{},False),(set(),True))

    def test_duplicate_conditions_for_one_id_are_not_conflicts(self):
        m=dict(openalex_id='I1',conditions=None)
        self.assertEqual(r.apply_matches([m,m],{}),({'I1'},False))

    def test_context_predicate_independent_of_missing_year(self):
        m=[dict(openalex_id='I1',conditions={'any':[{'all':[dict(field='year',op='lt',value=1957)]},
                {'all':[dict(field='education_context',op='regex',value='Columbus')]}]})]
        self.assertEqual(r.apply_matches(m,dict(year='',education_context='Ohio State Columbus')),({'I1'},False))
        self.assertEqual(r.apply_matches(m,dict(year='')),(set(),False))


class Decisions(unittest.TestCase):
    def test_audit_paths_preserve_stage_when_raw_name_is_researched_again(self):
        decisions=[dict(record_id='same',institution_as_written='A',group=g)
                   for g in ('likely_exact','web_research')]
        items=[dict(d,result_file=d['group']+'/result.json',full_awards_file=d['group']+'/evidence.json') for d in decisions]
        with tempfile.TemporaryDirectory() as folder:
            with patch.object(r,'local_root',return_value=Path(folder)),patch.object(r,'load_local',return_value={'batches':[{'records':2}]}),patch.object(r,'results',return_value=decisions),patch.object(r,'batch_inputs',return_value=items):
                r.prepare_audit()
            selected=r.read(Path(folder)/'audit/sample_manifest.json')['records']
        self.assertEqual(len(selected),1)
        self.assertEqual(selected[0]['group'],'likely_exact')
        self.assertEqual(selected[0]['original_decision_file'],'likely_exact/result.json')

    def fixture(self,group='web_research'):
        original=dict(record_id='a',institution_as_written='A',group=group)
        record=dict(openalex_id='I1',canonical_name='A University',type='education',is_super_system=False)
        match=dict(openalex_id='I1',canonical_name='A University',conditions=None,source_urls=['https://openalex.org/institutions/I1'],evidence='Opened identity',
                   official_page_verification=dict(url='https://openalex.org/institutions/I1',canonical_id='I1',identity_visible=True,observed_name='A University',evidence='Visible title and ID'))
        decision=dict(original,status='resolved',reason='Evidence supports identity',research_date='2026-10-12',matches=[match],
                      research_log=dict(searches=['A University'],opened_urls=['https://openalex.org/institutions/I1']))
        return original,decision,{'I1':record}

    def test_visible_record_required_no_API_calls_and_open_budget(self):
        original,decision,records=self.fixture();self.assertTrue(r.validate(decision,original,records))
        d=copy.deepcopy(decision);d['matches'][0]['official_page_verification']['identity_visible']=False
        with self.assertRaises(ValueError):r.validate(d,original,records)
        d=copy.deepcopy(decision);d['research_log']['opened_urls'].append('https://api.openalex.org/institutions/I1')
        with self.assertRaises(ValueError):r.validate(d,original,records)
        d=copy.deepcopy(decision);d['research_log']['opened_urls']*=4
        with self.assertRaises(ValueError):r.validate(d,original,records)

    def test_snapshot_ID_type_system_and_name(self):
        original,d,records=self.fixture()
        for change in [dict(type='company'),dict(is_super_system=True),dict(canonical_name='Different University')]:
            changed={'I1':dict(records['I1'],**change)}
            with self.assertRaises(ValueError):r.validate(d,original,changed)
        with self.assertRaises(ValueError):r.validate(d,original,{})
        d=copy.deepcopy(d);d['institution_as_written']='Changed'
        with self.assertRaises(ValueError):r.validate(d,original,records)

    def test_query_deviation_requires_full_log_and_withheld_acceptance(self):
        original,d,records=self.fixture()
        d['research_log']['searches']=['first','second','third']
        with self.assertRaises(ValueError):r.validate(d,original,records)
        d['research_policy_deviation']=dict(kind='query_budget_exceeded',limit=2,actual=3,
             acceptance_withheld=True,reason='Interrupted recovery duplicated a query; retain full log.')
        with self.assertRaises(ValueError):r.validate(d,original,records)
        d.update(status='ambiguous',matches=[])
        self.assertTrue(r.validate(d,original,records))
        d['research_policy_deviation']['actual']=2
        with self.assertRaises(ValueError):r.validate(d,original,records)

    def test_blocked_results_require_retry(self):
        original,d,records=self.fixture();d.update(status='access_blocked',matches=[])
        with self.assertRaises(ValueError):r.validate(d,original,records)
        d['research_log']['access_retry']=dict(result='Still blocked')
        self.assertTrue(r.validate(d,original,records))

    def test_all_awards_reviewed_and_exact_candidate_only(self):
        original,d,records=self.fixture('likely_exact')
        with tempfile.TemporaryDirectory() as folder:
            path=Path(folder)/'awards.json';awards=[dict(source_key='1955:1',award_index='1'),dict(source_key='1955:1',award_index='2')]
            r.write(path,awards);original.update(full_awards_file=str(path),components=[dict(exact_matches=[dict(openalex_id='I1')])])
            d['reviewed_awards']=awards[:1]
            with self.assertRaises(ValueError):r.validate(d,original,records)
            d['reviewed_awards']=awards;self.assertTrue(r.validate(d,original,records))
            original['components'][0]['exact_matches']=[]
            with self.assertRaises(ValueError):r.validate(d,original,records)

    def test_conditional_mapping_immutable(self):
        original,d,records=self.fixture('conditional')
        d.update(status='conditional');d['matches'][0]['conditions']={'all':[dict(field='year',op='lt',value=1957)]}
        original['existing_matches']=copy.deepcopy(d['matches'])
        self.assertTrue(r.validate(d,original,records))
        d['matches'][0]['conditions']['all'][0]['value']=1960
        with self.assertRaises(ValueError):r.validate(d,original,records)

    def test_checkpoint_recovery_and_conflict_prevention(self):
        with tempfile.TemporaryDirectory() as folder:
            p=Path(folder)/'result.json';r.immutable(p,dict(a=1));data=p.read_bytes()
            r.immutable(p,dict(a=1));self.assertEqual(data,p.read_bytes())
            with self.assertRaises(ValueError):r.immutable(p,dict(a=2))


class SnapshotQueries(unittest.TestCase):
    def test_collision_and_duplicate_aliases_and_qualifiers(self):
        records={i:dict(openalex_id=i,canonical_name=n,type='education',is_super_system=False) for i,n in [('I1','A University'),('I2','A University, North Campus')]}
        aliases=[dict(id='I1',alias='A University',source='canonical'),dict(id='I1',alias='A University',source='alternative'),
                 dict(id='I2',alias='A University, North Campus',source='canonical')]
        idx=b.SnapshotIndex(records,aliases)
        self.assertEqual(len(idx.exact_result('A University')['exact_matches']),1)
        self.assertEqual(idx.exact_result('A University, North Campus')['exact_matches'][0]['openalex_id'],'I2')
        aliases.append(dict(id='I2',alias='A University',source='alternative'))
        self.assertEqual(b.SnapshotIndex(records,aliases).exact_result('A University')['exact_status'],'ambiguous')


if __name__=='__main__':unittest.main()

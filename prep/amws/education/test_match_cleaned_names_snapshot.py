"""Acceptance-policy, collision, multi-award, and offline scorer checks."""
import copy
import csv
import tempfile
import unittest
from pathlib import Path

import match_cleaned_names_snapshot as m


def record(name, kind='education', system=False):
    return dict(canonical_name=name, type=kind, is_super_system=system)


def original(identifier='r1', conditions=None):
    return dict(record_id=identifier, institution_as_written='Raw Univ', award_count=1,
                award_refs=[dict(source_key='bio-'+identifier, award_index=1)],
                prior_conditions=conditions or [], prior_accepted_ids=[])


def cleanup(names, status='clear', identifier='r1'):
    return dict(record_id=identifier, institution_as_written='Raw Univ',
                standard_names=names, status=status, note='')


def query(index, name):
    result = index.exact_result(name)
    result.update(query_id='q-'+name, candidates=result['exact_matches'][:5])
    return result


class SnapshotMatchingTests(unittest.TestCase):
    def index(self):
        records = {'I1':record('Alpha University'), 'I2':record('Beta University'),
                   'I3':record('Alpha University System', system=True),
                   'I4':record('Medical Center', 'healthcare')}
        aliases = [dict(id=i, alias=r['canonical_name'], source='canonical') for i,r in records.items()]
        aliases += [dict(id='I1', alias='Alpha University', source='alternative'),
                    dict(id='I2', alias='Alpha University', source='alternative'),
                    dict(id='I1', alias='AU', source='acronym'),
                    dict(id='I1', alias='External Alias', source='crosswalk.csv')]
        return m.SnapshotIndex(records, aliases)

    def test_normalization_preserves_order_words_and_qualifiers(self):
        self.assertEqual(m.normalize('École & University, Campus—East'), 'ecole and university campus east')
        self.assertNotEqual(m.normalize('College State'), m.normalize('State College'))
        self.assertNotEqual(m.normalize('Alpha University East'), m.normalize('Alpha University'))

    def test_alternative_collision_blocks_canonical(self):
        row = self.index().exact_result('Alpha University')
        self.assertEqual(row['exact_status'], 'ambiguous')
        self.assertEqual(len(row['exact_matches']), 2)

    def test_duplicate_aliases_do_not_create_collision(self):
        index = m.SnapshotIndex({'I1':record('Alpha')}, [dict(id='I1',alias='Alpha',source=s) for s in ['canonical','alternative']])
        row = index.exact_result('Alpha')
        self.assertEqual(row['exact_status'], 'eligible_unique_exact')
        self.assertEqual(row['exact_matches'][0]['alias_source'], 'canonical')

    def test_all_types_participate_in_collision(self):
        index = m.SnapshotIndex({'I1':record('Same'), 'I2':record('Same', 'healthcare')},
                                [dict(id=i,alias='Same',source='canonical') for i in ['I1','I2']])
        self.assertEqual(index.exact_result('Same')['exact_status'], 'ambiguous')

    def test_systems_and_non_education_are_blocked(self):
        self.assertEqual(self.index().exact_result('Alpha University System')['exact_status'], 'system')
        self.assertEqual(self.index().exact_result('Medical Center')['exact_status'], 'non_education')

    def test_lead_aliases_cannot_authorize_exact_matches(self):
        for name in ['AU', 'External Alias']:
            self.assertEqual(self.index().exact_result(name)['exact_status'], 'no_exact_match')
            self.assertIn('I1', self.index().retrieved(name))

    def test_clear_only_and_preserved_conditions(self):
        index = self.index(); proposed = {'Beta University':query(index,'Beta University')}
        self.assertEqual(m.match_raw(cleanup(['Beta University']),original(),proposed)['status'], 'fully_provisional_matched')
        likely = m.match_raw(cleanup(['Beta University'],'likely'),original(),proposed)
        self.assertFalse(likely['accepted_provisional_ids'])
        condition = [{'all':[{'field':'year','operator':'gte','value':1950}]}]
        conditional = m.match_raw(cleanup(['Beta University']),original(conditions=condition),proposed)
        self.assertEqual(conditional['status'],'conditional_preserved')
        self.assertEqual(conditional['prior_conditions'],condition)
        self.assertFalse(conditional['accepted_provisional_ids'])

    def test_multiple_names_partial_match_and_uncertainty_unchanged(self):
        index = self.index(); proposed = {n:query(index,n) for n in ['Alpha University','Beta University']}
        row = m.match_raw(cleanup(list(proposed)),original(),proposed)
        self.assertEqual(row['status'],'partially_provisional_matched')
        self.assertEqual(row['accepted_provisional_ids'],['I2'])
        self.assertEqual(len(row['components']),2)
        for status in ['unresolved','not_an_institution']:
            result = m.match_raw(cleanup([],status),original(),{})
            self.assertEqual(result['cleanup_status'],status)
            self.assertFalse(result['components'])

    def test_award_weights_count_multi_institution_award_once(self):
        index = self.index(); proposed = {n:query(index,n) for n in ['Alpha University','Beta University']}
        row = m.match_raw(cleanup(list(proposed)),original(),proposed)
        report = m.coverage([row],{'r1':original()})
        self.assertEqual(report['partially_provisional_matched_award_references'],1)
        self.assertEqual(report['unique_award_references'],1)
        with self.assertRaises(ValueError):m.coverage([row,row],{'r1':original()})

    def test_validation_rejects_unsupported_acceptance(self):
        index = self.index(); proposed = {'Beta University':query(index,'Beta University')}
        old = cleanup(['Beta University'])
        row = m.match_raw(old,original(),proposed)
        m.validate_outputs([row],[old],{'r1':original()},proposed,index)
        bad = copy.deepcopy(row)
        bad['components'][0]['accepted_provisional_match']['openalex_id']='I999'
        with self.assertRaises(ValueError):m.validate_outputs([bad],[old],{'r1':original()},proposed,index)

    def test_offline_DL_scoring_and_checkpoint_recovery(self):
        records = {f'I{i}':record('Alpha abcd'+str(i)) for i in range(1,8)}
        aliases = [dict(id=i,alias=r['canonical_name'],source='canonical') for i,r in records.items()]
        aliases += [dict(id='I1',alias='Alpha abcd1',source='alternative')]
        index = m.SnapshotIndex(records,aliases)
        with tempfile.TemporaryDirectory() as folder:
            stage = Path(folder)
            args=(index,{'Alpha abdc1'},stage,Path('C:/Program Files/R/R-4.5.2/bin/Rscript.exe'))
            first,_=m.score_queries(*args)
            second,_=m.score_queries(*args)
            self.assertEqual(first,second)
            candidates=first['Alpha abdc1']['candidates']
            self.assertEqual(len(candidates),5)
            best=next(c for c in candidates if c['openalex_id']=='I1')
            self.assertAlmostEqual(best['score'],100*(1-1/11))
            self.assertEqual(len(list(m.storage.checkpoint_dir(stage,'query_checkpoints').glob('*.json'))),1)


if __name__=='__main__':unittest.main()

"""Processed fallback, precedence, provenance, counting, and publication tests."""
import copy
import csv
import json
from pathlib import Path
import tempfile
import unittest
from unittest import mock

import match_cleaned_names_snapshot as base
import match_cleaned_names_snapshot_v2 as v2
from test_match_cleaned_names_snapshot import record, original, cleanup, query


def proposed(index, names):
    texts=set(names)|{v2.process_name(n)['processed_name'] for n in names if base.normalize(v2.process_name(n)['processed_name'])}
    queries={n:query(index,n) for n in texts}
    return {n:v2.proposal(n,queries) for n in names}


class ProcessedMatchingTests(unittest.TestCase):
    def index(self):
        records={'I1':record('Indiana University'),'I2':record('Indiana University Indianapolis'),
                 'I3':record('Alpha University'),'I4':record('Beta University'),
                 'I5':record('System University',system=True),'I6':record('Hospital','healthcare')}
        aliases=[dict(id=i,alias=r['canonical_name'],source='canonical') for i,r in records.items()]
        aliases.append(dict(id='I2',alias='Indiana University, Indianapolis',source='alternative'))
        return base.SnapshotIndex(records,aliases)

    def test_processing_first_delimiter_and_unchanged(self):
        self.assertEqual(v2.process_name('Alpha, City (Country)')['processed_name'],'Alpha')
        self.assertEqual(v2.process_name('Alpha (Campus), City')['processed_name'],'Alpha')
        self.assertEqual(v2.process_name('Alpha (Campus), City')['removed_suffix'],'(Campus), City')
        self.assertEqual(v2.process_name('  Alpha  ')['processed_name'],'  Alpha  ')
        self.assertEqual(v2.process_name(', City')['processed_name'],'')

    def test_clear_processed_fallback_and_flag(self):
        index=self.index(); name='Alpha University, Country'; data=proposed(index,[name])
        row=v2.match_raw(cleanup([name]),original(),data)
        component=row['components'][0]
        self.assertEqual(row['accepted_provisional_ids'],['I3'])
        self.assertEqual(component['matching_version'],'processed')
        self.assertIn('qualifier_removed',component['flags'])
        self.assertEqual(component['cleaned_name'],name)
        self.assertEqual(component['removed_suffix'],', Country')

    def test_full_campus_match_has_priority(self):
        index=self.index(); name='Indiana University, Indianapolis'; data=proposed(index,[name])
        row=v2.match_raw(cleanup([name]),original(),data)
        self.assertEqual(row['accepted_provisional_ids'],['I2'])
        self.assertEqual(row['components'][0]['matching_version'],'cleaned')
        self.assertNotIn('qualifier_removed',row['flags'])

    def test_full_collision_blocks_unique_processed(self):
        index=self.index()
        aliases=[dict(id='I3',alias='Alpha University, Country',source='alternative'),
                 dict(id='I4',alias='Alpha University, Country',source='alternative'),
                 dict(id='I3',alias='Alpha University',source='canonical')]
        index=base.SnapshotIndex(index.records,aliases)
        name='Alpha University, Country'; row=v2.match_raw(cleanup([name]),original(),proposed(index,[name]))
        self.assertEqual(row['status'],'ambiguous')
        self.assertFalse(row['accepted_provisional_ids'])

    def test_full_system_or_non_education_blocks_fallback(self):
        records={'I1':record('Alpha University'),'I2':record('Alpha University, City','healthcare'),
                 'I3':record('Alpha University (System)',system=True)}
        aliases=[dict(id=i,alias=r['canonical_name'],source='canonical') for i,r in records.items()]
        index=base.SnapshotIndex(records,aliases)
        for name in ['Alpha University, City','Alpha University (System)']:
            row=v2.match_raw(cleanup([name]),original(),proposed(index,[name]))
            self.assertFalse(row['accepted_provisional_ids'])

    def test_processed_collision_system_non_education(self):
        records={'I1':record('Alpha'),'I2':record('Alpha'),'I3':record('System',system=True),'I4':record('Hospital','healthcare')}
        aliases=[dict(id=i,alias=r['canonical_name'],source='canonical') for i,r in records.items()]
        index=base.SnapshotIndex(records,aliases)
        for name in ['Alpha, City','System, Country','Hospital (City)']:
            self.assertFalse(v2.match_raw(cleanup([name]),original(),proposed(index,[name]))['accepted_provisional_ids'])

    def test_likely_conditions_and_empty_processed(self):
        index=self.index(); name='Alpha University, City'; data=proposed(index,[name])
        self.assertFalse(v2.match_raw(cleanup([name],'likely'),original(),data)['accepted_provisional_ids'])
        condition=[{'all':[{'field':'year','operator':'gte','value':1950}]}]
        row=v2.match_raw(cleanup([name]),original(conditions=condition),data)
        self.assertEqual(row['prior_conditions'],condition)
        self.assertEqual(row['status'],'conditional_preserved')
        for name in [', City','(Campus) Alpha']:
            row=v2.match_raw(cleanup([name]),original(),proposed(index,[name]))
            self.assertIn('empty_processed_name',row['flags'])
            self.assertFalse(row['accepted_provisional_ids'])

    def test_multi_institution_partial_and_full(self):
        index=self.index(); names=['Alpha University, City','Unknown University (Campus)']
        row=v2.match_raw(cleanup(names),original(),proposed(index,names))
        self.assertEqual(row['status'],'partially_provisional_matched')
        self.assertEqual(len(row['components']),2)
        names=['Alpha University, City','Beta University (Country)']
        self.assertEqual(v2.match_raw(cleanup(names),original(),proposed(index,names))['status'],'fully_provisional_matched')

    def test_candidate_merge_highest_score_versions_and_distinct_IDs(self):
        index=self.index(); cleaned=query(index,'Alpha University'); processed=query(index,'Beta University')
        item=copy.deepcopy(cleaned['candidates'][0]); item['score']=70
        cleaned['candidates']=[item]; alternate=copy.deepcopy(item); alternate['score']=95
        processed['candidates']=[alternate,processed['candidates'][0]]
        merged=v2.merge_candidates(cleaned,processed)
        candidate=next(c for c in merged if c['openalex_id']=='I3')
        self.assertEqual(candidate['score'],95)
        self.assertEqual(candidate['best_query_version'],'processed')
        self.assertEqual(candidate['contributing_query_versions'],['cleaned','processed'])
        self.assertEqual(len(merged),len({c['openalex_id'] for c in merged}))

    def test_validation_and_checkpoint_recovery(self):
        index=self.index(); name='Alpha University, City'; old_cleanup=cleanup([name]); inputs={'r1':original()}
        old_proposed={name:query(index,name)}; previous={'r1':base.match_raw(old_cleanup,original(),old_proposed)}
        data=proposed(index,[name]); row=v2.match_raw(old_cleanup,original(),data)
        v2.validate_rows([row],previous,[old_cleanup],inputs,data,index)
        bad=copy.deepcopy(row); bad['components'][0]['flags']=[]
        with self.assertRaises(ValueError):v2.validate_rows([bad],previous,[old_cleanup],inputs,data,index)
        with tempfile.TemporaryDirectory() as folder:
            file=Path(folder)/'checkpoint.json'; v2.checkpoint(file,row); v2.checkpoint(file,row)
            with self.assertRaises(ValueError):v2.checkpoint(file,bad)

    def test_award_person_coverage_no_double_counting(self):
        with tempfile.TemporaryDirectory() as folder:
            stage=Path(folder); people=stage/'people.json'; base.write(people,[dict(source_key='bio-r1',edition='1986')])
            ledger=stage/'awards.csv'
            fields=['source_key','award_index','requested_family','family_award_index','honorary','institution_as_written']
            with ledger.open('w',newline='',encoding='utf-8') as target:
                writer=csv.DictWriter(target,fieldnames=fields); writer.writeheader()
                writer.writerow(dict(source_key='bio-r1',award_index=1,requested_family='ba_bs',family_award_index=1,honorary='False',institution_as_written='Raw Univ'))
            old=dict(record_id='r1',institution_as_written='Raw Univ',origin='offline_cleanup',award_count=1,
                     accepted_provisional_ids=[],status='candidate_only')
            new=dict(old,accepted_provisional_ids=['I1','I2'],status='fully_provisional_matched')
            report=v2.reconcile_coverage([old],[new],{'r1':original()},ledger,people,stage)
            self.assertEqual(report['additional_award_references_with_any_ID'],1)
            self.assertEqual(report['additional_AMWS_entries_with_any_ID'],1)
            self.assertEqual(report['AMWS_entries_with_multiple_IDs_after'],1)

    def test_publication_copy_fallback_and_checksum_gate(self):
        with tempfile.TemporaryDirectory() as folder:
            root=Path(folder); stage=root/'stage'; stage.mkdir(); output=root/'release'
            file=stage/'result.json'; base.write(file,{'passed':True})
            manifest=dict(source_checksums={},output_checksums={'result.json':base.digest(file)},status='complete')
            with mock.patch.object(Path,'rename',side_effect=PermissionError),mock.patch.object(v2.time,'sleep'):
                self.assertEqual(v2.publish(stage,output,manifest),'verified_copy')
            self.assertTrue((output/'completion_manifest.json').exists())
            self.assertEqual(base.digest(output/'result.json'),manifest['output_checksums']['result.json'])
        with tempfile.TemporaryDirectory() as folder:
            stage=Path(folder)/'stage'; stage.mkdir(); base.write(stage/'result.json',{})
            manifest=dict(source_checksums={},output_checksums={'result.json':'wrong'})
            output=Path(folder)/'release'
            with self.assertRaises(ValueError):v2.publish(stage,output,manifest)
            self.assertFalse((output/'completion_manifest.json').exists())


if __name__=='__main__':unittest.main()

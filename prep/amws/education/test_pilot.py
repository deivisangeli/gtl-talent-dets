"""Runner checks using synthetic biographies; never read held-out predictions."""
import copy
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import pilot
import sample_pilot


def degree(group='bachelors', institution='Example College', year=1950,
           literal='50', honorary=False):
    return dict(group=group, label_as_written='BS',
                institution_as_written=institution, year_raw=literal,
                year=year, honorary=honorary)


class MetricTests(unittest.TestCase):
    def setUp(self):
        self.rows = {i: dict(edition=edition) for i, edition in
                     enumerate((1906, 1938, 1955, 1986), 1)}
        self.gold = {i: [] for i in self.rows}
        self.pred = {i: dict(degrees=[], flags=[]) for i in self.rows}

    def evaluate(self, ids=None):
        with patch.object(pilot, 'sample', return_value=self.rows):
            return pilot.evaluate(list(self.rows) if ids is None else ids,
                                  self.pred, self.gold)[0]

    def metric(self, name, group='bachelors', edition=1906):
        return next(m for m in self.evaluate()
                    if m['edition'] == edition and m['group'] == group
                    and m['metric'] == name)

    def test_extra_prediction_occurrence_is_false_positive(self):
        d = degree()
        self.gold[1] = [d]
        self.pred[1]['degrees'] = [copy.deepcopy(d), copy.deepcopy(d)]
        for name in ('degree', 'institution', 'literal_year', 'year',
                     'complete_tuple'):
            m = self.metric(name)
            self.assertEqual((m['tp'], m['fp'], m['fn']), (1, 1, 0))
        m = self.metric('record_detection')
        self.assertEqual((m['tp'], m['fp'], m['fn']), (1, 0, 0))

    def test_repeated_reference_awards_require_two_predictions(self):
        d = degree()
        self.gold[1] = [d, copy.deepcopy(d)]
        self.pred[1]['degrees'] = [d]
        m = self.metric('complete_tuple')
        self.assertEqual((m['tp'], m['fp'], m['fn']), (1, 0, 1))

    def test_matching_is_per_record_not_across_people(self):
        self.rows[2]['edition'] = 1906
        d = degree()
        self.gold[1] = [d]
        self.pred[2]['degrees'] = [d]
        m = self.metric('complete_tuple')
        self.assertEqual((m['tp'], m['fp'], m['fn']), (0, 1, 1))

    def test_missing_gold_components_are_outside_recall_denominator(self):
        self.gold[1] = [degree(institution=None, year=None, literal=None),
                        degree(institution='School Only', year=None,
                               literal=None),
                        degree(institution=None, year=1951, literal='51')]
        self.assertEqual(self.metric('degree')['fn'], 3)
        self.assertEqual(self.metric('institution')['fn'], 1)
        self.assertEqual(self.metric('year')['fn'], 1)
        self.assertEqual(self.metric('literal_year')['fn'], 1)
        m = self.metric('complete_tuple')
        self.assertEqual((m['tp'], m['fp'], m['fn']), (0, 0, 0))
        self.assertIsNone(m['recall'])

    def test_unverifiable_predicted_field_does_not_gain_true_positive(self):
        self.gold[1] = [degree(institution=None)]
        self.pred[1]['degrees'] = [degree(institution='Invented College')]
        m = self.metric('institution')
        self.assertEqual((m['tp'], m['fp'], m['fn']), (0, 1, 0))
        self.assertIsNone(m['recall'])

    def test_component_matches_do_not_make_crossed_awards_correct(self):
        self.gold[1] = [degree(institution='Alpha', year=1950),
                        degree(institution='Beta', year=1960)]
        self.pred[1]['degrees'] = [degree(institution='Alpha', year=1960),
                                  degree(institution='Beta', year=1950)]
        for name in ('institution', 'year'):
            self.assertEqual(self.metric(name)['tp'], 2)
        m = self.metric('complete_tuple')
        self.assertEqual((m['tp'], m['fp'], m['fn']), (0, 2, 2))

    def test_honorary_awards_are_excluded_on_both_sides(self):
        self.gold[1] = [degree(honorary=True)]
        self.pred[1]['degrees'] = [degree(honorary=True)]
        m = self.metric('degree')
        self.assertEqual((m['tp'], m['fp'], m['fn']), (0, 0, 0))
        self.pred[1]['degrees'][0]['honorary'] = False
        self.assertEqual(self.metric('degree')['fp'], 1)

    def test_academic_and_professional_masters_stay_separate(self):
        self.gold[1] = [degree(group='masters_academic')]
        self.pred[1]['degrees'] = [degree(group='masters_professional')]
        self.assertEqual(self.metric('degree', 'masters_academic')['fn'], 1)
        self.assertEqual(self.metric('degree', 'masters_professional')['fp'], 1)
        self.assertEqual(self.metric('degree', 'any_master')['tp'], 0)

    def test_empty_components_have_no_artificial_perfect_score(self):
        m = self.metric('complete_tuple')
        self.assertIsNone(m['precision'])
        self.assertIsNone(m['recall'])
        self.assertIsNone(m['f1'])
        self.assertIsNone(m['f1_ci_low'])

    def test_stratified_intervals_are_invariant_to_id_order(self):
        self.rows = {ed * 100 + j: dict(edition=ed)
                     for ed in (1906, 1938, 1955, 1986) for j in range(1, 9)}
        self.gold = {i: [degree()] for i in self.rows}
        self.pred = {i: dict(degrees=([degree()] if i % 3 else []), flags=[])
                     for i in self.rows}
        ids = sorted(self.rows)
        scrambled = ids[::2] + list(reversed(ids[1::2]))
        self.assertEqual(self.evaluate(ids), self.evaluate(scrambled))


class BoundaryTests(unittest.TestCase):
    def test_cross_edition_link_stays_together_with_exact_quotas(self):
        rows = {ed_index * 10 + j: dict(edition=ed)
                for ed_index, ed in enumerate(sample_pilot.EDITIONS)
                for j in range(1, 11)}
        groups = [[1, 11]] + [[i] for i in rows if i not in (1, 11)]
        seeded = {1, 2, 3, 12, 13, 14, 21, 22, 23, 31, 32, 33}
        hold = sample_pilot.allocate_linked(groups, rows, seeded, quota=3)
        self.assertEqual(1 in hold, 11 in hold)
        self.assertEqual(len(hold), 12)
        for ed in sample_pilot.EDITIONS:
            self.assertEqual(sum(rows[i]['edition'] == ed for i in hold), 3)
        self.assertEqual(hold, sample_pilot.allocate_linked(
            list(reversed(groups)), rows, seeded, quota=3))

    def test_default_fifty_per_edition_with_multiple_linked_groups(self):
        rows = {ed_index * 250 + j: dict(edition=ed)
                for ed_index, ed in enumerate(sample_pilot.EDITIONS)
                for j in range(1, 251)}
        linked = [[1, 2, 251], [252, 501, 751]]
        linked_ids = {i for g in linked for i in g}
        groups = linked + [[i] for i in rows if i not in linked_ids]
        seeded = {ed_index * 250 + j for ed_index in range(4)
                  for j in range(1, 51)}
        hold = sample_pilot.allocate_linked(groups, rows, seeded)
        self.assertEqual(len(hold), 200)
        self.assertTrue(hold <= set(rows))
        for ed in sample_pilot.EDITIONS:
            self.assertEqual(sum(rows[i]['edition'] == ed for i in hold), 50)
        for g in linked:
            self.assertIn(len(set(g) & hold), (0, len(g)))

    def test_oversized_group_with_insufficient_singletons_is_infeasible(self):
        rows = {1: dict(edition=1906), 2: dict(edition=1906),
                3: dict(edition=1906), 4: dict(edition=1906)}
        for ed_index, ed in enumerate(sample_pilot.EDITIONS[1:], 1):
            rows[ed_index * 10 + 1] = dict(edition=ed)
            rows[ed_index * 10 + 2] = dict(edition=ed)
        groups = [[1, 2, 3]] + [[i] for i in rows if i not in (1, 2, 3)]
        with self.assertRaisesRegex(AssertionError, 'exact edition quotas'):
            sample_pilot.allocate_linked(groups, rows, set(rows), quota=2)

    def test_duplicate_source_keys_are_rejected_before_sampling(self):
        prior = dict(doc_id='prior', lineid='1', entry_instance='1')
        duplicated = dict(source_key='1906:7', raw_text_adjusted='synthetic')
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            def fake_read(path):
                if Path(path).name == 'sample_1000.csv':
                    return [prior]
                return [duplicated, copy.deepcopy(duplicated)]
            with patch.object(sample_pilot, 'OUT', root / 'out'), \
                 patch.object(sample_pilot, 'DATA_ROOT', root), \
                 patch.object(sample_pilot, 'digest', return_value='synthetic'), \
                 patch.object(sample_pilot, 'read_csv', side_effect=fake_read):
                with self.assertRaises(AssertionError):
                    sample_pilot.prepare()

    def test_changed_split_is_rejected_before_holdout_predictions(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            manifest = dict(parser_sha256='parser', split_sha256='original',
                            reference_version='reference_v2',
                            reference_sha256='reference')
            (root / 'freeze_manifest.json').write_text(json.dumps(manifest))
            (root / 'split_manifest.json').write_text('{}')
            with patch.object(pilot, 'OUT', root), \
                 patch.object(pilot, 'digest', side_effect=['parser', 'changed']), \
                 patch.object(pilot, 'predict') as predict:
                with self.assertRaises(AssertionError):
                    pilot.holdout()
                predict.assert_not_called()

    def test_changed_reference_is_rejected_before_holdout_predictions(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            manifest = dict(parser_sha256='parser', split_sha256='split',
                            reference_version='reference_v2',
                            reference_sha256='original')
            (root / 'freeze_manifest.json').write_text(json.dumps(manifest))
            (root / 'split_manifest.json').write_text('{}')
            with patch.object(pilot, 'OUT', root), \
                 patch.object(pilot, 'digest', side_effect=['parser', 'split', 'changed']), \
                 patch.object(pilot, 'predict') as predict:
                with self.assertRaises(AssertionError):
                    pilot.holdout()
                predict.assert_not_called()

    def test_second_holdout_evaluation_is_rejected_before_predictions(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / 'freeze_manifest.json').write_text('{}')
            (root / 'split_manifest.json').write_text('{}')
            (root / 'holdout').mkdir()
            with patch.object(pilot, 'OUT', root), \
                 patch.object(pilot, 'predict') as predict:
                with self.assertRaises(AssertionError):
                    pilot.holdout()
                predict.assert_not_called()

    def test_live_reference_manifest_cannot_redirect_frozen_gold(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            frozen = dict(parser_sha256='parser', split_sha256='split',
                          reference_version='reference_v2',
                          reference_sha256='reference')
            (root / 'freeze_manifest.json').write_text(json.dumps(frozen))
            (root / 'split_manifest.json').write_text('{"holdout": [1]}')
            (root / 'reference_manifest.json').write_text(json.dumps(
                dict(version='reference_v1', sha256='different-reference')))
            (root / 'reference_v1.json').write_text('{"1": []}')
            (root / 'reference_v2.json').write_text('{"1": []}')

            def fake_digest(path):
                return {'frozen_education_regex.py': 'parser',
                        'split_manifest.json': 'split',
                        'reference_v2.json': 'reference'}.get(
                            Path(path).name, 'saved-output')

            with patch.object(pilot, 'OUT', root), \
                 patch.object(pilot, 'digest', side_effect=fake_digest), \
                 patch.object(pilot, 'parser'), \
                 patch.object(pilot, 'predict', return_value={1: {}}) as predict, \
                 patch.object(pilot, 'evaluate', return_value=([], [])):
                with self.assertRaises(AssertionError):
                    pilot.holdout()
                predict.assert_not_called()


if __name__ == '__main__':
    unittest.main()

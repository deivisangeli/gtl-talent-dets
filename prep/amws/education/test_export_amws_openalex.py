"""Boundary and preservation checks for the existing-assignment export."""
import json
import unittest

import export_amws_openalex as export


class ExportTests(unittest.TestCase):
    def award(self, family='ba_bs', position='1', year='1900', ids=None):
        ids = ids if ids is not None else ['I1', 'I2']
        return dict(requested_family=family, family_award_index=position,
                    award_index='3', institution_as_written='College A and College B',
                    label_as_written='B.A', year=year, year_raw='00',
                    assignment_class='["condition_satisfied"]', conditional_IDs='["I1"]',
                    flags='[]', matching_flags='[]', condition_conflict='False',
                    fully_matched='True', oa_ids=ids, oa_names=['A', 'B'][:len(ids)])

    def test_distinct_institution_slots_preserve_both_ids(self):
        values = export.award_values(self.award())
        self.assertEqual(values['ba_bs_award_1_institution_1_oa_id'], 'I1')
        self.assertEqual(values['ba_bs_award_1_institution_2_oa_id'], 'I2')
        self.assertEqual(values['ba_bs_award_1_conditional_ids'], '["I1"]')

    def test_repeated_awards_have_separate_columns(self):
        first = export.award_values(self.award())
        second = export.award_values(self.award(position='2'))
        self.assertFalse(set(first) & set(second))

    def test_missing_year_remains_blank(self):
        self.assertEqual(export.award_values(self.award(year=''))['ba_bs_award_1_year'], '')

    def test_unmatched_award_has_no_institution_id(self):
        award = self.award(ids=[])
        award['conditional_IDs'] = '[]'
        values = export.award_values(award)
        self.assertFalse(any(k.endswith('_oa_id') for k in values))
        self.assertEqual(values['ba_bs_award_1_raw_institution'], award['institution_as_written'])
        self.assertEqual(values['ba_bs_award_1_oa_id_count'], '0')

    def test_snapshot_membership_format_and_duplicate_rejection(self):
        records = {'I1': {}, 'I2': {}}
        self.assertEqual(export.assigned_ids({'IDs_after': '["I2","I1"]'}, records), ['I1', 'I2'])
        for value in ('["I3"]', '["I1","I1"]', '["not_an_id"]'):
            with self.assertRaises(ValueError):
                export.assigned_ids({'IDs_after': value}, records)

    def test_edition_specific_keys_and_numeric_forms(self):
        self.assertEqual(export.source_key({'entry_id': '33.0'}, export.PERSON_FILES[-1]), '1986:33')
        self.assertEqual(export.source_key({'edition': '1938', 'lineid': '33'}, export.PERSON_FILES[0]), '1938:33')
        with self.assertRaises(ValueError):
            export.integer('33.5')

    def test_provenance_preserves_absent_blank_and_conflicting_values(self):
        self.assertEqual(export.differing_fields({'city': '', 'x': 'old'}, {'x': 'new'}), ['city', 'x'])
        row = {'raw': 'A, B\nC', 'missing': '', 'ids': '["I1"]'}
        self.assertEqual(json.loads(export.dumps(row)), row)

    def test_honorary_and_nonqualifying_awards_excluded(self):
        r = dict(requested_family='phd', family_award_index='1', honorary='False')
        self.assertTrue(export.eligible_award(r))
        self.assertFalse(export.eligible_award(dict(r, honorary='True')))
        self.assertFalse(export.eligible_award(dict(r, family_award_index='')))
        self.assertFalse(export.eligible_award(dict(r, requested_family='')))


if __name__ == '__main__':
    unittest.main()

"""Check coordinate attachment boundaries and existing-cell preservation."""
import csv
import json
from pathlib import Path
import tempfile
import unittest

import attach_amws_openalex_coordinates as geo


class CoordinateTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.reference = {
            'I1': dict(id='I1', name='College A', latitude='42.3751', longitude='-71.10561',
                       city='Cambridge', country='US', valid_coordinates='True'),
            'I2': dict(id='I2', name='College B', latitude='0', longitude='0', city='B', country='GB', valid_coordinates='True'),
        }
        self.prefix = 'ba_bs_award_1_institution_1_'
        self.other = 'ba_bs_award_1_institution_2_'

    def write_csv(self, name, fields, rows):
        path = self.root / name
        with path.open('w', encoding='utf-8-sig', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=fields)
            writer.writeheader()
            writer.writerows(rows)
        return path

    def test_two_institutions_receive_separate_coordinates(self):
        result = geo.wide_coordinates({self.prefix + 'oa_id': 'I1', self.other + 'oa_id': 'I2'},
                                      [self.prefix, self.other], self.reference)
        self.assertEqual(result[self.prefix + 'latitude'], '42.3751')
        self.assertEqual(result[self.other + 'latitude'], '0')
        self.assertEqual(result[self.other + 'country'], 'GB')

    def test_empty_slots_are_blank_and_candidates_do_not_receive_coordinates(self):
        row = {self.prefix + 'oa_id': '', 'candidate_id': 'I1'}
        result = geo.wide_coordinates(row, [self.prefix], self.reference)
        self.assertTrue(all(v == '' for v in result.values()))
        arrays = geo.award_coordinates({'oa_ids': '[]', 'oa_names': '[]'}, self.reference)
        self.assertTrue(all(json.loads(v) == [] for v in arrays.values()))

    def test_repeated_degrees_have_independent_coordinate_columns(self):
        second = 'ba_bs_award_2_institution_1_'
        row = {self.prefix + 'oa_id': 'I1', second + 'oa_id': 'I2'}
        result = geo.wide_coordinates(row, [self.prefix, second], self.reference)
        self.assertNotEqual(result[self.prefix + 'latitude'], result[second + 'latitude'])

    def test_arrays_follow_existing_id_order(self):
        row = {'oa_ids': '["I2","I1"]', 'oa_names': '["B","A"]'}
        result = geo.award_coordinates(row, self.reference)
        self.assertEqual(json.loads(result['oa_latitudes']), [0, 42.3751])
        self.assertEqual(json.loads(result['oa_countries']), ['GB', 'US'])

    def test_duplicate_and_missing_reference_ids_are_rejected(self):
        rows = list(self.reference.values())
        fields = list(rows[0])
        path = self.write_csv('duplicate.csv', fields, [rows[0], rows[0]])
        with self.assertRaisesRegex(ValueError, 'Duplicate reference'):
            geo.load_reference(path, {'I1'})
        path = self.write_csv('missing.csv', fields, rows)
        with self.assertRaisesRegex(ValueError, 'missing from reference'):
            geo.load_reference(path, {'I3'})

    def test_invalid_coordinates_fail_but_zero_and_boundaries_are_valid(self):
        for latitude, longitude, valid in [('NaN', '0', 'True'), ('inf', '0', 'True'),
                                           ('91', '0', 'True'), ('0', '-181', 'True'),
                                           ('', '0', 'True'), ('1', '2', 'False')]:
            row = dict(self.reference['I1'], latitude=latitude, longitude=longitude, valid_coordinates=valid)
            path = self.write_csv('invalid.csv', list(row), [row])
            with self.assertRaises(ValueError):
                geo.load_reference(path, {'I1'})
        for latitude, longitude in [('0', '0'), ('-90', '-180'), ('90', '180')]:
            row = dict(self.reference['I1'], latitude=latitude, longitude=longitude)
            path = self.write_csv('valid.csv', list(row), [row])
            reference, count = geo.load_reference(path, {'I1'})
            self.assertEqual(reference['I1']['latitude'], latitude)
            self.assertEqual(count, 1)

    def test_original_conditional_and_birthplace_values_and_duplicates_preserved(self):
        fields = ['source_key', 'lat', 'lon', 'conditional_ids', self.prefix + 'oa_id']
        row = dict(zip(fields, ['1955:1', '10.0000', '20.1', '["I1"]', 'I1']))
        source = self.write_csv('source.csv', fields, [row, row])
        target = self.root / 'target.csv'
        prefixes, added = geo.transform_wide(source, target, self.reference)
        verified = list(geo.preserved_rows(source, target, added))
        self.assertEqual(len(verified), 2)
        self.assertEqual(verified[0][1]['lat'], '10.0000')
        self.assertEqual(verified[0][1]['conditional_ids'], '["I1"]')
        self.assertEqual(prefixes, [self.prefix])

    def test_schema_collision_and_unknown_id_fail(self):
        with self.assertRaises(ValueError):
            geo.coordinate_schema([self.prefix + 'oa_id', self.prefix + 'latitude'])
        with self.assertRaises(ValueError):
            geo.wide_coordinates({self.prefix + 'oa_id': 'I99'}, [self.prefix], self.reference)
        with self.assertRaises(ValueError):
            geo.award_coordinates({'oa_ids': '["I1","I1"]', 'oa_names': '["A","A"]'}, self.reference)


if __name__ == '__main__':
    unittest.main()

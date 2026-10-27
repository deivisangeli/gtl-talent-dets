"""Edition-aware filtering, missing flags and lossless retained-row checks."""
import csv
from pathlib import Path
import tempfile
import unittest

import publish_amws_open_alex as publish


class PublicationTests(unittest.TestCase):
    def test_earlier_editions_preserved_even_with_blank_or_false_flags(self):
        for edition in ('1906', '1938', '1955'):
            for flag in ('', None, 'FALSE', 'TRUE'):
                self.assertTrue(publish.decision({'source_key': edition + ':1', 'is_us_birth': flag})[1])

    def test_1986_true_flag_normalization(self):
        for flag in ('TRUE', 'true', 'True', ' true ', '\tTRUE\n'):
            self.assertEqual(publish.decision({'source_key': '1986:1', 'is_us_birth': flag}), ('1986', True, 'true'))

    def test_false_missing_and_unknown_flags_excluded(self):
        for flag, reason in [('FALSE', 'false'), (' false ', 'false'), ('', 'missing'),
                             (None, 'missing'), (' ', 'missing'), ('unknown', 'unrecognized'), ('1', 'unrecognized')]:
            self.assertEqual(publish.decision({'source_key': '1986:1', 'is_us_birth': flag}), ('1986', False, reason))
        self.assertFalse(publish.decision({'source_key': '1986:1'})[1])

    def test_source_key_overrides_blank_original_edition_column(self):
        self.assertTrue(publish.decision({'source_key': '1938:1', 'edition': '', 'is_us_birth': ''})[1])
        self.assertFalse(publish.decision({'source_key': '1986:1', 'edition': '1938', 'is_us_birth': 'FALSE'})[1])

    def test_invalid_or_unknown_source_keys_rejected(self):
        for key in ('', '1986', '1986:bad', '2020:1'):
            with self.assertRaises(ValueError):
                publish.decision({'source_key': key})

    def fixture(self, directory):
        source = directory / 'source.csv'
        fields = ['source_key', 'is_us_birth', 'birthplace', 'oa_ids', 'oa_id_count',
                  'oa_matched_award_count', 'oa_multi_institution_award_count', 'oa_award_count',
                  'ba_bs_award_1_institution_1_oa_id', 'ba_bs_award_1_institution_1_latitude']
        rows = []
        for key, flag in [('1906:1', ''), ('1986:1', 'FALSE'), ('1986:2', ' TRUE '), ('1986:3', '')]:
            rows.append(dict(zip(fields, [key, flag, 'A, B\nC', '["I1"]', '1', '1', '0', '1', 'I1', '10.0000'])))
        with source.open('w', encoding='utf-8-sig', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=fields)
            writer.writeheader()
            writer.writerows(rows)
        return source, fields, rows

    def test_filter_preserves_every_retained_cell_and_relative_order(self):
        with tempfile.TemporaryDirectory() as directory:
            source, fields, rows = self.fixture(Path(directory))
            target = Path(directory) / 'filtered.csv'
            counts = publish.write_filtered(source, target)
            validation = publish.validate_filtered(source, target)
            self.assertEqual(counts['after_rows'], 2)
            self.assertEqual(counts['exclusions_1986'], {'false': 1, 'missing': 1, 'unrecognized': 0})
            with target.open(encoding='utf-8-sig', newline='') as f:
                actual = list(csv.DictReader(f))
            self.assertEqual(actual, [rows[0], rows[2]])
            self.assertEqual(validation['coverage']['entries_with_id'], 2)
            self.assertEqual(validation['coverage']['distinct_ids'], 1)

    def test_duplicate_input_keys_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            source, fields, rows = self.fixture(Path(directory))
            with source.open('a', encoding='utf-8', newline='') as f:
                csv.DictWriter(f, fieldnames=fields).writerow(rows[0])
            with self.assertRaisesRegex(ValueError, 'Duplicate source key'):
                publish.write_filtered(source, Path(directory) / 'filtered.csv')

    def test_changed_retained_values_fail_validation(self):
        with tempfile.TemporaryDirectory() as directory:
            source, fields, rows = self.fixture(Path(directory))
            target = Path(directory) / 'filtered.csv'
            publish.write_filtered(source, target)
            with target.open(encoding='utf-8-sig', newline='') as f:
                changed = list(csv.DictReader(f))
            changed[0]['ba_bs_award_1_institution_1_latitude'] = '10.1'
            with target.open('w', encoding='utf-8-sig', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=fields)
                writer.writeheader()
                writer.writerows(changed)
            with self.assertRaisesRegex(ValueError, 'Retained fields'):
                publish.validate_filtered(source, target)


if __name__ == '__main__':
    unittest.main()

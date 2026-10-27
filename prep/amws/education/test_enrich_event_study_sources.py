"""Mapping, paired-list integrity, source joins and publication regression tests."""
import contextlib
import io
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import education_regex as parser
import enrich_event_study_sources as enrich
from pilot_io import read_csv, write_csv, write_json


class AdapterTests(unittest.TestCase):
    def prediction(self, text): return parser.extract(text, 1920, 1986)

    def test_separate_master_families_and_exclusions(self):
        p = self.prediction('Educ: Univ Alpha, MA,45, MS,47, MBA,49, MPhil,51, PhD,55. Prof Exp: Research.')
        fields, groups = enrich.project(p, parser)
        self.assertEqual([d['label_as_written'] for d in groups['ma']], ['MA'])
        self.assertEqual([d['label_as_written'] for d in groups['ms']], ['MS'])
        self.assertEqual(json.loads(fields['ms_graduation_years']), [1947])
        self.assertEqual(len(p['degrees']), 5)

    def test_complete_academic_master_label_mapping(self):
        for label in parser.LABELS['masters_academic']:
            award = dict(group='masters_academic', label_as_written=label)
            expected = 'ma' if label in ('MA', 'AM') else None if label in ('MPh', 'PhM', 'MPhil') else 'ms'
            self.assertEqual(enrich.family(award, parser), expected, label)
        for label in parser.LABELS['masters_professional']:
            self.assertIsNone(enrich.family(dict(group='masters_professional', label_as_written=label), parser))

    def test_multiple_awards_preserve_null_positions(self):
        p = self.prediction('Educ: Univ Alpha, BS,45; Univ Beta, BS; Univ Gamma, BS,53. Prof Exp: Research.')
        fields, groups = enrich.project(p, parser)
        self.assertEqual(json.loads(fields['ba_bs_institutions']), ['Univ Alpha', 'Univ Beta', 'Univ Gamma'])
        self.assertEqual(json.loads(fields['ba_bs_graduation_years']), [1945, None, 1953])
        self.assertEqual(json.loads(fields['ba_bs_graduation_years_raw']), ['45', None, '53'])
        self.assertEqual(fields['education_status'], 'partial_or_flagged')

    def test_honorary_excluded_but_hons_earned(self):
        p = self.prediction('Educ: Univ Alpha, BS Hons,45; hon. PhD,55. Prof Exp: Research.')
        fields, groups = enrich.project(p, parser)
        self.assertEqual(len(groups['ba_bs']), 1)
        self.assertEqual(groups['phd'], [])
        self.assertEqual(json.loads(fields['phd_institutions']), [])
        self.assertEqual(len(p['degrees']), 2)

    def test_ambiguous_year_retains_literal(self):
        p = parser.extract('A.B., Alpha, 04.', None, 1906)
        fields, groups = enrich.project(p, parser)
        self.assertEqual(json.loads(fields['ba_bs_graduation_years']), [None])
        self.assertEqual(json.loads(fields['ba_bs_graduation_years_raw']), ['04'])

    def test_medical_mb_bs_excluded(self):
        fields, groups = enrich.project(self.prediction('Educ: Univ Alpha, MB BS,45.'), parser)
        self.assertEqual(groups['ba_bs'], [])
        self.assertEqual(fields['education_status'], 'no_requested_earned_degree_match')

    def test_evidence_offset_validation_catches_corruption(self):
        text = 'Educ: Univ Alpha, BA,45.'
        p = self.prediction(text)
        enrich.validate_awards(text, p['degrees'], 1986)
        p['degrees'][0]['evidence'] = 'invented'
        with self.assertRaises(AssertionError): enrich.validate_awards(text, p['degrees'], 1986)

    def test_institution_from_career_outside_citation_rejected(self):
        text = 'Educ: Univ Alpha, BA,45. Prof Exp: Univ Beta.'
        p = self.prediction(text)
        p['degrees'][0]['institution_as_written'] = 'Univ Beta'
        with self.assertRaises(AssertionError): enrich.validate_awards(text, p['degrees'], 1986)

    def test_prebirth_literal_year_must_be_flagged(self):
        text = 'Educ: Univ Alpha, BA,1901.'
        p = self.prediction(text)
        enrich.validate_awards(text, p['degrees'], 1986, 1920)
        p['degrees'][0]['flags'] = []
        with self.assertRaises(AssertionError): enrich.validate_awards(text, p['degrees'], 1986, 1920)

    def test_source_keys_are_exact_not_fuzzy(self):
        file = Path(enrich.EARLY_FILES[0])
        self.assertEqual(enrich.source_key(dict(edition='1906', lineid='12.0'), file), '1906:12')
        with self.assertRaises(AssertionError): enrich.source_key(dict(edition='1906', lineid='oops'), file)


class PublicationTests(unittest.TestCase):
    def fixture(self, root):
        folder = root/'output/amws'; folder.mkdir(parents=True)
        for ed in (1906, 1938, 1955):
            raw = root/'input/amws'/f'amws_{ed}.{ "xlsx" if ed == 1906 else "dta" }'
            raw.parent.mkdir(parents=True, exist_ok=True); raw.write_text('fixture raw source')
            canonical = folder/('amws_1955_split_corrected.csv' if ed == 1955 else f'amws_{ed}_cleaned_corrected.csv')
            canonical.write_text('fixture canonical birth years')
        write_csv(folder/enrich.EARLY_FILES[0], [dict(edition=1906, lineid=1, geoid='01001', kept='TRUE'),
            dict(edition=1906, lineid=1, geoid='01003', kept='FALSE')])
        for ed, name in zip((1906, 1938, 1955), enrich.EARLY_FILES[1:]):
            write_csv(folder/name, [dict(lineid=2, state='MA')])
        late = root/'Data/processed/amws/amws_ed86_final.csv'
        write_csv(late, [dict(entry_id=1, raw_text_adjusted='Educ: Univ Alpha, MA,45, MS,47.', birth_year=1920)])
        pilot = folder/'education_pilot/multi_edition_regex_pilot'
        pilot.mkdir(parents=True)
        (pilot/'frozen_education_regex.py').write_bytes((enrich.HERE/'education_regex.py').read_bytes())
        write_json(pilot/'freeze_manifest.json', dict(version='multi_v03', parser_sha256=enrich.EXPECTED_HASH))
        write_json(pilot/'split_manifest.json', dict(source_sha256={}))
        rows = {}
        for ed, lineid in ((1906,1),(1906,2),(1938,2),(1955,2)):
            rows[f'{ed}:{lineid}'] = dict(edition=str(ed), birth_year='1870',
                raw_text_adjusted='A.B., Alpha, 04.', encoding_note='fixture')
        return rows

    def test_end_to_end_preserves_duplicate_rows_and_reconciles_ledger(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp); rows = self.fixture(root); dest = root/'output/amws/education_enriched'
            with patch.object(enrich, 'raw_early', return_value=rows), contextlib.redirect_stdout(io.StringIO()):
                enrich.run(root, dest, Path('unused'))
            original = read_csv(root/'output/amws'/enrich.EARLY_FILES[0])
            enriched = read_csv(dest/'amws_combined_us_geocoded_education.csv')
            self.assertEqual(len(enriched), 2)
            self.assertEqual([{k: r[k] for k in original[0]} for r in enriched], original)
            self.assertEqual(enriched[0]['ba_bs_institutions'], enriched[1]['ba_bs_institutions'])
            validation = json.loads((dest/'validation_report.json').read_text())
            self.assertTrue(validation['all_checks_passed'])
            self.assertEqual(validation['distinct_source_records'], 5)
            self.assertEqual(validation['award_rows'], 6)
            with self.assertRaises(AssertionError): enrich.run(root, dest, Path('unused'))

    def test_missing_join_blocks_publication(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp); rows = self.fixture(root); del rows['1906:1']
            dest = root/'output/amws/education_enriched'
            with patch.object(enrich, 'raw_early', return_value=rows):
                with self.assertRaises(ValueError): enrich.run(root, dest, Path('unused'))
            self.assertFalse(dest.exists())
            self.assertTrue((dest.parent/'education_enrichment_join_failures.json').exists())

    def test_verified_append_reproduces_old_entries_and_reports_additions(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp); rows = self.fixture(root)
            previous = root/'output/amws/education_enriched'
            with patch.object(enrich, 'raw_early', return_value=rows), contextlib.redirect_stdout(io.StringIO()):
                enrich.run(root, previous, Path('unused'))
            late = root/'Data/processed/amws/amws_ed86_final.csv'
            updated = read_csv(late)
            updated += [dict(entry_id=2, raw_text_adjusted='Educ: Univ Beta, BA,45; Univ Gamma, PhD,50.', birth_year=1920),
                        dict(entry_id=3, raw_text_adjusted='Prof Exp: Research.', birth_year=1920)]
            write_csv(late, updated)
            baseline = root/'archive'
            baseline.mkdir()
            dest = root/'output/amws/education_updated'
            with patch.object(enrich, 'raw_early', return_value=rows), contextlib.redirect_stdout(io.StringIO()):
                enrich.run(root, dest, Path('unused'), baseline, previous)
            refresh = json.loads((dest/'refresh_validation.json').read_text())
            self.assertEqual(refresh['previous_1986_entries_reproduced'], 1)
            self.assertEqual(refresh['new_1986_entries'], 2)
            self.assertEqual(refresh['new_entry_coverage'][0]['institution'], 1)
            self.assertEqual(refresh['new_entry_coverage'][0]['resolved_year'], 1)
            self.assertEqual(refresh['new_entry_coverage'][0]['no_requested_earned_degree_match'], 1)
            self.assertEqual(read_csv(previous/late.with_name('amws_ed86_final_education.csv').name)[0],
                             read_csv(dest/'amws_ed86_final_education.csv')[0])
            self.assertEqual(enrich.digest(dest/'frozen_education_regex.py'), enrich.EXPECTED_HASH)

    def test_changed_previous_person_blocks_refresh(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp); rows = self.fixture(root)
            previous = root/'output/amws/education_enriched'
            with patch.object(enrich, 'raw_early', return_value=rows), contextlib.redirect_stdout(io.StringIO()):
                enrich.run(root, previous, Path('unused'))
            late = root/'Data/processed/amws/amws_ed86_final.csv'
            updated = read_csv(late); updated[0]['birth_year'] = '1921'; write_csv(late, updated)
            baseline = root/'archive'; baseline.mkdir()
            dest = root/'output/amws/education_updated'
            with self.assertRaisesRegex(AssertionError, 'Baseline row changed'):
                enrich.run(root, dest, Path('unused'), baseline, previous)
            self.assertFalse(dest.exists())

    def test_changed_previous_extraction_blocks_publication(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp); rows = self.fixture(root)
            previous = root/'output/amws/education_enriched'
            with patch.object(enrich, 'raw_early', return_value=rows), contextlib.redirect_stdout(io.StringIO()):
                enrich.run(root, previous, Path('unused'))
            records = read_csv(previous/'education_records.csv')
            records[0]['ba_bs_institutions'] = '["Wrong institution"]'
            write_csv(previous/'education_records.csv', records)
            baseline = root/'archive'; baseline.mkdir()
            dest = root/'output/amws/education_updated'
            with patch.object(enrich, 'raw_early', return_value=rows):
                with self.assertRaisesRegex(AssertionError, 'Previous extraction differs'):
                    enrich.run(root, dest, Path('unused'), baseline, previous)
            self.assertFalse(dest.exists())


class PilotAppendTests(unittest.TestCase):
    def fixture(self, root):
        path = root/'Data/intermediary/amws/amws_ed86_full.csv'
        write_csv(path, [dict(doc_id='A', lineid=1, raw_text_adjusted='Educ: Alpha, BA,45.')])
        baseline = root/'archive'
        old = baseline/path.relative_to(root)
        old.parent.mkdir(parents=True)
        old.write_bytes(path.read_bytes())
        split = dict(source_sha256={str(path): enrich.digest(path)})
        return path, baseline, split

    def test_verified_pilot_append_accepted_without_changing_freeze(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp); path, baseline, split = self.fixture(root)
            write_csv(path, read_csv(path) + [dict(doc_id='B', lineid=2, raw_text_adjusted='Educ: Beta, MS,50.')])
            result = enrich.verify_pilot_sources(root, split, baseline)
            self.assertEqual(result['baseline_rows'], 1)
            self.assertEqual(result['added_rows'], 1)
            self.assertEqual(result['baseline_sha256'], split['source_sha256'][str(path)])
            with self.assertRaisesRegex(AssertionError, 'Pilot input has changed'):
                enrich.verify_pilot_sources(root, split)

    def test_modified_archived_baseline_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp); path, baseline, split = self.fixture(root)
            write_csv(path, read_csv(path) + [dict(doc_id='B', lineid=2, raw_text_adjusted='Extra')])
            (baseline/path.relative_to(root)).write_text('tampered')
            with self.assertRaisesRegex(AssertionError, 'Archived pilot baseline differs'):
                enrich.verify_pilot_sources(root, split, baseline)

    def test_modified_original_pilot_row_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp); path, baseline, split = self.fixture(root)
            updated = read_csv(path); updated[0]['raw_text_adjusted'] = 'Altered'
            updated.append(dict(doc_id='B', lineid=2, raw_text_adjusted='Extra'))
            write_csv(path, updated)
            with self.assertRaisesRegex(AssertionError, 'Baseline row changed'):
                enrich.verify_pilot_sources(root, split, baseline)

    def test_unrelated_changed_pilot_source_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp); path, baseline, split = self.fixture(root)
            other = root/'input/amws/other.csv'; write_csv(other, [dict(value='old')])
            split['source_sha256'][str(other)] = enrich.digest(other)
            write_csv(other, [dict(value='new')])
            with self.assertRaisesRegex(AssertionError, 'Pilot input has changed'):
                enrich.verify_pilot_sources(root, split, baseline)


if __name__ == '__main__': unittest.main()

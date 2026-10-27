"""Tests for the manual audit's sampling, evidence and protected outputs."""
import copy
import tempfile
from pathlib import Path
import unittest
from unittest.mock import patch

import amws_workplace_feasibility as audit


class SamplingTests(unittest.TestCase):
    def test_draw_has_20_per_edition_unique_ids_and_one_shared_rng(self):
        groups={ed:[dict(source_key=f"{ed}:{i}",edition=ed) for i in range(50)] for ed in audit.EDITIONS}
        sample=audit.draw_sample(groups)
        self.assertEqual([r['sample_id'] for r in sample],[str(i) for i in range(1,81)])
        self.assertEqual(len({r['source_key'] for r in sample}),80)
        self.assertEqual(audit.collections.Counter(r['edition'] for r in sample),{ed:20 for ed in audit.EDITIONS})
        reverse={ed:list(reversed(rows)) for ed,rows in groups.items()}
        self.assertEqual(sample,audit.draw_sample(reverse))
        rng=audit.random.Random(47)
        expected=[]
        for ed in audit.EDITIONS:expected += [r['source_key'] for r in rng.sample(sorted(groups[ed],key=lambda r:r['source_key']),20)]
        self.assertEqual([r['source_key'] for r in sample],expected)


class EvidenceTests(unittest.TestCase):
    def setUp(self):
        self.row=dict(sample_id='1',source_key='1955:1',edition='1955',address_text='Dept Physics, Alpha University, Town, Mass.',
                      birthplace_text='Otherplace, Ohio',biography_text='Prof, Alpha University, 50-; instructor, Beta College, 40-49.')
        ev=[audit.evidence(self.row,'address_text',self.row['address_text']),
            audit.evidence(self.row,'biography_text','Prof, Alpha University, 50-')]
        self.work=dict(employer_as_written='Alpha University',university_as_written='Alpha University',role_as_written='Prof',
                       employer_type='university',country_scope='US',employer_status='explicit',university_status='explicit',
                       state_status='explicit',state_as_written='Mass.',city_as_written='Town',county_status='lookup_needed',county_as_written='',
                       evidence=ev,explanation='Departmental address corroborates current university work; county needs lookup.')
        self.review=dict(sample_id='1',source_key='1955:1',edition='1955',full_entry_reviewed=True,employment_status='active',workplaces=[self.work],
                         history_status='available',history_date_clarity='two_digit_or_open_ended',
                         history_evidence=[audit.evidence(self.row,'biography_text','instructor, Beta College, 40-49')],notes='Current and historical roles are separate.')

    def test_literal_evidence_and_values_accepted(self):
        audit.check_review(self.review,self.row)

    def test_birthplace_field_rejected_as_work_evidence(self):
        with self.assertRaises(ValueError):audit.evidence(self.row,'birthplace_text','Otherplace, Ohio')

    def test_inferred_state_or_county_cannot_be_filled(self):
        bad=copy.deepcopy(self.review)
        bad['workplaces'][0]['county_as_written']='Example County'
        with self.assertRaises(ValueError):audit.check_review(bad,self.row)
        bad=copy.deepcopy(self.review)
        bad['workplaces'][0]['state_as_written']='Ohio'
        with self.assertRaises(ValueError):audit.check_review(bad,self.row)

    def test_evidence_offsets_and_quotes_cannot_change(self):
        bad=copy.deepcopy(self.review)
        bad['workplaces'][0]['evidence'][0]['start']=1
        with self.assertRaises(ValueError):audit.check_review(bad,self.row)

    def test_city_requires_literal_support(self):
        bad=copy.deepcopy(self.review)
        bad['workplaces'][0]['city_as_written']='Imagined City'
        with self.assertRaises(ValueError):audit.check_review(bad,self.row)

    def test_foreign_workplace_not_assigned_us_county(self):
        bad=copy.deepcopy(self.review)
        bad['workplaces'][0]['country_scope']='foreign'
        with self.assertRaises(ValueError):audit.check_review(bad,self.row)
        bad['workplaces'][0].update(state_status='not_applicable',state_as_written='',county_status='not_applicable')
        audit.check_review(bad,self.row)

    def test_non_university_is_not_a_missing_university(self):
        bad=copy.deepcopy(self.review)
        bad['workplaces'][0]['employer_type']='other'
        with self.assertRaises(ValueError):audit.check_review(bad,self.row)
        bad['workplaces'][0].update(university_status='not_applicable',university_as_written='')
        audit.check_review(bad,self.row)

    def test_missing_history_decision_not_inferred(self):
        bad=copy.deepcopy(self.review)
        bad['history_evidence']=[]
        with self.assertRaises(ValueError):audit.check_review(bad,self.row)

    def test_aggregation_means_at_least_one_not_every_workplace(self):
        review=copy.deepcopy(self.review)
        other=copy.deepcopy(self.work)
        other.update(state_status='ambiguous',state_as_written='')
        review['workplaces'].append(other)
        self.assertEqual(audit.record_status(review,'state'),'explicit')
        self.assertEqual(audit.record_status(dict(workplaces=[]),'state'),'unavailable')


class ImmutabilityTests(unittest.TestCase):
    def test_completed_release_refuses_more_reviews(self):
        with tempfile.TemporaryDirectory() as temp:
            path=Path(temp)
            (path/'completion_manifest.json').write_text('{}')
            with patch.object(audit,'destination',return_value=path):
                with self.assertRaisesRegex(ValueError,'immutable'):audit.append_reviews([])

    def test_prepare_does_not_overwrite_existing_sample(self):
        with tempfile.TemporaryDirectory() as temp:
            path=Path(temp)
            sample=path/'sample_80.csv';sample.write_text('preserve')
            with patch.object(audit,'destination',return_value=path):
                with self.assertRaisesRegex(ValueError,'already exists'):audit.prepare()
            self.assertEqual(sample.read_text(),'preserve')


if __name__=='__main__':unittest.main()

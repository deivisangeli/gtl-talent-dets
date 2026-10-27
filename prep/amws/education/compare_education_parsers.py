"""Compare the frozen multi_v03 parser with multi_v04 on every source biography.

Both parsers run with the postsecondary label extension, so the comparison
matches the scope of amws_education_postsecondary_20261019. Writes, under the
unknown-training audit folder:

- award_diff.csv            every award gained or lost, keyed by literal offsets
- award_diff_summary.csv    gained/lost counts by edition and group
- spotcheck_gained.csv      a seeded sample of gained training awards to read
- classification_estimate.csv / _summary.csv
                            50 km class of the 12,360 college-allocation entries,
                            re-derived for both parsers with exact-text reuse of
                            published institution assignments

The class estimate reuses an assignment only when a published award has the same
institution text, family and resolved year (then text and year), which is how
conditional crosswalk decisions are keyed. It is validated by re-deriving the
published classes from the v03 awards. Nothing is republished.
"""
import argparse
import collections
import csv
import json
import random
import sys
from pathlib import Path

import education_regex as v03
import education_regex_v04 as v04
import enrich_event_study_sources as enrichment
import postsecondary_qualifications as pq
from pilot_io import DATA_ROOT, write_csv, write_json
from postsecondary_qualifications_v04 import extra_awards, with_extra_labels

csv.field_size_limit(min(sys.maxsize, 2**31 - 1))
if hasattr(sys.stdout, 'reconfigure'):
    sys.stdout.reconfigure(encoding='utf-8')

BIOS = 'output/amws/education_enriched_20261011_updated_final/source_biographies.csv'
RELEASE = 'output/amws/amws_education_postsecondary_20261019'
OUTCOMES = 'output/land_grants/amws_birth_county_training_postsecondary_20261019'
OUT = 'output/amws/education_audit_unknown_training_20261019'
RADIUS_M = 50000
# 20261019 drew the development spot check; 20261020 is the frozen-rule check.
SPOTCHECK_SEED, SPOTCHECK_N = 20261020, 30
INCLUDED = ('eligible', 'unresolved_level')
PUBLISHED = {'ever': 'ever_nearby', 'never': 'never_nearby'}


def awards(parser, ext, text, birth_year, edition):
    result = parser.extract(text, birth_year, edition)
    ds = result['degrees'] + extra_awards(ext, text, birth_year, edition)
    for d in ds:
        d['requested_family'] = enrichment.family(d, parser)
        fam, elig = pq.qualification(d)
        if 'board_certification' in d['flags']:
            fam, elig = None, 'board_certification'
        d['family'], d['training_eligibility'] = fam, elig
    return result['flags'], sorted(ds, key=lambda d: d['label_start'])


def key(d):
    return (d['label_start'], d['label_end'], d['label_as_written'], d['institution_as_written'] or '',
            d['year_raw'] or '', d['training_eligibility'])


def birth(value):
    return enrichment.integer(value)


class Locator:
    """Exact-text reuse of published assignments; distances from the release."""

    def __init__(self, root):
        self.by_family, self.by_year = {}, {}
        with (root / RELEASE / 'education_awards.csv').open(encoding='utf-8-sig', newline='') as f:
            for r in csv.DictReader(f):
                if r['training_eligibility'] != 'eligible' or not r['institution_as_written']:
                    continue
                ids = json.loads(r['oa_ids'] or '[]')
                ok = r['fully_matched'] == 'True' and r['condition_conflict'] != 'True' and bool(ids)
                value = tuple(sorted(ids)) if ok else None
                for table, k in ((self.by_family, (r['institution_as_written'], r['year'], r['requested_family'])),
                                 (self.by_year, (r['institution_as_written'], r['year']))):
                    if value is not None or k not in table:
                        table[k] = value if value is not None else table.get(k)
        self.distance = {}
        with (root / OUTCOMES / 'county_institution_distances.csv').open(encoding='utf-8-sig', newline='') as f:
            for r in csv.DictReader(f):
                self.distance[(r['GEOID'], r['openalex_id'])] = float(r['distance_m'])

    def ids(self, d):
        if d['training_eligibility'] != 'eligible' or not d['institution_as_written']:
            return None
        year = '' if d['year'] is None else str(d['year'])
        k = (d['institution_as_written'], year, d['family'])
        if k in self.by_family:
            return self.by_family[k]
        return self.by_year.get((d['institution_as_written'], year))

    def classify(self, geoid, ds):
        included = [d for d in ds if d['training_eligibility'] in INCLUDED]
        if not included:
            return 'unknown', 'no_qualifying_award'
        dists, complete = [], True
        for d in included:
            ids = self.ids(d)
            located = [self.distance.get((geoid, i)) for i in ids] if ids else []
            dists += [x for x in located if x is not None]
            if not ids or any(x is None for x in located):
                complete = False
        if dists and min(dists) <= RADIUS_M:
            return 'ever_nearby', ''
        if complete and dists:
            return 'never_nearby', ''
        if all(not d['institution_as_written'] for d in included):
            return 'unknown', 'no_institution_text'
        return 'unknown', 'incomplete'


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--data-root', default=str(DATA_ROOT))
    args = ap.parse_args()
    root = Path(args.data_root)
    out = root / OUT
    ext3, ext4 = pq.EXTENDED, with_extra_labels(v04)
    eligible = {}
    with (root / OUTCOMES / 'entry_classification_and_exclusions.csv').open(encoding='utf-8-sig', newline='') as f:
        for r in csv.DictReader(f):
            if r['exclusion_reason'] == 'eligible':
                eligible[r['source_key']] = r
    diff, summary, per_entry = [], collections.Counter(), {}
    entries = collections.Counter()
    with (root / BIOS).open(encoding='utf-8-sig', newline='') as f:
        for n, r in enumerate(csv.DictReader(f), 1):
            text, ed, by = r['raw_text_adjusted'] or '', int(r['edition']), birth(r['birth_year_used'])
            f3, a3 = awards(v03, ext3, text, by, ed)
            f4, a4 = awards(v04, ext4, text, by, ed)
            k3, k4 = {key(d): d for d in a3}, {key(d): d for d in a4}
            for sign, src, other in (('lost', k3, k4), ('gained', k4, k3)):
                for k, d in src.items():
                    if k in other:
                        continue
                    summary[(ed, d['group'], d['training_eligibility'], sign)] += 1
                    diff.append(dict(source_key=r['source_key'], edition=ed, change=sign, group=d['group'],
                                     family=d['family'] or '', training_eligibility=d['training_eligibility'],
                                     label_as_written=d['label_as_written'], institution_as_written=d['institution_as_written'] or '',
                                     year_raw=d['year_raw'] or '', year=d['year'] if d['year'] is not None else '',
                                     flags=';'.join(d['flags']), record_flags=';'.join(f4 if sign == 'gained' else f3),
                                     label_start=d['label_start'], evidence=d['evidence']))
            t3 = any(d['training_eligibility'] in INCLUDED for d in a3)
            t4 = any(d['training_eligibility'] in INCLUDED for d in a4)
            entries[(ed, t3, t4)] += 1
            if r['source_key'] in eligible:
                per_entry[r['source_key']] = (a3, a4)
            if n % 20000 == 0:
                print('parsed', n, flush=True)
    write_csv(out / 'award_diff.csv', diff)
    write_csv(out / 'award_diff_summary.csv', [
        dict(edition=e, group=g, training_eligibility=t, change=s, awards=c) for (e, g, t, s), c in sorted(summary.items())])
    write_csv(out / 'entry_training_award_presence.csv', [
        dict(edition=e, any_training_award_v03=a, any_training_award_v04=b, entries=c) for (e, a, b), c in sorted(entries.items())])

    gained = [d for d in diff if d['change'] == 'gained' and d['training_eligibility'] in INCLUDED]
    write_csv(out / 'spotcheck_gained.csv', random.Random(SPOTCHECK_SEED).sample(gained, min(SPOTCHECK_N, len(gained))))

    loc = Locator(root)
    rows, transitions, agree = [], collections.Counter(), collections.Counter()
    for sk, (a3, a4) in sorted(per_entry.items()):
        e = eligible[sk]
        c3, r3 = loc.classify(e['GEOID'], a3)
        c4, r4 = loc.classify(e['GEOID'], a4)
        agree[(PUBLISHED.get(e['class_50km'], e['class_50km']), c3)] += 1
        transitions[(e['class_50km'], c3, c4)] += 1
        rows.append(dict(source_key=sk, edition=e['edition'], GEOID=e['GEOID'], published_class_50km=e['class_50km'],
                         published_unknown_reason=e['unknown_reason_50km'], simulated_v03=c3, simulated_v03_reason=r3,
                         simulated_v04=c4, simulated_v04_reason=r4))
    write_csv(out / 'classification_estimate.csv', rows)
    write_csv(out / 'classification_estimate_summary.csv', [
        dict(published=p, simulated_v03=a, simulated_v04=b, entries=c) for (p, a, b), c in sorted(transitions.items())])
    matched = sum(c for (p, s), c in agree.items() if p == s)
    report = dict(
        bios=sum(entries.values()), awards_gained=sum(1 for d in diff if d['change'] == 'gained'),
        awards_lost=sum(1 for d in diff if d['change'] == 'lost'),
        college_allocation_entries=len(per_entry),
        simulation_agreement_v03_vs_published=round(matched / len(per_entry), 4),
        unknown_published=sum(1 for e in eligible.values() if e['class_50km'] == 'unknown'),
        unknown_simulated_v03=sum(1 for r in rows if r['simulated_v03'] == 'unknown'),
        unknown_simulated_v04=sum(1 for r in rows if r['simulated_v04'] == 'unknown'),
        v04_class_counts=collections.Counter(r['simulated_v04'] for r in rows),
        v03_class_counts=collections.Counter(r['simulated_v03'] for r in rows))
    write_json(out / 'comparison_report.json', report)
    print(json.dumps(report, indent=1))


if __name__ == '__main__':
    main()

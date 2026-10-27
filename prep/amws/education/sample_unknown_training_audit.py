"""Draw the unknown-training audit sample and write its review packets.

Pool: eligible college-allocation entries whose 50 km training class is
`unknown` in the postsecondary release. One seeded draw of 150 keys from the
source-key-sorted pool; the first 100 are the development sample and the next
50 a holdout that is read only after the multi_v04 rules are frozen.
"""
import argparse
import csv
import html
import json
import random
import sys
from pathlib import Path

from pilot_io import DATA_ROOT, digest, write_json

csv.field_size_limit(min(sys.maxsize, 2**31 - 1))
if hasattr(sys.stdout, 'reconfigure'):
    sys.stdout.reconfigure(encoding='utf-8')

SEED = 20261019
N_DEV, N_HOLDOUT = 100, 50
OUTCOMES = 'output/land_grants/amws_birth_county_training_postsecondary_20261019'
RELEASE = 'output/amws/amws_education_postsecondary_20261019'
BIOS = 'output/amws/education_enriched_20261011_updated_final/source_biographies.csv'
OUT = 'output/amws/education_audit_unknown_training_20261019'
AWARD_FIELDS = ['award_index', 'requested_family', 'group', 'label_as_written', 'institution_as_written',
                'year_raw', 'year', 'honorary', 'evidence', 'flags', 'assignment_class', 'fully_matched',
                'training_eligibility', 'oa_names']
LOCATION_FIELDS = ['award_index', 'family', 'raw_institution', 'degree_label', 'n_ids', 'n_locations',
                   'min_distance_m', 'complete']


def rows(path, keep=None):
    with Path(path).open(encoding='utf-8-sig', newline='') as f:
        for r in csv.DictReader(f):
            if keep is None or r['source_key'] in keep:
                yield r


def draw(pool):
    keys = random.Random(SEED).sample(sorted(pool), N_DEV + N_HOLDOUT)
    return keys[:N_DEV], keys[N_DEV:]


def packet(keys, entries, bios, awards, locations):
    out = []
    for rank, k in enumerate(keys, 1):
        e, b = entries[k], bios[k]
        out.append(dict(
            sample_rank=rank, source_key=k, edition=int(e['edition']), birth_year=e['year'],
            birth_geoid=e['GEOID'], unknown_reason_50km=e['unknown_reason_50km'],
            education_status=e['education_status'], n_reported_awards=int(e['n_reported_awards'] or 0),
            raw_text=b['raw_text_adjusted'], birth_year_used=b['birth_year_used'],
            awards=[{f: a[f] for f in AWARD_FIELDS} for a in awards.get(k, [])],
            locations=[{f: a[f] for f in LOCATION_FIELDS} for a in locations.get(k, [])]))
    return out


def write_html(path, entries, title):
    parts = [f'<!doctype html><meta charset="utf-8"><title>{html.escape(title)}</title>',
             '<style>body{font:14px/1.45 system-ui;max-width:1000px;margin:auto;padding:16px}'
             'pre{white-space:pre-wrap;background:#f4f4f4;padding:8px}td,th{border:1px solid #ccc;padding:2px 6px}'
             'table{border-collapse:collapse;font-size:12px}</style>', f'<h1>{html.escape(title)}</h1>']
    for e in entries:
        parts.append(f"<h2>{e['sample_rank']}. {e['source_key']} — {html.escape(e['unknown_reason_50km'])}</h2>")
        parts.append(f"<pre>{html.escape(e['raw_text'])}</pre>")
        if e['awards']:
            parts.append('<table><tr>' + ''.join(f'<th>{f}</th>' for f in AWARD_FIELDS[:12]) + '</tr>')
            for a in e['awards']:
                parts.append('<tr>' + ''.join(f'<td>{html.escape(a[f] or "")}</td>' for f in AWARD_FIELDS[:12]) + '</tr>')
            parts.append('</table>')
    Path(path).write_text('\n'.join(parts), encoding='utf-8')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--data-root', default=str(DATA_ROOT))
    args = ap.parse_args()
    root = Path(args.data_root)
    classes = root / OUTCOMES / 'entry_classification_and_exclusions.csv'
    entries = {r['source_key']: r for r in rows(classes)
               if r['exclusion_reason'] == 'eligible' and r['class_50km'] == 'unknown'}
    dev, holdout = draw(entries)
    chosen = set(dev) | set(holdout)
    bios = {r['source_key']: r for r in rows(root / BIOS, chosen)}
    awards, locations = {}, {}
    for r in rows(root / RELEASE / 'education_awards.csv', chosen):
        awards.setdefault(r['source_key'], []).append(r)
    for r in rows(root / OUTCOMES / 'award_location_evidence.csv', chosen):
        locations.setdefault(r['source_key'], []).append(r)
    assert chosen <= set(bios), sorted(chosen - set(bios))
    out = root / OUT
    out.mkdir(parents=True, exist_ok=True)
    for name, keys in (('dev', dev), ('holdout', holdout)):
        p = packet(keys, entries, bios, awards, locations)
        with (out / f'review_packet_{name}.jsonl').open('w', encoding='utf-8', newline='\n') as f:
            for e in p:
                f.write(json.dumps(e, ensure_ascii=False) + '\n')
        if name == 'dev':
            write_html(out / 'review_packet_dev.html', p, 'Unknown-training audit — development sample')
    inputs = [classes, root / BIOS, root / RELEASE / 'education_awards.csv',
              root / OUTCOMES / 'award_location_evidence.csv']
    write_json(out / 'sample_manifest.json', dict(
        seed=SEED, pool_size=len(entries), n_dev=N_DEV, n_holdout=N_HOLDOUT,
        sampling='random.Random(seed).sample over the source_key-sorted pool; first 100 dev, next 50 holdout',
        filter="exclusion_reason == 'eligible' and class_50km == 'unknown'",
        dev_keys=dev, holdout_keys=holdout,
        input_sha256={str(p.relative_to(root)): digest(p) for p in inputs}))
    print('pool', len(entries), 'dev', len(dev), 'holdout', len(holdout), '->', out)


if __name__ == '__main__':
    main()

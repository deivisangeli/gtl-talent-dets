"""Inventory updated AMWS earned-degree institutions for manual online research."""
import argparse
import collections
import csv
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import sys

SKILL = Path.home()/'.codex/skills/recover-openalex-institution-id'
sys.path.insert(0, str(SKILL/'scripts'))
import research_batches as batches


def digest(path):
    value = hashlib.sha256()
    with path.open('rb') as source:
        for block in iter(lambda: source.read(1048576), b''):
            value.update(block)
    return value.hexdigest()


def build(root, output):
    release = root/'output/amws/education_enriched_20261011_updated_final'
    old = root/'output/amws/education_geolocated'
    ledger = release/'education_awards.csv'
    biographies = release/'source_biographies.csv'
    crosswalk = {r['name']: r for r in batches.read_json(old/'published_crosswalk.json')}
    inventory = collections.defaultdict(list)
    with ledger.open(encoding='utf-8-sig', newline='') as source:
        for award in csv.DictReader(source):
            if award['requested_family'] in ('ba_bs', 'ma', 'ms', 'phd') and award['family_award_index'] and award['honorary'] == 'False' and award['institution_as_written']:
                inventory[award['institution_as_written']].append(award)
    retained = []; needed = []
    for name, awards in inventory.items():
        prior = crosswalk.get(name)
        if prior and prior.get('accepted_ids') and not prior.get('conditions'):
            retained.append(prior)
        else:
            needed.append((name, awards, prior))
    needed.sort(key=lambda item: (-len(item[1]), item[0]))
    records = []
    for number, (name, awards, prior) in enumerate(needed, 1):
        representatives = {}
        for award in awards:
            year = award['year']
            decade = int(year)//10*10 if year else None
            key = (award['edition'], award['requested_family'], decade)
            representatives.setdefault(key, award)
        examples = list(representatives.values())
        examples = examples[:12] if len(examples) <= 12 else [examples[int(i*(len(examples)-1)/11)] for i in range(12)]
        records.append(dict(record_id=f'institution-{number:06d}', institution_as_written=name,
                            award_count=len(awards), editions=sorted({a['edition'] for a in awards}),
                            families=sorted({a['requested_family'] for a in awards}),
                            contexts=examples,
                            award_refs=[dict(source_key=a['source_key'], award_index=a['award_index']) for a in awards],
                            award_evidence_file=str(ledger), source_biographies_file=str(biographies),
                            prior_status=prior.get('status') if prior else 'new_name',
                            prior_candidates=prior.get('candidates', []) if prior else [],
                            prior_conditions=prior.get('conditions', []) if prior else [],
                            prior_accepted_ids=prior.get('accepted_ids', []) if prior else []))
    expected = dict(all_names=len(inventory), retained_unconditional=len(retained), research_names=len(records))
    assert expected == dict(all_names=18909, retained_unconditional=1449, research_names=17460), expected
    manifest = batches.prepare(records, output, 100)
    batches.atomic_json(output/'retained_unconditional_matches.json', retained)
    batches.atomic_json(output/'retained_conditional_matches.json', [r for r in crosswalk.values() if r.get('conditions') and r['name'] in inventory])
    files = [ledger, biographies, release/'manifest.json', old/'published_crosswalk.json']
    batches.atomic_json(output/'source_manifest.json', dict(**expected, sources={str(p): digest(p) for p in files},
                                                           skill_path=str(SKILL), selection='unresolved_exact_literal_names'))
    report = batches.aggregate(output)
    print(json.dumps(dict(**expected, batches=len(manifest['batches']), final_batch=manifest['batches'][-1]['records']), indent=2))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-root', type=Path, default=Path(os.environ.get('TALENT_DETS_DATA_DIR', str(Path.home()/'Globtalent Dropbox/gtl_talent_dets'))))
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    root = args.data_root.resolve()
    build(root, (args.output or root/'output/amws/openalex_id_research_20261011').resolve())

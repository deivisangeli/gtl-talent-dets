"""Report actual award coverage of documented ID conditions; change no mappings."""
import argparse
import csv
import datetime
import hashlib
import importlib.util
import os
from pathlib import Path
import sys

sys.path.insert(0, str(Path.home()/'.codex/skills/recover-openalex-institution-id/scripts'))
import research_batches as batches


def fields(condition):
    if 'any' in condition:
        return set().union(*(fields(group) for group in condition['any']))
    return {test['field'] for test in condition['all']}


def earned(award):
    return (award['requested_family'] in ('ba_bs', 'ma', 'ms', 'phd')
            and award['family_award_index'] and award['honorary'] == 'False')


def validate(run):
    results = batches.read_input(run/'research_results.jsonl')
    conditional = {r['institution_as_written']: r for r in results if r['status'] == 'conditional'}
    inputs = batches.read_input(run/'inventory.jsonl')
    inventory_counts = {r['institution_as_written']: r['award_count'] for r in inputs}
    ledger = Path(inputs[0]['award_evidence_file'])
    biographies = Path(inputs[0]['source_biographies_file'])
    contextual = {name for name, result in conditional.items()
                  if any('education_context' in fields(m['conditions']) for m in result['matches'])}
    needed = set()
    with ledger.open(encoding='utf-8-sig', newline='') as source:
        for award in csv.DictReader(source):
            if earned(award) and award['institution_as_written'] in contextual:
                needed.add(award['source_key'])
    contexts = {}
    manifest = batches.read_json(ledger.parent/'manifest.json')
    parser_file = ledger.parent/manifest['frozen_parser_snapshot']
    assert hashlib.sha256(parser_file.read_bytes()).hexdigest() == manifest['parser']['parser_sha256']
    spec = importlib.util.spec_from_file_location('frozen_education_bounds', parser_file)
    parser = importlib.util.module_from_spec(spec); spec.loader.exec_module(parser)
    if needed:
        with biographies.open(encoding='utf-8-sig', newline='') as source:
            for row in csv.DictReader(source):
                if row['source_key'] in needed:
                    region, _ = parser.bounds(row['raw_text_adjusted'], int(row['edition']))
                    contexts[row['source_key']] = row['raw_text_adjusted'][slice(*region)] if region else ''
        assert needed <= contexts.keys(), 'Conditional education source biography is missing'
    counts = {name: dict(record_id=r['record_id'], institution_as_written=name,
                        total_awards=0, satisfying_awards=0, uncovered_awards=0,
                        possible_conflicting_conditions=0, examples_uncovered=[])
              for name, r in conditional.items()}
    with ledger.open(encoding='utf-8-sig', newline='') as source:
        for award in csv.DictReader(source):
            name = award['institution_as_written']
            if not earned(award) or name not in conditional:
                continue
            context = dict(award, education_context=contexts.get(award['source_key'], ''))
            selected = {m['openalex_id'] for m in conditional[name]['matches']
                        if batches.condition_applies(m['conditions'], context)}
            row = counts[name]; row['total_awards'] += 1
            if len(selected) == 1:
                row['satisfying_awards'] += 1
            else:
                row['uncovered_awards'] += 1
                row['possible_conflicting_conditions'] += len(selected) > 1
                if len(row['examples_uncovered']) < 3:
                    row['examples_uncovered'].append({k:award[k] for k in
                        ('source_key','award_index','requested_family','year','flags','evidence')})
    for name, row in counts.items():
        assert row['total_awards'] == inventory_counts[name], 'Conditional award links do not reconcile with inventory'
    report = dict(validated_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                  documented_names=len(results), conditional_names=len(counts),
                  satisfying_awards=sum(r['satisfying_awards'] for r in counts.values()),
                  uncovered_awards=sum(r['uncovered_awards'] for r in counts.values()),
                  names_with_no_satisfying_awards=[r['record_id'] for r in counts.values() if not r['satisfying_awards']],
                  names_with_possible_conflicts=[r['record_id'] for r in counts.values() if r['possible_conflicting_conditions']],
                  names=list(counts.values()),
                  interpretation='Diagnostic condition coverage only; no training locations or county outcomes constructed.')
    batches.atomic_json(run/'conditional_award_coverage.json', report)
    print({k:v for k,v in report.items() if k != 'names'})


if __name__ == '__main__':
    default_root = Path(os.environ.get('TALENT_DETS_DATA_DIR', str(Path.home()/'Globtalent Dropbox/gtl_talent_dets')))
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument('--run', type=Path, default=default_root/'output/amws/openalex_id_research_20261011')
    validate(cli.parse_args().run.resolve())

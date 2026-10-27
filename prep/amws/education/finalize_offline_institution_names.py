"""Finish the offline release using structural checks only; never revise choices."""
import argparse
import collections
import datetime
import json
from pathlib import Path
import statistics

import offline_institution_name_continuation as c
import offline_institution_name_pilot as p


def finalize(run):
    run = Path(run)
    manifest = p.read(run / 'sample_manifest.json')
    assignments = p.read(run / 'worker_assignments.json')
    numbers = manifest['batch_numbers']
    if assignments['status'] != 'complete' or assignments['completed_batches'] != numbers:
        raise ValueError('All batches must pass coordinator structural validation first')
    if assignments['unassigned_batches'] or any(x['status'] != 'done' for x in assignments['workers'].values()):
        raise ValueError('Outstanding worker assignments')
    report = c.aggregate(run)
    if not report['complete'] or report['continuation_completed'] != 15681 or report['offline_entries'] != 15981:
        raise ValueError('Incomplete release coverage')
    protected = c.verify_protected(run)
    originals = []
    batches = []
    for number in numbers:
        originals.extend(p.inputs(run, number).values())
        result = c.validate_batch(run, number)
        timer = p.read(p.batch_folder(run, number) / 'timing.json')
        batches.append(dict(result, started_at_utc=timer['started_at_utc'], completed_at_utc=timer['completed_at_utc']))
    if [r['record_id'] for r in originals] != manifest['selected_ids']:
        raise ValueError('Input inventory order changed')
    if [len(p.inputs(run, n)) for n in numbers] != [100] * 156 + [81]:
        raise ValueError('Batch sizes changed')
    inventory_path = run.parent / 'openalex_id_research_20261011' / 'inventory.jsonl'
    inventory = c.jsonl(inventory_path)
    prior_oa = c.jsonl(run / 'preserved/oa_research_results.jsonl')
    prior_researched = p.read(run / 'preserved/researched_name_pilot_results.json')
    pilot = p.read(run / 'preserved/offline_pilot_results.json')
    groups = [set(r['record_id'] for r in rows) for rows in (prior_oa, prior_researched, pilot, originals)]
    if any(groups[a] & groups[b] for a in range(4) for b in range(a + 1, 4)):
        raise ValueError('Accounted groups overlap')
    if set.union(*groups) != {r['record_id'] for r in inventory}:
        raise ValueError('Prior groups and continuation do not partition original inventory')
    excluded = set.union(*groups[:3])
    expected = [dict(record_id=r['record_id'], institution_as_written=r['institution_as_written'])
                for r in inventory if r['record_id'] not in excluded]
    if expected != originals:
        raise ValueError('Continuation does not preserve raw strings and source inventory order')
    rows = p.read(run / 'results.json')
    if rows[:300] != pilot:
        raise ValueError('Imported offline pilot decisions changed')
    if [r['record_id'] for r in rows[300:]] != manifest['selected_ids']:
        raise ValueError('Consolidated continuation inventory order changed')
    source_map = {r['record_id']: r for r in inventory}
    strict_fields = p.RESULT_FIELDS
    for row in rows:
        p.validate(row, source_map[row['record_id']])
        if set(row) != strict_fields or not isinstance(row['note'], str):
            raise ValueError('Required result fields or note type invalid: ' + row['record_id'])
    if len({r['record_id'] for r in rows}) != len(rows):
        raise ValueError('Duplicate consolidated record')
    proposed = p.read(run / 'proposed_names.json')
    exact_names = collections.defaultdict(list)
    for row in rows:
        for name in row['standard_names']:
            exact_names[name].append(row['record_id'])
    expected_names = [dict(standard_name=name, raw_record_ids=ids) for name, ids in sorted(exact_names.items())]
    if proposed != expected_names or p.read(run / 'deduplicated_names.json') != expected_names:
        raise ValueError('Exact-string proposed-name inventory does not reconcile')
    # Repeating aggregation must reproduce the same documented decisions.
    result_hash = c.digest(run / 'results.json')
    c.aggregate(run)
    if c.digest(run / 'results.json') != result_hash:
        raise ValueError('Checkpoint reaggregation changed documented results')
    elapsed = [x['elapsed_seconds'] for x in batches]
    parse = datetime.datetime.fromisoformat
    first = min(parse(x['started_at_utc']) for x in batches)
    last = max(parse(x['completed_at_utc']) for x in batches)
    events_path = run / 'execution_events.json'
    timing = dict(first_batch_started_at_utc=first.isoformat(), last_batch_completed_at_utc=last.isoformat(),
                  wall_seconds_including_interruptions=(last - first).total_seconds(),
                  sum_agent_batch_seconds=sum(elapsed), mean_batch_seconds=statistics.mean(elapsed),
                  median_batch_seconds=statistics.median(elapsed), minimum_batch_seconds=min(elapsed),
                  maximum_batch_seconds=max(elapsed), batches=batches,
                  execution_events=p.read(events_path) if events_path.exists() else [],
                  timing_scope='Timers start before raw input reads and continue across interruptions; summed timers are not active computation time. Wall time includes dispatch and interruptions.',
                  semantic_review_seconds=0, semantic_review=False)
    validation = dict(completed_at_utc=p.now(), passed=True, scope='Automated structural checks only',
                      semantic_review=False, identity_accuracy_verified=False, batches=157,
                      continuation_records=15681, imported_pilot_records=300, combined_offline_records=15981,
                      batch_sizes_preserved=True, source_inventory_order_preserved=True,
                      raw_identifiers_and_names_preserved=True, exact_unique_coverage=True,
                      valid_result_schema=True, checkpoint_reaggregation_idempotent=True,
                      pilot_results_unchanged=True, exact_string_name_deduplication=True,
                      immutable_release_inputs_verified=len(manifest['immutable_files']),
                      protected_source_files_verified=len(protected),
                      accounted_prior_groups=manifest['accounted_prior_groups'], all_raw_inventory_names=18909)
    p.write(run / 'timing_report.json', timing)
    p.write(run / 'validation_report.json', validation)
    counts = report['combined_status_counts']
    new_counts = report['continuation_status_counts']
    minutes = timing['wall_seconds_including_interruptions'] / 60
    text = f'''# Unreviewed provisional institution-name cleanup

Completed 15,681 continuation entries in 157 batches, using three medium-effort agents. Imported the existing 300 offline-pilot decisions unchanged: 15,981 offline-cleanup entries in total.

| Status | Continuation | Combined with pilot |
| --- | ---: | ---: |
| Clear | {new_counts.get('clear', 0):,} | {counts.get('clear', 0):,} |
| Likely | {new_counts.get('likely', 0):,} | {counts.get('likely', 0):,} |
| Unresolved | {new_counts.get('unresolved', 0):,} | {counts.get('unresolved', 0):,} |
| Not an institution | {new_counts.get('not_an_institution', 0):,} | {counts.get('not_an_institution', 0):,} |

There are {report['distinct_proposed_names']:,} distinct proposed names after exact-string deduplication. These are provisional text assignments, not verified institution identities. The continuation received no semantic review; the imported pilot retains its prior decisions and review history.

Raw names and stable identifiers were the only worker inputs. No web research, APIs, biographies, or degree validation were used. Unresolved entries count as documented completion.

Automated validation passed for full coverage, unique identifiers, unchanged input strings and inventory order, valid result fields, batch sizes, imported pilot preservation, checkpoint reaggregation, and exact-string deduplication. Verified {len(protected)} protected source-file checksums. Coverage and agreement do not establish identity accuracy.

The remaining inventory is accounted for separately: 1,379 OA-researched names, 100 researched naming-pilot entries, and 1,449 retained unconditional matches. Their original outputs are preserved. The full accounting covers 18,909 raw institution names.

Batch execution wall time was {minutes:.1f} minutes, including dispatch and interruptions. Summed batch timer durations were {sum(elapsed) / 60:.1f} minutes, also including interruptions; these durations are not active computation time. Median batch time was {statistics.median(elapsed):.1f} seconds. Detailed timestamps and resume events appear in timing_report.json.

Files: results.json (combined crosswalk), continuation_results.json, proposed_names.json, coverage_report.json, validation_report.json, timing_report.json, sample_manifest.json, individual batch checkpoints, and preserved prior results. No additional batches were started after the approved continuation. Existing geolocation mappings, source datasets, and county outcomes were not updated.
'''
    (run / 'findings.md').write_text(text, encoding='utf-8')
    outputs = ['results.json', 'continuation_results.json', 'proposed_names.json', 'deduplicated_names.json',
               'coverage_report.json', 'validation_report.json', 'timing_report.json', 'findings.md',
               'sample_manifest.json', 'worker_assignments.json']
    p.write(run / 'completion_manifest.json', dict(completed_at_utc=p.now(), status='complete',
            method='Unreviewed provisional cleanup', semantic_review=False,
            outputs={name: c.digest(run / name) for name in outputs},
            protected_sources=manifest['protected_files']))
    return dict(status='complete', combined_status_counts=counts, distinct_proposed_names=report['distinct_proposed_names'],
                elapsed_wall_minutes=minutes, validation_passed=True)


if __name__ == '__main__':
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument('--run', type=Path, required=True)
    args = cli.parse_args()
    print(json.dumps(finalize(args.run), indent=2))

"""Publish pilot documentation and a CSV authoring payload, without data mutation."""
import argparse
import collections
import json
import os
import shutil
from pathlib import Path
from pilot import DEFAULT_ROOT, NAME, LABELS, read, write, now, sha


def percent(stat):
    return 'n/a' if stat['value'] is None else f'{stat["value"]*100:.1f}% ({stat["numerator"]}/{stat["denominator"]})'


def run(out):
    evaluation=read(out/'evaluation.json');guard=read(out/'evaluation_boundary_guard_posthoc.json')
    frozen=read(out/'predictions.json');guarded=read(out/'predictions_boundary_guard_posthoc.json')
    manifest=read(out/'sample_manifest.json');h=evaluation['heldout']
    errors=read(out/'errors.json')
    report=[
        'AMWS EDITION 16: 500-ENTRY SCIENCE REGEX PILOT',
        'Audit date: October 20, 2026', '',
        'RESULT',
        'All 500 sampled entries were manually labeled: 300 development and 200 held out.',
        f'Frozen v1 agreement on held-out entries: {percent(h["accuracy"])}.',
        'This is an initial precision-focused prototype with substantial uncovered fields.',
        'It has not been applied to the full AMWS release.', '',
        'SOURCE AND SAMPLING',
        'Population: the 79,658 US-birth edition-16 entries in the October 19, 2026',
        'multi_v04 combined release. Source inclusion rules were preserved.',
        'Counting unit is an edition-specific entry, not a cross-edition unique person.',
        'Draw: random.Random(20261020).sample(sorted(source_keys), 500).',
        'Split: copy the draw; random.Random(20261021).shuffle(copy); first 300 development,',
        'last 200 held out. Sample IDs retain original draw order.',
        'Exact name + full biography matches recover fields from amws_ed86.csv.',
        'Every source match is retained in provenance; duplicate matches must agree.',
        'All 500 join successfully; 477 recovered fields are nonblank and 23 are blank.',
        'No difficult cases were replaced. Source checksums are in sample_manifest.json.', '',
        'MANUAL BENCHMARK',
        'Individual Codex manual decisions are recorded for all 500 biographies, using',
        'discipline headings and research excerpts; complete text was consulted when needed.',
        'Literal excerpts, offsets, rationale and confidence accompany each manual label.',
        'There are 485 high-, 14 medium- and one low-confidence manual labels.',
        'These are not independent human-validated labels or measured confidence probabilities.',
        'Manual totals: Physical 260, Biological 217, Other 23.',
        'The development labels preceded rule building. The rule/code snapshot was frozen',
        'before held-out labeling; predictions were not displayed during held-out labeling.',
        'rules_frozen.json and gold_labels_frozen.json record the sequence and hashes.', '',
        'THREE-CATEGORY POLICY',
        'Physical: chemistry, physics, Earth/space science, engineering, mathematics,',
        'statistics and computing. Biological: life sciences, agriculture/forestry, food',
        'science, medicine, veterinary science and public health.',
        'Biochemistry, biophysics, biostatistics, bioengineering, medicinal/pharmaceutical',
        'chemistry and physical anthropology are named biological specialties.',
        'Agricultural/pesticide chemistry remains chemical science unless a distinct',
        'biological specialty is named. Agricultural engineering remains Physical;',
        'agricultural soil science is Biological. Soil physics is Physical.',
        'Human psychology, economics, policy and science communication are non-target.',
        'A target field plus non-target work retains the target group.',
        'Other reasons: non_target, mixed, missing, damaged or unmatched.',
        'mixed means Physical AND Biological evidence, never merely two different fields.',
        'Single named specialties such as biochemistry do not automatically create mixed.',
        'Mixed and unresolved entries require review; non_target is a substantive Other.', '',
        'FROZEN V1: GENUINE HELD-OUT PERFORMANCE',
        'Rows are manual labels; columns are regex predictions.',
        'Manual label             Physical  Biological  Other',
    ]
    for label in LABELS:
        cells=h['confusion_matrix_rows_gold_columns_predicted'][label]
        report.append(f'{label:<25}{cells["physical_science"]:>8}{cells["biological_science"]:>12}{cells["other"]:>7}')
    report+=['', 'Group                  Precision                 Recall']
    for label in LABELS:
        m=h['per_class'][label]
        report.append(f'{label:<23}{percent(m["precision"]):<26}{percent(m["recall"])}')
    report += [
        '', f'Unresolved: {percent(h["unresolved_rate"])}; mixed flags: 7; genuine non-target predictions: 2.',
        f'All review flags: {h["review_required_count"]}/200. Predictions accepted without review: {percent(h["accepted_without_review_accuracy"])}.',
        f'Mixed flag precision: {percent(h["mixed_flag_precision"])}; recall: {percent(h["mixed_flag_recall"])}.',
        'Other precision is low because it intentionally combines true Other with abstentions.',
        'It must not be interpreted as a clean non-science category without other_reason.',
        'Wilson 95% interval for overall agreement: 80.0%-89.7%. Exact per-class intervals',
        'and high-confidence-label sensitivity results are in evaluation.json.',
        'Only 13 manually labeled Other entries occur in the held-out set; its estimates',
        'and the mixed-flag estimates have particularly wide uncertainty.', '',
        'ERRORS AND LIMITATIONS',
        'There are 29 held-out group disagreements: 27 are target-science entries placed',
        'in Other, one is Biological labeled Physical, and one mixed entry labeled Biological.',
        'Three additional entries have the correct Other group but the wrong Other reason.',
        'Missed vocabulary includes cardiology, pharmacy/dentistry, neurosciences,',
        'cytogenetics, lichenology, weed science, lasers and information retrieval.',
        'Protein/enzyme chemistry was incorrectly assigned Physical; coordinated',
        'electrical & biomedical engineering lost its electrical component.',
        'OCR-damaged clinical chemistry caused a false mixed flag.',
        'Damaged headings and nonspecific methods also reduce coverage. The manually',
        'identified research field can remain clear even when the bounded regex abstains.',
        'Some source rows contain concatenated or inconsistent biographies; no source',
        'repairs or exclusions were made during this pilot.', '',
        'Every held-out discrepancy (sample ID, source key, manual and regex decision):',
    ]
    for e in errors:
        if e['split']=='heldout':
            report.append(f'Sample {e["sample_id"]} {e["source_key"]}: {e["gold"]}/{e["gold_reason"] or "target"} -> {e["prediction"]}/{e["predicted_reason"] or "target"}; field={e["field_raw"] or "[blank]"}.')
    report += [
        '', 'POST-HOC BOUNDARY CORRECTION',
        'A code audit found that malformed section markers could allow the initial',
        'heading reader to reach employment text. The v1_1 boundary-guard candidate',
        'cuts at probable credentials/institutions and additional damaged career markers.',
        'Its discipline vocabulary is identical to frozen v1; held-out vocabulary gaps',
        'were not patched. This guard is included for safer subsequent pilot use.',
        f'Its agreement on the original 200-entry test partition is {percent(guard["original_heldout_partition_posthoc"]["accuracy"])}.',
        'THIS IS POST-HOC REGRESSION AGREEMENT, NOT A FRESH HELD-OUT ACCURACY ESTIMATE.',
        'The original v1 snapshot, predictions and 85.5% held-out result remain intact.',
        'The CSV includes both versions: unprefixed science_group/other_reason/mixed',
        'belong to frozen v1; guarded_* columns belong to the post-hoc guard candidate.',
        'Do not combine their scores or silently treat the guarded version as prevalidated.', '',
        'REGEX STRATEGY',
        'Normalize Unicode/case/spacing while mapping every hit to literal source offsets.',
        'Resolve named compound specialties before generic science words, masking overlaps.',
        'Validate recovered fields against the initial header and reject obvious debris.',
        'Use bounded header extraction for missing/invalid fields and incomplete field labels.',
        'Detect Physical/Biological conflicts after compound resolution; preserve rule IDs.',
        'The guard candidate adds conservative career/credential boundaries. It does not',
        'search research, education or affiliations to fill an otherwise absent field.', '',
        'DELIVERABLES AND REPRODUCTION',
        'amws_field_regex_pilot_500.csv: all 500 manual labels, both predictions, source',
        'text, original field values, evidence, mixed flags, rules and source references.',
        'rules_v1.json and classifier.py: original frozen version in frozen_code/.',
        'candidate_code/: current source, guard candidate, tests and reproduction scripts.',
        'errors.json: all group or Other-reason discrepancies, including development.',
        'evaluation.json: genuine frozen-v1 held-out metrics and interval denominators.',
        'evaluation_boundary_guard_posthoc.json: separately identified guard regression.',
        'validation.json: independent seeded-draw, provenance, evidence, counts, code',
        'freeze, CSV roundtrip and source-preservation checks; output checksums.',
        'See README.md in the repository fields folder for commands and output definitions.', '',
        'BEFORE FULL-DATASET APPLICATION',
        'Extend the vocabulary with explicit discipline families and coordinated headings,',
        'keeping generic chemistry from consuming protein/enzyme/clinical specialties.',
        'The 200 held-out entries are now visible and cannot validate a retuned classifier.',
        'Use a new untouched random evaluation sample for any expanded version. Resolve',
        'the remaining uncertain manual labels and source contamination separately.',
        'No full-population category shares are inferred from this pilot.', ''
    ]
    (out/'report.txt').write_text('\n'.join(report),encoding='utf-8')
    code=out/'candidate_code';code.mkdir(exist_ok=True)
    for p in Path(__file__).parent.iterdir():
        if p.is_file() and p.suffix in ('.py','.json','.md','.mjs'):
            shutil.copyfile(p,code/p.name)
    write(out/'publication_metadata.json',dict(created_at_utc=now(),
        original_evaluation_sha256=sha(out/'evaluation.json'),
        guarded_evaluation_status='posthoc_no_fresh_holdout',
        tests_passed=22,
        csv_column_definitions={'science_group':'Original frozen v1 regex prediction',
            'manual_group':'Manual benchmark label',
            'mixed':'Original v1 physical_match AND biological_match',
            'guarded_science_group':'Post-hoc boundary-guard candidate; not fresh held-out validated',
            'manual_confidence':'Subjective manual label confidence, not a model probability'},
        original_rules_untuned_after_holdout=True))
    print('Report, candidate code snapshot and publication metadata written.')


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--output',type=Path)
    a=p.parse_args();run(a.output or a.data_root/'output/amws'/NAME)

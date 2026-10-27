"""Post-evaluation publication audit; reads cached predictions, never retunes.

Adds accessible tables and explicit leakage/error summaries to this new release.
The immutable engine, references, frozen samples and cached evaluation are not
modified. Repeated use verifies the final publication manifest and returns.
"""
import collections
from datetime import datetime,timezone
import json
from pathlib import Path

import amws_university_affiliation_calibration as c
import amws_university_affiliation_engine as engine


def jsonl(path):
    return [json.loads(line) for line in path.read_text(encoding='utf-8').splitlines() if line]


def audit(root=None):
    dest=c.destination(root)
    completion=c.read_json(dest/'completion_manifest.json')
    for name,value in completion['output_checksums'].items():
        if c.base.digest(dest/name)!=value:raise ValueError(f'Publication file changed: {name}')
    if (dest/'publication_audit.json').exists():return dest/'REPORT.md'
    frozen=c.read_json(dest/'frozen_rules.json')
    if c.base.digest(Path(engine.__file__))!=frozen['engine_sha256']:raise ValueError('Frozen engine changed')
    c.workplace.verify_checksums(frozen['frozen_checksums'])
    rows=c.base.read_csv(dest/'sample_1000.csv')
    old=c.base.read_csv(c.workplace.destination(root)/'sample_80.csv')
    dev=[r for r in rows if r['partition']=='development'];test=[r for r in rows if r['partition']=='test']
    def person(r):
        return c.fingerprint(r['v06_name']+'|'+r['v06_birth_year']) if r['v06_name'].strip() and r['v06_birth_year'].strip() else ''
    protected_bios={c.fingerprint(r['biography_text']) for r in dev+old}-{''}
    protected_people={person(r) for r in dev+old}-{''}
    duplicate_bios=[r['source_key'] for r in test if c.fingerprint(r['biography_text']) in protected_bios]
    duplicate_people=[r['source_key'] for r in test if person(r) in protected_people]
    prior_overlap={r['source_key'] for r in old}&{r['source_key'] for r in rows}
    partition_overlap={r['source_key'] for r in dev}&{r['source_key'] for r in test}
    assert len(rows)==1000 and len({r['source_key'] for r in rows})==1000
    assert not duplicate_bios and not duplicate_people and not prior_overlap and not partition_overlap
    for row in rows:
        assert row['biography_fingerprint']==c.fingerprint(row['biography_text'])
        assert row['person_fingerprint']==person(row)
    refs=c.reviews('development',root)+c.reviews('test',root)
    assert len(refs)==1000
    refmap={r['source_key']:r for r in refs}
    decisions=[];affiliations=[]
    for row in rows:
        ref=refmap[row['source_key']];c.check_review(ref,row)
        common={k:row[k] for k in ('sample_id','source_key','partition','edition')}
        decisions.append(dict(**common,decision=ref['decision'],full_entry_reviewed=True,
            affiliation_blocks=len(ref['affiliations']),institution_reference_groups=len(c.reference_groups(ref)),
            negative_evidence=json.dumps(ref['negative_evidence'],ensure_ascii=False),
            unresolved_evidence=json.dumps(ref['unresolved_evidence'],ensure_ascii=False),explanation=ref['explanation']))
        for n,a in enumerate(ref['affiliations'],1):
            inst=a['institution']
            affiliations.append(dict(**common,affiliation_block=n,institution_as_written=inst['text'],
                institution_field=inst['field'],institution_start=inst['start'],institution_end=inst['end'],
                temporal_status=a['temporal_status'],appointment_types='; '.join(a['appointment_types']),
                roles_as_written=' | '.join(e['text'] for e in a['roles']),date_text=' | '.join(e['text'] for e in a['dates']),
                supporting_excerpts=' | '.join(e['text'] for e in a['contexts']),
                complete_evidence_json=json.dumps(a,ensure_ascii=False)))
    c.base.write_csv(dest/'review_decisions_1000.csv',decisions,list(decisions[0]))
    c.base.write_csv(dest/'reference_affiliations.csv',affiliations,list(affiliations[0]))
    cached={part:jsonl(dest/('development_versions/'+frozen['version'] if part=='development' else 'test_evaluation')/'predictions.jsonl') for part in ('development','test')}
    comparisons=[]
    for part,predictions in cached.items():
        assert len(predictions)==500
        for prediction in predictions:
            key=prediction['source_key'];row=next(r for r in rows if r['source_key']==key);ref=refmap[key]
            names=[a['institution']['text'] for a in prediction['affiliations']]
            assert len(names)==len({engine.norm(s) for s in names})
            comparisons.append(dict(**{k:row[k] for k in ('sample_id','source_key','partition','edition')},
                original_name=row['raw_name'],reference_decision=ref['decision'],
                reference_institutions_json=json.dumps([a['institution']['text'] for a in ref['affiliations']],ensure_ascii=False),
                predicted_institutions_json=json.dumps(names,ensure_ascii=False),
                complete_prediction_json=json.dumps(prediction,ensure_ascii=False)))
    c.base.write_csv(dest/'prediction_reference_comparison_1000.csv',comparisons,list(comparisons[0]))
    version_metrics=[]
    for folder in sorted((dest/'development_versions').iterdir()):
        for m in c.read_json(folder/'metrics.json'):
            version_metrics.append(dict(version=folder.name,**m))
    c.base.write_csv(dest/'development_version_comparison.csv',version_metrics,sorted({k for r in version_metrics for k in r}))
    audit_result=dict(audited_at_utc=datetime.now(timezone.utc).isoformat(),
        source_and_protected_release_reconstruction=c.read_json(dest/'validation.json'),
        test_vs_development_and_prior80_biography_overlap=duplicate_bios,
        test_vs_development_and_prior80_person_signature_overlap=duplicate_people,
        prior80_source_id_overlap=sorted(prior_overlap),cross_partition_id_overlap=sorted(partition_overlap),
        biography_and_person_fingerprints_recomputed=True,
        review_counts={p:dict(collections.Counter(r['decision'] for r in refs if r['partition']==p)) for p in ('development','test')},
        manual_review_coverage=1000,test_evaluation_runs=1,
        engine_unchanged_since_freeze=True,predictions_and_test_metrics_recomputed=False,
        unit_tests=dict(cases=14,passed=13,expected_failures=1,
                        known_failure='1986 section marker glued to a preceding date permits membership leakage; frozen rule unchanged'),
        post_test_publication_changes_only=['Readable reference and prediction comparison tables','Rule-family/version summaries','Explicit duplicate audit','Report interpretation'],
        prior_completion_manifest_sha256=c.base.digest(dest/'completion_manifest.json'))
    c.base.write_json(dest/'publication_audit.json',audit_result)
    lines=['','## Completed manual-review coverage','',
        '| Partition | Edition | Positive | Positive + unresolved | Unresolved only | Negative | Total |',
        '|---|---:|---:|---:|---:|---:|---:|']
    for part in ('development','test'):
        for edition in c.EDITIONS:
            ct=collections.Counter(r['decision'] for r in refs if r['partition']==part and r['edition']==edition)
            lines.append(f"| {part} | {edition} | {ct['positive']} | {ct['positive_with_unresolved']} | {ct['unresolved']} | {ct['negative']} | {sum(ct.values())} |")
    lines+=['','Every original field was read, including empty or damaged entries. Positive references may contain multiple appointment blocks; reported institution counts consolidate only shared documented literal forms. Address-only references are explicitly labelled and do not assert paid employment. All ambiguous decisions include literal evidence and an explanation.',
        '', '## Saved development versions','',
        '| Version | Edition | Correct / predicted | Reference institutions | Precision | Recall | F1 |',
        '|---|---:|---:|---:|---:|---:|---:|']
    fmt=lambda x:f'{100*x:.1f}%' if x is not None else 'undefined'
    for m in version_metrics:
        if m['rule_family']=='all':
            lines.append(f"| {m['version']} | {m['edition']} | {m.get('true_positive',0)} / {m.get('predicted_institutions',0)} | {m.get('reference_institutions',0)} | {fmt(m['precision'])} | {fmt(m['recall'])} | {fmt(m['f1'])} |")
    lines+=['','Each version retains its original executable engine snapshot and reference hash. The final structural guards reduced observed false positives; the final version still missed every edition-specific precision target before test opening. Baseline header-only extraction produces no 1986 predictions. The target was not redefined or relaxed after testing.',
        '', '## Results by rule family','',
        '| Partition | Edition | Rule family | Correct / predicted | Precision | Unsupported predictions on unresolved entries |',
        '|---|---:|---|---:|---:|---:|']
    for part in ('development','test'):
        folder=dest/'development_versions'/frozen['version'] if part=='development' else dest/'test_evaluation'
        for m in c.read_json(folder/'metrics.json'):
            if m['rule_family']!='all':
                lines.append(f"| {part} | {m['edition']} | {m['rule_family']} | {m.get('true_positive',0)} / {m.get('predicted_institutions',0)} | {fmt(m['precision'])} | {m.get('predictions_on_unresolved',0)} |")
    lines+=['','Rule-family precision is attributed to the first emitting family after within-record consolidation. Recall is reported by edition: missed reference institutions cannot uniquely be assigned to a rule that did not emit them. Counts of role/date literals and temporal classification are stored with explicit denominators in metrics.csv/json.',
        '', '## Material frozen-test failures','',
        'The 1986 strategy can miss section boundaries when OCR glues a marker to a date (for example, `75Mem:`). This permits professional-society members or governance committees to be misread as employers. Cached failures include source 1986:10264 (an “Am Col Clin Pharmacol & Chemother” membership), 1986:1180 (a university/college affiliation split into separate fragments), and 1986:2594 (curriculum advisory institutions incorrectly emitted). A synthetic regression fixture now documents the glued-boundary failure as an expected failure; the frozen engine has not been repaired using test errors.',
        '', 'Some false positives are strict literal-form mismatches rather than invented universities: departmental or medical-school prefixes, comma-separated campus names and laboratory qualifiers can be dropped or split. They remain errors under the predeclared reference forms. Other errors are unsupported institutions, damaged names, education/membership contamination or duplicated full/abbreviated forms. No post-test alias expansion or rescoring was used to improve the figures.',
        '', 'All four editions fail the 95% test precision target. This frozen version is **not recommended for population extraction**. Further development requires a new independent test sample. Candidate pairs and production affiliation data remain unchanged.',
        '', '## Additional publication checks','',
        'There are exactly 125 distinct records per edition in each partition and 1,000 complete reviews. Source joins and original text reproduce exactly; protected source/release checksums are unchanged. Recomputed nonempty normalized biography and full-name/birth-year fingerprints find zero test overlaps with development or the previous 80 reviewed entries. No cross-partition or prior-80 ID overlap exists. These are conservative duplicate checks, not complete person linkage.',
        '', 'Validation: 14 focused unit-test cases, 13 passing and one documented expected failure. One cached test evaluation exists. Publication tables read cached outputs and do not run a second evaluation. All original evidence spans remain literal and end-exclusive. Readable review/reference/comparison CSVs accompany the full JSONL ledgers. The publication audit and final completion manifest include these additional files.', '']
    with (dest/'REPORT.md').open('a',encoding='utf-8') as f:f.write('\n'.join(lines))
    completion['publication_audit']=audit_result
    completion['output_checksums']={str(p.relative_to(dest)):c.base.digest(p) for p in dest.rglob('*') if p.is_file() and p.name!='completion_manifest.json'}
    c.base.write_json(dest/'completion_manifest.json',completion)
    return dest/'REPORT.md'


if __name__=='__main__':print(audit())

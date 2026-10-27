"""Reproducible education regex development/evaluation, with a freeze gate.

Usage: education_pilot.py init | reference | develop VERSION | freeze VERSION
       education_pilot.py holdout | report
"""
import argparse
import collections
import csv
import hashlib
import importlib.util
import json
import math
import random
import shutil
from pathlib import Path
from degree_audit_io import ROOT

OUT = ROOT / 'regex_pilot'
GROUPS = ['bachelors', 'other_bachelor', 'masters_academic', 'masters_professional', 'phd']
GROUP_SETS = {g: {g} for g in GROUPS}
GROUP_SETS.update(any_bachelor={'bachelors', 'other_bachelor'},
                  any_master={'masters_academic', 'masters_professional'},
                  all_target_degrees=set(GROUPS))


def digest(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as f:
        for block in iter(lambda: f.read(1024*1024), b''):
            h.update(block)
    return h.hexdigest()


def write_json(path, data):
    Path(path).write_text(json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')


def read_json(path):
    return json.loads(Path(path).read_text(encoding='utf-8'))


def write_csv(path, rows, fields=None):
    if fields is None:
        fields = list(rows[0]) if rows else ['sample_id']
    with Path(path).open('w', encoding='utf-8-sig', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        w.writerows(rows)


def sample():
    with (ROOT / 'reviewed_sample_1000.csv').open(encoding='utf-8-sig', newline='') as f:
        return {int(r['sample_id']): r for r in csv.DictReader(f)}


def init():
    OUT.mkdir(parents=True, exist_ok=True)
    ids = sorted(sample())
    assert ids == list(range(1, 1001))
    random.Random(42).shuffle(ids)
    manifest = dict(seed=42, algorithm='Python random.Random(42).shuffle(sorted sample_ids)',
                    development=ids[:800], holdout=ids[800:],
                    reviewed_sample_sha256=digest(ROOT/'reviewed_sample_1000.csv'),
                    sample_sha256=digest(ROOT/'sample_1000.csv'),
                    original_degrees_sha256=digest(ROOT/'degree_extractions.csv'),
                    original_manifest_sha256=digest(ROOT/'sample_manifest.json'))
    path = OUT/'split_manifest.json'
    if path.exists():
        assert read_json(path) == manifest
    else:
        write_json(path, manifest)
    return manifest


def reference(require_masters=True):
    rows = sample()
    master_rows = {}
    for p in sorted(OUT.glob('masters_review_*.jsonl')):
        for line in p.read_text(encoding='utf-8').splitlines():
            r = json.loads(line)
            i = r['sample_id']
            assert i not in master_rows and r['full_review'] is True, (p, i)
            master_rows[i] = r
    if require_masters:
        assert set(master_rows) == set(rows), f'Masters reviewed: {len(master_rows)}/1000'
    for p in sorted(OUT.glob('masters_reference_adjudications_*.json')):
        for correction in read_json(p):
            i = correction['sample_id']
            assert i in master_rows
            master_rows[i] = dict(master_rows[i], masters=correction['replacement_masters'])
    result = {}
    for i, r in rows.items():
        ds = [d for d in json.loads(r['degrees_json']) if d['group'] in GROUPS]
        ds += master_rows.get(i, {}).get('masters', [])
        for d in ds:
            assert d['group'] in GROUPS
            t = r[d['evidence_source']]
            assert d['evidence'] and d['evidence'] in t, (i, d)
            for k in ['label_as_written', 'institution_as_written', 'year_raw']:
                assert not d[k] or d[k] in d['evidence'], (i, k, d)
        result[i] = ds
    return result, master_rows


def wilson(k, n):
    if not n:
        return None, None
    z = 1.959963984540054
    p = k/n
    den = 1+z*z/n
    center = (p+z*z/(2*n))/den
    margin = z*math.sqrt(p*(1-p)/n+z*z/(4*n*n))/den
    return center-margin, center+margin


def normalized(s):
    import re, unicodedata
    return re.sub(r'[^a-z0-9]', '', unicodedata.normalize('NFKC', s or '').lower())


def key(d, metric):
    # Family-based occurrence counts retain multiple awards and penalize duplicates.
    # Exact institution scoring only normalizes case/punctuation/whitespace.
    k = (d['group'],)
    if metric in ('institution', 'complete_tuple'):
        if not d['institution_as_written']:
            return None
        k += (normalized(d['institution_as_written']),)
    if metric in ('year', 'complete_tuple'):
        if d['year'] is None:
            return None
        k += (int(d['year']),)
    if metric == 'literal_year':
        if not d['year_raw']:
            return None
        k += (normalized(d['year_raw']),)
    return k


def evaluate(ids, predictions, gold):
    metrics, errors = [], []
    for group_name, members in GROUP_SETS.items():
        for metric in ['degree', 'institution', 'literal_year', 'year', 'complete_tuple', 'record_detection']:
            tp = fp = fn = 0
            for i in sorted(ids):
                p = [d for d in predictions[i]['degrees'] if d['group'] in members and not d['honorary']]
                g = [d for d in gold[i] if d['group'] in members and not d['honorary']]
                if metric == 'record_detection':
                    pc, gc = collections.Counter({group_name:1}) if p else collections.Counter(), collections.Counter({group_name:1}) if g else collections.Counter()
                else:
                    pc = collections.Counter(k for d in p if (k := key(d, metric)) is not None)
                    gc = collections.Counter(k for d in g if (k := key(d, metric)) is not None)
                correct = pc & gc
                tp += sum(correct.values())
                fp += sum((pc-gc).values())
                fn += sum((gc-pc).values())
                if group_name in GROUPS:
                    for typ, values in [('false_positive', pc-gc), ('false_negative', gc-pc)]:
                        for value, count in values.items():
                            errors.append(dict(sample_id=i, group=group_name, metric=metric,
                                               error_type=typ, key=json.dumps(value), count=count,
                                               gold=json.dumps(g, ensure_ascii=False),
                                               predicted=json.dumps(p, ensure_ascii=False)))
            precision = tp/(tp+fp) if tp+fp else None
            recall = tp/(tp+fn) if tp+fn else None
            f1 = 2*tp/(2*tp+fp+fn) if 2*tp+fp+fn else None
            pl, pu = wilson(tp, tp+fp)
            rl, ru = wilson(tp, tp+fn)
            metrics.append(dict(group=group_name, metric=metric, records=len(ids), tp=tp, fp=fp, fn=fn,
                                precision=precision, recall=recall, f1=f1,
                                precision_ci_low=pl, precision_ci_high=pu,
                                recall_ci_low=rl, recall_ci_high=ru))
    return metrics, errors


def import_parser(path):
    spec = importlib.util.spec_from_file_location('frozen_education_regex', path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def predict(ids, parser):
    rows = sample()
    out = {}
    for i in ids:
        r = rows[i]
        try:
            birth_year = int(r['birth_year'])
        except (ValueError, TypeError):
            birth_year = None
        out[i] = parser.extract(r['raw_text_adjusted'], birth_year)
        for d in out[i]['degrees']:
            assert r['raw_text_adjusted'][d['start']:d['end']] == d['evidence']
            assert r['raw_text_adjusted'][d['label_start']:d['label_end']] == d['label_as_written']
            for k in ['label_as_written', 'institution_as_written', 'year_raw']:
                assert not d[k] or d[k] in d['evidence'], (i, k, d)
    return out


def run(version, provisional=False):
    manifest = init()
    gold, masters = reference(not provisional)
    path = OUT/'iterations'/version
    path.mkdir(parents=True, exist_ok=True)
    parser_path = path/'education_regex.py'
    current = Path(__file__).with_name('education_regex.py')
    if parser_path.exists():
        current_parser = import_parser(current)
        if current_parser.RULE_VERSION == version:
            assert digest(current) == digest(parser_path), 'Use a new version for changed rules'
    else:
        shutil.copyfile(current, parser_path)
    parser = import_parser(parser_path)
    assert parser.RULE_VERSION == version
    predictions = predict(manifest['development'], parser)
    if provisional:
        gold = {i:[d for d in ds if not d['group'].startswith('masters')] for i, ds in gold.items()}
        predictions = {i:dict(r, degrees=[d for d in r['degrees'] if not d['group'].startswith('masters')]) for i,r in predictions.items()}
    metrics, errors = evaluate(manifest['development'], predictions, gold)
    prefix = 'provisional_' if provisional else ''
    write_json(path/(prefix+'predictions.json'), predictions)
    write_csv(path/(prefix+'metrics.csv'), metrics)
    write_csv(path/(prefix+'errors.csv'), errors)
    write_json(path/'rules.json', dict(version=version, sha256=digest(parser_path), labels=parser.LABELS,
               regexes={k:getattr(parser,k).pattern for k in ['DEGREE','EDUC','STOP','PERSON','YEAR','INST']},
               provisional=provisional, masters_reviewed=len(masters),
               reference_sha256=read_json(OUT/'reference_manifest.json')['sha256'] if (OUT/'reference_manifest.json').exists() else None))
    for m in metrics:
        if m['metric'] == 'complete_tuple':
            print(m['group'], 'precision', m['precision'], 'recall', m['recall'], 'F1', m['f1'])


def build_reference():
    init()
    gold, masters = reference()
    rows = sample()
    ledger = [dict(sample_id=i, degree_id=j, **d) for i, ds in sorted(gold.items()) for j,d in enumerate(ds,1)]
    version = 'reference_v2' if any(OUT.glob('masters_reference_adjudications_*.json')) else 'reference_v1'
    write_json(OUT/(version+'.json'), gold)
    write_csv(OUT/(version+'.csv'), ledger)
    write_json(OUT/'reference_manifest.json',dict(version=version, sha256=digest(OUT/(version+'.json')),
               original_reviews_preserved=True, masters_reviews_sha256={p.name:digest(p) for p in sorted(OUT.glob('masters_review_*.jsonl'))},
               adjudications=[read_json(p) for p in sorted(OUT.glob('masters_reference_adjudications_*.json'))]))
    write_json(OUT/'reference_validation.json', dict(records=1000, masters_full_review=len(masters),
               source_hashes_unchanged=True, degrees=len(ledger),
               groups=dict(collections.Counter(d['group'] for ds in gold.values() for d in ds))))


def freeze(version):
    init()
    gold, _ = reference()
    p = OUT/'iterations'/version
    assert (p/'metrics.csv').exists()
    freeze_path = OUT/'freeze_manifest.json'
    manifest = dict(version=version, parser_sha256=digest(p/'education_regex.py'),
                    reference_file=read_json(OUT/'reference_manifest.json')['version']+'.json',
                    reference_sha256=read_json(OUT/'reference_manifest.json')['sha256'],
                    split_sha256=digest(OUT/'split_manifest.json'),
                    policy='Frozen before holdout evaluation; no tuning on holdout errors')
    with (p/'metrics.csv').open(encoding='utf-8-sig',newline='') as f:
        targets=[m for m in csv.DictReader(f) if m['metric']=='complete_tuple' and m['group'] in GROUPS]
    manifest['development_targets']=[dict(group=m['group'],precision=float(m['precision']),recall=float(m['recall'])) for m in targets]
    manifest['development_quality_gate_passed']=all(m['precision']>=.95 and m['recall']>=.90 for m in manifest['development_targets'])
    if freeze_path.exists():
        assert read_json(freeze_path) == manifest, 'Already frozen: do not retune using holdout'
    else:
        shutil.copyfile(p/'education_regex.py', OUT/'frozen_education_regex.py')
        write_json(freeze_path, manifest)


def holdout():
    split = init()
    frozen = read_json(OUT/'freeze_manifest.json')
    assert digest(OUT/'frozen_education_regex.py') == frozen['parser_sha256']
    assert digest(OUT/frozen['reference_file']) == frozen['reference_sha256']
    assert digest(OUT/'split_manifest.json') == frozen['split_sha256']
    dest = OUT/'holdout'
    assert not dest.exists(), 'Holdout already evaluated; inspect saved outputs, do not rerun tuning'
    gold = {int(i):ds for i,ds in read_json(OUT/frozen['reference_file']).items()}
    predictions = predict(split['holdout'], import_parser(OUT/'frozen_education_regex.py'))
    metrics, errors = evaluate(split['holdout'], predictions, gold)
    dest.mkdir()
    write_json(dest/'predictions.json', predictions)
    write_csv(dest/'metrics.csv', metrics)
    write_csv(dest/'errors.csv', errors)
    write_json(dest/'evaluation_manifest.json', dict(**frozen, records=200,
               prediction_sha256=digest(dest/'predictions.json'), metric_sha256=digest(dest/'metrics.csv')))
    for m in metrics:
        if m['metric']=='complete_tuple':
            print(m['group'],m['tp'],m['fp'],m['fn'],m['precision'],m['recall'])


if __name__ == '__main__':
    p = argparse.ArgumentParser()
    p.add_argument('action', choices=['init','reference','develop','freeze','holdout','report'])
    p.add_argument('version', nargs='?')
    p.add_argument('--provisional', action='store_true')
    args = p.parse_args()
    if args.action == 'init': init()
    elif args.action == 'reference': build_reference()
    elif args.action == 'develop': run(args.version, args.provisional)
    elif args.action == 'freeze': freeze(args.version)
    elif args.action == 'holdout': holdout()
    elif args.action == 'report':
        from education_pilot_report import main
        main()

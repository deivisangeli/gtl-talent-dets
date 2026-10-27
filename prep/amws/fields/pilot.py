"""Reproducible AMWS edition-16 field-classification pilot (standard library)."""
import argparse
import collections
import csv
import hashlib
import json
import os
import platform
import random
import re
import sys
from datetime import datetime, timezone
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]
DEFAULT_ROOT = Path.home() / 'Globtalent Dropbox/gtl_talent_dets'
RELEASE = 'output/amws/amws_education_postsecondary_v04_20261019/amws_open_alex.csv'
PROCESSED = 'Data/processed/amws/amws_ed86.csv'
NAME = 'field_regex_pilot_20261020'
LABELS = ['physical_science', 'biological_science', 'other']


def now():
    return datetime.now(timezone.utc).isoformat()


def sha(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as f:
        for b in iter(lambda: f.read(1048576), b''):
            h.update(b)
    return h.hexdigest()


def rows(path):
    with Path(path).open(encoding='utf-8-sig', newline='') as f:
        yield from csv.DictReader(f)


def read(path):
    return json.loads(Path(path).read_text(encoding='utf-8'))


def write(path, obj, immutable=False):
    path = Path(path)
    text = json.dumps(obj, ensure_ascii=False, indent=2) + '\n'
    if immutable and path.exists():
        assert path.read_text(encoding='utf-8') == text, f'Frozen file differs: {path}'
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding='utf-8')


def prepare(root, out):
    if (out / 'sample_manifest.json').exists():
        m = read(out / 'sample_manifest.json')
        assert all(sha(p) == h for p, h in m['source_checksums'].items())
        print('Frozen sample already exists; input checksums match.')
        return
    checks = {str(root / rel): sha(root / rel) for rel in (RELEASE, PROCESSED)}
    pool = []
    for r in rows(root / RELEASE):
        if r['source_key'].startswith('1986:'):
            assert r['is_us_birth'].strip().lower() == 'true'
            pool.append(r['source_key'])
    assert len(pool) == len(set(pool)) == 79658
    sample_keys = random.Random(20261020).sample(sorted(pool), 500)
    shuffled = sample_keys.copy()
    random.Random(20261021).shuffle(shuffled)
    assignments = {k: ('development' if i < 300 else 'heldout', i + 1 if i < 300 else i - 299)
                   for i, k in enumerate(shuffled)}
    selected = {}
    for row_number, r in enumerate(rows(root / RELEASE), 1):
        if r['source_key'] in assignments:
            selected[r['source_key']] = dict(source_key=r['source_key'],
                entry_id=r['entry_id'], name_raw=r['name_raw'], raw_text_adjusted=r['raw_text_adjusted'],
                birth_year=r['birth_year'], release_row=row_number)
    wanted = {(r['name_raw'], r['raw_text_adjusted']) for r in selected.values()}
    lookup = collections.defaultdict(list)
    for row_number, r in enumerate(rows(root / PROCESSED), 1):
        key = r['name_raw'], r['raw_text_adjusted']
        if key in wanted:
            lookup[key].append(dict(processed_row=row_number, field=r['field'],
                doc_id=r['doc_id'], source_file=r['source_file'], lineid=r['lineid'], entry_instance=r['entry_instance']))
    records = []
    for sample_id, key in enumerate(sample_keys, 1):
        r = selected[key]
        matches = lookup[(r['name_raw'], r['raw_text_adjusted'])]
        assert matches, key
        fields = {m['field'].strip() for m in matches}
        assert len(fields) == 1, (key, fields)
        split, review_id = assignments[key]
        records.append(dict(r, sample_id=sample_id, split=split, review_id=review_id,
                            field_raw=next(iter(fields)), source_references=matches))
    out.mkdir(parents=True, exist_ok=True)
    for split in ('development', 'heldout'):
        write(out / f'{split}_sample.json', sorted([r for r in records if r['split'] == split], key=lambda r: r['review_id']), True)
    manifest = dict(created_at_utc=now(), population_size=79658, sample_size=500,
        draw_seed=20261020, split_seed=20261021,
        algorithm='random.Random(20261020).sample(sorted(source_keys), 500); copy then random.Random(20261021).shuffle; first 300 development, last 200 heldout',
        sample_keys_in_draw_order=sample_keys, development_keys=shuffled[:300], heldout_keys=shuffled[300:],
        counting_unit='edition-specific entry', release=RELEASE,
        source_checksums=checks, python_version=platform.python_version(),
        exact_biography_joins=500, nonblank_recovered_fields=sum(bool(r['field_raw']) for r in records),
        source_reference_count=sum(len(r['source_references']) for r in records),
        development_sample_sha256=sha(out / 'development_sample.json'),
        heldout_sample_sha256=sha(out / 'heldout_sample.json'))
    write(out / 'sample_manifest.json', manifest, True)
    assert all(sha(p) == h for p, h in checks.items())
    print(json.dumps({k: manifest[k] for k in ['population_size', 'sample_size', 'nonblank_recovered_fields', 'exact_biography_joins']}))


def review(out, split, start, end, full=False):
    if split == 'heldout':
        assert (out / 'rules_frozen.json').exists(), 'Freeze rules before opening held-out biographies'
    data = read(out / f'{split}_sample.json')
    for r in data:
        if not start <= r['review_id'] <= end:
            continue
        text = r['raw_text_adjusted']
        # Human review packets omit degrees, memberships and addresses, not scientific
        # field evidence. Complete original text remains available with --full.
        educ = re.search(r'\bEduc\b', text, re.I)
        prefix = text[:educ.start()] if educ else text[:500]
        research = re.search(r'\bRes\s*[:.;]', text)
        tail = text[research.end():] if research else ''
        tail = re.split(r'\bMailing\s+Add\b', tail, flags=re.I)[0].strip()
        print(f'\n[{r["review_id"]}] {r["source_key"]} {r["name_raw"]}\nFIELD: {r["field_raw"]}\nBIO: {prefix.strip()}\nRESEARCH: {tail}')
        if full:
            print('FULL:', text)


def freeze(out):
    assert (out / 'gold_development.json').exists()
    assert len(read(out / 'gold_development.json')) == 300
    assert not (out / 'gold_heldout.json').exists(), 'Holdout labels already exist'
    files = [Path(__file__), Path(__file__).with_name('classifier.py'), Path(__file__).with_name('rules_v1.json'), Path(__file__).with_name('test_classifier.py')]
    assert not (out / 'rules_frozen.json').exists(), 'Rule freeze is immutable'
    snapshot = out / 'frozen_code'
    snapshot.mkdir(exist_ok=True)
    for p in files:
        (snapshot / p.name).write_bytes(p.read_bytes())
    write(out / 'rules_frozen.json', dict(frozen_at_utc=now(),
          development_gold_sha256=sha(out / 'gold_development.json'),
          sample_manifest_sha256=sha(out / 'sample_manifest.json'),
          code_sha256={p.name: sha(p) for p in files},
          holdout_labels_present_at_freeze=False), True)
    print('Rules frozen. Held-out labels and predictions were not opened.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['prepare', 'review', 'freeze'])
    parser.add_argument('--data-root', type=Path, default=Path(os.environ.get('TALENT_DETS_DATA_DIR', DEFAULT_ROOT)))
    parser.add_argument('--output', type=Path)
    parser.add_argument('--split', choices=['development', 'heldout'], default='development')
    parser.add_argument('--start', type=int, default=1)
    parser.add_argument('--end', type=int, default=50)
    parser.add_argument('--full', action='store_true')
    args = parser.parse_args()
    out = args.output or args.data_root / 'output/amws' / NAME
    if args.command == 'prepare':
        prepare(args.data_root, out)
    elif args.command == 'review':
        review(out, args.split, args.start, args.end, args.full)
    else:
        freeze(out)


if __name__ == '__main__':
    sys.stdout.reconfigure(encoding='utf-8')
    main()

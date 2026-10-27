"""Scrape institution-level faculty counts from Education by the Numbers.

https://www.educationbythenumbers.com/browse/ lists, under "Higher Education",
about a hundred datasets transcribed from historical reports (American Almanac
1830-1861, Commissioner of Education reports 1870-1940, land-grant statistics
1949-1961). Each page embeds its full wpDataTables table(s) in the HTML, one
row per institution. This script:

  1. reads the browse page and keeps the Higher Education links (from the
     first college-enrollment page to the last land-grant page);
  2. downloads each page once into a cache (polite: one request per second,
     robots.txt only disallows /wp-admin/);
  3. parses every table with the standard library;
  4. keeps every column whose header is about faculty (faculty, professor,
     instructor, teacher, tutor, lecturer, teaching / instructional /
     professional staff, corps of instruction), with the institution, city and
     state, and classifies each as a count or a money / other column.

Output (OUT_DIR, default <Dropbox>/output/college_faculty_ebtn/):
  faculty_long.csv       one row per dataset x institution x faculty column
  faculty_columns.csv    every faculty column per dataset, its type and fill
  datasets.csv           every Higher Education page: title, rows, faculty columns
  provenance.json        URLs, fetch times, SHA-256 of each cached page
Cache: <Dropbox>/raw/educationbythenumbers/html/

Run from <repo>:
    python -I prep/occupational_fertility/scrape_ebtn_faculty.py
"""
import csv
import hashlib
import json
import os
import re
import time
import urllib.request
from datetime import datetime, timezone
from html.parser import HTMLParser
from pathlib import Path

DATA_ROOT = Path(os.environ.get('TALENT_DETS_DATA_DIR') or os.environ.get('DET_DIR')
                 or 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets')
BROWSE = 'https://www.educationbythenumbers.com/browse/'
CACHE = DATA_ROOT / 'raw/educationbythenumbers/html'
OUT_DIR = Path(os.environ.get('EBTN_OUT_DIR') or DATA_ROOT / 'output/college_faculty_ebtn')
FIRST_TITLE = 'College Enrollment by Gender'
LAST_TITLE = '1960-61 Statistics of Land-Grant Colleges'
USER_AGENT = 'Mozilla/5.0 (research scraper; gtl_talent_dets)'
FACULTY = re.compile(r'facult|professor|instructor|teacher|tutor|lecturer|teaching|instructional staff|'
                     r'professional staff|corps of instruction|preceptor', re.I)
NOT_COUNT = re.compile(r'salar|\$|expend|income|pay|cost|value|fund|degree|student|pupil|graduat|'
                       r'per cent|percent|ratio|average|endowed|training', re.I)
ID_COLUMNS = {'institution': re.compile(r'^(college|institution|name|university|school|name of)', re.I),
              'city': re.compile(r'^(city|location|post.?office|town)', re.I),
              'state': re.compile(r'^state', re.I)}


class Tables(HTMLParser):
    """Every <table> as {'id', 'headers', 'rows'}; links as (text, href)."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.tables, self.stack, self.links = [], [], []
        self.cell = self.row = None
        self.in_head = False
        self.link = None

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if tag == 'a':
            self.link = [a.get('href') or '', []]
        if tag == 'table':
            self.stack.append({'id': a.get('id'), 'headers': [], 'rows': []})
        elif not self.stack:
            return
        elif tag == 'thead':
            self.in_head = True
        elif tag == 'tr':
            self.row = []
        elif tag in ('td', 'th'):
            self.cell = []

    def handle_endtag(self, tag):
        if tag == 'a' and self.link is not None:
            self.links.append((' '.join(''.join(self.link[1]).split()), self.link[0]))
            self.link = None
        if not self.stack:
            return
        if tag in ('td', 'th') and self.cell is not None and self.row is not None:
            self.row.append(' '.join(''.join(self.cell).split()))
            self.cell = None
        elif tag == 'tr' and self.row is not None:
            t = self.stack[-1]
            if self.in_head and not t['headers']:
                t['headers'] = self.row
            elif not self.in_head:
                t['rows'].append(self.row)
            self.row = None
        elif tag == 'thead':
            self.in_head = False
        elif tag == 'table':
            self.tables.append(self.stack.pop())

    def handle_data(self, data):
        if self.cell is not None:
            self.cell.append(data)
        if self.link is not None:
            self.link[1].append(data)


def fetch(url, provenance):
    """Cached GET; one request per second when the network is used."""
    name = re.sub(r'[^a-z0-9]+', '_', url.split('educationbythenumbers.com', 1)[-1].lower()).strip('_') or 'root'
    path = CACHE / f'{name}.html'
    if not path.exists():
        req = urllib.request.Request(url, headers={'User-Agent': USER_AGENT})
        with urllib.request.urlopen(req, timeout=60) as resp:
            path.write_bytes(resp.read())
        provenance[url] = {'fetched_utc': datetime.now(timezone.utc).isoformat(timespec='seconds')}
        time.sleep(1.0)
    raw = path.read_bytes()
    provenance.setdefault(url, {}).update({'file': path.name, 'bytes': len(raw),
                                           'sha256': hashlib.sha256(raw).hexdigest()})
    return raw.decode('utf-8', errors='replace')


def parse(text):
    p = Tables()
    p.feed(text)
    return p


def to_number(value):
    v = value.replace(',', '').strip()
    return float(v) if re.fullmatch(r'-?\d+(\.\d+)?', v) else None


def main():
    CACHE.mkdir(parents=True, exist_ok=True)
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    prov_path = OUT_DIR / 'provenance.json'
    provenance = json.loads(prov_path.read_text()).get('pages', {}) if prov_path.exists() else {}

    links = parse(fetch(BROWSE, provenance)).links
    titles = [t for t, _ in links]
    start = next(i for i, t in enumerate(titles) if t.startswith(FIRST_TITLE))
    end = next(i for i, t in enumerate(titles) if t.startswith(LAST_TITLE))
    pages = [(t, h) for t, h in links[start:end + 1] if 'educationbythenumbers.com' in h]
    assert len(pages) > 90, f'Expected about 100 Higher Education pages, found {len(pages)}'
    print(f'{len(pages)} Higher Education pages')

    long_rows, column_rows, dataset_rows = [], [], []
    for title, url in pages:
        tables = parse(fetch(url, provenance)).tables
        year = (re.match(r'(\d{4})', title) or [None, ''])[1]
        n_faculty_cols = 0
        n_rows = max((len(t['rows']) for t in tables), default=0)
        for t in tables:
            h = t['headers']
            if not h:
                continue
            idx = {k: next((i for i, c in enumerate(h) if rx.search(c)), None) for k, rx in ID_COLUMNS.items()}
            if idx['institution'] is None:
                idx['institution'] = 0
            for j, col in enumerate(h):
                if not FACULTY.search(col) or j in idx.values():
                    continue
                n_faculty_cols += 1
                kind = 'other' if NOT_COUNT.search(col) else 'count'
                values = [r[j] if j < len(r) else '' for r in t['rows']]
                numeric = [to_number(v) for v in values]
                column_rows.append({
                    'dataset': title, 'year': year, 'url': url, 'table': t['id'], 'column': col, 'kind': kind,
                    'rows': len(values), 'filled': sum(1 for v in values if v.strip()),
                    'numeric': sum(1 for v in numeric if v is not None),
                    'total': sum(v for v in numeric if v is not None)})
                for r, v, num in zip(t['rows'], values, numeric):
                    get = lambda k: r[idx[k]] if idx[k] is not None and idx[k] < len(r) else ''
                    long_rows.append({'dataset': title, 'year': year, 'table': t['id'],
                                      'institution': get('institution'), 'city': get('city'),
                                      'state': get('state'), 'column': col, 'kind': kind,
                                      'value': v, 'value_numeric': '' if num is None else num})
        dataset_rows.append({'dataset': title, 'year': year, 'url': url, 'tables': len(tables),
                             'rows': n_rows, 'faculty_columns': n_faculty_cols})
        print(f'  {title[:70]:70s} tables {len(tables)} rows {n_rows:5d} faculty columns {n_faculty_cols}')

    def write(name, rows):
        with open(OUT_DIR / name, 'w', newline='', encoding='utf-8') as f:
            w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
            w.writeheader()
            w.writerows(rows)
    write('faculty_long.csv', long_rows)
    write('faculty_columns.csv', column_rows)
    write('datasets.csv', dataset_rows)
    prov_path.write_text(json.dumps({'source': BROWSE, 'completed_utc':
                                     datetime.now(timezone.utc).isoformat(timespec='seconds'),
                                     'pages': provenance}, indent=2))
    print(f'Wrote {len(long_rows):,} faculty values from {len(column_rows)} columns in '
          f'{sum(1 for d in dataset_rows if d["faculty_columns"])} of {len(dataset_rows)} datasets to {OUT_DIR}')


if __name__ == '__main__':
    main()

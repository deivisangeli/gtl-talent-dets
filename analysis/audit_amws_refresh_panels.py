"""Reconcile release, annual support, date assignments and gender audit totals."""
import csv
import json
from collections import Counter

from amws_publication_refresh import ROOT, RUN, digest


def rows(path):
    with path.open(encoding='utf-8-sig', newline='') as stream:
        yield from csv.DictReader(stream)


def count(value):
    return int(float(value)) if value else 0


def audit():
    output = ROOT / 'output/mandatory_schooling'
    monthly = output / 'monthly_gapminder_1800_1960_audits'
    quarterly = output / 'quarterly_gapminder_1800_1960_audits'
    source = {r['metric']: r['value'] for r in rows(ROOT / 'output/amws/amws_consolidated_county_year_summary.csv')}
    totals = Counter()
    annual_path = monthly / 'annual_scientist_reconciliation.csv'
    for r in rows(annual_path):
        assert r['early_reconciles'] == r['late_reconciles'] == 'TRUE'
        early, late = count(r['n_early_year']), count(r['n_1986_year'])
        assigned = count(r['n_early_assigned']) + count(r['n_1986_assigned'])
        unassigned = count(r['n_unassigned_month'])
        assert early + late == assigned + unassigned
        key = 'annual_supported' if r['annual_panel_present'] == 'TRUE' else 'outside_annual_support'
        totals[key] += early + late
        totals['early'] += early
        totals['late'] += late
        totals['assigned'] += assigned
        totals['unassigned'] += unassigned
        if key == 'annual_supported':
            assert early == count(r['n_early_existing']) and late == count(r['n_1986_existing'])
        totals['county_years'] += 1
    assert totals['early'] + totals['late'] == totals['assigned'] + totals['unassigned']
    totals['outside_contiguous_state_universe'] = int(source['total_amws_births_in_panel']) - totals['annual_supported']
    assert totals['outside_contiguous_state_universe'] >= 0
    gender = {}
    for r in rows(monthly / 'gender_classification_coverage.csv'):
        key = r['gender_80']
        group = gender.setdefault(key, Counter())
        group['records'] += count(r['records'])
        group['assigned'] += count(r['assigned_month'])
        group['unassigned'] += count(r['unassigned_month'])
        assert count(r['records']) == count(r['assigned_month']) + count(r['unassigned_month'])
    for field in ('assigned', 'unassigned'):
        assert sum(g[field] for g in gender.values()) == totals[field]
    assert sum(g['records'] for g in gender.values()) == totals['early'] + totals['late']
    recon_paths = []
    for cadence, directory in [('monthly', monthly), ('quarterly', quarterly)]:
        path = directory / f'gender_{cadence}_annual_reconciliation.csv'
        recon_paths.append(path)
        groups = Counter()
        for r in rows(path):
            assert r['gender_inference_threshold'] == '0.8'
            for group in ('male', 'female', 'unclassified'):
                assert r[f'{cadence}_reconciles_{group}_80'] == 'TRUE'
                n = count(r[f'n_amws_{group}_80'])
                assert n == count(r[f'n_early_assigned_{group}_80']) + count(r[f'n_1986_assigned_{group}_80'])
                groups[group] += n
        assert all(groups[g] == gender[g]['assigned'] for g in groups)
    coverage_m = list(rows(monthly / 'coverage_state_year.csv'))
    coverage_q = list(rows(quarterly / 'coverage_state_year.csv'))
    assert len(coverage_m) == len(coverage_q)
    for m, q in zip(coverage_m, coverage_q):
        assert all(q[k] == v for k, v in m.items() if k in q)
    sources = [annual_path, monthly / 'gender_classification_coverage.csv',
               monthly / 'coverage_state_year.csv', quarterly / 'coverage_state_year.csv', *recon_paths]
    result = {
        'annual_source_records': int(source['total_amws_births_in_panel']),
        'annual_early_records': int(source['amws_1906_1955_dedup_births_in_panel']),
        'annual_1986_records': int(source['amws_1986_births_in_panel']),
        'date_panel_universe': dict(totals),
        'gender_threshold': 0.8,
        'gender_groups': {g: dict(values) for g, values in gender.items()},
        'explanation': 'The annual source panel contains only county-years with valid population support. '
            'The monthly/quarterly grid follows the existing contiguous-state county universe and includes '
            'date records outside annual denominator support. Those extra county-years remain excluded '
            'from models requiring positive birth denominators. No-saved-inference records retain the '
            'established unclassified status.',
        'sources': {str(p): digest(p) for p in sources},
        'passed': True,
    }
    (RUN / 'panel_reconciliation.json').write_text(json.dumps(result, indent=2))
    print(json.dumps({k: v for k, v in result.items() if k not in ('sources', 'explanation')}))


if __name__ == '__main__':
    audit()

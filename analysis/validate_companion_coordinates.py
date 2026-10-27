"""Independently check embedded companion points and intervals against CSVs."""
import csv
import json
import math
from pathlib import Path
import re

from amws_publication_refresh import REPO, ROOT, RUN, TAG, digest

NUMBER = r'[-+0-9.eE]+'
POINT = re.compile(r'\((' + NUMBER + r'),(' + NUMBER + r')\)')
ERROR = re.compile(r'\((' + NUMBER + r'),(' + NUMBER + r')\)\s*\+=\s*\(0,(' + NUMBER + r')\)\s*-=\s*\(0,(' + NUMBER + r')\)')
PLOT = re.compile(r'\\addplot\[([^\]]+)\]\s*coordinates\s*\{([^}]+)\};')


def read(path, **filters):
    with path.open(encoding='utf-8-sig', newline='') as stream:
        return [r for r in csv.DictReader(stream) if all(r[k] == str(v) for k, v in filters.items())]


def close(a, b):
    assert abs(a - b) <= 1e-9 * max(1, abs(a), abs(b)), (a, b)


def check_axis(code, expected, y='estimate', low='ci_low', high='ci_high', reference=-10):
    index = {float(r['event_time']): r for r in expected}
    assert len(index) == len(expected)
    points = {}
    for options, coordinates in PLOT.findall(code):
        if 'mark=' not in options:
            continue
        if 'error bars/.cd' in options:
            for event, value, plus, minus in ERROR.findall(coordinates):
                e, value, plus, minus = map(float, (event, value, plus, minus))
                row = index[e]
                close(value, float(row[y])); close(value + plus, float(row[high])); close(value - minus, float(row[low]))
                assert e not in points
                points[e] = value
        else:
            for event, value in POINT.findall(coordinates):
                e, value = float(event), float(value)
                assert e not in points
                close(value, float(index[e][y]))
                points[e] = value
    required = {e for e, r in index.items() if r.get(y) not in ('', 'NA', None)
                and math.isfinite(float(r[y]))}
    assert set(points) == required, (set(points), required)
    return {'points': len(points), 'reference': reference, 'passed': True}


def validate():
    text = (REPO / 'project/report_companion.tex').read_text()
    axes = re.findall(r'\\begin\{axis\}(.*?)\\end\{axis\}', text, re.S)
    assert len(axes) == 10
    result_root = ROOT / 'results'
    gi_asset = REPO / 'project/figures/wwii_gi_bill_amws_reduced_form_baseline_quarterly_male.tex'
    gi_axes = re.findall(r'\\begin\{axis\}(.*?)\\end\{axis\}', gi_asset.read_text(), re.S)
    assert PLOT.findall(axes[0]) == PLOT.findall(gi_axes[0])
    checks = [{'axis': 1, 'source': str(gi_asset), 'passed': True}]
    college = result_root / 'land_grants/event_studies/amws_county_pairs_temporal_support_1830_1950_baseline_pop_balanced_event_time_max70/baseline_pop_dynamic_att.csv'
    fairs = result_root / 'worlds_fair/worlds_fairs_us_amws_event_studies_fair_cohorts_1840_1900_treatment_m20_e_m30_p50_balanced_oldest_logpop_m10_single_fair/all_dynamic_att.csv'
    uk = result_root / 'worlds_fair/synthdid/ukds_1911_lgd_wikipedia_donor_pools/match_v4_balanced_e0/pooled_dz_rematched_placebos/dz_pooled_dynamic_effects.csv'
    facilities = result_root / 'scientific_facilities/event_studies/amws_scientific_facilities_decennial_pre1944_end1949' / TAG / 'dynamic_att.csv'
    # Discover the tagged appendix folders through the source manifest rather
    # than imposing an inferred folder layout.
    provenance = json.loads((RUN / 'companion_validation.json').read_text())
    appendix_sources = [Path(p) for p in provenance['sources'] if TAG in p and Path(p).name.endswith('dynamic_att.csv')]
    college_a = next(p for p in appendix_sources if 'land_grants' in str(p))
    facility_a = next(p for p in appendix_sources if 'facilities' in str(p) and p != facilities)
    specifications = [
        (2, college, {'outcome': 'amws_per_1000_births', 'spec': 'births_level', 'timing': 'standard_decade'}, {'reference': 0}),
        (3, college, {'outcome': 'county_births_estimate', 'spec': 'births_level', 'timing': 'standard_decade'}, {'reference': 0}),
        (4, fairs, {'outcome': 'amws_per_1000_births', 'treatment_spec': 'hosted'}, {}),
        (5, uk, {'model_id': 'lead20_standard'}, {'y': 'pooled_effect', 'low': 'effect_ci_lower', 'high': 'effect_ci_upper'}),
        (6, uk, {'model_id': 'lead20_standard'}, {'y': 'mean_percentile_rank', 'low': 'rank_null_lower', 'high': 'rank_null_upper'}),
        (7, facilities, {'outcome': 'amws_per_1000_births', 'control_group': 'nevertreated', 'balance': 'unbalanced', 'timing': 'standard_decade'}, {}),
        (8, college_a, {'outcome': 'amws_per_1000_births', 'timing': 'standard_decade'}, {'reference': 0}),
        (9, college_a, {'outcome': 'county_births_estimate', 'timing': 'standard_decade'}, {'reference': 0}),
        (10, facility_a, {'outcome': 'amws_per_1000_births', 'control_group': 'nevertreated', 'balance': 'calendar_balanced_event_unbalanced', 'timing': 'standard_decade'}, {}),
    ]
    for axis, path, filters, options in specifications:
        check = check_axis(axes[axis - 1], read(path, **filters), **options)
        checks.append({'axis': axis, 'source': str(path), **check})
    assert all(digest(Path(p)) == h for p, h in provenance['sources'].items())
    (RUN / 'companion_coordinate_validation.json').write_text(json.dumps(checks, indent=2))
    print('Verified all 10 companion axes against saved exports; source checksums unchanged.')


if __name__ == '__main__':
    validate()

"""Independently reconstruct paired DiD, joint influence and bootstrap outputs.

Uses numpy/pandas and no R model objects. Set TALENT_DETS_DATA_DIR before running.
"""
from pathlib import Path
import hashlib
import json
import os

import numpy as np
import pandas as pd


def verify(root):
    out = root / "results/boll_weevil/amws_county_csdid_fe_counties"
    roster = pd.read_csv(out / "county_roster_and_roles.csv", dtype={"GEOID": str})
    prior = pd.read_csv(root / "results/boll_weevil/amws_county_twfe/counties_broader_590.csv", dtype={"GEOID": str})
    assert len(roster) == 590 and set(roster.GEOID) == set(prior.GEOID)
    assert roster.id.is_monotonic_increasing and not roster.id.duplicated().any()
    assert (roster.g == roster.infestation_year - 18).all()
    assert (roster.target == (roster.infestation_year <= 1907)).all()
    assert roster.target.sum() == 133
    observed = pd.read_csv(out / "available_county_birth_year_panel.csv", dtype={"GEOID": str})
    assert not observed.duplicated(["id", "year"]).any()
    np.testing.assert_allclose(observed.model_y, 1000 * observed.n_amws / observed.county_births_estimate_year, atol=1e-9)
    assert (observed.n_amws == observed.n_amws_1906_1955_dedup + observed.n_amws_1986).all()
    source = pd.read_csv(root / "output/us_panel_county_amws_combined_year.csv", dtype={"GEOID": str},
                         usecols=["GEOID", "year", "n_amws", "county_births_estimate_year", "amws_per_1000_births"])
    original = observed.merge(source, on=["GEOID", "year"], suffixes=("", "_original"), validate="one_to_one")
    assert len(original) == len(observed)
    for column in ["n_amws", "county_births_estimate_year", "amws_per_1000_births"]:
        np.testing.assert_allclose(original[column], original[column + "_original"], atol=1e-9)
    wide = observed.pivot(index="id", columns="year", values="model_y").reindex(roster.id)
    cells = pd.read_csv(out / "group_age_coefficients_and_validation.csv").sort_values("comparison_id")
    assert len(cells) == 405
    influence = np.zeros((590, 405))
    counts = np.zeros(590, dtype=int)
    G = roster.infestation_year.to_numpy()
    onset = G - 18
    errors = []
    membership = pd.read_csv(out / "comparison_membership.csv")
    member_sets = membership.groupby("comparison_id").id.agg(set).to_dict()
    for j, row in enumerate(cells.itertuples()):
        base, year = int(row.base_year), int(row.year)
        target = G == row.infestation_year
        y0, y1 = wide[base].to_numpy(), wide[year].to_numpy()
        present = np.isfinite(y0) & np.isfinite(y1)
        assert present[target].all()
        control = (~target) & (onset > max(base, year)) & present
        selected = target | control
        assert control.sum() == row.control_counties and target.sum() == row.treated_counties
        assert set(roster.id[selected]) == member_sets[row.comparison_id]
        delta = y1 - y0
        mt, mc = delta[target].mean(), delta[control].mean()
        estimate = mt - mc
        errors.append(abs(estimate - row.estimate))
        influence[target, j] = 590 / target.sum() * (delta[target] - mt)
        influence[control, j] = -590 / control.sum() * (delta[control] - mc)
        if row.age_at_infestation != 19:
            counts += selected
        else:
            assert estimate == 0 and not influence[:, j].any()
    assert max(errors) < 1e-9 and cells.control_counties.min() == 35
    assert np.array_equal(counts, roster.nonreference_comparisons)
    assert (counts > 0).sum() == 588
    assert set(roster.loc[counts == 0, "GEOID"]) == {"37063", "37181"}
    annual = pd.read_csv(out / "event_study_coefficients.csv")
    pooled = pd.read_csv(out / "pooled_age_window_coefficients.csv")
    groups = sorted(roster.loc[roster.target, "infestation_year"].unique())
    ng = np.array([(G == g).sum() for g in groups])
    weights = ng / 133
    member = (G[:, None] == groups).astype(int)
    annual_if = np.zeros((590, len(annual)))
    for j, row in enumerate(annual.itertuples()):
        z = cells[cells.age_at_infestation == row.age_at_infestation].set_index("infestation_year").loc[groups]
        theta = z.estimate.to_numpy()
        expected = weights @ theta
        assert abs(expected - row.estimate) < 1e-9
        positions = z.comparison_id.to_numpy().astype(int) - 1
        annual_if[:, j] = influence[:, positions] @ weights + 590 / 133 * member @ (theta - expected)
    pooled_if = np.zeros((590, len(pooled)))
    for j, row in enumerate(pooled.itertuples()):
        lo, hi = map(int, row.age_window.split("-"))
        index = annual.age_at_infestation.between(lo, hi)
        assert abs(annual.loc[index, "estimate"].mean() - row.estimate) < 1e-9
        pooled_if[:, j] = annual_if[:, index].mean(axis=1)
        assert row.county_age_observations == 133 * (hi - lo + 1)
    combined = np.column_stack([annual_if, pooled_if])
    analytic = combined.T @ combined / 590**2
    saved_cov = pd.read_csv(out / "joint_analytic_covariance.csv").iloc[:, 1:].to_numpy()
    np.testing.assert_allclose(analytic, saved_cov, atol=1e-9, rtol=1e-8)
    high = pd.read_csv(out / "county_mammen_high_draws.csv.gz")
    assert list(high.columns) == list(roster.id.astype(str)) and high.shape == (1000, 590)
    assert np.isin(high, [0, 1]).all()
    multipliers = np.where(high, (1 + np.sqrt(5)) / 2, (1 - np.sqrt(5)) / 2)
    draws = multipliers @ combined / 590
    # R quantile(type=1) is the inverse empirical CDF, i.e. order ceil(n*p).
    ordered = np.sort(draws, axis=0)
    normal_iqr = 1.3489795003921634
    se = (ordered[749] - ordered[249]) / normal_iqr
    np.testing.assert_allclose(se[:27], annual.se, atol=1e-9)
    np.testing.assert_allclose(se[27:], pooled.se, atol=1e-9)
    nonzero = np.abs(annual_if).sum(axis=0) > 1e-10
    supremum = np.max(np.abs(draws[:, :27][:, nonzero] / se[:27][nonzero]), axis=1)
    raw_critical = np.sort(supremum)[949]
    critical = max(1.959963984540054, raw_critical)
    np.testing.assert_allclose(annual.simultaneous_critical_value, critical, atol=1e-9)
    np.testing.assert_allclose(annual.simultaneous_low, annual.estimate - critical * annual.se, atol=1e-9)
    np.testing.assert_allclose(annual.simultaneous_high, annual.estimate + critical * annual.se, atol=1e-9)
    np.testing.assert_allclose(pooled.conf_low, pooled.estimate - 1.959963984540054 * pooled.se, atol=1e-9)
    np.testing.assert_allclose(pooled.conf_high, pooled.estimate + 1.959963984540054 * pooled.se, atol=1e-9)
    assert np.mean(supremum <= critical) >= 0.95
    for row in pd.read_csv(out / "input_and_code_manifest.csv").itertuples():
        assert hashlib.md5(Path(row.path).read_bytes()).hexdigest() == row.md5, row.path
    for row in pd.read_csv(out / "prior_results_protection.csv").itertuples():
        assert row.md5_before == row.md5_after == hashlib.md5(Path(row.path).read_bytes()).hexdigest(), row.path
    assert pd.read_csv(out / "validation_checks.csv").passed.all()
    report = {"status": "passed", "county_roster": 590, "contributing_counties": 588,
              "target_counties": 133, "group_age_cells": 405, "minimum_paired_controls": 35,
              "maximum_direct_did_error": float(max(errors)),
              "maximum_joint_covariance_error": float(np.max(np.abs(analytic - saved_cov))),
              "bootstrap_draws_reconstructed": 1000, "joint_critical_value": float(critical),
              "paired_membership_verified": True, "input_and_prior_results_hashes_unchanged": True}
    (out / "independent_python_validation.json").write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    return report


if __name__ == "__main__":
    root = Path(os.environ.get("TALENT_DETS_DATA_DIR", str(Path.home() / "Globtalent Dropbox/gtl_talent_dets")))
    print(json.dumps(verify(root), indent=2))

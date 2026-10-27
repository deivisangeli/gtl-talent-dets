"""Independent checks of exported age-4-40 FE sample, contrasts, and provenance."""
import hashlib
import json
import os
from pathlib import Path

import numpy as np
import pandas as pd

root = Path(os.environ.get(
    "TALENT_DETS_DATA_DIR",
    str(Path.home() / "Globtalent Dropbox" / "gtl_talent_dets"),
))
out = root / "results/boll_weevil/amws_county_fe_ages_4_40"
panel = pd.read_csv(out / "estimation_panel.csv", dtype={"GEOID": str})
annual = pd.read_csv(out / "annual_age_coefficients.csv")
tests = pd.read_csv(out / "age_window_tests.csv")
cov = pd.read_csv(out / "annual_coefficient_covariance.csv", index_col=0)
reported_weights = pd.read_csv(out / "contrast_weights.csv", index_col=0)
reported_contrast_cov = pd.read_csv(out / "contrast_covariance.csv", index_col=0)
ages = list(range(4, 19)) + list(range(31, 41))
terms = [f"age_{a}" for a in ages]
assert len(panel) == 20535 and panel.GEOID.nunique() == 555
assert not panel.duplicated(["GEOID", "year"]).any()
assert (panel.groupby("GEOID").size() == 37).all()
assert (panel.groupby("age_at_infestation").size() == 555).all()
assert (panel.age_at_infestation == panel.infestation_year - panel.year).all()
assert (panel.county_births_estimate_year > 0).all()
assert np.isfinite(panel.model_y).all()
assert np.max(np.abs(panel.model_y - 1000 * panel.n_amws / panel.county_births_estimate_year)) < 1e-10
assert list(annual.age_at_infestation) == ages
for a, term in zip(ages, terms):
    assert (panel[term] == (panel.age_at_infestation == a)).all()
assert (panel.loc[panel.age_at_infestation.between(19, 30), terms] == 0).all().all()
windows = [range(4, 10), range(10, 14), range(14, 19), range(13, 19), range(16, 19)]
weights = np.zeros((6, 25))
for i, window in enumerate(windows):
    for a in window:
        weights[i, ages.index(a)] = 1 / len(window)
weights[5] = weights[2] - weights[0]
assert np.max(np.abs(weights - reported_weights[terms].to_numpy())) < 1e-14
beta = annual.set_index("term").loc[terms, "estimate"].to_numpy()
V = cov.loc[terms, terms].to_numpy()
theta = weights @ beta
C = weights @ V @ weights.T
assert np.max(np.abs(theta - tests.estimate)) < 1e-12
assert np.max(np.abs(C - reported_contrast_cov.to_numpy())) < 1e-12
assert np.max(np.abs(np.sqrt(np.diag(C)) - tests.se)) < 1e-12
assert np.max(np.abs(theta - tests.critical_value * tests.se - tests.conf_low)) < 1e-12
assert np.max(np.abs(theta + tests.critical_value * tests.se - tests.conf_high)) < 1e-12
order = np.argsort(tests.p_value.to_numpy())
adjusted = np.minimum(1, np.maximum.accumulate(tests.p_value.to_numpy()[order] * np.arange(6, 0, -1)))
assert np.max(np.abs(tests.p_holm.to_numpy()[order] - adjusted)) < 1e-12
for filename in ["input_and_code_manifest.csv", "prior_results_protection.csv"]:
    manifest = pd.read_csv(out / filename)
    for row in manifest.itertuples():
        digest = hashlib.md5(Path(row.path).read_bytes()).hexdigest()
        assert digest == (row.md5 if hasattr(row, "md5") else row.md5_before)
audit = pd.read_csv(out / "county_inclusion_audit.csv", dtype={"GEOID": str})
missing = pd.read_csv(out / "missing_age_observations.csv", dtype={"GEOID": str})
assert len(audit) == 590 and audit.included.sum() == 555
assert set(audit.loc[audit.included, "GEOID"]) == set(panel.GEOID)
assert missing.GEOID.nunique() == 35
assert pd.read_csv(out / "validation_checks.csv").passed.all()
result = {"passed": True, "counties": 555, "observations": 20535,
          "annual_estimates": 25, "contrast_estimate_max_error": float(np.max(np.abs(theta - tests.estimate))),
          "checks": "sample, reference, age indicators, explicit weights, joint covariance, intervals, Holm, exclusions, all protected hashes"}
(out / "independent_export_validation.json").write_text(json.dumps(result, indent=2), encoding="utf-8")
print(json.dumps(result))

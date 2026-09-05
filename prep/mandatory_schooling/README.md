# Mandatory Schooling Prep Pipeline

There is no dedicated prep script for mandatory schooling yet.

The current pipeline is manual:

1. Maintain `DATA_INPUT/compulsory_schooling_laws.csv` in Dropbox.
2. Run `analysis/mandatory_schooling/analysis_compulsory_schooling.R`, which reads the manual CSV directly and assigns treatment by state law decade.

The AMWS-outcome analysis uses the consolidated county-decade panel shared
with the USA world's-fairs pipeline. Its default profile is unbalanced. To run
the event-time-balanced `e = -40,...,+50` profile in PowerShell:

```powershell
$env:COMPULSORY_AMWS_PROFILE = "balanced_m40_p50"
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws.R
```

The balanced profile requires every retained county at every reported event
time, but keeps each retained county's other available decades in estimation
so the `e = +50` coefficient remains identified.

The balanced run also writes treatment-cohort event studies to
`results/mandatory_schooling/compulsory_schooling_amws_balanced_m40_p50/per_cohort/`.
These figures show the unaggregated Sun-Abraham cohort-by-event-time cells for
all four AMWS outcomes in levels and cohort-linear-detrended form. All seven law
cohorts are shown. Collinear boundary cells are left blank and recorded in the
support audit (`g = 1850` at `e = -40,-30,-20`; `g = 1910` at `e = +50`). The
balanced sample has no never-treated states, so this is the appropriate analogue
of the college-allocation per-cohort CSDID figures rather than a literal reuse
of their never-treated runner-up design.

This is separate from `prep/elite_schools`: mandatory schooling is a broad state-level compulsory attendance law treatment, while elite schools are selective secondary-school openings.

## Annual AMWS Callaway–Sant'Anna studies

Run both annual profiles from the repository root:

```powershell
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_annual_csdid.R
```

To select one profile, set `COMPULSORY_AMWS_ANNUAL_PROFILE` to `pre20` or `pre40`.
The default is `both`. Set `TALENT_DETS_DATA_DIR` to override the Dropbox root.

| Profile | Annual event window | Balanced treated sample |
|---|---|---|
| `pre20` | -20 through +20 | 322 counties; 11 states plus DC |
| `pre40` | -40 through +20 | 195 counties; 8 states plus DC |

Targets are jurisdictions with a recorded first compulsory-law year during
1850–1880, inclusive. Each retained target county must have every event year
and valid values for all four AMWS outcomes and both denominators. Other years
are retained for estimation. Later adopters supply controls before exposure;
later early-adopter cohorts can also serve as controls while unexposed. Counties
with no observed unexposed year cannot supply controls and are excluded from
both levels and detrended fits. Missing law dates are excluded, not classified
as never treated. The historical dates remain provisional inputs requiring
independent verification.

The outcome is indexed by **birth year**, with event time zero at law passage.
The agreed exposure assumption allows children born up to 14 years before
passage to be exposed: `did::att_gt(anticipation=14, base_period="universal",
control_group="notyettreated")`. The reference birth cohort is **-15**, and
birth cohorts -14 through -1 are shaded as potentially exposed. Consequently,
the two profiles contain **6 and 26 unexposed pre-law years**, respectively,
including the reference. These are not windows of 20 and 40 wholly unexposed
years. The 14-year allowance is a modeling assumption, not a verified statement
about each state's historical school-age rules.

The four outcomes are AMWS counts, log(1+counts), births per 1,000 population,
and births per 1,000 estimated births. Each is estimated in levels and after
subtracting an adoption-year-cohort linear trend fitted only over -pre through
-15. Inference for detrended outcomes is conditional on the estimated trends.
Annual scientist counts use recorded birth years; county population is linearly
interpolated between censuses and county birth denominators are estimated.

Dynamic aggregation includes only retained early-adopter cohorts, with weights
proportional to their county counts. It preserves joint influence functions and
uncertainty in cohort shares. No requested event year is silently reweighted
because one target cohort is missing. Confidence intervals are 95% pointwise,
using a state-clustered multiplier bootstrap with 1,000 draws. Base seed
`20260905` has deterministic profile/model offsets recorded in the exports.

Outputs are in
`results/mandatory_schooling/compulsory_schooling_amws_annual_csdid/`, under
`pre20_post20_a14/` and `pre40_post20_a14/`: pooled and per-cohort PNG/PDF figures,
coefficient CSVs, input/sample/support audits, trend fits, estimator warnings,
direct-DiD validation, and fitted RDS models. Previous decennial outputs are
preserved. Each model RDS includes the complete fit and the target-only
aggregation, so joint inference can be reproduced.

The supplied calendar panel extends through 1960. The `did` package internally
truncates estimation when no not-yet-exposed comparison remains (1903 in these
runs). `sample_summary.csv` reports supplied rows; `model_metadata.csv` reports
the processed rows and years. Every requested target event year is earlier than
that cutoff. The final `validation_summary.csv` checks sample counts, estimates,
aggregation, and cluster alignment. To verify saved models and regenerate figures
without fitting the models again:

```powershell
Rscript analysis/mandatory_schooling/analysis_compulsory_schooling_amws_annual_csdid.R --verify-outputs
```

Run the focused synthetic estimator checks from the repository root:

```powershell
Rscript analysis/mandatory_schooling/test_annual_csdid.R
```

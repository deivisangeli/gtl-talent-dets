# GI Bill: descriptive AMWS birth-cohort comparison

This pipeline compares inferred male and female AMWS record counts for US-born
cohorts from **1910 through 1950**. It does not estimate a causal GI Bill effect.
Its primary input is the saved full-dataset classification under
`AMWS_OUTPUT/gender_inference/`; existing AMWS inclusion and extraction rules
remain intact. Earlier-edition cross-references in 1986 are excluded through the
county requirement and the existing contaminated-cross-reference safeguard.
Retained 1986 biographies are later-directory contributions, rather than complete
coverage of every cohort; the combined series is the primary outcome.

## Execution

Install `data.table`, `ggplot2`, `gender`, and `genderdata` if needed. The last two
packages and the name references were already installed for the gender pilot.
From the repository root, run:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/<username>/Globtalent Dropbox/gtl_talent_dets'
Rscript analysis/gi_bill/analysis_amws_gi_bill_cohorts.R
```

Alternatively, set `TALENT_DETS_DATA_DIR` and source the script from the repository
root in R. The script resolves `repo_root` from `--file` or `GTL_REPO`/`getwd()`
and sources `paths.R`. No name API requests or input-file writes are made.

## Measures and sensitivity

- **Absolute gap**: inferred male records minus inferred female records. Years
  on the horizontal axis are birth years, not calendar years of scientific work.
- **Women's share**: women divided by men plus women. Unresolved records are
  excluded from this denominator. `relative_count_gap` is (men minus women)
  divided by (men plus women); `men_per_woman` is men divided by women.
  `female_per_male` is women divided by men: 0.10 means 10 female records per
  100 male records. It is undefined when male counts are zero.
- **Coverage**: men plus women divided by all included records. Reference
  proportions and coverage do not measure verified classification precision.
- **Indexed counts**: each sex's count divided by its own 1910-1914 annual mean,
  multiplied by 100. Baselines are calculated separately for each specification.
- **SSA-only sensitivity**: query the same extracted/reviewed first names using
  SSA for all 1910-1950 cohorts, retaining the birth-decade reference windows,
  95% cutoff, and any documented identity-specific correction. The 1950 birth
  cohort uses the 1950-1959 reference window. Unrecoverable names remain
  unreadable; no additional name cleaning is applied. Post-1929 results must
  reproduce the primary SSA results exactly, within numeric tolerance.

Shares, ratios, percentage changes, and indices are blank when their denominator
is zero. Zero-count years retain count zeros and undefined shares. Five-year
tables contain eight complete groups, 1910-1914 through 1945-1949; 1950 remains
in annual tables only. Five-year changes are calculated against the immediately
preceding group and against 1920-1924, within each specification. The first group
has no preceding comparison. Percentage shares/coverage in CSVs are fractions;
columns ending `_pct` and `_pp` contain percentages and percentage points.

## Outputs

All outputs are separate in the shared Dropbox project:
`results/gi_bill/cohort_comparison/`.

| File | Contents |
|---|---|
| `annual_counts.csv` | 41 years for each of primary and SSA-only inference; all count categories, gaps, shares, coverage and indices |
| `five_year_counts.csv` | Eight complete cohort groups for each specification |
| `five_year_changes.csv` | Group counts, changes from the previous group and from 1920-1924 |
| `edition_annual_counts.csv` | Primary counts/coverage by birth year and edition, including zero cells |
| `baseline_indices.csv` | Sex/specification-specific baseline annual means |
| `ssa_record_audit.csv` | Every selected record's name, extraction status, primary and SSA predictions/classifications, correction provenance, change indicator |
| `ssa_name_reference.csv` | All queried name/window pairs, retaining unmatched lookups |
| `sensitivity_summary.csv` | Changed-record counts by birth decade and primary reference method |
| `classification_transitions.csv` | Primary-to-SSA category transition counts |
| `source_checksums.csv` | Before/after MD5 checks for protected source, panel, pilot and gender outputs |
| `run_metadata.csv` | Execution time, versions, population, reference batches and definitions |

`figures/` contains eight figures, each in PNG and PDF: `annual_gender_gap`,
`annual_relative_count_gap`, `annual_female_male_ratio`, `annual_gender_counts`,
`annual_indexed_counts`, `annual_relative_gaps`,
`annual_classification_coverage`, and `annual_edition_contributions`. The gap,
relative-gap and coverage figures compare primary with SSA-only inference.
The 1920-1939 birth cohorts are shaded, and the primary method switch to SSA at
1930 is marked. Male/female count panels have separate labeled count axes.
The dedicated `annual_relative_count_gap` figure plots 100 times (men minus
women) divided by (men plus women), with the primary and SSA-only series. Zero
means equal counts; 100% means every classified record is male. Unresolved
records are excluded from the denominator.
The dedicated `annual_female_male_ratio` figure compares primary and SSA-only
female-to-male ratios. It uses the ratio itself on the vertical axis, rather
than the female share of all classified records.

## Validation and interpretation

Embedded checks require 112,291 input records with unique record identifiers;
reproduce existing annual category counts; verify every year, edition sums,
complete five-year sums, zero-denominator behavior, threshold boundaries and
indexed baselines; reproduce primary SSA predictions for births from 1930;
and verify protected files are unchanged. The four published cohort summaries
for 1920-1939 are regression checks. Original inputs, panels, pilot and gender
outputs are read only; rerunning replaces only this pipeline's own outputs.

The absolute gap depends on birth-cohort size as well as scientific production.
Changes also reflect directory inclusion, geography coverage, career timing and
unresolved classifications. Younger cohorts had less time to become established
before the 1986 directory. The 1930 reference-method switch can affect comparisons
across that boundary; the SSA-only results test sensitivity to that choice,
without validating name-based accuracy.

The [1944 GI Bill](https://www.archives.gov/milestone-documents/servicemens-readjustment-act)
provided benefits based on military service. Birth cohort and inferred sex do not
establish actual eligibility, use or treatment. Women are a descriptive comparison
whose trends must be assessed, not assumed to be an unaffected control.
The [1952 Korean War legislation](https://department.va.gov/history/wp-content/uploads/sites/10/2026/04/NVAHCA.2023.09.Box1_.History.of_.GI_.Bill_.Comparisons.and_.Summaries.pdf)
and subsequent programs complicate using younger cohorts as unexposed controls.

A subsequent effect analysis should obtain sex-specific birth-cohort denominators
and cohort-specific military-service or benefit-use rates. The
[Bound and Turner study](https://www.nber.org/papers/w7452) provides a starting
point for exposure-based comparisons. This descriptive pipeline does not fetch
those additional data or fit an effect estimator.

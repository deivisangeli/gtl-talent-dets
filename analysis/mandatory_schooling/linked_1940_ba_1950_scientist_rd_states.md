# BA-equivalent completion: state cohort RDs

Run from the repo root, or pass the script's absolute path from another directory:

```powershell
Rscript analysis/mandatory_schooling/estimate_linked_1940_ba_1950_scientist_rd_states.R
Rscript analysis/mandatory_schooling/test_linked_1940_ba_1950_scientist_rd_states.R
```

The runner reuses the main linked IV's saved cells and the original RD law and
event inputs. It keeps all people with valid 1940 schooling and defines
`ba_completed = 1(school_years >= 16)`, including postgraduate schooling. The
historical census records completed grades, not a degree award. The outcome
remains scientist occupation in the linked 1950 census record.

## Designs

- First-law: the same 14 states, six cohorts per side, state intercepts and
  state-specific slopes on each side. `post = 1(k >= 0)` instruments completion.
- Continuation reforms: the same 17 events in 16 states, three cohorts per
  side, event intercepts and event-specific slopes. The all-event and
  introductions-only samples have fuzzy estimates. The four continuation-only
  states remain descriptive: no ratio, SE, p-value, or interval.
- The cutoff is the midpoint `k = -0.5`. Model weights are the triangular
  kernel times cell person counts; inference clusters on birth state with
  `K.adj = FALSE`. Window, cutoff-location, leave-one-state-out, and prelaw
  placebo checks follow the original designs. Fixed-bandwidth Calonico outputs
  remain diagnostics for a discrete score with limited support.

The broad IV's five requirement-bin instruments are not used in these RDs.
BA jumps are saved in probability and percentage-point units. Fuzzy effects
are probabilities and scientist occupations per 100,000 men for a change
from noncompletion to completion. Required-year and scientist reduced-form
coefficients, SEs and p-values must match the original RDs.

The two primary pooled samples invert 2,001 birth-state clustered AR tests
over `[-1,1]`, with crossings refined to `1e-8`. All detected intervals are
retained. Boundary flags indicate search truncation, not proven infinite
endpoints; the grid can miss regions narrower than 0.001 probability units.
The AR p-value at zero must equal the scientist reduced-form p-value.

## Outputs

Results live in Dropbox under `results/mandatory_schooling/`:

- `linked_1940_ba_1950_scientist_rd/`
- `linked_1940_ba_1950_scientist_continuation_rd/`

Both contain coefficient results with SEs, p-values and identification status;
completion shares and first-stage diagnostics; AR grids and intervals;
sample/event audits; event contrasts; fitted models; input checksums; baseline
validation; and R session information. The first directory includes window,
cutoff-location and prelaw placebo outputs. The second includes pooled sample
variants and leave-one-state-out estimates. If BA effects cannot be identified,
the runner records the failure while retaining legal-requirement and scientist
estimates and all state cards.

`MS_BA_RD_RESULTS_ROOT` overrides the destination parent for both result
directories. `MS_BA_RD_PROJECT_DIR` overrides the figure-deck destination.
Source data continue to use `paths.R`/`TALENT_DETS_DATA_DIR` and the original
saved RD result directories. Existing schooling-years outputs are preserved.

The runner creates `project/mandatory_schooling_ba_scientist_rd_states.tex` and
two separately named images in `project/figures/`. It copies the original
state deck's two-page A3 landscape layout, margins, image sizing and notes
placement. Page one retains 14 first-law cards; page two retains 17 reform
cards, including both Ohio events. Only the schooling measure changes to
BA-equivalent completion in percent. Required years and scientist panels,
event labels, colors, free scales, cutoff lines and descriptive person-weighted
fits retain their original definitions. Plot fits do not use the model kernel.

Open the saved `.tex` in the built-in editor. Its two image dependencies may
require the installed TeX compiler if native compilation is unavailable.

## Interpretation

RD validity still requires smooth counterfactual cohorts, relevance, exclusion
and appropriate monotonicity. A BA-specific interpretation additionally
requires law-induced changes at other schooling margins to have no separate
effect on scientist occupation. Replacing schooling years with completion
does not establish that assumption. Linked records are selected, and birth
year inferred from 1940 age has measurement error near cutoffs.

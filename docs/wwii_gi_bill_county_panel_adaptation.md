# Adapting Abramitzky et al.'s WWII/G.I. Bill strategy to the AMWS county panel

**Status:** proposed empirical adaptation, not a replication or an estimated result. This note accompanies the original strategy summary.

## Original strategy and the change in outcome

Abramitzky, Kowalski, Pérez, and Price use year and quarter of birth, separating **1927Q4 and earlier** from **1928Q1 and later**. WWII service probability falls across this boundary, making exposure fuzzy rather than assigning service or benefit receipt deterministically. Their reduced form fits separate linear trends on each side, primarily studying socioeconomic composition among observed elite-college students. The baseline uses men born in **1925–1930**, with standard errors clustered by birth quarter. College fixed effects are a robustness check. See [the paper, §4.2 and Table 1](https://extranet.sioe.org/uploads/sioe2025/price-2025-01-10_1270.pdf) and its [NBER record](https://www.nber.org/papers/w33164).

Here, the observation becomes a **county × birth-month** or **county × birth-quarter** cell, and the outcome is the rate of recorded AMWS scientists born in that cell. Time denotes birth cohorts, not the month of service, college attendance, or scientific work. The current panel pools sexes; it does not reproduce the paper's male sample or student-composition estimand.

## Data and proposed sample

Use the existing Dropbox output `output/mandatory_schooling/county_amws_monthly_gapminder_1915_1950.csv`. Its keys are `GEOID`, `year`, and `month`; the numerator is `n_amws`, and the denominator is `monthly_allocated_births`. The proposed outcome is the existing `amws_per_1000_births`:

$$
Y_{cm}=1000\frac{N_{cm}}{\widehat B_{cm}},\qquad
\widehat B_{cm}=\frac{\widehat B_{cy}}{12},\qquad
\widehat B_{cy}=P_{cy}\frac{B^{US}_y}{P^{US}_y}.
$$

Here \(c\) is county, \(m\) is birth month, and \(y\) is birth year. National births come from Gapminder; national population uses the upstream annual panel's universe. County birth estimates assume a common national annual birth rate, then allocate births equally across months. They contain neither measured monthly seasonality nor county-specific fertility rates. Definitions are documented in the [monthly panel note](../prep/mandatory_schooling/monthly_amws_panel.md) and [builder](../prep/mandatory_schooling/build_amws_county_month.R).

For both specifications, use **January 1925–December 1930**, retaining the same counties with all 72 unique monthly cells, finite positive denominators, and valid nonnegative counts throughout. Keep count zeros; do not replace missing denominators with zero. Report the retained counties and estimated births covered. Use denominator weights, \(w_{cm}=\widehat B_{cm}\), so larger estimated birth cohorts contribute more; this is not an equal-county estimand.

## Monthly and quarterly specifications

Both specifications are proposed analyses. Define running variables in consecutive cohort bins, centered at the last earlier-born bin:

| Specification | Running variable | Earlier-born indicator | Adjacent bins |
|---|---|---|---|
| Monthly | \(x_m=12(y-1927)+(month-12)\) | \(D_m=1\{x_m\leq0\}\) | December 1927 / January 1928 |
| Quarterly | \(x_q=4(y-1927)+(quarter-4)\) | \(D_q=1\{x_q\leq0\}\) | 1927Q4 / 1928Q1 |

Thus the adjacent bins have values 0 and 1. This centering follows the original last-pre-cutoff-bin convention; it does not establish an exact legal eligibility date.

For resolution \(r\in\{m,q\}\), estimate weighted least squares:

$$
Y_{cr}=\alpha_c+\beta_rD_r
+\gamma_rx_rD_r+\delta_rx_r(1-D_r)
+\lambda_{s(r)}+\varepsilon_{cr}.
$$

County fixed effects \(\alpha_c\) account for persistent differences in recorded scientist rates. Seasonal fixed effects \(\lambda_{s(r)}\) denote month of year for monthly estimates and quarter of year for quarterly estimates, imposing a recurring additive seasonal pattern. The two trend terms allow different slopes across the boundary.

**County effects, seasonal controls, birth-denominator weights, and the inference choices below are adaptations proposed here.** They are not attributed to the original baseline. Fully saturated birth-cohort fixed effects would absorb the national cutoff indicator and prevent identification of \(\beta_r\).

Construct quarterly data from the retained monthly sample, with \(quarter=\lceil month/3\rceil\):

$$
N_{cq}=\sum_{m\in q}N_{cm},\qquad
\widehat B_{cq}=\sum_{m\in q}\widehat B_{cm},\qquad
Y_{cq}=1000\frac{N_{cq}}{\widehat B_{cq}},\qquad
w_{cq}=\widehat B_{cq}.
$$

Aggregate counts and denominators, then recalculate rates. This is a denominator-weighted monthly rate average; do not assume an unweighted average is valid for other denominator constructions.

At either resolution, \(\beta_r\) is the fitted **earlier-born minus later-born** rate difference evaluated at \(x_r=0\), conditional on county and seasonal effects. Its units are **recorded scientists per 1,000 estimated births**, not percentage points. It uses multiple cohorts to estimate trends and is not simply an adjacent-month or adjacent-quarter mean difference. With discrete birth bins, the jump depends on the specified extrapolation.

## Identification and inference

Monthly dates increase outcome resolution but do not establish a sharper discontinuity in service probability. This panel has no veteran status, benefit use, or estimated first stage. Its coefficients remain reduced-form cohort comparisons; they cannot be converted into effects per veteran. Pooling sexes mixes groups with different WWII exposure and prevents a specifically male interpretation.

A causal interpretation would require smooth counterfactual scientist rates and stable recording, date assignment, geography, and denominator construction around the boundary. **Any identified exposure effect bundles WWII service with G.I. Bill and other veteran benefits; it cannot isolate the pure education-subsidy effect.** The authors also acknowledge this separation problem in [§§4.1 and 4.3](https://extranet.sioe.org/uploads/sioe2025/price-2025-01-10_1270.pdf).

Propose two-way standard-error clustering by **county and birth cohort**: county × birth-month cohort for the monthly specification and county × birth-quarter cohort for the quarterly specification. These are two separate clustering dimensions, not a single cluster for each cell. They allow within-county dependence and common same-cohort shocks, but do not automatically handle serially correlated national cohort shocks.

The baseline contains only **72 monthly or 24 quarterly cohorts**. Thousands of county cells do not supply thousands of independent treatment cohorts. Treat conventional intervals cautiously, assess national-cohort serial dependence, and consider inference allowing approximation bias in a discrete running variable. Clustering by the running variable does not itself correct trend misspecification; see [Kolesár and Rothe (2018)](https://www.aeaweb.org/articles?id=10.1257/aer.20160945).

## Robustness and measurement checks

- **Resolution and windows:** report both resolutions, expand to 1923–1930, compare with 1926–1929, and exclude all 1928 births. Rebuild each window's balanced county sample; also compare windows on a fixed common county set. Excluding 1928 creates a gap and increases reliance on extrapolation.
- **Denominators and weights:** compare Gapminder with NHGIS recorded occurrence births on identical usable counties, and compare birth-weighted with equal-county estimates. NHGIS brings registration coverage and geography-matching limitations. Inspect annual denominator changes: dividing annual totals by twelve can mechanically create December/January rate changes.
- **Completeness and seasonality:** examine `birth_month_completeness`, `n_unassigned_month`, and assigned counts by source/year around 1927–1928; do not redistribute unassigned records. Compare December/January changes at other years and estimates with and without seasonal effects. Placebo boundaries are diagnostics, not automatically untreated cohorts.
- **AMWS and geography:** inspect sparse/zero cells, source-specific results, county coverage and geography flags. Counts preserve directory selection and career censoring; 1986 is not deduplicated against early editions. Zero counts mean no assigned records, not proven absence of scientists.
- **Checks needing more data:** male-specific estimates need sex-resolved counts and suitable denominators; a first stage needs cohort-specific service data. The paper's college fixed effects and restriction to starting college before 1952 require linked college and entry-date information absent from this panel. Younger cohorts may receive Korean War benefits. The original robustness checks appear in [§4.3 and Figures B18–B19](https://extranet.sioe.org/uploads/sioe2025/price-2025-01-10_1270.pdf).

These checks support assessment of the proposed design; this note reports no regression estimates or causal findings.

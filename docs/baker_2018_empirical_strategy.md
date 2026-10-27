# Baker et al. (2018): empirical strategy

**Paper:** Richard B. Baker, John Blanchette, and Katherine Eriksson, *Long-run
Impacts of Agricultural Shocks on Educational Attainment: Evidence from the
Boll Weevil*, NBER Working Paper 25400, December 2018.
[Local source PDF](<C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets/Lit/Schooling/baker_2018.pdf>).
References below use **printed page numbers**; PDF viewer page numbers are one
greater. This note describes the 2018 working paper, not a later published version.

## 1. Question, shock, and interpretation

The paper asks whether lower demand for agricultural child labor increases
schooling. The boll weevil spread across the Southern United States during
1892-1922, damaging cotton and encouraging substitution toward less
child-labor-intensive crops. The proposed mechanism is a reduction in the
opportunity cost of school. The authors combine county-year evidence on
enrollment and attendance with individual evidence on completed schooling
(Sections 1-2 and 4, pp. 2-6 and 9-17).

These are reduced-form effects of infestation, not an instrumental-variables
estimate of child labor's effect on education. Income losses can offset the
opportunity-cost channel; changed occupational aspirations and nutrition/health
are additional possible mechanisms. The design does not separately identify
their contributions (Section 6, pp. 19-21).

## 2. Treatment dates and sample restrictions

The authors digitize Hunter and Coad's (1923) USDA map, classifying counties
annually as free, partially infested, or fully infested during 1892-1922. They
define first arrival as the first partial/full year, complete infestation as
the first full year with no subsequent partial/free observation, and the
infestation year as the rounded-up midpoint of those dates (Section 3.1,
pp. 6-7).

The paper reports 844 counties, on 1920 boundaries, fully infested by 1922. It
then excludes 120 experiencing full retreat and 38 taking over four years from
first arrival to complete infestation. The arithmetic implies 686 counties;
that is not a reported count of counties represented in the final individual
regressions. The census extraction covers men born in 13 Southern states;
Missouri, Kansas, and New Mexico are excluded from the broader Cotton Belt
state list (p. 2, footnote 1; p. 7).

## 3. County-year enrollment and attendance design

This analysis uses 1900-1934 education records from Alabama, Arkansas, Georgia,
North Carolina, South Carolina, and Tennessee. Education data for 1910-1934
come from Carruthers and Wanamaker; earlier observations come from state
education reports. The sample contains **347 geographically consistent
counties**, invaded during 1906-1922, after exclusions for boundary changes,
full retreats, and little/no Black population (pp. 9-10, footnotes 10-11).

Separately for White and Black students, the specification can be written:

$$
\log(Y_{ct})=\beta D_{ct}+\gamma\log(Teachers_{ct})
+\alpha_c+\lambda_t+\delta_c t+\varepsilon_{ct}.
$$

Here, $Y$ is enrollment or average daily attendance, and $D$ indicates boll
weevil presence. County and year fixed effects enter all specifications;
teacher controls and county-specific linear trends enter progressively. The
equation above includes both additions, following the text and table notes;
the displayed Equation (1) does not itself print the trend term. Errors are
clustered by county. Additional attendance specifications condition on log
enrollment (Equation 1, p. 10; Tables 2-3, pp. 29-30).

The event study replaces $D$ with event-time indicators relative to the first
infestation year. It omits **event time zero**, bins times at or below -6 and
at or above +12, and includes teachers, fixed effects, and county trends.
Coefficients therefore compare outcomes with the arrival year, rather than
the preceding year (Equation 2 and Figure 1, pp. 10-11 and 25).

With teachers and trends, enrollment coefficients are 0.040 and 0.060,
approximately 4.1% and 6.2% for White and Black students. Attendance
coefficients are 0.041 and 0.053. Conditional on enrollment, attendance effects
are small and insignificant. These are changes in counts, not enrollment
rates: absent school-age population controls, migration cannot be excluded as
an explanation for part of the increase (Tables 2-3; pp. 11-12).

## 4. Linked-census attainment design

The authors link men observed in the 1940 full-count census to themselves at
ages 3-18 in 1900, 1910, or 1920, retaining the earliest childhood observation
when multiple censuses match. Childhood location determines exposure; 1940
supplies completed schooling. The childhood age restriction corresponds to
ages 23-58 in 1940 (Section 3.2, pp. 7-9).

Matching uses first/last names, birth year, race, and birth state. Names are
cleaned and standardized using NYSIIS. Matches are sought at exact birth year,
then within one and two years; only unique matches surviving both forward and
backward procedures are retained. The baseline additionally requires
uniqueness within a three-year age band. Match rates are 27.51% for White men
and 18.63% for Black men; matched individuals have more advantaged observable
backgrounds than the population (pp. 8-9; Table 1, p. 28).

Let $a$ denote birth year, $c$ childhood county, and $G_c$ the midpoint
infestation year. Define exposure age $A_{ic}=G_c-a$. Equation (3), expressed
using explicit inclusive bins, is:

$$
y_{ica}=\sum_{b\in\mathcal B}\beta_b\mathbf{1}\{A_{ic}\in b\}
+X_i'\gamma+\alpha_c+\lambda_a+\varepsilon_{ica},
\qquad
\mathcal B=\{[4,6],[7,9],[10,12],[13,15],[16,18]\}.
$$

The omitted exposure ages are **19-30**. Outcomes are years of schooling and an
indicator for completing at least eighth grade. Regressions are separate by
race and include childhood-county and birth-year fixed effects. Childhood
controls are household head occupational score, homeownership, literacy, and
indicators for urban location and farm residence. Errors are clustered by
childhood county. County-specific trends are not part of this baseline
attainment specification (Equation 3, p. 13; Table 4, p. 31).

## 5. Identification and the pretrend diagnostic

Identification combines differences across birth cohorts within counties with
differences in infestation timing across counties, conditional on fixed effects
and background controls. County effects absorb persistent geographic
differences; birth-year effects absorb common cohort differences. A causal
interpretation requires that remaining county-specific cohort changes do not
systematically track exposure timing, and that ages 19-30 do not change their
schooling in response (pp. 13-15).

Older cohorts are a plausible schooling comparison because most had finished
education, particularly eighth grade. Nevertheless, some still attended
school. Positive effects on their attainment would attenuate the younger
cohorts' estimated gains. They are not a valid unaffected comparison for labor
market outcomes, which infestation could change at adult ages (pp. 14-16,
including footnote 17).

With everyone eventually exposed, unrestricted exposure-age effects cannot
be fully disentangled from county and birth-cohort effects. Following the
Borusyak-Jaravel normalization discussed in the paper, the diagnostic sets
coefficients for **ages 19-21 and 28-30 to zero**, estimates ages 22-24 and
25-27, and jointly tests those two older-cohort coefficients. The reported
F-statistics do not reject at 10%. This tests nonlinear pretrends under that
normalization, not every possible departure from the identifying assumptions
(Equation 4, pp. 15-16; Table 5, p. 32).

## 6. Principal attainment estimates

Table 4 (p. 31) reports the following effects relative to exposure at ages
19-30:

| Age at exposure | White: schooling years | Black: schooling years | White: eighth-grade completion, pp | Black: eighth-grade completion, pp |
| --- | ---: | ---: | ---: | ---: |
| 4-6 | 0.2609 | 0.3581 | 1.76 | 3.25 |
| 7-9 | 0.2314 | 0.2423 | 2.55 | 2.47 |
| 10-12 | 0.1459 | 0.1512 | 1.72 | 1.26 |
| 13-15 | 0.1017 | 0.1407 | 1.40 | 1.15 |
| 16-18 | 0.0423 | 0.0612 | 0.54 | 0.35 |

The samples contain **432,235 White men and 170,923 Black men**. Completion
effects above are percentage points, converted from probability coefficients.
Schooling gains decline with exposure age; the oldest-bin White schooling
estimate is significant only at 10%, and the oldest-bin Black completion
estimate is insignificant. Race differences are not statistically established
(pp. 13-14; Table 4).

## 7. Heterogeneity and robustness

Cotton intensity is 1889 cotton acres divided by **improved farm acreage**,
not total farm area. Exposure bins are interacted with a fourth-order
polynomial in intensity. Estimated gains initially increase and then decline
as cotton dependence rises. The authors interpret this pattern as consistent
with crop substitution and offsetting income losses, rather than direct
identification of either channel (pp. 16-17; Figures 2-3, pp. 26-27).

Robustness checks add Rosenwald exposure, its rural interaction, and compulsory
schooling exposure; vary matching uniqueness, birth-year tolerances, and name
standardization; and use inverse-probability weights based on childhood or
adult characteristics (Tables 6-8, pp. 33-35). Appendix checks vary retreat and
timing restrictions, require infestation before 1915 or stable boundaries,
require at least 100 cotton acres, alter the older comparison category, and
top-code schooling at 12 (Tables A.1-A.4, pp. 37-40). Broad stability supports
the findings, but observable reweighting cannot eliminate selection on
unobservables, linkage error, or migration concerns.

## 8. Connection to our treatment dataset

Our county files recover all three timing measures, retreat flags, baseline
acreage, and cotton intensity. Annual indicators are derived from these
dates; they do not recover observed annual infestation histories. Replicating
the attainment design still requires linked individuals, birth years,
childhood residence/controls, and 1940 outcomes. The enrollment design needs
race-specific education records. Talent outcomes would define a different
estimand and require their own justification for an unaffected comparison.

**Local deposit audit, separate from paper results:** our 1920 table contains
1,083 counties, including 843 with complete infestation dates. Timing rules
retain 685; excluding Missouri leaves 681, of which 677 have cotton intensity.
These are not the paper's linked-person estimation sample. The discrepancy
from its 844 starting counties remains unresolved. Verified nominal GEOIDs
also do not establish unchanged historical boundaries. See the
[dataset README](C:/Users/megaj/repos/gtl_talent_dets/prep/boll_weevil/README.md)
and its source/count audits for details.

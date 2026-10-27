# Pooling Multiple Synthetic-Control Case Studies: A Generic Analysis Plan

## Purpose

This document provides a reusable workflow for applications with **multiple treated events or units**, where a separate synthetic control is constructed for each treated case and the resulting estimates are subsequently pooled.

The framework is motivated primarily by Dube and Zipperer (2015), *Pooling Multiple Case Studies Using Synthetic Controls*, and by the placebo-inference tradition of Abadie, Diamond, and Hainmueller (2010). It is intentionally written in generic terms so that it can be adapted to different treatments, outcomes, geographic units, and time periods.

The central principle is to keep four tasks conceptually separate:

1. **Constructing a credible counterfactual for each treated event.**
2. **Assessing counterfactual quality using only pre-treatment information.**
3. **Estimating and standardizing event-specific treatment effects.**
4. **Pooling evidence and conducting inference using placebo assignments that reproduce the full estimation procedure.**

A key rule throughout the analysis is:

> **Post-treatment outcomes should never be used to determine which treated events are retained for the main pooled analysis.**

---

## 1. Data Structure

Suppose there are \(E\) treated events indexed by \(e=1,\ldots,E\).

For each event \(e\):

- \(i_e\) denotes the treated unit;
- \(T_e\) denotes the treatment date;
- \(\mathcal{D}_e\) denotes the eligible donor pool;
- \(Y_{it}\) denotes the outcome for unit \(i\) in period \(t\);
- \(T_{0e}\) denotes the number of pre-treatment periods;
- \(H_e\) denotes the post-treatment evaluation horizon.

Treatment dates and donor pools may differ across events.

A unit may be eligible as a donor for one event but not another. For example, a unit that is untreated at an early event date may cease to be eligible after receiving treatment itself.

---

## 2. Define Donor Eligibility Before Estimation

For each event, construct an eligible donor pool using criteria motivated by the research design rather than by post-treatment fit or estimated effects.

Possible exclusion criteria include:

- treatment before the focal event;
- treatment during the post-treatment evaluation window;
- likely exposure to treatment spillovers;
- incompatible geographic definitions;
- missing or unreliable outcome data;
- institutional or structural differences that make a unit an implausible counterfactual;
- simultaneous shocks that clearly invalidate the unit as a control.

Denote the resulting donor set by

\[
\mathcal{D}_e.
\]

### Recommended practice

Use a **relatively broad pool of causally valid donors** as the baseline. Avoid selecting a very small set simply because those units happen to minimize pre-treatment outcome distance.

If a restricted donor pool is substantively motivated, report it as either:

- a pre-specified main design choice; or
- a robustness specification.

The same eligibility rules must be applied when constructing placebo synthetic controls.

---

## 3. Estimate a Synthetic Control for Each Event

For each treated event \(e\), construct a synthetic counterfactual

\[
\widehat{Y}_{et}(0)
=
\sum_{j\in\mathcal{D}_e} w_{ej}Y_{jt},
\]

where the weights \(w_{ej}\) are estimated using only pre-treatment information.

Depending on the estimator, the weights may be:

- conventional non-negative synthetic-control weights;
- augmented synthetic-control weights;
- synthetic-DiD unit weights;
- partially pooled weights;
- another pre-specified synthetic-control estimator.

Define the estimated treatment-effect path as

\[
\widehat{\tau}_{et}
=
Y_{i_e t}
-
\widehat{Y}_{et}(0).
\]

Equivalently, define the treated-minus-synthetic gap:

\[
g_{et}
=
Y_{i_e t}
-
\widehat{Y}_{et}(0).
\]

All subsequent fit diagnostics and treatment-effect statistics can be constructed from \(g_{et}\).

---

## 4. Measure Pre-Treatment Fit

For every treated event, calculate the pre-treatment root mean squared prediction error:

\[
RMSPE^{pre}_e
=
\sqrt{
\frac{1}{T_{0e}}
\sum_{t<T_e}
g_{et}^{2}
}.
\]

This statistic measures the quality of the synthetic counterfactual **before treatment**.

A small value implies that the synthetic control closely reproduces the treated unit during the pre-treatment period.

### Scale-adjusted alternatives

If outcome levels vary greatly across events, raw RMSPE may mechanically favor low-outcome units. Consider reporting an additional normalized fit measure, such as

\[
NRMSPE^{pre}_e
=
\frac{RMSPE^{pre}_e}
{\overline{|Y|}^{pre}_e},
\]

or another scale normalization appropriate to the application.

The normalization rule must be defined before inspecting treatment effects.

---

## 5. Use Pre-Treatment Fit to Screen or Trim Events

The credibility of a pooled synthetic-control estimate depends on the quality of the underlying event-specific counterfactuals.

Therefore, pre-treatment fit may be used to identify events for which synthetic-control estimation is sufficiently credible.

### Critical rule

Event inclusion must depend **only on pre-treatment information**.

Do **not** select events based on:

\[
\frac{\widehat{\tau}_e}
{RMSPE^{pre}_e},
\]

post-treatment RMSPE, statistical significance, or the sign or magnitude of the estimated treatment effect.

Doing so would condition sample selection on post-treatment outcomes and mechanically bias the pooled estimate.

### Option A: Pre-specified fit threshold

Define a threshold \(c\) and retain events satisfying

\[
RMSPE^{pre}_e < c.
\]

If outcomes have different scales, use the corresponding normalized measure.

### Option B: Progressive trimming

Rank treated events from best to worst according to pre-treatment fit and report pooled estimates for:

1. all treated events;
2. excluding the single worst-fitting event;
3. excluding the two worst-fitting events;
4. continuing progressively when informative.

This approach follows the logic emphasized by Dube and Zipperer and avoids placing excessive weight on a single arbitrary cutoff.

### Recommended presentation

Report a table containing, for every treated event:

| Event | Treated unit | Treatment date | Donors | Pre-RMSPE | Normalized pre-RMSPE | Included in baseline? |
|---|---|---:|---:|---:|---:|---|
| 1 | Unit A | \(T_1\) | \(J_1\) | ... | ... | Yes/No |
| 2 | Unit B | \(T_2\) | \(J_2\) | ... | ... | Yes/No |

Also display treated and synthetic trajectories before treatment.

---

## 6. Define Event-Specific Effects

The estimand should be fixed before placebo inference.

### Point-specific effect

For event-time horizon \(h\),

\[
\widehat{\tau}_{e,h}
=
Y_{i_e,T_e+h}
-
\widehat{Y}_{e,T_e+h}(0).
\]

### Average post-treatment effect

For a post-treatment horizon \(H\),

\[
\widehat{\tau}_{e}^{(H)}
=
\frac{1}{H}
\sum_{h=0}^{H-1}
\widehat{\tau}_{e,h}.
\]

When treatment effects may evolve slowly, report the full event-time path rather than relying exclusively on one average post-treatment effect.

---

## 7. Standardize Event-Specific Effects by Pre-Treatment Fit

A useful event-specific statistic is

\[
S_e
=
\frac{\widehat{\tau}_{e}^{(H)}}
{RMSPE^{pre}_e}.
\]

This measures the post-treatment effect relative to the amount of prediction error observed before treatment.

It can be interpreted as a **standardized synthetic-control effect**, or informally as a pseudo-\(t\)-type statistic.

It is **not** a conventional \(t\)-statistic and should not be evaluated against a Student-\(t\) distribution. Its reference distribution comes from placebo assignments.

### Why standardize?

Consider two events with identical estimated effects:

\[
\widehat{\tau}_A=2,\qquad
\widehat{\tau}_B=2,
\]

but

\[
RMSPE^{pre}_A=0.1,\qquad
RMSPE^{pre}_B=1.
\]

Then

\[
S_A=20,\qquad S_B=2.
\]

The first effect represents a much larger post-treatment departure relative to the synthetic control's pre-treatment prediction error.

---

## 8. Also Report the Post/Pre RMSPE Ratio

A complementary statistic from the synthetic-control literature is

\[
R_e
=
\frac{RMSPE^{post}_e}
{RMSPE^{pre}_e},
\]

where

\[
RMSPE^{post}_e
=
\sqrt{
\frac{1}{H}
\sum_{h=0}^{H-1}
g_{e,T_e+h}^{2}
}.
\]

The two statistics answer different questions:

| Statistic | Interpretation |
|---|---|
| \(RMSPE^{pre}\) | Quality of the pre-treatment counterfactual |
| \(\widehat{\tau}\) | Magnitude and direction of the treatment effect |
| \(\widehat{\tau}/RMSPE^{pre}\) | Signed effect relative to pre-treatment prediction error |
| \(RMSPE^{post}/RMSPE^{pre}\) | Extent to which prediction errors increase after treatment |

The post/pre RMSPE ratio is useful for detecting any large post-treatment deviation but loses the sign of the effect because it is based on squared errors.

For applications with a directional substantive hypothesis, report both the signed standardized effect and the post/pre RMSPE ratio.

---

## 9. Event-Level Placebo Inference

For each treated event \(e\), conduct an in-space placebo exercise.

For every eligible donor \(j\in\mathcal{D}_e\):

1. Pretend that \(j\) received treatment at \(T_e\).
2. Remove \(j\) from its own donor pool.
3. Re-estimate the synthetic control using the same model-selection and tuning procedure as in the treated case.
4. Compute the placebo pre-treatment RMSPE.
5. Compute the placebo post-treatment effect.
6. Calculate the same statistic used for the treated event.

For placebo \(j\),

\[
S^P_{ej}
=
\frac{
\widehat{\tau}^{P}_{ej}
}{
RMSPE^{pre,P}_{ej}
}.
\]

The actual statistic \(S_e\) is then compared with the placebo distribution

\[
\{
S^P_{e1},
S^P_{e2},
\ldots,
S^P_{eJ_e}
\}.
\]

### Placebo p-value

For a two-sided test,

\[
p_e
=
\frac{
1+
\sum_{j=1}^{J_e}
\mathbf{1}
\left(
|S^P_{ej}|
\geq
|S_e|
\right)
}{
J_e+1
}.
\]

For a directional hypothesis, use the corresponding one-sided rank.

The finite number of placebo units limits p-value resolution. For example, with 30 eligible placebo units,

\[
p_{min}=\frac{1}{31}\approx0.032.
\]

---

## 10. Exclude Extremely Poor-Fitting Placebos as a Robustness Check

Some placebo units may have synthetic controls with dramatically worse pre-treatment fit than the actual treated unit. Their post-treatment gaps can be uninformative because the placebo synthetic control was already a poor predictor before treatment.

A common robustness procedure is to retain placebo units satisfying

\[
\frac{
MSPE^{pre,P}_{ej}
}{
MSPE^{pre}_e
}
<c,
\]

for alternative values such as

\[
c\in\{2,5,10,20\}.
\]

This procedure concerns the **quality of the placebo distribution** and is distinct from the screening rule used to select real treated events for pooling.

Report the full placebo distribution as well as reasonable fit-restricted placebo distributions.

---

## 11. Convert Event-Level Placebo Results into Percentile Ranks

Following the pooling logic of Dube and Zipperer, calculate the percentile rank of the treated statistic within the corresponding event-specific placebo distribution.

For a positive directional effect, define schematically

\[
r_e
=
\frac{
1+
\sum_j
\mathbf{1}
(S^P_{ej}\leq S_e)
}{
J_e+1
}.
\]

Thus:

- \(r_e\) close to 1 means the treated effect is unusually positive relative to its placebos;
- \(r_e\) close to 0 means it is unusually negative;
- \(r_e\) close to 0.5 means it is typical of placebo outcomes.

Percentile ranks are useful because each event may have:

- a different outcome scale;
- a different number of donors;
- a different treatment date;
- different pre-treatment prediction error.

Ranks put the event-specific evidence on a common scale.

---

## 12. Pool Effect Magnitudes Separately from Statistical Evidence

It is useful to distinguish two pooled quantities.

### 12.1 Pooled economic magnitude

For \(K\) retained events,

\[
\widehat{ATT}^{(H)}
=
\frac{1}{K}
\sum_{e=1}^{K}
\widehat{\tau}_{e}^{(H)}.
\]

For dynamic treatment effects,

\[
\widehat{ATT}_h
=
\frac{1}{K_h}
\sum_{e\in\mathcal{E}_h}
\widehat{\tau}_{e,h},
\]

where \(\mathcal{E}_h\) contains events observed at horizon \(h\).

Alternative weighting schemes may be used when substantively justified, but equal weighting is transparent and prevents large units from mechanically dominating the pooled case-study estimate.

### 12.2 Pooled statistical evidence

Define the mean percentile rank

\[
\overline r
=
\frac{1}{K}
\sum_{e=1}^{K}r_e.
\]

This follows the central pooling idea proposed by Dube and Zipperer.

Alternatively, a pooled standardized-effect statistic can be reported:

\[
\overline S
=
\frac{1}{K}
\sum_{e=1}^{K}S_e.
\]

The preferred pooled inference statistic should be specified before inspecting the final pooled result.

---

## 13. Donor Sharing Across Events

The same donor may receive positive weight in several event-specific synthetic controls.

For example,

\[
SC_A
=
0.5A+0.3B+0.2C,
\]

while

\[
SC_B
=
0.4A+0.4D+0.2B.
\]

This does **not** by itself invalidate either synthetic control.

However, the resulting treatment-effect estimates are not independent:

\[
Cov(\widehat{\tau}_A,\widehat{\tau}_B)\neq0.
\]

Therefore, standard inference procedures that assume independent event-level estimates are inappropriate.

### Do not mechanically prohibit donor sharing

Forcing donor pools to be disjoint can substantially worsen counterfactual quality. The baseline design should generally allow a valid donor to contribute to multiple synthetic controls.

Instead, account for donor sharing in pooled inference.

---

## 14. Diagnose Donor Overlap

Store the complete event-by-donor weight matrix

\[
W=
\begin{pmatrix}
w_{11} & \cdots & w_{1J}\\
\vdots & & \vdots\\
w_{E1} & \cdots & w_{EJ}
\end{pmatrix}.
\]

Useful diagnostics include:

### Number of synthetic controls using donor \(j\)

\[
N_j
=
\sum_e
\mathbf{1}(w_{ej}>0).
\]

### Cumulative donor weight

\[
C_j
=
\sum_e w_{ej}.
\]

### Pairwise weighted overlap

For two events \(e\) and \(e'\),

\[
O_{ee'}
=
\sum_j
\min(w_{ej},w_{e'j}).
\]

This measure ranges from zero for completely disjoint donor weights to one for identical synthetic-control weights.

Report:

- the most frequently used donors;
- donors with the largest cumulative weights;
- the event-by-event overlap matrix.

These are diagnostics, not automatic exclusion rules.

---

## 15. Pooled Randomization Inference

The preferred inference procedure should reproduce the **entire estimation pipeline** under placebo treatment assignments.

For randomization draw \(b\):

1. Generate a joint placebo assignment for the set of events.
2. Preserve the original treatment dates or treatment-timing structure.
3. Apply the same donor eligibility rules.
4. Re-estimate every synthetic control.
5. Re-run all model selection and tuning.
6. Recalculate pre-treatment RMSPE.
7. Apply the same pre-fit event-screening rule.
8. Calculate placebo event-specific treatment effects.
9. Calculate event-specific standardized effects or percentile ranks.
10. Calculate the same pooled statistic used for the observed data.

This yields a placebo distribution

\[
Q^{(1)},Q^{(2)},\ldots,Q^{(B)},
\]

where \(Q\) may be the mean percentile rank, mean standardized effect, or another pre-specified pooled statistic.

The observed pooled statistic \(Q^{obs}\) is compared against this empirical null distribution.

---

## 16. Preserve Donor Dependence in Joint Placebo Assignments

When donors overlap across event-specific synthetic controls, placebo assignments should preserve as much of the original dependence structure as possible.

A useful approach is to **permute unit identities jointly rather than assigning independent placebo units event by event**.

The objective is to preserve:

- treatment timing;
- donor eligibility;
- overlap between donor pools;
- repeated use of the same geographic units across events;
- other structural restrictions of the original research design.

This follows the motivation of Dube and Zipperer: pooled inference should account for dependence created when multiple case studies draw controls from overlapping donor pools.

A naive procedure that independently samples one placebo for each event may incorrectly treat event-level statistics as independent.

---

## 17. Event Selection Must Be Repeated Inside the Placebo Procedure

Suppose the main analysis retains events satisfying

\[
RMSPE^{pre}_e<c.
\]

The randomization procedure must reproduce this step.

For every placebo assignment:

1. estimate placebo synthetic controls;
2. compute placebo pre-treatment RMSPE;
3. apply the same threshold \(c\);
4. pool only the placebo events that satisfy the corresponding fit rule.

More generally, if the real analysis uses progressive trimming, cross-validation, regularization selection, or any other pre-treatment model-selection step, it should be reproduced within the placebo procedure whenever computationally feasible.

The target of inference is the **full analysis algorithm**, not merely the final regression or effect calculation.

---

## 18. Recommended Robustness Checks

### A. Progressive pre-fit trimming

Report results using:

- all events;
- excluding the worst pre-fit event;
- excluding the two worst;
- increasingly stringent pre-fit restrictions.

The treatment effect should not appear only after selectively removing most treated events.

### B. Alternative post-treatment horizons

Re-estimate effects for alternative windows, for example:

\[
H_1,\quad H_2,\quad H_3.
\]

These windows should be substantively motivated.

### C. Alternative donor pools

Compare:

- broad causally valid donor pool;
- more substantively comparable donor pool;
- alternative spillover exclusions.

### D. Leave-one-event-out pooling

For every retained event \(e\), compute

\[
\widehat{ATT}_{-e}.
\]

This identifies whether the pooled result is driven by a single treated case.

### E. Leave-shared-donor-out analysis

Identify donors that receive large weights across many events and re-estimate the entire analysis after excluding each influential donor.

This tests whether the pooled estimate is effectively driven by one repeatedly used comparison unit.

### F. Disjoint donor pools

As a deliberately conservative robustness check, impose mutually exclusive donor pools across treated events.

This should generally **not** be the baseline specification because it may sacrifice counterfactual quality.

### G. Alternative synthetic-control estimators

When appropriate, compare:

- conventional SCM;
- Augmented SCM;
- Synthetic DiD;
- partially pooled SCM;
- another pre-specified estimator.

Agreement across estimators can help show that the pooled result is not driven by a particular weighting algorithm.

### H. Placebo-in-time tests

Assign treatment artificially to pre-treatment dates and check whether comparable effects appear before the true intervention.

### I. Predictor/model-selection robustness

If predictor sets or regularization parameters are selected by cross-validation, report:

- the selection criterion;
- the candidate specifications;
- whether results change materially under reasonable alternatives.

---

## 19. Suggested Main Results

A transparent empirical presentation could contain the following.

### Figure 1: Individual treated vs. synthetic trajectories

One panel per treated event.

### Figure 2: Individual treatment-effect gaps

Plot

\[
g_{et}
\]

in event time.

### Figure 3: Pooled event-time effects

Plot

\[
\widehat{ATT}_h.
\]

### Figure 4: Event-specific placebo distributions

Show the observed standardized statistic or post/pre RMSPE ratio relative to placebo values.

### Figure 5: Progressive trimming

Plot the pooled effect against increasingly strict pre-fit restrictions.

### Figure 6: Pooled randomization distribution

Show the observed mean percentile rank or other pooled statistic relative to its joint placebo distribution.

### Table 1: Event characteristics and donor-pool rules

### Table 2: Synthetic-control fit and weights

### Table 3: Event-level treatment effects and percentile ranks

Suggested columns:

| Event | Pre-RMSPE | Effect | Effect / Pre-RMSPE | Post/Pre RMSPE | Percentile rank |
|---|---:|---:|---:|---:|---:|

### Table 4: Pooled estimates and robustness

Include:

- baseline pooled effect;
- progressively trimmed samples;
- leave-one-event-out estimates;
- alternative donor pools;
- alternative estimators;
- influential-donor exclusions.

---

## 20. Pre-Analysis Decisions

Before estimating final effects, document the following decisions.

### Treatment definition

- What constitutes treatment?
- What is the treatment date?
- How are repeated treatments handled?

### Outcome definition

- Level, rate, logarithm, or standardized measure?
- Annual observations or multi-year bins?

### Evaluation horizon

- Immediate effect?
- Short-run average?
- Long-run average?
- Dynamic event-study path?

### Donor eligibility

- Prior treatment exclusions?
- Future treatment exclusions?
- Geographic/spillover restrictions?
- Institutional restrictions?

### Counterfactual estimator

- Conventional SCM?
- Augmented SCM?
- Synthetic DiD?
- Another estimator?

### Event-quality rule

- Raw or normalized pre-RMSPE?
- Fixed threshold?
- Progressive trimming?

### Main event-level test statistic

For example,

\[
S_e
=
\frac{\widehat{\tau}_e}{RMSPE^{pre}_e}.
\]

### Main pooled statistic

For example,

\[
\overline r
=
\frac{1}{K}\sum_e r_e.
\]

### Placebo procedure

- In-space placebo eligibility;
- treatment timing;
- minimum placebo fit;
- number of joint randomizations;
- one-sided or two-sided inference.

Pre-specifying these choices reduces researcher degrees of freedom.

---

## 21. Algorithmic Summary

The recommended pipeline can be summarized as:

```text
1. Define treated events and treatment dates
                         |
2. Define causally valid donor pool for each event
                         |
3. Estimate one synthetic control per treated event
                         |
4. Compute pre-treatment RMSPE
                         |
5. Apply event-quality screen using PRE-treatment fit only
                         |
6. Estimate event-specific post-treatment effects
                         |
7. Standardize effects by pre-treatment RMSPE
                         |
8. Re-estimate the model for placebo treated units
                         |
9. Convert observed effects into placebo percentile ranks
                         |
10. Pool economic magnitudes across retained events
                         |
11. Pool statistical evidence using mean percentile rank
                         |
12. Run JOINT randomization inference
    reproducing donor overlap + selection + estimation
                         |
13. Conduct progressive trimming and influence checks
```

---

## 22. Interpretation

The pooled analysis answers a different question from any single synthetic-control case study.

An individual event estimate,

\[
\widehat{\tau}_e,
\]

asks:

> What was the estimated effect of treatment for treated case \(e\) relative to its synthetic counterfactual?

The pooled effect,

\[
\widehat{ATT},
\]

asks:

> What is the average effect across the set of treated events for which credible synthetic counterfactuals can be constructed?

The mean percentile rank asks:

> Across treated events, are the observed effects systematically more extreme than effects generated by comparable placebo assignments?

These quantities should not be conflated.

In particular, the sample underlying the pooled estimate may be a **fit-selected subset of treated events**. If some treated events are excluded because no credible synthetic control can be constructed, the target estimand should be described accordingly rather than interpreted automatically as the effect for all treated units.

---

## 23. Important Pitfalls

### Do not select events using post-treatment effects

Incorrect:

\[
\frac{\widehat{\tau}_e}
{RMSPE^{pre}_e}
\rightarrow
\text{select events}
\rightarrow
\text{pool}.
\]

Correct:

\[
RMSPE^{pre}_e
\rightarrow
\text{select events}
\]

followed by

\[
\frac{\widehat{\tau}_e}
{RMSPE^{pre}_e}
\rightarrow
\text{test statistic}.
\]

### Do not treat synthetic controls as independent observed units

Synthetic controls are estimated combinations of donors. When the same donors enter several synthetic controls, pooled estimates are dependent.

### Do not combine event-level p-values under an independence assumption when donor pools overlap

Use a joint placebo/randomization procedure instead.

### Do not use a conventional t distribution for standardized synthetic-control effects

Inference should come from the empirical placebo distribution.

### Do not change donor-pool or fit thresholds after inspecting post-treatment results

If alternative choices are investigated, report them transparently as robustness analyses.

### Do not discard poor placebo fits only for the treated cases

Any placebo-fit restriction must be systematic and explicitly reported.

---

## 24. Recommended Baseline Specification

For a generic application with a small number of treated events, a defensible baseline is:

1. Construct one synthetic control per event using all causally valid donors.
2. Evaluate fit using pre-treatment RMSPE.
3. Report the full treated-event sample.
4. Define a transparent pre-fit trimming rule and show progressive trimming.
5. Estimate signed event-specific treatment effects.
6. Standardize each effect by pre-treatment RMSPE.
7. Construct in-space placebo distributions.
8. Convert each observed statistic into a placebo percentile rank.
9. Report the equal-weighted average treatment effect for economic magnitude.
10. Use the mean percentile rank for pooled statistical evidence.
11. Obtain pooled p-values from a joint randomization procedure that reproduces:
    - treatment timing;
    - donor eligibility;
    - donor sharing;
    - model selection;
    - pre-fit screening;
    - the final pooling rule.
12. Report leave-one-event-out and influential-donor robustness checks.

---

## 25. References

Abadie, A., Diamond, A., & Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. *Journal of the American Statistical Association*, 105(490), 493–505.

Dube, A., & Zipperer, B. (2015). Pooling Multiple Case Studies Using Synthetic Controls: An Application to Minimum Wage Policies. *IZA Discussion Paper No. 8944*.

### Useful extensions

Ben-Michael, E., Feller, A., & Rothstein, J. (2021). The Augmented Synthetic Control Method. *Journal of the American Statistical Association*.

Ben-Michael, E., Feller, A., & Rothstein, J. (2022). Synthetic Controls with Staggered Adoption. *Journal of the Royal Statistical Society: Series B*.

Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S. (2021). Synthetic Difference-in-Differences. *American Economic Review*, 111(12), 4088–4118.

---

## 26. Reusable Placeholders

Before applying this document to a specific project, replace or explicitly define:

```text
TREATED EVENTS:
TREATMENT DATES:
OUTCOME:
UNIT OF OBSERVATION:
PRE-TREATMENT WINDOW:
POST-TREATMENT WINDOW:
DONOR ELIGIBILITY RULE:
SPILLOVER EXCLUSION RULE:
SYNTHETIC-CONTROL ESTIMATOR:
PREDICTOR SET:
MODEL/TUNING SELECTION RULE:
PRE-FIT METRIC:
PRE-FIT SELECTION RULE:
EVENT-SPECIFIC EFFECT ESTIMAND:
EVENT-SPECIFIC TEST STATISTIC:
PLACEBO ELIGIBILITY RULE:
PLACEBO FIT RESTRICTION:
POOLED EFFECT WEIGHTING:
POOLED INFERENCE STATISTIC:
JOINT RANDOMIZATION PROCEDURE:
NUMBER OF RANDOMIZATION DRAWS:
ROBUSTNESS SPECIFICATIONS:
```

Keeping these decisions explicit is especially important when the number of treated events is small.

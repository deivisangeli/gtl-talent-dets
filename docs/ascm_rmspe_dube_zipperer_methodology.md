# Augmented Synthetic Control with RMSPE-Standardized Placebo Inference and Dube–Zipperer Pooling

## Purpose

This document provides a reusable methodology for applications with **multiple treated events or units**, where:

1. an **Augmented Synthetic Control Method (ASCM)** is estimated separately for each treated event;
2. the credibility of each event-specific counterfactual is evaluated using **pre-treatment prediction error**;
3. event-specific effects are standardized by pre-treatment RMSPE following the logic used in the **standardized placebo / pseudo-\(t\)** literature associated with Galiani and Quistorff;
4. placebo distributions are constructed separately for each event;
5. event-level evidence is converted into **percentile ranks**;
6. these ranks are pooled following the framework of **Dube and Zipperer (DZ)**; and
7. pooled inference can be conducted using a **joint randomization procedure** that preserves donor overlap and the full event structure.

This framework deliberately combines ideas from three related strands of the synthetic-control literature:

- **Augmented Synthetic Control**: Ben-Michael, Feller, and Rothstein;
- **RMSPE-standardized placebo inference**: Galiani and Quistorff and subsequent empirical applications;
- **Pooling multiple synthetic-control case studies**: Dube and Zipperer.

The combined procedure is therefore an **adaptation**, not a literal reproduction of any single paper.

---

# 1. Conceptual Overview

Suppose there are \(E\) treated events indexed by

\[
e=1,\ldots,E.
\]

Each event has:

- one treated unit \(i_e\);
- treatment date \(T_e\);
- an event-specific donor pool \(\mathcal D_e\);
- an outcome \(Y_{it}\);
- a pre-treatment window;
- a post-treatment evaluation window.

The analysis has two parallel objectives.

### Economic magnitude

Estimate the treatment effect in the original outcome scale:

\[
\widehat{\tau}_{e,h}
=
Y_{i_e,T_e+h}
-
\widehat{Y}_{e,T_e+h}(0).
\]

These estimates answer:

> How large is the estimated treatment effect?

### Statistical evidence

Evaluate whether the observed post-treatment deviation is unusually large relative to:

1. the event's own pre-treatment prediction error; and
2. the corresponding placebo distribution.

The event-specific standardized statistic is

\[
S_e
=
\frac{\widehat{\tau}_e}
{RMSPE^{pre}_e}.
\]

These standardized statistics are converted into event-specific placebo percentile ranks and then pooled.

The key distinction is:

\[
\boxed{
\widehat{\tau}
\rightarrow
\text{economic magnitude}
}
\]

while

\[
\boxed{
\widehat{\tau}/RMSPE^{pre}
\rightarrow
\text{placebo inference and rank-based pooling}
}
\]

---

# 2. Recommended Analysis Pipeline

The proposed baseline workflow is:

```text
Define treatment events
        |
Define causally valid donor pools
        |
Optional pre-treatment support restriction
        |
Estimate one ASCM per treated event
        |
Compute treated-minus-ASCM gaps
        |
Measure pre-treatment RMSPE
        |
Assess / trim poor-fitting treated events
        |
Estimate event-specific treatment effects
        |
Standardize effect by pre-treatment RMSPE
        |
Construct event-specific placebo ASCMs
        |
Standardize every placebo effect by its own pre-RMSPE
        |
Calculate percentile rank of actual event
        |
Pool economic effect magnitudes
        |
Pool percentile ranks
        |
Conduct pooled inference
        |
Joint randomization inference when donor overlap matters
```

Several robustness analyses should then repeat this pipeline under alternative:

- donor-pool sizes;
- covariate specifications;
- treatment-effect horizons;
- fit thresholds;
- synthetic-control estimators.

---

# 3. Step 1 — Define the Treatment Events

For each event \(e\), define:

\[
(i_e,T_e).
\]

The treatment date must be determined independently of post-treatment outcomes.

If units may receive treatment repeatedly, specify in advance whether the estimand concerns:

- first exposure;
- a particular exposure;
- treatment episodes with non-overlapping windows;
- cumulative exposure.

Treatment timing may differ across events.

---

# 4. Step 2 — Define Causally Eligible Donors

Before statistical matching or ASCM estimation, define an event-specific set of units that are substantively valid counterfactual candidates.

Let

\[
\mathcal D^{eligible}_e
\]

denote the eligible donor pool.

Possible exclusions include:

- units treated before \(T_e\);
- units treated during the event's post-treatment evaluation window;
- units likely to experience treatment spillovers;
- units subject to simultaneous shocks that invalidate them as controls;
- incompatible geographic definitions;
- substantial missing data;
- units belonging to clearly incomparable institutional regimes.

These restrictions should be based on the research design rather than on observed post-treatment effects.

## Important principle

ASCM relaxes the requirement of exact pre-treatment balance, but it does **not** make substantively invalid controls valid.

Therefore:

\[
\boxed{
\text{causal donor eligibility}
\neq
\text{statistical donor weighting}
}
\]

The researcher determines the first; ASCM handles the second.

---

# 5. Optional Step — Restrict to Local Pre-Treatment Support

When the potential donor universe is very large, for example hundreds or thousands of units, it may be useful to perform a pre-treatment support restriction before ASCM estimation.

This is optional.

A generic distance can be constructed from standardized pre-treatment characteristics.

For example,

\[
d_{ij}
=
\sqrt{
\sum_{k=1}^{K}
a_k
(z_{ik}-z_{jk})^2
},
\]

where

\[
\sum_k a_k=1.
\]

Possible variables include:

- average pre-treatment outcome;
- pre-treatment outcome trend;
- pre-treatment population growth;
- population density;
- industrial composition;
- other substantively important predictors.

If equal weights are desired,

\[
a_k=\frac{1}{K}.
\]

All variables should generally be standardized before calculating Euclidean distance.

The donor pool may then be restricted to the \(K^\star\) nearest eligible units:

\[
\mathcal D_e
=
\text{Top-}K^\star
\left(
\mathcal D^{eligible}_e
\right).
\]

## Choosing \(K^\star\)

Do not choose the donor-pool size using the treated unit's post-treatment outcomes.

Candidate values might be

\[
K \in
\{
20,30,50,75,100,\text{all eligible}
\}.
\]

A donor-based validation exercise can be used to choose among them.

For each untreated donor \(j\):

1. treat \(j\) as a pseudo-treated unit;
2. construct its donor pool using pre-treatment information;
3. estimate the same ASCM;
4. assess prediction performance during periods where \(j\) remains untreated.

Choose the specification that minimizes prediction error among untreated units.

This follows the general cross-validation philosophy developed by Dube and Zipperer for selecting synthetic-control specifications.

---

# 6. Step 3 — Estimate an Augmented Synthetic Control for Each Event

For each event, estimate

\[
\widehat{Y}_{et}(0),
\]

the untreated potential outcome of the treated unit.

The standard synthetic-control component can be written as

\[
\widehat{Y}^{SC}_{et}(0)
=
\sum_{j\in\mathcal D_e}
w_{ej}Y_{jt}.
\]

The Augmented Synthetic Control Method adds an outcome-model correction for residual imbalance.

Schematically,

\[
\widehat{Y}^{ASCM}_{et}(0)
=
\widehat{Y}^{SC}_{et}(0)
+
\text{bias correction from outcome model}.
\]

The ridge-augmented version of Ben-Michael, Feller, and Rothstein uses regularization to trade off:

- improved pre-treatment balance; and
- excessive extrapolation beyond the donor support.

ASCM is particularly useful when exact SCM balance is infeasible.

## Recommended predictors

The outcome history itself should play a central role.

Auxiliary pre-treatment covariates may include:

- population level;
- population growth;
- population density;
- industrial structure;
- education;
- innovation proxies;
- other determinants of the outcome.

Avoid adding covariates mechanically. Predictor choice should be substantively motivated and, when possible, validated using untreated donors.

---

# 7. Step 4 — Construct the Event-Specific Gap

Define

\[
g_{et}
=
Y_{i_e t}
-
\widehat{Y}^{ASCM}_{et}(0).
\]

This is the estimated treatment-effect path.

Before treatment,

\[
g_{et},
\qquad t<T_e,
\]

measures counterfactual prediction error.

After treatment,

\[
g_{et},
\qquad t\geq T_e,
\]

combines prediction error and any treatment effect.

The ideal pre-treatment gap is close to zero.

---

# 8. Step 5 — Measure Pre-Treatment Fit

For each event calculate:

\[
RMSPE^{pre}_e
=
\sqrt{
\frac{1}{T_{0e}}
\sum_{t<T_e}
g_{et}^{2}
}.
\]

This answers:

> How accurately does the estimated counterfactual reproduce the treated unit before treatment?

This measure is used for **fit assessment** and potentially for **event trimming**.

It must be calculated exclusively using pre-treatment observations.

---

# 9. Raw versus Normalized Pre-RMSPE

If the outcome scale differs materially across treated events, raw RMSPE may not be directly comparable.

One possible normalization is

\[
NRMSPE^{pre}_e
=
\frac{
RMSPE^{pre}_e
}{
\overline{|Y|}^{pre}_e
}.
\]

Other sensible scale normalizations may be used.

The choice should be fixed before inspecting treatment effects.

For event-specific placebo comparisons, the statistic described later already normalizes the treatment effect by each unit's own RMSPE.

---

# 10. Step 6 — Pre-Fit Screening of Treated Events

Not all treated events may admit a credible counterfactual.

Dube and Zipperer emphasize that pooling poor-quality case studies can weaken the credibility of the aggregate analysis and examine results after trimming on pre-intervention fit.

The key restriction is:

\[
\boxed{
\text{event inclusion may depend on pre-treatment fit only}
}
\]

Do not select events using:

- the post-treatment effect;
- statistical significance;
- placebo rank;
- post-treatment RMSPE;
- effect divided by RMSPE.

---

# 11. Progressive Trimming

A transparent strategy is to rank treated events by pre-treatment RMSPE.

Report results for:

1. all eligible events;
2. all except the worst-fitting event;
3. all except the two worst-fitting events;
4. increasingly stringent samples when useful.

This shows whether the pooled result depends on low-quality synthetic controls.

For a small number of events, progressive trimming is often more informative than committing to a single arbitrary threshold.

---

# 12. Fixed Fit Threshold

Alternatively, pre-specify a fit rule such as

\[
RMSPE^{pre}_e<c
\]

or

\[
NRMSPE^{pre}_e<c.
\]

If used, the threshold must be justified independently of post-treatment results.

A recommended empirical presentation is to report both:

- the full sample;
- the fit-screened sample.

---

# 13. Step 7 — Define the Economic Treatment Effect

For event time \(h\),

\[
\widehat{\tau}_{e,h}
=
g_{e,T_e+h}.
\]

For a fixed post-treatment horizon \(H\),

\[
\widehat{\tau}^{(H)}_e
=
\frac{1}{H}
\sum_{h=0}^{H-1}
g_{e,T_e+h}.
\]

This is the event-specific effect in the original outcome scale.

The post-treatment horizon must be chosen before conducting placebo inference.

Possible estimands include:

- immediate effect;
- effect at a specific horizon;
- average effect over years \(0\)–\(5\);
- average effect over years \(0\)–\(10\);
- long-run average effect;
- cumulative effect.

The choice should follow the substantive mechanism.

---

# 14. Preserve the Original-Scale Effect

The original-scale effect remains central.

For example,

\[
\widehat{\tau}^{(H)}_e=0.40
\]

has direct substantive meaning if the outcome is measured in:

- percentage points;
- scientists per 100,000 residents;
- log employment;
- patents per capita;
- test-score standard deviations.

Do not replace this effect with the RMSPE-standardized statistic when discussing economic magnitude.

---

# 15. Step 8 — Construct the RMSPE-Standardized Effect

Define

\[
S_e
=
\frac{
\widehat{\tau}^{(H)}_e
}{
RMSPE^{pre}_e
}.
\]

This statistic asks:

> How large is the post-treatment effect relative to the counterfactual prediction error already observed before treatment?

This follows the logic of the RMSPE-standardized placebo statistics implemented and discussed in the synthetic-control inference literature associated with Galiani and Quistorff.

It is often described informally as a **pseudo-\(t\)-type statistic**.

However:

\[
\boxed{
S_e
\text{ is not a conventional Student-}t\text{ statistic.}
}
\]

No conventional \(t\) distribution should be used.

Its null distribution is generated empirically from placebo units.

---

# 16. Interpretation of the Standardized Statistic

Consider two events:

\[
\widehat{\tau}_A=0.50,
\qquad
RMSPE^{pre}_A=0.05,
\]

and

\[
\widehat{\tau}_B=0.50,
\qquad
RMSPE^{pre}_B=0.50.
\]

Then:

\[
S_A=10,
\]

while

\[
S_B=1.
\]

The economic effect is identical, but the first effect represents a much larger post-treatment deviation relative to the model's pre-treatment prediction error.

This is why RMSPE standardization is useful for placebo inference.

---

# 17. Time-Specific Standardized Effects

The same logic can be applied at every event-time horizon:

\[
S_{e,h}
=
\frac{
\widehat{\tau}_{e,h}
}{
RMSPE^{pre}_e
}.
\]

This produces a standardized event-study path.

For example:

\[
h=-10,-5,0,5,10,15,20.
\]

The pre-treatment standardized gaps should remain centered around zero if the counterfactual is credible.

Post-treatment values can then be compared with event-time-specific placebo distributions.

---

# 18. Average-Horizon Standardized Effect

For pooling, it is useful to reduce each event to a pre-specified summary statistic.

For example,

\[
\overline{\tau}^{0:H}_e
=
\frac{1}{H+1}
\sum_{h=0}^{H}
\widehat{\tau}_{e,h}.
\]

Then define

\[
S^{0:H}_e
=
\frac{
\overline{\tau}^{0:H}_e
}{
RMSPE^{pre}_e
}.
\]

This gives one standardized statistic per treated event.

The horizon must be defined before inspecting placebo ranks.

---

# 19. Step 9 — Construct Event-Specific Placebos

For each treated event \(e\), conduct an in-space placebo exercise.

For every eligible donor \(j\in\mathcal D_e\):

1. pretend \(j\) was treated at \(T_e\);
2. remove \(j\) from its own donor pool;
3. apply the same causal eligibility rules;
4. apply the same local-support restriction, if used;
5. re-estimate the same ASCM;
6. compute the placebo pre-treatment RMSPE;
7. compute the placebo post-treatment effect;
8. construct the same standardized statistic.

For placebo unit \(j\),

\[
\widehat{\tau}^{P}_{ej}
\]

denotes the placebo treatment effect.

Its pre-treatment error is

\[
RMSPE^{pre,P}_{ej}.
\]

The standardized placebo statistic is

\[
S^P_{ej}
=
\frac{
\widehat{\tau}^{P}_{ej}
}{
RMSPE^{pre,P}_{ej}
}.
\]

---

# 20. The Placebo Must Reproduce the Full Estimator

If the real treated event is estimated using:

```text
causal donor eligibility
      ->
nearest-K pre-treatment support
      ->
ASCM
      ->
chosen effect horizon
      ->
pre-RMSPE standardization
```

then every placebo must use the same algorithm:

```text
placebo causal eligibility
      ->
recalculate nearest-K around placebo
      ->
ASCM
      ->
same effect horizon
      ->
placebo pre-RMSPE standardization
```

Do not reuse the treated unit's nearest-neighbor donor pool mechanically for placebo units.

Each placebo must be treated as if it were a genuine treated event.

---

# 21. Step 10 — Compare the Treated Statistic with Its Placebos

For event \(e\), compare

\[
S_e
\]

with

\[
\{
S^P_{e1},
S^P_{e2},
\ldots,
S^P_{eJ_e}
\}.
\]

The resulting empirical distribution represents the event-specific reference distribution under placebo treatment assignments.

---

# 22. Event-Specific Placebo p-Value

For a two-sided hypothesis:

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

For a directional hypothesis, use the appropriate one-sided comparison.

With \(J_e=30\), the minimum attainable permutation p-value is approximately

\[
\frac{1}{31}
\approx0.032.
\]

This discreteness should be acknowledged.

---

# 23. Step 11 — Convert the Event-Specific Statistic into a Percentile Rank

Define the percentile rank of the treated standardized statistic within its placebo distribution.

For a positive directional effect, one convenient definition is

\[
r_e
=
\frac{
1+
\sum_{j=1}^{J_e}
\mathbf{1}
\left(
S^P_{ej}\leq S_e
\right)
}{
J_e+1
}.
\]

Then:

- \(r_e\approx1\): unusually positive treatment effect;
- \(r_e\approx0\): unusually negative treatment effect;
- \(r_e\approx0.5\): typical placebo result.

Percentile ranks map heterogeneous event-specific placebo distributions onto the common interval

\[
[0,1].
\]

This makes them particularly useful for pooling multiple case studies.

---

# 24. Relation to Dube–Zipperer

Dube and Zipperer's core rank-based procedure computes the percentile rank of each **event-specific treatment effect** within its donor-based placebo distribution.

Their baseline can be summarized as:

\[
\widehat{\tau}_e
\rightarrow
\text{placebo effects}
\rightarrow
r_e
\rightarrow
\overline r.
\]

The methodology proposed here modifies the event-specific statistic to:

\[
S_e
=
\frac{
\widehat{\tau}_e
}{
RMSPE^{pre}_e
}.
\]

Thus the adapted sequence is:

\[
\boxed{
\widehat{\tau}_e
\rightarrow
\widehat{\tau}_e/RMSPE^{pre}_e
\rightarrow
\text{standardized placebo distribution}
\rightarrow
r_e
\rightarrow
\overline r
}
\]

The **normalization step is not the original DZ baseline**.

It is an adaptation combining DZ pooling with RMSPE-standardized placebo inference.

---

# 25. Recommended Dual Specification

To make the methodology transparent, report two rank specifications.

## Specification A — Strict DZ-style rank

Compare

\[
\widehat{\tau}_e
\]

directly with

\[
\{
\widehat{\tau}^{P}_{ej}
\}.
\]

Obtain:

\[
r^{DZ}_e.
\]

## Specification B — RMSPE-standardized rank

Compare

\[
S_e
=
\frac{
\widehat{\tau}_e
}{
RMSPE^{pre}_e
}
\]

with

\[
S^P_{ej}
=
\frac{
\widehat{\tau}^{P}_{ej}
}{
RMSPE^{pre,P}_{ej}
}.
\]

Obtain:

\[
r^{STD}_e.
\]

If both procedures yield similar conclusions, confidence in the pooled result increases.

If they differ substantially, investigate whether large raw effects are concentrated among poor-fitting synthetic controls.

---

# 26. Step 12 — Optional Post/Pre RMSPE Ratio

A second diagnostic common in the synthetic-control literature is

\[
R_e
=
\frac{
RMSPE^{post}_e
}{
RMSPE^{pre}_e
}.
\]

Where

\[
RMSPE^{post}_e
=
\sqrt{
\frac{1}{H}
\sum_{h=0}^{H-1}
g_{e,T_e+h}^{2}
}.
\]

The statistics answer different questions.

| Statistic | Interpretation |
|---|---|
| \(RMSPE^{pre}\) | Counterfactual fit before treatment |
| \(\widehat{\tau}\) | Economic magnitude and sign |
| \(\widehat{\tau}/RMSPE^{pre}\) | Signed post-treatment deviation relative to pre-fit error |
| \(RMSPE^{post}/RMSPE^{pre}\) | Increase in overall prediction error after treatment |

Because post-RMSPE squares the gaps, it does not preserve the treatment-effect sign.

Therefore, when the hypothesis is directional, the signed standardized effect is especially informative.

---

# 27. Poor-Fitting Placebos

Placebo units whose synthetic controls fit very poorly before treatment can generate extreme post-treatment gaps for mechanical reasons.

As a robustness exercise, restrict placebo units according to:

\[
\frac{
MSPE^{pre,P}_{ej}
}{
MSPE^{pre}_{e}
}
<c.
\]

Dube and Zipperer examine alternative thresholds such as:

\[
c\in\{20,10,5,2\}.
\]

Report results under multiple placebo-fit restrictions.

This restriction concerns **placebo quality**.

It is different from trimming the actual treated events.

---

# 28. Distinguish the Two Fit Screens

### Treated-event fit screen

Uses

\[
RMSPE^{pre}_e.
\]

Question:

> Is this treated case study credible enough to contribute to the pooled analysis?

### Placebo fit screen

Uses

\[
\frac{
MSPE^{pre,P}_{ej}
}{
MSPE^{pre}_e
}.
\]

Question:

> Is this placebo synthetic control sufficiently well fitted to provide an informative comparison?

Do not conflate these two operations.

---

# 29. Step 13 — Pool Economic Magnitudes

The pooled substantive effect should remain in the original outcome scale.

For \(K\) retained treated events,

\[
\widehat{ATT}
=
\frac{1}{K}
\sum_{e=1}^{K}
\widehat{\tau}_e.
\]

For event-time horizon \(h\),

\[
\widehat{ATT}_h
=
\frac{1}{K_h}
\sum_{e\in\mathcal E_h}
\widehat{\tau}_{e,h}.
\]

This provides a pooled event-study trajectory.

Alternative weighting schemes can be considered if justified.

Equal event weights are often attractive because every case study contributes equally rather than allowing large units to dominate mechanically.

---

# 30. Report Mean and Median Effects

With a small number of events and possible outliers, report at least:

\[
\text{Mean}(\widehat{\tau}_e)
\]

and

\[
\text{Median}(\widehat{\tau}_e).
\]

This makes sensitivity to extreme event-specific estimates transparent.

Dube and Zipperer also report both mean and median effects alongside their rank-based pooled estimator.

---

# 31. Step 14 — Pool Statistical Evidence Using the Mean Percentile Rank

Let

\[
r_e
\]

denote the event-specific percentile rank.

Define

\[
\overline r
=
\frac{1}{K}
\sum_{e=1}^{K}r_e.
\]

This is the **mean percentile rank**, the central pooled rank statistic proposed by Dube and Zipperer.

Interpretation:

- \(\overline r\approx0.5\): treated effects look typical relative to placebos;
- \(\overline r>0.5\): treated effects tend to be unusually positive;
- \(\overline r<0.5\): treated effects tend to be unusually negative.

The standardized version proposed here uses ranks based on \(S_e\) rather than raw \(\widehat{\tau}_e\).

---

# 32. Baseline Rank Inference Under Independence

Under a sharp null and idealized independent continuous placebo ranks,

\[
r_e
\sim U(0,1).
\]

Therefore,

\[
\overline r
\]

has the distribution of the mean of \(K\) independent uniform variables.

This is related to the scaled Irwin–Hall distribution.

Dube and Zipperer use this as their baseline inferential framework.

However, two practical complications matter:

1. ranks are discrete because donor pools are finite;
2. ranks may be correlated because events can share donor units.

For applications with few events or substantial donor overlap, a joint randomization procedure is preferable.

---

# 33. Finite-Donor Rank Distribution

When event \(e\) has \(J_e\) donors, its rank takes values on a finite grid rather than continuously over \([0,1]\).

A finite-sample rank transformation such as

\[
r_e
=
\frac{
R_e
}{
J_e+1
}
\]

can be used, where \(R_e\) is the event's integer rank.

Simulation can then account for the event-specific number of donors.

Dube and Zipperer explicitly consider this discrete-rank alternative.

---

# 34. Donor Sharing Across Events

The same control unit may appear in several synthetic controls.

Suppose:

\[
SC_1
=
0.4A+0.3B+0.3C,
\]

and

\[
SC_2
=
0.5A+0.3D+0.2B.
\]

Then shocks to \(A\) and \(B\) affect both synthetic controls.

Consequently,

\[
Cov(\widehat{\tau}_1,\widehat{\tau}_2)
\neq0.
\]

The event-specific percentile ranks may therefore also be correlated.

This violates the independent-rank approximation.

---

# 35. Do Not Mechanically Eliminate Shared Donors

Shared donors are not inherently invalid.

If a unit is a plausible counterfactual for multiple treated cases, forcing it to appear in only one synthetic control may worsen pre-treatment fit and counterfactual quality.

The baseline approach should generally:

\[
\boxed{
\text{allow donor sharing}
}
\]

and handle the resulting dependence at the inference stage.

---

# 36. Diagnose Donor Overlap

Store the event-by-donor weight matrix:

\[
W
=
\left[
w_{ej}
\right].
\]

Useful diagnostics include:

### Number of events using donor \(j\)

\[
N_j
=
\sum_e
\mathbf 1(w_{ej}\neq0).
\]

### Cumulative donor weight

\[
C_j
=
\sum_e w_{ej}.
\]

### Pairwise weighted donor overlap

\[
O_{ee'}
=
\sum_j
\min(w_{ej},w_{e'j}).
\]

For conventional nonnegative SCM weights, \(O_{ee'}\) ranges naturally between 0 and 1.

For ASCM effective weights that may be negative, alternative diagnostics should also consider:

- absolute cumulative weight;
- positive and negative weights separately;
- cosine similarity of effective-weight vectors;
- sensitivity to influential donors.

---

# 37. Step 15 — Joint Randomization Inference

When donor sharing or other dependence is important, conduct pooled inference by reproducing the complete event structure under placebo assignments.

For randomization draw \(b\):

1. generate a joint placebo assignment for all events;
2. preserve each event's treatment date;
3. preserve each event's pre/post window;
4. apply the same donor eligibility rules;
5. recalculate any local-support donor selection;
6. estimate all placebo ASCMs;
7. calculate placebo pre-RMSPEs;
8. apply the same treated-event fit screening rule;
9. calculate the same event-specific treatment effects;
10. standardize each effect by its own placebo pre-RMSPE;
11. calculate placebo percentile ranks;
12. calculate the pooled mean rank.

This produces:

\[
\overline r^{(1)},
\overline r^{(2)},
\ldots,
\overline r^{(B)}.
\]

Compare

\[
\overline r^{obs}
\]

with this empirical null distribution.

---

# 38. Preserving Donor Overlap

Dube and Zipperer explicitly discuss donor overlap as a source of correlation across event-specific ranks.

Their randomization approach permutes unit identities while preserving:

- treatment timing;
- event windows;
- donor-pool structure;
- recurring-treatment structure;
- overlap among donor sets.

The same logic should be followed here.

Independent placebo sampling event by event is less attractive when the same units repeatedly enter donor pools.

---

# 39. The Full Selection Algorithm Should Be Reproduced Under Randomization

Suppose the real analysis uses:

```text
1,000 potential controls
   ->
causal eligibility
   ->
top 50 nearest donors
   ->
ASCM
   ->
pre-RMSPE screen
   ->
standardized statistic
   ->
rank
```

Then every joint placebo draw should reproduce:

```text
placebo potential controls
   ->
same causal eligibility rule
   ->
recompute top 50 around placebo
   ->
same ASCM
   ->
same pre-RMSPE screen
   ->
same standardized statistic
   ->
same rank
```

The inference target is the complete **analysis algorithm**, not only the final statistic.

---

# 40. Event Selection Inside the Randomization Procedure

If the observed analysis drops treated events failing a pre-fit threshold,

\[
RMSPE^{pre}_e<c,
\]

the corresponding screening step should be applied within placebo/randomized samples whenever feasible.

This prevents the randomization distribution from ignoring an important model-selection step that affected the observed statistic.

If computational constraints make this infeasible, report this limitation and provide a simpler inference procedure as a comparison.

---

# 41. Step 16 — Pooled p-Value

For an upper-tail alternative,

\[
p_{pool}
=
\frac{
1+
\sum_{b=1}^{B}
\mathbf 1
\left(
\overline r^{(b)}
\geq
\overline r^{obs}
\right)
}{
B+1
}.
\]

For two-sided inference, use the appropriate distance from the null center of 0.5.

For example,

\[
\left|
\overline r^{obs}-0.5
\right|.
\]

---

# 42. Hodges–Lehmann Pooled Effect

Dube and Zipperer go beyond the mean rank and construct a pooled Hodges–Lehmann-type effect by inverting the rank test.

For a candidate common shift \(\delta\), adjust each treated estimate:

\[
\widehat{\tau}_e-\delta.
\]

Recalculate its event-specific percentile rank:

\[
r_e(\delta).
\]

Then compute

\[
\overline r(\delta)
=
\frac{1}{K}
\sum_e
r_e(\delta).
\]

The pooled Hodges–Lehmann estimate is the value of \(\delta\) that centers the pooled rank statistic appropriately.

Confidence intervals can be obtained by inverting the pooled rank test.

This is closer to the original DZ pooled point-estimation procedure than simply reporting the arithmetic mean effect.

---

# 43. Hodges–Lehmann under RMSPE Standardization

If using the RMSPE-standardized statistic, a direct analogue requires care because subtracting a common treatment effect \(\delta\) changes the standardized statistic as

\[
S_e(\delta)
=
\frac{
\widehat{\tau}_e-\delta
}{
RMSPE^{pre}_e
}.
\]

The same transformation must be applied consistently to the placebo comparison.

This is feasible but should be described explicitly as an **adaptation of DZ**, not their original estimator.

For transparency, a recommended baseline is:

- use mean/median original-scale effects for economic magnitude;
- use the standardized mean rank for primary placebo-based evidence;
- report a strict DZ Hodges–Lehmann estimate as an additional specification if desired.

---

# 44. Main versus Robustness Specifications

A clean empirical hierarchy is:

## Main counterfactual estimator

\[
\text{ASCM}.
\]

## Main fit diagnostic

\[
RMSPE^{pre}.
\]

## Main event-specific test statistic

\[
S_e
=
\frac{
\widehat{\tau}_e
}{
RMSPE^{pre}_e
}.
\]

## Main event-specific inference

Placebo percentile rank of \(S_e\).

## Main pooled statistical evidence

\[
\overline r^{STD}.
\]

## Main pooled economic magnitude

\[
\text{Mean}(\widehat{\tau}_e)
\]

and event-time pooled effects.

## Main pooled inference

Joint randomization inference preserving event structure and donor overlap.

## Key robustness

Strict DZ ranks based on raw \(\widehat{\tau}_e\).

---

# 45. Recommended Robustness Checks

## A. Strict DZ raw-effect ranks

Replace

\[
S_e
\]

with

\[
\widehat{\tau}_e
\]

when constructing event-specific ranks.

Compare:

\[
\overline r^{STD}
\]

with

\[
\overline r^{DZ}.
\]

---

## B. Progressive treated-event trimming

Report pooled results for:

- all events;
- drop worst pre-fit event;
- drop two worst;
- additional restrictions when informative.

---

## C. Alternative placebo-fit restrictions

Use placebo MSPE ratios:

\[
20,\quad10,\quad5,\quad2.
\]

---

## D. Alternative donor-pool sizes

For example:

\[
K
=
20,\ 30,\ 50,\ 75,\ 100,\ all.
\]

---

## E. All eligible donors versus local support

Compare:

\[
ASCM(\text{all causally eligible})
\]

with

\[
ASCM(\text{nearest }K).
\]

---

## F. Alternative post-treatment horizons

For example:

\[
0\text{--}5,
\qquad
0\text{--}10,
\qquad
0\text{--}20.
\]

---

## G. Leave-one-event-out

For every event \(e\), estimate:

\[
\widehat{ATT}_{-e}
\]

and

\[
\overline r_{-e}.
\]

---

## H. Leave-influential-donor-out

Identify donors with:

- large cumulative ASCM weight;
- frequent use across events;
- large absolute effective weights.

Remove each influential donor and re-estimate the complete analysis.

---

## I. Disjoint donor pools

As a deliberately conservative robustness check, prohibit donors from contributing to more than one event.

This should generally not be the main specification because it may sacrifice counterfactual quality.

---

## J. Alternative estimators

Compare:

- conventional SCM;
- Augmented SCM;
- Synthetic Difference-in-Differences;
- partially pooled SCM;
- other justified estimators.

---

## K. Placebo-in-time

Artificially assign treatment dates during the pre-treatment period.

Large apparent effects before the true treatment date weaken the causal interpretation.

---

# 46. Model Selection Using Untreated Donors

Dube and Zipperer propose a useful cross-validation logic for choosing among predictor specifications.

For candidate model \(m\):

1. estimate synthetic controls for untreated donor units;
2. use pre-treatment observations to estimate weights;
3. assess prediction performance during periods in which those donors remain untreated;
4. calculate validation RMSPE;
5. choose the model with the best untreated-unit prediction performance.

This avoids choosing the model because it performs well on the genuinely treated unit after treatment.

The same idea can be used here to select:

- predictor sets;
- donor-pool size;
- distance metric;
- ASCM tuning alternatives;
- temporal aggregation.

---

# 47. Cross-Validation Criterion

For pseudo-treated donor \(j\) and model \(m\), define:

\[
MSPE^{val}_{jm}
=
\frac{1}{T^{val}}
\sum_{t\in\mathcal T^{val}}
\left[
Y_{jt}
-
\widehat{Y}_{jt}^{(m)}(0)
\right]^2.
\]

Then:

\[
CV(m)
=
\sqrt{
\frac{1}{J}
\sum_j
MSPE^{val}_{jm}
}.
\]

Select:

\[
m^\star
=
\arg\min_m CV(m).
\]

The validation periods must be periods in which pseudo-treated donors remain untreated.

---

# 48. Why Not Select Models Using Treated Post-Treatment Outcomes?

Because the treated post-treatment outcome potentially contains the treatment effect.

Choosing the model that predicts it best would favor models that mechanically explain away the treatment effect.

This contaminates causal estimation.

Therefore:

\[
\boxed{
\text{treated post-treatment outcomes are not validation data}
}
\]

for model selection.

---

# 49. Suggested Tables

## Table 1 — Event definitions

| Event | Treated unit | Treatment date | Pre window | Post window | Eligible donors |
|---|---|---:|---:|---:|---:|

---

## Table 2 — ASCM fit diagnostics

| Event | Pre-RMSPE | Normalized pre-RMSPE | Effective donors | Largest weight | Included? |
|---|---:|---:|---:|---:|---|

---

## Table 3 — Event-specific effects

| Event | Raw effect | Pre-RMSPE | Standardized effect | Raw-effect rank | Standardized rank |
|---|---:|---:|---:|---:|---:|

---

## Table 4 — Pooled results

| Specification | Events | Mean effect | Median effect | Mean raw rank | Mean standardized rank | Pooled p-value |
|---|---:|---:|---:|---:|---:|---:|

---

## Table 5 — Progressive trimming

| Sample | Events retained | Mean effect | Mean standardized rank | Pooled p-value |
|---|---:|---:|---:|---:|

---

## Table 6 — Donor overlap

| Donor | Number of events used | Cumulative weight | Cumulative absolute weight |
|---|---:|---:|---:|

---

# 50. Suggested Figures

## Figure 1 — Treated versus ASCM trajectory

One panel per event.

Show:

- treated outcome;
- augmented synthetic counterfactual;
- treatment date.

---

## Figure 2 — Event-specific gaps

Plot:

\[
g_{et}
\]

in event time.

---

## Figure 3 — Pooled event-study effects

Plot:

\[
\widehat{ATT}_h.
\]

This figure remains in the original outcome scale.

---

## Figure 4 — Standardized event-study gaps

Plot:

\[
S_{e,h}
=
\frac{
\widehat{\tau}_{e,h}
}{
RMSPE^{pre}_e
}.
\]

---

## Figure 5 — Event-specific placebo distributions

For every event, display:

- standardized placebo statistics;
- actual standardized statistic.

---

## Figure 6 — Raw versus standardized ranks

Plot:

\[
r^{DZ}_e
\]

against

\[
r^{STD}_e.
\]

Large discrepancies identify events whose apparent evidence is sensitive to pre-fit quality.

---

## Figure 7 — Progressive trimming

Horizontal axis:

- increasingly strict pre-fit samples.

Vertical axes:

- pooled economic effect;
- mean standardized rank.

---

## Figure 8 — Joint randomization distribution

Display the empirical distribution of

\[
\overline r^{(b)}
\]

and mark

\[
\overline r^{obs}.
\]

---

# 51. Recommended Main Results Narrative

A generic results sequence could be:

### First

Show that credible augmented synthetic counterfactuals can be constructed for the treated events.

### Second

Report pre-treatment RMSPE and demonstrate which cases have stronger or weaker counterfactual fit.

### Third

Report original-scale treatment effects.

### Fourth

Normalize each event effect by its pre-treatment RMSPE:

\[
S_e
=
\frac{
\widehat{\tau}_e
}{
RMSPE^{pre}_e
}.
\]

### Fifth

Construct identical standardized statistics for placebo units.

### Sixth

Convert each treated event into a placebo percentile rank.

### Seventh

Pool those ranks using:

\[
\overline r.
\]

### Eighth

Evaluate the pooled rank with a randomization distribution preserving donor overlap and event structure.

### Ninth

Report the original-scale pooled effect separately from the pooled rank evidence.

---

# 52. Interpretation of the Two Pooled Objects

The pooled mean effect:

\[
\widehat{ATT}
\]

answers:

> How large is the average estimated treatment effect across the retained events?

The mean standardized percentile rank:

\[
\overline r^{STD}
\]

answers:

> Are treated events systematically associated with post-treatment deviations that are unusually large relative both to their own pre-treatment prediction error and to corresponding placebo deviations?

These are related but distinct questions.

Neither statistic should replace the other.

---

# 53. Interpretation of Fit-Selected Samples

Suppose \(E=10\) events are observed but only \(K=7\) satisfy the pre-fit criterion.

Then the pooled effect is not automatically the effect across all 10 treated events.

It is more accurately described as:

> the average effect among treated events for which the chosen synthetic-control procedure produces a sufficiently credible pre-treatment counterfactual.

This limitation should be stated explicitly.

Progressive trimming helps show whether the substantive conclusion depends strongly on this selection.

---

# 54. Potential Concern — Dividing by Very Small RMSPE

If

\[
RMSPE^{pre}_e
\approx0,
\]

then

\[
S_e
=
\frac{\widehat{\tau}_e}
{RMSPE^{pre}_e}
\]

may become extremely large.

This is not automatically evidence of a huge treatment effect.

It may reflect an unusually precise pre-treatment fit.

Recommended safeguards:

- inspect the raw effect;
- inspect the pre-RMSPE distribution;
- compare with equally standardized placebos;
- report raw-effect ranks as a robustness check;
- consider a pre-specified lower-bound regularization only if strongly justified.

Do not introduce an ad hoc denominator floor after observing results.

---

# 55. Potential Concern — ASCM and RMSPE

ASCM is explicitly designed to improve performance when SCM pre-treatment balance is imperfect.

Therefore, very low ASCM pre-RMSPE should not be interpreted in exactly the same way as a low conventional-SCM RMSPE without examining:

- extrapolation;
- effective weights;
- regularization;
- donor influence.

A strong ASCM analysis should report both:

1. prediction fit; and
2. the degree of extrapolation or weight instability.

---

# 56. Potential Concern — Outcome Sparsity

When the outcome is rare, annual observations may contain substantial noise.

Possible responses include:

- multi-year outcome bins;
- smoothed pre-treatment predictors;
- average pre-treatment outcome;
- outcome trends;
- validation across alternative temporal aggregation.

Do not maximize the number of pre-treatment observations mechanically if finer-frequency observations are dominated by noise.

Dube and Zipperer's model-selection discussion is useful here: better in-sample pre-fit does not necessarily imply better out-of-sample counterfactual prediction.

---

# 57. Potential Concern — Many Donors

With hundreds or thousands of potential controls, the baseline ASCM may involve many units.

This is not inherently invalid, because regularization is part of ASCM.

However, extremely large donor pools can create concerns about:

- weak substantive comparability;
- computational burden;
- diffuse effective weights;
- out-of-support extrapolation;
- complexity of placebo inference.

Therefore, when the donor universe is very large, compare:

\[
ASCM(\text{all eligible donors})
\]

against

\[
ASCM(\text{local-support donors}).
\]

A donor-based validation procedure can guide the choice.

---

# 58. Potential Concern — Multiple Testing Across Event Time

If standardized placebo p-values are reported for many event-time horizons, do not interpret isolated significant periods without acknowledging multiple comparisons.

The primary inferential target should ideally be pre-specified, for example:

\[
\overline{\tau}^{0:10}.
\]

The event-study path then provides dynamic descriptive evidence.

Simultaneous inference can be added when necessary.

---

# 59. Potential Concern — Directional versus Two-Sided Tests

If theory strongly predicts a positive effect and this hypothesis is pre-specified, a one-sided rank may be appropriate.

Otherwise use a two-sided test.

Do not switch between one-sided and two-sided inference after observing the sign of the estimates.

---

# 60. Recommended Pre-Analysis Decisions

Before final estimation, document:

```text
TREATMENT DEFINITION:
TREATMENT DATES:
REPEATED-TREATMENT RULE:
OUTCOME:
OUTCOME SCALE:
UNIT OF OBSERVATION:
PRE-TREATMENT WINDOW:
POST-TREATMENT WINDOW:
PRIMARY EFFECT HORIZON:

CAUSAL DONOR ELIGIBILITY RULE:
SPILLOVER RULE:
LOCAL-SUPPORT RESTRICTION:
DISTANCE VARIABLES:
DISTANCE STANDARDIZATION:
CANDIDATE DONOR-POOL SIZES:
DONOR-POOL VALIDATION PROCEDURE:

ASCM SPECIFICATION:
OUTCOME MODEL:
AUXILIARY COVARIATES:
REGULARIZATION / TUNING RULE:

PRE-FIT METRIC:
TREATED-EVENT FIT RULE:
PROGRESSIVE TRIMMING PLAN:

PRIMARY EVENT-LEVEL EFFECT:
PRIMARY STANDARDIZED STATISTIC:
RAW DZ RANK AS ROBUSTNESS?:
PLACEBO FIT RESTRICTION:
ONE- OR TWO-SIDED TEST:

POOLED ECONOMIC EFFECT:
POOLED RANK STATISTIC:
JOINT RANDOMIZATION PROCEDURE:
NUMBER OF RANDOMIZATION DRAWS:

DONOR-OVERLAP DIAGNOSTICS:
LEAVE-ONE-EVENT-OUT RULE:
INFLUENTIAL-DONOR RULE:
ADDITIONAL ESTIMATORS:
```

---

# 61. Baseline Algorithm in Mathematical Form

For each treated event \(e\):

### Donor construction

\[
\mathcal D_e
=
\mathcal S
\left(
\mathcal D^{eligible}_e
\right),
\]

where \(\mathcal S\) is either:

- no statistical support restriction; or
- a pre-specified / validated local-support rule.

### ASCM counterfactual

\[
\widehat{Y}_{et}(0)
=
ASCM
\left(
Y_{jt}:j\in\mathcal D_e
\right).
\]

### Gap

\[
g_{et}
=
Y_{i_et}
-
\widehat{Y}_{et}(0).
\]

### Pre-fit

\[
RMSPE^{pre}_e
=
\sqrt{
\frac{1}{T_{0e}}
\sum_{t<T_e}
g_{et}^2
}.
\]

### Economic effect

\[
\widehat{\tau}_e
=
\frac{1}{H}
\sum_{h=0}^{H-1}
g_{e,T_e+h}.
\]

### Standardized statistic

\[
S_e
=
\frac{
\widehat{\tau}_e
}{
RMSPE^{pre}_e
}.
\]

### Placebo statistic

For every eligible placebo \(j\):

\[
S^P_{ej}
=
\frac{
\widehat{\tau}^{P}_{ej}
}{
RMSPE^{pre,P}_{ej}
}.
\]

### Percentile rank

\[
r_e
=
\widehat F^P_e(S_e).
\]

### Pooled economic effect

\[
\widehat{ATT}
=
\frac{1}{K}
\sum_e
\widehat{\tau}_e.
\]

### Pooled rank

\[
\overline r
=
\frac{1}{K}
\sum_e r_e.
\]

### Pooled inference

Compare

\[
\overline r^{obs}
\]

against the joint randomization distribution

\[
\{
\overline r^{(1)},\ldots,\overline r^{(B)}
\}.
\]

---

# 62. Baseline Algorithm in Pseudocode

```text
FOR each true treatment event e:

    1. define causally eligible donors

    2. if local-support restriction is used:
           compute pre-treatment distance
           retain nearest K* donors

    3. estimate ASCM

    4. compute pre-treatment RMSPE

    5. compute event-specific treatment effect

    6. compute standardized effect:
           effect / pre-RMSPE

    7. FOR each eligible placebo donor j:

           treat j as pseudo-treated

           reconstruct j's donor pool

           re-run support restriction

           estimate placebo ASCM

           compute placebo pre-RMSPE

           compute placebo effect

           compute placebo standardized effect

    8. calculate event-specific percentile rank


AFTER all true events:

    9. apply pre-fit trimming / screening

    10. pool original-scale treatment effects

    11. average event-specific standardized percentile ranks

    12. run joint randomization inference

    13. repeat under robustness specifications
```

---

# 63. Joint Randomization Pseudocode

```text
FOR b = 1,...,B:

    jointly permute treatment identities
    while preserving the event structure

    FOR each pseudo-event e:

        apply causal eligibility rule

        reconstruct donor support

        estimate ASCM

        calculate pseudo-event pre-RMSPE

        apply event-fit rule

        calculate pseudo treatment effect

        standardize by pseudo pre-RMSPE

        construct its event-specific placebo distribution

        calculate its percentile rank

    END

    calculate mean percentile rank for draw b

END

pooled p-value =
share of randomized mean ranks
at least as extreme as observed mean rank
```

---

# 64. What Is Directly from Each Literature?

## Ben-Michael, Feller, and Rothstein

Directly motivates:

- ASCM;
- bias correction for imperfect pre-treatment balance;
- ridge outcome model;
- regularization to limit extrapolation.

---

## Galiani and Quistorff / standardized placebo literature

Directly motivates:

- automating placebo synthetic controls;
- evaluating treatment effects relative to placebo effects;
- RMSPE-standardized / pseudo-\(t\)-type placebo inference used in subsequent empirical applications.

The particular use of a **multi-period average effect divided by pre-RMSPE** is a natural extension of the same normalization principle.

---

## Dube and Zipperer

Directly motivates:

- multiple synthetic-control case studies;
- event-specific placebo distributions;
- percentile ranks;
- mean percentile-rank pooling;
- trimming based on pre-treatment fit;
- donor-based cross-validation for specification choice;
- treatment of finite donor counts;
- concern about correlated ranks from donor overlap;
- joint randomization inference preserving donor overlap;
- Hodges–Lehmann pooled estimation via inversion of the rank test.

---

# 65. What Is the Adaptation Proposed Here?

The distinctive combined methodology is:

\[
\boxed{
ASCM
+
RMSPE\text{-standardized event statistic}
+
DZ\ percentile\ rank\ pooling
}
\]

Specifically:

\[
\boxed{
S_e
=
\frac{
\widehat{\tau}^{ASCM}_e
}{
RMSPE^{pre,ASCM}_e
}
}
\]

is compared with identically constructed placebo statistics.

Then:

\[
r_e
=
\text{percentile rank of }S_e
\]

is pooled as:

\[
\overline r
=
\frac{1}{K}
\sum_e r_e.
\]

This exact combination should be presented as a methodological adaptation grounded in the three literatures above.

---

# 66. Recommended Terminology in a Paper

Avoid calling

\[
S_e
\]

a conventional \(t\)-statistic.

Preferred terminology:

- **RMSPE-standardized treatment effect**;
- **standardized synthetic-control statistic**;
- **pseudo-\(t\)-type statistic**, when explicitly linked to the synthetic-control placebo literature.

For the pooled statistic use:

- **mean percentile rank of the RMSPE-standardized treatment effects**.

For the economic pooled effect use:

- **mean event-specific treatment effect**;
- **pooled event-time treatment effect**.

---

# 67. Example Methodological Description

A generic paper could describe the procedure as follows:

> For each treatment event, we estimate an augmented synthetic control using only untreated comparison units that satisfy our pre-specified donor-eligibility criteria. We assess counterfactual quality using the pre-treatment RMSPE. We then summarize the post-treatment gap over a pre-specified horizon and standardize this effect by the event's pre-treatment RMSPE. To conduct placebo-based inference, we repeat the complete estimation procedure treating each eligible donor unit as pseudo-treated and construct the corresponding RMSPE-standardized placebo effect. The treated event's statistic is converted into its percentile rank within this event-specific placebo distribution. Finally, we pool evidence across treatment events using the mean percentile rank, adapting the rank-based procedure of Dube and Zipperer. Because event-specific synthetic controls may share donors, pooled inference is based on a joint randomization procedure designed to preserve the timing and donor-overlap structure of the original events.

This wording should be adapted to the exact estimator and randomization implementation.

---

# 68. Minimum Set of Outputs

At minimum, retain for every treated event:

```text
event_id
treated_unit
treatment_date
number_eligible_donors
number_final_donors
pre_RMSPE
post_RMSPE
raw_effect
standardized_effect
raw_DZ_rank
standardized_rank
placebo_p_value
largest_effective_donor_weight
number_effective_donors
```

For every placebo:

```text
event_id
placebo_unit
placebo_pre_RMSPE
placebo_post_RMSPE
placebo_raw_effect
placebo_standardized_effect
placebo_rank_component
```

For pooled runs:

```text
specification_id
events_retained
mean_effect
median_effect
mean_raw_rank
mean_standardized_rank
joint_randomization_p_value
```

---

# 69. Reproducibility Requirement

The complete analysis should be written so that a single function or pipeline can receive:

```text
treated unit
treatment date
eligible population of controls
pre-treatment window
post-treatment window
model specification
```

and return:

```text
ASCM estimate
fit metrics
effect path
summary effect
standardized effect
placebo distribution
percentile rank
donor weights
```

The exact same function should be called for true treatments and placebo treatments.

This sharply reduces the risk of unintentionally treating genuine and placebo cases differently.

---

# 70. Recommended Empirical Hierarchy

A particularly clean hierarchy is:

### Baseline

- ASCM;
- pre-treatment fit diagnostics;
- original-scale effect;
- RMSPE-standardized statistic;
- standardized placebo percentile rank;
- mean standardized rank;
- joint pooled randomization inference.

### First robustness

Strict DZ procedure using raw event effects rather than RMSPE-standardized effects.

### Second robustness

Alternative donor support:

- 30 nearest;
- 50 nearest;
- 100 nearest;
- all eligible.

### Third robustness

Alternative counterfactual estimators:

- SCM;
- SDID;
- ASCM.

### Fourth robustness

Progressive pre-fit trimming.

### Fifth robustness

Alternative effect horizons.

---

# 71. Core Identification and Inference Assumptions

The method still relies on substantive assumptions.

## No anticipation

Units should not respond to treatment before \(T_e\), unless anticipation is explicitly modeled.

## No relevant spillovers

Donor outcomes should not be affected by the treated event.

## Stable untreated relationship

Pre-treatment relationships used to construct the counterfactual should remain informative after treatment in the absence of treatment.

## Valid placebo exchangeability

Placebo assignments should represent meaningful untreated comparisons under the null.

## Pre-treatment model selection

Donor, model, and fit decisions should not be optimized using treated post-treatment outcomes.

---

# 72. Final Recommended Workflow

The preferred generic procedure can be summarized as:

\[
\boxed{
\begin{array}{c}
\text{Define events}\\
\downarrow\\
\text{Causal donor eligibility}\\
\downarrow\\
\text{Optional validated local-support restriction}\\
\downarrow\\
\text{Event-specific ASCM}\\
\downarrow\\
\text{Pre-treatment RMSPE}\\
\downarrow\\
\text{Pre-fit event screening / progressive trimming}\\
\downarrow\\
\text{Original-scale treatment effect}\\
\downarrow\\
\text{Effect / pre-RMSPE}\\
\downarrow\\
\text{Identically constructed placebo statistics}\\
\downarrow\\
\text{Event-specific percentile rank}\\
\downarrow\\
\text{Mean percentile-rank pooling}\\
\downarrow\\
\text{Joint randomization inference}\\
\downarrow\\
\text{Robustness using strict DZ ranks and alternative estimators}
\end{array}
}
\]

---

# 73. Key Things Not to Do

Do **not**:

- select treated events because their post-treatment effects are large;
- select donors using treated post-treatment outcomes;
- divide the true effect by RMSPE but leave placebo effects unstandardized;
- use the treated unit's donor-support ranking unchanged for every placebo;
- interpret \(S_e\) using a Student-\(t\) distribution;
- assume event-level ranks are independent when donor overlap is substantial without checking robustness;
- treat synthetic controls as ordinary independent observed units in a second-stage regression without accounting for their construction;
- report only standardized effects and hide the original-scale economic effects;
- choose the post-treatment horizon after seeing where effects are largest;
- use ASCM as a substitute for substantive donor eligibility.

---

# 74. References

Abadie, A., Diamond, A., & Hainmueller, J. (2010).  
**Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program.**  
*Journal of the American Statistical Association*, 105(490), 493–505.

Ben-Michael, E., Feller, A., & Rothstein, J. (2021).  
**The Augmented Synthetic Control Method.**  
*Journal of the American Statistical Association*, 116(536), 1789–1803.

Dube, A., & Zipperer, B. (2015).  
**Pooling Multiple Case Studies Using Synthetic Controls: An Application to Minimum Wage Policies.**  
IZA Discussion Paper No. 8944.

Galiani, S., & Quistorff, B. (2017/2018).  
**The Synth_Runner Package: Utilities to Automate Synthetic Control Estimation Using Synth.**  
*The Stata Journal*, 17(4), 834–849.

Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S. (2021).  
**Synthetic Difference-in-Differences.**  
*American Economic Review*, 111(12), 4088–4118.

---

# 75. Short Methodological Summary

The method should be remembered as four distinct layers:

### Layer 1 — Counterfactual construction

\[
ASCM.
\]

### Layer 2 — Counterfactual credibility

\[
RMSPE^{pre}.
\]

### Layer 3 — Event-specific placebo evidence

\[
\frac{
\widehat{\tau}
}{
RMSPE^{pre}
}
\rightarrow
\text{placebo rank}.
\]

### Layer 4 — Multiple-event pooling

\[
\text{mean percentile rank}
+
\text{joint randomization inference}.
\]

And separately:

\[
\boxed{
\widehat{\tau}
\rightarrow
\text{economic magnitude}
}
\]

must always be reported in the original outcome scale.

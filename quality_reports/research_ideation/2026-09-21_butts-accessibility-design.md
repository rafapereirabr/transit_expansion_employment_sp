# Butts-style nonparametric DiD indexed by accessibility change

## Study-level objective and fixed design commitments

The empirical study asks how rapid transit expansion affects employment, formalization and job
quality for socioeconomically vulnerable residents of metropolitan São Paulo. The scope of an
individual consultant's technical deliverable does not restrict that question: the project team
will build and run the full causal analysis. For the CAF study, a causal difference-in-differences
framework is a fixed requirement. Cross-sectional comparisons and generic fixed-effects panels may
be diagnostics, but they are not alternative primary designs. The immediate executive box remains
a separate descriptive product without DiD claims.

For intervention cohort or window $c=(t_0,t_1)$, let $P_{c0}$ be the eligible population resident
in the study area at baseline, $N_{0c}$ the network without the specified expansion, $N_{1c}$ the
network with it, and $X_{ic}$ the modeled accessibility change at the baseline residence with land
use held fixed. The central conditional policy estimand is

$$
\tau_c(x)=E\left[Y_{i,t_1}(N_{1c})-Y_{i,t_1}(N_{0c})
\mid i\in P_{c0}, X_{ic}=x\right].
$$

This is the total effect of a defined network transition for baseline residents, indexed by modeled
connectivity gain. A population-weighted average of the curve over $X_{ic}>\varepsilon$ supplies a
policy summary for residents with substantively positive modeled gains. It is ATT-like for that
prespecified exposed population; it is not an unqualified ATE for metropolitan São Paulo.

## Bottom line

The proposal is worth pursuing as an **adaptation of Butts's identification logic to a different
scalar exposure index**. The fact that every residence has an accessibility change is not an
objection: in the original design, every residence also has a distance to treatment. What matters is
whether some prespecified region of the index is unaffected and can identify the common
counterfactual trend. For distance, that region is an outer interval; for accessibility change, the
proposal is a band around zero.

This is not a continuous-treatment DiD and it should not be interpreted as the marginal causal
effect of one additional unit of accessibility. The intervention is a change in the transport
network, and $\Delta A$ is an exposure mapping that orders locations by how much modeled
connectivity changed. Under the maintained assumptions below, the curve describes how the total
effect of that network intervention varies with this index.

The strongest current strategy is therefore a **paired design** within the required causal DiD:

1. Estimate the primary substantive analysis indexed by
   $X_i=A_{i,t_1}^{(L)}-A_{i,t_0}^{(L)}$, with land use $L$ fixed. This asks whether network
   connectivity correctly maps exposure, recognizing that a station without useful service may
   generate little accessibility gain.
2. Estimate the original-style curve indexed by distance to the relevant newly opened station or
   line segment. This asks whether proximity correctly maps exposure to the intervention and is the
   closest application of Butts's spatial design.
3. Retain both as complementary primary strategies and report a population-weighted summary of the
   accessibility curve for the prespecified positive-gain population. Neither curve is merely a
   robustness check for the other, and disagreement may reveal which channel each exposure mapping
   captures.

The causal DiD framework is fixed, but causal language is earned specification by specification.
With four sparse years, the first task is to validate the exposure contrast, comparison group,
residential assignment rule, pre-trends and support. If the near-zero accessibility band fails,
retain the DiD requirement and move the main causal specification to the distance mapping or another
explicitly defensible DiD comparison; do not replace it with a cross-section or unstructured panel.

## 1. What Butts identifies

Butts considers a treatment introduced at a location between two periods. For unit $i$, distance
to that location, $Dist_i$, is fixed. First-differencing gives a conditional outcome-change curve

$$
E[\Delta Y_i\mid Dist_i=d]=\tau(d)+\lambda(d).
$$

The key local-parallel-trends assumption sets the untreated trend $\lambda(d)$ constant throughout
an outer neighborhood $[0,d_c]$. Partitioning-based least squares then estimates the conditional
mean of $\Delta Y$ in data-driven, quantile-spaced distance bins. The curve is identified only up
to a constant until one additionally assumes that the treatment effect is zero somewhere before
$d_c$; the unaffected outer portion supplies the normalization. Thus the procedure relaxes the
need to know the exact inner-ring cutoff, but it does **not** eliminate the need for an unaffected
comparison region or local parallel trends. See the [published article](https://www.sciencedirect.com/science/article/pii/S0094119022000705),
[author manuscript](https://www.kylebutts.com/files/Rings.pdf), and
[replication repository](https://github.com/kylebutts/Difference-in-Differences-Ring-Method).

This is a local spatial DiD whose original exposure index is distance. Its natural question here is:

> How did labor market outcomes change as a flexible function of baseline residential distance to
> a specified station opening or line expansion, relative to nearby locations far enough away to be
> unaffected but still subject to the same counterfactual trend?

The treatment-effect curve may include accessibility benefits, amenity effects, construction,
displacement and other consequences of proximity. It is a reduced-form effect of the spatially
located intervention, not the causal effect of accessibility itself.

## 2. Accessibility change as the Butts exposure index

Let $A_{it}^{(L)}$ be residential access to a fixed land-use surface $L$, computed from the
travel-time matrix for year $t$. Define the exposure index

$$
X_i(t_0,t_1)=A_{i,t_1}^{(L)}-A_{i,t_0}^{(L)}.
$$

Holding $L$ fixed is an important strength: it prevents changes in the location or total number
of opportunities from mechanically entering the index. The main historical comparison deliberately
allows the rest of the transport network to change. Rail openings, feeder-bus reorganization,
frequency changes and related operational adjustments can be interpreted as one policy package
rather than as contamination, provided the estimand is named accordingly: **the effect of the
observed network transition as indexed by modeled accessibility change**.

The direct analogue of Butts is

$$
E[\Delta Y_i\mid X_i=x]=\tau_A(x)+\lambda_A(x),
$$

where $\tau_A(x)$ is the average total effect of the network transition among locations with
exposure index $x$, and $\lambda_A(x)$ is their counterfactual outcome trend. Let
$B_0=[-\varepsilon,+\varepsilon]$ be a prespecified reference band around zero. The adapted
identification argument requires:

1. **Common counterfactual trends over the analyzed support:** $\lambda_A(x)=\lambda$, possibly
   after conditioning on prespecified baseline covariates or restricting geography.
2. **Null reference band:** $\tau_A(x)=0$ for $x\in B_0$. The mean outcome change in that band
   identifies $\lambda$. The width $\varepsilon$ should be chosen from substantive relevance,
   routing precision and support—not by searching for outcome insignificance.
3. **Exposure-mapping sufficiency:** conditional on $X_i=x$, the curve is a meaningful average of
   the effects of the network transition. It need not imply that accessibility itself is the causal
   treatment, but unmeasured channels that also affect nominally zero-change areas threaten the null
   band.
4. **Stable baseline location and sample:** residence, linkage and inclusion rules must not create
   post-treatment composition differences across $X$.

The estimator can then flexibly estimate $E[\Delta Y\mid X=x]$ using partitioning-based least
squares and subtract the estimated reference-band trend. The original replication code cannot
necessarily be reused unchanged: it normalizes using an unaffected outer-distance region, whereas
the proposed $B_0$ can lie inside a two-sided support.

### All units have an index; not all need be affected

Distance zero, distance ten kilometers and every value between them are observed in the original
design. Analogously, every residence can have an $X_i$, including values close to zero. “Untreated”
here means zero causal effect of the network transition within $B_0$, not absence of a defined
exposure variable. The substantive concern is therefore not universal assignment of $X$, but
whether the zero band is truly unaffected. It may fail if construction, amenity, fare or general-
equilibrium effects reach locations whose computed accessibility barely changes.

### Negative accessibility changes

Negative values are not innocuous controls: they describe losses in modeled connectivity and may
have effects with a different shape. The cleanest initial presentation is three-part:

- estimate the positive-change curve for $X>\varepsilon$, anchored on $B_0$;
- estimate a separate loss curve for $X<-\varepsilon$, also anchored on $B_0$, when support is
  sufficient; and
- report a two-sided flexible curve as a sensitivity analysis.

If the primary analysis excludes negative values, it must define its target population as locations
with nonnegative modeled change and report how much population and geography are removed. Exclusion
should not be justified merely because negative values complicate the expected shape.


## 3. Assessment of the proposed panels

The intended initial labor panels use 2011, 2015, 2019 and 2023. The oldest available GTFS is from
2012, so the 2012 TTM is a deliberate proxy for transport conditions in 2011. The current final TTM
is for 2025 and is not automatically a valid proxy for the 2023 labor endpoint; the causal analysis
needs a 2023 transport scenario or an explicit, substantively defended approximation. Every
specification should distinguish the **outcome year**, **residential reference year**, **TTM scenario
year** and **network-change window**, and record any outcome-to-routing-year approximation.

Opening timing is also part of the intervention definition, not a detail implied by the endpoints.
Using the station counts supplied by the project team and the provisional convention
$t_0<\text{opening year}\leq t_1$, the windows contain:

| Window | Openings by year | Total | Lines represented |
|---|---:|---:|---|
| 2011--2015 | 2013: 1; 2014: 6 | 7 | Rubi: 1; Amarela: 1; Diamante: 2; Lilás: 1; Prata: 2 |
| 2015--2019 | 2017: 3; 2018: 16; 2019: 5 | 24 | Amarela: 3; Jade: 3; Lilás: 10; Prata: 8 |
| 2019--2023 | 2021: 3 | 3 | Amarela: 1; Esmeralda: 2 |
| 2015--2023 | 2017: 3; 2018: 16; 2019: 5; 2021: 3 | 27 | Amarela: 4; Esmeralda: 2; Jade: 3; Lilás: 10; Prata: 8 |

These year-level counts are diagnostic, not final treatment assignments. The five stations listed
with a 2011 opening year require exact dates relative to the baseline residence and outcome reference
dates. The same boundary issue applies to the five 2019 openings when 2019 is an endpoint or baseline.
For every included opening, record the announcement/construction date, service-opening date and
elapsed exposure at the outcome endpoint. Planned or announced openings may require an anticipation
window, and annual labor outcomes may mix months before and after an opening.

Station counts should not be used as equal statistical weights. Each window identifies the effect of
its defined network-transition package for its baseline population. If opening cohorts are summarized,
the weights should follow the estimand--for example, baseline population or prespecified exposed
population--rather than the number of stations. Windows combining openings at different dates also
combine different exposure durations unless cohorts are separated or that heterogeneity is explicit.

Subject to those alignments, the three comparisons have useful but different roles:

| Window | Earliest usable labor pre-period | Main value | Timing-based limitation |
|---|---|---|---|
| 2015--2023 | 2011 | Broad cumulative package with 27 openings and a longer response horizon for the early cohorts | Two opening clusters have very different exposure durations; COVID, secular spatial change and bundled interventions complicate interpretation |
| 2019--2023 | 2011 and 2015 | Narrower package of three 2021 openings is easier to name and may isolate a more coherent transition | Limited geographic/exposure support and only about two years to the endpoint; the response window overlaps the COVID period |
| 2015--2019 | 2011 | Pre-COVID endpoint and the richest opening variation, with 24 openings | Twenty-one openings occur in 2018--2019, leaving little post-opening time; exact 2019 dates and annual-outcome timing are decisive |

A two-period contrast never has an internal pre-trend test. The statement “always have at least one
period for pre-trends” is valid only if the earlier observation is retained in the estimation or a
formally separate placebo exercise. For example:

- For the 2015--2019 effect window, relate the earlier 2011--2015 outcome change to the **future**
  2015--2019 accessibility index. Because seven stations opened in 2013--2014, this is a placebo for
  future exposure, not a literal no-network-change interval.
- For the 2019--2023 window, examine both 2011--2015 and 2015--2019 outcome changes against the future
  2019--2023 index. The latter interval contains 24 openings, so it can reveal persistent spatial
  selection or sequential expansion but is not an untreated network period.
- For the 2015--2023 window, only the 2011--2015 change precedes the intervention window. It still
  contains seven earlier openings, while the 2015--2019 change is already part of the treatment
  package and cannot validate the counterfactual trend for the full window.

These are placebo/pre-trend diagnostics, not proofs of parallel trends. With one pre-change, they
have low power and cannot reveal nonlinear differential trends. More annual observations will
materially improve this part of the design.

### Do not pool the three estimates as repetitions of one estimand

The windows overlap, contain different station openings, embody different treatment durations and
may use different comparison supports. Their coefficients are correlated and need not estimate the
same parameter. Treat them as event/window-specific estimands. A future annual panel should replace
the collection of overlapping long differences with a design that explicitly handles timing,
dynamic effects and treatment histories.

## 4. Recommended empirical sequence

### Stage A — freeze the causal objects

For each window, create a design table before modeling:

- outcome baseline and endpoint;
- TTM baseline and endpoint;
- fixed land-use year and opportunity definition;
- transit changes included in the intervention package;
- stations/segments opened in the window;
- baseline residence rule;
- eligible population and panel-balance rule;
- outer spatial study area;
- prespecified near-zero reference band and broader exposure support.

Use baseline residence as the primary exposure assignment. Updating residence after an opening can
condition on a post-treatment choice and mix access effects with residential sorting. A secondary
analysis can describe effects by contemporaneous residence or explicitly study mobility.

Fix the eligible person-level cohort at baseline and follow its labor outcomes without requiring
continued residence at the same address or reappearance in CadÚnico. Residential mobility is a
potential outcome or mechanism. Restricting to non-movers changes the target population and belongs
in sensitivity analysis. When RAIS supplies the outcome, verify whether nonappearance represents no
formal employment, failed linkage or missingness before coding the extensive margin.

The natural primary outcomes are formal employment and entry into or exit from formalization.
Unconditional formal labor income combines extensive- and intensive-margin responses. Wages, hours
and job quality among employed workers are secondary selected outcomes because treatment may alter
who is formally employed. RAIS alone cannot establish effects on total or informal employment.
Productivity and inclusion are broader study objectives that require their own operational
definitions: an individual wage is not automatically productivity, and an average employment
effect is not by itself evidence of inclusion.

### Stage B — build two primary curves

**Distance curve (faithful Butts benchmark).** For each coherent opening cohort or corridor, use
continuous metric/network distance from baseline residence to the new station/segment, restrict to a
defensible local outer neighborhood, and estimate the partitioning-based curve in first-differenced
outcomes. H3 ring order may be a useful computational index, but metric distance should be the
primary running variable because ring order is discrete and orientation-dependent. Multiple nearby
openings require distance to the newly available network or an explicit exposure mapping, not
distance to the nearest station of any vintage.

**Accessibility-indexed curve.** Estimate $E[\Delta Y\mid X]$ flexibly, identify the common trend
from a prespecified band around $X=0$, and present positive and negative changes separately before
showing a two-sided sensitivity curve. Avoid imposing linearity or converting the index into
arbitrary quartiles for the main result; partitions can be selected by a documented data-driven rule
while the null band itself remains substantively justified and prespecified.

### Stage C — use a factorial feed decomposition

Treat the historical network transition as the primary policy package, including bus rationalization
that follows rail openings. Then construct four internally consistent scenarios when feasible:

| Scenario | Bus network | Rail network |
|---|---|---|
| $N_{00}$ | $t_0$ | $t_0$ |
| $N_{01}$ | $t_0$ | $t_1$ |
| $N_{10}$ | $t_1$ | $t_0$ |
| $N_{11}$ | $t_1$ | $t_1$ |

This yields several transparent accessibility contrasts:

- total observed package: $A(N_{11})-A(N_{00})$;
- rail change holding buses at baseline: $A(N_{01})-A(N_{00})$;
- bus adjustment holding rail at the endpoint:
  $A(N_{11})-A(N_{01})$, corresponding to the proposed comparison of `t1_all` with
  `t0_bus + t1_rail`;
- rail change holding buses at the endpoint: $A(N_{11})-A(N_{10})$; and
- interaction/order dependence: compare the rail and bus contrasts across the two backgrounds.

For one specific line, use the same endpoint background network and add/remove only that line. These
contrasts do not “purify” the historical treatment automatically—the components can interact—but
they show whether the main curve is driven by rail connectivity, bus reorganization or both.

### Stage D — diagnostics that decide whether causal language is warranted

1. Plot index support, mass near zero, negative changes and spatial distribution for every window.
2. Test whether prior outcome changes vary flexibly with the future index; show confidence bands and raw
   support rather than only a joint p-value.
3. Compare baseline covariates, initial outcomes and residential mobility across index ranges.
4. Map residualized exposure and residualized outcome changes to detect broad center-periphery gradients.
5. Re-estimate within narrower local/corridor neighborhoods where common trends are more plausible.
6. Compare fixed-residence and mover-excluded samples, while acknowledging that excluding movers
   changes the target population and can itself select on treatment response.
7. Decompose accessibility changes across alternative network scenarios already contemplated in
   `MEMORY.md` (original, harmonized and conservative bus assumptions).
8. Use spatially appropriate inference and cluster levels tied to treatment assignment/corridors;
   individual-level sample size does not remove spatial dependence.
9. Correctly treat multiple outcomes, windows and curve points as a family of hypotheses; uniform
   confidence bands are more informative than pointwise significance hunting.

## 5. Distance versus accessibility

| Dimension | Distance to new station/segment | Change in accessibility |
|---|---|---|
| Fidelity to Butts (2023) | Direct application | Adapted scalar index with an interior null band |
| Captures network connectivity | Weakly | Directly |
| Exposure construction | Baseline residence and station geometry | Baseline residence and frozen network scenarios |
| Unaffected reference | Outer-distance interval, still assumed | Prespecified band around zero change, still assumed |
| Treatment versions | Station/corridor heterogeneity remains | The network package can generate the same index through different components |
| Policy interpretation | Reduced form of proximity to expansion | Reduced form of network transition indexed by connectivity change |
| Main threat | Endogenous siting and local differential trends | Invalid null band or insufficient exposure mapping |

Agreement in sign and spatial concentration would strengthen the mechanism story but would not prove
that accessibility mediates the distance-based effect. Disagreement can be informative: distance may
capture local amenities/disamenities, while accessibility captures system-wide connectivity.

## 6. Constructive adversarial review

| Challenge | Classification | Failure mechanism and threatened claim | Most informative response |
|---|---|---|---|
| The near-zero band may not be unaffected | Potentially fatal | Construction, amenity or system-wide effects can reach places with $X\approx0$, invalidating the normalization | Prespecify $B_0$, inspect alternative widths, use placebo outcomes/times and confront non-access channels directly |
| Common trends may fail across the accessibility index | Potentially fatal | Locations with large future gains may have distinct untreated labor trajectories | Plot earlier outcome changes against future $X$; restrict geography or condition only on baseline variables when justified |
| The scalar index may be an insufficient exposure mapping | Design-limiting | Equal $\Delta A$ values can summarize different network changes with different effects | Estimate package and component-specific curves; stratify by corridor or intervention cohort |
| Endogenous station placement and sequencing | Design-limiting | Openings target places with distinct labor trajectories | Cohort/corridor local comparisons, planning-history evidence and pre-trend diagnostics |
| Residential sorting and panel composition | Design-limiting and empirically assessable | Treatment changes residence or CadÚnico/RAIS observation, altering who appears at each index value | Baseline residence, baseline-cohort follow-up, attrition/linkage audits and non-mover sensitivity |
| Network interference is the treatment | Design-limiting | One opening changes routes and access far away; no-spillover controls are hard to define | Explicit exposure mapping; do not claim buffers restore SUTVA |
| Sparse years and COVID | Design-limiting | Long differences combine dynamic effects with a major aggregate shock that may vary spatially | Prioritize the 2015--2019 pilot; add annual years; model/report 2019--2023 separately |
| TTM and service-scenario measurement | Empirically assessable | The index changes with bus/runtime/feed assumptions, creating nonclassical error | Factorial scenario decomposition and rank/support stability checks |

The first objection to resolve is **whether the band around zero accessibility change is genuinely
unaffected by the network transition and shares counterfactual trends with the rest of the selected
support**. Without it, the curve remains descriptively useful but its vertical level is not a causal
effect curve for the intervention.

## 7. Code and data-readiness observations

These observations are diagnostic only; no code was changed.


- The current accessibility target still consumes `ttm_bypass`, while the existing four-year plan
  says it should eventually consume the complete year-specific transit matrices. Index construction
  should wait for provenance and scenario-year validation.
- `set_study_area()` manually assigns station groups and keeps the nearest/earliest station after
  overlapping H3 rings. That is acceptable for exploration, but it encodes a single-nearest-station
  exposure mapping for the distance specification; it need not define the accessibility-indexed
  study area and should not be treated as final design code.
- H3 ring order 1--6 defines the present study area. Before applying Butts, convert this into an
  explicit metric outer radius and verify that the outer part plausibly shares counterfactual trends
  and is unaffected.
- `make_panel()` attaches travel times to home and workplace H3 cells. The causal exposure rule
  should be fixed before extending it to accessibility; home accessibility is the natural primary
  treatment, while workplace access is potentially post-treatment and better treated as an outcome
  or mechanism.
- The current balanced-panel helper appears designed around selected observed years and employment
  records. Balancing can change the estimand toward continuously observed/formally linked people;
  both inclusion and attrition rules need an explicit substantive justification.

## 8. Claim calibration

**Planned causal estimand:** the effect of a defined rapid transit network expansion on the labor
outcomes of eligible baseline residents, allowed to vary with modeled accessibility change and
compared with the complementary distance-indexed effect curve.

**Current credibility class:** a causal estimand and design have been specified, but no empirical
causal result is established until the exposure support, comparison region, pre-trends, sample
retention and interference assumptions are evaluated for a concrete intervention window.

**Conditional result claim:** “For eligible residents of the stated baseline population and window,
the defined rapid transit expansion affected the stated labor outcome according to the estimated
accessibility-indexed curve.”

That stronger statement requires: (i) a well-defined network transition and exposure index; (ii) a
credible null band and sufficient support; (iii) untreated trends invariant across the index after any stated
conditioning; (iv) no outcome-relevant endogenous mobility or selection under the chosen estimand;
(v) an explicit interference mapping; and (vi) measurement stability across TTM scenarios.

This design belongs in the CAF policy analysis or longer-term research product, not in the
descriptive executive box.

## 9. Recommended next decision

Before writing estimator code, produce one schema-only “design support” table for each of the three
windows with counts and distributions (subject to disclosure rules) for:

- individuals and baseline H3 cells;
- accessibility change, including negative, near-zero and positive mass;
- metric distance to openings in that window;
- movers, linkage retention and outcome availability;
- earlier outcome changes by future-index partitions;
- overlap of opening-specific local neighborhoods.

Then choose **one pilot window**, preferably 2015--2019 because it avoids the COVID endpoint, and one
coherent opening cohort/corridor. If that pilot has no credible near-zero reference band, retain the
accessibility-indexed curve as descriptive/mechanistic and make the distance curve the primary local
causal DiD. This is a change of exposure mapping inside the fixed causal framework, not permission
to substitute a cross-section or generic panel. If it has good support and flat prior trends,
prespecify the null band, exposure support and package/component contrasts before implementation.

## 10. Literature scope and limitations

The review focused on the paper named in the request, its author manuscript and replication code.
It was not a comprehensive review of transit-accessibility applications or a formal proof that the
partitioning estimator retains all inferential properties after replacing a tail normalization with
an interior reference band. That adaptation should be derived and checked before coding. A later
literature review should examine transit openings, endogenous route siting, residential sorting and
exposure mappings once the pilot intervention cohort is fixed.

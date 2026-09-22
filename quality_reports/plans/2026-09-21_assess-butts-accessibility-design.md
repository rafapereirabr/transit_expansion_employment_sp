# Assess a Butts-style nonparametric design indexed by accessibility change

## Objective

Assess whether the nonparametric treatment-effect curve in Butts (2023) can be indexed by changes
in job accessibility, using a band around zero accessibility change as the unaffected reference
region. Compare that proposal with the original distance-indexed design and clarify what can be
learned from the pilot years before any R implementation.

## Scope

- Separate the executive-box, CAF-report and longer-term research-design claims.
- Reconstruct the estimand and identifying assumptions in Butts (2023).
- Examine the proposed 2015--2025, 2019--2025 and 2015--2019 comparisons, using earlier years only
  as genuine pre-treatment observations when timing permits.
- Record that the 2012 TTM is the necessary proxy for 2011 transport conditions because no older
  GTFS is available; do not treat stale `pilot_years` orchestration as the active design.
- Treat accessibility change as a scalar exposure mapping for the network intervention, not as a
  marginal causal effect of accessibility.
- Assess whether a prespecified band around zero accessibility change can play the same identifying
  role as the unaffected outer-distance region in Butts.
- Compare the accessibility-indexed design with a distance-to-new-station design while retaining
  both as substantively distinct primary specifications.
- Consider factual and counterfactual feed combinations that decompose total network change into
  rail expansion, bus adjustment and their interaction.
- Inspect the current pilot-area and panel code only to identify data/design constraints; do not
  implement or refactor it.
- Do not inspect or expose row-level restricted data.

## Method

1. Read the project records and relevant pipeline code.
2. Review Butts (2023) and its replication materials, focusing on which parts of the identification
   argument depend specifically on distance and which extend to a scalar exposure index.
3. State candidate estimands and the assumptions needed for each.
4. Stress-test timing, treatment construction, comparison groups, sorting, interference and
   measurement.
5. Recommend a staged empirical diagnostic and comparison plan.
6. Save a durable research-ideation report with citations, open questions and a clear verdict.
7. Render the updated report to a self-contained HTML file when the local toolchain permits it.

## Acceptance criteria

- The report characterizes the proposal as an exposure-index adaptation of Butts, not as a
  continuous-treatment design.
- The report states the analogous Butts normalization for an accessibility-change reference band
  around zero and the assumptions needed for that analogy.
- Every proposed panel has explicit baseline, endpoint, exposure window and available pre-period.
- The report explains why a two-period contrast does not itself test pre-trends.
- Negative accessibility changes have an explicit primary and sensitivity-analysis treatment.
- Feed contrasts distinguish the total network package from rail-only and bus-adjustment contrasts.
- Positive features, design limitations and potentially fatal threats are separated.
- Claims are labelled descriptive, associational or causal conditional on named assumptions.
- No estimator is implemented and no existing analytical code is changed.

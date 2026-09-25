# Align the causal study objective and estimand

## Objective

Consolidate the study-level objective and the agreed causal estimand across the canonical project
records. Treat causal difference-in-differences as a fixed design requirement for the CAF study,
while preserving the separate descriptive scope of the immediate executive box.

## Decisions to record

- The scope of an individual consultant's deliverable does not limit the study's empirical scope;
  the project team will build and run the full causal analysis.
- The main causal object is the effect of a rapid transit network expansion on labor market outcomes
  for the vulnerable population resident in the eligible study area at baseline.
- Baseline residence assigns exposure. Subsequent residential mobility is a possible outcome or
  mechanism, not a reason to redefine treatment or restrict the primary sample to non-movers.
- Accessibility change is an exposure mapping for the network intervention, not a continuous causal
  dose. The preferred substantive curve indexes the policy effect by modeled accessibility change.
- Distance to the newly opened station or segment remains a complementary primary mapping and the
  closest application of Butts's spatial DiD.
- Each intervention cohort or time window identifies its own estimand. Overlapping windows are not
  repetitions to be pooled as if they estimated one common effect.
- If the near-zero accessibility band cannot support a causal normalization, retain the causal DiD
  requirement and use the most credible alternative comparison, beginning with the distance-based
  design. Do not replace the design with a cross-section or an unstructured fixed-effects panel.

## Files in scope

- `MEMORY.md`: durable study objective, population, causal contrast and residential assignment rule.
- `BACKLOG.md`: unresolved design, data and diagnostic work needed before estimation.
- `quality_reports/plans/2026-09-21_assess-butts-accessibility-design.md`: align the original plan
  with the fixed causal-DiD requirement and the study-level objective.
- `quality_reports/research_ideation/2026-09-21_butts-accessibility-design.md`: revise the synthesis,
  estimand hierarchy, claim calibration and next decision.
- `quality_reports/session_logs/2026-09-25_causal-estimand-alignment.md`: concise handoff linking the
  canonical records.

## Verification

1. Inspect the final diff and confirm that no analytical R code or confidential document changed.
2. Search the records for contradictory descriptions of accessibility as a causal dose, mover-only
   sampling, or the causal analysis as optional.
3. Render the revised research-ideation report to HTML and confirm that the output opens.
4. Confirm that the source contracts and terms of reference are not referenced as repository
   dependencies and can be removed from the working directory after their non-confidential study
   objectives have been distilled.


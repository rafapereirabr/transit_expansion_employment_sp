# Causal estimand alignment — 2026-09-25

## Decision

- The study-level objective, rather than the division of work across individual consultancy
  deliverables, defines the empirical scope.
- Causal difference-in-differences is a fixed requirement for the CAF employment study. The
  immediate executive box remains a separate descriptive product.
- The primary population is the eligible vulnerable population resident in the study area at
  baseline. Baseline residence assigns exposure; subsequent mobility is an outcome or mechanism.
- The central object is the total effect of a defined rapid transit network expansion, indexed by
  modeled accessibility change. Accessibility is not interpreted as a continuous causal dose.
- Accessibility- and distance-indexed nonparametric spatial DiDs are complementary primary designs.
  If the accessibility null band fails, the fallback remains an explicit causal DiD, beginning with
  a coherent distance-based opening or corridor design.

## Basis and confidentiality

The study-level and technical terms of reference in the working directory were reviewed to separate
the overall research question from individual responsibilities. Only non-confidential methodological
objectives and decisions were distilled into the repository records. Contractual, personal and
commercial details were not copied. The source documents are not pipeline inputs or repository
dependencies and can be removed from the repository directory.

## Records updated

- `MEMORY.md`: durable causal objective, population, exposure interpretation and fallback rule.
- `BACKLOG.md`: concrete data, design, diagnostics, inference and outcome tasks.
- `quality_reports/plans/2026-09-21_assess-butts-accessibility-design.md`: fixed causal-DiD scope.
- `quality_reports/plans/2026-09-25_align-causal-study-estimand.md`: consolidation plan.
- `quality_reports/research_ideation/2026-09-21_butts-accessibility-design.md`: revised estimand
  hierarchy, population rule and claim calibration.

## Verification

- Inspected the local documentation diff and searched the updated records for contradictory
  descriptions of the causal framework, accessibility dose and residential assignment.
- Confirmed that no repository record or pipeline file names the source contracts or terms of
  reference as a dependency.
- Rendered the revised research-ideation report to a self-contained HTML file in the local checkout
  and confirmed that the updated study-level and outcome-limit sections are present.
- `git diff --check` passed in the local checkout. No analytical R code was changed or executed for
  this consolidation.
- The `checkpoint` skill's referenced session-log template was absent; this log follows the
  structure of existing project session logs instead.

## Workspace correction

The Positron project and R session use the local checkout. Earlier documentation edits were applied
to a storage-backed checkout because the Codex environment supplied the stale workspace. The local
checkout is canonical for this work; verify the repository root explicitly before future actions.

## Remaining decisions

- Select one coherent 2015–2019 pilot opening cohort or corridor.
- Establish whether the near-zero accessibility band has adequate support, flat prior trends and a
  defensible no-effect interpretation.
- Define the exact extensive-margin formal-employment outcome after auditing RAIS nonappearance and
  linkage failure.
- Prespecify the no-expansion network scenario and distinguish total-package from component
  contrasts.

## Exact next action

Build one disclosure-safe, schema-level design-support table for the selected pilot window before
implementing either nonparametric estimator.

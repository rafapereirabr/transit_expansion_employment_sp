# Handoff: employment-only preliminary DiD - 2026-11-12

## Current objective and user decision

Resume with y = celetista_31dec only. The user explicitly defers the workplace-quality
investigation. Wage audits remain necessary for future wage outcomes, not for this
employment pilot. Employment must not depend on valid geocoding or wage/hour coverage.

## Completed and verified

User completed the upstream build. Current saved aggregate audits reviewed in
labor_audit_review: annual person/link/geocoder arithmetic and risk-set funnels reconcile.
The report target and validation assertions passed. See
2026-11-12_integrated-audit-review.md for details and the report-generation command.
HTML/RDS outputs remain ignored in restricted output storage; locate with
`targets::tar_read(labor_audit_review)`. No individual records opened in that audit review.
No DiD estimation run. Air unavailable. No commit/push made; worktree remains dirty.
Existing RAIS fixes cover basename-based file selection and exact geocode deduplication;
distinct geocoder conflicts still fail. Preserve all current user/agent changes.

## Gates and uncertainties

- Annual source_coverage_confirmed remains false. This is a configuration/validation
  gate, not proof of incomplete source data. extraction_completed alone is insufficient
  to certify employment zeros; do not replace unmatched NA automatically.
- Baseline sex/race/literacy missingness and treatment/control/status_num/H3 support
  audits are still needed. Missing-covariate policy has not been selected.
- Saved artifacts reconcile, but tar_outdated flagged stale dependencies. Report used
  shortcut=TRUE only to summarize saved snapshots without re-extraction. Inspect which
  dependencies are stale before the next analytical build; do not blindly rerun everything.
- Minimal person-year outcome panel and downstream membership/spec/inference interfaces
  still require execution/verification; existing functions are not proof of readiness.

## Maintained pilot decisions

Anticipation 0 first; 1/3 later. Compare unadjusted and adjusted baseline age/sex/race/
literacy after missingness review. H3-7 baseline inference first; no cross-cohort joint
inference until repeated-person dependence across baselines is addressed. Show available
cohort trajectories, no overall mean yet. Unsupported models suspend with logged reasons.
Alternative controls literally status_num == 5; do not silently substitute other groups.
Prospective treatment timing may use the explicitly approved preliminary approximation,
with focal g for status 2/3; exact address-specific earliest coverage remains deferred.
Spillover activation, Varginha/corridor expansion and L15-local implementation remain
pending per their existing decisions; do not add them as automatic employment-pilot gates.
Read MEMORY.md/BACKLOG.md for the full analytical contract and unresolved implementation.

## Deferred

Investigate annual workplace-geocoding quality only when returning to distance outcomes.
Validate salary units and complete/available-link classifications when returning to wages.
Neither should delay an otherwise valid employment-only pilot.

## Exact next action

Inspect make_rais_spec(), read_rais() and the annual manifest/coverage checks to identify
what evidence is required to confirm the annual source universe. Confirm that evidence
before altering coverage_confirmed_years. Then add the aggregate covariate/support audits
in the canonical DAG, decide the missingness policy with the user, and verify the narrow
employment-panel and first-fit targets. Inspect git status and current diffs first.

## Records

Durable decision: MEMORY.md, Active pilot scope at chat handoff.
Task queue: BACKLOG.md, Next chat: employment-only preliminary DiD.
The skill-referenced session-log template was absent; this checkpoint retains its
required decisions, verification, blockers and exact next-action content.
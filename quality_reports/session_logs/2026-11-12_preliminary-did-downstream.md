# Preliminary DiD downstream — 2026-11-12

## User-run upstream build

User chose to proceed as currently implemented through rais_workplaces and defer
Varginha. Provided a scoped tar_make command for the user to run. This agent did not
run R/targets and did not modify upstream extraction, treatment builders or _targets.R
during this task. Other worktree/index changes made concurrently were preserved.

## Changes

- make_did_spec now includes celetista employment, both wage-threshold candidates,
  hourly wage and workplace distance, with unadjusted and baseline-adjusted variants.
  Covariates are completed age at December 31 of baseline, categorical sex/race/literacy.
  Code maps are explicit and must be checked against the annual harmonized schema.
  Main comparison and existing L6/L17 comparison remain default; generic corridor flag
  can be configured later, but no expanded-corridor assignment was generated here.
- First anticipation is 0; vector scenarios are supported and appear in unique spec IDs.
  Policy audit_pending prevents silently deleting people or inventing unknown categories.
- prepare_did_baseline and audit_did_covariates prepare baseline features and report
  missing/invalid coverage. No confidential data was processed to produce these audits.
- Focal sample checks reject invalid/time-varying exposure and attributes, missing
  references, empty year/group support and wholly disjoint marginal covariate support.
  Other marginal support gaps are reported, not silently trimmed. These checks are not
  proof of conditional parallel trends or sufficient joint overlap.
- Future G beyond the outcome window is retained in membership but recoded to 0 locally
  in the finite fit: this does not assert historical never-treatment.
- Full-cohort missing outcomes still block a fit until coverage/selection is settled;
  selected wage/distance outcomes explicitly use observed outcomes. No extra missing-
  outcome policy was chosen on the user's behalf.
- estimate_did_grid records a status per fit, catches typed suspensions and external
  errors, stores only generic reasons and warning counts, and continues other fits.
  Only focal ATT(g,t) is reported; reference/anticipation/opening/full-year periods
  are distinguished. Both strata must be present for a combined estimate.
- No cross-cohort/time average is computed by the preliminary grid or bootstrap.

## Spatial inference scope

bootstrap_did now samples whole H3-7 baseline cells within each focal cohort, repeating
all people, years, service strata and model variants in the same cell draw. Duplicate
draws receive new person IDs consistently across years. Combined weights are refitted.
The input must supply verified h3_7_baseline; H3-9 is never silently reused as H3-7.
Resolution 6 is supported only by explicit configuration and its matching baseline column.

Cluster support is checked for the actual model samples, not just the whole risk set.
One failed bootstrap series suppresses its own interval, while others can continue.
No failed draws are dropped silently. Simultaneous bands are withheld if any requested
model/series in the cohort is unavailable; pointwise intervals for valid series remain.
The RNG state is restored. Draws/covariance are returned separately for each cohort.

This supplies within-cohort spatial inference, including first/additional combined,
not cross-cohort joint inference. If a person has different baseline cells across risk
sets, independent cell sampling across cohorts is not a valid basis for cross-cohort
contrasts. No cross-cohort covariance, simultaneous band or average is produced.
Additional work is required before reporting those contrasts.

## Verification and next action

Textual inspection and git diff --check passed. No Air, R syntax parse, synthetic test,
empirical target or bootstrap run was performed. Upstream user-run build status was not
inspected or claimed successful.

Before activating DiD: construct audited exposure/membership, prepare outcomes and
baseline H3-7 cells, inspect missing covariates, validate annual category maps, settle
missing cohort outcomes and source coverage. Then format, parse and test synthetic
skips, mixed missingness, reference cells, two strata, repeated cell draws, partial
bootstrap failures and within-cohort covariance. Run a small real-data pilot only then.

Reference consulted: https://bcallaway11.github.io/did/reference/att_gt.html

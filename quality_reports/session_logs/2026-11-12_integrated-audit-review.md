# Integrated audit review - 2026-11-12

Reviewed aggregate artifacts referenced by current dynamic parents, not all historical
files/branches. No individual records, identifiers, workplace rows or addresses were
opened for this review. Applied credible-claims: findings concern measurement and
eligibility, not causal identification or effects.

Added R/labor_audits.R and labor_audit_review in the canonical DAG. The report provides
annual person/link/geocoder reconciliation, salary-unit distributions, sequential
risk-set exclusions, relevant CadUnico snapshots, explicit gates and denominator limits.
Detailed HTML/RDS outputs stay in restricted storage; no audit tables committed here.

Verification: helper/script parsing, expected-year bundles, missing-bundle rejection,
manual percentage denominators, annual salary-unit percentage sums, HTML escaping,
and reconciliation assertions passed. The narrow report target completed successfully
with all error-severity checks passing and both output files present. Air unavailable.
The first guarded build stopped because tar_outdated reported upstream stale targets.
Then ran tar_make(names = tidyselect::all_of("labor_audit_review"), shortcut = TRUE,
callr_function = NULL): one target completed, no extraction, routing or DiD executed.
This reviews saved snapshots and does not certify upstream code/source freshness.
A historical orphan error branch was excluded from the current-child status assessment.

Next: validate annual source coverage for employment zeros; validate wage units and
complete/available classifications; investigate annual geocoder deviation differences;
add covariate missingness and treatment/control/H3 support audits. No analytical
coverage flags, eligibility rules or estimates were changed by this review.
Final validation caught a scalar-denominator recycling bug in the report's salary-unit
percentages; corrected the ratio helper and repeated the per-year 100-percent sums.
Also escaped non-ASCII R string literals for portability under this Windows locale.
These corrections affect the new audit report only, not source data or outcomes.
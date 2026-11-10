---
name: review-r
description: Review R code and targets-pipeline changes for correctness, reproducibility, spatial/data risks, and maintainability. Use for read-only review; do not fix unless asked.
---

# Review R

1. Read `AGENTS.md`, `.agents/rules/r-code.md`, and `.agents/rules/targets-verification.md`.
2. Inspect the requested files, their callers in `_targets.R`, relevant tests/audits, and the diff.
3. Prioritize findings by severity: correctness/data loss, confidentiality, broken DAG or
   reproducibility, methodological validity, performance, then maintainability.
4. Check join cardinality, filters, missingness, units, CRS, service dates/time zones, Arrow
   collection, target dependencies, file targets, stochastic seeds, and output schemas as relevant.
5. Cite file and line. Explain the failure mode and smallest credible remediation.
6. Report verification that was actually run. If none was run, say so.

Return findings first. Do not edit code, rebuild long targets, or change analytical choices during a
review unless the user explicitly expands the task.

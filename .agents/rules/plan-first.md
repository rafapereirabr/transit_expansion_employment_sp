# Plan-first rule

For a task that changes multiple files, changes analytical assumptions, or may take substantial
time, create `quality_reports/plans/YYYY-MM-DD_short-description.qmd` before implementation. The
QMD source is canonical; render a self-contained HTML copy when useful for circulation. Historical
Markdown plans remain valid and do not need conversion.

A plan contains: objective, known context, assumptions, files or targets affected, ordered steps,
verification, and any decision that requires user approval. Check `MEMORY.md` first. Mark the plan
`DRAFT`, `APPROVED`, or `COMPLETED`; do not treat plan approval as commit authorization.

Simple questions, read-only inspection, and obvious single-line fixes do not need a plan.

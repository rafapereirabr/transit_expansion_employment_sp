# R and spatial-pipeline rule

- Use relative paths, `TRUE`/`FALSE`, explicit namespaces where ambiguity matters, and documented
  seeds for stochastic work.
- Put reusable functions in `R/` and orchestration in `_targets.R`.
- Do not edit generated `_targets_packages.R`.
- Make join keys and intended cardinality explicit; check row counts and duplicates around
  CadÚnico–RAIS and spatial joins.
- Make CRS, H3 resolution, service date, time zone, analysis year, and units explicit.
- Preserve lazy Arrow/DuckDB execution where practical; avoid accidental full-data collection.
- Document assumptions and reasons. Avoid comments that merely translate code into English.
- Format touched R files with Air when available, then verify the affected targets.

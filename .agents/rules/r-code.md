# R and spatial-pipeline rule

- Use relative paths, `TRUE`/`FALSE`, explicit namespaces where ambiguity matters, and documented
  seeds for stochastic work.
- Put reusable functions in `R/` and orchestration in `_targets.R`.
- Do not edit generated `_targets_packages.R`.
- Prefer Arrow, `dplyr`/the tidy ecosystem, and DuckDB/`duckspatial` where they make the solution
  clear and efficient. Use base R, `data.table`, or other tools when they are more natural; avoid
  unnecessary complexity just to remain inside the preferred stack.
- Make join keys and intended cardinality explicit; check row counts and duplicates around
  CadÚnico–RAIS and spatial joins.
- Make CRS, H3 resolution, service date, time zone, analysis year, and units explicit.
- Preserve lazy Arrow/DuckDB execution where practical; avoid accidental full-data collection.
- Document assumptions and reasons. Avoid comments that merely translate code into English.
- Format touched R files with `air format .` from the repository root so the checked-in `air.toml`
  is used. Do not replace it with generated or generic defaults. Then verify the affected targets.

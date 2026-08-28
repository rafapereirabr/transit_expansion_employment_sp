# Project Memory

Corrections and learned facts that persist across sessions, specific to `aoplanduse`.
When a mistake is corrected, or a non-obvious approach is confirmed, append a `[LEARN:category]` entry below.

---

<!-- Append new entries below. Most recent at bottom. -->

## Jobs Pipeline (RAIS) — detailed review, 2026-08-24

Found while writing the detailed `AGENTS.md` "Pipeline main steps > jobs (RAIS)" section (`get_rais()` → `aggreg_rais_h3()` → `summarise_jobs()` + `adjust_rais_outliers()`, all in `R/jobs.R` / `R/adjust_rais_outliers.R`). Full narrative lives in `AGENTS.md`; entries below are the parts most likely to bite someone who only skims the code.

[LEARN:jobs-pipeline] **RESOLVED 2026-08-24.** `adjust_rais_outliers()` computed an outlier adjustment (`dynamic`/`sharp_95`) but the line that would overwrite `qt_vinc_ativos` with the adjusted value was commented out (`R/adjust_rais_outliers.R:135`) — `adjust_outliers = TRUE` tagged outliers via `adjusted` (0/1) but never actually corrected them. Fixed by uncommenting that line: `qt_vinc_ativos` is now overwritten with `dynamic` when `adjusted == 1`, left as-is otherwise; `sharp_95` remains computed but unused (diagnostic only). Any `rais_agg` output built **before** 2026-08-24 was NOT outlier-corrected despite the parameter name — don't treat pre-fix parquet outputs in `output/rais/` as adjusted. `targets` tracks the function body of sourced `R/` functions as part of each target's hash, so a plain `targets::tar_make()` will detect this edit and automatically rebuild `rais_agg` (and anything downstream) — no manual `tar_invalidate()` needed.

[LEARN:jobs-pipeline] H3 resolution 9 is the finest resolution `ipeadatalake::adicionar_geoloc()` can produce. Coarser resolutions are derived downstream via `h3o::get_parents()`. Requesting a resolution finer than 9 doesn't error — it just falls back with a `message()`, easy to miss.

[LEARN:jobs-pipeline] The geographic filter (join to `munis`) and the legal-nature filter applied in `get_rais()` are not reapplied inside `summarise_jobs()`'s bottom-up vínculos reload — they propagate implicitly only because the vínculos data is `inner_join`ed against the already-filtered estabelecimentos. If that join ever changes to a different join type, the filters silently stop applying.

[LEARN:jobs-pipeline] `adjust_rais_outliers()`'s percentile calculation is grouped **only by `cat`** (the CNAE division/group flagged as problematic) — nationally, within the year. It is NOT localized by hexagon or município, even though it's called from inside a per-hex aggregation pipeline. Easy to wrongly assume the outlier cutoff is hex-local.

[LEARN:jobs-pipeline] Composition columns coming out of `summarise_jobs()` (`industry_*`, `educ_*`) are establishment-level **shares** (sum to 1 per establishment), not counts. Hex-level aggregation must use `wtd_mean()` weighted by `qt_vinc_ativos` — never a plain `mean()`. (Same rule as `.agents/rules/r-code-conventions.md` §3/§6.)

[LEARN:workflow] `AGENTS.md`'s "Pipeline main steps" section is meant to carry full function-level detail per domain, not a high-level list — non-obvious defaults/params, implicit filter propagation across steps, and any bugs/WIP states found during review get flagged explicitly (⚠️) in place, not silently fixed or omitted. Established during the jobs-pipeline review; apply the same depth to `schools` next, then CNES/CRAS once those are implemented.

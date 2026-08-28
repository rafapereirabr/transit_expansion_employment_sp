---
name: capture-environment
description: Capture and check the existing R/renv environment for reproducibility and handoff without upgrading packages or rewriting analytical code.
---

# Capture environment

1. Inspect `renv.lock`, `.Rprofile`, `renv/settings.json`, R version, and `renv::status()`.
2. Record `sessionInfo()` to a non-confidential project artifact agreed with the user, normally
   `quality_reports/environment/sessionInfo.txt`.
3. Report lockfile/library drift, unavailable repositories, system dependencies, Java/R5
   requirements, and relevant environment variables without printing secrets.
4. Do not call `renv::snapshot()`, `renv::restore()`, upgrade packages, or edit `renv.lock` without
   explicit authorization; these operations can materially change the environment.
5. If a clean restore is requested, perform it in a disposable environment and report the result.

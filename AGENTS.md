# AGENTS.md

## Project

**Title:** Urban Transport and Employment — the case of São Paulo

**Members:** Gregório Luz (UC Berkeley), Rafael Pereira (Ipea), and Arthur Bazolli (Ipea), in partnership with CAF

**Objective:** study how rapid-transit expansion in São Paulo affects labor-market outcomes,
especially for people in socioeconomic vulnerability.

CadÚnico is the main empirical source because it supplies residential locations and repeated
observations for vulnerable households. Linking CadÚnico to RAIS adds formal employment histories,
workplace locations, earnings, hours, and establishment information. Metro/CPTM station openings
and reconstructed transit networks define changes in exposure and accessibility.

## Deliverables and horizons

1. **Immediate — executive box:** descriptive statistics and accessibility/GTFS evidence, without
   causal or DiD claims.
2. **Medium term — CAF report (Dec 2026–Jan 2027):** credible policy analysis of employment and
   formalization effects. A conventional spatial cutoff design is acceptable if a better design is
   not ready in time.
3. **Longer term — research design:** a stronger strategy for interference/SUTVA, spatial spillovers,
   accessibility, transit feeds, and staggered treatment timing.

Do not collapse these products into one estimand or apply publication-level causal claims to the
descriptive executive box.

## Canonical project records

- `AGENTS.md`: stable context, operating rules, commands, and verification expectations.
- `MEMORY.md`: durable decisions, findings, and `[LEARN:category] wrong → right` corrections.
- `BACKLOG.md`: unfinished work, organized by deliverable or technical dependency.
- `CHANGELOG.md`: changes to the agents/workflow infrastructure only.
- `quality_reports/plans/`: plans for non-trivial tasks.
- `quality_reports/session_logs/`: decisions, alternatives, blockers, and handoff context.

The `targets` DAG (`_targets.R` plus `R/*.R`) is the source of truth for what the pipeline builds.
Do not maintain a second canonical script or output outside the DAG. `sidequests/` is exploratory;
promote work into the DAG only after the approach is accepted.

## Workflow

- Read relevant parts of `MEMORY.md` and `BACKLOG.md` before substantial work.
- For non-trivial changes, save a plan to
  `quality_reports/plans/YYYY-MM-DD_short-description.md` before implementation.
- Preserve user changes in a dirty worktree. Inspect `git status` and the relevant diff first.
- Implement, verify, and report. Do not commit, push, open a PR, or merge without explicit user
  authorization.
- Record durable corrections in `MEMORY.md`; record pending work in `BACKLOG.md`, not memory.
- Use the shared rules and skills under `.agents/`. They are written to be model-agnostic.

## Data security

CadÚnico and linked RAIS microdata are restricted administrative data.

- Never commit raw or row-level confidential data, identifiers, addresses, secrets, or absolute
  machine paths.
- Do not upload restricted data to external services or web tools.
- Use synthetic examples or schema-only excerpts when external assistance is necessary.
- Treat tables, maps, and fine-geography outputs derived from restricted data as potentially
  disclosive until cleared under the applicable agreement.
- Paths to restricted storage are machine-local and must not enter committed documentation.

## Repository structure

```text
.
├── _targets.R              # Pipeline DAG
├── _targets_packages.R     # Generated package bootstrap; do not edit by hand
├── R/                      # Functions loaded by tar_source()
├── data-raw/               # Reference and transport inputs; most large inputs are ignored
├── data/                   # Derived/local data; ignored except explicit reference files
├── figures/                # Generated figures
├── sidequests/             # Exploratory scripts and diagnostics
├── .agents/                # Shared rules, skills, and workflow guide
├── quality_reports/        # Plans and session handoffs
├── MEMORY.md
├── BACKLOG.md
└── CHANGELOG.md
```

## Commands

```r
renv::restore()
targets::tar_outdated()
targets::tar_make()
targets::tar_make(names = tidyselect::any_of(c("target_name")))
targets::tar_read(target_name)
```

```bash
air format .
```

`_wizard.R` contains interactive shortcuts for common `targets` operations.

## Verification

After changing `_targets.R` or `R/*.R`:

1. Identify the affected target and downstream scope.
2. Run the narrowest meaningful `targets::tar_make()` scope.
3. Confirm expected target status and output files.
4. Run broader downstream targets when the interface, schema, spatial unit, or assumptions changed.
5. Report commands run, outputs checked, failures, and anything not run because of runtime or data
   restrictions.

Do not claim verification from code inspection alone. Long R5 routing jobs may take many hours;
use a smoke test when available and state clearly when a full run remains pending.

## R conventions

- Use relative paths and `TRUE`/`FALSE`, not `T`/`F`.
- Keep reusable logic in documented functions under `R/`; keep orchestration in `_targets.R`.
- Do not edit `_targets_packages.R` by hand.
- Prefer Arrow, `dplyr` and the broader tidy ecosystem, and DuckDB/`duckspatial` when they provide a
  clear and efficient solution, especially for large or spatial data. This is a preference, not a
  prohibition: use base R, `data.table`, or another appropriate tool when it is simpler, clearer,
  or better suited to the task. Do not contort code merely to stay within the preferred stack.
- Preserve `targets` dependency tracking: pass files and parameters as target dependencies instead
  of hiding them in global state.
- Set and document seeds for stochastic work.
- Make joins explicit and check cardinality when linking CadÚnico, RAIS, spatial units, or GTFS.
- Treat CRS, time zone, service date, analysis year, and spatial resolution as explicit data.
- Comments should explain assumptions and reasons, not restate syntax.
- After R edits, run `air format .` from the repository root so Air discovers and applies the local
  `air.toml` (tabs, 101-character line width, and the project's other settings). Do not generate a
  new Air configuration or rely on generic defaults in place of this file.

## Methodological guardrails

- Current accessibility matrices compare 2012 and 2025 morning-peak transit conditions at 06:50,
  with a 15-minute departure window and 60-minute maximum trip.
- CadÚnico residential exposure is conceptually distinct from RAIS workplace exposure.
- Descriptive accessibility changes are not causal employment effects.
- Treatment cutoffs, exclusion buffers, distance bands, not-yet-treated controls, residential
  mobility, COVID-era heterogeneity, anticipation, and spatial spillovers require explicit
  sensitivity analysis.
- Do not assume no interference merely because a buffer was applied; state the maintained
  assumption and the geography it covers.

## Shared skills

- `review-r`: read-only R and pipeline review.
- `diagnose`: root-cause a failed or incorrect target.
- `interview-me`: turn an ambiguous research request into an approved specification.
- `capture-environment`: record the reproducible R environment without upgrading it.
- `checkpoint`: write a compact cross-session handoff.
- `commit`: verify and commit only after explicit authorization.

See `.agents/WORKFLOW_QUICK_REF.md` for usage by Codex and Claude.

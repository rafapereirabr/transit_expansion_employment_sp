# Labor pipeline checkpoint — 2026-11-05

## Purpose

Consolidate the methodological decisions, pipeline refactor and current implementation boundary
from the long labor-design session. This checkpoint contains no restricted rows, identifiers or
machine-specific storage paths.

## Durable decisions

- The main estimand is ITT assigned from residence at `g - 4`; people already near older rapid
  transit remain eligible and define the `additional` service stratum. `first` and `additional`
  treatment are estimated separately and may also be summarized as `combined`.
- Opening events are line × `dt_trial_svc` packages. `did::att_gt()` still receives calendar
  treatment years; qualitative event identity belongs to the surrounding risk-set branch, not its
  `gname` argument.
- Risk sets are cohort-specific. Eligibility and residence are fixed at `g - 4`, assignment is
  absorbing within the 2015--2023 study window, and controls are eligible not-yet-treated RMSP
  residents. Nationwide RAIS linkage is not filtered by workplace municipality.
- Primary eligibility is age 22--60 on the opening package's trial-service date, a valid CPF and a
  usable baseline geocode, with no additional income cutoff.
- The active outcomes are `formal_31dec`, `formal_above`, `hourly_wage` and `work_dist_km`.
- The design comparisons currently represented in `did_spec` are `main`, `no_2_4km` and `l6_l17`.
  A stricter large-accessibility-gain spillover rule and fictitious corridor placebos remain future
  decisions rather than active specifications.
- Routing and accessibility targets are intentionally dormant while the annual population, opening
  events and risk sets are built. The existing routing code is preserved for later reactivation.

## Implemented state

- `_targets.R` now keeps the population block together: `family_records`, dynamically branched
  `person_files`, and the `person_records`/`person_audit` file projections. The obsolete early
  `person_roster`, `rais_eoy`/panel path and related fossils are not part of the active DAG.
- `R/read_cadunico.R` uses harmonized `cpf_recuperado` as canonical, validates CPF format and check
  digits, assigns mutually exclusive exclusion reasons, and writes annual aggregate audit sidecars.
  Historical optional person fields are selected only when present because the harmonized schema
  varies by year.
- `family_records` completed, and the disclosure-safe audit of `year × co_familiar_fam` returned
  zero duplicate groups. No family-year arbitration rule is required.
- A person-branch schema error reported during the session was resolved by the user and is not an
  outstanding blocker. Completion and aggregate outputs of all annual branches still need checking.
- `R/did.R` contains the compact model table and helpers to prepare one estimator sample, call
  `did::att_gt()`, extract the focal cohort ATT and form a treated-count-weighted `combined` point
  estimate. Combined uncertainty is explicitly marked pending. The `did` package and dependencies
  are present in `renv.lock`.
- The station pipeline separates reproducible station geometry, the provenance workbook and the
  curated opening catalog. The curated catalog now has dates/sources for 2015--2023 openings; the
  2014 Line 8 reconstruction/reopening is excluded from the main pooled design, Engenheiro Goulart
  is additional service, and Ambuitá's 2025 opening is outside the analysis window.

## Verification completed

- `family_records` was built successfully by the user.
- The family-year duplicate audit returned zero rows.
- Earlier code-level checks confirmed the active population target names and the compact DiD helper
  interface; a synthetic first/additional combination produced the expected treated-count-weighted
  point estimate.
- `targets::tar_manifest()` succeeded after the active target-list separator was corrected.
- No full estimator run against restricted data has been performed. No full annual person-target
  completion was claimed in this checkpoint.
- Air was unavailable in the environment during the earlier edits, so project-wide formatting has
  not been verified with `air format .`.

## Records and plans

- Durable decisions and confirmed findings are in `MEMORY.md`.
- Outstanding implementation is in `BACKLOG.md` under “Current implementation sequence”.
- The model skeleton is described in
  `quality_reports/plans/2026-11-03_labor-did-matryoshka-skeleton.qmd`.
- Station provenance work is described in
  `quality_reports/plans/2026-11-04_station-opening-provenance.qmd`.

## Exact next action

Complete the annual person branches and inspect their aggregate audits:

```r
targets::tar_make(
  names = tidyselect::any_of(c('person_records', 'person_audit'))
)
```

If all branches complete, summarize `person_audit` by year and exclusion reason and audit geocode
coverage. Then implement `opening_events`; do not begin RAIS extraction before outcome-free
`risk_sets` and their `cpf_roster` exist.

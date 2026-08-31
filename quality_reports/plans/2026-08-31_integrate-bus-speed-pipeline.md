# Integrate the bus-speed model into the targets pipeline

## Problem and scope

`bus_speed_surface` currently declares an already-generated exploratory CSV under
`sidequests/check_busways_output/` as a file target. The file is produced outside the DAG by two
scripts:

1. `sidequests/bus_speed_surface.R` discovers the 2015--2017 SPTrans archives and extracts valid
   morning-peak interstop segments.
2. `sidequests/check_busways.R` classifies those segments by busway infrastructure and estimates
   the conditional H3-8 speed surface consumed by `write_bus_feeds()`.

This makes the corrected bus feeds depend on undeclared code, inputs, parameters and intermediate
files. The promotion should cover the production logic from those two scripts. Other sidequests
are audits or experiments and should remain outside the DAG unless a pipeline target actually
consumes their results.

## Intended DAG

```text
historical GTFS paths + bus-speed specification
  -> reference feed inventory
  -> HPM segment branches
  -> combined HPM segments

raw busway paths + classification specification + one reference GTFS
  -> dated busway geometry
  -> reference segment index
  -> segment-to-busway matches

combined HPM segments + segment-to-busway matches + model specification
  -> conditional bus-speed surface
  -> bus feeds
```

The main production chain should contain only data targets. Maps and tuning diagnostics should be
downstream diagnostic targets so that changing a figure does not invalidate `bus_feeds`.

## Implementation plan

### 1. Make assumptions and raw inputs explicit

- Add a compact `bus_speed_spec` target containing the reference years (2015--2017), 06:00--07:00
  extraction window, H3 resolution 8, segment length and speed filters, reference feed/date,
  projected CRS, 25 m corridor buffer, 60% minimum overlap, minimum five observations per
  feed-year-cell, shrinkage prior of 50 observations and three reference years.
- Declare the exact historical GTFS ZIPs selected for 2015--2017 as a `format = "file"` target.
  Build the inventory deterministically from these declared paths and parsed dates; fail on missing,
  duplicated or out-of-range dates. Do not let an untracked `list.files()` result silently change the
  model.
- Keep `data-raw/busways.gpkg` and `data-raw/mobilidados_2025.zip` in the existing
  `raw_busway_paths` file target. Make the selected reference GTFS an explicit dependency rather
  than repeating its path inside a function.
- Validate the specification early: reference years and feeds agree, the reference feed belongs to
  the inventory, time and filter bounds are ordered, and all required raw files exist.

### 2. Promote reusable production code into `R/`

- Create a focused module such as `R/bus_speeds.R`; move and document the production functions from
  both sidequests. Keep orchestration and literal paths in `_targets.R`.
- Consolidate duplicate helpers with existing pipeline code where their semantics match:
  `gtfs_time_to_seconds()`, active-service selection, H3 conversion, distance calculation, busway
  reading and segment classification. Avoid retaining parallel implementations that can drift.
- Split the work into pure functions with explicit arguments and tabular return values:
  inventory construction, HPM segment extraction, segment diagnostics, dated busway preparation,
  segment geometry construction, busway matching and conditional-surface estimation.
- Preserve the current statistical construction exactly during the first migration: median within
  feed, then year, then across years; infrastructure classes; support rules; and shrinkage toward
  class-wide medians. Refactoring the estimator should be a separate, reviewable change.
- Add assertions for required GTFS tables/columns, unique segment lookup keys, join cardinality,
  finite coordinates and speeds, allowed segregation classes, nonempty reference-year coverage and
  a unique `(h3, segregation)` key in the final surface.

### 3. Declare granular targets stored as Parquet

- Dynamically branch HPM extraction over the historical feed inventory so that adding or replacing
  one archive rebuilds only its branch. Return segment data and diagnostics separately, then combine
  the branches.
- Use the repository's default `format = "parquet"` for the in-store tabular targets. Do not create
  CSVs or parallel `format = "file"` copies for pipeline inspection.
- Represent `reference_feed_inventory`, `hpm_segments`, `segment_busway_matches`,
  `bus_speed_surface` and `feed_diagnostics` as ordinary data targets with
  `format = "parquet"`. Let `targets` manage their storage and retrieval instead of assigning stable
  paths under `data/` and calling `write_parquet_target()`.
- Keep geometry-bearing targets compatible with GeoParquet storage. For the production match table,
  retain only stable keys, class and overlap after geometry has served its purpose.
- Pass the validated `bus_speed_surface` table directly to `write_bus_feeds()`. Remove the
  path-oriented `read_bus_speed_surface()` boundary, or retain a more general validator that accepts
  a table without writing and reading it again.
- Add a separately named export target only in the future if another workflow needs a stable file
  outside the target store. Such an export would be downstream of the model and would not be read
  back by `bus_feeds`.

### 4. Separate production outputs from diagnostics

- Promote diagnostics that protect the estimator: feed coverage and invalid-segment shares,
  buffer-match counts, class coverage, fallback shares, support by year and key uniqueness.
- Express these as data targets and add a validation target that fails on structural errors and
  reports threshold warnings without contaminating the model table.
- Rebuild the useful maps from target data under the single directory `figures/diagnostics/`:
  conditional speed, observational coverage, temporal stability and busway classification. Figures
  are `format = "file"`; their source tables remain in-store Parquet targets.
- Leave tuning-only outputs (the 15/25/40 m buffer comparison, alternative speed-factor tables and
  the single-2015 exploratory summaries) outside the production path: they may depend on production
  targets, but neither the final surface nor `bus_feeds` should depend on them. Retain them as
  explicitly named diagnostic targets where they remain useful for methodological review.

### 5. Cut over safely and retire the hidden dependency

- First build the new surface alongside the legacy CSV and compare schemas, keys, row counts,
  class counts, numeric summaries and row-level values within a documented tolerance.
- Compare a small deterministic sample of regularized GTFS trips produced with the old and new
  surfaces: stop ordering, first departure, runtime, modeled speed class and resulting arrival and
  departure times.
- After equivalence is established, point `bus_feeds` only to the generated target and remove the
  `sidequests/check_busways_output/...csv` declaration from `_targets.R`.
- Keep the two sidequest scripts temporarily as clearly marked historical wrappers or replace their
  bodies with calls that read target outputs for interactive diagnostics. Delete them only in a
  later cleanup after confirming they contain no unique analysis worth retaining.
- Record the completed promotion and any intentional estimator changes in `MEMORY.md`; remove or
  update any corresponding pending item in `BACKLOG.md`.

## Verification

1. Run `air format .` after the R and `_targets.R` edits.
2. Inspect the graph and invalidation with `targets::tar_outdated()`; confirm that raw historical
   feeds and busway inputs are ancestors of the surface and that no `sidequests/` path is an ancestor
   of `bus_feeds`.
3. Build the narrow chain through the surface and its validation target. Read the five in-store
   Parquet targets and check their schemas, keys and diagnostic thresholds.
4. Run the legacy/new equivalence checks before removing the CSV dependency.
5. Build `bus_feeds` and the narrow scenario-feed audits for 2012 and 2025. Inspect GTFS validator
   results and deterministic trip samples.
6. Because the final change affects bus runtimes, run the relevant downstream routing smoke test.
   A full R5/accessibility rebuild remains required before final matrices are treated as verified;
   report explicitly if it is deferred because of runtime.

## Acceptance criteria

- No target consumed by the production pipeline reads from `sidequests/`.
- Every raw input, parameter and transformation used to estimate bus speeds is represented in the
  DAG and invalidates the appropriate downstream targets.
- All tabular targets in the promoted chain use `format = "parquet"`; none is declared as
  `format = "file"`, and no new CSV is created.
- Diagnostic figures are written under `figures/diagnostics/` and do not invalidate or gate the
  production of the speed surface and bus feeds.
- The final surface has the expected schema, positive finite speeds and a unique
  `(h3, segregation)` key, with documented fallback coverage.
- The migrated estimator and regularized feed output match the legacy implementation within the
  agreed tolerance, or every difference is explained and approved as a methodological change.
- Narrow feed validation and routing smoke tests pass, and any unrun full rebuild is clearly noted.

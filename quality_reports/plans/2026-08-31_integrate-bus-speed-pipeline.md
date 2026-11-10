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
original historical RAR + bus-speed specification
  -> automatically selected/extracted 2015-2017 SPTrans ZIPs
  -> reference feed inventory

reference feed inventory + raw busways + specification
  -> compact bus-speed model
  -> conditional bus-speed surface
     -> bus feeds
     -> diagnostic figures
```

The model target combines transient segment extraction, spatial classification, validation and
diagnostic summaries. These steps take about one minute together, so separate target boundaries add
more graph complexity than useful caching. Maps remain downstream so figure changes do not
invalidate `bus_feeds`.

## Implementation plan

### 1. Make assumptions and raw inputs explicit

- Add a compact `bus_speed_spec` target containing the reference years (2015--2017), 06:00--07:00
  extraction window, H3 resolution 8, segment length and speed filters, reference feed/date,
  projected CRS, 25 m corridor buffer, 60% minimum overlap, minimum five observations per
  feed-year-cell, shrinkage prior of 50 observations and three reference years.
- Declare `data-raw/3550308_sao_paulo.rar` as the original `format = "file"` input. Discover SPTrans
  members from its archive table, parse dates from their names, select the configured reference
  years and extract the selected ZIPs under `data/gtfs/history/`.
- Build a portable Parquet inventory from the extracted filenames, including relative feed paths,
  parsed dates, audit dates and the relative path of the source RAR. Fail on missing, duplicated or
  out-of-range dates.
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

### 3. Keep only useful target boundaries

- Keep six operational targets: source RAR, extracted ZIP paths, Parquet inventory, compact RDS
  model, Parquet surface and diagnostic figures.
- Run HPM extraction, feed diagnostics, dated busway preparation, segment geometry, spatial matching
  and validation inside `bus_speed_model`. Return only compact results needed for
  inspection or downstream figures; do not retain the million-row segment table in the target store.
- Store `reference_feed_inventory` and `bus_speed_surface` with the default `format = "parquet"`.
  Use RDS only for `bus_speed_model`, whose heterogeneous result contains tables and `sf` geometry.
- Pass the validated `bus_speed_surface` table directly to `write_bus_feeds()`.

### 4. Separate production outputs from diagnostics

- Promote diagnostics that protect the estimator: feed coverage and invalid-segment shares,
  buffer-match counts, class coverage, fallback shares, support by year and key uniqueness.
- Retain these as named components of `bus_speed_model`; validation runs before that target can
  complete, while diagnostics remain accessible with `tar_read(bus_speed_model)`.
- Rebuild the useful maps from target data under the single directory `figures/diagnostics/`:
  conditional speed, observational coverage, temporal stability and busway classification. Figures
  are `format = "file"`; their source tables remain in-store Parquet targets.
- Leave tuning-only outputs (the 15/25/40 m buffer comparison, alternative speed-factor tables and
  the single-2015 exploratory summaries) outside the DAG. Keep the buffer-comparison function and a
  short commented example with the tested values in `R/bus_speeds.R`, but do not create targets or
  stored outputs for this small one-off choice.

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
3. Build the narrow chain through `bus_speed_model` and the surface. Inspect the inventory, model
   validation, counts and feed diagnostics.
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
- The inventory and production surface use `format = "parquet"`; the heterogeneous compact model
  uses RDS, raw/extracted archives use file targets, and no CSV is created.
- Diagnostic figures are written under `figures/diagnostics/` and do not invalidate or gate the
  production of the speed surface and bus feeds.
- The final surface has the expected schema, positive finite speeds and a unique
  `(h3, segregation)` key, with documented fallback coverage.
- The migrated estimator and regularized feed output match the legacy implementation within the
  agreed tolerance, or every difference is explained and approved as a methodological change.
- Narrow feed validation and routing smoke tests pass, and any unrun full rebuild is clearly noted.

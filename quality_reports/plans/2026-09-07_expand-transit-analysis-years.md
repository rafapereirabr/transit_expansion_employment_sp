# Expand transit analysis to 2012, 2015, 2019 and 2025

## Objective

Extend the transit scenarios from the former 2012/2025 comparison to four analysis years without
silently borrowing anachronistic rail topology. Build and validate all scenario feeds before any
expensive R5 network or travel-time calculation starts.

The immediate failure is the 2015 rail branch: its January SPTrans template has no Line 15 route,
while the October 2015 scenario correctly expects Line 15 to operate. The 2019 template contains
Line 15 but includes stations that were not available in 2015, so it cannot replace the full 2015
template.

## Decisions

- Keep the same-year SPTrans feed as the default rail-topology template.
- Add an explicit line-level template override only for canonical Line 15 in 2015, sourcing its
  route/trip/shape topology from the 2019 feed.
- Restrict the borrowed 2019 Line 15 stop sequence to the 2015 operating segment between Vila
  Prudente and Oratório, in both directions.
- Represent the override and historical terminal restriction as declarative data in the DAG, not as
  hidden conditionals or permissive matching inside `rail_route_crosswalk()`.
- Preserve the standardized runtime/headway scenario already encoded in `rail_service_spec`; this
  task changes topology selection and year coverage, not the operating-assumption methodology.
- Keep the four scenario feeds in one shared R5 network with non-overlapping service calendars, but
  do not rebuild that network until feed audits and validator reports pass for every year.
- Use the Wednesday in the first week of October as the representative analysis date in every year:
  2012-10-03, 2015-10-07, 2019-10-02 and 2025-10-01.
- Retain accessibility levels for all four years. Define changes explicitly rather than treating
  every non-2012 observation as a generic post period.
- Keep `ttm_bypass` declared for possible interactive use, but make no pipeline target depend on it;
  the accessibility target should consume `ttm_transit_all` directly.
- Defer all accessibility-plot changes. This task may adapt the accessibility data calculation, but
  it must not modify `plot_access()` or rebuild `access_plot`.

## Implementation

### 1. Stabilize the four-year feed specification

- Review and format `R/set_feed_spec.R`; keep one row for each of 2012, 2015, 2019 and 2025.
- Validate a one-to-one match between `raw_feed_paths` and `routing_spec` by year.
- Store calendar boundaries as `Date`, retain the analysis datetime and assert that years agree.
- Confirm the intended source feed behind each renamed annual ZIP. Record that the 2015 input is the
  2015-01-13 archive and the 2019 input is the 2019-10-02 archive.
- Set `routing_spec` to 06:50 on 2012-10-03, 2015-10-07, 2019-10-02 and 2025-10-01.
- Confirm that all four feeds remove synthetic rail, regularize bus times and enter the R5 scenario.

### 2. Add explicit rail-template overrides

- Add a small `rail_template_overrides` parameter target with the fields `year`, `code_line`,
  `template_year`, and the historical terminal/allowed-stop rule.
- Pass the override through `rail_feeds` into the rail-building functions.
- Refactor template selection so each requested canonical line normally comes from the same-year
  feed, while L15/2015 alone comes from the 2019 feed.
- Import only the selected route's required `routes`, `trips`, `stop_times`, `stops` and `shapes`
  records. Avoid merging the complete 2019 rail system into the 2015 template.
- Restrict both directions of L15/2015 to Vila Prudente and Oratório, then renumber stop sequences
  and allocate the configured runtime over the retained geometry.
- Keep `rail_route_crosswalk()` strict: missing or duplicated canonical lines must still fail.

### 3. Validate rail topology before writing feeds

- Assert exactly one route and two representative directions per expected line/year.
- Assert that every retained trip has at least two stops, valid shapes and positive segment distance.
- Compare output line sets with `rail_service_spec` for each year.
- For L15, explicitly verify:
  - absent in 2012;
  - Vila Prudente–Oratório only in 2015;
  - the intended 2019 terminal on 2019-10-02;
  - the full configured 2025 segment.
- Review whether the 2012 platform exclusions/transfers are needed in later templates; do not copy
  them to other years without evidence from their stop sequences.
- Update comments that still describe the model as a two-year or 2012/2025-only scenario.

### 4. Build and audit feeds in increasing scope

- Rebuild `feed_spec`, `prepared_feeds`, `rail_service_spec` and all four `rail_feeds`.
- Inspect route, trip, stop, frequency and calendar counts by year.
- Build `scenario_feed_audit` for all years and check active service at each analysis datetime.
- Build `scenario_feed_reports` and inspect validator errors before exporting to R5.
- Build `r5_feeds` only after the four scenario branches pass.
- Confirm service calendars do not overlap across scenario years and that the shared directory
  contains exactly the intended bus and reconstructed-rail feed for each year.

### 5. Generalize accessibility to four years

- Change the `access` target input from `ttm_bypass` to `ttm_transit_all`. Leave `ttm_bypass`
  declared but inactive, with no downstream target depending on it.
- Replace `n == 2` in `calc_access()` with an explicit completeness check against the configured
  analysis years.
- Keep `year` as the primary period identifier; remove the two-label factor construction.
- Produce accessibility levels for 2012, 2015, 2019 and 2025.
- Define requested comparisons explicitly. Initial defaults should include consecutive changes
  (2015–2012, 2019–2015 and 2025–2019) and the cumulative 2025–2012 comparison.
- Do not modify `plot_access()` or build `access_plot` in this task. Plot adaptation remains a
  separate follow-up after the four-year accessibility table is validated.
- Keep the pilot-study year vector independent unless the empirical design explicitly requires 2015.

### 6. Update durable records

- Update `MEMORY.md` with the four analysis dates, source-feed provenance and L15/2015 topology
  override.
- Update `BACKLOG.md` items that still assume two networks or only two TTM branches.
- Record commands, branch-level results and any deferred R5 work in a session log.
- Do not commit `_targets/meta/meta`, absolute machine paths or confidential data outputs as part of
  the implementation change.

## Verification

1. Run `air format .`, reverting formatter-only edits outside the intended scope.
2. Parse all changed R files and load `targets::tar_manifest(callr_function = NULL)`.
3. Inspect `targets::tar_outdated()` and confirm the affected scope before building.
4. Build through `rail_feeds` first; inspect each ZIP directly for expected line and stop sets.
5. Build `scenario_feed_audit`, then `scenario_feed_reports`, for all four branches.
6. Build `r5_feeds` and confirm the shared input directory and non-overlapping calendars.
7. Run focused synthetic/unit checks for accessibility completeness and comparison construction;
   exclude `access_plot` from this verification scope.
8. Run the four-year R5 network and TTM jobs only after the feed gate passes. Report clearly if
   these long jobs are deferred.

## Acceptance criteria

- All four `rail_feeds` branches complete without weakening unique-route validation.
- The 2015 rail feed includes L15 only between Vila Prudente and Oratório and does not import later
  stations from the 2019 template.
- Each scenario contains exactly the bus and rail services intended for its analysis year/date.
- Feed audits and validator reports pass before `r5_feeds` is considered ready for routing.
- No four-year accessibility calculation relies on a hardcoded two-period count or label.
- Every cross-year accessibility change identifies its baseline and comparison year explicitly.
- No expensive R5 build is started merely to diagnose a feed-construction failure.

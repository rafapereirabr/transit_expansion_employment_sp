# Backlog

This file contains unfinished work only. Durable findings and decisions belong in `MEMORY.md`.

## Immediate deliverable — executive box

- [ ] Agree on the box's question, audience, length, and core descriptive claims with the project
  team.
- [ ] Select a small set of defensible statistics from the GTFS/accessibility work.
- [ ] Produce final accessibility figures and accompanying assumption notes.
- [ ] Keep causal language and DiD estimates out of this product.

## Medium term — CAF report (Dec 2026–Jan 2027)

- [ ] Finalize the CadÚnico–RAIS linkage, analysis panel, outcomes, and disclosure protocol.
- [ ] Define residential treatment cohorts, eligible controls, exclusion buffers, and sensitivity
  cutoffs.
- [ ] Assess residential mobility, staggered timing, COVID-era heterogeneity, anticipation, and
  pre-trends.
- [ ] Compare the feasible baseline design with explicit spatial-spillover specifications.
- [ ] Pre-specify the minimum credible fallback design that can ship by the deadline.

## Longer-term research design

- [ ] Develop an interference-aware estimand connecting transit accessibility to direct and
  spillover labor-market effects.
- [ ] Investigate how continuous accessibility changes can enter a staggered design without being
  reduced entirely to station-distance cutoffs.
- [ ] Separate mechanisms involving employment, formalization, wages, commuting, job matching, and
  residential sorting.

## Accessibility pipeline — required before final travel-time matrices

- [ ] Build a historically sourced alternative to the pragmatic rail-service
  scenario, with 2012 and 2025 HPM runtimes/headways for every operating line.
  The documentary search already found Metrô 2024 peak headways of 132, 137,
  131 and 179 seconds for L1, L2, L3 and L15; a CPTM-wide 2012 interval of
  6.17 minutes; and a 2024 L12 interval of 4 minutes 45 seconds. Line-specific
  2012 CPTM operating data may require a SIC/LAI request because the official
  methodology points to an internal operational database.
- [ ] Decide how to represent express/short-turn rail service when it materially
  affects the 06:50 analysis window.
- [ ] Validate each reconstructed rail feed and compare end-to-end and
  interstation runtimes against its source table.
- [x] Declare bus-only SPTrans feeds that remove route types 1 and 2.
- [x] Declare yearly standalone rail-feed branches and export them alongside the
  corresponding bus feed.
- [x] Run the corrected full 2012 and 2025 transit matrices and inspect the
  resulting accessibility maps.
- [x] Estimate and apply 2012 bus runtimes from the 2015–2017 reference period,
  conditional on H3-8 location and busway class, with documented fallbacks.
- [ ] Add a pre-routing gate confirming that only the intended year's routes and
  services are active at each analysis datetime.
- [ ] Fix R5 log capture so the second sequential branch cannot write its live
  log into the first year's directory.
- [ ] Run original, harmonized and conservative 2012 scenarios and decompose the
  accessibility difference.

## Useful audits

- [x] Record active routes/departures, headways, speeds and runtimes at 06:50 in
  tidy source and scenario feed audits.
- [x] Validate raw feeds before transformation and corrected bus-plus-rail feeds
  after transformation.
- [ ] Compare raw and HPM-only feed counts, ZIP size, network-build time, peak
  memory and a fixed small-OD routing time.
- [ ] Confirm that the HPM-only feed reproduces raw-feed travel times for a fixed
  random sample before using it in the full pipeline.
- [ ] Validate transfer links between independently reconstructed rail stops and
  SPTrans bus stops.
- [ ] Investigate the three corrupt historical archives dated 2018-03-09,
  2018-06-15 and 2022-03-15 only if they become necessary for identification.

## Routing review follow-up — 2026-08-28

Revalidate these recommendations against the current worktree before implementing them; the review
examined commit `c74f1e6`, while routing code has since changed.

- [ ] Make `calc_ttm()` require an explicit analysis datetime (preferably from `routing_spec`) and
  remove any `Sys.time()` or hard-coded date fallback; preserve an explicit `year` in its output.
- [ ] Assert that the R5 network/scenario year agrees with the analysis datetime before routing.
  This is the function-level part of the pre-routing gate already listed above.
- [ ] Assert unique destination station IDs before sending the OD table to `r5r`.
- [ ] Make the origins' EPSG:4326 transformation explicit before combining origins and destinations,
  or use exact H3 cell centers.
- [ ] Return relative paths from GTFS/network helpers and remove machine-specific absolute paths
  from versioned target metadata.
- [ ] Remove the machine-specific `JAVA_HOME` from the versioned `.Rprofile`, or guard it with an
  existence check and keep local configuration unversioned.
- [ ] Test whether repeated R5 builds produce a stable `network.dat` hash; use the result to decide
  whether `build_r5r_network(overwrite = TRUE)` is safe.
- [ ] Make R5 log capture a checked pipeline artifact rather than a warning whose return value is
  discarded. This complements the log-provenance item already listed above.
- [ ] Decide and document whether the study area is the municipality of São Paulo or the RMSP,
  including the consequences for EMTU service and destinations outside the municipality.
- [ ] Preserve station opening/status fields in the walking-to-stations result, or require an
  explicit analysis-date filter so future stations cannot be treated as contemporaneous.
- [ ] Document and assess the assumption that the 2025 street network can represent walking access
  in every analysis year.
- [ ] Replace `T` and `F` with `TRUE` and `FALSE` in the affected R and pipeline code.

## Deferred spin-off work

- [ ] Extract generic GTFS repair/reconstruction helpers into a dedicated package.
- [ ] Consolidate archive parsing and MobilityData validation with `aopgtfs`.
- [ ] Add reusable schemas for provenance, repair manifests and scenario specs.
- [ ] Generalize the workflow beyond weekday morning-peak accessibility.
- [ ] Revisit `detailed_itineraries()` only if aggregate counterfactuals cannot
  identify the remaining routing discrepancy.

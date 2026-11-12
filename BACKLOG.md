# Backlog

This file contains unfinished work only. Durable findings and decisions belong in `MEMORY.md`.

## Immediate deliverable — executive box

- [ ] Agree on the box's question, audience, length, and core descriptive claims with the project
  team.
- [ ] Select a small set of defensible statistics from the GTFS/accessibility work.
- [ ] Produce final accessibility figures and accompanying assumption notes.
- [ ] Keep causal language and DiD estimates out of this product.

## Medium term — CAF report (Dec 2026–Jan 2027)

### Current implementation sequence — checkpoint 2026-11-05

- [ ] Finish and inspect all annual `person_records` and `person_audit` branches after the resolved
  schema-compatibility adjustment. Check aggregate exclusion counts, geocode availability and
  cross-year CPF/birth-date inconsistencies without exporting identifiers.
- [ ] Build `opening_events` from `stations_sf`, with one row per line × trial-service date, a stable
  `event_id`, station membership, exact dates, event classification and inclusion flags. Keep the
  station XLSX-to-target pipeline unchanged while doing this.
- [ ] Build outcome-free rolling `risk_sets`: baseline residence and eligibility at `g - 4`, age
  22--60 on the trial-service date, exact geodesic station distance, first/additional service,
  absorbing study-window treatment assignment, not-yet-treated controls and comparison flags.
- [ ] Derive `cpf_roster` only from eligible risk-set members; then reactivate nationwide RAIS
  extraction and construct outcomes. Do not restore the obsolete all-CadÚnico `person_roster` or
  `panel_eoy` workflow.
- [ ] Wire the risk sets to `did_spec`, `run_att_gt()` and focal-ATT extraction. Implement joint
  person-level inference before reporting `combined` first/additional estimates with uncertainty.
- [ ] Keep routing and accessibility targets dormant during this sequence; preserve them for later
  reactivation and do not delete or refactor them as part of the labor-risk-set work.

- [ ] Finalize the CadÚnico–RAIS linkage, baseline cohort frame, outcome definitions and disclosure
  protocol. Implement and audit the December 31 rule: exclude missing or invalid CPFs before cohort
  construction; code no exact RAIS match as zero only for a valid CPF-year covered by a completed
  extraction; reserve `NA` for source, extraction, coverage or classification failures; and determine
  formal employment before filtering wages, hours, establishments or workplace geocodes.
- [ ] Define coherent opening cohorts/windows, their no-expansion counterfactual network, outcome
  baseline/end point, residential baseline and follow-up horizon. Use the actual opening year as the
  first post-treatment year for the December 31 outcome, retain exact opening dates and days of
  operation for diagnostics, and implement rolling risk sets with eligibility and residence at
  `g - 4`. Use RMSP CadÚnico residents aged 22--60 on the package's trial-service date in `g`, with a
  valid CPF and usable baseline geocode and no additional income cutoff. Define
  line-by-trial-service-date opening packages and
  assign `G_i` at the first eligible 2015--2023 package that adds public service within 2 km of the
  baseline address, then keep that assignment absorbing across later risk sets. Separate first from
  additional treatment using baseline service. Do not
  impose an `ever_observed_near_station` history or pool overlapping windows as repetitions of one
  estimand. Use eligible not-yet-treated RMSP residents in the primary control risk set,
  subject to common-support and baseline-service restrictions. Implement a central robustness
  specification using the future Line 6 and Line 17 corridors as controls, and interpret it as the
  incremental effect of opening/operation conditional on planning and implementation rather than
  as the total effect of the expansion process. Evaluate Line 19 separately before adding it to any
  planned-corridor control pool.
- [ ] Pre-specify spillover sensitivities. First exclude controls between 2 and 4 km from a new
  opening. Then use a stricter combined specification that also excludes controls beyond 4 km with
  substantively large modeled accessibility gains, fixing the gain threshold without outcome-based
  selection. Record but defer the ever-treated-only comparison. Confirm with CAF whether to include
  pre-specified fictitious opening dates for the Line 6 and Line 17 corridors as placebo diagnostics.
- [ ] Finish the opening-event audit before cohort construction: distinguish new stations, new line
  service at existing stations, infill stations and reopenings. Exclude reform-only events and the
  2014 Line 8 Amador Bueno/Santa Rita reconstruction/reopening from the pooled design; classify
  Line 13 at Engenheiro Goulart as additional service. Trial/full-service dates and their sources
  have been completed for all 2015--2023 openings.
- [ ] Build a 2023 transport/accessibility scenario for the 2023 labor endpoint, or document and
  validate an explicit proxy; do not silently substitute the existing 2025 TTM.
- [ ] Build the design-support table for each window: baseline people and cells, modeled
  accessibility gains/losses, distance to new infrastructure, near-zero and outer-distance support,
  movers, linkage retention, outcome availability and overlap among intervention neighborhoods.
- [ ] Pre-specify the accessibility null band, positive/loss support, distance study radius and
  unaffected comparison region using substantive and measurement criteria rather than outcome
  significance.
- [ ] Evaluate prior outcome trends against future exposure, anticipation, residential mobility,
  administrative attrition, COVID-era heterogeneity, station siting and spatial interference.
- [ ] Estimate paired nonparametric spatial DiDs: the policy-effect curve indexed by modeled
  accessibility change and the complementary curve indexed by baseline distance to newly opened
  stations or segments.
- [ ] Define the population-weighted summary effect for baseline residents with substantively
  positive accessibility gains and report it with the full curve and support.
- [ ] Implement the two agreed wage outcomes: real contractual hourly wage among workers formally
  employed on December 31, and an indicator for formal employment at or above the cohort-specific
  median among all eligible employed members of the RMSP risk set at `g - 4`. Aggregate concurrent
  active links using total contractual salary
  over `(52 / 12) * total weekly hours`, remove exact duplicate links, deflate to December 2023 reais
  with the IPCA, and verify that contractual salary is standardized to a monthly unit across salary
  types. Report the first outcome as conditional on observed formal employment; use the second for
  the full-cohort policy contrast. Pre-specify spatial inference and multiple-testing control.
- [ ] Implement baseline-home-to-workplace geodesic distance among workers formally employed on
  December 31. Use `g - 4` residential latitude and longitude, workplace coordinates and an
  hours-weighted mean across concurrent active links. Compute with DuckDB Spatial outside R memory;
  use exact geodesic distance for treatment and the outcome, reserving H3 for aggregation and
  candidate spatial clustering. Audit
  missing workplace geocodes separately and describe the outcome as a conditional spatial mechanism,
  not as observed commute distance or travel time.
- [ ] If projected `ST_DWithin()` candidate generation becomes a bottleneck, benchmark a
  persistent H3 candidate lookup for repeated station-distance cutoffs. Treat H3 only as candidate
  generation, require 100% recall against exact geodesic cutoffs on the study geography, and avoid
  adopting a fine resolution such as 11 without checking the expanded `gridDisk()` size.
- [ ] If time permits, analyze people aged 18--21 on the package's trial-service date as a separate
  school-to-work estimand; keep ages 22--60 on that date as the primary population.
- [ ] After the initial annual analysis, evaluate secondary formal-employment attachment outcomes:
  annual days or month-equivalents employed and, if useful, quarterly or monthly panels. Keep
  hourly remuneration in the initial outcome set rather than deferring it with these extensions.
- [ ] Operationalize productivity and inclusion separately, or delimit the corresponding claims;
  do not relabel individual wages as productivity or an average effect as inclusion.
- [ ] Compare total-network and rail/bus component accessibility scenarios without treating
  simulated feed decompositions as automatically causal component effects.
- [ ] Pre-specify the minimum credible fallback DiD that can ship by the deadline, beginning with a
  coherent distance-based opening/corridor design if the accessibility null band fails.

## Longer-term research design

- [ ] Extend the baseline-resident policy estimand to distinguish direct and spillover effects under
  an explicit interference mapping.
- [ ] Extend the cohort/window-specific spatial DiDs to annual staggered timing and dynamic effects
  without treating accessibility as a continuous causal dose.
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

## Positron / renv startup investigation

Current incident recovered and console responsiveness confirmed by the user. Evidence and recovery:
`quality_reports/session_logs/2026-09-11_positron-renv-startup.md`.

- [ ] Prevent recurrence: investigate/fix the renv 1.2.4 Windows liveness check, which classified
  a terminated but accessible process object as alive. Determine why the original owner left its
  sandbox lock. No permanent package or configuration fix has been installed.
- [ ] If startup latency remains disruptive, profile the synchronization report and package
  metadata checks separately. Normal startup after recovery completed in 47.67 seconds.

- [ ] Track whether a future release of `openai.chatgpt` fixes repeated Git watcher failures on
  network/UNC repositories. The current pre-release has no release-channel build; isolating Codex
  with `extensions.experimental.affinity` restores sub-second R interaction but mitigates rather
  than fixes the watcher.

## DiD downstream — revision 2026-11-11 (not executed)

- [ ] Resolve upstream design discrepancies before estimation: age/reference date, planar versus
  geodesic distance, municipality versus RMSP, 15 km support, stale registry/geocoding exclusions.
- [ ] Produce the exposure table required by build_did_membership(): prospective first opening
  at the fixed address, prior absorbing assignment, independent L6/L17 overlap. Audit stations_nyet
  against strictly future openings; nearest station alone does not identify the first opening.
- [ ] Build annual outcomes and extraction coverage; calculate the common baseline wage threshold
  on the full eligible RMSP risk set before comparison/support restrictions. Resolve monthly salary
  units and workplace geocoding cardinality, preserving employment with missing establishments.
- [ ] Format with air format ., parse, and run synthetic tests for the revised did helpers before
  any restricted-data run. No R, formatter, bootstrap or targets were executed in this revision.
- [ ] Validate joint resampling of repeated people, changing stratum weights, failed replicates,
  same-year treatment exclusion, prospective control removal, and missing outcome coverage.
- [ ] Confirm installed did API and benchmark a representative focal risk set without bootstrap.
- [ ] Choose common cohorts/horizon, baseline covariates and overlap diagnostics; determine spatial
  inference and the reported multiple-testing family. Person bootstrap alone is insufficient for
  geographic dependence. Its current uncertainty conditions on constructed outcomes and exposure.
- [ ] Reactivate spillover modes only on explicit instruction, after their flags and substantive
  accessibility threshold have been defined and audited. Do not activate by calendar date.

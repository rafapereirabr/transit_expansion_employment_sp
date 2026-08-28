# Backlog

## Required before final travel-time matrices

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
- [ ] Rerun a small OD smoke test before the full matrix, after sourced rail
  service parameters pass the feed gates.
- [ ] Estimate 2012 bus-runtime correction factors by comparable route groups,
  using several feeds from 2015–2017 rather than one arbitrary snapshot.
- [ ] Run original, harmonized and conservative 2012 scenarios and decompose the
  accessibility difference.

## Useful audits

- [ ] Record active routes and departures at 06:50 in the tidy feed audit.
- [ ] Compare raw and HPM-only feed counts, ZIP size, network-build time, peak
  memory and a fixed small-OD routing time.
- [ ] Confirm that the HPM-only feed reproduces raw-feed travel times for a fixed
  random sample before using it in the full pipeline.
- [ ] Validate transfer links between independently reconstructed rail stops and
  SPTrans bus stops.
- [ ] Investigate the three corrupt historical archives dated 2018-03-09,
  2018-06-15 and 2022-03-15 only if they become necessary for identification.

## Deferred spin-off work

- [ ] Extract generic GTFS repair/reconstruction helpers into a dedicated package.
- [ ] Consolidate archive parsing and MobilityData validation with `aopgtfs`.
- [ ] Add reusable schemas for provenance, repair manifests and scenario specs.
- [ ] Generalize the workflow beyond weekday morning-peak accessibility.
- [ ] Revisit `detailed_itineraries()` only if aggregate counterfactuals cannot
  identify the remaining routing discrepancy.

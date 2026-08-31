# Bus-speed pipeline integration — 2026-08-31

## Decision and implementation

- Promoted the production logic from `sidequests/bus_speed_surface.R` and
  `sidequests/check_busways.R` into `R/bus_speeds.R`.
- Declared the 13 selected 2015--2017 SPTrans GTFS archives, busway sources and
  estimator parameters as explicit DAG dependencies.
- Dynamically branched HPM segment extraction by archive.
- Stored `reference_feed_inventory`, `hpm_segments`, `feed_diagnostics`,
  `segment_busway_matches` and `bus_speed_surface` as internal
  `format = "parquet"` targets.
- Passed the surface table directly to `write_bus_feeds()` and made the
  structural validation target a gate for `bus_feeds`.
- Added buffer diagnostics and four downstream figures under
  `figures/diagnostics/`.
- Retained the sidequest scripts as historical exploratory records; no
  production target consumes their outputs.

## Verification

- Ran `air format .`; it reformatted unrelated legacy files, so those incidental
  edits were reversed. Ran `air format R/bus_speeds.R` after the final changes.
- Parsed `R/bus_speeds.R`, `R/analysis_feeds.R` and `_targets.R` successfully.
- Tested extraction against the 2015-01-13 feed: 1,149 active bus routes and
  1,999 active patterns.
- Ran the complete estimator directly and compared it with the legacy CSV by
  `(h3, segregation)`: all 1,842 rows matched, with maximum numeric difference
  below `5.2e-14`.
- Built `bus_speed_validation` through `targets`: 1,842 cells, four classes,
  median speed 11.1 km/h and finite positive speeds from 4.71 to 33.2 km/h.
- Built `busway_buffer_diagnostics` and `bus_speed_diagnostic_figures`; visually
  inspected all four figures.
- Rebuilt `bus_feeds` and both `scenario_feed_audit` branches successfully.
- Confirmed in target metadata that the five main tabular targets use
  `format = "parquet"`.

## Notes and deferred verification

- Arrow emitted sandbox-only warnings because it could not query macOS CPU cache
  information through `sysctl`; target results were unaffected.
- The full R5 network and travel-time matrices were not rebuilt. The existing
  surface was reproduced numerically and the scenario feed audits passed, so a
  routing smoke/full rebuild can be run with the next routing change rather than
  repeating the multi-hour job here.
- `targets::tar_manifest()` through its default subprocess stalled and was
  interrupted. Subsequent builds used `callr_function = NULL` and completed
  without leaving orphan R processes.

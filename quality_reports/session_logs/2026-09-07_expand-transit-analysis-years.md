# Four-year transit scenarios — 2026-09-07

## Implementation

- Set the four routing dates to 06:50 on 2012-10-03, 2015-10-07, 2019-10-02 and
  2025-10-01.
- Moved annual feed construction into `set_feed_spec()` with explicit one-to-one year validation
  and `Date` service windows.
- Added the declarative `rail_template_overrides` target. L15/2015 borrows only its route topology
  from the 2019 template and is restricted to Vila Prudente and Oratório in both directions.
- Generated two-point shapes for the restricted L15/2015 directions instead of retaining the
  anachronistic full 2019 shape.
- Kept strict unique-route validation for every other line and year.
- Removed a non-GTFS administrative TXT from prepared feeds. Its zero-length ZIP entry caused the
  Java validator to fail on the 2019 bus feed.
- Changed the `access` target to consume `ttm_transit_all`; `ttm_bypass` remains declared but has no
  downstream consumer.
- Generalized `calc_access()` to require complete 2012/2015/2019/2025 OD coverage, retain cumulative
  changes relative to 2012 and add consecutive-year changes. Plot code was not changed.

## Verification

- Parsed the changed R files and loaded a 47-target manifest.
- Built `feed_spec`, `prepared_feeds`, `rail_service_spec`, `rail_template_overrides` and all four
  `rail_feeds`.
- Confirmed L15/2015 has exactly two directions and two stops per direction: Vila Prudente and
  Oratório. Each direction references its restricted generated shape.
- Inspected line/direction stop counts for all four reconstructed rail feeds.
- Built all four `scenario_feed_audit` branches: eight bus/rail rows are active on the intended
  analysis dates.
- Built all four `scenario_feed_reports` branches: all eight validator JSON reports contain zero
  errors.
- Built `r5_feeds` in the shared directory. The R5 network and four travel-time matrices were not
  built.
- Passed a synthetic four-year `calc_access()` smoke test. `access_plot` was intentionally excluded.

## Notes

- Reading the source 2019 ZIP still emits warnings about the malformed administrative TXT before it
  is discarded; the prepared and bus outputs no longer contain that file.
- Rewriting the four prepared and bus feeds took about 25 minutes in this run. No R5 process was
  started.
- `air format .` also reformatted legacy R and sidequest files outside the narrow implementation
  scope. The user explicitly chose to retain those formatting changes in the worktree.

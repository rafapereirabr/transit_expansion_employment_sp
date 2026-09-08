# Project memory

This file records durable methodological decisions and findings. It is not a
task list; deferred work belongs in `BACKLOG.md`.

## Research objective and products

The overarching objective is to study how São Paulo's rapid-transit expansion affects labor-market
outcomes, with emphasis on people in socioeconomic vulnerability. CadÚnico is the main source
because it provides residential addresses and repeated observations; linkage to RAIS adds detailed
formal-employment outcomes and workplace information.

The work has three horizons:

1. An immediate executive box with descriptive statistics and the GTFS/accessibility analysis,
   without causal or DiD claims.
2. A CAF report due around December 2026–January 2027 with a credible policy analysis; a conventional
   cutoff/buffer design remains an acceptable fallback if a stronger design is not ready.
3. A longer-term design that treats interference, spatial spillovers, accessibility, transit feeds,
   and staggered timing more explicitly.

The current technical subproject is to:

Estimate comparable morning-peak transit travel-time matrices for São Paulo in
2012, 2015, 2019 and 2025. The analytical departure time is 06:50 on the
Wednesday in the first week of October: 2012-10-03, 2015-10-07, 2019-10-02 and
2025-10-01, with a 15-minute departure window and a 60-minute maximum trip.

## Current feed architecture

- Raw GTFS archives in `data-raw/` are immutable inputs.
- `source_feed_audit` and `source_feed_reports` describe and validate the raw
  inputs before any transformation.
- `prepared_feeds` fixes service dates and known structural defects.
- `bus_feeds` removes synthetic route types 1 and 2 from the SPTrans feeds,
  retains referential integrity and regularizes SPTrans scheduled runtimes.
- `rail_feeds` reconstructs one standalone rail GTFS per routing-year branch.
- Reconstructed rail feeds use analytical agencies `METRO` and `CPTM`, including
  privately operated lines under the corresponding system. This keeps every rail
  route distinct from the SPTrans agency without modeling concession history.
- `scenario_feed_audit` and `scenario_feed_reports` describe and validate the
  bus-plus-reconstructed-rail scenario after transformation. The tidy audits
  include active service at 06:50, headways, speeds, runtimes and the main model
  assumptions.
- The pipeline exports all year-specific bus and rail feeds to one shared R5
  network under `data/r5/all`, with non-overlapping service calendars and
  year/date TTM branches. Scenario audits gate the inputs by confirming that the
  intended services are active at each analysis date.

## Empirical GTFS findings

### Rail

- The SPTrans feeds contain synthetic rail schedules, not reliable operational
  timetables.
- Every interstation segment within a line/direction has an identical scheduled
  duration (`segment_time_sd == 0`).
- In 2025, Line 3 takes 53.8 minutes in each direction. This is one-way runtime,
  not a round trip, and is much slower than the approximately 37-minute current
  end-to-end runtime checked independently.
- Line 15 is encoded with a 15-minute headway in every available SPTrans archive
  from 2016 through 2026, another sign of a static placeholder.
- Lines 15 and 8 also have particularly suspicious runtimes. Plausible totals on
  other lines do not validate their schedules because their segment timing is
  still synthetic.
- Decision: remove the SPTrans rail component and replace it with independently
  parameterized rail feeds for each year. Do not activate replacement feeds
  using an explicit standardized-service scenario. A fully historical,
  source-by-source reconstruction remains a deferred robustness exercise.

### Buses

- The decline around Sapopemba is part of a citywide pattern, not a local-only
  anomaly.
- Weighted citywide scheduled speed falls from about 14.8 km/h in 2012 to 11.0
  km/h in 2025; median headway rises from about 10.9 to 12 minutes.
- Citywide and Sapopemba histories are strongly correlated: approximately 0.88
  for speed and 0.84 for headway.
- Historical feeds suggest a measurement/construction break after 2012. Older
  schedules may be systematically optimistic, while later feeds may incorporate
  more realistic traffic conditions.
- Implemented decision: regularize SPTrans runtimes using the 2015–2017 reference
  feeds and conditional median speeds by H3 resolution 8 and busway class.
- Busway matching combines GeoSampa and MobilityData geometry, respects opening
  dates and distinguishes ordinary streets, managed/exclusive corridors and
  fully segregated infrastructure. Segment speeds fall back to class-wide and
  global medians when the local conditional cell is unsupported.
- The complete bus-speed estimation chain is now part of the `targets` DAG. The
  original `data-raw/3550308_sao_paulo.rar`, model parameters and busway inputs
  are explicit dependencies. The 13 SPTrans feeds for 2015--2017 are selected
  automatically from the RAR member names and extracted under
  `data/gtfs/history/`; their portable metadata are stored in the Parquet
  `reference_feed_inventory`. Transient extraction, spatial matching,
  validation and diagnostics run inside the compact RDS `bus_speed_model`, and
  only the production `bus_speed_surface` is exposed as a separate Parquet
  target. Production targets no longer read generated files from `sidequests/`.
- The 15/25/40 m busway-buffer comparison was a one-off tuning exercise and is
  intentionally outside the DAG. `measure_busway_buffers()` remains available
  with a compact example; production uses 25 m and 60% minimum overlap.
- The original 2012 schedule remains conceptually useful as an optimistic-bound
  robustness scenario, but it is not the primary corrected feed.

[LEARN:targets] Export every inspectable bus-speed intermediate as a stable
`format = "file"` Parquet → keep pipeline intermediates as ordinary
`format = "parquet"` targets and pass the validated surface directly to the bus
feed builder; reserve file targets for raw inputs and actual external outputs.

[LEARN:targets] Give every computational intermediate its own target → retain a
target boundary only when it provides material caching, parallelism, inspection
or an external file contract. The one-minute bus-speed estimator is clearer as
one compact model target than as ten transient targets.

## Analytical scenarios

The intended robustness design is:

1. Original 2012 bus schedules with reconstructed 2012 rail.
2. Harmonized 2012 bus runtimes with reconstructed 2012 rail (primary corrected
   scenario, now implemented).
3. A conservative sensitivity scenario applying the post-break bus-speed regime
   to 2012 while retaining 2012 routes and frequencies.
4. 2025 bus routes/frequencies with the same harmonized runtime model and
   reconstructed 2025 rail.

This design separates network expansion, frequency, bus runtime and rail
service assumptions instead of conflating all changes in a single comparison.

## Reconstruction implementation

- `R/analysis_feeds.R` creates the HPM-only derivative feeds without expanding
  `frequencies.txt` into scheduled trips. It is intentionally not connected to
  `_targets.R`: a benchmark preserved all 06:50 service but reduced the SPTrans
  ZIPs only from 6.2 to 5.9 MB (2012) and 10.0 to 9.7 MB (2025), too little to
  justify changing the primary pipeline without a timing benchmark.
- `R/rail_feeds.R` builds a standalone rail feed from the topology of a template
  GTFS and explicit line-level operating assumptions.
- Interstation running time is allocated using cumulative straight-line distance
  between stops, avoiding the equal-time-per-segment defect in the source feed.
- `rail_service_spec` uses the project's canonical `code_line` and `name_line`
  conventions. These are validated against the `stations_sf` target,
  whose upstream opening-date input is `data/station_openings.xlsx`. Feed-specific
  `route_id` values are inferred and checked rather than treated as line identity.
  The primary pragmatic scenario applies common service assumptions in every
  year: 35 km/h commercial speed for metro/monorail, 40 km/h for metropolitan
  rail, and HPM headways from a prior project table. Metrô headways came from
  annual reports; CPTM values came from a professional contact. Ranges use their
  midpoint: L9 5.5 minutes, L11 6 minutes and L12 7 minutes. Runtime is derived
  transparently as operational extension divided by commercial speed.
- The January 2015 SPTrans template has no usable L15 route. The reconstructed
  2015 rail feed borrows only L15 topology from the 2019 template and restricts
  it declaratively to Vila Prudente–Oratório in both directions; no other 2019
  rail topology enters the 2015 scenario.
- Rail `stops` and `shapes` remain year-specific GTFS inputs. The canonical
  station registry supplies line identity, not replacement coordinates. The 2012
  feed embeds platform IDs from connecting lines as consecutive stops at Paraíso,
  Tatuapé and Brás because it has no `transfers.txt`. Reconstructed trips remove
  those borrowed/redundant stops and the `rail_stop_corrections` target creates
  explicit bidirectional transfers between the platform IDs that remain.
- Bus regularization preserves routes, trips, stops and shapes and rewrites
  stop-time progression from modeled segment speeds. The current speed surface
  is the conditional H3-8 x busway-class median from 2015–2017, with a 25 m
  busway matching tolerance and a 60% minimum overlap rule recorded in audits.

## Current routing result and performance

- The corrected feeds produced substantially more plausible preliminary
  accessibility maps, including the 2012–2025 difference using 2019 land use.
- The latest complete pair of transit TTMs finished successfully. Separate
  networks took 7 h 46 min (2012) and 6 h 42 min (2025), but the R5 logs show
  roughly nine hours of long inactivity gaps consistent with macOS sleep or
  process suspension. Memory compression and swap were also high.
- Long local runs should prevent sleep (for example with `caffeinate`) and keep
  the two TTM branches sequential because the JVM already parallelizes routing.
- The persistent JVM may keep writing the second branch's live log into the
  first network directory. Log location therefore needs correction before logs
  are treated as year-specific evidence.

## External repositories reviewed

- `shenanigans`: useful philosophy for assembling
  routes, directional trips, distance-derived stop times and HPM frequencies.
- `gtfs_santos_vlt`: useful example of deriving service
  periods from published timetables.
- `aopgtfs`: useful validation patterns and
  handling of malformed archives. General package development is out of scope
  for this project.

## Reproducibility cautions

- Do not copy or rename R5 logs between years; provenance then becomes ambiguous.
- Do not expand frequency feeds merely to satisfy `detailed_itineraries()` unless
  that diagnostic becomes necessary. R5 routing accepts frequency service.
- A feed-window filter must select trips using events at all stops, not only the
  first departure, and must preserve the complete stop sequence.
- Historical archive dates are taken from filenames when calendars are overly
  broad. Corrupt historical archives are logged rather than silently included.

## Corrections and workflow learnings

Append durable corrections as `[LEARN:category] wrong assumption → corrected practice or fact`.

[LEARN:workflow] `AGENTS.md`, `MEMORY_2.md`, and the initial `CHANGELOG.md` imported on 2026-08-24
described the separate `aoplanduse` repository → treat imported workflow files as templates until
their claims are verified against this repository; `MEMORY.md` and `BACKLOG.md` were the factual
starting points for this project.

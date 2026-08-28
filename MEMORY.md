# Project memory

This file records durable methodological decisions and findings. It is not a
task list; deferred work belongs in `BACKLOG.md`.

## Research objective

Estimate comparable morning-peak transit travel-time matrices for São Paulo in
2012 and 2025. The analytical departure time is 06:50 on a representative
weekday, with a 15-minute departure window and a 60-minute maximum trip.

## Current feed architecture

- Raw GTFS archives in `data-raw/` are immutable inputs.
- `prepared_feeds` fixes service dates and known structural defects.
- `bus_feeds` removes synthetic route types 1 and 2 from the SPTrans feeds while
  retaining the original bus service and referential integrity.
- `rail_feeds` reconstructs one standalone rail GTFS per routing-year branch.
- Reconstructed rail feeds use analytical agencies `METRO` and `CPTM`, including
  privately operated lines under the corresponding system. This keeps every rail
  route distinct from the SPTrans agency without modeling concession history.
- `r5_feeds` copies the selected bus feed, reconstructed rail feed, shared OSM
  PBF and elevation raster into separate `data/r5/<year>/` directories.
- R5 networks and Java processes are separated by year. The JVM itself uses
  multiple threads, so the two large routing branches are currently run
  sequentially to control memory pressure.

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
- Decision: do not silently replace 2012 bus runtimes with 2015 values. Preserve
  the original as an optimistic bound and build a sourced, group-specific
  harmonization using a multi-feed reference period such as 2015–2017.

## Analytical scenarios

The intended robustness design is:

1. Original 2012 bus schedules with reconstructed 2012 rail.
2. Harmonized 2012 bus runtimes with reconstructed 2012 rail.
3. A conservative sensitivity scenario applying the post-break bus-speed regime
   to 2012 while retaining 2012 routes and frequencies.
4. Observed 2025 buses with reconstructed 2025 rail.

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
  The primary pragmatic scenario applies common service assumptions in both
  years: 35 km/h commercial speed for metro/monorail, 40 km/h for metropolitan
  rail, and HPM headways from a prior project table. Metrô headways came from
  annual reports; CPTM values came from a professional contact. Ranges use their
  midpoint: L9 5.5 minutes, L11 6 minutes and L12 7 minutes. Runtime is derived
  transparently as operational extension divided by commercial speed.
- Rail `stops` and `shapes` remain year-specific GTFS inputs. The canonical
  station registry supplies line identity, not replacement coordinates. The 2012
  feed embeds platform IDs from connecting lines as consecutive stops at Paraíso,
  Tatuapé and Brás because it has no `transfers.txt`. Reconstructed trips remove
  those borrowed/redundant stops and the `rail_stop_corrections` target creates
  explicit bidirectional transfers between the platform IDs that remain.

## External repositories reviewed

- `/Users/baarthur/projects/shenanigans/src`: useful philosophy for assembling
  routes, directional trips, distance-derived stop times and HPM frequencies.
- `/Users/baarthur/projects/gtfs_santos_vlt`: useful example of deriving service
  periods from published timetables.
- `/Users/baarthur/ipea/aop/git_baarthur/aopgtfs`: useful validation patterns and
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

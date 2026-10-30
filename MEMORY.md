# Project memory

This file records durable methodological decisions and findings. It is not a
task list; deferred work belongs in `BACKLOG.md`.

## Research objective and products

The overarching objective is to study how São Paulo's rapid transit expansion affects labor market
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

## Causal study objective and estimand

For the CAF study, causal difference-in-differences is a fixed design requirement. Cross-sectional
comparisons and generic fixed-effects panels may support description or diagnostics, but they are
not substitutes for an explicit intervention, comparison group, pre/post contrast and identifying
assumptions. This does not change the separate decision that the immediate executive box remains
descriptive and contains no DiD claims.

The primary causal question is how rapid transit network expansion affects labor market outcomes
for the socioeconomically vulnerable population resident in the eligible study area at baseline.
The target population is fixed before the intervention. Baseline residence assigns exposure;
subsequent residential mobility is a possible outcome or mechanism. The primary analysis must not
condition on remaining at the same address or on reappearing in CadÚnico. Non-movers define a
selected sensitivity population, not the main estimand.

The intervention is the network expansion or clearly defined network-transition package. Modeled
accessibility change, holding the opportunity surface fixed, is an exposure mapping that indexes
heterogeneity in the total policy effect. It is not a continuous causal dose and its coefficient or
curve is not the marginal causal effect of one unit of accessibility. Distance from baseline
residence to the newly opened station or segment is a complementary primary exposure mapping and
the closest application of Butts's spatial DiD.

Each coherent intervention cohort and time window has its own estimand. The preferred substantive
result is an accessibility-indexed policy-effect curve plus a population-weighted average over the
baseline residents with substantively positive modeled gains. The paired distance-indexed curve is
retained rather than treated as a disposable robustness check. If a near-zero accessibility band
cannot plausibly identify the counterfactual trend, the causal fallback remains a distance-based or
otherwise explicitly defined DiD; it is not a cross-section or an unstructured panel regression.

The initial labor outcome panels use four-year intervals: 2011, 2015, 2019 and 2023. This does not
change the separate 2012--2025 accessibility comparison. Exposure for the 2023 causal endpoint needs
a 2023 transport scenario or an explicitly justified proxy; the current 2025 TTM is not automatically
interchangeable with 2023 conditions.

Formal employment and formalization are the natural primary outcomes from the CadÚnico–RAIS link.
Unconditional formal labor income can summarize extensive- and intensive-margin changes. Wages and
job quality conditional on employment are secondary outcomes because treatment may change who is
formally employed. RAIS alone does not identify an effect on total or informal employment. An
individual wage is not automatically a measure of productivity, and an average employment effect is
not by itself evidence of inclusion; those study objectives require separate definitions.

The initial primary outcome is an indicator of a qualifying formal job active on December 31 as
observed in RAIS. Missing or invalid CPFs are excluded and audited when the CadÚnico roster is
constructed, before baseline cohorts are defined. For a valid cohort CPF in a fully processed RAIS
year, no exact CPF match means `formal = 0`; `formal = NA` is reserved for source, extraction or
coverage failures, or records whose December 31 status cannot be classified. Missing wage, hours,
establishment or workplace geocoding must not turn an observed formal-employment record into zero.
The CadÚnico employment indicator is not a primary outcome because it has extensive missingness and
frequent discordance with RAIS.

The CAF policy brief uses two wage outcomes. The communicable outcome is real contractual hourly
wage among people formally employed on December 31, aggregating concurrent active jobs as total
contractual salary divided by `(52 / 12) * total weekly contracted hours` and expressing values in
December 2023 reais using the IPCA. It is `NA` when `formal = 0`. Because employment is itself
affected by treatment, this conditional outcome supports wording limited to the formally employed
population and does not by itself identify a causal wage effect for the baseline population.

The complementary, more defensible outcome is an indicator for holding a formal job on December 31
whose real contractual hourly wage is at least the median among all eligible employed members of that
cohort's RMSP risk set at `g - 4`. The threshold is common to treated and comparison groups and does
not change across comparison-group robustness specifications. The outcome equals zero for a valid
non-employed person-year and `NA` when coverage or wage classification fails. It is defined for the
full baseline cohort and may support a causal policy contrast subject to the DiD assumptions.
Describe it literally as employment at or above the baseline median, not normatively as a good job.
Remove exact duplicate links before aggregation and verify that the extracted contractual salary is
standardized to a monthly unit across salary types.

The policy brief also uses baseline-home-to-workplace geodesic distance as a spatial mechanism
outcome among people formally employed on December 31. Measure distance from the fixed `g - 4`
residential latitude and longitude to each active workplace, then average concurrent links using
weekly contracted hours as weights. Compute the ellipsoidal distance outside R memory with DuckDB
Spatial. Exact geodesic distance, not H3 membership, defines the 2 km treatment buffer; H3 remains
available for territorial aggregation and candidate spatial clustering. The outcome is `NA` for
non-employed person-years or when required geocodes
cannot be resolved. It is conditional on formal employment and workplace geocoding, and a positive
estimate is not interpreted as a worse commute: greater distance can reflect an expanded labor
market search radius. Public-transit travel time is deferred because RAIS does not reveal commute
mode and the modeled measure would combine network improvement with workplace choice.

Opening cohorts use the historical calendar year of the actual station or segment opening, without
rounding late-year openings into the following year. Because the primary employment outcome is
measured on December 31, the opening year is the first post-treatment observation whenever service
began by that date. `ATT(g, g)` is therefore an initial effect whose operational exposure may be
short; preserve the exact opening date and days of operation before December 31 for interpretation
and support diagnostics. Report it separately from the primary average beginning in the first full
calendar year of operation, using a common follow-up horizon across included cohorts.

The source population is eligible CadÚnico residents of the RMSP, so municipal borders do not cut a
2 km treatment buffer. Baseline eligibility currently requires registration, a valid CPF and a
usable residential geocode at `g - 4`, without an additional income cutoff. The primary population
is age 22--60 on the package's trial-service date in `g`, calculated from birth information available
at baseline; subsequent aging does not remove a person from the fixed cohort. Formal employment is
measured from nationwide RAIS links for the cohort roster and is not filtered by workplace
municipality.

The causal target is the intention-to-treat effect of opening and operating new rapid transit service
as assigned from `g - 4` residence, in a setting where planning, construction and expectations may
already be present. Claims are therefore conditional on the pre-opening policy history and require
explicit anticipation diagnostics; they are not the total effect of the project from announcement.

The distance-based design uses rolling cohort-specific risk sets. For each opening cohort `g`,
eligibility, residence and baseline service are measured at `g - 4`. A line-by-trial-service-date
package is the operational event. Exact distance from the baseline residence to its station points
defines exposure: a person is treated when new public line or station service becomes available
within 2 km, including additional service at a previously served location. First treatment and
additional treatment are separate baseline-service strata. Walking travel time to stations is a
complementary exposure; whether both measures share one table or separate targets remains an
implementation choice.

Within the 2015--2023 study interventions, define `G_i` as the earliest opening year `g` for which
person `i` is eligible in `g - 4`, is still at risk and has a baseline address newly covered by the
opening. Once assigned, `G_i` is absorbing, so a person first treated in 2015 is previously treated
in the 2019 risk set and cannot be first treated again. Proximity to older stations at residential
addresses observed before a later cohort is historical baseline service, not lifetime absorbing
treatment. The design does not attempt to recover lifetime transit-exposure histories or create an
`ever_observed_near_station` restriction; claims concern first assigned exposure to expansions in
the study window, conditional on each cohort''s `g - 4` residence.

The primary comparison group for each cohort is the RMSP rolling risk set of eligible,
not-yet-treated residents, subject to common-support and baseline-service strata. A local-spillover
sensitivity excludes comparison residents between 2 and 4 km from a new opening. A stricter combined
sensitivity also excludes comparison residents beyond 4 km whose modeled accessibility gain exceeds
a threshold fixed from substantive and measurement criteria before outcome estimation. A
central robustness specification restricts controls to the future Line 6 and Line 17 corridors,
which had not entered operation during the 2011--2023 outcome window. This corridor comparison is
not interpreted as an untreated counterfactual for the entire expansion process: planning,
construction and anchored expectations may already affect those residents. It instead provides a
narrower comparison for the incremental effect of opening and operation, conditional on corridor
selection and implementation. Line 19 is not part of this central robustness pool unless its
distinct planning status and exposure history are evaluated separately.

Initial accessibility implementation holds the 2019 `aopdata` land-use surface fixed across network
scenarios. The preferred main causal specification will instead fix opportunities at each window's
baseline $t_0$ when those inputs are available; the common 2019 surface remains a diagnostic or
sensitivity specification. Exposure is assigned from $t_0$ residence, movers remain in the baseline
cohort, and the analysis does not require a balanced CadÚnico panel.

The current technical subproject is to:

Estimate comparable morning-peak transit travel-time matrices for São Paulo in
2012, 2015, 2019 and 2023. The analytical departure time is 06:50 on
2012-10-03, 2015-07-29, 2019-10-02 and 2023-10-01, with a 15-minute departure
window and a 60-minute maximum trip. The 2015 date is the last Wednesday covered
by that source feed; its calendar ends on Saturday, 2015-08-01.

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
- `source_feed_audit`, `bus_feed_audit` and `rail_feed_audit` are tidy views of
  `gtfstools::get_summary()` for an explicit 06:00--07:00 window. Their HTML and
  RDS artifacts are tracked together, and the common table keeps generic feed
  metrics rather than project-specific transformation assumptions.
- `prepared_feed_audit` and `bus_feed_transform_audit` read tracked JSON
  manifests with row-count changes and referential-integrity diagnostics. The
  intermediate prepared feeds do not rerun the expensive summary calculations.
- `scenario_feed_audit` reuses the cached bus and rail audit rows instead of
  reopening the feeds. `scenario_feed_reports` retains the separate MobilityData
  validation reports.
- `scenario_calendar_overlap` uses `gtfstools::get_calendar_overlap()` as an
  explicit gate: every selected bus feed and the reconstructed rail feed must
  share service on the branch's analysis date before the scenario audit runs.
- The pipeline uses `gtfstools` for active-service queries, aggregate route
  frequencies, feed summaries and referentially complete GTFS filtering.
  Project code remains
  responsible for the H3 x busway-class segment speed surface and explicit rail
  reconstruction because the package's route/trip setters do not represent
  those analytical assumptions.
- The pipeline exports all year-specific bus and rail feeds to one shared R5
  network under `data/r5/all`, with non-overlapping service calendars and
  year/date TTM branches. Scenario audits gate the inputs by confirming that the
  intended services are active at each analysis date.


[LEARN:gtfs] Build routing dates by recycling a shorter vector of day strings,
which silently changed the fifth branch to 2025-10-03 -> enumerate every full
analysis datetime explicitly; the canonical 2025 branch is 2025-10-01.

[LEARN:gtfs] Use 2015-10-07 as the common October routing date even though the
raw 2015 feed calendar ends on 2015-08-01 -> use 2015-07-29, the last covered
Wednesday, to preserve the weekday service pattern without extending the source calendar.

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

[LEARN:workspace] A previously injected Codex workspace pointed to the storage-backed checkout even
though Positron and R were opened in the local checkout → when locations disagree, use the checkout
explicitly identified by the user as canonical and verify `git rev-parse --show-toplevel` before any
read or write; do not infer the active project from a prior session's workspace context.

[LEARN:workflow] `AGENTS.md`, `MEMORY_2.md`, and the initial `CHANGELOG.md` imported on 2026-08-24
described the separate `aoplanduse` repository → treat imported workflow files as templates until
their claims are verified against this repository; `MEMORY.md` and `BACKLOG.md` were the factual
starting points for this project.

[LEARN:diagnostics] A long-lived `renv::paths$lockfile()` query does not establish a lock on
`renv.lock`: project profile initialization runs before that expression. On 2026-09-11 the actual
blocker was an orphaned sandbox coordination lock. Windows APIs confirmed its owner had exited,
but renv 1.2.4's psnice-based liveness check treated the residual process object as alive.
Preserving and renaming the confirmed orphan released the console and old helpers; normal startup
with sandbox enabled completed in 47.67 seconds, and the user confirmed console responsiveness.
Recovery is not permanent recurrence prevention. An old warning reporting 1300 seconds activating
the sandbox is consistent with the recovered wait, not evidence of a new hang. See
`quality_reports/session_logs/2026-09-11_positron-renv-startup.md`.

[LEARN:diagnostics] Slow R console submission and completion in Positron need not be an R, Ark,
renv, or profile-state problem. On 2026-09-15 Ark completed `libr` in 338 ms while the UI waited
seconds and the Codex extension repeatedly failed to watch Git metadata on the network/UNC
repository. Disabling only `openai.chatgpt` made R immediate; assigning it a separate extension
host with `extensions.experimental.affinity` preserved Codex while making `1 + 1` immediate and
R completions sub-second. Treat extension-host contention separately from backend execution time.

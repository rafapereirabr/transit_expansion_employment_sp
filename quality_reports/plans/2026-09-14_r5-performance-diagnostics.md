# Diagnose R5 performance across the server and Mac

## Objective

Separate storage, CPU, JVM configuration and shared-server contention as explanations for the
observed R5 runtime difference. Keep RAIS, CadÚnico and all restricted derivatives on storage6.
Only public transport inputs and synthetic or non-confidential OD samples may be staged locally.

## Current evidence

- On the Mac, tar_meta() reports 2,109.94 seconds for four r5_network branches and 38,325.34
  seconds for four ttm_transit branches.
- On the server, the completed 2012 r5_network branch took 776.64 seconds. This is observational:
  the VM was shared with another active user.
- A short sample of a separate compute-heavy R process showed about one effective CPU core and no
  measurable I/O. The active network builder was observed serializing network.dat to storage6.
- A warm scan of 1,000 renv-library files took 0.785 seconds on storage6 and 0.267 seconds in the
  local cache. This indicates a metadata penalty but is not a complete R5 benchmark.
- Runtime resources are intentionally operational settings. Changing CPU, RAM or scratch location
  must not invalidate analytical targets or their downstream targets merely because execution moved
  between machines.

## Diagnostic sequence

1. Record each current branch elapsed time, network and Parquet size, software versions, effective
   runtime resources, host load and relevant R5 phase timestamps. Treat shared-host runs as
   observational evidence rather than controlled benchmarks.
2. Build a fixed, seeded smoke test for one routing year with the same GTFS, OSM, routing datetime,
   time window and maximum duration on both machines. Use a representative public OD sample.
3. Run the smoke test with network inputs/output on storage6, then with local R5 inputs and local
   scratch while preserving any required artifact on storage6. Do not move restricted data.
4. With storage fixed, compare 1, 2, 4 and 8 R5 threads at fixed JVM heap. Record wall time, CPU time,
   effective cores, peak memory and I/O. Do not clear operating-system caches; label first and warm
   runs explicitly.
5. Repeat the selected configuration twice on the Mac and server using the same commit, inputs, R,
   Java/R5 versions and routing parameters. Prevent Mac sleep and record server contention.
6. Validate row counts and travel-time summaries, then compare one identical full-year branch before
   adopting local staging in the DAG.

## Runtime-resource logging

Add a concise message at the start of calc_ttm() with the network-year label, routing threads,
JVM processor limit and heap size. Preserve the same record in the copied TTM-specific R5 log after
a successful run. The values remain outside target dependency hashes and therefore do not invalidate
downstream targets. A console message alone is transient; the copied log supplies durable evidence.

Do not change build_r5r_network() or its shared helpers in this step. A tar_make() already in
progress has loaded the previous function definitions, so this instrumentation applies only to a
future process. It must not be used as evidence about a TTM that started before the edit.

## Verification

- Format from the repository root with air format .
- Parse R/routing.R without activating the project profile.
- Inspect the diff and confirm that build_r5r_network() and shared log helpers did not change.
- Do not launch an R5 build or TTM solely to test the instrumentation.

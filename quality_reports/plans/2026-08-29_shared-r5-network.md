# Shared R5 network

## Goal

Build one R5 network in `data/r5/all` containing the selected 2012 and 2025 bus and rail feeds,
while leaving the existing year-specific network directories untouched.

## Changes

1. Extend `export_feeds()` so `year = NULL` exports every selected feed into an explicit shared
   subdirectory and rejects duplicate output basenames.
2. Change `r5_feeds` and `r5_network` from year-branched targets into single shared targets.
3. Keep travel-time matrices branched by `routing_spec`, with both dates using the shared network.

## Verification

- Format the R sources with the repository Air configuration.
- Parse `_targets.R` and source the changed functions.
- Exercise `export_feeds()` with synthetic ZIP/PBF inputs in a temporary directory.
- Inspect target dependencies/status; do not run the expensive network build unless already cached.

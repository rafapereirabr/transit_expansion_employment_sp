# Workplace geocode duplication — 2026-11-12

Failure: rais_workplace_files_09185f34d5c7a3fb, saved input rais_active_2023.parquet.
Installed ipeadatalake unir_geoloc joins id_estab and cei_vinc; source geocodes can
repeat keys. A restricted read-only aggregate diagnostic found exact repeated
projected rows, but no distinct location records or conflicting coordinates per key.
No identifiers, coordinates or row-level values were printed.

Added deduplicate_workplace_locations(): removes only identical rows, audits raw/unique
counts, fails for conflicting records. get_work_location writes an additional lookup
audit; the DAG exposes it with the existing workplace quality audit. Employment links
and annual extraction code were preserved.

Verification: source/parse and synthetic Arrow tests for exact repeats, changed
coordinates, changed quality, empty inputs, and file selection passed. A second
in-memory lookup of the failed 2023 key set confirmed one row per key after correction.
No link extraction, targets execution or row-level persistence was performed. R emitted
locale startup warnings; git diff --check passed. Air remains unavailable.

Next: user resumes tar_make(names = tidyselect::all_of("rais_workplaces")), then checks
the workplace quality and lookup audits. Other years and full target execution remain
unverified; distinct-location conflicts will still fail rather than choose arbitrarily.

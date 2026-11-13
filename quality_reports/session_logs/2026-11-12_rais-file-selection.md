# RAIS file-selector failure — 2026-11-12

Failed target: rais_records_37527d9a4ccfa412. Saved command:
rais_files[["records"]]. Saved dependency rais_files_2ec048501955e671 had
five paths, names NULL. Named indexing reproduced subscript out of bounds.
All five saved 2012 artifacts existed. No microdata were opened.

Added rais_artifact() with exact basename matching and exactly-one-match validation.
Updated active-link, person-year, three audit and both workplace selectors in _targets.R.
The read_rais extraction body and existing files/cache were preserved.

Verification: standalone Rscript --vanilla sourced function definitions, parsed _targets.R,
reproduced the original index error, selected rais_active_2012.parquet, tested reversed
order, audit selection, workplace selection, and rejection of missing/duplicate matches.
Tests passed. The initial diagnostic probe incorrectly indexed an environment numerically;
that probe was corrected before successful checks. R emitted locale startup warnings.
git diff --check passed. Air is unavailable. No targets or source extraction executed.

Next: user resumes tar_make(names = tidyselect::all_of("rais_workplaces")); targets
should reuse completed extractions where still current. Full downstream build is unverified.

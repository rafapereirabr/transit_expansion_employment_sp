# Build the longitudinal CadÚnico–RAIS pipeline

## Purpose

Build the mechanical R/`targets` layer that supports the causal design without embedding the
identification strategy in data-extraction functions. The pipeline should first assemble a broad,
auditable longitudinal source population and only then construct window-specific baseline cohorts.

This plan is separate from the estimand and identification notes. It covers extraction, linkage,
schemas, branching, cardinality checks, restricted-data outputs and target verification. It does not
select the final spatial DiD estimator, accessibility null band or distance comparison radius.

## Central distinction

Maintain two different populations:

1. **Extraction universe:** everyone observed as resident in the eligible São Paulo geography in at
   least one configured CadÚnico snapshot. Extract their CadÚnico history and RAIS records for every
   configured source year, regardless of whether they reappear in CadÚnico in that year.
2. **Analysis cohort:** for each intervention window, people eligible and resident in the study
   geography at baseline `t0`. Membership must depend only on baseline information. Follow-up
   CadÚnico residence and RAIS outcomes do not determine inclusion.

The extraction universe is a staging device, not the causal target population. Using future
CadÚnico presence to define an analysis cohort would condition on post-baseline information.

## Current behavior and failure mode

`read_cad_families()` already implements a family-level catchall within the configured snapshots:
it unions family IDs observed in the selected municipalities in any year and retains those family
IDs in every selected snapshot where they occur.

The catchall is lost at the CadÚnico–RAIS boundary. `_targets.R` currently branches
`cadunico_ind` by year and then maps `read_rais()` over matching pairs of `time_window` and
`cadunico_ind`. Consequently, RAIS year `y` is filtered to CPFs present in the CadÚnico individual
file for year `y`. A person observed in CadÚnico in one configured year but absent from another is
not searched for in RAIS in the latter year.

Family IDs may also change when household composition changes. The longitudinal extraction roster
therefore needs a person-level CPF union after the family catchall rather than relying on stable
family IDs alone.

## Proposed DAG

```text
source_year_spec
├── cad_family_snapshots ──→ ever_sp_family_ids
│                                  ↓
├── cad_person_snapshots ──→ ever_sp_person_ids
│                                  ├──→ cad_person_history
│                                  └──→ rais_jobs_by_year
│                                              ↓
│                                      rais_person_year
│                                              ↓
analysis_windows ──────────→ baseline_cohorts ──→ window_outcomes
                                     │                  │
                                     └──→ mobility_audit│
                                                        ↓
                                                analysis_panel
```

## Target contracts

### Specifications

- `source_year_spec`: explicit CadÚnico and RAIS extraction years. Do not reuse routing years as
  labor-data years.
- `analysis_windows`: one row per causal window, including `window_id`, `t0`, `t1`, required prior
  outcome years, routing scenario years and land-use year. Preserve the exact infrastructure
  opening date, use its calendar year as the first post-treatment year for the December 31 outcome,
  and derive days of operation before that first outcome snapshot. Do not round late-year openings
  into the following cohort.
- `rais_outcome_spec`: define whether the primary indicator means a qualifying job active on
  December 31, any qualifying job during the year, or another rule.
- `eligibility_spec`: baseline age, geography, identifier and geocode requirements.

The configured labor-data snapshots are 2011, 2015, 2019 and 2023. Keep these years explicit in
`source_year_spec`, not in function internals. The existing 2025 transport scenario is a separate
pipeline product and must not enter the labor panel as an outcome or CadÚnico/RAIS snapshot year.

### CadÚnico extraction

- `cad_family_snapshots`: family records by source year, preserving observation year and residence.
- `ever_sp_family_ids`: unique family IDs observed in the eligible geography in any configured
  snapshot.
- `cad_person_snapshots`: first-pass people belonging to catchall families in each configured
  snapshot, used to discover the person-level roster.
- `ever_sp_person_ids`: one restricted CPF roster formed from the union across all person snapshots.
- `cad_person_history`: second-pass extraction of the configured CadÚnico snapshots filtered by
  `ever_sp_person_ids`, regardless of the person's family ID or municipality in that year. Join each
  person to their current family record to recover contemporaneous residence. This captures moves
  and family-ID changes without requiring presence in every year.
- `cad_linkage_audit`: missing CPFs, duplicate CPFs, changing family IDs, multiple residences,
  geocode quality and counts added by each snapshot.

Do not drop every CPF that appears more than once without first classifying the duplication. Resolve
or flag duplicates using an explicit rule and preserve an audit of exclusions.

### RAIS extraction and outcomes

- `rais_jobs_by_year`: for each RAIS year, query all CPFs in `ever_sp_person_ids`. Dynamic branching
  should be over RAIS year only; the CPF roster is a common upstream dependency, not a same-year
  CadÚnico branch.
- `rais_person_year`: collapse job records to exactly one row per CPF-year for the primary outcome.
- `rais_job_history`: retain multiple jobs, establishments, earnings, hours and workplaces in a
  separate table for secondary outcomes and mechanisms.
- `rais_linkage_audit`: count queried CPFs, matched CPFs, multiple active jobs, records removed by
  the outcome rule, invalid establishment identifiers and unclassifiable person-years.

The primary person-year table must distinguish:

- `formal = 1`: satisfies the prespecified RAIS employment rule;
- `formal = 0`: validly searched and no qualifying formal job found;
- `formal = NA`: linkage or source coverage does not support interpreting absence as zero.

The initial rule is a qualifying formal job active on December 31. Missing or invalid CPFs are
excluded and audited before baseline-cohort construction rather than represented as missing
outcomes. For each valid roster CPF in a fully processed RAIS year, the person-year table must start
from the complete CPF roster and use an exact left join to RAIS: no matching qualifying record is
`formal = 0`. Use `formal = NA` only when source coverage, extraction completeness or active-status
classification is not established. Determine `formal` before applying wage, hours, establishment or
workplace-geocoding validity filters; failures in those secondary fields leave `formal = 1` and make
only the affected secondary outcomes unavailable.

Workplace geocoding must not determine inclusion in the primary formal-employment outcome. Attach
workplace data only to the separate job-level or secondary-outcome tables.

#### Outcome comparison and sequencing

| Outcome | Measures | Main advantage | Main limitation | Workplan priority |
|---|---|---|---|---|
| Formal employment on December 31 | Point prevalence | Simple and communicable | Depends on one reference date | Initial |
| Any formal employment during the year | Any formal insertion | Captures short spells | Treats one day and a full year alike | Deferred diagnostic |
| Days or month-equivalents in formal employment | Intensity of formal attachment | Uses the full annual employment trajectory | Requires careful reconstruction and union of overlapping job spells | Deferred secondary outcome |
| Quarterly or monthly formal-employment panel | Timing and dynamics | Can distinguish entry, exit and persistence | Strong serial correlation and greater analytical complexity | Deferred extension |

The initial wage outcomes are: (1) real contractual hourly wage among people formally employed on
December 31, and (2) an indicator for formal employment at or above the cohort-specific employed-
worker median at `g - 4`. For (1), remove exact duplicate links and aggregate concurrent active jobs
as total contractual salary divided by `(52 / 12) * total weekly contracted hours`; use `NA` for
valid non-employed person-years. For (2), use zero for valid non-employed person-years and `NA` only
when coverage or wage classification fails. Express wages and frozen baseline thresholds in
December 2023 reais using the IPCA. Before implementation, verify that extracted contractual salary
is standardized to a monthly unit across salary types. Treat (1) as conditional on observed formal
employment and use (2) for the full-cohort policy contrast.

### Window-specific analytical data

- `baseline_cohorts`: branch over `analysis_windows`; select eligibility and residence using only
  the CadÚnico snapshot at `g - 4`. Construct a rolling risk set for each opening cohort. Define
  `G_i` as the earliest 2015--2023 opening year for which an eligible, still-at-risk person''s
  baseline address is newly covered by the opening; keep `G_i` absorbing in later risk sets. Treat
  proximity to older stations at earlier observed addresses as historical baseline service, not as
  lifetime treatment, and do not require an `ever_observed_near_station` history. Retain all
  eligible not-yet-treated municipal residents as the primary control pool, subject to explicit
  common-support and baseline-service restrictions. Also flag baseline residence in the future
  Line 6 and Line 17 corridors for the central robustness specification; this alternative estimates
  an opening/operation contrast conditional on planning and implementation. Keep Line 19 separate
  until its different planning and exposure history has been assessed.
- `window_outcomes`: join each baseline cohort to `rais_person_year` for the required baseline,
  follow-up and prior-trend years. Do not require follow-up CadÚnico observation.
- `mobility_audit`: describe later observed residences without changing baseline exposure or primary
  cohort membership.
- `analysis_panel`: one row per baseline person-year after explicitly classifying outcome
  availability. An unbalanced observation structure is allowed; missing outcomes are not zeros.

Exposure targets should join later by `baseline_h3 × window_id`. They should not be constructed
inside CadÚnico or RAIS readers.

## Implementation phases

### Phase 1 — specifications and schemas

1. Replace the overloaded `time_window` parameter with separate source-year and analysis-window
   specifications.
2. Define schemas and unique keys for family snapshots, person snapshots, CPF roster, RAIS jobs,
   RAIS person-years and baseline cohorts.
3. Fix the primary RAIS outcome rule and document its temporal interpretation.

### Phase 2 — longitudinal CadÚnico roster

1. Refactor the family catchall into inspectable snapshot, ID-union and history targets.
2. Build the person-level CPF union across the first-pass snapshots.
3. Re-read/filter every configured person snapshot using the common CPF roster, then join the family
   ID observed in that year to its corresponding family/residence record.
4. Add duplicate, missing-ID, family-transition and geography audits.
5. Verify that a person found in only one CadÚnico snapshot remains in the extraction roster and
   that a person who changes family ID can still be followed.

### Phase 3 — RAIS for the full extraction universe

1. Split RAIS job extraction from CadÚnico joining.
2. Map RAIS years against the common `ever_sp_person_ids` roster.
3. Aggregate the primary person-year outcome separately from multiple-job data.
4. Verify that a CPF found in CadÚnico only in year `a` can still receive a RAIS outcome in year
   `b`.

### Phase 4 — baseline cohorts and panels

1. Build one baseline cohort per analysis window using only `t0` information.
2. Attach RAIS person-year outcomes for every required year.
3. Add mobility histories as outcomes/diagnostics rather than inclusion conditions.
4. Produce a compact design-support audit before attaching distance or accessibility exposure.

### Phase 5 — exposure interfaces

1. Compute straight-line distance from each `g - 4` residence to station points and flag new
   coverage by the 2 km buffer for each opening cohort.
2. Join walking travel time to stations as a complementary exposure. It may share the exposure
   table with straight-line distance or use a separate target, provided keys and provenance remain
   explicit.
3. Join accessibility change by baseline H3 and window.
4. Assert many-to-one joins from person rows to exposure rows.
5. Keep restricted person-level panels under `restricted_dir`; export only approved aggregates.

## Required invariants

- `ever_sp_person_ids`: one row per canonical CPF.
- `rais_person_year`: at most one row per CPF-year.
- `baseline_cohorts`: at most one row per CPF-window.
- `analysis_panel`: at most one row per CPF-window-year before explicitly modeled job-level joins.
- Baseline cohort size is unchanged when follow-up CadÚnico snapshots are removed.
- RAIS extraction counts do not depend on same-year CadÚnico presence.
- Every `formal = 0` has evidence that the CPF-year was validly covered by the RAIS search.
- Multiple jobs do not duplicate the primary outcome row.
- Residence and exposure at `t0` remain fixed after a move.
- All joins declare keys and audit unmatched and duplicated rows.

## Verification plan

1. Use a disclosure-safe synthetic fixture to test family-ID changes, intermittent CadÚnico
   presence, multiple jobs, missing identifiers, mobility and valid RAIS zeros.
2. Run `air format .` after R edits.
3. Inspect `targets::tar_outdated()` and run the narrowest target sequence for specifications,
   roster, RAIS person-years and audits.
4. Check schemas, unique keys, row-count conservation and join cardinality at every boundary.
5. Run the 2015--2019 pilot cohort first; it can use existing routing/accessibility inputs.
6. Defer 2023 exposure estimation until the 2023 transport scenario is built or an explicit proxy
   is approved.

## Non-goals

- Do not reactivate the commented legacy pilot targets.
- Do not use `make_panel()` unchanged; it mixes balancing, workplace geocoding, TTM joins and legacy
  treatment fields.
- Do not select the null band, distance radius or final estimator in this workplan.
- Do not write restricted identifiers, row-level data or machine-local restricted paths to the
  repository.

## First implementation slice

Implement and verify only:

1. `source_year_spec` and `analysis_windows`;
2. `cad_person_snapshots` and `ever_sp_person_ids`;
3. `rais_jobs_by_year` and `rais_person_year`;
4. `cad_linkage_audit` and `rais_linkage_audit`.

Stop before exposure or estimation. This slice directly tests whether the catchall works across
years and removes the present dependence of RAIS observation on same-year CadÚnico presence.

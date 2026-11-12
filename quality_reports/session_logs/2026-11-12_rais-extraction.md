# RAIS extraction revision — 2026-11-12

## Scope and decisions

User requested incorporation of approved RAIS choices and deferred the family_records
snapshot-window optimization until Monday 2026-11-16. No R, formatter, tests, source
reads, targets or bootstrap were executed. Only source editing and textual review.

## Implemented contracts

- build_cpf_roster(): union of CPF values from risk_set_<year>.parquet files,
  excluding audit files. One row per person, not person-year or person-risk-set.
- Annual read_rais(): the same union is looked up nationwide in every time_window year.
  semi_join prevents duplicate roster rows from multiplying jobs.
- Exact duplicates are removed over the complete matched source schema, before
  projection; all source fields of active records are retained. Inactive and
  unclassifiable-status records contribute to audit/status before being discarded.
- Output files: active links; restricted CPF-year employment counts and classification;
  aggregate status/type/salary-quality link audit; aggregate person audit; manifest.
- Type/status code lists are explicit in rais_spec. The initial salaried-employee mapping
  covers urban/rural fixed and indefinite contracts, temporary workers, apprentices and
  the Law 9601 category (10,15,20,25,50,55,60,65,70,75,90). Public statutory,
  non-effective non-CLT, casual, non-employee director and administrative contracts are
  excluded (30,31,35,40,80,95,96,97). Unrecognized codes remain uncertain.
  The codebook is grounded in MTE Manual RAIS 2020, C.1; annual harmonized-source
  compatibility still needs to be inspected before reporting.
- Employment = 1 if any observed active eligible job exists. Potential eligible jobs
  with uncertain status/type prevent assigning zero in their absence.
- source coverage is separate from successful extraction. coverage_confirmed_years
  defaults empty: no-match/all-inactive cases remain NA until a year is explicitly
  confirmed. Positive observed employment remains 1. Manifest records this distinction.
- Salary and hours are preserved, with positive/finite flags. salary_type_col is
  configurable and autodetection checks only named candidates, failing on ambiguity.
  If source unit is absent, it remains unknown; monthly_salary_verified is FALSE until
  explicitly verified. Monthly type code 1 is flagged as monthly; other source types
  remain raw. There is no unverified conversion to hourly/monthly wages.
- get_work_location() is now a separate file target producing an establishment-key
  lookup and audit. Empty inputs produce typed empty outputs without calling the
  geocoder. Duplicate lookup keys fail. Missing locations preserve establishment keys.
  Quality requires valid coordinates and known nonnegative deviation <= 500 m.
  Link-level employment remains independent of the geocoding result.
- DAG now defines cpf_roster, rais_spec, rais_files, rais_records, rais_person_year,
  rais_audits and separate workplace outputs. Outcome aggregation and estimation
  remain separate, not activated by these extraction helpers.

## Interpretation and remaining work

The outcome layer must use the stored per-person uncertainty flags, not just active
links: an absent active row is not automatically zero. When an active eligible job
coexists with uncertain potentially eligible links, employment is observed but full
job coverage for wage outcomes cannot be assumed.

Next build the two wage/threshold versions, IPCA deflation, distance from baseline
residence and final person x cohort x year panel. Maintain all years since 2011,
regardless of CadUnico reappearance. Prospective treatment G is computed separately
from openings at each baseline address and determines when future controls cease
to be controls. No future outcomes or observed moves should assign exposure.

## Verification

git diff --check passed. This is textual verification only; no functional validation.
After execution is authorized: check syntax/format, synthetic empty/mixed/unknown cases,
annual schema and salary-unit evidence, source coverage, graph branching, and a narrow
single-year extraction. Then check manifests/cardinality before broader extraction.

Source: https://www.gov.br/trabalho-e-emprego/pt-br/servicos/empregador/rais/manualrais2020.pdf

The repository patch wrapper had already failed earlier in this conversation on
native Windows multiline argument handling; files were written directly as in the
preceding authorized revisions, without altering that helper.

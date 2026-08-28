# AGENTS.md

**Project:** aoplanduse **Institution:** IPEA (Instituto de Pesquisa Econômica Aplicada) **Branch:** main

------------------------------------------------------------------------

## What this project does

`aoplanduse` builds a spatially-aggregated database of access to opportunities across urbanized areas in Brazil. It geocodes several administrative microdata sources — formal jobs (RAIS), the school census, health facilities (CNES), and welfare/social-assistance reference centers (CRAS) — and aggregates each to an H3 hexagonal grid at **resolution 9**, ultimately producing a per-city, per-year table combining all domains.

The pipeline is an R [`targets`](https://books.ropensci.org/targets/) workflow (`_targets.R`), parallelized with `crew`, using `geocodebr`/`ipeadatalake` for geocoding, `duckdb`/`arrow`/parquet for storage, and `renv` for dependency management.

------------------------------------------------------------------------

## Core Principles

- **Plan first** — enter plan mode before non-trivial tasks; save plans to `quality_reports/plans/`.
- **Verify after** — after any pipeline change, run the affected `targets::tar_make()` scope and confirm outputs exist (see `.agents/rules/orchestrator-research.md`).
- **The `targets` DAG is the single source of truth** — `_targets.R` + `R/*.R` define what gets built; there is no parallel "canonical" artifact to keep in sync (unlike a Beamer/Quarto pair).
- **Quality gates are advisory** — see "Quality" below; nothing here is a hard pre-commit block beyond `git-guardrails.py`.
- **`[LEARN]` tags** — when corrected, save `[LEARN:category] wrong → right` to [MEMORY.md](MEMORY.md).

Cross-session context lives in [MEMORY.md](MEMORY.md); past plans, specs, and session logs are in [quality_reports/](quality_reports/).

------------------------------------------------------------------------

## Folder Structure

```         
aoplanduse/
├── AGENTS.md                    # This file
├── .agents/                     # Rules, skills, agents, hooks (LLM-agnostic; see WORKFLOW_QUICK_REF.md)
├── _targets.R                   # The targets pipeline definition
├── _targets/                    # targets' own cache/metadata store (gitignored contents)
├── R/                           # Pipeline functions, loaded via targets::tar_source()
│   ├── jobs.R                   #   RAIS geocoding + H3 aggregation
│   ├── schools.R                #   School census geocoding + H3 aggregation
│   ├── adjust_rais_outliers.R   #   RAIS outlier handling
│   ├── get_munis.R              #   Municipality list from pop_units
│   ├── make_rais_dic.R          #   RAIS variable dictionary
│   └── utils.R
├── figures/                     # Map scripts (maps_br.R, maps_muni.R) — see r-code-conventions.md §4
├── data-raw/                    # Small reference inputs (e.g. nat_jur.xlsx)
├── quality_reports/             # Plans, session logs, merge reports, checkpoints
├── templates/                   # Session log, quality report, skill, and other templates
├── logs/crew_workers/           # crew worker logs (gitignored)
└── renv.lock, aoplanduse.Rproj, air.toml   # Dependency lock, RStudio project, R formatter config
```

------------------------------------------------------------------------

## Commands

``` r
# Restore the dependency environment
renv::restore()

# Run the full pipeline (or scope it while iterating)
targets::tar_make()
targets::tar_make(names = tidyselect::any_of(c("rais_agg")))

# Inspect the DAG / what's outdated
targets::tar_visnetwork()
targets::tar_outdated()

# Read a built target
targets::tar_read(rais_agg)
```

``` bash
# Format R code (config at air.toml: tabs, line-width 101)
air format .
```

There is no `quality_score.py`, `sync_to_docs.sh`, or pre-commit hook script in this repo — see "Quality" below for what's actually enforced.

------------------------------------------------------------------------

## Quality (advisory)

| Score | Checkpoint    | Meaning                                    |
|-------|---------------|--------------------------------------------|
| 80    | Commit        | Good enough to save                        |
| 90    | Merge to main | Ready to feed downstream targets/consumers |
| 95    | Excellence    | Aspirational                               |

These are applied by judgment inside `/review-r` and `/commit` (see `.agents/rules/quality-gates.md`) — there is no scoring script and no pre-commit hook beyond `.agents/hooks/git-guardrails.py` (blocks a narrow set of destructive git commands, warns on hardcoded machine paths).

------------------------------------------------------------------------

## Skills Quick Reference

Full skill bodies live under [`.agents/skills/`](.agents/skills/). By workflow:

- **Pipeline work:** `/review-r` (code quality against `r-code-conventions.md`), `/diagnose` (root-cause a wrong/failing target), `/audit-reproducibility` (verify a rerun reproduces prior outputs within tolerance)
- **Environment / handoff:** `/capture-environment` (snapshot `renv.lock` + session info), `/replication-package` (assemble a reproducibility package for a downstream deposit)
- **Research framing:** `/interview-me` (formalize a fuzzy question into a spec)
- **Meta / workflow:** `/commit` `/learn` `/new-skill` `/checkpoint` `/context-status` `/compress-session` `/deep-audit` `/promote-memory` `/permission-check`

------------------------------------------------------------------------

## Pipeline Domains

| Domain | Source | Status | Key `R/` file | `_targets.R` targets |
|---------------|---------------|---------------|---------------|---------------|
| Jobs | RAIS (formal employment) | Implemented (years 2022–2025) | `R/jobs.R`, `R/adjust_rais_outliers.R`, `R/make_rais_dic.R` | `rais_years`, `rais_dic`, `rais_h3`, `rais_agg` |
| Schools | School census | Implemented (years 2022–2023) | `R/schools.R` | `school_years`, `escolas_coords`, `escolas_h3`, `escolas_aggreg` |
| Health facilities | CNES | Not yet implemented | — | — |
| Welfare / social assistance | CRAS registry | Not yet implemented | — | — |

Spatial unit of analysis (`pop_units`, `munis`) and the H3 resolution (`h3_res = 9`) are shared across all domains — see the top of `_targets.R`.

------------------------------------------------------------------------

## Pipeline main steps

### jobs (RAIS)

**Cadeia de funções:** `get_rais()` (`R/jobs.R`, target `rais_h3`, `pattern = map(rais_years)`, um branch por ano) → `aggreg_rais_h3()` → `summarise_jobs()` (+ `adjust_rais_outliers()`) (`R/jobs.R` + `R/adjust_rais_outliers.R`, target `rais_agg`, `pattern = cross(rais_h3, h3_res)`, um branch por ano × resolução h3).

**Etapa 1 — `get_rais()`, por ano:**

1. Lê RAIS **estabelecimentos** do ano via `ipeadatalake::ler_rais()` — colunas: `tipo_estab, id_estab, cei_vinc, codemun, uf, cep, nat_jur2018, clas_cnae20, qt_vinc_ativos`.
2. `inner_join` com `munis` por `codemun == code_muni_6` (código IBGE de 6 dígitos) — já restringe a RAIS aos municípios de interesse (dentro de `pop_units`/urban concentrations); adiciona `code_pop_unit`, `code_muni`, `year`. **Estabelecimentos fora desses municípios são descartados aqui, silenciosamente.**
3. Geocodifica via `ipeadatalake::adicionar_geoloc()` (lat/lon + hexágonos H3 res 7–9 + métricas de precisão); filtra `desvio_metros < 1200` (descarta geocodificações com desvio acima de 1200m) e descarta as colunas de metadado do geocode (`lat, lon, precisao, desvio_metros, tipo_resultado, endereco_encontrado`).
4. **H3 resolução 9 é o limite mais fino disponível na geocodificação.** Resoluções mais grossas (`h3_res` fora de 7–9) são derivadas via `h3o::get_parents()` a partir de `h3_09` — não é possível pedir resolução mais fina que 9 (o código emite apenas uma mensagem informativa nesse caso, não erro).
5. `filter_legal_nature()`: remove estabelecimentos de **poderes de governo** (executivo/legislativo/judiciário — prefixos `exe_`, `leg_`, `jud_` no dicionário `data-raw/nat_jur.xlsx`), pois esses têm atribuição de endereço de trabalho não confiável (vínculo registrado num endereço central/administrativo, não no local real de trabalho). **Mantém** — via `keep_nature`, chamado em `_targets.R` como `c("public_adm", "public_companies", "organizations")` — autarquias/fundações/comissões, empresas públicas/de economia mista, e organismos internacionais/diplomáticos. `nat_jur.xlsx` é versionado por ano (colunas `from`/`to`), pois a classificação de natureza jurídica muda ao longo do tempo (vintage `nat_jur1995` vs `nat_jur2018`) — o filtro seleciona a vintage certa por `from <= year & (to >= year | is.na(to))`.
6. Salva em parquet: `{save_path}_{year}.parquet` (ex.: `data-temp/rais_2022.parquet`). Target `rais_h3` é `format = "file"` — o valor do target é o **caminho do arquivo**, não os dados em memória.

**Etapa 2 — `aggreg_rais_h3()` → `summarise_jobs()`, por ano × h3_res:**

1. Lê o parquet do ano (`arrow::open_dataset`), descarta colunas H3 de outras resoluções, renomeia a coluna da resolução alvo para `h3_address` genérico + adiciona `h3_res`.
2. **Recontagem "bottom-up" dos vínculos** (`bottom_up = TRUE`, padrão e não sobrescrito em `_targets.R`): em vez de confiar em `qt_vinc_ativos` do arquivo de estabelecimentos, recarrega a RAIS **vínculos** (microdados de contrato individual) do mesmo ano, filtra para **vínculos ativos apenas** (`data_deslig` ausente = sem data de desligamento) e remove duplicatas exatas, depois `inner_join` com os estabelecimentos já filtrados/geocodificados na Etapa 1 — isso propaga o filtro de natureza jurídica e o filtro geográfico da Etapa 1 para o nível de vínculo.
3. Agrega ao **nível de estabelecimento** (grupo: hex, h3_res, ano, `codemun`, `id_estab`, `cei_vinc`, + `clas_cnae20` quando `adjust_outliers = TRUE`, pois o ajuste de outliers opera por subclasse CNAE): `qt_vinc_ativos = n()` (a recontagem bottom-up) + médias de firma (ex.: `rem_med_r`, salário médio).
4. **Composição demográfica/setorial**: para cada variável em `counts` (neste projeto: `clas_cnae20` → setor de atividade via `rais_dic$clas_cnae20`, `grau_instr` → escolaridade via `rais_dic$grau_instr`; `genero`/`raca_cor` já existem no dicionário mas não estão habilitados em `_targets.R`), mapeia os códigos brutos via `rais_dic` (`make_rais_dic()`) e calcula a **participação (share) de vínculos por categoria dentro do estabelecimento** (`pct = n / sum(n)`), pivotada para colunas largas (ex.: `industry_1`…`industry_7`, `educ_1`…`educ_3`). **São shares no nível do estabelecimento, não contagens absolutas** — importante porque a agregação ao hexágono precisa ponderar essas colunas, não somá-las.
5. **Ajuste de outliers** (`adjust_rais_outliers()`, ver abaixo) — aplicado ao `qt_vinc_ativos` no nível de estabelecimento, **antes** de reincorporar as shares de composição calculadas no passo 4.
6. Agrega ao **nível do hexágono** (grupo: hex, h3_res, ano): `total_firms = n_distinct(id_estab)`, `total_jobs = sum(qt_vinc_ativos)` (já pós-ajuste de outliers), e médias/shares agregadas via `wtd_mean(x, qt_vinc_ativos)` — **média ponderada pelo `qt_vinc_ativos` ajustado**, arredondada a 4 casas decimais. Este é exatamente o padrão de ponderação documentado em `.agents/rules/r-code-conventions.md` §3/§6 — nunca usar `mean()` simples aqui.
7. **Checagem de sanidade (hard stop):** as shares de composição (`industry_*`, `educ_*` etc.) devem somar ~100% por hexágono após a agregação; se alguma não somar, a função **para com `stop()`** e um erro explícito — não retorna dado silenciosamente errado.
8. Renomeia `rem_med_r` → `avg_wage`; salva em `{save_dir}/rais_{year}_{hex_res}.parquet` (ex.: `output/rais/rais_2022_9.parquet`).

**`adjust_rais_outliers()` (`R/adjust_rais_outliers.R`) — correção de setores com má atribuição de local de trabalho:**

Adaptado de [`ipeaGIT/acesso_oport`](https://github.com/ipeaGIT/acesso_oport/blob/master/R/fun/empregos/empregos.R). Alguns setores CNAE têm vínculos sistematicamente atribuídos ao endereço da sede/matriz em vez do local real de trabalho (ex.: uma concessionária de serviço público registra todos os empregados no endereço-sede), gerando concentrações espúrias de empregos em poucos hexágonos.

- **Divisões CNAE (2 dígitos) marcadas como problemáticas** (`faulty_div`): 35, 36, 38, 41, 42, 43, 49, 51, 64, 78, 80, 81, 82, 84 — eletricidade/gás, captação/tratamento de água, coleta de resíduos, construção (edifícios/infraestrutura/serviços especializados), transporte terrestre e aéreo, atividades financeiras, seleção/locação de mão-de-obra (78), vigilância/segurança, serviços para edifícios, serviços de escritório/apoio administrativo, administração pública.
- **Grupo CNAE (3 dígitos) marcado à parte** (`faulty_gru`): 562 (catering/bufê) — uma exceção de granularidade mais fina dentro da divisão 56 (que não é toda problemática).
- **Método**: para os registros marcados como `faulty`, calcula percentis de `qt_vinc_ativos` (p75, p80, p85, p90, p95, p97, p99, p100) — **agrupado apenas por `cat`** (a divisão ou grupo CNAE problemático — `groups` é `NULL` na chamada real em `summarise_jobs()`, então **não há agrupamento por hexágono, município ou ano dentro desta função**; os percentis são calculados sobre o conjunto de dados já filtrado por ano que chega até aqui). Identifica o "salto" mais acentuado entre percentis consecutivos (maior razão `pXX/pYY`, dando preferência ao salto mais alto na cauda quando há empate) como ponto de corte.
- Calcula duas formas de ajuste: `sharp_95` (winsorização simples no p95) e `dynamic` (encolhimento suave acima do ponto de corte: `pct_value + (valor_original − pct_value) / razão_do_salto` — não é um cap rígido, suaviza a curva).
- **`qt_vinc_ativos` é de fato sobrescrito pelo ajuste** (`R/adjust_rais_outliers.R:135`: `!!vinc_sym := ifelse(adjusted > 0, dynamic, {{vinc_sym}})`) — para registros marcados como outlier (`adjusted == 1`), o valor original é substituído pelo `dynamic` (o encolhimento suave); os demais mantêm o valor original. A coluna `adjusted` (0/1) permanece no resultado, então dá pra saber quais firmas foram corrigidas. `sharp_95` é calculado mas não usado na substituição final — só serve de comparação/diagnóstico. *(Corrigido em 2026-08-24 — antes dessa data a linha de substituição estava comentada; ver `MEMORY.md` para o histórico.)*
- Invariante checado: `stopifnot("Some firms were dropped in the process." = nrow(rais) == nrow(rais_new))` — o `left_join` do ajuste não pode alterar o número de linhas (nem duplicar por join fan-out, nem descartar firmas).



### schools


------------------------------------------------------------------------

## Contact / Repo

`github.com/ipea/aoplanduse`. Working with `.agents/` and this workflow for the first time? See [`.agents/WORKFLOW_QUICK_REF.md`](.agents/WORKFLOW_QUICK_REF.md).

# Expansão do metrô, do trem e do monotrilho em São Paulo e o emprego formal de pessoas do CadÚnico

Código de replicação do relatório dos desenhos (estudo CAF *Urban Transport and Employment — São Paulo*; Gregório Luz,
Rafael Pereira, Arthur Bazolli). Pergunta: as aberturas de metrô, trem e monotrilho em São Paulo (2013–2022) ajudaram
pessoas de baixa renda do CadÚnico a conseguir emprego formal (acessibilidade a partir de casa) e a mantê-lo (tempo de
viagem casa → trabalho)?

Tudo usa a **data exata de serviço pleno** de cada abertura (cada data é um evento; referência = mês anterior ao serviço
pleno; horizontes de 6, 12, 18 e 24 meses; placebo 25 meses antes da primeira abertura). Desenhos do relatório:

| Desenho | O que compara | Estimador |
|---|---|---|
| A | acessibilidade a empregos a partir de casa | Borusyak & Hull (199 sorteios das datas) |
| B | tempo de viagem ao próprio emprego | Borusyak & Hull |
| C | destino de quem pediu demissão | Borusyak & Hull |
| D | tratados × controles reponderados | IPTW duplamente robusto (pesos cortados no p99) |
| E | tratados × quem terá ganho da mesma faixa mais tarde | diferença em diferenças com controles futuros |

## Dados restritos

CadÚnico e RAIS são **restritos**: nenhum dado, identificador ou caminho da máquina está neste repositório. `data/` e
`outputs/` ficam fora do git, e os caminhos externos ficam só no arquivo local `R/00_config_local.R` (não versionado;
modelo em `R/00_config_local_exemplo.R`).

## Como rodar

Requisitos: R 4.6 com `targets`, `crew`, `arrow`, `data.table`, `fixest`, `sf`, `h3jsr`, `r5r` 2.5.0 (Java 21) e
Quarto. Sempre a partir da raiz do repositório:

1. Copie `R/00_config_local_exemplo.R` para `R/00_config_local.R` e preencha os caminhos dos dados.
2. Rode o pipeline:

```
Rscript -e "source('R/00_ambiente_r5.R'); targets::tar_validate()"   # confere o pipeline (não roda nada)
Rscript -e "source('R/00_ambiente_r5.R'); targets::tar_make()"       # roda o que estiver desatualizado
```

Variáveis de ambiente (opcionais):

| Variável | Efeito |
|---|---|
| `TSP_MONTANTE` | vazio (padrão): as etapas a montante só acompanham os arquivos já prontos; `leve`: roda as que não roteiam; `tudo`: roda também o roteamento r5r (dias de máquina) |
| `TSP_PENDENTES=1` | inclui a robustez R4 (curva mês a mês) e R5 (endereço-base g−1 a g−4) |
| `TSP_PESADOS`, `TSP_LEVES` | estimações pesadas e alvos leves ao mesmo tempo (padrão 3 e 4) |
| `TSP_RESERVA_GB`, `TSP_ESPERA_S` | memória livre que deve sobrar (padrão 60 GB) e intervalo entre largadas pesadas (padrão 120 s) |

Log de cada alvo: `outputs/logs_pipeline/<alvo>.txt`. O relatório sai em `outputs/relatorio/relatorio_desenhos.html`.

## Relatório renderizado

`relatorio/relatorio_desenhos.html` é uma cópia do relatório gerado pelo pipeline (só números agregados). A versão de
24/11/2026 ainda tem a acessibilidade calculada com os empregos de 2015; a versão final, com os empregos de 2012 e as
matrizes de tempo até 240 min, vai substituí-la.

## Estágios

| Estágio | Scripts |
|---|---|
| 0. Redes e insumos de apoio (rodados antes do pipeline) | R/34 (catálogo das 27 redes), R/35 (GTFS de trilhos de cada rede), R/39 (checagens), R/61 (hexágonos-sede da Sé), R/85 (celetistas), R/128 (empregos por hexágono), R/130 (matrizes de tempo até 240 min), R/66 e R/87 (acessibilidade cumulativa) |
| 1. Insumos externos | acompanhados pelo pipeline (pasta do Arthur, CadÚnico e RAIS restritos) |
| 2. Montante | R/01, R/02, R/04, R/05 (insumos, estações, RAIS), R/42 (roteamento até 90 min), R/89, R/90, R/107, R/109–R/112, R/116 |
| 3. Preparação | R/117 |
| 4. Bases por evento | R/118, R/119 (+ `R/evento_par_monta.R`) |
| 5. Estimações | R/120–R/124, R/127 |
| 6. Tabelas, mapa de resultados e relatório | R/125, `R/pipeline_final.R`, `R/funcoes_relatorio.R`, `relatorio/relatorio_desenhos.qmd` |

Funções comuns: `R/funcoes_desenhos.R`, `R/funcoes_evento.R`, `R/funcoes_bh_evento.R`, `R/funcoes_iptw_evento.R`,
`R/funcoes.R`. A grade de especificações está em `grade_final()` (`R/pipeline_final.R`).

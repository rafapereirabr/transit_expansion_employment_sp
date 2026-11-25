# Catálogo das redes da família A do plano de bandas de ganho de acessibilidade
# (quality_reports/plans/2026-11-13_accessibility-gain-bands-design.qmd, aprovado para a família A em
# 13/11/2026), a partir da tabela de estações (dados públicos), com as correções de data verificadas
# em fontes em 13/11/2026 (tabela `correcoes` abaixo):
# - Estados históricos A00, A01, ...: A00 é a rede de 2025 (depois da abertura de Ambuitá,
#   04/04/2025; as mudanças de serviço da CPTM de agosto de 2025 não entram); cada estado seguinte
#   retira um pacote (linha × data de abertura), do mais recente ao mais antigo; o último é a base
#   anterior a 2013.
# - Obras futuras datadas, cada uma como linha inteira sobre A00 (L17, L6, L2), numeradas depois dos
#   estados históricos.
# A reconstrução da L8 em 2014 (Santa Rita, Amador Bueno) fica em todas as redes. Pacotes no mesmo
# dia saem pelo número de linha maior primeiro (decisão 11, provisória). Obras futuras sem dia usam
# 1º de julho do ano previsto (só ordenam; ficam fora da janela dos outcomes).
#
# Mudanças de rede que não são abertura de estação e não entram nos estados (não mudam o ganho de
# nenhum pacote; importariam só para níveis absolutos de acessibilidade por ano): L12 Engenheiro
# Goulart fechada de 23/06/2014 a 04/08/2017; L7 estendida de Luz a Brás em 28/10/2019; Serviço 710
# (L7 + L10 sem baldeação) de 04/05/2021 a 27/08/2025; suspensão da L15 entre 29/02 e 18/06/2020.
#
# Uso: Rscript R/34_catalogo_redes.R   (R 4.6)
# Saídas: data/redes/catalogo_familia_a.csv (uma linha por rede),
#         data/redes/estacoes_familia_a.csv (uma linha por estação que entra ou sai) e
#         data/redes/correcoes_catalogo.csv (correções aplicadas, com fontes)

source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(data.table))
dir_redes <- "data/redes"
dir.create(dir_redes, recursive = TRUE, showWarnings = FALSE)

## correções de data (primeiro dia com passageiros), verificadas em 13/11/2026 ------------------------
correcoes <- data.table::fread(text = "
code_line|name_station|data_catalogo|data_corrigida|fonte
15|SÃO MATEUS|2019-08-26|2019-12-16|https://diariodotransporte.com.br/2019/12/15/tres-estacoes-do-monotrilho-da-linha-15-prata-serao-inauguradas-nesta-segunda-16/
15|FAZENDA DA JUTA|2019-08-26|2019-12-16|https://diariodotransporte.com.br/2019/12/15/tres-estacoes-do-monotrilho-da-linha-15-prata-serao-inauguradas-nesta-segunda-16/
15|SAPOPEMBA|2019-08-26|2019-12-16|https://diariodotransporte.com.br/2019/12/15/tres-estacoes-do-monotrilho-da-linha-15-prata-serao-inauguradas-nesta-segunda-16/
5|CAMPO BELO|2019-08-26|2019-04-08|https://diariodotransporte.com.br/2019/04/08/horario-inicial-de-operacao-da-estacao-campo-belo-da-linha-5-lilas-sera-das-10h-as-15h/
4|VILA SÔNIA|2021-12-10|2021-12-17|https://www.correiobraziliense.com.br/brasil/2021/12/4971683-apos-17-anos-de-obras-sao-paulo-conclui-entrega-da-linha-amarela-do-metro.html
9|MENDES / BRUNO COVAS|2021-08-10|2021-08-11|https://abcdoabc.com.br/governo-de-sp-inaugura-estacao-mendes-vila-natal-da-linha-9-esmeralda-da-cptm/
5|ADOLFO PINHEIRO||2014-02-12|https://exame.com/brasil/metro-de-sao-paulo-adia-abertura-de-estacao-da-linha-5 ; G1 12/02/2014 (Após atrasos, Alckmin inaugura estação Adolfo Pinheiro)
6|VILA CARDOSO|2026-07-01|2027-07-01|https://www.metrocptm.com.br/mesmo-com-brasilandia-quase-pronta-linha-6-laranja-so-deve-ganhar-novas-estacoes-no-finalzinho-do-ano/ (renomeada Maristela)
", sep = "|", colClasses = "character", encoding = "UTF-8")

e <- as.data.table(arrow::read_parquet(file.path(dir_base, "estacoes.parquet")))
e[, dt := as.Date(dt_abertura)]
correcoes[, code_line := as.integer(code_line)]
achou <- correcoes[e, on = .(code_line, name_station), nomatch = 0L]
stopifnot(nrow(achou) == nrow(correcoes))
e[correcoes, on = .(code_line, name_station), dt := as.Date(i.data_corrigida)]
e[correcoes, on = .(code_line, name_station), yr_open := as.integer(substr(i.data_corrigida, 1, 4))]
fwrite(correcoes, file.path(dir_redes, "correcoes_catalogo.csv"))
e[is.na(dt) & !is.na(yr_open) & yr_open >= 2026L, dt := as.Date(sprintf("%d-07-01", yr_open))]

## estados históricos ------------------------------------------------------------------------------
hist <- e[!is.na(yr_open) & yr_open %between% c(2013L, 2025L) & !(rebuilt %in% 1L)]
stopifnot(!anyNA(hist$dt))
pac <- hist[, .(estacoes = paste(name_station, collapse = "; "), n_estacoes = .N), by = .(code_line, dt)]
setorder(pac, -dt, -code_line)
pac[, rede := sprintf("A%02d", seq_len(.N))]
n_hist <- nrow(pac)
hist <- merge(hist, pac[, .(code_line, dt, rede)], by = c("code_line", "dt"))

## obras futuras como linha inteira ------------------------------------------------------------------
fut <- e[!is.na(yr_open) & yr_open >= 2026L]
redes_fut <- setNames(sprintf("A%02d", n_hist + 1:3), c("17", "6", "2"))
stopifnot(setequal(as.character(unique(fut$code_line)), names(redes_fut)))
fut[, rede := redes_fut[as.character(code_line)]]
fpk <- fut[, .(estacoes = paste(name_station, collapse = "; "), n_estacoes = .N, primeira = min(dt), ultima = max(dt)),
	by = .(rede, code_line)]

## saídas ----------------------------------------------------------------------------------------------
cat_a <- rbind(
	data.table(rede = "A00", familia = "A", base = NA_character_, acao = "rede de 2025 completa",
		code_line = NA_integer_, data_abertura = as.Date(NA), n_estacoes = NA_integer_, estacoes = NA_character_),
	pac[, .(rede, familia = "A", base = sprintf("A%02d", as.integer(substr(rede, 2, 3)) - 1L), acao = "retira",
		code_line, data_abertura = dt, n_estacoes, estacoes)],
	fpk[, .(rede, familia = "A'", base = "A00", acao = "acrescenta linha inteira",
		code_line, data_abertura = primeira, n_estacoes, estacoes)],
	fill = TRUE
)
setorder(cat_a, rede)
fwrite(cat_a, file.path(dir_redes, "catalogo_familia_a.csv"))
est <- rbind(
	hist[, .(rede, acao = "retira", code_line, code_station, name_station, infill, data_abertura = dt, lon, lat)],
	fut[, .(rede, acao = "acrescenta", code_line, code_station, name_station, infill, data_abertura = dt, lon, lat)]
)
fwrite(est, file.path(dir_redes, "estacoes_familia_a.csv"))
options(width = 200)
print(cat_a[, .(rede, familia, base, acao, linha = code_line, data_abertura, n_estacoes, estacoes = substr(estacoes, 1, 60))])
message("gravado: ", n_hist + 1L, " estados históricos e 3 obras futuras em ", file.path(dir_redes, "catalogo_familia_a.csv"))

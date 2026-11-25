# Configuração da base de análise: caminhos e parâmetros do desenho.
#
# Todos os scripts rodam a partir da raiz deste repositório. A pasta do Arthur é usada SOMENTE para leitura.
# Os caminhos absolutos desta máquina ficam em R/00_config_local.R (fora do git; modelo em R/00_config_local_exemplo.R).

suppressPackageStartupMessages({
	library(arrow)
	library(dplyr)
})
options(arrow.pull_as_vector = TRUE, arrow.use_altrep = FALSE)

# caminhos ------------------------------------------------------------------------------------

## origem (somente leitura): pasta de trabalho do Arthur, cujos arquivos de CadÚnico foram
## gerados pelo pipeline local dele (população = famílias que moraram na capital em algum ano)
if (!exists("raiz_arthur")) {
	if (!file.exists("R/00_config_local.R")) stop("falta R/00_config_local.R: copie R/00_config_local_exemplo.R e preencha os caminhos desta máquina")
	source("R/00_config_local.R")
}
dir_arthur <- file.path(raiz_arthur, "data")

dir_insumos <- "data/insumos" # cópias dos insumos (restrito, fora do git)
dir_base <- "data/base" # base construída (restrito, fora do git)
dir_rais <- "data/base/rais_vinc" # vínculos RAIS dos CPFs elegíveis (restrito)
dir_saida <- "outputs" # agregados e auditorias

for (d in c(dir_insumos, dir_base, dir_rais, dir_saida)) {
	dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# parâmetros do desenho -----------------------------------------------------------------------
## Fonte: MEMORY.md e plano de 19/10 do projeto (registrados no README deste repositório).

anos_outcome <- 2011:2025 # anos com outcome RAIS (31/12); até 2025 desde 14/11/2026 (decisão 6 do plano de bandas)
g_lag <- 4L # residência e elegibilidade medidas em g - 4
janela_tratamento <- 2015:2023 # anos de abertura estudados
raio_tratamento_m <- 2000 # tratamento: estação nova a até 2 km da residência em g - 4
raio_spillover_m <- 4000 # sensibilidade: exclui controles entre 2 e 4 km de estação nova de g
raio_candidato_m <- 4500 # pré-seleção planar antes da distância geodésica exata
desvio_max_m <- 500 # geocodificação do CadÚnico: desvio máximo aceito (regra do Arthur)
idade_min <- 22
idade_max <- 60
linhas_futuras <- c(6, 17) # corredores de controle da comparação l6_l17
muni_estudo <- 3550308L # município de São Paulo (insumos atuais cobrem só a capital)
crs_utm <- 31983 # SIRGAS 2000 / UTM 23S (coordenadas das estações)

## argumentos de linha de comando: aceita "nome=valor" ou posição (compatível com chamadas antigas)
args_cli <- commandArgs(trailingOnly = TRUE)
arg_cli <- function(nome, pos = NA, padrao = NULL) {
	hit <- grep(paste0("^", nome, "="), args_cli, value = TRUE)
	if (length(hit)) {
		return(sub(paste0("^", nome, "="), "", hit[1]))
	}
	posicionais <- args_cli[!grepl("=", args_cli, fixed = TRUE)]
	if (!is.na(pos) && length(posicionais) >= pos) {
		return(posicionais[pos])
	}
	padrao
}
## sufixo das saídas de variantes do desenho (defasagem do ano-base e raio de tratamento)
sufixo_variante <- function(lag, raio) {
	paste0(
		if (lag != g_lag) paste0("_lag", lag) else "",
		if (raio != raio_tratamento_m) paste0("_r", raio) else ""
	)
}

## salário mínimo nacional vigente em dezembro de cada ano (R$ nominais)
salario_minimo_dez <- c(
	`2011` = 545, `2012` = 622, `2013` = 678, `2014` = 724, `2015` = 788, `2016` = 880,
	`2017` = 937, `2018` = 954, `2019` = 998, `2020` = 1045, `2021` = 1100, `2022` = 1212,
	`2023` = 1320, `2024` = 1412, `2025` = 1518
)

# EXPLORATÓRIO (17/11/2026, pedido do pesquisador): lista dos pares casa → trabalho dos desenhos mensais do T3 (R/100,
# R/103, R/105) cujo trajeto passa de 90 min, para rotear até 240 min (R/110).
# - Vínculos de referência como no R/100: celetista ativo no fim do mês de referência com maior remuneração média, local
#   de trabalho do Arthur daquele ano. Meses de referência por coorte: pós com o marco no serviço pleno e na abertura, e
#   o pré (25 meses antes da primeira abertura). Endereço-base g−1 a g−4 (todas as populações dos desenhos).
# - Inclusão: o par não tem tempo na matriz de 90 min da rede A23 (fim de 2012, a menor rede da família A; como a rede
#   só cresce, quem passa de 90 min em alguma rede passa na A23).
# Saída: data/base/t3_pares_acima90.parquet (id_casa, id_trab, vinculos) e o resumo no log.
# Uso: Rscript R/109_pares_longos_t3.R

source("R/00_ambiente_r5.R")
source("R/00_config.R")
source("R/funcoes_desenhos.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
dir_art <- file.path(dir_arthur, "temp")
eleg <- c(10L, 15L, 20L, 25L, 50L, 55L, 60L, 65L, 70L, 75L, 90L)
niv <- le_niveis()
ids <- le_ganhos(60L)$id
mes_idx <- function(d) (year(d) - 2011L) * 12L + month(d)
ano_de <- function(m) 2011L + (m - 1L) %/% 12L
mes_de <- function(m) (m - 1L) %% 12L + 1L
REF <- rbindlist(lapply(coortes_plano, function(g) {
	Lg <- niv[coorte == g]
	o_min <- min(mes_idx(Lg$abertura))
	data.table(coorte = g, ref = unique(c(max(mes_idx(Lg$pleno)) - 1L, o_min - 1L, o_min - 25L)))
}))[ref >= 1L]
RV <- list()
rais_ano <- function(a) {
	kk <- as.character(a)
	if (is.null(RV[[kk]])) {
		v <- as.data.table(arrow::read_parquet(file.path(dir_rais, sprintf("rais_vinc_%d.parquet", a)),
			col_select = c("cpf_num", "id_estab", "tipo_vinculo", "emp_31dez", "data_adm", "data_deslig", "mes_deslig", "rem_med_r")))
		v <- v[as.integer(tipo_vinculo) %in% eleg]
		v[, `:=`(adm = as.integer(as.IDate(data_adm)), m_sep = fifelse(emp_31dez %in% 1L, 13L, fifelse(!is.na(data_deslig) & year(data_deslig) == a, month(data_deslig),
			fifelse(as.integer(mes_deslig) %in% 1:12, as.integer(mes_deslig), NA_integer_))))]
		RV[[kk]] <- v[, .(cpf_num, id_estab, adm, m_sep, rem_med_r)]
	}
	RV[[kk]]
}
WP <- list()
local_trabalho <- function(r) {
	kk <- as.character(r)
	if (is.null(WP[[kk]])) {
		w <- as.data.table(arrow::read_parquet(file.path(dir_art, sprintf("rais_workplaces_%d.parquet", r)), col_select = c("id_estab", "cei_vinc", "work_h3_id", "work_desvio", "work_geo_valid")))
		w <- w[work_geo_valid %in% TRUE & !is.na(work_desvio) & work_desvio <= 500 & !is.na(work_h3_id)]
		WP[[kk]] <- w[order(!(is.na(cei_vinc) | cei_vinc %in% c("", "0")))][!duplicated(id_estab)][, .(estab = id_estab, trab = work_h3_id)]
	}
	WP[[kk]]
}
PR <- list()
for (k in 1:4) for (g in coortes_plano) {
	x <- pop_coorte(k, g, 18L, ids)
	if (is.null(x)) next
	for (ref in REF[coorte == g, ref]) {
		a <- ano_de(ref)
		mm <- mes_de(ref)
		fim_mes <- as.integer(as.IDate(sprintf("%d-%02d-01", a + (mm == 12L), mm %% 12L + 1L))) - 1L
		v <- rais_ano(a)[cpf_num %in% x$cpf_num & adm <= fim_mes & !is.na(m_sep) & m_sep > mm]
		v <- v[order(-fcoalesce(as.numeric(rem_med_r), 0))][!duplicated(cpf_num)]
		w <- local_trabalho(a)
		v[, trab := w$trab[match(id_estab, w$estab)]]
		v <- v[!is.na(trab)]
		PR[[length(PR) + 1L]] <- data.table(id_casa = x$h3_09[match(v$cpf_num, x$cpf_num)], id_trab = v$trab)
	}
	msg("lag ", k, " coorte ", g, ": feito")
}
pares <- rbindlist(PR)[, .(vinculos = .N), by = .(id_casa, id_trab)]
m23 <- as.data.table(arrow::read_parquet(file.path("data/redes/familia_a", "ttm_A23.parquet"), col_select = c("from_id", "to_id", "travel_time_p50")))
pares[m23, t23 := i.travel_time_p50, on = .(id_casa = from_id, id_trab = to_id)]
longos <- pares[is.na(t23), .(id_casa, id_trab, vinculos)]
arrow::write_parquet(longos, file.path(dir_base, "t3_pares_acima90.parquet"))
msg("pares únicos: ", nrow(pares), " (", sum(pares$vinculos), " vínculos); acima de 90 min na A23: ", nrow(longos), " pares (", sum(longos$vinculos), " vínculos, ",
	round(100 * sum(longos$vinculos) / sum(pares$vinculos), 1), "%); origens ", uniqueN(longos$id_casa), "; destinos ", uniqueN(longos$id_trab))
msg("fim")

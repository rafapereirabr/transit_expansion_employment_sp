# Montagem da base por evento do desenho de tempo de viagem (especificação final, 24/11/2026).
# Versão limpa do que antes era uma cópia remendada do R/100_t3_retencao_mensal.R: reúne, num só lugar e sem remendos,
# (1) os eventos (cada data de serviço pleno é um evento), (2) o marcador de criança de 0 a 5 anos na família, (3) as
# máscaras de emprego no fim do mês e a permanência, (4) o destino de quem pediu demissão e (5) as covariáveis do IPTW.
# O R/100 continua como está (material de coortes anuais); este arquivo não o lê nem o altera.
#
# Não é rodado sozinho: é carregado por
#   R/119_base_evento_par.R   (modo_par <- "cache": base em células para o B&H, com o destino de quem pediu demissão);
#   R/124_iptw_evento_par.R   (modo_par <- "iptw": vínculos com as covariáveis do escore).
# Deixa prontos: EVT (eventos), POP (vínculos de referência por janela e evento), pares, TT (tempos nas 27 redes),
# DL (ganho de tempo por nível), chave, redes, trab_h7, niv, hx, ids, H e msg.
#
# Desenho: vínculo de referência = celetista ativo no fim do mês de referência com maior remuneração média, com o local de
# trabalho do Arthur daquele ano; seguido nos rais_vinc (mesmo CPF, estabelecimento e data de admissão) até o desligamento,
# com a causa (pedido 21; demissão pelo empregador 10 e 11; transferências 30 a 34 não contam como saída). Janelas:
# pos = referência no mês anterior ao serviço pleno do evento; pre (placebo) = referência 25 meses antes da primeira
# abertura dos níveis do evento, seguida por 24 meses. População: CadÚnico de 18 a 60 anos da coorte anual do evento, com
# o endereço mais recente entre g−4 e g−1 (lag=0) ou o de g−lag (lag=1..4, robustez R5). Tempos: matrizes de até 90 min
# (R/42) e, nos pares da lista do R/109, os tempos até 240 min (R/110); teto do ganho censurado de 91 ou 241 min.
# Argumentos de linha de comando: [lag=0] [idade=18] [horizontes=6,12,18,24]

stopifnot(exists("modo_par"), modo_par %in% c("cache", "iptw"))
source("R/00_ambiente_r5.R")
source("R/00_config.R")
.libPaths(c(.libPaths(), "lib_r46_extra"))
source("R/funcoes_desenhos.R")
suppressPackageStartupMessages({
	library(data.table)
	library(fixest)
})
setDTthreads(4L)
setFixest_nthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
options(width = 220)
source("R/funcoes_evento.R")
k <- as.integer(arg_cli("lag", NA, 0L))
idade <- as.integer(arg_cli("idade", NA, 18L))
H <- as.integer(strsplit(arg_cli("horizontes", NA, "6,12,18,24"), ",")[[1]])
dir_art <- file.path(dir_arthur, "temp")
niv <- le_niveis()
hx <- le_hex()
ids <- le_ganhos(60L)$id
hx <- hx[match(ids, hx$id)]
redes <- sort(unique(c(niv$com, niv$sem)))
janelas <- c("pos", "pre")

## eventos: uma janela por data de serviço pleno; população da coorte anual do nível --------------------------------------
EVT <- niv[coorte %in% coortes_plano, .(ano = coorte[1], m_e = mes_idx(pleno[1]), d_e = as.integer(pleno[1]), o_min = min(mes_idx(abertura)),
	niveis = paste(nivel, collapse = ",")), by = .(pleno)][order(pleno)]
EVT[, `:=`(coorte = seq_len(.N), ref_pos = m_e - 1L, ref_pre = o_min - 25L, ab1 = NA_integer_)]
EVT <- EVT[ref_pre >= 1L]
JAN <- EVT[, .(coorte, ano, ref_pos, ref_pre, ab1)]
print(EVT)
print(JAN)

## vínculos -------------------------------------------------------------------------------------------------------------
## (como no R/100, a atribuição dentro das funções é local: cada ano é relido a cada chamada, o que poupa memória)
RV <- list()
rais_ano <- function(a) {
	kk <- as.character(a)
	if (is.null(RV[[kk]])) {
		v <- as.data.table(arrow::read_parquet(file.path(dir_rais, sprintf("rais_vinc_%d.parquet", a)),
			col_select = c("cpf_num", "id_estab", "tipo_vinculo", "emp_31dez", "data_adm", "data_deslig", "mes_deslig", "causa_deslig", "rem_med_r")))
		v <- v[as.integer(tipo_vinculo) %in% eleg]
		v[, `:=`(adm = as.integer(as.IDate(data_adm)), tipo_vinculo = NULL,
			m_sep = fifelse(emp_31dez %in% 1L, 13L, fifelse(!is.na(data_deslig) & year(data_deslig) == a, month(data_deslig),
				fifelse(as.integer(mes_deslig) %in% 1:12, as.integer(mes_deslig), NA_integer_))))]
		RV[[kk]] <- v
	}
	RV[[kk]]
}
WP <- list()
local_trabalho <- function(r) {
	kk <- as.character(r)
	if (is.null(WP[[kk]])) {
		w <- as.data.table(arrow::read_parquet(file.path(dir_art, sprintf("rais_workplaces_%d.parquet", r)), col_select = c("id_estab", "cei_vinc", "work_h3_id", "work_desvio", "work_geo_valid")))
		w <- w[work_geo_valid %in% TRUE & !is.na(work_desvio) & work_desvio <= 500 & !is.na(work_h3_id)]
		w <- w[order(!(is.na(cei_vinc) | cei_vinc %in% c("", "0")))][!duplicated(id_estab)]
		WP[[kk]] <- w[, .(estab = id_estab, trab = work_h3_id)]
	}
	WP[[kk]]
}

## destino de quem pediu demissão até max(H) meses depois do mês de referência (só no modo "cache"): primeiro vínculo
## celetista em outro estabelecimento admitido entre o mês anterior ao pedido e 12 meses depois dele; local (H3-9), meses
## até a admissão, razão entre os salários reais (remuneração média do ano de admissão contra a do ano de referência, IPCA
## dez/2023) e distâncias em linha reta casa–trabalho antiga e nova. dest: novo / semloc / semnovo.
hq_dest <- max(H)
destino_pedido <- function(v, x, ref) {
	v[, `:=`(dest = NA_character_, trab_n = NA_character_, m_novo = NA_real_, w_rat = NA_real_, d_old = NA_real_, d_new = NA_real_)]
	q <- v[tipo == "pedido" & fim <= ref + hq_dest, .(cpf_num, id_estab, trab, fim, w_old = as.numeric(rem_med_r) * fator_ano(ano_de(ref)))]
	if (!nrow(q)) return(v)
	q[, `:=`(ini = as.integer(as.IDate(sprintf("%d-%02d-01", ano_de(fim - 1L), mes_de(fim - 1L)))),
		fim_d = as.integer(as.IDate(sprintf("%d-%02d-01", ano_de(fim + 13L), mes_de(fim + 13L)))) - 1L)]
	anos <- sort(unique(unlist(lapply(c(-1L, 0L, 6L, 12L, 13L), function(d) ano_de(q$fim + d)))))
	anos <- anos[anos <= 2025L]
	cand <- rbindlist(lapply(anos, function(a) rais_ano(a)[cpf_num %in% q$cpf_num, .(cpf_num, id_estab_n = id_estab, adm_n = adm,
		w_new = as.numeric(rem_med_r) * fator_ano(a))]))
	cand <- unique(cand, by = c("cpf_num", "id_estab_n", "adm_n"))
	cand <- merge(cand, q[, .(cpf_num, id_estab, ini, fim_d)], by = "cpf_num")
	cand <- cand[id_estab_n != id_estab & adm_n >= ini & adm_n <= fim_d][order(cpf_num, adm_n)][!duplicated(cpf_num)]
	cand[, trab_n := NA_character_]
	if (nrow(cand)) for (a in unique(year(as.IDate(cand$adm_n)))) {
		w <- tryCatch(local_trabalho(a), error = function(e) NULL)
		if (is.null(w)) next
		ix <- which(year(as.IDate(cand$adm_n)) == a)
		set(cand, ix, "trab_n", w$trab[match(cand$id_estab_n[ix], w$estab)])
	}
	q <- merge(q, cand[, .(cpf_num, trab_n, adm_n, w_new, achou = TRUE)], by = "cpf_num", all.x = TRUE)
	casa <- x$h3_09[match(q$cpf_num, x$cpf_num)]
	q[, `:=`(d_old = dist_h3(casa, trab), d_new = dist_h3(casa, trab_n),
		m_novo = as.numeric(mes_idx(as.IDate(adm_n)) - fim), w_rat = fifelse(w_old > 0 & w_new > 0, w_new / w_old, NA_real_))]
	q[, dest := fifelse(is.na(achou), "semnovo", fifelse(is.na(trab_n), "semloc", "novo"))]
	v[q, `:=`(dest = i.dest, trab_n = i.trab_n, m_novo = i.m_novo, w_rat = i.w_rat, d_old = i.d_old, d_new = i.d_new), on = "cpf_num"]
	msg("pedidos até ", hq_dest, " meses: ", nrow(q), "; novo vínculo em 12 meses: ", round(100 * mean(!is.na(q$achou)), 1), "%; sem local: ",
		sum(q$dest == "semloc"), "; salário real do novo > 1,05 × o antigo: ", round(100 * mean(q$w_rat > 1.05, na.rm = TRUE), 1), "%")
	v
}

## vínculos de referência ativos no fim do mês ref (índice), seguidos até o desligamento; J = linha do evento em JAN
referencia <- function(x, ref, J, h_max = max(H)) {
	a <- ano_de(ref)
	mm <- mes_de(ref)
	fim_mes <- as.integer(as.IDate(sprintf("%d-%02d-01", a + (mm == 12L), mm %% 12L + 1L))) - 1L
	v <- rais_ano(a)[cpf_num %in% x$cpf_num & adm <= fim_mes & !is.na(m_sep) & m_sep > mm]
	v <- v[order(-fcoalesce(as.numeric(rem_med_r), 0))][!duplicated(cpf_num)]
	w <- local_trabalho(a)
	v[, trab := w$trab[match(id_estab, w$estab)]]
	v <- v[!is.na(trab)]
	## saída no próprio ano de referência
	v[, `:=`(fim = fifelse(m_sep <= 12L, (a - 2011L) * 12L + m_sep, NA_integer_), causa = fifelse(m_sep <= 12L, as.integer(causa_deslig), NA_integer_), censura = FALSE)]
	vivos <- v[is.na(fim), cpf_num]
	## anos seguintes até o horizonte (nenhum se o horizonte termina no próprio ano de referência)
	a_fim <- min(2025L, ano_de(ref + h_max))
	for (aa in a + seq_len(max(0L, a_fim - a))) {
		if (!length(vivos)) break
		u <- rais_ano(aa)[cpf_num %in% vivos]
		u <- u[!duplicated(u, by = c("cpf_num", "id_estab", "adm"))]
		ix <- v[, .I[cpf_num %in% vivos]]
		m <- u[v[ix, .(cpf_num, id_estab, adm)], on = .(cpf_num, id_estab, adm)]
		achou <- !is.na(m$emp_31dez)
		saiu <- achou & m$m_sep %in% 1:12
		sem_mes <- achou & !(m$emp_31dez %in% 1L) & is.na(m$m_sep)
		set(v, ix[saiu], "fim", (aa - 2011L) * 12L + m$m_sep[saiu])
		set(v, ix[saiu], "causa", as.integer(m$causa_deslig[saiu]))
		set(v, ix[!achou | sem_mes], "fim", (aa - 2011L) * 12L)
		set(v, ix[!achou | sem_mes], "censura", TRUE)
		vivos <- v[is.na(fim), cpf_num]
	}
	v[is.na(fim), `:=`(fim = 9999L, censura = FALSE)]
	v[, tipo := fcase(fim == 9999L, "ativo", censura, "censura", causa %in% 21L, "pedido", causa %in% c(10L, 11L), "demissao", causa %in% 30:34, "transferencia",
		default = "outra")]
	if (!"crianca" %in% names(x)) x[, crianca := NA]
	if (modo_par == "cache") {
		v <- destino_pedido(v, x, ref)
		v[, tenure := (fim_mes - adm) / 30.4375]
		v <- merge(v[, .(cpf_num, trab, fim, tipo, causa, tenure, dest, trab_n, m_novo, w_rat, d_old, d_new)], x[, .(cpf_num, h3_09, ih, sexo, idade, crianca)], by = "cpf_num")
	} else {
		v[, `:=`(tenure = (fim_mes - adm) / 30.4375, rem_ref = as.numeric(rem_med_r))]
		v <- merge(v[, .(cpf_num, trab, fim, tipo, causa, tenure, rem_ref)], x[, .(cpf_num, h3_09, ih, sexo, idade, crianca, raca, deficiencia, alfabetizado, anos_atualizacao)], by = "cpf_num")
	}
	## desfechos por horizonte (meses depois do mês de referência)
	for (h in H) {
		lim <- ref + h
		obs <- !(v$tipo == "censura" & v$fim < lim) & lim <= 180L & h <= h_max
		saiu_h <- v$fim <= lim & v$tipo %in% c("pedido", "demissao", "outra")
		set(v, j = paste0("qualquer_", h), value = fifelse(obs, as.numeric(saiu_h), NA_real_))
		set(v, j = paste0("pedido_", h), value = fifelse(obs, as.numeric(v$fim <= lim & v$tipo == "pedido"), NA_real_))
		set(v, j = paste0("demissao_", h), value = fifelse(obs, as.numeric(v$fim <= lim & v$tipo == "demissao"), NA_real_))
		## demissão com justa causa (causa 10) e sem justa causa (causa 11)
		set(v, j = paste0("justa_", h), value = fifelse(obs, as.numeric(v$fim <= lim & v$tipo == "demissao" & v$causa %in% 10L), NA_real_))
		set(v, j = paste0("sem_justa_", h), value = fifelse(obs, as.numeric(v$fim <= lim & v$tipo == "demissao" & v$causa %in% 11L), NA_real_))
	}
	## permanência no emprego formal (qualquer vínculo; máscaras do R/89)
	if (paste0("emp_mask_", ano_de(ref)) %in% names(x)) {
		xx <- x[match(v$cpf_num, x$cpf_num)]
		for (h in H) {
			if (ref + h > 180L) next
			set(v, j = paste0("empregado_", h), value = as.numeric(emp_mes(xx, ref + h)))
			set(v, j = paste0("meses_", h), value = Reduce(`+`, lapply((ref + 1L):(ref + h), function(m) as.numeric(emp_mes(xx, m)))) / h)
		}
		if (modo_par == "iptw") {
			## covariáveis da linha de base b (mês anterior à primeira abertura na janela posterior; a referência do placebo
			## no placebo): emprego em b−12 e fração dos 12 meses até b empregado
			b <- if (identical(as.integer(ref), as.integer(J$ref_pre))) ref else J$ref_pre + 24L
			set(v, j = "e_b12", value = fcoalesce(as.numeric(emp_mes(xx, b - 12L)), 0))
			set(v, j = "m_b12", value = Reduce(`+`, lapply((b - 11L):b, function(m) fcoalesce(as.numeric(emp_mes(xx, m)), 0))) / 12)
		}
	}
	v
}
POP <- list()
for (g in JAN$coorte) {
	J <- JAN[coorte == g]
	ano_g <- J$ano
	x <- pop_coorte(k, ano_g, idade, ids)
	if (is.null(x)) next
	x <- marca_crianca(x, ano_g, k)
	x <- junta_pa(x, 2011:2025, "emp_mask")
	for (jn in janelas) {
		v <- if (jn == "pos") referencia(x, J$ref_pos, J) else referencia(x, J$ref_pre, J, 24L)
		v[, `:=`(coorte = g, janela = jn)]
		POP[[paste(jn, g)]] <- v
		msg("lag ", k, " coorte ", g, " ", jn, ": ", nrow(v), " vínculos; ", paste(names(table(v$tipo)), table(v$tipo), collapse = ", "))
	}
}
rm(RV)
gc()

## pares, tempos e ganhos -----------------------------------------------------------------------------------------------
pares <- unique(rbindlist(lapply(POP, function(v) v[, .(casa = h3_09, trab)])))
TT <- matrix(NA_real_, nrow(pares), length(redes), dimnames = list(NULL, redes))
pl <- as.data.table(arrow::read_parquet(file.path(dir_base, "t3_pares_acima90.parquet"), col_select = c("id_casa", "id_trab")))
eh_longo <- !is.na(pl[pares, on = .(id_casa = casa, id_trab = trab), which = TRUE, mult = "first"])
rm(pl)
msg("pares na lista de trajetos acima de 90 min (R/109): ", sum(eh_longo), " de ", nrow(pares))
for (rd in redes) {
	m <- as.data.table(arrow::read_parquet(file.path("data/redes/familia_a", sprintf("ttm_%s.parquet", rd)), col_select = c("from_id", "to_id", "travel_time_p50")))
	TT[, rd] <- m[pares, on = .(from_id = casa, to_id = trab)]$travel_time_p50
	rm(m)
	f_l <- file.path(dir_base, "ttm_longos_t3", sprintf("ttm_longos_%s.parquet", rd))
	stopifnot("falta rede do R/110" = file.exists(f_l))
	lo <- as.data.table(arrow::read_parquet(f_l))
	TT[eh_longo, rd] <- lo[pares[eh_longo], on = .(from_id = casa, to_id = trab), mult = "first"]$travel_time_p50
	rm(lo)
}
teto <- fifelse(eh_longo, 241, 91)
## ganho de tempo de cada nível: rede sem o nível − rede com ele, com o teto do ganho censurado
DL <- sapply(seq_len(nrow(niv)), function(i) {
	s <- TT[, niv$sem[i]]
	c1 <- TT[, niv$com[i]]
	fifelse(is.na(s) & is.na(c1), 0, fifelse(is.na(s), teto - c1, fifelse(is.na(c1), 0, s - c1)))
})
colnames(DL) <- niv$nivel
chave <- paste(pares$casa, pares$trab)
ut <- unique(pares$trab)
trab_h7 <- h3jsr::get_parent(ut, res = 7)[match(pares$trab, ut)]
msg("pares: ", nrow(pares))

# Especificação final, bloco D1 (24/11/2026): IPTW de faixas de ganho de acessibilidade no desenho por evento (data exata
# de serviço pleno), com os desfechos do bloco A (emprego, entrada, permanência e fração dos meses, salário; h = 6, 12, 18,
# 24 meses depois do mês anterior ao serviço pleno do evento; placebo com a referência 25 meses antes da primeira abertura).
# - Tratados: ganho do evento (níveis com a data dele) em % da acessibilidade de logo antes do evento (base mínima de 1.000
#   empregos) nas faixas 1–5%, 5–20% e 20% ou mais; controles: ganho abaixo de 1%. Indicadores: cumulativa a 60 min e
#   gravitacional calibrada (cortes= para outros).
# - Janela limpa: fica fora quem tem ganho de 1% ou mais de outro evento cuja abertura ou serviço pleno cai entre a
#   referência do placebo (25 meses antes da primeira abertura do evento) e 24 meses depois da referência.
# - Escore (logit, por evento e faixa, contra os controles): idade (spline), sexo, criança de 0 a 5 anos na família, raça,
#   deficiência, alfabetização, anos sem atualização do cadastro, emprego no fim do mês da linha de base, 12 e 24 meses
#   antes, fração dos 12 meses anteriores empregado; da área: log da acessibilidade antes, distância à Sé, Censo 2010
#   (renda per capita, aglomerado subnormal, densidade, esgoto, pavimentação). Sem a distância à estação. Linha de base:
#   mês anterior à primeira abertura (janela posterior) ou o mês de referência do placebo.
# - Pesos ATT p/(1−p) nos controles, normalizados por evento (e por segmento); estimador duplamente robusto (regressão
#   ponderada do desfecho no tratamento, nas covariáveis e nos eventos); EP por célula de 5 km. Segmentos: o escore é o da
#   amostra toda (que já tem sexo, idade e criança); o ATT do segmento usa a subamostra com os pesos renormalizados.
#   Contrastes com EP de cluster conjunto. nivel=h38: o mesmo com hexágonos H3-8 × evento (médias, pesos = pessoas;
#   faixa pela média do ganho ponderada por pessoas).
# Origem: explora_iptw_area.R. Lê as pessoas diretamente (não usa a base por evento em células).
# Uso: Rscript R/123_iptw_evento_area.R [nivel=ind|h38] [cortes=60,grav] [janelas=pos,pre] [sufixo=]
# Saída: outputs/final/iptw_area_<nivel><sufixo>_segmentos.csv, _contrastes.csv e _balanco.csv
source("R/00_ambiente_r5.R")
source("R/00_config.R")
source("R/funcoes_desenhos.R")
suppressPackageStartupMessages({
	library(data.table)
	library(splines)
})
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
nivel <- arg_cli("nivel", NA, "ind")
stopifnot(nivel %in% c("ind", "h38"))
cortes <- strsplit(arg_cli("cortes", NA, "60,grav"), ",")[[1]]
janelas <- strsplit(arg_cli("janelas", NA, "pos,pre"), ",")[[1]]
suf_x <- arg_cli("sufixo", NA, "")
dir.create("outputs/final", showWarnings = FALSE)
k <- 0L
idade_min <- 18L
H <- c(6L, 12L, 18L, 24L)
source("R/funcoes_evento.R")
niv <- le_niveis()
hx <- le_hex()
ids <- le_ganhos(60L)$id
hx <- hx[match(ids, hx$id)]
for (v in c("renda_pc", "subnormal", "densidade", "esgoto", "pavim")) set(hx, which(is.na(hx[[v]])), v, median(hx[[v]], na.rm = TRUE))
EVT <- niv[coorte %in% coortes_plano, .(ano = coorte[1], m_e = mes_idx(pleno[1]), d_e = as.integer(pleno[1]), o_min = min(mes_idx(abertura)),
	niveis = paste(nivel, collapse = ",")), by = .(pleno)][order(pleno)]
EVT[, `:=`(ev = seq_len(.N), ref_pos = m_e - 1L, ref_pre = o_min - 25L)]
EVT <- EVT[ref_pre >= 1L]
pleno_niv <- as.integer(niv$pleno)
## ganho de cada evento em % da acessibilidade de logo antes dele, por hexágono
GAN <- list()
for (cc in cortes) {
	if (cc == "grav") {
		g <- le_ganhos("gcal")
		Gm <- as.matrix(g[match(ids, id), niv$nivel, with = FALSE])
		A0 <- g$acc_A00[match(ids, g$id)]
	} else {
		g <- le_ganhos(as.integer(cc))
		Gm <- as.matrix(g[match(ids, id), niv$nivel, with = FALSE])
		A0 <- g$acc_A00[match(ids, g$id)]
	}
	GAN[[cc]] <- lapply(EVT$ev, function(e) {
		E <- EVT[ev == e]
		base <- A0 - rowSums(Gm[, pleno_niv >= E$d_e & niveis_cadeia(niv), drop = FALSE])
		list(pct = 100 * rowSums(Gm[, pleno_niv == E$d_e, drop = FALSE]) / pmax(base, 1000), lacc = log1p(pmax(base, 0)))
	})
	rm(g, Gm)
}
p8 <- h3jsr::get_parent(ids, res = 8)

## base de pessoas por evento e janela, com desfechos, covariáveis e ganhos -------------------------------------------------
oc_sets <- c(list(emprego = paste0("emprego_", H), entrada = paste0("entrada_", H), permanencia = c(paste0("empregado_", H), paste0("meses_", H))),
	setNames(lapply(H, function(h) paste0("salario_", h)), paste0("salario_", H)))
BASE <- list()
XC <- list()
for (e in EVT$ev) {
	E <- EVT[ev == e]
	kk <- as.character(E$ano)
	if (is.null(XC[[kk]])) {
		XC <- list()
		gc()
		x <- pop_coorte(k, E$ano, idade_min, ids)
		x <- marca_crianca(x, E$ano, k)
		x <- junta_pa(x, 2011:2025, "emp_mask")
		XC[[kk]] <- x
		rm(x)
	}
	x <- XC[[kk]]
	## janela limpa: outros eventos com abertura ou serviço pleno entre ref_pre e ref_pos + 24
	suja <- lapply(cortes, function(cc) {
		s <- rep(FALSE, length(ids))
		for (e2 in setdiff(EVT$ev, e)) {
			E2 <- EVT[ev == e2]
			if (E2$o_min <= E$ref_pos + 24L && E2$m_e >= E$ref_pre) s <- s | GAN[[cc]][[e2]]$pct >= 1
		}
		s
	})
	names(suja) <- cortes
	for (jn in janelas) {
		ref <- E[[paste0("ref_", jn)]]
		b <- if (jn == "pos") E$o_min - 1L else E$ref_pre
		e0 <- emp_mes(x, ref)
		D <- data.table(ev = e, ih = x$ih, sx = as.integer(as.character(x$sexo)), idade = x$idade, cr = as.integer(x$crianca),
			raca = x$raca, deficiencia = x$deficiencia, alfabetizado = x$alfabetizado, anos_atualizacao = x$anos_atualizacao,
			e_b = as.numeric(emp_mes(x, b)), e_b12 = as.numeric(emp_mes(x, b - 12L)), e_b24 = as.numeric(emp_mes(x, b - 24L)),
			m_b12 = Reduce(`+`, lapply((b - 11L):b, function(m) as.numeric(emp_mes(x, m)))) / 12)
		for (v in c("e_b12", "e_b24")) set(D, which(is.na(D[[v]])), v, 0)
		s0 <- lsalh_mes(x$cpf_num, ref)
		for (h in H) {
			if (ref + h > 180L) next
			eh <- emp_mes(x, ref + h)
			set(D, j = paste0("emprego_", h), value = as.numeric(eh) - as.numeric(e0))
			set(D, j = paste0("entrada_", h), value = fifelse(e0, NA_real_, as.numeric(Reduce(`|`, lapply((ref + 1L):(ref + h), function(m) emp_mes(x, m))))))
			set(D, j = paste0("empregado_", h), value = fifelse(e0, as.numeric(eh), NA_real_))
			set(D, j = paste0("meses_", h), value = fifelse(e0, Reduce(`+`, lapply((ref + 1L):(ref + h), function(m) as.numeric(emp_mes(x, m)))) / h, NA_real_))
			sh <- lsalh_mes(x$cpf_num, ref + h)
			set(D, j = paste0("salario_", h), value = fifelse(e0 & eh, 100 * (sh - s0), NA_real_))
		}
		for (cc in cortes) {
			set(D, j = paste0("pct_", cc), value = fifelse(suja[[cc]][x$ih], NA_real_, GAN[[cc]][[e]]$pct[x$ih]))
			set(D, j = paste0("lacc_", cc), value = GAN[[cc]][[e]]$lacc[x$ih])
		}
		BASE[[paste(jn, e)]] <- D[, janela := jn]
		msg("evento ", e, " ", jn, ": ", nrow(D), " pessoas; tratados (≥1%) e controles na janela limpa, corte ", cortes[1], ": ",
			sum(D[[paste0("pct_", cortes[1])]] >= 1, na.rm = TRUE), " e ", sum(D[[paste0("pct_", cortes[1])]] < 1, na.rm = TRUE))
	}
}
rm(XC, RS)
gc()
BASE <- rbindlist(BASE, fill = TRUE)
BASE[, `:=`(dist_se = hx$dist_se_km[ih], renda_pc = hx$renda_pc[ih], subnormal = hx$subnormal[ih], densidade = hx$densidade[ih],
	esgoto = hx$esgoto[ih], pavim = hx$pavim[ih], cel5 = hx$cel5[ih], h38 = p8[ih])]
BASE[is.na(anos_atualizacao), anos_atualizacao := median(BASE$anos_atualizacao, na.rm = TRUE)]
## categorias como indicadoras (no H3-8 viram proporções); criança e sexo como indicadoras
DUM <- character(0)
for (v in c("raca", "deficiencia", "alfabetizado")) {
	f <- factor(BASE[[v]])
	for (l in levels(f)[-1]) {
		nm <- paste0(v, "_", make.names(l))
		set(BASE, j = nm, value = as.numeric(!is.na(f) & f == l))
		DUM <- c(DUM, nm)
	}
	set(BASE, j = paste0(v, "_na"), value = as.numeric(is.na(f)))
	DUM <- c(DUM, paste0(v, "_na"))
	set(BASE, j = v, value = NULL)
}
BASE[, `:=`(mulher = as.numeric(sx %in% 2L), cr1 = as.numeric(cr %in% 1L), cr_na = as.numeric(is.na(cr)))]
msg("base: ", nrow(BASE), " pessoas × evento × janela")

## estimação --------------------------------------------------------------------------------------------------------------
source("R/funcoes_iptw_evento.R")
SEG <- list(todos = function(d) rep(TRUE, nrow(d)), mulheres = function(d) d$sx %in% 2L, homens = function(d) d$sx %in% 1L,
	"18 a 29" = function(d) d$idade < 30, "30 a 44" = function(d) d$idade >= 30 & d$idade < 45, "45 a 60" = function(d) d$idade >= 45,
	"com criança" = function(d) d$cr %in% 1L, "sem criança" = function(d) d$cr %in% 0L,
	"mulheres com criança" = function(d) d$sx %in% 2L & d$cr %in% 1L, "mulheres sem criança" = function(d) d$sx %in% 2L & d$cr %in% 0L,
	"homens com criança" = function(d) d$sx %in% 1L & d$cr %in% 1L, "homens sem criança" = function(d) d$sx %in% 1L & d$cr %in% 0L)
COVS <- c("idade", "mulher", "cr1", "cr_na", DUM, "anos_atualizacao", "e_b", "e_b12", "e_b24", "m_b12", "lacc", "dist_se", "renda_pc", "subnormal",
	"densidade", "esgoto", "pavim")
faixas <- list(f1a5 = c(1, 5), f5a20 = c(5, 20), f20mais = c(20, Inf))
PARES <- list(c("mulheres", "homens"), c("30 a 44", "18 a 29"), c("45 a 60", "18 a 29"), c("com criança", "sem criança"),
	c("mulheres com criança", "mulheres sem criança"), c("homens com criança", "homens sem criança"))
RES <- list()
CT <- list()
BAL <- list()
for (cc in cortes) for (jn in janelas) {
	Bj <- BASE[janela == jn & !is.na(get(paste0("pct_", cc)))]
	Bj[, `:=`(pct = get(paste0("pct_", cc)), lacc = get(paste0("lacc_", cc)))]
	for (fx in names(faixas)) {
		lim <- faixas[[fx]]
		Bf <- Bj[pct < 1 | (pct >= lim[1] & pct < lim[2])]
		RR <- list()
		if (nivel == "ind") {
			Bf[, t1 := as.integer(pct >= 1)]
			Bf <- pesos_att(Bf, COVS)
			BAL[[length(BAL) + 1L]] <- balanco(Bf, COVS)[, `:=`(nivel = nivel, corte = cc, janela = jn, faixa = fx, segmento = "todos")]
		}
		for (sg in names(SEG)) {
			if (nivel == "ind") {
				Bs <- Bf[SEG[[sg]](Bf)]
				if (sum(Bs$t1) < 50L) next
				Bs[, w := renorm(w0, t1, ev)]
			} else {
				## H3-8: pessoas do segmento agregadas por hexágono H3-8 × evento; faixa pela média do ganho
				Ps <- Bf[SEG[[sg]](Bf)]
				Bs <- agrega_cel(Ps, c(COVS, "pct", unlist(oc_sets)), "h38", oc_sets)
				rm(Ps)
				Bs <- Bs[pct < 1 | (pct >= lim[1] & pct < lim[2])]
				Bs[, t1 := as.integer(pct >= 1)]
				Bs <- pesos_att(Bs, COVS)
				if (sum(Bs$t1) < 30L) next
				Bs[, w := renorm(w0, t1, ev)]
				BAL[[length(BAL) + 1L]] <- balanco(Bs, COVS)[, `:=`(nivel = nivel, corte = cc, janela = jn, faixa = fx, segmento = sg)]
			}
			for (cj in names(oc_sets)) {
				oc <- intersect(oc_sets[[cj]], names(Bs))
				po <- if (nivel == "h38") Bs[[paste0("cnt_", cj)]] / Bs$n else NULL
				r <- dr_iptw(Bs, oc, COVS, po)
				if (is.null(r)) next
				for (o in seq_along(oc)) {
					esc <- if (grepl("^salario", oc[o])) 1 else 100
					RES[[length(RES) + 1L]] <- data.table(nivel = nivel, corte = cc, janela = jn, faixa = fx, segmento = sg, conjunto = sub("_[0-9]+$", "", cj),
						outcome = oc[o], att = esc * r$b[o], ep = esc * r$se[o], p = 2 * pnorm(-abs(r$b[o] / r$se[o])), tratados = r$nt[o], controles = r$nc[o],
						media_tratados = esc * r$my[o])
				}
				RR[[paste(sg, cj)]] <- r
			}
			rm(Bs)
		}
		for (cj in names(oc_sets)) for (pp in PARES) {
			a <- RR[[paste(pp[1], cj)]]
			bb <- RR[[paste(pp[2], cj)]]
			if (is.null(a) || is.null(bb)) next
			for (o in seq_along(a$oc)) {
				esc <- if (grepl("^salario", a$oc[o])) 1 else 100
				z <- dif_iptw(a, bb, o)
				CT[[length(CT) + 1L]] <- data.table(nivel = nivel, corte = cc, janela = jn, faixa = fx, conjunto = sub("_[0-9]+$", "", cj), outcome = a$oc[o],
					contraste = paste(pp[1], "−", pp[2]), diferenca = esc * z[["b"]], ep = esc * z[["se"]], p = z[["p"]])
			}
		}
		msg(nivel, " corte ", cc, " ", jn, " ", fx, " feito")
		fwrite(rbindlist(RES), sprintf("outputs/final/iptw_area_%s%s_segmentos.csv", nivel, suf_x))
		gc()
	}
}
fwrite(rbindlist(CT), sprintf("outputs/final/iptw_area_%s%s_contrastes.csv", nivel, suf_x))
fwrite(rbindlist(BAL, fill = TRUE), sprintf("outputs/final/iptw_area_%s%s_balanco.csv", nivel, suf_x))
msg("fim iptw área ", nivel)

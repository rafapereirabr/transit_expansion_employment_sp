# Especificação final, bloco A (24/11/2026): acessibilidade a partir de casa e emprego, entrada, permanência e salário,
# no desenho por evento (data exata de serviço pleno), a partir da base por evento (R/118_base_evento_area.R).
# Origem: explora_final_area.R.
# - Unidade: nivel=h39 (célula H3-9 × evento com pesos = pessoas; numericamente igual ao painel individual, porque dose,
#   controles e efeitos fixos variam só nessas células) ou nivel=h38 (hexágono H3-8 × evento; dose = média das doses H3-9
#   ponderada por pessoas, em cada sorteio).
# - Dose dos níveis que recebem a data do evento em cada sorteio: acessibilidade cumulativa a 30, 45, 60 e 90 min (por
#   +100 mil empregos) e gravitacional calibrada (10 × log(1 + ganho/base), por +0,1 no log; base = acessibilidade de
#   logo antes do evento). Também o efeito por 1 desvio-padrão do ganho realizado.
# - B&H: dose recentrada pela média nos sorteios, média × evento como controle, efeito fixo de célula H3-7 × evento,
#   EP por célula de 5 km, p por permutação (199 sorteios).
# - Desfechos (h = 6, 12, 18, 24): emprego (todos; pesos = pessoas), entrada (sem vínculo em ref; pesos = essas pessoas),
#   permanência e fração dos meses (com vínculo em ref; pesos = essas pessoas), salário (empregadas em ref e ref+h com
#   salário válido; pesos = essas pessoas, um h por vez).
# - Segmentos: todos; mulheres, homens; 18–29, 30–44, 45–60; com e sem criança; mulheres e homens com e sem criança.
#   Contrastes: diferença com EP de cluster conjunto e p de permutação com os mesmos sorteios; teste conjunto da idade.
# Efeito fixo (fe=): casa7 (principal, célula H3-7 de casa × evento), casa8 (H3-8 × evento) ou ev (só evento).
# seg= escolhe os segmentos por número (1 todos, 2 mulheres, 3 homens, 4 18–29, 5 30–44, 6 45–60, 7 com criança,
# 8 sem criança, 9 mulheres com, 10 mulheres sem, 11 homens com, 12 homens sem criança), separados por ";"; o padrão são
# os 12. Robustez: base= lê a base com esse sufixo (R5: base=_lag1...; R4: base=_mensal com horizontes=1,...,24);
# conjuntos= restringe os desfechos (emprego, entrada, permanencia, salario).
# Uso: Rscript R/120_bh_evento_area.R nivel=h39|h38 doses=c30,c45,c60,c90,grav [perm=199] [fe=casa7] [seg=] [sufixo=]
#      [base=] [horizontes=6,12,18,24] [conjuntos=]
# Saídas: outputs/final/area_<nivel><sufixo>_segmentos.csv e _contrastes.csv
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
source("R/funcoes_bh_evento.R")
nivel <- arg_cli("nivel", NA, "h39")
stopifnot(nivel %in% c("h39", "h38"))
doses_pedidas <- strsplit(arg_cli("doses", NA, "c30,c45,c60,c90,grav"), ",")[[1]]
n_perm <- as.integer(arg_cli("perm", NA, 199L))
suf_x <- arg_cli("sufixo", NA, "")
## efeito fixo: casa7 (principal, célula H3-7 de casa × evento), casa8 (H3-8 × evento, só com nivel=h39) ou ev (só evento)
fe_tipo <- arg_cli("fe", NA, "casa7")
stopifnot(fe_tipo %in% c("casa7", "casa8", "ev"), !(nivel == "h38" && fe_tipo == "casa8"))
dir.create("outputs/final", showWarnings = FALSE)
H <- as.integer(strsplit(arg_cli("horizontes", NA, "6,12,18,24"), ",")[[1]])
suf_base <- arg_cli("base", NA, "")
niv <- le_niveis()
Dd <- le_sorteios()[, 1:200]
hx <- le_hex()
ids <- le_ganhos(60L)$id
hx <- hx[match(ids, hx$id)]
GM <- list()
A0M <- list()
for (c in c(30L, 45L, 60L, 90L)) {
	g <- le_ganhos(c)
	GM[[paste0("c", c)]] <- as.matrix(g[match(ids, id), niv$nivel, with = FALSE])
	A0M[[paste0("c", c)]] <- g$acc_A00[match(ids, g$id)]
}
gcg <- le_ganhos("gcal")
Gacc <- as.matrix(gcg[match(ids, id), niv$nivel, with = FALSE])
A00 <- gcg$acc_A00[match(ids, gcg$id)]
cad <- niveis_cadeia(niv)
rm(gcg, g)
EVT <- fread(sprintf("data/base/explora/ev_cache/eventos_area%s.csv", suf_base))
CEL <- as.data.table(arrow::read_parquet(sprintf("data/base/explora/ev_cache/area_celulas%s.parquet", suf_base)))
msg("células: ", nrow(CEL))

## unidade
if (nivel == "h39") {
	CEL[, un := ih]
} else {
	p8 <- h3jsr::get_parent(ids, res = 8)
	u8 <- unique(p8)
	CEL[, un := match(p8[ih], u8)]
}
## dose de um hexágono H3-9 por sorteio
dose_hex <- function(tp, ih, ev) {
	out <- matrix(0, length(ih), ncol(Dd))
	for (e in unique(ev)) {
		ix <- which(ev == e)
		E <- EVT[ev == e]
		on <- (Dd == E$d_e) * 1
		antes <- (Dd < E$d_e) * 1
		if (tp == "grav") {
			ganho <- Gacc[ih[ix], , drop = FALSE] %*% on
			base <- A00[ih[ix]] - rowSums(Gacc[ih[ix], cad, drop = FALSE]) + Gacc[ih[ix], , drop = FALSE] %*% antes
			out[ix, ] <- 10 * log1p(pmin(ganho / pmax(base, 1000), 1))
		} else if (tp == "gravabs") {
			## gravitacional em valor absoluto (24/11/2026): por +100 mil empregos ponderados pela distância
			out[ix, ] <- (Gacc[ih[ix], , drop = FALSE] %*% on) / 1e5
		} else if (grepl("p$", tp)) {
			## cumulativa em percentual (24/11/2026): ganho em % da acessibilidade de logo antes do evento em cada sorteio
			## (base mínima de 1.000 empregos), por +10%
			cc <- sub("p$", "", tp)
			ganho <- GM[[cc]][ih[ix], , drop = FALSE] %*% on
			base <- A0M[[cc]][ih[ix]] - rowSums(GM[[cc]][ih[ix], cad, drop = FALSE]) + GM[[cc]][ih[ix], , drop = FALSE] %*% antes
			out[ix, ] <- 100 * ganho / pmax(base, 1000) / 10
		} else out[ix, ] <- (GM[[tp]][ih[ix], , drop = FALSE] %*% on) / 1e5
	}
	out[is.na(out)] <- 0
	out
}
rot_dose <- c(c30 = "cumulativa 30 min (+100 mil empregos)", c45 = "cumulativa 45 min (+100 mil empregos)", c60 = "cumulativa 60 min (+100 mil empregos)",
	c90 = "cumulativa 90 min (+100 mil empregos)", grav = "gravitacional (+0,1 no log)",
	c30p = "cumulativa 30 min (+10% da acessibilidade de antes)", c45p = "cumulativa 45 min (+10% da acessibilidade de antes)",
	c60p = "cumulativa 60 min (+10% da acessibilidade de antes)", c90p = "cumulativa 90 min (+10% da acessibilidade de antes)",
	gravabs = "gravitacional (+100 mil empregos ponderados)")
SEG <- list(todos = function(d) rep(TRUE, nrow(d)), mulheres = function(d) d$sx %in% 2L, homens = function(d) d$sx %in% 1L,
	"18 a 29" = function(d) d$id3 %in% 1L, "30 a 44" = function(d) d$id3 %in% 2L, "45 a 60" = function(d) d$id3 %in% 3L,
	"com criança" = function(d) d$cr %in% 1L, "sem criança" = function(d) d$cr %in% 0L,
	"mulheres com criança" = function(d) d$sx %in% 2L & d$cr %in% 1L, "mulheres sem criança" = function(d) d$sx %in% 2L & d$cr %in% 0L,
	"homens com criança" = function(d) d$sx %in% 1L & d$cr %in% 1L, "homens sem criança" = function(d) d$sx %in% 1L & d$cr %in% 0L)
CONJ <- c(list(emprego = list(oc = paste0("emprego_", H), w = "n_todos"), entrada = list(oc = paste0("entrada_", H), w = "n_sem"),
	permanencia = list(oc = c(paste0("empregado_", H), paste0("meses_", H)), w = "n_com")),
	setNames(lapply(H, function(h) list(oc = paste0("salario_", h), w = paste0("n_salario_", h))), paste0("salario_", H)))
somas <- setdiff(names(CEL), c("ih", "sx", "id3", "cr", "janela", "ev", "un"))
## segmentos (seg= por número, porque acentos em argumentos se corrompem no Windows) e conjuntos de desfechos pedidos
segs <- strsplit(arg_cli("seg", NA, paste(seq_along(SEG), collapse = ";")), ";")[[1]]
if (all(grepl("^[0-9]+$", segs))) segs <- names(SEG)[as.integer(segs)]
stopifnot(all(segs %in% names(SEG)))
conj_pedidos <- arg_cli("conjuntos", NA, "")
if (nzchar(conj_pedidos)) CONJ <- CONJ[sub("_[0-9]+$", "", names(CONJ)) %in% strsplit(conj_pedidos, ",")[[1]]]

RES <- list()
RR <- list()
for (jn in c("pos", "pre")) for (sg in segs) {
	C1 <- CEL[janela == jn][SEG[[sg]](CEL[janela == jn])]
	## agregação à unidade; a dose da unidade H3-8 é a média das doses dos hexágonos H3-9 ponderada por pessoas
	U <- C1[, lapply(.SD, sum), by = .(un, ev), .SDcols = somas][order(ev, un)]
	HX <- C1[, .(n = sum(n_todos)), by = .(un, ev, ih)]
	ihr <- HX[, .SD[which.max(n)], by = .(un, ev)][match(paste(U$un, U$ev), paste(un, ev)), ih]
	fe <- switch(fe_tipo, casa7 = list(fc = paste(hx$h3_7[ihr], U$ev)), casa8 = list(fc = paste(h3jsr::get_parent(ids[ihr], res = 8), U$ev)), ev = list(ev = U$ev))
	cl <- hx$cel5[ihr]
	for (tp in doses_pedidas) {
		Dh <- dose_hex(tp, HX$ih, HX$ev)
		if (nivel == "h39") {
			D <- Dh[match(paste(U$un, U$ev), paste(HX$un, HX$ev)), , drop = FALSE]
		} else {
			key <- paste(HX$un, HX$ev)
			D <- rowsum(Dh * HX$n, key) / as.vector(rowsum(HX$n, key))
			D <- D[match(paste(U$un, U$ev), rownames(D)), , drop = FALSE]
		}
		rm(Dh)
		for (cj in names(CONJ)) {
			oc <- intersect(CONJ[[cj]]$oc, names(U))
			w <- U[[CONJ[[cj]]$w]]
			Y <- as.matrix(U[, oc, with = FALSE]) / w
			r <- tryCatch(bh_lista2(Y, w, list(D), U$ev, cl, fe, n_perm = n_perm),
				error = function(err) { msg("erro ", jn, " ", sg, " ", tp, " ", cj, ": ", conditionMessage(err)); NULL })
			if (is.null(r)) next
			ok <- r$ok
			dsd <- sqrt(sum(w[ok] * (D[ok, 1] - weighted.mean(D[ok, 1], w[ok]))^2) / sum(w[ok]))
			for (o in seq_along(oc)) {
				z <- comb_bh2(r, o, 1)
				esc <- if (grepl("^salario", oc[o])) 1 else 100
				RES[[length(RES) + 1L]] <- data.table(nivel = nivel, fe = fe_tipo, janela = jn, segmento = sg, dose = rot_dose[[tp]], conjunto = sub("_[0-9]+$", "", cj),
					outcome = oc[o], efeito = esc * z[["b"]], ep = esc * z[["se"]], p_perm = z[["p_perm"]], p_normal = z[["p_normal"]],
					efeito_1dp = esc * z[["b"]] * dsd, dose_media = weighted.mean(D[ok, 1], w[ok]), dose_dp = dsd,
					media_y = esc * weighted.mean(Y[ok, o], w[ok]), pessoas = sum(w[ok]), unidades = sum(ok), perm = dim(r$B)[1] - 1L)
			}
			RR[[paste(jn, sg, tp, cj)]] <- list(B = r$B, SC = r$SC, INV = r$INV, oc = oc)
			rm(r)
		}
		rm(D)
		gc()
	}
	msg(nivel, " ", jn, " ", sg, " feito")
	fwrite(rbindlist(RES), sprintf("outputs/final/area_%s%s_segmentos.csv", nivel, suf_x))
}

## contrastes entre grupos ------------------------------------------------------------------------------------------------
PARES <- list(c("mulheres", "homens"), c("30 a 44", "18 a 29"), c("45 a 60", "18 a 29"), c("com criança", "sem criança"),
	c("mulheres com criança", "mulheres sem criança"), c("homens com criança", "homens sem criança"))
CT <- list()
for (jn in c("pos", "pre")) for (tp in doses_pedidas) for (cj in names(CONJ)) {
	pega <- function(sg) RR[[paste(jn, sg, tp, cj)]]
	r0 <- pega(segs[1])
	if (is.null(r0)) next
	for (o in seq_along(r0$oc)) {
		esc <- if (grepl("^salario", r0$oc[o])) 1 else 100
		add <- function(rot, z) CT[[length(CT) + 1L]] <<- data.table(nivel = nivel, janela = jn, dose = rot_dose[[tp]], conjunto = sub("_[0-9]+$", "", cj),
			outcome = r0$oc[o], contraste = rot, diferenca = esc * z[["b"]], ep = esc * z[["se"]], p_normal = z[["p_normal"]], p_perm = z[["p_perm"]])
		for (pp in PARES) {
			a <- pega(pp[1])
			b <- pega(pp[2])
			if (is.null(a) || is.null(b)) next
			add(paste(pp[1], "−", pp[2]), dif_bh2(a, o, 1, b, o, 1))
		}
		## diferença das diferenças: (mulheres com − sem) − (homens com − sem)
		g4 <- lapply(c("mulheres com criança", "mulheres sem criança", "homens com criança", "homens sem criança"), pega)
		if (!any(sapply(g4, is.null))) {
			cc <- c(1, -1, -1, 1)
			b <- sum(sapply(1:4, function(g) cc[g] * g4[[g]]$B[1, 1, o]))
			P <- min(sapply(g4, function(r) dim(r$B)[1]))
			dr <- Reduce(`+`, lapply(1:4, function(g) cc[g] * g4[[g]]$B[2:P, 1, o]))
			A <- lapply(1:4, function(g) g4[[g]]$SC[[o]] %*% g4[[g]]$INV)
			cls <- unique(unlist(lapply(A, rownames)))
			s <- setNames(rep(0, length(cls)), cls)
			for (g in 1:4) s[rownames(A[[g]])] <- s[rownames(A[[g]])] + cc[g] * A[[g]][, 1]
			se <- sqrt(sum(s^2))
			add("(mulheres com − sem criança) − (homens com − sem criança)", c(b = b, se = se, p_normal = 2 * pnorm(-abs(b / se)), p_perm = mean(abs(dr) >= abs(b))))
		}
		## idade: teste conjunto
		g3 <- lapply(c("18 a 29", "30 a 44", "45 a 60"), pega)
		if (!any(sapply(g3, is.null))) {
			wz <- wald_grupos2(g3, rep(o, 3), list(1, 1, 1))
			CT[[length(CT) + 1L]] <- data.table(nivel = nivel, janela = jn, dose = rot_dose[[tp]], conjunto = sub("_[0-9]+$", "", cj), outcome = r0$oc[o],
				contraste = "idade: teste conjunto de igualdade (3 grupos)", wald = wz[["wald"]], gl = wz[["gl"]], p_normal = wz[["p"]])
		}
	}
}
fwrite(rbindlist(CT, fill = TRUE), sprintf("outputs/final/area_%s%s_contrastes.csv", nivel, suf_x))
msg("fim final área ", nivel)

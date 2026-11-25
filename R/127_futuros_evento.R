# Desenho de controles futuros na mesma faixa (24/11/2026, pedido do pesquisador), com a data exata de serviço pleno,
# a partir das bases em cache da especificação final.
# - Para cada evento e (data de serviço pleno) e cada faixa de ganho b:
#   tratados = unidades com ganho do evento e na faixa b;
#   controles = unidades sem ganho (abaixo de 1% na área; abaixo de 1 min no tempo de viagem) de qualquer abertura que
#     toque a janela do evento (da referência do placebo a 24 meses depois da referência) e com o PRIMEIRO ganho futuro na
#     mesma faixa b: de um evento posterior cuja primeira abertura cai depois dessa janela, das linhas abertas em 2025
#     (A01, A02, fora dos eventos porque a RAIS termina em 2025) ou das obras futuras (A24–A26).
#   Os tratados também não podem ter ganho de outra abertura que toque a janela.
# - Ganho na área: % da acessibilidade de logo antes da abertura (base mínima de 1.000 empregos), nos cortes 30, 45, 60
#   e 90 min e na gravitacional; faixas 1–5%, 5–20%, 20% ou mais. No tempo de viagem: minutos ganhos no trajeto casa →
#   trabalho, faixas 1–5, 5–10, 10 ou mais; separado por trajeto antes até 90 e acima de 90 min e com todos os trajetos juntos.
# - Estimador: diferença em diferenças empilhada (os desfechos já são variações dentro da pessoa: emprego e salário entre
#   ref e ref+h, entrada, permanência, desligamentos), com efeito fixo de evento × faixa (cada evento e faixa só compara
#   tratados e controles entre si), pesos = pessoas (área) ou vínculos (tempo de viagem), EP por célula de 5 km de casa.
#   Por faixa e com todas as faixas juntas. Placebo: a mesma comparação na janela 25 meses antes da primeira abertura.
# - Segmentos e contrastes no mesmo modelo (totalmente interagido, covariância conjunta): sexo, idade, criança de 0 a 5
#   anos na família, sexo × criança e, no tempo de viagem, tempo de vínculo.
# - nivel=h39: unidade H3-9 (área) ou par H3-9 × H3-9 (igual ao painel individual). nivel=h38: as unidades H3-9 de mesmo
#   status (tratado ou controle) e faixa somadas por hexágono H3-8 (ou par H3-8 × H3-8).
# Uso: Rscript R/127_futuros_evento.R parte=area|par nivel=h39|h38 [cortes=c30,c45,c60,c90,grav]
# (bloco G do pipeline; mesma versão do script solto explora_futuros.R das rodadas de 24/11/2026)
# Saídas: outputs/final/futuros_<parte>_<nivel>_segmentos.csv, _contrastes.csv e _amostra.csv
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
setFixest_notes(FALSE)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
parte <- arg_cli("parte", NA, "area")
nivel <- arg_cli("nivel", NA, "h39")
stopifnot(parte %in% c("area", "par"), nivel %in% c("h39", "h38"))
cortes <- strsplit(arg_cli("cortes", NA, "c30,c45,c60,c90,grav"), ",")[[1]]
dir.create("outputs/final", showWarnings = FALSE)
H <- c(6L, 12L, 18L, 24L)
mes_idx <- function(d) (year(d) - 2011L) * 12L + month(d)
niv <- le_niveis()
cad <- niveis_cadeia(niv)
hx <- le_hex()
ids <- le_ganhos(60L)$id
hx <- hx[match(ids, hx$id)]
p8 <- h3jsr::get_parent(ids, res = 8)
dd <- "data/base/explora/ev_cache"
EVT <- fread(file.path(dd, if (parte == "area") "eventos_area.csv" else "eventos.csv"))
if (parte == "par") setnames(EVT, "coorte", "ev")

## grupos de níveis por data de serviço pleno (todas as aberturas, inclusive 2025 e obras futuras)
GR <- niv[, cadeia_flag := cad][, .(o_min = min(mes_idx(abertura)), m_e = mes_idx(pleno[1]), d_e = as.integer(pleno[1]), niveis = list(nivel),
	futura = !all(cadeia_flag)), by = .(pleno)][order(pleno)]
GR[, g := seq_len(.N)]
## evento de cada grupo (grupos dos eventos da especificação têm a mesma data de serviço pleno)
GR[, ev := EVT$ev[match(d_e, EVT$d_e)]]
msg("grupos de abertura: ", nrow(GR), " (", sum(!is.na(GR$ev)), " eventos; ", sum(is.na(GR$ev)), " fora dos eventos: ",
	paste(format(GR[is.na(ev), pleno]), collapse = ", "), ")")

## ganho de cada grupo: área (% da acessibilidade de logo antes, por hexágono) ou tempo (minutos, por par)
ganho_area <- function(Gm, A0) {
	sapply(GR$g, function(k) {
		G <- GR[g == k]
		lv <- match(G$niveis[[1]], niv$nivel)
		base <- if (G$futura) A0 else A0 - rowSums(Gm[, cad & as.integer(niv$pleno) >= G$d_e, drop = FALSE])
		100 * rowSums(Gm[, lv, drop = FALSE]) / pmax(base, 1000)
	})
}
faixa_de <- function(x, cortes_f) {
	b <- cut(x, c(cortes_f, Inf), right = FALSE, labels = FALSE)
	nomes <- if (parte == "area") c("f1a5", "f5a20", "f20mais") else c("f1a5", "f5a10", "f10mais")
	nomes[b]
}
lim_f <- if (parte == "area") c(1, 5, 20) else c(1, 5, 10)
## status de cada unidade (linhas de M = unidades, colunas = grupos) para o evento e: tratado (faixa do ganho do evento),
## controle (faixa do primeiro ganho futuro) ou fora
status <- function(M, e) {
	E <- EVT[ev == e]
	k_e <- GR[ev == e, g]
	toca <- GR$o_min <= E$ref_pos + 24L & GR$m_e >= E$ref_pre
	outros <- setdiff(which(toca), k_e)
	suja <- if (length(outros)) rowSums(M[, outros, drop = FALSE] >= 1) > 0 else rep(FALSE, nrow(M))
	ge <- M[, k_e]
	fut <- which(GR$o_min > E$ref_pos + 24L)
	## primeiro ganho futuro de pelo menos 1 (% ou min)
	pf <- rep(NA_real_, nrow(M))
	for (k in fut) {
		s <- is.na(pf) & M[, k] >= 1
		pf[s] <- M[s, k]
	}
	trat <- !suja & ge >= 1
	ctrl <- !suja & ge < 1 & !is.na(pf)
	data.table(T = fifelse(trat, 1L, fifelse(ctrl, 0L, NA_integer_)), faixa = fifelse(trat, faixa_de(ge, lim_f), fifelse(ctrl, faixa_de(pf, lim_f), NA_character_)),
		ganho = fifelse(trat, ge, fifelse(ctrl, pf, NA_real_)))
}

## esquemas de segmentos (grupos disjuntos estimados no mesmo modelo totalmente interagido)
ESQ <- list(todos = function(d) rep("todos", nrow(d)),
	sexo = function(d) fcase(d$sx %in% 2L, "mulheres", d$sx %in% 1L, "homens"),
	idade = function(d) fcase(d$id3 %in% 1L, "18 a 29", d$id3 %in% 2L, "30 a 44", d$id3 %in% 3L, "45 a 60"),
	crianca = function(d) fcase(d$cr %in% 1L, "com criança", d$cr %in% 0L, "sem criança"),
	sexo_crianca = function(d) fcase(d$sx %in% 2L & d$cr %in% 1L, "mulheres com criança", d$sx %in% 2L & d$cr %in% 0L, "mulheres sem criança",
		d$sx %in% 1L & d$cr %in% 1L, "homens com criança", d$sx %in% 1L & d$cr %in% 0L, "homens sem criança"))
if (parte == "par") ESQ$vinculo <- function(d) fcase(d$tn2 %in% 1L, "vínculo até 12 meses", d$tn2 %in% 2L, "vínculo mais de 12 meses")
CONTR <- list(sexo = list(c("mulheres", "homens")), idade = list(c("30 a 44", "18 a 29"), c("45 a 60", "18 a 29")), crianca = list(c("com criança", "sem criança")),
	sexo_crianca = list(c("mulheres com criança", "mulheres sem criança"), c("homens com criança", "homens sem criança")),
	vinculo = list(c("vínculo até 12 meses", "vínculo mais de 12 meses")))

RES <- list()
CT <- list()
AM <- list()
## estima um conjunto de desfechos (mesmos pesos) para um esquema: y ~ T × grupo | grupo × evento × faixa
estima <- function(D, oc, rot) {
	grs <- sort(unique(D$grp))
	for (gname in grs) set(D, j = paste0("T_", make.names(gname)), value = as.numeric(D$T == 1L & D$grp == gname))
	tn <- paste0("T_", make.names(grs))
	D[, efx := paste(grp, eb)]
	f <- as.formula(paste0("c(", paste(oc, collapse = ", "), ") ~ ", paste(tn, collapse = " + "), " | efx"))
	m <- tryCatch(feols(f, data = D, weights = ~w, cluster = ~cel5), error = function(err) { msg("erro ", paste(rot, collapse = " "), ": ", conditionMessage(err)); NULL })
	if (is.null(m)) return(invisible(NULL))
	ms <- if (length(oc) == 1L) list(m) else as.list(m)
	for (o in seq_along(oc)) {
		mo <- ms[[o]]
		b <- coef(mo)
		V <- vcov(mo)
		esc <- if (grepl("^salario", oc[o])) 1 else 100
		for (i in seq_along(grs)) {
			nm <- tn[i]
			if (!nm %in% names(b)) next
			s <- D$grp == grs[i] & is.finite(D[[oc[o]]])
			RES[[length(RES) + 1L]] <<- data.table(rot, segmento = grs[i], outcome = oc[o], efeito = esc * b[[nm]], ep = esc * sqrt(V[nm, nm]),
				p = 2 * pnorm(-abs(b[[nm]] / sqrt(V[nm, nm]))), tratados = sum(D$w[s & D$T == 1L]), controles = sum(D$w[s & D$T == 0L]),
				media_tratados = esc * weighted.mean(D[[oc[o]]][s & D$T == 1L], D$w[s & D$T == 1L]),
				media_controles = esc * weighted.mean(D[[oc[o]]][s & D$T == 0L], D$w[s & D$T == 0L]))
		}
		cts <- CONTR[[rot$esquema]]
		for (pp in cts) {
			a <- paste0("T_", make.names(pp[1]))
			z <- paste0("T_", make.names(pp[2]))
			if (!all(c(a, z) %in% names(b))) next
			d <- b[[a]] - b[[z]]
			se <- sqrt(V[a, a] + V[z, z] - 2 * V[a, z])
			CT[[length(CT) + 1L]] <<- data.table(rot, outcome = oc[o], contraste = paste(pp[1], "−", pp[2]), diferenca = esc * d, ep = esc * se, p = 2 * pnorm(-abs(d / se)))
		}
		if (rot$esquema == "sexo_crianca" && all(paste0("T_", make.names(c("mulheres com criança", "mulheres sem criança", "homens com criança", "homens sem criança"))) %in% names(b))) {
			cc <- setNames(c(1, -1, -1, 1), paste0("T_", make.names(c("mulheres com criança", "mulheres sem criança", "homens com criança", "homens sem criança"))))
			d <- sum(cc * b[names(cc)])
			se <- sqrt(as.numeric(t(cc) %*% V[names(cc), names(cc)] %*% cc))
			CT[[length(CT) + 1L]] <<- data.table(rot, outcome = oc[o], contraste = "(mulheres com − sem criança) − (homens com − sem criança)", diferenca = esc * d,
				ep = esc * se, p = 2 * pnorm(-abs(d / se)))
		}
		if (rot$esquema == "idade" && length(grs) == 3L) {
			R <- rbind(c(-1, 1, 0), c(-1, 0, 1))
			bb <- b[tn]
			W <- as.numeric(t(R %*% bb) %*% solve(R %*% V[tn, tn] %*% t(R)) %*% (R %*% bb))
			CT[[length(CT) + 1L]] <<- data.table(rot, outcome = oc[o], contraste = "idade: teste conjunto de igualdade (3 grupos)", wald = W, gl = 2,
				p = pchisq(W, 2, lower.tail = FALSE))
		}
	}
	invisible(NULL)
}
grava <- function() {
	suf <- sprintf("outputs/final/futuros_%s_%s", parte, nivel)
	fwrite(rbindlist(RES, fill = TRUE), paste0(suf, "_segmentos.csv"))
	fwrite(rbindlist(CT, fill = TRUE), paste0(suf, "_contrastes.csv"))
	fwrite(rbindlist(AM, fill = TRUE), paste0(suf, "_amostra.csv"))
}

if (parte == "area") {
	CEL <- as.data.table(arrow::read_parquet(file.path(dd, "area_celulas.parquet")))
	somas <- setdiff(names(CEL), c("ih", "sx", "id3", "cr", "janela", "ev"))
	CONJ <- c(list(emprego = list(oc = paste0("emprego_", H), w = "n_todos"), entrada = list(oc = paste0("entrada_", H), w = "n_sem"),
		permanencia = list(oc = c(paste0("empregado_", H), paste0("meses_", H)), w = "n_com")),
		setNames(lapply(H, function(h) list(oc = paste0("salario_", h), w = paste0("n_salario_", h))), paste0("salario_", H)))
	for (cc in cortes) {
		g <- if (cc == "grav") le_ganhos("gcal") else le_ganhos(as.integer(sub("c", "", cc)))
		Gm <- as.matrix(g[match(ids, id), niv$nivel, with = FALSE])
		M <- ganho_area(Gm, g$acc_A00[match(ids, g$id)])
		M[!is.finite(M)] <- 0
		rm(g, Gm)
		ST <- rbindlist(lapply(EVT$ev, function(e) cbind(ih = seq_along(ids), ev = e, status(M, e))))[!is.na(T)]
		for (jn in c("pos", "pre")) {
			C1 <- merge(CEL[janela == jn], ST, by = c("ih", "ev"))
			AM[[length(AM) + 1L]] <- C1[, .(hexagonos = uniqueN(ih), pessoas = sum(n_todos), ganho_medio = weighted.mean(ganho, n_todos)), by = .(ev, faixa, T)][,
				`:=`(corte = cc, janela = jn)]
			for (es in names(ESQ)) {
				C1[, grp := ESQ[[es]](C1)]
				C2 <- C1[!is.na(grp)]
				C2[, un := if (nivel == "h39") ih else match(p8[ih], unique(p8))]
				U <- C2[, c(lapply(.SD, sum), list(ihr = ih[which.max(n_todos)])), by = .(un, ev, T, faixa, grp), .SDcols = somas]
				U[, cel5 := hx$cel5[ihr]]
				for (fx in c("todas", "f1a5", "f5a20", "f20mais")) {
					Ub <- if (fx == "todas") U else U[faixa == fx]
					if (!nrow(Ub) || sum(Ub$T == 1L) < 10L || sum(Ub$T == 0L) < 10L) next
					Ub <- copy(Ub)
					Ub[, eb := paste(ev, faixa)]
					for (cj in names(CONJ)) {
						oc <- intersect(CONJ[[cj]]$oc, names(Ub))
						D <- copy(Ub)
						D[, w := get(CONJ[[cj]]$w)]
						for (o in oc) set(D, j = o, value = fifelse(D$w > 0, D[[o]] / D$w, NA_real_))
						D <- D[w > 0]
						estima(D, oc, data.table(parte = parte, nivel = nivel, corte = cc, janela = jn, faixa = fx, esquema = es, conjunto = sub("_[0-9]+$", "", cj)))
					}
				}
			}
			msg("área ", nivel, " ", cc, " ", jn, " feito")
			grava()
		}
	}
}

if (parte == "par") {
	PR <- as.data.table(arrow::read_parquet(file.path(dd, "par_pares.parquet")))
	DL <- as.matrix(PR[, niv$nivel, with = FALSE])
	## minutos ganhos por grupo de abertura, por par
	M <- sapply(GR$g, function(k) rowSums(DL[, match(GR[g == k, niveis][[1]], niv$nivel), drop = FALSE]))
	M[!is.finite(M)] <- 0
	CEL <- as.data.table(arrow::read_parquet(file.path(dd, "par_celulas.parquet")))
	CEL <- CEL[t_antes > 0 & t_antes <= 241]
	CEL[, `:=`(pr = match(ip, PR$ip), g90 = t_antes <= 90)]
	setnames(CEL, "evento", "ev")
	OC <- c(as.vector(outer(c("qualquer", "demissao", "sem_justa", "justa", "pedido"), H, paste, sep = "_")), as.vector(outer(c("empregado", "meses"), H, paste, sep = "_")))
	somas <- c("n", paste0("cnt_", H), OC)
	ST <- rbindlist(lapply(EVT$ev, function(e) {
		pe <- unique(CEL[ev == e, pr])
		cbind(pr = pe, ev = e, status(M[pe, , drop = FALSE], e))
	}))[!is.na(T)]
	rm(M, DL)
	gc()
	for (jn in c("pos", "pre")) {
		C1 <- merge(CEL[janela == jn], ST, by = c("pr", "ev"))
		AM[[length(AM) + 1L]] <- C1[, .(pares = uniqueN(pr), vinculos = sum(n), ganho_medio = weighted.mean(ganho, n)), by = .(ev, g90, faixa, T)][, janela := jn]
		for (es in names(ESQ)) {
			C1[, grp := ESQ[[es]](C1)]
			C2 <- C1[!is.na(grp)]
			C2[, un := if (nivel == "h39") as.character(pr) else paste(PR$casa8[pr], PR$trab8[pr])]
			U <- C2[, c(lapply(.SD, sum), list(ihr = ih[which.max(n)])), by = .(un, ev, g90, T, faixa, grp), .SDcols = somas]
			U[, cel5 := hx$cel5[ihr]]
			for (o in OC) {
				h <- sub(".*_", "", o)
				den <- if (grepl("^(empregado|meses)_", o)) U$n else U[[paste0("cnt_", h)]]
				set(U, j = o, value = fifelse(den > 0, U[[o]] / den, NA_real_))
			}
			U[, w := n]
			U <- U[rowSums(!is.finite(as.matrix(U[, OC, with = FALSE]))) == 0L]
			for (gg in c(TRUE, FALSE, NA)) for (fx in c("todas", "f1a5", "f5a10", "f10mais")) {
				D <- if (is.na(gg)) U else U[g90 == gg]
				if (fx != "todas") D <- D[faixa == fx]
				if (!nrow(D) || sum(D$T == 1L) < 10L || sum(D$T == 0L) < 10L) next
				D <- copy(D)
				D[, eb := paste(ev, faixa)]
				estima(D, OC, data.table(parte = parte, nivel = nivel, trajeto = if (is.na(gg)) "todos" else if (gg) "até 90" else "acima de 90", janela = jn, faixa = fx, esquema = es))
			}
			rm(U, C2)
			gc()
		}
		msg("tempo de viagem ", nivel, " ", jn, " feito")
		grava()
	}
}
grava()
msg("fim futuros ", parte, " ", nivel)

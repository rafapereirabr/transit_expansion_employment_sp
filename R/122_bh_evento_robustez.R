# Especificação final, bloco E (24/11/2026; origem: explora_final_robustez.R): robustez no desenho por evento, a partir
# das bases por evento (R/118 e R/119), para todos,
# mulheres e mulheres com criança.
# Tempo de viagem (modelo A, trajeto até 90 / acima de 90; demissão, sem justa causa, pedido e permanência, h = 6–24):
#   R1 controles de outras aberturas (ganho de tempo de outros níveis com serviço pleno nos 12 meses antes da referência e
#      durante os 24 meses de acompanhamento, cada um recentrado);
#   R2 efeitos fixos H3-8 de casa × evento e de trabalho × evento no lugar de H3-7;
#   R3 retirando um evento por vez;
#   R6 não linearidade: faixas de ganho em minutos (0–5, 5–10, 10–20, 20 ou mais), faixas em % do tempo antes (abaixo de 1%
#      como zero, 1–10%, 10% ou mais), quintis do ganho realizado positivo (indicadoras recentradas pela fração dos
#      sorteios em que o par cairia na faixa), interação com o tempo antes (centrado) e dose percentual (por 10% do tempo);
#   R8 dose única, sem dividir em até 90 e acima de 90 min: ganho em minutos (por 10 min) e ganho percentual (por 10% do
#      tempo antes), cada um numa regressão própria.
# Área (emprego e entrada, h = 6–24; cumulativa 60 min e gravitacional): R1 (ganho de outros eventos), R2 (efeito fixo
#   H3-8 × evento), R3, e faixas do ganho em % (1–5, 5–20, 20 ou mais; abaixo de 1% como zero).
# O R7 (pares com tempo zero) roda com R/121 tmin=-1; o R4 (curva mês a mês) e o R5 (endereço-base g−1 a g−4) pedem
# bases próprias e rodam com R/118–R/121 (argumentos base=, lag= e horizontes=; ver R/pipeline_final.R).
# Uso: Rscript R/122_bh_evento_robustez.R parte=par|area [perm=199]
# Saída: outputs/final/robustez_<parte>.csv
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
parte <- arg_cli("parte", NA, "par")
n_perm <- as.integer(arg_cli("perm", NA, 199L))
dir.create("outputs/final", showWarnings = FALSE)
H <- c(6L, 12L, 18L, 24L)
mes_idx <- function(d) (year(d) - 2011L) * 12L + month(d)
niv <- le_niveis()
Dd <- le_sorteios()[, 1:200]
MD <- matrix(mes_idx(as.IDate(as.vector(Dd))), nrow(Dd))
hx <- le_hex()
ids <- le_ganhos(60L)$id
hx <- hx[match(ids, hx$id)]
p8 <- h3jsr::get_parent(ids, res = 8)
dd <- "data/base/explora/ev_cache"
RES <- list()
grava <- function() fwrite(rbindlist(RES, fill = TRUE), sprintf("outputs/final/robustez_%s.csv", parte))
reg <- function(Y, w, Gl, ev, cl, fe, nomes, rot, seg, jn, extra = list()) {
	r <- tryCatch(bh_lista2(Y, w, Gl, ev, cl, fe, n_perm = n_perm), error = function(err) { msg("erro ", rot, " ", seg, " ", jn, ": ", conditionMessage(err)); NULL })
	if (is.null(r)) return(invisible(NULL))
	K <- length(Gl)
	for (o in seq_len(ncol(Y))) for (j in seq_len(K)) {
		if (is.na(nomes[j])) next
		z <- comb_bh2(r, o, as.numeric(seq_len(K) == j))
		esc <- if (grepl("^salario", colnames(Y)[o])) 1 else 100
		RES[[length(RES) + 1L]] <<- data.table(teste = rot, segmento = seg, janela = jn, outcome = colnames(Y)[o], termo = nomes[j],
			efeito = esc * z[["b"]], ep = esc * z[["se"]], p_perm = z[["p_perm"]], p_normal = z[["p_normal"]], unidades = sum(r$ok), as.data.table(extra))
	}
	invisible(r)
}
SEG3 <- list(todos = function(d) rep(TRUE, nrow(d)), mulheres = function(d) d$sx %in% 2L, "mulheres com criança" = function(d) d$sx %in% 2L & d$cr %in% 1L)

if (parte == "par") {
	EVT <- fread(file.path(dd, "eventos.csv"))
	PR <- as.data.table(arrow::read_parquet(file.path(dd, "par_pares.parquet")))
	DL <- as.matrix(PR[, niv$nivel, with = FALSE])
	CEL <- as.data.table(arrow::read_parquet(file.path(dd, "par_celulas.parquet")))
	CEL <- CEL[t_antes > 0 & t_antes <= 241]
	CEL[, pr := match(ip, PR$ip)]
	OC <- c(as.vector(outer(c("demissao", "sem_justa", "pedido"), H, paste, sep = "_")), paste0("empregado_", H))
	for (jn in c("pos", "pre")) for (sg in names(SEG3)) {
		C1 <- CEL[janela == jn][SEG3[[sg]](CEL[janela == jn])]
		U <- C1[, lapply(.SD, sum), by = .(pr, ih, evento, t_antes), .SDcols = c("n", paste0("cnt_", H), OC)][order(evento, pr)]
		Y <- sapply(OC, function(o) {
			h <- sub(".*_", "", o)
			den <- if (grepl("^empregado_", o)) U$n else U[[paste0("cnt_", h)]]
			fifelse(den > 0, U[[o]] / den, NA_real_)
		})
		w <- U$n
		g90 <- U$t_antes <= 90
		cl <- hx$cel5[U$ih]
		fe7 <- list(fc = paste(hx$h3_7[U$ih], U$evento), ft = paste(PR$trab7[U$pr], U$evento))
		fe8 <- list(fc = paste(p8[U$ih], U$evento), ft = paste(PR$trab8[U$pr], U$evento))
		G <- matrix(0, nrow(U), ncol(Dd))
		Cpre <- matrix(0, nrow(U), ncol(Dd))
		Cpos <- matrix(0, nrow(U), ncol(Dd))
		for (e in unique(U$evento)) {
			ix <- which(U$evento == e)
			E <- EVT[coorte == e]
			m0 <- if (jn == "pos") E$m_e else E$ref_pre + 1L
			X <- DL[U$pr[ix], , drop = FALSE]
			G[ix, ] <- X %*% ((Dd == E$d_e) * 1)
			Cpre[ix, ] <- X %*% (((MD >= m0 - 12L & MD <= m0 - 1L) & Dd != E$d_e) * 1) / 10
			Cpos[ix, ] <- X %*% (((MD >= m0 & MD <= m0 + 23L) & Dd != E$d_e) * 1) / 10
		}
		Dm <- G / 10
		nA <- c("trajeto, até 90", "trajeto, acima de 90")
		reg(Y, w, list(Dm * g90, Dm * (!g90)), U$evento, cl, fe7, nA, "principal (modelo A)", sg, jn)
		reg(Y, w, list(Dm * g90, Dm * (!g90), Cpre, Cpos), U$evento, cl, fe7, c(nA, "outras aberturas, 12 meses antes", "outras aberturas, acompanhamento"),
			"R1 outras aberturas", sg, jn)
		reg(Y, w, list(Dm * g90, Dm * (!g90)), U$evento, cl, fe8, nA, "R2 efeitos fixos H3-8", sg, jn)
		rm(Cpre, Cpos)
		if (sg == "todos" || jn == "pos") for (e in sort(unique(U$evento))) {
			s <- U$evento != e
			reg(Y[s, , drop = FALSE], w[s], list((Dm * g90)[s, ], (Dm * (!g90))[s, ]), U$evento[s], cl[s], lapply(fe7, function(f) f[s]), nA,
				"R3 sem um evento", sg, jn, list(evento_fora = e))
		}
		## R6 não linearidade (só trajeto até 90, onde está o efeito; o termo acima de 90 entra como controle linear)
		ta <- U$t_antes
		pct <- 100 * G / pmax(ta, 1)
		ind <- function(M, a, b) (M >= a & M < b) * 1
		fx_min <- list("0 a 5 min" = c(0.001, 5), "5 a 10 min" = c(5, 10), "10 a 20 min" = c(10, 20), "20 min ou mais" = c(20, Inf))
		Gl <- c(lapply(fx_min, function(f) ind(G, f[1], f[2]) * g90), list(Dm * (!g90)))
		reg(Y, w, Gl, U$evento, cl, fe7, c(paste("até 90, faixa", names(fx_min)), "acima de 90 (linear)"), "R6 faixas de ganho em minutos", sg, jn)
		fx_pct <- list("1 a 10%" = c(1, 10), "10% ou mais" = c(10, Inf))
		Gl <- c(lapply(fx_pct, function(f) ind(pct, f[1], f[2]) * g90), list(Dm * (!g90)))
		reg(Y, w, Gl, U$evento, cl, fe7, c(paste("até 90, faixa", names(fx_pct)), "acima de 90 (linear)"), "R6 faixas de ganho em % (abaixo de 1% = zero)", sg, jn)
		qq <- quantile(G[G[, 1] > 0 & g90, 1], c(0.2, 0.4, 0.6, 0.8))
		cortes_q <- c(0.001, qq, Inf)
		Gl <- c(lapply(1:5, function(i) ind(G, cortes_q[i], cortes_q[i + 1]) * g90), list(Dm * (!g90)))
		reg(Y, w, Gl, U$evento, cl, fe7, c(sprintf("até 90, quintil %d (%.1f a %.1f min)", 1:5, cortes_q[1:5], cortes_q[2:6]), "acima de 90 (linear)"),
			"R6 quintis do ganho", sg, jn)
		tc <- (ta - weighted.mean(ta[g90], w[g90])) / 10
		reg(Y, w, list(Dm * g90, Dm * g90 * tc, Dm * (!g90)), U$evento, cl, fe7, c("até 90", "até 90 × (tempo antes − média)/10", "acima de 90"),
			"R6 interação com o tempo antes", sg, jn)
		reg(Y, w, list(pct / 10 * g90, pct / 10 * (!g90)), U$evento, cl, fe7, c("até 90, por 10% do tempo", "acima de 90, por 10% do tempo"),
			"R6 dose percentual", sg, jn)
		## R8 (pedido do pesquisador, 24/11/2026): dose única, sem dividir em até 90 e acima de 90 min, em minutos e em %
		reg(Y, w, list(Dm), U$evento, cl, fe7, "dose única, por 10 min", "R8 dose única em minutos (sem divisão em 90 min)", sg, jn)
		reg(Y, w, list(pct / 10), U$evento, cl, fe7, "dose única, por 10% do tempo", "R8 dose única percentual (sem divisão em 90 min)", sg, jn)
		rm(G, Dm, pct, Gl)
		gc()
		msg("par ", jn, " ", sg, " feito")
		grava()
	}
}

if (parte == "area") {
	EVT <- fread(file.path(dd, "eventos_area.csv"))
	CEL <- as.data.table(arrow::read_parquet(file.path(dd, "area_celulas.parquet")))
	g60 <- le_ganhos(60L)
	GM <- list(c60 = as.matrix(g60[match(ids, id), niv$nivel, with = FALSE]))
	gcg <- le_ganhos("gcal")
	Gacc <- as.matrix(gcg[match(ids, id), niv$nivel, with = FALSE])
	A00g <- gcg$acc_A00[match(ids, gcg$id)]
	A0060 <- g60$acc_A00[match(ids, g60$id)]
	cad <- niveis_cadeia(niv)
	rm(g60, gcg)
	pct_ev <- function(tp, ih, on, antes) {
		Gm <- if (tp == "grav") Gacc else GM$c60
		A0 <- if (tp == "grav") A00g else A0060
		base <- A0[ih] - rowSums(Gm[ih, cad, drop = FALSE]) + Gm[ih, , drop = FALSE] %*% antes
		100 * (Gm[ih, , drop = FALSE] %*% on) / pmax(base, 1000)
	}
	for (jn in c("pos", "pre")) for (sg in names(SEG3)) {
		C1 <- CEL[janela == jn][SEG3[[sg]](CEL[janela == jn])]
		somas <- setdiff(names(C1), c("ih", "sx", "id3", "cr", "janela", "ev"))
		U <- C1[, lapply(.SD, sum), by = .(ih, ev), .SDcols = somas][order(ev, ih)]
		cl <- hx$cel5[U$ih]
		fe7 <- list(fc = paste(hx$h3_7[U$ih], U$ev))
		fe8 <- list(fc = paste(p8[U$ih], U$ev))
		for (tp in c("c60", "grav")) {
			D <- matrix(0, nrow(U), ncol(Dd))
			O <- matrix(0, nrow(U), ncol(Dd))
			P <- matrix(0, nrow(U), ncol(Dd))
			for (e in unique(U$ev)) {
				ix <- which(U$ev == e)
				E <- EVT[ev == e]
				m0 <- if (jn == "pos") E$m_e else E$ref_pre + 1L
				on <- (Dd == E$d_e) * 1
				antes <- (Dd < E$d_e) * 1
				oo <- ((MD >= m0 - 12L & MD <= m0 + 23L) & Dd != E$d_e) * 1
				if (tp == "grav") {
					base <- A00g[U$ih[ix]] - rowSums(Gacc[U$ih[ix], cad, drop = FALSE]) + Gacc[U$ih[ix], , drop = FALSE] %*% antes
					D[ix, ] <- 10 * log1p(pmin((Gacc[U$ih[ix], , drop = FALSE] %*% on) / pmax(base, 1000), 1))
					O[ix, ] <- 10 * log1p(pmin((Gacc[U$ih[ix], , drop = FALSE] %*% oo) / pmax(base, 1000), 1))
				} else {
					D[ix, ] <- (GM$c60[U$ih[ix], , drop = FALSE] %*% on) / 1e5
					O[ix, ] <- (GM$c60[U$ih[ix], , drop = FALSE] %*% oo) / 1e5
				}
				P[ix, ] <- pct_ev(tp, U$ih[ix], on, antes)
			}
			D[is.na(D)] <- 0
			O[is.na(O)] <- 0
			P[is.na(P)] <- 0
			for (cj in c("emprego", "entrada")) {
				oc <- intersect(paste0(cj, "_", H), names(U))
				wv <- if (cj == "emprego") U$n_todos else U$n_sem
				Y <- as.matrix(U[, oc, with = FALSE]) / wv
				ex <- list(dose = tp)
				reg(Y, wv, list(D), U$ev, cl, fe7, "dose", "principal", sg, jn, ex)
				reg(Y, wv, list(D, O), U$ev, cl, fe7, c("dose", "outras aberturas (12 meses antes e acompanhamento)"), "R1 outras aberturas", sg, jn, ex)
				reg(Y, wv, list(D), U$ev, cl, fe8, "dose", "R2 efeito fixo H3-8", sg, jn, ex)
				if (sg == "todos" || jn == "pos") for (e in sort(unique(U$ev))) {
					s <- U$ev != e
					reg(Y[s, , drop = FALSE], wv[s], list(D[s, , drop = FALSE]), U$ev[s], cl[s], lapply(fe7, function(f) f[s]), "dose", "R3 sem um evento", sg, jn,
						c(ex, list(evento_fora = e)))
				}
				fx <- list("1 a 5%" = c(1, 5), "5 a 20%" = c(5, 20), "20% ou mais" = c(20, Inf))
				reg(Y, wv, lapply(fx, function(f) (P >= f[1] & P < f[2]) * 1), U$ev, cl, fe7, paste("faixa", names(fx)), "R6 faixas de ganho em % (abaixo de 1% = zero)",
					sg, jn, ex)
			}
			rm(D, O, P)
			gc()
		}
		msg("área ", jn, " ", sg, " feito")
		grava()
	}
}
grava()
msg("fim robustez ", parte)

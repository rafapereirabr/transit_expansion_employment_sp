# Especificação final, blocos B e C (24/11/2026): tempo de viagem ao próprio emprego e estabilidade, permanência e destino
# de quem pediu demissão, no desenho por evento (data exata de serviço pleno), a partir da base em cache
# por evento (R/119_base_evento_par.R). Origem: explora_final_par.R.
# - Unidade: nivel=h39 (par casa H3-9 × trabalho H3-9 × evento, pesos = vínculos; igual ao painel individual exceto pelos
#   vínculos censurados, que ficam no peso da célula) ou nivel=h38 (par casa H3-8 × trabalho H3-8 × grupo de trajeto
#   (até 90 / acima de 90) × evento; doses = médias ponderadas por vínculos em cada sorteio).
# - Doses dos níveis que recebem a data do evento em cada sorteio: ganho de tempo casa → trabalho (por 10 min) e ganho de
#   acessibilidade gravitacional calibrada a partir de casa (por +0,1 no log), divididos em trajeto antes até 90 e acima
#   de 90 min. Modelo A: trajeto. Modelo B: trajeto e acesso. Recentradas pela média nos sorteios, média × evento como
#   controle; efeitos fixos de célula H3-7 de casa × evento e de trabalho × evento; EP por célula de 5 km; p por
#   permutação (199 sorteios). Amostra principal: trajeto antes de 0 a 241 min, exclusive 0 (tmin=0).
# - Desfechos (h = 6, 12, 18, 24): qualquer saída, demissão, sem justa causa, justa causa, pedido; empregado em ref+h e
#   fração dos meses; destinos de quem pediu até h (trajeto mais curto / não mais curto / sem local / sem novo vínculo
#   formal em 12 meses; salário real maior que 1,05 × o antigo / não maior; novo vínculo em até 3 meses / de 4 a 12).
# - Descrição (bloco C): ganho realizado e recentrado dos dois tratamentos entre quem pediu demissão até h, por destino,
#   e quem não pediu.
# - Segmentos (seg= nomes separados por ";"); contrastes calculados entre os segmentos do mesmo processo.
# Efeito fixo (fe=): casa7trab7 (principal), casa7, casa8trab8 ou ev (só evento). tmin=-1 inclui os pares com tempo zero.
# Robustez: base= lê a base com esse sufixo (R5: base=_lag1...; R4: base=_mensal com horizontes=1,...,24); desfechos=
# restringe os desfechos (por exemplo demissao,pedido) e, nesse caso, a descrição do bloco C não é gravada.
# Uso: Rscript R/121_bh_evento_par.R nivel=h39|h38 seg="1;2;3" [modelos=A,B] [perm=199] [tmin=0] [fe=casa7trab7] [sufixo=]
#      [base=] [horizontes=6,12,18,24] [desfechos=]
# Saídas: outputs/final/par_<nivel><sufixo>_segmentos.csv, _contrastes.csv e _descricao.csv
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
segs <- strsplit(arg_cli("seg", NA, "todos"), ";")[[1]]
modelos <- strsplit(arg_cli("modelos", NA, "A,B"), ",")[[1]]
n_perm <- as.integer(arg_cli("perm", NA, 199L))
tmin <- as.numeric(arg_cli("tmin", NA, 0))
suf_x <- arg_cli("sufixo", NA, "")
## efeito fixo: casa7trab7 (principal), casa7, casa8trab8 (só com nivel=h39) ou ev (só evento)
fe_tipo <- arg_cli("fe", NA, "casa7trab7")
stopifnot(fe_tipo %in% c("casa7trab7", "casa7", "casa8trab8", "ev"), !(nivel == "h38" && fe_tipo == "casa8trab8"))
dir.create("outputs/final", showWarnings = FALSE)
H <- as.integer(strsplit(arg_cli("horizontes", NA, "6,12,18,24"), ",")[[1]])
suf_base <- arg_cli("base", NA, "")
desf <- arg_cli("desfechos", NA, "")
niv <- le_niveis()
Dd <- le_sorteios()[, 1:200]
hx <- le_hex()
ids <- le_ganhos(60L)$id
hx <- hx[match(ids, hx$id)]
p8_ids <- h3jsr::get_parent(ids, res = 8)
gcg <- le_ganhos("gcal")
Gacc <- as.matrix(gcg[match(ids, id), niv$nivel, with = FALSE])
A00 <- gcg$acc_A00[match(ids, gcg$id)]
cad <- niveis_cadeia(niv)
rm(gcg)
dd <- "data/base/explora/ev_cache"
EVT <- fread(file.path(dd, sprintf("eventos%s.csv", suf_base)))
PR <- as.data.table(arrow::read_parquet(file.path(dd, sprintf("par_pares%s.parquet", suf_base))))
DL <- as.matrix(PR[, niv$nivel, with = FALSE])
CEL <- as.data.table(arrow::read_parquet(file.path(dd, sprintf("par_celulas%s.parquet", suf_base))))
CEL <- CEL[t_antes > tmin & t_antes <= 241]
CEL[, `:=`(pr = match(ip, PR$ip), g90 = t_antes <= 90)]
msg("células: ", nrow(CEL), "; vínculos: ", sum(CEL$n))
base_oc <- c("qualquer", "demissao", "sem_justa", "justa", "pedido")
dest_oc <- c("ped_curto", "ped_naocurto", "ped_semloc", "ped_semnovo", "ped_salmaior", "ped_salnaomaior", "ped_novo3", "ped_novo4a12")
OC <- c(as.vector(outer(c(base_oc, dest_oc), H, paste, sep = "_")), as.vector(outer(c("empregado", "meses"), H, paste, sep = "_")))
## desfechos= restringe os desfechos (robustez); sem a lista completa, a descrição do bloco C não é feita
if (nzchar(desf)) OC <- OC[sub("_[0-9]+$", "", OC) %in% strsplit(desf, ",")[[1]]]
faz_desc <- !nzchar(desf)
somas <- c("n", paste0("cnt_", H), OC)

## unidade
if (nivel == "h39") {
	CEL[, un := paste(ip, g90)]
} else {
	CEL[, un := paste(PR$casa8[pr], PR$trab8[pr], g90)]
}
SEG <- list(todos = function(d) rep(TRUE, nrow(d)), mulheres = function(d) d$sx %in% 2L, homens = function(d) d$sx %in% 1L,
	"18 a 29" = function(d) d$id3 %in% 1L, "30 a 44" = function(d) d$id3 %in% 2L, "45 a 60" = function(d) d$id3 %in% 3L,
	"com criança" = function(d) d$cr %in% 1L, "sem criança" = function(d) d$cr %in% 0L,
	"mulheres com criança" = function(d) d$sx %in% 2L & d$cr %in% 1L, "mulheres sem criança" = function(d) d$sx %in% 2L & d$cr %in% 0L,
	"homens com criança" = function(d) d$sx %in% 1L & d$cr %in% 1L, "homens sem criança" = function(d) d$sx %in% 1L & d$cr %in% 0L,
	"vínculo até 12 meses" = function(d) d$tn2 %in% 1L, "vínculo mais de 12 meses" = function(d) d$tn2 %in% 2L)
## seg= por número (acentos em argumentos se corrompem no Windows): 1 todos, 2 mulheres, 3 homens, 4 18–29, 5 30–44,
## 6 45–60, 7 com criança, 8 sem criança, 9 mulheres com, 10 mulheres sem, 11 homens com, 12 homens sem criança,
## 13 vínculo até 12 meses, 14 mais de 12 meses
if (all(grepl("^[0-9]+$", segs))) segs <- names(SEG)[as.integer(segs)]
stopifnot(all(segs %in% names(SEG)))

## doses de cada célula (par H3-9) por sorteio
doses_cel <- function(pr, ih, ev, ta = NULL) {
	Dm <- matrix(0, length(pr), ncol(Dd))
	Ac <- matrix(0, length(pr), ncol(Dd))
	Dp <- if (!is.null(ta)) matrix(0, length(pr), ncol(Dd)) else NULL
	for (e in unique(ev)) {
		ix <- which(ev == e)
		E <- EVT[coorte == e]
		on <- (Dd == E$d_e) * 1
		G <- DL[pr[ix], , drop = FALSE] %*% on
		Dm[ix, ] <- G / 10
		## dose percentual (modelo P): ganho em % do tempo de trajeto antes do evento, por 10% (24/11/2026)
		if (!is.null(ta)) Dp[ix, ] <- 100 * G / pmax(ta[ix], 1) / 10
		ganho <- Gacc[ih[ix], , drop = FALSE] %*% on
		base <- A00[ih[ix]] - rowSums(Gacc[ih[ix], cad, drop = FALSE]) + Gacc[ih[ix], , drop = FALSE] %*% ((Dd < E$d_e) * 1)
		Ac[ix, ] <- 10 * log1p(pmin(ganho / pmax(base, 1000), 1))
	}
	Dm[is.na(Dm)] <- 0
	Ac[is.na(Ac)] <- 0
	if (!is.null(Dp)) Dp[is.na(Dp)] <- 0
	list(Dm = Dm, Ac = Ac, Dp = Dp)
}
## modelos (24/11/2026): A = trajeto dividido em até 90 e acima de 90 min; B = A + acesso; U = trajeto sem divisão (por 10
## min); P = trajeto sem divisão em % do tempo antes (por 10%); UB = U + acesso sem divisão (B sem a divisão em 90 min)
NOMES_MOD <- list(A = c("trajeto, até 90", "trajeto, acima de 90"), B = c("trajeto, até 90", "trajeto, acima de 90", "acesso, até 90", "acesso, acima de 90"),
	U = "trajeto, sem divisão (por 10 min)", P = "trajeto, sem divisão (por 10% do tempo)", UB = c("trajeto, sem divisão (por 10 min)", "acesso, sem divisão"))
stopifnot(all(modelos %in% names(NOMES_MOD)))

RES <- list()
DESC <- list()
RR <- list()
for (jn in c("pos", "pre")) for (sg in segs) {
	C1 <- CEL[janela == jn][SEG[[sg]](CEL[janela == jn])]
	if (!nrow(C1)) next
	## células H3-9 do segmento (para as doses) e unidades
	P9 <- C1[, c(list(n = sum(n)), list(ih = ih[1], g90 = g90[1], un = un[1], ta = t_antes[1])), by = .(pr, evento)]
	U <- C1[, lapply(.SD, sum), by = .(un, evento), .SDcols = somas][order(evento, un)]
	U[, g90 := grepl("TRUE$", un)]
	ref <- P9[, .SD[which.max(n)], by = .(un, evento)][match(paste(U$un, U$evento), paste(un, evento))]
	d9 <- doses_cel(P9$pr, P9$ih, P9$evento, if ("P" %in% modelos) P9$ta else NULL)
	key <- paste(P9$un, P9$evento)
	agrega <- function(M) {
		if (nivel == "h39") return(M[match(paste(U$un, U$evento), key), , drop = FALSE])
		A <- rowsum(M * P9$n, key) / as.vector(rowsum(P9$n, key))
		A[match(paste(U$un, U$evento), rownames(A)), , drop = FALSE]
	}
	Dm <- agrega(d9$Dm)
	Ac <- agrega(d9$Ac)
	Dp <- if (!is.null(d9$Dp)) agrega(d9$Dp) else NULL
	rm(d9)
	gc()
	## desfechos: médias por unidade (desligamentos e destinos sobre os vínculos observados em h; permanência sobre todos)
	Y <- sapply(OC, function(o) {
		h <- sub(".*_", "", o)
		den <- if (grepl("^(empregado|meses)_", o)) U$n else U[[paste0("cnt_", h)]]
		fifelse(den > 0, U[[o]] / den, NA_real_)
	})
	w <- U$n
	g90 <- U$g90
	fe <- switch(fe_tipo, casa7trab7 = list(fc = paste(hx$h3_7[ref$ih], U$evento), ft = paste(PR$trab7[ref$pr], U$evento)),
		casa7 = list(fc = paste(hx$h3_7[ref$ih], U$evento)), ev = list(ev = U$evento),
		casa8trab8 = list(fc = paste(p8_ids[ref$ih], U$evento), ft = paste(PR$trab8[ref$pr], U$evento)))
	cl <- hx$cel5[ref$ih]
	## descrição dos ganhos por destino (bloco C)
	rec_m <- Dm[, 1] - rowMeans(Dm[, -1, drop = FALSE])
	rec_a <- Ac[, 1] - rowMeans(Ac[, -1, drop = FALSE])
	if (faz_desc) for (h in H) for (gg in c(TRUE, FALSE, NA)) for (o in c("pedido", "nao_pediu", dest_oc)) {
		y <- if (o == "nao_pediu") 1 - Y[, paste0("pedido_", h)] else Y[, paste0(o, "_", h)]
		ww <- w * y
		s <- (if (is.na(gg)) TRUE else g90 == gg) & is.finite(ww)
		DESC[[length(DESC) + 1L]] <- data.table(nivel = nivel, janela = jn, segmento = sg, h = h, trajeto = if (is.na(gg)) "todos" else if (gg) "até 90" else "acima de 90", grupo = o,
			vinculos = sum(ww[s]), traj_realizado_min = 10 * weighted.mean(Dm[s, 1], ww[s]), traj_recentrado_min = 10 * weighted.mean(rec_m[s], ww[s]),
			acesso_realizado_log = weighted.mean(Ac[s, 1], ww[s]) / 10, acesso_recentrado_log = weighted.mean(rec_a[s], ww[s]) / 10)
	}
	## memória (24/11/2026): tira antes as linhas sem desfecho (assim o estimador não copia as matrizes de dose) e libera
	## Dm e Ac depois de montar as matrizes do último modelo
	okr <- rowSums(!is.finite(Y)) == 0L & is.finite(w) & w > 0
	if (!all(okr)) {
		Y <- Y[okr, , drop = FALSE]
		w <- w[okr]
		g90 <- g90[okr]
		U <- U[okr]
		ref <- ref[okr]
		Dm <- Dm[okr, , drop = FALSE]
		Ac <- Ac[okr, , drop = FALSE]
		if (!is.null(Dp)) Dp <- Dp[okr, , drop = FALSE]
		fe <- lapply(fe, function(f) f[okr])
		cl <- cl[okr]
		gc()
	}
	for (md in modelos) {
		Gl <- switch(md, A = list(Dm * g90, Dm * (!g90)), B = list(Dm * g90, Dm * (!g90), Ac * g90, Ac * (!g90)), U = list(Dm), P = list(Dp), UB = list(Dm, Ac))
		if (identical(md, modelos[length(modelos)])) {
			rm(list = intersect(c("Dm", "Ac", "Dp"), ls()))
			gc()
		}
		nomes <- NOMES_MOD[[md]]
		r <- tryCatch(bh_lista2(Y, w, Gl, U$evento, cl, fe, n_perm = n_perm),
			error = function(err) { msg("erro ", jn, " ", sg, " ", md, ": ", conditionMessage(err)); NULL })
		rm(Gl)
		if (is.null(r)) next
		ok <- r$ok
		K <- length(nomes)
		for (o in seq_along(OC)) {
			for (j in seq_len(K)) {
				z <- comb_bh2(r, o, as.numeric(seq_len(K) == j))
				s <- if (md %in% c("U", "P", "UB")) ok else ok & (if (j %% 2 == 1) g90 else !g90)
				RES[[length(RES) + 1L]] <- data.table(nivel = nivel, fe = fe_tipo, janela = jn, segmento = sg, modelo = md, outcome = OC[o], termo = nomes[j],
					efeito = 100 * z[["b"]], ep = 100 * z[["se"]], p_perm = z[["p_perm"]], p_normal = z[["p_normal"]],
					media_y = 100 * weighted.mean(Y[s, o], w[s]), vinculos = sum(w[s]), unidades = sum(s), perm = dim(r$B)[1] - 1L)
			}
			if (K >= 2L && md %in% c("A", "B")) {
				z <- comb_bh2(r, o, c(1, -1, rep(0, K - 2L)))
				RES[[length(RES) + 1L]] <- data.table(nivel = nivel, janela = jn, segmento = sg, modelo = md, outcome = OC[o], termo = "trajeto: até 90 − acima de 90",
					efeito = 100 * z[["b"]], ep = 100 * z[["se"]], p_perm = z[["p_perm"]], p_normal = z[["p_normal"]], perm = dim(r$B)[1] - 1L)
			}
		}
		RR[[paste(jn, sg, md)]] <- list(B = r$B, SC = r$SC, INV = r$INV, K = K)
		msg(nivel, " ", jn, " ", sg, " modelo ", md, ": demissão 6 = ", paste(round(100 * r$B[1, , match("demissao_6", OC)], 2), collapse = " | "),
			"; ", sum(ok), " unidades")
		rm(r)
		gc()
	}
	rm(list = intersect(c("Dm", "Ac", "Dp", "Y", "U", "P9", "C1"), ls()))
	gc()
	fwrite(rbindlist(RES, fill = TRUE), sprintf("outputs/final/par_%s%s_segmentos.csv", nivel, suf_x))
	if (faz_desc) fwrite(rbindlist(DESC), sprintf("outputs/final/par_%s%s_descricao.csv", nivel, suf_x))
}

## contrastes entre os segmentos deste processo ----------------------------------------------------------------------------
PARES <- list(c("mulheres", "homens"), c("30 a 44", "18 a 29"), c("45 a 60", "18 a 29"), c("com criança", "sem criança"),
	c("mulheres com criança", "mulheres sem criança"), c("homens com criança", "homens sem criança"), c("vínculo até 12 meses", "vínculo mais de 12 meses"))
CT <- list()
for (jn in c("pos", "pre")) for (md in modelos) {
	pega <- function(sg) RR[[paste(jn, sg, md)]]
	for (o in seq_along(OC)) {
		termos <- NOMES_MOD[[md]]
		K <- length(termos)
		for (j in seq_len(K)) {
			cc <- as.numeric(seq_len(K) == j)
			add <- function(rot, z) CT[[length(CT) + 1L]] <<- data.table(nivel = nivel, janela = jn, modelo = md, outcome = OC[o], termo = termos[j],
				contraste = rot, diferenca = 100 * z[["b"]], ep = 100 * z[["se"]], p_normal = z[["p_normal"]], p_perm = z[["p_perm"]])
			for (pp in PARES) {
				a <- pega(pp[1])
				b <- pega(pp[2])
				if (is.null(a) || is.null(b)) next
				add(paste(pp[1], "−", pp[2]), dif_bh2(a, o, cc, b, o, cc))
			}
			g4 <- lapply(c("mulheres com criança", "mulheres sem criança", "homens com criança", "homens sem criança"), pega)
			if (!any(sapply(g4, is.null))) {
				sg4 <- c(1, -1, -1, 1)
				b <- sum(sapply(1:4, function(g) sg4[g] * sum(cc * g4[[g]]$B[1, , o])))
				P <- min(sapply(g4, function(r) dim(r$B)[1]))
				dr <- Reduce(`+`, lapply(1:4, function(g) sg4[g] * as.vector(matrix(g4[[g]]$B[2:P, , o], ncol = K) %*% cc)))
				A <- lapply(1:4, function(g) g4[[g]]$SC[[o]] %*% (g4[[g]]$INV %*% cc))
				cls <- unique(unlist(lapply(A, rownames)))
				s <- setNames(rep(0, length(cls)), cls)
				for (g in 1:4) s[rownames(A[[g]])] <- s[rownames(A[[g]])] + sg4[g] * A[[g]][, 1]
				se <- sqrt(sum(s^2))
				add("(mulheres com − sem criança) − (homens com − sem criança)", c(b = b, se = se, p_normal = 2 * pnorm(-abs(b / se)), p_perm = mean(abs(dr) >= abs(b))))
			}
			g3 <- lapply(c("18 a 29", "30 a 44", "45 a 60"), pega)
			if (!any(sapply(g3, is.null))) {
				wz <- wald_grupos2(g3, rep(o, 3), list(cc, cc, cc))
				CT[[length(CT) + 1L]] <- data.table(nivel = nivel, janela = jn, modelo = md, outcome = OC[o], termo = termos[j],
					contraste = "idade: teste conjunto de igualdade (3 grupos)", wald = wz[["wald"]], gl = wz[["gl"]], p_normal = wz[["p"]])
			}
		}
	}
}
if (length(CT)) fwrite(rbindlist(CT, fill = TRUE), sprintf("outputs/final/par_%s%s_contrastes.csv", nivel, suf_x))
msg("fim final par ", nivel, " ", paste(segs, collapse = ";"))

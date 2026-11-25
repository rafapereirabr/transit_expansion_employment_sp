# Funções comuns aos desenhos do plano de 17/11/2026 (R/91 a R/94). Carregar depois do 00_config.R.
# Insumos: R/89 (pessoa_ano, pop_base) e R/90 (datas: níveis, sorteios, ganhos, metadados dos hexágonos).

dir_datas <- file.path(dir_base, "datas")
coortes_plano <- c(2014L, 2015L, 2017L, 2018L, 2019L, 2020L, 2022L)

le_niveis <- function() {
	n <- fread(file.path(dir_datas, "niveis.csv"))
	n[, `:=`(abertura = as.IDate(abertura), pleno = as.IDate(pleno))]
	n[]
}
le_sorteios <- function() as.matrix(arrow::read_parquet(file.path(dir_datas, "sorteios_pleno.parquet")))
## coorte anual de datas inteiras (vetor ou matriz): primeiro ano com pelo menos 6 meses de serviço pleno
coorte_data <- function(d) {
	dd <- as.IDate(as.vector(d))
	r <- year(dd) + as.integer(month(dd) > 6L)
	if (is.matrix(d)) matrix(r, nrow(d)) else r
}
## corte: 30, 45, 60, 90 (acessibilidade acumulada) ou "gcal" (gravitacional calibrada, R/113 decaimento=calibrado)
le_ganhos <- function(corte) as.data.table(arrow::read_parquet(file.path(dir_datas, sprintf("ganhos_c%s.parquet", corte))))
## níveis da cadeia A23 → A00 (os níveis A24–A26 são linhas futuras somadas à rede de 2025 e não fazem parte da A00)
niveis_cadeia <- function(niv) niv$sem != "A00"
le_hex <- function() as.data.table(arrow::read_parquet(file.path(dir_datas, "hex_meta.parquet")))

## população da coorte g com endereço-base g−k, idade_min a 60 anos em 1º/jan de g; ih = posição do hexágono em ids.
## k = 0 (19/11/2026, pedido do pesquisador): base única com o endereço mais recente observado entre g−4 e g−1 (cada pessoa
## uma vez; k_base guarda de qual ano veio o endereço)
pop_coorte <- function(k, g, idade_min = 18L, ids = NULL) {
	if (k == 0L) {
		L <- lapply(1:4, function(kk) {
			z <- pop_coorte(kk, g, idade_min, ids)
			if (!is.null(z)) z[, k_base := kk]
			z
		})
		x <- rbindlist(L, fill = TRUE)
		if (!nrow(x)) return(NULL)
		setorder(x, k_base)
		return(x[!duplicated(cpf_num)])
	}
	b <- g - k
	f <- file.path(dir_base, "pop_base", sprintf("pop_%d.parquet", b))
	if (b < 2011L || !file.exists(f)) return(NULL)
	x <- as.data.table(arrow::read_parquet(f))
	x <- x[nascimento > as.IDate(sprintf("%d-01-01", g - 61L)) & nascimento <= as.IDate(sprintf("%d-01-01", g - idade_min))]
	x[, idade := as.numeric(as.IDate(sprintf("%d-01-01", g)) - nascimento) / 365.25]
	if (!is.null(ids)) {
		x[, ih := match(h3_09, ids)]
		x <- x[!is.na(ih)]
	}
	x[]
}

## outcomes pessoa × ano do R/89, juntados em colunas <var>_<ano>; contagens e emprego sem vínculo = 0
PA <- new.env()
pa_ano <- function(a) {
	k <- as.character(a)
	if (is.null(PA[[k]])) PA[[k]] <- as.data.table(arrow::read_parquet(file.path(dir_base, "pessoa_ano", sprintf("pa_%d.parquet", a))))
	PA[[k]]
}
junta_pa <- function(x, anos, vars) {
	for (a in sort(unique(anos))) {
		if (a < 2011L || a > 2025L) next
		p <- pa_ano(a)
		ix <- match(x$cpf_num, p$cpf_num)
		for (v in vars) {
			val <- p[[v]][ix]
			if (!v %in% c("estab31", "lsalh")) val[is.na(val)] <- 0L
			set(x, j = paste0(v, "_", a), value = val)
		}
	}
	x
}
col_ou_na <- function(x, nm) if (nm %in% names(x)) x[[nm]] else rep(NA_real_, nrow(x))

## regressão do B&H com a permutação calculada de uma vez (Frisch-Waugh): y e n por linha, Zs = doses recentradas
## (coluna 1 = realizada, demais = sorteios), mu = dose esperada, coh = coorte da linha, cl = cluster
bh_fwl <- function(y, w, Zs, mu, coh, cl) {
	Xc <- if (length(unique(coh)) > 1L) model.matrix(~ 0 + factor(coh)) else matrix(1, length(y), 1L)
	Xo <- cbind(Xc, Xc * mu)
	inv <- tryCatch(solve(crossprod(Xo, Xo * w)), error = function(e) NULL)
	if (is.null(inv)) return(NULL)
	Pm <- inv %*% t(Xo * w)
	yt <- y - as.vector(Xo %*% (Pm %*% y))
	Zt <- Zs - Xo %*% (Pm %*% Zs)
	den <- colSums(Zt * w * Zt)
	b <- colSums(Zt * w * yt) / den
	u <- yt - b[1] * Zt[, 1]
	sc <- rowsum(Zt[, 1] * w * u, cl)
	list(b = b[1], se = sqrt(sum(sc^2)) / den[1], p = mean(abs(b[-1]) >= abs(b[1]), na.rm = TRUE))
}

## B&H com duas doses (18/11/2026, quebra por linha): ZL = dose recentrada da linha L e Zo = dose recentrada das demais
## linhas (coluna 1 = realizada, demais = sorteios), muL e muo as doses esperadas. Coortes empilhadas com intercepto e
## inclinações de muL e muo por coorte (ou efeitos fixos fe, que já contêm a coorte); o coeficiente da linha L sai da
## regressão com as duas doses, e o p por permutação refaz essa regressão com as duas doses de cada sorteio.
bh_linha <- function(y, w, ZL, Zo, muL, muo, coh, cl, fe = NULL) {
	ok <- is.finite(y)
	y <- y[ok]
	w <- w[ok]
	ZL <- ZL[ok, , drop = FALSE]
	Zo <- Zo[ok, , drop = FALSE]
	muL <- muL[ok]
	muo <- muo[ok]
	coh <- coh[ok]
	cl <- cl[ok]
	S1 <- ncol(ZL)
	Dc <- if (length(unique(coh)) > 1L) model.matrix(~ 0 + factor(coh)) else matrix(1, length(y), 1L)
	X <- cbind(Dc * muL, Dc * muo)
	if (is.null(fe)) {
		X <- cbind(Dc, X)
		M <- cbind(y, ZL, Zo)
	} else {
		fe <- lapply(fe, function(f) f[ok])
		M <- fixest::demean(cbind(y, X, ZL, Zo), f = fe, weights = w, notes = FALSE)
		px <- ncol(X)
		X <- M[, 1 + seq_len(px), drop = FALSE]
		M <- M[, -(1 + seq_len(px)), drop = FALSE]
	}
	sw <- sqrt(w)
	M <- qr.resid(qr(X * sw), M * sw) / sw
	yr <- M[, 1]
	A <- M[, 1 + seq_len(S1), drop = FALSE]
	C <- M[, 1 + S1 + seq_len(S1), drop = FALSE]
	rm(M)
	saa <- colSums(w * A * A)
	scc <- colSums(w * C * C)
	sac <- colSums(w * A * C)
	say <- colSums(w * A * yr)
	scy <- colSums(w * C * yr)
	det <- saa * scc - sac^2
	## sem variação nas demais linhas (scc = 0), a regressão fica só com a dose da linha L
	bL <- ifelse(scc > 0, (scc * say - sac * scy) / det, say / saa)
	if (!is.finite(bL[1]) || saa[1] <= 0) return(NULL)
	bC <- if (scc[1] > 0) (saa[1] * scy[1] - sac[1] * say[1]) / det[1] else 0
	at <- if (scc[1] > 0) A[, 1] - (sac[1] / scc[1]) * C[, 1] else A[, 1]
	u <- yr - bL[1] * A[, 1] - bC * C[, 1]
	sc <- rowsum(at * w * u, cl)
	## p condicional: em cada sorteio só a dose da linha L muda; a dose realizada das demais linhas fica como controle
	## (a permutação conjunta testa a hipótese de nenhuma linha ter efeito e fica larga quando as outras linhas importam)
	c1 <- C[, 1]
	sac1 <- colSums(w * A * c1)
	bLc <- if (scc[1] > 0) (scc[1] * say - sac1 * scy[1]) / (saa * scc[1] - sac1^2) else say / saa
	list(b = bL[1], se = sqrt(sum(sc^2)) / sum(w * at * at), p = mean(abs(bL[-1]) >= abs(bL[1]), na.rm = TRUE),
		p_cond = mean(abs(bLc[-1]) >= abs(bLc[1]), na.rm = TRUE), b_outras = bC)
}
## B&H com a dose dividida em dois grupos de pares (19/11/2026, pedido do pesquisador): G = dose de cada linha (par) em
## cada sorteio (coluna 1 = realizada), grupo = TRUE no 1º grupo (ex.: trajeto até 90 min antes da coorte). Uma regressão
## só, com as duas doses recentradas, μ de cada grupo com inclinação por coorte e os mesmos efeitos fixos. Devolve o efeito
## de cada grupo e a diferença (1º − 2º), com EP agrupado (sanduíche 2×2) e p por permutação: conjunto (as duas doses do
## mesmo sorteio; testa a hipótese nula de nenhum efeito em nenhum par) e condicional (só a dose do grupo em questão
## sorteada, a do outro grupo fica a realizada).
bh_dividida <- function(y, w, G, grupo, coh, cl, fe) {
	ok <- is.finite(y)
	y <- y[ok]
	w <- w[ok]
	G <- G[ok, , drop = FALSE]
	g1 <- as.numeric(grupo[ok])
	coh <- coh[ok]
	cl <- cl[ok]
	fe <- lapply(fe, function(f) f[ok])
	if (sum(g1) < 50 || sum(1 - g1) < 50) return(NULL)
	S1 <- ncol(G)
	m <- rowMeans(G[, -1, drop = FALSE])
	Z <- G - m
	rm(G)
	Dc <- if (length(unique(coh)) > 1L) model.matrix(~ 0 + factor(coh)) else matrix(1, length(y), 1L)
	X <- cbind(Dc * (m * g1), Dc * (m * (1 - g1)))
	M <- fixest::demean(cbind(y, X, Z * g1, Z * (1 - g1)), f = fe, weights = w, notes = FALSE)
	rm(Z)
	px <- ncol(X)
	Xd <- M[, 1 + seq_len(px), drop = FALSE]
	M <- M[, -(1 + seq_len(px)), drop = FALSE]
	sw <- sqrt(w)
	M <- qr.resid(qr(Xd * sw), M * sw) / sw
	yr <- M[, 1]
	A <- M[, 1 + seq_len(S1), drop = FALSE]
	C <- M[, 1 + S1 + seq_len(S1), drop = FALSE]
	rm(M)
	saa <- colSums(w * A * A)
	scc <- colSums(w * C * C)
	sac <- colSums(w * A * C)
	say <- colSums(w * A * yr)
	scy <- colSums(w * C * yr)
	det <- saa * scc - sac^2
	b1 <- (scc * say - sac * scy) / det
	b2 <- (saa * scy - sac * say) / det
	inv <- solve(matrix(c(saa[1], sac[1], sac[1], scc[1]), 2))
	u <- yr - b1[1] * A[, 1] - b2[1] * C[, 1]
	sc <- rowsum(cbind(A[, 1], C[, 1]) * (w * u), cl)
	V <- inv %*% crossprod(sc) %*% inv
	## condicionais: grupo 1 sorteado com a dose realizada do grupo 2, e o contrário
	sac_1 <- colSums(w * A * C[, 1])
	b1c <- (scc[1] * say - sac_1 * scy[1]) / (saa * scc[1] - sac_1^2)
	sca_1 <- colSums(w * C * A[, 1])
	b2c <- (saa[1] * scy - sca_1 * say[1]) / (scc * saa[1] - sca_1^2)
	pj <- function(b) mean(abs(b[-1]) >= abs(b[1]), na.rm = TRUE)
	list(b1 = b1[1], se1 = sqrt(V[1, 1]), p1 = pj(b1), p1c = pj(b1c), b2 = b2[1], se2 = sqrt(V[2, 2]), p2 = pj(b2), p2c = pj(b2c),
		d = b1[1] - b2[1], sed = sqrt(V[1, 1] + V[2, 2] - 2 * V[1, 2]), pd = pj(b1 - b2))
}
## linhas com estações abertas nas coortes do plano (para a quebra por linha)
linhas_plano <- function(niv) sort(unique(niv$linha[niv$coorte %in% coortes_plano]))

## quem ficou no mesmo endereço (18/11/2026): anos em que a pessoa precisa ter o mesmo hexágono no CadÚnico.
## pre = g−4 a g−1; pre_pos = g−4 a g+1. Endereços anuais só existem de 2011 a 2021; sem g+1, a coorte fica fora do pre_pos.
## Os arquivos anuais não são um painel completo: a pessoa só aparece no ano em que o cadastro está atualizado (de quem
## está em 2018, 48% aparece em 2017 e 24% em 2015). Por isso há duas versões: estrita (presente com o mesmo hexágono em
## todos os anos: pre, pre_pos) e observada (nenhum ano com outro hexágono e presente em pelo menos mais um ano além do
## ano-base: pre_obs, pre_pos_obs).
anos_ficaram <- function(g, modo) {
	pos <- grepl("pre_pos", modo)
	if (pos && g + 1L > 2021L) return(NULL)
	a <- if (pos) (g - 4L):(g + 1L) else (g - 4L):(g - 1L)
	a[a >= 2011L & a <= 2021L]
}
estaveis <- function(cpf, h3, anos, estrito = TRUE) {
	ok <- rep(TRUE, length(cpf))
	vistos <- rep(0L, length(cpf))
	for (a in anos) {
		p <- arrow::read_parquet(file.path(dir_base, "pop_base", sprintf("pop_%d.parquet", a)), col_select = c("cpf_num", "h3_09"))
		hh <- p$h3_09[match(cpf, p$cpf_num)]
		ok <- ok & (if (estrito) !is.na(hh) & hh == h3 else is.na(hh) | hh == h3)
		vistos <- vistos + !is.na(hh)
	}
	if (estrito) ok else ok & vistos >= 2L
}

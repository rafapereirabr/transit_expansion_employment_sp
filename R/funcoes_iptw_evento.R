# Funções do IPTW da especificação final (24/11/2026), usadas por R/123_iptw_evento_area.R e R/124_iptw_evento_par.R.
# Dados: data.table com ev (evento), t1 (tratado), cel5 (cluster), as covariáveis e os desfechos; no nível H3-8, também
# n (pessoas da célula) e cnt_<conjunto> (pessoas com o desfecho do conjunto observado).
suppressPackageStartupMessages(library(splines))

## matriz do escore e da regressão de desfecho: spline da idade + demais covariáveis em termos lineares
mm_cov <- function(B, covs) {
	outras <- setdiff(covs, "idade")
	X <- cbind(if ("idade" %in% covs) ns(B$idade, 4) else NULL, as.matrix(B[, outras, with = FALSE]))
	X[!is.finite(X)] <- 0
	keep <- apply(X, 2, function(z) length(unique(z)) > 1L)
	X[, keep, drop = FALSE]
}
## pesos ATT por evento: tratados = peso-base; controles = peso-base × p/(1−p), normalizados para somar o peso dos tratados.
## corte (24/11/2026, decisão do pesquisador depois do diagnóstico): os pesos dos controles de cada evento são limitados ao
## percentil 99 dos pesos dos controles daquele evento. Sem o corte, em alguns eventos o escore quase separava tratados e
## controles, e poucos controles com escore perto de 1 carregavam a comparação (maior diferença padronizada de 2,1 depois
## dos pesos e só 114 controles efetivos no ganho de 10 min ou mais); com ele, 0,13 e cerca de 180 mil.
pesos_att <- function(B, covs, min_t = 30L, corte = 0.99) {
	## sem copy(): os chamadores passam um recorte novo, e a cópia da base inteira estourava a memória (24/11/2026)
	if (!"n" %in% names(B)) B[, n := 1]
	B[, `:=`(w0 = NA_real_, ps = NA_real_)]
	for (e in unique(B$ev)) {
		ix <- which(B$ev == e)
		t1 <- B$t1[ix]
		if (sum(t1) < min_t || sum(1L - t1) < min_t) next
		X <- cbind(1, mm_cov(B[ix], covs))
		ps <- tryCatch(suppressWarnings(glm.fit(X, t1, weights = B$n[ix], family = quasibinomial())$fitted.values), error = function(err) NULL)
		if (is.null(ps)) next
		ps <- pmin(pmax(ps, 1e-6), 1 - 1e-6)
		set(B, ix, "ps", ps)
		w0 <- B$n[ix] * fifelse(t1 == 1L, 1, ps / (1 - ps))
		jc <- which(t1 == 0L)
		if (corte < 1 && length(jc)) w0[jc] <- pmin(w0[jc], quantile(w0[jc], corte, names = FALSE))
		set(B, ix, "w0", w0)
	}
	B[!is.na(w0)]
}
## renormaliza os pesos dos controles por evento para somarem o peso dos tratados
renorm <- function(w0, t1, ev) {
	w <- w0
	for (e in unique(ev)) {
		ix <- which(ev == e)
		st <- sum(w0[ix][t1[ix] == 1L])
		sc <- sum(w0[ix][t1[ix] == 0L])
		if (st == 0 || sc == 0) {
			w[ix] <- 0
			next
		}
		j <- ix[t1[ix] == 0L]
		w[j] <- w0[j] * st / sc
	}
	w
}
## balanço: maior diferença padronizada entre tratados e controles, antes e depois dos pesos
balanco <- function(B, covs) {
	X <- mm_cov(B, covs)
	t1 <- B$t1
	w <- renorm(B$w0, t1, B$ev)
	nb <- B$n
	m1 <- colSums(X[t1 == 1L, , drop = FALSE] * nb[t1 == 1L]) / sum(nb[t1 == 1L])
	m0 <- colSums(X[t1 == 0L, , drop = FALSE] * nb[t1 == 0L]) / sum(nb[t1 == 0L])
	m0w <- colSums(X[t1 == 0L, , drop = FALSE] * w[t1 == 0L]) / sum(w[t1 == 0L])
	s <- sqrt((apply(X[t1 == 1L, , drop = FALSE], 2, var) + apply(X[t1 == 0L, , drop = FALSE], 2, var)) / 2)
	s[s == 0] <- NA
	data.table(tratados = sum(nb[t1 == 1L]), controles = sum(nb[t1 == 0L]), ess_controles = sum(w[t1 == 0L])^2 / sum(w[t1 == 0L]^2),
		smd_max_antes = max(abs(m1 - m0) / s, na.rm = TRUE), smd_max_depois = max(abs(m1 - m0w) / s, na.rm = TRUE),
		eventos = uniqueN(B$ev))
}
## estimador duplamente robusto: regressão ponderada de cada desfecho no tratamento, nas covariáveis e nos eventos, nas
## linhas com todos os desfechos do conjunto observados; EP de cluster (pesos tratados como fixos); devolve também a
## influência de cada cluster no coeficiente do tratamento (para as diferenças entre segmentos)
dr_iptw <- function(B, oc, covs, peso_oc = NULL) {
	Y <- as.matrix(B[, oc, with = FALSE])
	ok <- rowSums(!is.finite(Y)) == 0L & B$w > 0
	w <- B$w[ok]
	if (!is.null(peso_oc)) w <- w * peso_oc[ok]
	if (sum(B$t1[ok]) < 30L || sum(1L - B$t1[ok]) < 30L) return(NULL)
	Y <- Y[ok, , drop = FALSE]
	t1 <- B$t1[ok]
	Xc <- mm_cov(B[ok], covs)
	Xc <- scale(Xc)
	Xc[!is.finite(Xc)] <- 0
	evd <- if (length(unique(B$ev[ok])) > 1L) model.matrix(~ factor(B$ev[ok]))[, -1, drop = FALSE] else NULL
	X <- cbind(t1, 1, Xc, evd)
	sw <- sqrt(w)
	q <- qr(X * sw)
	keep <- sort(q$pivot[seq_len(q$rank)])
	if (keep[1] != 1L) return(NULL)
	X <- X[, keep, drop = FALSE]
	q <- qr(X * sw)
	Bc <- qr.coef(q, Y * sw)
	inv1 <- chol2inv(qr.R(q))[order(q$pivot), order(q$pivot), drop = FALSE][, 1]
	cl <- B$cel5[ok]
	out <- list(oc = oc, b = numeric(0), se = numeric(0), nt = numeric(0), nc = numeric(0), my = numeric(0), A = list())
	nb <- if ("n" %in% names(B)) B$n[ok] else rep(1, sum(ok))
	for (o in seq_along(oc)) {
		u <- as.vector(Y[, o] - X %*% Bc[, o])
		a <- rowsum((X * (w * u)) %*% inv1, cl)[, 1]
		out$b[o] <- Bc[1, o]
		out$se[o] <- sqrt(sum(a^2))
		out$A[[o]] <- a
		out$nt[o] <- sum(nb[t1 == 1L])
		out$nc[o] <- sum(nb[t1 == 0L])
		out$my[o] <- weighted.mean(Y[t1 == 1L, o], nb[t1 == 1L])
	}
	out
}
## diferença entre segmentos estimados separadamente, com EP de cluster conjunto
dif_iptw <- function(a, b, o) {
	A1 <- a$A[[o]]
	A2 <- b$A[[o]]
	cl <- union(names(A1), names(A2))
	s <- setNames(rep(0, length(cl)), cl)
	s[names(A1)] <- s[names(A1)] + A1
	s[names(A2)] <- s[names(A2)] - A2
	d <- a$b[o] - b$b[o]
	se <- sqrt(sum(s^2))
	c(b = d, se = se, p = 2 * pnorm(-abs(d / se)))
}
## agregação de pessoas (ou vínculos) a células: médias das covariáveis e dos desfechos, n e contagens por conjunto
agrega_cel <- function(B, vars, chave, conjuntos) {
	cnt <- lapply(conjuntos, function(oc) as.numeric(rowSums(!is.finite(as.matrix(B[, oc, with = FALSE]))) == 0L))
	names(cnt) <- paste0("cnt_", names(conjuntos))
	for (cc in names(cnt)) set(B, j = cc, value = cnt[[cc]])
	A <- B[, c(list(n = .N, cel5 = cel5[1]), lapply(.SD, function(z) if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE))), by = c(chave, "ev"),
		.SDcols = vars]
	C <- B[, lapply(.SD, sum), by = c(chave, "ev"), .SDcols = names(cnt)]
	merge(A, C, by = c(chave, "ev"))
}

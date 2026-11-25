# Estimador B&H da especificação final por evento (origem: bh_lista2.R, 24/11/2026). Usado por R/120, R/121 e R/122.
# bh_lista2 (24/11/2026): como bh_lista (termos como matrizes n × sorteios, coluna 1 = realizado, cada um recentrado pela
# média em TODOS os sorteios, com essa média × coorte/evento como controle), mas a distribuição de permutação usa só os
# primeiros n_perm sorteios (o EP é o de cluster do realizado, que não depende disso). n_perm = NULL usa todos.
bh_lista2 <- function(Y, w, Gl, coh, cl, fe, n_perm = NULL, lote = 20L) {
	Y <- as.matrix(Y)
	ok <- rowSums(!is.finite(Y)) == 0L & is.finite(w) & w > 0
	## só recorta (e copia as matrizes de dose, que são grandes) se alguma linha cair; quem chama pode filtrar antes
	if (!all(ok)) {
		Y <- Y[ok, , drop = FALSE]
		w <- w[ok]
		Gl <- lapply(Gl, function(m) m[ok, , drop = FALSE])
		coh <- coh[ok]
		cl <- cl[ok]
		fe <- lapply(fe, function(f) f[ok])
	}
	n <- nrow(Y)
	q <- ncol(Y)
	K <- length(Gl)
	S1 <- ncol(Gl[[1]])
	P1 <- if (is.null(n_perm)) S1 else min(S1, as.integer(n_perm) + 1L)
	M <- sapply(Gl, function(m) rowMeans(m[, -1, drop = FALSE]))
	if (!is.matrix(M)) M <- matrix(M, ncol = K)
	Dc <- if (length(unique(coh)) > 1L) model.matrix(~ 0 + factor(coh)) else matrix(1, n, 1L)
	X <- do.call(cbind, lapply(seq_len(K), function(j) Dc * M[, j]))
	X <- X[, colSums(X != 0) > 0, drop = FALSE]
	sw <- sqrt(w)
	MX <- fixest::demean(cbind(Y, X), f = fe, weights = w, notes = FALSE)
	qx <- qr(MX[, q + seq_len(ncol(X)), drop = FALSE] * sw)
	Yr <- qr.resid(qx, MX[, seq_len(q), drop = FALSE] * sw) / sw
	rm(MX, X)
	B <- array(NA_real_, c(P1, K, q))
	Vc <- vector("list", q)
	for (ini in seq(1L, P1, by = lote)) {
		ss <- ini:min(P1, ini + lote - 1L)
		ns <- length(ss)
		R <- do.call(cbind, lapply(seq_len(K), function(j) Gl[[j]][, ss, drop = FALSE] - M[, j]))
		R <- fixest::demean(R, f = fe, weights = w, notes = FALSE)
		R <- qr.resid(qx, R * sw) / sw
		for (i in seq_len(ns)) {
			Ai <- R[, (seq_len(K) - 1L) * ns + i, drop = FALSE]
			AtA <- crossprod(Ai * sw)
			b <- tryCatch(solve(AtA, crossprod(Ai * w, Yr)), error = function(e) matrix(NA_real_, K, q))
			B[ss[i], , ] <- b
			if (ss[i] == 1L) {
				inv <- solve(AtA)
				INV <- inv
				SC <- vector("list", q)
				for (o in seq_len(q)) {
					u <- as.vector(Yr[, o] - Ai %*% b[, o])
					sc <- rowsum(Ai * (w * u), cl)
					Vc[[o]] <- inv %*% crossprod(sc) %*% inv
					SC[[o]] <- sc
				}
			}
		}
		rm(R)
		gc()
	}
	list(B = B, V = Vc, n = n, ok = ok, SC = SC, INV = INV)
}
## diferença entre estimativas de dois grupos estimados separadamente (modelo totalmente interagido): cc1·b1 − cc2·b2,
## EP de cluster conjunto (escores dos dois grupos somados por cluster), p normal e p de permutação (mesmos sorteios;
## o p de permutação testa a hipótese nula de nenhum efeito nos dois grupos).
dif_bh2 <- function(r1, o1, cc1, r2, o2, cc2) {
	b <- sum(cc1 * r1$B[1, , o1]) - sum(cc2 * r2$B[1, , o2])
	P <- min(dim(r1$B)[1], dim(r2$B)[1])
	dr <- as.vector(matrix(r1$B[2:P, , o1], ncol = length(cc1)) %*% cc1) - as.vector(matrix(r2$B[2:P, , o2], ncol = length(cc2)) %*% cc2)
	a1 <- r1$SC[[o1]] %*% (r1$INV %*% cc1)
	a2 <- r2$SC[[o2]] %*% (r2$INV %*% cc2)
	cl <- union(rownames(a1), rownames(a2))
	s <- rep(0, length(cl))
	names(s) <- cl
	s[rownames(a1)] <- s[rownames(a1)] + a1[, 1]
	s[rownames(a2)] <- s[rownames(a2)] - a2[, 1]
	se <- sqrt(sum(s^2))
	c(b = b, se = se, p_normal = 2 * pnorm(-abs(b / se)), p_perm = mean(abs(dr) >= abs(b), na.rm = TRUE))
}
## teste conjunto de igualdade entre G grupos (G ≥ 3): diferenças em relação ao primeiro, Wald com a covariância de cluster
## conjunta; devolve a estatística, os graus de liberdade e o p (qui-quadrado)
wald_grupos2 <- function(RL, oL, ccL) {
	G <- length(RL)
	A <- lapply(seq_len(G), function(g) RL[[g]]$SC[[oL[g]]] %*% (RL[[g]]$INV %*% ccL[[g]]))
	cl <- unique(unlist(lapply(A, rownames)))
	M <- matrix(0, length(cl), G, dimnames = list(cl, NULL))
	for (g in seq_len(G)) M[rownames(A[[g]]), g] <- A[[g]][, 1]
	V <- crossprod(M)
	b <- sapply(seq_len(G), function(g) sum(ccL[[g]] * RL[[g]]$B[1, , oL[g]]))
	R <- cbind(-1, diag(G - 1))
	d <- R %*% b
	W <- as.numeric(t(d) %*% solve(R %*% V %*% t(R)) %*% d)
	c(wald = W, gl = G - 1, p = pchisq(W, G - 1, lower.tail = FALSE))
}
## combinação linear dos termos para o desfecho o: efeito, EP de cluster, p de permutação e p normal
comb_bh2 <- function(r, o, cc) {
	b <- sum(cc * r$B[1, , o])
	dr <- as.vector(matrix(r$B[-1, , o], ncol = length(cc)) %*% cc)
	se <- sqrt(as.numeric(t(cc) %*% r$V[[o]] %*% cc))
	c(b = b, se = se, p_perm = mean(abs(dr) >= abs(b), na.rm = TRUE), p_normal = 2 * pnorm(-abs(b / se)))
}

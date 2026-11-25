# Funções auxiliares da base de análise.

#' Distância geodésica (m) no elipsoide WGS84 pela fórmula inversa de Vincenty.
#' Vetorizada; adequada para distâncias urbanas (não trata pontos antípodas).
vincenty_m <- function(lat1, lon1, lat2, lon2) {
	a <- 6378137
	f <- 1 / 298.257223563
	b <- (1 - f) * a
	rad <- pi / 180
	L <- (lon2 - lon1) * rad
	U1 <- atan((1 - f) * tan(lat1 * rad))
	U2 <- atan((1 - f) * tan(lat2 * rad))
	sinU1 <- sin(U1)
	cosU1 <- cos(U1)
	sinU2 <- sin(U2)
	cosU2 <- cos(U2)
	lambda <- L
	for (it in 1:200) {
		sinL <- sin(lambda)
		cosL <- cos(lambda)
		sinSigma <- sqrt((cosU2 * sinL)^2 + (cosU1 * sinU2 - sinU1 * cosU2 * cosL)^2)
		cosSigma <- sinU1 * sinU2 + cosU1 * cosU2 * cosL
		sigma <- atan2(sinSigma, cosSigma)
		sinAlpha <- ifelse(sinSigma == 0, 0, cosU1 * cosU2 * sinL / sinSigma)
		cos2Alpha <- 1 - sinAlpha^2
		cos2SigmaM <- ifelse(cos2Alpha == 0, 0, cosSigma - 2 * sinU1 * sinU2 / cos2Alpha)
		C <- f / 16 * cos2Alpha * (4 + f * (4 - 3 * cos2Alpha))
		lambdaP <- lambda
		lambda <- L +
			(1 - C) * f * sinAlpha * (sigma + C * sinSigma * (cos2SigmaM + C * cosSigma * (-1 + 2 * cos2SigmaM^2)))
		if (max(abs(lambda - lambdaP), na.rm = TRUE) < 1e-12) break
	}
	u2 <- cos2Alpha * (a^2 - b^2) / b^2
	A <- 1 + u2 / 16384 * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
	B <- u2 / 1024 * (256 + u2 * (-128 + u2 * (74 - 47 * u2)))
	deltaSigma <- B *
		sinSigma *
		(cos2SigmaM +
			B / 4 * (cosSigma * (-1 + 2 * cos2SigmaM^2) - B / 6 * cos2SigmaM * (-3 + 4 * sinSigma^2) * (-3 + 4 * cos2SigmaM^2)))
	d <- b * A * (sigma - deltaSigma)
	d[sinSigma == 0] <- 0
	d
}

#' Menor distância geodésica de cada endereço a um conjunto de estações.
#' Pré-seleciona pares por distância planar em UTM (<= raio_cand) e calcula a distância exata só
#' para esses pares. Endereços sem estação dentro do raio de candidatos recebem NA (= longe).
#' @return data.frame com dist_m e o índice (linha de `st`) da estação mais próxima.
dist_min_estacoes <- function(ex, ey, elat, elon, st, raio_cand) {
	n <- length(ex)
	best <- rep(Inf, n)
	who <- rep(NA_integer_, n)
	for (k in seq_len(nrow(st))) {
		cand <- which(abs(ex - st$x_utm[k]) <= raio_cand & abs(ey - st$y_utm[k]) <= raio_cand)
		if (!length(cand)) next
		dp <- sqrt((ex[cand] - st$x_utm[k])^2 + (ey[cand] - st$y_utm[k])^2)
		cand <- cand[dp <= raio_cand]
		if (!length(cand)) next
		d <- vincenty_m(elat[cand], elon[cand], st$lat[k], st$lon[k])
		upd <- d < best[cand]
		best[cand[upd]] <- d[upd]
		who[cand[upd]] <- k
	}
	best[is.infinite(best)] <- NA_real_
	data.frame(dist_m = best, idx = who)
}

#' Converte o salário contratual da RAIS para valor mensal conforme tipo_salario.
#' 1 mensal; 2 quinzenal; 3 semanal; 4 diário; 5 por hora; 6 por tarefa; 7 outros.
#' Hora: valor × 5 × horas semanais (CLT, inclui o descanso semanal); diário × 30; semanal × 30/7;
#' tarefa e outros: o manual da RAIS manda informar a média mensal (× 1).
salario_mensal <- function(salario, tipo_salario, horas) {
	dplyr::case_when(
		is.na(salario) | salario <= 0 | is.na(tipo_salario) ~ NA_real_,
		tipo_salario == 1 ~ salario,
		tipo_salario == 2 ~ salario * 2,
		tipo_salario == 3 ~ salario * 30 / 7,
		tipo_salario == 4 ~ salario * 30,
		tipo_salario == 5 ~ salario * 5 * horas,
		tipo_salario %in% c(6, 7) ~ salario,
		.default = NA_real_
	)
}

# EXPLORATÓRIO (16/11/2026, decisão do pesquisador): o banco de dados do Arthur com os NOSSOS tratamentos (acessibilidade
# e tempo de viagem), testando o endereço-base em g−4, g−3, g−2 e g−1. Não usa o tratamento nem as análises do Arthur,
# nem o raio de 2 km.
#
# População (regras de dados do Arthur; preregistro e mapa na auditoria interna, pipeline_arthur.md):
# - pessoas do cad_individ do ano-base b = g−k, lidas da pasta do Arthur (só leitura), com as regras de CPF e
#   nascimento dele;
# - no município de São Paulo em b;
# - regra de endereço: se qualquer registro com o mesmo id_addr_cad em b tem desvio ausente ou > 500 m, ou cadastro sem
#   atualização há mais de 2 anos (years_update), todas as pessoas daquele ponto saem;
# - 18 a 60 anos completos em 1º/jan de g;
# - as exclusões do Arthur ligadas ao raio (15 km de estação; estação aberta a até 2 km entre 2011 e g−1) NÃO são usadas.
# Nos anos-base que não são g−4 do Arthur (fora de 2013, 2014, 2015 e 2017), os arquivos não têm menores de 18 em 31/12 de
# b, então faltam os mais novos (18 a 19 ou 20 anos em g). A variante idade=21 usa 21 a 60 em todos os anos-base.
# Emprego: celetista em 31/12 (regra do Arthur: vínculo ativo de tipo 10, 15, 20, 25, 50, 55, 60, 65, 70, 75 ou 90),
# calculado dos nossos rais_vinc e conferido com o rais_person_year do Arthur.
# Tratamentos (por coorte g, de 31/12/(g−1) a 31/12/g, cadeia A, recentrados pela média de S sorteios D1 do R/57):
# - acesso: empregos a até 60 min (sem as sedes), por +100 mil;
# - tempo: tempo médio de viagem até os empregos (R/86), em 10 min economizados.
# Outcomes por pessoa, relativos a e = −1 (31/12/(g−1)): pós h1, h2 e h3; placebo (média de e = −4 a −2); entrada (vínculo
# em g+2 entre quem estava sem vínculo em g−1). Modelo do R/57/R/70: média por hexágono ponderada por pessoas, coortes
# empilhadas com intercepto e inclinação de μ por coorte; EP por célula de 5 km; p por permutação.
#
# Uso: Rscript R/85_bh_base_arthur.R [lags=4,3,2,1] [coortes=2014,2017,2018,2019,2021] [idade=18] [sorteios=999]
# Saídas: data/base/celetista_pessoa_ano.parquet (restrito) e outputs/explora_bh_base_arthur[_i21].csv

source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(6L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
options(width = 220)
lags <- as.integer(strsplit(arg_cli("lags", NA, "4,3,2,1"), ",")[[1]])
coortes <- as.integer(strsplit(arg_cli("coortes", NA, "2014,2017,2018,2019,2021"), ",")[[1]])
idade_min_g <- as.integer(arg_cli("idade", NA, 18L))
S <- as.integer(arg_cli("sorteios", NA, 999L))
dir_art <- file.path(dir_arthur, "temp")
eleg <- c(10L, 15L, 20L, 25L, 50L, 55L, 60L, 65L, 70L, 75L, 90L)

## 1. emprego celetista (regra do Arthur) dos nossos rais_vinc ---------------------------------------------------------
arq_cel <- file.path(dir_base, "celetista_pessoa_ano.parquet")
if (!file.exists(arq_cel)) {
	CL <- list()
	for (a in anos_outcome) {
		v <- as.data.table(arrow::read_parquet(file.path(dir_rais, sprintf("rais_vinc_%d.parquet", a)), col_select = c("cpf_num", "emp_31dez", "tipo_vinculo")))
		CL[[as.character(a)]] <- v[emp_31dez %in% 1L & as.integer(tipo_vinculo) %in% eleg, .(celetista = 1L), by = cpf_num][, year := a]
		msg("celetista ", a, ": ", nrow(CL[[as.character(a)]]), " pessoas")
	}
	arrow::write_parquet(rbindlist(CL), arq_cel)
}
CEL <- as.data.table(arrow::read_parquet(arq_cel))
## conferência com o rais_person_year do Arthur (2017 e 2021)
for (a in c(2017L, 2021L)) {
	ar <- as.data.table(arrow::read_parquet(file.path(dir_art, sprintf("rais_person_year_%d.parquet", a)), col_select = c("cpf", "celetista_31dec")))
	ar[, cpf_num := as.numeric(cpf)]
	ar[CEL[year == a], nosso := i.celetista, on = .(cpf_num)]
	ar[is.na(nosso), nosso := 0L]
	msg("conferência ", a, ": ", nrow(ar), " CPFs do Arthur; concordância ", round(100 * mean(ar$celetista_31dec == ar$nosso, na.rm = TRUE), 4), "%; NA no Arthur ",
		sum(is.na(ar$celetista_31dec)))
	rm(ar)
}

## 2. tratamentos: acesso e tempo, por nível e por coorte, com os sorteios D1 ------------------------------------------
cat_a <- fread("data/redes/catalogo_familia_a.csv")[familia %in% c("A", "A'")]
niv <- rbind(
	cat_a[familia == "A" & acao == "retira", .(nivel = rede, com = base, sem = rede, linha = code_line, data = as.IDate(data_abertura))],
	cat_a[familia == "A'", .(nivel = rede, com = rede, sem = "A00", linha = code_line, data = as.IDate(data_abertura))]
)
le_rede <- function(tipo, r) {
	if (tipo == "acesso") {
		x <- as.data.table(arrow::read_parquet(file.path(dir_base, "acesso_sem_se", sprintf("acesso_%s.parquet", r))))[corte == 60L, .(id, v = acc)]
	} else {
		x <- as.data.table(arrow::read_parquet(file.path(dir_base, "tempo_medio", sprintf("tm_%s.parquet", r))))[, .(id, v = tm)]
	}
	setnames(x, "v", r)
}
dint <- as.integer(niv$data)
limite <- function(t) as.integer(as.IDate(sprintf("%d-12-31", t)))
set.seed(20261115L)
ordem <- lapply(split(seq_len(nrow(niv)), niv$linha), function(ix) ix[order(dint[ix])])
sortear <- function() {
	d <- sample(dint)
	for (ix in ordem) d[ix] <- sort(d[ix])
	d
}
D <- cbind(dint, replicate(S, sortear()))
DOSE <- list()
HEX <- list()
for (tipo in c("acesso", "tempo")) {
	V <- Reduce(function(x, y) merge(x, y, by = "id"), lapply(cat_a$rede, function(r) le_rede(tipo, r)))
	## acesso: ganho em 100 mil empregos; tempo: minutos economizados / 10
	G <- if (tipo == "acesso") sapply(seq_len(nrow(niv)), function(k) V[[niv$com[k]]] - V[[niv$sem[k]]]) / 1e5 else
		sapply(seq_len(nrow(niv)), function(k) V[[niv$sem[k]]] - V[[niv$com[k]]]) / 10
	HEX[[tipo]] <- V$id
	for (g in coortes) {
		dl <- G %*% ((D <= limite(g)) * 1 - (D <= limite(g - 1L)) * 1)
		mu <- rowMeans(dl[, -1, drop = FALSE])
		DOSE[[paste(tipo, g)]] <- list(dl = dl, mu = mu)
		msg(tipo, " ", g, ": dose realizada média ", signif(mean(dl[, 1]), 3), "; μ médio ", signif(mean(mu), 3), "; correlação ", round(cor(dl[, 1], mu), 3))
	}
}
stopifnot(identical(HEX$acesso, HEX$tempo))
hexid <- HEX$acesso
xy <- sf::st_coordinates(h3jsr::cell_to_point(hexid, simple = FALSE))
utm <- sf::sf_project("EPSG:4326", sprintf("EPSG:%d", crs_utm), xy)
cel5 <- paste(floor(utm[, 1] / 5000), floor(utm[, 2] / 5000))

## 3. populações e outcomes ---------------------------------------------------------------------------------------------
le_cad <- function(b) {
	x <- as.data.table(arrow::read_parquet(file.path(dir_art, sprintf("cad_individ_%d.parquet", b)),
		col_select = c("cpf", "birthdate_cad", "code_muni_cad", "h3_09_cad", "id_addr_cad", "desvio_metros", "years_update")))
	x[, cpf_num := as.numeric(cpf)]
	x[, cpf := NULL]
	x
}
wls <- function(y, X, wt, cl) {
	Xw <- X * wt
	inv <- tryCatch(solve(crossprod(X, Xw)), error = function(e) NULL)
	if (is.null(inv)) return(list(b = rep(NA_real_, ncol(X)), se = rep(NA_real_, ncol(X))))
	b <- as.vector(inv %*% crossprod(Xw, y))
	u <- as.vector(y - X %*% b)
	sc <- rowsum(Xw * u, cl)
	list(b = b, se = sqrt(diag(inv %*% crossprod(sc) %*% inv)))
}
wls_b <- function(y, X, wt) {
	Xw <- X * wt
	inv <- tryCatch(solve(crossprod(X, Xw)), error = function(e) NULL)
	if (is.null(inv)) return(rep(NA_real_, ncol(X)))
	as.vector(inv %*% crossprod(Xw, y))
}
RES <- list()
POPN <- list()
for (k in lags) {
	BL <- list()
	for (g in coortes) {
		b <- g - k
		if (b < 2011L) next
		x <- le_cad(b)
		n0 <- nrow(x)
		x <- x[code_muni_cad == muni_estudo]
		ruim <- x[is.na(desvio_metros) | desvio_metros > 500 | is.na(years_update) | years_update > 2, unique(id_addr_cad)]
		x <- x[!is.na(id_addr_cad) & !id_addr_cad %in% ruim]
		n_end <- nrow(x)
		x <- x[!is.na(birthdate_cad) & birthdate_cad > as.IDate(sprintf("%d-01-01", g - 61L)) & birthdate_cad <= as.IDate(sprintf("%d-01-01", g - idade_min_g))]
		x <- unique(x, by = "cpf_num")
		x[, ih := match(h3_09_cad, hexid)]
		x <- x[!is.na(ih)]
		POPN[[paste(k, g)]] <- data.table(lag = k, coorte = g, ano_base = b, registros = n0, apos_regra_endereco = n_end, populacao = nrow(x))
		## outcomes: celetista em g−4..g+4
		anos <- (g - 4L):(g + 4L)
		anos <- anos[anos >= 2011L & anos <= max(anos_outcome)]
		for (a in anos) {
			x[CEL[year == a], (paste0("y", a - g)) := i.celetista, on = .(cpf_num)]
			set(x, which(is.na(x[[paste0("y", a - g)]])), paste0("y", a - g), 0L)
		}
		ym <- function(es) {
			cs <- intersect(paste0("y", es), names(x))
			if (!length(cs)) return(rep(NA_real_, nrow(x)))
			rowMeans(as.matrix(x[, cs, with = FALSE]))
		}
		Y <- list(pos_h1 = ym(1) - x$`y-1`, pos_h2 = ym(1:2) - x$`y-1`, pos_h3 = ym(1:3) - x$`y-1`, placebo = ym(-4:-2) - x$`y-1`,
			entrada_h2 = fifelse(x$`y-1` == 0L, x$y2, NA_real_))
		for (oc in names(Y)) {
			ok <- !is.na(Y[[oc]])
			if (!any(ok)) next
			h <- data.table(ih = x$ih[ok], y = Y[[oc]][ok])[, .(y = mean(y), n = .N), by = ih]
			BL[[oc]][[as.character(g)]] <- h
		}
		msg("lag ", k, " coorte ", g, " (base ", b, "): ", n0, " registros; após endereço ", n_end, "; população ", nrow(x))
		rm(x)
		gc()
	}
	## estimativas por tratamento e outcome
	for (tipo in c("acesso", "tempo")) for (oc in names(BL)) {
		L <- BL[[oc]]
		cs <- names(L)
		for (alvo in c("todas", cs)) {
			cc <- if (alvo == "todas") cs else alvo
			pz <- rbindlist(lapply(cc, function(cg) L[[cg]][, .(ih, y, n, coorte = cg)]))
			if (nrow(pz) < 50) next
			mu <- unlist(lapply(cc, function(cg) DOSE[[paste(tipo, cg)]]$mu[L[[cg]]$ih]))
			Zs <- do.call(rbind, lapply(cc, function(cg) DOSE[[paste(tipo, cg)]]$dl[L[[cg]]$ih, , drop = FALSE] - DOSE[[paste(tipo, cg)]]$mu[L[[cg]]$ih]))
			Xc <- if (length(cc) > 1L) model.matrix(~ 0 + factor(coorte), pz) else matrix(1, nrow(pz), 1L)
			X <- cbind(Xc, Xc * mu, Zs[, 1])
			j <- ncol(X)
			r <- wls(pz$y, X, pz$n, cel5[pz$ih])
			b_cf <- vapply(seq_len(S), function(s) {
				X[, j] <- Zs[, s + 1L]
				wls_b(pz$y, X, pz$n)[j]
			}, 0)
			RES[[length(RES) + 1L]] <- data.table(lag = k, tratamento = tipo, outcome = oc, coorte = alvo, efeito_pp = 100 * r$b[j], ep_pp = 100 * r$se[j],
				p_perm = mean(abs(b_cf) >= abs(r$b[j]), na.rm = TRUE), pessoas = sum(pz$n), hexagonos = nrow(pz), media_y_pct = 100 * weighted.mean(pz$y, pz$n),
				unidade = if (tipo == "acesso") "p.p. por +100 mil empregos em 60 min" else "p.p. por 10 min a menos de tempo médio aos empregos")
		}
		msg("lag ", k, " ", tipo, " ", oc, ": feito")
	}
}
res <- rbindlist(RES)
suf <- if (idade_min_g != 18L) sprintf("_i%d", idade_min_g) else ""
fwrite(res, sprintf("outputs/explora_bh_base_arthur%s.csv", suf))
fwrite(rbindlist(POPN), sprintf("outputs/explora_bh_base_arthur_populacao%s.csv", suf))
print(rbindlist(POPN))
print(res[coorte == "todas", .(lag, tratamento, outcome, efeito_pp = round(efeito_pp, 3), ep_pp = round(ep_pp, 3), p_perm = round(p_perm, 3), pessoas)], nrows = 200)
msg("fim")

# EXPLORATÓRIO (17/11/2026, pedido do pesquisador): o nulo é real? Previsão no estilo de Ahlfeldt (2013, Environment and
# Planning A) com acessibilidade gravitacional.
# A. Decaimento: com os trajetos casa → trabalho dos inscritos no CadÚnico empregados em 31/12/2012 (endereço de 2012,
#    local de trabalho do Arthur de 2012) e a rede de fim de 2012 (A23, antes de qualquer abertura), modelo de faixas de
#    tempo (PPML): trabalhadores da origem i na faixa b ~ exp(EF da origem + log dos empregos alcançáveis em b + γ_b).
#    β = decaimento exponencial (também estimado com o tempo contínuo).
# B. Acessibilidade gravitacional nas 27 redes: A_i = Σ_j empregos_j · exp(−β · t_ij), t até 90 min (a 90 min o peso é
#    desprezível), empregos de 2015 sem as duas sedes da Sé. Ganho por nível pelo atalho aditivo, como nos cortes.
# C. Relação de antes das aberturas (corte transversal, rede A23): por hexágono, emprego em 31/12/2012 (população com
#    endereço em 2012), meses com vínculo em 2012 e entrada (sem vínculo em 2011, com vínculo em 2013, população de 2011)
#    contra log(A). Três versões: sem controles; com composição (mulheres, pretos e pardos, alfabetizados, idade média) e
#    Censo 2010 e distância à Sé; e mais efeito fixo de célula de 5 km (só variação local).
# D. Efeito estimado: B&H do D2 (médias por hexágono do R/92, referência g−1) com a dose Δlog(A) da coorte, recentrada
#    pelos 999 sorteios das datas de serviço pleno; p por permutação; EP por célula de 5 km.
# E. Comparação: efeito previsto = β_corte_transversal × Δlog(A); razão W = estimado / previsto (Ahlfeldt: W = 1 se o
#    efeito é o que o corte transversal prevê); intervalo de 95% do estimado contra o previsto.
# Variante limite=240 (17/11/2026, pedido do pesquisador): decaimento com todos os trajetos até 240 min, com a matriz
# completa da rede de 2012 do R/111 (empregos por minuto e tempos dos pares de 2012, 1 sorteio por minuto); faixas de 5
# min até 240. A acessibilidade das 27 redes (B) usa o β novo, mas continua somando só até 90 min (só há matrizes completas
# até 90 min); o log mostra quanto da acessibilidade da rede de 2012 vem de trajetos acima de 90 min com o β novo.
# Variante limite=240 acesso=completo: a acessibilidade das 27 redes soma até 240 min, com os empregos por minuto das
# matrizes completas do R/112 (sem o corte em 90 min).
# Uso: Rscript R/107_ahlfeldt_previsao.R [limite=90] [acesso=90]
# Saídas: data/base/acesso_grav[_240][_completo]/ (restrito), outputs/ahlfeldt_decaimento[_240][_completo].csv,
# outputs/ahlfeldt_previsao[_240][_completo].csv

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
options(width = 220)
dir_art <- file.path(dir_arthur, "temp")
limite <- as.integer(arg_cli("limite", NA, 90L))
acesso <- arg_cli("acesso", NA, "90")
stopifnot(limite %in% c(90L, 240L), acesso %in% c("90", "completo"), acesso == "90" || limite == 240L)
suf <- paste0(if (limite > 90L) paste0("_", limite) else "", if (acesso == "completo") "_completo" else "")
nf <- limite %/% 5L
niv <- le_niveis()
hx <- le_hex()
ids <- le_ganhos(60L)$id
hx <- hx[match(ids, hx$id)]
for (v in c("renda_pc", "subnormal", "densidade", "esgoto", "pavim")) set(hx, which(is.na(hx[[v]])), v, median(hx[[v]], na.rm = TRUE))
emp <- as.data.table(arrow::read_parquet(file.path(dir_base, "empregos_h3_2015.parquet")))
se <- fread("data/base/acesso_cortes/hex_se.csv")$to_id
emp <- emp[!h3_09 %in% se & empregos > 0]
le_ttm <- function(r) as.data.table(arrow::read_parquet(file.path("data/redes/familia_a", sprintf("ttm_%s.parquet", r)), col_select = c("from_id", "to_id", "travel_time_p50")))
local_trabalho <- function(r) {
	w <- as.data.table(arrow::read_parquet(file.path(dir_art, sprintf("rais_workplaces_%d.parquet", r)), col_select = c("id_estab", "cei_vinc", "work_h3_id", "work_desvio", "work_geo_valid")))
	w <- w[work_geo_valid %in% TRUE & !is.na(work_desvio) & work_desvio <= 500 & !is.na(work_h3_id)]
	w[order(!(is.na(cei_vinc) | cei_vinc %in% c("", "0")))][!duplicated(id_estab)][, .(estab = id_estab, trab = work_h3_id)]
}

## A. decaimento ---------------------------------------------------------------------------------------------------------
x12 <- pop_coorte(1L, 2013L, 18L, ids)
x12 <- junta_pa(x12, 2011:2013, c("emp31", "meses", "estab31"))
w12 <- local_trabalho(2012L)
fl <- x12[emp31_2012 == 1L, .(casa = h3_09, trab = w12$trab[match(estab31_2012, w12$estab)])][!is.na(trab), .N, by = .(casa, trab)]
grade <- arrow::open_dataset(file.path(dir_insumos, "ttm_transit_2025.parquet")) |> dplyr::distinct(from_id) |> dplyr::collect() |> dplyr::pull(from_id)
if (limite > 90L) {
	## matriz completa da rede de 2012 até 240 min (R/111): tempos dos pares e empregos por minuto
	tp <- as.data.table(arrow::read_parquet(file.path(dir_base, "acesso_grav", sprintf("t_pares2012_A23_%d.parquet", limite))))
	fl[tp, t := i.t, on = .(casa, trab)]
	jm <- as.data.table(arrow::read_parquet(file.path(dir_base, "acesso_grav", sprintf("j_min_A23_%d.parquet", limite))))
	rm(tp)
} else {
	m23 <- le_ttm("A23")
	fl[m23, t := i.travel_time_p50, on = .(casa = from_id, trab = to_id)]
	jm <- m23[emp, on = .(to_id = h3_09), nomatch = 0L][, .(empregos = sum(empregos)), by = .(from_id, t = travel_time_p50)]
	rm(m23)
}
msg("decaimento: ", sum(fl$N), " trabalhadores; fora: ", round(100 * sum(fl[!trab %in% grade, N]) / sum(fl$N), 1), "% com trabalho fora da grade e ",
	round(100 * sum(fl[trab %in% grade & is.na(t), N]) / sum(fl$N), 1), "% com trajeto acima de ", limite, " min")
fl <- fl[!is.na(t)]
fl[, faixa := pmin(t %/% 5L, nf - 1L)]
J <- jm[, .(J = sum(empregos)), by = .(casa = from_id, faixa = pmin(t %/% 5L, nf - 1L))]
Fb <- fl[, .(F = sum(N)), by = .(casa, faixa)]
DB <- merge(J[casa %in% Fb$casa], Fb, by = c("casa", "faixa"), all.x = TRUE)
DB[is.na(F), F := 0]
DB[, t_meio := faixa * 5 + 2.5]
mb <- fepois(F ~ i(faixa, ref = 0) | casa, DB, offset = ~ log(J), notes = FALSE)
mc <- fepois(F ~ t_meio | casa, DB, offset = ~ log(J), notes = FALSE)
beta <- -unname(coef(mc)["t_meio"])
gam <- coef(mb)
dec <- data.table(faixa = 0:(nf - 1L))
dec[, `:=`(faixa_min = paste0(faixa * 5L, "-", faixa * 5L + 5L), gamma = fifelse(faixa == 0L, 0, unname(gam[paste0("faixa::", faixa)])),
	trabalhadores = Fb[, sum(F), by = faixa][match(dec$faixa, faixa), V1])]
dec[, peso_relativo := exp(gamma)]
fwrite(cbind(dec, beta_exponencial = beta, ep_beta = unname(se(mc)["t_meio"])), sprintf("outputs/ahlfeldt_decaimento%s.csv", suf))
msg("β (decaimento por minuto) = ", round(beta, 4), " (EP ", round(se(mc)["t_meio"], 4), "); peso a 30 min = ", round(exp(-30 * beta), 3), "; a 60 = ", round(exp(-60 * beta), 3),
	"; a 90 = ", round(exp(-90 * beta), 3), if (limite > 90L) paste0("; a 120 = ", round(exp(-120 * beta), 4), "; a 180 = ", round(exp(-180 * beta), 5)))
print(dec)
if (limite > 90L) {
	## quanto da acessibilidade da rede de 2012 vem de trajetos acima de 90 min, com o β novo
	ca <- jm[, .(a90 = sum((empregos * exp(-beta * t))[t <= 90]), tot = sum(empregos * exp(-beta * t))), by = from_id][tot > 0]
	msg("parte da acessibilidade (rede de 2012, β novo) que vem de trajetos acima de 90 min: mediana ", round(100 * median(1 - ca$a90 / ca$tot), 1), "%; p90 ",
		round(100 * quantile(1 - ca$a90 / ca$tot, 0.9), 1), "%")
}
rm(J, DB, jm)
gc()

## B. acessibilidade gravitacional ---------------------------------------------------------------------------------------
dir_g <- file.path(dir_base, paste0("acesso_grav", suf))
dir.create(dir_g, showWarnings = FALSE)
redes <- sort(unique(c(niv$com, niv$sem)))
V <- NULL
for (r in redes) {
	if (acesso == "completo") {
		## empregos por minuto até 240 min (R/112)
		jr <- as.data.table(arrow::read_parquet(file.path(dir_base, "acesso_240", sprintf("jmin_%s.parquet", r))))
		origens <- arrow::read_parquet(file.path("data/redes", "origens_familia_a.parquet"))$id
		a <- jr[, .(A = sum(empregos * exp(-beta * t))), by = .(id = from_id)]
		rm(jr)
	} else {
		m <- le_ttm(r)
		origens <- unique(m$from_id)
		a <- m[emp, on = .(to_id = h3_09), nomatch = 0L][, .(A = sum(empregos * exp(-beta * travel_time_p50))), by = .(id = from_id)]
		rm(m)
	}
	a <- a[data.table(id = origens), on = "id"]
	a[is.na(A), A := 0]
	arrow::write_parquet(a, file.path(dir_g, sprintf("grav_%s.parquet", r)))
	setnames(a, "A", r)
	V <- if (is.null(V)) a else merge(V, a, by = "id")
}
V <- V[match(ids, V$id)]
Gg <- sapply(seq_len(nrow(niv)), function(k) V[[niv$com[k]]] - V[[niv$sem[k]]])
colnames(Gg) <- niv$nivel
A00 <- V$A00
A23 <- V$A23
msg("acessibilidade gravitacional: mediana na rede de 2012 = ", round(median(A23)), "; hexágonos sem acesso = ", sum(A23 <= 0))

## C. relação de antes das aberturas -------------------------------------------------------------------------------------
cs_dados <- function(x, y, ano_comp) {
	x[, `:=`(fem = as.numeric(sexo == "2"), negra = as.numeric(raca %in% c("2", "4")), alfab = as.numeric(alfabetizado == "1"))]
	d <- data.table(ih = x$ih, y = y, fem = x$fem, negra = x$negra, alfab = x$alfab, idade = x$idade)[!is.na(y)]
	d <- d[, .(y = mean(y), fem = mean(fem), negra = mean(negra), alfab = mean(alfab), idade = mean(idade), n = .N), by = ih]
	d[, `:=`(lA = log(pmax(A23[ih], 1)), renda_pc = hx$renda_pc[ih], subnormal = hx$subnormal[ih], densidade = hx$densidade[ih], esgoto = hx$esgoto[ih],
		pavim = hx$pavim[ih], dist_se = hx$dist_se_km[ih], cel5 = hx$cel5[ih])]
	d[A23[ih] > 0]
}
cs_est <- function(d, nome, esc) {
	fs <- list(sem_controles = y ~ lA, com_controles = y ~ lA + fem + negra + alfab + idade + log(renda_pc) + subnormal + log1p(densidade) + esgoto + pavim + dist_se,
		com_controles_ef5km = y ~ lA + fem + negra + alfab + idade + log(renda_pc) + subnormal + log1p(densidade) + esgoto + pavim | cel5)
	rbindlist(lapply(names(fs), function(fn) {
		m <- feols(fs[[fn]], d, weights = ~n, cluster = ~cel5, notes = FALSE, warn = FALSE)
		data.table(outcome = nome, especificacao = fn, beta_cs = esc * unname(coef(m)["lA"]), ep_cs = esc * unname(se(m)["lA"]), hexagonos = nobs(m), pessoas = sum(d$n))
	}))
}
CS <- list()
CS$emp <- cs_est(cs_dados(x12, x12$emp31_2012, 2012L), "emprego", 100)
CS$meses <- cs_est(cs_dados(x12, x12$meses_2012, 2012L), "meses_no_ano", 1)
x11 <- pop_coorte(1L, 2012L, 18L, ids)
x11 <- junta_pa(x11, 2011:2013, "emp31")
x11 <- x11[emp31_2011 == 0L]
CS$ent <- cs_est(cs_dados(x11, x11$emp31_2013, 2011L), "entrada_2anos", 100)
cs <- rbindlist(CS)
print(cs)

## D. B&H com a dose Δlog(A) ----------------------------------------------------------------------------------------------
HY <- as.data.table(arrow::read_parquet(file.path(dir_base, "d2_hex", "hy_ref1_i18.parquet")))
COH <- coorte_data(le_sorteios())
Z <- list()
MU <- list()
TIP <- list()
for (g in coortes_plano) {
	ganho <- Gg %*% ((COH == g) * 1)
	## acessibilidade antes da coorte: rede sem nenhum nível (A23) + ganhos dos níveis com serviço pleno antes da coorte.
	## (Correção de 18/11/2026: antes era A00 − ganhos dos níveis da coorte em diante, o que também tirava o ganho das linhas
	## futuras A24–A26, que não estão na A00, e gerava doses extremas perto delas.)
	antes <- A23 + Gg %*% ((COH < g) * 1)
	M <- log1p(ganho / pmax(antes, 1))
	mu <- rowMeans(M[, -1, drop = FALSE])
	Z[[as.character(g)]] <- M - mu
	MU[[as.character(g)]] <- mu
	TIP[[as.character(g)]] <- M[, 1]
}
cel5 <- hx$cel5
mapa <- c(emp_h1 = "emprego", emp_h2 = "emprego", emp_h3 = "emprego", meses = "meses_no_ano", entrada_h2 = "entrada_2anos", placebo = "emprego")
RB <- list()
for (k in 1:4) for (oc in names(mapa)) {
	pz <- HY[lag == k & outcome == oc][order(coorte)]
	if (nrow(pz) < 100L) next
	cs_g <- sort(unique(pz$coorte))
	Zs <- do.call(rbind, lapply(cs_g, function(g) Z[[as.character(g)]][pz[coorte == g, ih], , drop = FALSE]))
	mu <- unlist(lapply(cs_g, function(g) MU[[as.character(g)]][pz[coorte == g, ih]]))
	r <- bh_fwl(pz$y, pz$n, Zs, mu, pz$coorte, cel5[pz$ih])
	esc <- if (oc == "meses") 1 else 100
	dose_real <- Zs[, 1] + mu
	RB[[length(RB) + 1L]] <- data.table(lag = k, outcome = oc, b = esc * r$b, ep = esc * r$se, p_perm = r$p,
		dose_mediana_tratados = median(dose_real[dose_real > 0.01]), hex_tratados = sum(dose_real > 0.01), pessoas = sum(pz$n))
}
rb <- rbindlist(RB)

## E. comparação ---------------------------------------------------------------------------------------------------------
rb[, outcome_cs := mapa[outcome]]
cmp <- merge(rb[, .(lag, outcome, outcome_cs, b, ep, p_perm, dose_mediana_tratados, hex_tratados)], cs[, .(outcome_cs = outcome, especificacao, beta_cs, ep_cs)],
	by = "outcome_cs", allow.cartesian = TRUE)
## meses: o D2 soma três anos de meses (g+1..g+3 contra r−2..r); a previsão anual vezes 3
cmp[outcome == "meses", `:=`(beta_cs = 3 * beta_cs, ep_cs = 3 * ep_cs)]
cmp[, `:=`(W = b / beta_cs, ic_inf = b - 1.96 * ep, ic_sup = b + 1.96 * ep)]
cmp[, previsao_fora_do_ic := beta_cs < ic_inf | beta_cs > ic_sup]
cmp[, `:=`(efeito_previsto_tipico = beta_cs * dose_mediana_tratados, efeito_estimado_tipico = b * dose_mediana_tratados,
	ic_inf_tipico = ic_inf * dose_mediana_tratados, ic_sup_tipico = ic_sup * dose_mediana_tratados)]
fwrite(cmp, sprintf("outputs/ahlfeldt_previsao%s.csv", suf))
print(cmp[outcome != "placebo", .(lag, outcome, especificacao, previsto = round(beta_cs, 2), estimado = round(b, 2), ic = sprintf("[%.2f; %.2f]", ic_inf, ic_sup),
	W = round(W, 2), fora_ic = previsao_fora_do_ic, dose_tipica = round(dose_mediana_tratados, 3))], nrows = 200)
print(rb[outcome == "placebo"])
msg("fim")

# Especificação final, bloco D2 (24/11/2026; origem: explora_iptw_par.R): IPTW de faixas de ganho de tempo casa → próprio emprego no desenho por
# evento, com os desfechos do bloco B (qualquer saída, demissão, sem justa causa, justa causa, pedido, permanência e fração
# dos meses; h = 6, 12, 18, 24). Roda depois de R/evento_par_monta.R (modo "iptw") montar os vínculos
# (POP), os pares, os tempos (TT) e os ganhos por nível (DL).
# - Tratados: ganho de tempo dos níveis com a data do evento nas faixas 1–5, 5–10 e 10 min ou mais; controles: ganho
#   abaixo de 1 min. Separado por trajeto antes até 90 e acima de 90 min (até 241).
# - Janela limpa: fica fora quem ganha 1 min ou mais de outro evento cuja abertura ou serviço pleno cai entre a referência
#   do placebo e 24 meses depois da referência.
# - Escore (logit, por evento e faixa): idade (spline), sexo, criança, raça, deficiência, alfabetização, anos sem
#   atualização, emprego 12 meses antes da linha de base e fração dos 12 meses empregado, log da remuneração do vínculo,
#   tempo de vínculo, tempo de trajeto antes, log da acessibilidade gravitacional de casa antes do evento, distâncias da
#   casa e do trabalho à Sé, Censo 2010 da área de casa. Pesos ATT normalizados por evento; duplamente robusto com
#   efeitos de evento; EP por célula de 5 km de casa.
# - Segmentos: no nível individual, escore da amostra toda e pesos renormalizados no segmento; nivel=h38, cada segmento
#   agregado por par casa H3-8 × trabalho H3-8 × evento e estimado à parte. Contrastes com EP de cluster conjunto.
# Uso: Rscript R/124_iptw_evento_par.R nivel=ind|h38 [janelas=pos,pre] [sufixo=]
# Saídas: outputs/final/iptw_par_<nivel><sufixo>_segmentos.csv, _contrastes.csv, _balanco.csv
modo_par <- "iptw"
source("R/evento_par_monta.R")
source("R/funcoes_iptw_evento.R")
nivel_x <- arg_cli("nivel", NA, "ind")
stopifnot(nivel_x %in% c("ind", "h38"))
janelas_x <- strsplit(arg_cli("janelas", NA, "pos,pre"), ",")[[1]]
suf_x <- arg_cli("sufixo", NA, "")
dir.create("outputs/final", showWarnings = FALSE)
for (v in c("renda_pc", "subnormal", "densidade", "esgoto", "pavim")) set(hx, which(is.na(hx[[v]])), v, median(hx[[v]], na.rm = TRUE))
gcg <- le_ganhos("gcal")
Gacc <- as.matrix(gcg[match(ids, id), niv$nivel, with = FALSE])
A00 <- gcg$acc_A00[match(ids, gcg$id)]
rm(gcg)
pleno_niv <- as.integer(niv$pleno)
rede_antes_ev <- function(e) {
	Lg <- strsplit(EVT[coorte == e, niveis], ",")[[1]]
	niv$sem[match(Lg[which.max(as.integer(sub("A", "", Lg)))], niv$nivel)]
}
p8 <- pai_h3(pares$casa, 8)
q8 <- pai_h3(pares$trab, 8)
dist_trab <- hx$dist_se_km[match(pares$trab, hx$id)]
dist_trab[is.na(dist_trab)] <- median(dist_trab, na.rm = TRUE)
OCS <- c(as.vector(outer(c("qualquer", "demissao", "sem_justa", "justa", "pedido"), H, paste, sep = "_")), as.vector(outer(c("empregado", "meses"), H, paste, sep = "_")))
oc_sets <- c(setNames(lapply(H, function(h) paste0(c("qualquer", "demissao", "sem_justa", "justa", "pedido"), "_", h)), paste0("saidas_", H)),
	list(permanencia = c(paste0("empregado_", H), paste0("meses_", H))))

## base de vínculos por evento e janela ------------------------------------------------------------------------------------
BASE <- list()
for (nm in names(POP)) {
	jn <- sub(" .*", "", nm)
	if (!jn %in% janelas_x) next
	v <- POP[[nm]]
	e <- v$coorte[1]
	E <- EVT[coorte == e]
	ipx <- match(paste(v$h3_09, v$trab), chave)
	ta <- TT[cbind(ipx, match(rede_antes_ev(e), redes))]
	ok <- !is.na(ta) & ta > 0 & ta <= 241
	v <- copy(v[ok])
	v[, `:=`(ip = ipx[ok], t_antes = ta[ok])]
	lv <- strsplit(E$niveis, ",")[[1]]
	v[, dose := rowSums(DL[ip, match(lv, niv$nivel), drop = FALSE])]
	suja <- rep(FALSE, nrow(v))
	for (e2 in setdiff(EVT$coorte, e)) {
		E2 <- EVT[coorte == e2]
		if (E2$o_min <= E$ref_pos + 24L && E2$m_e >= E$ref_pre) suja <- suja | rowSums(DL[v$ip, match(strsplit(E2$niveis, ",")[[1]], niv$nivel), drop = FALSE]) >= 1
	}
	v <- v[!suja]
	base <- A00[v$ih] - rowSums(Gacc[v$ih, pleno_niv >= E$d_e & niveis_cadeia(niv), drop = FALSE])
	v[, `:=`(ev = e, janela = jn, lacc = log1p(pmax(base, 0)), dist_se = hx$dist_se_km[ih], dist_se_trab = dist_trab[ip], renda_pc = hx$renda_pc[ih],
		subnormal = hx$subnormal[ih], densidade = hx$densidade[ih], esgoto = hx$esgoto[ih], pavim = hx$pavim[ih], cel5 = hx$cel5[ih],
		g90 = t_antes <= 90, h38 = paste(p8[ip], q8[ip], t_antes <= 90), lsal = log(pmax(rem_ref, 1)), ltenure = log1p(pmax(tenure, 0)))]
	for (oc in setdiff(OCS, names(v))) set(v, j = oc, value = NA_real_)
	BASE[[nm]] <- v[, c("ev", "janela", "ih", "ip", "h38", "cel5", "g90", "dose", "sexo", "idade", "crianca", "raca", "deficiencia", "alfabetizado",
		"anos_atualizacao", "e_b12", "m_b12", "lsal", "ltenure", "tenure", "t_antes", "lacc", "dist_se", "dist_se_trab", "renda_pc", "subnormal",
		"densidade", "esgoto", "pavim", OCS), with = FALSE]
	POP[[nm]] <- NULL
	msg(nm, ": ", nrow(BASE[[nm]]), " vínculos na janela limpa; tratados (≥1 min) ", sum(BASE[[nm]]$dose >= 1))
}
BASE <- rbindlist(BASE)
rm(TT)
gc()
BASE[is.na(anos_atualizacao), anos_atualizacao := median(BASE$anos_atualizacao, na.rm = TRUE)]
DUM <- character(0)
for (v in c("raca", "deficiencia", "alfabetizado")) {
	f <- factor(BASE[[v]])
	for (l in levels(f)[-1]) {
		nmv <- paste0(v, "_", make.names(l))
		set(BASE, j = nmv, value = as.numeric(!is.na(f) & f == l))
		DUM <- c(DUM, nmv)
	}
	set(BASE, j = paste0(v, "_na"), value = as.numeric(is.na(f)))
	DUM <- c(DUM, paste0(v, "_na"))
	set(BASE, j = v, value = NULL)
}
BASE[, `:=`(sx = fifelse(as.character(sexo) %in% c("1", "2"), as.integer(as.character(sexo)), NA_integer_), cr = as.integer(crianca))]
BASE[, `:=`(mulher = as.numeric(sx %in% 2L), cr1 = as.numeric(cr %in% 1L), cr_na = as.numeric(is.na(cr)), sexo = NULL, crianca = NULL)]
msg("base: ", nrow(BASE), " vínculos × evento × janela")

## estimação --------------------------------------------------------------------------------------------------------------
SEG <- list(todos = function(d) rep(TRUE, nrow(d)), mulheres = function(d) d$sx %in% 2L, homens = function(d) d$sx %in% 1L,
	"18 a 29" = function(d) d$idade < 30, "30 a 44" = function(d) d$idade >= 30 & d$idade < 45, "45 a 60" = function(d) d$idade >= 45,
	"com criança" = function(d) d$cr %in% 1L, "sem criança" = function(d) d$cr %in% 0L,
	"mulheres com criança" = function(d) d$sx %in% 2L & d$cr %in% 1L, "mulheres sem criança" = function(d) d$sx %in% 2L & d$cr %in% 0L,
	"homens com criança" = function(d) d$sx %in% 1L & d$cr %in% 1L, "homens sem criança" = function(d) d$sx %in% 1L & d$cr %in% 0L,
	"vínculo até 12 meses" = function(d) d$tenure <= 12, "vínculo mais de 12 meses" = function(d) d$tenure > 12)
COVS <- c("idade", "mulher", "cr1", "cr_na", DUM, "anos_atualizacao", "e_b12", "m_b12", "lsal", "ltenure", "t_antes", "lacc", "dist_se", "dist_se_trab",
	"renda_pc", "subnormal", "densidade", "esgoto", "pavim")
faixas <- list(f1a5 = c(1, 5), f5a10 = c(5, 10), f10mais = c(10, Inf))
PARES <- list(c("mulheres", "homens"), c("30 a 44", "18 a 29"), c("45 a 60", "18 a 29"), c("com criança", "sem criança"),
	c("mulheres com criança", "mulheres sem criança"), c("homens com criança", "homens sem criança"), c("vínculo até 12 meses", "vínculo mais de 12 meses"))
RES <- list()
CT <- list()
BAL <- list()
## trajetos= (24/11/2026): ate90, acima90 e todos (todos os trajetos juntos, sem a divisão em 90 min)
GGS <- list(ate90 = TRUE, acima90 = FALSE, todos = NA)[strsplit(arg_cli("trajetos", NA, "ate90,acima90,todos"), ",")[[1]]]
for (jn in janelas_x) for (gg in GGS) {
	Bj <- if (is.na(gg)) BASE[janela == jn] else BASE[janela == jn & g90 == gg]
	trajeto <- if (is.na(gg)) "todos" else if (gg) "até 90" else "acima de 90"
	for (fx in names(faixas)) {
		lim <- faixas[[fx]]
		Bf <- Bj[dose < 1 | (dose >= lim[1] & dose < lim[2])]
		RR <- list()
		if (nivel_x == "ind") {
			Bf[, t1 := as.integer(dose >= 1)]
			Bf <- pesos_att(Bf, COVS)
			if (!nrow(Bf)) next
			BAL[[length(BAL) + 1L]] <- balanco(Bf, COVS)[, `:=`(nivel = nivel_x, janela = jn, trajeto = trajeto, faixa = fx, segmento = "todos")]
		}
		for (sg in names(SEG)) {
			if (nivel_x == "ind") {
				Bs <- Bf[SEG[[sg]](Bf)]
				if (sum(Bs$t1) < 50L) next
				Bs[, w := renorm(w0, t1, ev)]
			} else {
				Ps <- Bf[SEG[[sg]](Bf)]
				Bs <- agrega_cel(Ps, c(COVS, "dose", OCS), "h38", oc_sets)
				rm(Ps)
				Bs <- Bs[dose < 1 | (dose >= lim[1] & dose < lim[2])]
				Bs[, t1 := as.integer(dose >= 1)]
				Bs <- pesos_att(Bs, COVS)
				if (sum(Bs$t1) < 30L) next
				Bs[, w := renorm(w0, t1, ev)]
				BAL[[length(BAL) + 1L]] <- balanco(Bs, COVS)[, `:=`(nivel = nivel_x, janela = jn, trajeto = trajeto, faixa = fx, segmento = sg)]
			}
			for (cj in names(oc_sets)) {
				oc <- intersect(oc_sets[[cj]], names(Bs))
				po <- if (nivel_x == "h38") Bs[[paste0("cnt_", cj)]] / Bs$n else NULL
				r <- dr_iptw(Bs, oc, COVS, po)
				if (is.null(r)) next
				for (o in seq_along(oc)) RES[[length(RES) + 1L]] <- data.table(nivel = nivel_x, janela = jn, trajeto = trajeto, faixa = fx, segmento = sg,
					outcome = oc[o], att = 100 * r$b[o], ep = 100 * r$se[o], p = 2 * pnorm(-abs(r$b[o] / r$se[o])), tratados = r$nt[o], controles = r$nc[o],
					media_tratados = 100 * r$my[o])
				RR[[paste(sg, cj)]] <- r
			}
			rm(Bs)
		}
		for (cj in names(oc_sets)) for (pp in PARES) {
			a <- RR[[paste(pp[1], cj)]]
			bb <- RR[[paste(pp[2], cj)]]
			if (is.null(a) || is.null(bb)) next
			for (o in seq_along(a$oc)) {
				z <- dif_iptw(a, bb, o)
				CT[[length(CT) + 1L]] <- data.table(nivel = nivel_x, janela = jn, trajeto = trajeto, faixa = fx, outcome = a$oc[o],
					contraste = paste(pp[1], "−", pp[2]), diferenca = 100 * z[["b"]], ep = 100 * z[["se"]], p = z[["p"]])
			}
		}
		msg(nivel_x, " ", jn, " trajeto ", trajeto, " ", fx, " feito")
		fwrite(rbindlist(RES), sprintf("outputs/final/iptw_par_%s%s_segmentos.csv", nivel_x, suf_x))
		gc()
	}
}
fwrite(rbindlist(CT), sprintf("outputs/final/iptw_par_%s%s_contrastes.csv", nivel_x, suf_x))
fwrite(rbindlist(BAL, fill = TRUE), sprintf("outputs/final/iptw_par_%s%s_balanco.csv", nivel_x, suf_x))
msg("fim iptw par ", nivel_x)

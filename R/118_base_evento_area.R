# Base por evento do desenho de área (especificação final, 24/11/2026; origem: explora_cache_area.R).
# Cada data de serviço pleno é um evento; mês de referência da janela posterior = mês anterior ao serviço pleno do evento;
# placebo = 25 meses antes da primeira abertura dos níveis do evento. População: CadÚnico de 18 a 60 anos com o endereço
# mais recente entre g−4 e g−1 (g = ano da coorte do evento).
# Desfechos por pessoa, h = 6, 12, 18 e 24 meses depois do mês de referência:
#   emprego_h: empregada no fim do mês ref+h menos empregada no fim do mês ref (qualquer vínculo celetista; máscaras R/89);
#   entrada_h: entre as sem vínculo em ref, entrou em algum mês de ref+1 a ref+h;
#   empregado_h e meses_h (permanência): entre as com vínculo em ref, empregada em ref+h e fração dos meses empregada;
#   salario_h: entre as empregadas em ref e em ref+h com salário válido nos dois, 100 × variação do log do salário-hora
#     real do vínculo principal (salário contratual com a correção e a faixa válida do R/05; reais de dez/2023).
# Grava (dado restrito, só nesta máquina) data/base/explora/ev_cache/area_celulas.parquet: somas e contagens por
# janela × evento × hexágono H3-9 × sexo × faixa de idade (18–29, 30–44, 45–60) × criança de 0 a 5 anos na família.
# Variantes da robustez (gravam com o sufixo base=): lag=1..4 usa só o endereço de g−lag (R5; padrão lag=0, o mais
# recente entre g−4 e g−1); horizontes=1,2,...,24 monta a curva mês a mês (R4).
# Uso: Rscript R/118_base_evento_area.R [lag=0] [horizontes=6,12,18,24] [base=]
# Saídas: data/base/explora/ev_cache/area_celulas<base>.parquet e eventos_area<base>.csv
source("R/00_ambiente_r5.R")
source("R/00_config.R")
source("R/funcoes_desenhos.R")
suppressPackageStartupMessages({
	library(data.table)
})
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
k <- as.integer(arg_cli("lag", NA, 0L))
idade <- 18L
H <- as.integer(strsplit(arg_cli("horizontes", NA, "6,12,18,24"), ",")[[1]])
suf_base <- arg_cli("base", NA, "")
source("R/funcoes_evento.R")
niv <- le_niveis()
ids <- le_ganhos(60L)$id
dd <- "data/base/explora/ev_cache"
dir.create(dd, showWarnings = FALSE, recursive = TRUE)
EVT <- niv[coorte %in% coortes_plano, .(ano = coorte[1], m_e = mes_idx(pleno[1]), d_e = as.integer(pleno[1]), o_min = min(mes_idx(abertura)),
	niveis = paste(nivel, collapse = ",")), by = .(pleno)][order(pleno)]
EVT[, `:=`(ev = seq_len(.N), ref_pos = m_e - 1L, ref_pre = o_min - 25L)]
EVT <- EVT[ref_pre >= 1L]
print(EVT)
fwrite(EVT, file.path(dd, sprintf("eventos_area%s.csv", suf_base)))

CEL <- list()
XC <- list()
for (e in EVT$ev) {
	E <- EVT[ev == e]
	kk <- as.character(E$ano)
	if (is.null(XC[[kk]])) {
		XC <- list()
		gc()
		x <- pop_coorte(k, E$ano, idade, ids)
		x <- marca_crianca(x, E$ano, k)
		x <- junta_pa(x, 2011:2025, "emp_mask")
		x[, `:=`(sx = fifelse(as.character(sexo) %in% c("1", "2"), as.integer(as.character(sexo)), NA_integer_),
			id3 = fifelse(idade < 30, 1L, fifelse(idade < 45, 2L, 3L)), cr = fifelse(is.na(crianca), NA_integer_, as.integer(crianca)))]
		XC[[kk]] <- x
		rm(x)
	}
	x <- XC[[kk]]
	for (jn in c("pos", "pre")) {
		ref <- E[[paste0("ref_", jn)]]
		e0 <- emp_mes(x, ref)
		s0 <- lsalh_mes(x$cpf_num, ref)
		Y <- list(n_todos = rep(1, nrow(x)), n_sem = as.numeric(!e0), n_com = as.numeric(e0))
		for (h in H) {
			if (ref + h > 180L) next
			eh <- emp_mes(x, ref + h)
			Y[[paste0("emprego_", h)]] <- as.numeric(eh) - as.numeric(e0)
			Y[[paste0("entrada_", h)]] <- fifelse(e0, 0, as.numeric(Reduce(`|`, lapply((ref + 1L):(ref + h), function(m) emp_mes(x, m)))))
			Y[[paste0("empregado_", h)]] <- fifelse(e0, as.numeric(eh), 0)
			Y[[paste0("meses_", h)]] <- fifelse(e0, Reduce(`+`, lapply((ref + 1L):(ref + h), function(m) as.numeric(emp_mes(x, m)))) / h, 0)
			sh <- lsalh_mes(x$cpf_num, ref + h)
			ds <- 100 * (sh - s0)
			okw <- e0 & eh & !is.na(ds)
			Y[[paste0("n_salario_", h)]] <- as.numeric(okw)
			Y[[paste0("salario_", h)]] <- fifelse(okw, ds, 0)
		}
		Y <- as.data.table(Y)
		Y[, `:=`(ih = x$ih, sx = x$sx, id3 = x$id3, cr = x$cr)]
		CEL[[paste(jn, e)]] <- Y[, lapply(.SD, sum), by = .(ih, sx, id3, cr)][, `:=`(janela = jn, ev = e)]
		msg("evento ", e, " (", E$pleno, ", coorte ", E$ano, ") ", jn, ": ", nrow(x), " pessoas; empregadas em ref ", round(100 * mean(e0), 1),
			"%; salário válido em ref ", round(100 * mean(!is.na(s0[e0])), 1), "% das empregadas; ", nrow(CEL[[paste(jn, e)]]), " células")
		rm(Y)
		gc()
	}
}
CEL <- rbindlist(CEL, fill = TRUE)
arrow::write_parquet(CEL, file.path(dd, sprintf("area_celulas%s.parquet", suf_base)))
msg("fim cache área: ", nrow(CEL), " células")

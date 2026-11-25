# Base por evento do desenho de tempo de viagem (especificação final, 24/11/2026). Versão limpa, sem remendos: a montagem
# dos vínculos, pares, tempos e ganhos por evento está em R/evento_par_monta.R (derivada do R/100, que continua como está
# para o material de coortes anuais); este script agrega em células e grava (origem da agregação: explora_cache_par.R).
# Grava (dado restrito, só nesta máquina), em data/base/explora/ev_cache/:
#   par_celulas<base>.parquet: somas por célula = janela × evento × par casa H3-9 × trabalho H3-9 × sexo × faixa de idade ×
#     criança × tempo de vínculo: n (vínculos), cnt_h (vínculos com desfecho observado em h) e as somas dos desfechos;
#   par_pares<base>.parquet: para cada par, casa, trabalho, pais H3-8, hexágono de casa (ih) e o ganho de tempo de cada nível;
#   eventos<base>.csv e par_checagem<base>.csv.
# Desfechos, h = 6, 12, 18, 24: qualquer saída, demissão, sem justa causa, justa causa, pedido; empregado no fim do mês
# ref+h e fração dos meses empregado (máscaras do R/89); destinos de quem pediu até h (por tempo de transporte, salário e
# rapidez). Pares com tempo antes de 0 a 241 min ficam na base (o principal usa tempo > 0).
# Variantes da robustez (gravam com o sufixo base=): lag=1..4 (R5, endereço de g−lag) e horizontes=1,...,24 (R4).
# Uso: Rscript R/119_base_evento_par.R [lag=0] [horizontes=6,12,18,24] [base=]
modo_par <- "cache"
source("R/evento_par_monta.R")
suf_base <- arg_cli("base", NA, "")
dd <- "data/base/explora/ev_cache"
dir.create(dd, showWarnings = FALSE, recursive = TRUE)
rede_antes_ev <- function(e) {
	Lg <- strsplit(EVT[coorte == e, niveis], ",")[[1]]
	niv$sem[match(Lg[which.max(as.integer(sub("A", "", Lg)))], niv$nivel)]
}
rede_ref <- function(e, jn) {
	Lg <- strsplit(EVT[coorte == e, niveis], ",")[[1]]
	o <- as.integer(sub("A", "", Lg))
	if (jn == "pos") niv$com[match(Lg[which.max(o)], niv$nivel)] else niv$sem[match(Lg[which.min(o)], niv$nivel)]
}
tempo_novo <- function(rd, casa, trab_n) {
	out <- rep(NA_real_, length(casa))
	ok <- !is.na(trab_n) & !is.na(casa)
	if (!any(ok)) return(out)
	alvo <- unique(trab_n[ok])
	m <- as.data.table(dplyr::collect(dplyr::filter(arrow::open_dataset(file.path("data/redes/familia_a", sprintf("ttm_%s.parquet", rd))), to_id %in% alvo)))
	out[ok] <- m[data.table(f = casa[ok], t = trab_n[ok]), on = .(from_id = f, to_id = t), mult = "first"]$travel_time_p50
	out
}
base_oc <- c("qualquer", "demissao", "sem_justa", "justa", "pedido", "empregado", "meses")
dest_oc <- c("ped_curto", "ped_naocurto", "ped_semloc", "ped_semnovo", "ped_salmaior", "ped_salnaomaior", "ped_novo3", "ped_novo4a12")
oc_all <- as.vector(outer(c(base_oc, dest_oc), H, paste, sep = "_"))
CEL <- list()
CHK <- list()
for (nm in names(POP)) {
	v <- POP[[nm]]
	jn <- sub(" .*", "", nm)
	e <- v$coorte[1]
	ipx <- match(paste(v$h3_09, v$trab), chave)
	ta <- TT[cbind(ipx, match(rede_antes_ev(e), redes))]
	ok <- !is.na(ta) & ta >= 0 & ta <= 241
	v <- copy(v[ok])
	v[, `:=`(ip = ipx[ok], t_antes = ta[ok])]
	## destino de quem pediu demissão
	rd <- rede_ref(e, jn)
	t_old <- TT[cbind(v$ip, match(rd, redes))]
	tn <- tempo_novo(rd, v$h3_09, v$trab_n)
	novo <- v$dest %in% c("novo", "semloc")
	curto <- fifelse(!is.na(tn), is.na(t_old) | tn < t_old, fifelse(!is.na(t_old) & t_old <= 90, FALSE, v$d_new < v$d_old))
	salm <- !is.na(v$w_rat) & v$w_rat > 1.05
	rapido <- !is.na(v$m_novo) & v$m_novo <= 3
	cls <- list(ped_curto = v$dest %in% "novo" & curto %in% TRUE, ped_naocurto = v$dest %in% "novo" & curto %in% FALSE,
		ped_semloc = v$dest %in% "semloc" | (v$dest %in% "novo" & is.na(curto)), ped_semnovo = v$dest %in% "semnovo",
		ped_salmaior = novo & salm, ped_salnaomaior = novo & !salm, ped_novo3 = novo & rapido, ped_novo4a12 = novo & !rapido)
	for (h in H) {
		p <- v[[paste0("pedido_", h)]]
		for (cn in names(cls)) set(v, j = paste0(cn, "_", h), value = p * cls[[cn]])
		for (b in c("empregado", "meses")) if (!paste0(b, "_", h) %in% names(v)) set(v, j = paste0(b, "_", h), value = NA_real_)
	}
	q <- which(v[[paste0("pedido_", max(H))]] %in% 1)
	CHK[[nm]] <- data.table(janela = jn, evento = e, rede = rd, vinculos = nrow(v), pedidos = length(q), novo_12m = sum(novo[q]),
		com_local = sum(v$dest[q] %in% "novo"), tempo_ate90 = sum(!is.na(tn[q]) & v$dest[q] %in% "novo"), curto = sum(cls$ped_curto[q]),
		min_poupados_mediana = median((t_old - tn)[q][v$dest[q] %in% "novo"], na.rm = TRUE), salmaior = sum(cls$ped_salmaior[q]),
		com_salario = sum(novo[q] & !is.na(v$w_rat[q])), novo_3m = sum(cls$ped_novo3[q]), sem_destino = sum(is.na(v$dest[q])))
	## grupos
	v[, `:=`(sx = fifelse(as.character(sexo) %in% c("1", "2"), as.integer(as.character(sexo)), NA_integer_),
		id3 = fifelse(idade < 30, 1L, fifelse(idade < 45, 2L, 3L)), cr = fifelse(is.na(crianca), NA_integer_, as.integer(crianca)),
		tn2 = fifelse(is.na(tenure), NA_integer_, fifelse(tenure <= 12, 1L, 2L)))]
	cnt <- lapply(H, function(h) as.integer(!is.na(v[[paste0("demissao_", h)]])))
	names(cnt) <- paste0("cnt_", H)
	for (cc in names(cnt)) set(v, j = cc, value = cnt[[cc]])
	CEL[[nm]] <- v[, c(list(n = .N), lapply(.SD, function(z) sum(z, na.rm = TRUE))), by = .(ip, ih, sx, id3, cr, tn2, t_antes),
		.SDcols = c(names(cnt), oc_all)][, `:=`(janela = jn, evento = e)]
	msg(nm, ": ", nrow(v), " vínculos, ", nrow(CEL[[nm]]), " células")
	rm(v)
	POP[[nm]] <- NULL
	gc()
}
CEL <- rbindlist(CEL)
arrow::write_parquet(CEL, file.path(dd, sprintf("par_celulas%s.parquet", suf_base)))
up <- sort(unique(CEL$ip))
PR <- data.table(ip = up, casa = pares$casa[up], trab = pares$trab[up])
PR[, `:=`(casa8 = pai_h3(casa, 8), trab8 = pai_h3(trab, 8), trab7 = trab_h7[up])]
PR <- cbind(PR, as.data.table(DL[up, , drop = FALSE]))
arrow::write_parquet(PR, file.path(dd, sprintf("par_pares%s.parquet", suf_base)))
fwrite(EVT, file.path(dd, sprintf("eventos%s.csv", suf_base)))
fwrite(rbindlist(CHK), file.path(dd, sprintf("par_checagem%s.csv", suf_base)))
msg("fim cache par: ", nrow(CEL), " células, ", length(up), " pares, ", sum(CEL$n), " vínculos")

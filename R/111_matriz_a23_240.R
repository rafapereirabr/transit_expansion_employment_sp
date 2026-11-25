# EXPLORATÓRIO (17/11/2026, pedido do pesquisador): matriz completa da rede de fim de 2012 (A23) até 240 min, para
# estimar o decaimento da acessibilidade gravitacional com todos os trajetos (R/107 limite=240). O modelo de faixas do
# R/107 precisa, para cada origem, dos empregos alcançáveis em cada faixa de tempo; acima de 90 min isso exige a matriz
# completa, e não só os pares do R/110. Mesmos parâmetros do R/110: TRANSIT, partida 01/10/2025 06:50, janela de 15 min,
# percentil 50, 1 sorteio por minuto, semente 1, r5r 2.5.0 com a rede já montada. Roteia em lotes de origens e agrega na
# hora (retomável: lotes já gravados são pulados).
# Saídas (data/base/acesso_grav/, restrito):
# - j_min_A23_<limite>.parquet: from_id, t (minutos), empregos (empregos de 2015 sem as duas sedes da Sé, como no R/107);
# - t_pares2012_A23_<limite>.parquet: casa, trab, t (pares casa → trabalho dos empregados em 31/12/2012, como no R/107).
# Uso (R 4.6; R5R_VERSAO=2.5.0): Rscript R/111_matriz_a23_240.R [limite=240] [nucleos=12] [ram=64] [lote=500] [teste=0]
# (teste=N roteia só as N primeiras origens e grava com o sufixo _teste)

source("R/00_ambiente_r5.R")
source("R/00_config.R")
source("R/funcoes_desenhos.R")
stopifnot("rode com R5R_VERSAO=2.5.0" = versao_r5r == "2.5.0")
limite <- as.integer(arg_cli("limite", NA, 240L))
nucleos <- as.integer(arg_cli("nucleos", NA, 12L))
ram <- as.integer(arg_cli("ram", NA, 64L))
lote <- as.integer(arg_cli("lote", NA, 500L))
teste <- as.integer(arg_cli("teste", NA, 0L))
options(java.parameters = c(paste0("-Xmx", ram, "G"), paste0("-XX:ActiveProcessorCount=", nucleos)))
suppressPackageStartupMessages({
	library(data.table)
	library(sf)
	library(r5r)
})
stopifnot(packageVersion("r5r") == "2.5.0")
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
dir_art <- file.path(dir_arthur, "temp")
dir_g <- file.path(dir_base, "acesso_grav")
suf <- paste0("A23_", limite, if (teste > 0L) "_teste" else "")
f_j <- file.path(dir_g, sprintf("j_min_%s.parquet", suf))
f_t <- file.path(dir_g, sprintf("t_pares2012_%s.parquet", suf))
dir_tmp <- file.path(dir_g, paste0("lotes_", suf))
dir.create(dir_tmp, showWarnings = FALSE, recursive = TRUE)

## empregos (como no R/107) e pares casa → trabalho dos empregados em 31/12/2012 (endereço de 2012)
emp <- as.data.table(arrow::read_parquet(file.path(dir_base, "empregos_h3_2015.parquet")))
se <- fread("data/base/acesso_cortes/hex_se.csv")$to_id
emp <- emp[!h3_09 %in% se & empregos > 0, .(h3_09, empregos)]
ids <- le_ganhos(60L)$id
x12 <- pop_coorte(1L, 2013L, 18L, ids)
x12 <- junta_pa(x12, 2012L, c("emp31", "estab31"))
w <- as.data.table(arrow::read_parquet(file.path(dir_art, "rais_workplaces_2012.parquet"), col_select = c("id_estab", "cei_vinc", "work_h3_id", "work_desvio", "work_geo_valid")))
w <- w[work_geo_valid %in% TRUE & !is.na(work_desvio) & work_desvio <= 500 & !is.na(work_h3_id)]
w <- w[order(!(is.na(cei_vinc) | cei_vinc %in% c("", "0")))][!duplicated(id_estab)]
fl <- unique(x12[emp31_2012 == 1L, .(casa = h3_09, trab = w$work_h3_id[match(estab31_2012, w$id_estab)])][!is.na(trab)])
rm(x12, w)
gc()
msg("pares casa → trabalho de 2012: ", nrow(fl))

## origens (todas as da família A) e destinos (a grade)
grade <- arrow::open_dataset(file.path(dir_insumos, "ttm_transit_2025.parquet")) |> dplyr::distinct(from_id) |> dplyr::collect() |> dplyr::pull(from_id)
orig <- sort(arrow::read_parquet(file.path("data/redes", "origens_familia_a.parquet"))$id)
if (teste > 0L) orig <- orig[seq_len(teste)]
pontos <- function(ids) st_sf(id = ids, geometry = st_geometry(h3jsr::cell_to_point(ids, simple = FALSE)))
dest <- pontos(grade)
blocos <- split(orig, ceiling(seq_along(orig) / lote))
partida <- as.POSIXct("2025-10-01 06:50:00", tz = "America/Sao_Paulo")
msg("origens: ", length(orig), " em ", length(blocos), " lotes; destinos: ", length(grade))
t0 <- Sys.time()
rede <- r5r::build_network(file.path("data/redes/familia_a", "A23"), verbose = FALSE, overwrite = FALSE)
for (b in seq_along(blocos)) {
	f_b <- file.path(dir_tmp, sprintf("j_%03d.parquet", b))
	if (file.exists(f_b)) next
	m <- as.data.table(r5r::travel_time_matrix(rede, origins = pontos(blocos[[b]]), destinations = dest, mode = "TRANSIT", departure_datetime = partida,
		time_window = 15L, max_trip_duration = limite, draws_per_minute = 1L, n_threads = nucleos, progress = FALSE, seed = 1L))
	jm <- m[emp, on = .(to_id = h3_09), nomatch = 0L][, .(empregos = sum(empregos)), by = .(from_id, t = travel_time_p50)]
	tp <- m[fl[casa %in% blocos[[b]]], on = .(from_id = casa, to_id = trab), nomatch = 0L][, .(casa = from_id, trab = to_id, t = travel_time_p50)]
	## o lote só conta como feito quando o arquivo j_ é gravado (por último)
	arrow::write_parquet(tp, file.path(dir_tmp, sprintf("t_%03d.parquet", b)))
	arrow::write_parquet(jm, f_b)
	rm(m, jm, tp)
	gc()
	msg("lote ", b, " de ", length(blocos), " (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")
}
r5r::stop_r5(rede)
le_lotes <- function(p) rbindlist(lapply(list.files(dir_tmp, pattern = p, full.names = TRUE), function(f) as.data.table(arrow::read_parquet(f))))
J <- le_lotes("^j_")
TP <- le_lotes("^t_")
arrow::write_parquet(J, f_j)
arrow::write_parquet(TP, f_t)
unlink(dir_tmp, recursive = TRUE)
ja <- J[, .(a90 = sum(empregos[t <= 90]), tot = sum(empregos)), by = from_id]
msg("origens com tempo: ", nrow(ja), "; empregos alcançáveis por origem (mediana): até 90 min ", round(median(ja$a90)), ", até ", limite, " min ", round(median(ja$tot)))
fo <- fl[casa %in% orig]
msg("pares de 2012 (origens roteadas): ", nrow(fo), "; com trabalho na grade: ", nrow(fo[trab %in% grade]), "; com tempo até ", limite, " min: ", nrow(TP),
	" (até 90 min: ", TP[t <= 90, .N], ")")
msg("fim")

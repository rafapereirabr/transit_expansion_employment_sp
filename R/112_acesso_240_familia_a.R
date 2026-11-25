# EXPLORATÓRIO (17/11/2026, pedido do pesquisador): matrizes completas até 240 min nas 27 redes da família A, guardando
# só os empregos alcançáveis por minuto de cada origem (empregos sem as duas sedes da Sé; ano dos empregos por argumento, 2012 desde 24/11/2026). Serve
# para a acessibilidade gravitacional sem o corte em 90 min (R/107 limite=240 acesso=completo) e, se for preciso, para
# acessos acumulados em qualquer corte até 240 min. Mesmos parâmetros do R/110 e do R/111: TRANSIT, partida 01/10/2025
# 06:50, janela de 15 min, percentil 50, 1 sorteio por minuto, semente 1, r5r 2.5.0 com as redes já montadas. A rede A23
# reaproveita o arquivo do R/111 (mesma rede, mesmas origens, destinos e parâmetros).
# Pode rodar em mais de um processo ao mesmo tempo (ordem=asc ou desc): cada rede é reservada por uma pasta de trava,
# apagada ao fim da rede (se um processo cair, apagar a trava à mão). Retomável por rede e por lote.
# Uso (R 4.6; R5R_VERSAO=2.5.0): Rscript R/112_acesso_240_familia_a.R [limite=240] [nucleos=12] [ram=48] [lote=500] [ordem=asc] [teste=0]
#      [empregos=2012] [matriz=1]
# (teste=N roteia só as N primeiras origens da rede A00 e grava com o sufixo _teste)
# Saída: data/base/acesso_240/jmin_<rede>.parquet (from_id, t, empregos)

source("R/00_ambiente_r5.R")
source("R/00_config.R")
stopifnot("rode com R5R_VERSAO=2.5.0" = versao_r5r == "2.5.0")
limite <- as.integer(arg_cli("limite", NA, 240L))
nucleos <- as.integer(arg_cli("nucleos", NA, 12L))
ram <- as.integer(arg_cli("ram", NA, 48L))
lote <- as.integer(arg_cli("lote", NA, 500L))
ordem <- arg_cli("ordem", NA, "asc")
teste <- as.integer(arg_cli("teste", NA, 0L))
stopifnot(ordem %in% c("asc", "desc"))
options(java.parameters = c(paste0("-Xmx", ram, "G"), paste0("-XX:ActiveProcessorCount=", nucleos)))
suppressPackageStartupMessages({
	library(data.table)
	library(sf)
	library(r5r)
})
stopifnot(packageVersion("r5r") == "2.5.0")
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
dir_fa <- "data/redes/familia_a"
## 24/11/2026: ano dos empregos como argumento (decisão: 2012, último ano completo antes da primeira abertura). Com 2015,
## grava em acesso_240 como antes; com outro ano, em acesso_240_emp<ano>. Com matriz=1 (padrão), guarda também a matriz
## completa de tempos (data/base/matriz_240/<rede>/lote_NNN.parquet: from_id, to_id, t), que não depende dos empregos,
## para que trocar os empregos não exija rotear de novo.
ano_emp <- as.integer(arg_cli("empregos", NA, 2012L))
guarda_matriz <- as.integer(arg_cli("matriz", NA, 1L)) == 1L
dir_out <- file.path(dir_base, if (ano_emp == 2015L) "acesso_240" else sprintf("acesso_240_emp%d", ano_emp))
dir.create(dir_out, showWarnings = FALSE)
dir_mat <- file.path(dir_base, "matriz_240")
if (guarda_matriz) dir.create(dir_mat, showWarnings = FALSE)

emp <- as.data.table(arrow::read_parquet(file.path(dir_base, sprintf("empregos_h3_%d.parquet", ano_emp))))
se <- fread("data/base/acesso_cortes/hex_se.csv")$to_id
emp <- emp[!h3_09 %in% se & empregos > 0, .(h3_09, empregos)]
grade <- arrow::open_dataset(file.path(dir_insumos, "ttm_transit_2025.parquet")) |> dplyr::distinct(from_id) |> dplyr::collect() |> dplyr::pull(from_id)
orig <- sort(arrow::read_parquet(file.path("data/redes", "origens_familia_a.parquet"))$id)
redes <- sprintf("A%02d", 0:26)
if (ordem == "desc") redes <- rev(redes)
suf <- ""
if (teste > 0L) {
	orig <- orig[seq_len(teste)]
	redes <- "A00"
	suf <- "_teste"
}
pontos <- function(ids) st_sf(id = ids, geometry = st_geometry(h3jsr::cell_to_point(ids, simple = FALSE)))
dest <- pontos(grade)
blocos <- split(orig, ceiling(seq_along(orig) / lote))
partida <- as.POSIXct("2025-10-01 06:50:00", tz = "America/Sao_Paulo")
msg("origens: ", length(orig), " em ", length(blocos), " lotes; destinos: ", length(grade), "; redes: ", paste(redes, collapse = ","),
	"; empregos de ", ano_emp, " (", format(sum(emp$empregos), big.mark = " "), " sem as sedes da Sé); saída ", dir_out, if (guarda_matriz) "; guarda a matriz" else "")

## travas deixadas por uma rodada anterior deste mesmo processo (mesma ordem) que caiu
for (tv in list.files(dir_out, pattern = "^trava_", full.names = TRUE)) {
	if (file.exists(file.path(tv, paste0("dono_", ordem, suf)))) {
		unlink(tv, recursive = TRUE)
		msg("trava antiga removida: ", basename(tv))
	}
}
## a rede A23 já foi roteada no R/111 com os mesmos parâmetros
f23 <- file.path(dir_base, "acesso_grav", sprintf("j_min_A23_%d.parquet", limite))
s23 <- file.path(dir_out, "jmin_A23.parquet")
if (ano_emp == 2015L && teste == 0L && !file.exists(s23) && file.exists(f23)) {
	file.copy(f23, s23)
	msg("A23: copiada do R/111")
}
for (r in redes) {
	saida <- file.path(dir_out, sprintf("jmin_%s%s.parquet", r, suf))
	if (file.exists(saida)) next
	trava <- file.path(dir_out, paste0("trava_", r, suf))
	if (!dir.create(trava, showWarnings = FALSE)) {
		msg(r, ": reservada por outro processo, pulando")
		next
	}
	file.create(file.path(trava, paste0("dono_", ordem, suf)))
	stopifnot("rede sem network.dat" = file.exists(file.path(dir_fa, r, "network.dat")))
	dir_tmp <- file.path(dir_out, paste0("lotes_", r, suf))
	dir.create(dir_tmp, showWarnings = FALSE)
	t0 <- Sys.time()
	rede <- r5r::build_network(file.path(dir_fa, r), verbose = FALSE, overwrite = FALSE)
	for (b in seq_along(blocos)) {
		f_b <- file.path(dir_tmp, sprintf("j_%03d.parquet", b))
		if (file.exists(f_b)) next
		m <- as.data.table(r5r::travel_time_matrix(rede, origins = pontos(blocos[[b]]), destinations = dest, mode = "TRANSIT", departure_datetime = partida,
			time_window = 15L, max_trip_duration = limite, draws_per_minute = 1L, n_threads = nucleos, progress = FALSE, seed = 1L))
		if (guarda_matriz) {
			dm <- file.path(dir_mat, paste0(r, suf))
			dir.create(dm, showWarnings = FALSE)
			arrow::write_parquet(m[, .(from_id, to_id, t = as.integer(travel_time_p50))], file.path(dm, sprintf("lote_%03d.parquet", b)))
		}
		jm <- m[emp, on = .(to_id = h3_09), nomatch = 0L][, .(empregos = sum(empregos)), by = .(from_id, t = travel_time_p50)]
		arrow::write_parquet(jm, f_b)
		rm(m, jm)
		gc()
		if (b %% 5L == 0L) msg(r, ": lote ", b, " de ", length(blocos), " (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")
	}
	r5r::stop_r5(rede)
	J <- rbindlist(lapply(list.files(dir_tmp, pattern = "^j_", full.names = TRUE), function(f) as.data.table(arrow::read_parquet(f))))
	arrow::write_parquet(J, saida)
	unlink(dir_tmp, recursive = TRUE)
	unlink(trava, recursive = TRUE)
	ja <- J[, .(a90 = sum(empregos[t <= 90]), tot = sum(empregos)), by = from_id]
	msg(r, ": ", nrow(ja), " origens; empregos alcançáveis por origem (mediana): até 90 min ", round(median(ja$a90)), ", até ", limite, " min ", round(median(ja$tot)),
		"; ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
	rm(J, ja)
	gc()
}
msg("fim")

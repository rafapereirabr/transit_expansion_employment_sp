# EXPLORATÓRIO (17/11/2026, pedido do pesquisador): tempos de viagem até 240 min dos pares casa → trabalho do T3 que
# passam de 90 min (lista do R/109), nas 27 redes da família A. Mesmos parâmetros do R/42 e do R/62: TRANSIT, partida
# 01/10/2025 06:50, janela de 15 min, percentil 50, semente 1, r5r 2.5.0 com as redes já montadas; 1 sorteio por minuto
# (como o R/62; os tempos de um par em todas as redes saem com os mesmos parâmetros). Para não montar a matriz cheia
# (origens × destinos), roteia em lotes de origens, cada lote só para os destinos dos seus pares. Retomável: redes já
# gravadas são puladas; lotes já gravados também.
#
# Uso (R 4.6; R5R_VERSAO=2.5.0): Rscript R/110_rotear_pares_t3.R [limite=240] [nucleos=12] [ram=64] [lote=800] [redes=A00,...]
# Saída: data/base/ttm_longos_t3/ttm_longos_<rede>.parquet (from_id, to_id, travel_time_p50)

source("R/00_ambiente_r5.R")
source("R/00_config.R")
stopifnot("rode com R5R_VERSAO=2.5.0" = versao_r5r == "2.5.0")
limite <- as.integer(arg_cli("limite", NA, 240L))
nucleos <- as.integer(arg_cli("nucleos", NA, 12L))
ram <- as.integer(arg_cli("ram", NA, 64L))
lote <- as.integer(arg_cli("lote", NA, 800L))
options(java.parameters = c(paste0("-Xmx", ram, "G"), paste0("-XX:ActiveProcessorCount=", nucleos)))
suppressPackageStartupMessages({
	library(data.table)
	library(sf)
	library(r5r)
})
stopifnot(packageVersion("r5r") == "2.5.0")
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
dir_fa <- "data/redes/familia_a"
dir_out <- file.path(dir_base, "ttm_longos_t3")
dir.create(dir_out, showWarnings = FALSE)
pares <- as.data.table(arrow::read_parquet(file.path(dir_base, "t3_pares_acima90.parquet")))
## só pares com casa entre as origens das redes e trabalho na grade de destinos (como no R/62)
grade <- arrow::open_dataset(file.path(dir_insumos, "ttm_transit_2025.parquet")) |> dplyr::distinct(from_id) |> dplyr::collect() |> dplyr::pull(from_id)
origens <- arrow::read_parquet(file.path("data/redes", "origens_familia_a.parquet"))$id
n0 <- nrow(pares)
pares <- pares[id_casa %in% origens & id_trab %in% grade]
msg("pares: ", n0, "; com casa e trabalho na grade: ", nrow(pares), " (", sum(pares$vinculos), " vínculos)")
redes <- strsplit(arg_cli("redes", NA, paste(sprintf("A%02d", 0:26), collapse = ",")), ",")[[1]]
partida <- as.POSIXct("2025-10-01 06:50:00", tz = "America/Sao_Paulo")
pontos <- function(ids) st_sf(id = ids, geometry = st_geometry(h3jsr::cell_to_point(ids, simple = FALSE)))
orig <- sort(unique(pares$id_casa))
blocos <- split(orig, ceiling(seq_along(orig) / lote))
for (r in redes) {
	saida <- file.path(dir_out, sprintf("ttm_longos_%s.parquet", r))
	if (file.exists(saida)) next
	stopifnot("rede sem network.dat" = file.exists(file.path(dir_fa, r, "network.dat")))
	dir_tmp <- file.path(dir_out, paste0("lotes_", r))
	dir.create(dir_tmp, showWarnings = FALSE)
	t0 <- Sys.time()
	rede <- r5r::build_network(file.path(dir_fa, r), verbose = FALSE, overwrite = FALSE)
	for (b in seq_along(blocos)) {
		f_b <- file.path(dir_tmp, sprintf("lote_%03d.parquet", b))
		if (file.exists(f_b)) next
		p <- pares[id_casa %in% blocos[[b]], .(id_casa, id_trab)]
		m <- r5r::travel_time_matrix(rede, origins = pontos(unique(p$id_casa)), destinations = pontos(unique(p$id_trab)), mode = "TRANSIT", departure_datetime = partida,
			time_window = 15L, max_trip_duration = limite, draws_per_minute = 1L, n_threads = nucleos, progress = FALSE, seed = 1L)
		m <- as.data.table(m)[p, on = .(from_id = id_casa, to_id = id_trab), nomatch = 0L]
		arrow::write_parquet(m[, .(from_id, to_id, travel_time_p50)], f_b)
		rm(m)
		gc()
		if (b %% 5L == 0L) msg(r, ": lote ", b, " de ", length(blocos), " (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")
	}
	r5r::stop_r5(rede)
	todos <- rbindlist(lapply(list.files(dir_tmp, full.names = TRUE), function(f) as.data.table(arrow::read_parquet(f))))
	arrow::write_parquet(todos, saida)
	unlink(dir_tmp, recursive = TRUE)
	msg(r, ": ", nrow(pares), " pares pedidos, ", nrow(todos), " com tempo até ", limite, " min, em ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
	rm(todos)
	gc()
}
msg("fim")

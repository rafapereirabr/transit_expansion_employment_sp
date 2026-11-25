# Matrizes de tempo de viagem até 240 min das 27 redes da família A (24/11/2026, pedido do pesquisador).
# Substitui o R/112, que roteava até 240 min mas guardava só os empregos alcançáveis por minuto (com 1 sorteio por minuto).
# Mesmos parâmetros da matriz de até 90 min do R/42: TRANSIT, partida 01/10/2025 06:50, janela de 15 min, percentil 50,
# 5 sorteios por minuto, semente 1, r5r 2.5.0 com as redes já montadas; origens = data/redes/origens_familia_a.parquet
# (10.613 hexágonos); destinos = a grade (hexágonos de origem da matriz de 2025 do pipeline do Arthur). Limite: 240 min.
#
# Dois modos, que devem produzir a mesma matriz:
#   modo=zero         roteia todos os pares até 240 min (o cálculo de referência, do zero);
#   modo=complemento  parte da matriz de até 90 min do R/42 (ttm_<rede>.parquet) e acrescenta só os pares que faltam nela,
#                     roteados até 240 min com os mesmos parâmetros. Lote a lote, confere que os pares de até 90 min do
#                     roteamento novo coincidem com os da matriz do R/42 (mesmos pares e mesmos tempos) e grava a conferência.
# A matriz não depende dos empregos: as acessibilidades (cumulativas e gravitacional) são calculadas dela à parte.
# Pode rodar em mais de um processo (ordem=asc ou desc): cada rede é reservada por uma pasta de trava, apagada ao fim da rede
# (se um processo cair, apagar a trava à mão). Retomável por rede e por lote.
#
# Uso (R 4.6; R5R_VERSAO=2.5.0): Rscript R/130_matriz_240_familia_a.R [modo=zero|complemento] [limite=240] [nucleos=12]
#        [ram=48] [lote=500] [ordem=asc] [redes=A00,A01,...] [teste=0]
#   (teste=N roteia só as N primeiras origens da rede A00 e grava com o sufixo _teste)
# Saídas: data/redes/familia_a/ttm240_<rede>.parquet (from_id, to_id, travel_time_p50; mesmo formato do ttm_<rede>)
#         data/redes/familia_a/ttm240_checagem.csv (por rede e lote: pares de até 90 min do roteamento novo, pares da matriz
#         de 90 min, pares só num dos dois, pares com tempo diferente, pares acrescentados)

source("R/00_ambiente_r5.R")
source("R/00_config.R")
stopifnot("rode com R5R_VERSAO=2.5.0" = versao_r5r == "2.5.0")
modo <- arg_cli("modo", NA, "zero")
limite <- as.integer(arg_cli("limite", NA, 240L))
nucleos <- as.integer(arg_cli("nucleos", NA, 12L))
ram <- as.integer(arg_cli("ram", NA, 48L))
lote <- as.integer(arg_cli("lote", NA, 500L))
ordem <- arg_cli("ordem", NA, "asc")
teste <- as.integer(arg_cli("teste", NA, 0L))
stopifnot(modo %in% c("zero", "complemento"), ordem %in% c("asc", "desc"))
options(java.parameters = c(paste0("-Xmx", ram, "G"), paste0("-XX:ActiveProcessorCount=", nucleos)))
suppressPackageStartupMessages({
	library(data.table)
	library(sf)
	library(r5r)
})
stopifnot(packageVersion("r5r") == "2.5.0")
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)

## parâmetros do R/42 (não mudar: a matriz de 90 min foi feita com eles)
semente <- 1L
sorteios <- 5L
janela <- 15L
partida <- as.POSIXct("2025-10-01 06:50:00", tz = "America/Sao_Paulo")

dir_fa <- "data/redes/familia_a"
ids_grade <- arrow::open_dataset(file.path(dir_insumos, "ttm_transit_2025.parquet")) |> dplyr::distinct(from_id) |> dplyr::collect() |> dplyr::pull(from_id)
orig <- as.data.table(arrow::read_parquet(file.path("data/redes", "origens_familia_a.parquet")))$id
redes <- strsplit(arg_cli("redes", NA, paste(sprintf("A%02d", 0:26), collapse = ",")), ",")[[1]]
if (ordem == "desc") redes <- rev(redes)
suf <- ""
if (teste > 0L) {
	orig <- orig[seq_len(teste)]
	redes <- "A00"
	suf <- "_teste"
}
pontos <- function(ids) st_sf(id = ids, geometry = st_geometry(h3jsr::cell_to_point(ids, simple = FALSE)))
dest <- pontos(ids_grade)
blocos <- split(orig, ceiling(seq_along(orig) / lote))
msg("modo ", modo, "; origens: ", length(orig), " em ", length(blocos), " lotes; destinos: ", length(ids_grade), "; limite ", limite, " min; ",
	sorteios, " sorteios por minuto; semente ", semente, "; redes: ", paste(redes, collapse = ","))
arq_chk <- file.path(dir_fa, paste0("ttm240_checagem", suf, ".csv"))

## travas deixadas por uma rodada anterior deste mesmo processo (mesma ordem) que caiu
for (tv in list.files(dir_fa, pattern = "^trava240_", full.names = TRUE)) {
	if (file.exists(file.path(tv, paste0("dono_", ordem, suf)))) {
		unlink(tv, recursive = TRUE)
		msg("trava antiga removida: ", basename(tv))
	}
}
for (r in redes) {
	saida <- file.path(dir_fa, sprintf("ttm240_%s%s.parquet", r, suf))
	if (file.exists(saida)) next
	trava <- file.path(dir_fa, paste0("trava240_", r, suf))
	if (!dir.create(trava, showWarnings = FALSE)) {
		msg(r, ": reservada por outro processo, pulando")
		next
	}
	file.create(file.path(trava, paste0("dono_", ordem, suf)))
	stopifnot("rede sem network.dat" = file.exists(file.path(dir_fa, r, "network.dat")))
	dir_tmp <- file.path(dir_fa, paste0("lotes240_", r, suf))
	dir.create(dir_tmp, showWarnings = FALSE)
	t0 <- Sys.time()
	## matriz de até 90 min do R/42, para o modo complemento
	M90 <- NULL
	if (modo == "complemento") {
		M90 <- as.data.table(arrow::read_parquet(file.path(dir_fa, sprintf("ttm_%s.parquet", r))))
		if (teste > 0L) M90 <- M90[from_id %in% orig]
		setkey(M90, from_id, to_id)
	}
	rede <- r5r::build_network(file.path(dir_fa, r), verbose = FALSE, overwrite = FALSE)
	for (b in seq_along(blocos)) {
		f_b <- file.path(dir_tmp, sprintf("m_%03d.parquet", b))
		if (file.exists(f_b)) next
		m <- as.data.table(r5r::travel_time_matrix(rede, origins = pontos(blocos[[b]]), destinations = dest, mode = "TRANSIT", departure_datetime = partida,
			time_window = janela, max_trip_duration = limite, draws_per_minute = sorteios, n_threads = nucleos, progress = FALSE, seed = semente))
		m <- m[, .(from_id, to_id, travel_time_p50 = as.integer(travel_time_p50))]
		if (modo == "complemento") {
			v <- M90[from_id %in% blocos[[b]]]
			n90 <- m[travel_time_p50 <= 90L]
			cmp <- merge(n90, v, by = c("from_id", "to_id"), all = TRUE, suffixes = c("_novo", "_r42"))
			ck <- data.table(rede = r, lote = b, pares_ate90_novo = nrow(n90), pares_r42 = nrow(v), so_no_novo = cmp[is.na(travel_time_p50_r42), .N],
				so_no_r42 = cmp[is.na(travel_time_p50_novo), .N], tempo_diferente = cmp[!is.na(travel_time_p50_novo) & !is.na(travel_time_p50_r42) &
					travel_time_p50_novo != travel_time_p50_r42, .N])
			add <- m[!v, on = .(from_id, to_id)]
			ck[, `:=`(acrescentados = nrow(add), acrescentados_ate90 = add[travel_time_p50 <= 90L, .N])]
			fwrite(ck, arq_chk, append = file.exists(arq_chk))
			arrow::write_parquet(add, f_b)
			rm(v, n90, cmp, add)
		} else {
			arrow::write_parquet(m, f_b)
		}
		rm(m)
		gc()
		if (b %% 5L == 0L || teste > 0L) msg(r, ": lote ", b, " de ", length(blocos), " (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")
	}
	r5r::stop_r5(rede)
	L <- rbindlist(lapply(list.files(dir_tmp, pattern = "^m_", full.names = TRUE), function(f) as.data.table(arrow::read_parquet(f))))
	M <- if (modo == "complemento") rbind(M90, L) else L
	rm(M90, L)
	setorder(M, from_id, to_id)
	stopifnot(!anyDuplicated(M, by = c("from_id", "to_id")))
	arrow::write_parquet(M, saida)
	msg(r, ": ", format(nrow(M), big.mark = " "), " pares até ", limite, " min (", format(M[travel_time_p50 <= 90L, .N], big.mark = " "), " até 90 min); ",
		round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
	rm(M)
	gc()
	unlink(dir_tmp, recursive = TRUE)
	unlink(trava, recursive = TRUE)
}
msg("fim")

# Executor da família A (27 redes): monta as redes R5 e calcula a acessibilidade a empregos de todas
# as origens, com o r5r 2.5.0 e semente fixa. Plano: quality_reports/plans/
# 2026-11-13_accessibility-gain-bands-design.qmd (seções "Origens: a cidade toda" e "Semente do r5r").
#
# Duas etapas, para rodar em dois processos ao mesmo tempo neste servidor (montar usa quase um núcleo;
# rotear usa os demais):
#   etapa=montar  monta, em ordem, a rede R5 de cada estado (network.dat) e grava um marcador de pronto;
#   etapa=rotear  espera o marcador de cada rede, calcula a acessibilidade e grava o resultado.
# Retomável: uma rede com resultado gravado para os mesmos GTFS (md5) e parâmetros é pulada.
#
# Origens: grade H3-9 do pipeline ∪ hexágonos com moradores do CadÚnico 2011-2023 (capital, desvio até
# 500 m), 10.613 hexágonos; destinos: a grade, com empregos formais em 31/12 do ano dos empregos.
# Partida 01/10/2025 06:50, janela de 15 min, até 90 min, percentil 50, cortes de 30, 45, 60 e 90 min.
#
# Uso (R 4.6, depois do 39 limpo; R5R_VERSAO=2.5.0):
#   Rscript R/42_rotear_familia_a.R etapa=montar
#   Rscript R/42_rotear_familia_a.R etapa=rotear [modo=matriz|acessibilidade] [semente=1] [sorteios=5]
#     [empregos=2015] [nucleos=14] [ram=96] [redes=A00,A01,...]
# Saídas: data/redes/familia_a/<rede>/ (rede R5), data/redes/familia_a/acesso_<rede>.parquet
#         (ou ttm_<rede>.parquet no modo matriz), data/redes/familia_a/registro.csv, origens em
#         data/redes/origens_familia_a.parquet

source("R/00_ambiente_r5.R")
source("R/00_config.R")
stopifnot("rode com R5R_VERSAO=2.5.0" = versao_r5r == "2.5.0")
etapa <- arg_cli("etapa", NA, "rotear")
stopifnot(etapa %in% c("montar", "rotear"))
modo <- arg_cli("modo", NA, "matriz") # decidido em 14/11/2026: matriz completa, 5 sorteios por minuto
stopifnot(modo %in% c("acessibilidade", "matriz"))
semente <- as.integer(arg_cli("semente", NA, 1L))
sorteios <- as.integer(arg_cli("sorteios", NA, 5L))
ano_emp <- as.integer(arg_cli("empregos", NA, 2015L))
nucleos <- as.integer(arg_cli("nucleos", NA, if (etapa == "montar") 2L else 14L))
ram <- as.integer(arg_cli("ram", NA, if (etapa == "montar") 48L else 96L))
options(java.parameters = c(paste0("-Xmx", ram, "G"), paste0("-XX:ActiveProcessorCount=", nucleos)))
suppressPackageStartupMessages({
	library(data.table)
	library(sf)
	library(r5r)
})
stopifnot(packageVersion("r5r") == "2.5.0")
dir_redes <- "data/redes"
dir_fa <- file.path(dir_redes, "familia_a")
dir.create(dir_fa, recursive = TRUE, showWarnings = FALSE)
log <- function(...) {
	msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [", etapa, "] ", ...)
	message(msg)
	cat(msg, "\n", file = file.path(dir_fa, sprintf("log_%s.txt", etapa)), append = TRUE)
}

## portão: checagens pré-roteamento limpas e atualizadas ---------------------------------------------------
rel_pre <- file.path(dir_redes, "checagens", "pre_roteamento.csv")
vigiados <- c(list.files(file.path(dir_redes, "gtfs"), "\\.zip$", full.names = TRUE), file.path(dir_redes, "catalogo_familia_a.csv"))
if (!file.exists(rel_pre) || any(fread(rel_pre)$status == "FALHA") || any(file.mtime(vigiados) > file.mtime(rel_pre))) {
	stop("rode R/39_checar_redes.R (sem FALHA) antes de montar ou rotear")
}
cat_a <- fread(file.path(dir_redes, "catalogo_familia_a.csv"))
redes <- strsplit(arg_cli("redes", NA, paste(cat_a$rede, collapse = ",")), ",")[[1]]
stopifnot(all(redes %in% cat_a$rede))
md5_gtfs <- function(r) unname(tools::md5sum(file.path(dir_redes, "gtfs", sprintf("trilho_%s.zip", r))))
pronto <- function(r) file.path(dir_fa, r, "PRONTO.txt")

## etapa montar --------------------------------------------------------------------------------------------
if (etapa == "montar") {
	for (r in redes) {
		d <- file.path(dir_fa, r)
		if (file.exists(pronto(r)) && readLines(pronto(r))[1] == md5_gtfs(r)) {
			log(r, ": rede já montada para este GTFS")
			next
		}
		dir.create(d, recursive = TRUE, showWarnings = FALSE)
		unlink(pronto(r))
		invisible(file.copy(c(file.path(dir_redes, "insumos", c("street_network.osm.pbf", "elevation.tif", "gtfs_sptrans_2025.zip")),
			file.path(dir_redes, "gtfs", sprintf("trilho_%s.zip", r))), d, overwrite = TRUE))
		t0 <- Sys.time()
		rede <- r5r::build_network(d, verbose = FALSE, elevation = "TOBLER", overwrite = TRUE)
		r5r::stop_r5()
		writeLines(c(md5_gtfs(r), format(Sys.time())), pronto(r))
		log(r, ": rede montada em ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
	}
	log("todas as redes montadas")
	quit(status = 0)
}

## etapa rotear: origens, destinos e empregos ----------------------------------------------------------------
arq_or <- file.path(dir_redes, "origens_familia_a.parquet")
ids_grade <- arrow::open_dataset(file.path(dir_insumos, "ttm_transit_2025.parquet")) |>
	dplyr::distinct(from_id) |>
	dplyr::collect() |>
	dplyr::pull(from_id)
if (!file.exists(arq_or)) {
	h_cad <- unique(rbindlist(lapply(sprintf("data/insumos/cad_individ_%d.parquet", 2011:2023), function(a) {
		arrow::open_dataset(a) |>
			dplyr::filter(code_muni_cad == 3550308L) |>
			dplyr::select(h3_09_cad, desvio_metros) |>
			dplyr::collect() |>
			as.data.table() |>
			_[!is.na(h3_09_cad) & as.numeric(desvio_metros) <= 500, .(id = h3_09_cad)]
	})))
	or <- data.table(id = union(ids_grade, h_cad$id))
	or[, `:=`(na_grade = id %in% ids_grade, tem_cadunico = id %in% h_cad$id)]
	arrow::write_parquet(or, arq_or)
}
or <- as.data.table(arrow::read_parquet(arq_or))
origens <- st_sf(or[, .(id)], geometry = st_geometry(h3jsr::cell_to_point(or$id, simple = FALSE)))
arq_emp <- file.path(dir_base, sprintf("empregos_h3_%d.parquet", ano_emp))
stopifnot("empregos do ano pedido ainda não calculados (ver 38)" = file.exists(arq_emp))
emp <- as.data.table(arrow::read_parquet(arq_emp))
destinos <- st_sf(id = ids_grade, geometry = st_geometry(h3jsr::cell_to_point(ids_grade, simple = FALSE)))
destinos$empregos <- emp$empregos[match(destinos$id, emp$h3_09)]
destinos$empregos[is.na(destinos$empregos)] <- 0
partida <- as.POSIXct("2025-10-01 06:50:00", tz = "America/Sao_Paulo")
cortes <- c(30L, 45L, 60L, 90L)
log("origens: ", nrow(origens), " (", sum(or$na_grade), " na grade; ", sum(!or$na_grade), " só do CadÚnico); destinos: ", nrow(destinos),
	"; modo ", modo, ", semente ", semente, ", ", sorteios, " sorteios por minuto, empregos de ", ano_emp)

## etapa rotear: laço ----------------------------------------------------------------------------------------
reg_arq <- file.path(dir_fa, "registro.csv")
assinatura <- function(r) paste(md5_gtfs(r), modo, semente, sorteios, ano_emp, nrow(origens), sep = "|")
for (r in redes) {
	saida <- file.path(dir_fa, sprintf("%s_%s.parquet", if (modo == "acessibilidade") "acesso" else "ttm", r))
	if (file.exists(reg_arq) && file.exists(saida) && nrow(fread(reg_arq)[rede == r & assinatura == assinatura(r)])) {
		log(r, ": já roteada com os mesmos GTFS e parâmetros")
		next
	}
	while (!file.exists(pronto(r)) || readLines(pronto(r))[1] != md5_gtfs(r)) {
		Sys.sleep(30)
	}
	t0 <- Sys.time()
	rede <- r5r::build_network(file.path(dir_fa, r), verbose = FALSE, overwrite = FALSE)
	x <- if (modo == "acessibilidade") {
		r5r::accessibility(rede, origins = origens, destinations = destinos, opportunities_colnames = "empregos", mode = "TRANSIT",
			departure_datetime = partida, time_window = 15L, percentiles = 50L, decay_function = "step", cutoffs = cortes,
			max_trip_duration = 90L, draws_per_minute = sorteios, n_threads = nucleos, progress = FALSE, seed = semente)
	} else {
		r5r::travel_time_matrix(rede, origins = origens, destinations = destinos, mode = "TRANSIT", departure_datetime = partida,
			time_window = 15L, max_trip_duration = 90L, draws_per_minute = sorteios, n_threads = nucleos, progress = FALSE, seed = semente)
	}
	r5r::stop_r5()
	arrow::write_parquet(as.data.table(x), saida)
	minutos <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
	fwrite(data.table(rede = r, assinatura = assinatura(r), linhas = nrow(x), minutos = minutos, fim = format(Sys.time())), reg_arq, append = file.exists(reg_arq))
	log(r, ": roteada em ", minutos, " min (", nrow(x), " linhas)")
}
log("todas as redes roteadas")

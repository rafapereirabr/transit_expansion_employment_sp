# Tabela de estações com ano de abertura e coordenadas (UTM 23S e lon/lat), base para atribuir
# tratamento. Uma linha por estação × linha (estações de integração aparecem uma vez por linha).

source("R/00_config.R")
suppressPackageStartupMessages({
	library(sf)
	library(geoarrow)
})

est <- arrow::open_dataset(file.path(dir_insumos, "transit_stations.parquet")) |>
	sf::st_as_sf()

## as coordenadas da origem estão em UTM 23S; o CRS é verificado (ou declarado) explicitamente
crs_origem <- sf::st_crs(est)
if (is.na(crs_origem)) {
	sf::st_crs(est) <- crs_utm
} else {
	stopifnot(crs_origem$epsg == crs_utm)
}
xy <- sf::st_coordinates(est)
stopifnot(all(xy[, 1] > 2e5 & xy[, 1] < 5e5), all(xy[, 2] > 7.3e6 & xy[, 2] < 7.5e6))
ll <- sf::st_coordinates(sf::st_transform(est, 4326))

estacoes <- est |>
	sf::st_drop_geometry() |>
	transmute(
		code_line = as.integer(code_line),
		name_line,
		code_station = as.integer(code_station),
		name_station,
		infill = as.integer(infill),
		rebuilt = as.integer(rebuilt),
		yr_open = as.integer(yr_open),
		dt_abertura = as.Date(dt_trial_svc),
		dt_servico_pleno = as.Date(dt_full_svc),
		## ano de abertura = ano da operação experimental; sem data, usa yr_open do catálogo
		ano_abertura = dplyr::coalesce(as.integer(format(dt_abertura, "%Y")), yr_open),
		x_utm = xy[, 1],
		y_utm = xy[, 2],
		lon = ll[, 1],
		lat = ll[, 2],
		futura_l6_l17 = code_line %in% linhas_futuras & (is.na(ano_abertura) | ano_abertura > max(anos_outcome))
	)

## validações do catálogo
stopifnot(!anyDuplicated(estacoes$code_station))
diverge <- estacoes |> filter(!is.na(dt_abertura), ano_abertura != yr_open)
if (nrow(diverge) > 0) {
	print(diverge)
	stop("Ano da operação experimental difere de yr_open no catálogo.")
}
abertas_janela <- estacoes |> filter(ano_abertura %in% janela_tratamento)
stopifnot(all(!is.na(abertas_janela$dt_abertura)))

arrow::write_parquet(estacoes, file.path(dir_base, "estacoes.parquet"))

eventos <- abertas_janela |>
	arrange(dt_abertura, code_line) |>
	select(ano_abertura, dt_abertura, dt_servico_pleno, code_line, name_line, name_station, infill)
write.csv(eventos, file.path(dir_saida, "aberturas_2015_2023.csv"), row.names = FALSE)
print(count(eventos, ano_abertura))

# EXPLORATÓRIO (16/11/2026), canal B (acessibilidade a partir de casa), etapa 1: empregos alcançáveis por corte de tempo
# (30, 45, 60 e 90 min) em cada estado de 31/12 da cadeia REAL (coluna `estado` de data/redes/cadeia_historica.csv) e da
# cadeia A (`estado_a`, CPTM fixa), e nas obras futuras (F24-F26 sobre a rede real de 2025; A24-A26 sobre A00), a partir
# das matrizes já roteadas pelo R/42 (data/redes/familia_a/ttm_<rede>.parquet; SÓ LEITURA, colunas filtradas no arrow).
#
# Empregos: formais de 2015 (data/base/empregos_h3_2015.parquet, o mesmo do R/42), restritos à grade de destinos.
# Principal: SEM os dois hexágonos-sede da Sé (os dois maiores da grade, a 0,1-0,2 km da Sé, 615 mil vínculos, 11,9% dos
# empregos da cidade; provavelmente sedes que registram quem trabalha em outro lugar: auditoria\out\a8_maiores_hexagonos.csv).
# Robustez: COM eles (acc + acc_se).
#
# Área (unidade do SDID) = H3 res 8. Acessibilidade da área = média das origens H3-9 filhas ponderada pelo número de
# pessoas dos risk sets com endereço em g-1 (risk_sets_lag1*.parquet, todas as coortes somadas), para representar quem
# mora lá. Áreas sem ninguém: média simples (não entram nas estimativas).
#
# Uso: Rscript --vanilla R/61_canalB_acesso.R      (R 4.6; ~15 min; 4 núcleos)
# Saídas (data/base/acesso_cortes/; NÃO em data/redes):
#   acesso_hex9.parquet   estado × origem H3-9 × corte: acc (empregos sem os 2 hexágonos da Sé) e acc_se (só os 2)
#   acesso_area8.parquet  estado × área H3-8 × corte: acc e acc_se (média ponderada por pessoas)
#   areas8.parquet        atributos da área: centróide, distância à Sé, célula de 5 km, subprefeitura, região, H3-7,
#                         pessoas (lag1, todas as coortes)
#   hex_se.csv            os dois hexágonos-sede
#   estados.csv           estado de 31/12 de cada ano nas duas cadeias

source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
arrow::set_cpu_count(4L)
arrow::set_io_thread_count(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
dir_ac <- file.path(dir_base, "acesso_cortes")
dir.create(dir_ac, showWarnings = FALSE, recursive = TRUE)
dir_fa <- "data/redes/familia_a"
cortes <- c(30L, 45L, 60L, 90L)

## estados de 31/12 nas duas cadeias ------------------------------------------------------------------------------------
cad <- fread("data/redes/cadeia_historica.csv")[, `:=`(inicio = as.IDate(inicio), fim = as.IDate(fim))]
estados <- rbindlist(lapply(2012:2025, function(y) {
	d <- as.IDate(sprintf("%d-12-31", y))
	x <- cad[inicio <= d & fim >= d]
	stopifnot(nrow(x) == 1L)
	data.table(ano = y, real = x$estado, A = x$estado_a)
}))
fwrite(estados, file.path(dir_ac, "estados.csv"))
print(estados)
redes <- unique(c(estados$real, estados$A, "F24", "F25", "F26", "A24", "A25", "A26"))
stopifnot(all(file.exists(file.path(dir_fa, sprintf("ttm_%s.parquet", redes)))))
msg("estados a calcular: ", paste(redes, collapse = " "))

## empregos na grade e os dois hexágonos da Sé --------------------------------------------------------------------------
grade <- arrow::open_dataset(file.path(dir_insumos, "ttm_transit_2025.parquet")) |>
	dplyr::distinct(to_id) |>
	dplyr::collect() |>
	as.data.table()
emp <- as.data.table(arrow::read_parquet(file.path(dir_base, "empregos_h3_2015.parquet")))[h3_09 %in% grade$to_id]
setnames(emp, c("to_id", "E"))
setorder(emp, -E)
se <- emp[1:2]
lon_lat <- sf::st_coordinates(h3jsr::cell_to_point(se$to_id, simple = FALSE))
dist_km <- function(lon1, lat1, lon2, lat2) {
	p <- pi / 180
	a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
	2 * 6371 * asin(sqrt(a))
}
se[, dist_se_km := round(dist_km(lon_lat[, 1], lon_lat[, 2], -46.6339, -23.5503), 2)]
fwrite(se, file.path(dir_ac, "hex_se.csv"))
msg("empregos na grade: ", emp[, sum(E)], " em ", nrow(emp), " hexágonos; os 2 da Sé: ", paste(se$E, collapse = " + "),
	" (", round(100 * sum(se$E) / emp[, sum(E)], 1), "%), a ", paste(se$dist_se_km, collapse = " e "), " km da Sé")
stopifnot(se$E[1] == 451787L, se$E[2] == 163712L) # conferem com auditoria\out\a8_maiores_hexagonos.csv
emp[, `:=`(E0 = fifelse(to_id %in% se$to_id, 0L, E), E1 = fifelse(to_id %in% se$to_id, E, 0L))]
emp_tab <- arrow::arrow_table(emp[, .(to_id, E0, E1)])

## acessibilidade por origem H3-9 ----------------------------------------------------------------------------------------
acc_rede <- function(r) {
	t0 <- Sys.time()
	x <- arrow::open_dataset(file.path(dir_fa, sprintf("ttm_%s.parquet", r))) |>
		dplyr::select(from_id, to_id, travel_time_p50) |>
		dplyr::inner_join(emp_tab, by = "to_id") |>
		dplyr::mutate(
			a30 = dplyr::if_else(travel_time_p50 <= 30L, E0, 0L), s30 = dplyr::if_else(travel_time_p50 <= 30L, E1, 0L),
			a45 = dplyr::if_else(travel_time_p50 <= 45L, E0, 0L), s45 = dplyr::if_else(travel_time_p50 <= 45L, E1, 0L),
			a60 = dplyr::if_else(travel_time_p50 <= 60L, E0, 0L), s60 = dplyr::if_else(travel_time_p50 <= 60L, E1, 0L),
			a90 = dplyr::if_else(travel_time_p50 <= 90L, E0, 0L), s90 = dplyr::if_else(travel_time_p50 <= 90L, E1, 0L)
		) |>
		dplyr::group_by(from_id) |>
		dplyr::summarise(dplyr::across(c(a30, s30, a45, s45, a60, s60, a90, s90), ~ sum(.x, na.rm = TRUE)), tt_max = max(travel_time_p50)) |>
		dplyr::collect() |>
		as.data.table()
	stopifnot(max(x$tt_max) <= 90L)
	x[, tt_max := NULL]
	l <- rbindlist(lapply(cortes, function(k) x[, .(from_id, corte = k, acc = as.numeric(get(paste0("a", k))), acc_se = as.numeric(get(paste0("s", k))))]))
	l[, estado := r]
	msg(r, ": ", nrow(x), " origens; ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
	l
}
A9 <- rbindlist(lapply(redes, acc_rede))
## origens sem nenhum destino alcançável em 90 min não aparecem na matriz: completa com zero
orig <- as.data.table(arrow::read_parquet("data/redes/origens_familia_a.parquet"))
comp <- CJ(estado = redes, from_id = orig$id, corte = cortes)
A9 <- A9[comp, on = .(estado, from_id, corte)]
msg("origens sem destino em algum estado (zeradas): ", A9[is.na(acc), uniqueN(from_id)])
A9[is.na(acc), `:=`(acc = 0, acc_se = 0)]
setcolorder(A9, c("estado", "from_id", "corte", "acc", "acc_se"))
arrow::write_parquet(A9, file.path(dir_ac, "acesso_hex9.parquet"))

## conferência: a 90 min, com a Sé, a acessibilidade de A00 deve bater com a auditada (acesso_aud_A00.parquet)
aud <- file.path(get0("dir_auditoria", ifnotfound = "outputs/auditoria"), "out", "acesso_aud_A00.parquet")
if (file.exists(aud)) {
	a <- as.data.table(arrow::read_parquet(aud))
	print(head(a, 3))
}

## pesos por pessoas (risk sets com endereço em g-1, todas as coortes) e área H3-8 ----------------------------------------
rs <- rbindlist(lapply(c("risk_sets_lag1.parquet", "risk_sets_lag1_c2014.parquet"), function(a) {
	arrow::open_dataset(file.path(dir_base, a)) |>
		dplyr::count(h3_09_cad) |>
		dplyr::collect() |>
		as.data.table()
}))[, .(n = sum(n)), by = h3_09_cad]
msg("hexágonos com pessoas: ", nrow(rs), "; fora das origens da matriz: ", sum(!rs$h3_09_cad %in% orig$id), " (", rs[!h3_09_cad %in% orig$id, sum(n)], " pessoas-coorte)")
hx <- data.table(from_id = orig$id)
hx[, area := h3jsr::get_parent(from_id, res = 8, simple = TRUE)]
hx[, w := rs$n[match(from_id, rs$h3_09_cad)]]
hx[is.na(w), w := 0]
A8 <- merge(A9, hx, by = "from_id")
A8 <- A8[, .(acc = if (sum(w) > 0) sum(w * acc) / sum(w) else mean(acc), acc_se = if (sum(w) > 0) sum(w * acc_se) / sum(w) else mean(acc_se)),
	by = .(estado, area, corte)]
arrow::write_parquet(A8, file.path(dir_ac, "acesso_area8.parquet"))

## atributos das áreas ------------------------------------------------------------------------------------------------
ar <- hx[, .(pessoas_lag1 = sum(w), hex9 = .N), by = area]
pt <- h3jsr::cell_to_point(ar$area, simple = FALSE)
xy <- sf::st_coordinates(pt)
ar[, `:=`(lon = xy[, 1], lat = xy[, 2])]
ar[, dist_se := dist_km(lon, lat, -46.6339, -23.5503)]
utm <- sf::sf_project("EPSG:4326", sprintf("EPSG:%d", crs_utm), xy)
ar[, celula := paste(floor(utm[, 1] / 5000), floor(utm[, 2] / 5000))]
ar[, h3_07 := h3jsr::get_parent(area, res = 7, simple = TRUE)]
gs <- sf::st_read(file.path(dir_insumos, "geosampa", "distrito_municipal.geojson"), quiet = TRUE) |> sf::st_transform(crs_utm)
message("colunas do GeoSampa: ", paste(names(gs), collapse = ", "))
pts <- sf::st_transform(pt, crs_utm)
kg <- vapply(sf::st_intersects(pts, gs), function(h) if (length(h)) h[1] else NA_integer_, integer(1))
msg("áreas fora dos distritos (vizinho mais próximo): ", sum(is.na(kg)))
if (anyNA(kg)) kg[is.na(kg)] <- sf::st_nearest_feature(pts[is.na(kg), ], gs)
ar[, `:=`(subpref = as.character(gs$cd_identificador_subprefeitura[kg]), regiao = as.character(gs$nm_regiao_05[kg]))]
arrow::write_parquet(ar, file.path(dir_ac, "areas8.parquet"))
msg("áreas H3-8: ", nrow(ar), " (", ar[pessoas_lag1 > 0, .N], " com pessoas); subprefeituras: ", uniqueN(ar$subpref), "; células de 5 km: ", uniqueN(ar$celula),
	"; H3-7: ", uniqueN(ar$h3_07))

## conferência com a acessibilidade auditada (auditor 2, a6_cptm.R): mesmas matrizes, com todos os empregos da grade
aud <- file.path(get0("dir_auditoria", ifnotfound = "outputs/auditoria"), "out")
for (r in c("A00", "H01", "H04", "H09", "H12")) {
	f <- file.path(aud, sprintf("acesso_aud_%s.parquet", r))
	if (!file.exists(f)) next
	a <- as.data.table(arrow::read_parquet(f))
	if (r == "A00") message("colunas do arquivo auditado: ", paste(names(a), collapse = ", "))
	idc <- intersect(c("id", "from_id"), names(a))[1]
	if (is.na(idc) || !all(c("corte", "acc") %in% names(a))) next
	m <- merge(a[, .(from_id = get(idc), corte, acc_aud = acc)], A9[estado == r, .(from_id, corte, acc_tot = acc + acc_se)], by = c("from_id", "corte"))
	msg("conferência ", r, ": ", nrow(m), " origem-corte; máx |diferença| = ", m[, max(abs(acc_aud - acc_tot))])
}

## resumo (agregado): mediana da acessibilidade por estado e corte, ponderada por pessoas
wmed <- function(x, w) {
	o <- order(x)
	x <- x[o]
	w <- cumsum(w[o]) / sum(w)
	x[which(w >= 0.5)[1]]
}
r <- merge(A8, ar[pessoas_lag1 > 0, .(area, pessoas_lag1)], by = "area")[, .(acc_mediana_mil = round(wmed(acc, pessoas_lag1) / 1000, 1),
	acc_se_mediana_mil = round(wmed(acc_se, pessoas_lag1) / 1000, 1)), by = .(estado, corte)]
options(width = 200)
print(dcast(r, estado ~ corte, value.var = "acc_mediana_mil"))
fwrite(r, "outputs/explora_canalB_acesso_mediana_estado.csv")
msg("fim")

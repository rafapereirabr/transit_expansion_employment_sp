# GTFS de trilho dos estados históricos da família A (A00 ao último estado do catálogo do 34), a partir do GTFS de trilho de 2025 do
# pipeline (pasta do Arthur, copiado com manifesto). Plano: quality_reports/plans/
# 2026-11-13_accessibility-gain-bands-design.qmd. Regras:
# - A00 acrescenta ao GTFS de 2025 as estações de 2025 que ele não tem (Varginha, L9; Ambuitá, L8):
#   ponta nova no fim da linha ou estação intermediária no trecho mais próximo. O tempo segue o
#   critério do pipeline: o tempo da linha é repartido pela distância em linha reta entre estações,
#   com a velocidade média da própria viagem.
# - Cada estado seguinte retira as estações de um pacote só das viagens da linha do pacote (uma
#   estação compartilhada com outra linha continua nela). Estação intermediária retirada: o trem
#   passa sem parar e os demais tempos ficam iguais; ponta retirada: a linha encurta e os tempos dos
#   trechos restantes ficam iguais.
# - Números aleatórios comuns (decidido em 14/11/2026, depois da rodada v1): todas as redes têm as
#   mesmas viagens, na mesma ordem, com os mesmos headways, para que a semente do r5r sorteie os
#   mesmos horários em todas elas.
#   * Fase: as paradas que permanecem mantêm o horário absoluto de partida. Ao retirar a ponta inicial,
#     o início e o fim da janela de frequências da viagem andam com o tempo do trecho retirado; ao
#     acrescentar uma ponta antes dela, só o início recua (o fim fica, para a janela continuar cobrindo
#     o horário de análise na ponta nova).
#   * Linha que fica com menos de duas estações não sai do GTFS: vira trem fantasma, com os mesmos
#     horários e paradas do estado anterior e sem embarque nem desembarque (pickup_type =
#     drop_off_type = 1). As linhas futuras entram como fantasmas em todas as redes no 36.
# - Viagens alteradas perdem o shape (o R5 liga as paradas em linha reta; o tempo vem de stop_times).
# - Calendário reescrito para 01/10/2025, a data canônica da rede de 2025.
# - Tabelas gravadas em ordem fixa (viagens por trip_id), a mesma em todas as redes.
# Validação: o estado vigente na data de roteamento de cada GTFS anual do pipeline (2012, 2015, 2019,
# 2023) deve ter as mesmas paradas por linha (casamento por proximidade, até 300 m); o tempo de
# viagem por linha pode diferir porque o pipeline usa a extensão operacional de cada ano.
#
# Uso: Rscript R/35_gtfs_trilho_estados.R   (R 4.6; depois do 34)
# Saídas: data/redes/insumos/ (cópias e _manifesto.csv), data/redes/gtfs/trilho_A00.zip a A22.zip,
#         data/redes/validacao_trilho.csv

source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages({
	library(data.table)
	library(gtfstools)
})
dir_redes <- "data/redes"
dir_ins <- file.path(dir_redes, "insumos")
dir_gtfs <- file.path(dir_redes, "gtfs")
for (d in c(dir_ins, dir_gtfs)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

## cópia dos insumos, com manifesto ---------------------------------------------------------------
origem <- c(
	file.path(raiz_arthur, "data/r5/2025", c("gtfs_rail_2025.zip", "gtfs_sptrans_2025.zip", "street_network.osm.pbf", "elevation.tif")),
	file.path(raiz_arthur, "data/gtfs/rail", sprintf("gtfs_rail_%d.zip", c(2012L, 2015L, 2019L, 2023L)))
)
stopifnot(all(file.exists(origem)))
destino <- file.path(dir_ins, basename(origem))
md5_o <- unname(tools::md5sum(origem))
precisa <- !file.exists(destino) | unname(tools::md5sum(destino)) != md5_o
if (any(precisa)) invisible(file.copy(origem[precisa], destino[precisa], overwrite = TRUE))
stopifnot(identical(unname(tools::md5sum(destino)), md5_o))
fwrite(data.table(
	arquivo = basename(destino), origem = sub(paste0(raiz_arthur, "/"), "", origem, fixed = TRUE),
	bytes = file.size(destino), md5 = md5_o, copiado_em = format(Sys.time(), "%Y-%m-%d %H:%M")
), file.path(dir_ins, "_manifesto.csv"))

## funções -----------------------------------------------------------------------------------------
seg <- function(x) vapply(strsplit(x, ":", fixed = TRUE), function(v) sum(as.numeric(v) * c(3600, 60, 1)), numeric(1))
hms <- function(s) {
	s <- round(s)
	sprintf("%02d:%02d:%02d", s %/% 3600, (s %% 3600) %/% 60, s %% 60)
}
dist_m <- function(lon1, lat1, lon2, lat2) {
	p <- pi / 180
	a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
	2 * 6371000 * asin(sqrt(a))
}
## viagens com embarque ou desembarque em alguma parada (as demais são trens fantasmas)
viagens_vivas <- function(g) {
	st <- g$stop_times
	pu <- if (is.null(st$pickup_type)) rep(0L, nrow(st)) else fcoalesce(as.integer(st$pickup_type), 0L)
	do <- if (is.null(st$drop_off_type)) rep(0L, nrow(st)) else fcoalesce(as.integer(st$drop_off_type), 0L)
	unique(st$trip_id[pu != 1L | do != 1L])
}
## janela de frequências de cada viagem deslocada em desloc[trip_id] segundos (o fim também, se fim = TRUE)
deslocar_frequencias <- function(g, desloc, fim = TRUE) {
	desloc <- desloc[desloc != 0]
	for (t in names(desloc)) {
		g$frequencies[trip_id == t, start_time := hms(seg(start_time) + desloc[[t]])]
		if (fim) g$frequencies[trip_id == t, end_time := hms(seg(end_time) + desloc[[t]])]
	}
	g
}
paradas_linha <- function(g, rota) {
	ids <- unique(g$stop_times[trip_id %in% intersect(g$trips[route_id == rota, trip_id], viagens_vivas(g)), stop_id])
	g$stops[stop_id %in% ids, .(stop_id, stop_name, lon = as.numeric(stop_lon), lat = as.numeric(stop_lat))]
}
## parada da linha mais próxima de uma estação do catálogo (NA se nenhuma a até max_m)
casar <- function(g, rota, lon, lat, max_m = 400) {
	p <- paradas_linha(g, rota)
	if (!nrow(p)) return(list(stop_id = NA_character_, dist = NA_real_))
	d <- dist_m(lon, lat, p$lon, p$lat)
	i <- which.min(d)
	list(stop_id = if (d[i] <= max_m) p$stop_id[i] else NA_character_, dist = d[i])
}
limpar <- function(g) {
	g$routes <- g$routes[route_id %in% g$trips$route_id]
	g$frequencies <- g$frequencies[trip_id %in% g$trips$trip_id]
	g$stops <- g$stops[stop_id %in% g$stop_times$stop_id]
	g$shapes <- g$shapes[shape_id %in% g$trips$shape_id]
	g
}
retirar <- function(g, rota, ids) {
	tr <- g$trips[route_id == rota, trip_id]
	stopifnot("linha já é fantasma" = length(intersect(tr, viagens_vivas(g))) > 0L)
	mod <- g$stop_times[trip_id %in% tr]
	desloc <- numeric()
	novos <- rbindlist(lapply(split(mod, by = "trip_id"), function(x) {
		setorder(x, stop_sequence)
		y <- x[!stop_id %in% ids]
		## linha sem trecho: trem fantasma com os horários do estado anterior
		if (nrow(y) < 2L) return(x[, `:=`(pickup_type = 1L, drop_off_type = 1L)])
		s <- seg(y$arrival_time)
		desloc[[y$trip_id[1]]] <<- s[1] # ponta inicial retirada: a janela anda junto (fase preservada)
		s <- s - s[1]
		y[, `:=`(arrival_time = hms(s), departure_time = hms(s), stop_sequence = seq_len(.N))]
	}))
	g$stop_times <- rbind(g$stop_times[!trip_id %in% tr], novos, use.names = TRUE)
	g <- deslocar_frequencias(g, desloc)
	g$trips[trip_id %in% tr, shape_id := NA_character_]
	limpar(g)
}
acrescentar <- function(g, rota, novo_id, nome, lon, lat, intermediaria) {
	g$stops <- rbind(g$stops, data.table(stop_id = novo_id, stop_name = nome, stop_lat = lat, stop_lon = lon), fill = TRUE)
	tr <- g$trips[route_id == rota, trip_id]
	mod <- g$stop_times[trip_id %in% tr]
	desloc <- numeric()
	novos <- rbindlist(lapply(split(mod, by = "trip_id"), function(x) {
		setorder(x, stop_sequence)
		x <- merge(x, g$stops[, .(stop_id, lon = as.numeric(stop_lon), lat = as.numeric(stop_lat))], by = "stop_id", sort = FALSE)
		setorder(x, stop_sequence)
		n <- nrow(x)
		s <- seg(x$arrival_time)
		d_seg <- dist_m(x$lon[-n], x$lat[-n], x$lon[-1], x$lat[-1])
		d_x <- dist_m(x$lon, x$lat, lon, lat)
		vel <- (s[n] - s[1]) / sum(d_seg) # segundos por metro em linha reta, da própria viagem
		nova <- x[1][, `:=`(stop_id = novo_id, lon = lon, lat = lat)]
		if (intermediaria) {
			i <- which.min(d_x[-n] + d_x[-1] - d_seg)
			s_x <- s[i] + (s[i + 1] - s[i]) * d_x[i] / (d_x[i] + d_x[i + 1])
			x <- rbind(x[seq_len(i)], nova, x[(i + 1):n])
			s <- c(s[seq_len(i)], s_x, s[(i + 1):n])
		} else if (d_x[n] <= d_x[1]) {
			x <- rbind(x, nova)
			s <- c(s, s[n] + d_x[n] * vel)
		} else {
			x <- rbind(nova, x)
			s <- c(0, s + d_x[1] * vel)
			desloc[[x$trip_id[1]]] <<- -d_x[1] * vel # ponta nova antes da inicial: a janela recua (fase preservada)
		}
		x[, `:=`(arrival_time = hms(s), departure_time = hms(s), stop_sequence = seq_len(.N))][, c("lon", "lat") := NULL]
	}))
	g$stop_times <- rbind(g$stop_times[!trip_id %in% tr], novos, use.names = TRUE)
	## só o início recua: as paradas antigas mantêm as partidas e ganham alguns minutos de serviço depois do
	## fim da janela (fora do horizonte de análise); recuar o fim tiraria a ponta nova da janela de análise
	g <- deslocar_frequencias(g, desloc, fim = FALSE)
	g$trips[trip_id %in% tr, shape_id := NA_character_]
	limpar(g)
}
fixar_calendario <- function(g, dia = as.Date("2025-10-01")) {
	d <- if (inherits(g$calendar$start_date, "Date")) dia else as.integer(format(dia, "%Y%m%d"))
	g$calendar[, `:=`(monday = 1L, tuesday = 1L, wednesday = 1L, thursday = 1L, friday = 1L, saturday = 0L, sunday = 0L, start_date = d, end_date = d)]
	g
}
## ordem fixa das tabelas (a mesma em todas as redes) e embarque/desembarque explícitos
ordenar <- function(g) {
	for (cc in c("pickup_type", "drop_off_type")) {
		v <- g$stop_times[[cc]]
		g$stop_times[, (cc) := if (is.null(v)) 0L else fcoalesce(as.integer(v), 0L)]
	}
	setorder(g$trips, trip_id)
	setorder(g$stop_times, trip_id, stop_sequence)
	setorder(g$frequencies, trip_id, start_time)
	setorder(g$routes, route_id)
	setorder(g$stops, stop_id)
	g
}
gravar <- function(g, rede) {
	arq <- file.path(dir_gtfs, sprintf("trilho_%s.zip", rede))
	if (file.exists(arq)) file.remove(arq)
	gtfstools::write_gtfs(ordenar(g), arq, quiet = TRUE)
	arq
}

## estados ---------------------------------------------------------------------------------------------
## a numeração vem do catálogo (34); GTFS de numerações anteriores são apagados
invisible(file.remove(list.files(dir_gtfs, "^trilho_A\\d{2}\\.zip$", full.names = TRUE)))
cat_a <- fread(file.path(dir_redes, "catalogo_familia_a.csv"))
g <- fixar_calendario(gtfstools::read_gtfs(file.path(dir_ins, "gtfs_rail_2025.zip")))
for (cc in c("pickup_type", "drop_off_type")) if (is.null(g$stop_times[[cc]])) g$stop_times[, (cc) := 0L]
est <-fread(file.path(dir_redes, "estacoes_familia_a.csv"), encoding = "UTF-8")[acao == "retira"]
est[, rota := paste0("rail_L", code_line)]
est[, c("stop_id", "dist_m") := {
	r <- lapply(seq_len(.N), function(i) casar(g, rota[i], lon[i], lat[i]))
	list(vapply(r, `[[`, "", "stop_id"), vapply(r, `[[`, 0, "dist"))
}]
faltam <- est[is.na(stop_id)]
message("estações de 2013-2025 ausentes do GTFS de 2025: ", paste(faltam$name_station, collapse = ", "))
stopifnot(all(faltam$rede %in% c("A01", "A02")))
message("distância máxima entre estação do catálogo e parada casada: ", round(max(est$dist_m[!is.na(est$stop_id)])), " m")

for (i in seq_len(nrow(faltam))) {
	f <- faltam[i]
	novo <- paste0("A00_", f$code_station)
	g <- acrescentar(g, f$rota, novo, tools::toTitleCase(tolower(f$name_station)), f$lon, f$lat, f$infill %in% 1L)
	est[code_station == f$code_station & rota == f$rota, stop_id := novo]
}
arqs <- c(A00 = gravar(g, "A00"))
redes <- cat_a[familia == "A" & acao == "retira", rede]
for (r in redes) {
	x <- est[rede == r]
	g <- retirar(g, unique(x$rota), x$stop_id)
	arqs[r] <- gravar(g, r)
}
fwrite(est[, .(rede, rota, code_station, name_station, stop_id, dist_m = round(dist_m))], file.path(dir_redes, "casamento_trilho.csv"))

## resumo e validação contra os GTFS anuais do pipeline -------------------------------------------------
resumo <- rbindlist(lapply(names(arqs), function(r) {
	h <- gtfstools::read_gtfs(arqs[[r]])
	st <- merge(h$stop_times[trip_id %in% viagens_vivas(h)], h$trips[, .(trip_id, route_id)], by = "trip_id")
	st[, .(n_paradas = uniqueN(stop_id), minutos = round(max(seg(arrival_time)) / 60, 1)), by = route_id][, rede := r][]
}))
## estado vigente na data de roteamento de cada GTFS anual do pipeline
datas_ref <- as.Date(c(`2012` = "2012-10-03", `2015` = "2015-07-29", `2019` = "2019-10-02", `2023` = "2023-10-01"))
ref <- vapply(datas_ref, function(d) sprintf("A%02d", cat_a[familia == "A" & acao == "retira", sum(as.Date(data_abertura) > d)]), "")
names(ref) <- names(datas_ref)
message("estado equivalente a cada GTFS anual: ", paste(names(ref), ref, sep = " = ", collapse = "; "))
val <- rbindlist(lapply(names(ref), function(a) {
	gr <- gtfstools::read_gtfs(file.path(dir_ins, sprintf("gtfs_rail_%s.zip", a)))
	ge <- gtfstools::read_gtfs(arqs[[ref[[a]]]])
	rotas <- union(gr$routes$route_id, ge$routes$route_id)
	rbindlist(lapply(rotas, function(ro) {
		pr <- paradas_linha(gr, ro)
		pe <- paradas_linha(ge, ro)
		so_ref <- pr[vapply(seq_len(.N), function(i) nrow(pe) == 0 || min(dist_m(lon[i], lat[i], pe$lon, pe$lat)) > 300, TRUE), stop_name]
		so_est <- pe[vapply(seq_len(.N), function(i) nrow(pr) == 0 || min(dist_m(lon[i], lat[i], pr$lon, pr$lat)) > 300, TRUE), stop_name]
		tempo <- function(gg) {
			x <- gg$stop_times[trip_id %in% intersect(gg$trips[route_id == ro, trip_id], viagens_vivas(gg))]
			if (nrow(x)) round(max(seg(x$arrival_time)) / 60, 1) else NA_real_
		}
		data.table(
			ano_pipeline = a, estado = ref[[a]], route_id = ro, n_ref = nrow(pr), n_estado = nrow(pe),
			so_no_pipeline = paste(so_ref, collapse = "; "), so_no_estado = paste(so_est, collapse = "; "),
			min_ref = tempo(gr), min_estado = tempo(ge)
		)
	}))
}))
fwrite(val, file.path(dir_redes, "validacao_trilho.csv"))
options(width = 220)
cat("\nparadas por linha em cada estado:\n")
print(dcast(resumo, route_id ~ rede, value.var = "n_paradas"))
cat("\nvalidação (linhas com diferença de paradas):\n")
print(val[so_no_pipeline != "" | so_no_estado != "", .(ano_pipeline, estado, route_id, n_ref, n_estado, so_no_pipeline, so_no_estado)])
cat("\nlinhas iguais em paradas:", val[so_no_pipeline == "" & so_no_estado == "", .N], "de", nrow(val), "\n")
message("gravado: ", length(arqs), " GTFS de trilho em ", dir_gtfs)

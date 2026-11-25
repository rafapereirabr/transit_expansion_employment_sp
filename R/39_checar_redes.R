# Checagens das redes da família A antes do roteamento (portão: termina com erro se houver FALHA).
# Plano: quality_reports/plans/2026-11-13_accessibility-gain-bands-design.qmd, seção "Sistema de
# checagens". Independente do gerador (35, 36): recalcula o esperado a partir do catálogo e do GTFS de
# trilho de 2025 do pipeline, em vez de reler o que o gerador gravou.
#
# Grupos de checagem (status OK, AVISO ou FALHA):
# 1. catálogo: datas completas e sem empate, ordem dos estados, correções aplicadas;
# 2. procedência: insumos com o md5 do manifesto, validador da MobilityData sem erros e mais novo
#    que cada GTFS;
# 3. estrutura de cada GTFS: integridade referencial, calendário na data de roteamento, frequências
#    cobrindo a janela, tempos começando em 0 e estritamente crescentes, sentidos espelhados com o
#    mesmo tempo total;
# 4. conteúdo de cada estado: A00 = GTFS de 2025 + Varginha + Ambuitá; cada estado = anterior menos
#    exatamente as estações do pacote, só na linha do pacote, com as demais linhas idênticas e o tempo
#    entre estações que permanecem inalterado; a base não tem nenhuma estação de 2013-2025; obras
#    futuras = A00 + a linha nova, com as estações do catálogo e os parâmetros previstos;
# 5. geografia: paradas dentro da RMSP, espaçamento, velocidade em linha reta, sinuosidade;
# 6. baldeações: paradas de mesmo nome em linhas diferentes a menos de 600 m (o R5 só faz a
#    baldeação a pé) e a lista explícita das baldeações das obras futuras.
# 7. números aleatórios comuns (14/11/2026, depois da rodada v1, em que retirar uma linha inteira e
#    encurtar a ponta inicial embaralharam o sorteio de horários): todas as redes com as mesmas viagens,
#    na mesma ordem e com os mesmos headways; cada viagem toda viva ou toda fantasma (sem embarque nem
#    desembarque); fantasmas exatamente nas linhas sem trecho aberto no estado; horário absoluto
#    (início da janela + tempo da parada) igual nas paradas comuns de cada par com/sem pacote e entre
#    A00 e o GTFS de 2025; linha nova das obras futuras = o fantasma de A00 com embarque.
#
# Paradas "da linha" são as das viagens vivas: os trens fantasmas não contam como estação aberta.
#
# As checagens são testadas por mutação em tests/teste_checagens_redes.R (defeitos plantados em
# cópias dos GTFS devem virar FALHA).
#
# Uso: Rscript R/39_checar_redes.R [dir=data/redes]   (R 4.6; depois do 34, 35, 36 e 37)
# Saídas: data/redes/checagens/pre_roteamento.csv (uma linha por rede × checagem) e md5_gtfs.csv

source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages({
	library(data.table)
	library(gtfstools)
})
dir_redes <- arg_cli("dir", NA, "data/redes") # outra pasta só no teste de mutação (tests/teste_checagens_redes.R)
dir_chk <- file.path(dir_redes, "checagens")
dir.create(dir_chk, recursive = TRUE, showWarnings = FALSE)
data_rot <- as.Date("2025-10-01")
janela <- c(6 * 3600 + 35 * 60, 8 * 3600 + 20 * 60) # partida 06:50 ± janela de 15 min, folga para viagens de 90 min
res <- list()
reg <- function(rede, grupo, checagem, ok, detalhe = "", aviso = FALSE) {
	st <- if (isTRUE(ok)) "OK" else if (aviso) "AVISO" else "FALHA"
	res[[length(res) + 1L]] <<- data.table(rede = rede, grupo = grupo, checagem = checagem, status = st, detalhe = as.character(detalhe))
}
seg <- function(x) vapply(strsplit(x, ":", fixed = TRUE), function(v) sum(as.numeric(v) * c(3600, 60, 1)), numeric(1))
dist_m <- function(lon1, lat1, lon2, lat2) {
	p <- pi / 180
	a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
	2 * 6371000 * asin(sqrt(a))
}
norm <- function(s) toupper(gsub("[^A-Za-z0-9]", "", iconv(s, "UTF-8", "ASCII//TRANSLIT")))
## embarque e desembarque (ausentes = permitidos) e viagens vivas
embarque <- function(st) {
	pu <- if (is.null(st$pickup_type)) rep(0L, nrow(st)) else fcoalesce(as.integer(st$pickup_type), 0L)
	do <- if (is.null(st$drop_off_type)) rep(0L, nrow(st)) else fcoalesce(as.integer(st$drop_off_type), 0L)
	data.table(trip_id = st$trip_id, pu = pu, do = do)
}
viagens_vivas <- function(g) embarque(g$stop_times)[pu != 1L | do != 1L, unique(trip_id)]

## entradas ------------------------------------------------------------------------------------------
cat_a <- fread(file.path(dir_redes, "catalogo_familia_a.csv"))
est <- fread(file.path(dir_redes, "estacoes_familia_a.csv"), encoding = "UTF-8")
corr <- fread(file.path(dir_redes, "correcoes_catalogo.csv"), encoding = "UTF-8")
hist_ids <- cat_a[familia == "A", rede]
fut_ids <- cat_a[familia == "A'", rede]
v3_ids <- cat_a[familia %in% c("H", "F", "S", "N"), rede] # redes v3 (53 e 54): grupo 8 e grupos 1, 2, 3, 5 e 6
zips <- file.path(dir_redes, "gtfs", sprintf("trilho_%s.zip", c(hist_ids, fut_ids, v3_ids)))
gt <- setNames(lapply(zips, function(z) if (file.exists(z)) gtfstools::read_gtfs(z) else NULL), c(hist_ids, fut_ids, v3_ids))
g25 <- gtfstools::read_gtfs(file.path(dir_redes, "insumos", "gtfs_rail_2025.zip"))

## 1. catálogo -----------------------------------------------------------------------------------------
pk <- cat_a[familia == "A" & acao == "retira"][order(rede)]
reg("catalogo", "catalogo", "datas completas", !anyNA(pk$data_abertura))
reg("catalogo", "catalogo", "datas sem empate", !anyDuplicated(pk$data_abertura), paste(pk$data_abertura[duplicated(pk$data_abertura)], collapse = ", "))
reg("catalogo", "catalogo", "estados em ordem decrescente de data", !is.unsorted(rev(as.Date(pk$data_abertura)), strictly = TRUE))
reg("catalogo", "catalogo", "pacotes entre 2013 e 2025", all(year(pk$data_abertura) %between% c(2013L, 2025L)))
reg("catalogo", "catalogo", "obras futuras depois de 2025", all(year(cat_a[familia == "A'", data_abertura]) >= 2026L))
corr[, data_corrigida := as.Date(as.character(data_corrigida))]
ap <- merge(corr[!is.na(data_corrigida)], est[, .(code_line, name_station, data_abertura)], by = c("code_line", "name_station"))
reg("catalogo", "catalogo", "correções aplicadas", nrow(ap) == nrow(corr) && all(as.Date(ap$data_abertura) == as.Date(ap$data_corrigida)),
	paste(ap[as.Date(data_abertura) != as.Date(data_corrigida), name_station], collapse = ", "))
reg("catalogo", "catalogo", "um GTFS por rede do catálogo", all(file.exists(zips)), paste(basename(zips[!file.exists(zips)]), collapse = ", "))

## 2. procedência --------------------------------------------------------------------------------------
man <- fread(file.path(dir_redes, "insumos", "_manifesto.csv"))
md5_atual <- unname(tools::md5sum(file.path(dir_redes, "insumos", man$arquivo)))
reg("insumos", "procedencia", "insumos com o md5 do manifesto", all(md5_atual == man$md5), paste(man$arquivo[md5_atual != man$md5], collapse = ", "))
val <- file.path(dir_redes, "validacao_mobilitydata.csv")
if (file.exists(val)) {
	v <- fread(val)
	for (i in seq_along(zips)) {
		nm <- sub("\\.zip$", "", basename(zips[i]))
		rj <- file.path(dir_redes, "validacao_mobilitydata", nm, "report.json")
		reg(names(gt)[i], "procedencia", "validador sem erros", file.exists(rj) && !nrow(v[feed == nm & severity == "ERROR"]),
			if (!file.exists(rj)) "sem relatório" else paste(v[feed == nm & severity == "ERROR", code], collapse = ", "))
		reg(names(gt)[i], "procedencia", "validação mais nova que o GTFS", file.exists(rj) && file.mtime(rj) >= file.mtime(zips[i]))
	}
} else {
	reg("todas", "procedencia", "validador rodado", FALSE, "rode R/37_validar_gtfs.R")
}
fwrite(data.table(rede = names(gt), arquivo = basename(zips), md5 = unname(tools::md5sum(zips))), file.path(dir_chk, "md5_gtfs.csv"))

## 3. estrutura de cada GTFS ---------------------------------------------------------------------------------
tempos_rota <- function(g, rota, so_vivas = FALSE) {
	tr <- g$trips[route_id == rota, trip_id]
	if (so_vivas) tr <- intersect(tr, viagens_vivas(g))
	st <- g$stop_times[trip_id %in% tr][order(trip_id, stop_sequence)]
	e <- embarque(st)
	st[, `:=`(s = seg(arrival_time), pu = e$pu, do = e$do)]
	st
}
for (r in names(gt)) {
	g <- gt[[r]]
	if (is.null(g)) next
	refs <- c(
		paradas = all(g$stop_times$stop_id %in% g$stops$stop_id),
		viagens = all(g$stop_times$trip_id %in% g$trips$trip_id) && all(g$trips$trip_id %in% g$stop_times$trip_id),
		rotas = all(g$trips$route_id %in% g$routes$route_id) && all(g$routes$route_id %in% g$trips$route_id),
		frequencias = all(g$frequencies$trip_id %in% g$trips$trip_id) && all(g$trips$trip_id %in% g$frequencies$trip_id),
		servico = all(g$trips$service_id %in% g$calendar$service_id),
		agencia = all(g$routes$agency_id %in% g$agency$agency_id)
	)
	reg(r, "estrutura", "integridade referencial", all(refs), paste(names(refs)[!refs], collapse = ", "))
	cal <- g$calendar
	ini <- as.Date(as.character(cal$start_date), tryFormats = c("%Y-%m-%d", "%Y%m%d"))
	fim <- as.Date(as.character(cal$end_date), tryFormats = c("%Y-%m-%d", "%Y%m%d"))
	reg(r, "estrutura", "calendário cobre 01/10/2025 (quarta)", all(ini <= data_rot & fim >= data_rot & cal$wednesday == 1L))
	fq <- g$frequencies
	reg(r, "estrutura", "frequências cobrem 06:35-08:20", all(seg(fq$start_time) <= janela[1] & seg(fq$end_time) >= janela[2]))
	reg(r, "estrutura", "headway entre 60 e 1200 s", all(fq$headway_secs %between% c(60L, 1200L)), paste(range(fq$headway_secs), collapse = "-"), aviso = TRUE)
	st <- g$stop_times[order(trip_id, stop_sequence)][, s := seg(arrival_time)]
	t_ok <- st[, .(zero = s[1] == 0, cresce = all(diff(s) > 0), n = .N, seq_ok = identical(stop_sequence, seq_len(.N)),
		dep = all(departure_time == arrival_time)), by = trip_id]
	reg(r, "estrutura", "viagens com 2 ou mais paradas", all(t_ok$n >= 2L))
	reg(r, "estrutura", "tempo começa em 0", all(t_ok$zero), paste(t_ok[zero == FALSE, trip_id], collapse = ", "))
	reg(r, "estrutura", "tempos estritamente crescentes", all(t_ok$cresce), paste(t_ok[cresce == FALSE, trip_id], collapse = ", "))
	reg(r, "estrutura", "stop_sequence 1..n", all(t_ok$seq_ok))
	reg(r, "estrutura", "sem tempo de parada (chegada = partida)", all(t_ok$dep), aviso = TRUE)
	## sentidos espelhados: cada viagem tem par com a sequência invertida e o mesmo tempo total (±2 s)
	pares <- st[, .(seq = paste(stop_id, collapse = ">"), inv = paste(rev(stop_id), collapse = ">"), total = max(s)), by = .(trip_id, route_id = g$trips$route_id[match(trip_id, g$trips$trip_id)])]
	pares[, par := vapply(seq_len(.N), function(i) any(pares$seq == inv[i] & pares$route_id == route_id[i] & abs(pares$total - total[i]) <= 2), TRUE)]
	reg(r, "estrutura", "sentidos espelhados com o mesmo tempo", all(pares$par), paste(pares[par == FALSE, trip_id], collapse = ", "))
	dup <- g$stops[, .N, by = .(round(as.numeric(stop_lat), 5), round(as.numeric(stop_lon), 5))][N > 1]
	reg(r, "estrutura", "paradas sem coordenada duplicada", !nrow(dup), nrow(dup), aviso = TRUE)
}

## 4. conteúdo de cada estado --------------------------------------------------------------------------------------
paradas_rota <- function(g, rota) {
	ids <- unique(g$stop_times[trip_id %in% intersect(g$trips[route_id == rota, trip_id], viagens_vivas(g)), stop_id])
	g$stops[stop_id %in% ids, .(stop_id, stop_name, lon = as.numeric(stop_lon), lat = as.numeric(stop_lat))]
}
## casamento independente: estação do catálogo -> parada da linha a até 400 m
casa <- function(g, rota, x) {
	p <- paradas_rota(g, rota)
	vapply(seq_len(nrow(x)), function(i) {
		d <- dist_m(x$lon[i], x$lat[i], p$lon, p$lat)
		if (length(d) && min(d) <= 400) p$stop_id[which.min(d)] else NA_character_
	}, "")
}
mesma_rota <- function(g1, g2, rota) {
	a <- tempos_rota(g1, rota)[, .(trip_id, stop_id, s, pu, do)]
	b <- tempos_rota(g2, rota)[, .(trip_id, stop_id, s, pu, do)]
	isTRUE(all.equal(a, b, check.attributes = FALSE))
}
## tempo entre todos os pares de paradas que permanecem, por viagem com a mesma sequência relativa
tempos_preservados <- function(g_antes, g_depois, rota) {
	a <- tempos_rota(g_antes, rota)
	b <- tempos_rota(g_depois, rota)
	if (!nrow(b)) return(TRUE)
	ok <- vapply(unique(b$trip_id), function(t) {
		y <- b[trip_id == t]
		cand <- unique(a$trip_id)
		any(vapply(cand, function(ta) {
			x <- a[trip_id == ta][stop_id %in% y$stop_id]
			if (!identical(x$stop_id, y$stop_id)) return(FALSE)
			isTRUE(all(abs(diff(x$s) - diff(y$s)) <= 1))
		}, TRUE))
	}, TRUE)
	all(ok)
}
rotas_todas <- function(g) unique(g$routes$route_id)

## A00 contra o GTFS de 2025 do pipeline
g0 <- gt[["A00"]]
novas <- est[rede %in% c("A01", "A02")]
for (ro in setdiff(rotas_todas(g25), paste0("rail_L", novas$code_line))) {
	reg("A00", "conteudo", paste("linha igual ao GTFS de 2025:", ro), mesma_rota(g25, g0, ro))
}
for (i in seq_len(nrow(novas))) {
	ro <- paste0("rail_L", novas$code_line[i])
	sid <- casa(g0, ro, novas[i])
	reg("A00", "conteudo", paste("estação acrescentada:", novas$name_station[i]), !is.na(sid))
	reg("A00", "conteudo", paste(ro, "= GTFS de 2025 + 1 parada"), nrow(paradas_rota(g0, ro)) == nrow(paradas_rota(g25, ro)) + 1L)
	reg("A00", "conteudo", paste(ro, "mantém os tempos entre as paradas antigas"), tempos_preservados(g0, g25, ro))
}

## cada estado = anterior menos exatamente o pacote
for (k in seq_len(nrow(pk))) {
	r <- pk$rede[k]
	b <- pk$base[k]
	ga <- gt[[b]]
	gd <- gt[[r]]
	if (is.null(ga) || is.null(gd)) next
	x <- est[rede == r]
	ro <- paste0("rail_L", unique(x$code_line))
	sid <- casa(ga, ro, x)
	reg(r, "conteudo", "estações do pacote encontradas no estado anterior", !anyNA(sid) && uniqueN(sid) == nrow(x), paste(x$name_station[is.na(sid)], collapse = ", "))
	pa <- paradas_rota(ga, ro)$stop_id
	pdp <- paradas_rota(gd, ro)$stop_id
	esperado <- setdiff(pa, sid)
	reg(r, "conteudo", "linha do pacote perde exatamente as estações do pacote",
		(length(esperado) < 2L && !length(pdp)) || setequal(pdp, esperado),
		paste("esperado", length(esperado), "obtido", length(pdp)))
	outras <- setdiff(union(rotas_todas(ga), rotas_todas(gd)), ro)
	iguais <- vapply(outras, function(o) mesma_rota(ga, gd, o), TRUE)
	reg(r, "conteudo", "demais linhas idênticas ao estado anterior", all(iguais), paste(outras[!iguais], collapse = ", "))
	reg(r, "conteudo", "tempo entre estações que permanecem inalterado", tempos_preservados(ga, gd, ro))
}
## a base não tem nenhuma estação de 2013-2025
base <- pk$rede[nrow(pk)]
gb <- gt[[base]]
xh <- est[acao == "retira"]
na_base <- vapply(seq_len(nrow(xh)), function(i) !is.na(casa(gb, paste0("rail_L", xh$code_line[i]), xh[i])), TRUE)
sobra <- xh[na_base]
reg(base, "conteudo", "base sem estações abertas em 2013-2025", !nrow(sobra), paste(sobra$name_station, collapse = ", "))

## obras futuras = A00 + a linha nova
param <- data.table(code_line = c(17L, 6L, 2L), headway = c(360L, 100L, 128L))
for (r in fut_ids) {
	g <- gt[[r]]
	x <- est[rede == r]
	l <- unique(x$code_line)
	ro <- paste0("rail_L", l)
	outras <- setdiff(rotas_todas(g0), ro)
	iguais <- vapply(outras, function(o) mesma_rota(g0, g, o), TRUE)
	reg(r, "conteudo", "demais linhas idênticas a A00", all(iguais), paste(outras[!iguais], collapse = ", "))
	sid <- casa(g, ro, x)
	reg(r, "conteudo", "todas as estações do catálogo na linha nova", !anyNA(sid) && uniqueN(sid) == nrow(x), paste(x$name_station[is.na(sid)], collapse = ", "))
	n_esp <- if (l == 2L) nrow(paradas_rota(g0, ro)) + nrow(x) else nrow(x)
	reg(r, "conteudo", "número de paradas da linha", nrow(paradas_rota(g, ro)) == n_esp, paste("esperado", n_esp, "obtido", nrow(paradas_rota(g, ro))))
	hw <- unique(g$frequencies[trip_id %in% g$trips[route_id == ro, trip_id], headway_secs])
	reg(r, "conteudo", "headway da linha análoga", identical(as.integer(hw), param[code_line == l, headway]), paste(hw, collapse = ", "))
	if (l == 2L) reg(r, "conteudo", "L2 antiga mantém os tempos", tempos_preservados(g, g0, ro) || tempos_preservados(g0, g, ro))
}

## 5. geografia ---------------------------------------------------------------------------------------------
for (r in names(gt)) {
	g <- gt[[r]]
	s <- g$stops[, .(stop_id, lon = as.numeric(stop_lon), lat = as.numeric(stop_lat))]
	reg(r, "geografia", "paradas dentro da RMSP", all(s$lon %between% c(-47.3, -45.6) & s$lat %between% c(-24.1, -23.0)))
	st <- merge(g$stop_times[order(trip_id, stop_sequence)][, s := seg(arrival_time)], s, by = "stop_id")[order(trip_id, stop_sequence)]
	st[, route_id := g$trips$route_id[match(trip_id, g$trips$trip_id)]]
	sg <- st[, .(d = dist_m(lon[-.N], lat[-.N], lon[-1], lat[-1]), dt = diff(s)), by = .(route_id, trip_id)]
	sg[, kmh := d / dt * 3.6]
	reg(r, "geografia", "espaçamento entre paradas de 200 m a 12 km", all(sg$d %between% c(200, 12000)),
		paste(sg[!d %between% c(200, 12000), unique(route_id)], collapse = ", "), aviso = TRUE)
	reg(r, "geografia", "velocidade em linha reta de 12 a 80 km/h", all(sg$kmh %between% c(12, 80)),
		paste(sg[!kmh %between% c(12, 80), sprintf("%s %.0f", route_id, kmh)][1:min(5, sum(!sg$kmh %between% c(12, 80)))], collapse = "; "), aviso = TRUE)
	sin <- st[, .(caminho = sum(dist_m(lon[-.N], lat[-.N], lon[-1], lat[-1])), reta = dist_m(lon[1], lat[1], lon[.N], lat[.N])), by = .(route_id, trip_id)]
	reg(r, "geografia", "sinuosidade das linhas abaixo de 2,5", all(sin$caminho / pmax(sin$reta, 1) < 2.5), paste(sin[caminho / reta >= 2.5, unique(route_id)], collapse = ", "), aviso = TRUE)
}

## 6. baldeações -------------------------------------------------------------------------------------------
for (r in names(gt)) {
	g <- gt[[r]]
	pr <- unique(merge(g$stop_times[, .(trip_id, stop_id)], g$trips[, .(trip_id, route_id)], by = "trip_id")[, .(stop_id, route_id)])
	pr <- merge(pr, g$stops[, .(stop_id, nome = norm(stop_name), lon = as.numeric(stop_lon), lat = as.numeric(stop_lat))], by = "stop_id")
	pp <- merge(pr, pr, by = "nome", allow.cartesian = TRUE)[route_id.x < route_id.y]
	pp[, d := dist_m(lon.x, lat.x, lon.y, lat.y)]
	longe <- pp[d > 600]
	reg(r, "baldeacao", "paradas de mesmo nome em linhas diferentes a menos de 600 m", !nrow(longe),
		paste(unique(longe[, sprintf("%s %s-%s %.0f m", nome, route_id.x, route_id.y, d)]), collapse = "; "), aviso = TRUE)
}
balde <- data.table(
	nova = c("CAMPO BELO", "MORUMBI", "JABAQUARA", "SÃO PAULO-MORUMBI", "HIGIENÓPOLIS-MACKENZIE", "SÃO JOAQUIM", "ÁGUA BRANCA", "PENHA"),
	linha_nova = c(17L, 17L, 17L, 17L, 6L, 6L, 6L, 2L),
	existente = c("Campo Belo", "Morumbi", "Jabaquara", "São Paulo-Morumbi", "Higienópolis-Mackenzie", "São Joaquim", "Água Branca", "Penha"),
	rota_existente = c("rail_L5", "rail_L9", "rail_L1", "rail_L4", "rail_L4", "rail_L1", "rail_L7", "rail_L3")
)
for (i in seq_len(nrow(balde))) {
	b <- balde[i]
	r <- cat_a[familia == "A'" & code_line == b$linha_nova, rede]
	g <- gt[[r]]
	pn <- est[rede == r & name_station == b$nova]
	pe <- paradas_rota(g, b$rota_existente)[norm(stop_name) == norm(b$existente)]
	d <- if (nrow(pn) && nrow(pe)) min(dist_m(pn$lon, pn$lat, pe$lon, pe$lat)) else NA_real_
	reg(r, "baldeacao", sprintf("baldeação %s (L%d) - %s a menos de 600 m", b$nova, b$linha_nova, b$rota_existente), !is.na(d) && d <= 600,
		if (is.na(d)) "parada não encontrada" else sprintf("%.0f m", d), aviso = TRUE)
}

## 7. números aleatórios comuns ----------------------------------------------------------------------------
ref <- gt[["A00"]]
fq_ref <- ref$frequencies[order(trip_id, start_time), .(trip_id, headway_secs)]
## linhas fantasmas esperadas em cada rede, a partir do catálogo e do GTFS de 2025 (independente do gerador):
## linha histórica cujas estações retiradas até o estado deixam menos de duas; linha futura fora da própria rede
n_a00 <- function(ro) nrow(paradas_rota(g25, ro)) + nrow(est[rede %in% c("A01", "A02") & paste0("rail_L", code_line) == ro])
fantasmas_esperados <- function(r) {
	fut <- c(rail_L17 = cat_a[familia == "A'" & code_line == 17L, rede], rail_L6 = cat_a[familia == "A'" & code_line == 6L, rede])
	esp <- names(fut)[fut != r]
	if (r %in% pk$rede) {
		feitos <- pk$rede[seq_len(match(r, pk$rede))]
		rem <- est[rede %in% feitos & acao == "retira", .N, by = .(ro = paste0("rail_L", code_line))]
		esp <- c(esp, rem[vapply(ro, n_a00, 0L) - N < 2L, ro])
	}
	sort(esp)
}
## horário absoluto (início da janela de frequências + tempo desde a primeira parada) das paradas vivas
fase <- function(g) {
	st <- g$stop_times[trip_id %in% viagens_vivas(g)]
	ini <- g$frequencies[, .(ini = min(seg(start_time))), by = trip_id]
	merge(st[, .(trip_id, stop_id, s = seg(arrival_time))], ini, by = "trip_id")[, .(trip_id, stop_id, abs = ini + s)]
}
fase_igual <- function(g1, g2) {
	x <- merge(fase(g1), fase(g2), by = c("trip_id", "stop_id"))
	x[, dif := abs(abs.x - abs.y)]
	list(ok = nrow(x) > 0L && all(x$dif <= 1), det = sprintf("%d paradas comuns; maior diferença %s s; viagens: %s", nrow(x),
		if (nrow(x)) max(x$dif) else NA, paste(unique(x[dif > 1, trip_id]), collapse = ", ")))
}
for (r in c(hist_ids, fut_ids)) {
	g <- gt[[r]]
	if (is.null(g)) next
	reg(r, "sementes", "mesmas viagens que A00, na mesma ordem", identical(g$trips$trip_id, ref$trips$trip_id),
		paste(c(setdiff(ref$trips$trip_id, g$trips$trip_id), setdiff(g$trips$trip_id, ref$trips$trip_id)), collapse = ", "))
	fq <- g$frequencies[order(trip_id, start_time), .(trip_id, headway_secs)]
	reg(r, "sementes", "mesmos headways por viagem que A00", isTRUE(all.equal(fq, fq_ref, check.attributes = FALSE)))
	e <- embarque(g$stop_times)[, .(viva = all(pu == 0L & do == 0L), fantasma = all(pu == 1L & do == 1L)), by = trip_id]
	reg(r, "sementes", "viagem toda viva ou toda fantasma", all(e$viva | e$fantasma), paste(e[!(viva | fantasma), trip_id], collapse = ", "))
	fant <- sort(unique(g$trips[trip_id %in% e[fantasma == TRUE, trip_id], route_id]))
	esp <- fantasmas_esperados(r)
	reg(r, "sementes", "fantasmas exatamente nas linhas sem trecho aberto", identical(fant, esp),
		sprintf("obtido: %s; esperado: %s", paste(fant, collapse = ","), paste(esp, collapse = ",")))
}
f <- fase_igual(g25, ref)
reg("A00", "sementes", "horário absoluto das paradas igual ao GTFS de 2025", f$ok, f$det)
for (k in seq_len(nrow(pk))) {
	if (is.null(gt[[pk$base[k]]]) || is.null(gt[[pk$rede[k]]])) next
	f <- fase_igual(gt[[pk$base[k]]], gt[[pk$rede[k]]])
	reg(pk$rede[k], "sementes", "horário absoluto igual ao estado anterior nas paradas comuns", f$ok, f$det)
}
for (r in fut_ids) {
	f <- fase_igual(ref, gt[[r]])
	reg(r, "sementes", "horário absoluto igual a A00 nas paradas comuns", f$ok, f$det)
}
for (l in c(17L, 6L)) {
	r <- cat_a[familia == "A'" & code_line == l, rede]
	ro <- paste0("rail_L", l)
	a <- tempos_rota(ref, ro)
	b <- tempos_rota(gt[[r]], ro)
	ja <- ref$frequencies[trip_id %in% a$trip_id][order(trip_id), .(trip_id, start_time, end_time, headway_secs)]
	jb <- gt[[r]]$frequencies[trip_id %in% b$trip_id][order(trip_id), .(trip_id, start_time, end_time, headway_secs)]
	reg(r, "sementes", "linha nova = fantasma de A00 com embarque", nrow(a) > 0L && all(a$pu == 1L & a$do == 1L) && all(b$pu == 0L & b$do == 0L) &&
		isTRUE(all.equal(a[, .(trip_id, stop_id, s)], b[, .(trip_id, stop_id, s)], check.attributes = FALSE)) && isTRUE(all.equal(ja, jb, check.attributes = FALSE)))
}

## 8. redes v3 (53 e 54): estados reais (H), obras futuras sobre a rede real de 2025 (F), obras sem data (S)
## e linhas novas sem data (N). Esperado recalculado do GTFS de 2025 do pipeline e do catálogo, por nome de
## parada, sem reler o gerador. -----------------------------------------------------------------------------
if (length(v3_ids)) {
	cad <- fread(file.path(dir_redes, "cadeia_historica.csv"))
	ev3 <- fread(file.path(dir_redes, "eventos_v3.csv"))
	est3 <- fread(file.path(dir_redes, "estacoes_v3.csv"), encoding = "UTF-8")
	fda <- fread(file.path(dir_redes, "estado_fim_de_ano.csv"))
	cad[, `:=`(inicio = as.Date(inicio), fim = as.Date(fim))]
	## 8a. catálogo
	reg("catalogo_v3", "catalogo", "cadeia histórica contínua até 31/12/2025", all(cad$inicio[-1] == cad$fim[-nrow(cad)] + 1L) &&
		max(cad$fim) == as.Date("2025-12-31") && min(cad$inicio) <= as.Date("2012-12-31"))
	reg("catalogo_v3", "catalogo", "estados da cadeia no catálogo", all(cad$estado %in% cat_a$rede))
	reg("catalogo_v3", "catalogo", "estado A de cada intervalo = pacotes abertos até o início",
		all(cad$estado_a == vapply(cad$inicio, function(d) sprintf("A%02d", sum(as.Date(pk$data_abertura) > d)), "")))
	reg("catalogo_v3", "catalogo", "intervalo sem mudança usa o próprio estado A", all(cad[is.na(mudancas) | mudancas == "", estado == estado_a]))
	reg("catalogo_v3", "catalogo", "cada abertura da família A é um evento da cadeia", setequal(ev3[tipo == "abertura", pacote], pk$rede))
	reg("catalogo_v3", "catalogo", "eventos com redes do catálogo", all(c(ev3$com, ev3$sem) %in% cat_a$rede))
	reg("catalogo_v3", "catalogo", "estado em 31/12 de 2012 a 2025", identical(as.integer(fda$ano), 2012:2025) && all(fda$estado %in% cat_a$rede))

	## 8b. conteúdo de H e F: linhas da CPTM, L12 e L15 conforme as mudanças; demais linhas iguais ao estado A
	vivas_rota <- function(g, rota) {
		tr <- intersect(g$trips[route_id == rota, trip_id], viagens_vivas(g))
		st <- merge(g$stop_times[trip_id %in% tr], g$stops[, .(stop_id, stop_name)], by = "stop_id")[order(trip_id, stop_sequence)]
		split(norm(st$stop_name), st$trip_id)
	}
	seq_ok <- function(g, rota, esperado) {
		v <- vivas_rota(g, rota)
		length(v) == 2L && any(vapply(v, identical, TRUE, esperado)) && any(vapply(v, identical, TRUE, rev(esperado)))
	}
	ida <- function(g, rota, comeca) {
		v <- vivas_rota(g, rota)
		v[vapply(v, function(s) s[1] == norm(comeca), TRUE)][[1]]
	}
	l7 <- ida(g25, "rail_L7", "Luz")
	l10 <- ida(g25, "rail_L10", "Brás")
	l12 <- ida(g25, "rail_L12", "Brás")
	t25 <- function(rota, de, ate) {
		x <- tempos_rota(g25, rota)
		x <- merge(x, g25$stops[, .(stop_id, nome = norm(stop_name))], by = "stop_id")
		v <- x[, abs(s[nome == norm(ate)] - s[nome == norm(de)]), by = trip_id]$V1
		mean(v)
	}
	tg <- function(g, rota, de, ate) {
		x <- merge(tempos_rota(g, rota, so_vivas = TRUE), g$stops[, .(stop_id, nome = norm(stop_name))], by = "stop_id")
		x[, if (any(nome == norm(de)) && any(nome == norm(ate))) abs(s[nome == norm(ate)] - s[nome == norm(de)]), by = trip_id]$V1
	}
	bras10 <- unique(g25$stop_times[trip_id %in% g25$trips[route_id == "rail_L10", trip_id]][stop_id %in% g25$stops[norm(stop_name) == "BRAS", stop_id], stop_id])
	for (r in cat_a[familia %in% c("H", "F"), rede]) {
		x <- cat_a[rede == r]
		g <- gt[[r]]
		gb <- gt[[x$base]]
		m <- strsplit(x$modificacoes, ";", fixed = TRUE)[[1]]
		mexe <- c(if (any(m %in% c("l7_bras", "s710", "pos710"))) c("rail_L7", "rail_L10"), if ("eg_fechada" %in% m) "rail_L12", if ("l15_suspensa" %in% m) "rail_L15")
		outras <- setdiff(union(rotas_todas(gb), rotas_todas(g)), mexe)
		iguais <- vapply(outras, function(o) mesma_rota(gb, g, o), TRUE)
		reg(r, "conteudo", paste("demais linhas idênticas a", x$base), all(iguais), paste(outras[!iguais], collapse = ", "))
		e7 <- if ("s710" %in% m) c(rev(l10), l7) else if ("l7_bras" %in% m) c("BRAS", l7) else if ("pos710" %in% m) l7[-1] else l7
		reg(r, "conteudo", "L7 com as paradas e pontas da mudança", seq_ok(g, "rail_L7", e7), paste(e7[1], "->", e7[length(e7)]))
		if ("s710" %in% m) {
			reg(r, "conteudo", "L10 sem viagens vivas (Serviço 710)", !length(vivas_rota(g, "rail_L10")))
			reg(r, "conteudo", "L7 de Brás a Rio Grande da Serra com os tempos da L10", isTRUE(all(abs(sort(tg(g, "rail_L7", "Brás", "Rio Grande Da Serra")) - t25("rail_L10", "Brás", "Rio Grande Da Serra")) <= 2)))
		} else {
			e10 <- if ("pos710" %in% m) c(norm("Palmeiras - Barra Funda"), "LUZ", l10) else l10
			reg(r, "conteudo", "L10 com as paradas e pontas da mudança", seq_ok(g, "rail_L10", e10), paste(e10[1], "->", e10[length(e10)]))
		}
		if (any(m %in% c("l7_bras", "s710"))) {
			st7 <- g$stop_times[trip_id %in% g$trips[route_id == "rail_L7", trip_id], unique(stop_id)]
			reg(r, "conteudo", "L7 para no Brás na parada da L10", any(bras10 %in% st7))
			reg(r, "conteudo", "L7 Luz-Brás com o tempo da L11", isTRUE(all(abs(tg(g, "rail_L7", "Luz", "Brás") - t25("rail_L11", "Luz", "Brás")) <= 2)))
			reg(r, "conteudo", "L7 mantém os tempos entre as paradas antigas", tempos_preservados(g, gb, "rail_L7"))
		}
		if ("pos710" %in% m) {
			reg(r, "conteudo", "L10 Brás-Luz com o tempo da L11 e Luz-Barra Funda com o da L7",
				isTRUE(all(abs(tg(g, "rail_L10", "Brás", "Luz") - t25("rail_L11", "Luz", "Brás")) <= 2)) &&
				isTRUE(all(abs(tg(g, "rail_L10", "Luz", "Palmeiras - Barra Funda") - t25("rail_L7", "Luz", "Palmeiras - Barra Funda")) <= 2)))
			reg(r, "conteudo", "L7 e L10 mantêm os tempos entre as paradas antigas", tempos_preservados(gb, g, "rail_L7") && tempos_preservados(g, gb, "rail_L10"))
		}
		if ("eg_fechada" %in% m) {
			reg(r, "conteudo", "L12 sem Engenheiro Goulart", seq_ok(g, "rail_L12", setdiff(l12, "ENGGOULART")))
			reg(r, "conteudo", "L12 mantém os tempos entre as paradas que ficam", tempos_preservados(gb, g, "rail_L12"))
		}
		if ("l15_suspensa" %in% m) {
			a <- tempos_rota(gb, "rail_L15")[, .(trip_id, stop_id, s)]
			b <- tempos_rota(g, "rail_L15")[, .(trip_id, stop_id, s)]
			reg(r, "conteudo", "L15 suspensa: mesmas paradas e tempos do estado A, sem embarque", !length(vivas_rota(g, "rail_L15")) && isTRUE(all.equal(a, b, check.attributes = FALSE)))
		}
		if (x$familia == "F") {
			h1 <- gt[[cat_a[familia == "N" & rede == "N00", base]]]
			reg(r, "conteudo", "L7 e L10 iguais às da rede real de 2025 (H01)", mesma_rota(h1, g, "rail_L7") && mesma_rota(h1, g, "rail_L10"))
		}
	}

	## 8c. conteúdo de S: só a linha da obra muda; paradas novas = estações do catálogo; tempos antigos preservados
	vel_trechos <- function(g, rota, ids) {
		st <- merge(g$stop_times[trip_id %in% intersect(g$trips[route_id == rota, trip_id], viagens_vivas(g))],
			g$stops[, .(stop_id, lon = as.numeric(stop_lon), lat = as.numeric(stop_lat))], by = "stop_id")[order(trip_id, stop_sequence)]
		st[, s := seg(arrival_time)]
		sg <- st[, .(a = stop_id[-.N], b = stop_id[-1], d = dist_m(lon[-.N], lat[-.N], lon[-1], lat[-1]), dt = diff(s)), by = trip_id]
		sg[a %in% ids | b %in% ids, d / dt * 3.6]
	}
	for (r in cat_a[familia == "S", rede]) {
		x <- cat_a[rede == r]
		g <- gt[[r]]
		gb <- gt[[x$base]]
		e3 <- est3[rede == r]
		rotas <- union(rotas_todas(gb), rotas_todas(g))
		dif <- rotas[!vapply(rotas, function(o) mesma_rota(gb, g, o), TRUE)]
		esperada <- if (x$code_line %in% c(7L, 10L)) "rail_L10" else paste0("rail_L", x$code_line)
		reg(r, "conteudo", paste("só a linha da obra muda em relação a", x$base), identical(dif, esperada), paste(dif, collapse = ", "))
		pb <- paradas_rota(gb, esperada)
		pn <- paradas_rota(g, esperada)
		nv <- pn[!stop_id %in% pb$stop_id]
		casadas <- vapply(seq_len(nrow(e3)), function(i) nrow(nv) > 0 && min(dist_m(e3$lon[i], e3$lat[i], nv$lon, nv$lat)) <= 50, TRUE)
		reg(r, "conteudo", "paradas novas = estações do catálogo, antigas mantidas", nrow(nv) == nrow(e3) && all(casadas) && all(pb$stop_id %in% pn$stop_id),
			sprintf("novas %d, catálogo %d", nrow(nv), nrow(e3)))
		reg(r, "conteudo", "tempos entre as paradas antigas preservados", tempos_preservados(g, gb, esperada))
		v <- vel_trechos(g, esperada, nv$stop_id)
		if (x$acao == "acrescenta extensão") {
			reg(r, "conteudo", "trechos novos a até 35 km/h em linha reta (35 km/h pelo traçado)", length(v) > 0 && all(v <= 35.5 & v >= 10), paste(round(range(v), 1), collapse = "-"))
		} else {
			reg(r, "conteudo", "estação intermediária com a velocidade do trecho original nos dois lados", length(v) > 0 && diff(range(v)) / max(v) < 0.05, paste(round(range(v), 1), collapse = "-"))
		}
	}

	## 8d. conteúdo de N: N00 = rede real de 2025 + seis linhas fantasmas; cada rede N abre exatamente uma
	g_n0 <- gt[["N00"]]
	g_h1 <- gt[[cat_a[rede == "N00", base]]]
	iguais <- vapply(rotas_todas(g_h1), function(o) mesma_rota(g_h1, g_n0, o), TRUE)
	reg("N00", "conteudo", "linhas da rede real de 2025 idênticas", all(iguais), paste(rotas_todas(g_h1)[!iguais], collapse = ", "))
	rot_n <- setdiff(rotas_todas(g_n0), rotas_todas(g_h1))
	lin_n <- cat_a[familia == "N" & rede != "N00"]
	reg("N00", "conteudo", "linhas novas = as do catálogo", setequal(rot_n, paste0("rail_L", lin_n$code_line)))
	param_n <- data.table(code_line = c(14L, 16L, 19L, 20L, 22L, 24L), headway = c(360L, 100L, 100L, 100L, 100L, 360L))
	for (r in lin_n$rede) {
		g <- gt[[r]]
		l <- cat_a[rede == r, code_line]
		ro <- paste0("rail_L", l)
		outras <- setdiff(rotas_todas(g_n0), ro)
		iguais <- vapply(outras, function(o) mesma_rota(g_n0, g, o), TRUE)
		reg(r, "conteudo", "demais linhas idênticas a N00", all(iguais), paste(outras[!iguais], collapse = ", "))
		a <- tempos_rota(g_n0, ro)
		b <- tempos_rota(g, ro)
		reg(r, "conteudo", "linha aberta = fantasma de N00 com embarque", all(a$pu == 1L & a$do == 1L) && all(b$pu == 0L & b$do == 0L) &&
			isTRUE(all.equal(a[, .(trip_id, stop_id, s)], b[, .(trip_id, stop_id, s)], check.attributes = FALSE)))
		e3 <- est3[rede == r]
		pn <- paradas_rota(g, ro)
		casadas <- vapply(seq_len(nrow(e3)), function(i) min(dist_m(e3$lon[i], e3$lat[i], pn$lon, pn$lat)) <= 50, TRUE)
		reg(r, "conteudo", "paradas = estações do catálogo", nrow(pn) == nrow(e3) && all(casadas), sprintf("%d paradas, %d estações", nrow(pn), nrow(e3)))
		hw <- unique(g$frequencies[trip_id %in% g$trips[route_id == ro, trip_id], headway_secs])
		reg(r, "conteudo", "headway da linha análoga", identical(as.integer(hw), param_n[code_line == l, headway]), paste(hw, collapse = ", "))
		v <- vel_trechos(g, ro, pn$stop_id)
		reg(r, "conteudo", "trechos a até 35 km/h em linha reta (35 km/h pelo traçado)", all(v <= 35.5 & v >= 10), paste(round(range(v), 1), collapse = "-"))
	}

	## 8e. números aleatórios comuns: universo v2 (H, F, S) = viagens de A00; universo v3 (N) = viagens de N00
	esp_v3 <- function(r) {
		x <- cat_a[rede == r]
		if (x$familia %in% c("A", "A'")) return(fantasmas_esperados(r))
		e <- esp_v3(x$base)
		if (x$familia %in% c("H", "F")) {
			m <- strsplit(x$modificacoes, ";", fixed = TRUE)[[1]]
			if ("s710" %in% m) e <- c(e, "rail_L10")
			if ("l15_suspensa" %in% m) e <- c(e, "rail_L15")
		}
		if (r == "N00") e <- c(e, paste0("rail_L", lin_n$code_line))
		if (x$familia == "N" && r != "N00") e <- setdiff(e, paste0("rail_L", x$code_line))
		sort(unique(e))
	}
	ref_n <- gt[["N00"]]
	for (r in v3_ids) {
		g <- gt[[r]]
		x <- cat_a[rede == r]
		rf <- if (x$familia == "N") ref_n else ref
		nm <- if (x$familia == "N") "N00" else "A00"
		reg(r, "sementes", paste("mesmas viagens que", nm, "na mesma ordem"), identical(g$trips$trip_id, rf$trips$trip_id))
		fq <- g$frequencies[order(trip_id, start_time), .(trip_id, headway_secs)]
		fr <- rf$frequencies[order(trip_id, start_time), .(trip_id, headway_secs)]
		reg(r, "sementes", paste("mesmos headways por viagem que", nm), isTRUE(all.equal(fq, fr, check.attributes = FALSE)))
		e <- embarque(g$stop_times)[, .(viva = all(pu == 0L & do == 0L), fantasma = all(pu == 1L & do == 1L)), by = trip_id]
		reg(r, "sementes", "viagem toda viva ou toda fantasma", all(e$viva | e$fantasma), paste(e[!(viva | fantasma), trip_id], collapse = ", "))
		fant <- sort(unique(g$trips[trip_id %in% e[fantasma == TRUE, trip_id], route_id]))
		es <- esp_v3(r)
		reg(r, "sementes", "fantasmas exatamente nas linhas esperadas", identical(fant, es), sprintf("obtido: %s; esperado: %s", paste(fant, collapse = ","), paste(es, collapse = ",")))
		f <- fase_igual(gt[[x$base]], g)
		reg(r, "sementes", paste("horário absoluto igual a", x$base, "nas paradas comuns"), f$ok, f$det)
	}
	for (k in seq_len(nrow(ev3))) {
		f <- fase_igual(gt[[ev3$sem[k]]], gt[[ev3$com[k]]])
		reg(ev3$com[k], "sementes", paste("horário absoluto igual nas paradas comuns:", ev3$evento[k]), f$ok, f$det)
	}
}

## relatório -------------------------------------------------------------------------------------------
rel <- rbindlist(res)
fwrite(rel, file.path(dir_chk, "pre_roteamento.csv"))
options(width = 220)
cat("\nresumo por grupo e status:\n")
print(dcast(rel, grupo ~ status, fun.aggregate = length, value.var = "status"))
cat("\nFALHAS:\n")
print(rel[status == "FALHA", .(rede, grupo, checagem, detalhe)])
cat("\nAVISOS (agrupados):\n")
print(rel[status == "AVISO", .(redes = paste(head(rede, 6), collapse = ","), n = .N, detalhe = detalhe[1]), by = .(grupo, checagem)])
if (any(rel$status == "FALHA")) {
	message("checagens com FALHA: ", sum(rel$status == "FALHA"))
	quit(status = 1)
}
message("todas as checagens sem FALHA (", nrow(rel), " checagens; ", sum(rel$status == "AVISO"), " avisos)")

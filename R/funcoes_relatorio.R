# Funções do relatório dos desenhos (relatorio/relatorio_desenhos.qmd, 24/11/2026). Leem só as saídas agregadas de
# outputs/final/ e as bases em cache por evento (somas por célula), nunca dados linha a linha.
suppressPackageStartupMessages({
	library(data.table)
	library(ggplot2)
})

## lê e empilha os arquivos de outputs/final que casam com o padrão; NULL se não houver nenhum
le_final <- function(padrao, dir = "outputs/final") {
	fs <- list.files(dir, padrao, full.names = TRUE)
	if (!length(fs)) return(NULL)
	rbindlist(lapply(fs, function(f) fread(f)[, arquivo := basename(f)]), fill = TRUE)
}
estrela <- function(p) fifelse(is.na(p), "", fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.1, "*", ""))))
num <- function(x, d = 2) {
	s <- formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
	s <- sub("^-", "−", s)
	fifelse(is.na(x), "", s)
}
sinal <- function(x, d = 2) fifelse(is.na(x), "", fifelse(x >= 0, paste0("+", num(x, d)), num(x, d)))
cel <- function(b, se, p, d = 2) fifelse(is.na(b), "", paste0(sinal(b, d), estrela(p), " (", num(se, d), ")"))
em_execucao <- function(oque = "") cat(sprintf('\n<p class="pendente">Em execução%s: este bloco será preenchido quando a estimação terminar.</p>\n',
	if (nzchar(oque)) paste0(" (", oque, ")") else ""))

## rótulos legíveis de desfechos, janelas e termos
ROT_OC <- c(emprego = "emprego", entrada = "entrada", empregado = "permanência", meses = "meses empregado", salario = "salário",
	qualquer = "qualquer desligamento", demissao = "demissão", sem_justa = "sem justa causa", justa = "com justa causa", pedido = "pedido de demissão",
	ped_curto = "pediu → trajeto mais curto", ped_naocurto = "pediu → trajeto não mais curto", ped_semloc = "pediu → novo vínculo sem local",
	ped_semnovo = "pediu → sem novo vínculo formal", ped_salmaior = "pediu → salário maior", ped_salnaomaior = "pediu → salário não maior",
	ped_novo3 = "pediu → novo vínculo em até 3 m", ped_novo4a12 = "pediu → novo vínculo em 4–12 m", nao_pediu = "não pediu")
rot_oc <- function(o) {
	b <- sub("_[0-9]+$", "", o)
	h <- ifelse(grepl("_[0-9]+$", o), sub(".*_", "", o), "")
	r <- ifelse(b %in% names(ROT_OC), ROT_OC[b], b)
	ifelse(nzchar(h), paste0(r, ", ", h, " m"), r)
}
rot_jan <- function(j) fifelse(j == "pos", "pós", fifelse(j == "pre", "placebo", j))
## tabela larga: rótulos legíveis e colunas "a · b"
larga <- function(d, formula, valor = "c") {
	d <- copy(d)
	if ("outcome" %in% names(d)) d[, outcome := rot_oc(outcome)]
	if ("janela" %in% names(d)) d[, janela := rot_jan(janela)]
	if ("grupo" %in% names(d)) d[, grupo := rot_oc(grupo)]
	w <- dcast(d, as.formula(formula), value.var = valor, sep = " · ", fill = "—")
	for (cc in names(w)) if (is.character(w[[cc]])) set(w, which(is.na(w[[cc]]) | w[[cc]] == ""), cc, "—")
	setnames(w, names(w), sub("^outcome$", "desfecho", names(w)))
	w
}

## tabela HTML; dobravel = TRUE põe dentro de um bloco que abre e fecha
tabela <- function(d, titulo = NULL, dobravel = FALSE, legenda = NULL, texto = FALSE) {
	if (is.null(d) || !nrow(d)) return(invisible(NULL))
	h <- knitr::kable(d, format = "html", escape = FALSE, table.attr = if (texto) 'class="tab texto"' else 'class="tab"', align = rep("l", ncol(d)))
	h <- paste0('<div class="rolagem">', h, "</div>")
	if (!is.null(legenda)) h <- paste0(h, '<p class="legenda">', legenda, "</p>")
	h <- if (dobravel) sprintf("<details><summary>%s</summary>\n%s\n</details>", if (is.null(titulo)) "Tabela completa" else titulo, h) else
		paste0(if (!is.null(titulo)) sprintf("<p class=\"titulo-tab\">%s</p>\n", titulo), h)
	## bloco HTML bruto: o pandoc não precisa interpretar a tabela (muito mais rápido com centenas de tabelas)
	cat("\n\n```{=html}\n", h, "\n```\n\n", sep = "")
	invisible(NULL)
}

## gráfico de coeficientes por horizonte (6, 12, 18, 24 meses): efeito e IC de 95%, pós e placebo lado a lado
graf_h <- function(d, facetas = "rotulo", cor = "janela", titulo = NULL, ylab = "efeito") {
	if (is.null(d) || !nrow(d)) return(invisible(NULL))
	d <- copy(d)
	d[, h := as.integer(sub(".*_", "", outcome))]
	d[, janela := factor(fifelse(janela == "pos", "depois da abertura", "placebo (antes)"), c("depois da abertura", "placebo (antes)"))]
	d[, `:=`(lo = efeito - 1.96 * ep, hi = efeito + 1.96 * ep)]
	g <- ggplot(d, aes(x = h, y = efeito, color = .data[[cor]], group = .data[[cor]])) +
		geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
		geom_errorbar(aes(ymin = lo, ymax = hi), width = 1.2, position = position_dodge(2.2), linewidth = 0.45) +
		geom_point(position = position_dodge(2.2), size = 1.9) +
		scale_x_continuous(breaks = c(6, 12, 18, 24)) +
		scale_color_manual(values = c("depois da abertura" = "#1F5A96", "placebo (antes)" = "#C0632C", "até 90" = "#1F5A96", "acima de 90" = "#8E8E8E"), name = NULL) +
		labs(x = "meses depois da referência", y = ylab, title = titulo) +
		theme_minimal(base_size = 10.5) +
		theme(legend.position = "bottom", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", hjust = 0), plot.title.position = "plot")
	if (!is.null(facetas)) g <- g + facet_wrap(as.formula(paste("~", facetas)), scales = "free_y")
	print(g)
	invisible(NULL)
}

## mapa das linhas e estações por nível (leaflet): rede de 2025, estações de cada abertura e obras futuras
cores_linha <- c(`1` = "#0455A1", `2` = "#007E5E", `3` = "#EE372F", `4` = "#FFD400", `5` = "#9B3894", `6` = "#F68B1F", `7` = "#CA016B",
	`8` = "#97A098", `9` = "#01A9A7", `10` = "#049FC3", `11` = "#F04E23", `12` = "#083E89", `13` = "#00B352", `15` = "#868D91", `17` = "#BE9B43")
## GTFS de trilho: sequência de paradas de cada padrão de viagem (sentido 0) e as formas, quando existem
le_trilho <- function(zipf) {
	le <- function(f) fread(text = readLines(unz(zipf, f), warn = FALSE))
	arq <- unzip(zipf, list = TRUE)$Name
	rt <- le("routes.txt")[route_type %in% c(1L, 2L, 12L)]
	tr <- le("trips.txt")[route_id %in% rt$route_id]
	if (!"shape_id" %in% names(tr)) tr[, shape_id := NA_character_]
	tr[, shape_id := as.character(shape_id)]
	if ("direction_id" %in% names(tr)) tr <- tr[direction_id %in% 0L | !route_id %in% tr[direction_id %in% 0L, route_id]]
	st <- le("stop_times.txt")[trip_id %in% tr$trip_id, .(trip_id, stop_id, stop_sequence)]
	sp <- le("stops.txt")[, .(stop_id, lon = stop_lon, lat = stop_lat)]
	p <- merge(merge(st, sp, by = "stop_id"), tr[, .(trip_id, route_id, shape_id)], by = "trip_id")[order(route_id, trip_id, stop_sequence)]
	p[, linha := sub("^rail_L", "", route_id)]
	## um padrão de paradas por viagem distinta (ramais aparecem como padrões diferentes)
	p[, padrao := paste(stop_id, collapse = ">"), by = trip_id]
	p <- p[trip_id %in% unique(p[, .(route_id, padrao, trip_id)], by = c("route_id", "padrao"))$trip_id]
	list(paradas = p, shapes = if ("shapes.txt" %in% arq) le("shapes.txt")[, shape_id := as.character(shape_id)] else NULL)
}
dist_km <- function(lon1, lat1, lon2, lat2) sqrt(((lon2 - lon1) * 111.32 * cos(lat1 * pi / 180))^2 + ((lat2 - lat1) * 110.57)^2)
## traçados reais das linhas: as formas do GTFS original de trilhos de 2025 (o GTFS montado para o roteamento não tem as das
## linhas 8 e 9) e, para as obras futuras (linhas 6 e 17 e extensão da Linha 2), as linhas projetadas do GeoSampa.
## Devolve, por linha, uma lista de matrizes (lon, lat).
geom_linhas <- function() {
	z <- "data/redes/insumos/gtfs_rail_2025.zip"
	le <- function(f) fread(text = readLines(unz(z, f), warn = FALSE))
	tr <- le("trips.txt")[, shape_id := as.character(shape_id)]
	sh <- le("shapes.txt")[, shape_id := as.character(shape_id)]
	atual <- list()
	for (r in unique(tr$route_id)) {
		x <- sh[shape_id %in% unique(tr[route_id == r, shape_id])]
		if (!nrow(x)) next
		x <- x[shape_id == x[, .N, by = shape_id][order(-N)]$shape_id[1]][order(shape_pt_sequence)]
		atual[[sub("^rail_L", "", r)]] <- list(cbind(x$shape_pt_lon, x$shape_pt_lat))
	}
	partes <- function(g) {
		cc <- sf::st_coordinates(g)
		unname(split.data.frame(cc[, 1:2, drop = FALSE], cc[, ncol(cc)]))
	}
	pj <- sf::st_transform(sf::st_read("data/redes/insumos/planned_subway_lines.gpkg", quiet = TRUE), 4326)
	futura <- list()
	for (i in seq_len(nrow(pj))) {
		l <- as.character(pj$cd_identificador_linha[i])
		futura[[l]] <- c(futura[[l]], partes(pj[i, ]))
	}
	## a extensão da Linha 9 até Varginha (2021) não está na forma do GTFS de 2025; vem do arquivo de linhas de trem do GeoSampa
	pt <- sf::st_transform(sf::st_read("data/redes/insumos/planned_train_lines.gpkg", quiet = TRUE), 4326)
	pt <- pt[pt$cd_identificador_linha %in% 9, ]
	for (i in seq_len(nrow(pt))) atual[["9"]] <- c(atual[["9"]], partes(pt[i, ]))
	list(atual = atual, futura = futura)
}
## caminho entre duas estações ao longo do traçado da linha (o pedaço da forma entre os pontos mais próximos de cada uma);
## se nenhuma forma passa a menos de 1 km das duas, uma reta
caminho <- function(a, b, geoms) {
	melhor <- NULL
	dm <- Inf
	for (g in geoms) {
		da <- dist_km(a[1], a[2], g[, 1], g[, 2])
		db <- dist_km(b[1], b[2], g[, 1], g[, 2])
		ia <- which.min(da)
		ib <- which.min(db)
		if (da[ia] < 1 && db[ib] < 1 && ia != ib && da[ia] + db[ib] < dm) {
			dm <- da[ia] + db[ib]
			melhor <- g[ia:ib, , drop = FALSE]
		}
	}
	if (is.null(melhor)) rbind(a, b) else rbind(a, melhor, b)
}
## trechos abertos por um nível: entre estações consecutivas da linha em que uma ponta é estação (não infill) do nível e a
## outra ponta já existia (estação anterior a 2013 ou aberta antes) ou é do mesmo nível. Assim, uma extensão aberta
## em etapas mostra só o trecho daquela etapa; estações de infill (num trecho que já existia) não acrescentam trecho.
## todas: estações de todos os níveis (para saber a data de abertura de cada ponta)
trechos_nivel <- function(g, e, todas, geoms = NULL) {
	L <- e$rede[1]
	dL <- if (e$acao[1] == "acrescenta") Inf else as.numeric(min(as.IDate(e$data_abertura)))
	todas <- copy(todas)
	todas[, dt := fifelse(acao == "acrescenta", Inf, as.numeric(as.IDate(data_abertura)))]
	out <- list()
	for (l in unique(e$code_line)) {
		p <- g$paradas[linha == as.character(l)]
		tl <- todas[code_line == l]
		for (tid in unique(p$trip_id)) {
			q <- p[trip_id == tid]
			if (nrow(q) < 2L) next
			## a que nível pertence cada parada (a estação da lista mais próxima, até 0,4 km); NA = estação antiga
			ix <- vapply(seq_len(nrow(q)), function(k) {
				d <- dist_km(q$lon[k], q$lat[k], tl$lon, tl$lat)
				if (length(d) && min(d) < 0.4) which.min(d) else NA_integer_
			}, integer(1))
			rede_q <- tl$rede[ix]
			inf_q <- tl$infill[ix] %in% 1L
			dt_q <- fifelse(is.na(ix), -Inf, tl$dt[ix])
			## a linha como era quando o nível abriu: só as estações que já existiam e as do próprio nível
			fica <- which(is.na(rede_q) | dt_q <= dL | rede_q %in% L)
			if (length(fica) < 2L) next
			for (j in seq_len(length(fica) - 1L)) {
				a <- fica[j]
				b <- fica[j + 1L]
				eh <- function(i) !is.na(rede_q[i]) && rede_q[i] == L && !inf_q[i]
				if (!(eh(a) || eh(b))) next
				cc <- if (!is.null(geoms[[as.character(l)]])) caminho(c(q$lon[a], q$lat[a]), c(q$lon[b], q$lat[b]), geoms[[as.character(l)]]) else cbind(q$lon[a:b], q$lat[a:b])
				out[[length(out) + 1L]] <- data.table(id = paste(tid, a), linha = as.character(l), lon = cc[, 1], lat = cc[, 2])
			}
		}
	}
	if (length(out)) unique(rbindlist(out)) else NULL
}
mapa_linhas <- function(niv, ev) {
	est <- fread("data/redes/estacoes_familia_a.csv", encoding = "UTF-8")
	est <- merge(est, niv[, .(rede = nivel, pleno, abertura_nivel = abertura)], by = "rede", all.x = TRUE)
	est[, ev_id := ev$ev[match(as.integer(pleno), ev$d_e)]]
	est[, status := fcase(acao == "acrescenta", "obra futura", !is.na(ev_id), "aberta no período (evento)", default = "aberta em 2025")]
	G <- list(A00 = le_trilho("data/redes/familia_a/A00/trilho_A00.zip"))
	for (r in unique(est[acao == "acrescenta", rede])) G[[r]] <- le_trilho(sprintf("data/redes/familia_a/%s/trilho_%s.zip", r, r))
	## rede de 2025: os traçados reais do GTFS original de trilhos de 2025 (todas as linhas em operação)
	GL <- geom_linhas()
	base <- rbindlist(lapply(names(GL$atual), function(l) rbindlist(lapply(seq_along(GL$atual[[l]]), function(k)
		data.table(id = paste(l, k), linha = l, lon = GL$atual[[l]][[k]][, 1], lat = GL$atual[[l]][[k]][, 2])))))
	## fundos: o cinza do Esri é o padrão (não bloqueia o HTML aberto como arquivo local, ao contrário do OpenStreetMap)
	fundos <- c("Fundo cinza (Esri)", "Ruas (Esri)", "OpenStreetMap")
	m <- leaflet::leaflet(width = "100%", height = 640, options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
		leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, group = fundos[1]) |>
		leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap, group = fundos[2]) |>
		leaflet::addTiles(group = fundos[3]) |>
		leaflet::fitBounds(min(est$lon), min(est$lat), max(est$lon), max(est$lat))
	for (s in unique(base$id)) {
		x <- base[id == s]
		m <- leaflet::addPolylines(m, lng = x$lon, lat = x$lat, color = cores_linha[[x$linha[1]]], weight = 3, opacity = 0.7,
			group = "rede de 2025", label = paste("Linha", x$linha[1]))
	}
	est[, grupo := fifelse(status == "obra futura", sprintf("%s · obra futura (Linha %s)", rede, code_line),
		sprintf("%s · Linha %s · serviço pleno %s%s", rede, code_line, format(pleno, "%d/%m/%Y"), fifelse(is.na(ev_id), " (2025, fora dos eventos)", paste0(" (evento ", ev_id, ")"))))]
	## trecho de cada nível: destacado (largo) nas aberturas; tracejado nas obras futuras
	for (gq in unique(est$grupo)) {
		x <- est[grupo == gq]
		fut <- x$acao[1] == "acrescenta"
		tq <- trechos_nivel(if (fut) G[[x$rede[1]]] else G$A00, x, est, if (fut) GL$futura else GL$atual)
		if (is.null(tq)) next
		for (s in unique(tq$id)) {
			y <- tq[id == s]
			m <- leaflet::addPolylines(m, lng = y$lon, lat = y$lat, color = cores_linha[[y$linha[1]]], weight = if (fut) 4 else 8, opacity = if (fut) 0.9 else 0.55,
				dashArray = if (fut) "7 6" else NULL, group = gq, label = paste0("Linha ", y$linha[1], if (fut) " (obra futura)" else paste0(" · nível ", x$rede[1])))
		}
	}
	est[, cor := cores_linha[as.character(code_line)]]
	est[, pop := sprintf("<b>%s</b><br>Linha %s · nível %s<br>%s<br>abertura: %s<br>serviço pleno: %s", name_station, code_line, rede, status,
		fifelse(is.na(abertura_nivel), "—", format(abertura_nivel, "%d/%m/%Y")), fifelse(is.na(pleno), "—", format(pleno, "%d/%m/%Y")))]
	grupos <- unique(est[order(rede), grupo])
	for (gq in grupos) {
		x <- est[grupo == gq]
		m <- leaflet::addCircleMarkers(m, lng = x$lon, lat = x$lat, radius = 6, stroke = TRUE, color = "#222222", weight = 1, fillColor = x$cor,
			fillOpacity = fifelse(x$status == "obra futura", 0.35, 0.95), popup = x$pop, label = x$name_station, group = gq)
	}
	m |>
		leaflet::addLayersControl(baseGroups = fundos, overlayGroups = c("rede de 2025", grupos), options = leaflet::layersControlOptions(collapsed = TRUE)) |>
		leaflet::addLegend(position = "bottomleft", colors = c("#333333", "#333333", "#333333"),
			labels = c("círculo cheio: estação aberta (evento ou 2025)", "faixa larga: trecho aberto naquele nível (extensões; estações de infill só têm o círculo)",
				"círculo claro e linha tracejada: obra futura"), opacity = c(0.95, 0.55, 0.35), title = "Estações e trechos por nível")
}

## ---- desenho A: indicador, medida (absoluta ou percentual) e efeito por 1 DP ----------------------------------------------
OC_TAB_A <- c("emprego_12", "emprego_24", "entrada_12", "entrada_24", "empregado_12", "empregado_24", "meses_12", "salario_12", "salario_24")
prepara_area <- function(d) {
	if (is.null(d)) return(NULL)
	d <- copy(d)
	if (!"fe" %in% names(d)) d[, fe := NA_character_]
	d[is.na(fe), fe := fifelse(grepl("_fe_ev", arquivo), "ev", fifelse(grepl("_fe_casa8", arquivo), "casa8", "casa7"))]
	d[, ind := fcase(grepl("30 min", dose), "30 min", grepl("45 min", dose), "45 min", grepl("60 min", dose), "60 min", grepl("90 min", dose), "90 min", default = "gravitacional")]
	d[, ind := factor(ind, c("30 min", "45 min", "60 min", "90 min", "gravitacional"))]
	d[, medida := fifelse(grepl("10%|log", dose), "percentual", "absoluto")]
	if ("ep" %in% names(d) && "dose_dp" %in% names(d)) d[, ep_1dp := ep * dose_dp]
	d[, desf := factor(sub("_[0-9]+$", "", outcome), c("emprego", "entrada", "empregado", "meses", "salario"),
		c("emprego", "entrada", "permanência", "fração dos meses empregado", "salário-hora (%)"))]
	d[, `:=`(versao = ROT_IND(as.character(ind), medida), por = por_area(as.character(ind), medida))]
	d[]
}
## a dose de cada indicador e medida, para as frases ("o efeito de +10% na acessibilidade de 60 min sobre ...")
por_area <- function(ind, medida) fifelse(medida == "absoluto",
	fifelse(ind == "gravitacional", "de +100 mil empregos ponderados (gravitacional)", paste0("de +100 mil empregos alcançáveis em ", ind)),
	fifelse(ind == "gravitacional", "de +10% na acessibilidade gravitacional", paste0("de +10% nos empregos alcançáveis em ", ind)))
graf_area <- function(d) {
	d <- copy(d[desf %in% c("emprego", "entrada", "permanência", "salário-hora (%)")])
	if (!nrow(d)) return(invisible(NULL))
	d[, `:=`(h = as.integer(sub(".*_", "", outcome)), jan = factor(fifelse(janela == "pos", "depois da abertura", "placebo (antes)")),
		grupo = paste(medida, janela))]
	g <- ggplot(d, aes(h, efeito_1dp, color = jan, shape = medida, group = grupo)) + geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
		geom_errorbar(aes(ymin = efeito_1dp - 1.96 * ep_1dp, ymax = efeito_1dp + 1.96 * ep_1dp), width = 1.4, position = position_dodge(3.2), linewidth = 0.4) +
		geom_point(position = position_dodge(3.2), size = 1.6) + facet_grid(desf ~ ind, scales = "free_y") +
		scale_x_continuous(breaks = c(6, 12, 18, 24)) + scale_color_manual(values = c("#1F5A96", "#C0632C"), name = NULL) +
		scale_shape_manual(values = c(absoluto = 16, percentual = 17), name = "ganho") +
		labs(x = "meses depois da referência", y = "efeito de 1 desvio-padrão do ganho (p.p.; salário em %)") +
		theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
	print(g)
	invisible(NULL)
}
ESPEC_AREA <- c("Principal (H3-9; vizinhos do mesmo bairro, célula H3-7)", "Agregado em H3-8", "Vizinhança menor (célula H3-8)", "Sem comparação por bairro (só evento)")
especificacao_area <- function(nivel, fe) fcase(nivel == "h39" & fe == "casa7", ESPEC_AREA[1], nivel == "h38" & fe == "casa7", ESPEC_AREA[2],
	nivel == "h39" & fe == "casa8", ESPEC_AREA[3], nivel == "h39" & fe == "ev", ESPEC_AREA[4], default = NA_character_)
ESPEC_PAR <- c("Principal (pares H3-9; vizinhos de casa e de trabalho, células H3-7)", "Agregado em H3-8", "Vizinhança menor (células H3-8 de casa e de trabalho)",
	"Só vizinhança de casa (célula H3-7)", "Sem comparação por bairro (só evento)", "Com pares de tempo zero")
especificacao_par <- function(nivel, fe, arquivo) fcase(grepl("tempo0", arquivo), ESPEC_PAR[6], nivel == "h38", ESPEC_PAR[2], fe == "casa8trab8", ESPEC_PAR[3],
	fe == "casa7", ESPEC_PAR[4], fe == "ev", ESPEC_PAR[5], default = ESPEC_PAR[1])
ROT_TESTE <- c("principal (modelo A)" = "principal", "R1 outras aberturas" = "controlando outras aberturas do período", "R2 efeitos fixos H3-8" = "vizinhança menor (células H3-8)",
	"R2 efeito fixo H3-8" = "vizinhança menor (células H3-8)",
	"R6 faixas de ganho em minutos" = "faixas de ganho em minutos", "R6 faixas de ganho em % (abaixo de 1% = zero)" = "faixas de ganho em % (abaixo de 1% = zero)",
	"R6 quintis do ganho" = "quintis do ganho", "R6 interação com o tempo antes" = "interação com o tempo de trajeto antes", "R6 dose percentual" = "ganho em % do tempo, dividido em 90 min",
	"R8 dose única em minutos (sem divisão em 90 min)" = "ganho em minutos, sem dividir em 90 min", "R8 dose única percentual (sem divisão em 90 min)" = "ganho em % do tempo, sem dividir em 90 min")
rot_teste <- function(t) ifelse(t %in% names(ROT_TESTE), ROT_TESTE[t], t)
ROT_FAIXA <- c(f1a5 = "ganho de 1 a 5", f5a10 = "ganho de 5 a 10 min", f10mais = "ganho de 10 min ou mais", f5a20 = "ganho de 5 a 20%", f20mais = "ganho de 20% ou mais",
	todas = "todas as faixas")
rot_faixa <- function(f) ifelse(f %in% names(ROT_FAIXA), ROT_FAIXA[f], f)
graf_loo <- function(d, principal) {
	d <- copy(d[grepl("^R3", teste)])
	if (!nrow(d)) return(invisible(NULL))
	p <- copy(principal[outcome %in% unique(d$outcome)])
	d[, `:=`(ind = fifelse(dose == "c60", "60 min (absoluta)", "gravitacional (log)"), oc = rot_oc(outcome))]
	p[, `:=`(ind = fifelse(dose == "c60", "60 min (absoluta)", "gravitacional (log)"), oc = rot_oc(outcome))]
	g <- ggplot(d, aes(factor(evento_fora), efeito)) + geom_hline(data = p, aes(yintercept = efeito), linetype = 2, color = "#1F5A96") +
		geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
		geom_errorbar(aes(ymin = efeito - 1.96 * ep, ymax = efeito + 1.96 * ep), width = 0.3, color = "grey45", linewidth = 0.35) + geom_point(size = 1.5, color = "#1F5A96") +
		facet_grid(oc ~ ind, scales = "free_y") + labs(x = "evento retirado", y = "efeito sem o evento") +
		theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
	print(g)
	invisible(NULL)
}

## ---- textos do resumo ---------------------------------------------------------------------------------------------------
txt_ef <- function(b, se, p, d = 2) paste0(sinal(b, d), estrela(p), " (", num(se, d), ")")
resumo_a <- function(A) {
	if (is.null(A)) return("Em execução.")
	x <- A[nivel == "h39" & fe == "casa7" & segmento == "todos" & outcome %in% c("emprego_12", "entrada_12", "salario_24")]
	if (!nrow(x)) return("Em execução.")
	f <- function(i, m, o, j) {
		z <- x[ind == i & medida == m & outcome == o & janela == j]
		if (!nrow(z)) return("—")
		txt_ef(z$efeito_1dp, z$ep_1dp, z$p_perm, 3)
	}
	paste0("<b>Sem efeito sobre emprego e entrada.</b> Efeito de 1 desvio-padrão do ganho sobre o emprego em 12 meses: ", f("gravitacional", "percentual", "emprego_12", "pos"),
		" p.p. na gravitacional (placebo ", f("gravitacional", "percentual", "emprego_12", "pre"), ") e ", f("60 min", "absoluto", "emprego_12", "pos"),
		" p.p. na de 60 min (placebo ", f("60 min", "absoluto", "emprego_12", "pre"), "); entrada em 12 meses: ", f("60 min", "absoluto", "entrada_12", "pos"),
		" p.p. (60 min). Salário em 24 meses: ", f("60 min", "absoluto", "salario_24", "pos"), "% por desvio-padrão (60 min): positivo, mas minúsculo.")
}
resumo_b <- function(P) {
	if (is.null(P)) return("Em execução.")
	f <- function(s, o, j) {
		z <- unique(P[nivel == "h39" & fe == "casa7trab7" & modelo == "A" & segmento == s & termo == "trajeto, até 90" & outcome == o & janela == j], by = "janela")
		if (!nrow(z)) return("em execução")
		txt_ef(z$efeito, z$ep, z$p_perm)
	}
	paste0("<b>Menos demissões.</b> Cada 10 minutos a menos no trajeto (trajetos de até 90 min) muda a probabilidade de demissão em 6 meses em: todos ", f("todos", "demissao_6", "pos"),
		" p.p. (placebo ", f("todos", "demissao_6", "pre"), "); mulheres ", f("mulheres", "demissao_6", "pos"), " (placebo ", f("mulheres", "demissao_6", "pre"),
		"); homens ", f("homens", "demissao_6", "pos"), " (placebo ", f("homens", "demissao_6", "pre"), "). A queda vem da demissão sem justa causa; o pedido de demissão não muda.")
}
resumo_c <- function(P) if (is.null(P) || !nrow(P[grepl("^ped_", outcome)])) "Em execução." else "Ver a seção C: destino de quem pediu demissão (trajeto, salário e tempo até o novo vínculo)."
resumo_d <- function(IP, IA, BAL = NULL) {
	aviso <- ""
	if (!is.null(BAL)) {
		z <- BAL[nivel == "ind" & segmento == "todos" & janela == "pos" & trajeto == "até 90" & faixa == "f10mais"]
		if (nrow(z) && (z$smd_max_depois[1] >= 0.1 || z$ess_controles[1] < 1000)) aviso <- sprintf(
			" <b>Cautela:</b> nessa faixa o balanço não é bom (maior diferença padronizada de %s depois dos pesos; só %s controles efetivos), então o IPTW pesa menos que o B&amp;H.",
			num(z$smd_max_depois[1], 2), num(z$ess_controles[1], 0))
	}
	f <- function(s, j) {
		if (is.null(IP)) return("em execução")
		z <- IP[nivel == "ind" & segmento == s & trajeto == "até 90" & faixa == "f10mais" & outcome == "demissao_6" & janela == j]
		if (!nrow(z)) return("em execução")
		txt_ef(z$att, z$ep, z$p)
	}
	paste0("<b>Tempo de viagem:</b> quem ganhou 10 minutos ou mais (trajetos de até 90 min) tem a demissão em 6 meses alterada em ", f("todos", "pos"), " p.p. (placebo ", f("todos", "pre"),
		"); mulheres ", f("mulheres", "pos"), " (placebo ", f("mulheres", "pre"), "); homens ", f("homens", "pos"), " (placebo ", f("homens", "pre"), ").", aviso, " ",
		if (is.null(IA)) "<b>Área:</b> em execução." else "<b>Área:</b> ver a seção D.")
}
resumo_e <- function(FA, FP) {
	if (is.null(FA) && is.null(FP)) return("Em execução.")
	f <- function(s, j) {
		if (is.null(FP)) return("em execução")
		z <- FP[nivel == "h39" & esquema %in% c("todos", "sexo") & segmento == s & trajeto == "até 90" & faixa == "todas" & outcome == "demissao_6" & janela == j]
		if (!nrow(z)) return("em execução")
		txt_ef(z$efeito, z$ep, z$p)
	}
	g <- function(j) {
		if (is.null(FA)) return("em execução")
		z <- FA[nivel == "h39" & esquema == "todos" & faixa == "todas" & corte == "c60" & outcome == "emprego_12" & janela == j]
		if (!nrow(z)) return("em execução")
		txt_ef(z$efeito, z$ep, z$p)
	}
	paste0("<b>Tempo de viagem:</b> ser tratado (todas as faixas, trajetos de até 90 min), contra quem terá o mesmo ganho depois, muda a demissão em 6 meses em ", f("todos", "pos"),
		" p.p. (placebo ", f("todos", "pre"), "); mulheres ", f("mulheres", "pos"), "; homens ", f("homens", "pos"), ". <b>Área:</b> emprego em 12 meses (60 min) ", g("pos"),
		" (placebo ", g("pre"), ").")
}

## ---- organização comum das tabelas: uma tabela por categoria de desfecho, linhas = grupo, colunas em ordem cronológica ----
SEGS <- c("todos", "mulheres", "homens", "18 a 29", "30 a 44", "45 a 60", "com criança", "sem criança", "mulheres com criança", "mulheres sem criança",
	"homens com criança", "homens sem criança", "vínculo até 12 meses", "vínculo mais de 12 meses")
CAT_AREA <- c(emprego = "Emprego", entrada = "Entrada no emprego formal", empregado = "Permanência no emprego", meses = "Fração dos meses empregado", salario = "Salário-hora")
CAT_PAR <- c(demissao = "Demissão (total)", sem_justa = "Demissão sem justa causa", justa = "Demissão com justa causa", pedido = "Pedido de demissão",
	qualquer = "Qualquer desligamento", empregado = "Permanência no emprego", meses = "Fração dos meses empregado")
categoria <- function(o) sub("_[0-9]+$", "", o)
horizonte <- function(o) as.integer(sub(".*_", "", o))
## tabela larga com colunas "6 meses · depois", "6 meses · placebo", "12 meses · depois"...
tab_cat <- function(d, linha, valor = "c", hs = NULL) {
	d <- copy(d)
	d[, h := horizonte(outcome)]
	if (!is.null(hs)) d <- d[h %in% hs]
	d[, coluna := paste0(h, " meses · ", fifelse(janela == "pos", "depois", "placebo"))]
	niveis <- unique(d[order(h, janela != "pos")]$coluna)
	w <- dcast(d, as.formula(paste0("`", linha, "` ~ coluna")), value.var = valor, fill = "—")
	setcolorder(w, c(linha, intersect(niveis, names(w))))
	for (cc in names(w)) if (is.character(w[[cc]])) set(w, which(is.na(w[[cc]]) | w[[cc]] == ""), cc, "—")
	w
}
## ordem das diferenças entre grupos
ORD_CT <- c("mulheres − homens", "30 a 44 − 18 a 29", "45 a 60 − 18 a 29", "idade: teste conjunto de igualdade (3 grupos)", "com criança − sem criança",
	"mulheres com criança − mulheres sem criança", "homens com criança − homens sem criança", "(mulheres com − sem criança) − (homens com − sem criança)",
	"vínculo até 12 meses − vínculo mais de 12 meses")
## célula de uma diferença entre grupos (o teste conjunto da idade não tem diferença, só o p)
cel_ct <- function(dif, ep, p, d = 2) fifelse(is.na(dif) & !is.na(p), paste0("teste conjunto: p = ", num(p, 3)), cel(dif, ep, p, d))
## uma aba por categoria; linha = coluna que vira as linhas (ex.: segmento); ordem = ordem das linhas; nivel = nível do título
## das abas (2 = ##; 3 = ### quando as abas ficam dentro de outras abas); ct = diferenças entre grupos (colunas outcome, janela,
## contraste, c), mostradas logo abaixo da tabela da mesma categoria
## bloco = coluna que divide cada aba em várias tabelas empilhadas (ex.: faixa de ganho), na ordem ord_bloco, com o título rot_bloco()
tabs_categoria <- function(d, cats, linha, nome_linha, ordem = SEGS, hs = NULL, legenda = NULL, nivel = 2, ct = NULL, leg_ct = NULL,
	bloco = NULL, ord_bloco = NULL, rot_bloco = identity) {
	d <- d[categoria(outcome) %in% names(cats)]
	if (!nrow(d)) return(invisible(NULL))
	um <- function(x, y, titulo) {
		w <- tab_cat(x, linha, hs = hs)
		if (!is.null(ordem)) { o <- order(match(w[[linha]], ordem)); w <- w[o] }
		setnames(w, linha, nome_linha)
		tabela(w, titulo = titulo, legenda = legenda)
		if (!is.null(y) && nrow(y)) {
			wc <- tab_cat(y, "contraste", hs = hs)
			wc <- wc[order(match(contraste, ORD_CT))]
			setnames(wc, "contraste", "Diferença entre grupos")
			tabela(wc, titulo = paste0("Diferenças entre grupos", if (!is.null(titulo)) paste0(" (", tolower(titulo), ")")),
				legenda = if (is.null(leg_ct)) "Efeito no primeiro grupo menos o efeito no segundo, estimados juntos; entre parênteses, o erro-padrão da diferença." else leg_ct)
		}
	}
	cat("\n::: {.panel-tabset}\n")
	for (k in names(cats)) {
		x <- d[categoria(outcome) == k]
		if (!nrow(x)) next
		cat("\n", strrep("#", nivel), " ", cats[[k]], "\n\n", sep = "")
		y <- if (!is.null(ct) && nrow(ct)) ct[categoria(outcome) == k] else NULL
		if (is.null(bloco)) um(x, y, NULL) else for (b in intersect(ord_bloco, unique(x[[bloco]]))) { ix <- which(x[[bloco]] == b); iy <- if (!is.null(y)) which(y[[bloco]] == b); um(x[ix], if (!is.null(y)) y[iy], rot_bloco(b)) }
	}
	cat("\n:::\n")
	invisible(NULL)
}
## só as diferenças entre grupos, uma aba por categoria (linhas = diferença)
tabs_contrastes <- function(ct, cats, hs = NULL, nivel = 2, legenda = NULL) {
	ct <- ct[categoria(outcome) %in% names(cats)]
	if (!nrow(ct)) return(invisible(NULL))
	cat("\n::: {.panel-tabset}\n")
	for (k in names(cats)) {
		y <- ct[categoria(outcome) == k]
		if (!nrow(y)) next
		cat("\n", strrep("#", nivel), " ", cats[[k]], "\n\n", sep = "")
		wc <- tab_cat(y, "contraste", hs = hs)
		wc <- wc[order(match(contraste, ORD_CT))]
		setnames(wc, "contraste", "Diferença entre grupos")
		tabela(wc, legenda = if (is.null(legenda)) "Efeito no primeiro grupo menos o efeito no segundo, estimados juntos; entre parênteses, o erro-padrão da diferença." else legenda)
	}
	cat("\n:::\n")
	invisible(NULL)
}
## bloco que abre e fecha (callout do Quarto), para conteúdo em markdown (inclusive abas)
abre <- function(titulo) cat(sprintf('\n::: {.callout-note collapse="true" appearance="simple"}\n## %s\n\n', titulo))
fecha <- function() cat("\n:::\n")

## ---- leitura guiada: resumo automático dos números -----------------------------------------------------------------------
classe <- function(p, pp) fifelse(is.na(p), "sem estimativa", fifelse(p < 0.05 & (is.na(pp) | pp >= 0.10), "efeito",
	fifelse(p < 0.05, "efeito, mas o placebo também é diferente de zero (cautela)", fifelse(p < 0.10, "efeito fraco (só a 10%)", "sem efeito distinguível de zero"))))
pareia <- function(d, chave, bcol = "efeito", secol = "ep", pcol = "p_perm") {
	pos <- d[janela == "pos", c(chave, bcol, secol, pcol), with = FALSE]
	pre <- d[janela == "pre", c(chave, bcol, pcol), with = FALSE]
	setnames(pos, c(bcol, secol, pcol), c("b", "se", "p"))
	setnames(pre, c(bcol, pcol), c("b0", "p0"))
	merge(pos, pre, by = chave, all.x = TRUE)
}

## ---- desenho B: distribuição do ganho de tempo, gráfico, tabelas, B2, robustez ----------------------------------------------
wq <- function(x, w, p) {
	if (!length(x)) return(NA_real_)
	o <- order(x)
	cw <- cumsum(w[o]) / sum(w)
	x[o][which(cw >= p)[1]]
}
distrib_ganho_par <- function(dd, EV) {
	C <- as.data.table(arrow::read_parquet(file.path(dd, "par_celulas.parquet"), col_select = c("ip", "evento", "janela", "n", "t_antes")))[janela == "pos" & t_antes > 0 & t_antes <= 241,
		.(n = sum(n)), by = .(ip, evento, t_antes)]
	PR <- as.data.table(arrow::read_parquet(file.path(dd, "par_pares.parquet")))
	lvc <- grep("^A[0-9]{2}$", names(PR), value = TRUE)
	DL <- as.matrix(PR[, lvc, with = FALSE])
	C[, pr := match(ip, PR$ip)]
	C[, ganho := 0]
	for (e in unique(C$evento)) {
		lv <- intersect(strsplit(EV[ev == e, niveis], ",")[[1]], lvc)
		ix <- which(C$evento == e)
		set(C, ix, "ganho", rowSums(DL[C$pr[ix], lv, drop = FALSE]))
	}
	f <- function(x) {
		g <- x$ganho >= 1
		data.table(`Vínculos × evento` = num(sum(x$n), 0), `Com ganho de 1 min ou mais` = paste0(num(100 * sum(x$n[g]) / sum(x$n), 1), "%"),
			`Ganho médio, todos (min)` = num(weighted.mean(x$ganho, x$n), 2), `Ganho médio de quem ganhou (min)` = num(weighted.mean(x$ganho[g], x$n[g]), 1),
			`Mediana de quem ganhou (min)` = num(wq(x$ganho[g], x$n[g], 0.5), 1), `Percentil 90 de quem ganhou (min)` = num(wq(x$ganho[g], x$n[g], 0.9), 1))
	}
	rbind(cbind(`Trajeto antes do evento` = "todos", f(C)), cbind(`Trajeto antes do evento` = "até 90 min", f(C[t_antes <= 90])),
		cbind(`Trajeto antes do evento` = "mais de 90 min", f(C[t_antes > 90])))
}
par_principal <- function(P, md = "A", tm = "trajeto, até 90") unique(P[nivel == "h39" & fe == "casa7trab7" & modelo == md & termo == tm & !grepl("^ped_", outcome)],
	by = c("segmento", "outcome", "janela"))
## recorte de trajeto do modelo só com o trajeto (A para até 90 e acima de 90; U para todos juntos)
par_trj <- function(P, trj = "ate90") {
	tt <- TERMOS_TRJ[[trj]]
	par_principal(P, tt$mA, if (trj == "todos") tt$tr else sub("^trajeto", "trajeto", tt$tr))
}
graf_par <- function(P, segs, trj = "ate90") {
	d <- par_trj(P, trj)[segmento %in% segs & categoria(outcome) %in% c("demissao", "sem_justa", "justa", "pedido", "empregado")]
	if (!nrow(d)) return(invisible(NULL))
	d[, `:=`(h = horizonte(outcome), catr = factor(CAT_PAR[categoria(outcome)], CAT_PAR), seg = factor(segmento, segs),
		jan = factor(fifelse(janela == "pos", "depois da abertura", "placebo (antes)"), c("depois da abertura", "placebo (antes)")))]
	g <- ggplot(d, aes(h, efeito, color = jan)) + geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
		geom_errorbar(aes(ymin = efeito - 1.96 * ep, ymax = efeito + 1.96 * ep), width = 1.3, position = position_dodge(2.4), linewidth = 0.45) +
		geom_point(position = position_dodge(2.4), size = 1.7) + facet_grid(catr ~ seg, scales = "free_y", labeller = label_wrap_gen(16)) +
		scale_x_continuous(breaks = c(6, 12, 18, 24)) + scale_color_manual(values = c("#1F5A96", "#C0632C"), name = NULL, drop = FALSE) +
		labs(x = "meses depois da referência", y = "efeito de 10 min a menos no trajeto (p.p.)") +
		theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
	print(g)
	invisible(NULL)
}
tabelas_par_categoria <- function(P, modelo_x, termo_x, legenda = NULL, nivel = 2) {
	d <- par_principal(P, modelo_x, termo_x)
	if (!nrow(d)) return(em_execucao())
	d[, c := cel(efeito, ep, p_perm)]
	tabs_categoria(d, CAT_PAR, "segmento", "Grupo", legenda = legenda, nivel = nivel)
}
tabelas_b2 <- function(P, trj = "ate90", nivel = 2) {
	tt <- TERMOS_TRJ[[trj]]
	d <- unique(P[nivel == "h39" & fe == "casa7trab7" & modelo == tt$md & termo %in% c(tt$tr, tt$ac) & !grepl("^ped_", outcome)],
		by = c("segmento", "outcome", "janela", "termo"))
	if (!nrow(d)) return(em_execucao())
	d[, `:=`(c = cel(efeito, ep, p_perm), h = horizonte(outcome))]
	d[, coluna := paste0(fifelse(termo == tt$tr, "trajeto 10 min mais curto", "acessibilidade +10%"), " · ", h, " meses")]
	cat("\n::: {.panel-tabset}\n")
	for (k in c("demissao", "sem_justa", "pedido", "empregado")) {
		x <- d[categoria(outcome) == k & janela == "pos"]
		if (!nrow(x)) next
		cat("\n", strrep("#", nivel), " ", CAT_PAR[[k]], "\n\n", sep = "")
		niveis <- unique(x[order(h, termo != tt$tr)]$coluna)
		w <- dcast(x, segmento ~ coluna, value.var = "c", fill = "—")
		setcolorder(w, c("segmento", intersect(niveis, names(w))))
		w <- w[order(match(segmento, SEGS))]
		setnames(w, "segmento", "Grupo")
		tabela(w, legenda = "Depois da abertura; os dois efeitos são estimados juntos na mesma regressão.")
	}
	cat("\n:::\n")
}
ROB_PAR_TXT <- list(
	"R1 outras aberturas" = list(titulo = "Controle de outras aberturas", txt = "Enquanto acompanhamos um evento, outras estações abrem e também podem encurtar trajetos. Este teste acrescenta à regressão o ganho de tempo trazido por todas as outras aberturas dos 12 meses anteriores e dos 24 meses de acompanhamento (também recentrado pelos sorteios). <b>Objetivo:</b> verificar que o efeito atribuído ao evento não vem de outras aberturas próximas no tempo."),
	"R2 efeitos fixos H3-8" = list(titulo = "Vizinhança menor", txt = "Troca as células de bairro de cerca de 5 km² (H3-7) por células de cerca de 0,7 km² (H3-8), de casa e de trabalho. <b>Objetivo:</b> comparar trabalhadores ainda mais parecidos; se o efeito se mantém, ele não vem de diferenças dentro dos bairros maiores."),
	"R6 faixas de ganho em minutos" = list(titulo = "Faixas de ganho em minutos", txt = "Em vez de supor que cada minuto a menos tem o mesmo efeito, estima um efeito para cada faixa de ganho (0 a 5, 5 a 10, 10 a 20 e 20 minutos ou mais), em trajetos de até 90 min. <b>Objetivo:</b> ver se o efeito cresce com o tamanho do ganho, como esperado se ele for causal."),
	"R6 faixas de ganho em % (abaixo de 1% = zero)" = list(titulo = "Faixas de ganho em % do tempo", txt = "O mesmo, com o ganho medido em % do tempo de antes (1 a 10% e 10% ou mais; menos de 1% conta como zero). <b>Objetivo:</b> conferir que ganhos muito pequenos, que podem ser só ruído do cálculo de rotas, não dirigem o resultado."),
	"R6 quintis do ganho" = list(titulo = "Quintis do ganho", txt = "Divide quem ganhou em cinco grupos do mesmo tamanho, do menor ao maior ganho. <b>Objetivo:</b> outra forma de ver se o efeito cresce com o ganho."),
	"R6 interação com o tempo antes" = list(titulo = "Interação com o tempo de trajeto de antes", txt = "Permite que o efeito de 10 minutos a menos dependa de quão longo era o trajeto antes. <b>Objetivo:</b> ver se o efeito é maior para quem tinha trajetos mais longos."),
	"R6 dose percentual" = list(titulo = "Ganho em % do tempo, separado em 90 min", txt = "Mede o ganho em % do tempo de antes, mantendo a separação em até 90 e acima de 90 min. <b>Objetivo:</b> conferir que o resultado não depende de medir o ganho em minutos."),
	"R8 dose única em minutos (sem divisão em 90 min)" = list(titulo = "Sem separar trajetos, ganho em minutos", txt = "Um único efeito para todos os trajetos, sem separar os de até 90 e os de mais de 90 minutos, com o ganho em minutos. <b>Objetivo:</b> conferir que o resultado não é criado pela separação em 90 minutos."),
	"R8 dose única percentual (sem divisão em 90 min)" = list(titulo = "Sem separar trajetos, ganho em % do tempo", txt = "O mesmo, com o ganho medido em % do tempo de antes (por 10%). Em % do tempo, um ganho de 10 minutos pesa mais num trajeto de 30 minutos que num de 120. <b>Objetivo:</b> conferir que o resultado não depende de medir o ganho em minutos nem da separação em 90 minutos."))
robustez_par_texto <- function(R) {
	for (t in names(ROB_PAR_TXT)) {
		x <- R[teste == t & categoria(outcome) %in% c("demissao", "pedido")]
		if (!nrow(x)) next
		cat("\n#### ", ROB_PAR_TXT[[t]]$titulo, "\n\n", ROB_PAR_TXT[[t]]$txt, "\n\n", sep = "")
		x[, `:=`(c = cel(efeito, ep, p_perm), linha = paste0(segmento, " · ", termo))]
		tabs_categoria(x, CAT_PAR[c("demissao", "pedido")], "linha", "Grupo · termo", ordem = NULL, hs = c(6, 12, 24), nivel = 5)
	}
	p <- R[teste == "principal (modelo A)" & termo == "trajeto, até 90" & categoria(outcome) == "demissao"]
	if (nrow(p)) {
		cat("\n#### Comparação: a especificação principal\n\nPara referência, o mesmo desfecho na especificação principal.\n\n")
		p[, `:=`(c = cel(efeito, ep, p_perm))]
		tabela(tab_cat(p, "segmento", hs = c(6, 12, 24))[order(match(segmento, SEGS))])
	}
}
graf_loo_par <- function(R) {
	d <- R[grepl("^R3", teste) & janela == "pos" & termo == "trajeto, até 90" & outcome %in% c("demissao_6", "demissao_12")]
	if (!nrow(d)) return(invisible(NULL))
	p <- R[teste == "principal (modelo A)" & janela == "pos" & termo == "trajeto, até 90" & outcome %in% c("demissao_6", "demissao_12")]
	cat("")
	d[, oc := rot_oc(outcome)]
	p[, oc := rot_oc(outcome)]
	g <- ggplot(d, aes(factor(evento_fora), efeito)) + geom_hline(data = p, aes(yintercept = efeito), linetype = 2, color = "#1F5A96") +
		geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
		geom_errorbar(aes(ymin = efeito - 1.96 * ep, ymax = efeito + 1.96 * ep), width = 0.3, color = "grey45", linewidth = 0.35) + geom_point(size = 1.5, color = "#1F5A96") +
		facet_grid(oc ~ segmento, scales = "free_y") + labs(x = "evento retirado", y = "efeito sem o evento (p.p. por 10 min)") +
		theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
	print(g)
	invisible(NULL)
}

## ---- desenho C: composição e efeitos por destino --------------------------------------------------------------------------
DEST <- data.table(oc = c("pedido", "ped_curto", "ped_naocurto", "ped_semloc", "ped_semnovo", "ped_salmaior", "ped_salnaomaior", "ped_novo3", "ped_novo4a12"),
	dimensao = c("Total", rep("Trajeto do novo emprego", 4), rep("Salário do novo emprego", 2), rep("Rapidez do novo emprego", 2)),
	rotulo = c("pediu demissão (total)", "novo emprego com trajeto mais curto", "novo emprego com trajeto não mais curto", "novo emprego sem local conhecido",
		"sem emprego formal novo em 12 meses", "novo emprego com salário ≥ 5% maior", "novo emprego com salário não maior", "novo emprego em até 3 meses",
		"novo emprego em 4 a 12 meses"))
TRJ_ROT <- c(ate90 = "até 90", acima90 = "acima de 90", todos = "todos")
composicao_destinos <- function(P, h = 12, trj = "ate90") {
	tt <- TERMOS_TRJ[[trj]]
	d <- unique(P[nivel == "h39" & fe == "casa7trab7" & modelo == tt$md & termo == tt$tr & janela == "pos" & outcome %in% paste0(DEST$oc, "_", h)], by = c("segmento", "outcome"))
	if (!nrow(d)) return(em_execucao())
	d[, oc := categoria(outcome)]
	tot <- d[oc == "pedido", .(segmento, tot = media_y)]
	d <- merge(d, tot, by = "segmento")
	d[, pct := 100 * media_y / tot]
	d <- merge(d, DEST, by = "oc")
	d[, v := fifelse(oc == "pedido", paste0(num(media_y, 1), "% dos vínculos"), paste0(num(pct, 0), " de cada 100"))]
	w <- dcast(d, dimensao + rotulo ~ segmento, value.var = "v", fill = "—")
	w <- w[order(match(dimensao, unique(DEST$dimensao)), match(rotulo, DEST$rotulo))]
	setcolorder(w, c("dimensao", "rotulo", intersect(SEGS, names(w))))
	setnames(w, c("dimensao", "rotulo"), c("Dimensão", "Destino"))
	tabela(w, legenda = "Primeira linha: % dos vínculos de referência que terminaram em pedido de demissão em até 12 meses. Demais linhas: de cada 100 pedidos, quantos foram para cada destino (cada dimensão soma 100).")
}
graf_destinos <- function(P, h = 12, segs = c("todos", "mulheres", "homens"), trj = "ate90") {
	tt <- TERMOS_TRJ[[trj]]
	d <- unique(P[nivel == "h39" & fe == "casa7trab7" & modelo == tt$md & termo %in% c(tt$tr, tt$ac) & janela == "pos" & segmento %in% segs &
		outcome %in% paste0(DEST$oc, "_", h)], by = c("segmento", "outcome", "termo"))
	if (!nrow(d)) return(invisible(NULL))
	segs <- intersect(segs, unique(d$segmento))
	d[, oc := categoria(outcome)]
	d <- merge(d, DEST, by = "oc")
	d[, `:=`(rot = factor(rotulo, rev(DEST$rotulo)), dim = factor(dimensao, unique(DEST$dimensao)), seg = factor(segmento, segs),
		tr = factor(fifelse(termo == tt$tr, "trajeto 10 min mais curto", "acessibilidade +10%"), c("trajeto 10 min mais curto", "acessibilidade +10%")))]
	g <- ggplot(d, aes(efeito, rot, color = seg)) + geom_vline(xintercept = 0, color = "grey55", linewidth = 0.3) +
		geom_errorbarh(aes(xmin = efeito - 1.96 * ep, xmax = efeito + 1.96 * ep), height = 0.3, position = position_dodge(0.6), linewidth = 0.45) +
		geom_point(position = position_dodge(0.6), size = 1.8) + facet_grid(dim ~ tr, scales = "free", space = "free_y") +
		scale_color_manual(values = c(todos = "#1F5A96", mulheres = "#B0377B", homens = "#2E8B57"), name = NULL) +
		labs(x = "efeito sobre a chance de pedir demissão e ir para o destino (p.p.)", y = NULL) +
		theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), strip.text.y = element_text(angle = 0))
	print(g)
	invisible(NULL)
}
graf_ganho_destino <- function(D, hz = 12, segs = c("todos", "mulheres", "homens"), trj = "ate90") {
	d <- D[nivel == "h39" & janela == "pos" & h == hz & trajeto == TRJ_ROT[[trj]] & segmento %in% segs]
	segs <- intersect(segs, unique(d$segmento))
	if (!nrow(d)) return(invisible(NULL))
	d <- d[, .(traj = weighted.mean(traj_recentrado_min, vinculos), acc = 100 * weighted.mean(acesso_recentrado_log, vinculos)), by = .(segmento, grupo)]
	d <- merge(d, rbind(DEST[, .(grupo = oc, rotulo)], data.table(grupo = "nao_pediu", rotulo = "não pediu demissão")), by = "grupo")
	d <- melt(d, id.vars = c("segmento", "grupo", "rotulo"), measure.vars = c("traj", "acc"))
	d[, `:=`(var = factor(fifelse(variable == "traj", "ganho inesperado de trajeto (min)", "ganho inesperado de acessibilidade (%)"),
		c("ganho inesperado de trajeto (min)", "ganho inesperado de acessibilidade (%)")), rot = factor(rotulo, rev(c("não pediu demissão", DEST$rotulo))), seg = factor(segmento, segs))]
	g <- ggplot(d, aes(value, rot, fill = seg)) + geom_col(position = position_dodge(0.75), width = 0.7) + geom_vline(xintercept = 0, color = "grey40", linewidth = 0.3) +
		facet_wrap(~var, scales = "free_x") + scale_fill_manual(values = c(todos = "#1F5A96", mulheres = "#B0377B", homens = "#2E8B57"), name = NULL) +
		labs(x = "média do ganho realizado menos o esperado", y = NULL) + theme_minimal(base_size = 10) +
		theme(legend.position = "bottom", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
	print(g)
	invisible(NULL)
}

## ---- desenho A: tabelas por categoria, heterogeneidade e robustez ----------------------------------------------------------
ROT_IND <- function(ind, medida) paste0(ind, fifelse(medida == "absoluto", " · ganho absoluto", " · ganho percentual"))
ORD_IND <- paste0(rep(c("30 min", "45 min", "60 min", "90 min", "gravitacional"), each = 2), c(" · ganho absoluto", " · ganho percentual"))
area_principal <- function(A) unique(A[nivel == "h39" & fe == "casa7" & segmento == "todos"], by = c("ind", "medida", "outcome", "janela"))
tabelas_area_principal <- function(A) {
	d <- area_principal(A)
	if (!nrow(d)) return(em_execucao())
	d[, `:=`(c = cel(efeito, ep, p_perm, 3), linha = ROT_IND(as.character(ind), medida))]
	tabs_categoria(d, CAT_AREA, "linha", "Indicador · medida do ganho", ordem = ORD_IND,
		legenda = "Efeito por unidade da dose. Ganho absoluto: por +100 mil empregos alcançáveis (na gravitacional, +100 mil empregos ponderados). Ganho percentual: por +10% da acessibilidade de antes (na gravitacional, +0,1 no log). Salário em %; os demais em p.p.")
}
prepara_ct_area <- function(ACT) {
	if (is.null(ACT)) return(NULL)
	x <- copy(ACT[nivel == "h39" & !grepl("_fe_", arquivo)])
	x[, p := p_normal]
	unique(x, by = c("ind", "medida", "contraste", "outcome", "janela"))
}
het_area <- function(A, CT) {
	d0 <- A[nivel == "h39" & fe == "casa7"]
	cat("\n::: {.panel-tabset}\n")
	for (i in levels(A$ind)) for (m in c("absoluto", "percentual")) {
		d <- unique(d0[ind == i & medida == m], by = c("segmento", "outcome", "janela"))
		if (!nrow(d) || length(unique(d$segmento)) < 2) next
		cat("\n## ", i, ", ", m, "\n\n", sep = "")
		d[, c := cel(efeito, ep, p_perm, 3)]
		ct <- if (!is.null(CT)) CT[ind == i & medida == m] else NULL
		if (!is.null(ct) && nrow(ct)) ct[, c := cel_ct(diferenca, ep, p, 3)]
		texto_contrastes(ct, por = por_area(i, m), d_dig = 3, escopo = "de grupos, desfechos e horizontes")
		tabs_categoria(d, CAT_AREA, "segmento", "Grupo", nivel = 3, ct = ct)
	}
	cat("\n:::\n")
}
ROB_AREA_TXT <- list(
	"R1 outras aberturas" = list(titulo = "Controle de outras aberturas no mesmo período",
		txt = "Enquanto acompanhamos um evento, outras estações abrem e também mudam a acessibilidade. Este teste acrescenta à regressão o ganho de acessibilidade de todas as outras aberturas dos 12 meses anteriores e dos 24 meses de acompanhamento (também recentrado pelos sorteios). <b>Objetivo:</b> verificar que o efeito atribuído ao evento não é, na verdade, efeito de outras aberturas próximas no tempo. <b>Como ler:</b> se a linha \"controlando outras aberturas\" for parecida com a \"principal\", essas outras aberturas não contaminam o resultado."),
	"R2 efeito fixo H3-8" = list(titulo = "Vizinhança menor",
		txt = "Troca as células de bairro de cerca de 5 km² (H3-7) por células de cerca de 0,7 km² (H3-8): só comparamos pessoas que moram ainda mais perto umas das outras. <b>Objetivo:</b> afastar a possibilidade de que diferenças entre partes de um mesmo bairro grande expliquem o resultado. <b>Como ler:</b> se o efeito muda pouco, a comparação por bairro maior já era suficiente."),
	"R6 faixas de ganho em % (abaixo de 1% = zero)" = list(titulo = "Faixas de ganho em vez de efeito linear",
		txt = "A especificação principal supõe que cada unidade de ganho tem o mesmo efeito. Aqui o ganho percentual é dividido em faixas (1–5%, 5–20% e 20% ou mais; ganho abaixo de 1% conta como zero), e cada coeficiente é o efeito de estar naquela faixa contra quem ganhou menos de 1%. <b>Objetivo:</b> ver se algum efeito aparece só em ganhos grandes, o que a reta da especificação principal poderia esconder. <b>Como ler:</b> se o efeito fosse real, esperaríamos que ele crescesse da faixa menor para a maior."))
## ---- desenho B: diferenças entre grupos e leitura da robustez --------------------------------------------------------------
prepara_ct_par <- function(PC, md = "A", tm = "trajeto, até 90") {
	if (is.null(PC)) return(NULL)
	x <- copy(PC[modelo == md & termo == tm])
	if ("fe" %in% names(x)) x <- x[is.na(fe) | fe %in% c("", "casa7trab7")]
	if ("nivel" %in% names(x)) x <- x[nivel == "h39"]
	x[, p := p_normal]
	x <- unique(x, by = c("contraste", "outcome", "janela"))
	x[, c := cel_ct(diferenca, ep, p)]
	x
}
## ---- desenho D: IPTW --------------------------------------------------------------------------------------------------------
ROT_FAIXA_P <- c(todas = "todas as faixas", f1a5 = "ganho de 1 a 5 min", f5a10 = "ganho de 5 a 10 min", f10mais = "ganho de 10 min ou mais")
ROT_FAIXA_A <- c(todas = "todas as faixas", f1a5 = "ganho de 1 a 5%", f5a20 = "ganho de 5 a 20%", f20mais = "ganho de 20% ou mais")
rot_corte <- function(x) fifelse(as.character(x) %in% c("grav", "gravitacional"), "gravitacional", paste0(sub("^c", "", as.character(x)), " min"))
ORD_CORTE <- c("30 min", "45 min", "60 min", "90 min", "gravitacional")
tabs_por_faixa <- function(d, ct, faixas, rot, cats, bcol, secol, pcol, d_dig = 2, legenda = NULL, nivel = 2) {
	cat("\n::: {.panel-tabset}\n")
	for (fx in faixas) {
		x <- unique(d[faixa == fx], by = c("segmento", "outcome", "janela"))
		if (!nrow(x)) next
		cat("\n", strrep("#", nivel), " ", rot[[fx]], "\n\n", sep = "")
		x[, c := cel(get(bcol), get(secol), get(pcol), d_dig)]
		y <- if (!is.null(ct) && nrow(ct)) unique(ct[faixa == fx], by = c("contraste", "outcome", "janela")) else NULL
		if (!is.null(y) && nrow(y)) y[, c := cel_ct(diferenca, ep, p, d_dig)]
		tabs_categoria(x, cats, "segmento", "Grupo", nivel = nivel + 1, ct = y, legenda = legenda)
	}
	cat("\n:::\n")
}
## abas por recorte de trajeto (até 90, acima de 90, todos juntos); f(trj, nivel) imprime o conteúdo de cada aba
abas_trajeto <- function(f, disponivel = function(trj) TRUE, nivel = 2) {
	rot <- c(ate90 = "Trajetos de até 90 minutos", acima90 = "Trajetos de mais de 90 minutos", todos = "Todos os trajetos juntos")
	cat("\n::: {.panel-tabset}\n")
	for (trj in names(rot)) {
		cat("\n", strrep("#", nivel), " ", rot[[trj]], "\n\n", sep = "")
		if (disponivel(trj)) f(trj, nivel + 1) else em_execucao(paste0(tolower(rot[[trj]]), ": estimação em execução"))
	}
	cat("\n:::\n")
}
## uma aba por indicador de acessibilidade; dentro dela, uma aba por desfecho, com uma tabela por faixa de ganho (linhas = grupos)
tabs_por_corte <- function(d, ct, faixas, rot, cats, bcol, secol, pcol, d_dig = 3, legenda = NULL) {
	d <- copy(d)
	d[, ic := rot_corte(corte)]
	if (!is.null(ct) && nrow(ct)) {
		ct <- copy(ct)
		ct[, ic := rot_corte(corte)]
	}
	cat("\n::: {.panel-tabset}\n")
	for (i in intersect(ORD_CORTE, unique(d$ic))) {
		x <- unique(d[ic == i], by = c("faixa", "segmento", "outcome", "janela"))
		cat("\n## ", i, "\n\n", sep = "")
		x[, c := cel(get(bcol), get(secol), get(pcol), d_dig)]
		y <- if (!is.null(ct) && nrow(ct)) unique(ct[ic == i], by = c("faixa", "contraste", "outcome", "janela")) else NULL
		if (!is.null(y) && nrow(y)) y[, c := cel_ct(diferenca, ep, p, d_dig)]
		tabs_categoria(x, cats, "segmento", "Grupo", nivel = 3, ct = y, legenda = legenda, bloco = "faixa", ord_bloco = faixas, rot_bloco = function(b) unname(rot[b]))
	}
	cat("\n:::\n")
}
## tabela principal (todas as pessoas): uma aba por desfecho; linhas = indicador · faixa
tab_corte_faixa <- function(d, faixas, rot, cats, bcol, secol, pcol, d_dig = 3, legenda = NULL) {
	d <- unique(copy(d), by = c("corte", "faixa", "outcome", "janela"))
	if (!nrow(d)) return(em_execucao())
	d[, `:=`(c = cel(get(bcol), get(secol), get(pcol), d_dig), linha = paste0(rot_corte(corte), " · ", unname(rot[faixa])))]
	ordl <- as.vector(t(outer(ORD_CORTE, unname(rot[faixas]), paste, sep = " · ")))
	tabs_categoria(d[faixa %in% faixas], cats, "linha", "Indicador · faixa de ganho", ordem = ordl, legenda = legenda)
}
balanco_tabela <- function(B, rot = ROT_FAIXA_P) {
	if (is.null(B) || !nrow(B)) return(invisible(NULL))
	B <- copy(B)
	if (!"corte" %in% names(B)) B[, corte := NA]
	if (!"trajeto" %in% names(B)) B[, trajeto := NA_character_]
	B <- B[segmento == "todos"]
	tabela(B[order(janela != "pos", trajeto, corte, match(faixa, names(rot))), .(Janela = fifelse(janela == "pos", "depois", "placebo"),
		`Indicador ou trajeto` = fifelse(is.na(corte), fifelse(trajeto %in% "todos", "todos os trajetos", paste0("trajeto ", trajeto, " min")), rot_corte(corte)), Faixa = unname(rot[faixa]),
		Tratados = num(tratados, 0), Controles = num(controles, 0), `Controles efetivos (depois dos pesos)` = num(ess_controles, 0),
		`Maior diferença padronizada, antes dos pesos` = num(smd_max_antes, 3), `Maior diferença padronizada, depois dos pesos` = num(smd_max_depois, 3))],
		legenda = "Controles efetivos: quantos controles com peso igual dariam a mesma precisão; quando é muito menor que o número de controles, poucos controles carregam muito peso. Diferença padronizada: ver o quadro acima; abaixo de 0,1 é considerada boa.")
}

## ---- desenho E: controles futuros na mesma faixa -----------------------------------------------------------------------------
amostra_futuros <- function(AM) {
	if (is.null(AM)) return(invisible(NULL))
	a <- AM[grepl("_area_h39_", arquivo) & janela == "pos"]
	if (nrow(a)) {
		s <- a[, .(pessoas = sum(pessoas), ganho = weighted.mean(ganho_medio, pessoas), eventos = uniqueN(ev)), by = .(corte, faixa, T)]
		w <- dcast(s, corte + faixa ~ T, value.var = c("pessoas", "ganho", "eventos"))
		w <- w[order(match(rot_corte(corte), ORD_CORTE), match(faixa, names(ROT_FAIXA_A)))]
		tabela(w[, .(Indicador = rot_corte(corte), Faixa = unname(ROT_FAIXA_A[faixa]), `Pessoas tratadas` = num(pessoas_1, 0), `Pessoas controle (ganham depois)` = num(pessoas_0, 0),
			`Ganho médio dos tratados (%)` = num(ganho_1, 1), `Ganho futuro médio dos controles (%)` = num(ganho_0, 1), `Eventos com tratados` = eventos_1, `Eventos com controles` = eventos_0)],
			titulo = "Área: pessoas × evento em cada grupo (janela depois da abertura)")
	}
	p <- AM[grepl("_par_h39_", arquivo) & janela == "pos" & g90 %in% TRUE]
	if (nrow(p)) {
		s <- p[, .(vinculos = sum(vinculos), ganho = weighted.mean(ganho_medio, vinculos), eventos = uniqueN(ev)), by = .(faixa, T)]
		w <- dcast(s, faixa ~ T, value.var = c("vinculos", "ganho", "eventos"))
		w <- w[order(match(faixa, names(ROT_FAIXA_P)))]
		tabela(w[, .(Faixa = unname(ROT_FAIXA_P[faixa]), `Vínculos tratados` = num(vinculos_1, 0), `Vínculos controle (ganham depois)` = num(vinculos_0, 0),
			`Ganho médio dos tratados (min)` = num(ganho_1, 1), `Ganho futuro médio dos controles (min)` = num(ganho_0, 1), `Eventos com tratados` = eventos_1, `Eventos com controles` = eventos_0)],
			titulo = "Tempo de viagem: vínculos × evento em cada grupo (trajetos de até 90 min, janela depois da abertura)")
	}
}

## ---- leitura guiada em texto corrido (24/11/2026) ---------------------------------------------------------------------------
## Os quadros verdes descrevem os números em frases: o que muda, em quanto, com que segurança, o que diz o placebo e o que
## isso quer dizer. Todos os textos são gerados a partir das estimativas e se atualizam com elas.
fp <- function(p) ifelse(is.na(p), "p não disponível", ifelse(p < 0.001, "p &lt; 0,001", paste0("p = ", num(p, 3))))
un_oc <- function(o) ifelse(categoria(o) == "salario", "%", "p.p.")
maiuscula <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
minuscula1 <- function(x) paste0(tolower(substr(x, 1, 1)), substring(x, 2))
rot_grupo <- function(g) ifelse(g == "todos", "Todas as pessoas", maiuscula(g))
## nome do grupo dentro de uma frase ("para quem tem criança pequena")
GRUPO_TXT <- c("com criança" = "quem tem criança pequena", "sem criança" = "quem não tem criança pequena", "18 a 29" = "quem tem 18 a 29 anos",
	"30 a 44" = "quem tem 30 a 44 anos", "45 a 60" = "quem tem 45 a 60 anos", "mulheres com criança" = "mulheres com criança pequena",
	"mulheres sem criança" = "mulheres sem criança pequena", "homens com criança" = "homens com criança pequena", "homens sem criança" = "homens sem criança pequena",
	"vínculo até 12 meses" = "quem tinha até 12 meses de vínculo", "vínculo mais de 12 meses" = "quem tinha mais de 12 meses de vínculo")
grupo_txt <- function(g) ifelse(g %in% names(GRUPO_TXT), GRUPO_TXT[g], g)
DESF <- c(emprego = "a chance de estar empregado (em relação ao mês de referência)", entrada = "a chance de entrar no emprego formal",
	empregado = "a chance de continuar empregado", meses = "a fração dos meses em que a pessoa esteve empregada", salario = "o salário-hora",
	qualquer = "a chance de o vínculo terminar por qualquer motivo", demissao = "a chance de ser demitido", sem_justa = "a chance de ser demitido sem justa causa",
	justa = "a chance de ser demitido por justa causa", pedido = "a chance de pedir demissão",
	ped_curto = "a chance de pedir demissão e ir para um emprego com trajeto mais curto",
	ped_naocurto = "a chance de pedir demissão e ir para um emprego com trajeto que não é mais curto",
	ped_semloc = "a chance de pedir demissão e ir para um emprego sem local conhecido",
	ped_semnovo = "a chance de pedir demissão e não ter emprego formal novo depois",
	ped_salmaior = "a chance de pedir demissão e ir para um emprego com salário pelo menos 5% maior",
	ped_salnaomaior = "a chance de pedir demissão e ir para um emprego com salário que não é maior",
	ped_novo3 = "a chance de pedir demissão e ter emprego novo em até 3 meses",
	ped_novo4a12 = "a chance de pedir demissão e só ter emprego novo entre 4 e 12 meses depois")
## horizonte: "em até 6 meses" para saídas, entradas e destinos; "6 meses depois" para estados (empregado, emprego, salário)
desf_txt <- function(o) vapply(o, function(x) {
	k <- categoria(x)
	paste0(if (k %in% names(DESF)) DESF[[k]] else k, if (k %in% c("emprego", "empregado", "salario")) paste0(" ", horizonte(x), " meses depois") else paste0(" em até ", horizonte(x), " meses"))
}, character(1), USE.NAMES = FALSE)
## unidades e significância por extenso
un_ext <- function(o, b) ifelse(categoria(o) == "salario", "%", ifelse(abs(b) < 2, "ponto percentual", "pontos percentuais"))
valor_ext <- function(o, b, d = 2) ifelse(categoria(o) == "salario", paste0(num(abs(b), d), "%"), paste(num(abs(b), d), un_ext(o, b)))
sig_ext <- function(p) ifelse(is.na(p), "sem significância calculada", ifelse(p < 0.01, "estatisticamente muito significativo", ifelse(p < 0.05, "estatisticamente significativo",
	ifelse(p < 0.10, "significativo só ao nível de 10%, uma evidência fraca", "estatisticamente indistinguível de zero"))))
sig_curto <- function(p) ifelse(is.na(p), "sem p", ifelse(p < 0.05, "significativo", ifelse(p < 0.10, "significativo só a 10%", "não significativo")))
sig_ext_f <- function(p) sub("significativo", "significativa", sub("indistinguível", "indistinguível", sig_ext(p)))
## quadro verde em HTML bruto (o markdown não mexe nas estrelas)
limpa_txt <- function(x) gsub("p\\.p\\.\\.", "p.p.", x)
html_leitura <- function(titulo, corpo) cat('\n\n```{=html}\n<div class="leitura"><p><b>', titulo, "</b></p>\n", limpa_txt(paste(corpo, collapse = "\n")), "\n</div>\n```\n\n", sep = "")
## "reduz a chance de demissão em 6 meses em 0,95 p.p. (p < 0,001; cerca de 8% da média, de 12,3%)"; com p >= 0,10,
## "não altera de forma distinguível de zero ..."
frase_ef <- function(o, b, p, media = NA, d = 2) {
	rel <- ifelse(!is.na(media) & media > 1 & !categoria(o) %in% c("salario", "emprego"),
		sprintf(", o que equivale a cerca de %s%% da taxa média, que é de %s%%", num(100 * abs(b) / media, 0), num(media, 1)), "")
	ifelse(is.na(p) | p >= 0.10,
		sprintf("não altera de forma estatisticamente distinguível de zero %s (a estimativa é de %s %s, com %s)", desf_txt(o), sinal(b, d), un_ext(o, b), fp(p)),
		sprintf("%s %s em %s%s; o efeito é %s (%s)", ifelse(b < 0, "reduz", "aumenta"), desf_txt(o), valor_ext(o, b, d), rel, sig_ext(p), fp(p)))
}
lista_e <- function(x) if (length(x) <= 1) x else paste0(paste(head(x, -1), collapse = "; "), " e ", tail(x, 1))
## "em" + artigo: "na chance de...", "no salário-hora..."
em_ <- function(x) sub("^o ", "no ", sub("^a ", "na ", x))
CONECTIVOS <- c("Também ", "Além disso, ", "Da mesma forma, ", "E ainda ")
lista_v <- function(x) if (length(x) <= 1) x else paste0(paste(head(x, -1), collapse = ", "), " e ", tail(x, 1))
## frase sobre o placebo de um conjunto de desfechos
frase_placebo <- function(x, d = 2) {
	falta <- x[is.na(b0)]
	pl <- x[!is.na(p0) & p0 < 0.10]
	tem_ef <- nrow(x[!is.na(p) & p < 0.10]) > 0
	f <- character(0)
	if (!nrow(pl) && nrow(falta) < nrow(x)) f <- if (tem_ef) "No placebo — a mesma comparação feita antes de a estação existir —, nenhum desses desfechos mostra diferença significativa, o que reforça que os efeitos acima vêm da abertura." else
		"No placebo — a mesma comparação feita antes de a estação existir —, nenhum desses desfechos mostra diferença significativa, como esperado."
	if (nrow(pl)) f <- paste0("No placebo, porém, também aparece diferença ", lista_e(paste0(em_(desf_txt(pl$outcome)), " (", sinal(pl$b0, d), " ", un_ext(pl$outcome, pl$b0), ", ", fp(pl$p0), ")")),
		": nesses casos, a diferença entre os grupos já existia antes da abertura, e o resultado pede cautela.")
	if (nrow(falta)) f <- c(f, sprintf("O placebo de %d %s ainda não foi estimado.", nrow(falta), if (nrow(falta) == 1) "desfecho" else "desfechos"))
	paste(f, collapse = " ")
}
## parágrafo por grupo: o que muda (p < 0,10), o que não muda e o que diz o placebo.
## y: colunas <grupo>, outcome, b, se, p, b0, p0 e, se houver, media (média do desfecho sem o tratamento, em %)
texto_resultados <- function(y, por, intro = NULL, d = 2, grupo = "segmento", titulo = "O que estes resultados mostram", rot = rot_grupo) {
	if (is.null(y) || !nrow(y)) return(invisible(NULL))
	y <- copy(y)
	if (!"media" %in% names(y)) y[, media := NA_real_]
	corpo <- if (!is.null(intro)) paste0("<p>", intro, "</p>") else character(0)
	for (g in unique(y[[grupo]])) {
		x <- y[y[[grupo]] == g]
		s <- x[!is.na(p) & p < 0.10]
		n <- x[is.na(p) | p >= 0.10]
		f <- character(0)
		if (nrow(s)) for (i in seq_len(nrow(s))) f <- c(f, paste0(if (i == 1) paste0(por, " ") else CONECTIVOS[(i - 2) %% length(CONECTIVOS) + 1], frase_ef(s$outcome[i], s$b[i], s$p[i], s$media[i], d), "."))
		if (nrow(n)) f <- c(f, paste0(if (nrow(s)) "Já " else paste0(por, " não altera de forma estatisticamente distinguível de zero "),
			lista_v(paste0(desf_txt(n$outcome), " (estimativa de ", sinal(n$b, d), " ", un_ext(n$outcome, n$b), ")")),
			if (nrow(s)) if (nrow(n) == 1) " não muda de forma estatisticamente distinguível de zero." else " não mudam de forma estatisticamente distinguível de zero." else "."))
		f <- c(f, frase_placebo(x, d))
		corpo <- c(corpo, sprintf("<p><b>%s.</b> %s</p>", rot(g), paste(f, collapse = " ")))
	}
	html_leitura(titulo, corpo)
}
## várias versões (indicadores, medidas, faixas) do mesmo desfecho: quantas são significativas, quais, e o que diz o placebo de
## cada uma. d: colunas outcome, janela, <rot>, <bcol>, <pcol>
texto_contagem <- function(d, cats, hs = c(12, 24), bcol = "efeito", pcol = "p", rot = "versao", versoes = "versões", intro = NULL, d_dig = 3,
	titulo = "O que estes resultados mostram") {
	if (is.null(d) || !nrow(d)) return(invisible(NULL))
	corpo <- character(0)
	for (k in names(cats)) {
		fr <- character(0)
		limpos <- list()
		un <- if (k == "salario") "%" else "p.p."
		for (hz in hs) {
			x <- d[categoria(outcome) == k & horizonte(outcome) == hz]
			pos <- unique(x[janela == "pos" & !is.na(get(bcol))], by = rot)
			if (!nrow(pos)) next
			z <- pos[, c(rot, bcol, pcol), with = FALSE]
			setnames(z, c("v", "b", "p"))
			pre <- unique(x[janela == "pre" & !is.na(get(bcol))], by = rot)[, c(rot, bcol, pcol), with = FALSE]
			setnames(pre, c("v", "b0", "p0"))
			z <- merge(z, pre, by = "v", all.x = TRUE)
			sg <- z[!is.na(p) & p < 0.05]
			masc <- grepl("^indicador", versoes)
			unx <- if (k == "salario") "%" else if (max(abs(z$b)) < 2) "ponto percentual" else "pontos percentuais"
			if (!nrow(sg)) {
				fr <- c(fr, sprintf("No horizonte de %d meses, %s %d %s tem efeito estatisticamente significativo: as estimativas ficam entre %s e %s %s.", hz,
					if (masc) "nenhum dos" else "nenhuma das", nrow(z), versoes, sinal(min(z$b), d_dig), sinal(max(z$b), d_dig), unx))
				next
			}
			sg[, limpo := is.na(p0) | p0 >= 0.10]
			det <- paste0(sg$v, ", com efeito de ", sinal(sg$b, d_dig), if (k == "salario") "%" else "", " (", fp(sg$p), ") e placebo de ",
				ifelse(is.na(sg$b0), "ainda não estimado", paste0(sinal(sg$b0, d_dig), " (", fp(sg$p0), ")")))
			fr <- c(fr, sprintf("No horizonte de %d meses, %d %s %d %s %s efeito estatisticamente significativo (a 5%%): %s.", hz, nrow(sg), if (masc) "dos" else "das", nrow(z), versoes,
				if (nrow(sg) == 1) "tem" else "têm", lista_e(det)))
			nl <- sum(!sg$limpo)
			if (nl == nrow(sg)) fr <- c(fr, if (nl == 1) "Mas o placebo dessa versão também é significativo: a diferença já existia antes da abertura e, por isso, não deve ser lida como efeito da estação." else
				"Mas em todas elas o placebo também é significativo: as diferenças já existiam antes da abertura e, por isso, não devem ser lidas como efeito da estação.")
			else if (nl > 0) fr <- c(fr, sprintf("Em %d delas, porém, o placebo também é significativo, o que enfraquece a leitura causal dessas versões.", nl))
			limpos[[length(limpos) + 1L]] <- sg[limpo == TRUE]
		}
		if (!length(fr)) next
		L <- rbindlist(limpos)
		resumo <- if (!nrow(L)) sprintf("Em resumo, não há sinal confiável de efeito sobre %s.", tolower(cats[[k]])) else
			sprintf("Em resumo, há algum sinal de efeito %s sobre %s em %d %s com placebo nulo; o maior deles, em módulo, é de %s %s.",
				if (all(L$b > 0)) "positivo" else if (all(L$b < 0)) "negativo" else "de sinais opostos", tolower(cats[[k]]), nrow(L), if (nrow(L) == 1) "versão" else "versões",
				num(max(abs(L$b)), d_dig), if (k == "salario") "%" else if (max(abs(L$b)) < 2) "ponto percentual" else "pontos percentuais")
		corpo <- c(corpo, sprintf("<p><b>%s.</b> %s %s</p>", cats[[k]], paste(fr, collapse = " "), resumo))
	}
	if (!length(corpo)) return(invisible(NULL))
	html_leitura(titulo, c(if (!is.null(intro)) paste0("<p>", intro, "</p>"), corpo))
}
## frase de uma diferença entre grupos: "o efeito de ... sobre ... é 0,25 p.p. mais negativo para mulheres do que para homens"
frase_ct <- function(contraste, o, dif, p, b0, p0, por, d = 2) {
	dir <- ifelse(dif < 0, "mais negativo", "mais positivo")
	quem <- vapply(contraste, function(cc) {
		if (grepl("^\\(mulheres com", cc)) return("NA")
		g <- strsplit(cc, " − ", fixed = TRUE)[[1]]
		paste0("para ", grupo_txt(g[1]), " do que para ", grupo_txt(g[2]))
	}, character(1), USE.NAMES = FALSE)
	base <- ifelse(quem == "NA",
		sprintf("a diferença entre ter e não ter criança pequena no efeito %s sobre %s é %s %s entre as mulheres do que entre os homens", por, desf_txt(o), valor_ext(o, dif, d), dir),
		sprintf("o efeito %s sobre %s é %s %s %s", por, desf_txt(o), valor_ext(o, dif, d), dir, quem))
	pl <- ifelse(is.na(b0), " O placebo dessa diferença ainda não foi estimado.",
		ifelse(p0 >= 0.10, sprintf(" No placebo, a mesma diferença é nula (%s, %s), o que reforça que ela surge com a abertura.", sinal(b0, d), fp(p0)),
			sprintf(" Mas o placebo também mostra diferença (%s, %s): os grupos já se comportavam de forma diferente antes da abertura, e o resultado pede cautela.", sinal(b0, d), fp(p0))))
	paste0(maiuscula(base), "; a diferença é ", sig_ext_f(p), " (", fp(p), ").", pl)
}
## diferenças entre grupos: quantas dão significativas, contra quantas o acaso daria, e as mais fortes em frases.
## ct: colunas contraste, outcome, janela, diferenca, p; opcionais versao (rótulo do indicador) e por (texto da dose)
texto_contrastes <- function(ct, por = "", max_itens = 6, d_dig = 2, titulo = "O que as diferenças entre grupos mostram", escopo = "de grupos, desfechos e horizontes") {
	if (is.null(ct) || !nrow(ct)) return(invisible(NULL))
	ch <- c("contraste", "outcome", intersect("versao", names(ct)))
	x <- unique(ct[janela == "pos" & !is.na(diferenca)], by = ch)
	if (!nrow(x)) return(invisible(NULL))
	pre <- unique(ct[janela == "pre" & !is.na(diferenca)], by = ch)[, c(ch, "diferenca", "p"), with = FALSE]
	setnames(pre, c("diferenca", "p"), c("b0", "p0"))
	x <- merge(x, pre, by = ch, all.x = TRUE)
	set(x, j = "por_txt", value = if ("por" %in% names(x)) x$por else rep(por, nrow(x)))
	sig <- x[!is.na(p) & p < 0.05]
	sig <- sig[order(!(is.na(p0) | p0 >= 0.10), p)]
	esp <- 0.05 * nrow(x)
	f1 <- sprintf("Comparamos os grupos em %d combinações %s depois da abertura. Se os grupos não diferissem em nada, cerca de %s dessas comparações dariam significativas a 5%% só por acaso; deram %d.",
		nrow(x), escopo, num(esp, 0), nrow(sig))
	f2 <- if (!nrow(sig)) " Nenhuma diferença entre grupos é significativa: o efeito é o mesmo, dentro da precisão das estimativas, em todos os grupos." else
		if (nrow(sig) <= 1.5 * esp) " Esse número está perto do que o acaso produziria, então nenhuma das diferenças abaixo deve ser lida isoladamente como prova de que o efeito muda entre os grupos." else
		" Há mais diferenças do que o acaso explicaria. As mais fortes, começando pelas que não aparecem no placebo, são:"
	corpo <- paste0("<p>", f1, f2, "</p>")
	if (nrow(sig)) {
		k <- seq_len(min(nrow(sig), max_itens))
		it <- frase_ct(sig$contraste[k], sig$outcome[k], sig$diferenca[k], sig$p[k], sig$b0[k], sig$p0[k], sig$por_txt[k], d_dig)
		if ("versao" %in% names(sig)) it <- paste0("<b>", sig$versao[k], ":</b> ", it)
		corpo <- c(corpo, paste0("<ul>", paste0("<li>", it, "</li>", collapse = ""), "</ul>"))
		if (nrow(sig) > max_itens) corpo <- c(corpo, sprintf("<p>Há mais %d diferenças significativas nas tabelas.</p>", nrow(sig) - max_itens))
	}
	html_leitura(titulo, corpo)
}
## efeito em cada grupo: em quais grupos é significativo e em quais não, para cada desfecho escolhido
texto_segmentos <- function(d, ocs, bcol = "efeito", secol = "ep", pcol = "p", intro = NULL, d_dig = 2) {
	if (is.null(d) || !nrow(d)) return(invisible(NULL))
	x <- pareia(unique(d[outcome %in% ocs], by = c("segmento", "outcome", "janela")), c("segmento", "outcome"), bcol, secol, pcol)
	if (!nrow(x)) return(invisible(NULL))
	corpo <- if (!is.null(intro)) paste0("<p>", intro, "</p>") else character(0)
	for (o in ocs) {
		y <- x[outcome == o][order(match(segmento, SEGS))]
		if (!nrow(y)) next
		s <- y[!is.na(p) & p < 0.05]
		n <- y[is.na(p) | p >= 0.05]
		un <- if (categoria(o) == "salario") "%" else "pontos percentuais"
		f <- if (!nrow(s)) sprintf("Em nenhum dos %d grupos o efeito é estatisticamente significativo; as estimativas ficam entre %s e %s %s.", nrow(y), sinal(min(y$b), d_dig), sinal(max(y$b), d_dig), un) else
			paste0(sprintf("O efeito é estatisticamente significativo em %d dos %d grupos: %s (em %s).", nrow(s), nrow(y),
				lista_v(paste0(grupo_txt(s$segmento), ", ", sinal(s$b, d_dig))), un),
				if (nrow(n)) paste0(" Nos outros ", nrow(n), " — ", lista_v(paste0(grupo_txt(n$segmento), ", ", sinal(n$b, d_dig))), " —, ",
					if (nrow(s) && all(sign(n$b) == sign(s$b[1]))) "as estimativas têm o mesmo sinal, mas não se distinguem de zero." else "as estimativas não se distinguem de zero.") else "")
		pl <- y[!is.na(p0) & p0 < 0.10]
		if (nrow(pl)) f <- paste0(f, " Em ", lista_v(grupo_txt(pl$segmento)), ", o placebo também é significativo, o que pede cautela nesses grupos.")
		corpo <- c(corpo, sprintf("<p><b>%s.</b> %s</p>", maiuscula(desf_txt(o)), f))
	}
	corpo <- c(corpo, "<p>Um efeito significativo num grupo e não significativo em outro não basta para dizer que os grupos diferem: isso é testado diretamente pelas diferenças entre grupos.</p>")
	html_leitura("O que os grupos mostram", corpo)
}
## uma especificação principal contra suas variações (robustez): estável, sensível ou com sinal trocado.
## z: colunas versao, b, p; ref = rótulo da principal
frase_variantes <- function(z, rotulo, ref = "principal", d = 3, un = "p.p.") {
	r <- z[versao == ref]
	o <- z[versao != ref]
	if (!nrow(r) || !nrow(o)) return(NULL)
	b0 <- r$b[1]
	s0 <- !is.na(r$p[1]) && r$p[1] < 0.05
	so <- o[!is.na(p) & p < 0.05]
	mesmo <- o[sign(b) == sign(b0)]
	aspas <- function(v) paste0("“", minuscula1(v), "”")
	juizo <- if (!s0 && !nrow(so)) "o resultado nulo se mantém em todas as variações" else
		if (!s0) paste0("a principal não é significativa, mas ", if (nrow(so) == 1) "a versão " else "as versões ", lista_e(aspas(so$versao)),
			if (nrow(so) == 1) " indica" else " indicam", " efeito significativo") else
		if (nrow(so) == nrow(o) && nrow(mesmo) == nrow(o)) "o efeito se mantém, com o mesmo sinal e significativo, em todas as variações" else
		if (nrow(mesmo) == nrow(o)) paste0("o sinal se mantém em todas as variações, mas o efeito deixa de ser significativo na versão ", lista_e(aspas(o[is.na(p) | p >= 0.05, versao]))) else
		paste0("o sinal muda na versão ", lista_e(aspas(o[sign(b) != sign(b0), versao])), ", então o resultado depende dessa escolha")
	une <- if (un == "p.p.") (if (abs(b0) < 2) "ponto percentual" else "pontos percentuais") else un
	sprintf("<p><b>%s.</b> Na especificação principal, o efeito é de %s %s, %s (%s). %s. Ou seja, %s.</p>", rotulo, sinal(b0, d), une, sig_ext(r$p[1]), fp(r$p[1]),
		maiuscula(lista_e(paste0("com a versão ", aspas(o$versao), ", a estimativa passa a ", sinal(o$b, d), " (", sig_curto(o$p), ")"))), juizo)
}
## efeito por faixa de ganho: cresce com o ganho?
frase_faixas <- function(z, rotulo, d = 3, un = "p.p.") {
	if (!nrow(z)) return(NULL)
	s <- sign(z$b[nrow(z)])
	mon <- nrow(z) > 1 && all(diff(s * z$b) >= 0)
	juizo <- if (!any(!is.na(z$p) & z$p < 0.05)) "Nenhuma faixa tem efeito significativo a 5%, nem a de ganhos maiores." else
		if (mon) "O efeito cresce em módulo com o tamanho do ganho, como se espera se ele for causal." else
		"O efeito não cresce de forma sistemática com o tamanho do ganho, o que enfraquece a leitura causal."
	une <- if (un == "p.p.") "pontos percentuais" else un
	sprintf("<p><b>%s.</b> Separando pelo tamanho do ganho, o efeito é de %s (em %s). %s</p>", rotulo,
		lista_e(paste0(sinal(z$b, d), ifelse(grepl("^quintil", z$versao), paste0(" no ", z$versao), paste0(" na ", sub("^(ganho|faixa)( de)? ?", "faixa de ", z$versao))),
			" (", sig_curto(z$p), ", ", fp(z$p), ")")), une, juizo)
}
## retirando um evento por vez
frase_loo <- function(l, b, p, rotulo, d = 3, un = "p.p.") {
	if (!nrow(l)) return(NULL)
	ns <- sum(!is.na(l$p) & l$p < 0.05)
	troca <- l[sign(efeito) != sign(b)]
	s0 <- !is.na(p) && p < 0.05
	juizo <- if (!s0 && ns == 0) "O resultado nulo não depende de nenhum evento." else
		if (!s0) "Sem alguns eventos o efeito fica significativo, mas a estimativa com todos os eventos é nula." else
		if (nrow(troca)) paste0("Sem o evento ", lista_e(as.character(troca$evento_fora)), ", o sinal muda: o resultado depende desses eventos.") else
		if (ns == nrow(l)) "Nenhum evento sozinho muda o resultado." else "O sinal nunca muda, mas a significância depende de alguns eventos."
	sprintf("<p><b>%s.</b> Com todos os eventos, o efeito é %s %s (%s). Retirando um evento de cada vez, ele fica entre %s e %s %s e é significativo a 5%% em %d das %d estimativas. %s</p>",
		rotulo, sinal(b, d), un, fp(p), sinal(min(l$efeito), d), sinal(max(l$efeito), d), un, ns, nrow(l), juizo)
}
## leitura do balanço do IPTW, em frases
leitura_balanco <- function(b, rot) {
	if (is.null(b) || !nrow(b)) return(invisible(NULL))
	b <- copy(b)
	if (!"corte" %in% names(b)) b[, corte := NA]
	if (!"trajeto" %in% names(b)) b[, trajeto := NA_character_]
	b[, nome := paste0(fifelse(is.na(corte), fifelse(trajeto %in% "todos", "todos os trajetos", paste0("trajetos ", trajeto, " min")), rot_corte(corte)), ", ", unname(rot[faixa]))]
	pp <- b[janela == "pos"]
	ruins <- pp[smd_max_depois >= 0.1 | ess_controles < 1000][order(-smd_max_depois)]
	corpo <- sprintf("<p>Antes dos pesos, a maior diferença padronizada entre tratados e controles vai de %s a %s nas %d comparações (faixas × trajetos × janelas): os grupos eram diferentes, como esperado. Depois dos pesos, ela vai de %s a %s, e só %d das %d comparações ficam abaixo de 0,1, o limite usual para dizer que os grupos ficaram parecidos.</p>",
		num(min(b$smd_max_antes), 2), num(max(b$smd_max_antes), 2), nrow(b), num(min(b$smd_max_depois), 2), num(max(b$smd_max_depois), 2), sum(b$smd_max_depois < 0.1), nrow(b))
	if (nrow(ruins)) corpo <- c(corpo, sprintf("<p><b>O balanço não é bom em %d das %d comparações depois da abertura.</b> %s</p>", nrow(ruins), nrow(pp),
		paste0("Em ", ruins$nome, ", a maior diferença padronizada vai de ", num(ruins$smd_max_antes, 2), " antes para ", num(ruins$smd_max_depois, 2),
			" depois dos pesos, e só ", num(ruins$ess_controles, 0), " controles efetivos (de ", num(ruins$controles, 0), ") carregam a comparação.", collapse = " ")),
		"<p>Isso quer dizer que os pesos ficaram concentrados em poucas pessoas e, mesmo assim, não deixaram os grupos parecidos em todas as características. Nesses casos, o resultado do IPTW depende de poucos controles e pesa menos que os do B&amp;H.</p>")
	html_leitura("O que o balanço mostra", corpo)
}

## ---- robustez da área e do tempo de viagem, com leitura em texto -------------------------------------------------------------
robustez_area_texto <- function(R) {
	R <- copy(R)
	R[, `:=`(ind = fifelse(dose == "c60", "60 min (absoluta)", "gravitacional (+0,1 log)"), c = cel(efeito, ep, p_perm, 3))]
	for (t in names(ROB_AREA_TXT)) {
		x <- R[teste == t | (teste == "principal" & t != "R6 faixas de ganho em % (abaixo de 1% = zero)")]
		if (t == "R1 outras aberturas") x <- x[termo == "dose"]
		if (!nrow(x[teste == t])) next
		cat("\n#### ", ROB_AREA_TXT[[t]]$titulo, "\n\n", ROB_AREA_TXT[[t]]$txt, "\n\n", sep = "")
		x[, versao := fifelse(teste == "principal", "principal", fifelse(grepl("^faixa", termo), sub("^faixa ", "faixa de ", termo), rot_teste(teste)))]
		x[, linha := paste0(segmento, " · ", ind, " · ", versao)]
		ordl <- unique(x[order(match(segmento, SEGS), ind, teste != "principal", versao)]$linha)
		tabs_categoria(x, CAT_AREA[c("emprego", "entrada")], "linha", "Grupo · indicador · versão", ordem = ordl, nivel = 5)
		y <- x[segmento == "todos" & janela == "pos" & outcome %in% c("emprego_12", "entrada_12", "entrada_24")]
		if (!nrow(y)) next
		corpo <- character(0)
		for (o in unique(y$outcome)) for (i in unique(y$ind)) {
			z <- y[outcome == o & ind == i, .(versao, b = efeito, p = p_perm)]
			rot <- paste0(maiuscula(desf_txt(o)), ", ", i)
			corpo <- c(corpo, if (grepl("^R6", t)) frase_faixas(z, rot) else frase_variantes(z, rot))
		}
		html_leitura("O que este teste mostra (todas as pessoas, depois da abertura)", corpo)
	}
}
leitura_rob_par <- function(R) {
	x <- R[segmento == "todos" & janela == "pos" & outcome %in% c("demissao_6", "demissao_12") & !grepl("^R3", teste) & !grepl("acima de 90|acesso", termo)]
	if (!nrow(x)) return(invisible(NULL))
	corpo <- character(0)
	for (o in c("demissao_6", "demissao_12")) {
		pr <- x[outcome == o & teste == "principal (modelo A)" & termo == "trajeto, até 90"]
		if (!nrow(pr)) next
		simples <- x[outcome == o & teste %in% c("R1 outras aberturas", "R2 efeitos fixos H3-8") & grepl("^trajeto, até 90", termo)]
		u <- x[outcome == o & grepl("^R8", teste)]
		z <- rbind(pr[, .(versao = "principal", b = efeito, p = p_perm)], simples[, .(versao = rot_teste(teste), b = efeito, p = p_perm)],
			u[, .(versao = paste0(rot_teste(teste), " (", termo, ")"), b = efeito, p = p_perm)])
		corpo <- c(corpo, frase_variantes(z, paste0(maiuscula(desf_txt(o)), ", todas as pessoas"), d = 2))
		for (t in c("R6 faixas de ganho em minutos", "R6 faixas de ganho em % (abaixo de 1% = zero)", "R6 quintis do ganho")) {
			f <- x[outcome == o & teste == t]
			if (nrow(f)) corpo <- c(corpo, frase_faixas(f[, .(versao = termo, b = efeito, p = p_perm)], paste0(maiuscula(desf_txt(o)), ", ", rot_teste(t)), d = 2))
		}
	}
	html_leitura("O que os testes de robustez mostram", corpo)
}

## ---- B2: trajeto e acessibilidade juntos ---------------------------------------------------------------------------------------
## termos de cada recorte de trajeto: modelo com trajeto e acesso (B ou UB) e os nomes dos dois termos
TERMOS_TRJ <- list(ate90 = list(md = "B", tr = "trajeto, até 90", ac = "acesso, até 90", mA = "A", rot = "trajetos de até 90 minutos"),
	acima90 = list(md = "B", tr = "trajeto, acima de 90", ac = "acesso, acima de 90", mA = "A", rot = "trajetos de mais de 90 minutos"),
	todos = list(md = "UB", tr = "trajeto, sem divisão (por 10 min)", ac = "acesso, sem divisão", mA = "U", rot = "todos os trajetos juntos"))
texto_b2 <- function(P, trj = "ate90", ocs = c("demissao_6", "demissao_12", "pedido_6", "empregado_12")) {
	tt <- TERMOS_TRJ[[trj]]
	x <- unique(P[nivel == "h39" & fe == "casa7trab7" & modelo == tt$md & termo %in% c(tt$tr, tt$ac) & outcome %in% ocs], by = c("segmento", "outcome", "janela", "termo"))
	if (!nrow(x)) return(invisible(NULL))
	y <- pareia(x, c("segmento", "outcome", "termo"))
	corpo <- sprintf("<p>Em cada desfecho, os dois efeitos vêm da mesma regressão (%s): o de 10 minutos a menos no trajeto até o emprego atual e o de +10%% de acessibilidade a outros empregos a partir de casa.</p>", tt$rot)
	for (g in intersect(SEGS, unique(y$segmento))) {
		f <- character(0)
		so_traj <- FALSE
		for (o in intersect(ocs, unique(y[segmento == g, outcome]))) {
			tr <- y[segmento == g & outcome == o & termo == tt$tr]
			ac <- y[segmento == g & outcome == o & termo == tt$ac]
			if (!nrow(tr) || !nrow(ac)) next
			f <- c(f, sprintf("Em %s, 10 minutos a menos no trajeto mudam a chance em %s %s (%s, %s), enquanto +10%% de acessibilidade a outros empregos a muda em %s (%s, %s).",
				sub("^a chance de", "relação à chance de", desf_txt(o)), sinal(tr$b), un_ext(o, tr$b), sig_ext(tr$p), fp(tr$p), sinal(ac$b), sig_ext(ac$p), fp(ac$p)))
			if (o == "demissao_6") so_traj <- !is.na(tr$p) && tr$p < 0.05 && (is.na(ac$p) || ac$p >= 0.05)
		}
		if (length(f)) corpo <- c(corpo, sprintf("<p><b>%s.</b> %s%s</p>", rot_grupo(g), paste(f, collapse = " "),
			if (so_traj) " Ou seja, a queda nas demissões vem do próprio trajeto: com os dois ganhos juntos, a acessibilidade a outros empregos não tem efeito significativo sobre a demissão." else ""))
	}
	html_leitura("O que estes resultados mostram", corpo)
}
## efeito por faixa de ganho (desenho E): cresce com o tamanho do ganho?
texto_faixas_e <- function(P0, ocs = c("demissao_6", "demissao_12")) {
	z <- unique(P0[segmento == "todos" & faixa != "todas" & outcome %in% ocs & janela == "pos"], by = c("faixa", "outcome"))
	if (!nrow(z)) return(invisible(NULL))
	corpo <- character(0)
	for (o in ocs) {
		x <- z[outcome == o][order(match(faixa, names(ROT_FAIXA_P)))]
		if (nrow(x)) corpo <- c(corpo, frase_faixas(x[, .(versao = unname(ROT_FAIXA_P[faixa]), b = efeito, p = p)], paste0(maiuscula(desf_txt(o)), ", todas as pessoas"), d = 2))
	}
	html_leitura("O efeito cresce com o tamanho do ganho?", corpo)
}

## ---- conclusões de cada desenho (24/11/2026) ---------------------------------------------------------------------------------
## Textos articulados a partir das estimativas: as frases dependem do que os números mostram (sinal, significância, placebo),
## para continuarem corretas quando as estimações em execução terminarem.
html_conclusao <- function(corpo) cat('\n\n```{=html}\n<div class="conclusao">\n', limpa_txt(paste(corpo[!is.na(corpo) & nzchar(corpo)], collapse = "\n")), "\n</div>\n```\n\n", sep = "")
sig <- function(r) nrow(r) > 0 && !is.na(r$p[1]) && r$p[1] < 0.05
## "a demissão sem justa causa cai 0,88 p.p. (p < 0,001)" / "não muda de forma distinguível de zero (−0,07 p.p., p = 0,216)"
muda <- function(r, nome, d = 2) {
	if (!nrow(r)) return(NA_character_)
	r <- r[1]
	un <- un_oc(r$outcome)
	if (!is.na(r$p) && r$p < 0.05) sprintf("%s %s %s %s (%s)", nome, if (r$b < 0) "cai" else "sobe", num(abs(r$b), d), un, fp(r$p)) else
		if (!is.na(r$p) && r$p < 0.10) sprintf("%s %s %s %s, com evidência fraca (%s)", nome, if (r$b < 0) "cai" else "sobe", num(abs(r$b), d), un, fp(r$p)) else
		sprintf("%s não muda de forma distinguível de zero (%s %s, %s)", nome, sinal(r$b, d), un, fp(r$p))
}
## placebos significativos (10%) num conjunto de efeitos
frase_placebos <- function(y) {
	pl <- y[!is.na(p0) & p0 < 0.10]
	if (!nrow(y[!is.na(b0)])) return("Os placebos ainda não foram estimados.")
	if (!nrow(pl)) return(sprintf("Nenhum dos %d placebos correspondentes é significativo a 10%%: antes da abertura, quem teria o trajeto encurtado não se comportava de forma diferente.", nrow(y[!is.na(b0)])))
	sprintf("Atenção: %d dos %d placebos são significativos a 10%% (%s), o que pede cautela nesses casos.", nrow(pl), nrow(y[!is.na(b0)]),
		lista_e(paste0(pl$segmento, ", ", desf_txt(pl$outcome))))
}
## diferença entre dois grupos num desfecho, em frase (ou NA se não houver)
frase_dif <- function(CT, contraste_x, o, por) {
	if (is.null(CT) || !nrow(CT)) return(NA_character_)
	x <- unique(CT[contraste == contraste_x & outcome == o & !is.na(diferenca)], by = "janela")
	pos <- x[janela == "pos"]
	pre <- x[janela == "pre"]
	if (!nrow(pos)) return(NA_character_)
	frase_ct(contraste_x, o, pos$diferenca, pos$p, if (nrow(pre)) pre$diferenca else NA, if (nrow(pre)) pre$p else NA, por)
}

## o mesmo efeito nos três recortes de trajeto; rr: lista nomeada (ate90, acima90, todos) de linhas com outcome, b, p
frase_recortes <- function(rr, nome_ef) {
	rot <- c(ate90 = "nos trajetos de até 90 minutos", acima90 = "nos trajetos de mais de 90 minutos", todos = "com todos os trajetos juntos")
	it <- character(0)
	falta <- character(0)
	for (k in names(rot)) {
		r <- rr[[k]]
		if (is.null(r) || !nrow(r)) falta <- c(falta, rot[[k]]) else
			it <- c(it, sprintf("%s, %s %s (%s, %s)", rot[[k]], sinal(r$b[1]), un_ext(r$outcome[1], r$b[1]), sig_curto(r$p[1]), fp(r$p[1])))
	}
	if (!length(it)) return(NA_character_)
	sprintf("<p><b>Recortes de trajeto.</b> %s é de %s.%s</p>", nome_ef, lista_e(it),
		if (length(falta)) paste0(" A estimativa ", lista_v(falta), " ainda está em execução.") else "")
}

## ---- A ---------------------------------------------------------------------------------------------------------------------
conclusao_a <- function(A, CT, R) {
	if (is.null(A)) return(em_execucao())
	d <- area_principal(A)
	z <- pareia(d, c("versao", "outcome"), "efeito_1dp", "ep_1dp", "p_perm")
	z[, `:=`(k = categoria(outcome), h = horizonte(outcome))]
	z[, limpo := !is.na(p) & p < 0.05 & (is.na(p0) | p0 >= 0.10)]
	ee <- z[k %in% c("emprego", "entrada") & h %in% c(12, 24)]
	ss <- z[k == "salario" & h %in% c(12, 24)]
	m_ent <- d[outcome == "entrada_12" & janela == "pos", mean(media_y, na.rm = TRUE)]
	teto <- ee[, max(abs(b) + 1.96 * se, na.rm = TRUE)]
	nl <- sum(ee$limpo)
	esp <- 0.05 * nrow(ee)
	lp <- ee[limpo == TRUE & b > 0]
	ln <- ee[limpo == TRUE & b < 0]
	## sem efeito positivo: nenhuma versão positiva com placebo nulo, ou poucas (no nível do acaso) e minúsculas
	nulo <- !nrow(lp) || (nrow(lp) <= 1.5 * esp && max(lp$b) < 0.1)
	sujos <- ee[!is.na(p) & p < 0.05 & !limpo]
	lista_v <- function(x) lista_e(paste0(x$versao, ", ", desf_txt(x$outcome), " (", sinal(x$b, 3), " p.p.)"))
	c1 <- sprintf("<p><b>Emprego e entrada no emprego formal.</b> Testamos %d combinações: cinco indicadores de acessibilidade (cumulativas de 30, 45, 60 e 90 minutos e gravitacional), duas medidas do ganho (absoluta e percentual), dois desfechos (emprego e entrada no emprego formal) e dois horizontes (12 e 24 meses). %s Os efeitos de um desvio-padrão do ganho vão de %s a %s p.p., e os intervalos de 95%% excluem efeitos maiores que %s p.p. em módulo. Para comparação, %s%% das pessoas sem vínculo formal entram no emprego formal em 12 meses.%s</p>",
		nrow(ee), if (!nl) sprintf("Nenhuma delas tem efeito significativo a 5%% com placebo nulo (o acaso sozinho produziria cerca de %s).", num(esp, 0)) else
			if (!nrow(lp)) sprintf("%d delas %s efeito significativo a 5%% com placebo nulo (o acaso sozinho produziria cerca de %s), mas %s negativo%s e minúsculo%s, o contrário do que se esperaria de um efeito positivo da acessibilidade: %s.",
				nl, if (nl == 1) "tem" else "têm", num(esp, 0), if (nl == 1) "é" else "todos são", if (nl == 1) "" else "s", if (nl == 1) "" else "s", lista_v(ln)) else
			sprintf("%d delas têm efeito significativo a 5%% com placebo nulo (o acaso sozinho produziria cerca de %s): %d positivas (%s)%s.", nl, num(esp, 0), nrow(lp), lista_v(lp),
				if (nrow(ln)) paste0(" e ", nrow(ln), " negativas (", lista_v(ln), ")") else ""),
		sinal(min(ee$b), 3), sinal(max(ee$b), 3), num(teto, 2), num(m_ent, 1),
		if (nrow(sujos)) sprintf(" Outras %d versões dão significativas, mas o placebo delas também dá (%s): a diferença já existia antes da abertura.", nrow(sujos),
			lista_e(paste0(sujos$versao, ", ", desf_txt(sujos$outcome)))) else "")
	sl <- ss[limpo == TRUE]
	sal_peq <- nrow(sl) && max(abs(sl$b)) < 0.5
	c2 <- if (nrow(ss)) sprintf("<p><b>Salário-hora.</b> Entre quem estava empregado no mês de referência e no fim do período, %s</p>",
		if (!nrow(sl)) sprintf("nenhuma das %d versões (12 e 24 meses) tem efeito significativo com placebo nulo.", nrow(ss)) else
			sprintf("%d das %d versões (12 e 24 meses) têm efeito significativo com placebo nulo, %s, de no máximo %s%% por desvio-padrão do ganho%s.", nrow(sl), nrow(ss),
				if (all(sl$b > 0)) "todas positivas" else if (all(sl$b < 0)) "todas negativas" else "com sinais opostos", num(max(abs(sl$b)), 3),
				if (sal_peq) ": um efeito pequeno demais para ter importância econômica" else "")) else NA
	c3 <- NA
	if (!is.null(CT) && nrow(CT)) {
		x <- unique(CT[janela == "pos" & !is.na(diferenca)], by = c("versao", "contraste", "outcome"))
		pre <- unique(CT[janela == "pre" & !is.na(diferenca)], by = c("versao", "contraste", "outcome"))[, .(versao, contraste, outcome, p0 = p)]
		x <- merge(x, pre, by = c("versao", "contraste", "outcome"), all.x = TRUE)
		s <- x[!is.na(p) & p < 0.05]
		e2 <- 0.05 * nrow(x)
		fq <- s[is.na(p0) | p0 >= 0.10, .N, by = .(contraste, k = categoria(outcome), sn = sign(diferenca))][order(-N)]
		padrao <- if (nrow(fq) && fq$N[1] >= 3 && !grepl("^\\(|teste", fq$contraste[1])) {
			g <- strsplit(fq$contraste[1], " − ", fixed = TRUE)[[1]]
			sprintf(" O padrão que mais se repete é que o efeito sobre %s é %s para %s do que para %s, em %d combinações de indicador e horizonte, sempre com placebo nulo.",
				sub(" \\(.*", "", DESF[[fq$k[1]]]), if (fq$sn[1] < 0) "menor" else "maior", grupo_txt(g[1]), grupo_txt(g[2]), fq$N[1])
		} else ""
		c3 <- sprintf("<p><b>Diferenças entre grupos.</b> Das %d comparações entre grupos (todos os indicadores, desfechos e horizontes), %d são significativas a 5%%, contra cerca de %s esperadas por acaso. %s%s</p>",
			nrow(x), nrow(s), num(e2, 0), if (nrow(s) <= 1.5 * e2) "Não há, portanto, sinal de que algum grupo se beneficie de forma diferente dos demais." else
				"Há mais diferenças do que o acaso explicaria; elas estão nas tabelas de heterogeneidade.", padrao)
	}
	c4 <- NA
	sp <- A[segmento == "todos" & janela == "pos" & outcome == "emprego_12" & ind == "60 min" & medida == "absoluto"]
	sp[, espec := especificacao_area(nivel, fe)]
	sp <- unique(sp[!is.na(espec)], by = "espec")
	if (nrow(sp) > 1) {
		pr <- sp[espec == ESPEC_AREA[1]]
		ou <- sp[espec != ESPEC_AREA[1]]
		loo <- if (!is.null(R)) R[grepl("^R3", teste) & segmento == "todos" & janela == "pos" & outcome == "emprego_12" & dose == "c60"] else NULL
		c4 <- sprintf("<p><b>Robustez.</b> Para o emprego em 12 meses na cumulativa de 60 minutos (por +100 mil empregos), a estimativa principal é %s (%s); nas outras unidades e definições de vizinhança fica entre %s e %s, com %d de %d significativas%s.</p>",
			sinal(pr$efeito, 3), fp(pr$p_perm), sinal(min(ou$efeito), 3), sinal(max(ou$efeito), 3), sum(ou$p_perm < 0.05, na.rm = TRUE), nrow(ou),
			if (!is.null(loo) && nrow(loo)) sprintf(", e retirando um evento por vez fica entre %s e %s, com %d de %d significativas", sinal(min(loo$efeito), 3), sinal(max(loo$efeito), 3),
				sum(loo$p_perm < 0.05, na.rm = TRUE), nrow(loo)) else "")
	}
	c5 <- if (nulo) paste0("<p><b>Conclusão.</b> A margem de <i>conseguir</i> emprego não respondeu às novas estações no horizonte de até dois anos. Para as pessoas de baixa renda do CadÚnico, ter mais empregos ao alcance do transporte público a partir de casa não se traduziu em mais emprego formal nem em mais entradas no emprego formal, em nenhum dos cinco indicadores e nas duas formas de medir o ganho.",
		if (teto < 0.1 * m_ent) " Os intervalos de confiança são estreitos: descartam efeitos de tamanho relevante, e não apenas deixam de detectá-los." else "",
		if (nrow(ln)) " As poucas estimativas significativas são negativas e minúsculas, e não mudam essa leitura." else "",
		if (nrow(ss)) if (nrow(sl) && sal_peq) " O salário de quem já estava empregado sobe, no máximo, de forma desprezível." else if (!nrow(sl)) " O salário de quem já estava empregado também não muda." else "" else "",
		" Esses resultados não excluem efeitos em prazos mais longos, no trabalho informal (que a RAIS não observa) ou sobre quem se mudou depois da abertura.</p>") else
		"<p><b>Conclusão.</b> Há sinais de efeito da acessibilidade sobre o emprego em algumas versões com placebo nulo (listadas acima). Antes de lê-los como efeito, é preciso ver se eles se repetem entre indicadores próximos e entre os horizontes de 12 e 24 meses, e se resistem aos testes de robustez.</p>"
	html_conclusao(c(c1, c2, c3, c4, c5))
}

## ---- B ---------------------------------------------------------------------------------------------------------------------
conclusao_b <- function(P, PCT, R) {
	if (is.null(P)) return(em_execucao())
	pp <- par_principal(P)
	segs <- intersect(c("todos", "mulheres", "homens"), unique(pp$segmento))
	if (!length(segs)) return(em_execucao())
	ocs <- c("demissao_6", "demissao_12", "sem_justa_6", "justa_6", "pedido_6", "empregado_12")
	y <- pareia(pp[segmento %in% segs & outcome %in% ocs], c("segmento", "outcome"))
	y <- merge(y, unique(pp[janela == "pos", .(segmento, outcome, media = media_y)], by = c("segmento", "outcome")), by = c("segmento", "outcome"), all.x = TRUE)
	g <- function(s, o) y[segmento == s & outcome == o]
	it <- vapply(segs, function(s) {
		r <- rbind(g(s, "demissao_6"), g(s, "demissao_12"))
		sprintf("<li><i>%s:</i> %s.</li>", rot_grupo(s), lista_e(frase_ef(r$outcome, r$b, r$p, r$media)))
	}, character(1))
	c1 <- paste0("<p><b>Demissões.</b> Cada 10 minutos a menos no trajeto casa → trabalho (trajetos que levavam até 90 minutos):</p><ul>", paste(it, collapse = ""), "</ul>")
	s0 <- segs[1]
	sj <- g(s0, "sem_justa_6")
	jc <- g(s0, "justa_6")
	pd <- g(s0, "pedido_6")
	em <- g(s0, "empregado_12")
	c2 <- sprintf("<p><b>De onde vem a queda (%s, 6 meses).</b> %s; %s; e %s. %s %s</p>", tolower(rot_grupo(s0)),
		maiuscula(muda(sj, "a demissão sem justa causa")), muda(jc, "a demissão com justa causa"), muda(pd, "o pedido de demissão"),
		if (nrow(em)) paste0("Em 12 meses, ", muda(em, "a chance de continuar empregado"), ".") else "", frase_placebos(y))
	c3 <- NA
	d6 <- unique(pp[outcome == "demissao_6" & janela == "pos"], by = "segmento")
	if (nrow(d6) > 3) {
		sg <- d6[!is.na(p_perm) & p_perm < 0.05][order(match(segmento, SEGS))]
		difs <- c(frase_dif(PCT, "mulheres − homens", "demissao_6", "de 10 minutos a menos no trajeto"),
			frase_dif(PCT, "mulheres com criança − mulheres sem criança", "demissao_6", "de 10 minutos a menos no trajeto"))
		difs <- difs[!is.na(difs)]
		c3 <- sprintf("<p><b>Grupos.</b> A queda na demissão em 6 meses é significativa em %d dos %d grupos (%s).%s</p>", nrow(sg), nrow(d6), lista_e(sg$segmento),
			if (length(difs)) paste0(" ", paste(difs, collapse = " ")) else " As diferenças formais entre grupos (por exemplo, mulheres − homens) ainda estão em execução; até lá, não dá para dizer se o efeito é maior num grupo do que no outro.")
	}
	c4 <- NA
	b2 <- unique(P[nivel == "h39" & fe == "casa7trab7" & modelo == "B" & segmento == s0 & outcome == "demissao_6" & janela == "pos" & termo %in% c("trajeto, até 90", "acesso, até 90")], by = "termo")
	b2_ok <- FALSE
	if (nrow(b2) == 2) {
		tr <- b2[termo == "trajeto, até 90"]
		ac <- b2[termo == "acesso, até 90"]
		b2_ok <- tr$p_perm < 0.05 && ac$p_perm >= 0.05
		c4 <- sprintf("<p><b>Trajeto ou acessibilidade? (B2)</b> Com o ganho de acessibilidade a outros empregos na mesma regressão, 10 minutos a menos no trajeto mudam a demissão em 6 meses em %s p.p. (%s), e +10%% de acessibilidade a partir de casa a mudam em %s p.p. (%s)%s.</p>",
			sinal(tr$efeito), fp(tr$p_perm), sinal(ac$efeito), fp(ac$p_perm), if (b2_ok) ": o efeito vem do próprio trajeto, não da facilidade de chegar a outros empregos" else "")
	}
	un <- unique(P[nivel == "h39" & modelo %in% c("U", "P") & segmento == s0 & outcome == "demissao_6" & janela == "pos"], by = "modelo")
	c5 <- if (nrow(un)) sprintf("<p><b>Sem separar os trajetos em 90 minutos.</b> Com um único coeficiente para todos os trajetos, %s.</p>", lista_e(paste0(
		fifelse(un$modelo == "U", "10 minutos a menos mudam a demissão em 6 meses em ", "um trajeto 10% mais curto muda a demissão em 6 meses em "), sinal(un$efeito), " p.p. (", fp(un$p_perm), ")"))) else NA
	c6 <- NA
	ro <- P[modelo == "A" & termo == "trajeto, até 90" & outcome == "demissao_6" & janela == "pos" & segmento %in% segs]
	if (nrow(ro)) {
		ro[, espec := especificacao_par(nivel, fe, arquivo)]
		ro <- unique(ro[espec != ESPEC_PAR[1]], by = c("espec", "segmento"))
		if (nrow(ro)) c6 <- sprintf("<p><b>Robustez.</b> Nas variações de unidade, de vizinhança e com pares de tempo zero, o efeito sobre a demissão em 6 meses é negativo em %d de %d estimativas e significativo a 5%% em %d (de %s a %s p.p.).%s</p>",
			sum(ro$efeito < 0), nrow(ro), sum(ro$p_perm < 0.05, na.rm = TRUE), sinal(min(ro$efeito)), sinal(max(ro$efeito)),
			if (is.null(R)) " Os testes de outras aberturas, faixas de ganho e retirada de eventos ainda estão em execução." else "")
	}
	rr <- lapply(c(ate90 = "ate90", acima90 = "acima90", todos = "todos"), function(k) par_trj(P, k)[segmento == s0 & outcome == "demissao_6" & janela == "pos", .(outcome, b = efeito, p = p_perm)])
	c6b <- frase_recortes(rr, sprintf("Para %s, o efeito de 10 minutos a menos no trajeto sobre a chance de ser demitido em até 6 meses", tolower(rot_grupo(s0))))
	c6 <- c(c6, c6b)
	r0 <- g(s0, "demissao_6")
	caiu <- sig(r0) && r0$b < 0
	sj_cai <- sig(sj) && sj$b < 0
	pd_nulo <- nrow(pd) && !sig(pd)
	c7 <- if (caiu) paste0("<p><b>Conclusão.</b> A margem de <i>manter</i> o emprego respondeu às novas estações: trajetos mais curtos reduziram as demissões",
		if (sj_cai) ", sobretudo as sem justa causa" else "", if (pd_nulo) ", sem mudar os pedidos de demissão" else "", ". ",
		sprintf("Cada 10 minutos a menos equivalem a cerca de %s%% a menos na taxa de demissão em 6 meses, que é de %s%%. ", num(100 * abs(r0$b) / r0$media, 0), num(r0$media, 1)),
		if (sj_cai && pd_nulo) "O padrão é coerente com um canal pela relação de trabalho — menos atrasos, faltas e desgaste, e por isso menos dispensas — e não com trabalhadores usando o trajeto mais curto para procurar outro emprego. " else "",
		if (b2_ok) "O efeito vem do próprio trajeto: a acessibilidade a outros empregos, estimada junto, não explica a queda." else "", "</p>") else
		"<p><b>Conclusão.</b> Com as estimativas disponíveis até agora, não há queda significativa nas demissões no grupo principal; a conclusão deste desenho será revista quando as estimações em execução terminarem.</p>"
	html_conclusao(c(c1, c2, c3, c4, c5, c6, c7))
}

## ---- C ---------------------------------------------------------------------------------------------------------------------
conclusao_c <- function(P, D) {
	if (is.null(P) || !nrow(P[grepl("^ped_", outcome)])) return(em_execucao())
	ocs <- paste0(c("pedido", DEST$oc[-1]), "_12")
	pb <- unique(P[nivel == "h39" & fe == "casa7trab7" & modelo == "B" & termo == "trajeto, até 90" & outcome %in% ocs], by = c("segmento", "outcome", "janela"))
	segs <- intersect(c("todos", "mulheres", "homens"), unique(pb$segmento))
	if (!length(segs)) return(em_execucao())
	s0 <- segs[1]
	y <- pareia(pb[segmento == s0], c("segmento", "outcome"))
	md <- unique(pb[segmento == s0 & janela == "pos", .(outcome, media_y)], by = "outcome")
	tot <- y[outcome == "pedido_12"]
	des <- y[outcome != "pedido_12"]
	ds <- des[!is.na(p) & p < 0.05]
	semnovo <- md[outcome == "ped_semnovo_12", media_y] / md[outcome == "pedido_12", media_y]
	c1 <- sprintf("<p><b>O que acontece com quem pede demissão.</b> Para %s, %s%% dos vínculos terminam em pedido de demissão em 12 meses. Desses pedidos, %s%% não são seguidos de nenhum emprego formal novo em 12 meses; os demais vão para um novo emprego formal.</p>",
		tolower(rot_grupo(s0)), num(md[outcome == "pedido_12", media_y], 1), num(100 * semnovo, 0))
	c2 <- sprintf("<p><b>Efeito da estação sobre os destinos.</b> Cada 10 minutos a menos no trajeto (modelo B2) %s. %s</p>",
		if (nrow(tot)) sub("^(reduz|aumenta)", "\\1", frase_ef(tot$outcome, tot$b, tot$p)) else "—",
		if (!nrow(ds)) "Nenhum destino muda de forma significativa: nem a ida para empregos com trajeto mais curto, nem para empregos com salário maior, nem a saída do emprego formal." else
			paste0("Entre os destinos, ", lista_e(frase_ef(ds$outcome, ds$b, ds$p)), "; os demais não mudam de forma significativa."))
	c3 <- NA
	if (!is.null(D)) {
		g <- D[nivel == "h39" & janela == "pos" & h == 12 & trajeto == "até 90" & segmento == s0 & grupo %in% c("pedido", "nao_pediu"),
			.(traj = weighted.mean(traj_recentrado_min, vinculos), acc = 100 * weighted.mean(acesso_recentrado_log, vinculos)), by = grupo]
		if (nrow(g) == 2) c3 <- sprintf("<p><b>Quem pediu demissão tinha ganhado mais?</b> Em média, quem pediu demissão teve um ganho inesperado de trajeto de %s min e de acessibilidade de %s%%, contra %s min e %s%% de quem não pediu: os dois grupos tiveram, na prática, o mesmo ganho.</p>",
			sinal(g[grupo == "pedido", traj], 2), sinal(g[grupo == "pedido", acc], 2), sinal(g[grupo == "nao_pediu", traj], 2), sinal(g[grupo == "nao_pediu", acc], 2))
	}
	curto <- des[outcome == "ped_curto_12"]
	salm <- des[outcome == "ped_salmaior_12"]
	troca <- (sig(curto) && curto$b > 0) || (sig(salm) && salm$b > 0)
	c4 <- if (!troca) paste0("<p><b>Conclusão.</b> Não há sinal de que o trajeto mais curto tenha levado as pessoas a pedir demissão para trocar de emprego por outro mais perto de casa ou mais bem pago. ",
		if (nrow(tot) && !sig(tot)) "O total de pedidos não muda. " else "",
		if (!nrow(ds)) "Nenhum destino muda de forma significativa. " else
			sprintf("%s significativa entre os destinos (%s) não aponta para empregos mais próximos nem mais bem pagos. ", if (nrow(ds) == 1) "A única mudança" else "As mudanças",
				lista_e(sub("^a chance de ", "", desf_txt(ds$outcome)))),
		"Isso reforça a leitura do desenho B: o efeito das estações sobre a permanência no emprego passa pela redução das demissões, e não por mais mobilidade voluntária entre empregos.</p>") else
		"<p><b>Conclusão.</b> Há sinal de que parte dos pedidos de demissão passou a ir para empregos mais próximos ou mais bem pagos, como indicado acima: o trajeto mais curto pode ter facilitado a troca de emprego.</p>"
	html_conclusao(c(c1, c2, c3, c4))
}

## ---- D ---------------------------------------------------------------------------------------------------------------------
conclusao_d <- function(IP, BAL, IPC, IA) {
	if (is.null(IP)) return(em_execucao())
	x <- unique(IP[nivel == "ind" & trajeto == "até 90" & faixa == "f10mais" & segmento %in% c("todos", "mulheres", "homens") & outcome %in% c("demissao_6", "demissao_12")],
		by = c("segmento", "outcome", "janela"))
	y <- pareia(x, c("segmento", "outcome"), "att", "ep", "p")
	y <- merge(y, unique(x[janela == "pos", .(segmento, outcome, media = media_tratados - att)], by = c("segmento", "outcome")), by = c("segmento", "outcome"), all.x = TRUE)
	it <- vapply(intersect(c("todos", "mulheres", "homens"), unique(y$segmento)), function(s) {
		r <- y[segmento == s][order(outcome == "demissao_12")]
		sprintf("<li><i>%s:</i> ganhar 10 minutos ou mais %s.</li>", rot_grupo(s), lista_e(frase_ef(r$outcome, r$b, r$p, r$media)))
	}, character(1))
	c1 <- paste0("<p><b>Demissões (trajetos de até 90 minutos, ganho de 10 minutos ou mais).</b></p><ul>", paste(it, collapse = ""), "</ul>")
	dmh <- if (is.null(IPC)) NA else frase_dif(IPC[nivel == "ind" & trajeto == "até 90" & faixa == "f10mais"], "mulheres − homens", "demissao_6", "de ganhar 10 minutos ou mais")
	c2 <- if (!is.na(dmh)) paste0("<p><b>Mulheres e homens.</b> ", dmh, "</p>") else NA
	ruim <- FALSE
	c3 <- NA
	if (!is.null(BAL)) {
		b <- BAL[nivel == "ind" & segmento == "todos" & janela == "pos" & trajeto == "até 90" & faixa == "f10mais"]
		if (nrow(b)) {
			ruim <- b$smd_max_depois >= 0.1 || b$ess_controles < 1000
			c3 <- sprintf("<p><b>Balanço.</b> Nessa comparação, a maior diferença padronizada entre tratados e controles é de %s antes e %s depois dos pesos, e só %s dos %s controles carregam, na prática, a comparação. %s</p>",
				num(b$smd_max_antes, 2), num(b$smd_max_depois, 2), num(b$ess_controles, 0), num(b$controles, 0),
				if (ruim) "Os pesos não deixaram os grupos parecidos e ficaram concentrados em poucas pessoas." else "Os pesos deixaram os grupos parecidos.")
		}
	}
	c4 <- if (is.null(IA) || !nrow(IA[nivel == "ind" & janela == "pos"])) "<p><b>Área.</b> O IPTW de área com todos os indicadores ainda está em execução.</p>" else {
		a <- pareia(unique(IA[nivel == "ind" & segmento == "todos" & categoria(outcome) %in% c("emprego", "entrada") & horizonte(outcome) %in% c(12, 24)], by = c("corte", "faixa", "outcome", "janela")),
			c("corte", "faixa", "outcome"), "att", "ep", "p")
		nl <- sum(!is.na(a$p) & a$p < 0.05 & (is.na(a$p0) | a$p0 >= 0.10))
		sprintf("<p><b>Área.</b> No emprego e na entrada no emprego formal (12 e 24 meses), %d das %d combinações de indicador e faixa de ganho têm efeito significativo com placebo nulo, contra cerca de %s esperadas por acaso.</p>", nl, nrow(a), num(0.05 * nrow(a), 0))
	}
	rr <- lapply(c(ate90 = "até 90", acima90 = "acima de 90", todos = "todos"), function(k)
		unique(IP[nivel == "ind" & trajeto == k & faixa == "f10mais" & segmento == "todos" & outcome == "demissao_6" & janela == "pos", .(outcome, b = att, p)]))
	c4 <- c(c4, frase_recortes(rr, "Para todas as pessoas, o efeito de ganhar 10 minutos ou mais sobre a chance de ser demitido em até 6 meses"))
	cai <- nrow(y[segmento == "todos" & outcome == "demissao_6"]) && y[segmento == "todos" & outcome == "demissao_6", b] < 0
	c5 <- paste0("<p><b>Conclusão.</b> ", if (cai) "O IPTW aponta na mesma direção do desenho B: quem teve o trajeto encurtado em 10 minutos ou mais passou a ser menos demitido, com a queda concentrada nas mulheres. " else "O IPTW não reproduz a queda nas demissões do desenho B. ",
		if (ruim) "Mas o balanço ruim, com poucos controles carregando a comparação, impede usar esse resultado como confirmação independente: ele depende de poucas pessoas e de uma hipótese mais forte que a do B&amp;H (de que, igualadas as características medidas, não há outras diferenças). Por enquanto, o IPTW é um indício coerente, e não uma prova adicional." else
			"Como os pesos deixaram os grupos parecidos, o resultado reforça o do desenho B com uma hipótese de identificação diferente.", "</p>")
	html_conclusao(c(c1, c2, c3, c4, c5))
}

## ---- E ---------------------------------------------------------------------------------------------------------------------
conclusao_e <- function(FP, FPC, FA) {
	if (is.null(FP)) return(em_execucao())
	P0 <- FP[nivel == "h39" & trajeto == "até 90"]
	x <- unique(P0[faixa == "todas" & segmento %in% c("todos", "mulheres", "homens") & outcome %in% c("demissao_6", "demissao_12", "sem_justa_6", "pedido_6", "empregado_12")],
		by = c("segmento", "outcome", "janela"))
	y <- pareia(x, c("segmento", "outcome"), "efeito", "ep", "p")
	y <- merge(y, unique(x[janela == "pos", .(segmento, outcome, media = media_controles)], by = c("segmento", "outcome")), by = c("segmento", "outcome"), all.x = TRUE)
	g <- function(s, o) y[segmento == s & outcome == o]
	it <- vapply(intersect(c("todos", "mulheres", "homens"), unique(y$segmento)), function(s) {
		r <- rbind(g(s, "demissao_6"), g(s, "demissao_12"))
		sprintf("<li><i>%s:</i> ser tratado %s.</li>", rot_grupo(s), lista_e(frase_ef(r$outcome, r$b, r$p, r$media)))
	}, character(1))
	c1 <- paste0("<p><b>Demissões (trajetos de até 90 minutos, todas as faixas de ganho).</b> Em comparação com quem terá um ganho da mesma faixa numa abertura posterior:</p><ul>", paste(it, collapse = ""), "</ul>")
	c2 <- sprintf("<p><b>De onde vem a queda (todas as pessoas, 6 meses).</b> %s; e %s. %s %s</p>", maiuscula(muda(g("todos", "sem_justa_6"), "a demissão sem justa causa")),
		muda(g("todos", "pedido_6"), "o pedido de demissão"), if (nrow(g("todos", "empregado_12"))) paste0("Em 12 meses, ", muda(g("todos", "empregado_12"), "a chance de continuar empregado"), ".") else "",
		frase_placebos(y))
	fx <- unique(P0[segmento == "todos" & faixa != "todas" & outcome == "demissao_12" & janela == "pos"], by = "faixa")[order(match(faixa, names(ROT_FAIXA_P)))]
	c3 <- if (nrow(fx) > 1) sprintf("<p><b>Tamanho do ganho.</b> Na demissão em 12 meses, o efeito é %s.%s</p>", lista_e(paste0(sinal(fx$efeito), " p.p. na faixa de ", sub("^ganho de ", "", ROT_FAIXA_P[fx$faixa]), " (", fp(fx$p), ")")),
		if (all(diff(-fx$efeito) >= 0)) " O efeito cresce com o tamanho do ganho." else " O efeito não cresce de forma sistemática com o tamanho do ganho: ganhos pequenos já parecem bastar.") else NA
	dmh <- if (is.null(FPC)) NA else frase_dif(FPC[nivel == "h39" & trajeto == "até 90" & faixa == "todas"], "mulheres − homens", "demissao_6", "de ser tratado")
	c4 <- if (!is.na(dmh)) paste0("<p><b>Mulheres e homens.</b> ", dmh, "</p>") else NA
	c5 <- NA
	if (!is.null(FA)) {
		a <- pareia(unique(FA[nivel == "h39" & segmento == "todos" & categoria(outcome) %in% c("emprego", "entrada") & horizonte(outcome) %in% c(12, 24)], by = c("corte", "faixa", "outcome", "janela")),
			c("corte", "faixa", "outcome"), "efeito", "ep", "p")
		nl <- sum(!is.na(a$p) & a$p < 0.05 & (is.na(a$p0) | a$p0 >= 0.10))
		c5 <- sprintf("<p><b>Área.</b> No emprego e na entrada no emprego formal (12 e 24 meses), %d das %d combinações de indicador de acessibilidade e faixa de ganho têm efeito significativo com placebo nulo, contra cerca de %s esperadas por acaso: %s</p>",
			nl, nrow(a), num(0.05 * nrow(a), 0), if (nl <= 1.5 * 0.05 * nrow(a)) "como no desenho A, ganhar acessibilidade não muda o emprego formal." else "há alguns sinais de efeito, listados no quadro da seção de área.")
	}
	rr <- lapply(c(ate90 = "até 90", acima90 = "acima de 90", todos = "todos"), function(k)
		unique(FP[nivel == "h39" & trajeto == k & faixa == "todas" & segmento == "todos" & outcome == "demissao_6" & janela == "pos", .(outcome, b = efeito, p)]))
	c5 <- c(c5, frase_recortes(rr, "Para todas as pessoas, o efeito de ser tratado sobre a chance de ser demitido em até 6 meses"))
	r <- g("todos", "demissao_6")
	cai <- sig(r) && r$b < 0
	pl_ok <- nrow(r) && (is.na(r$p0) || r$p0 >= 0.10)
	c6 <- paste0("<p><b>Conclusão.</b> ", if (cai && pl_ok) "A comparação com quem vai receber o mesmo ganho depois — pessoas em lugares escolhidos pelo mesmo planejamento, separadas só pelo momento — confirma o resultado do desenho B: ser tratado reduz as demissões, sem mudar os pedidos de demissão, e o placebo é nulo. Como este desenho depende de uma hipótese diferente (tendências paralelas entre tratados e futuros tratados), a concordância com o B&amp;H torna a queda nas demissões mais confiável. " else
		"A comparação com quem vai receber o mesmo ganho depois não confirma, com as estimativas atuais, uma queda significativa nas demissões. ",
		if (!is.null(FA)) "Na margem de conseguir emprego, este desenho também não encontra efeito da acessibilidade, como o desenho A." else "", "</p>")
	html_conclusao(c(c1, c2, c3, c4, c5, c6))
}

## ganho inesperado (realizado − esperado) de quem pediu demissão, por destino, contra quem não pediu: leitura em texto
texto_ganho_destino <- function(D, hz = 12, segs = c("todos", "mulheres", "homens"), trj = "ate90") {
	d <- D[nivel == "h39" & janela == "pos" & h == hz & trajeto == TRJ_ROT[[trj]] & segmento %in% segs]
	if (!nrow(d)) return(invisible(NULL))
	d <- d[, .(traj = weighted.mean(traj_recentrado_min, vinculos), acc = 100 * weighted.mean(acesso_recentrado_log, vinculos)), by = .(segmento, grupo)]
	corpo <- character(0)
	for (s in intersect(segs, unique(d$segmento))) {
		x <- d[segmento == s]
		np <- x[grupo == "nao_pediu"]
		pd <- x[grupo != "nao_pediu"]
		if (!nrow(np) || !nrow(pd)) next
		dt <- max(abs(pd$traj - np$traj))
		da <- max(abs(pd$acc - np$acc))
		fecho <- if (dt < 1 && da < 1) "As diferenças são de frações de minuto e de menos de um ponto percentual: quem pediu demissão não tinha recebido um ganho maior do que quem ficou no emprego." else
			sprintf("A maior diferença em relação a quem não pediu é de %s min no trajeto e de %s p.p. na acessibilidade.", num(dt, 2), num(da, 2))
		corpo <- c(corpo, sprintf("<p><b>%s.</b> Quem não pediu demissão teve, em média, ganho inesperado de %s min no trajeto e de %s%% na acessibilidade. Entre quem pediu, conforme o destino, o ganho inesperado de trajeto vai de %s a %s min e o de acessibilidade de %s%% a %s%%. %s</p>",
			rot_grupo(s), sinal(np$traj, 2), sinal(np$acc, 2), sinal(min(pd$traj), 2), sinal(max(pd$traj), 2), sinal(min(pd$acc), 2), sinal(max(pd$acc), 2), fecho))
	}
	html_leitura("O que esta comparação mostra", corpo)
}

## ---- diagnóstico do balanço do IPTW de tempo de viagem (24/11/2026) ----------------------------------------------------------
## Lê as tabelas agregadas do diagnóstico (outputs/final/iptw_diag_par_*): balanço e efeitos de cada versão dos pesos.
ROT_VERSAO <- c("V0 original" = "Original, sem corte", "V1 pesos cortados no p99" = "Corte no percentil 99 (adotada)",
	"V2 sem escore acima de 0,9" = "Sem escore acima de 0,9", "V3 controles do mesmo bairro de casa" = "Controles do mesmo bairro de casa",
	"V4 controles do mesmo bairro de casa e de trabalho" = "Controles do mesmo bairro de casa e de trabalho")
diagnostico_iptw <- function(dir = "outputs/final") {
	fb <- file.path(dir, "iptw_diag_par_balanco_versoes.csv")
	fe <- file.path(dir, "iptw_diag_par_efeitos_versoes.csv")
	if (!file.exists(fb) || !file.exists(fe)) return(invisible(NULL))
	b <- fread(fb)
	e <- fread(fe)
	b[, versao_r := factor(ROT_VERSAO[versao], ROT_VERSAO)]
	e[, versao_r := factor(ROT_VERSAO[versao], ROT_VERSAO)]
	## leitura em texto
	bb <- function(v, j = "pos", fx = "f10mais") b[versao == v & janela == j & faixa == fx]
	ee <- function(v, s, o = "demissao_6", j = "pos", fx = "f10mais") e[versao == v & segmento == s & outcome == o & janela == j & faixa == fx]
	f <- function(r) if (nrow(r)) sprintf("%s (%s)", sinal(r$att[1]), fp(r$p[1])) else "—"
	v0 <- bb("V0 original")
	v1 <- bb("V1 pesos cortados no p99")
	outras <- b[janela == "pos" & faixa == "f10mais" & !versao %in% c("V0 original", "V1 pesos cortados no p99")]
	V1 <- "V1 pesos cortados no p99"
	pl1 <- rbind(ee(V1, "todos", j = "pre"), ee(V1, "mulheres", j = "pre"), ee(V1, "homens", j = "pre"))
	corpo <- c(
		sprintf("<p><b>Balanço.</b> No ganho de 10 minutos ou mais, a maior diferença padronizada depois dos pesos cai de %s, na versão original, para %s com o corte no percentil 99, e os controles efetivos sobem de %s para %s. As outras correções também deixam os grupos parecidos (maior diferença entre %s e %s), mas com menos controles efetivos (de %s a %s).</p>",
			num(v0$smd_max_depois, 2), num(v1$smd_max_depois, 2), num(v0$ess_controles, 0), num(v1$ess_controles, 0), num(min(outras$smd_max_depois), 2),
			num(max(outras$smd_max_depois), 2), num(min(outras$ess_controles), 0), num(max(outras$ess_controles), 0)),
		sprintf("<p><b>Efeito.</b> Com o corte, ganhar 10 minutos ou mais muda a chance de ser demitido em até 6 meses em %s para todas as pessoas, em %s para as mulheres e em %s para os homens, em pontos percentuais (na versão original: %s, %s e %s). %s</p>",
			f(ee(V1, "todos")), f(ee(V1, "mulheres")), f(ee(V1, "homens")), f(ee("V0 original", "todos")), f(ee("V0 original", "mulheres")), f(ee("V0 original", "homens")),
			if (nrow(pl1) && all(pl1$p >= 0.10)) sprintf("No placebo, os mesmos efeitos são %s, %s e %s, todos estatisticamente indistinguíveis de zero: a queda nas demissões das mulheres sobrevive à correção e não aparece antes da abertura.",
				f(pl1[segmento == "todos"]), f(pl1[segmento == "mulheres"]), f(pl1[segmento == "homens"])) else
				sprintf("No placebo, os mesmos efeitos são %s, %s e %s.", f(pl1[segmento == "todos"]), f(pl1[segmento == "mulheres"]), f(pl1[segmento == "homens"]))),
		sprintf("<p><b>Controles do mesmo bairro.</b> Quando os controles vêm só do mesmo bairro de casa, a estimativa cai para %s em todas as pessoas e para %s nas mulheres; a demissão sem justa causa das mulheres fica em %s. O placebo dessa versão é menos limpo (demissão em até 12 meses de todas as pessoas: %s). Com o mesmo bairro de casa e de trabalho, sobram poucos controles efetivos e as estimativas ficam imprecisas.</p>",
			f(ee("V3 controles do mesmo bairro de casa", "todos")), f(ee("V3 controles do mesmo bairro de casa", "mulheres")),
			f(ee("V3 controles do mesmo bairro de casa", "mulheres", "sem_justa_6")), f(ee("V3 controles do mesmo bairro de casa", "todos", "demissao_12", "pre"))),
		sprintf("<p><b>Faixa de 5 a 10 minutos.</b> Com o corte, o efeito para as mulheres é %s, mas o placebo é %s: nessa faixa, o resultado pede cautela.</p>",
			f(ee(V1, "mulheres", fx = "f5a10")), f(ee(V1, "mulheres", j = "pre", fx = "f5a10"))))
	html_leitura("O que o diagnóstico mostra", corpo)
	## tabela de balanço
	tb <- b[order(faixa != "f10mais", janela != "pos", versao_r), .(Faixa = unname(ROT_FAIXA_P[faixa]), Janela = fifelse(janela == "pos", "depois", "placebo"),
		`Versão` = as.character(versao_r), Tratados = num(tratados, 0), Controles = num(controles, 0), `Controles efetivos` = num(ess_controles, 0),
		`Maior diferença padronizada, antes dos pesos` = num(smd_max_antes, 2), `Maior diferença padronizada, depois dos pesos` = num(smd_max_depois, 2),
		`Características mais desiguais depois dos pesos` = pior)]
	tabela(tb, titulo = "Balanço de cada versão (trajetos de até 90 minutos, todas as pessoas)")
	## efeitos: uma aba por faixa; linhas = versão; colunas = grupo · desfecho · janela
	e[, c := cel(att, ep, p)]
	cat("\n::: {.panel-tabset}\n")
	for (fx in c("f10mais", "f5a10")) {
		x <- e[faixa == fx & outcome %in% c("demissao_6", "demissao_12", "sem_justa_6")]
		if (!nrow(x)) next
		cat("\n#### ", ROT_FAIXA_P[[fx]], "\n\n", sep = "")
		x[, coluna := paste0(segmento, " · ", rot_oc(outcome), " · ", fifelse(janela == "pos", "depois", "placebo"))]
		ordc <- unique(x[order(match(segmento, SEGS), horizonte(outcome), categoria(outcome) != "demissao", janela != "pos")]$coluna)
		w <- dcast(x, versao_r ~ coluna, value.var = "c", fill = "—")
		setcolorder(w, c("versao_r", intersect(ordc, names(w))))
		w[, versao_r := as.character(versao_r)]
		setnames(w, "versao_r", "Versão")
		tabela(w, legenda = "Efeito médio nos tratados (trajetos de até 90 minutos), em pontos percentuais; entre parênteses, o erro-padrão.")
	}
	cat("\n:::\n")
	invisible(NULL)
}

## ---- conclusão geral (24/11/2026) ------------------------------------------------------------------------------------------
## Junta os principais números dos desenhos A a E num texto articulado. As frases dependem do que os números mostram.
conclusao_geral <- function(A, PAR, PCT, FP, FPC, FA, IP, IPC, dir = "outputs/final") {
	## efeito por extenso: "−0,95 ponto percentual (p < 0,001)"; efv: sem parênteses, para usar dentro de outros parênteses
	ef <- function(b, p, d = 2) if (length(b) && !is.na(b[1])) sprintf("%s %s (%s)", sinal(b[1], d), if (abs(b[1]) < 2) "ponto percentual" else "pontos percentuais", fp(p[1])) else "ainda em execução"
	efv <- function(b, p, d = 2) if (length(b) && !is.na(b[1])) sprintf("%s %s, %s", sinal(b[1], d), if (abs(b[1]) < 2) "ponto percentual" else "pontos percentuais", fp(p[1])) else "ainda em execução"
	para_g <- function(s) c(todos = "para todas as pessoas", mulheres = "para as mulheres", homens = "para os homens")[[s]]
	de_g <- function(s) c(todos = "de todas as pessoas", mulheres = "das mulheres", homens = "dos homens")[[s]]
	pp_ <- function(b) if (abs(b) < 2) "ponto percentual" else "pontos percentuais"
	corpo <- character(0)
	bh <- list(m = NA, h = NA)
	sj <- data.table()
	mulheres_todos <- FALSE
	## A e E (área): conseguir emprego
	nl_a <- NA
	if (!is.null(A)) {
		d <- area_principal(A)
		z <- pareia(d, c("versao", "outcome"), "efeito_1dp", "ep_1dp", "p_perm")
		z <- z[categoria(outcome) %in% c("emprego", "entrada") & horizonte(outcome) %in% c(12, 24)]
		z[, limpo := !is.na(p) & p < 0.05 & (is.na(p0) | p0 >= 0.10)]
		nl_a <- z[limpo == TRUE & b > 0, .N]
		teto <- z[, max(abs(b) + 1.96 * se, na.rm = TRUE)]
		m_ent <- d[outcome == "entrada_12" & janela == "pos", mean(media_y, na.rm = TRUE)]
	}
	nl_e <- NA
	if (!is.null(FA)) {
		a <- pareia(unique(FA[nivel == "h39" & segmento == "todos" & categoria(outcome) %in% c("emprego", "entrada") & horizonte(outcome) %in% c(12, 24)], by = c("corte", "faixa", "outcome", "janela")),
			c("corte", "faixa", "outcome"), "efeito", "ep", "p")
		nl_e <- sum(!is.na(a$p) & a$p < 0.05 & (is.na(a$p0) | a$p0 >= 0.10) & a$b > 0)
		esp_e <- 0.05 * nrow(a)
	}
	if (!is.na(nl_a)) corpo <- c(corpo, paste0("<p><b>1. Conseguir emprego: as estações não ajudaram.</b> ",
		if (nl_a == 0) "No desenho A, nenhuma das versões da acessibilidade (cinco indicadores, ganho absoluto e percentual, emprego e entrada no emprego formal, 12 e 24 meses) mostra efeito positivo com placebo nulo. " else
			sprintf("No desenho A, só %d das versões da acessibilidade mostram efeito positivo com placebo nulo, o que está no nível do acaso. ", nl_a),
		sprintf("As estimativas são precisas: os intervalos de 95%% descartam efeitos maiores que %s %s por desvio-padrão de ganho, diante de uma taxa de entrada no emprego formal de %s%% em 12 meses. ", num(teto, 2), pp_(teto), num(m_ent, 1)),
		if (!is.na(nl_e)) sprintf("A comparação com quem vai ganhar a mesma acessibilidade mais tarde (desenho E) chega ao mesmo resultado: %s em %d combinações de indicador e faixa de ganho, contra cerca de %s que o acaso produziria. ",
			if (nl_e == 0) "nenhum efeito positivo com placebo nulo" else if (nl_e == 1) "só 1 efeito positivo com placebo nulo" else paste(nl_e, "efeitos positivos com placebo nulo"), nrow(a), num(esp_e, 0)) else "",
		"Ter mais empregos ao alcance do transporte público a partir de casa não aumentou, em até dois anos, a chance de as pessoas de baixa renda do CadÚnico terem emprego formal.</p>"))
	## B: manter o emprego
	if (!is.null(PAR)) {
		pp <- par_principal(PAR)
		segs <- intersect(c("todos", "mulheres", "homens"), unique(pp$segmento))
		y <- pareia(pp[segmento %in% segs & outcome %in% c("demissao_6", "sem_justa_6", "pedido_6")], c("segmento", "outcome"))
		y <- merge(y, unique(pp[janela == "pos", .(segmento, outcome, media = media_y)], by = c("segmento", "outcome")), by = c("segmento", "outcome"), all.x = TRUE)
		g <- function(s, o) y[segmento == s & outcome == o]
		s0 <- segs[1]
		frs <- vapply(segs, function(s) {
			r <- g(s, "demissao_6")
			if (!nrow(r)) return(NA_character_)
			sprintf("em %s %s%s", ef(r$b, r$p), para_g(s), if (!is.na(r$media) && r$p < 0.05) sprintf(", o equivalente a cerca de %s%% da taxa de demissão %s", num(100 * abs(r$b) / r$media, 0), de_g(s)) else "")
		}, character(1))
		frs <- frs[!is.na(frs)]
		## cada item já tem vírgulas internas: separa com "; " e liga o último com "e"
		junta <- function(v) if (length(v) <= 1) v else if (length(v) == 2) paste0(v[1], ", e ", v[2]) else paste0(paste(v[-length(v)], collapse = "; "), "; e ", v[length(v)])
		sj <- g(s0, "sem_justa_6")
		pd <- g(s0, "pedido_6")
		b2 <- unique(PAR[nivel == "h39" & fe == "casa7trab7" & modelo == "B" & segmento == s0 & outcome == "demissao_6" & janela == "pos" & termo %in% c("trajeto, até 90", "acesso, até 90")], by = "termo")
		corpo <- c(corpo, paste0("<p><b>2. Manter o emprego: trajetos mais curtos reduziram as demissões.</b> No desenho principal de tempo de viagem (B), cada 10 minutos a menos no trajeto casa → trabalho, em trajetos de até 90 minutos, muda a chance de ser demitido em até 6 meses ",
			junta(frs), ". ",
			if (nrow(sj) && nrow(pd)) sprintf("A queda vem da demissão sem justa causa, que muda em %s %s; o pedido de demissão não muda de forma significativa (%s). ", ef(sj$b, sj$p), para_g(s0), efv(pd$b, pd$p)) else "",
			if (nrow(b2) == 2) sprintf("%s, quando o ganho de acessibilidade a outros empregos entra na mesma regressão, o efeito do trajeto se mantém (%s) e o da acessibilidade não é significativo (%s): o que importa é o próprio trajeto.",
				maiuscula(para_g(s0)), efv(b2[termo == "trajeto, até 90", efeito], b2[termo == "trajeto, até 90", p_perm]), efv(b2[termo == "acesso, até 90", efeito], b2[termo == "acesso, até 90", p_perm])) else "", "</p>"))
	}
	## E e D: comparações independentes
	if (!is.null(FP)) {
		x <- unique(FP[nivel == "h39" & trajeto == "até 90" & faixa == "todas" & segmento %in% c("todos", "mulheres", "homens") & outcome == "demissao_6"], by = c("segmento", "janela"))
		ge <- function(s, j = "pos") x[segmento == s & janela == j]
		## IPTW com o corte: as estimações finais; enquanto não saem, o diagnóstico (mesma especificação)
		di <- NULL
		fonte_d <- ""
		if (!is.null(IP) && nrow(IP[nivel == "ind" & trajeto == "até 90" & faixa == "f10mais" & outcome == "demissao_6"])) {
			di <- unique(IP[nivel == "ind" & trajeto == "até 90" & faixa == "f10mais" & outcome == "demissao_6"], by = c("segmento", "janela"))[, .(segmento, janela, b = att, p)]
		} else if (file.exists(file.path(dir, "iptw_diag_par_efeitos_versoes.csv"))) {
			e <- fread(file.path(dir, "iptw_diag_par_efeitos_versoes.csv"))
			di <- e[versao == "V1 pesos cortados no p99" & faixa == "f10mais" & outcome == "demissao_6", .(segmento, janela, b = att, p)]
			fonte_d <- " Esses números do IPTW vêm do diagnóstico, com a mesma especificação, enquanto a estimação final com o corte roda."
		}
		gd <- function(s, j = "pos") if (is.null(di)) data.table() else di[segmento == s & janela == j]
		pl_ok <- all(c(ge("todos", "pre")$p, ge("mulheres", "pre")$p, gd("mulheres", "pre")$p) >= 0.10, na.rm = TRUE)
		dmh <- if (!is.null(FPC)) FPC[nivel == "h39" & trajeto == "até 90" & faixa == "todas" & contraste == "mulheres − homens" & outcome == "demissao_6" & janela == "pos"] else data.table()
		corpo <- c(corpo, paste0("<p><b>3. A queda nas demissões aparece também com comparações diferentes.</b> ",
			sprintf("Comparando quem teve o trajeto encurtado com quem terá um ganho da mesma faixa mais tarde (desenho E), a chance de ser demitido em até 6 meses muda em %s para todas as pessoas, %s para as mulheres e %s para os homens. ",
				ef(ge("todos")$efeito, ge("todos")$p), ef(ge("mulheres")$efeito, ge("mulheres")$p), ef(ge("homens")$efeito, ge("homens")$p)),
			if (!is.null(di)) sprintf("Comparando com controles reponderados para se parecerem com os tratados (IPTW, desenho D, ganho de 10 minutos ou mais, pesos com corte), a mudança é de %s para todas as pessoas, %s para as mulheres e %s para os homens.%s ",
				ef(gd("todos")$b, gd("todos")$p), ef(gd("mulheres")$b, gd("mulheres")$p), ef(gd("homens")$b, gd("homens")$p), fonte_d) else "",
			if (pl_ok) "Os placebos dessas comparações são nulos. " else "Algum placebo dessas comparações não é nulo, o que pede cautela. ",
			"Como cada desenho depende de uma hipótese diferente (o acaso do calendário das aberturas no desenho B, tendências paralelas com os futuros tratados no desenho E, igualdade nas características medidas no IPTW), a concordância entre eles torna o resultado mais confiável.</p>"))
		## mulheres e homens: o texto segue o que cada desenho mostra
		cai <- function(r, bcol = "b") nrow(r) > 0 && !is.na(r$p[1]) && r$p[1] < 0.05 && r[[bcol]][1] < 0
		bh <- if (!is.null(PAR)) list(m = cai(g("mulheres", "demissao_6")), h = cai(g("homens", "demissao_6"))) else list(m = NA, h = NA)
		ee_ <- list(m = cai(ge("mulheres"), "efeito"), h = cai(ge("homens"), "efeito"))
		dd_ <- list(m = cai(gd("mulheres")), h = cai(gd("homens")))
		quem <- function(l) if (isTRUE(l$m) && isTRUE(l$h)) "para mulheres e para homens" else if (isTRUE(l$m)) "só para as mulheres" else if (isTRUE(l$h)) "só para os homens" else "para nenhum dos dois"
		dbh <- if (!is.null(PCT)) PCT[contraste == "mulheres − homens" & outcome == "demissao_6" & janela == "pos"] else data.table()
		mulheres_todos <- isTRUE(bh$m) && isTRUE(ee_$m) && (is.null(di) || isTRUE(dd_$m))
		homens_so_bh <- isTRUE(bh$h) && !isTRUE(ee_$h) && !isTRUE(dd_$h)
		## agrupa os desenhos que dizem a mesma coisa: "para mulheres e para homens no desenho B, mas só para as mulheres no desenho E e no IPTW"
		qd <- data.table(q = c(quem(bh), quem(ee_), if (!is.null(di)) quem(dd_)), d = c("no desenho B", "no desenho E", if (!is.null(di)) "no IPTW (desenho D)"))
		partes <- qd[, .(t = paste(q[1], lista_e(d))), by = q]$t
		corpo <- c(corpo, paste0("<p><b>4. Mulheres e homens.</b> ",
			sprintf("A queda significativa nas demissões aparece %s. ", if (length(partes) == 2) paste(partes, collapse = ", mas ") else lista_e(partes)),
			if (nrow(dmh)) sprintf("A diferença formal entre mulheres e homens no desenho E é de %s. ", ef(dmh$diferenca[1], dmh$p[1])) else "",
			if (nrow(dbh)) sprintf("No desenho B, é de %s. ", ef(dbh$diferenca[1], dbh$p[1])) else "A diferença formal no desenho B ainda está sendo recalculada. ",
			if (mulheres_todos && homens_so_bh) "Por enquanto, o resultado mais seguro é que as mulheres têm uma queda nas demissões que aparece em todos os desenhos, e os homens, só no principal." else
				if (mulheres_todos) "A queda nas demissões das mulheres aparece em todos os desenhos." else "", "</p>"))
	}
	## C: mecanismo
	if (!is.null(PAR) && nrow(PAR[grepl("^ped_", outcome)])) {
		pb <- unique(PAR[nivel == "h39" & fe == "casa7trab7" & modelo == "B" & termo == "trajeto, até 90" & janela == "pos" & outcome %in% c("pedido_12", "ped_curto_12", "ped_salmaior_12")], by = c("segmento", "outcome"))
		s1 <- intersect(c("todos", "mulheres", "homens"), unique(pb$segmento))[1]
		x <- pb[segmento == s1]
		gc <- function(o) x[outcome == o]
		pc <- c(gc("pedido_12")$p_perm, gc("ped_curto_12")$p_perm, gc("ped_salmaior_12")$p_perm)
		nulos <- length(pc) == 3 && all(!is.na(pc) & pc >= 0.05)
		numeros <- sprintf("o pedido de demissão em até 12 meses muda em %s, a saída para um emprego com trajeto mais curto em %s e a saída para um emprego com salário maior em %s",
			ef(gc("pedido_12")$efeito, gc("pedido_12")$p_perm), ef(gc("ped_curto_12")$efeito, gc("ped_curto_12")$p_perm), ef(gc("ped_salmaior_12")$efeito, gc("ped_salmaior_12")$p_perm))
		corpo <- c(corpo, if (nulos) sprintf("<p><b>5. Por que menos demissões?</b> %s, o trajeto mais curto não levou a trocar de emprego (desenho C): %s, todos estatisticamente indistinguíveis de zero. Junto com a queda concentrada nas demissões sem justa causa, isso aponta para um canal pela relação de trabalho: menos atrasos, faltas e desgaste, e por isso menos dispensas, e não mais mobilidade voluntária entre empregos.</p>",
			maiuscula(para_g(s1)), numeros) else
			sprintf("<p><b>5. Por que menos demissões?</b> %s, no desenho C, %s. Parte da queda nas demissões pode, portanto, vir de pessoas que trocaram de emprego por conta própria.</p>", maiuscula(para_g(s1)), numeros))
	}
	## limites
	corpo <- c(corpo, "<p><b>6. Limites.</b></p><ul><li>Os resultados valem para pessoas de baixa renda inscritas no CadÚnico e para o emprego formal (RAIS). Trabalho informal, desemprego e saída da força de trabalho não são observados separadamente.</li><li>O horizonte é de até dois anos depois de cada abertura; efeitos de mais longo prazo, ou sobre quem se mudou, ficam fora.</li><li>O recorte principal são trajetos de até 90 minutos. Os de mais de 90 minutos e a versão com todos os trajetos juntos estão nos desenhos, e parte deles ainda está em estimação.</li><li>Algumas estimações seguem rodando (todas as pessoas no desenho B, todos os trajetos juntos, IPTW com o corte dos pesos e as versões em H3-8). Esta conclusão se atualiza com elas.</li></ul>")
	## em uma frase (só quando os números sustentam as duas partes)
	area_nula <- !is.na(nl_a) && nl_a <= 1
	demite_menos <- isTRUE(bh$m) || isTRUE(bh$h)
	if (area_nula && demite_menos) corpo <- c(corpo, paste0("<p><b>Em uma frase.</b> As novas estações de metrô, trem e monotrilho não ajudaram as pessoas de baixa renda a conseguir emprego formal, mas ajudaram quem já estava empregado a mantê-lo: trajetos mais curtos reduziram as demissões",
		if (nrow(sj) && !is.na(sj$p[1]) && sj$p[1] < 0.05) ", sobretudo as sem justa causa" else "",
		if (mulheres_todos) " e, de forma mais robusta, entre as mulheres" else "", ".</p>"))
	html_conclusao(corpo)
}

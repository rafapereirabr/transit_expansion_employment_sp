# EXPLORATÓRIO (17/11/2026, plano de desenhos, passo R/90): datas de tratamento e insumos comuns aos desenhos.
# - Níveis da família A com a data de abertura (operação assistida) e a de serviço pleno (tabela de estações do Arthur;
#   sem casamento, vale a abertura), a coorte anual (primeiro ano com pelo menos 6 meses de serviço pleno: serviço pleno
#   até 30/06) e o ano de abertura. A transição vai do ano de abertura até a coorte − 1.
# - Sorteios D1 para o B&H com as datas de serviço pleno: as 26 datas permutadas entre os níveis, mantendo a ordem
#   dentro de cada linha (semente 20261117). A coluna s0 é a realizada.
# - Ganhos de acessibilidade por nível e hexágono, em empregos (atalho aditivo: rede com o nível − rede sem ele), nos
#   cortes de 30, 45, 60 e 90 min, mais o acesso na rede completa (A00).
# - Metadados dos hexágonos: coordenadas UTM, célula de 5 km, H3-7 e H3-8, distância à Sé, Censo 2010 (por H3-8) e
#   distância à estação aberta mais próxima em 31/12 de cada ano de 2010 a 2025.
#
# Uso: Rscript R/90_datas_insumos.R [cortes=30,45,60,90] [sorteios=999]
# Saídas (data/base/datas/): niveis.csv, sorteios_pleno.parquet, ganhos_c<corte>.parquet, hex_meta.parquet

source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
options(width = 200)
S <- as.integer(arg_cli("sorteios", NA, 999L))
cortes <- as.integer(strsplit(arg_cli("cortes", NA, "30,45,60,90"), ",")[[1]])
dir_out <- file.path(dir_base, "datas")
dir.create(dir_out, showWarnings = FALSE)

## 1. níveis e datas ---------------------------------------------------------------------------------------------------
cat_a <- fread("data/redes/catalogo_familia_a.csv")[familia %in% c("A", "A'")]
niv <- rbind(
	cat_a[familia == "A" & acao == "retira", .(nivel = rede, com = base, sem = rede, linha = code_line, abertura = as.IDate(data_abertura), estacoes)],
	cat_a[familia == "A'", .(nivel = rede, com = rede, sem = "A00", linha = code_line, abertura = as.IDate(data_abertura), estacoes)]
)
norm <- function(x) trimws(toupper(iconv(x, to = "ASCII//TRANSLIT")))
est <- as.data.table(arrow::read_parquet(file.path(dir_base, "estacoes.parquet")))
e2 <- est[, .(code_line, nome = norm(name_station), pleno = as.IDate(dt_servico_pleno))]
niv[, pleno := as.IDate(NA)]
for (k in seq_len(nrow(niv))) {
	nomes <- norm(strsplit(niv$estacoes[k], ";")[[1]])
	e <- e2[code_line == niv$linha[k] & nome %in% nomes & !is.na(pleno)]
	if (nrow(e)) set(niv, k, "pleno", max(e$pleno))
}
niv[, casou_pleno := !is.na(pleno)]
niv[is.na(pleno) | pleno < abertura, pleno := abertura]
niv[, `:=`(coorte = year(pleno) + as.integer(month(pleno) > 6L), ano_abertura = year(abertura))]
fwrite(niv, file.path(dir_out, "niveis.csv"))
print(niv[order(pleno), .(nivel, linha, abertura, pleno, coorte, ano_abertura, casou_pleno)])

## 2. sorteios com as datas de serviço pleno ---------------------------------------------------------------------------
set.seed(20261117L)
dint <- as.integer(niv$pleno)
ordem <- lapply(split(seq_len(nrow(niv)), niv$linha), function(ix) ix[order(dint[ix])])
sortear <- function() {
	d <- sample(dint)
	for (ix in ordem) d[ix] <- sort(d[ix])
	d
}
D <- cbind(dint, replicate(S, sortear()))
colnames(D) <- paste0("s", 0:S)
arrow::write_parquet(as.data.table(D), file.path(dir_out, "sorteios_pleno.parquet"))
msg("sorteios: ", S)

## 3. ganhos por nível -------------------------------------------------------------------------------------------------
le_acesso <- function(r, cc) {
	f <- if (cc == 30L) file.path(dir_base, "acesso_sem_se_30", sprintf("acesso_%s.parquet", r)) else file.path(dir_base, "acesso_sem_se", sprintf("acesso_%s.parquet", r))
	x <- as.data.table(arrow::read_parquet(f))
	x <- x[x$corte == cc, .(id, acc)]
	setnames(x, "acc", r)
	x
}
redes <- sort(unique(c(niv$com, niv$sem)))
ids_ref <- NULL
for (cc in cortes) {
	if (cc == 30L && length(list.files(file.path(dir_base, "acesso_sem_se_30"), pattern = "parquet$")) < 27L) {
		msg("corte 30: acessibilidade ainda incompleta, pulado")
		next
	}
	V <- Reduce(function(x, y) merge(x, y, by = "id"), lapply(redes, function(r) le_acesso(r, cc)))
	G <- as.data.table(sapply(seq_len(nrow(niv)), function(k) V[[niv$com[k]]] - V[[niv$sem[k]]]))
	setnames(G, niv$nivel)
	G[, `:=`(id = V$id, acc_A00 = V$A00)]
	setcolorder(G, c("id", "acc_A00"))
	arrow::write_parquet(G, file.path(dir_out, sprintf("ganhos_c%d.parquet", cc)))
	if (is.null(ids_ref)) ids_ref <- G$id
	msg("corte ", cc, ": ", nrow(G), " hexágonos; ganho total mediano por nível ", round(median(colSums(as.matrix(G[, niv$nivel, with = FALSE])))))
}

## 4. metadados dos hexágonos ------------------------------------------------------------------------------------------
if (!is.null(ids_ref)) {
	xy <- sf::st_coordinates(h3jsr::cell_to_point(ids_ref, simple = FALSE))
	utm <- sf::sf_project("EPSG:4326", sprintf("EPSG:%d", crs_utm), xy)
	hx <- data.table(id = ids_ref, x = utm[, 1], y = utm[, 2])
	hx[, cel5 := paste(floor(x / 5000), floor(y / 5000))]
	hx[, `:=`(h3_7 = h3jsr::get_parent(id, res = 7), h3_8 = h3jsr::get_parent(id, res = 8))]
	se <- est[norm(name_station) == "SE"][1L]
	hx[, dist_se_km := sqrt((x - se$x_utm)^2 + (y - se$y_utm)^2) / 1000]
	arq_censo <- file.path(dir_base, "censo_h3_8.parquet")
	if (!file.exists(arq_censo)) {
		stop("falta ", arq_censo, " (Censo 2010 por H3-8; insumo acompanhado pelo pipeline, ver notas/plano_targets_2026-11-24.md)")
	}
	cz <- as.data.table(arrow::read_parquet(arq_censo))
	hx <- merge(hx, cz, by.x = "h3_8", by.y = "h8", all.x = TRUE)
	## distância à estação aberta mais próxima em 31/12 de cada ano (abertura da operação, qualquer linha)
	est[, ab := fcoalesce(as.IDate(dt_abertura), as.IDate(sprintf("%d-07-01", ano_abertura)))]
	for (t in 2010:2025) {
		e <- est[!is.na(ab) & ab <= as.IDate(sprintf("%d-12-31", t)) & !is.na(x_utm)]
		d <- rep(Inf, nrow(hx))
		for (i in seq_len(nrow(e))) d <- pmin(d, (hx$x - e$x_utm[i])^2 + (hx$y - e$y_utm[i])^2)
		set(hx, j = paste0("dist_est_", t), value = sqrt(d) / 1000)
	}
	setcolorder(hx, c("id", "x", "y", "cel5", "h3_7", "h3_8"))
	arrow::write_parquet(hx, file.path(dir_out, "hex_meta.parquet"))
	msg("hexágonos: ", nrow(hx), "; sem Censo ", sum(is.na(hx$renda_pc)), "; distância à Sé mediana ", round(median(hx$dist_se_km), 1), " km")
}
msg("fim")

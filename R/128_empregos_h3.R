# Empregos formais por hexágono H3-9 (24/11/2026): vínculos ativos em 31/12 do ano, pelo hexágono do estabelecimento
# (RAIS geocodificada). Mesmo código do R/38 (que gravou empregos_h3_2015.parquet), com o ano como argumento.
# Decisão de 24/11/2026: a acessibilidade passa a usar os empregos de 2012 (último ano completo antes da primeira
# abertura, set/2013; mesmo ano da calibração do decaimento), fixos para todos os eventos.
# Uso: Rscript R/128_empregos_h3.R [ano=2012]
# Saída: data/base/empregos_h3_<ano>.parquet (h3_09, empregos), agregado; não vai para o git.
source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(data.table))
ano <- as.integer(arg_cli("ano", NA, 2012L))
arq <- file.path(dir_base, sprintf("empregos_h3_%d.parquet", ano))
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
if (file.exists(arq)) stop("já existe: ", arq)
rv <- ipeadatalake::ler_rais(ano, "vinc", c("id_estab", "cei_vinc", "emp_31dez"), verboso = FALSE) |>
	dplyr::filter(emp_31dez == 1) |>
	dplyr::count(id_estab, cei_vinc, name = "empregos") |>
	dplyr::compute()
geo <- arrow::open_dataset(file.path(dir_rais_geocode, sprintf("rais_geolocalizada_%d.parquet", ano))) |>
	dplyr::transmute(id_estab = arrow::cast(id_estab, arrow::utf8()), cei_vinc = arrow::cast(cei_vinc, arrow::utf8()), h3_09) |>
	dplyr::distinct(id_estab, cei_vinc, .keep_all = TRUE)
emp <- rv |>
	dplyr::inner_join(geo, by = c("id_estab", "cei_vinc")) |>
	dplyr::group_by(h3_09) |>
	dplyr::summarise(empregos = sum(empregos)) |>
	dplyr::collect() |>
	as.data.table()
arrow::write_parquet(emp, arq)
msg("empregos ", ano, ": ", format(sum(emp$empregos), big.mark = " "), " em ", nrow(emp), " hexágonos")
## comparação agregada com 2015, se existir
f15 <- file.path(dir_base, "empregos_h3_2015.parquet")
if (file.exists(f15) && ano != 2015L) {
	e15 <- as.data.table(arrow::read_parquet(f15))
	m <- merge(emp, e15, by = "h3_09", all = TRUE, suffixes = c("_a", "_15"))
	m[is.na(empregos_a), empregos_a := 0][is.na(empregos_15), empregos_15 := 0]
	msg("2015: ", format(sum(e15$empregos), big.mark = " "), "; correlação entre hexágonos ", round(cor(m$empregos_a, m$empregos_15), 3),
		"; hexágonos só em ", ano, ": ", m[empregos_15 == 0 & empregos_a > 0, .N], "; só em 2015: ", m[empregos_a == 0 & empregos_15 > 0, .N])
}
msg("fim")

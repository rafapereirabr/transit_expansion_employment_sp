# Famílias com criança de 0 a 5 anos (nascida depois de 1º/jan de b−6 e até 1º/jan de b) no CadÚnico bruto harmonizado
# de dezembro do ano b (todos os membros, inclusive crianças sem CPF), 2011–2022. Lê só cd_ibge_cadastro, co_familiar_fam
# e dt_nasc_pessoa; guarda só os códigos de família do estado de SP em data/base/explora/criancas (dado restrito, local).
# Retomável: anos já gravados são pulados. A pasta do CadÚnico bruto vem de R/00_config_local.R (dir_cadunico_bruto).
# Uso: Rscript R/117_criancas_cadunico.R
source("R/00_ambiente_r5.R")
suppressPackageStartupMessages({ library(arrow); library(dplyr) })
msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
dd <- "data/base/explora/criancas"
dir.create(dd, showWarnings = FALSE, recursive = TRUE)
for (b in 2011:2022) {
	out <- file.path(dd, sprintf("fam_crianca_%d.rds", b))
	if (file.exists(out)) next
	f <- file.path(dir_cadunico_bruto, sprintf("cadunico_pes_%d12_harmonizado.parquet", b))
	lo <- as.Date(sprintf("%d-01-01", b - 6L))
	hi <- as.Date(sprintf("%d-01-01", b))
	ds <- open_dataset(f) |> select(cd_ibge_cadastro, co_familiar_fam, dt_nasc_pessoa)
	if (b == 2011L) print(ds |> summarise(mn = min(cd_ibge_cadastro, na.rm = TRUE), mx = max(cd_ibge_cadastro, na.rm = TRUE)) |> collect())
	sp <- ds |> filter(cd_ibge_cadastro >= 3500000, cd_ibge_cadastro < 3600000)
	n_pes <- sp |> summarise(n = n(), kid = sum(dt_nasc_pessoa > lo & dt_nasc_pessoa <= hi, na.rm = TRUE)) |> collect()
	fam <- sp |> filter(dt_nasc_pessoa > lo, dt_nasc_pessoa <= hi) |> mutate(fam = as.numeric(co_familiar_fam)) |> distinct(fam) |> collect()
	saveRDS(fam$fam, out)
	msg("ano ", b, ": pessoas em SP ", n_pes$n, ", crianças 0–5 ", n_pes$kid, " (", round(100 * n_pes$kid / n_pes$n, 1), "%); famílias com criança ", nrow(fam))
	rm(ds, sp, fam)
	gc(verbose = FALSE)
}
msg("fim")

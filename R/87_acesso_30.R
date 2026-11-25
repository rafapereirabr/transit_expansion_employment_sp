# EXPLORATÓRIO (17/11/2026, plano de desenhos, passo R/87): acessibilidade a empregos a até 30 min, sem as duas sedes da
# Sé, nas 27 redes da família A, com a mesma conta do R/66 (que fez 45, 60 e 90 min).
#
# Uso: Rscript R/87_acesso_30.R   (R 4.6)
# Saídas: data/base/acesso_sem_se_30/acesso_Axx.parquet (id, corte = 30, acc)

source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
emp <- as.data.table(arrow::read_parquet(file.path(dir_base, "empregos_h3_2015.parquet")))
se <- fread("data/base/acesso_cortes/hex_se.csv")$to_id
stopifnot(length(se) == 2L, all(se %in% emp$h3_09))
emp <- emp[!h3_09 %in% se]
dir_out <- file.path(dir_base, "acesso_sem_se_30")
dir.create(dir_out, showWarnings = FALSE)
for (r in sprintf("A%02d", 0:26)) {
	out <- file.path(dir_out, sprintf("acesso_%s.parquet", r))
	if (file.exists(out)) next
	m <- as.data.table(arrow::read_parquet(file.path("data/redes/familia_a", sprintf("ttm_%s.parquet", r)), col_select = c("from_id", "to_id", "travel_time_p50")))
	origens <- unique(m$from_id)
	a <- m[travel_time_p50 <= 30L][emp, on = .(to_id = h3_09), nomatch = 0L][, .(acc = sum(empregos)), by = .(id = from_id)]
	a <- a[data.table(id = origens), on = "id"]
	a[is.na(acc), acc := 0]
	a[, corte := 30L]
	arrow::write_parquet(a[, .(id, corte, acc)], out)
	msg(r, ": ", nrow(a), " origens; acesso mediano ", round(median(a$acc)))
	rm(m, a)
	gc()
}
msg("fim")

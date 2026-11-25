# EXPLORATÓRIO (16/11/2026, pedido do Gregório): acessibilidade a empregos SEM os dois hexágonos-sede da Sé (os dois
# maiores em empregos, a 0,1-0,2 km da Sé; data/base/acesso_cortes/hex_se.csv), a 45, 60 e 90 min, para todas as redes da
# família A (A00-A26), a partir das matrizes do R/42. Mesma conta do R/43 (empregos de 2015 alcançados em até o corte),
# só sem as duas sedes. Usada pelo Borusyak e Hull com a dose nova (R/57 dir_acesso=data/base/acesso_sem_se corte=60).
#
# Uso: Rscript R/66_acesso_sem_se.R   (R 4.6)
# Saídas: data/base/acesso_sem_se/acesso_Axx.parquet (id, corte, acc)

source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(2L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
emp <- as.data.table(arrow::read_parquet(file.path(dir_base, "empregos_h3_2015.parquet")))
se <- fread("data/base/acesso_cortes/hex_se.csv")$to_id
stopifnot(length(se) == 2L, all(se %in% emp$h3_09))
msg("empregos sem as sedes: ", sum(emp[!h3_09 %in% se, empregos]), " de ", sum(emp$empregos))
emp <- emp[!h3_09 %in% se]
cortes <- c(45L, 60L, 90L)
dir_out <- file.path(dir_base, "acesso_sem_se")
dir.create(dir_out, showWarnings = FALSE)
for (r in sprintf("A%02d", 0:26)) {
	out <- file.path(dir_out, sprintf("acesso_%s.parquet", r))
	if (file.exists(out)) next
	m <- as.data.table(arrow::read_parquet(file.path("data/redes/familia_a", sprintf("ttm_%s.parquet", r)), col_select = c("from_id", "to_id", "travel_time_p50")))
	origens <- unique(m$from_id)
	m <- m[emp, on = .(to_id = h3_09), nomatch = 0L]
	a <- rbindlist(lapply(cortes, function(cc) m[travel_time_p50 <= cc, .(acc = sum(empregos)), by = .(id = from_id)][, corte := cc]))
	a <- merge(CJ(id = origens, corte = cortes), a, by = c("id", "corte"), all.x = TRUE)
	a[is.na(acc), acc := 0]
	arrow::write_parquet(a, out)
	msg(r, ": ", uniqueN(a$id), " origens")
	rm(m, a)
	gc()
}
msg("fim")

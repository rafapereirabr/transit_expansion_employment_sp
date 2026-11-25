# Ganhos de acessibilidade gravitacional calibrada por nível e hexágono (24/11/2026, organização do pipeline).
# Mesma conta do trecho da dose do R/113 (acesso=completo decaimento=calibrado), separada da estimação anual daquele
# script para que a especificação final (data exata de serviço pleno) não dependa de nenhuma etapa de coortes anuais:
# - A_i = Σ_j empregos_j · exp(−β t_ij), somada até 240 min nas 27 redes da família A (empregos por minuto do R/112);
# - β calibrado = 1 / tempo médio casa → trabalho dos inscritos empregados em 31/12/2012 (rede de 2012 até 240 min, R/111);
# - ganho de cada nível = rede com o nível − rede sem ele (atalho aditivo), no formato de le_ganhos (corte "gcal").
# Uso: Rscript R/116_ganhos_gravitacional.R [empregos=2012] [saida=data/base/datas/ganhos_cgcal.parquet]
# (24/11/2026: empregos de 2012 por padrão; lê os empregos por minuto de data/base/acesso_240_emp<ano>, ou acesso_240 se 2015)
# Saída: data/base/datas/ganhos_cgcal.parquet (id, acc_A00 e uma coluna por nível)
source("R/00_ambiente_r5.R")
source("R/00_config.R")
source("R/funcoes_desenhos.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
ano_emp <- as.integer(arg_cli("empregos", NA, 2012L))
dir_jmin <- file.path(dir_base, if (ano_emp == 2015L) "acesso_240" else sprintf("acesso_240_emp%d", ano_emp))
saida <- arg_cli("saida", NA, file.path(dir_datas, "ganhos_cgcal.parquet"))
niv <- le_niveis()
ids <- le_ganhos(60L)$id
redes <- sort(unique(c(niv$com, niv$sem)))
beta <- fread("outputs/ahlfeldt_decaimento_240.csv")$beta_exponencial[1]

## tempo médio casa → trabalho dos empregados de 31/12/2012 (como no R/107), rede de 2012, até 240 min (R/111)
dir_art <- file.path(dir_arthur, "temp")
x12 <- junta_pa(pop_coorte(1L, 2013L, 18L, ids), 2012L, c("emp31", "estab31"))
w12 <- as.data.table(arrow::read_parquet(file.path(dir_art, "rais_workplaces_2012.parquet"), col_select = c("id_estab", "cei_vinc", "work_h3_id", "work_desvio", "work_geo_valid")))
w12 <- w12[work_geo_valid %in% TRUE & !is.na(work_desvio) & work_desvio <= 500 & !is.na(work_h3_id)]
w12 <- w12[order(!(is.na(cei_vinc) | cei_vinc %in% c("", "0")))][!duplicated(id_estab)]
fl <- x12[emp31_2012 == 1L, .(casa = h3_09, trab = w12$work_h3_id[match(estab31_2012, w12$id_estab)])][!is.na(trab), .N, by = .(casa, trab)]
tp <- as.data.table(arrow::read_parquet(file.path(dir_base, "acesso_grav", "t_pares2012_A23_240.parquet")))
fl[tp, t := i.t, on = .(casa, trab)]
tm <- fl[!is.na(t), weighted.mean(t, N)]
msg("tempo médio casa → trabalho dos empregados de 2012 (rede de 2012, até 240 min): ", round(tm, 1), " min (", fl[!is.na(t), sum(N)], " trabalhadores); β estimado = ",
	round(beta, 4), "; β calibrado = 1 / tempo médio = ", round(1 / tm, 4))
beta <- 1 / tm
rm(x12, w12, fl, tp)

V <- Reduce(function(a, b) merge(a, b, by = "id"), lapply(redes, function(r) {
	jr <- as.data.table(arrow::read_parquet(file.path(dir_jmin, sprintf("jmin_%s.parquet", r))))
	z <- jr[, .(A = sum(empregos * exp(-beta * t))), by = .(id = from_id)][data.table(id = ids), on = "id"]
	z[is.na(A), A := 0]
	setnames(z, "A", r)
	z
}))
V <- V[match(ids, V$id)]
stopifnot(identical(V$id, ids))
Gg <- sapply(seq_len(nrow(niv)), function(k) V[[niv$com[k]]] - V[[niv$sem[k]]])
gc_out <- data.table(id = ids, acc_A00 = V$A00, Gg)
setnames(gc_out, c("id", "acc_A00", niv$nivel))
arrow::write_parquet(gc_out, saida)
msg("ganhos por nível (empregos de ", ano_emp, ") gravados em ", saida, "; acessibilidade mediana na rede de 2012 = ", round(median(V$A23)))

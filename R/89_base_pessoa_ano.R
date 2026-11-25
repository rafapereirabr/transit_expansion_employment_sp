# EXPLORATÓRIO (17/11/2026, plano de desenhos, passo R/89): bases comuns aos desenhos D1 a D3 e ao T3.
#
# etapa=outcomes: por CPF elegível e ano (2011–2025), dos nossos rais_vinc, só vínculos celetistas (regra do Arthur:
#   tipos 10, 15, 20, 25, 50, 55, 60, 65, 70, 75 e 90):
#   - emp31: vínculo ativo em 31/12 (O1);
#   - meses e emp_mask: meses com vínculo ativo no último dia do mês, e a máscara de 12 bits desses meses (O6, painel
#     mensal). Emprego no fim do mês m: admitido até o mês m e desligado depois do mês m;
#   - adm_n e adm_mask: admissões no ano e máscara dos meses com admissão;
#   - sep_mask: meses com desligamento, sem transferências (causas 30 a 34);
#   - dem_n (causas 10 e 11, pelo empregador), ped_n (21, pedido), fim_n (12, fim de contrato), acordo_n (90);
#   - estab31 e lsalh: estabelecimento e log do salário-hora (remuneração de dezembro / (horas contratuais × 4,348)) do
#     vínculo ativo em 31/12 de maior remuneração de dezembro (O4 e O7).
#   Desligamentos sem mês (cerca de 1% em 2019–2020) entram nas contagens do ano, mas não nas máscaras.
#   Saída: data/base/pessoa_ano/pa_<ano>.parquet (restrito).
# etapa=populacoes: por ano-base b, as pessoas do cad_individ_b do Arthur (só leitura) com as regras dele: município de
#   São Paulo e regra de endereço (se algum registro do mesmo id_addr_cad tem desvio ausente ou acima de 500 m, ou
#   cadastro sem atualização há mais de 2 anos, todo o ponto sai). A idade é filtrada depois, por coorte.
#   Saída: data/base/pop_base/pop_<b>.parquet (restrito).
#
# Uso: Rscript R/89_base_pessoa_ano.R etapa=outcomes [anos=2011,...,2025]
#      Rscript R/89_base_pessoa_ano.R etapa=populacoes [anos=2011,...,2021]

source("R/00_ambiente_r5.R")
source("R/00_config.R")
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " ", ...)
etapa <- arg_cli("etapa", NA, "outcomes")
eleg <- c(10L, 15L, 20L, 25L, 50L, 55L, 60L, 65L, 70L, 75L, 90L)
dir_art <- file.path(dir_arthur, "temp")

if (etapa == "outcomes") {
	anos <- as.integer(strsplit(arg_cli("anos", NA, paste(2011:2025, collapse = ",")), ",")[[1]])
	dir_out <- file.path(dir_base, "pessoa_ano")
	dir.create(dir_out, showWarnings = FALSE)
	for (a in anos) {
		v <- as.data.table(arrow::read_parquet(file.path(dir_rais, sprintf("rais_vinc_%d.parquet", a)),
			col_select = c("cpf_num", "id_estab", "tipo_vinculo", "emp_31dez", "data_adm", "data_deslig", "mes_deslig", "causa_deslig", "horas_contr", "rem_dez_r")))
		v <- v[as.integer(tipo_vinculo) %in% eleg]
		v[, `:=`(causa = as.integer(causa_deslig), ativo = emp_31dez %in% 1L)]
		v[, m_adm := fifelse(is.na(data_adm) | year(data_adm) > a, NA_integer_, fifelse(year(data_adm) < a, 0L, month(data_adm)))]
		v[, m_sep := fifelse(ativo, 13L, fifelse(!is.na(data_deslig) & year(data_deslig) == a, month(data_deslig),
			fifelse(as.integer(mes_deslig) %in% 1:12, as.integer(mes_deslig), NA_integer_)))]
		v[, `:=`(lo = pmax(m_adm, 1L), hi = pmin(m_sep - 1L, 12L))]
		ok_e <- !is.na(v$lo) & !is.na(v$hi)
		transf <- v$causa %in% 30:34
		for (m in 1:12) {
			set(v, j = paste0("e", m), value = as.integer(ok_e & v$lo <= m & v$hi >= m))
			set(v, j = paste0("a", m), value = as.integer(v$m_adm %in% m))
			set(v, j = paste0("s", m), value = as.integer(!v$ativo & v$m_sep %in% m & !transf))
		}
		v[, `:=`(i_ativo = as.integer(ativo), i_adm = as.integer(m_adm %in% 1:12), i_dem = as.integer(!ativo & causa %in% c(10L, 11L)),
			i_ped = as.integer(!ativo & causa %in% 21L), i_fim = as.integer(!ativo & causa %in% 12L), i_acordo = as.integer(!ativo & causa %in% 90L))]
		cm <- c(paste0("e", 1:12), paste0("a", 1:12), paste0("s", 1:12), "i_ativo")
		p <- v[, lapply(.SD, max), by = cpf_num, .SDcols = cm]
		q <- v[, lapply(.SD, sum), by = cpf_num, .SDcols = c("i_adm", "i_dem", "i_ped", "i_fim", "i_acordo")]
		p <- p[q, on = "cpf_num"]
		pot <- 2L^(0:11)
		p[, `:=`(emp_mask = as.integer(as.matrix(.SD[, paste0("e", 1:12), with = FALSE]) %*% pot),
			adm_mask = as.integer(as.matrix(.SD[, paste0("a", 1:12), with = FALSE]) %*% pot),
			sep_mask = as.integer(as.matrix(.SD[, paste0("s", 1:12), with = FALSE]) %*% pot),
			meses = as.integer(rowSums(.SD[, paste0("e", 1:12), with = FALSE])))]
		## vínculo principal em 31/12
		pr <- v[ativo == TRUE][order(-fcoalesce(as.numeric(rem_dez_r), 0))][!duplicated(cpf_num)]
		pr[, lsalh := fifelse(as.numeric(rem_dez_r) > 0 & as.numeric(horas_contr) > 0, log(as.numeric(rem_dez_r) / (as.numeric(horas_contr) * 4.348)), NA_real_)]
		p[pr, `:=`(estab31 = as.character(i.id_estab), lsalh = i.lsalh), on = "cpf_num"]
		out <- p[, .(cpf_num, ano = a, emp31 = i_ativo, meses, emp_mask, adm_mask, sep_mask, adm_n = i_adm, dem_n = i_dem, ped_n = i_ped,
			fim_n = i_fim, acordo_n = i_acordo, estab31, lsalh)]
		## conferência: emp31 igual ao emprego celetista do R/85
		cel <- as.data.table(arrow::read_parquet(file.path(dir_base, "celetista_pessoa_ano.parquet")))[year == a]
		msg(a, ": ", nrow(out), " pessoas com vínculo; emp31 = ", sum(out$emp31), " (R/85: ", nrow(cel), "); meses médios ", round(mean(out$meses), 2),
			"; admissões ", sum(out$adm_n), "; sem mês no desligamento ", sum(!v$ativo & is.na(v$m_sep)))
		arrow::write_parquet(out, file.path(dir_out, sprintf("pa_%d.parquet", a)))
		rm(v, p, q, pr, out, cel)
		gc()
	}
}

if (etapa == "populacoes") {
	anos <- as.integer(strsplit(arg_cli("anos", NA, paste(2011:2021, collapse = ",")), ",")[[1]])
	dir_out <- file.path(dir_base, "pop_base")
	dir.create(dir_out, showWarnings = FALSE)
	for (b in anos) {
		x <- as.data.table(arrow::read_parquet(file.path(dir_art, sprintf("cad_individ_%d.parquet", b)),
			col_select = c("cpf", "birthdate_cad", "gender_cad", "race_cad", "disability_cad", "literate_cad", "years_update", "code_muni_cad", "h3_09_cad",
				"id_addr_cad", "desvio_metros")))
		n0 <- nrow(x)
		x[, cpf_num := as.numeric(cpf)]
		x[, cpf := NULL]
		x <- x[code_muni_cad == muni_estudo]
		ruim <- x[is.na(desvio_metros) | desvio_metros > 500 | is.na(years_update) | years_update > 2, unique(id_addr_cad)]
		x <- x[!is.na(id_addr_cad) & !id_addr_cad %in% ruim & !is.na(h3_09_cad) & !is.na(birthdate_cad) & !is.na(cpf_num)]
		x <- unique(x, by = "cpf_num")
		x <- x[, .(cpf_num, h3_09 = h3_09_cad, nascimento = as.IDate(birthdate_cad), sexo = as.character(gender_cad), raca = as.character(race_cad),
			deficiencia = as.character(disability_cad), alfabetizado = as.character(literate_cad), anos_atualizacao = as.numeric(years_update))]
		arrow::write_parquet(x, file.path(dir_out, sprintf("pop_%d.parquet", b)))
		msg("base ", b, ": ", n0, " registros; ", nrow(x), " pessoas após município e endereço")
		rm(x)
		gc()
	}
}
msg("fim")

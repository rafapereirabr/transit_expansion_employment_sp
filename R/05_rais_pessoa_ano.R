# Outcomes RAIS que não dependem da coorte, uma linha por CPF do roster × ano (2011-2023).
#
# Regras (plano de 19/10 com as correções da resposta de 22/10):
# - formal_31dec = 1 se há ao menos um vínculo com emp_31dez == 1 no ano; 0 se o CPF (válido, do
#   roster) não tem vínculo ativo em 31/12. Todos os anos da RAIS estão completos, então não há NA.
#   Decidido antes de qualquer filtro de salário, horas ou geocódigo.
# - remove só duplicatas exatas de vínculo.
# - salário-hora contratual: soma dos salários mensais / ((52/12) × soma das horas semanais) dos
#   vínculos ativos em 31/12; NA se algum vínculo ativo não tem salário, tipo de salário ou horas,
#   ou é intermitente (horas = 1 por convenção). Em reais de dezembro de 2023 (IPCA de dezembro).
# - também guarda variantes: sem vínculos intermitentes, qualquer vínculo no ano, remuneração de
#   dezembro.

source("R/00_config.R")
source("R/funcoes.R")

## IPCA número-índice de dezembro (IBGE/SIDRA, tabela 1737, variável 2266)
ipca_path <- file.path(dir_saida, "ipca_dezembro.csv")
if (!file.exists(ipca_path)) {
	per <- paste0(anos_outcome, "12", collapse = ",")
	ip <- sidrar::get_sidra(api = paste0("/t/1737/n1/all/v/2266/p/", per))
	ipca <- data.frame(ano = as.integer(substr(ip[["Mês (Código)"]], 1, 4)), ipca_dez = ip$Valor)
	write.csv(ipca, ipca_path, row.names = FALSE)
}
ipca <- read.csv(ipca_path)
stopifnot(setequal(ipca$ano, anos_outcome))
ipca$fator_dez2023 <- ipca$ipca_dez[ipca$ano == 2023] / ipca$ipca_dez

chave <- arrow::read_parquet(file.path(dir_base, "roster_cadunico.parquet")) # todos os CPFs do CadÚnico
dir_pa <- file.path(dir_base, "rais_pessoa_ano")
dir_geo <- file.path(dir_base, "rais_ativos_31dez")
log_path <- file.path(dir_saida, "log_rais_pessoa_ano.csv")
if (file.exists(log_path)) file.remove(log_path)

for (a in anos_outcome) {
	fator <- ipca$fator_dez2023[ipca$ano == a]
	## duplicatas exatas já removidas no 04; identificadores de estabelecimento ficam de fora
	## (milhões de textos únicos são lentos no R e não são usados aqui)
	x <- arrow::read_parquet(file.path(dir_rais, sprintf("rais_vinc_%d.parquet", a)), as_data_frame = FALSE) |>
		select(-any_of(c("id_estab", "cei_vinc"))) |>
		collect()
	for (v in c("ind_trab_intermitente", "ind_trab_parcial")) {
		if (!v %in% names(x)) x[[v]] <- NA_integer_ # informação só existe a partir de 2017
	}
	n0 <- nrow(x)

	## validade do salário contratual por vínculo: vínculo com salário-hora fora de [0,5; 100] × o
	## salário-hora mínimo do ano (salário mínimo de dezembro / ((52/12) × 44)) é tratado como não
	## classificável.
	## Correção de 16/11/2026 (auditoria): de 2016 a 2021, cerca de um terço dos vínculos ativos, por
	## estabelecimento, tem o salário contratual dividido por 100 (casa decimal deslocada). Nesses
	## vínculos a remuneração média do ano (rem_med_r) é cerca de 100 vezes o salário convertido para
	## mês (mediana 100-103, contra 1,0 nos demais), e o salário × 100 cai no salário mínimo do ano.
	## Vínculo fora da faixa com rem_med_r / salário mensal entre 50 e 200 tem o salário multiplicado
	## por 100; a faixa é testada de novo depois da correção.
	sm_hora <- salario_minimo_dez[[as.character(a)]] / ((52 / 12) * 44)
	fora <- function(h, ok) !is.na(h) & ok & (h < 0.5 * sm_hora | h > 100 * sm_hora)
	ativos <- x |>
		filter(emp_31dez == 1) |>
		mutate(
			intermitente = coalesce(as.integer(ind_trab_intermitente) == 1L, FALSE),
			sal_mensal = salario_mensal(salario, tipo_salario, horas_contr),
			horas_ok = !is.na(horas_contr) & horas_contr > 0 & !intermitente,
			razao_rem = rem_med_r / sal_mensal,
			sal_x100 = fora(sal_mensal / ((52 / 12) * horas_contr), horas_ok) & !is.na(razao_rem) & razao_rem >= 50 & razao_rem <= 200,
			sal_mensal = if_else(sal_x100, sal_mensal * 100, sal_mensal),
			hora_link = sal_mensal / ((52 / 12) * horas_contr),
			sal_fora_faixa = fora(hora_link, horas_ok),
			sal_mensal = if_else(sal_fora_faixa, NA_real_, sal_mensal)
		)

	pa <- ativos |>
		group_by(cpf_num) |>
		summarise(
			n_vinc_31dez = n(),
			n_vinc_31dez_nao_interm = sum(!intermitente),
			horas_semanais = sum(horas_contr),
			sal_mensal_nominal = sum(sal_mensal),
			salario_classificavel = all(!is.na(sal_mensal) & horas_ok),
			rem_dez_nominal = sum(rem_dez_r),
			.groups = "drop"
		) |>
		mutate(
			hourly_wage = if_else(
				salario_classificavel,
				sal_mensal_nominal / ((52 / 12) * horas_semanais) * fator,
				NA_real_
			),
			rem_dez_real = rem_dez_nominal * fator
		)

	algum <- x |> distinct(cpf_num) |> mutate(formal_algum_vinculo_ano = 1L)

	out <- chave |>
		select(cpf_num) |>
		left_join(pa, by = "cpf_num", relationship = "one-to-one") |>
		left_join(algum, by = "cpf_num", relationship = "one-to-one") |>
		transmute(
			cpf_num,
			year = as.integer(a),
			formal_31dec = as.integer(!is.na(n_vinc_31dez)),
			formal_31dec_sem_interm = as.integer(coalesce(n_vinc_31dez_nao_interm, 0L) > 0),
			formal_algum_vinculo_ano = coalesce(formal_algum_vinculo_ano, 0L),
			n_vinc_31dez = coalesce(n_vinc_31dez, 0L),
			horas_semanais,
			hourly_wage,
			salario_classificavel,
			rem_dez_real
		)
	stopifnot(nrow(out) == nrow(chave))

	dir.create(file.path(dir_pa, paste0("year=", a)), recursive = TRUE, showWarnings = FALSE)
	arrow::write_parquet(select(out, -year), file.path(dir_pa, paste0("year=", a), "part-0.parquet"))

	## vínculos ativos em 31/12 com local de trabalho, para a distância (depende da coorte)
	geo <- ativos |>
		transmute(cpf_num, horas_contr, intermitente, work_lat, work_lon, work_precisao, work_desvio_m)
	dir.create(file.path(dir_geo, paste0("year=", a)), recursive = TRUE, showWarnings = FALSE)
	arrow::write_parquet(geo, file.path(dir_geo, paste0("year=", a), "part-0.parquet"))

	lg <- data.frame(
		ano = a,
		vinculos = n0,
		vinculos_ativos_31dez = nrow(ativos),
		ativos_intermitentes = sum(ativos$intermitente),
		ativos_tipo_salario_na = sum(is.na(ativos$tipo_salario)),
		ativos_salario_x100 = sum(ativos$sal_x100),
		ativos_salario_fora_faixa = sum(ativos$sal_fora_faixa),
		ativos_sem_geocodigo = sum(is.na(ativos$work_lat)),
		pessoas_roster = nrow(out),
		pessoas_formal_31dec = sum(out$formal_31dec),
		pessoas_salario_hora_na_entre_empregados = sum(out$formal_31dec == 1 & is.na(out$hourly_wage)),
		mediana_salario_hora_real = median(out$hourly_wage, na.rm = TRUE)
	)
	write.table(lg, log_path, sep = ",", row.names = FALSE, append = file.exists(log_path), col.names = !file.exists(log_path))
	message(a, ": formal_31dec = ", lg$pessoas_formal_31dec, " de ", lg$pessoas_roster)
}

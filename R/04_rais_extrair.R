# Extrai da RAIS (vínculos, 2011-2023, todo o Brasil) os registros dos CPFs presentes em algum
# risk set, e anexa o geocódigo do estabelecimento do próprio ano. Retomável: anos já gravados
# são pulados. Nenhum filtro de vínculo é aplicado aqui (a regra de emprego vem no 05).

source("R/00_config.R")

## roster: todos os CPFs dos arquivos do CadÚnico 2011-2023. Cobre os risk sets de qualquer
## defasagem e o desenho por células. Construído no Arrow (sem criar textos no R).
roster <- arrow::open_dataset(file.path(dir_insumos, sprintf("cad_individ_%d.parquet", 2011:2023))) |>
	distinct(cpf) |>
	compute()
message("CPFs no roster: ", nrow(roster))
arrow::write_parquet(
	roster |> transmute(cpf_num = as.numeric(cpf)) |> compute(),
	file.path(dir_base, "roster_cadunico.parquet")
)

## se o roster mudou desde a última extração, todos os anos são refeitos
tag_path <- file.path(dir_rais, "_roster_n.txt")
refazer <- !file.exists(tag_path) || readLines(tag_path) != as.character(nrow(roster))
writeLines(as.character(nrow(roster)), tag_path)

cols <- c(
	"cpf", "tipo_estab", "id_estab", "cei_vinc", "codemun", "tipo_vinculo", "emp_31dez", "data_adm",
	"data_deslig", "mes_deslig", "causa_deslig", "horas_contr", "tipo_salario", "salario", "rem_dez_r",
	"rem_med_r", "cbo2002", "clas_cnae20", "grau_instr"
)
cols_opc <- c("ind_trab_intermitente", "ind_trab_parcial") # só a partir de 2017

log_path <- file.path(dir_saida, "log_extracao_rais.csv")
for (a in anos_outcome) {
	out <- file.path(dir_rais, sprintf("rais_vinc_%d.parquet", a))
	if (file.exists(out) && !refazer) {
		message(a, ": já extraído, pulando.")
		next
	}
	t0 <- Sys.time()
	rv <- ipeadatalake::ler_rais(a, "vinc", verboso = FALSE)
	n_total <- nrow(rv)
	cl <- c(cols, intersect(cols_opc, names(rv)))
	## colunas ausentes em anos antigos (intermitente/parcial) são criadas como NA no 05
	x <- rv |>
		select(all_of(cl)) |>
		semi_join(roster, by = "cpf") |>
		compute()
	## só duplicatas exatas (todas as colunas extraídas iguais) são removidas
	n_antes_dedup <- nrow(x)
	x <- x |>
		distinct() |>
		compute()
	n_dup <- n_antes_dedup - nrow(x)

	## geocódigo do estabelecimento (mesmo ano); chave id_estab + cei_vinc
	chaves <- x |>
		distinct(id_estab, cei_vinc) |>
		compute()
	geo <- arrow::open_dataset(
		file.path(dir_rais_geocode, sprintf("rais_geolocalizada_%d.parquet", a))
	) |>
		transmute(
			id_estab = cast(id_estab, arrow::utf8()),
			cei_vinc = cast(cei_vinc, arrow::utf8()),
			work_lat = as.numeric(lat),
			work_lon = as.numeric(lon),
			work_precisao = precisao,
			work_desvio_m = as.numeric(desvio_metros)
		) |>
		semi_join(chaves, by = c("id_estab", "cei_vinc")) |>
		compute()
	n_geo <- nrow(geo)
	n_geo_chaves <- geo |> distinct(id_estab, cei_vinc) |> compute() |> nrow()
	if (n_geo_chaves < n_geo) {
		## chave duplicada no geocódigo: mantém uma linha por chave (a de menor desvio)
		geo <- geo |>
			arrange(id_estab, cei_vinc, work_desvio_m) |>
			collect() |>
			distinct(id_estab, cei_vinc, .keep_all = TRUE) |>
			arrow::arrow_table()
	}
	x <- x |>
		left_join(geo, by = c("id_estab", "cei_vinc")) |>
		mutate(ano = as.integer(a), cpf_num = as.numeric(cpf)) |>
		select(-cpf) |>
		compute()

	arrow::write_parquet(x, out)
	lg <- data.frame(
		ano = a,
		n_vinculos_rais = n_total,
		n_vinculos_roster = nrow(x),
		n_duplicatas_exatas = n_dup,
		n_cpf_roster_com_vinculo = x |> distinct(cpf_num) |> compute() |> nrow(),
		n_chaves_geo_duplicadas = n_geo - n_geo_chaves,
		minutos = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
	)
	write.table(lg, log_path, sep = ",", row.names = FALSE, append = file.exists(log_path), col.names = !file.exists(log_path))
	message(a, ": ", lg$n_vinculos_roster, " vínculos de ", lg$n_cpf_roster_com_vinculo, " CPFs (", lg$minutos, " min).")
	rm(rv, x, geo)
	gc()
}

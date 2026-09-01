#' read_rais

# tar_load(pilot_years)
# tar_load(pilot_individuals)
# year <- pilot_years[1]
# individuals <- pilot_individuals[1]

read_rais <- function(year, individuals, save_dir = NULL) {
	if (inherits(individuals, "character")) {
		individuals <- arrow::open_dataset(individuals)
	} else if (!inherits(individuals, "ArrowTabular")) {
		individuals <- arrow::as_arrow_table(individuals)
	}

	cols_rais <- c(
		"id_estab",
		"cei_vinc",
		"codemun",
		"cpf",
		"grau_instr",
		"tipo_vinculo",
		"emp_31dez",
		"data_adm",
		"data_deslig",
		"temp_empr",
		"horas_contr",
		"salario",
		"rem_med_r"
	)

	rais <- ipeadatalake::ler_rais(year, "vinc", cols_rais)

	cpfs <- individuals |>
		pull(cpf_recuperado)

	rais <- rais |>
		filter(cpf %in% cpfs & !is.na(id_estab))

	## dealing with multiple entries: we'll drop exact duplicates & keep only active jobs @ eoy
	rais <- rais |>
		distinct() |>
		filter(emp_31dez == 1) |>
		compute()

	## add firm location
	rais <- get_work_location(rais, 2012)

	cad_rais <- left_join(individuals, rais, by = c("cpf_recuperado" = "cpf")) |>
		compute()

	## if more than one per year, keep only the last entry? not now, maybe downstream.
	# cad_rais |>
	#   count(year, cpf_recuperado, emp_31dez, sort = T) |>
	#     filter(n > 1) |> collect()

	## save
	file_name <- paste0("rais_vinc_", year, ".parquet")
	file_path <- file.path(save_dir, file_name)
	write_parquet_target(
		cad_rais,
		file_path,
		overwrite = T
	)

	return(file_path)
}


get_work_location <- function(rais_vinc, year) {
	if (inherits(rais_vinc, "character")) {
		rais_vinc <- arrow::open_dataset(rais_vinc)
	} else if (!inherits(rais_vinc, "ArrowTabular")) {
		rais_vinc <- arrow::as_arrow_table(rais_vinc)
	}

	rais_estab <- rais_vinc |>
		distinct(id_estab, cei_vinc) |>
		ipeadatalake::adicionar_geoloc("rais", year)

	rais_estab <- rais_estab |>
		transmute(
			id_estab,
			cei_vinc,
			work_lat = lat,
			work_lon = lon,
			work_desvio = desvio_metros,
			work_id = h3_09
		)

	rais_geo <- left_join(rais_vinc, rais_estab) |>
		compute()

	return(rais_geo)
}

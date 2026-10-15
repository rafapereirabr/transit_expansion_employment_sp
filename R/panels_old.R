# tar_load(cadunico_rais_pilot)
# cadunico_rais <- cadunico_rais_pilot
# tar_load(ttm_bypass)
# ttm <- ttm_bypass
# tar_load(pilot_years)
# years <- pilot_years

make_panel <- function(cadunico_rais, ttm = NULL, balance = TRUE, years = NULL, save_dir = NULL) {
	if (inherits(cadunico_rais, "character")) {
		cadunico_rais <- arrow::open_dataset(cadunico_rais)
	} else if (!inherits(cadunico_rais, "ArrowTabular")) {
		cadunico_rais <- arrow::as_arrow_table(cadunico_rais)
	}

	## remove poorly geocoded units
	bad_home_addr <- cadunico_rais |>
		filter(desvio_metros > 1200) |>
		pull(cpf_recuperado)
	bad_work_addr <- cadunico_rais |>
		filter(work_desvio > 1200) |>
		pull(cpf_recuperado)

	cadunico_rais <- cadunico_rais |>
		filter(!(cpf_recuperado %in% bad_home_addr)) |>
		filter(!(cpf_recuperado %in% bad_work_addr)) |>
		compute()

	panel <- cadunico_rais |>
		mutate(
			woman = if_else(co_sexo_pessoa == 1, 0, 1),
			gender_na = if_else(is.na(co_sexo_pessoa), 1, 0),
			white_asian = if_else(co_raca_cor_pessoa == 1 | co_raca_cor_pessoa == 3, 1, 0),
			race_na = if_else(is.na(co_raca_cor_pessoa), 1, 0),
		) |>
		mutate(
			woman = if_else(gender_na == 1, NA, woman),
			white_asian = if_else(race_na == 1, NA, white_asian),
			formal = if_else(is.na(id_estab), 0, 1)
		)

	panel <- panel |>
		rename(h3_home = id, h3_work = work_id, h3_ref = id_ref) |>
		select(
			year,
			cpf_recuperado,
			woman,
			white_asian,
			age,
			h3_home,
			h3_ref,
			pilot_group,
			pilot_group_fct,
			ring_order,
			pilot_treated,
			pilot_post,
			formal,
			id_estab,
			cei_vinc,
			h3_work,
			yr_open,
			emp_31dez,
			horas_contr,
			salario,
			rem_med_r
		) |>
		compute()

	## join ttm
	if (inherits(ttm, "character")) {
		ttm <- arrow::open_dataset(ttm)
	} else if (!inherits(ttm, "ArrowTabular")) {
		ttm <- arrow::as_arrow_table(ttm)
	}

	if (!is.null(ttm)) {
		home_h3 <- distinct(panel, h3_home) |> pull(h3_home)
		work_h3 <- distinct(panel, h3_work) |> pull(h3_work)
		ttm <- ttm |>
			filter(from_id %in% home_h3 & to_id %in% work_h3) |>
			compute()
	}

	panel <- left_join(
		panel,
		ttm,
		by = c("h3_home" = "from_id", "h3_work" = "to_id", "year" = "year")
	) |>
		compute()

	if (balance) {
		panel <- balance_panel(panel, years)
		file_name <- paste0("panel_pilot_balanced.parquet")
	} else {
		file_name <- paste0("panel_pilot.parquet")
	}

	cpf_mask <- distinct(panel, cpf_recuperado) |>
		collect() |>
		tibble::rowid_to_column("cpf_mask")

	estab_mask <- distinct(panel, id_estab, cei_vinc) |>
		collect() |>
		tibble::rowid_to_column("estab_mask")

	panel <- left_join(panel, cpf_mask) |>
		left_join(estab_mask) |>
		select(-c(cpf_recuperado, id_estab, cei_vinc))

	## save
	file_path <- file.path(save_dir, file_name)
	write_parquet_target(panel, file_path, overwrite = TRUE)
	return(file_path)
}


# balance_panel ------------------------------------------------------------------------------

balance_panel <- function(panel, years, two_by_two = TRUE) {
	stopifnot(!is.null(years))

	if (inherits(panel, "character")) {
		panel <- arrow::open_dataset(panel)
	} else if (!inherits(panel, "ArrowTabular")) {
		panel <- arrow::as_arrow_table(panel)
	}

	if (two_by_two) {
		years <- range(years)

		panel <- panel |>
			filter(year %in% years) |>
			compute()
	}

	compliers <- panel |>
		distinct(cpf_recuperado, year) |>
		count(cpf_recuperado) |>
		filter(n == length(years)) |>
		pull(cpf_recuperado)

	multiple_jobs <- panel |>
		filter(formal > 0) |>
		count(cpf_recuperado, year) |>
		filter(n > 1) |>
		pull(cpf_recuperado)

	panel_balanced <- panel |>
		filter(cpf_recuperado %in% compliers) |>
		filter(!(cpf_recuperado %in% multiple_jobs)) |>
		compute()

	n <- panel_balanced |> distinct(cpf_recuperado) |> compute() |> nrow()
	nT <- n * length(years)
	stopifnot(nT == nrow(panel_balanced))

	return(panel_balanced)
}

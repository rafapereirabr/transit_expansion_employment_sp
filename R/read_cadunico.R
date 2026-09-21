# read_cad_fams -------------------------------------------------------------------------------

# source("R/utils.R")
# tar_load(c(munis_sf, time_window))
# munis <- munis_sf
# time_window <- c(2011, 2015, 2019, 2024)
# years <- time_window

read_cad_families <- function(years, munis, save_dir = NULL) {
	cad_dates <- 100 * years + 12

	if (inherits(munis, "character")) {
		munis <- sf_from_parquet(munis, drop_geometry = T)
	}

	all_fams <- purrr::map2(
		cad_dates,
		years,
		function(x, y) {
			ipeadatalake::ler_cadunico(
				x,
				"familia",
				c("dt_atualizacao_fam", "cd_ibge_cadastro", "co_familiar_fam"),
				verboso = F
			) |>
				mutate(year = y, .before = everything()) |>
				compute()
		}
	)

	unique_fams <- purrr::reduce(all_fams, arrow::concat_tables) |>
		filter(cd_ibge_cadastro %in% munis$code_muni) |>
		distinct(co_familiar_fam) |>
		pull(as_vector = T)

	fams <- all_fams |>
		purrr::map(\(x) filter(x, co_familiar_fam %in% unique_fams) |> compute())

	fams <- purrr::map2(
		fams,
		cad_dates,
		function(x, y) {
			ipeadatalake::adicionar_geoloc(x, "cadunico", y)
		}
	)

	fams <- purrr::reduce(fams, arrow::concat_tables)

	gc()

	fams <- fams |>
		distinct(
			year,
			dt_atualizacao_fam,
			cd_ibge_cadastro,
			co_familiar_fam,
			lat,
			lon,
			desvio_metros,
			h3_09
		) |>
		compute()

	write_parquet_target(fams, file.path(save_dir, "cad_fams.parquet"), T)
}

# read_cad_individs ---------------------------------------------------------------------------

# tar_load(c(time_window, cadunico_fam))
# # tar_load(c(pilot_years, pilot_families))
# save_dir <- "data/temp"
# families <- cadunico_fam
# year <- time_window[1]
# # families <- pilot_families
# # year <- pilot_years[1]

read_cad_individuals <- function(year, families, save_dir = NULL) {
	if (inherits(families, "character")) {
		families <- arrow::open_dataset(families)
	} else if (!inherits(families, "ArrowTabular")) {
		families <- arrow::as_arrow_table(families)
	}

	families <- families |>
		filter(year == !!year) |>
		compute()

	individ <- ipeadatalake::ler_cadunico(100 * year + 12, "pessoa")

	cols_ind <- c(
		"co_familiar_fam",
		"cpf",
		"cpf_imputado",
		"cpf_recuperado",
		"dt_atual_memb",
		"dt_nasc_pessoa",
		"co_sexo_pessoa",
		"co_raca_cor_pessoa",
		"co_deficiencia_memb",
		"co_sabe_ler_escrever_memb",
		"co_principal_trab_memb",
		"co_trabalho_12_meses_memb",
		"co_curso_frequenta_memb",
		"in_dormir_rua_memb",
		"in_dormir_albergue_memb",
		"in_dormir_dom_part_memb",
		"in_dormir_outro_memb"
	)

	individ <- individ |>
		select(any_of(cols_ind)) |>
		rename(any_of(c(cpf = "cpf_imputado", cpf = "cpf_recuperado")))

	## remove duplicates --- needs improvement
	duplicate_fam <- families |>
		count(co_familiar_fam) |>
		filter(n > 1) |>
		pull(co_familiar_fam)
	stopifnot(length(duplicate_fam) == 0)
	duplicate_ind <- individ |>
		count(cpf) |>
		filter(n > 1) |>
		pull(cpf)
	matched_ind <- individ |>
		filter(!(cpf %in% duplicate_ind | is.na(cpf))) |>
		inner_join(families)

	## remove minors and outdated data
	end_of_year <- paste(year, 12, 31, sep = "-") |>
		as.Date()
	matched_ind <- matched_ind |>
		mutate(
			age = (as.integer(end_of_year) - as.integer(dt_nasc_pessoa)) / 365.25,
			# years_since_update = (as.integer(end_of_year) - as.integer(dt_atual_memb)) / 365.25
		) |>
		filter(age >= 18) |> # & years_since_update <= 2.083) |>
		compute()

	# tidy data
	matched_ind <- matched_ind |>
		rename(
			id_family = co_familiar_fam,
			code_muni_cad = cd_ibge_cadastro,
			birthdate_cad = dt_nasc_pessoa,
			gender_cad = co_sexo_pessoa,
			race_cad = co_raca_cor_pessoa,
			disability_cad = co_deficiencia_memb,
			literate_cad = co_sabe_ler_escrever_memb,
			main_job_status_cad = co_principal_trab_memb,
			h3_09_cad = h3_09,
			employed_12mo_cad = co_trabalho_12_meses_memb
		) |>
		mutate(
			# student = if_else(is.na(co_curso_frequenta_memb), 0, 1),
			unhoused = in_dormir_rua_memb +
				in_dormir_albergue_memb +
				in_dormir_dom_part_memb +
				in_dormir_outro_memb,
			last_update = dt_atualizacao_fam,
			years_update = (as.integer(end_of_year) - as.integer(dt_atualizacao_fam)) / 365.25,
			.keep = "unused"
		) |>
		relocate(year, cpf, .before = everything()) |>
		relocate(code_muni_cad, h3_09_cad, lat, lon, desvio_metros, .after = everything()) |>
		compute()

	## check if all duplicates are gone
	n_duplicates <- matched_ind |>
		count(id_family, cpf) |>
		filter(n > 1) |>
		compute() |>
		nrow()
	stopifnot(n_duplicates == 0)

	## save
	file_name <- paste0("cad_individ_", year, ".parquet")
	file_path <- file.path(save_dir, file_name)
	write_parquet_target(
		matched_ind,
		file_path,
		overwrite = T
	)

	return(file_path)
}

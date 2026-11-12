# read_cad_fams -------------------------------------------------------------------------------

# source("R/utils.R")
# tar_load(c(munis_sf, time_window))
# munis <- munis_sf
# # time_window <- c(2011:2015, 2019, 2024)
# years <- time_window[1:5]

read_cad_families <- function(years, munis, save_dir = NULL) {
	cad_dates <- 100 * years + 12

	if (inherits(munis, "character")) {
		munis <- sf_from_parquet(munis, drop_geometry = T)
	}

	## find all families in all years
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

	# keep only those that at some point live in the study area
	unique_fams <- purrr::reduce(all_fams, concat_tables) |>
		filter(cd_ibge_cadastro %in% munis$code_muni) |>
		distinct(co_familiar_fam) |>
		pull(as_vector = T)

	fams <- all_fams |>
		purrr::map(\(x) filter(x, co_familiar_fam %in% unique_fams) |> compute())

	## now read everything for selected families
	fams <- purrr::map2(
		fams,
		cad_dates,
		function(x, y) {
			ipeadatalake::adicionar_geoloc(x, "cadunico", y)
		}
	)

	fams <- purrr::reduce(fams, concat_tables)

	gc()

	## remove duplicate entries
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

	## now we need an address id for ttm purposes (when using addr instead of h3)
	addr_roster <- fams |>
		distinct(lat, lon) |>
		collect() |>
		tibble::rowid_to_column("id_addr_cad")
	sch <- schema(id_addr_cad = int64(), lat = decimal128(8, 6), lon = decimal128(8, 6))
	addr_roster <- as_arrow_table(addr_roster, schema = sch)
	fams <- left_join(fams, addr_roster) |>
		compute()

	write_parquet_target(fams, file.path(save_dir, "cad_fams.parquet"), T)
}

# read_cad_individs ---------------------------------------------------------------------------

## some cols are not available throughout all the period; this guarantees we'll only keep those that are available.
select_available_cols <- function(col_string, years) {
	nm <- purrr::map(
		years,
		~ ipeadatalake::ler_cadunico(.x * 100 + 12, "pessoa", harmonizado = T, verboso = F) |> names()
	)

	cols <- purrr::map2(
		years,
		nm,
		function(x, y) {
			tibble(year = x, name = col_string, present = col_string %in% y)
		}
	) |>
		bind_rows()

	not_always <- cols |>
		filter(!present)
	# not_always |>
	#   tidyr::pivot_wider(names_from = name, values_from = present)

	cols_remove <- not_always |> distinct(name) |> pull()
	new_string <- setdiff(col_string, cols_remove)
	return(new_string)
}

check_cpf <- function(cpf) {
	if (length(cpf) == 0L) {
		return(logical())
	}

	formatted <- !is.na(cpf) & grepl('^[0-9]{11}$', cpf)
	safe_cpf <- ifelse(formatted, cpf, '00000000000')
	digits <- do.call(rbind, strsplit(safe_cpf, '', fixed = TRUE))
	digits <- matrix(as.integer(digits), ncol = 11L)
	check_1 <- (rowSums(sweep(digits[, 1:9, drop = FALSE], 2L, 10:2, FUN = '*')) * 10) %% 11
	check_1[check_1 == 10L] <- 0L
	check_2 <- (rowSums(sweep(digits[, 1:10, drop = FALSE], 2L, 11:2, FUN = '*')) * 10) %% 11
	check_2[check_2 == 10L] <- 0L
	repeated <- apply(digits, 1L, function(x) length(unique(x)) == 1L)

	formatted & !repeated & digits[, 10L] == check_1 & digits[, 11L] == check_2
}

flag_person_records <- function(data, duplicate_cpf, min_age = 18) {
	data <- as.data.frame(data)
	data$drop_reason <- dplyr::case_when(
		is.na(data$cpf) ~ 'missing_cpf',
		data$cpf %in% duplicate_cpf ~ 'duplicate_cpf',
		!check_cpf(data$cpf) ~ 'invalid_cpf',
		is.na(data$age) ~ 'missing_birthdate',
		data$age < min_age ~ 'under_18',
		.default = 'retained'
	)
	data
}

# tar_load(c(time_window, family_records))
# # tar_load(c(pilot_years, pilot_families))
# save_dir <- "data/temp"
# families <- family_records
# year <- time_window[1]
# # families <- pilot_families
# # year <- pilot_years[1]

read_cad_individuals <- function(year, families, cols_individ, save_dir = NULL) {
	if (inherits(families, "character")) {
		families <- arrow::open_dataset(families)
	} else if (!inherits(families, "ArrowTabular")) {
		families <- as_arrow_table(families)
	}

	families <- families |>
		filter(year == !!year) |>
		compute()

	individ <- ipeadatalake::ler_cadunico(
		100 * year + 12,
		"pessoa",
		harmonizado = TRUE
	)

	individ <- individ |>
		select(all_of(cols_individ)) |>
		rename(cpf = cpf_recuperado)

	## remove duplicates --- zero policy for now (very few cases, not worth the trouble)
	duplicate_fam <- families |>
		count(co_familiar_fam) |>
		filter(n > 1) |>
		pull(co_familiar_fam)
	stopifnot(length(duplicate_fam) == 0)
	duplicate_ind <- individ |>
		filter(!is.na(cpf)) |>
		count(cpf) |>
		filter(n > 1) |>
		pull(cpf)
	matched_ind <- individ |>
		inner_join(families, by = 'co_familiar_fam')

	## remove minors
	end_of_year <- paste(year, 12, 31, sep = "-") |>
		as.Date()
	matched_ind <- matched_ind |>
		mutate(
			age = (as.integer(end_of_year) - as.integer(dt_nasc_pessoa)) / 365.25,
		) |>
		arrow::map_batches(flag_person_records, duplicate_cpf = duplicate_ind) |>
		compute()
	audit_reasons <- c(
		'missing_cpf',
		'duplicate_cpf',
		'invalid_cpf',
		'missing_birthdate',
		'under_18',
		'retained'
	)
	cpf_audit <- matched_ind |>
		filter(!is.na(cpf)) |>
		distinct(drop_reason, cpf) |>
		count(drop_reason, name = 'n_cpf') |>
		collect()
	person_audit <- matched_ind |>
		count(drop_reason, name = 'n_records') |>
		collect() |>
		left_join(cpf_audit, by = 'drop_reason') |>
		tidyr::complete(
			drop_reason = audit_reasons,
			fill = list(n_records = 0L, n_cpf = 0L)
		) |>
		mutate(
			year = !!year,
			excluded = drop_reason != 'retained',
			step = match(drop_reason, audit_reasons),
			.before = everything()
		) |>
		arrange(step) |>
		rename(reason = drop_reason)
	message(
		'CadUnico ',
		year,
		': retained ',
		person_audit$n_records[person_audit$reason == 'retained'],
		'; excluded ',
		sum(person_audit$n_records[person_audit$excluded]),
		'.'
	)
	matched_ind <- matched_ind |>
		filter(drop_reason == 'retained') |>
		select(-drop_reason) |>
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
			last_updated_cad = dt_atualizacao_fam,
			h3_09_cad = h3_09,
			lat_cad = lat,
			lon_cad = lon #,
			# employed_12mo_cad = co_trabalho_12_meses_memb
		) |>
		mutate(
			year = !!year,
			# unhoused = in_dormir_rua_memb +
			# 	in_dormir_albergue_memb +
			# 	in_dormir_dom_part_memb +
			# 	in_dormir_outro_memb,
			years_update = (as.integer(end_of_year) - as.integer(last_updated_cad)) / 365.25,
			.keep = "unused"
		) |>
		relocate(year, cpf, .before = everything()) |>
		relocate(
			years_update,
			code_muni_cad,
			h3_09_cad,
			id_addr_cad,
			lat_cad,
			lon_cad,
			desvio_metros,
			.after = everything()
		) |>
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
		overwrite = TRUE
	)
	audit_path <- file.path(save_dir, paste0('cad_individ_audit_', year, '.parquet'))
	write_parquet_target(
		person_audit,
		audit_path,
		overwrite = TRUE
	)

	return(c(records = file_path, audit = audit_path))
}

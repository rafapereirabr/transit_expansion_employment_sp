# RAIS extraction inputs and outputs are restricted artifacts.
# Code meanings: MTE, Manual RAIS 2020, sections B.6--B.8 and C.1:
# https://www.gov.br/trabalho-e-emprego/pt-br/servicos/empregador/rais/manualrais2020.pdf
# Preserve unrecognized annual codes as unknown; never classify them as non-employment.
make_rais_spec <- function(
	salary_type_col = NULL,
	monthly_salary_verified = FALSE,
	coverage_confirmed_years = integer()
) {
	return(list(
		eligible_types = c("10", "15", "20", "25", "50", "55", "60", "65", "70", "75", "90"),
		excluded_types = c("30", "31", "35", "40", "80", "95", "96", "97"),
		active_codes = "1",
		inactive_codes = "0",
		salary_type_col = salary_type_col,
		monthly_salary_verified = monthly_salary_verified,
		coverage_confirmed_years = coverage_confirmed_years,
		max_work_geo_dev = 500
	))
}

rais_input <- function(data) {
	if (is.character(data)) return(arrow::open_dataset(data))
	if (inherits(data, "data.frame")) return(arrow::as_arrow_table(data))
	return(data)
}

rais_count <- function(data) {
	result <- data |> dplyr::summarise(n = dplyr::n()) |> dplyr::collect()
	return(as.numeric(result$n[[1]]))
}

rais_require <- function(data, columns) {
	missing <- setdiff(columns, names(data))
	if (length(missing)) stop("Missing RAIS columns: ", paste(missing, collapse = ", "))
	invisible(TRUE)
}

# A union roster is deliberately NOT crossed with years or risk sets here.
# Named records/audit vectors and flattened target branches are accepted;
# only the builder's known risk_set_<year>.parquet record files enter the union.
build_cpf_roster <- function(risk_sets, save_dir) {
	files <- unlist(risk_sets, use.names = FALSE)
	if (!is.character(files)) stop("Supply risk-set record files.")
	files <- unique(files[grepl("^risk_set_[0-9]{4}\\.parquet$", basename(files))])
	if (!length(files)) stop("No risk-set record files found.")
	roster <- arrow::open_dataset(files)
	rais_require(roster, "cpf")
	invalid <- roster |>
		dplyr::filter(is.na(cpf) | nchar(cpf) != 11L) |>
		rais_count()
	if (invalid) stop("Risk-set roster has missing or malformed CPFs.")
	roster <- roster |> dplyr::distinct(cpf)
	path <- file.path(save_dir, "cpf_roster.parquet")
	return(write_parquet_target(roster, path, overwrite = TRUE))
}

# Read once per annual source, using the same union roster in every year.
# Returns active links, restricted CPF-year status, and an aggregate audit.
# A successful read does not certify source coverage. Zero requires a year
# explicitly listed in coverage_confirmed_years after the source-coverage audit.
read_rais <- function(year, individuals, save_dir, spec = make_rais_spec()) {
	stopifnot(
		length(year) == 1L, is.finite(year), year == as.integer(year),
		length(save_dir) == 1L, dir.exists(save_dir),
		!length(intersect(spec$eligible_types, spec$excluded_types)),
		!length(intersect(spec$active_codes, spec$inactive_codes)),
		is.logical(spec$monthly_salary_verified), length(spec$monthly_salary_verified) == 1L,
		!is.na(spec$monthly_salary_verified)
	)
	roster <- rais_input(individuals) |> dplyr::select(cpf) |> dplyr::distinct()
	if (rais_count(dplyr::filter(roster, is.na(cpf) | nchar(cpf) != 11L))) {
		stop("Invalid CPF roster.")
	}
	n_roster <- rais_count(roster)
	if (!n_roster) stop("Cannot extract RAIS for an empty roster.")
	source <- ipeadatalake::ler_rais(year, "vinc")
	rais_require(source, c(
		"cpf", "id_estab", "cei_vinc", "tipo_vinculo", "emp_31dez",
		"salario", "horas_contr"
	))
	# Deduplicate before projecting columns: two different source records must not
	# collapse just because their selected wage/establishment fields coincide.
	matched <- source |> dplyr::semi_join(roster, by = "cpf") |> dplyr::compute()
	n_raw <- rais_count(matched)
	matched <- matched |> dplyr::distinct() |> dplyr::compute()
	n_unique <- rais_count(matched)
	reserved <- c(
		"year", "rais_active", "rais_inactive", "rais_celetista", "rais_type_known",
		"rais_possible_celetista", "rais_status_unknown", "salary_type_rais",
		"salary_monthly_verified", "salary_positive", "hours_positive"
	)
	if (length(intersect(reserved, names(matched)))) {
		stop("Source conflicts with reserved extraction columns.")
	}
	salary_col <- spec$salary_type_col
	if (is.null(salary_col)) {
		candidates <- intersect(c("tipo_salario", "tipo_sal", "tipo_sal_contratual"), names(matched))
		if (length(candidates) > 1L) stop("Set salary_type_col explicitly; multiple candidates exist.")
		salary_col <- if (length(candidates)) candidates[[1]] else NULL
	}
	if (!is.null(salary_col)) {
		rais_require(matched, salary_col)
		matched <- matched |>
			dplyr::mutate(salary_type_rais = as.character(.data[[salary_col]]))
	} else {
		matched <- matched |> dplyr::mutate(salary_type_rais = NA_character_)
	}
	active_codes <- spec$active_codes
	inactive_codes <- spec$inactive_codes
	eligible_types <- spec$eligible_types
	known_types <- c(spec$eligible_types, spec$excluded_types)
	monthly_verified <- spec$monthly_salary_verified
	matched <- matched |>
		dplyr::mutate(
			year = as.integer(!!year),
			rais_active = !is.na(emp_31dez) & as.character(emp_31dez) %in% active_codes,
			rais_inactive = !is.na(emp_31dez) & as.character(emp_31dez) %in% inactive_codes,
			rais_celetista = !is.na(tipo_vinculo) & as.character(tipo_vinculo) %in% eligible_types,
			rais_type_known = !is.na(tipo_vinculo) & as.character(tipo_vinculo) %in% known_types,
			rais_status_unknown = !rais_active & !rais_inactive,
			rais_possible_celetista = !rais_inactive & (!rais_type_known | rais_celetista),
			salary_monthly_verified = monthly_verified | (!is.na(salary_type_rais) & salary_type_rais == "1"),
			salary_positive = !is.na(salario) & is.finite(salario) & salario > 0,
			hours_positive = !is.na(horas_contr) & is.finite(horas_contr) & horas_contr > 0
		) |>
		dplyr::compute()
	# Unknown status/type are stored as person-year flags before inactive links are discarded.
	by_person <- matched |>
		dplyr::group_by(cpf) |>
		dplyr::summarise(
			n_source_links = dplyr::n(),
			n_active_celetista = sum(as.integer(rais_active & rais_celetista)),
			n_active_other = sum(as.integer(rais_active & rais_type_known & !rais_celetista)),
			n_uncertain_links = sum(as.integer(rais_possible_celetista & !(rais_active & rais_celetista))),
			.groups = "drop"
		)
	coverage_confirmed <- year %in% spec$coverage_confirmed_years
	person_year <- roster |>
		dplyr::left_join(by_person, by = "cpf") |>
		dplyr::mutate(
			year = as.integer(!!year),
			n_source_links = dplyr::coalesce(n_source_links, 0L),
			n_active_celetista = dplyr::coalesce(n_active_celetista, 0L),
			n_active_other = dplyr::coalesce(n_active_other, 0L),
			n_uncertain_links = dplyr::coalesce(n_uncertain_links, 0L),
			extraction_completed = TRUE,
			source_coverage_confirmed = coverage_confirmed,
			celetista_31dec = dplyr::case_when(
				n_active_celetista > 0L ~ 1L,
				n_uncertain_links > 0L ~ NA_integer_,
				source_coverage_confirmed ~ 0L,
				TRUE ~ NA_integer_
			),
			employment_status = dplyr::case_when(
				n_active_celetista > 0L ~ "observed_celetista",
				n_uncertain_links > 0L ~ "unclassifiable_links",
				!source_coverage_confirmed ~ "source_coverage_unconfirmed",
				n_source_links == 0L ~ "no_match",
				TRUE ~ "no_active_celetista"
			)
		) |>
		dplyr::compute()
	# Keep all observed-active types for audit/reclassification; downstream outcomes
	# and workplace lookup use rais_celetista. No inactive/unknown-status rows persist here.
	active <- matched |> dplyr::filter(rais_active) |> dplyr::compute()
	link_audit <- matched |>
		dplyr::count(
			year, emp_31dez, tipo_vinculo, salary_type_rais, rais_active, rais_inactive,
			rais_celetista, rais_type_known, salary_monthly_verified,
			salary_positive, hours_positive, name = "n_links"
		) |>
		dplyr::collect()
	manifest <- tibble::tibble(
		year = as.integer(year), n_roster = n_roster, n_matched_raw = n_raw,
		n_matched_unique = n_unique, n_exact_duplicates_removed = n_raw - n_unique,
		n_active_retained = rais_count(active), extraction_completed = TRUE,
		source_coverage_confirmed = coverage_confirmed,
		salary_type_available = !is.null(salary_col),
		salary_type_column = if (is.null(salary_col)) NA_character_ else salary_col,
		monthly_salary_verified = monthly_verified,
		completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
	)
	person_audit <- person_year |>
		dplyr::count(year, employment_status, name = "n_people") |>
		dplyr::collect()
	paths <- c(
		records = file.path(save_dir, paste0("rais_active_", year, ".parquet")),
		person_year = file.path(save_dir, paste0("rais_person_year_", year, ".parquet")),
		link_audit = file.path(save_dir, paste0("rais_link_audit_", year, ".parquet")),
		person_audit = file.path(save_dir, paste0("rais_person_audit_", year, ".parquet")),
		manifest = file.path(save_dir, paste0("rais_manifest_", year, ".parquet"))
	)
	write_parquet_target(active, paths[["records"]], overwrite = TRUE)
	write_parquet_target(person_year, paths[["person_year"]], overwrite = TRUE)
	write_parquet_target(link_audit, paths[["link_audit"]], overwrite = TRUE)
	write_parquet_target(person_audit, paths[["person_audit"]], overwrite = TRUE)
	# Write manifest last; any failed step leaves the target failed.
	write_parquet_target(manifest, paths[["manifest"]], overwrite = TRUE)
	return(paths)
}

# Separate file target. Lookup only; no employment or wage filtering depends on it.
get_work_location <- function(rais_vinc, year, save_dir, spec = make_rais_spec()) {
	links <- rais_input(rais_vinc)
	rais_require(links, c("id_estab", "cei_vinc", "rais_celetista"))
	keys <- links |>
		dplyr::filter(rais_celetista, !is.na(id_estab)) |>
		dplyr::distinct(id_estab, cei_vinc) |>
		dplyr::compute()
	n_keys <- rais_count(keys)
	if (!n_keys) {
		locations <- keys |>
			dplyr::mutate(
				work_lat = NA_real_, work_lon = NA_real_, work_desvio = NA_real_,
				work_h3_id = NA_character_
			)
	} else {
		locations <- ipeadatalake::adicionar_geoloc(keys, "rais", year)
		rais_require(locations, c("id_estab", "cei_vinc", "lat", "lon", "desvio_metros", "h3_09"))
		locations <- locations |>
			dplyr::transmute(
				id_estab, cei_vinc, work_lat = lat, work_lon = lon,
				work_desvio = desvio_metros, work_h3_id = h3_09
			) |>
			dplyr::compute()
		if (rais_count(locations) != rais_count(dplyr::distinct(locations, id_estab, cei_vinc))) {
			stop("Workplace lookup has duplicate establishment keys.")
		}
		# Preserve keys when the geocoder omits establishments.
		locations <- keys |>
			dplyr::left_join(locations, by = c("id_estab", "cei_vinc")) |>
			dplyr::compute()
	}
	max_dev <- spec$max_work_geo_dev
	locations <- locations |>
		dplyr::mutate(
			year = as.integer(!!year),
			work_coordinates_valid =
				!is.na(work_lat) & !is.na(work_lon) &
				is.finite(work_lat) & is.finite(work_lon) &
				work_lat >= -90 & work_lat <= 90 & work_lon >= -180 & work_lon <= 180 &
				!(work_lat == 0 & work_lon == 0),
			work_geo_valid = work_coordinates_valid &
				!is.na(work_desvio) & is.finite(work_desvio) &
				work_desvio >= 0 & work_desvio <= max_dev,
			work_geo_reason = dplyr::case_when(
				!work_coordinates_valid ~ "invalid_coordinates",
				is.na(work_desvio) ~ "unknown_deviation",
				!is.finite(work_desvio) | work_desvio < 0 | work_desvio > max_dev ~ "invalid_deviation",
				TRUE ~ "usable"
			)
		) |>
		dplyr::compute()
	if (rais_count(locations) != n_keys) stop("Geocoding changed establishment-key count.")
	audit <- locations |>
		dplyr::count(year, work_geo_reason, name = "n_establishments") |>
		dplyr::collect()
	paths <- c(
		records = file.path(save_dir, paste0("rais_workplaces_", year, ".parquet")),
		audit = file.path(save_dir, paste0("rais_workplace_audit_", year, ".parquet"))
	)
	write_parquet_target(locations, paths[["records"]], overwrite = TRUE)
	write_parquet_target(audit, paths[["audit"]], overwrite = TRUE)
	return(paths)
}

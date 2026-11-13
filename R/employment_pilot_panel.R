# All person-level products stay in restricted storage; no estimation is run here.
match_cohort_files <- function(files, prefix, expected_years) {
	files <- unlist(files, use.names = FALSE)
	pattern <- paste0("^", prefix, "([0-9]{4})\\.parquet$")
	files <- unique(files[grepl(pattern, basename(files))])
	years <- as.integer(sub(pattern, "\\1", basename(files)))
	if (anyDuplicated(years) || !setequal(years, expected_years) || !all(file.exists(files))) {
		stop("Missing, duplicate or unexpected annual artifacts.")
	}
	files[match(expected_years, years)]
}

# Select one already built cohort panel from the static multi-file target.
select_employment_panel <- function(panel_files, focal_year) {
	panel_files <- unlist(panel_files, use.names = FALSE)
	if (length(focal_year) != 1L || is.na(focal_year)) stop("Supply one focal year.")
	pattern <- paste0("^did_panel_employment_", as.integer(focal_year), "\\.parquet$")
	matches <- panel_files[grepl(pattern, basename(panel_files))]
	if (length(matches) != 1L || !file.exists(matches[[1]])) {
		stop("Expected exactly one built employment panel for the focal cohort.")
	}
	unname(matches[[1]])
}
build_employment_memberships <- function(risk_sets, exposure_files, g_lag, save_dir) {
	exposure_files <- unlist(exposure_files, use.names = FALSE)
	g <- as.integer(sub("^did_exposure_provisional_([0-9]{4})\\.parquet$", "\\1",
		basename(exposure_files)))
	if (anyNA(g) || anyDuplicated(g) || !dir.exists(save_dir)) {
		stop("Invalid provisional exposure set.")
	}
	risk_files <- match_cohort_files(risk_sets, "risk_set_", g)
	paths <- character(length(g))
	for (i in seq_along(g)) {
		risk <- arrow::read_parquet(risk_files[[i]], col_select = tidyselect::all_of(c(
			"cpf", "year", "status_num", "birthdate_cad", "gender_cad",
			"race_cad", "literate_cad", "lat_cad", "lon_cad"
		)))
		exposure <- arrow::read_parquet(exposure_files[[i]])
		if (anyDuplicated(exposure$cpf) || anyNA(exposure$cpf) ||
			any(exposure$risk_set_g != g[[i]])) stop("Invalid exposure keys.")
		# Unknown prospective timing is excluded from this provisional sample,
		# with counts retained in did_exposure_provisional_audit.
		exposure <- exposure[!is.na(exposure$treat_year), , drop = FALSE]
		risk <- risk[risk$cpf %in% exposure$cpf, , drop = FALSE]
		if (!nrow(risk)) stop("No known-timing membership candidates.")
		membership <- build_did_membership(risk, exposure, g[[i]], g[[i]] - g_lag)
		membership <- prepare_did_baseline(membership)
		valid <- is.finite(membership$lat_cad) & is.finite(membership$lon_cad) &
			membership$lat_cad >= -90 & membership$lat_cad <= 90 &
			membership$lon_cad >= -180 & membership$lon_cad <= 180
		if (!all(valid)) stop("Baseline H3-7 requires valid residence coordinates.")
		points <- sf::st_as_sf(
			data.frame(lon = membership$lon_cad, lat = membership$lat_cad),
			coords = c("lon", "lat"), crs = 4326
		)
		membership$h3_7_baseline <- as.character(
			h3o::h3_from_points(sf::st_geometry(points), 7L)
		)
		membership$lat_cad <- NULL
		membership$lon_cad <- NULL
		membership$birthdate_cad <- NULL
		membership$gender_cad <- NULL
		membership$race_cad <- NULL
		membership$literate_cad <- NULL
		paths[[i]] <- file.path(save_dir, paste0("did_membership_employment_", g[[i]], ".parquet"))
		write_parquet_target(membership, paths[[i]], overwrite = TRUE)
	}
	paths
}

build_employment_panels <- function(membership_files, person_year_files, outcome_years, save_dir) {
	membership_files <- unlist(membership_files, use.names = FALSE)
	g <- as.integer(sub("^did_membership_employment_([0-9]{4})\\.parquet$", "\\1",
		basename(membership_files)))
	if (anyNA(g) || anyDuplicated(g) || !dir.exists(save_dir)) {
		stop("Invalid employment membership artifacts.")
	}
	outcome_years <- sort(unique(as.integer(outcome_years)))
	person_year_files <- match_cohort_files(person_year_files, "rais_person_year_", outcome_years)
	source <- arrow::open_dataset(person_year_files) |>
		dplyr::select(cpf, year, celetista_31dec, source_coverage_confirmed, employment_status)
	paths <- character(length(g))
	for (i in seq_along(g)) {
		membership <- arrow::read_parquet(membership_files[[i]])
		membership <- membership[membership$prior_treat_year == 0L &
			membership$base_service == "first", , drop = FALSE]
		if (!nrow(membership)) stop("No members without prior assignment.")
		roster <- arrow::as_arrow_table(data.frame(cpf = membership$cpf))
		outcomes <- source |> dplyr::semi_join(roster, by = "cpf") |> dplyr::collect()
		if (anyNA(outcomes[c("cpf", "year")]) ||
			anyDuplicated(outcomes[c("cpf", "year")]) ||
			nrow(outcomes) != nrow(membership) * length(outcome_years) ||
			!all(outcomes$source_coverage_confirmed)) {
			stop("Incomplete or duplicate certified annual employment outcomes.")
		}
		outcomes$source_coverage_confirmed <- NULL
		outcomes$employment_status <- NULL
		outcomes$risk_set_g <- g[[i]]
		panel <- build_did_panel(membership, outcomes, outcome_years)
		paths[[i]] <- file.path(save_dir, paste0("did_panel_employment_", g[[i]], ".parquet"))
		write_parquet_target(panel, paths[[i]], overwrite = TRUE)
	}
	paths
}

# Restricted aggregate check of yearly outcome availability after panel assembly.
audit_employment_panels <- function(panel_files, save_dir) {
	panel_files <- unlist(panel_files, use.names = FALSE)
	if (!length(panel_files) || !all(file.exists(panel_files))) stop("Missing employment panels.")
	rows <- lapply(panel_files, function(path) {
		arrow::open_dataset(path) |>
			dplyr::mutate(outcome_class = dplyr::case_when(
				celetista_31dec == 1L ~ "employed",
				celetista_31dec == 0L ~ "not_employed",
				TRUE ~ "unknown"
			)) |>
			dplyr::count(risk_set_g, year, base_service, status_num,
				outcome_class, name = "n_people") |>
			dplyr::collect()
	})
	path <- file.path(save_dir, "did_panel_employment_audit.rds")
	saveRDS(dplyr::bind_rows(rows), path)
	path
}
# Support after unresolved timing and prior assignments are removed.
audit_employment_memberships <- function(membership_files, save_dir) {
	membership_files <- unlist(membership_files, use.names = FALSE)
	if (!length(membership_files) || !all(file.exists(membership_files))) {
		stop("Missing membership artifacts.")
	}
	rows <- lapply(membership_files, function(path) {
		x <- arrow::read_parquet(path, col_select = tidyselect::all_of(c(
			"risk_set_g", "base_service", "status_num", "treat_year", "prior_treat_year",
			"age_baseline", "gender_baseline", "race_baseline", "literacy_baseline",
			"h3_7_baseline"
		)))
		x <- x[x$prior_treat_year == 0L, , drop = FALSE]
		x$exposure_role <- ifelse(x$treat_year == x$risk_set_g,
			"focal_treated", "comparison_candidate")
		x$complete_covariates <- stats::complete.cases(x[c(
			"age_baseline", "gender_baseline", "race_baseline", "literacy_baseline"
		)])
		summary <- dplyr::group_by(x, risk_set_g, base_service, exposure_role, status_num) |>
			dplyr::summarise(n_people = dplyr::n(),
				n_complete_covariates = sum(complete_covariates), .groups = "drop")
		cells <- dplyr::count(x, risk_set_g, base_service, exposure_role, h3_7_baseline,
			name = "n_people_cell") |>
			dplyr::group_by(risk_set_g, base_service, exposure_role) |>
			dplyr::summarise(n_cells = dplyr::n(),
				max_cell_share = max(n_people_cell) / sum(n_people_cell), .groups = "drop")
		list(summary = summary, clusters = cells)
	})
	path <- file.path(save_dir, "did_membership_employment_audit.rds")
	saveRDS(list(
		summary = dplyr::bind_rows(lapply(rows, `[[`, "summary")),
		clusters = dplyr::bind_rows(lapply(rows, `[[`, "clusters"))
	), path)
	path
}
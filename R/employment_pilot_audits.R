# Aggregate, outcome-free support audit for the employment pilot. The output is restricted.
audit_employment_pilot_inputs <- function(risk_sets, g_lag, save_dir) {
	files <- unlist(risk_sets, use.names = FALSE)
	files <- unique(files[grepl("^risk_set_[0-9]{4}\\.parquet$", basename(files))])
	if (!length(files) || !all(file.exists(files))) stop("Missing risk-set record artifacts.")
	if (!dir.exists(save_dir)) stop("Restricted output directory is unavailable.")
	cohorts <- as.integer(sub("^risk_set_([0-9]{4})\\.parquet$", "\\1", basename(files)))
	if (anyDuplicated(cohorts)) stop("Duplicate risk-set cohort artifacts.")
	needed <- c(
		"cpf", "year", "status_num", "birthdate_cad", "gender_cad", "race_cad",
		"literate_cad", "lat_cad", "lon_cad"
	)
	by_cohort <- lapply(seq_along(files), function(i) {
		data <- arrow::read_parquet(files[[i]], col_select = tidyselect::all_of(needed))
		if (anyDuplicated(data$cpf) || anyNA(data$cpf)) stop("Invalid risk-set person key.")
		g <- cohorts[[i]]
		if (anyNA(data$year) || any(data$year != g - g_lag) ||
			anyNA(data$status_num) || any(!data$status_num %in% 0:6)) {
			stop("Invalid risk-set baseline year or status.")
		}
		data <- data[data$status_num != 0L, , drop = FALSE]
		if (!nrow(data)) stop("Empty eligible risk set.")
		data$risk_set_g <- g
		data$baseline_year <- data$year
		data$base_service <- ifelse(data$status_num %in% c(1L, 2L), "additional", "first")
		data$exposure_role <- ifelse(data$status_num %in% c(2L, 3L),
			"focal_treated", "comparison_candidate")
		data <- prepare_did_baseline(data)
		valid_geo <- is.finite(data$lat_cad) & is.finite(data$lon_cad) &
			data$lat_cad >= -90 & data$lat_cad <= 90 &
			data$lon_cad >= -180 & data$lon_cad <= 180
		data$h3_7_baseline <- NA_character_
		if (any(valid_geo)) {
			points <- sf::st_as_sf(
				data.frame(lon = data$lon_cad[valid_geo], lat = data$lat_cad[valid_geo]),
				coords = c("lon", "lat"), crs = 4326
			)
			data$h3_7_baseline[valid_geo] <- as.character(
				h3o::h3_from_points(sf::st_geometry(points), 7L)
			)
		}
		variables <- c("age_baseline", "gender_baseline", "race_baseline", "literacy_baseline")
		data$complete_covariates <- stats::complete.cases(data[variables])
		group_cols <- c("risk_set_g", "base_service", "exposure_role", "status_num")
		coverage <- data |>
			dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
			dplyr::summarise(
				n_people = dplyr::n(),
				n_complete_covariates = sum(complete_covariates),
				n_missing_age = sum(is.na(age_baseline)),
				n_missing_gender = sum(is.na(gender_baseline)),
				n_missing_race = sum(is.na(race_baseline)),
				n_missing_literacy = sum(is.na(literacy_baseline)),
				n_missing_h3_7 = sum(is.na(h3_7_baseline)),
				.groups = "drop"
			)
		# Count H3 support for each actual comparison population, not by status alone.
		main <- data
		main$comparison <- "main"
		alternative <- data[data$exposure_role == "focal_treated" |
			data$status_num == 5L, , drop = FALSE]
		alternative$comparison <- "status_5"
		sample <- dplyr::bind_rows(main, alternative)
		support_cols <- c("risk_set_g", "base_service", "comparison", "exposure_role")
		support <- sample |>
			dplyr::group_by(dplyr::across(dplyr::all_of(support_cols))) |>
			dplyr::summarise(
				n_people = dplyr::n(),
				n_complete_covariates = sum(complete_covariates),
				n_missing_h3_7 = sum(is.na(h3_7_baseline)),
				.groups = "drop"
			)
		cells <- sample[!is.na(sample$h3_7_baseline), , drop = FALSE] |>
			dplyr::count(dplyr::across(dplyr::all_of(support_cols)), h3_7_baseline,
				name = "n_people_cell") |>
			dplyr::group_by(dplyr::across(dplyr::all_of(support_cols))) |>
			dplyr::summarise(
				n_cells = dplyr::n(), n_people_geocoded = sum(n_people_cell),
				max_cell_people = max(n_people_cell),
				max_cell_share = max(n_people_cell) / sum(n_people_cell),
				.groups = "drop"
			)
		list(coverage = coverage, support = support, clusters = cells)
	})
	result <- list(
		coverage = dplyr::bind_rows(lapply(by_cohort, `[[`, "coverage")),
		support = dplyr::bind_rows(lapply(by_cohort, `[[`, "support")),
		clusters = dplyr::bind_rows(lapply(by_cohort, `[[`, "clusters")),
		definition = list(
			role = "Status 2/3 are focal treated; all other retained statuses are candidates only.",
			alternative_controls = "Only status_num == 5 is eligible for the alternative comparison.",
			cluster = "H3 resolution 7 from baseline latitude/longitude; missing cells stay missing."
		)
	)
	path <- file.path(save_dir, "employment_pilot_input_audit.rds")
	saveRDS(result, path)
	path
}

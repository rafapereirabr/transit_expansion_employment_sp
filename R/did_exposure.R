# Outcome-free, explicitly provisional timing from current risk-set labels.
# Status 1 has old service, but its future additional service is unresolved.
derive_provisional_exposure <- function(risk, focal_year, prior_year = NULL) {
	required <- c("cpf", "year", "status_num", "dt_trial_svc")
	missing <- setdiff(required, names(risk))
	if (length(missing)) stop("Missing risk-set timing fields.")
	if (anyNA(risk$cpf) || anyDuplicated(risk$cpf) ||
		anyNA(risk$year) || any(risk$year >= focal_year) ||
		anyNA(risk$status_num) || any(!risk$status_num %in% 1:6)) {
		stop("Invalid provisional exposure input.")
	}
	if (is.null(prior_year)) prior_year <- rep(0L, nrow(risk))
	if (length(prior_year) != nrow(risk) || anyNA(prior_year) ||
		any(prior_year < 0L | (prior_year >= focal_year & prior_year != 0L))) {
		stop("Invalid prior treatment assignment.")
	}
	future_year <- suppressWarnings(as.integer(format(as.Date(risk$dt_trial_svc), "%Y")))
	status <- as.integer(risk$status_num)
	g <- rep(NA_integer_, nrow(risk))
	g[status %in% c(2L, 3L)] <- as.integer(focal_year)
	valid_future <- status == 4L & !is.na(future_year) & future_year > focal_year
	g[valid_future] <- future_year[valid_future]
	g[status %in% c(5L, 6L)] <- 0L
	basis <- rep("unresolved", nrow(risk))
	basis[status %in% c(2L, 3L)] <- "focal_status"
	basis[valid_future] <- "nearest_future_station_provisional"
	basis[status == 5L] <- "planned_after_window_provisional"
	basis[status == 6L] <- "no_catalogued_near_future_provisional"
	basis[status == 1L] <- "old_service_future_unknown"
	data.frame(
		cpf = risk$cpf, risk_set_g = as.integer(focal_year),
		treat_year = g, prior_treat_year = as.integer(prior_year),
		timing_basis = basis, stringsAsFactors = FALSE
	)
}

# Persist person-level exposure only inside restricted storage. Risk-set records
# are processed by cohort so earlier focal assignments follow people across baselines.
build_provisional_exposure <- function(risk_sets, g_lag, save_dir) {
	files <- unlist(risk_sets, use.names = FALSE)
	files <- unique(files[grepl("^risk_set_[0-9]{4}\\.parquet$", basename(files))])
	if (!length(files) || !all(file.exists(files)) || !dir.exists(save_dir)) {
		stop("Missing risk-set or restricted output artifacts.")
	}
	cohorts <- as.integer(sub("^risk_set_([0-9]{4})\\.parquet$", "\\1", basename(files)))
	if (anyDuplicated(cohorts)) stop("Duplicate focal cohorts.")
	order <- base::order(cohorts)
	files <- files[order]
	cohorts <- cohorts[order]
	prior <- setNames(integer(), character())
	paths <- character(length(files))
	for (i in seq_along(files)) {
		risk <- arrow::read_parquet(files[[i]], col_select = tidyselect::all_of(c(
			"cpf", "year", "status_num", "dt_trial_svc"
		)))
		if (anyNA(risk$year) || any(risk$year != cohorts[[i]] - g_lag)) {
			stop("Risk-set baseline year does not match cohort lag.")
		}
		risk <- risk[risk$status_num != 0L, , drop = FALSE]
		previous <- unname(prior[as.character(risk$cpf)])
		previous[is.na(previous)] <- 0L
		exposure <- derive_provisional_exposure(risk, cohorts[[i]], previous)
		paths[[i]] <- file.path(save_dir, paste0("did_exposure_provisional_", cohorts[[i]], ".parquet"))
		write_parquet_target(exposure, paths[[i]], overwrite = TRUE)
		focal <- as.character(risk$cpf[risk$status_num %in% c(2L, 3L)])
		new <- focal[!focal %in% names(prior)]
		prior <- c(prior, stats::setNames(rep(as.integer(cohorts[[i]]), length(new)), new))
	}
	paths
}

# Aggregate exposure audit, including unknown prospective G. No identifiers leave storage.
audit_provisional_exposure <- function(exposure_files, save_dir) {
	cohorts <- as.integer(sub("^did_exposure_provisional_([0-9]{4})\\.parquet$", "\\1",
		basename(exposure_files)))
	if (anyNA(cohorts) || anyDuplicated(cohorts)) stop("Invalid exposure artifacts.")
	rows <- lapply(seq_along(exposure_files), function(i) {
		x <- arrow::read_parquet(exposure_files[[i]])
		dplyr::count(x, risk_set_g, timing_basis,
			prospective_known = !is.na(treat_year),
			prior_assigned = prior_treat_year > 0L, name = "n_people")
	})
	path <- file.path(save_dir, "did_exposure_provisional_audit.rds")
	saveRDS(dplyr::bind_rows(rows), path)
	path
}

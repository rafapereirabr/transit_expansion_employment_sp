# All specifications share the same input risk set. Failed fits get aggregate status;
# raw external errors/warnings may contain identifiers and are never persisted here.
estimate_did_grid <- function(panel, specs, cohorts = NULL, horizon = NULL) {
	if (!is.null(cohorts) || !is.null(horizon)) {
		stop("Preliminary output is cohort trajectories only; no horizon aggregation is enabled.")
	}
	panel <- collect_did_data(panel)
	require_did_columns(panel, c("person_id", "risk_set_g", "year"))
	specs <- tibble::as_tibble(specs)
	if (!nrow(specs) || anyDuplicated(specs$spec_id)) stop("Empty or duplicate grid.")
	estimates <- list()
	status <- list()
	support <- list()
	overlap <- list()
	k <- 0L
	for (g in sort(unique(panel$risk_set_g))) {
		risk <- panel[panel$risk_set_g == g, , drop = FALSE]
		for (j in seq_len(nrow(specs))) {
			k <- k + 1L
			warning_count <- 0L
			attempt <- tryCatch(
				withCallingHandlers({
					fit <- run_att_gt(risk, specs[j, , drop = FALSE])
					list(fit = fit, estimates = extract_focal_att(fit))
				}, warning = function(w) {
					warning_count <<- warning_count + 1L
					invokeRestart("muffleWarning")
				}),
				did_model_suspended = function(e) list(reason = e$reason),
				error = function(e) list(reason = "estimator_or_input_error")
			)
			ok <- is.null(attempt$reason)
			status[[k]] <- tibble::tibble(
				risk_set_g = as.integer(g), spec_id = specs$spec_id[j],
				status = if (ok) "estimated" else "suspended",
				reason = if (ok) NA_character_ else attempt$reason,
				n_warnings = warning_count
			)
			if (!ok) next
			estimates[[k]] <- attempt$estimates
			support[[k]] <- dplyr::mutate(
				attempt$fit$support, risk_set_g = as.integer(g), spec_id = specs$spec_id[j]
			)
			overlap[[k]] <- dplyr::mutate(
				attempt$fit$overlap, risk_set_g = as.integer(g), spec_id = specs$spec_id[j]
			)
		}
	}
	focal <- if (length(estimates)) dplyr::bind_rows(estimates) else empty_att()
	result <- dplyr::bind_rows(focal, combine_att(focal)) |>
		dplyr::mutate(estimand = "annual")
	status <- dplyr::bind_rows(status)
	return(list(
		estimates = result, status = status,
		skipped = dplyr::filter(status, status == "suspended"),
		support = dplyr::bind_rows(support), overlap = dplyr::bind_rows(overlap)
	))
}

# Whole H3 cells, including all their people/years/strata, are sampled together.
# Within a focal cohort every person has one fixed cell. All specs share a draw,
# so combined effects retain covariance and their treated-person weights are refit.
# Different cohorts may place the same person in different baseline cells:
# this function supplies NO cross-cohort covariance or cross-cohort confidence band.
bootstrap_did <- function(
	panel, specs, inference = make_did_inference(), cohorts = NULL, horizon = NULL
) {
	if (!is.null(cohorts) || !is.null(horizon)) stop("Cross-cohort aggregation is not enabled.")
	inference <- make_did_inference(
		inference$biters, inference$seed, inference$alpha,
		inference$cluster_col, inference$h3_resolution
	)
	panel <- collect_did_data(panel)
	cluster_col <- inference$cluster_col
	require_did_columns(panel, c("person_id", "risk_set_g", "year", cluster_col))
	if (anyNA(panel[c("person_id", "risk_set_g", "year", cluster_col)]) ||
		anyDuplicated(panel[c("person_id", "risk_set_g", "year")])) {
		stop("Invalid cohort bootstrap keys or missing baseline cluster.")
	}
	cell <- as.character(panel[[cluster_col]])
	if (any(!grepl("^[0-9a-fA-F]{15}$", cell)) ||
		any(substr(cell, 1L, 2L) != paste0("8", inference$h3_resolution))) {
		stop("Baseline H3 column has the wrong resolution/index format.")
	}
	membership_cells <- unique(panel[c("person_id", "risk_set_g", cluster_col)])
	if (anyDuplicated(membership_cells[c("person_id", "risk_set_g")])) {
		stop("A person must have one fixed cluster within each risk set.")
	}
	point <- estimate_did_grid(panel, specs)
	if (!nrow(point$estimates)) {
		return(list(
			estimates = point$estimates, model_status = point$status,
			skipped = point$skipped, support = point$support, overlap = point$overlap,
			inference = inference, audit = tibble::tibble(),
			draws = list(), covariance = list(),
			cross_cohort_inference = "not_available"
		))
	}
	had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
	if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
	on.exit({
		if (had_seed) {
			assign(".Random.seed", old_seed, envir = .GlobalEnv)
		} else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
			rm(".Random.seed", envir = .GlobalEnv)
		}
	}, add = TRUE)
	set.seed(inference$seed)
	results <- list()
	audits <- list()
	cluster_audits <- list()
	draw_tables <- list()
	covariances <- list()
	key <- function(x) paste(x$spec_id, x$year, sep = "|")
	for (g in sort(unique(point$estimates$risk_set_g))) {
		risk <- panel[panel$risk_set_g == g, , drop = FALSE]
		result <- point$estimates[point$estimates$risk_set_g == g, , drop = FALSE]
		keys <- key(result)
		if (anyDuplicated(keys)) stop("Duplicate focal estimand keys.")
		units <- risk[!duplicated(risk$person_id), , drop = FALSE]
		cells <- unique(as.character(units[[cluster_col]]))
		treated_cells <- unique(as.character(units[[cluster_col]][units$treat_year == g]))
		control_cells <- unique(as.character(units[[cluster_col]][units$treat_year == 0 | units$treat_year > g]))
		label <- as.character(g)
		cluster_audits[[label]] <- tibble::tibble(
			risk_set_g = as.integer(g), n_clusters = length(cells),
			n_treated_clusters = length(treated_cells), n_candidate_control_clusters = length(control_cells),
			max_people_per_cluster = max(table(units[[cluster_col]]))
		)
		result$h3_resolution <- inference$h3_resolution
		result$inference_scope <- "within_focal_cohort"
		result$n_bootstrap_complete <- 0L
		if (length(cells) < 2L || length(treated_cells) < 2L || length(control_cells) < 2L) {
			result$infer_status <- "insufficient_cluster_support"
			results[[label]] <- result
			next
		}
		draws <- matrix(
			NA_real_, nrow = inference$biters, ncol = nrow(result),
			dimnames = list(NULL, keys)
		)
		point_specs <- unique(result$spec_id[result$treat_type != "combined"])
		active_specs <- specs[specs$spec_id %in% point_specs, , drop = FALSE]
		# Counts must reflect each model's actual comparison and observed outcome sample,
		# not just clusters available somewhere in the full risk set.
		model_cluster_support <- vapply(seq_len(nrow(active_specs)), function(j) {
			prepared <- tryCatch(
				prepare_did_sample(risk, active_specs[j, , drop = FALSE]),
				error = function(e) NULL
			)
			if (is.null(prepared)) return(FALSE)
			d <- prepared$data
			a <- unique(d[[cluster_col]][d$treat_year == g])
			b <- unique(d[[cluster_col]][d$treat_year == 0 | d$treat_year > g])
			return(length(a) >= 2L && length(b) >= 2L)
		}, logical(1))
		names(model_cluster_support) <- active_specs$spec_id
		cluster_supported <- vapply(seq_len(nrow(result)), function(j) {
			id <- result$spec_id[j]
			if (result$treat_type[j] == "combined") {
				ids <- c(sub("__combined__", "__first__", id, fixed = TRUE),
					sub("__combined__", "__additional__", id, fixed = TRUE))
				return(all(!is.na(model_cluster_support[ids]) & model_cluster_support[ids]))
			}
			return(isTRUE(model_cluster_support[[id]]))
		}, logical(1))
		active_specs <- active_specs[model_cluster_support, , drop = FALSE]
		if (!any(cluster_supported)) {
			result$infer_status <- "insufficient_cluster_support"
			results[[label]] <- result
			next
		}
		for (b in seq_len(inference$biters)) {
			sampled <- tibble::tibble(
				.cluster_key = sample(cells, length(cells), replace = TRUE),
				.draw_id = seq_along(cells)
			)
			replica <- risk
			replica$.cluster_key <- as.character(replica[[cluster_col]])
			replica <- dplyr::inner_join(
				sampled, replica, by = ".cluster_key", relationship = "many-to-many"
			)
			replica$person_id <- match(
				paste(replica$.draw_id, replica$person_id, sep = "|"),
				unique(paste(replica$.draw_id, replica$person_id, sep = "|"))
			)
			attempt <- tryCatch(
				estimate_did_grid(replica, active_specs),
				error = function(e) NULL
			)
			if (is.null(attempt)) next
			rep_key <- key(attempt$estimates)
			if (anyDuplicated(rep_key)) next
			position <- match(keys, rep_key)
			valid <- !is.na(position)
			draws[b, valid] <- attempt$estimates$att[position[valid]]
		}
		# A missing stratum invalidates the corresponding combined draw. Failure
		# does not invalidate successful models elsewhere or get silently discarded.
		valid_draw <- is.finite(draws)
		n_complete <- colSums(valid_draw)
		complete <- n_complete == inference$biters & cluster_supported
		result$n_bootstrap_complete <- n_complete
		result$infer_status <- ifelse(!cluster_supported, "insufficient_cluster_support",
			ifelse(complete, "H3_cluster_bootstrap_within_cohort", "bootstrap_incomplete_model"))
		se <- rep(NA_real_, nrow(result))
		if (any(complete)) se[complete] <- apply(draws[, complete, drop = FALSE], 2L, stats::sd)
		critical <- stats::qnorm(1 - inference$alpha / 2)
		result$std_error <- se
		result$conf_low <- result$att - critical * se
		result$conf_high <- result$att + critical * se
		result$uniform_low <- NA_real_
		result$uniform_high <- NA_real_
		# Simultaneous family is the originally requested focal-cohort estimand set.
		# Do not shrink it silently when any fit/bootstrap series fails.
		all_models_ok <- all(point$status$status[point$status$risk_set_g == g] == "estimated")
		if (all(complete) && all_models_ok) {
			varying <- se > 0
			uniform_critical <- critical
			if (any(varying)) {
				centered <- sweep(draws[, varying, drop = FALSE], 2L,
					colMeans(draws[, varying, drop = FALSE]), "-")
				standardized <- sweep(centered, 2L, se[varying], "/")
				uniform_critical <- max(critical, as.numeric(stats::quantile(
					apply(abs(standardized), 1L, max), 1 - inference$alpha
				)))
			}
			result$uniform_low <- result$att - uniform_critical * se
			result$uniform_high <- result$att + uniform_critical * se
		}
		audits[[label]] <- tibble::tibble(
			risk_set_g = as.integer(g), spec_id = result$spec_id, year = result$year,
			n_requested = inference$biters, n_complete = n_complete,
			n_failed = inference$biters - n_complete
		)
		draw_tables[[label]] <- draws
		if (any(complete)) covariances[[label]] <- stats::cov(draws[, complete, drop = FALSE])
		results[[label]] <- result
	}
	return(list(
		estimates = dplyr::bind_rows(results), model_status = point$status,
		skipped = point$skipped, support = point$support, overlap = point$overlap,
		audit = dplyr::bind_rows(audits), clusters = dplyr::bind_rows(cluster_audits),
		draws = draw_tables, covariance = covariances, inference = inference,
		cross_cohort_inference = "not_available"
	))
}

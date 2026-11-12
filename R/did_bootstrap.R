# Evaluate the whole grid together so one person-level draw feeds every risk set,
# stratum and comparison. This is deliberately serial until a benchmark is available.
estimate_did_grid <- function(panel, specs, cohorts = NULL, horizon = NULL) {
	panel <- collect_did_data(panel)
	specs <- as.data.frame(specs)
	if (!nrow(specs) || anyDuplicated(specs$spec_id)) stop("Empty or duplicate model grid.")
	if (xor(is.null(cohorts), is.null(horizon))) stop("Supply both cohorts and horizon.")
	estimates <- list()
	skipped <- list()
	k <- 0L
	for (g in sort(unique(panel$risk_set_g))) {
		risk <- panel[panel$risk_set_g == g, , drop = FALSE]
		for (j in seq_len(nrow(specs))) {
			fit <- run_att_gt(risk, specs[j, , drop = FALSE])
			k <- k + 1L
			estimates[[k]] <- extract_focal_att(fit)
			if (inherits(fit, "did_skip")) {
				skipped[[k]] <- tibble::tibble(
					risk_set_g = g, spec_id = specs$spec_id[j], reason = fit$reason
				)
			}
		}
	}
	focal <- dplyr::bind_rows(estimates)
	result <- dplyr::bind_rows(focal, combine_att(focal)) |>
		dplyr::mutate(estimand = "annual", horizon = NA_integer_)
	if (!is.null(horizon)) {
		summary <- summarise_did_horizon(result, cohorts, horizon) |>
			dplyr::mutate(
				estimand = "common_horizon", risk_set_g = NA_integer_,
				year = NA_integer_, event_time = NA_integer_
			)
		result <- dplyr::bind_rows(result, summary)
	}
	return(list(estimates = result, skipped = dplyr::bind_rows(skipped)))
}

# Nonparametric pairs bootstrap over unique people, conditional on the constructed
# roster, exposure mapping and outcome definitions (including the wage threshold).
# It captures repeated-person dependence, not geographic/corridor-level shocks.
bootstrap_did <- function(
	panel,
	specs,
	inference = make_did_inference(),
	cohorts = NULL,
	horizon = NULL
) {
	panel <- collect_did_data(panel)
	require_did_columns(panel, c("person_id", "risk_set_g", "year"))
	if (anyNA(panel$person_id) ||
		anyDuplicated(panel[c("person_id", "risk_set_g", "year")])) {
		stop("Invalid joint-bootstrap panel key.")
	}
	# Revalidate user-supplied inference settings.
	inference <- make_did_inference(inference$biters, inference$seed, inference$alpha)
	point <- estimate_did_grid(panel, specs, cohorts, horizon)
	estimates <- point$estimates
	if (!nrow(estimates) || any(!is.finite(estimates$att))) {
		stop("Non-finite point estimates; resolve support before bootstrap.")
	}
	key_cols <- c("spec_id", "estimand", "risk_set_g", "year", "horizon")
	key <- function(x) do.call(paste, c(x[key_cols], sep = "|"))
	point_key <- key(estimates)
	if (anyDuplicated(point_key)) stop("Duplicate estimand keys.")
	ids <- unique(panel$person_id)
	if (length(ids) < 2L) stop("Bootstrap requires at least two people.")
	# Restore the calling RNG state; the configured seed is stored with the result.
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
	draws <- matrix(NA_real_, nrow = inference$biters, ncol = nrow(estimates))
	status <- rep("failed", inference$biters)
	for (b in seq_len(inference$biters)) {
		sampled <- tibble::tibble(
			person_id = sample(ids, length(ids), replace = TRUE),
			.bootstrap_id = seq_along(ids)
		)
		# A repeated draw gets a new ID, shared across all that person's memberships.
		replica <- dplyr::inner_join(
			sampled, panel, by = "person_id", relationship = "many-to-many"
		)
		replica$person_id <- replica$.bootstrap_id
		replica$.bootstrap_id <- NULL
		attempt <- tryCatch(
			estimate_did_grid(replica, specs, cohorts, horizon)$estimates,
			error = function(e) NULL
		)
		# Do not persist raw errors: external estimators may include restricted values.
		if (is.null(attempt)) next
		rep_key <- key(attempt)
		if (anyDuplicated(rep_key) || !setequal(point_key, rep_key)) {
			status[b] <- "changed_support"
			next
		}
		values <- attempt$att[match(point_key, rep_key)]
		if (any(!is.finite(values))) {
			status[b] <- "nonfinite"
			next
		}
		draws[b, ] <- values
		status[b] <- "ok"
	}
	audit <- tibble::tibble(replicate = seq_len(inference$biters), status = status)
	# Never condition the reported distribution on successful draws silently.
	if (any(status != "ok")) {
		estimates$infer_status <- "bootstrap_failed_replicates"
		return(list(
			estimates = estimates, audit = audit, draws = draws,
			skipped = point$skipped, inference = inference, covariance = NULL
		))
	}
	se <- apply(draws, 2L, stats::sd)
	critical <- stats::qnorm(1 - inference$alpha / 2)
	estimates$std_error <- se
	estimates$conf_low <- estimates$att - critical * se
	estimates$conf_high <- estimates$att + critical * se
	# One simultaneous family: all requested finite annual and horizon contrasts.
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
	estimates$uniform_low <- estimates$att - uniform_critical * se
	estimates$uniform_high <- estimates$att + uniform_critical * se
	estimates$infer_status <- "person_bootstrap_conditional_on_design"
	colnames(draws) <- point_key
	covariance <- stats::cov(draws)
	return(list(
		estimates = estimates, audit = audit, draws = draws,
		skipped = point$skipped, inference = inference, covariance = covariance,
		uniform_critical = uniform_critical
	))
}

# Typed reasons let the model grid continue without persisting raw external errors.
did_suspend <- function(reason) {
	stop(structure(
		list(message = reason, call = NULL, reason = reason),
		class = c("did_model_suspended", "error", "condition")
	))
}

prepare_did_sample <- function(data, spec) {
	spec <- as_did_spec(spec)
	data <- collect_did_data(data)
	covariates <- all.vars(spec$xformla[[1]])
	required <- c(
		"person_id", "year", "treat_year", "risk_set_g", "base_service",
		spec$outcome, covariates
	)
	if (length(setdiff(required, names(data)))) did_suspend("missing_input_columns")
	if (anyNA(data[c("person_id", "year", "treat_year", "risk_set_g", "base_service")])) {
		did_suspend("missing_keys_or_exposure")
	}
	if (any(!data$base_service %in% c("first", "additional"))) did_suspend("invalid_stratum")
	g <- unique(data$risk_set_g)
	if (length(g) != 1L || !is.finite(g)) did_suspend("multiple_or_invalid_focal_cohorts")
	if (anyDuplicated(data[c("person_id", "year")])) did_suspend("duplicate_person_year")
	if (any(!is.finite(data$treat_year)) ||
		any(data$treat_year < 0 | data$treat_year != as.integer(data$treat_year)) ||
		any(data$treat_year > 0 & data$treat_year < g)) did_suspend("invalid_prospective_G")
	years <- sort(unique(data$year))
	reference <- g - spec$anticipation - 1L
	if (any(!is.finite(years)) || any(years != as.integer(years)) ||
		any(diff(years) != 1L) || !g %in% years || !reference %in% years) {
		did_suspend("missing_annual_reference_or_opening")
	}
	control_flags <- stats::na.omit(c(spec$control_flag, spec$spillover_flag))
	if (spec$comparison == "status_5") {
		if (!"status_num" %in% names(data)) did_suspend("missing_status_num")
		if (anyNA(data$status_num)) did_suspend("unknown_status_num")
	}
	if (length(setdiff(control_flags, names(data)))) did_suspend("missing_control_flags")
	fixed <- unique(c("treat_year", "base_service", covariates, control_flags,
		if (spec$comparison == "status_5") "status_num"))
	for (column in fixed) {
		if (any(tapply(data[[column]], data$person_id, function(x) length(unique(x))) != 1L)) {
			did_suspend("time_varying_baseline_attribute")
		}
	}
	keep <- data$base_service == spec$treat_type
	if (spec$comparison == "status_5") {
		keep <- keep & (data$treat_year == g | data$status_num == 5L)
	}
	for (flag in control_flags) {
		if (anyNA(data[[flag]]) || !all(data[[flag]] %in% c(TRUE, FALSE))) {
			did_suspend("unknown_control_eligibility")
		}
		keep <- keep & (data$treat_year == g | data[[flag]] == TRUE)
	}
	data <- data[keep, , drop = FALSE]
	if (!nrow(data)) did_suspend("empty_stratum")
	n_baseline_treated <- length(unique(data$person_id[data$treat_year == g]))
	if (!n_baseline_treated) did_suspend("no_focal_treated")
	missing_cov <- if (length(covariates)) !stats::complete.cases(data[covariates]) else rep(FALSE, nrow(data))
	n_missing_cov_people <- length(unique(data$person_id[missing_cov]))
	if (n_missing_cov_people) {
		if (spec$missing_covariates == "audit_pending") did_suspend("covariate_policy_pending")
		if (spec$missing_covariates == "complete_case") {
			data <- data[!missing_cov, , drop = FALSE]
		} else {
			# This policy must be deliberately selected after the baseline audit.
			for (column in covariates) {
				if (anyNA(data[[column]])) {
					if (is.numeric(data[[column]])) did_suspend("missing_numeric_covariate")
					value <- as.character(data[[column]])
					value[is.na(value)] <- "Unknown"
					data[[column]] <- factor(value)
				}
			}
		}
	}
	# Keep unknown cohort outcomes explicit until their coverage/selection policy is settled.
	if (!is.numeric(data[[spec$outcome]])) did_suspend("nonnumeric_outcome")
	n_missing_outcomes <- sum(!is.finite(data[[spec$outcome]]))
	if (spec$pop_scope == "cohort") {
		if (n_missing_outcomes || any(table(data$person_id) != length(years))) {
			did_suspend("unresolved_cohort_outcome_coverage")
		}
	} else {
		data <- data[is.finite(data[[spec$outcome]]), , drop = FALSE]
	}
	if (!nrow(data)) did_suspend("no_observed_outcomes")
	# Retain the complete baseline cohort counts, separately from observed selected populations.
	n_treated <- length(unique(data$person_id[data$treat_year == g]))
	n_controls <- length(unique(data$person_id[data$treat_year == 0 | data$treat_year > g]))
	if (!n_treated) did_suspend("no_observed_focal_outcomes")
	if (!n_controls) did_suspend("no_eligible_controls")
	# H3-7 pairs inference cannot be identified from a one-cell arm.
	if (!"h3_7_baseline" %in% names(data) || anyNA(data$h3_7_baseline)) {
		did_suspend("missing_h3_7_baseline")
	}
	cluster_units <- data[!duplicated(data$person_id), , drop = FALSE]
	if (length(unique(cluster_units$h3_7_baseline[cluster_units$treat_year == g])) < 2L ||
		length(unique(cluster_units$h3_7_baseline[
			cluster_units$treat_year == 0 | cluster_units$treat_year > g
		])) < 2L) {
		did_suspend("insufficient_h3_7_cluster_support")
	}
	support <- lapply(years, function(y) {
		# Not-yet-treated controls must be untreated at both the outcome and reference.
		cutoff <- max(y, reference) + spec$anticipation
		eligible <- data$treat_year == 0 | data$treat_year > cutoff
		tibble::tibble(
			year = as.integer(y),
			n_treated_observed = length(unique(data$person_id[data$year == y & data$treat_year == g])),
			n_controls_observed = length(unique(data$person_id[data$year == y & eligible & data$treat_year != g]))
		)
	}) |> dplyr::bind_rows()
	if (any(support$n_treated_observed == 0L | support$n_controls_observed == 0L)) {
		did_suspend("empty_yearly_treated_or_control_support")
	}
	# Check marginal support at baseline; do not silently trim or change the target population.
	units <- data[!duplicated(data$person_id), , drop = FALSE]
	treated <- units$treat_year == g
	control <- units$treat_year == 0 | units$treat_year > g
	overlap <- lapply(covariates, function(column) {
		a <- units[[column]][treated]
		b <- units[[column]][control]
		if (is.numeric(a)) {
			if (any(!is.finite(c(a, b)))) did_suspend("nonfinite_covariate")
			outside <- sum(a < min(b) | a > max(b))
			disjoint <- max(a) < min(b) || min(a) > max(b)
		} else {
			outside <- sum(!as.character(a) %in% as.character(b))
			disjoint <- !length(intersect(as.character(a), as.character(b)))
		}
		if (disjoint) did_suspend("disjoint_marginal_covariate_support")
		tibble::tibble(covariate = column, n_treated_outside_control_support = outside)
	}) |> dplyr::bind_rows()
	# The package has no observed treatment group for events beyond the outcome window.
	# Keep the historical G in membership; code it as untreated WITHIN this finite fit only.
	data$treat_year[data$treat_year > max(years)] <- 0L
	if (!any(data$treat_year == 0L)) {
		did_suspend("no_controls_for_terminal_followup")
	}
	original_ids <- unique(data$person_id)
	data$person_id <- match(data$person_id, original_ids)
	if (length(covariates)) data <- droplevels(data)
	return(list(
		data = data, risk_set_g = as.integer(g), n_treated = n_treated,
		n_baseline_treated = n_baseline_treated, n_controls = n_controls,
		n_missing_covariate_people = n_missing_cov_people,
		n_missing_outcomes = n_missing_outcomes, support = support, overlap = overlap, spec = spec
	))
}

run_att_gt <- function(data, spec, cores = 1L, bstrap = FALSE, cband = FALSE) {
	if (bstrap || cband) did_suspend("nested_bootstrap_not_supported")
	prepared <- prepare_did_sample(data, spec)
	model <- did::att_gt(
		yname = prepared$spec$outcome, tname = "year", idname = "person_id",
		gname = "treat_year", data = prepared$data, panel = TRUE,
		control_group = "notyettreated", xformla = prepared$spec$xformla[[1]],
		anticipation = prepared$spec$anticipation,
		base_period = prepared$spec$base_period, est_method = prepared$spec$est_method,
		allow_unbalanced_panel = TRUE, bstrap = FALSE, cband = FALSE,
		pl = cores > 1L, cores = as.integer(cores)
	)
	if (!any(model$group == prepared$risk_set_g)) did_suspend("focal_group_dropped_by_estimator")
	prepared$data <- NULL
	return(structure(c(prepared, list(model = model)), class = "did_fit"))
}

empty_att <- function() {
	return(tibble::tibble(
		spec_id = character(), outcome = character(), pop_scope = character(), claim = character(),
		treat_type = character(), comparison = character(), adjustment = character(),
		anticipation = integer(), spillover = character(), risk_set_g = integer(),
		year = integer(), event_time = integer(), period = character(), att = double(),
		std_error = double(), conf_low = double(), conf_high = double(),
		n_treated = integer(), n_controls = integer(), infer_status = character()
	))
}

extract_focal_att <- function(fit) {
	if (!inherits(fit, "did_fit")) stop("Expected run_att_gt() output.")
	model <- fit$model
	result <- tibble::tibble(
		group = as.integer(model$group), year = as.integer(model$t), att = as.numeric(model$att)
	) |>
		dplyr::filter(group == fit$risk_set_g) |>
		dplyr::mutate(
			spec_id = fit$spec$spec_id, outcome = fit$spec$outcome,
			pop_scope = fit$spec$pop_scope, claim = fit$spec$claim,
			treat_type = fit$spec$treat_type, comparison = fit$spec$comparison,
			adjustment = fit$spec$adjustment, anticipation = fit$spec$anticipation,
			spillover = fit$spec$spillover, risk_set_g = fit$risk_set_g,
			event_time = year - risk_set_g,
			period = dplyr::case_when(
				event_time == -anticipation - 1L ~ "reference",
				event_time >= -anticipation & event_time < 0L ~ "anticipation",
				event_time < 0L ~ "pre",
				event_time == 0L ~ "opening_year",
				TRUE ~ "full_years"
			),
			std_error = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
			n_treated = fit$n_treated, n_controls = fit$n_controls,
			infer_status = "spatial_bootstrap_pending"
		) |>
		dplyr::select(-group)
	if (!nrow(result) || any(!is.finite(result$att))) did_suspend("nonfinite_focal_ATT")
	return(result)
}

# Combined is within a cohort and only for full-cohort outcomes. Both strata are
# required; a failed stratum never becomes a single-stratum estimate labelled combined.
combine_att <- function(estimates) {
	if (is.list(estimates) && !inherits(estimates, "data.frame")) estimates <- dplyr::bind_rows(estimates)
	if (!nrow(estimates)) return(empty_att())
	estimates <- dplyr::filter(
		estimates, treat_type %in% c("first", "additional"), n_treated > 0L, pop_scope == "cohort"
	)
	if (!nrow(estimates)) return(empty_att())
	keys <- c("outcome", "comparison", "adjustment", "anticipation", "spillover", "risk_set_g", "year")
	if (anyDuplicated(estimates[c(keys, "treat_type")])) stop("Duplicate stratum ATT.")
	result <- estimates |>
		dplyr::group_by(
			outcome, pop_scope, claim, comparison, adjustment, anticipation, spillover,
			risk_set_g, year, event_time, period
		) |>
		dplyr::filter(dplyr::n_distinct(treat_type) == 2L) |>
		dplyr::summarise(
			att = stats::weighted.mean(att, n_treated),
			n_treated = sum(n_treated), n_controls = sum(n_controls),
			n_strata = dplyr::n_distinct(treat_type), .groups = "drop"
		) |>
		dplyr::mutate(
			spec_id = paste(
				outcome, "combined", comparison, adjustment,
				paste0("ant", anticipation), spillover, sep = "__"
			),
			treat_type = "combined", std_error = NA_real_, conf_low = NA_real_,
			conf_high = NA_real_, infer_status = "spatial_bootstrap_pending"
		)
	return(result)
}

# Kept for later deliberate use. Preliminary grid/bootstrap never calls this helper.
summarise_did_horizon <- function(estimates, cohorts, horizon) {
	stopifnot(
		length(horizon) == 1L, is.finite(horizon), horizon >= 1L,
		horizon == as.integer(horizon), length(cohorts) > 0L, !anyNA(cohorts)
	)
	expected <- unique(estimates$spec_id[estimates$pop_scope == "cohort"])
	selected <- dplyr::filter(
		estimates, spec_id %in% expected, risk_set_g %in% cohorts,
		event_time %in% seq_len(horizon)
	)
	if (anyDuplicated(selected[c("spec_id", "risk_set_g", "event_time")])) stop("Duplicate horizon cells.")
	support <- dplyr::count(selected, spec_id, risk_set_g)
	if (!length(expected) || any(support$n != horizon) ||
		nrow(support) != length(expected) * length(unique(cohorts)) ||
		any(!is.finite(selected$att))) stop("Incomplete common-horizon support.")
	result <- selected |>
		dplyr::group_by(spec_id, outcome, treat_type, comparison, adjustment, anticipation, spillover, risk_set_g) |>
		dplyr::summarise(att = mean(att), n_treated = dplyr::first(n_treated), .groups = "drop") |>
		dplyr::group_by(spec_id, outcome, treat_type, comparison, adjustment, anticipation, spillover) |>
		dplyr::summarise(
			att = stats::weighted.mean(att, n_treated), n_memberships = sum(n_treated), .groups = "drop"
		) |>
		dplyr::mutate(horizon = horizon, infer_status = "cross_cohort_inference_pending")
	return(result)
}

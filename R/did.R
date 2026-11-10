# DiD estimation matryoshka

make_did_spec <- function() {
	outcomes <- c(
		"formal_31dec",
		"formal_above",
		"hourly_wage",
		"work_dist_km"
	)
	comparisons <- tibble::tribble(
		~comparison , ~sample_flag  ,
		"main"      , "elig_main"   ,
		"no_2_4km"  , "elig_4km"    ,
		"l6_l17"    , "elig_l6_l17"
	)

	tidyr::crossing(
		outcome = outcomes,
		treat_type = c("first", "additional")
	) |>
		dplyr::cross_join(comparisons) |>
		dplyr::mutate(
			spec_id = paste(outcome, treat_type, comparison, sep = "__"),
			pop_scope = dplyr::case_when(
				outcome == "hourly_wage" ~ "formal",
				outcome == "work_dist_km" ~ "formal_geo",
				TRUE ~ "cohort"
			),
			unbalanced = pop_scope != 'cohort',
			anticipation = 0L,
			base_period = "varying",
			est_method = "dr",
			xformla = list(stats::as.formula("~ 1"))
		) |>
		dplyr::relocate(spec_id)
}


as_did_spec <- function(spec) {
	spec <- tibble::as_tibble(spec)
	if (nrow(spec) != 1L) {
		stop("Each estimator call must receive exactly one DiD specification row.")
	}
	required <- c(
		'unbalanced',
		"spec_id",
		"outcome",
		"pop_scope",
		"treat_type",
		"comparison",
		"sample_flag",
		"anticipation",
		"base_period",
		"est_method",
		"xformla"
	)
	missing <- setdiff(required, names(spec))
	if (length(missing) > 0L) {
		stop("The DiD specification is missing: ", paste(missing, collapse = ", "), ".")
	}
	spec
}


collect_did_data <- function(data) {
	if (inherits(data, "character")) {
		data <- arrow::open_dataset(data)
	}
	if (!inherits(data, "data.frame")) {
		data <- dplyr::collect(data)
	}
	as.data.frame(data)
}


prepare_did_sample <- function(
	data,
	spec,
	idname = "person_id",
	tname = "year",
	gname = "treat_year",
	risk_col = "risk_set_g",
	type_col = "base_service"
) {
	spec <- as_did_spec(spec)
	data <- collect_did_data(data)
	required <- c(
		idname,
		tname,
		gname,
		risk_col,
		type_col,
		spec$outcome,
		spec$sample_flag
	)
	missing <- setdiff(required, names(data))
	if (length(missing) > 0L) {
		stop("The risk set is missing: ", paste(missing, collapse = ", "), ".")
	}

	data <- data[
		data[[type_col]] == spec$treat_type &
			!is.na(data[[spec$sample_flag]]) &
			data[[spec$sample_flag]],
		,
		drop = FALSE
	]
	risk_set_g <- unique(stats::na.omit(data[[risk_col]]))
	if (length(risk_set_g) != 1L) {
		stop("Each estimator call must contain exactly one non-missing risk_set_g.")
	}
	if (any(is.na(data[[gname]]))) {
		stop(gname, " must use 0 for never treated and cannot contain NA.")
	}
	if (any(data[[gname]] < 0)) {
		stop(gname, " must use non-negative calendar years.")
	}
	if (any(data[[gname]] > 0 & data[[gname]] < risk_set_g)) {
		stop("The rolling risk set contains people treated before its focal cohort.")
	}
	if (anyDuplicated(data[c(idname, tname)]) > 0L) {
		stop("The estimator sample must contain at most one row per person and year.")
	}

	focal_ids <- unique(data[[idname]][data[[gname]] == risk_set_g])
	control_ids <- unique(data[[idname]][data[[gname]] == 0 | data[[gname]] > risk_set_g])
	list(
		data = data,
		risk_set_g = as.integer(risk_set_g),
		n_treated = length(focal_ids),
		n_controls = length(control_ids),
		spec = spec
	)
}


run_att_gt <- function(
	data,
	spec,
	idname = "person_id",
	tname = "year",
	gname = "treat_year",
	cores = 1L,
	bstrap = TRUE,
	cband = TRUE
) {
	prepared <- prepare_did_sample(data, spec, idname, tname, gname)
	if (prepared$n_treated == 0L) {
		return(structure(
			c(prepared[c("risk_set_g", "n_treated", "n_controls", "spec")], reason = "no_focal_treated"),
			class = "did_skip"
		))
	}
	if (prepared$n_controls == 0L) {
		stop("The estimator sample has no not-yet-treated comparison people.")
	}
	if (!requireNamespace("did", quietly = TRUE)) {
		stop("Package 'did' is required to estimate the DiD models.")
	}

	model <- did::att_gt(
		yname = prepared$spec$outcome,
		tname = tname,
		idname = idname,
		gname = gname,
		data = prepared$data,
		control_group = "notyettreated",
		xformla = prepared$spec$xformla[[1]],
		anticipation = prepared$spec$anticipation,
		base_period = prepared$spec$base_period,
		est_method = prepared$spec$est_method,
		allow_unbalanced_panel = prepared$spec$unbalanced,
		bstrap = bstrap,
		cband = cband,
		pl = cores > 1L,
		cores = as.integer(cores)
	)
	structure(
		list(
			model = model,
			risk_set_g = prepared$risk_set_g,
			n_treated = prepared$n_treated,
			n_controls = prepared$n_controls,
			spec = prepared$spec
		),
		class = "did_fit"
	)
}


empty_att <- function() {
	tibble::tibble(
		spec_id = character(),
		outcome = character(),
		pop_scope = character(),
		treat_type = character(),
		comparison = character(),
		risk_set_g = integer(),
		year = integer(),
		event_time = integer(),
		period = character(),
		att = double(),
		std_error = double(),
		crit_val = double(),
		conf_low = double(),
		conf_high = double(),
		n_treated = integer(),
		n_controls = integer()
	)
}


extract_focal_att <- function(fit) {
	if (inherits(fit, "did_skip")) {
		return(empty_att())
	}
	if (!inherits(fit, "did_fit")) {
		stop("fit must be an object returned by run_att_gt().")
	}

	model <- fit$model
	crit_val <- if (is.null(model$c) || length(model$c) == 0L) {
		stats::qnorm(0.975)
	} else {
		as.numeric(model$c[[1]])
	}
	tibble::tibble(
		group = as.integer(model$group),
		year = as.integer(model$t),
		att = as.numeric(model$att),
		std_error = as.numeric(model$se)
	) |>
		dplyr::filter(group == fit$risk_set_g) |>
		dplyr::mutate(
			spec_id = fit$spec$spec_id,
			outcome = fit$spec$outcome,
			pop_scope = fit$spec$pop_scope,
			treat_type = fit$spec$treat_type,
			comparison = fit$spec$comparison,
			risk_set_g = fit$risk_set_g,
			event_time = year - risk_set_g,
			period = dplyr::case_when(
				event_time < 0L ~ "pre",
				event_time == 0L ~ "opening_year",
				TRUE ~ "full_years"
			),
			crit_val = crit_val,
			conf_low = att - crit_val * std_error,
			conf_high = att + crit_val * std_error,
			n_treated = fit$n_treated,
			n_controls = fit$n_controls
		) |>
		dplyr::select(-group) |>
		dplyr::relocate(
			spec_id,
			outcome,
			pop_scope,
			treat_type,
			comparison,
			risk_set_g,
			year,
			event_time,
			period
		)
}


combine_att <- function(estimates) {
	if (is.list(estimates) && !inherits(estimates, "data.frame")) {
		estimates <- dplyr::bind_rows(estimates)
	}
	if (nrow(estimates) == 0L) {
		return(estimates)
	}
	required <- c(
		"outcome",
		"pop_scope",
		"comparison",
		"risk_set_g",
		"year",
		"event_time",
		"period",
		"att",
		"n_treated",
		"treat_type"
	)
	missing <- setdiff(required, names(estimates))
	if (length(missing) > 0L) {
		stop("The focal ATT table is missing: ", paste(missing, collapse = ", "), ".")
	}

	estimates |>
		dplyr::filter(treat_type %in% c("first", "additional"), n_treated > 0L) |>
		dplyr::group_by(
			outcome,
			pop_scope,
			comparison,
			risk_set_g,
			year,
			event_time,
			period
		) |>
		dplyr::summarise(
			att = stats::weighted.mean(att, n_treated),
			n_treated = sum(n_treated),
			n_strata = dplyr::n_distinct(treat_type),
			.groups = "drop"
		) |>
		dplyr::mutate(
			spec_id = paste(outcome, "combined", comparison, sep = "__"),
			treat_type = "combined",
			std_error = NA_real_,
			crit_val = NA_real_,
			conf_low = NA_real_,
			conf_high = NA_real_,
			infer_status = "pending"
		) |>
		dplyr::relocate(
			spec_id,
			outcome,
			pop_scope,
			treat_type,
			comparison,
			risk_set_g,
			year,
			event_time,
			period
		)
}

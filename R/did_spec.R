# Preliminary model grid; missing-data policy remains pending until its audit.
make_did_spec <- function(
	comparisons = c("main", "l6_l17"),
	spillover = "none",
	anticipation = 0L,
	xformla = NULL,
	missing_covariates = "audit_pending"
) {
	stopifnot(
		length(comparisons) > 0L, all(comparisons %in% c("main", "l6_l17", "corridors")),
		length(spillover) == 1L,
		spillover %in% c("none", "exclude_2_4km", "exclude_2_4km_and_gain"),
		length(anticipation) > 0L, all(is.finite(anticipation)),
		all(anticipation >= 0 & anticipation == as.integer(anticipation)),
		length(missing_covariates) == 1L,
		missing_covariates %in% c("audit_pending", "complete_case", "unknown_category")
	)
	formulas <- if (is.null(xformla)) {
		list(
			unadjusted = stats::as.formula("~ 1"),
			baseline_adjusted = stats::as.formula(
				"~ age_baseline + gender_baseline + race_baseline + literacy_baseline"
			)
		)
	} else {
		stopifnot(inherits(xformla, "formula"))
		list(custom = xformla)
	}
	spec <- tidyr::crossing(
		outcome = c(
			"celetista_31dec", "celetista_above_complete", "celetista_above_available",
			"hourly_wage", "work_dist_km"
		),
		treat_type = c("first", "additional"),
		comparison = unique(comparisons),
		adjustment = names(formulas),
		anticipation = as.integer(unique(anticipation))
	) |>
		dplyr::mutate(
			spillover = spillover,
			spillover_flag = switch(
				spillover,
				none = NA_character_,
				exclude_2_4km = "control_outside_2_4km",
				exclude_2_4km_and_gain = "control_outside_2_4km_low_gain"
			),
			control_flag = dplyr::case_when(
				comparison == "l6_l17" ~ "elig_l6_l17",
				comparison == "corridors" ~ "elig_corridors",
				TRUE ~ NA_character_
			),
			spec_id = paste(
				outcome, treat_type, comparison, adjustment,
				paste0("ant", anticipation), spillover, sep = "__"
			),
			pop_scope = dplyr::case_when(
				outcome == "hourly_wage" ~ "celetista_employed",
				outcome == "work_dist_km" ~ "celetista_employed_geocoded",
				TRUE ~ "cohort"
			),
			claim = ifelse(pop_scope == "cohort", "conditional_itt", "selected_population"),
			unbalanced = TRUE,
			missing_covariates = missing_covariates,
			base_period = "universal",
			est_method = "dr",
			xformla = unname(formulas[adjustment])
		)
	return(spec)
}

# Cluster definition is explicit; H3-6 is a later sensitivity, not an automatic run.
make_did_inference <- function(
	biters = 999L, seed = 20261111L, alpha = 0.05,
	cluster_col = "h3_7_baseline", h3_resolution = 7L
) {
	stopifnot(
		length(biters) == 1L, is.finite(biters), biters >= 2, biters == as.integer(biters),
		length(seed) == 1L, is.finite(seed), seed == as.integer(seed),
		length(alpha) == 1L, is.finite(alpha), alpha > 0, alpha < 1,
		length(cluster_col) == 1L, !is.na(cluster_col), nzchar(cluster_col),
		length(h3_resolution) == 1L, h3_resolution %in% c(6L, 7L)
	)
	return(list(
		biters = as.integer(biters), seed = as.integer(seed), alpha = alpha,
		cluster_col = cluster_col, h3_resolution = as.integer(h3_resolution),
		method = "cluster_pairs", inference_scope = "within_focal_cohort"
	))
}

as_did_spec <- function(spec) {
	spec <- tibble::as_tibble(spec)
	require_did_columns(spec, c(
		"spec_id", "outcome", "pop_scope", "claim", "treat_type", "comparison",
		"control_flag", "adjustment", "spillover", "spillover_flag", "unbalanced",
		"anticipation", "base_period", "est_method", "xformla", "missing_covariates"
	))
	if (nrow(spec) != 1L) stop("Supply exactly one DiD specification row.")
	stopifnot(
		spec$treat_type %in% c("first", "additional"),
		spec$comparison %in% c("main", "l6_l17", "corridors"),
		spec$spillover %in% c("none", "exclude_2_4km", "exclude_2_4km_and_gain"),
		isTRUE(spec$unbalanced),
		spec$base_period %in% c("universal", "varying"),
		spec$est_method %in% c("dr", "ipw", "reg"),
		is.finite(spec$anticipation), spec$anticipation >= 0,
		spec$anticipation == as.integer(spec$anticipation),
		inherits(spec$xformla[[1]], "formula"),
		spec$missing_covariates %in% c("audit_pending", "complete_case", "unknown_category")
	)
	if (spec$spillover != "none" && is.na(spec$spillover_flag)) {
		stop("Activated spillover specification requires an explicit control flag.")
	}
	if (spec$comparison != "main" && is.na(spec$control_flag)) {
		stop("Restricted comparison requires an independent baseline control flag.")
	}
	return(spec)
}

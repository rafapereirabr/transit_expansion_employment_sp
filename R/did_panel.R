# Helpers accept only explicitly supplied inputs; never discover restricted files.
require_did_columns <- function(data, required) {
	missing <- setdiff(required, names(data))
	if (length(missing)) stop("Missing DiD columns: ", paste(missing, collapse = ", "), ".")
	invisible(TRUE)
}

collect_did_data <- function(data) {
	if (is.character(data)) data <- arrow::open_dataset(data)
	if (!inherits(data, "data.frame")) data <- dplyr::collect(data)
	return(as.data.frame(data))
}

# membership is outcome-free, one row per CPF and focal cohort:
# cpf, risk_set_g, baseline_year, base_service, treat_year, prior_treat_year,
# elig_l6_l17. G is prospective at THIS risk set's fixed baseline address.
# prior_treat_year is the absorbing assignment across earlier eligible risk sets
# (0 if none); it is NOT the opening date of a historical nearby station.
# The current status_num/id_closest risk files do not yet satisfy this contract.
build_did_panel <- function(membership, outcomes, outcome_years) {
	membership <- collect_did_data(membership)
	outcomes <- collect_did_data(outcomes)
	require_did_columns(membership, c(
		"cpf", "risk_set_g", "baseline_year", "base_service", "treat_year",
		"prior_treat_year", "elig_l6_l17"
	))
	require_did_columns(outcomes, c("cpf", "risk_set_g", "year"))
	if (anyNA(membership$cpf) || anyNA(outcomes$cpf)) stop("Missing person key.")
	if (anyDuplicated(membership[c("cpf", "risk_set_g")])) {
		stop("Membership must be unique by person and risk set.")
	}
	if (anyDuplicated(outcomes[c("cpf", "risk_set_g", "year")])) {
		stop("Outcomes must be unique by person, risk set and year.")
	}
	if (anyNA(membership[c("risk_set_g", "baseline_year", "treat_year", "prior_treat_year")])) {
		stop("Treatment timing and baseline must be explicit; use 0 for no assignment.")
	}
	if (any(membership$baseline_year >= membership$risk_set_g) ||
		any(membership$prior_treat_year < 0) ||
		any(membership$prior_treat_year >= membership$risk_set_g &
			membership$prior_treat_year != 0)) {
		stop("Invalid baseline or prior assignment.")
	}
	# Keep movers and people absent from subsequent CadUnico snapshots.
	membership <- membership[membership$prior_treat_year == 0, , drop = FALSE]
	if (!nrow(membership)) stop("No eligible risk-set members.")
	if (anyNA(membership$base_service) ||
		any(!membership$base_service %in% c("first", "additional"))) {
		stop("base_service must classify both treated and controls.")
	}
	if (any(membership$treat_year != 0 & membership$treat_year < membership$risk_set_g)) {
		stop("Prospective treatment predates the focal cohort.")
	}
	outcome_years <- sort(unique(outcome_years))
	if (!length(outcome_years) || anyNA(outcome_years) ||
		any(outcome_years != as.integer(outcome_years)) ||
		any(diff(outcome_years) != 1L)) stop("Supply a consecutive annual outcome window.")
	if (any(!membership$baseline_year %in% outcome_years) ||
		any(!membership$risk_set_g %in% outcome_years)) stop("Window omits baseline or opening.")
	# Numeric IDs are shared across all cohorts; CPF never becomes a numeric identifier.
	membership$person_id <- match(membership$cpf, unique(membership$cpf))
	overlap <- intersect(setdiff(names(membership), c("cpf", "risk_set_g")), names(outcomes))
	if (length(overlap)) stop("Outcome table overwrites membership columns: ", paste(overlap, collapse = ", "))
	# Eligibility is fixed at baseline, but earlier annual outcomes remain available.
	panel <- tidyr::crossing(membership, year = outcome_years) |>
		dplyr::left_join(outcomes, by = c("cpf", "risk_set_g", "year"), relationship = "one-to-one")
	# Missing rows remain NA. Only the outcome builder may code a certified no-match as zero.
	return(panel)
}


# Adapter for one current risk_set records artifact. exposure is a separately
# audited, outcome-free person x focal-year table; nearest-station dates cannot
# substitute for its prospective and historical assignments.
build_did_membership <- function(risk_set, exposure, focal_year, baseline_year) {
	if (is.character(risk_set)) {
		if (!is.null(names(risk_set)) && "records" %in% names(risk_set)) {
			risk_set <- unname(risk_set["records"])
		} else if (length(risk_set) != 1L || grepl("audit", basename(risk_set))) {
			stop("Pass only the risk-set records artifact, never its audit file.")
		}
	}
	risk <- collect_did_data(risk_set)
	exposure <- collect_did_data(exposure)
	require_did_columns(risk, c("cpf", "year", "status_num"))
	require_did_columns(exposure, c(
		"cpf", "risk_set_g", "treat_year", "prior_treat_year", "elig_l6_l17"
	))
	stopifnot(
		length(focal_year) == 1L, length(baseline_year) == 1L,
		is.finite(focal_year), is.finite(baseline_year), baseline_year < focal_year
	)
	if (anyNA(risk[c("cpf", "year", "status_num")]) ||
		any(risk$year != baseline_year) || any(!risk$status_num %in% 0:6)) {
		stop("Invalid annual risk-set snapshot.")
	}
	if (anyDuplicated(risk$cpf) ||
		anyDuplicated(exposure[c("cpf", "risk_set_g")])) stop("Duplicate membership key.")
	risk <- risk[risk$status_num != 0L, , drop = FALSE]
	risk$baseline_year <- risk$year
	risk$year <- NULL
	risk$risk_set_g <- as.integer(focal_year)
	risk$base_service <- ifelse(risk$status_num %in% c(1L, 2L), "additional", "first")
	exposure <- exposure[exposure$risk_set_g == focal_year, , drop = FALSE]
	if (anyNA(exposure$cpf) || anyNA(exposure$risk_set_g)) stop("Missing exposure key.")
	conflicts <- intersect(setdiff(names(exposure), c("cpf", "risk_set_g")), names(risk))
	if (length(conflicts)) stop("Exposure overwrites risk-set attributes.")
	membership <- dplyr::left_join(
		risk, exposure, by = c("cpf", "risk_set_g"), relationship = "one-to-one"
	)
	if (anyNA(membership[c("treat_year", "prior_treat_year", "elig_l6_l17")])) {
		stop("Every retained member needs audited prospective exposure and prior assignment.")
	}
	is_focal <- membership$status_num %in% c(2L, 3L)
	if (any(is_focal != (membership$treat_year == focal_year))) {
		stop("Focal status conflicts with the audited first prospective exposure.")
	}
	# Status 1 may receive later additional service. Status 5 is not the full L6/L17
	# overlap indicator, and status 4's nearest station need not be its earliest opening.
	return(membership)
}

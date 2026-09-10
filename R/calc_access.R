# library(ggplot2)
# library(sf)
# source("R/utils.R")

# tar_load(grid_sf)
# tar_load(ttm_transit, branches = 1)
# # tar_load(ttm_bypass)
# grid <- grid_sf
# ttm_path <- ttm_transit

calc_access <- function(
	ttm,
	grid,
	year,
	crit_val = 60,
	group = NULL,
	method = c("cumulative_cutoff", "cumulative_interval", "decay_logistic"),
	...
) {
	rlang::arg_match(method)
	if(method == "decay_logistic" & !is.null(group)) {
		message("Ignoring `group` parameter since the decay_logistic method does not accept it.")
		group <- NULL
	}

	require(accessibility)

	if (inherits(ttm, "character") | inherits(ttm, "ArrowTabular")) {
		ttm <- arrow::open_dataset(ttm) |>
			collect()
	} #else if (!inherits(ttm, "ArrowTabular")) {
	# 	ttm <- arrow::as_arrow_table(ttm)
	# } else {
	# 	ttm <- ttm_path
	# }

	# in_all <- ttm |>
	# 	count(from_id, to_id) |>
	# 	filter(n == length(years)) |>
	# 	collect()

	# ttm <- ttm |>
	# 	filter(from_id %in% grid$id & to_id %in% grid$id) |>
	# 	semi_join(in_all, by = c("from_id", "to_id")) |>
	# 	collect()

	acc_args <- list(
		travel_matrix = ttm,
		land_use_data = grid,
		opportunity = "T001",
		travel_cost = "travel_time_p50",
		...
	)

	if (method %in% c("cumulative_cutoff", "decay_logistic")) {
		acc_args <- append(acc_args, list(cutoff = crit_val))
	} else {
		acc_args <- append(acc_args, list(interval = crit_val))
	}

	if(!is.null(group)) {
		acc_args <- append(acc_args, group)
	}

	ttm <- select(ttm, all_of(c(group, "from_id", "to_id", "travel_time_p50")))
	acc <- do.call(method, acc_args)

	access_name <- paste0(
		case_when(
			method == "cumulative_cutoff" ~ "CMATT",
			method == "cumulative_interval" ~ "CIATT",
			method == "decay_logistic" ~ "DLATT"
		),
		paste(crit_val, collapse = "-")
	)

	## tidy --- level data
	acc_df <- acc |>
		mutate(!!access_name := T001, year = lubridate::year(dep_datetime)) |>
		select(-T001)

	return(acc_df)
}


calc_access_change <- function(access_data) {
	stopifnot("year" %in% names(access_data))
	access_name <- grep("^\\w{2}ATT\\d{2}", names(access_data), value = T)
	access_sym <- rlang::sym(access_name)

	d1 <- paste0("delta_previous_", access_name)
	d1_pct <- paste0("delta_previous_", access_name, "_pct")
	d2 <- paste0("delta_", access_name)
	d2_pct <- paste0("delta_", access_name, "_pct")

	## Keep the cumulative 2012 comparison used downstream and add consecutive changes.
	acc_df <- access_data |>
		arrange(year, .by_group = FALSE) |>
		mutate(
			previous_year = lag(year),
			!!d1 := {{ access_sym }} - lag({{ access_sym }}),
			!!d1_pct := {{ access_sym }} / lag({{ access_sym }}) - 1,
			!!d2 := {{ access_sym }} - first({{ access_sym }}),
			!!d2_pct := {{ access_sym }} / first({{ access_sym }}) - 1,
			.by = "id"
		)

	delta_sym <- rlang::sym(d2)

  ## quantiles
	acc_quantiles <- acc_df |>
		select({{ delta_sym }}, id, year) |>
		filter(year == max(year)) |>
		mutate(
			!!paste0(d2, "quartile") := quantilize({{ delta_sym }}, n_qt = 4, na.rm = TRUE),
			!!paste0(d2, "decile") := quantilize({{ delta_sym }}, n_qt = 10, na.rm = TRUE)
		)

	acc_df <- left_join(
		acc_df,
		acc_quantiles,
		by = c("id", "year", "delta_CMATT120"),
		relationship = "one-to-one"
	)
}

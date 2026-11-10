# library(ggplot2)
# library(sf)
# source("R/utils.R")

# grid <- tar_read(grid_sf)
# ttm <- tar_read(ttm_transit, branches = 1)
# # ttm <- tar_read(ttm_bypass)

# # ttm_all <- list.files("_targets/objects", pattern = "ttm_transit_[^all_]", full.names = T) |>
# # 	arrow::open_dataset() |>
# # 	filter(from_id == "89a810071abffff")
# # ttm <- collect(ttm_all)

# year <- unique(ttm$year)
# crit_val = list(cutoff = 60)
# method = "cumulative_cutoff"
# group = NULL
# balance_origins = TRUE
# tar_load(balanced_units)
# ... = NULL

calc_access <- function(
	ttm,
	grid,
	crit_val = list(),
	method = c("cumulative_cutoff", "cumulative_interval", "gravity"),
	group = NULL,
	balance_origins = TRUE,
	balanced_units = NULL,
	...
) {
	## dealbreakers
	rlang::arg_match(method)
	if (balance_origins) {
		stopifnot(!is.null(balanced_units))
	}
	stopifnot(inherits(crit_val, "list"))
	if (method == "cumulative_cutoff") {
		stopifnot("cutoff" == names(crit_val))
	}
	if (method == "cumulative_interval") {
		stopifnot(c("interval", "interval_increment") %in% names(crit_val))
	}
	if (method == "gravity") {
		stopifnot(c("cutoff", "sd") %in% names(crit_val))
	}
	require(accessibility)

	## pre-filter data
	if (inherits(ttm, "character")) {
		ttm <- arrow::open_dataset(ttm) |>
			collect()
	}

	if (balance_origins) {
		ttm <- ttm |>
			filter(from_id %in% balanced_units)
	}

	## build arguments list
	acc_args <- list(
		travel_matrix = ttm,
		land_use_data = grid,
		opportunity = "T001",
		travel_cost = "travel_time_p50",
		...
	)
	rm(ttm)

	if (method == "gravity") {
		acc_args <- append(
			acc_args,
			list(decay_function = decay_logistic(crit_val[["cutoff"]], crit_val[["sd"]]))
		)
	} else {
		acc_args <- append(acc_args, crit_val)
	}
	if (!is.null(group)) {
		acc_args <- append(acc_args, list(group = group))
	}

	acc <- do.call(method, acc_args)

	method_abbr <- case_when(
		method == "cumulative_cutoff" ~ "CMATT",
		method == "cumulative_interval" ~ "CIATT",
		method == "gravity" ~ "GRATT"
	)

	method_alias <- suppressWarnings(case_when(
		method == "cumulative_cutoff" ~ paste0(method_abbr, crit_val[["cutoff"]]),
		method == "cumulative_interval" ~ paste0(method_abbr, mean(crit_val[["interval"]])),
		method == "gravity" ~ paste0(method_abbr, crit_val[["cutoff"]])
	))

	## tidy
	acc <- acc |>
		mutate(method = method_alias, accessibility = T001, year = lubridate::year(dep_datetime)) |>
		select(-T001) |>
		relocate(method, year, !!!group, id, accessibility)

	return(acc)
}

# crit_val <- list(
# 	list(cutoff = 60, sd = 5),
# 	list(cutoff = 45, sd = 3.75),
# 	list(cutoff = 30, sd = 2.5)
# 	# list(interval = c(30, 90), interval_increment = 15),
# 	# list(interval = c(45, 75), interval_increment = 15),
# 	# list(interval = c(45, 75), interval_increment = 5)
# )

# acc <- purrr::map(
# 	crit_val,
# 	\(x) calc_access(ttm, grid, crit_val = x, method = "gravity", balanced_units = balanced_units)
# )

# acc_df <- bind_rows(acc) |>
#   tidyr::pivot_longer(starts_with("CIATT"), names_to = "method", values_to = "accessibility") |>
#   filter(!is.na(accessibility)) |>
#   left_join(grid) |>
#   st_as_sf()

# acc_df |>
#   ggplot() +
#   # stat_ecdf(aes(x = accessibility, color = method))
#   geom_sf(aes(fill = accessibility), color = NA) +
#   scale_fill_viridis_c(option = "inferno") +
#   facet_wrap(vars(method)) +
#   theme_void()

# access delta -------------------------------------------------------------------------------

# we'll actually use that inside plot_access
# access_data <- tar_read(access)
# acc_delta <- calc_access_delta(access_data)

calc_access_delta <- function(access_data, quantiles = FALSE) {
	stopifnot("year" %in% names(access_data))

	## Keep the cumulative 2012 comparison used downstream and add consecutive changes.
	acc_df <- access_data |>
		arrange(year) |>
		mutate(
			d_last_access := accessibility - lag(accessibility),
			d_pct_last_access := accessibility / lag(accessibility) - 1,
			d_max_access := accessibility - first(accessibility),
			d_pct_max_access := accessibility / first(accessibility) - 1,
			.by = "id"
		)

	## quantiles
	if (quantiles) {
		y1 <- min(acc_df$year)
		acc_quantiles <- acc_df |>
			select(d_last_access, d_max_access, id, year) |>
			filter(year > y1) |>
			mutate(
				across(starts_with("d_"), ~ quantilize(.x, n_qt = 4, na.rm = T), .names = "quart_{.col}"),
				across(starts_with("d_"), ~ quantilize(.x, n_qt = 10, na.rm = T), .names = "dec_{.col}"),
				.by = "year"
			)

		acc_df <- left_join(
			acc_df,
			acc_quantiles,
			by = c("id", "year", "d_last_access", "d_max_access")
		)
	}

	return(acc_df)
}

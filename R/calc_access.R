# library(ggplot2)
# library(sf)
# source("R/utils.R")

# tar_load(grid_sf)
# tar_load(ttm_bypass)
# grid <- grid_sf
# # ttm_path <- ttm_transit_all
# ttm_path <- ttm_bypass

calc_access <- function(ttm_path, grid, years) {
	years <- sort(unique(as.integer(years)))
	stopifnot(identical(years, c(2012L, 2015L, 2019L, 2025L)))
	if (inherits(ttm_path, "character")) {
		ttm <- arrow::open_dataset(ttm_path)
	} else if (!inherits(ttm_path, "ArrowTabular")) {
		ttm <- arrow::as_arrow_table(ttm_path)
	} else {
		ttm <- ttm_path
	}

	in_all <- ttm |>
		count(from_id, to_id) |>
		filter(n == length(years)) |>
		collect()

	ttm <- ttm |>
		filter(from_id %in% grid$id & to_id %in% grid$id) |>
		semi_join(in_all, by = c("from_id", "to_id")) |>
		collect()

	acc <- accessibility::cumulative_cutoff(
		travel_matrix = ttm,
		land_use_data = grid,
		opportunity = "T001",
		travel_cost = "travel_time_p50",
		cutoff = 60,
		group_by = "year"
	)

	## tidy --- level data
	acc_df <- acc |>
		mutate(
			CMATT120 = T001,
			period = factor(
				year,
				levels = years,
				labels = c("Baseline (pre-2012)", "2015", "2019", "Post-2025")
			)
		) |>
		select(-T001)
	## Keep the cumulative 2012 comparison used downstream and add consecutive changes.
	acc_df <- acc_df |>
		arrange(year, .by_group = FALSE) |>
		mutate(
			previous_year = lag(year),
			delta_previous_CMATT120 = CMATT120 - lag(CMATT120),
			delta_previous_CMATT120_pct = CMATT120 / lag(CMATT120) - 1,
			delta_CMATT120 = CMATT120 - first(CMATT120),
			delta_CMATT120_pct = CMATT120 / first(CMATT120) - 1,
			.by = "id"
		)

	acc_quantiles <- acc_df |>
		select(delta_CMATT120, id, period) |>
		filter(period == "Post-2025") |>
		mutate(
			delta_CMATT120_quartile = quantilize(delta_CMATT120, n_qt = 4, na.rm = TRUE),
			delta_CMATT120_decile = quantilize(delta_CMATT120, n_qt = 10, na.rm = TRUE)
		)

	acc_df <- left_join(
		acc_df,
		acc_quantiles,
		by = c("id", "period", "delta_CMATT120"),
		relationship = "one-to-one"
	)

	return(acc_df)
}

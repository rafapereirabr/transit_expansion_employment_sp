library(ggplot2)
library(sf)
source("R/utils.R")

tar_load(grid_sf)
tar_load(ttm_bypass)
grid <- grid_sf
# ttm_path <- ttm_transit_all
ttm_path <- ttm_bypass

calc_access <- function(ttm_path, grid) {
	if (inherits(ttm_path, "character")) {
		ttm <- arrow::open_dataset(ttm_path)
	} else if (!inherits(individuals, "ArrowTabular")) {
		ttm <- arrow::as_arrow_table(ttm_path)
	}

	in_all <- ttm |>
		count(from_id, to_id) |>
		filter(n == 2) |>
		collect()

	ttm <- ttm |>
		filter(from_id %in% grid$id & to_id %in% grid$id) |>
		inner_join(in_all) |>
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
			period = factor(year, labels = c("Baseline (pre-2012)", "Post-2025")),
			.keep = "unused"
		)

	## tidy --- change
	acc_df <- acc_df |>
		arrange(period) |>
		mutate(
			delta_CMATT120 = CMATT120 - lag(CMATT120),
			delta_CMATT120_pct = CMATT120 / lag(CMATT120) - 1,
			.by = "id"
		)

	acc_quantiles <- acc_df |>
		select(delta_CMATT120, id, period) |>
		filter(period == "Post-2025") |>
		mutate(
			delta_CMATT120_quartile = quantilize(delta_CMATT120, n_qt = 4, na.rm = T),
			delta_CMATT120_decile = quantilize(delta_CMATT120, n_qt = 10, na.rm = T)
		)

	acc_df <- left_join(acc_df, acc_quantiles)

	return(acc_df)
}

# # treatment assignment

# library(sf)
# source("R/utils.R")

# tar_load(c(ttm_walk_stations, stations_sf, time_window))
# stations <- stations_sf
# ttm <- ttm_walk_stations

# time_cutoffs <- range(time_window) + c(0, 1)

# ttm <- ttm |>
# 	rename(code_station = to_id)
# stations <- sf_from_parquet(stations)
# stations <- stations |>
# 	select(code_station, code_line, label_line, yr_open, dt_trial_svc) |>
# 	mutate(code_station = as.character(code_station)) |>
# 	st_drop_geometry()

# ttm_min <- purrr::map(
# 	time_cutoffs,
# 	function(t) {
# 		ttm |>
# 			left_join(stations, by = "code_station") |>
# 			filter(yr_open < t) |>
# 			group_by(from_id) |>
# 			mutate(period = if_else(t == 2012, "Baseline (pre-2012)", "Post-2025")) |>
# 			slice_min(travel_time_p50)
# 	}
# ) |>
# 	bind_rows()

# ttm_min$geometry <- h3o::h3_from_strings(ttm_min$from_id) |>
# 	sf::st_as_sfc()

# ttm_min <- sf::st_as_sf(ttm_min)

# library(ggplot2)

# ttm_min |>
# 	arrange(period) |>
# 	mutate(delta = travel_time_p50 - lag(travel_time_p50)) |>
# 	filter(period == "Post-2025") |>
# 	ggplot() +
# 	geom_sf(
# 		aes(fill = as.numeric(delta)),
# 		color = NA,
# 		stroke = 0
# 	) +
# 	scale_fill_viridis_c(direction = -1, option = "inferno") +
# 	labs(
# 		title = "Walking time to closest station - 2025 vs 2011",
# 		fill = "Time\n(minutes)"
# 	) +
# 	theme_void()

# ggsave("sidequsts/walk_delta.png", dpi = 300, bg = "white")

# ttm_min |>
# 	ggplot() +
# 	geom_sf(
# 		aes(fill = as.numeric(travel_time_p50)),
# 		color = NA,
# 		stroke = 0
# 	) +
# 	scale_fill_viridis_c(direction = -1, option = "inferno") +
# 	labs(
# 		title = "Walking time to closest station - 2025 vs 2011",
# 		fill = "Time\n(minutes)"
# 	) +
# 	facet_wrap(vars(period)) +
# 	theme_void()

# library(mapview)
# mapviewOptions(platform = "leafgl")

# ttm_min |>
# 	mapview(zcol = "travel_time_p50", alpha.regions = .5, lwd = 0)

# readxl::read_excel("data/station_openings.xlsx", sheet = "data")

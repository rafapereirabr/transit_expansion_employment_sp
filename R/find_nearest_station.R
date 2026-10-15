# library(sf)
# source("R/utils.R")
# tar_load(c(ttm_walk_stations, stations_sf))
# ttm <- ttm_walk_stations

find_nearest_station <- function(ttm, stations_sf, year_cutoff = NULL, upper_as_control = T) {
	ttm <- arrow::open_dataset(ttm)
	stations_df <- sf_from_parquet(stations_sf, drop_geometry = T) |>
		select(code_station, code_line, name_station, yr_open) |>
		mutate(code_station = as.character(code_station))

	ttm <- ttm |>
		left_join(stations_df, by = c("to_id" = "code_station")) |>
		compute()

	## min times pt 1
	if (is.null(year_cutoff)) {
		min_times <- filter(ttm, !is.na(yr_open))
	} else {
		min_times <- filter(ttm, yr_open <= year_cutoff)
	}

	min_times <- collect(min_times) |>
		slice_min(travel_time_p50, by = from_id, with_ties = F)

	## min times pt 2
	if (upper_as_control) {
		min_times_2 <- filter(ttm, !(from_id %in% min_times$from_id)) |>
			mutate(yr_open = Inf) |>
			collect() |>
			slice_min(travel_time_p50, by = from_id, with_ties = F)

		min_times <- bind_rows(min_times, min_times_2)
	}

	min_times <- min_times |>
		rename(code_station = to_id, walk_time = travel_time_p50) |>
		select(from_id, code_line, code_station, name_station, yr_open, walk_time)

	return(min_times)
}

# min_times <- find_nearest_station(ttm_walk_stations, stations_sf, 2023)

# foo

library(sf)
source("R/utils.R")
tar_load(c(person_records, rais_records, ttm_walk_stations))

ttm <- arrow::open_dataset(ttm_walk_stations)
tar_load(stations_sf)
stations_sf <- sf_from_parquet(stations_sf)

stations_df <- st_drop_geometry(stations_sf) |>
	select(code_station, yr_open) |>
	mutate(code_station = as.character(code_station))

ttm_2 <- ttm |>
	left_join(stations_df, by = c("to_id" = "code_station")) |>
	compute()

ttm_3 <- ttm_2 |>
	filter(!is.na(yr_open)) |>
	select(from_id, yr_open, travel_time_p50) |>
	collect()

yrs <- c(2011, 2015, 2019, 2023)

min_times <- purrr::map(
	yrs,
	function(x) {
		ttm_3 |>
			filter(yr_open <= x) |>
			summarise(
				year = x,
				yr_open = yr_open[which.min(travel_time_p50)],
				time = min(travel_time_p50),
				.by = from_id,
			)
	}
) |>
	bind_rows()

min_times |>
	filter(year == 2023) |>
	mutate(
		era = case_when(
			yr_open < 2011 ~ "Always treated",
			TRUE ~ as.character(yr_open)
		)
	) |>
	count(era, sort = T)


pnt <- min_times |>
	group_by(year) |>
	summarise(
		pnt_15 = sum(time <= 15),
		pnt_30 = sum(time <= 30),
		pnt_45 = sum(time <= 45),
		pnt_60 = sum(time <= 60)
	)

library(ggplot2)
min_times |>
	mutate(year = factor(year)) |>
	ggplot() +
	stat_ecdf(aes(x = yr_open, color = year), alpha = 0.55) +
	# stat_bin(aes(x = time, color = year, fill = year), bins = 12, alpha = 0.55) +
	# facet_wrap(vars(year)) +
	theme_minimal()

min_times |>
	summarise(
		time_q1 = quantile(time, 0.25),
		time_q2 = quantile(time, 0.5),
		time_q3 = quantile(time, 0.75),
		time_p95 = quantile(time, 0.95),
		.by = year
	)

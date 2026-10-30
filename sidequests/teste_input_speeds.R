# script aim: fixing speeds in the 2012 feed with data from the more realistic 2025 feed.

library(gtfstools)
library(dplyr)
library(purrr)

## read & tidy feeds
feed_paths <- c(
	"2012" = "data-raw/gtfs_sptrans_2012.zip", 
	"2015" = "data-raw/gtfs_sptrans_2015.zip",
	"2025" = "data-raw/gtfs_sptrans_20250604_07.zip"
)
# feed_path <- feed_paths[1]
# purrr::map(feeds, gtfstools::get_summary)

feed_wizard <- function(feed_path) {
	feed <- gtfstools::read_gtfs(feed_path)
	### filter morning peak time only (we don't need the rest for our purposes)
	feed <- gtfstools::filter_by_time_of_day(feed, from = "06:00:00", to = "06:59:59", full_trips = T)

	### annoingly, feeds start in 2008, this workaround fixes this for our purposes
	feed$calendar <- feed$calendar |>
		mutate(start_date = lubridate::add_with_rollback(end_date, -months(12)))

  ### assume block time is valid, ignore the middle, and interpolate
  feed$stop_times <- feed$stop_times |>
	mutate(
		across(
			c(departure_time, arrival_time),
			~ if_else(stop_sequence %in% c(1, max(stop_sequence)), .x, "")
		),
		.by = trip_id
	)
  feed <- interpolate_stop_times(feed)
  feed <- interpolate_shape_dist_traveled(feed)
  
	return(feed)
}

feeds <- map(feed_paths, feed_wizard)
feeds_combined <- merge_gtfs(feeds, prefix = names(feeds))
### some stops are in practice the same, even though they change the name from one feed to the other
feeds_combined <- combine_nearby_stops(feeds_combined, 4)


## calculate speeds

### audit - feeds were made with constant travel times between stops.
### we assume block times are accurate in 2025 (gps2gtfs), so we'll estimate speeds based on that year
audit_speeds <- function(feed) {
  speeds <- get_trip_speed(feed)
  lengths <- get_trip_length(feed)
  durations <- get_trip_duration(feed)
	stops <- summarise(feed$stop_times, stops = max(stop_sequence), .by = trip_id)
  data <- left_join(speeds, lengths, by = "trip_id") |>
		left_join(durations, by = "trip_id") |>
		left_join(stops, by = "trip_id") |>
		mutate(avg_time_btw_stops = round(60*duration / stops)) |> 
  	tidyr::separate_wider_delim(route_id, delim = "_", names = c("year", "route_id")) |> 
  	arrange(route_id, year)
	return(data)
}

trips <- outer(c("2012", "2015", "2025"), c("1022-10-0", "408A-10-0", "1016-10-0"), paste, sep = "_") |> 
  as.vector()
mini_feed <- feeds_combined |> filter_by_trip_id(trips)
# mini_feed |> interpolate_shape_dist_traveled()
mini_audit <- audit_speeds(mini_feed)
audit <- audit_speeds(feeds_combined)

library(ggplot2)
ggplot(audit) +
	# stat_bin(aes(x = avg_time_btw_stops, fill = year), bins = 30) +
	stat_ecdf(aes(x = avg_time_btw_stops, fill = year, color = year)) +
  # scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Set3") 

ggplot(audit) +
	stat_density(aes(x = speed, fill = year, color = year), alpha = 0.5) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Set3") 

### vector of start and end dates
dates <- feeds_combined$calendar |>
	distinct(start_date, end_date)
start_dates <- pull(dates, start_date)
end_dates <- pull(dates, end_date)

speeds_2025 <- feeds_combined |>
	gtfstools::filter_by_period(start_date = start_dates[3], end_date = end_dates[3]) |>
	gtfstools::get_trip_speed(by = "segment")

mean_speeds_2025 <- speeds_2025 |> 
  summarise(speed = mean(speed, na.rm = T), .by = c("from_stop_id", "to_stop_id"))

speeds_2012 <- feeds_combined |>
	gtfstools::filter_by_period(start_date = start_dates[1], end_date = end_dates[1]) |>
	gtfstools::get_trip_speed(by = "segment")

mean_speeds_2012 <- speeds_2012 |> 
  summarise(speed = mean(speed, na.rm = T), .by = c("from_stop_id", "to_stop_id"))

filter(speeds_2025, is.na(speed))
filter(mean_speeds_2025, is.na(speed))

full_join(mean_speeds_2012, rename(mean_speeds_2025, new_speed = speed))
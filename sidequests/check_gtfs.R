# Setup --------------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(gtfstools)
  library(h3o)
  library(purrr)
  library(r5r)
  library(sf)
  library(stringr)
  library(tidyr)
})

source("R/gtfs.R")

if (packageVersion("r5r") < "2.4.0") {
  stop("Run this script in the project renv: the cached networks require r5r >= 2.4.0.")
}

output_dir <- "sidequests/check_gtfs_output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

counterfactual_dir <- file.path(output_dir, "r5_2025_l3_37min")
counterfactual_feed <- file.path(counterfactual_dir, "gtfs_sptrans_2025_l3_37min.zip")
counterfactual_runtime <- 37 * 60

l15_counterfactual_dir <- file.path(output_dir, "r5_2025_l15_179sec")
l15_counterfactual_feed <- file.path(
  l15_counterfactual_dir,
  "gtfs_sptrans_2025_l15_179sec.zip"
)
l15_counterfactual_headway <- 179L
l15_query_time <- 6L * 3600L + 50L * 60L
l15_query_end <- l15_query_time + 15L * 60L

routing_spec <- tibble::tribble(
  ~year, ~date, ~datetime, ~feed_path, ~network_path,
  2012L, "2012-04-10", "2012-04-10 06:50:00",
  "data/gtfs/processed/gtfs_sptrans_2012.zip", "data/r5/2012/network.dat",
  2025L, "2025-04-08", "2025-04-08 06:50:00",
  "data/gtfs/processed/gtfs_sptrans_2025.zip", "data/r5/2025/network.dat"
) |>
  mutate(
    date = as.Date(date),
    datetime = as.POSIXct(datetime, tz = "America/Sao_Paulo")
  )

rail_route_types <- c(1L, 2L)
sapopemba_id <- "89a81008e8bffff"
sapopemba_h3 <- h3_from_strings(sapopemba_id)
hive_h3 <- grid_disk(sapopemba_h3, k = 3L)[[1]]
sapopemba_hive <- tibble::tibble(
  id = as.character(hive_h3),
  ring = grid_distance(rep(sapopemba_h3, length(hive_h3)), hive_h3),
  geometry = st_as_sfc(hive_h3)
) |>
  st_as_sf(crs = 4326)

# General helpers ----------------------------------------------------------------------------

line_code <- function(route_id) {
  code <- str_extract(route_id, "L\\d+")
  case_when(
    !is.na(code) ~ paste0("L", as.integer(str_remove(code, "L"))),
    str_detect(route_id, "15") ~ "L15",
    TRUE ~ route_id
  )
}

haversine_m <- function(lon1, lat1, lon2, lat2) {
  radius <- 6371000
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  delta_phi <- (lat2 - lat1) * pi / 180
  delta_lambda <- (lon2 - lon1) * pi / 180
  a <- sin(delta_phi / 2)^2 + cos(phi1) * cos(phi2) * sin(delta_lambda / 2)^2
  2 * radius * atan2(sqrt(a), sqrt(1 - a))
}

frequency_weights <- function(frequencies, lower = -Inf, upper = Inf) {
  frequencies |>
    mutate(
      start_seconds = gtfs_time_to_seconds(start_time),
      end_seconds = gtfs_time_to_seconds(end_time),
      lower_seconds = pmax(start_seconds, lower),
      upper_seconds = pmin(end_seconds, upper),
      departures = pmax(
        0,
        ceiling((upper_seconds - start_seconds) / headway_secs) -
          pmax(0, ceiling((lower_seconds - start_seconds) / headway_secs))
      ),
      duration = pmax(0, upper_seconds - lower_seconds)
    ) |>
    filter(duration > 0, departures > 0)
}

# GTFS audits --------------------------------------------------------------------------------

audit_rail_lines <- function(spec) {
  gtfs <- read_gtfs(spec$feed_path)
  service_ids <- gtfs_active_service_ids(gtfs, spec$date)
  routes <- gtfs$routes |>
    filter(route_type %in% rail_route_types) |>
    select(route_id, route_short_name, route_long_name, route_type)
  trips <- gtfs$trips |>
    filter(service_id %in% service_ids) |>
    inner_join(routes, by = "route_id")
  frequencies <- gtfs$frequencies |>
    inner_join(trips |> select(trip_id, route_id, direction_id), by = "trip_id")
  speeds <- get_trip_speed(gtfs, trip_id = unique(trips$trip_id), file = "shapes") |>
    as_tibble() |>
    select(trip_id, speed)

  summarize_window <- function(lower = -Inf, upper = Inf) {
    frequency_weights(frequencies, lower, upper) |>
      left_join(speeds, by = "trip_id") |>
      group_by(route_id, direction_id, trip_id) |>
      summarise(
        span = sum(duration),
        departures = sum(departures),
        headway_seconds = sum(headway_secs * duration) / sum(duration),
        speed_kmh = first(speed),
        .groups = "drop"
      ) |>
      group_by(route_id) |>
      summarise(
        headway_minutes = weighted.mean(headway_seconds, span) / 60,
        speed_kmh = weighted.mean(speed_kmh, departures, na.rm = TRUE),
        departures = sum(departures),
        directions = n_distinct(direction_id),
        .groups = "drop"
      )
  }

  day <- summarize_window() |>
    rename_with(~ paste0(.x, "_day"), -route_id)
  hpm <- summarize_window(6 * 3600, 7 * 3600) |>
    rename_with(~ paste0(.x, "_hpm"), -route_id)

  routes |>
    left_join(day, by = "route_id") |>
    left_join(hpm, by = "route_id") |>
    mutate(
      year = spec$year,
      mode = if_else(route_type == 1L, "Metro", "Train"),
      line = line_code(route_id),
      across(
        c(headway_minutes_day, headway_minutes_hpm, speed_kmh_day, speed_kmh_hpm),
        ~ round(.x, 1)
      )
    ) |>
    relocate(year, mode, line)
}

audit_rail_segments <- function(spec) {
  gtfs <- read_gtfs(spec$feed_path)
  service_ids <- gtfs_active_service_ids(gtfs, spec$date)
  rail_routes <- gtfs$routes |>
    filter(route_type %in% rail_route_types)
  trips <- gtfs$trips |>
    filter(service_id %in% service_ids) |>
    semi_join(rail_routes, by = "route_id") |>
    group_by(route_id, direction_id) |>
    slice_head(n = 1L) |>
    ungroup()

  gtfs$stop_times |>
    semi_join(trips, by = "trip_id") |>
    left_join(trips |> select(trip_id, route_id, direction_id), by = "trip_id") |>
    left_join(gtfs$stops |> select(stop_id, stop_name, stop_lon, stop_lat), by = "stop_id") |>
    arrange(route_id, direction_id, stop_sequence) |>
    group_by(route_id, direction_id) |>
    mutate(
      next_stop_id = lead(stop_id),
      next_stop_name = lead(stop_name),
      next_lon = lead(stop_lon),
      next_lat = lead(stop_lat),
      departure_seconds = gtfs_time_to_seconds(departure_time),
      next_arrival_seconds = lead(gtfs_time_to_seconds(arrival_time)),
      travel_minutes = (next_arrival_seconds - departure_seconds) / 60,
      distance_km = haversine_m(stop_lon, stop_lat, next_lon, next_lat) / 1000,
      segment_speed_kmh = distance_km / (travel_minutes / 60)
    ) |>
    filter(!is.na(next_stop_id)) |>
    ungroup() |>
    transmute(
      year = spec$year,
      mode = if_else(
        route_id %in% rail_routes$route_id[rail_routes$route_type == 1L],
        "Metro", "Train"
      ),
      line = line_code(route_id), route_id, direction_id, stop_sequence,
      from_stop_id = stop_id, from_stop = stop_name,
      to_stop_id = next_stop_id, to_stop = next_stop_name,
      travel_minutes = round(travel_minutes, 2),
      distance_km = round(distance_km, 3),
      segment_speed_kmh = round(segment_speed_kmh, 1)
    )
}

audit_rail_transfers <- function(spec, max_distance_m = 300) {
  gtfs <- read_gtfs(spec$feed_path)
  rail_routes <- gtfs$routes |>
    filter(route_type %in% rail_route_types)
  route_stops <- gtfs$trips |>
    semi_join(rail_routes, by = "route_id") |>
    select(route_id, trip_id) |>
    inner_join(gtfs$stop_times |> select(trip_id, stop_id), by = "trip_id") |>
    distinct(route_id, stop_id) |>
    left_join(gtfs$stops |> select(stop_id, stop_name, stop_lon, stop_lat), by = "stop_id")

  crossing(
    route_stops |> rename_with(~ paste0(.x, "_from")),
    route_stops |> rename_with(~ paste0(.x, "_to"))
  ) |>
    filter(route_id_from < route_id_to) |>
    mutate(
      distance_m = haversine_m(stop_lon_from, stop_lat_from, stop_lon_to, stop_lat_to)
    ) |>
    filter(distance_m <= max_distance_m) |>
    group_by(route_id_from, route_id_to) |>
    slice_min(distance_m, n = 1L, with_ties = FALSE) |>
    ungroup() |>
    transmute(
      year = spec$year,
      line_from = line_code(route_id_from), route_id_from,
      stop_from = stop_name_from,
      line_to = line_code(route_id_to), route_id_to,
      stop_to = stop_name_to,
      distance_m = round(distance_m, 1)
    ) |>
    arrange(distance_m)
}

audit_bus_hive <- function(spec, hive) {
  gtfs <- read_gtfs(spec$feed_path)
  service_ids <- gtfs_active_service_ids(gtfs, spec$date)
  bus_routes <- gtfs$routes |>
    filter(route_type == 3L)
  active_trips <- gtfs$trips |>
    filter(service_id %in% service_ids) |>
    semi_join(bus_routes, by = "route_id")

  stop_points <- gtfs$stops |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)
  local_stop_ids <- stop_points$stop_id[
    lengths(st_intersects(stop_points, st_union(hive))) > 0L
  ]
  local_trips <- active_trips |>
    semi_join(
      gtfs$stop_times |> filter(stop_id %in% local_stop_ids),
      by = "trip_id"
    )

  trip_speeds <- get_trip_speed(
    gtfs,
    trip_id = unique(local_trips$trip_id),
    file = "shapes"
  ) |>
    as_tibble() |>
    select(trip_id, speed_kmh = speed)

  service <- gtfs$frequencies |>
    inner_join(
      local_trips |> select(trip_id, route_id, direction_id),
      by = "trip_id"
    ) |>
    frequency_weights(l15_query_time, l15_query_end) |>
    left_join(trip_speeds, by = "trip_id") |>
    group_by(route_id, direction_id, trip_id) |>
    summarise(
      departures = sum(departures),
      headway_minutes = weighted.mean(headway_secs, duration) / 60,
      speed_kmh = first(speed_kmh),
      .groups = "drop"
    )

  local_stops_by_route <- gtfs$stop_times |>
    filter(stop_id %in% local_stop_ids) |>
    inner_join(local_trips |> select(trip_id, route_id), by = "trip_id") |>
    distinct(route_id, stop_id) |>
    count(route_id, name = "local_stops")

  service |>
    group_by(route_id) |>
    summarise(
      directions = n_distinct(direction_id),
      trip_patterns = n_distinct(trip_id),
      departures_15min = sum(departures),
      headway_minutes = weighted.mean(headway_minutes, departures),
      speed_kmh = weighted.mean(speed_kmh, departures, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(local_stops_by_route, by = "route_id") |>
    left_join(
      bus_routes |>
        select(route_id, route_short_name, route_long_name),
      by = "route_id"
    ) |>
    mutate(
      year = spec$year,
      headway_minutes = round(headway_minutes, 1),
      speed_kmh = round(speed_kmh, 1)
    ) |>
    relocate(year, route_id, route_short_name, route_long_name) |>
    arrange(route_short_name, route_id)
}

# Line 3 counterfactual ----------------------------------------------------------------------

seconds_to_gtfs_time <- function(seconds) {
  seconds <- round(seconds)
  sprintf(
    "%02d:%02d:%02d",
    seconds %/% 3600,
    (seconds %% 3600) %/% 60,
    seconds %% 60
  )
}

set_route_runtime <- function(gtfs, route_id, runtime_seconds) {
  trip_ids <- gtfs$trips |>
    filter(.data$route_id == .env$route_id) |>
    pull(trip_id)

  ## Frequency-based feeds store one timetable template per trip. Scaling offsets within each
  ## template changes in-vehicle time while leaving frequencies and departure windows untouched.
  timing <- gtfs$stop_times |>
    filter(trip_id %in% trip_ids) |>
    arrange(trip_id, stop_sequence) |>
    mutate(
      arrival_seconds = gtfs_time_to_seconds(arrival_time),
      departure_seconds = gtfs_time_to_seconds(departure_time)
    ) |>
    group_by(trip_id) |>
    summarise(
      first_departure = first(departure_seconds),
      original_runtime = last(arrival_seconds) - first_departure,
      scale = runtime_seconds / original_runtime,
      .groups = "drop"
    )

  stopifnot(nrow(timing) > 0L, all(timing$original_runtime > 0))

  gtfs$stop_times <- gtfs$stop_times |>
    left_join(timing, by = "trip_id") |>
    mutate(
      arrival_seconds = gtfs_time_to_seconds(arrival_time),
      departure_seconds = gtfs_time_to_seconds(departure_time),
      arrival_time = if_else(
        is.na(scale), arrival_time,
        seconds_to_gtfs_time(first_departure + (arrival_seconds - first_departure) * scale)
      ),
      departure_time = if_else(
        is.na(scale), departure_time,
        seconds_to_gtfs_time(first_departure + (departure_seconds - first_departure) * scale)
      )
    ) |>
    select(-first_departure, -original_runtime, -scale, -arrival_seconds, -departure_seconds)

  gtfs
}

prepare_l3_counterfactual <- function(
  feed_path,
  source_network_dir,
  output_dir,
  output_feed,
  runtime_seconds
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  resources <- c("street_network.osm.pbf", "elevation.tif")
  resource_paths <- file.path(source_network_dir, resources)
  stopifnot(all(file.exists(resource_paths)))
  file.copy(resource_paths, output_dir, overwrite = FALSE)

  gtfs <- read_gtfs(feed_path)
  l3_route <- gtfs$routes |>
    filter(line_code(route_id) == "L3", route_type %in% rail_route_types) |>
    pull(route_id)
  stopifnot(length(l3_route) == 1L)

  gtfs <- set_route_runtime(gtfs, l3_route, runtime_seconds)
  write_gtfs(gtfs, output_feed, overwrite = TRUE)
  output_feed
}

set_route_headway_in_window <- function(
  gtfs,
  route_id,
  window_start,
  window_end,
  headway_seconds
) {
  trip_ids <- gtfs$trips |>
    filter(.data$route_id == .env$route_id) |>
    pull(trip_id)

  active_frequency <- gtfs$frequencies$trip_id %in% trip_ids &
    gtfs_time_to_seconds(gtfs$frequencies$start_time) < window_end &
    gtfs_time_to_seconds(gtfs$frequencies$end_time) > window_start
  stopifnot(any(active_frequency))

  ## Only frequency entries overlapping the 15-minute routing window are changed. Other service
  ## periods remain exactly as supplied because this experiment concerns accessibility at 06:50.
  gtfs$frequencies$headway_secs[active_frequency] <- headway_seconds
  gtfs
}

prepare_l15_counterfactual <- function(
  feed_path,
  source_network_dir,
  output_dir,
  output_feed,
  window_start,
  window_end,
  headway_seconds
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  resources <- c("street_network.osm.pbf", "elevation.tif")
  resource_paths <- file.path(source_network_dir, resources)
  stopifnot(all(file.exists(resource_paths)))
  file.copy(resource_paths, output_dir, overwrite = FALSE)

  gtfs <- read_gtfs(feed_path)
  l15_route <- gtfs$routes |>
    filter(line_code(route_id) == "L15", route_type %in% rail_route_types) |>
    pull(route_id)
  stopifnot(length(l15_route) == 1L)

  gtfs <- set_route_headway_in_window(
    gtfs, l15_route, window_start, window_end, headway_seconds
  )
  write_gtfs(gtfs, output_feed, overwrite = TRUE)
  output_feed
}

# Run tabular audits -------------------------------------------------------------------------

line_audit <- map_dfr(seq_len(nrow(routing_spec)), ~ audit_rail_lines(routing_spec[.x, ]))
segment_audit <- map_dfr(seq_len(nrow(routing_spec)), ~ audit_rail_segments(routing_spec[.x, ]))
transfer_audit <- map_dfr(seq_len(nrow(routing_spec)), ~ audit_rail_transfers(routing_spec[.x, ]))
bus_hive_audit <- map_dfr(
  seq_len(nrow(routing_spec)),
  ~ audit_bus_hive(routing_spec[.x, ], sapopemba_hive)
)
bus_hive_summary <- bus_hive_audit |>
  group_by(year) |>
  summarise(
    routes = n_distinct(route_id),
    trip_patterns = sum(trip_patterns),
    median_headway_minutes = median(headway_minutes),
    weighted_speed_kmh = weighted.mean(speed_kmh, departures_15min, na.rm = TRUE),
    departures_15min = sum(departures_15min),
    route_stop_pairs = sum(local_stops),
    .groups = "drop"
  ) |>
  mutate(across(c(median_headway_minutes, weighted_speed_kmh), ~ round(.x, 1)))
bus_hive_comparison <- bus_hive_audit |>
  select(
    year, route_short_name, route_long_name, departures_15min,
    headway_minutes, speed_kmh, local_stops
  ) |>
  pivot_wider(
    names_from = year,
    values_from = -route_short_name,
    names_glue = "{.value}_{year}"
  ) |>
  mutate(
    status = case_when(
      is.na(route_long_name_2025) ~ "only_2012",
      is.na(route_long_name_2012) ~ "only_2025",
      TRUE ~ "both"
    ),
    delta_headway_minutes = headway_minutes_2025 - headway_minutes_2012,
    delta_speed_kmh = speed_kmh_2025 - speed_kmh_2012,
    delta_departures_15min = departures_15min_2025 - departures_15min_2012
  ) |>
  arrange(status, route_short_name)

runtime_audit <- segment_audit |>
  group_by(year, mode, line, route_id, direction_id) |>
  summarise(
    stops = n() + 1L,
    runtime_minutes = sum(travel_minutes),
    distance_km = sum(distance_km),
    average_speed_kmh = distance_km / (runtime_minutes / 60),
    .groups = "drop"
  ) |>
  mutate(
    runtime_minutes = round(runtime_minutes, 1),
    distance_km = round(distance_km, 1),
    average_speed_kmh = round(average_speed_kmh, 1)
  ) |>
  arrange(line, year, direction_id)

write.csv(line_audit, file.path(output_dir, "rail_line_audit.csv"), row.names = FALSE)
write.csv(segment_audit, file.path(output_dir, "rail_segment_audit.csv"), row.names = FALSE)
write.csv(runtime_audit, file.path(output_dir, "rail_runtime_audit.csv"), row.names = FALSE)
write.csv(transfer_audit, file.path(output_dir, "rail_transfer_audit.csv"), row.names = FALSE)
write.csv(bus_hive_audit, file.path(output_dir, "sapopemba_bus_routes.csv"), row.names = FALSE)
write.csv(bus_hive_summary, file.path(output_dir, "sapopemba_bus_summary.csv"), row.names = FALSE)
write.csv(
  bus_hive_comparison,
  file.path(output_dir, "sapopemba_bus_comparison.csv"),
  row.names = FALSE
)
write.csv(
  st_drop_geometry(sapopemba_hive),
  file.path(output_dir, "sapopemba_hive.csv"),
  row.names = FALSE
)

# Compare raw and processed Line 3 -----------------------------------------------------------

feed_stage_spec <- tibble::tribble(
  ~year, ~stage, ~date, ~feed_path,
  2012L, "raw", "2012-04-10", "data-raw/gtfs_sptrans_2012.zip",
  2012L, "processed", "2012-04-10", "data/gtfs/processed/gtfs_sptrans_2012.zip",
  2025L, "raw", "2025-04-08", "data-raw/gtfs_sptrans_2025.zip",
  2025L, "processed", "2025-04-08", "data/gtfs/processed/gtfs_sptrans_2025.zip"
) |>
  mutate(date = as.Date(date))

l3_feed_stage_audit <- map_dfr(seq_len(nrow(feed_stage_spec)), function(index) {
  spec <- feed_stage_spec[index, ]
  audit_rail_segments(spec) |>
    filter(line == "L3") |>
    group_by(year, direction_id) |>
    summarise(
      stops = n() + 1L,
      runtime_minutes = sum(travel_minutes),
      distance_km = sum(distance_km),
      .groups = "drop"
    ) |>
    mutate(stage = spec$stage, .after = year)
}) |>
  arrange(year, stage, direction_id)

# Build isolated 2025 counterfactual ---------------------------------------------------------

counterfactual_network <- file.path(counterfactual_dir, "network.dat")
counterfactual_settings_path <- file.path(counterfactual_dir, "settings.rds")
source_2025_feed <- routing_spec$feed_path[routing_spec$year == 2025L]
counterfactual_settings <- list(
  source_feed_md5 = unname(tools::md5sum(source_2025_feed)),
  runtime_seconds = counterfactual_runtime
)
cached_settings <- if (file.exists(counterfactual_settings_path)) {
  readRDS(counterfactual_settings_path)
} else {
  NULL
}
rebuild_counterfactual <- !file.exists(counterfactual_network) ||
  !identical(counterfactual_settings, cached_settings)

## Rebuild only when the source feed or desired runtime changes. This keeps routine audits fast.
if (rebuild_counterfactual) {
  prepare_l3_counterfactual(
    feed_path = source_2025_feed,
    source_network_dir = dirname(routing_spec$network_path[routing_spec$year == 2025L]),
    output_dir = counterfactual_dir,
    output_feed = counterfactual_feed,
    runtime_seconds = counterfactual_runtime
  )
  network <- build_network(counterfactual_dir, overwrite = TRUE, verbose = TRUE)
  stop_r5(network)
  saveRDS(counterfactual_settings, counterfactual_settings_path)
}

counterfactual_audit <- audit_rail_segments(tibble::tibble(
  year = 2025L,
  date = as.Date("2025-04-08"),
  feed_path = counterfactual_feed
)) |>
  filter(line == "L3") |>
  group_by(year, direction_id) |>
  summarise(
    stops = n() + 1L,
    runtime_minutes = sum(travel_minutes),
    distance_km = sum(distance_km),
    .groups = "drop"
  ) |>
  mutate(stage = "counterfactual_37min", .after = year)

l3_feed_stage_audit <- bind_rows(l3_feed_stage_audit, counterfactual_audit) |>
  arrange(year, stage, direction_id)
write.csv(
  l3_feed_stage_audit,
  file.path(output_dir, "l3_feed_stage_audit.csv"),
  row.names = FALSE
)

# Build isolated Line 15 headway counterfactual ----------------------------------------------

l15_counterfactual_network <- file.path(l15_counterfactual_dir, "network.dat")
l15_settings_path <- file.path(l15_counterfactual_dir, "settings.rds")
l15_settings <- list(
  source_feed_md5 = unname(tools::md5sum(source_2025_feed)),
  window_start = l15_query_time,
  window_end = l15_query_end,
  headway_seconds = l15_counterfactual_headway
)
l15_cached_settings <- if (file.exists(l15_settings_path)) {
  readRDS(l15_settings_path)
} else {
  NULL
}
rebuild_l15_counterfactual <- !file.exists(l15_counterfactual_network) ||
  !identical(l15_settings, l15_cached_settings)

if (rebuild_l15_counterfactual) {
  prepare_l15_counterfactual(
    feed_path = source_2025_feed,
    source_network_dir = dirname(routing_spec$network_path[routing_spec$year == 2025L]),
    output_dir = l15_counterfactual_dir,
    output_feed = l15_counterfactual_feed,
    window_start = l15_query_time,
    window_end = l15_query_end,
    headway_seconds = l15_counterfactual_headway
  )
  network <- build_network(l15_counterfactual_dir, overwrite = TRUE, verbose = TRUE)
  stop_r5(network)
  saveRDS(l15_settings, l15_settings_path)
}

l15_headway_audit <- bind_rows(
  audit_rail_lines(routing_spec |> filter(year == 2025L)) |>
    filter(line == "L15") |>
    mutate(scenario = "2025"),
  audit_rail_lines(tibble::tibble(
    year = 2025L,
    date = as.Date("2025-04-08"),
    feed_path = l15_counterfactual_feed
  )) |>
    filter(line == "L15") |>
    mutate(scenario = "2025_l15_179sec")
) |>
  select(scenario, everything())
write.csv(
  l15_headway_audit,
  file.path(output_dir, "l15_headway_audit.csv"),
  row.names = FALSE
)

scenario_spec <- bind_rows(
  routing_spec |> mutate(scenario = as.character(year)),
  routing_spec |>
    filter(year == 2025L) |>
    mutate(
      scenario = "2025_l3_37min",
      feed_path = counterfactual_feed,
      network_path = counterfactual_network
    ),
  routing_spec |>
    filter(year == 2025L) |>
    mutate(
      scenario = "2025_l15_179sec",
      feed_path = l15_counterfactual_feed,
      network_path = l15_counterfactual_network
    )
)

# Mini routing experiment --------------------------------------------------------------------

mini_od <- sapopemba_hive |>
  mutate(
    origin_label = if_else(ring == 0L, "Sapopemba", paste0("Ring ", ring)),
    geometry = st_centroid(geometry)
  )

grid <- readRDS("_targets/objects/grid_sf") |>
  rename(id = h3_address) |>
  st_centroid()

route_scenario <- function(spec) {
  network <- build_network(dirname(spec$network_path), overwrite = FALSE)
  on.exit(stop_r5(network), add = TRUE)
  travel_time_matrix(
    r5r_network = network,
    origins = mini_od,
    destinations = grid,
    mode = "TRANSIT",
    departure_datetime = spec$datetime,
    time_window = 15L,
    max_trip_duration = 60L,
    n_threads = 4L,
    verbose = TRUE
  ) |>
    mutate(year = spec$year, scenario = spec$scenario)
}

mini_ttm <- map_dfr(seq_len(nrow(scenario_spec)), ~ route_scenario(scenario_spec[.x, ]))
write_parquet(mini_ttm, file.path(output_dir, "mini_ttm.parquet"))

mini_access <- mini_ttm |>
  count(scenario, year, from_id, name = "accessible_hexagons") |>
  left_join(mini_od |> st_drop_geometry() |> select(from_id = id, origin_label), by = "from_id") |>
  arrange(origin_label, scenario)
write.csv(mini_access, file.path(output_dir, "mini_accessibility.csv"), row.names = FALSE)

hive_access_summary <- mini_access |>
  group_by(scenario, year) |>
  summarise(
    origins = n(),
    mean_accessible = mean(accessible_hexagons),
    median_accessible = median(accessible_hexagons),
    p10_accessible = quantile(accessible_hexagons, 0.1),
    p90_accessible = quantile(accessible_hexagons, 0.9),
    .groups = "drop"
  ) |>
  mutate(across(ends_with("accessible"), ~ round(.x, 1)))
write.csv(
  hive_access_summary,
  file.path(output_dir, "sapopemba_hive_accessibility.csv"),
  row.names = FALSE
)

paired_mini <- mini_ttm |>
  select(scenario, from_id, to_id, travel_time_p50) |>
  pivot_wider(names_from = scenario, values_from = travel_time_p50, names_prefix = "tt_") |>
  group_by(from_id) |>
  summarise(
    common = sum(!is.na(tt_2012) & !is.na(tt_2025)),
    only_2012 = sum(!is.na(tt_2012) & is.na(tt_2025)),
    only_2025 = sum(is.na(tt_2012) & !is.na(tt_2025)),
    median_delta_common = median(tt_2025 - tt_2012, na.rm = TRUE),
    accessible_2025_l3_37min = sum(!is.na(tt_2025_l3_37min)),
    recovered_vs_2025 = accessible_2025_l3_37min - sum(!is.na(tt_2025)),
    median_gain_l3_common = median(
      tt_2025_l3_37min - tt_2025,
      na.rm = TRUE
    ),
    accessible_2025_l15_179sec = sum(!is.na(tt_2025_l15_179sec)),
    recovered_l15_vs_2025 = accessible_2025_l15_179sec - sum(!is.na(tt_2025)),
    median_gain_l15_common = median(
      tt_2025_l15_179sec - tt_2025,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  left_join(mini_od |> st_drop_geometry() |> select(from_id = id, origin_label), by = "from_id")
write.csv(
  paired_mini,
  file.path(output_dir, "mini_accessibility_comparison.csv"),
  row.names = FALSE
)

print(
  line_audit |>
    select(
      year, mode, line, headway_minutes_day, headway_minutes_hpm,
      speed_kmh_day, speed_kmh_hpm
    )
)
print(runtime_audit)
print(l3_feed_stage_audit)
print(l15_headway_audit)
print(bus_hive_summary)
print(hive_access_summary)
print(mini_access)
print(paired_mini)

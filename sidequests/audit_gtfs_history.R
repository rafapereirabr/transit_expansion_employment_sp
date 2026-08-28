# Setup --------------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(gtfstools)
  library(h3o)
  library(purrr)
  library(sf)
  library(stringr)
  library(tidyr)
})

source("R/gtfs.R")

input_dir <- "data-raw"
output_dir <- "sidequests/audit_gtfs_history_output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

## Exact dates may use YYYY-MM-DD, YYYY_MM_DD, YYYYMMDD or DD-MM-YYYY anywhere in the filename.
## Multiple snapshots from the same year are intentionally kept as separate observations.

query_start <- 6L * 3600L + 50L * 60L
query_end <- query_start + 15L * 60L
rail_route_types <- c(1L, 2L)

## These two filenames contain only the year, but their snapshot dates are known from the project.
known_dates <- c(
  gtfs_sptrans_2012 = "2012-04-10",
  gtfs_sptrans_2025 = "2025-04-08"
)

sapopemba_h3 <- h3_from_strings("89a81008e8bffff")
hive_h3 <- grid_disk(sapopemba_h3, k = 3L)[[1]]
sapopemba_hive <- st_sf(geometry = st_as_sfc(hive_h3), crs = 4326)

# Feed inventory and dates ------------------------------------------------------------------

extract_filename_date <- function(path) {
  filename <- tools::file_path_sans_ext(basename(path))
  iso_match <- str_match(filename, "(20\\d{2})[-_.]?([01]\\d)[-_.]?([0-3]\\d)")
  br_match <- str_match(filename, "([0-3]\\d)[-_.]([01]\\d)[-_.](20\\d{2})")

  if (!is.na(iso_match[1, 1])) {
    return(as.Date(paste(iso_match[1, 2:4], collapse = "-")))
  }
  if (!is.na(br_match[1, 1])) {
    return(as.Date(paste(br_match[1, c(4, 3, 2)], collapse = "-")))
  }
  as.Date(NA)
}

nearest_tuesday <- function(date) {
  weekday <- as.POSIXlt(date)$wday
  date + ((2L - weekday + 3L) %% 7L - 3L)
}

calendar_midpoint <- function(gtfs) {
  dates <- as.Date(character())
  if ("calendar" %in% names(gtfs) && nrow(gtfs$calendar) > 0L) {
    dates <- c(dates, gtfs$calendar$start_date, gtfs$calendar$end_date)
  }
  if ("calendar_dates" %in% names(gtfs) && nrow(gtfs$calendar_dates) > 0L) {
    dates <- c(dates, gtfs$calendar_dates$date)
  }
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0L) return(as.Date(NA))
  as.Date(mean(range(as.numeric(dates))), origin = "1970-01-01")
}

choose_feed_date <- function(path, gtfs) {
  feed_id <- tools::file_path_sans_ext(basename(path))
  filename_date <- extract_filename_date(path)
  known_date <- unname(known_dates[feed_id])

  if (!is.na(filename_date)) {
    reference_date <- filename_date
    date_source <- "filename"
  } else if (length(known_date) == 1L && !is.na(known_date)) {
    reference_date <- as.Date(known_date)
    date_source <- "known_override"
  } else {
    reference_date <- calendar_midpoint(gtfs)
    date_source <- "calendar_midpoint"
  }

  stopifnot(!is.na(reference_date))
  tibble(
    reference_date = reference_date,
    audit_date = nearest_tuesday(reference_date),
    date_source = date_source
  )
}

discover_feeds <- function(dir) {
  candidates <- list.files(dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE) |>
    sort()
  candidates[str_detect(basename(candidates), regex("gtfs|sptrans", ignore_case = TRUE)) &
    !str_detect(basename(candidates), regex("emtu", ignore_case = TRUE))]
}

read_clean_gtfs <- function(path) {
  standard_files <- paste0(
    c(
      "agency", "stops", "routes", "trips", "stop_times", "calendar",
      "calendar_dates", "frequencies", "shapes", "feed_info", "transfers"
    ),
    ".txt"
  )
  root_files <- utils::unzip(path, list = TRUE)$Name |>
    keep(~ !str_detect(.x, "/") && .x %in% standard_files)
  read_gtfs(path, files = root_files)
}

# Shared metrics -----------------------------------------------------------------------------

line_code <- function(route_id) {
  code <- str_extract(route_id, "L\\d+")
  case_when(
    !is.na(code) ~ paste0("L", as.integer(str_remove(code, "L"))),
    str_detect(route_id, "15") ~ "L15",
    TRUE ~ route_id
  )
}

frequency_window <- function(frequencies, lower, upper) {
  if (is.null(frequencies) || nrow(frequencies) == 0L) {
    return(tibble(
      trip_id = character(),
      headway_secs = numeric(),
      duration = numeric(),
      departures = integer()
    ))
  }
  frequencies |>
    mutate(
      start_seconds = gtfs_time_to_seconds(start_time),
      end_seconds = gtfs_time_to_seconds(end_time),
      overlap_start = pmax(start_seconds, lower),
      overlap_end = pmin(end_seconds, upper),
      duration = pmax(0, overlap_end - overlap_start),
      departures = pmax(
        0,
        ceiling((overlap_end - start_seconds) / headway_secs) -
          pmax(0, ceiling((overlap_start - start_seconds) / headway_secs))
      )
    ) |>
    filter(duration > 0, departures > 0)
}

representative_segments <- function(gtfs, trips) {
  selected_trips <- trips |>
    group_by(route_id, direction_id) |>
    slice_head(n = 1L) |>
    ungroup()

  gtfs$stop_times |>
    semi_join(selected_trips, by = "trip_id") |>
    left_join(
      selected_trips |> select(trip_id, route_id, direction_id),
      by = "trip_id"
    ) |>
    arrange(route_id, direction_id, stop_sequence) |>
    group_by(route_id, direction_id) |>
    mutate(
      departure_seconds = gtfs_time_to_seconds(departure_time),
      next_arrival_seconds = lead(gtfs_time_to_seconds(arrival_time)),
      segment_minutes = (next_arrival_seconds - departure_seconds) / 60
    ) |>
    filter(!is.na(next_arrival_seconds)) |>
    ungroup()
}

# Per-feed audit -----------------------------------------------------------------------------

audit_one_feed <- function(path) {
  message("Auditing ", basename(path))
  gtfs <- read_clean_gtfs(path)
  date_info <- choose_feed_date(path, gtfs)
  audit_date <- date_info$audit_date
  service_ids <- gtfs_active_service_ids(gtfs, audit_date)
  active_trips <- gtfs$trips |>
    filter(service_id %in% service_ids)
  active_frequencies <- if ("frequencies" %in% names(gtfs)) {
    gtfs$frequencies |> filter(trip_id %in% active_trips$trip_id)
  } else {
    NULL
  }

  feed_id <- tools::file_path_sans_ext(basename(path))
  year <- as.integer(format(date_info$reference_date, "%Y"))
  common <- tibble(
    feed_id = feed_id,
    feed_path = path,
    year = year,
    reference_date = date_info$reference_date,
    audit_date = audit_date,
    date_source = date_info$date_source
  )

  inventory <- common |>
    mutate(
      file_size_mb = round(file.info(path)$size / 1024^2, 1),
      md5 = unname(tools::md5sum(path)),
      services = length(service_ids),
      routes = n_distinct(active_trips$route_id),
      trips = n_distinct(active_trips$trip_id),
      stops = nrow(gtfs$stops),
      stop_times = nrow(gtfs$stop_times),
      frequency_entries = if (is.null(active_frequencies)) 0L else nrow(active_frequencies)
    )

  rail_routes <- gtfs$routes |>
    filter(route_type %in% rail_route_types)
  rail_trips <- active_trips |>
    semi_join(rail_routes, by = "route_id")
  rail_segments <- representative_segments(gtfs, rail_trips)
  rail_frequency <- frequency_window(active_frequencies, query_start, query_end) |>
    inner_join(
      rail_trips |> select(trip_id, route_id, direction_id),
      by = "trip_id"
    )

  rail <- rail_segments |>
    group_by(route_id, direction_id) |>
    summarise(
      stops = n() + 1L,
      runtime_minutes = sum(segment_minutes),
      segment_time_mean = mean(segment_minutes),
      segment_time_sd = sd(segment_minutes),
      segment_time_cv = segment_time_sd / segment_time_mean,
      .groups = "drop"
    ) |>
    left_join(
      rail_frequency |>
        group_by(route_id, direction_id) |>
        summarise(
          departures_15min = sum(departures),
          headway_minutes = weighted.mean(headway_secs, duration) / 60,
          .groups = "drop"
        ),
      by = c("route_id", "direction_id")
    ) |>
    left_join(rail_routes |> select(route_id, route_type), by = "route_id") |>
    mutate(
      feed_id = feed_id,
      year = year,
      reference_date = date_info$reference_date,
      audit_date = audit_date,
      mode = if_else(route_type == 1L, "Metro", "Train"),
      line = line_code(route_id)
    ) |>
    relocate(feed_id, year, reference_date, audit_date, mode, line)

  stop_points <- gtfs$stops |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)
  local_stop_ids <- stop_points$stop_id[
    lengths(st_intersects(stop_points, st_union(sapopemba_hive))) > 0L
  ]
  bus_routes <- gtfs$routes |> filter(route_type == 3L)
  active_bus_trips <- active_trips |>
    semi_join(bus_routes, by = "route_id")
  city_bus_frequency <- frequency_window(active_frequencies, query_start, query_end) |>
    inner_join(
      active_bus_trips |> select(trip_id, route_id, direction_id),
      by = "trip_id"
    )
  city_bus_speeds <- if (nrow(city_bus_frequency) > 0L && "shapes" %in% names(gtfs)) {
    get_trip_speed(
      gtfs,
      trip_id = unique(city_bus_frequency$trip_id),
      file = "shapes"
    ) |>
      as_tibble() |>
      transmute(
        trip_id,
        speed_kmh = if_else(is.finite(speed) & speed > 0, speed, NA_real_)
      )
  } else {
    tibble(trip_id = character(), speed_kmh = numeric())
  }
  city_bus_routes <- city_bus_frequency |>
    left_join(city_bus_speeds, by = "trip_id") |>
    group_by(route_id) |>
    summarise(
      directions = n_distinct(direction_id),
      trip_patterns = n_distinct(trip_id),
      departures_15min = sum(departures),
      headway_minutes = weighted.mean(headway_secs, duration) / 60,
      speed_kmh = weighted.mean(speed_kmh, departures, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      bus_routes |> select(route_id, route_short_name, route_long_name),
      by = "route_id"
    ) |>
    mutate(
      feed_id = feed_id,
      year = year,
      reference_date = date_info$reference_date,
      audit_date = audit_date
    ) |>
    relocate(feed_id, year, reference_date, audit_date)
  city_bus_summary <- city_bus_routes |>
    summarise(
      routes = n_distinct(route_id),
      trip_patterns = sum(trip_patterns),
      median_headway_minutes = median(headway_minutes),
      weighted_speed_kmh = weighted.mean(speed_kmh, departures_15min, na.rm = TRUE),
      departures_15min = sum(departures_15min)
    ) |>
    bind_cols(common)

  local_bus_trips <- active_trips |>
    semi_join(bus_routes, by = "route_id") |>
    semi_join(gtfs$stop_times |> filter(stop_id %in% local_stop_ids), by = "trip_id")
  local_bus_frequency <- frequency_window(active_frequencies, query_start, query_end) |>
    inner_join(
      local_bus_trips |> select(trip_id, route_id, direction_id),
      by = "trip_id"
    )
  bus_speeds <- if (nrow(local_bus_trips) > 0L && "shapes" %in% names(gtfs)) {
    get_trip_speed(gtfs, trip_id = unique(local_bus_trips$trip_id), file = "shapes") |>
      as_tibble() |>
      transmute(
        trip_id,
        speed_kmh = if_else(is.finite(speed) & speed > 0, speed, NA_real_)
      )
  } else {
    tibble(trip_id = character(), speed_kmh = numeric())
  }

  bus_routes_detail <- local_bus_frequency |>
    left_join(bus_speeds, by = "trip_id") |>
    group_by(route_id) |>
    summarise(
      directions = n_distinct(direction_id),
      trip_patterns = n_distinct(trip_id),
      departures_15min = sum(departures),
      headway_minutes = weighted.mean(headway_secs, duration) / 60,
      speed_kmh = weighted.mean(speed_kmh, departures, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      bus_routes |> select(route_id, route_short_name, route_long_name),
      by = "route_id"
    ) |>
    mutate(
      feed_id = feed_id,
      year = year,
      reference_date = date_info$reference_date,
      audit_date = audit_date
    ) |>
    relocate(feed_id, year, reference_date, audit_date)

  bus_summary <- bus_routes_detail |>
    summarise(
      routes = n_distinct(route_id),
      trip_patterns = sum(trip_patterns),
      median_headway_minutes = median(headway_minutes),
      weighted_speed_kmh = weighted.mean(speed_kmh, departures_15min, na.rm = TRUE),
      departures_15min = sum(departures_15min)
    ) |>
    bind_cols(common)

  list(
    inventory = inventory,
    rail = rail,
    city_bus_routes = city_bus_routes,
    city_bus_summary = city_bus_summary,
    bus_routes = bus_routes_detail,
    bus_summary = bus_summary
  )
}

audit_one_feed_safe <- function(path) {
  tryCatch(
    audit_one_feed(path),
    error = function(error) {
      warning("Skipping ", basename(path), ": ", conditionMessage(error), call. = FALSE)
      list(
        error = tibble(
          feed_id = tools::file_path_sans_ext(basename(path)),
          feed_path = path,
          filename_date = extract_filename_date(path),
          error = conditionMessage(error)
        )
      )
    }
  )
}

# Run and export -----------------------------------------------------------------------------

feed_paths <- discover_feeds(input_dir)
if (length(feed_paths) == 0L) {
  stop("No SPTrans GTFS zip files found under ", input_dir, ".")
}

results <- map(feed_paths, audit_one_feed_safe)
feed_errors <- map_dfr(results, ~ if (!is.null(.x$error)) .x$error else tibble())
results <- keep(results, ~ is.null(.x$error))
inventory <- map_dfr(results, "inventory") |> arrange(reference_date, feed_id)
rail_history <- map_dfr(results, "rail") |> arrange(reference_date, line, direction_id)
city_bus_route_history <- map_dfr(results, "city_bus_routes") |>
  arrange(reference_date, route_short_name)
city_bus_history <- map_dfr(results, "city_bus_summary") |>
  arrange(reference_date, feed_id)
bus_route_history <- map_dfr(results, "bus_routes") |> arrange(reference_date, route_short_name)
bus_hive_history <- map_dfr(results, "bus_summary") |> arrange(reference_date, feed_id)

write.csv(inventory, file.path(output_dir, "feed_inventory.csv"), row.names = FALSE)
write.csv(feed_errors, file.path(output_dir, "feed_errors.csv"), row.names = FALSE)
write.csv(rail_history, file.path(output_dir, "rail_history.csv"), row.names = FALSE)
write.csv(
  city_bus_route_history,
  file.path(output_dir, "city_bus_route_history.csv"),
  row.names = FALSE
)
write.csv(city_bus_history, file.path(output_dir, "city_bus_history.csv"), row.names = FALSE)
write.csv(bus_route_history, file.path(output_dir, "sapopemba_bus_route_history.csv"), row.names = FALSE)
write.csv(bus_hive_history, file.path(output_dir, "sapopemba_bus_history.csv"), row.names = FALSE)

# Quick timeline -----------------------------------------------------------------------------

timeline <- bind_rows(
  rail_history |>
    filter(line == "L3") |>
    group_by(feed_id, reference_date) |>
    summarise(value = mean(runtime_minutes), .groups = "drop") |>
    mutate(metric = "L3 runtime (minutes)"),
  rail_history |>
    filter(line == "L15") |>
    group_by(feed_id, reference_date) |>
    summarise(value = mean(headway_minutes, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "L15 headway at 06:50 (minutes)"),
  bus_hive_history |>
    transmute(feed_id, reference_date, value = weighted_speed_kmh,
              metric = "Sapopemba bus speed (km/h)"),
  bus_hive_history |>
    transmute(feed_id, reference_date, value = median_headway_minutes,
              metric = "Sapopemba bus median headway (minutes)"),
  city_bus_history |>
    transmute(feed_id, reference_date, value = weighted_speed_kmh,
              metric = "City bus speed (km/h)"),
  city_bus_history |>
    transmute(feed_id, reference_date, value = median_headway_minutes,
              metric = "City bus median headway (minutes)")
)
write.csv(timeline, file.path(output_dir, "timeline.csv"), row.names = FALSE)

timeline_changes <- timeline |>
  group_by(metric) |>
  arrange(reference_date, feed_id, .by_group = TRUE) |>
  mutate(
    previous_date = lag(reference_date),
    previous_value = lag(value),
    change = value - previous_value,
    gap_days = as.integer(reference_date - previous_date)
  ) |>
  ungroup() |>
  filter(!is.na(change)) |>
  arrange(metric, desc(abs(change)))
write.csv(
  timeline_changes,
  file.path(output_dir, "timeline_changes.csv"),
  row.names = FALSE
)

timeline_plot <- ggplot(timeline, aes(reference_date, value, group = 1)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(vars(metric), scales = "free_y", ncol = 2) +
  labs(x = NULL, y = NULL, title = "SPTrans GTFS historical audit") +
  theme_minimal(base_size = 11)
ggsave(
  file.path(output_dir, "timeline.png"),
  timeline_plot,
  width = 10,
  height = 7,
  dpi = 200,
  bg = "white"
)

print(inventory)
print(timeline)

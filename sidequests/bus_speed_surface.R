# Setup --------------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(ggplot2)
  library(gtfstools)
  library(h3o)
  library(purrr)
  library(sf)
  library(stringr)
  library(tibble)
})

source("R/gtfs.R")

input_dir <- "data-raw/gtfs_history"
output_dir <- "sidequests/bus_speed_surface_output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

reference_years <- 2015:2017
h3_resolution <- 8L
window_start <- 6L * 3600L
window_end <- 7L * 3600L

## Straight-line interstop distances are a deliberately simple approximation.
## Medians across many routes dampen road curvature and imperfect stop placement.
minimum_segment_m <- 75
maximum_segment_m <- 5000
minimum_speed_kmh <- 2
maximum_speed_kmh <- 50

# Feed inventory -----------------------------------------------------------------------------

extract_filename_date <- function(path) {
  matched <- stringr::str_match(
    tools::file_path_sans_ext(basename(path)),
    "(20\\d{2})[-_.]?([01]\\d)[-_.]?([0-3]\\d)"
  )
  if (is.na(matched[1, 1])) return(as.Date(NA))
  as.Date(paste(matched[1, 2:4], collapse = "-"))
}


nearest_tuesday <- function(date) {
  date + ((2L - as.POSIXlt(date)$wday + 3L) %% 7L - 3L)
}


discover_reference_feeds <- function(dir, years) {
  paths <- list.files(dir, pattern = "[.]zip$", full.names = TRUE) |>
    sort()
  dates <- as.Date(
    unlist(purrr::map(paths, extract_filename_date)),
    origin = "1970-01-01"
  )
  tibble::tibble(
    feed_id = tools::file_path_sans_ext(basename(paths)),
    feed_path = paths,
    reference_date = as.Date(dates, origin = "1970-01-01"),
    year = as.integer(format(reference_date, "%Y")),
    audit_date = nearest_tuesday(reference_date)
  ) |>
    dplyr::filter(year %in% years)
}


read_clean_gtfs <- function(path) {
  files <- paste0(
    c(
      "agency", "stops", "routes", "trips", "stop_times", "calendar",
      "calendar_dates", "frequencies"
    ),
    ".txt"
  )
  root_files <- utils::unzip(path, list = TRUE)$Name
  root_files <- root_files[!stringr::str_detect(root_files, "/") & root_files %in% files]
  gtfstools::read_gtfs(path, files = root_files)
}

# HPM segments --------------------------------------------------------------------------------

frequency_departures <- function(frequencies, lower, upper) {
  if (is.null(frequencies) || nrow(frequencies) == 0L) {
    return(tibble::tibble(trip_id = character(), departures = integer()))
  }

  frequencies |>
    dplyr::mutate(
      start_seconds = gtfs_time_to_seconds(start_time),
      end_seconds = gtfs_time_to_seconds(end_time),
      overlap_start = pmax(start_seconds, lower),
      overlap_end = pmin(end_seconds, upper),
      first_index = pmax(0, ceiling((overlap_start - start_seconds) / headway_secs)),
      last_index = ceiling((overlap_end - start_seconds) / headway_secs) - 1L,
      departures = pmax(0L, last_index - first_index + 1L)
    ) |>
    dplyr::filter(overlap_end > overlap_start, departures > 0L) |>
    dplyr::summarise(departures = sum(departures), .by = trip_id)
}


haversine_m <- function(lon1, lat1, lon2, lat2) {
  radius <- 6371008.8
  lon1 <- lon1 * pi / 180
  lat1 <- lat1 * pi / 180
  lon2 <- lon2 * pi / 180
  lat2 <- lat2 * pi / 180
  delta_lon <- lon2 - lon1
  delta_lat <- lat2 - lat1
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  2 * radius * atan2(sqrt(a), sqrt(1 - a))
}


segment_bearing <- function(lon1, lat1, lon2, lat2) {
  lon1 <- lon1 * pi / 180
  lat1 <- lat1 * pi / 180
  lon2 <- lon2 * pi / 180
  lat2 <- lat2 * pi / 180
  bearing <- atan2(
    sin(lon2 - lon1) * cos(lat2),
    cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon2 - lon1)
  ) * 180 / pi
  (bearing + 360) %% 360
}


bearing_group <- function(bearing) {
  labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  labels[(floor((bearing + 22.5) %% 360 / 45) %% 8) + 1L]
}


points_to_h3 <- function(lon, lat, resolution) {
  points <- sf::st_as_sf(
    tibble::tibble(lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs = 4326
  )
  sf::st_geometry(points) |>
    h3o::h3_from_points(resolution) |>
    as.character()
}


extract_hpm_bus_segments <- function(feed_path, audit_date, feed_id, year) {
  message("Extracting ", feed_id)
  gtfs <- read_clean_gtfs(feed_path)
  service_ids <- gtfs_active_service_ids(gtfs, audit_date)
  bus_route_ids <- gtfs$routes |>
    dplyr::filter(route_type == 3L) |>
    dplyr::pull(route_id)
  active_trips <- gtfs$trips |>
    dplyr::filter(service_id %in% service_ids, route_id %in% bus_route_ids)
  departures <- frequency_departures(gtfs$frequencies, window_start, window_end) |>
    dplyr::inner_join(
      active_trips |>
        dplyr::select(trip_id, route_id, direction_id),
      by = "trip_id"
    )

  stops <- gtfs$stops |>
    dplyr::select(stop_id, stop_lon, stop_lat)
  segments <- gtfs$stop_times |>
    dplyr::inner_join(departures, by = "trip_id") |>
    dplyr::left_join(stops, by = "stop_id") |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::mutate(
      next_stop_id = dplyr::lead(stop_id),
      next_lon = dplyr::lead(stop_lon),
      next_lat = dplyr::lead(stop_lat),
      departure_seconds = gtfs_time_to_seconds(departure_time),
      next_arrival_seconds = dplyr::lead(gtfs_time_to_seconds(arrival_time)),
      .by = trip_id
    ) |>
    dplyr::filter(!is.na(next_stop_id)) |>
    dplyr::mutate(
      segment_seconds = next_arrival_seconds - departure_seconds,
      segment_m = haversine_m(stop_lon, stop_lat, next_lon, next_lat),
      speed_kmh = segment_m / segment_seconds * 3.6,
      bearing = segment_bearing(stop_lon, stop_lat, next_lon, next_lat),
      direction = bearing_group(bearing),
      midpoint_lon = (stop_lon + next_lon) / 2,
      midpoint_lat = (stop_lat + next_lat) / 2
    )

  valid <- segments |>
    dplyr::filter(
      segment_seconds > 0,
      dplyr::between(segment_m, minimum_segment_m, maximum_segment_m),
      dplyr::between(speed_kmh, minimum_speed_kmh, maximum_speed_kmh),
      is.finite(midpoint_lon),
      is.finite(midpoint_lat)
    ) |>
    dplyr::mutate(
      h3 = points_to_h3(midpoint_lon, midpoint_lat, h3_resolution),
      feed_id = feed_id,
      year = year,
      audit_date = audit_date
    ) |>
    dplyr::select(
      feed_id, year, audit_date, h3, direction, route_id, trip_id,
      departures, stop_id, next_stop_id, segment_m, segment_seconds, speed_kmh
    )

  diagnostics <- tibble::tibble(
    feed_id = feed_id,
    year = year,
    audit_date = audit_date,
    active_bus_routes = dplyr::n_distinct(departures$route_id),
    active_bus_patterns = dplyr::n_distinct(departures$trip_id),
    candidate_segments = nrow(segments),
    valid_segments = nrow(valid),
    valid_share = nrow(valid) / nrow(segments)
  )
  list(segments = valid, diagnostics = diagnostics)
}

# Balanced spatial summaries ----------------------------------------------------------------

summarise_speed_surface <- function(segments, directional = FALSE) {
  spatial_keys <- c("h3", if (directional) "direction")

  snapshot <- segments |>
    dplyr::summarise(
      speed_kmh = stats::median(speed_kmh),
      routes = dplyr::n_distinct(route_id),
      segments = dplyr::n(),
      departures = sum(departures),
      .by = dplyr::all_of(c("feed_id", "year", spatial_keys))
    )
  annual <- snapshot |>
    dplyr::summarise(
      speed_kmh = stats::median(speed_kmh),
      snapshots = dplyr::n(),
      routes = stats::median(routes),
      segments = sum(segments),
      departures = sum(departures),
      .by = dplyr::all_of(c("year", spatial_keys))
    )
  surface <- annual |>
    dplyr::summarise(
      speed_p25 = stats::quantile(speed_kmh, 0.25),
      speed_p75 = stats::quantile(speed_kmh, 0.75),
      speed_iqr = speed_p75 - speed_p25,
      speed_kmh = stats::median(speed_kmh),
      years = dplyr::n_distinct(year),
      snapshots = sum(snapshots),
      routes = stats::median(routes),
      segments = sum(segments),
      departures = sum(departures),
      .by = dplyr::all_of(spatial_keys)
    )
  list(snapshot = snapshot, annual = annual, surface = surface)
}


h3_as_sf <- function(data) {
  geometry <- data$h3 |>
    h3o::h3_from_strings() |>
    sf::st_as_sfc()
  sf::st_sf(data, geometry = geometry, crs = 4326)
}

# Run ----------------------------------------------------------------------------------------

feed_inventory <- discover_reference_feeds(input_dir, reference_years)
if (nrow(feed_inventory) == 0L) {
  stop("No 2015-2017 SPTrans feeds found in ", input_dir, ".")
}

results <- purrr::pmap(
  feed_inventory,
  \(feed_id, feed_path, reference_date, year, audit_date) {
    extract_hpm_bus_segments(feed_path, audit_date, feed_id, year)
  }
)
segments <- purrr::map_dfr(results, "segments")
diagnostics <- purrr::map_dfr(results, "diagnostics")

overall <- summarise_speed_surface(segments)
directional <- summarise_speed_surface(segments, directional = TRUE)
surface_sf <- h3_as_sf(overall$surface)
directional_sf <- h3_as_sf(directional$surface)

# Export tables ------------------------------------------------------------------------------

arrow::write_parquet(segments, file.path(output_dir, "bus_hpm_segments.parquet"))
utils::write.csv(feed_inventory, file.path(output_dir, "feed_inventory.csv"), row.names = FALSE)
utils::write.csv(diagnostics, file.path(output_dir, "feed_diagnostics.csv"), row.names = FALSE)
utils::write.csv(overall$annual, file.path(output_dir, "speed_by_h3_year.csv"), row.names = FALSE)
utils::write.csv(overall$surface, file.path(output_dir, "speed_surface_h3_8.csv"), row.names = FALSE)
utils::write.csv(
  directional$surface,
  file.path(output_dir, "speed_surface_h3_8_direction.csv"),
  row.names = FALSE
)

# Maps ---------------------------------------------------------------------------------------

map_theme <- ggplot2::theme_void(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "grey35"),
    legend.position = "right"
  )

speed_map <- ggplot2::ggplot(surface_sf) +
  ggplot2::geom_sf(ggplot2::aes(fill = speed_kmh), color = NA) +
  ggplot2::scale_fill_viridis_c(option = "magma", direction = -1) +
  ggplot2::labs(
    title = "Scheduled bus speeds in the morning peak",
    subtitle = "Median of yearly H3-8 medians, SPTrans GTFS 2015-2017",
    fill = "km/h"
  ) +
  map_theme

coverage_map <- ggplot2::ggplot(surface_sf) +
  ggplot2::geom_sf(ggplot2::aes(fill = log10(segments)), color = NA) +
  ggplot2::scale_fill_viridis_c(option = "viridis") +
  ggplot2::labs(
    title = "Observational coverage of the speed surface",
    subtitle = "Valid route segments across all 2015-2017 snapshots",
    fill = "log10\nsegments"
  ) +
  map_theme

stability_map <- ggplot2::ggplot(surface_sf) +
  ggplot2::geom_sf(ggplot2::aes(fill = speed_iqr), color = NA) +
  ggplot2::scale_fill_viridis_c(option = "plasma") +
  ggplot2::labs(
    title = "Stability of scheduled bus speeds across years",
    subtitle = "IQR of the 2015, 2016 and 2017 H3-8 medians",
    fill = "IQR\n(km/h)"
  ) +
  map_theme

direction_map <- directional_sf |>
  dplyr::filter(years == length(reference_years), segments >= 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = speed_kmh), color = NA) +
  ggplot2::scale_fill_viridis_c(option = "magma", direction = -1) +
  ggplot2::facet_wrap(ggplot2::vars(direction), ncol = 4) +
  ggplot2::labs(
    title = "Scheduled bus speeds by travel direction",
    subtitle = "Cells observed in all three years with at least 10 segments",
    fill = "km/h"
  ) +
  map_theme

ggplot2::ggsave(
  file.path(output_dir, "speed_surface_h3_8.png"),
  speed_map, width = 8, height = 8, dpi = 250, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "speed_surface_coverage.png"),
  coverage_map, width = 8, height = 8, dpi = 250, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "speed_surface_stability.png"),
  stability_map, width = 8, height = 8, dpi = 250, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "speed_surface_direction.png"),
  direction_map, width = 13, height = 7, dpi = 250, bg = "white"
)

print(diagnostics)
print(overall$surface |> dplyr::summarise(
  cells = dplyr::n(),
  median_speed_kmh = stats::median(speed_kmh),
  median_yearly_iqr = stats::median(speed_iqr),
  complete_year_cells = sum(years == length(reference_years))
))

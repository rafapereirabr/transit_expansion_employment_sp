# rail feed reconstruction -------------------------------------------------------------------

seconds_to_gtfs_time <- function(seconds) {
  seconds <- round(seconds)
  sprintf(
    "%02d:%02d:%02d",
    seconds %/% 3600,
    (seconds %% 3600) %/% 60,
    seconds %% 60
  )
}


haversine_distance_m <- function(lon1, lat1, lon2, lat2) {
  radius <- 6371008.8
  to_rad <- pi / 180
  lon1 <- lon1 * to_rad
  lat1 <- lat1 * to_rad
  lon2 <- lon2 * to_rad
  lat2 <- lat2 * to_rad
  delta_lon <- lon2 - lon1
  delta_lat <- lat2 - lat1
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  2 * radius * atan2(sqrt(a), sqrt(1 - a))
}


set_rail_service_spec <- function(stations_path) {
  station_lines <- arrow::read_parquet(
    stations_path,
    col_select = c("code_line", "name_line", "name_system", "label_line")
  ) |>
    as.data.frame() |>
    dplyr::distinct(code_line, name_line, name_system, label_line)
  ## This is a standardized-service scenario, not a historical timetable. The same
  ## commercial speeds and HPM headways are used in both years so that changes in
  ## accessibility primarily reflect the rail network available in each year.
  spec <- tibble::tribble(
    ~year, ~code_line, ~name_line, ~extension_km, ~commercial_speed_kmh,
    ~headway_minutes,
    2012L, 1L, "AZUL",      20.2, 35, 125 / 60,
    2012L, 2L, "VERDE",     14.7, 35, 128 / 60,
    2012L, 3L, "VERMELHA",  22.0, 35, 119 / 60,
    2012L, 4L, "AMARELA",    8.9, 35, 100 / 60,
    2012L, 5L, "LILAS",      8.4, 35, 171 / 60,
    2012L, 7L, "RUBI",      62.7, 40, 6.0,
    2012L, 8L, "DIAMANTE",  41.6, 40, 6.0,
    2012L, 9L, "ESMERALDA", 32.6, 40, 5.5,
    2012L, 10L, "TURQUESA", 38.0, 40, 6.0,
    2012L, 11L, "CORAL",    50.5, 40, 6.0,
    2012L, 12L, "SAFIRA",   39.0, 40, 7.0,
    2025L, 1L, "AZUL",      20.2, 35, 125 / 60,
    2025L, 2L, "VERDE",     14.7, 35, 128 / 60,
    2025L, 3L, "VERMELHA",  22.0, 35, 119 / 60,
    2025L, 4L, "AMARELA",   12.8, 35, 100 / 60,
    2025L, 5L, "LILAS",     19.9, 35, 171 / 60,
    2025L, 7L, "RUBI",      62.7, 40, 6.0,
    2025L, 8L, "DIAMANTE",  41.6, 40, 6.0,
    2025L, 9L, "ESMERALDA", 35.1, 40, 5.5,
    2025L, 10L, "TURQUESA", 38.0, 40, 6.0,
    2025L, 11L, "CORAL",    50.5, 40, 6.0,
    2025L, 12L, "SAFIRA",   39.0, 40, 7.0,
    2025L, 13L, "JADE",     12.2, 40, 20.0,
    2025L, 15L, "PRATA",    14.6, 35, 180 / 60
  ) |>
    dplyr::mutate(
      runtime_minutes = extension_km / commercial_speed_kmh * 60,
      runtime_source = paste0(
        "Model assumption: extension / ", commercial_speed_kmh,
        " km/h commercial speed"
      ),
      headway_source = paste0(
        "Prior study table: Metrô annual reports; CPTM professional contact; ",
        "range midpoints where applicable"
      ),
      notes = "Common operating assumptions for the 2012/2025 expansion comparison"
    )
  duplicated_lines <- spec |>
    dplyr::count(year, code_line) |>
    dplyr::filter(n > 1L)
  if (nrow(duplicated_lines) > 0L) {
    stop("Rail service spec has duplicated year/code_line rows.")
  }

  spec <- spec |>
    dplyr::left_join(
      station_lines,
      by = c("code_line", "name_line"),
      relationship = "many-to-one"
    )
  if (any(is.na(spec$name_system))) {
    invalid <- spec |>
      dplyr::filter(is.na(name_system)) |>
      dplyr::transmute(key = paste0(code_line, " (", name_line, ")")) |>
      dplyr::pull(key)
    stop(
      "Rail service spec contains lines absent from stations_sf: ",
      paste(invalid, collapse = ", "), "."
    )
  }
  spec
}


set_rail_stop_corrections <- function() {
  tibble::tribble(
    ~year, ~action, ~code_line, ~stop_id, ~other_stop_id,
    ~min_transfer_time, ~station_name, ~notes,
    2012L, "exclude", 2L, "18989", NA_character_, NA_integer_, "PARAÍSO",
      "Platform stop from Line 1 embedded in the Line 2 trip",
    2012L, "exclude", 3L, "8210164", NA_character_, NA_integer_, "TATUAPÉ",
      "CPTM platform stop embedded in the Line 3 trip",
    2012L, "exclude", 11L, "1010053", NA_character_, NA_integer_, "BRÁS",
      "Redundant stop shared by Lines 11 and 12",
    2012L, "exclude", 12L, "1010053", NA_character_, NA_integer_, "BRÁS",
      "Redundant stop shared by Lines 11 and 12",
    2012L, "transfer", NA_integer_, "18861", "18989", 180L, "PARAÍSO",
      "Line 2 to Line 1 platforms",
    2012L, "transfer", NA_integer_, "18944", "8210164", 180L, "TATUAPÉ",
      "Line 3 to Line 11 platforms",
    2012L, "transfer", NA_integer_, "18943", "18987", 240L, "BRÁS",
      "Lines 11/12 to Line 10 platforms",
    2012L, "transfer", NA_integer_, "18943", "1010054", 240L, "BRÁS",
      "Lines 11/12 to Line 3 platforms",
    2012L, "transfer", NA_integer_, "18987", "1010054", 240L, "BRÁS",
      "Line 10 to Line 3 platforms"
  )
}


check_rail_service_spec <- function(spec, year) {
  required <- c(
    "year", "code_line", "name_line", "runtime_minutes", "headway_minutes",
    "runtime_source", "headway_source"
  )
  stopifnot(all(required %in% names(spec)))
  selected <- spec |>
    dplyr::filter(year == .env$year)

  if (nrow(selected) == 0L) {
    stop("No rail service parameters supplied for ", year, ".")
  }
  invalid <- selected |>
    dplyr::filter(
      is.na(runtime_minutes) | runtime_minutes <= 0 |
        is.na(headway_minutes) | headway_minutes <= 0 |
        is.na(runtime_source) | runtime_source == "" |
        is.na(headway_source) | headway_source == ""
    )
  if (nrow(invalid) > 0L) {
    invalid_lines <- paste0(
      "L", invalid$code_line, " (", invalid$name_line, ")",
      collapse = ", "
    )
    stop(
      "Rail reconstruction is blocked: ", nrow(invalid),
      " line(s) lack positive parameters and traceable sources for ", year,
      ": ", invalid_lines, "."
    )
  }
  selected
}


rail_route_crosswalk <- function(gtfs, code_lines) {
  crosswalk <- gtfs$routes |>
    dplyr::filter(route_type %in% c(1L, 2L)) |>
    dplyr::mutate(
      code_line = as.integer(stringr::str_extract(route_short_name, "[0-9]+"))
    ) |>
    dplyr::filter(code_line %in% .env$code_lines) |>
    dplyr::select(route_id, code_line)
  missing_lines <- setdiff(code_lines, crosswalk$code_line)
  duplicated_lines <- crosswalk |>
    dplyr::count(code_line) |>
    dplyr::filter(n != 1L)
  if (length(missing_lines) > 0L || nrow(duplicated_lines) > 0L) {
    stop(
      "Could not establish a unique GTFS route for canonical rail line(s): ",
      paste(union(missing_lines, duplicated_lines$code_line), collapse = ", "), "."
    )
  }
  crosswalk
}


rail_representative_trips <- function(gtfs, crosswalk) {
  gtfs$trips |>
    dplyr::inner_join(crosswalk, by = "route_id") |>
    dplyr::left_join(
      gtfs$stop_times |>
        dplyr::count(trip_id, name = "n_stops"),
      by = "trip_id"
    ) |>
    dplyr::group_by(route_id, direction_id) |>
    dplyr::slice_max(n_stops, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()
}


build_rail_analysis_gtfs <- function(
  template_gtfs,
  spec,
  stop_corrections = NULL,
  year,
  service_date,
  frequency_start = "05:30:00",
  frequency_end = "08:30:00"
) {
  spec <- check_rail_service_spec(spec, year)
  code_lines <- spec$code_line
  crosswalk <- rail_route_crosswalk(template_gtfs, code_lines)

  representative <- rail_representative_trips(template_gtfs, crosswalk) |>
    dplyr::select(code_line, route_id, direction_id, old_trip_id = trip_id, shape_id) |>
    dplyr::left_join(spec, by = "code_line", relationship = "many-to-one") |>
    dplyr::mutate(
      service_id = paste0("rail_weekday_", year),
      trip_id = paste0("rail_", year, "_L", code_line, "_", direction_id)
    )

  corrections <- if (is.null(stop_corrections)) {
    tibble::tibble(
      year = integer(), action = character(), code_line = integer(),
      stop_id = character(), other_stop_id = character(),
      min_transfer_time = integer()
    )
  } else {
    stop_corrections |>
      dplyr::filter(year == .env$year)
  }
  exclusions <- corrections |>
    dplyr::filter(action == "exclude") |>
    dplyr::select(code_line, excluded_stop_id = stop_id)

  stop_times <- representative |>
    dplyr::select(
      code_line, route_id, direction_id, old_trip_id, trip_id,
      runtime_minutes
    ) |>
    dplyr::left_join(
      template_gtfs$stop_times,
      by = c("old_trip_id" = "trip_id")
    ) |>
    dplyr::left_join(
      template_gtfs$stops |>
        dplyr::select(stop_id, stop_name, stop_lat, stop_lon),
      by = "stop_id"
    ) |>
    dplyr::left_join(
      exclusions |>
        dplyr::mutate(exclude = TRUE),
      by = c("code_line", "stop_id" = "excluded_stop_id")
    ) |>
    dplyr::filter(is.na(exclude)) |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::group_by(trip_id) |>
    dplyr::mutate(
      # Some 2012 trips contain consecutive platform IDs for the same physical
      # interchange (Paraíso, Tatuapé and Brás). Keep both IDs for transfer
      # connectivity, but do not treat movement between platforms as train run
      # time when allocating the line's end-to-end runtime.
      same_station = dplyr::row_number() > 1L &
        stringr::str_to_upper(stop_name) ==
          dplyr::lag(stringr::str_to_upper(stop_name)),
      segment_distance_m = dplyr::if_else(
        dplyr::row_number() == 1L | same_station,
        0,
        haversine_distance_m(
          dplyr::lag(stop_lon), dplyr::lag(stop_lat), stop_lon, stop_lat
        )
      ),
      distance_share = cumsum(segment_distance_m) / sum(segment_distance_m),
      elapsed_seconds = runtime_minutes * 60 * distance_share,
      arrival_time = seconds_to_gtfs_time(elapsed_seconds),
      departure_time = arrival_time
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      trip_id, arrival_time, departure_time, stop_id, stop_sequence,
      dplyr::any_of(c("stop_headsign", "pickup_type", "drop_off_type"))
    )

  trips <- representative |>
    dplyr::transmute(
      route_id = paste0("rail_L", code_line),
      service_id,
      trip_id,
      trip_headsign = NA_character_,
      direction_id,
      shape_id
    )
  frequencies <- representative |>
    dplyr::transmute(
      trip_id,
      start_time = frequency_start,
      end_time = frequency_end,
      headway_secs = as.integer(round(headway_minutes * 60)),
      exact_times = 0L
    )

  service_date <- as.Date(service_date)
  weekday <- c(
    "sunday", "monday", "tuesday", "wednesday",
    "thursday", "friday", "saturday"
  )[as.POSIXlt(service_date)$wday + 1L]
  calendar <- tibble::tibble(
    service_id = unique(trips$service_id),
    monday = as.integer(weekday == "monday"),
    tuesday = as.integer(weekday == "tuesday"),
    wednesday = as.integer(weekday == "wednesday"),
    thursday = as.integer(weekday == "thursday"),
    friday = as.integer(weekday == "friday"),
    saturday = as.integer(weekday == "saturday"),
    sunday = as.integer(weekday == "sunday"),
    start_date = service_date,
    end_date = service_date
  )

  stop_ids <- unique(stop_times$stop_id)
  shape_ids <- unique(stats::na.omit(trips$shape_id))
  agencies <- tibble::tribble(
    ~agency_id, ~agency_name, ~agency_url, ~agency_timezone, ~agency_lang,
    "METRO", "Metrô de São Paulo", "https://www.metro.sp.gov.br", "America/Sao_Paulo", "pt-BR",
    "CPTM", "CPTM", "https://www.cptm.sp.gov.br", "America/Sao_Paulo", "pt-BR"
  ) |>
    dplyr::filter(agency_id %in% dplyr::if_else(
      spec$name_system == "Metro", "METRO", "CPTM"
    ))
  transfers <- corrections |>
    dplyr::filter(action == "transfer") |>
    dplyr::transmute(
      from_stop_id = stop_id,
      to_stop_id = other_stop_id,
      transfer_type = 2L,
      min_transfer_time
    ) |>
    dplyr::bind_rows(
      corrections |>
        dplyr::filter(action == "transfer") |>
        dplyr::transmute(
          from_stop_id = other_stop_id,
          to_stop_id = stop_id,
          transfer_type = 2L,
          min_transfer_time
        )
    ) |>
    dplyr::distinct()
  invalid_transfer_stops <- setdiff(
    union(transfers$from_stop_id, transfers$to_stop_id),
    stop_ids
  )
  if (length(invalid_transfer_stops) > 0L) {
    stop(
      "Rail transfer corrections reference stops absent from reconstructed trips: ",
      paste(invalid_transfer_stops, collapse = ", "), "."
    )
  }

  gtfs <- list(
    agency = agencies,
    stops = template_gtfs$stops |>
      dplyr::filter(stop_id %in% .env$stop_ids),
    routes = spec |>
      dplyr::transmute(
        route_id = paste0("rail_L", code_line),
        agency_id = dplyr::if_else(name_system == "Metro", "METRO", "CPTM"),
        route_short_name = as.character(label_line),
        route_long_name = name_line,
        route_type = dplyr::if_else(name_system == "Metro", 1L, 2L)
      ),
    trips = trips,
    stop_times = stop_times,
    calendar = calendar,
    frequencies = frequencies,
    shapes = template_gtfs$shapes |>
      dplyr::filter(shape_id %in% .env$shape_ids)
  )
  if (nrow(transfers) > 0L) {
    gtfs$transfers <- transfers
  }
  gtfstools::as_dt_gtfs(gtfs)
}


write_rail_analysis_feed <- function(
  template_path,
  spec,
  stop_corrections,
  output,
  year,
  service_date,
  frequency_start = "05:30:00",
  frequency_end = "08:30:00",
  overwrite = TRUE
) {
  gtfs <- build_rail_analysis_gtfs(
    template_gtfs = gtfstools::read_gtfs(template_path),
    spec = spec,
    stop_corrections = stop_corrections,
    year = year,
    service_date = service_date,
    frequency_start = frequency_start,
    frequency_end = frequency_end
  )
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  gtfstools::write_gtfs(gtfs, output, overwrite = overwrite)
  normalizePath(output)
}


write_scenario_rail_feed <- function(
  prepared_feeds,
  feed_spec,
  routing_spec,
  service_spec,
  stop_corrections,
  output_dir = "data/gtfs/rail"
) {
  year <- unique(routing_spec$year)
  datetime <- unique(routing_spec$datetime)
  if (length(year) != 1L || length(datetime) != 1L) {
    stop("Each routing branch must contain exactly one year and datetime.")
  }

  template_rows <- feed_spec$year == year & feed_spec$remove_rail
  if (sum(template_rows) != 1L) {
    stop("Exactly one rail template feed is required for ", year, ".")
  }

  write_rail_analysis_feed(
    template_path = prepared_feeds[template_rows],
    spec = service_spec,
    stop_corrections = stop_corrections,
    output = file.path(output_dir, paste0("gtfs_rail_", year, ".zip")),
    year = year,
    service_date = as.Date(datetime)
  )
}

# prepare feeds ------------------------------------------------------------------------------

set_gtfs_service_window <- function(gtfs, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  stopifnot(!is.na(start_date), !is.na(end_date), start_date <= end_date)
  start_value <- start_date
  end_value <- end_date

  if ("calendar" %in% names(gtfs)) {
    gtfs$calendar <- gtfs$calendar |>
      dplyr::mutate(
        start_date = start_value,
        end_date = end_value
      )
  }

  # Keep only exceptions that actually belong to the new service window. We do
  # not shift old holidays into another year because their weekdays/meaning may
  # differ. The comparison dates should therefore avoid holidays.
  if ("calendar_dates" %in% names(gtfs)) {
    gtfs$calendar_dates <- gtfs$calendar_dates |>
      dplyr::mutate(exception_date = as.Date(date)) |>
      dplyr::filter(dplyr::between(exception_date, start_date, end_date)) |>
      dplyr::select(-exception_date)
  }

  gtfs
}


deduplicate_gtfs_stops <- function(gtfs) {
  if (!"stops" %in% names(gtfs) || !"stop_id" %in% names(gtfs$stops)) {
    return(list(gtfs = gtfs, removed = 0L, conflicting = 0L))
  }

  stops <- gtfs$stops
  duplicated_id <- duplicated(stops$stop_id)
  duplicated_rows <- stops[duplicated_id]
  first_rows <- stops[match(duplicated_rows$stop_id, stops$stop_id)]
  comparison_columns <- setdiff(names(stops), "stop_id")
  conflicting <- if (nrow(duplicated_rows) == 0L) {
    logical()
  } else {
    duplicate_signature <- do.call(
      paste,
      c(duplicated_rows[, ..comparison_columns], sep = "\r")
    )
    first_signature <- do.call(
      paste,
      c(first_rows[, ..comparison_columns], sep = "\r")
    )
    duplicate_signature != first_signature
  }

  # remove_duplicates() handles exact duplicate rows in every GTFS table. The
  # distinct() call additionally enforces the stop_id primary key.
  gtfs <- gtfstools::remove_duplicates(gtfs)
  gtfs$stops <- gtfs$stops |>
    dplyr::distinct(stop_id, .keep_all = TRUE)

  list(
    gtfs = gtfs,
    removed = sum(duplicated_id),
    conflicting = sum(conflicting),
    conflicting_stop_ids = unique(duplicated_rows$stop_id[conflicting])
  )
}


drop_gtfs_shape_distances <- function(gtfs) {
  changed <- character()
  for (table in c("shapes", "stop_times")) {
    if (table %in% names(gtfs) && "shape_dist_traveled" %in% names(gtfs[[table]])) {
      gtfs[[table]] <- gtfs[[table]] |>
        dplyr::select(-shape_dist_traveled)
      changed <- c(changed, paste0(table, ".txt"))
    }
  }
  list(gtfs = gtfs, changed = changed)
}


prepare_feed <- function(
  input,
  output,
  service_start,
  service_end,
  deduplicate_stops = FALSE,
  drop_shape_distances = TRUE,
  overwrite = TRUE
) {
  stopifnot(file.exists(input))
  if (file.exists(output) && !overwrite) {
    return(normalizePath(output))
  }

  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  output <- file.path(normalizePath(dirname(output)), basename(output))
  gtfs <- gtfstools::read_gtfs(input)
  gtfs <- set_gtfs_service_window(gtfs, service_start, service_end)
  duplicate_audit <- if (deduplicate_stops) {
    deduplicate_gtfs_stops(gtfs)
  } else {
    list(
      gtfs = gtfs,
      removed = 0L,
      conflicting = 0L,
      conflicting_stop_ids = character()
    )
  }
  gtfs <- duplicate_audit$gtfs
  distance_result <- if (drop_shape_distances) {
    drop_gtfs_shape_distances(gtfs)
  } else {
    list(gtfs = gtfs, changed = character())
  }
  gtfs <- distance_result$gtfs

  gtfstools::write_gtfs(gtfs, output, overwrite = overwrite)

  manifest <- list(
    input = normalizePath(input),
    output = normalizePath(output),
    service_start = as.character(as.Date(service_start)),
    service_end = as.character(as.Date(service_end)),
    stop_duplicates_removed = duplicate_audit$removed,
    conflicting_stop_duplicates = duplicate_audit$conflicting,
    conflicting_stop_ids = duplicate_audit$conflicting_stop_ids,
    shape_distance_columns_removed_from = distance_result$changed
  )
  jsonlite::write_json(
    manifest,
    sub("\\.zip$", "_manifest.json", output),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  normalizePath(output)
}


prepare_feeds <- function(spec, output_dir, overwrite = TRUE) {
  required <- c(
    "input", "output_name", "service_start", "service_end",
    "deduplicate_stops", "drop_shape_distances"
  )
  stopifnot(all(required %in% names(spec)))

  purrr::pmap_chr(
    spec[, required],
    function(
      input, output_name, service_start, service_end,
      deduplicate_stops, drop_shape_distances
    ) {
      prepare_feed(
        input = input,
        output = file.path(output_dir, output_name),
        service_start = service_start,
        service_end = service_end,
        deduplicate_stops = deduplicate_stops,
        drop_shape_distances = drop_shape_distances,
        overwrite = overwrite
      )
    }
  )
}


export_feeds <- function(
  spec,
  prepared_feeds,
  year,
  additional_feeds = NULL,
  r5_dir = "data/r5"
) {
  stopifnot(
    nrow(spec) == length(prepared_feeds),
    all(c("year", "include_r5") %in% names(spec))
  )
  selected_rows <- spec$year == year & spec$include_r5
  selected <- c(prepared_feeds[selected_rows], additional_feeds)
  if (length(selected) == 0L) {
    stop("At least one feed must be selected for the ", year, " R5 network.")
  }

  year_dir <- file.path(r5_dir, year)
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
  old_feeds <- list.files(year_dir, pattern = "\\.zip$", full.names = TRUE)
  unlink(old_feeds)

  shared_inputs <- list.files(
    r5_dir,
    pattern = "(\\.osm\\.pbf|\\.tif)$",
    full.names = TRUE,
    recursive = FALSE
  )
  if (length(shared_inputs) == 0L) {
    stop("No shared OSM PBF or raster inputs found in ", r5_dir, ".")
  }
  shared_outputs <- file.path(year_dir, basename(shared_inputs))
  shared_copied <- file.copy(shared_inputs, shared_outputs, overwrite = TRUE)
  if (!all(shared_copied)) {
    stop("Could not copy shared OSM PBF/raster inputs to all yearly R5 directories.")
  }

  output <- file.path(year_dir, basename(selected))
  copied <- file.copy(selected, output, overwrite = TRUE)
  if (!all(copied)) {
    stop("Could not export all selected GTFS feeds to ", r5_dir, ".")
  }
  normalizePath(output)
}


# sanity checks ------------------------------------------------------------------------------

gtfs_active_service_ids <- function(gtfs, date) {
  query_date <- as.Date(date)
  active <- character()

  if ("calendar" %in% names(gtfs)) {
    calendar <- gtfs$calendar
    weekday <- c(
      "sunday", "monday", "tuesday", "wednesday",
      "thursday", "friday", "saturday"
    )[as.POSIXlt(query_date)$wday + 1L]
    active <- calendar |>
      dplyr::filter(
        .env$query_date >= start_date,
        .env$query_date <= end_date,
        .data[[weekday]] == 1L
      ) |>
      dplyr::pull(service_id)
  }

  if ("calendar_dates" %in% names(gtfs)) {
    exceptions <- gtfs$calendar_dates |>
      dplyr::filter(date == .env$query_date)
    active <- setdiff(
      active,
      exceptions |>
        dplyr::filter(exception_type == 2L) |>
        dplyr::pull(service_id)
    )
    active <- union(
      active,
      exceptions |>
        dplyr::filter(exception_type == 1L) |>
        dplyr::pull(service_id)
    )
  }
  unique(active)
}


gtfs_time_to_seconds <- function(x) {
  parts <- stringr::str_split_fixed(as.character(x), ":", 3)
  as.numeric(parts[, 1]) * 3600 +
    as.numeric(parts[, 2]) * 60 +
    as.numeric(parts[, 3])
}


gtfs_service_at_time <- function(active_trips, stop_times, frequencies, query_time,
                                 time_window = 15L) {
  window_start <- as.numeric(format(query_time, "%H")) * 3600 +
    as.numeric(format(query_time, "%M")) * 60 +
    as.numeric(format(query_time, "%S"))
  window_end <- window_start + as.numeric(time_window) * 60

  first_departures <- stop_times |>
    dplyr::filter(trip_id %in% active_trips$trip_id) |>
    dplyr::group_by(trip_id) |>
    dplyr::slice_min(stop_sequence, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(departure_seconds = gtfs_time_to_seconds(departure_time))

  frequency_trip_ids <- if (is.null(frequencies)) character() else unique(frequencies$trip_id)
  scheduled <- first_departures |>
    dplyr::filter(
      !trip_id %in% frequency_trip_ids,
      departure_seconds >= window_start,
      departure_seconds < window_end
    ) |>
    dplyr::select(trip_id) |>
    dplyr::mutate(departures = 1L)

  active_frequency_entries <- if (is.null(frequencies)) {
    tibble::tibble(trip_id = character(), departures = integer())
  } else {
    frequencies |>
      dplyr::filter(trip_id %in% active_trips$trip_id) |>
      dplyr::mutate(
        start_seconds = gtfs_time_to_seconds(start_time),
        end_seconds = gtfs_time_to_seconds(end_time),
        first_index = pmax(0, ceiling((window_start - start_seconds) / headway_secs)),
        last_index = ceiling(
          (pmin(window_end, end_seconds) - start_seconds) / headway_secs
        ) - 1L,
        departures = pmax(0L, last_index - first_index + 1L)
      ) |>
      dplyr::filter(departures > 0L)
  }

  departure_patterns <- dplyr::bind_rows(
    scheduled,
    active_frequency_entries |>
      dplyr::summarise(departures = sum(departures), .by = trip_id)
  ) |>
    dplyr::left_join(
      active_trips |>
        dplyr::select(trip_id, route_id, direction_id),
      by = "trip_id"
    )
  trip_ids <- unique(departure_patterns$trip_id)
  routes <- active_trips |>
    dplyr::filter(trip_id %in% trip_ids) |>
    dplyr::pull(route_id) |>
    unique()

  list(
    n_routes = length(routes),
    n_trips = length(trip_ids),
    n_departures = sum(departure_patterns$departures),
    n_frequency_entries = nrow(active_frequency_entries),
    trip_ids = trip_ids,
    departure_patterns = departure_patterns
  )
}


gtfs_trip_speeds_from_stops <- function(gtfs, trip_ids) {
  if (length(trip_ids) == 0L) return(numeric())
  radius <- 6371008.8
  gtfs$stop_times |>
    dplyr::filter(trip_id %in% .env$trip_ids) |>
    dplyr::left_join(
      gtfs$stops |>
        dplyr::select(stop_id, stop_lon, stop_lat) |>
        dplyr::distinct(stop_id, .keep_all = TRUE),
      by = "stop_id"
    ) |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::mutate(
      lon1 = stop_lon * pi / 180,
      lat1 = stop_lat * pi / 180,
      lon2 = dplyr::lead(stop_lon) * pi / 180,
      lat2 = dplyr::lead(stop_lat) * pi / 180,
      delta_lon = lon2 - lon1,
      delta_lat = lat2 - lat1,
      a = sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2,
      segment_m = 2 * radius * atan2(sqrt(a), sqrt(1 - a)),
      event_seconds = gtfs_time_to_seconds(departure_time),
      .by = trip_id
    ) |>
    dplyr::summarise(
      distance_m = sum(segment_m, na.rm = TRUE),
      duration_seconds = max(event_seconds, na.rm = TRUE) -
        min(event_seconds, na.rm = TRUE),
      .by = trip_id
    ) |>
    dplyr::transmute(speed = distance_m / duration_seconds * 3.6) |>
    dplyr::pull(speed)
}


audit_feed <- function(
  feed_path,
  datetimes,
  time_window = 15L,
  audit_stage = "source",
  scenario_year = NA_integer_,
  bus_times_regularized = FALSE,
  rail_reconstructed = FALSE,
  rail_removed = FALSE,
  hpm_start = "06:00:00",
  hpm_window = 60L
) {
  gtfs <- gtfstools::read_gtfs(feed_path)
  trips <- gtfs$trips
  routes <- gtfs$routes
  stops <- gtfs$stops
  stop_times <- gtfs$stop_times
  frequencies <- gtfs$frequencies

  purrr::map_dfr(datetimes, function(datetime) {
    datetime <- as.POSIXct(datetime, origin = "1970-01-01", tz = attr(datetimes, "tzone"))
    date <- as.Date(datetime)
    service_ids <- gtfs_active_service_ids(gtfs, date)
    active_trips <- trips |>
      dplyr::filter(service_id %in% service_ids)
    active_trip_ids <- unique(active_trips$trip_id)
    active_stop_times <- stop_times |>
      dplyr::filter(trip_id %in% active_trip_ids)
    active_frequencies <- if (is.null(frequencies)) {
      NULL
    } else {
      frequencies |>
        dplyr::filter(trip_id %in% active_trip_ids)
    }
    service_at_time <- gtfs_service_at_time(
      active_trips,
      stop_times,
      active_frequencies,
      datetime,
      time_window
    )
    hpm_datetime <- as.POSIXct(
      paste(date, hpm_start),
      tz = attr(datetimes, "tzone")
    )
    hpm_service <- gtfs_service_at_time(
      active_trips,
      stop_times,
      active_frequencies,
      hpm_datetime,
      hpm_window
    )
    departure_patterns <- hpm_service$departure_patterns
    headways <- departure_patterns |>
      dplyr::summarise(
        departures = sum(departures),
        .by = c(route_id, direction_id)
      ) |>
      dplyr::mutate(headway_minutes = hpm_window / departures) |>
      dplyr::pull(headway_minutes)
    hpm_trip_ids <- hpm_service$trip_ids
    service_trip_ids <- service_at_time$trip_ids
    speed_geometry_source <- if ("shapes" %in% names(gtfs)) "shapes" else "stop_times"
    speeds <- if (length(hpm_trip_ids) > 0L && speed_geometry_source == "shapes") {
      tryCatch(
        gtfstools::get_trip_speed(
          gtfs,
          trip_id = hpm_trip_ids,
          file = "shapes"
        )$speed,
        error = function(error) numeric()
      )
    } else {
      numeric()
    }
    if (!any(is.finite(speeds) & speeds > 0) && length(hpm_trip_ids) > 0L) {
      speed_geometry_source <- "stop_times"
      speeds <- gtfs_trip_speeds_from_stops(gtfs, hpm_trip_ids)
    }
    finite_speeds <- speeds[is.finite(speeds) & speeds > 0]
    speed_stat <- function(fun, ...) {
      if (length(finite_speeds) == 0L) NA_real_ else fun(finite_speeds, ...)
    }
    headway_stat <- function(fun, ...) {
      if (length(headways) == 0L) NA_real_ else fun(headways, ...)
    }
    runtimes <- if (length(hpm_trip_ids) == 0L) {
      numeric()
    } else {
      active_stop_times |>
        dplyr::filter(trip_id %in% .env$hpm_trip_ids) |>
        dplyr::mutate(event_seconds = gtfs_time_to_seconds(departure_time)) |>
        dplyr::filter(is.finite(event_seconds)) |>
        dplyr::summarise(
          runtime_minutes = (max(event_seconds) - min(event_seconds)) / 60,
          .by = trip_id
        ) |>
        dplyr::filter(is.finite(runtime_minutes), runtime_minutes > 0) |>
        dplyr::pull(runtime_minutes)
    }
    routes_at_time <- active_trips |>
      dplyr::filter(trip_id %in% .env$service_trip_ids) |>
      dplyr::distinct(route_id) |>
      dplyr::left_join(routes |> dplyr::select(route_id, route_type), by = "route_id")

    tibble::tibble(
      audit_stage = audit_stage,
      scenario_year = as.integer(scenario_year),
      feed = sub("\\.zip$", "", basename(feed_path)),
      feed_role = dplyr::case_when(
        rail_reconstructed ~ "synthetic_rail",
        all(routes$route_type == 3L, na.rm = TRUE) ~ "bus",
        TRUE ~ "mixed"
      ),
      bus_times_regularized = bus_times_regularized,
      rail_reconstructed = rail_reconstructed,
      rail_removed = rail_removed,
      speed_model = dplyr::if_else(
        bus_times_regularized,
        "conditional median: H3-8 x busway class, 2015-2017",
        "source schedule"
      ),
      bus_speed_reference_years = dplyr::if_else(
        bus_times_regularized, "2015-2017", NA_character_
      ),
      bus_speed_h3_resolution = dplyr::if_else(bus_times_regularized, 8L, NA_integer_),
      busway_buffer_m = dplyr::if_else(bus_times_regularized, 25, NA_real_),
      busway_minimum_overlap = dplyr::if_else(bus_times_regularized, 0.60, NA_real_),
      date = date,
      departure_time = format(datetime, "%H:%M:%S"),
      time_window_minutes = time_window,
      hpm_start = hpm_start,
      hpm_window_minutes = hpm_window,
      active = length(service_ids) > 0L,
      n_services = length(service_ids),
      n_routes = data.table::uniqueN(active_trips$route_id),
      n_trips = length(active_trip_ids),
      n_stop_times = nrow(active_stop_times),
      n_stops_served = data.table::uniqueN(active_stop_times$stop_id),
      n_frequency_entries = if (is.null(active_frequencies)) 0L else nrow(active_frequencies),
      n_routes_at_time = service_at_time$n_routes,
      n_trips_at_time = service_at_time$n_trips,
      n_departures_in_window = service_at_time$n_departures,
      n_frequency_entries_at_time = service_at_time$n_frequency_entries,
      n_route_directions_in_hpm = length(headways),
      n_routes_in_hpm = hpm_service$n_routes,
      n_departures_in_hpm = hpm_service$n_departures,
      headway_mean_minutes = headway_stat(mean),
      headway_median_minutes = headway_stat(stats::median),
      headway_p10_minutes = headway_stat(stats::quantile, 0.1, names = FALSE),
      headway_p90_minutes = headway_stat(stats::quantile, 0.9, names = FALSE),
      n_speed_estimates = length(speeds),
      speed_geometry_source = speed_geometry_source,
      n_invalid_speeds = sum(!is.finite(speeds) | speeds <= 0),
      speed_mean_kmh = speed_stat(mean),
      speed_median_kmh = speed_stat(stats::median),
      speed_p10_kmh = speed_stat(stats::quantile, 0.1, names = FALSE),
      speed_p90_kmh = speed_stat(stats::quantile, 0.9, names = FALSE),
      runtime_mean_minutes = if (length(runtimes) == 0L) NA_real_ else mean(runtimes),
      runtime_median_minutes = if (length(runtimes) == 0L) NA_real_ else stats::median(runtimes),
      n_bus_routes_at_time = sum(routes_at_time$route_type == 3L, na.rm = TRUE),
      n_rail_routes_at_time = sum(routes_at_time$route_type %in% c(0L, 1L, 2L), na.rm = TRUE),
      uses_frequencies = !is.null(frequencies) && nrow(frequencies) > 0L,
      n_stops_total = nrow(stops),
      n_routes_total = nrow(routes),
      n_trips_total = nrow(trips)
    )
  })
}


audit_feeds <- function(feed_paths, years, datetimes, time_window = 15L) {
  stopifnot(length(feed_paths) == length(years))
  datetime_years <- as.integer(format(datetimes, "%Y"))

  purrr::map2_dfr(feed_paths, years, function(feed_path, year) {
    selected_datetime <- datetimes[datetime_years == year]
    if (length(selected_datetime) != 1L) {
      stop("Expected exactly one routing datetime for year ", year, ".")
    }
    audit_feed(
      feed_path,
      datetimes = selected_datetime,
      time_window = time_window
    )
  })
}


audit_scenario_feeds <- function(
  bus_feeds,
  rail_feed,
  feed_spec,
  routing_spec,
  time_window = 15L
) {
  year <- unique(routing_spec$year)
  datetime <- unique(routing_spec$datetime)
  if (length(year) != 1L || length(datetime) != 1L) {
    stop("Each routing branch must contain exactly one year and datetime.")
  }

  selected <- feed_spec$year == year & feed_spec$include_r5
  scenario_feeds <- c(bus_feeds[selected], rail_feed)
  bus_rows <- purrr::map_dfr(which(selected), function(index) {
    audit_feed(
      bus_feeds[index], datetime, time_window,
      audit_stage = "scenario",
      scenario_year = year,
      bus_times_regularized = feed_spec$regularize_bus_times[index],
      rail_removed = feed_spec$remove_rail[index]
    )
  })
  rail_row <- audit_feed(
    rail_feed, datetime, time_window,
    audit_stage = "scenario",
    scenario_year = year,
    rail_reconstructed = TRUE
  )
  dplyr::bind_rows(bus_rows, rail_row)
}


audit_source_feeds <- function(feed_paths, feed_spec, time = "06:50:00", time_window = 15L) {
  stopifnot(length(feed_paths) == nrow(feed_spec))
  purrr::map_dfr(seq_along(feed_paths), function(index) {
    datetime <- as.POSIXct(
      paste(feed_spec$source_audit_date[index], time),
      tz = "America/Sao_Paulo"
    )
    audit_feed(
      feed_paths[index], datetime, time_window,
      audit_stage = "source",
      scenario_year = feed_spec$year[index]
    )
  })
}

# validate feeds -----------------------------------------------------------------------------

validate_feeds <- function(
  feed_paths,
  validator_dir,
  validator_root = "data/gtfs_validator"
) {
  if (!dir.exists(validator_dir)) {
    dir.create(validator_dir, recursive = TRUE)
  }
  if (!dir.exists(validator_root)) {
    dir.create(validator_root, recursive = TRUE)
  }
  validator_path <- list.files(validator_root, pattern = "jar$", full.names = T)
  if (length(validator_path) == 0) {
    gtfstools::download_validator(validator_root)
    validator_path <- list.files(
      validator_root,
      pattern = "jar$",
      full.names = T
    )
  }
  validator_path <- validator_path[1]

  reports <- purrr::map(
    feed_paths,
    function(x) {
      base_name <- stringr::str_remove(basename(x), "\\.zip$")
      gtfstools::validate_gtfs(
        x,
        output_path = validator_dir,
        validator = validator_path
      )
      html_old <- file.path(validator_dir, "report.html")
      html_new <- file.path(validator_dir, paste0("report_", base_name, ".html"))
      json_old <- file.path(validator_dir, "report.json")
      json_new <- file.path(validator_dir, paste0("report_", base_name, ".json"))
      file.copy(html_old, html_new, overwrite = TRUE)
      file.copy(json_old, json_new, overwrite = TRUE)
      c(html_new, json_new)
    }
  )

  unlink(file.path(validator_dir, c("report.html", "report.json")))
  return(unlist(reports, use.names = FALSE))
}


validate_scenario_feeds <- function(
  bus_feeds,
  rail_feed,
  feed_spec,
  routing_spec,
  validator_dir = "data/gtfs_validator/scenario"
) {
  year <- unique(routing_spec$year)
  stopifnot(length(year) == 1L)
  selected <- feed_spec$year == year & feed_spec$include_r5
  validate_feeds(
    c(bus_feeds[selected], rail_feed),
    file.path(validator_dir, as.character(year))
  )
}

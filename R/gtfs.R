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


prepare_gtfs_feed <- function(
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


prepare_gtfs_feeds <- function(spec, output_dir, overwrite = TRUE) {
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
      prepare_gtfs_feed(
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


export_selected_gtfs <- function(spec, prepared_feeds, r5_dir = "data/r5") {
  stopifnot(nrow(spec) == length(prepared_feeds), "include_r5" %in% names(spec))
  selected <- prepared_feeds[spec$include_r5]
  if (length(selected) == 0L) {
    stop("At least one feed must be selected for the R5 network.")
  }

  dir.create(r5_dir, recursive = TRUE, showWarnings = FALSE)
  excluded <- file.path(r5_dir, spec$output_name[!spec$include_r5])
  unlink(excluded[file.exists(excluded)])
  output <- file.path(normalizePath(r5_dir), basename(selected))
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


gtfs_feed_audit <- function(feed_path, dates) {
  gtfs <- gtfstools::read_gtfs(feed_path)
  trips <- gtfs$trips
  routes <- gtfs$routes
  stops <- gtfs$stops
  stop_times <- gtfs$stop_times
  frequencies <- gtfs$frequencies

  purrr::map_dfr(as.Date(dates), function(date) {
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
    speeds <- if ("shapes" %in% names(gtfs) && length(active_trip_ids) > 0L) {
      gtfstools::get_trip_speed(
        gtfs,
        trip_id = active_trip_ids,
        file = "shapes"
      )$speed
    } else {
      numeric()
    }
    finite_speeds <- speeds[is.finite(speeds) & speeds > 0]
    speed_stat <- function(fun, ...) {
      if (length(finite_speeds) == 0L) NA_real_ else fun(finite_speeds, ...)
    }

    tibble::tibble(
      feed = sub("\\.zip$", "", basename(feed_path)),
      date = date,
      active = length(service_ids) > 0L,
      n_services = length(service_ids),
      n_routes = data.table::uniqueN(active_trips$route_id),
      n_trips = length(active_trip_ids),
      n_stop_times = nrow(active_stop_times),
      n_stops_served = data.table::uniqueN(active_stop_times$stop_id),
      n_frequency_entries = if (is.null(active_frequencies)) 0L else nrow(active_frequencies),
      n_speed_estimates = length(speeds),
      n_invalid_speeds = sum(!is.finite(speeds) | speeds <= 0),
      speed_mean_kmh = speed_stat(mean),
      speed_median_kmh = speed_stat(stats::median),
      speed_p10_kmh = speed_stat(stats::quantile, 0.1, names = FALSE),
      speed_p90_kmh = speed_stat(stats::quantile, 0.9, names = FALSE),
      n_stops_total = nrow(stops),
      n_routes_total = nrow(routes),
      n_trips_total = nrow(trips)
    )
  })
}


audit_gtfs_feeds <- function(feed_paths, dates) {
  purrr::map_dfr(feed_paths, gtfs_feed_audit, dates = dates)
}

# validate feeds -----------------------------------------------------------------------------

validate_gtfs_feeds <- function(feed_paths, validator_dir) {
  if (!dir.exists(validator_dir)) {
    dir.create(validator_dir, recursive = TRUE)
  }
  validator_path <- list.files(validator_dir, pattern = "jar$", full.names = T)
  if (length(validator_path) == 0) {
    gtfstools::download_validator(validator_dir)
    validator_path <- list.files(
      validator_dir,
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

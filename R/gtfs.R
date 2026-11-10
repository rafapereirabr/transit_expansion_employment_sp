# prepare feeds ------------------------------------------------------------------------------

gtfs_integrity_counts <- function(gtfs) {
	count_orphans <- function(child, child_key, parent, parent_key) {
		if (is.null(child) || is.null(parent)) {
			return(NA_integer_)
		}
		length(setdiff(unique(child[[child_key]]), unique(parent[[parent_key]])))
	}

	list(
		orphan_stop_time_trip_ids = count_orphans(
			gtfs$stop_times,
			"trip_id",
			gtfs$trips,
			"trip_id"
		),
		orphan_trip_route_ids = count_orphans(gtfs$trips, "route_id", gtfs$routes, "route_id"),
		orphan_stop_time_stop_ids = count_orphans(
			gtfs$stop_times,
			"stop_id",
			gtfs$stops,
			"stop_id"
		),
		orphan_trip_service_ids = count_orphans(
			gtfs$trips,
			"service_id",
			gtfs$calendar,
			"service_id"
		)
	)
}


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


drop_nonstandard_gtfs_tables <- function(gtfs) {
	standard_tables <- c(
		"agency",
		"stops",
		"routes",
		"trips",
		"stop_times",
		"calendar",
		"calendar_dates",
		"fare_attributes",
		"fare_rules",
		"fare_media",
		"fare_products",
		"fare_leg_rules",
		"fare_transfer_rules",
		"shapes",
		"frequencies",
		"transfers",
		"pathways",
		"levels",
		"feed_info",
		"translations",
		"attributions",
		"areas",
		"stop_areas"
	)
	gtfs[names(gtfs) %in% standard_tables]
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
	gtfs <- gtfstools::read_gtfs(input) |>
		drop_nonstandard_gtfs_tables()
	input_counts <- purrr::map_int(gtfs, nrow)
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
	output_counts <- purrr::map_int(gtfs, nrow)
	integrity <- gtfs_integrity_counts(gtfs)

	gtfstools::write_gtfs(gtfs, output, overwrite = overwrite)

	manifest <- list(
		input = normalizePath(input),
		output = normalizePath(output),
		service_start = as.character(as.Date(service_start)),
		service_end = as.character(as.Date(service_end)),
		stop_duplicates_removed = duplicate_audit$removed,
		conflicting_stop_duplicates = duplicate_audit$conflicting,
		conflicting_stop_ids = duplicate_audit$conflicting_stop_ids,
		shape_distance_columns_removed_from = distance_result$changed,
		input_rows = as.list(input_counts),
		output_rows = as.list(output_counts),
		integrity = integrity
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
		"input",
		"output_name",
		"service_start",
		"service_end",
		"deduplicate_stops",
		"drop_shape_distances"
	)
	stopifnot(all(required %in% names(spec)))

	purrr::pmap_chr(
		spec[, required],
		function(
			input,
			output_name,
			service_start,
			service_end,
			deduplicate_stops,
			drop_shape_distances
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
	year = NULL,
	additional_feeds = NULL,
	r5_dir = "data/r5",
	output_subdir = NULL
) {
	stopifnot(
		# nrow(spec) == length(prepared_feeds),
		all(c("year", "include_r5") %in% names(spec))
	)
	if (is.null(year)) {
		output_subdir <- "all"
	} else {
		output_subdir <- as.character(year)
	}
	if (!is.null(year) && length(year) != 1L) {
		stop("`year` must be NULL or a single year.")
	}
	# selected_year <- if (is.null(year)) rep(TRUE, nrow(spec)) else spec$year == year
	# selected_rows <- spec$include_r5 & selected_year
	# selected <- c(prepared_feeds[selected_rows], additional_feeds)
	selected <- c(prepared_feeds, additional_feeds) |>
		grep(pattern = year, x = _, value = T)
	if (length(selected) == 0L) {
		stop("At least one feed must be selected for the R5 network.")
	}
	if (anyDuplicated(basename(selected))) {
		duplicates <- unique(basename(selected)[duplicated(basename(selected))])
		stop("R5 feed filenames must be unique: ", paste(duplicates, collapse = ", "), ".")
	}

	output_dir <- file.path(r5_dir, output_subdir)
	dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
	old_feeds <- list.files(output_dir, pattern = "\\.zip$", full.names = TRUE)
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
	shared_outputs <- file.path(output_dir, basename(shared_inputs))
	shared_copied <- file.copy(shared_inputs, shared_outputs, overwrite = TRUE)
	if (!all(shared_copied)) {
		stop("Could not copy shared OSM PBF/raster inputs to ", output_dir, ".")
	}

	output <- file.path(output_dir, basename(selected))
	copied <- file.copy(selected, output, overwrite = TRUE)
	if (!all(copied)) {
		stop("Could not export all selected GTFS feeds to ", output_dir, ".")
	}
	normalizePath(output)
}


# feed summaries -----------------------------------------------------------------------------

write_gtfs_summary <- function(
	feed_path,
	year,
	date,
	stage,
	feed_role,
	from = "06:00:00",
	to = "07:00:00",
	output_root = "data/gtfs/audits"
) {
	feed_name <- sub("\\.zip$", "", basename(feed_path))
	output_dir <- file.path(output_root, stage, as.character(year), feed_name)
	dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

	summary <- gtfstools::get_summary(
		feed_path,
		date = as.Date(date),
		from = from,
		to = to,
		output_dir = output_dir,
		open_browser = FALSE
	)
	bundle <- list(
		summary = summary,
		context = list(
			audit_stage = stage,
			scenario_year = as.integer(year),
			feed = feed_name,
			feed_role = feed_role,
			date = as.character(as.Date(date)),
			from = from,
			to = to
		)
	)
	bundle_path <- file.path(output_dir, "summary.rds")
	saveRDS(bundle, bundle_path)

	files <- c(bundle_path, list.files(output_dir, pattern = "^gtfs_summary", full.names = TRUE))
	normalizePath(unique(files))
}


write_gtfs_summaries <- function(
	feed_paths,
	feed_spec,
	stage,
	feed_role,
	from = "06:00:00",
	to = "07:00:00",
	output_root = "data/gtfs/audits"
) {
	stopifnot(length(feed_paths) == nrow(feed_spec))
	purrr::map2(
		feed_paths,
		seq_along(feed_paths),
		function(feed_path, index) {
			write_gtfs_summary(
				feed_path = feed_path,
				year = feed_spec$year[index],
				date = feed_spec$analysis_date[index],
				stage = stage,
				feed_role = feed_role,
				from = from,
				to = to,
				output_root = output_root
			)
		}
	) |>
		unlist(use.names = FALSE)
}


summary_indicator <- function(totals, indicator) {
	value <- totals$value[totals$indicator == indicator]
	if (length(value) == 0L) NA_real_ else value[[1]]
}


tidy_gtfs_summary <- function(bundle) {
	summary <- bundle$summary
	context <- bundle$context
	services <- summary$services[1]
	speed <- summary$speed[1]
	frequency <- summary$frequency[1]
	distance <- summary$distance[1]

	tibble::tibble(
		audit_stage = context$audit_stage,
		scenario_year = context$scenario_year,
		feed = context$feed,
		feed_role = context$feed_role,
		date = as.Date(context$date),
		from = context$from,
		to = context$to,
		active = summary_indicator(summary$totals, "n_trips") > 0,
		n_services = services$n_services,
		service_start_date = services$start_date,
		service_end_date = services$end_date,
		n_service_days = services$n_service_days,
		n_agencies = as.integer(summary_indicator(summary$totals, "n_agencies")),
		n_routes = as.integer(summary_indicator(summary$totals, "n_routes")),
		n_trips = as.integer(summary_indicator(summary$totals, "n_trips")),
		n_stops = as.integer(summary_indicator(summary$totals, "n_stops")),
		total_shape_length_km = summary_indicator(summary$totals, "total_shape_length"),
		mean_shape_length_km = summary_indicator(summary$totals, "mean_shape_length"),
		total_trip_length_km = summary_indicator(summary$totals, "total_trip_length"),
		mean_trip_length_km = summary_indicator(summary$totals, "mean_trip_length"),
		total_trip_duration_hours = summary_indicator(summary$totals, "total_trip_duration"),
		mean_trip_duration_minutes = summary_indicator(summary$totals, "mean_trip_duration"),
		n_speed_estimates = speed$n,
		speed_q25_kmh = speed$q25,
		speed_median_kmh = speed$median,
		speed_mean_kmh = speed$mean,
		speed_q75_kmh = speed$q75,
		n_headway_estimates = frequency$n,
		headway_q25_minutes = frequency$q25,
		headway_median_minutes = frequency$median,
		headway_mean_minutes = frequency$mean,
		headway_q75_minutes = frequency$q75,
		n_distance_estimates = distance$n,
		distance_q25_km = distance$q25,
		distance_median_km = distance$median,
		distance_mean_km = distance$mean,
		distance_q75_km = distance$q75
	)
}


read_gtfs_summary_audit <- function(summary_files) {
	bundle_paths <- summary_files[basename(summary_files) == "summary.rds"]
	purrr::map_dfr(bundle_paths, ~ tidy_gtfs_summary(readRDS(.x)))
}


feed_manifest_paths <- function(feed_paths) {
	paths <- sub("\\.zip$", "_manifest.json", feed_paths)
	if (!all(file.exists(paths))) {
		stop("Missing feed manifests: ", paste(paths[!file.exists(paths)], collapse = ", "))
	}
	normalizePath(paths)
}


manifest_table_count <- function(manifest, section, table) {
	value <- manifest[[section]][[table]]
	if (is.null(value)) NA_integer_ else as.integer(value)
}


manifest_integrity_count <- function(manifest, field) {
	value <- manifest$integrity[[field]]
	if (is.null(value)) NA_integer_ else as.integer(value)
}


read_prepared_feed_audit <- function(manifest_paths, feed_spec) {
	stopifnot(length(manifest_paths) == nrow(feed_spec))
	purrr::map2_dfr(manifest_paths, seq_along(manifest_paths), function(path, index) {
		manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
		tibble::tibble(
			audit_stage = "prepared",
			scenario_year = feed_spec$year[index],
			feed = sub("_manifest\\.json$", "", basename(path)),
			analysis_date = feed_spec$analysis_date[index],
			service_start = as.Date(manifest$service_start),
			service_end = as.Date(manifest$service_end),
			stop_duplicates_removed = manifest$stop_duplicates_removed,
			conflicting_stop_duplicates = manifest$conflicting_stop_duplicates,
			shape_distance_columns_removed_from = paste(
				manifest$shape_distance_columns_removed_from,
				collapse = ","
			),
			input_routes = manifest_table_count(manifest, "input_rows", "routes"),
			output_routes = manifest_table_count(manifest, "output_rows", "routes"),
			input_trips = manifest_table_count(manifest, "input_rows", "trips"),
			output_trips = manifest_table_count(manifest, "output_rows", "trips"),
			input_stops = manifest_table_count(manifest, "input_rows", "stops"),
			output_stops = manifest_table_count(manifest, "output_rows", "stops"),
			orphan_stop_time_trip_ids = manifest_integrity_count(
				manifest,
				"orphan_stop_time_trip_ids"
			),
			orphan_trip_route_ids = manifest_integrity_count(manifest, "orphan_trip_route_ids"),
			orphan_stop_time_stop_ids = manifest_integrity_count(
				manifest,
				"orphan_stop_time_stop_ids"
			),
			orphan_trip_service_ids = manifest_integrity_count(
				manifest,
				"orphan_trip_service_ids"
			)
		)
	})
}


read_bus_feed_manifest_audit <- function(manifest_paths, feed_spec) {
	stopifnot(length(manifest_paths) == nrow(feed_spec))
	purrr::map2_dfr(manifest_paths, seq_along(manifest_paths), function(path, index) {
		manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
		tibble::tibble(
			audit_stage = "bus_transform",
			scenario_year = feed_spec$year[index],
			feed = sub("_manifest\\.json$", "", basename(path)),
			analysis_date = feed_spec$analysis_date[index],
			remove_rail = manifest$remove_rail,
			regularize_bus_times = manifest$regularize_bus_times,
			routes_removed = manifest_table_count(manifest, "input_rows", "routes") -
				manifest_table_count(manifest, "output_rows", "routes"),
			trips_removed = manifest_table_count(manifest, "input_rows", "trips") -
				manifest_table_count(manifest, "output_rows", "trips"),
			stops_removed = manifest_table_count(manifest, "input_rows", "stops") -
				manifest_table_count(manifest, "output_rows", "stops"),
			orphan_stop_time_trip_ids = manifest_integrity_count(
				manifest,
				"orphan_stop_time_trip_ids"
			),
			orphan_trip_route_ids = manifest_integrity_count(manifest, "orphan_trip_route_ids"),
			orphan_stop_time_stop_ids = manifest_integrity_count(
				manifest,
				"orphan_stop_time_stop_ids"
			),
			orphan_trip_service_ids = manifest_integrity_count(
				manifest,
				"orphan_trip_service_ids"
			)
		)
	})
}


gtfs_time_to_seconds <- function(x) {
	parts <- stringr::str_split_fixed(x, ":", 3)
	as.numeric(parts[, 1]) * 3600 +
		as.numeric(parts[, 2]) * 60 +
		as.numeric(parts[, 3])
}


# sanity checks ------------------------------------------------------------------------------

audit_scenario_calendar_overlap <- function(
	bus_feeds,
	rail_feed,
	feed_spec,
	routing_spec
) {
	year <- unique(routing_spec$year)
	datetime <- unique(routing_spec$datetime)
	if (length(year) != 1L || length(datetime) != 1L) {
		stop("Each routing branch must contain exactly one year and datetime.")
	}

	selected <- feed_spec$year == year & feed_spec$include_r5
	feed_paths <- c(bus_feeds[selected], rail_feed)
	if (length(feed_paths) < 2L || !all(file.exists(feed_paths))) {
		stop("Calendar overlap requires every bus and rail feed in the routing scenario.")
	}

	analysis_date <- as.Date(datetime, tz = "America/Sao_Paulo")
	overlap_periods <- gtfstools::get_calendar_overlap(feed_paths, resolution = "periods")
	covered_periods <- overlap_periods |>
		dplyr::filter(
			start_date <= .env$analysis_date,
			end_date >= .env$analysis_date
		)
	if (nrow(covered_periods) == 0L) {
		stop(
			"The bus and rail feeds for ",
			year,
			" do not share service on ",
			analysis_date,
			"."
		)
	}

	tibble::tibble(
		scenario_year = as.integer(year),
		analysis_date = analysis_date,
		n_feeds = length(feed_paths),
		overlap_start = min(covered_periods$start_date),
		overlap_end = max(covered_periods$end_date),
		active = TRUE
	)
}


assemble_scenario_feed_audit <- function(
	bus_feed_audit,
	rail_feed_audit,
	routing_spec,
	calendar_overlap
) {
	year <- unique(routing_spec$year)
	analysis_date <- as.Date(unique(routing_spec$datetime), tz = "America/Sao_Paulo")
	if (
		length(year) != 1L ||
			length(analysis_date) != 1L ||
			nrow(calendar_overlap) != 1L ||
			!isTRUE(calendar_overlap$active) ||
			calendar_overlap$scenario_year != year ||
			calendar_overlap$analysis_date != analysis_date
	) {
		stop("The scenario calendar-overlap gate is missing or does not match the routing branch.")
	}

	dplyr::bind_rows(
		bus_feed_audit |> dplyr::filter(scenario_year == .env$year),
		rail_feed_audit
	) |>
		dplyr::mutate(
			component_stage = audit_stage,
			audit_stage = "scenario",
			.before = scenario_year
		)
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

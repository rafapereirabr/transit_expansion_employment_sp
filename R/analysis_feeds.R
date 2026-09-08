# analysis feed helpers ----------------------------------------------------------------------

gtfs_trip_time_bounds <- function(gtfs) {
	stop_times <- gtfs$stop_times |>
		dplyr::mutate(
			arrival_seconds = gtfs_time_to_seconds(arrival_time),
			departure_seconds = gtfs_time_to_seconds(departure_time),
			event_start = pmin(arrival_seconds, departure_seconds, na.rm = TRUE),
			event_end = pmax(arrival_seconds, departure_seconds, na.rm = TRUE)
		) |>
		dplyr::group_by(trip_id) |>
		dplyr::summarise(
			trip_start = min(event_start, na.rm = TRUE),
			trip_end = max(event_end, na.rm = TRUE),
			.groups = "drop"
		)

	# In frequency-based GTFS, stop_times are offsets from the template's first
	# departure. Translate those offsets to the clock time of each frequency row.
	if (!is.null(gtfs$frequencies) && nrow(gtfs$frequencies) > 0L) {
		frequency_bounds <- gtfs$frequencies |>
			dplyr::mutate(
				frequency_start = gtfs_time_to_seconds(start_time),
				frequency_end = gtfs_time_to_seconds(end_time)
			) |>
			dplyr::left_join(stop_times, by = "trip_id") |>
			dplyr::mutate(
				first_event = frequency_start,
				last_event = frequency_end + trip_end - trip_start
			) |>
			dplyr::select(trip_id, first_event, last_event)
	} else {
		frequency_bounds <- tibble::tibble(
			trip_id = character(),
			first_event = numeric(),
			last_event = numeric()
		)
	}

	scheduled_bounds <- stop_times |>
		dplyr::filter(!trip_id %in% frequency_bounds$trip_id) |>
		dplyr::transmute(trip_id, first_event = trip_start, last_event = trip_end)

	dplyr::bind_rows(scheduled_bounds, frequency_bounds) |>
		dplyr::group_by(trip_id) |>
		dplyr::summarise(
			first_event = min(first_event, na.rm = TRUE),
			last_event = max(last_event, na.rm = TRUE),
			.groups = "drop"
		)
}


subset_gtfs_relations <- function(gtfs, trip_ids, service_ids) {
	gtfs$trips <- gtfs$trips |>
		dplyr::filter(trip_id %in% .env$trip_ids)
	gtfs$stop_times <- gtfs$stop_times |>
		dplyr::filter(trip_id %in% .env$trip_ids)

	route_ids <- unique(gtfs$trips$route_id)
	shape_ids <- unique(stats::na.omit(gtfs$trips$shape_id))
	stop_ids <- unique(gtfs$stop_times$stop_id)

	gtfs$routes <- gtfs$routes |>
		dplyr::filter(route_id %in% .env$route_ids)
	if (!is.null(gtfs$agency) && "agency_id" %in% names(gtfs$routes)) {
		agency_ids <- unique(gtfs$routes$agency_id)
		gtfs$agency <- gtfs$agency |>
			dplyr::filter(agency_id %in% .env$agency_ids)
	}
	if (!is.null(gtfs$fare_rules) && "route_id" %in% names(gtfs$fare_rules)) {
		gtfs$fare_rules <- gtfs$fare_rules |>
			dplyr::filter(is.na(route_id) | route_id %in% .env$route_ids)
	}
	gtfs$stops <- gtfs$stops |>
		dplyr::filter(stop_id %in% .env$stop_ids)

	if (!is.null(gtfs$shapes)) {
		gtfs$shapes <- gtfs$shapes |>
			dplyr::filter(shape_id %in% .env$shape_ids)
	}
	if (!is.null(gtfs$frequencies)) {
		gtfs$frequencies <- gtfs$frequencies |>
			dplyr::filter(trip_id %in% .env$trip_ids)
	}
	if (!is.null(gtfs$calendar)) {
		gtfs$calendar <- gtfs$calendar |>
			dplyr::filter(service_id %in% .env$service_ids)
	}
	if (!is.null(gtfs$calendar_dates)) {
		gtfs$calendar_dates <- gtfs$calendar_dates |>
			dplyr::filter(service_id %in% .env$service_ids)
	}
	if (!is.null(gtfs$transfers)) {
		gtfs$transfers <- gtfs$transfers |>
			dplyr::filter(
				from_stop_id %in% .env$stop_ids,
				to_stop_id %in% .env$stop_ids
			)
	}

	gtfs
}


trim_gtfs_to_analysis_window <- function(
	gtfs,
	service_date,
	window_start = "05:30:00",
	window_end = "08:30:00"
) {
	service_date <- as.Date(service_date)
	active_services <- gtfs_active_service_ids(gtfs, service_date)
	if (length(active_services) == 0L) {
		stop("No active GTFS service found on ", service_date, ".")
	}

	active_trips <- gtfs$trips |>
		dplyr::filter(service_id %in% .env$active_services)

	stop_bounds <- gtfs$stop_times |>
		dplyr::mutate(
			event_start = pmin(
				gtfs_time_to_seconds(arrival_time),
				gtfs_time_to_seconds(departure_time),
				na.rm = TRUE
			),
			event_end = pmax(
				gtfs_time_to_seconds(arrival_time),
				gtfs_time_to_seconds(departure_time),
				na.rm = TRUE
			)
		) |>
		dplyr::group_by(trip_id) |>
		dplyr::summarise(
			trip_start = min(event_start, na.rm = TRUE),
			trip_end = max(event_end, na.rm = TRUE),
			.groups = "drop"
		)
	all_frequency_trip_ids <- if (is.null(gtfs$frequencies)) {
		character()
	} else {
		unique(gtfs$frequencies$trip_id)
	}

	if (!is.null(gtfs$frequencies) && nrow(gtfs$frequencies) > 0L) {
		frequency_columns <- names(gtfs$frequencies)
		gtfs$frequencies <- gtfs$frequencies |>
			dplyr::left_join(stop_bounds, by = "trip_id") |>
			dplyr::mutate(
				first_event = gtfs_time_to_seconds(start_time),
				last_event = gtfs_time_to_seconds(end_time) + trip_end - trip_start
			) |>
			dplyr::filter(
				first_event <= gtfs_time_to_seconds(window_end),
				last_event >= gtfs_time_to_seconds(window_start)
			) |>
			dplyr::select(dplyr::all_of(frequency_columns))
	}

	scheduled_trip_ids <- stop_bounds |>
		dplyr::filter(
			!trip_id %in% .env$all_frequency_trip_ids,
			trip_start <= gtfs_time_to_seconds(window_end),
			trip_end >= gtfs_time_to_seconds(window_start)
		) |>
		dplyr::pull(trip_id)
	retained_frequency_trip_ids <- if (is.null(gtfs$frequencies)) {
		character()
	} else {
		unique(gtfs$frequencies$trip_id)
	}
	trip_ids <- intersect(
		active_trips$trip_id,
		union(scheduled_trip_ids, retained_frequency_trip_ids)
	)
	if (length(trip_ids) == 0L) {
		stop("No trips overlap the analysis window on ", service_date, ".")
	}

	service_ids <- active_trips |>
		dplyr::filter(trip_id %in% .env$trip_ids) |>
		dplyr::pull(service_id) |>
		unique()
	gtfs <- subset_gtfs_relations(gtfs, trip_ids, service_ids)

	# The analytical feed represents one specific routing date. Calendar
	# exceptions have already been resolved above, so keeping them would only
	# reintroduce ambiguity.
	if (!is.null(gtfs$calendar)) {
		gtfs$calendar <- gtfs$calendar |>
			dplyr::mutate(start_date = service_date, end_date = service_date)
	}
	gtfs$calendar_dates <- NULL

	attr(gtfs, "analysis_manifest") <- list(
		service_date = as.character(service_date),
		window_start = window_start,
		window_end = window_end,
		active_services = length(service_ids),
		retained_trips = length(trip_ids),
		retained_routes = length(unique(gtfs$trips$route_id)),
		retained_stops = length(unique(gtfs$stop_times$stop_id))
	)
	gtfs
}


write_analysis_feed <- function(
	input,
	output,
	service_date,
	window_start = "05:30:00",
	window_end = "08:30:00",
	overwrite = TRUE
) {
	stopifnot(file.exists(input))
	dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

	gtfs <- gtfstools::read_gtfs(input)
	input_counts <- purrr::map_int(gtfs, nrow)
	gtfs <- trim_gtfs_to_analysis_window(
		gtfs,
		service_date = service_date,
		window_start = window_start,
		window_end = window_end
	)
	analysis_manifest <- attr(gtfs, "analysis_manifest")
	output_counts <- purrr::map_int(gtfs, nrow)

	gtfstools::write_gtfs(gtfs, output, overwrite = overwrite)
	manifest <- c(
		list(input = normalizePath(input), output = normalizePath(output)),
		analysis_manifest,
		list(input_rows = input_counts, output_rows = output_counts)
	)
	jsonlite::write_json(
		manifest,
		sub("\\.zip$", "_manifest.json", output),
		pretty = TRUE,
		auto_unbox = TRUE
	)

	normalizePath(output)
}


write_analysis_feeds <- function(
	feed_paths,
	spec,
	output_dir = "data/gtfs/analysis",
	window_start = "05:30:00",
	window_end = "08:30:00",
	overwrite = TRUE
) {
	stopifnot(
		length(feed_paths) == nrow(spec),
		all(c("output_name", "analysis_date") %in% names(spec))
	)

	purrr::map2_chr(seq_along(feed_paths), feed_paths, function(index, input) {
		write_analysis_feed(
			input = input,
			output = file.path(output_dir, spec$output_name[index]),
			service_date = spec$analysis_date[index],
			window_start = window_start,
			window_end = window_end,
			overwrite = overwrite
		)
	})
}


# synthetic bus travel times ----------------------------------------------------------------

validate_input_bus_speed_surface <- function(surface) {
	required <- c(
		"h3",
		"segregation",
		"speed_kmh",
		"class_speed_kmh",
		"observations",
		"years"
	)
	stopifnot(all(required %in% names(surface)))
	tibble::as_tibble(surface) |>
		dplyr::filter(
			!is.na(h3),
			is.finite(speed_kmh),
			speed_kmh > 0,
			observations > 0
		)
}


read_scenario_busways <- function(geosampa_path, mobilidados_path, service_date) {
	service_date <- as.Date(service_date)
	geosampa <- sf::st_read(geosampa_path, quiet = TRUE) |>
		dplyr::filter(
			!is.na(dt_implantacao_faixa),
			dt_implantacao_faixa <= service_date,
			nm_tipo_faixa_corredor != "TEMPORARIA"
		) |>
		dplyr::transmute(
			segregation = dplyr::if_else(
				nm_tipo_faixa_corredor == "EXCLUSIVA",
				"segregated_lane",
				"priority_lane"
			)
		) |>
		sf::st_transform(31983)
	names(geosampa)[names(geosampa) == attr(geosampa, "sf_column")] <- "geometry"
	sf::st_geometry(geosampa) <- "geometry"

	mobilidados_layer <- paste0(
		"/vsizip/",
		normalizePath(mobilidados_path),
		"/BRT_Corredores.shp"
	)
	mobilidados <- sf::st_read(mobilidados_layer, quiet = TRUE) |>
		dplyr::filter(
			Cidade_n == "São Paulo",
			`Situação` == "Operacional",
			is.finite(Ano),
			Ano >= 1900,
			Ano <= as.integer(format(service_date, "%Y")),
			Segregacao %in% c("Física", "Exclusiva", "Visual")
		) |>
		dplyr::transmute(
			segregation = dplyr::if_else(
				Segregacao %in% c("Física", "Exclusiva"),
				"fully_segregated",
				"priority_lane"
			)
		) |>
		sf::st_transform(31983)
	names(mobilidados)[names(mobilidados) == attr(mobilidados, "sf_column")] <-
		"geometry"
	sf::st_geometry(mobilidados) <- "geometry"

	dplyr::bind_rows(geosampa, mobilidados)
}


bus_points_to_h3 <- function(lon, lat, resolution = 8L) {
	result <- rep(NA_character_, length(lon))
	valid <- is.finite(lon) & is.finite(lat)
	if (!any(valid)) {
		return(result)
	}
	points <- sf::st_as_sf(
		tibble::tibble(lon = lon[valid], lat = lat[valid]),
		coords = c("lon", "lat"),
		crs = 4326
	)
	result[valid] <- as.character(
		h3o::h3_from_points(sf::st_geometry(points), resolution)
	)
	result
}


classify_busway_segments <- function(segments, busways, buffer_m = 25, overlap = 0.60) {
	segment_index <- segments |>
		dplyr::filter(!is.na(next_lon), !is.na(next_lat)) |>
		dplyr::distinct(stop_id, next_stop_id, stop_lon, stop_lat, next_lon, next_lat) |>
		dplyr::mutate(
			segment_key = dplyr::row_number(),
			geometry = purrr::pmap(
				list(stop_lon, stop_lat, next_lon, next_lat),
				\(x1, y1, x2, y2) {
					sf::st_linestring(
						matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE)
					)
				}
			) |>
				sf::st_sfc(crs = 4326)
		) |>
		sf::st_as_sf() |>
		sf::st_transform(31983)

	conn <- duckspatial::ddbs_create_conn()
	on.exit(duckspatial::ddbs_stop_conn(conn))
	duckspatial::ddbs_register_table(conn, segment_index, "segments", overwrite = TRUE)
	duckspatial::ddbs_register_table(conn, busways, "busways", overwrite = TRUE)
	DBI::dbExecute(
		conn,
		sprintf(
			paste(
				"CREATE TEMP TABLE busway_buffers AS",
				"SELECT segregation, ST_Union_Agg(ST_Buffer(geometry, %f)) AS geometry",
				"FROM busways GROUP BY segregation"
			),
			buffer_m
		)
	)
	matches <- DBI::dbGetQuery(
		conn,
		sprintf(
			paste(
				"WITH candidates AS (SELECT s.segment_key, b.segregation,",
				"ST_Length(ST_Intersection(s.geometry, b.geometry)) /",
				"NULLIF(ST_Length(s.geometry), 0) AS overlap_share",
				"FROM segments s JOIN busway_buffers b ON ST_Intersects(s.geometry, b.geometry)),",
				"ranked AS (SELECT *, ROW_NUMBER() OVER (PARTITION BY segment_key ORDER BY",
				"CASE segregation WHEN 'fully_segregated' THEN 1",
				"WHEN 'segregated_lane' THEN 2 ELSE 3 END, overlap_share DESC) AS rank",
				"FROM candidates WHERE overlap_share >= %f)",
				"SELECT segment_key, segregation FROM ranked WHERE rank = 1"
			),
			overlap
		)
	)

	segment_index |>
		sf::st_drop_geometry() |>
		dplyr::select(stop_id, next_stop_id, segment_key) |>
		dplyr::left_join(matches, by = "segment_key") |>
		dplyr::mutate(segregation = dplyr::coalesce(segregation, "mixed_traffic")) |>
		dplyr::select(-segment_key)
}


bus_segment_speed_lookup <- function(
	stop_times,
	stops,
	surface,
	busways,
	prior_segments = 200,
	reference_years = 3L
) {
	global_speed <- stats::median(surface$speed_kmh)
	speed_limits <- stats::quantile(surface$speed_kmh, c(0.05, 0.95))
	lookup <- surface |>
		dplyr::transmute(
			h3,
			segregation,
			model_speed_kmh = pmin(pmax(speed_kmh, speed_limits[[1]]), speed_limits[[2]]),
			class_speed_kmh
		)

	segments <- stop_times |>
		dplyr::left_join(
			stops |> dplyr::select(stop_id, stop_lon, stop_lat),
			by = "stop_id"
		) |>
		dplyr::arrange(trip_id, stop_sequence) |>
		dplyr::mutate(
			next_stop_id = dplyr::lead(stop_id),
			next_lon = dplyr::lead(stop_lon),
			next_lat = dplyr::lead(stop_lat),
			midpoint_lon = (stop_lon + next_lon) / 2,
			midpoint_lat = (stop_lat + next_lat) / 2,
			h3 = bus_points_to_h3(midpoint_lon, midpoint_lat),
			.by = trip_id
		)
	classes <- classify_busway_segments(segments, busways)
	segments |>
		dplyr::left_join(classes, by = c("stop_id", "next_stop_id")) |>
		dplyr::mutate(segregation = dplyr::coalesce(segregation, "mixed_traffic")) |>
		dplyr::left_join(lookup, by = c("h3", "segregation")) |>
		dplyr::mutate(
			class_speed_kmh = dplyr::coalesce(class_speed_kmh, global_speed),
			model_speed_kmh = dplyr::coalesce(model_speed_kmh, class_speed_kmh)
		)
}


regularize_bus_stop_times <- function(gtfs, surface, busways) {
	bus_route_ids <- gtfs$routes |>
		dplyr::filter(route_type == 3L) |>
		dplyr::pull(route_id)
	bus_trip_ids <- gtfs$trips |>
		dplyr::filter(route_id %in% .env$bus_route_ids) |>
		dplyr::pull(trip_id)
	original_columns <- names(gtfs$stop_times)

	bus_stop_times <- gtfs$stop_times |>
		dplyr::filter(trip_id %in% .env$bus_trip_ids) |>
		bus_segment_speed_lookup(gtfs$stops, surface, busways) |>
		dplyr::mutate(
			original_arrival = gtfs_time_to_seconds(arrival_time),
			original_departure = gtfs_time_to_seconds(departure_time),
			dwell_seconds = pmax(original_departure - original_arrival, 0),
			segment_m = bus_haversine_m(stop_lon, stop_lat, next_lon, next_lat),
			segment_seconds = dplyr::if_else(
				is.na(next_lon),
				0,
				pmax(1, round(segment_m / model_speed_kmh * 3.6))
			),
			first_arrival = dplyr::first(original_arrival),
			elapsed_before_arrival = cumsum(dplyr::lag(
				dwell_seconds + segment_seconds,
				default = 0
			)),
			arrival_time = seconds_to_gtfs_time(first_arrival + elapsed_before_arrival),
			departure_time = seconds_to_gtfs_time(
				first_arrival + elapsed_before_arrival + dwell_seconds
			),
			.by = trip_id
		) |>
		dplyr::select(dplyr::all_of(original_columns))

	gtfs$stop_times <- dplyr::bind_rows(
		gtfs$stop_times |> dplyr::filter(!trip_id %in% .env$bus_trip_ids),
		bus_stop_times
	) |>
		dplyr::arrange(trip_id, stop_sequence)
	gtfs
}


bus_haversine_m <- function(lon1, lat1, lon2, lat2) {
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


# remove synthetic rail and regularize buses -------------------------------------------------

write_bus_feed <- function(
	input,
	output,
	remove_rail = TRUE,
	regularize_bus_times = FALSE,
	speed_surface = NULL,
	geosampa_busways = NULL,
	mobilidados_busways = NULL,
	service_date = NULL,
	overwrite = TRUE
) {
	stopifnot(file.exists(input))
	dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
	if (!remove_rail && !regularize_bus_times) {
		copied <- file.copy(input, output, overwrite = overwrite)
		if (!copied) {
			stop("Could not copy bus feed to ", output, ".")
		}
		return(normalizePath(output))
	}

	gtfs <- gtfstools::read_gtfs(input)
	if (remove_rail) {
		rail_route_ids <- gtfs$routes |>
			dplyr::filter(route_type %in% c(1L, 2L)) |>
			dplyr::pull(route_id)
		bus_trips <- gtfs$trips |>
			dplyr::filter(!route_id %in% .env$rail_route_ids)
		service_ids <- unique(bus_trips$service_id)
		gtfs <- subset_gtfs_relations(
			gtfs,
			trip_ids = bus_trips$trip_id,
			service_ids = service_ids
		)
	}
	if (regularize_bus_times) {
		if (is.null(speed_surface)) {
			stop("A bus speed surface is required.")
		}
		busways <- read_scenario_busways(
			geosampa_busways,
			mobilidados_busways,
			service_date
		)
		gtfs <- regularize_bus_stop_times(gtfs, speed_surface, busways)
	}
	gtfstools::write_gtfs(gtfs, output, overwrite = overwrite)
	normalizePath(output)
}


write_bus_feeds <- function(
	feed_paths,
	spec,
	speed_surface = NULL,
	geosampa_busways = NULL,
	mobilidados_busways = NULL,
	output_dir = "data/gtfs/bus",
	overwrite = TRUE
) {
	stopifnot(
		length(feed_paths) == nrow(spec),
		all(c("output_name", "remove_rail", "regularize_bus_times") %in% names(spec))
	)
	speed_surface <- if (any(spec$regularize_bus_times)) {
		validate_input_bus_speed_surface(speed_surface)
	} else {
		NULL
	}
	purrr::map2_chr(seq_along(feed_paths), feed_paths, function(index, input) {
		write_bus_feed(
			input = input,
			output = file.path(output_dir, spec$output_name[index]),
			remove_rail = spec$remove_rail[index],
			regularize_bus_times = spec$regularize_bus_times[index],
			speed_surface = speed_surface,
			geosampa_busways = geosampa_busways,
			mobilidados_busways = mobilidados_busways,
			service_date = spec$analysis_date[index],
			overwrite = overwrite
		)
	})
}

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
	## commercial speeds and HPM headways are used in every year so that changes in
	## accessibility primarily reflect the rail network available in each year.
	spec <- tibble::tribble(
		~year            , ~code_line , ~name_line  , ~extension_km , ~commercial_speed_kmh ,
		~headway_minutes ,
		2012L            ,  1L        , "AZUL"      , 20.2          ,                    35 , 125 / 60 ,
		2012L            ,  2L        , "VERDE"     , 14.7          ,                    35 , 128 / 60 ,
		2012L            ,  3L        , "VERMELHA"  , 22.0          ,                    35 , 119 / 60 ,
		2012L            ,  4L        , "AMARELA"   ,  8.9          ,                    35 , 100 / 60 ,
		2012L            ,  5L        , "LILAS"     ,  8.4          ,                    35 , 171 / 60 ,
		2012L            ,  7L        , "RUBI"      , 62.7          ,                    40 ,  6.0     ,
		2012L            ,  8L        , "DIAMANTE"  , 41.6          ,                    40 ,  6.0     ,
		2012L            ,  9L        , "ESMERALDA" , 32.6          ,                    40 ,  5.5     ,
		2012L            , 10L        , "TURQUESA"  , 38.0          ,                    40 ,  6.0     ,
		2012L            , 11L        , "CORAL"     , 50.5          ,                    40 ,  6.0     ,
		2012L            , 12L        , "SAFIRA"    , 39.0          ,                    40 ,  7.0     ,
		2015L            ,  1L        , "AZUL"      , 20.2          ,                    35 , 125 / 60 ,
		2015L            ,  2L        , "VERDE"     , 14.7          ,                    35 , 128 / 60 ,
		2015L            ,  3L        , "VERMELHA"  , 22.0          ,                    35 , 119 / 60 ,
		2015L            ,  4L        , "AMARELA"   ,  8.9          ,                    35 , 100 / 60 ,
		2015L            ,  5L        , "LILAS"     ,  9.3          ,                    35 , 171 / 60 ,
		2015L            ,  7L        , "RUBI"      , 62.7          ,                    40 ,  6.0     ,
		2015L            ,  8L        , "DIAMANTE"  , 41.6          ,                    40 ,  6.0     ,
		2015L            ,  9L        , "ESMERALDA" , 32.6          ,                    40 ,  5.5     ,
		2015L            , 10L        , "TURQUESA"  , 38.0          ,                    40 ,  6.0     ,
		2015L            , 11L        , "CORAL"     , 50.5          ,                    40 ,  6.0     ,
		2015L            , 12L        , "SAFIRA"    , 39.0          ,                    40 ,  7.0     ,
		2015L            , 15L        , "PRATA"     ,  2.1          ,                    35 ,  6       ,
		2019L            ,  1L        , "AZUL"      , 20.2          ,                    35 , 125 / 60 ,
		2019L            ,  2L        , "VERDE"     , 14.7          ,                    35 , 128 / 60 ,
		2019L            ,  3L        , "VERMELHA"  , 22.0          ,                    35 , 119 / 60 ,
		2019L            ,  4L        , "AMARELA"   , 11.3          ,                    35 , 100 / 60 ,
		2019L            ,  5L        , "LILAS"     , 19.9          ,                    35 , 171 / 60 ,
		2019L            ,  7L        , "RUBI"      , 62.7          ,                    40 ,  6.0     ,
		2019L            ,  8L        , "DIAMANTE"  , 41.6          ,                    40 ,  6.0     ,
		2019L            ,  9L        , "ESMERALDA" , 32.6          ,                    40 ,  5.5     ,
		2019L            , 10L        , "TURQUESA"  , 38.0          ,                    40 ,  6.0     ,
		2019L            , 11L        , "CORAL"     , 50.5          ,                    40 ,  6.0     ,
		2019L            , 12L        , "SAFIRA"    , 39.0          ,                    40 ,  7.0     ,
		2019L            , 13L        , "JADE"      , 12.2          ,                    40 , 20.0     ,
		2019L            , 15L        , "PRATA"     , 12.9          ,                    35 , 180 / 60 ,
		2025L            ,  1L        , "AZUL"      , 20.2          ,                    35 , 125 / 60 ,
		2025L            ,  2L        , "VERDE"     , 14.7          ,                    35 , 128 / 60 ,
		2025L            ,  3L        , "VERMELHA"  , 22.0          ,                    35 , 119 / 60 ,
		2025L            ,  4L        , "AMARELA"   , 12.8          ,                    35 , 100 / 60 ,
		2025L            ,  5L        , "LILAS"     , 19.9          ,                    35 , 171 / 60 ,
		2025L            ,  7L        , "RUBI"      , 62.7          ,                    40 ,  6.0     ,
		2025L            ,  8L        , "DIAMANTE"  , 41.6          ,                    40 ,  6.0     ,
		2025L            ,  9L        , "ESMERALDA" , 35.1          ,                    40 ,  5.5     ,
		2025L            , 10L        , "TURQUESA"  , 38.0          ,                    40 ,  6.0     ,
		2025L            , 11L        , "CORAL"     , 50.5          ,                    40 ,  6.0     ,
		2025L            , 12L        , "SAFIRA"    , 39.0          ,                    40 ,  7.0     ,
		2025L            , 13L        , "JADE"      , 12.2          ,                    40 , 20.0     ,
		2025L            , 15L        , "PRATA"     , 14.6          ,                    35 , 180 / 60
	) |>
		mutate(
			runtime_minutes = extension_km / commercial_speed_kmh * 60,
			runtime_source = paste0(
				"Model assumption: extension / ",
				commercial_speed_kmh,
				" km/h commercial speed"
			),
			headway_source = paste0(
				"Prior study table: Metrô annual reports; CPTM professional contact; ",
				"range midpoints where applicable"
			),
			notes = "Common operating assumptions for the four-year expansion comparison"
		)
	duplicated_lines <- spec |>
		count(year, code_line) |>
		filter(n > 1L)
	if (nrow(duplicated_lines) > 0L) {
		stop("Rail service spec has duplicated year/code_line rows.")
	}

	spec <- spec |>
		left_join(
			station_lines,
			by = c("code_line", "name_line"),
			relationship = "many-to-one"
		)
	if (any(is.na(spec$name_system))) {
		invalid <- spec |>
			filter(is.na(name_system)) |>
			transmute(key = paste0(code_line, " (", name_line, ")")) |>
			pull(key)
		stop(
			"Rail service spec contains lines absent from stations_sf: ",
			paste(invalid, collapse = ", "),
			"."
		)
	}
	return(spec)
}


set_rail_stop_corrections <- function() {
	tibble::tribble(
		~year                                                   , ~action       , ~code_line  , ~stop_id  , ~other_stop_id ,
		~min_transfer_time                                      , ~station_name , ~notes      ,
		2012L                                                   , "exclude"     ,  2L         , "18989"   , NA_character_  , NA_integer_ , "PARAÍSO" ,
		"Platform stop from Line 1 embedded in the Line 2 trip" ,
		2012L                                                   , "exclude"     ,  3L         , "8210164" , NA_character_  , NA_integer_ , "TATUAPÉ" ,
		"CPTM platform stop embedded in the Line 3 trip"        ,
		2012L                                                   , "exclude"     , 11L         , "1010053" , NA_character_  , NA_integer_ , "BRÁS"    ,
		"Redundant stop shared by Lines 11 and 12"              ,
		2012L                                                   , "exclude"     , 12L         , "1010053" , NA_character_  , NA_integer_ , "BRÁS"    ,
		"Redundant stop shared by Lines 11 and 12"              ,
		2012L                                                   , "transfer"    , NA_integer_ , "18861"   , "18989"        , 180L        , "PARAÍSO" ,
		"Line 2 to Line 1 platforms"                            ,
		2012L                                                   , "transfer"    , NA_integer_ , "18944"   , "8210164"      , 180L        , "TATUAPÉ" ,
		"Line 3 to Line 11 platforms"                           ,
		2012L                                                   , "transfer"    , NA_integer_ , "18943"   , "18987"        , 240L        , "BRÁS"    ,
		"Lines 11/12 to Line 10 platforms"                      ,
		2012L                                                   , "transfer"    , NA_integer_ , "18943"   , "1010054"      , 240L        , "BRÁS"    ,
		"Lines 11/12 to Line 3 platforms"                       ,
		2012L                                                   , "transfer"    , NA_integer_ , "18987"   , "1010054"      , 240L        , "BRÁS"    ,
		"Line 10 to Line 3 platforms"
	)
}


set_rail_template_overrides <- function() {
	tibble::tribble(
		~year , ~code_line , ~template_year , ~stop_name                   ,
		2015L , 15L        , 2019L          , "Vila Prudente (monotrilho)" ,
		2015L , 15L        , 2019L          , "Oratorio"
	)
}


check_rail_service_spec <- function(spec, year) {
	required <- c(
		"year",
		"code_line",
		"name_line",
		"runtime_minutes",
		"headway_minutes",
		"runtime_source",
		"headway_source"
	)
	stopifnot(all(required %in% names(spec)))
	selected <- spec |>
		filter(year == .env$year)

	if (nrow(selected) == 0L) {
		stop("No rail service parameters supplied for ", year, ".")
	}
	invalid <- selected |>
		filter(
			is.na(runtime_minutes) |
				runtime_minutes <= 0 |
				is.na(headway_minutes) |
				headway_minutes <= 0 |
				is.na(runtime_source) |
				runtime_source == "" |
				is.na(headway_source) |
				headway_source == ""
		)
	if (nrow(invalid) > 0L) {
		invalid_lines <- paste0(
			"L",
			invalid$code_line,
			" (",
			invalid$name_line,
			")",
			collapse = ", "
		)
		stop(
			"Rail reconstruction is blocked: ",
			nrow(invalid),
			" line(s) lack positive parameters and traceable sources for ",
			year,
			": ",
			invalid_lines,
			"."
		)
	}
	selected
}


rail_route_crosswalk <- function(gtfs, code_lines) {
	crosswalk <- gtfs$routes |>
		filter(route_type %in% c(1L, 2L)) |>
		mutate(
			code_line = as.integer(stringr::str_extract(route_short_name, "[0-9]+"))
		) |>
		filter(code_line %in% .env$code_lines) |>
		select(route_id, code_line)
	missing_lines <- setdiff(code_lines, crosswalk$code_line)
	duplicated_lines <- crosswalk |>
		count(code_line) |>
		filter(n != 1L)
	if (length(missing_lines) > 0L || nrow(duplicated_lines) > 0L) {
		stop(
			"Could not establish a unique GTFS route for canonical rail line(s): ",
			paste(union(missing_lines, duplicated_lines$code_line), collapse = ", "),
			"."
		)
	}
	crosswalk
}


build_rail_template <- function(prepared_feeds, feed_spec, year, template_overrides = NULL) {
	base_rows <- feed_spec$year == year & feed_spec$remove_rail
	if (sum(base_rows) != 1L) {
		stop("Exactly one rail template feed is required for ", year, ".")
	}
	template <- gtfstools::read_gtfs(prepared_feeds[base_rows])
	overrides <- template_overrides |>
		dplyr::filter(year == .env$year)
	if (nrow(overrides) == 0L) {
		return(template)
	}

	for (code_line_value in unique(overrides$code_line)) {
		line_override <- overrides |>
			dplyr::filter(code_line == code_line_value)
		template_year <- unique(line_override$template_year)
		if (length(template_year) != 1L) {
			stop("Rail template override must name exactly one template year per line.")
		}
		source_rows <- feed_spec$year == template_year & feed_spec$remove_rail
		if (sum(source_rows) != 1L) {
			stop("Exactly one override template feed is required for ", template_year, ".")
		}
		source <- gtfstools::read_gtfs(prepared_feeds[source_rows])
		crosswalk <- rail_route_crosswalk(source, code_line_value)
		representative <- rail_representative_trips(source, crosswalk)
		if (dplyr::n_distinct(representative$direction_id) != 2L) {
			stop("Rail template override requires two directions for line ", code_line_value, ".")
		}

		trip_map <- representative |>
			dplyr::transmute(
				old_trip_id = trip_id,
				direction_id,
				trip_id = paste0("override_", year, "_L", code_line_value, "_", direction_id),
				shape_id = paste0("override_", year, "_L", code_line_value, "_", direction_id)
			)
		allowed_names <- stringr::str_to_upper(line_override$stop_name)
		stop_times <- source$stop_times |>
			dplyr::rename(old_trip_id = trip_id) |>
			dplyr::inner_join(trip_map, by = "old_trip_id") |>
			dplyr::left_join(
				source$stops |>
					dplyr::select(stop_id, stop_name, stop_lat, stop_lon),
				by = "stop_id",
				relationship = "many-to-one"
			) |>
			dplyr::filter(stringr::str_to_upper(stop_name) %in% allowed_names) |>
			dplyr::arrange(direction_id, stop_sequence) |>
			dplyr::mutate(stop_sequence = dplyr::row_number(), .by = direction_id)

		coverage <- stop_times |>
			dplyr::summarise(
				stops = dplyr::n_distinct(stop_id),
				.by = direction_id
			)
		if (nrow(coverage) != 2L || any(coverage$stops != length(allowed_names))) {
			stop("Rail template override did not retain every allowed stop in both directions.")
		}

		shapes <- stop_times |>
			dplyr::transmute(
				shape_id,
				shape_pt_lat = stop_lat,
				shape_pt_lon = stop_lon,
				shape_pt_sequence = stop_sequence
			)
		trips <- source$trips |>
			dplyr::rename(old_trip_id = trip_id) |>
			dplyr::select(-dplyr::any_of(c("direction_id", "shape_id"))) |>
			dplyr::inner_join(trip_map, by = "old_trip_id") |>
			dplyr::select(-old_trip_id)
		stop_times <- stop_times |>
			dplyr::select(
				-dplyr::any_of(c("old_trip_id", "direction_id", "shape_id")),
				-stop_name,
				-stop_lat,
				-stop_lon
			)
		route_ids <- unique(crosswalk$route_id)
		stops <- source$stops |>
			dplyr::filter(stop_id %in% stop_times$stop_id)

		template$routes <- dplyr::bind_rows(
			template$routes,
			source$routes |> dplyr::filter(route_id %in% route_ids)
		) |>
			dplyr::distinct(route_id, .keep_all = TRUE)
		template$trips <- dplyr::bind_rows(template$trips, trips) |>
			dplyr::distinct(trip_id, .keep_all = TRUE)
		template$stop_times <- dplyr::bind_rows(template$stop_times, stop_times)
		template$stops <- dplyr::bind_rows(template$stops, stops) |>
			dplyr::distinct(stop_id, .keep_all = TRUE)
		template$shapes <- dplyr::bind_rows(template$shapes, shapes)
	}

	gtfstools::as_dt_gtfs(template)
}


rail_representative_trips <- function(gtfs, crosswalk) {
	gtfs$trips |>
		inner_join(crosswalk, by = "route_id") |>
		left_join(
			gtfs$stop_times |>
				count(trip_id, name = "n_stops"),
			by = "trip_id"
		) |>
		group_by(route_id, direction_id) |>
		slice_max(n_stops, n = 1L, with_ties = FALSE) |>
		ungroup()
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
		select(code_line, route_id, direction_id, old_trip_id = trip_id, shape_id) |>
		left_join(spec, by = "code_line", relationship = "many-to-one") |>
		mutate(
			service_id = paste0("rail_weekday_", year),
			trip_id = paste0("rail_", year, "_L", code_line, "_", direction_id)
		)

	corrections <- if (is.null(stop_corrections)) {
		tibble::tibble(
			year = integer(),
			action = character(),
			code_line = integer(),
			stop_id = character(),
			other_stop_id = character(),
			min_transfer_time = integer()
		)
	} else {
		stop_corrections |>
			filter(year == .env$year)
	}
	exclusions <- corrections |>
		filter(action == "exclude") |>
		select(code_line, excluded_stop_id = stop_id)

	stop_times <- representative |>
		select(
			code_line,
			route_id,
			direction_id,
			old_trip_id,
			trip_id,
			runtime_minutes
		) |>
		left_join(
			template_gtfs$stop_times,
			by = c("old_trip_id" = "trip_id")
		) |>
		left_join(
			template_gtfs$stops |>
				select(stop_id, stop_name, stop_lat, stop_lon),
			by = "stop_id"
		) |>
		left_join(
			exclusions |>
				mutate(exclude = TRUE),
			by = c("code_line", "stop_id" = "excluded_stop_id")
		) |>
		filter(is.na(exclude)) |>
		arrange(trip_id, stop_sequence) |>
		group_by(trip_id) |>
		mutate(
			# Some 2012 trips contain consecutive platform IDs for the same physical
			# interchange (Paraíso, Tatuapé and Brás). Keep both IDs for transfer
			# connectivity, but do not treat movement between platforms as train run
			# time when allocating the line's end-to-end runtime.
			same_station = row_number() > 1L &
				stringr::str_to_upper(stop_name) == lag(stringr::str_to_upper(stop_name)),
			segment_distance_m = if_else(
				row_number() == 1L | same_station,
				0,
				haversine_distance_m(
					lag(stop_lon),
					lag(stop_lat),
					stop_lon,
					stop_lat
				)
			),
			distance_share = cumsum(segment_distance_m) / sum(segment_distance_m),
			elapsed_seconds = runtime_minutes * 60 * distance_share,
			arrival_time = seconds_to_gtfs_time(elapsed_seconds),
			departure_time = arrival_time
		) |>
		ungroup() |>
		select(
			trip_id,
			arrival_time,
			departure_time,
			stop_id,
			stop_sequence,
			any_of(c("stop_headsign", "pickup_type", "drop_off_type"))
		)

	trips <- representative |>
		transmute(
			route_id = paste0("rail_L", code_line),
			service_id,
			trip_id,
			trip_headsign = NA_character_,
			direction_id,
			shape_id
		)
	frequencies <- representative |>
		transmute(
			trip_id,
			start_time = frequency_start,
			end_time = frequency_end,
			headway_secs = as.integer(round(headway_minutes * 60)),
			exact_times = 0L
		)

	service_date <- as.Date(service_date)
	weekday <- c(
		"sunday",
		"monday",
		"tuesday",
		"wednesday",
		"thursday",
		"friday",
		"saturday"
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
		~agency_id , ~agency_name         , ~agency_url                   , ~agency_timezone    , ~agency_lang ,
		"METRO"    , "Metrô de São Paulo" , "https://www.metro.sp.gov.br" , "America/Sao_Paulo" , "pt-BR"      ,
		"CPTM"     , "CPTM"               , "https://www.cptm.sp.gov.br"  , "America/Sao_Paulo" , "pt-BR"
	) |>
		filter(
			agency_id %in%
				if_else(
					spec$name_system == "Metro",
					"METRO",
					"CPTM"
				)
		)
	transfers <- corrections |>
		filter(action == "transfer") |>
		transmute(
			from_stop_id = stop_id,
			to_stop_id = other_stop_id,
			transfer_type = 2L,
			min_transfer_time
		) |>
		bind_rows(
			corrections |>
				filter(action == "transfer") |>
				transmute(
					from_stop_id = other_stop_id,
					to_stop_id = stop_id,
					transfer_type = 2L,
					min_transfer_time
				)
		) |>
		distinct()
	invalid_transfer_stops <- setdiff(
		union(transfers$from_stop_id, transfers$to_stop_id),
		stop_ids
	)
	if (length(invalid_transfer_stops) > 0L) {
		stop(
			"Rail transfer corrections reference stops absent from reconstructed trips: ",
			paste(invalid_transfer_stops, collapse = ", "),
			"."
		)
	}

	gtfs <- list(
		agency = agencies,
		stops = template_gtfs$stops |>
			filter(stop_id %in% .env$stop_ids),
		routes = spec |>
			transmute(
				route_id = paste0("rail_L", code_line),
				agency_id = if_else(name_system == "Metro", "METRO", "CPTM"),
				route_short_name = as.character(label_line),
				route_long_name = name_line,
				route_type = if_else(name_system == "Metro", 1L, 2L)
			),
		trips = trips,
		stop_times = stop_times,
		calendar = calendar,
		frequencies = frequencies,
		shapes = template_gtfs$shapes |>
			filter(shape_id %in% .env$shape_ids)
	)
	if (nrow(transfers) > 0L) {
		gtfs$transfers <- transfers
	}
	gtfstools::as_dt_gtfs(gtfs)
}


write_rail_analysis_feed <- function(
	template,
	spec,
	stop_corrections,
	output,
	year,
	service_date,
	frequency_start = "05:30:00",
	frequency_end = "08:30:00",
	overwrite = TRUE
) {
	template_gtfs <- if (is.character(template)) {
		gtfstools::read_gtfs(template)
	} else {
		template
	}
	gtfs <- build_rail_analysis_gtfs(
		template_gtfs = template_gtfs,
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
	template_overrides = NULL,
	output_dir = "data/gtfs/rail"
) {
	year <- unique(routing_spec$year)
	datetime <- unique(routing_spec$datetime)
	if (length(year) != 1L || length(datetime) != 1L) {
		stop("Each routing branch must contain exactly one year and datetime.")
	}

	template <- build_rail_template(
		prepared_feeds = prepared_feeds,
		feed_spec = feed_spec,
		year = year,
		template_overrides = template_overrides
	)

	write_rail_analysis_feed(
		template = template,
		spec = service_spec,
		stop_corrections = stop_corrections,
		output = file.path(output_dir, paste0("gtfs_rail_", year, ".zip")),
		year = year,
		service_date = as.Date(datetime)
	)
}

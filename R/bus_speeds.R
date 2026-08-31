# Reference bus-speed model ------------------------------------------------------------------

set_bus_speed_spec <- function() {
	list(
		reference_years = 2015:2017,
		reference_feed_id = "gtfs_sao_paulo_sptrans_20150113",
		reference_date = as.Date("2015-01-13"),
		window_start = 6L * 3600L,
		window_end = 7L * 3600L,
		h3_resolution = 8L,
		minimum_segment_m = 75,
		maximum_segment_m = 5000,
		minimum_speed_kmh = 2,
		maximum_speed_kmh = 50,
		projected_crs = 31983L,
		buffer_m = 25,
		minimum_overlap = 0.60,
		shrinkage_observations = 50,
		reference_year_count = 3L,
		test_buffers_m = c(15, 25, 40)
	)
}


extract_gtfs_filename_date <- function(path) {
	matched <- stringr::str_match(
		tools::file_path_sans_ext(basename(path)),
		"(20\\d{2})[-_.]?([01]\\d)[-_.]?([0-3]\\d)"
	)
	if (is.na(matched[1, 1])) {
		return(as.Date(NA))
	}
	as.Date(paste(matched[1, 2:4], collapse = "-"))
}


nearest_tuesday <- function(date) {
	date + ((2L - as.POSIXlt(date)$wday + 3L) %% 7L - 3L)
}


build_reference_feed_inventory <- function(paths, spec) {
	stopifnot(length(paths) > 0L, all(file.exists(paths)))
	dates <- as.Date(
		vapply(paths, extract_gtfs_filename_date, as.Date(NA)),
		origin = "1970-01-01"
	)
	inventory <- tibble::tibble(
		feed_id = tools::file_path_sans_ext(basename(paths)),
		feed_path = paths,
		reference_date = dates,
		year = as.integer(format(reference_date, "%Y")),
		audit_date = nearest_tuesday(reference_date)
	) |>
		dplyr::arrange(reference_date, feed_id)

	stopifnot(
		!anyNA(inventory$reference_date),
		!anyDuplicated(inventory$feed_id),
		all(inventory$year %in% spec$reference_years),
		setequal(unique(inventory$year), spec$reference_years),
		spec$reference_feed_id %in% inventory$feed_id,
		inventory$reference_date[inventory$feed_id == spec$reference_feed_id] == spec$reference_date,
		spec$window_start < spec$window_end,
		spec$minimum_segment_m < spec$maximum_segment_m,
		spec$minimum_speed_kmh < spec$maximum_speed_kmh,
		length(spec$reference_years) == spec$reference_year_count
	)
	inventory
}


split_reference_feed_inventory <- function(inventory) {
	split(inventory, inventory$feed_id)
}


has_duplicate_keys <- function(data, keys) {
	any(duplicated(dplyr::select(data, dplyr::all_of(keys))))
}


read_reference_gtfs <- function(path) {
	files <- paste0(
		c(
			"agency",
			"stops",
			"routes",
			"trips",
			"stop_times",
			"calendar",
			"calendar_dates",
			"frequencies"
		),
		".txt"
	)
	root_files <- utils::unzip(path, list = TRUE)$Name
	root_files <- root_files[!stringr::str_detect(root_files, "/") & root_files %in% files]
	gtfs <- gtfstools::read_gtfs(path, files = root_files)
	required <- c("stops", "routes", "trips", "stop_times", "calendar", "frequencies")
	stopifnot(all(required %in% names(gtfs)))
	gtfs
}


count_frequency_departures <- function(frequencies, lower, upper) {
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


bus_segment_bearing <- function(lon1, lat1, lon2, lat2) {
	lon1 <- lon1 * pi / 180
	lat1 <- lat1 * pi / 180
	lon2 <- lon2 * pi / 180
	lat2 <- lat2 * pi / 180
	bearing <- atan2(
		sin(lon2 - lon1) * cos(lat2),
		cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon2 - lon1)
	) *
		180 /
		pi
	(bearing + 360) %% 360
}


bus_bearing_group <- function(bearing) {
	labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
	labels[(floor((bearing + 22.5) %% 360 / 45) %% 8) + 1L]
}


extract_hpm_bus_segments <- function(feed, spec) {
	stopifnot(nrow(feed) == 1L)
	message("Extracting ", feed$feed_id)
	gtfs <- read_reference_gtfs(feed$feed_path)
	service_ids <- gtfs_active_service_ids(gtfs, feed$audit_date)
	bus_route_ids <- gtfs$routes |>
		dplyr::filter(route_type == 3L) |>
		dplyr::pull(route_id)
	active_trips <- gtfs$trips |>
		dplyr::filter(service_id %in% service_ids, route_id %in% bus_route_ids)
	departures <- count_frequency_departures(
		gtfs$frequencies,
		spec$window_start,
		spec$window_end
	) |>
		dplyr::inner_join(
			active_trips |>
				dplyr::select(trip_id, route_id, direction_id),
			by = "trip_id",
			relationship = "many-to-one"
		)

	stops <- gtfs$stops |>
		dplyr::select(stop_id, stop_lon, stop_lat)
	stopifnot(!anyDuplicated(stops$stop_id))
	segments <- gtfs$stop_times |>
		dplyr::inner_join(departures, by = "trip_id", relationship = "many-to-one") |>
		dplyr::left_join(stops, by = "stop_id", relationship = "many-to-one") |>
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
			segment_m = bus_haversine_m(stop_lon, stop_lat, next_lon, next_lat),
			speed_kmh = segment_m / segment_seconds * 3.6,
			bearing = bus_segment_bearing(stop_lon, stop_lat, next_lon, next_lat),
			direction = bus_bearing_group(bearing),
			midpoint_lon = (stop_lon + next_lon) / 2,
			midpoint_lat = (stop_lat + next_lat) / 2
		)

	valid <- segments |>
		dplyr::filter(
			segment_seconds > 0,
			dplyr::between(segment_m, spec$minimum_segment_m, spec$maximum_segment_m),
			dplyr::between(speed_kmh, spec$minimum_speed_kmh, spec$maximum_speed_kmh),
			is.finite(midpoint_lon),
			is.finite(midpoint_lat)
		) |>
		dplyr::mutate(
			h3 = bus_points_to_h3(midpoint_lon, midpoint_lat, spec$h3_resolution),
			feed_id = feed$feed_id,
			year = feed$year,
			audit_date = feed$audit_date
		) |>
		dplyr::select(
			feed_id,
			year,
			audit_date,
			h3,
			direction,
			route_id,
			trip_id,
			departures,
			stop_id,
			next_stop_id,
			segment_m,
			segment_seconds,
			speed_kmh
		)

	diagnostics <- tibble::tibble(
		feed_id = feed$feed_id,
		year = feed$year,
		audit_date = feed$audit_date,
		active_bus_routes = dplyr::n_distinct(departures$route_id),
		active_bus_patterns = dplyr::n_distinct(departures$trip_id),
		candidate_segments = nrow(segments),
		valid_segments = nrow(valid),
		valid_share = if (nrow(segments) == 0L) NA_real_ else nrow(valid) / nrow(segments)
	)
	stopifnot(nrow(valid) > 0L)
	list(segments = valid, diagnostics = diagnostics)
}


combine_hpm_segments <- function(results) {
	dplyr::bind_rows(purrr::map(results, "segments"))
}


combine_feed_diagnostics <- function(results) {
	dplyr::bind_rows(purrr::map(results, "diagnostics")) |>
		dplyr::arrange(year, audit_date, feed_id)
}


summarise_bus_speed_surface <- function(segments) {
	snapshot <- segments |>
		dplyr::summarise(
			speed_kmh = stats::median(speed_kmh),
			routes = dplyr::n_distinct(route_id),
			segments = dplyr::n(),
			departures = sum(departures),
			.by = c(feed_id, year, h3)
		)
	annual <- snapshot |>
		dplyr::summarise(
			speed_kmh = stats::median(speed_kmh),
			snapshots = dplyr::n(),
			routes = stats::median(routes),
			segments = sum(segments),
			departures = sum(departures),
			.by = c(year, h3)
		)
	annual |>
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
			.by = h3
		)
}


read_reference_busways <- function(geosampa_path, mobilidados_path, date, projected_crs) {
	geosampa <- sf::st_read(geosampa_path, quiet = TRUE) |>
		dplyr::filter(
			!is.na(dt_implantacao_faixa),
			dt_implantacao_faixa <= date,
			nm_tipo_faixa_corredor != "TEMPORARIA"
		) |>
		dplyr::transmute(
			source = "GeoSampa",
			corridor = nm_denominacao_faixa_corredor,
			segment = nm_logradouro_faixa_corredor,
			segregation = dplyr::if_else(
				nm_tipo_faixa_corredor == "EXCLUSIVA",
				"segregated_lane",
				"priority_lane"
			),
			opening_date = dt_implantacao_faixa
		) |>
		sf::st_transform(projected_crs)

	layer <- paste0("/vsizip/", normalizePath(mobilidados_path), "/BRT_Corredores.shp")
	mobilidados <- sf::st_read(layer, quiet = TRUE) |>
		dplyr::filter(
			Cidade_n == "São Paulo",
			`Situação` == "Operacional",
			is.finite(Ano),
			Ano >= 1900,
			Ano <= as.integer(format(date, "%Y"))
		) |>
		dplyr::transmute(
			source = "MobiliDADOS",
			corridor = Corredor,
			segment = Segmento,
			segregation = dplyr::case_when(
				Segregacao %in% c("Física", "Exclusiva") ~ "fully_segregated",
				Segregacao == "Visual" ~ "priority_lane",
				TRUE ~ NA_character_
			),
			opening_date = as.Date(paste0(as.integer(Ano), "-12-31"))
		) |>
		dplyr::filter(!is.na(segregation)) |>
		sf::st_transform(projected_crs)

	for (object in c("geosampa", "mobilidados")) {
		result <- get(object)
		names(result)[names(result) == attr(result, "sf_column")] <- "geometry"
		sf::st_geometry(result) <- "geometry"
		assign(object, result)
	}
	dplyr::bind_rows(geosampa, mobilidados)
}


build_reference_segment_geometry <- function(segments, reference_feed, projected_crs) {
	segment_index <- segments |>
		dplyr::filter(feed_id == reference_feed$feed_id) |>
		dplyr::distinct(stop_id, next_stop_id) |>
		dplyr::mutate(segment_id = dplyr::row_number())
	stopifnot(
		nrow(segment_index) > 0L,
		!has_duplicate_keys(segment_index, c("stop_id", "next_stop_id"))
	)

	stops <- gtfstools::read_gtfs(reference_feed$feed_path, files = "stops.txt")$stops |>
		dplyr::select(stop_id, stop_name, stop_lon, stop_lat)
	stopifnot(!anyDuplicated(stops$stop_id))
	segment_index |>
		dplyr::left_join(
			stops |>
				dplyr::rename(
					from_name = stop_name,
					from_lon = stop_lon,
					from_lat = stop_lat
				),
			by = "stop_id",
			relationship = "many-to-one"
		) |>
		dplyr::left_join(
			stops |>
				dplyr::rename(to_name = stop_name, to_lon = stop_lon, to_lat = stop_lat),
			by = c("next_stop_id" = "stop_id"),
			relationship = "many-to-one"
		) |>
		dplyr::filter(
			is.finite(from_lon),
			is.finite(from_lat),
			is.finite(to_lon),
			is.finite(to_lat)
		) |>
		dplyr::mutate(
			geometry = purrr::pmap(
				list(from_lon, from_lat, to_lon, to_lat),
				\(x1, y1, x2, y2) {
					sf::st_linestring(
						matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE)
					)
				}
			) |>
				sf::st_sfc(crs = 4326)
		) |>
		sf::st_as_sf() |>
		sf::st_transform(projected_crs)
}


classify_reference_segments <- function(segments_sf, busways, buffer_m, minimum_overlap) {
	conn <- duckspatial::ddbs_create_conn()
	on.exit(duckspatial::ddbs_stop_conn(conn))
	duckspatial::ddbs_register_table(conn, segments_sf, "segments", overwrite = TRUE)
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
				"WITH candidates AS (",
				"SELECT s.segment_id, b.segregation,",
				"ST_Length(ST_Intersection(s.geometry, b.geometry)) /",
				"NULLIF(ST_Length(s.geometry), 0) AS overlap_share",
				"FROM segments s JOIN busway_buffers b ON ST_Intersects(s.geometry, b.geometry)",
				"), ranked AS (",
				"SELECT *, ROW_NUMBER() OVER (PARTITION BY segment_id ORDER BY",
				"overlap_share DESC, CASE segregation",
				"WHEN 'fully_segregated' THEN 1 WHEN 'segregated_lane' THEN 2 ELSE 3 END) AS rank",
				"FROM candidates WHERE overlap_share >= %f)",
				"SELECT segment_id, segregation, overlap_share FROM ranked WHERE rank = 1"
			),
			minimum_overlap
		)
	) |>
		tibble::as_tibble()

	result <- segments_sf |>
		sf::st_drop_geometry() |>
		dplyr::select(segment_id, stop_id, next_stop_id) |>
		dplyr::left_join(matches, by = "segment_id", relationship = "one-to-one") |>
		dplyr::mutate(segregation = dplyr::coalesce(segregation, "mixed_traffic"))
	stopifnot(
		!has_duplicate_keys(result, c("stop_id", "next_stop_id")),
		all(
			result$segregation %in%
				c(
					"mixed_traffic",
					"priority_lane",
					"segregated_lane",
					"fully_segregated"
				)
		)
	)
	result
}


estimate_bus_speed_surface <- function(segments, matches, spec) {
	classified <- segments |>
		dplyr::left_join(
			matches |>
				dplyr::select(stop_id, next_stop_id, segregation),
			by = c("stop_id", "next_stop_id"),
			relationship = "many-to-one"
		) |>
		dplyr::mutate(segregation = dplyr::coalesce(segregation, "mixed_traffic"))

	conditional_snapshot <- classified |>
		dplyr::summarise(
			speed_kmh = stats::median(speed_kmh),
			observations = dplyr::n(),
			routes = dplyr::n_distinct(route_id),
			.by = c(feed_id, year, h3, segregation)
		)
	conditional_year <- conditional_snapshot |>
		dplyr::summarise(
			speed_kmh = stats::median(speed_kmh),
			observations = sum(observations),
			snapshots = dplyr::n(),
			.by = c(year, h3, segregation)
		)
	conditional_global <- conditional_year |>
		dplyr::summarise(
			class_speed_kmh = stats::median(speed_kmh),
			h3_years = dplyr::n(),
			.by = segregation
		)

	conditional_year |>
		dplyr::summarise(
			raw_speed_kmh = stats::median(speed_kmh),
			observations = sum(observations),
			years = dplyr::n_distinct(year),
			.by = c(h3, segregation)
		) |>
		dplyr::left_join(conditional_global, by = "segregation", relationship = "many-to-one") |>
		dplyr::mutate(
			local_weight = observations /
				(observations + spec$shrinkage_observations) *
				pmin(years / spec$reference_year_count, 1),
			speed_kmh = local_weight * raw_speed_kmh + (1 - local_weight) * class_speed_kmh
		) |>
		dplyr::arrange(h3, segregation)
}


validate_bus_speed_surface <- function(surface, inventory, diagnostics, spec) {
	required <- c(
		"h3",
		"segregation",
		"speed_kmh",
		"class_speed_kmh",
		"observations",
		"years"
	)
	stopifnot(
		all(required %in% names(surface)),
		nrow(surface) > 0L,
		!has_duplicate_keys(surface, c("h3", "segregation")),
		all(!is.na(surface$h3)),
		all(is.finite(surface$speed_kmh)),
		all(surface$speed_kmh > 0),
		all(surface$observations > 0),
		all(surface$years >= 1L & surface$years <= spec$reference_year_count),
		setequal(unique(inventory$year), spec$reference_years),
		setequal(diagnostics$feed_id, inventory$feed_id),
		all(diagnostics$valid_segments > 0L)
	)
	tibble::tibble(
		cells = nrow(surface),
		classes = dplyr::n_distinct(surface$segregation),
		median_speed_kmh = stats::median(surface$speed_kmh),
		minimum_speed_kmh = min(surface$speed_kmh),
		maximum_speed_kmh = max(surface$speed_kmh),
		complete_year_cells = sum(surface$years == spec$reference_year_count)
	)
}


measure_busway_buffers <- function(segments_sf, busways, distances_m, minimum_overlap) {
	purrr::map_dfr(distances_m, function(distance_m) {
		conn <- duckspatial::ddbs_create_conn()
		on.exit(duckspatial::ddbs_stop_conn(conn))
		duckspatial::ddbs_register_table(conn, segments_sf, "segments", overwrite = TRUE)
		duckspatial::ddbs_register_table(conn, busways, "busways", overwrite = TRUE)
		DBI::dbExecute(
			conn,
			sprintf(
				paste(
					"CREATE TEMP TABLE busway_buffers AS",
					"SELECT ST_Union_Agg(ST_Buffer(geometry, %f)) AS geometry FROM busways"
				),
				distance_m
			)
		)
		overlap <- DBI::dbGetQuery(
			conn,
			paste(
				"SELECT s.segment_id,",
				"MAX(ST_Length(ST_Intersection(s.geometry, b.geometry)) /",
				"NULLIF(ST_Length(s.geometry), 0)) AS overlap_share",
				"FROM segments s JOIN busway_buffers b ON ST_Intersects(s.geometry, b.geometry)",
				"GROUP BY s.segment_id"
			)
		)
		tibble::tibble(
			buffer_m = distance_m,
			segments = nrow(segments_sf),
			any_overlap = nrow(overlap),
			matched = sum(overlap$overlap_share >= minimum_overlap),
			matched_80pct = sum(overlap$overlap_share >= 0.80),
			median_overlap = stats::median(overlap$overlap_share)
		)
	})
}


bus_speed_h3_as_sf <- function(data) {
	sf::st_sf(
		data,
		geometry = data$h3 |>
			h3o::h3_from_strings() |>
			sf::st_as_sfc(),
		crs = 4326
	)
}


plot_bus_speed_diagnostics <- function(segments, surface, busways, spec, output_dir) {
	dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
	summary_sf <- summarise_bus_speed_surface(segments) |>
		bus_speed_h3_as_sf()
	conditional_sf <- surface |>
		dplyr::filter(years >= 2L) |>
		bus_speed_h3_as_sf()
	theme <- ggplot2::theme_void(base_size = 11) +
		ggplot2::theme(
			plot.title = ggplot2::element_text(face = "bold"),
			plot.subtitle = ggplot2::element_text(color = "grey35"),
			legend.position = "right"
		)

	plots <- list(
		conditional_speed = ggplot2::ggplot(conditional_sf) +
			ggplot2::geom_sf(ggplot2::aes(fill = speed_kmh), color = NA) +
			ggplot2::facet_wrap(ggplot2::vars(segregation)) +
			ggplot2::scale_fill_viridis_c(option = "magma", direction = -1) +
			ggplot2::labs(
				title = "Conditional scheduled bus speeds",
				subtitle = sprintf(
					"H3-%d and infrastructure class; balanced %d-%d medians",
					spec$h3_resolution,
					min(spec$reference_years),
					max(spec$reference_years)
				),
				fill = "km/h"
			) +
			theme,
		coverage = ggplot2::ggplot(summary_sf) +
			ggplot2::geom_sf(ggplot2::aes(fill = log10(segments)), color = NA) +
			ggplot2::scale_fill_viridis_c(option = "viridis") +
			ggplot2::labs(
				title = "Observational coverage of the speed surface",
				subtitle = sprintf(
					"Valid route segments across all %d-%d snapshots",
					min(spec$reference_years),
					max(spec$reference_years)
				),
				fill = "log10\nsegments"
			) +
			theme,
		stability = ggplot2::ggplot(summary_sf) +
			ggplot2::geom_sf(ggplot2::aes(fill = speed_iqr), color = NA) +
			ggplot2::scale_fill_viridis_c(option = "plasma") +
			ggplot2::labs(
				title = "Stability of scheduled bus speeds across years",
				subtitle = sprintf(
					"IQR of the %s H3-%d medians",
					paste(spec$reference_years, collapse = ", "),
					spec$h3_resolution
				),
				fill = "IQR\n(km/h)"
			) +
			theme,
		busways = ggplot2::ggplot(busways) +
			ggplot2::geom_sf(ggplot2::aes(color = segregation), linewidth = 0.35) +
			ggplot2::scale_color_manual(
				values = c(
					fully_segregated = "#7A0177",
					segregated_lane = "#D7301F",
					priority_lane = "#2C7FB8"
				)
			) +
			ggplot2::labs(
				title = "Bus priority infrastructure active by 2015",
				subtitle = "GeoSampa, supplemented with operational MobiliDADOS BRT segments",
				color = NULL
			) +
			theme
	)
	paths <- file.path(output_dir, paste0(names(plots), ".png"))
	purrr::walk2(
		plots,
		paths,
		\(plot, path) {
			ggplot2::ggsave(
				path,
				plot,
				width = 9,
				height = 8,
				dpi = 250,
				bg = "white"
			)
		}
	)
	paths
}

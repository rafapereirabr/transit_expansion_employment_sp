# build_r5r_network --------------------------------------------------------------------------

# dir = "data/r5"
build_r5r_network <- function(dir, ram = 48, cpu = 12, overwrite = TRUE) {
	options(
		java.parameters = c(
			paste0("-Xmx", ram, "G"),
			paste0("-XX:ActiveProcessorCount=", cpu)
		)
	)

	java_installed <- rJavaEnv::java_check_version_cmd(quiet = T)
	if (!is.character(java_installed)) {
		rJavaEnv::java_quick_install()
	}

	network <- r5r::build_network(dir, verbose = TRUE, overwrite = overwrite)

	return(file.path(dir, "network.dat"))
}


# o-d table ----------------------------------------------------------------------------------

# tar_load(cadunico_fam)
# origins <- cadunico_fam
# tar_load(stations_sf)
# destinations <- stations_sf

set_od_table_stations <- function(origins, destinations, origin_filter = NULL) {
	## origins: cadunico families @ h3 centroid
	origins <- arrow::open_dataset(origins)

	if (!is.null(origin_filter)) {
		allowed_hex <- sf::st_drop_geometry(origin_filter) |>
			pull(h3_address)
		origins <- filter(origins, h3_09 %in% allowed_hex)
	}

	origins <- origins |>
		rename(id = h3_09) |>
		h3_from_sf(col_name = "id") |>
		mutate(origin = TRUE, .after = id) |>
		st_centroid()

	destinations <- sf_from_parquet(destinations)
	destinations <- destinations |>
		transmute(id = as.character(code_station), origin = FALSE, geometry) |>
		st_transform(crs = 4326)

	od <- bind_rows(origins, destinations) |>
		st_as_sf()
	return(od)
}


set_od_table_h3 <- function(grid) {
	grid <- grid |>
		rename(id = h3_address) |>
		st_centroid()

	od <- bind_rows(
		mutate(grid, origin = TRUE),
		mutate(grid, origin = FALSE)
	)

	return(od)
}


# ttm ----------------------------------------------------------------------------------------

# library(sf)
# tar_load(r5_network)
# threads <- 10
# # mode <- "WALK"
# # tar_load(od_table_stations)
# # od_table <- od_table_stations
# mode <- "TRANSIT"
# tar_load(od_table_grid)
# od_table <- od_table_grid

# ttm_12 <- calc_ttm(r5_network, od_table, "SUBWAY", 2012, max_duration = 15)
# arrow::write_parquet(ttm_12, "ttm_12.parquet")
# ttm_25 <- calc_ttm(r5_network, od_table, "SUBWAY", 2025, max_duration = 15)
# arrow::write_parquet(ttm_25, "ttm_25.parquet")

calc_ttm <- function(
	r5_network,
	od_table,
	mode,
	year = NULL,
	time_window = 15L,
	max_duration = 90L,
	threads = 10
) {
	require("data.table")
	network <- r5r::build_network(dirname(r5_network), overwrite = F)

	if (is.null(year)) {
		dep_datetime <- Sys.time()
	} else {
		dep_datetime <- paste(
			paste(year, "04-09", sep = "-"),
			"07:00:00 "
		) |>
			as.POSIXct(tz = "America/Sao_Paulo")
	}

	ttm <- r5r::travel_time_matrix(
		network,
		origins = filter(od_table, origin),
		destinations = filter(od_table, !origin),
		mode = mode,
		departure_datetime = dep_datetime,
		time_window = time_window,
		max_trip_duration = max_duration,
		n_threads = threads,
		verbose = T
	)

	ttm <- ttm |>
		mutate(year = !!year, dep_datetime = dep_datetime)

	return(ttm)
}

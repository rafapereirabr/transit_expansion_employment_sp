# R5 setup -----------------------------------------------------------------------------------

set_r5r_java_options <- function(ram = 48, cpu = 12) {
	parameters <- c(
		paste0("-Xmx", ram, "G"),
		paste0("-XX:ActiveProcessorCount=", cpu)
	)

	jvm_initialized <- "rJava" %in% loadedNamespaces() &&
		isTRUE(get(".jniInitialized", envir = asNamespace("rJava")))
	if (jvm_initialized && !all(parameters %in% getOption("java.parameters"))) {
		stop(
			"The JVM is already running with different parameters. ",
			"Start a fresh R process before calling r5r."
		)
	}
	if (!jvm_initialized) {
		options(java.parameters = parameters)
	}

	invisible(parameters)
}


# build_r5r_network --------------------------------------------------------------------------

# dir = "data/r5"
check_r5_feed_paths <- function(dir, feed_paths) {
	stopifnot(
		length(dir) == 1L,
		length(feed_paths) > 0L,
		all(file.exists(feed_paths)),
		all(tolower(tools::file_ext(feed_paths)) == "zip"),
		all(normalizePath(dirname(feed_paths)) == normalizePath(dir))
	)
	invisible(normalizePath(feed_paths))
}


save_r5r_log <- function(dir, label, required = FALSE) {
	source <- file.path(dir, "r5r-log.log")
	if (!file.exists(source)) {
		message <- paste0("R5 log not found: ", source)
		if (required) stop(message) else warning(message, call. = FALSE)
		return(invisible(NA_character_))
	}
	label <- gsub("[^[:alnum:]_-]", "_", tolower(label))
	destination <- file.path(dir, paste0("r5r-log_", label, ".log"))
	copied <- file.copy(source, destination, overwrite = TRUE)
	if (!copied) {
		message <- paste0("Could not preserve R5 log at ", destination, ".")
		if (required) stop(message) else warning(message, call. = FALSE)
		return(invisible(NA_character_))
	}
	normalizePath(destination)
}


build_r5r_network <- function(
	dir,
	feed_paths,
	ram = 48,
	cpu = 12,
	overwrite = TRUE,
	keep_log = TRUE
) {
	check_r5_feed_paths(dir, feed_paths)
	set_r5r_java_options(ram = ram, cpu = cpu)

	java_installed <- rJavaEnv::java_check_version_cmd(quiet = T)
	if (!is.character(java_installed)) {
		rJavaEnv::java_quick_install()
	}

	network <- r5r::build_network(dir, verbose = TRUE, overwrite = overwrite)
	on.exit(r5r::stop_r5(network), add = TRUE)

	if (keep_log) {
		save_r5r_log(dir, "build")
	}

	return(file.path(dir, "network.dat"))
}


# o-d table ----------------------------------------------------------------------------------

set_od_station_proximity <- function(origins, destinations, origin_filter = NULL) {
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


set_od_grid_all <- function(grid) {
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

calc_ttm <- function(
	r5_network,
	od_table,
	mode,
	year = NULL,
	departure_datetime = NULL,
	time_window = 15L,
	max_duration = 90L,
	threads = 10,
	ram = 48,
	java_cpu = 12,
	keep_log = TRUE
) {
	set_r5r_java_options(ram = ram, cpu = java_cpu)
	network <- r5r::build_network(dirname(r5_network), overwrite = F)
	on.exit(r5r::stop_r5(network), add = TRUE)

	if (is.null(year) && is.null(departure_datetime)) {
		dep_datetime <- Sys.time()
	} else {
		if (is.null(departure_datetime)) {
			departure_datetime <- paste(year, "04-09 06:50:00")
		}
		dep_datetime <- as.POSIXct(
			departure_datetime,
			origin = "1970-01-01",
			tz = "America/Sao_Paulo"
		)
	}
	if (is.null(year) && !is.null(departure_datetime)) {
		year <- as.integer(format(dep_datetime, "%Y"))
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
	if (keep_log) {
		log_label <- paste(tolower(mode), basename(dirname(r5_network)), sep = "_")
		save_r5r_log(dirname(r5_network), log_label)
	}

	ttm <- ttm |>
		mutate(year = !!year, dep_datetime = dep_datetime)

	return(ttm)
}

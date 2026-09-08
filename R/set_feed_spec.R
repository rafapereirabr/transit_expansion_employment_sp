# set_feed_spec

set_feed_spec <- function(raw_feed_paths, routing_spec) {
	feed_spec <- tibble::tibble(
		input = raw_feed_paths,
		output_name = basename(input),
		year = as.integer(stringr::str_extract(basename(input), "\\d{4}"))
	) |>
		dplyr::full_join(routing_spec, by = "year", relationship = "one-to-one")

	stopifnot(
		nrow(feed_spec) == length(raw_feed_paths),
		!anyNA(feed_spec$input),
		!anyNA(feed_spec$datetime),
		!anyDuplicated(feed_spec$year),
		all(as.integer(format(feed_spec$datetime, "%Y")) == feed_spec$year)
	)

	feed_spec <- feed_spec |>
		dplyr::mutate(
			service_start = as.Date(paste0(year, "-01-01")),
			service_end = as.Date(paste0(year, "-12-31")),
			analysis_date = as.Date(datetime),
			# Change only these flags after inspecting the audit/reports.
			deduplicate_stops = FALSE,
			drop_shape_distances = TRUE,
			remove_rail = TRUE,
			regularize_bus_times = TRUE,
			include_r5 = TRUE
		)

	feed_spec
}

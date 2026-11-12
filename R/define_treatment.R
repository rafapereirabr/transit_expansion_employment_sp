# list_assumptions -------------------------------------------------------

# tar_load(time_window)

list_assumptions <- function(
	time_window,
	g_lag = 4,
	cutoff_sharp = 2000,
	spillover_sharp = 4000,
	max_dist_sharp = NULL
) {
	if (is.null(g_lag)) {
		g_lag <- 0
	}

	treat_window <- time_window[(time_window - g_lag) %in% time_window]

	treat_list <- list(
		"time_window" = time_window,
		"g_lag" = g_lag,
		"age_min" = 18,
		"age_max" = 60,
		"treat_window" = treat_window,
		"cutoff_sharp" = cutoff_sharp,
		"spillover_sharp" = spillover_sharp,
		"max_dist_sharp" = max_dist_sharp
	)
	return(treat_list)
}

# assumptions <- list_assumptions(time_window, max_dist_sharp = 15e3)

# set_opening_events -----------------------------------------------------

# tar_load(c(stations_sf))
# stations <- stations_sf
# source("R/utils.R")

set_opening_events <- function(stations, assumptions) {
	stations <- sf_from_parquet(stations, drop_geometry = TRUE)

	events <- stations |>
		filter(yr_open %in% assumptions$treat_window) |>
		select(code_line, code_station, yr_open) |>
		mutate(yr_base = yr_open - assumptions$g_lag) |>
		arrange(yr_open)

	return(events)
}

# events <- set_opening_events(stations, assumptions)

# define_risk_sets -------------------------------------------------------

# source("R/utils.R")
# source("R/dist_nearest.R")
# tar_load(c(stations_sf, person_records, treat_assumptions, opening_events))
# stations <- stations_sf
# individual_data <- person_records
# assumptions <- treat_assumptions
# events <- opening_events |>
# 	filter(yr_open == 2018)
# # filter(id_event == "L05_2017_q3")
# # filter(id_event == "L05_2018_q2")

define_risk_sets <- function(
	stations,
	events,
	individual_data,
	assumptions,
	save_dir
) {
	# filter events
	event_stations <- events$code_station
	event_year <- unique(events$yr_open)
	event_year_lag <- event_year - assumptions$g_lag
	stopifnot(length(event_year) == 1)
	## forbidden comparisons are [g - g_lag, g), hence weuse time_window instead of treat_window
	forbidden_years <- assumptions$time_window[
		between(assumptions$time_window, event_year_lag, event_year - 1)
	]
	## not-yet-treated years
	nyet_years <- setdiff(
		assumptions$treat_window,
		c(event_year, forbidden_years)
	)

	# separate stations by treatment status:
	## always treated (pre), opened bef. g (remove), first treatment (new), not yet, and future treat.
	## we'll use that to find the closest station to each home address in each situation
	stations <- sf_from_parquet(stations) |>
		select(
			code_line,
			code_station,
			name_station,
			yr_open,
			dt_trial_svc,
			geometry
		)
	stations_list <- list(
		"stations_pre" = filter(stations, yr_open < min(forbidden_years)),
		"stations_remove" = filter(stations, yr_open %in% forbidden_years),
		"stations_new" = filter(stations, code_station %in% event_stations),
		"stations_nyet" = filter(stations, yr_open %in% nyet_years),
		"stations_future" = filter(stations, code_line %in% c(6, 17))
	)

	# load and filter individual data
	individual_data <- arrow::open_dataset(individual_data)
	individual_data <- filter(individual_data, year == event_year_lag) |>
		compute()
	n0_ind <- nrow(individual_data)

	## spatial criterion: remove bad geocoding
	bad_geocoding <- individual_data |>
		filter(desvio_metros > 500) |>
		distinct(id_addr_cad) |>
		pull()

	## up-to-date criterion: cadunico registry becomes stale after two years
	stale_entry <- individual_data |> filter(years_update > 2) |> distinct(id_addr_cad) |> pull()
	n_bad_geocoding <- individual_data |> filter(id_addr_cad %in% bad_geocoding) |> nrow()
	n_stale_entry <- individual_data |>
		filter(!id_addr_cad %in% bad_geocoding & id_addr_cad %in% stale_entry) |>
		nrow()

	## eligible units (pending age criterion; will check downstream)
	individual_data <- individual_data |>
		filter_out(id_addr_cad %in% c(bad_geocoding, stale_entry)) |>
		compute()

	addresses <- individual_data |>
		distinct(id_addr_cad, lat_cad, lon_cad) |>
		collect() |>
		sf::st_as_sf(coords = c("lon_cad", "lat_cad"), crs = 4326) |>
		sf::st_transform(crs = 31983)

	# distance to stations
	## register data on duckdb
	conn <- duckspatial::ddbs_create_conn()
	duckspatial::ddbs_write_table(conn, addresses, "addresses", overwrite = TRUE)
	purrr::imap(
		stations_list,
		function(x, idx) {
			duckspatial::ddbs_write_table(conn, x, idx, overwrite = TRUE)
		}
	)

	## find nearest station (by category)
	dist <- purrr::map(
		names(stations_list),
		function(x) {
			if (nrow(stations_list[[x]]) == 0) {
				return(
					addresses |>
						sf::st_drop_geometry() |>
						dplyr::slice(0) |>
						dplyr::transmute(id_addr_cad, id_closest = NA, distance = NA_real_)
				)
			}
			dist_nearest(
				"addresses",
				x,
				id_feature = TRUE,
				id_y = "code_station",
				drop_geometry = TRUE,
				conn = conn,
				dist_type = "planar",
				max_distance = assumptions$max_dist_sharp,
				candidate_crs = 31983
			) |>
				collect()
		}
	) |>
		rlang::set_names(names(stations_list))
	duckspatial::ddbs_stop_conn(conn)
	inside_max_dist <- bind_rows(dist) |> pull(id_addr_cad)
	n_outside_max_dist <- individual_data |>
		filter(!id_addr_cad %in% inside_max_dist) |>
		nrow()

	# transitive treatment status
	## if a unit is "close enough" to more than one station, we solve matches using this rule:
	## inside treatment buffer (2km) -> that category is the dominant one
	## (NOT IN USE) spillover zone -> that category is the dominant one
	## at least dist_max -> u.s.w.
	## e.g. if "stations_remove" dominates, the unit is marked as a forbidden comparison; instead,
	## it'll be considered first treatment iif it's outside the buffer of an older station but
	## close enough to a new one u.s.w.

	# cobs <- c(863, 951, 1314, 1360, 8106, 106974, 865499, 1127461) # compact sample with all cases

	d0 <- dist[["stations_remove"]] |>
		# filter(id_addr_cad %in% cobs) |>
		filter(distance <= assumptions$cutoff_sharp)
	forbidden_comparison <- d0$id_addr_cad
	n_forbidden_comparison <- individual_data |>
		filter(id_addr_cad %in% forbidden_comparison) |>
		nrow()
	drop_me <- forbidden_comparison
	d1_0 <- dist[["stations_new"]] |>
		# filter(id_addr_cad %in% cobs) |>
		filter(distance <= assumptions$cutoff_sharp & !id_addr_cad %in% drop_me)
	new_dose <- d1_0$id_addr_cad
	d2_0 <- dist[["stations_pre"]] |>
		# filter(id_addr_cad %in% cobs) |>
		filter(distance <= assumptions$cutoff_sharp & !id_addr_cad %in% drop_me)
	i_am_old <- d2_0$id_addr_cad
	no_more_doses <- setdiff(i_am_old, new_dose)
	more_doses <- setdiff(i_am_old, no_more_doses)
	i_am_new <- setdiff(new_dose, i_am_old)
	stopifnot(length(i_am_old) == length(no_more_doses) + length(more_doses))
	stopifnot(length(new_dose) == length(i_am_new) + length(more_doses))
	d1 <- filter(d2_0, id_addr_cad %in% no_more_doses)
	d2 <- filter(d2_0, id_addr_cad %in% more_doses)
	d3 <- filter(d1_0, id_addr_cad %in% i_am_new)
	if (is.null(dist[["stations_nyet"]])) {
		coming_soon <- 0
		d4 <- tibble()
	} else {
		d4 <- dist[["stations_nyet"]] |>
			# filter(id_addr_cad %in% cobs) |>
			filter(distance <= assumptions$cutoff_sharp) |>
			filter_out(id_addr_cad %in% c(drop_me, i_am_old, i_am_new))
		coming_soon <- d4$id_addr_cad
	}
	d5 <- dist[["stations_future"]] |>
		# filter(id_addr_cad %in% cobs) |>
		filter(distance <= assumptions$cutoff_sharp) |>
		filter_out(id_addr_cad %in% c(drop_me, i_am_old, i_am_new, coming_soon))
	one_day <- d5$id_addr_cad
	d6 <- bind_rows(dist) |>
		filter_out(
			id_addr_cad %in% c(drop_me, i_am_old, i_am_new, coming_soon, one_day)
		) |>
		arrange(distance) |>
		distinct(id_addr_cad, .keep_all = TRUE)
	not_me <- d6$id_addr_cad

	stopifnot(
		setdiff(c(drop_me, i_am_old, i_am_new, coming_soon, one_day, not_me), inside_max_dist) == 0L
	)

	treat_status <- bind_rows(d1, d2, d3, d4, d5, d6) |>
		mutate(
			status_num = case_when(
				id_addr_cad %in% drop_me ~ 0,
				id_addr_cad %in% no_more_doses ~ 1,
				id_addr_cad %in% more_doses ~ 2,
				id_addr_cad %in% i_am_new ~ 3,
				id_addr_cad %in% coming_soon ~ 4,
				id_addr_cad %in% one_day ~ 5,
				id_addr_cad %in% not_me ~ 6
			)
		)

	stopifnot(n_distinct(treat_status$id_addr_cad) == nrow(treat_status))
	station_dates <- sf::st_drop_geometry(stations) |>
		select(id_closest = code_station, dt_trial_svc)
	stopifnot(n_distinct(station_dates$id_closest) == nrow(station_dates))
	treat_status <- treat_status |>
		inner_join(station_dates, by = join_by(id_closest))

	treat_status <- treat_status |>
		mutate(reference_date = as.Date(paste(event_year, "01", "01", sep = "-")))

	# remove fossile chunks (not yet!)
	# treat_status <- bind_rows(dist, .id = "status") |>
	# 	mutate(
	# 		station_rank = recode_values(
	# 			status,
	# 			"stations_remove" ~ 5,
	# 			"stations_pre" ~ 4,
	# 			"stations_new" ~ 3,
	# 			"stations_nyet" ~ 2,
	# 			"stations_future" ~ 1
	# 		),
	# 		zone = case_when(
	# 			distance <= assumptions$cutoff_sharp ~ 3,
	# 			distance <= assumptions$spillover_sharp ~ 2,
	# 			TRUE ~ 1
	# 		)
	# 	)

	# # tester <- c(863, 951, 1314, 1360, 936039, 890589, 62826, 203926, 123955, 970770)
	# treat_status <- treat_status |>
	# 	# filter(id_addr_cad %in% tester) |>
	# 	group_by(id_addr_cad) |>
	# 	summarise(
	# 		forbidden = any(zone == 3 & station_rank == 5),
	# 		always_treated = any(zone == 3 & station_rank == 4),
	# 		new_treatment = any(zone == 3 & station_rank == 3),
	# 		not_yet_treated = any(zone == 3 & station_rank == 2),
	# 		planned = any(zone == 3 & station_rank == 1),
	# 		spillover = any(zone == 2 & station_rank > 2),
	# 	) |>
	# 	mutate(
	# 		first = (!always_treated) * new_treatment * (!forbidden),
	# 		always = always_treated * (!forbidden),
	# 		additional = always * new_treatment,
	# 		control_general = (!first) * (!always) * (!additional),
	# 		control_never = control_general * (!not_yet_treated),
	# 		control_nyet = control_general * not_yet_treated,
	# 		control_planned = control_general * planned
	# 		# pure_control_nyet = (!spillover) * control_nyet,
	# 		# pure_control_never = (!spillover) * control_never,
	# 		# pure_treat = (!spillover) * first
	# 	)

	# # retrieve G_i (yr_treat) and distance to treatment
	# ## although one would think that we only need that info for "first", "always", and "additional",
	# ##
	# stopifnot(n_distinct(treat_status$id_addr_cad) == nrow(treat_status))
	# treated_units <- list(
	# 	"first" = filter(treat_status, first == 1) |> select(id_addr_cad, always),
	# 	"always" = filter(treat_status, always == 1) |> select(id_addr_cad, always),
	# 	"additional" = filter(treat_status, additional == 1) |> select(id_addr_cad, always),
	# 	"future" = filter(treat_status, )
	# )

	# purrr::map2(
	# 	names(treated_units), dist,
	# 	function(x, y) {
	#     if(x == "always") {
	# 			d <- purrr::pluck(y, "stations_pre")
	# 		} else {
	#       d <- purrr::pluck(y, "")
	#     }
	# 	}
	# )
	# # this should accumulate
	# # bind_rows(
	# # 	inner_join(
	# #   	status_3,
	# # 		dist |> filter(status == "stations_pre") |> mutate(always_treated = TRUE)
	# # 	),
	# # 	inner_join(
	# #   	status_3,
	# # 		dist |> filter(status == "stations_new") |> mutate(new_treatment = TRUE)
	# # 	),
	# #   filter(status_3, !(always_treated | new_treatment))
	# # )

	# age criterion: individuals must be [18, 60] years at the "opening anniversary" at the g_lag
	individual_data <- individual_data |>
		inner_join(arrow::as_arrow_table(treat_status), by = join_by(id_addr_cad)) |>
		mutate(
			age_opening_lag = (as.integer(reference_date) - as.integer(birthdate_cad)) /
				365.25
		) |>
		compute()
	out_of_age <- individual_data |>
		filter(
			!between(age_opening_lag, assumptions$age_min, assumptions$age_max)
		) |>
		pull(cpf)

	# final data
	eligible_units <- filter(individual_data, !cpf %in% out_of_age) |>
		compute()
	n_out_of_age <- individual_data |> filter(cpf %in% out_of_age) |> nrow()
	risk_set_audit <- tibble(
		step = seq_len(7),
		reason = c(
			'initial',
			'bad_geocoding',
			'stale_entry',
			'outside_max_dist',
			'forbidden_comparison',
			'out_of_age',
			'retained'
		),
		n_records = c(
			n0_ind,
			n_bad_geocoding,
			n_stale_entry,
			n_outside_max_dist,
			n_forbidden_comparison,
			n_out_of_age,
			nrow(eligible_units)
		),
		event_year = event_year,
		baseline_year = event_year_lag
	)

	file_name <- paste0("risk_set_", event_year, ".parquet")
	file_path <- file.path(save_dir, file_name)
	write_parquet_target(
		eligible_units,
		file_path,
		overwrite = TRUE
	)

	audit_path <- file.path(save_dir, paste0('risk_set_audit_', event_year, '.parquet'))
	write_parquet_target(risk_set_audit, audit_path, overwrite = TRUE)

	return(c(records = file_path, audit = audit_path))
}


# plot_risk_sets -----------------------------------------------------------------------------

# source("R/utils.R")
# library(ggplot2)
# risk_set <- tar_read(risk_sets, branches = 2)
# stations <- tar_read(stations_sf)
# events <- tar_read(opening_events) |>
# 	filter(yr_open == 2018)
# tar_load(lines_sf)

plot_risk_sets <- function(risk_set, stations, events, lines_sf, save_dir = "figures") {
	# read data
	risk_set <- grep("audit", risk_set, value = TRUE, invert = TRUE) |>
		arrow::open_dataset()
	event_year <- unique(events$yr_open)
	event_year_lag <- pull(risk_set, year, as_vector = TRUE) |> unique()
	units_sf <- risk_set |>
		distinct(lat_cad, lon_cad, id_closest, distance, status_num) |>
		sf::st_as_sf(coords = c("lon_cad", "lat_cad"), crs = 4326) |>
		sf::st_transform(crs = 31983) |>
		mutate(status_fct = factor(status_num))
	stations <- sf_from_parquet(stations)
	stopifnot(n_distinct(events$code_station) == nrow(events))
	stations_new <- semi_join(stations, events, by = join_by(code_station))
	lines_sf <- sf_from_parquet(lines_sf)
	lines_sf <- lines_sf |>
		filter(yr_open <= event_year) |>
		mutate(status = if_else(yr_open <= event_year_lag, "Baseline", "New segment"))

	# buffers
	b1 <- stations_new |> sf::st_buffer(2e3) |> sf::st_union()
	# b2 <- stations_new |> sf::st_buffer(4e3) |> sf::st_union()
	b3 <- stations_new |> sf::st_buffer(5e3) |> sf::st_union()

	# labels
	status_labs <- c(
		"0" = "Forbidden comparison", #drop_me
		"1" = "Always treated\n(indirect effect)", #no_more_doses
		"2" = "Always treated\n(additional dose)", #more_doses
		"3" = "First treatment", #i_am_new
		"4" = "Not-yet-treated", #coming_soon
		"5" = "Future treatment\n(under construction)", #one_day
		"6" = "Never treated" #not_me
	)

	# plot
	p_risk_set <- ggplot() +
		geom_sf(data = lines_sf, aes(linetype = status), color = "gray60", size = 1) +
		geom_sf(data = units_sf, aes(color = status_fct), stroke = 0.125, size = 0.125) +
		geom_sf(data = b1, color = "red", fill = NA) +
		# geom_sf(data = b2, color = "blue", fill = NA) +
		geom_sf(data = stations_new, color = "red") +
		scale_color_viridis_d(labels = status_labs) +
		spatialops::geom_bbox(b3) +
		labs(
			title = paste("Risk set for the", event_year, "cohort"),
			subtitle = paste("Individuals are eligible based on their home location in", event_year_lag),
			color = "Treatment status"
		) +
		guides(
			color = guide_legend(order = 1, override.aes = list(size = 3)),
			linetype = guide_legend(title = NULL)
		) +
		theme_void()
	# p_risk_set

	# save
	file_name <- paste0("risk_set_", event_year, ".png")
	file_path <- file.path(save_dir, file_name)
	ggsave(
		filename = file_path,
		plot = p_risk_set,
		width = 16,
		height = 12,
		units = "cm",
		dpi = 300,
		bg = "white"
	)

	gg_name <- paste0("risk_set_", event_year, ".rds")
	gg_path <- file.path(save_dir, gg_name)
	saveRDS(p_risk_set, gg_path)

	return(c(plot_png = file_path, plot_gg = gg_path))
}

## dubg & dataviz - ignore
# # status_count <- count(
# # 	status,
# # 	forbidden,
# # 	# spillover,
# # 	first,
# # 	additional,
# # 	baseline,
# # 	control_never,
# # 	control_nyet #,
# # 	# pure_treat,
# # 	# pure_control_never,
# # 	# pure_control_nyet
# # )

# status_sf <- status |>
# 	mutate(
# 		status_lvl = case_when(
# 			# pure_treat == 1 ~ 8,
# 			first + pure_treat >= 1 ~ 7,
# 			# pure_control_never == 1 ~ 3,
# 			# pure_control_nyet == 1 ~ 2,
# 			# control_never == 1 ~ 5,
# 			# control_nyet == 1 ~ 4,
# 			control_never + pure_control_never >= 1 ~ 5,
# 			control_nyet + pure_control_nyet >= 1 ~ 4,
# 			additional == 1 ~ 6,
# 			TRUE ~ 1
# 		) |>
# 			factor()
# 	) |>
# 	# mutate(status_fct = forcats::fct_reorder(status_fct, status_lvl)) |>
# 	# filter(first == 1) |>
# 	inner_join(addresses) |>
# 	sf::st_as_sf()

# b1 <- stations_list$stations_new |> sf::st_buffer(2e3) |> sf::st_union()
# b2 <- stations_list$stations_new |> sf::st_buffer(4e3) |> sf::st_union()
# b3 <- stations_list$stations_new |> sf::st_buffer(7e3) |> sf::st_union()

# status_labs <- c(
# 	"0" = "Forbidden comparison",
# 	"1" = "Always treated\n(indirect effect)",
# 	# "2" = "Pure never treated",
# 	# "3" = "Pure not-yet-treated",
# 	"4" = "Not-yet-treated", # with spillovers",
# 	"5" = "Never treated", # with spillovers",
# 	"6" = "Additional treatment",
# 	"7" = "First treatment" #,
# 	# "8" = "Pure treatment"
# )

# metro_lines <- tar_read(lines_sf)
# metro_sf <- sf_from_parquet(metro_lines) |>
# 	filter(yr_open <= 2018)
# plot_risk_sets <- ggplot() +
# 	geom_sf(data = status_sf, aes(color = status_lvl), stroke = 0.0625, size = 0.125) +
# 	geom_sf(data = b1, color = "red", fill = NA) +
# 	geom_sf(data = b2, color = "blue", fill = NA) +
# 	geom_sf(data = metro_sf, color = "gray60", size = 1) +
# 	geom_sf(data = stations_list$stations_new, color = "red") +
# 	scale_color_viridis_d(labels = status_labs) +
# 	spatialops::geom_bbox(b3) +
# 	labs(
# 		title = "Risk set for the 2018 cohort",
# 		subtitle = "Individuals are eligible based on their home location in 2014",
# 		color = "Treatment status"
# 	) +
# 	guides(color = guide_legend(override.aes = list(size = 3))) +
# 	theme_void()

# # plot_risk_sets

# # ggsave("treat_map_2018.png", plot = plot_risk_sets, dpi = 300, bg = "white")

# # mapview(foo) +
# # 	mapview(b1) +
# # 	mapview(b2)

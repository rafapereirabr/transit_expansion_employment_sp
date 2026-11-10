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
		g_lag = 0
	}

	treat_window <- time_window[(time_window - g_lag) %in% time_window]

	treat_list <- list(
		"time_window" = time_window,
		"g_lag" = g_lag,
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
	stations <- sf_from_parquet(stations, drop_geometry = T)

	events <- stations |>
		filter(lubridate::year(dt_trial_svc) %in% assumptions$treat_window) |>
		select(code_line, code_station, yr_open, dt_trial_svc) |>
		mutate(
			id_event = paste0(
				"L",
				sprintf("%02d", code_line),
				"_",
				yr_open,
				"_q",
				lubridate::quarter(dt_trial_svc)
			),
			yr_base = lubridate::year(dt_trial_svc) - assumptions$g_lag,
			quarter = lubridate::quarter(dt_trial_svc),
			dt_standard = lubridate::ceiling_date(dt_trial_svc, "quarter") - lubridate::days(1)
		) |>
		mutate(dt_standard = as.Date(dt_standard)) |>
		relocate(id_event, .before = everything()) |>
		arrange(id_event)

	return(events)

	distinct(events, event_id)
}

# events <- set_opening_events(stations, assumptions)

# define_risk_sets -------------------------------------------------------

# source("R/utils.R")
# source("R/dist_nearest.R")
# tar_load(c(stations_sf, munis_sf, family_records, person_records, did_spec, time_window))
# stations <- stations_sf
# munis <- munis_sf
# household_data <- family_records
# individual_data <- person_records
# event <- events |>
#   # filter(yr_open == 2018)
#   # filter(id_event == "L05_2017_q3")
#   filter(id_event == "L05_2018_q2")

define_risk_set <- function(stations, munis, household_data, individual_data, assumptions) {
	# filter events
	event_stations <- event$code_station
	event <- distinct(event, id_event, yr_open, yr_base, dt_standard)
	date_integer <- as.integer(event$dt_standard)
	event_year <- event$yr_open
	forbidden_years <- assumptions$treat_window[assumptions$treat_window < event_year]
	nyet_years <- setdiff(assumptions$treat_window, c(event_year, forbidden_years))

	# separate stations by treatment status:
	## always treated, opened before g, first treatment, not yet, and future treatment
	stations <- sf_from_parquet(stations) |>
		select(code_line, code_station, name_station, yr_open, geometry)
	stations_list <- list(
		"stations_pre" = filter(stations, yr_open < min(assumptions$time_window)),
		"stations_remove" = filter(stations, yr_open %in% forbidden_years),
		"stations_new" = filter(stations, code_station %in% event_stations),
		"stations_nyet" = filter(stations, yr_open %in% nyet_years),
		"stations_future" = filter(stations, code_line %in% c(6, 17))
	)

	## remove any sublist with no station (e.g. _remove or _nyet)
	n_rows <- purrr::map(stations_list, nrow)
	stations_list <- stations_list[n_rows > 0]

	# individual data: age criterion
	individual_data <- grep(event$yr_base, individual_data, value = T) |>
		arrow::open_dataset()
	individual_data <- individual_data |>
		mutate(age_at_opening = (date_integer - as.integer(birthdate_cad)) / 365.25)
	out_of_age <- individual_data |>
		filter(!between(age_at_opening, 18, 60)) |>
		pull(cpf)
	individual_data <- filter(individual_data, !cpf %in% out_of_age)
	individ_roster <- individual_data |>
		distinct(year, id_family) |>
		compute()

	# household data: spatial criteria (bad geocoding and out of studied area)
	household_data <- arrow::open_dataset(household_data)
	household_data <- inner_join(
		household_data,
		individ_roster,
		by = join_by("co_familiar_fam" == "id_family", "year")
	) |>
		compute()
	n0_hh <- nrow(household_data)
	bad_geocoding <- household_data |> filter(desvio_metros > 500) |> distinct(id_addr_cad) |> pull()
	munis <- sf_from_parquet(munis, T)
	out_of_reach <- individual_data |>
		filter(!(code_muni_cad %in% munis$code_muni)) |>
		distinct(id_addr_cad) |>
		pull()

	# final address list
	addresses <- household_data |>
		filter(!(id_addr_cad %in% bad_geocoding)) |>
		filter(!(id_addr_cad %in% out_of_reach)) |>
		distinct(id_addr_cad, lat, lon) |>
		collect() |>
		sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
		sf::st_transform(crs = 31983)

	conn <- duckspatial::ddbs_create_conn()
	duckspatial::ddbs_write_table(conn, addresses, "addresses", overwrite = T)
	purrr::imap(
		stations_list,
		function(x, idx) {
			duckspatial::ddbs_write_table(conn, x, idx, overwrite = T)
		}
	)

	dist <- purrr::map(
		names(stations_list),
		function(x) {
			dist_nearest(
				"addresses",
				x,
				id_feature = T,
				id_y = "code_station",
				drop_geometry = T,
				conn = conn,
				dist_type = "planar",
				max_distance = assumptions$max_dist_sharp,
				candidate_crs = 31983
			)
		}
	)
	names(dist) <- names(stations_list)

	dist <- purrr::map(dist, collect) |>
		bind_rows(.id = "status")

	# transitive treatment status
	## if a unit is "close enough" to more than one station, we solve matches using this rule:
	## inside treatment buffer (2km) -> that category is the dominant one
	## spillover zone -> that category is the dominant one
	## at least dist_max -> u.s.w.
	## therefore, (nyet == 1) * (inside == 1) = 3*1 but
	status <- dist |>
		mutate(
			station_rank = recode_values(
				status,
				"stations_remove" ~ 5,
				"stations_pre" ~ 4,
				"stations_new" ~ 3,
				"stations_nyet" ~ 2,
				"stations_future" ~ 1
			),
			zone = case_when(
				distance <= assumptions$cutoff_sharp ~ 3,
				distance <= assumptions$spillover_sharp ~ 2,
				TRUE ~ 1
			)
			# treat = if_else(, 1, 0),
			# spillover = if_else(treat == 0 & distance <= assumptions$spillover_sharp, 1, 0),
			# control = if_else(treat == 0 & spillover == 0 & distance <= assumptions$max_dist_sharp, 1, 0),
			# removed = if_else(treat + spillover + control == 0, 1, 0)
		)

	# cobaias <- c(936039, 890589, 62826, 203926, 123955, 970770)
	status <- status |>
		# filter(id_addr_cad %in% cobaias) |>
		group_by(id_addr_cad) |>
		summarise(
			forbidden = any(zone == 3 & station_rank == 5),
			always_treated = any(zone == 3 & station_rank == 4),
			new_treatment = any(zone == 3 & station_rank == 3),
			not_yet_treated = any(zone == 3 & station_rank == 2),
			spillover = any(zone == 2 & station_rank > 2)
		) |>
		mutate(
			first = (!always_treated) * new_treatment * (!forbidden),
			additional = always_treated * new_treatment * (!forbidden),
			baseline = always_treated * (!new_treatment) * (!forbidden),
			control = (!always_treated) * (!new_treatment) * (!forbidden),
			pure_control = (!spillover) * control,
			pure_treat = (!spillover) * first
		)

	return(status)
}

## dubg & dataviz - ignore

# status_count <- count(
# 		status,
# 		forbidden,
# 		spillover,
# 		first,
# 		additional,
# 		baseline,
# 		control,
# 		pure_treat,
# 		pure_control
# 	)

# status_sf <- status |>
# 	mutate(
# 		status_lvl = case_when(
# 			pure_treat == 1 ~ 6,
# 			first == 1 ~ 5,
# 			pure_control == 1 ~ 2,
# 			control == 1 ~ 3,
# 			additional == 1 ~ 4,
# 			forbidden ~ 0,
# 			TRUE ~ 1
# 		),
# 		status_fct = recode_values(
# 			status_lvl,
# 			0 ~ "Forbidden",
# 			1 ~ "Baseline",
# 			2 ~ "Pure control",
# 			3 ~ "Control with spillovers",
# 			4 ~ "Additional treatment",
# 			5 ~ "First treatment",
# 			6 ~ "Pure treatment"
# 		),
# 	) |>
#   mutate(status_fct = forcats::fct_reorder(status_fct, status_lvl)) |>
# 	# filter(first == 1) |>
# 	inner_join(addresses) |>
# 	sf::st_as_sf()

# b1 <- stations_list$stations_new |> sf::st_buffer(2e3) |> sf::st_union()
# b2 <- stations_list$stations_new |> sf::st_buffer(4e3) |> sf::st_union()
# b3 <- stations_list$stations_new |> sf::st_buffer(7e3) |> sf::st_union()

# ggplot() +
# 	geom_sf(data = status_sf, aes(color = status_fct), stroke = 0, size = 0.125) +
# 	geom_sf(data = stations_list$stations_new, color = "red") +
# 	geom_sf(data = b1, color = "red", fill = NA) +
# 	geom_sf(data = b2, color = "blue", fill = NA) +
# 	spatialops::geom_bbox(b3) +
# 	guides(color = guide_legend(override.aes = list(size = 3))) +
# 	theme_void()

# ggsave("treat_map.png", dpi = 300, bg = "white")

# mapview(foo) +
# 	mapview(b1) +
# 	mapview(b2)

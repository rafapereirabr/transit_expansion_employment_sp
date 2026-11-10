# make_station_base ---------------------------------------------------------------------------

make_station_base <- function(
	subway,
	planned_subway,
	train,
	planned_train
) {
	col_select <- c("nm_linha_metro_trem", "cd_identificador", "nm_estacao_metro_trem", "ge_ponto")

	col_rename <- c(
		name_line = "nm_linha_metro_trem",
		code_station = "cd_identificador",
		name_station = "nm_estacao_metro_trem",
		geometry = "ge_ponto"
	)

	## dictionary -------------------------------------------------------------------------------
	lines_dic <- tibble::tribble(
		~name_line , ~code_line ,
		"AZUL"     ,          1 , "VERDE"     ,  2 , "VERMELHA" ,  3 , "AMARELA" ,  4 , "LILAS"   ,  5 , "LARANJA" ,  6 , "RUBI"   ,  7 ,
		"DIAMANTE" ,          8 , "ESMERALDA" ,  9 , "TURQUESA" , 10 , "CORAL"   , 11 , "SAFIRA"  , 12 , "JADE"    , 13 ,
		"ONIX"     ,         14 , "PRATA"     , 15 , "VIOLETA"  , 16 , "OURO"    , 17 , "CELESTE" , 19 , "ROSA"    , 20 , "MARROM" , 22 ,
		"QUARTZO"  ,         24
	) |>
		mutate(name_system = if_else(code_line %in% c(1:6, 15:23), "Metro", "Train"))

	### line labels for pretty plots
	lines_dic <- lines_dic |>
		mutate(label_line = paste0(name_system, " L", code_line)) |>
		mutate(label_line = factor(code_line, levels = code_line, labels = label_line))

	## subway stations --------------------------------------------------------------------------
	subway_stations <- sf::read_sf(subway)

	subway_stations <- subway_stations |>
		select(all_of(col_select)) |>
		rename(all_of(col_rename)) |>
		# GeoSampa duplicates Vila Mariana at the same coordinates under station
		# codes 33 and 50. Code 50 is the canonical identifier used downstream.
		filter(code_station != 33) |>
		mutate(running = 1)

	## planned subway stations ------------------------------------------------------------------
	planned_subway_stations <- sf::read_sf(planned_subway)

	planned_subway_stations <- planned_subway_stations |>
		select(all_of(col_select)) |>
		rename(all_of(col_rename)) |>
		filter(!(name_station %in% c("VILA SÔNIA", "VILA PRUDENTE"))) |>
		# Stable codes remove planned duplicates even when source names have encoding differences.
		filter(!(code_station %in% c(95, 136))) |>
		mutate(running = as.integer(code_station %in% 99:106))

	## train stations ---------------------------------------------------------------------------
	train_stations <- sf::read_sf(train)

	train_stations <- train_stations |>
		select(all_of(col_select)) |>
		rename(all_of(col_rename)) |>
		mutate(running = 1)

	## planned train stations -------------------------------------------------------------------
	planned_train_stations <- sf::read_sf(planned_train)

	planned_train_stations <- planned_train_stations |>
		select(all_of(col_select)) |>
		rename(all_of(col_rename)) |>
		filter(!(name_station %in% c("VARGINHA"))) |>
		mutate(
			name_line = ifelse(name_line == "ALPHAVILLE - CAMPO LIMPO", "QUARTZO", name_line),
			running = 0
		)

	## join stations ----------------------------------------------------------------------------
	stations_sf <- bind_rows(
		subway_stations,
		train_stations,
		planned_subway_stations,
		planned_train_stations
	) |>
		left_join(lines_dic, by = "name_line") |>
		sf::st_as_sf()
	sf::st_geometry(stations_sf) <- "geometry"

	return(stations_sf)
}


# write_station_raw ---------------------------------------------------------------------------

write_station_raw <- function(stations, save_path, overwrite = FALSE) {
	stopifnot(inherits(stations, "sf"))

	if (!file.exists(save_path) || overwrite) {
		writexl::write_xlsx(sf::st_drop_geometry(stations), save_path)
	}

	return(save_path)
}


# tidy_transit_stations -----------------------------------------------------------------------

tidy_transit_stations <- function(
	stations,
	openings,
	opening_sheet = "data",
	save_path,
	overwrite = FALSE
) {
	stopifnot(inherits(stations, "sf"))
	stopifnot(length(openings) == 1L)
	stopifnot(grepl("[.]xlsx$", openings, ignore.case = TRUE))

	dates <- readxl::read_xlsx(openings, sheet = opening_sheet)
	keys <- c("name_line", "code_station", "name_station")

	station_keys <- stations |>
		sf::st_drop_geometry() |>
		select(all_of(keys))
	date_keys <- dates |>
		select(all_of(keys))

	station_dupes <- station_keys |>
		group_by(across(all_of(keys))) |>
		summarise(n = n(), .groups = "drop") |>
		filter(n > 1L)
	date_dupes <- date_keys |>
		group_by(across(all_of(keys))) |>
		summarise(n = n(), .groups = "drop") |>
		filter(n > 1L)

	if (nrow(station_dupes) > 0L) {
		stop("Station base has duplicated opening-catalog keys.", call. = FALSE)
	}
	if (nrow(date_dupes) > 0L) {
		stop("Station opening catalog has duplicated keys.", call. = FALSE)
	}

	missing_dates <- anti_join(station_keys, date_keys, by = keys)
	missing_stations <- anti_join(date_keys, station_keys, by = keys)

	if (nrow(missing_dates) > 0L) {
		stop(
			sprintf("Station opening catalog is missing %d station keys.", nrow(missing_dates)),
			call. = FALSE
		)
	}
	if (nrow(missing_stations) > 0L) {
		stop(
			sprintf(
				"Station opening catalog has %d keys absent from the station base.",
				nrow(missing_stations)
			),
			call. = FALSE
		)
	}

	stations_sf <- left_join(
		select(stations, -running),
		dates,
		by = keys,
		relationship = "one-to-one"
	) |>
		mutate(across(c(infill, rebuilt), ~ tidyr::replace_na(.x, 0))) |>
		select(-any_of(c("reform", "yr_original", "yr_reform")))

	###' eras: even though we're in 2026 and so line 17 is open and line 6 will open next week,
	###' our max data ref is 2025 (if we use it!), so these stations will be treated as u/c. still,
	###' factor variable `running` will reflect the temporal truth, this is for pretty plotting.
	stations_sf <- stations_sf |>
		mutate(
			era = case_when(yr_open < 2013 ~ "Pre-2013", yr_open < 2026 ~ "2013-2025", TRUE ~ "Future")
		) |>
		mutate(
			era = factor(era, levels = c("Pre-2013", "2013-2025", "Future"), ordered = T),
			status = case_when(
				yr_open < 2026 ~ "Running",
				yr_open < 2030 ~ "Under Construction",
				TRUE ~ "Planned"
			)
		)

	## save -------------------------------------------------------------------------------------
	stations_sf <- stations_sf |>
		relocate(
			code_line,
			name_line,
			code_station,
			name_station,
			name_system, # basic info
			running,
			infill,
			rebuilt, # dummies
			yr_open,
			dt_trial_svc,
			dt_full_svc, # dates
			src_trial_svc,
			src_full_svc,
			obs, # text info
			label_line,
			era,
			status,
			geometry # cosmetics and geometry
		)

	obj <- write_parquet_target(stations_sf, save_path, overwrite)
	return(obj)
}


# tidy_transit_lines --------------------------------------------------------------------------

tidy_transit_lines <- function(
	stations_path,
	subway,
	planned_subway,
	train,
	planned_train,
	save_path = NULL,
	overwrite = FALSE
) {
	stations_sf <- sf_from_parquet(stations_path)

	## subway lines -----------------------------------------------------------------------------
	subway_lines <- sf::read_sf(subway)

	## planned subway lines ---------------------------------------------------------------------
	planned_subway_lines <- sf::read_sf(planned_subway)

	planned_subway_lines <- planned_subway_lines |>
		mutate(
			nm_empresa_metro_trem = ifelse(nm_linha_metro_trem == "VIOLETA", "METRO", nm_empresa_metro_trem)
		)

	## train lines ------------------------------------------------------------------------------
	train_lines <- sf::read_sf(train)

	##' we need this gimmick to select the geometry that better represents train lines, since there
	##' are multiple geometries (tracks and services apparently)
	# train_lines |> filter(cd_identificador_linha %in% c(10, 12)) |>
	#   ggplot() + geom_sf(aes(color = factor(cd_identificador))) #+ facet_wrap(vars(cd_identificador))

	codes <- c(10001, 10004, 10009, 10014, 10019, 10024, 10025)

	train_lines <- train_lines |>
		filter(cd_identificador %in% codes)

	## planned train lines ----------------------------------------------------------------------
	planned_train_lines <- sf::read_sf("data-raw/planned_train_lines.gpkg")

	##' we discard those because both are operational and present in the other shapefile; in additon,
	##' the airport express service runs entirely in already represented tracks and isn't necessary here.
	planned_train_lines <- planned_train_lines |>
		filter(!(nm_linha_metro_trem %in% c("EXPRESSO AEROPORTO", "ESMERALDA")))

	planned_train_lines <- planned_train_lines |>
		mutate(
			cd_identificador_linha = replace_values(cd_identificador_linha, NA ~ 24),
			nm_linha_metro_trem = replace_values(nm_linha_metro_trem, "ALPHAVILLE - CAMPO LIMPO" ~ "QUARTZO")
		)

	## join lines -------------------------------------------------------------------------------
	lines_sf <- bind_rows(subway_lines, train_lines, planned_subway_lines, planned_train_lines)

	lines_sf <- lines_sf |>
		select(
			code_line = "cd_identificador_linha",
			name_line = "nm_linha_metro_trem",
			name_operator = "nm_empresa_metro_trem",
			geometry = ge_linha
		)

	## split into segments ----------------------------------------------------------------------

	lines_sf <- segment_lines(lines_sf, stations_sf, snap_tol = 40)

	### our system is not perfect so we need to make some adjustments
	lines_sf <- lines_sf |>
		select(-infill) |>
		mutate(
			yr_open = case_when(
				code_line == 4 & from == "OSCAR FREIRE" ~ 2011,
				code_line %in% c(7, 10) ~ 1867,
				code_line == 11 & (yr_open > 1929 | is.na(yr_open)) ~ 1875,
				code_line == 12 ~ 1926,
				code_line == 8 & yr_open == 2025 ~ 2014,
				TRUE ~ yr_open
			)
		)

	## save -------------------------------------------------------------------------------------
	lines_dic <- stations_sf |>
		sf::st_drop_geometry() |>
		distinct(code_line, name_line, name_system, yr_open, label_line, era, status)

	lines_sf <- left_join(lines_sf, lines_dic) |>
		relocate(
			code_line,
			name_line,
			name_system,
			name_operator,
			running,
			yr_open,
			from,
			to,
			label_line,
			era,
			status,
			geometry
		)

	obj <- write_parquet_target(lines_sf, save_path, overwrite)
	return(obj)
}

# test ----------------------------------------------------------------------------------------

# stations_sf <- tidy_transit_stations(
#   subway = "data-raw/subway_stations.gpkg",
#   planned_subway = "data-raw/planned_subway_stations.gpkg",
#   train = "data-raw/train_stations.gpkg",
#   planned_train = "data-raw/planned_train_stations.gpkg",
#   save_path = "data/transit_stations.parquet",
#   overwrite = TRUE
# )
#
# lines_sf <- tidy_transit_lines(
#   stations_path = "data/transit_stations.parquet",
#   subway = "data-raw/subway_lines.gpkg",
#   planned_subway = "data-raw/planned_subway_lines.gpkg",
#   train = "data-raw/train_lines.gpkg",
#   planned_train = "data-raw/planned_train_lines.gpkg",
#   save_path = "data/transit_lines.parquet",
#   overwrite = TRUE
# )
#
#
## debugging
# subway = "data-raw/subway_stations.gpkg"
# planned_subway = "data-raw/planned_subway_stations.gpkg"
# train = "data-raw/train_stations.gpkg"
# planned_train = "data-raw/planned_train_stations.gpkg"
# save_path = "data/transit_stations.parquet"
# overwrite = TRUE
#
# stations_path = tar_read(stations_sf)
# subway = "data-raw/subway_lines.gpkg"
# planned_subway = "data-raw/planned_subway_lines.gpkg"
# train = "data-raw/train_lines.gpkg"
# planned_train = "data-raw/planned_train_lines.gpkg"
# save_path = "data/transit_lines.parquet"
# overwrite = TRUE

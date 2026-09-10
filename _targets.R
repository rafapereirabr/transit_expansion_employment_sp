# targets setup ------------------------------------------------------------------------------

options(
	arrow.pull_as_vector = FALSE,
	arrow.unsafe_metadata = TRUE,
	future.globals.maxSize = 1e4^1024
)

suppressPackageStartupMessages(
	{
		library(targets)
		library(crew)
		library(dplyr)
		library(geoarrow)
	}
)


# Set target options:
tar_option_set(
	packages = c(
		"archive",
		"arrow",
		"dplyr",
		"docstring",
		"duckspatial",
		"geoarrow",
		"ggplot2",
		"h3o",
		"lwgeom",
		"sf"
	),
	format = "parquet",
	deployment = "main",
	controller = crew_controller_local(
		workers = floor(.6 * parallelly::freeCores()[1])
	),
	trust_timestamps = TRUE,
	workspace_on_error = TRUE
)

tar_source(files = list.files("R", pattern = "\\.R$", full.names = TRUE))

if (!dir.exists("data")) {
	dir.create("data")
}


# targets list -------------------------------------------------------------------------------

list(
	## parameters ------------------------------------------------------------------------------
	tar_target(
		name = metro_palette,
		command = c(
			`Metro L1` = "#0153a0",
			`Metro L2` = "#008061",
			`Metro L3` = "#ee3e34",
			`Metro L4` = "#fed304",
			`Metro L5` = "#784d9f",
			`Metro L6` = "#f27800",
			`Train L7` = "#9e1766",
			`Train L8` = "#9e9e93",
			`Train L9` = "#00a78e",
			`Train L10` = "#007c8f",
			`Train L11` = "#f04d22",
			`Train L12` = "#083e89",
			`Train L13` = "#00ab5b",
			`Train L14` = "#1C1C1C",
			`Metro L15` = "#858d90",
			`Metro L16` = "#562283",
			`Metro L17` = "#bf9001",
			`Metro L18` = "#8C7853",
			`Metro L19` = "#21B4E2",
			`Metro L20` = "#e63271",
			`Metro L22` = "#764A30",
			`Train L24` = "#F7CAC9"
		),
		format = "rds"
	),
	tar_target(
		name = time_window,
		command = c(2012:2025),
		format = "rds"
	),
	tar_target(
		name = routing_spec,
		command = tibble::tibble(
			year = c(2012L, 2015L, 2019L, 2025L),
			datetime = paste0(year, "-10-", c("03", "07", "02", "01"), " 06:50:00") |>
				as.POSIXct(tz = "America/Sao_Paulo")
		) |>
			dplyr::group_by(year) |>
			targets::tar_group(),
		iteration = "group"
	),
	tar_target(
		name = r5_resources,
		command = list(
			ram = as.integer(Sys.getenv("r5r_ram", "8")),
			cpu = as.integer(Sys.getenv("r5r_cpu", "4"))
		),
		format = "rds"
	),
	tar_target(
		name = bus_speed_spec,
		command = set_bus_speed_spec(),
		format = "rds"
	),

	## shapefiles -------------------------------------------------------------------------------
	tar_target(
		name = stations_sf,
		command = tidy_transit_stations(
			subway = "data-raw/subway_stations.gpkg",
			planned_subway = "data-raw/planned_subway_stations.gpkg",
			train = "data-raw/train_stations.gpkg",
			planned_train = "data-raw/planned_train_stations.gpkg",
			save_path = "data/transit_stations.parquet",
			overwrite = T
		),
		format = "file"
	),
	tar_target(
		name = rail_service_spec,
		command = set_rail_service_spec(stations_sf)
	),
	tar_target(
		name = rail_stop_corrections,
		command = set_rail_stop_corrections()
	),
	tar_target(
		name = rail_template_overrides,
		command = set_rail_template_overrides()
	),
	tar_target(
		name = lines_sf,
		command = tidy_transit_lines(
			stations_path = stations_sf,
			subway = "data-raw/subway_lines.gpkg",
			planned_subway = "data-raw/planned_subway_lines.gpkg",
			train = "data-raw/train_lines.gpkg",
			planned_train = "data-raw/planned_train_lines.gpkg",
			save_path = "data/transit_lines.parquet",
			overwrite = T
		),
		format = "file"
	),
	tar_target(
		name = munis_sf,
		command = get_munis(save_path = "data/munis_sf.parquet", overwrite = T),
		format = "file"
	),
	tar_target(
		name = grid_sf,
		command = aopdata::read_landuse("spo", geometry = TRUE) |>
			dplyr::select(id = id_hex, P001, T001, E001, S001),
		format = "rds"
	),
	tar_target(
		name = footprint_sf,
		command = get_footprint(
			save_path = "data/footprint_sf.parquet",
			munis_path = munis_sf
		),
		format = "file"
	),

	## plots -----------------------------------------------------------------------------------
	tar_target(
		name = transit_map,
		command = plot_lines(
			footprint_sf = footprint_sf,
			munis_sf = munis_sf,
			lines_sf = lines_sf,
			stations_sf = stations_sf,
			metro_palette = metro_palette,
			save_path = "figures/fig_map.png"
		),
		format = "file"
	),
	tar_target(
		name = openings_plot,
		command = plot_openings(
			stations_sf = stations_sf,
			metro_palette = metro_palette,
			plot_type = "bars",
			save_path = "figures/fig_openings.png"
		),
		format = "file"
	),

	## transit feeds ---------------------------------------------------------------------------
	tar_target(
		name = raw_feed_paths,
		command = paste0("data-raw/gtfs_sptrans_", routing_spec$year, ".zip"),
		pattern = map(routing_spec),
		format = "file"
	),
	tar_target(
		name = feed_spec,
		command = set_feed_spec(raw_feed_paths, routing_spec)
	),
	tar_target(
		name = source_feed_audit,
		command = audit_source_feeds(
			feed_paths = raw_feed_paths,
			feed_spec = feed_spec,
			time_window = 15L
		)
	),
	tar_target(
		name = source_feed_reports,
		command = validate_feeds(
			feed_paths = raw_feed_paths,
			validator_dir = "data/gtfs_validator/source"
		),
		format = "file"
	),
	tar_target(
		name = raw_busway_paths,
		command = c("data-raw/busways.gpkg", "data-raw/mobilidados_2025.zip"),
		format = "file"
	),
	tar_target(
		name = gtfs_history_archive,
		command = "data-raw/3550308_sao_paulo.rar",
		format = "file"
	),
	tar_target(
		name = reference_feed_paths,
		command = extract_reference_feeds(
			archive_path = gtfs_history_archive,
			output_dir = "data/gtfs/history",
			spec = bus_speed_spec
		),
		format = "file"
	),
	tar_target(
		name = reference_feed_inventory,
		command = build_reference_feed_inventory(
			paths = reference_feed_paths,
			spec = bus_speed_spec,
			source_archive = gtfs_history_archive
		)
	),
	tar_target(
		name = bus_speed_model,
		command = estimate_reference_bus_speed_model(
			inventory = reference_feed_inventory,
			geosampa_path = raw_busway_paths[1],
			mobilidados_path = raw_busway_paths[2],
			spec = bus_speed_spec
		),
		format = "rds"
	),
	tar_target(
		name = bus_speed_surface,
		command = bus_speed_model$surface
	),
	tar_target(
		name = bus_speed_diagnostic_figures,
		command = plot_bus_speed_diagnostics(
			speed_summary = bus_speed_model$speed_summary,
			surface = bus_speed_surface,
			busways = bus_speed_model$busways,
			spec = bus_speed_spec,
			output_dir = "figures/diagnostics"
		),
		format = "file"
	),
	tar_target(
		name = prepared_feeds,
		command = prepare_feeds(feed_spec, output_dir = "data/gtfs/processed"),
		format = "file"
	),
	tar_target(
		name = bus_feeds,
		command = write_bus_feeds(
			feed_paths = prepared_feeds,
			spec = feed_spec,
			speed_surface = bus_speed_surface,
			geosampa_busways = raw_busway_paths[1],
			mobilidados_busways = raw_busway_paths[2],
			output_dir = "data/gtfs/bus"
		),
		format = "file"
	),
	tar_target(
		name = rail_feeds,
		command = write_scenario_rail_feed(
			prepared_feeds = prepared_feeds,
			feed_spec = feed_spec,
			routing_spec = routing_spec,
			service_spec = rail_service_spec,
			stop_corrections = rail_stop_corrections,
			template_overrides = rail_template_overrides
		),
		pattern = map(routing_spec),
		format = "file"
	),
	tar_target(
		name = scenario_feed_audit,
		command = audit_scenario_feeds(
			bus_feeds = bus_feeds,
			rail_feed = rail_feeds,
			feed_spec = feed_spec,
			routing_spec = routing_spec,
			time_window = 15L
		),
		pattern = map(routing_spec, rail_feeds)
	),
	tar_target(
		name = scenario_feed_reports,
		command = validate_scenario_feeds(
			bus_feeds = bus_feeds,
			rail_feed = rail_feeds,
			feed_spec = feed_spec,
			routing_spec = routing_spec,
			validator_dir = "data/gtfs_validator/scenario"
		),
		pattern = map(routing_spec, rail_feeds),
		format = "file"
	),
	tar_target(
		name = r5_feeds,
		command = export_feeds(
			spec = feed_spec,
			prepared_feeds = c(bus_feeds, rail_feeds),
			# additional_feeds = ,
			r5_dir = "data/r5",
			year = routing_spec$year
		),
		pattern = map(routing_spec),
		format = "file",
		deployment = "main"
	),

	## cadunico families ----------------------------------------------------------------------
	tar_target(
		name = cadunico_fam,
		command = read_cad_families(
			years = time_window,
			munis = munis_sf,
			save_dir = "data/temp"
		),
		format = "file"
	),

	## routing ---------------------------------------------------------------------------------
	tar_target(
		name = od_station_proximity,
		command = set_od_station_proximity(
			origins = cadunico_fam,
			destinations = stations_sf,
			origin_filter = grid_sf
		),
		format = "rds"
	),
	tar_target(
		name = od_grid_all,
		command = set_od_grid_all(grid = grid_sf),
		format = "rds"
	),
	tar_target(
		name = r5_network,
		command = build_r5r_network(
			dir = file.path("data/r5", routing_spec$year),
			feed_paths = r5_feeds,
			ram = r5_resources$ram,
			cpu = r5_resources$cpu
		),
		pattern = map(routing_spec, r5_feeds),
		format = "file",
		deployment = "main"
	),
	tar_target(
		name = r5_network_bypass,
		command = file.path("data/r5", routing_spec$year, "network.dat"),
		pattern = map(routing_spec),
		format = "file"
	),
	# tar_target(
	# 	name = ttm_walk_stations,
	# 	command = calc_ttm(
	# 		r5_network = r5_network,
	# 		od_table = od_station_proximity,
	# 		mode = "WALK",
	# 		max_duration = 180L,
	# 		threads = r5_resources$cpu,
	# 		ram = r5_resources$ram,
	# 		java_cpu = r5_resources$cpu
	# 	)
	# ),
	tar_target(
		name = ttm_transit,
		command = calc_ttm(
			od_table = od_grid_all,
			r5_network = r5_network,
			mode = "TRANSIT",
			departure_datetime = routing_spec$datetime,
			max_duration = 90L,
			threads = r5_resources$cpu,
			ram = r5_resources$ram,
			java_cpu = r5_resources$cpu
		),
		pattern = map(r5_network, routing_spec),
		deployment = "main"
	),

	## accessibility ---------------------------------------------------------------------------
	tar_target(
		name = ttm_bypass,
		command = c("data/temp/ttm_transit_2012", "data/temp/ttm_transit_2025"),
		format = "file"
	),
	tar_target(
		name = access,
		command = calc_access(
			ttm = ttm_transit,
			grid = grid_sf,
			year = ttm_transit$year,
			crit_val = 90,
			group = "dep_datetime",
			method = "cumulative_cutoff"
		),
		pattern = map(ttm_transit),
		deployment = "worker"
	),
	tar_target(name = access_plot, command = plot_access(access, grid_sf), format = "rds"),

	## pilot study -----------------------------------------------------------------------------
	tar_target(name = pilot_years, command = c(2012, 2019, 2025), format = "rds"),
	tar_target(name = pilot_cells, command = set_study_area(stations_sf, grid_sf)),
	tar_target(
		name = pilot_families,
		command = assign_treatment(cadunico_fam, pilot_cells, pilot_years)
	),
	tar_target(
		name = pilot_individuals,
		command = read_cad_individuals(pilot_years, pilot_families, "data/temp"),
		pattern = map(pilot_years),
		format = "file",
		deployment = "worker"
	),
	tar_target(
		name = cadunico_rais_pilot,
		command = read_rais(pilot_years, pilot_individuals, "data/temp"),
		pattern = map(pilot_years, pilot_individuals),
		format = "file",
		deployment = "worker"
	),
	tar_target(
		name = panel_pilot,
		command = make_panel(
			cadunico_rais = cadunico_rais_pilot,
			ttm = ttm_bypass,
			balance = TRUE,
			years = pilot_years,
			save_dir = "data/temp"
		),
		format = "file"
	),
	tar_target(
		name = naive_table,
		command = write_naive_table(panel_pilot, "tables", balanced = TRUE),
		format = "file"
	)

	## individuals: cadunico + rais ------------------------------------------------------------
	# tar_target(
	# 	name = cadunico_ind,
	# 	command = read_cad_individuals(
	# 		year = time_window,
	# 		families = cadunico_fam,
	# 		save_dir = "data/temp"
	# 	),
	# 	pattern = map(time_window),
	# 	format = "file",
	# 	deployment = "worker"
	# )
)

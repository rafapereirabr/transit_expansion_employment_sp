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
		"arrow",
		"dplyr",
		"docstring",
		"duckspatial",
		"sf",
		"geoarrow",
		"ggplot2",
		"h3o",
		"lwgeom"
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
			year = c(2012L, 2025L),
			datetime = as.POSIXct(
				c("2012-04-10 06:50:00", "2025-04-08 06:50:00"),
				tz = "America/Sao_Paulo"
			)
		) |>
			dplyr::group_by(year) |>
			targets::tar_group(),
		iteration = "group"
	),
	tar_target(
		name = r5_resources,
		command = list(
			ram = as.integer(Sys.getenv("R5R_RAM_GB", "8")),
			cpu = as.integer(Sys.getenv("R5R_CPU", "4"))
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
		command = c(
			"data-raw/gtfs_sptrans_2012.zip",
			"data-raw/gtfs_emtu_2014.zip",
			"data-raw/gtfs_sptrans_2025.zip",
			"data-raw/gtfs_emtu_2025.zip"
		),
		format = "file"
	),
	tar_target(
		name = feed_spec,
		command = tibble::tibble(
			input = raw_feed_paths,
			output_name = basename(input),
			year = c(2012L, 2012L, 2025L, 2025L),
			service_start = as.Date(c(rep("2012-01-01", 2), rep("2025-01-01", 2))),
			service_end = as.Date(c(rep("2012-12-31", 2), rep("2025-12-31", 2))),
			analysis_date = as.Date(c("2012-04-10", "2012-04-10", "2025-04-08", "2025-04-08")),
			source_audit_date = as.Date(c("2012-04-10", "2014-07-08", "2025-04-08", "2025-04-08")),
			deduplicate_stops = c(FALSE, TRUE, FALSE, FALSE),
			drop_shape_distances = TRUE,
			remove_rail = c(TRUE, FALSE, TRUE, FALSE),
			regularize_bus_times = c(TRUE, FALSE, TRUE, FALSE),
			# Change only these flags after inspecting the audit/reports.
			include_r5 = c(TRUE, FALSE, TRUE, FALSE)
		)
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
		name = reference_feed_paths,
		command = c(
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20150113.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20160614.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20160823.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20161221.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20170118.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20170215.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20170315.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20170417.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20170519.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20170615.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20170816.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20170920.zip",
			"data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20171016.zip"
		),
		format = "file"
	),
	tar_target(
		name = reference_feed_inventory,
		command = build_reference_feed_inventory(reference_feed_paths, bus_speed_spec)
	),
	tar_target(
		name = reference_feed_row,
		command = split_reference_feed_inventory(reference_feed_inventory),
		iteration = "list",
		format = "rds"
	),
	tar_target(
		name = hpm_segment_result,
		command = extract_hpm_bus_segments(reference_feed_row, bus_speed_spec),
		pattern = map(reference_feed_row),
		iteration = "list",
		format = "rds"
	),
	tar_target(
		name = hpm_segments,
		command = combine_hpm_segments(hpm_segment_result)
	),
	tar_target(
		name = feed_diagnostics,
		command = combine_feed_diagnostics(hpm_segment_result)
	),
	tar_target(
		name = reference_feed,
		command = reference_feed_inventory |>
			dplyr::filter(feed_id == bus_speed_spec$reference_feed_id)
	),
	tar_target(
		name = reference_busways,
		command = read_reference_busways(
			geosampa_path = raw_busway_paths[1],
			mobilidados_path = raw_busway_paths[2],
			date = bus_speed_spec$reference_date,
			projected_crs = bus_speed_spec$projected_crs
		),
		format = "rds"
	),
	tar_target(
		name = reference_segment_geometry,
		command = build_reference_segment_geometry(
			segments = hpm_segments,
			reference_feed = reference_feed,
			projected_crs = bus_speed_spec$projected_crs
		),
		format = "rds"
	),
	tar_target(
		name = segment_busway_matches,
		command = classify_reference_segments(
			segments_sf = reference_segment_geometry,
			busways = reference_busways,
			buffer_m = bus_speed_spec$buffer_m,
			minimum_overlap = bus_speed_spec$minimum_overlap
		)
	),
	tar_target(
		name = bus_speed_surface,
		command = estimate_bus_speed_surface(
			segments = hpm_segments,
			matches = segment_busway_matches,
			spec = bus_speed_spec
		)
	),
	tar_target(
		name = bus_speed_validation,
		command = validate_bus_speed_surface(
			surface = bus_speed_surface,
			inventory = reference_feed_inventory,
			diagnostics = feed_diagnostics,
			spec = bus_speed_spec
		)
	),
	tar_target(
		name = busway_buffer_diagnostics,
		command = measure_busway_buffers(
			segments_sf = reference_segment_geometry,
			busways = reference_busways,
			distances_m = bus_speed_spec$test_buffers_m,
			minimum_overlap = bus_speed_spec$minimum_overlap
		)
	),
	tar_target(
		name = bus_speed_diagnostic_figures,
		command = plot_bus_speed_diagnostics(
			segments = hpm_segments,
			surface = bus_speed_surface,
			busways = reference_busways,
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
		command = {
			bus_speed_validation
			write_bus_feeds(
				feed_paths = prepared_feeds,
				spec = feed_spec,
				speed_surface = bus_speed_surface,
				geosampa_busways = raw_busway_paths[1],
				mobilidados_busways = raw_busway_paths[2],
				output_dir = "data/gtfs/bus"
			)
		},
		format = "file"
	),
	tar_target(
		name = rail_feeds,
		command = write_scenario_rail_feed(
			prepared_feeds = prepared_feeds,
			feed_spec = feed_spec,
			routing_spec = routing_spec,
			service_spec = rail_service_spec,
			stop_corrections = rail_stop_corrections
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
			prepared_feeds = bus_feeds,
			additional_feeds = rail_feeds,
			r5_dir = "data/r5",
			output_subdir = "all"
		),
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
			dir = unique(dirname(r5_feeds)),
			feed_paths = r5_feeds,
			ram = r5_resources$ram,
			cpu = r5_resources$cpu
		),
		format = "file",
		deployment = "main"
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
		name = ttm_transit_all,
		command = calc_ttm(
			r5_network = r5_network,
			od_table = od_grid_all,
			mode = "TRANSIT",
			departure_datetime = routing_spec$datetime,
			max_duration = 120L,
			threads = r5_resources$cpu,
			ram = r5_resources$ram,
			java_cpu = r5_resources$cpu
		),
		pattern = map(routing_spec),
		deployment = "main"
	),

	## accessibility ---------------------------------------------------------------------------

	## individuals: cadunico + rais ------------------------------------------------------------
	tar_target(
		name = cadunico_ind,
		command = read_cad_individuals(
			year = time_window,
			families = cadunico_fam,
			save_dir = "data/temp"
		),
		pattern = map(time_window),
		format = "file",
		deployment = "worker"
	)
)

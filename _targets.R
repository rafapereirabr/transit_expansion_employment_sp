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

tar_source()

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
		name = routing_dates,
		command = setNames(
			as.Date(c("2012-04-10", "2025-04-08")),
			c("2012", "2025")
		),
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
		command = sf_from_parquet(munis_sf) |> st_buffer(1e3) |> h3_from_sf(),
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
		name = gtfs_raw_feeds,
		command = c(
			"data-raw/gtfs_sptrans_2012.zip",
			"data-raw/gtfs_emtu_2014.zip",
			"data-raw/gtfs_sptrans_2025.zip",
			"data-raw/gtfs_emtu_2025.zip"
		),
		format = "file"
	),
	tar_target(
		name = gtfs_spec,
		command = tibble::tibble(
			input = gtfs_raw_feeds,
			output_name = c(
				"gtfs_sptrans_2012.zip",
				"gtfs_emtu_2014_proxy_2012.zip",
				"gtfs_sptrans_2025.zip",
				"gtfs_emtu_2025.zip"
			),
			service_start = as.Date(c(
				"2012-01-01",
				"2012-01-01",
				"2025-01-01",
				"2025-01-01"
			)),
			service_end = as.Date(c(
				"2012-12-31",
				"2012-12-31",
				"2025-12-31",
				"2025-12-31"
			)),
			deduplicate_stops = c(FALSE, TRUE, FALSE, FALSE),
			drop_shape_distances = TRUE
		)
	),
	tar_target(
		name = prepared_gtfs_feeds,
		command = prepare_gtfs_feeds(
			gtfs_spec,
			output_dir = "data/gtfs/processed"
		),
		format = "file"
	),
	tar_target(
		name = gtfs_audit,
		command = audit_gtfs_feeds(
			feed_paths = prepared_gtfs_feeds,
			dates = routing_dates
		)
	),
	tar_target(
		name = gtfs_reports,
		command = validate_gtfs_feeds(
			feed_paths = prepared_gtfs_feeds,
			validator_dir = "data/gtfs_validator"
		),
		format = "file"
	),
	tar_target(
		name = gtfs_selection,
		command = {
			gtfs_audit
			gtfs_reports
			tibble::tibble(
				output_name = gtfs_spec$output_name,
				# Change only these flags after inspecting the audit/reports.
				include_r5 = c(TRUE, FALSE, TRUE, FALSE)
			)
		}
	),
	tar_target(
		name = r5_feeds,
		command = export_selected_gtfs(
			spec = gtfs_selection,
			prepared_feeds = prepared_gtfs_feeds,
			r5_dir = "data/r5"
		),
		format = "file"
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
		command = {
			r5_feeds
			gtfs_audit
			build_r5r_network(dir = "data/r5")
		},
		format = "file"
	),
	tar_target(
		name = ttm_walk_stations,
		command = calc_ttm(
			r5_network = r5_network,
			od_table = od_station_proximity,
			mode = "WALK",
			max_duration = 180L
		)
	),
	tar_target(
		name = ttm_transit_all,
		command = calc_ttm(
			r5_network = r5_network,
			od_table = od_grid_all,
			mode = "TRANSIT",
			departure_date = routing_dates,
			max_duration = 60L
		),
		pattern = map(routing_dates),
		deployment = "worker"
	),

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

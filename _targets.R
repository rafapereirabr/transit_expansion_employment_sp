# targets setup ------------------------------------------------------------------------------

options(
  arrow.pull_as_vector = FALSE,
  arrow.unsafe_metadata = TRUE,
  future.globals.maxSize = 1e4^1024,
  java.parameters = c(
    paste0("-Xmx", 96, "G"),
    paste0("-XX:ActiveProcessorCount=", 12)
  )
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
  controller = crew_controller_local(
    workers = floor(.6 * parallelly::freeCores()[1])
  ),
  storage = "worker",
  retrieval = "worker",
  trust_timestamps = TRUE,
  workspace_on_error = TRUE
)

tar_source()

if (!dir.exists("data")) {
  dir.create("data")
}


# targets list -------------------------------------------------------------------------------

list(
  ## parameters -------------------------------------------------------------------------------
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
    format = "rds",
    deployment = "main"
  ),
  tar_target(
    name = time_window,
    command = c(2012:2025),
    format = "rds",
    deployment = "main"
  ),
  tar_target(
    name = cutoff_years,
    command = range(time_window),
    format = "rds",
    deployment = "main"
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
    format = "file",
    deployment = "main"
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
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = munis_sf,
    command = get_munis(save_path = "data/munis_sf.parquet", overwrite = T),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = grid_sf,
    command = sf_from_parquet(munis_sf) |> st_buffer(1e3) |> h3_from_sf(),
    format = "rds",
    deployment = "main"
  ),
  tar_target(
    name = footprint_sf,
    command = get_footprint(
      save_path = "data/footprint_sf.parquet",
      munis_path = munis_sf
    ),
    format = "file",
    deployment = "main"
  ),

  ## plots ------------------------------------------------------------------------------------
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
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = openings_plot,
    command = plot_openings(
      stations_sf = stations_sf,
      metro_palette = metro_palette,
      plot_type = "bars",
      save_path = "figures/fig_openings.png"
    ),
    format = "file",
    deployment = "main"
  ),

  ## routing --------------------------------------------------------------------------------
  tar_target(
    name = od_table_stations,
    command = set_od_table_stations(
      origins = cadunico_fam,
      destinations = stations_sf,
      origin_filter = grid_sf
    ),
    format = "rds",
    deployment = "main"
  ),
  tar_target(
    name = od_table_grid,
    command = set_od_table_h3(grid = grid_sf),
    format = "rds",
    deployment = "main"
  ),
  tar_target(
    name = r5_network,
    command = build_r5r_network(dir = "data/r5"),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = feeds,
    command = unpack_feeds("data-raw/feeds_sptrans.zip", "data/r5"),
    deployment = "main",
    format = "file"
  ),
  tar_target(
    name = ttm_walk_stations,
    command = calc_ttm(
      r5_network = r5_network,
      od_table = od_table_stations,
      mode = "WALK",
      max_duration = 180L
    ),
    deployment = "main"
  ),
  tar_target(
    name = ttm_transit,
    command = calc_ttm(
      r5_network = r5_network,
      od_table = od_table_grid,
      mode = "TRANSIT",
      year = cutoff_years,
      max_duration = 60L
    ),
    pattern = map(cutoff_years),
    deployment = "main"
  ),

  ## cadunico and rais data -----------------------------------------------------------------
  tar_target(
    name = cadunico_fam,
    command = read_cad_families(
      years = time_window,
      munis = munis_sf,
      save_dir = "data/temp"
    ),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = cadunico_ind,
    command = read_cad_individuals(
      year = time_window,
      families = cadunico_fam,
      save_dir = "data/temp"
    ),
    pattern = map(time_window),
    format = "file"
  )
)

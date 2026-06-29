
suppressPackageStartupMessages(
  {
    library(targets)
    # library(tarchetypes)
    library(dplyr)
    library(docstring)
    library(geoarrow)
  }
)
# Set target options:
tar_option_set(
  packages = c("arrow", "dplyr", "docstring", "duckspatial", "sf", "geoarrow", "ggplot2"),
  format = "parquet",
  workspace_on_error = T
)

tar_source()

if(!dir.exists("data")) dir.create("data")

list(

  ## parameters -------------------------------------------------------------------------------
  tar_target(
    name = metro_palette,
    command = c(
      `Metro L1` = "#0153a0",  `Metro L2` = "#008061",  `Metro L3` = "#ee3e34",  `Metro L4` = "#fed304",
      `Metro L5` = "#784d9f",  `Metro L6` = "#f27800",  `Train L7` = "#9e1766",  `Train L8` = "#9e9e93",
      `Train L9` = "#00a78e", `Train L10` = "#007c8f", `Train L11` = "#f04d22", `Train L12` = "#083e89",
      `Train L13` = "#00ab5b", `Train L14` = "#1C1C1C", `Metro L15` = "#858d90", `Metro L16` = "#562283",
      `Metro L17` = "#bf9001", `Metro L18` = "#8C7853", `Metro L19` = "#21B4E2", `Metro L20` = "#e63271",
      `Metro L22` = "#764A30", `Train L24` = "#F7CAC9"
    ),
    format = "rds", deployment = "main"
  ),
  # tar_target(
  #   name = intervention_window, command = c(2014:2019),
  #   format = "rds", deployment = "main"
  # ),

  ## shapefiles -------------------------------------------------------------------------------
  tar_target(
    name = stations_sf,
    command = tidy_transit_stations(
      subway = "data-raw/subway_stations.gpkg",
      planned_subway = "data-raw/planned_subway_stations.gpkg",
      train = "data-raw/train_stations.gpkg", planned_train = "data-raw/planned_train_stations.gpkg",
      save_path = "data/transit_stations.parquet", overwrite = T
    ),
    format = "file", deployment = "main"
  ),
  tar_target(
    name = lines_sf,
    command = tidy_transit_lines(
      stations_path = stations_sf, subway = "data-raw/subway_lines.gpkg",
      planned_subway = "data-raw/planned_subway_lines.gpkg", train = "data-raw/train_lines.gpkg",
      planned_train = "data-raw/planned_train_lines.gpkg",
      save_path = "data/transit_lines.parquet", overwrite = T
    ),
    format = "file", deployment = "main"
  ),
  tar_target(
    name = munis_sf,
    command = get_munis(save_path = "data/munis_sf.parquet"),
    format = "file", deployment = "main"
  ),
  tar_target(
    name = footprint_sf,
    command = get_footprint(save_path = "data/footprint_sf.parquet", munis_path = munis_sf),
    format = "file", deployment = "main"
  ),

  ## plots ------------------------------------------------------------------------------------
  tar_target(
    name = transit_map,
    command = plot_lines(footprint_sf = footprint_sf, munis_sf = munis_sf, lines_sf = lines_sf,
                         stations_sf = stations_sf, metro_palette = metro_palette,
                         save_path = "figures/fig_map.png"),
    format = "file", deployment = "main"
  ),
  tar_target(
    name = openings_plot,
    command = plot_openings(stations_sf = stations_sf, metro_palette = metro_palette,
                            plot_type = "bars", save_path = "figures/fig_openings.png"),
    format = "file", deployment = "main"
  )
)

# Setup --------------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(duckspatial)
  library(ggplot2)
  library(gtfstools)
  library(h3o)
  library(purrr)
  library(sf)
  library(tibble)
})

source("R/gtfs.R")

output_dir <- "sidequests/check_busways_output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

geosampa_path <- "data-raw/busways.gpkg"
mobilidados_path <- "data-raw/mobilidados_2025.zip"
segments_path <- "sidequests/bus_speed_surface_output/bus_hpm_segments.parquet"
reference_feed <- "data-raw/gtfs_history/gtfs_sao_paulo_sptrans_20150113.zip"
reference_feed_id <- "gtfs_sao_paulo_sptrans_20150113"
reference_date <- as.Date("2015-01-13")
test_buffers_m <- c(15, 25, 40)
minimum_overlap <- 0.60
projected_crs <- 31983

h3_as_sf <- function(data) {
  sf::st_sf(
    data,
    geometry = data$h3 |>
      h3o::h3_from_strings() |>
      sf::st_as_sfc(),
    crs = 4326
  )
}

# Infrastructure ----------------------------------------------------------------------------

read_geosampa_busways <- function(path, date) {
  result <- sf::st_read(path, quiet = TRUE) |>
    dplyr::filter(
      !is.na(dt_implantacao_faixa),
      dt_implantacao_faixa <= date,
      nm_tipo_faixa_corredor != "TEMPORARIA"
    ) |>
    dplyr::transmute(
      source = "GeoSampa",
      corridor = nm_denominacao_faixa_corredor,
      segment = nm_logradouro_faixa_corredor,
      segregation = dplyr::case_when(
        nm_tipo_faixa_corredor == "EXCLUSIVA" ~ "segregated_lane",
        TRUE ~ "priority_lane"
      ),
      opening_date = dt_implantacao_faixa
    ) |>
    sf::st_transform(projected_crs)
  names(result)[names(result) == attr(result, "sf_column")] <- "geometry"
  sf::st_geometry(result) <- "geometry"
  result
}


read_mobilidados_busways <- function(path, date) {
  layer <- paste0("/vsizip/", normalizePath(path), "/BRT_Corredores.shp")
  result <- sf::st_read(layer, quiet = TRUE) |>
    dplyr::filter(
      Cidade_n == "São Paulo",
      `Situação` == "Operacional",
      is.finite(Ano),
      Ano >= 1900,
      Ano <= as.integer(format(date, "%Y"))
    ) |>
    dplyr::transmute(
      source = "MobiliDADOS",
      corridor = Corredor,
      segment = Segmento,
      segregation = dplyr::case_when(
        Segregacao %in% c("Física", "Exclusiva") ~ "fully_segregated",
        Segregacao == "Visual" ~ "priority_lane",
        TRUE ~ NA_character_
      ),
      opening_date = as.Date(paste0(as.integer(Ano), "-12-31"))
    ) |>
    sf::st_transform(projected_crs)
  result <- result |> dplyr::filter(!is.na(segregation))
  names(result)[names(result) == attr(result, "sf_column")] <- "geometry"
  sf::st_geometry(result) <- "geometry"
  result
}


busways <- dplyr::bind_rows(
  read_geosampa_busways(geosampa_path, reference_date),
  read_mobilidados_busways(mobilidados_path, reference_date)
)

# GTFS segments ------------------------------------------------------------------------------

segment_index <- arrow::read_parquet(segments_path) |>
  dplyr::filter(feed_id == reference_feed_id) |>
  dplyr::distinct(stop_id, next_stop_id) |>
  dplyr::mutate(segment_id = dplyr::row_number())

stops <- gtfstools::read_gtfs(reference_feed, files = "stops.txt")$stops |>
  dplyr::select(stop_id, stop_name, stop_lon, stop_lat)

segments_sf <- segment_index |>
  dplyr::left_join(
    stops |> dplyr::rename(from_name = stop_name, from_lon = stop_lon, from_lat = stop_lat),
    by = "stop_id"
  ) |>
  dplyr::left_join(
    stops |> dplyr::rename(to_name = stop_name, to_lon = stop_lon, to_lat = stop_lat),
    by = c("next_stop_id" = "stop_id")
  ) |>
  dplyr::filter(
    is.finite(from_lon), is.finite(from_lat),
    is.finite(to_lon), is.finite(to_lat)
  ) |>
  dplyr::mutate(
    geometry = purrr::pmap(
      list(from_lon, from_lat, to_lon, to_lat),
      \(x1, y1, x2, y2) sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
    ) |> sf::st_sfc(crs = 4326)
  ) |>
  sf::st_as_sf() |>
  sf::st_transform(projected_crs)

# DuckSpatial overlap ------------------------------------------------------------------------

measure_buffer <- function(distance_m) {
  conn <- duckspatial::ddbs_create_conn()
  on.exit(duckspatial::ddbs_stop_conn(conn))
  duckspatial::ddbs_register_table(conn, segments_sf, "segments", overwrite = TRUE)
  duckspatial::ddbs_register_table(conn, busways, "busways", overwrite = TRUE)

  DBI::dbExecute(conn, sprintf(
    paste(
      "CREATE TEMP TABLE busway_buffers AS",
      "SELECT segregation, ST_Union_Agg(ST_Buffer(geometry, %1$f)) AS geometry",
      "FROM busways GROUP BY segregation"
    ),
    distance_m
  ))

  sql <- paste(
    "SELECT s.segment_id,",
    "MAX(ST_Length(ST_Intersection(s.geometry, b.geometry)) /",
    "NULLIF(ST_Length(s.geometry), 0)) AS overlap_share",
    "FROM segments s JOIN busway_buffers b",
    "ON ST_Intersects(s.geometry, b.geometry)",
    "GROUP BY s.segment_id"
  )
  overlap <- DBI::dbGetQuery(conn, sql)
  tibble::tibble(
    buffer_m = distance_m,
    segments = nrow(segments_sf),
    any_overlap = nrow(overlap),
    matched_60pct = sum(overlap$overlap_share >= minimum_overlap),
    matched_80pct = sum(overlap$overlap_share >= 0.80),
    median_overlap = stats::median(overlap$overlap_share)
  )
}

buffer_diagnostics <- purrr::map_dfr(test_buffers_m, measure_buffer)
utils::write.csv(
  buffer_diagnostics,
  file.path(output_dir, "buffer_diagnostics.csv"),
  row.names = FALSE
)

# Selected matches and empirical speed factors ------------------------------------------------

classify_segments <- function(distance_m = 25, minimum_share = 0.60) {
  conn <- duckspatial::ddbs_create_conn()
  on.exit(duckspatial::ddbs_stop_conn(conn))
  duckspatial::ddbs_register_table(conn, segments_sf, "segments", overwrite = TRUE)
  duckspatial::ddbs_register_table(conn, busways, "busways", overwrite = TRUE)
  DBI::dbExecute(conn, sprintf(
    paste(
      "CREATE TEMP TABLE busway_buffers AS",
      "SELECT segregation, ST_Union_Agg(ST_Buffer(geometry, %f)) AS geometry",
      "FROM busways GROUP BY segregation"
    ),
    distance_m
  ))
  DBI::dbGetQuery(conn, sprintf(
    paste(
      "WITH candidates AS (",
      "SELECT s.segment_id, b.segregation,",
      "ST_Length(ST_Intersection(s.geometry, b.geometry)) /",
      "NULLIF(ST_Length(s.geometry), 0) AS overlap_share",
      "FROM segments s JOIN busway_buffers b",
      "ON ST_Intersects(s.geometry, b.geometry)",
      "), ranked AS (",
      "SELECT *, ROW_NUMBER() OVER (PARTITION BY segment_id ORDER BY",
      "overlap_share DESC, CASE segregation",
      "WHEN 'fully_segregated' THEN 1 WHEN 'segregated_lane' THEN 2 ELSE 3 END) AS rank",
      "FROM candidates WHERE overlap_share >= %f)",
      "SELECT segment_id, segregation, overlap_share FROM ranked WHERE rank = 1"
    ),
    minimum_share
  )) |>
    tibble::as_tibble()
}

selected_matches <- classify_segments()
selected_matches <- selected_matches |>
  dplyr::left_join(
    segment_index |> dplyr::select(segment_id, stop_id, next_stop_id),
    by = "segment_id"
  )
reference_speeds <- arrow::read_parquet(segments_path) |>
  dplyr::filter(feed_id == reference_feed_id) |>
  dplyr::left_join(
    segment_index |> dplyr::select(stop_id, next_stop_id, segment_id),
    by = c("stop_id", "next_stop_id")
  ) |>
  dplyr::left_join(selected_matches, by = "segment_id") |>
  dplyr::mutate(segregation = dplyr::coalesce(segregation, "mixed_traffic"))

speed_factors <- reference_speeds |>
  dplyr::summarise(
    speed_kmh = stats::median(speed_kmh),
    segments = dplyr::n(),
    routes = dplyr::n_distinct(route_id),
    .by = segregation
  ) |>
  dplyr::mutate(
    baseline_speed = speed_kmh[segregation == "mixed_traffic"],
    speed_factor = speed_kmh / baseline_speed
  )

utils::write.csv(
  selected_matches,
  file.path(output_dir, "segment_busway_matches_2015.csv"),
  row.names = FALSE
)

# Within-H3 comparison across 2015-2017 -------------------------------------------------------

all_reference_speeds <- arrow::read_parquet(segments_path) |>
  dplyr::filter(year %in% 2015:2017) |>
  dplyr::left_join(
    selected_matches |>
      dplyr::select(stop_id, next_stop_id, segregation) |>
      dplyr::distinct(),
    by = c("stop_id", "next_stop_id")
  ) |>
  dplyr::mutate(segregation = dplyr::coalesce(segregation, "mixed_traffic"))

snapshot_h3 <- all_reference_speeds |>
  dplyr::summarise(
    speed_kmh = stats::median(speed_kmh),
    observations = dplyr::n(),
    routes = dplyr::n_distinct(route_id),
    .by = c(feed_id, year, h3, segregation)
  ) |>
  dplyr::filter(observations >= 5)

baseline_h3 <- snapshot_h3 |>
  dplyr::filter(segregation == "mixed_traffic") |>
  dplyr::select(feed_id, year, h3, baseline_speed = speed_kmh)

ratio_h3_snapshot <- snapshot_h3 |>
  dplyr::filter(segregation != "mixed_traffic") |>
  dplyr::inner_join(baseline_h3, by = c("feed_id", "year", "h3")) |>
  dplyr::mutate(speed_factor = speed_kmh / baseline_speed)

ratio_h3_year <- ratio_h3_snapshot |>
  dplyr::summarise(
    speed_factor = stats::median(speed_factor),
    observations = sum(observations),
    snapshots = dplyr::n(),
    .by = c(year, h3, segregation)
  )

global_factors <- ratio_h3_year |>
  dplyr::summarise(
    global_factor = stats::median(speed_factor),
    h3_years = dplyr::n(),
    .by = segregation
  )

# Conditional speed surface used by the synthetic-feed model ---------------------------------

conditional_snapshot <- all_reference_speeds |>
  dplyr::summarise(
    speed_kmh = stats::median(speed_kmh),
    observations = dplyr::n(),
    routes = dplyr::n_distinct(route_id),
    .by = c(feed_id, year, h3, segregation)
  )

conditional_year <- conditional_snapshot |>
  dplyr::summarise(
    speed_kmh = stats::median(speed_kmh),
    observations = sum(observations),
    snapshots = dplyr::n(),
    .by = c(year, h3, segregation)
  )

conditional_global <- conditional_year |>
  dplyr::summarise(
    class_speed_kmh = stats::median(speed_kmh),
    h3_years = dplyr::n(),
    .by = segregation
  )

conditional_surface <- conditional_year |>
  dplyr::summarise(
    raw_speed_kmh = stats::median(speed_kmh),
    observations = sum(observations),
    years = dplyr::n_distinct(year),
    .by = c(h3, segregation)
  ) |>
  dplyr::left_join(conditional_global, by = "segregation") |>
  dplyr::mutate(
    local_weight = observations / (observations + 50) * pmin(years / 3, 1),
    speed_kmh = local_weight * raw_speed_kmh +
      (1 - local_weight) * class_speed_kmh
  )

utils::write.csv(
  conditional_surface,
  file.path(output_dir, "conditional_speed_surface_h3_8.csv"),
  row.names = FALSE
)
utils::write.csv(
  conditional_global,
  file.path(output_dir, "conditional_speed_fallbacks.csv"),
  row.names = FALSE
)

conditional_map <- conditional_surface |>
  dplyr::filter(years >= 2) |>
  h3_as_sf() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = speed_kmh), color = NA) +
  ggplot2::facet_wrap(ggplot2::vars(segregation)) +
  ggplot2::scale_fill_viridis_c(option = "magma", direction = -1) +
  ggplot2::labs(
    title = "Conditional scheduled bus speeds",
    subtitle = "H3-8 and infrastructure class; balanced 2015-2017 medians",
    fill = "km/h"
  ) +
  ggplot2::theme_void()

ggplot2::ggsave(
  file.path(output_dir, "conditional_speed_surface_h3_8.png"),
  conditional_map,
  width = 12,
  height = 7,
  dpi = 250,
  bg = "white"
)

ratio_h3 <- ratio_h3_year |>
  dplyr::summarise(
    raw_factor = stats::median(speed_factor),
    observations = sum(observations),
    years = dplyr::n_distinct(year),
    .by = c(h3, segregation)
  ) |>
  dplyr::left_join(global_factors, by = "segregation") |>
  dplyr::mutate(
    local_weight = observations / (observations + 50) * pmin(years / 3, 1),
    speed_factor = exp(
      local_weight * log(raw_factor) + (1 - local_weight) * log(global_factor)
    )
  )

utils::write.csv(
  ratio_h3,
  file.path(output_dir, "busway_speed_factors_h3_2015_2017.csv"),
  row.names = FALSE
)
utils::write.csv(
  global_factors,
  file.path(output_dir, "busway_speed_factors_balanced_2015_2017.csv"),
  row.names = FALSE
)

ratio_map <- ratio_h3 |>
  dplyr::filter(years >= 2) |>
  h3_as_sf() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = speed_factor), color = NA) +
  ggplot2::facet_wrap(ggplot2::vars(segregation)) +
  ggplot2::scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 1
  ) +
  ggplot2::labs(
    title = "Bus-priority speed differential within H3-8 cells",
    subtitle = "2015-2017 medians with shrinkage toward the class-wide factor",
    fill = "speed\nfactor"
  ) +
  ggplot2::theme_void()

ggplot2::ggsave(
  file.path(output_dir, "busway_speed_factors_h3.png"),
  ratio_map,
  width = 11,
  height = 6,
  dpi = 250,
  bg = "white"
)
utils::write.csv(
  speed_factors,
  file.path(output_dir, "busway_speed_factors_2015.csv"),
  row.names = FALSE
)

# Infrastructure map -------------------------------------------------------------------------

busway_map <- ggplot2::ggplot(busways) +
  ggplot2::geom_sf(ggplot2::aes(color = segregation), linewidth = 0.35) +
  ggplot2::scale_color_manual(
    values = c(
      fully_segregated = "#7A0177",
      segregated_lane = "#D7301F",
      priority_lane = "#2C7FB8"
    )
  ) +
  ggplot2::labs(
    title = "Bus priority infrastructure active by 2015",
    subtitle = "GeoSampa, supplemented with operational MobiliDADOS BRT segments",
    color = NULL
  ) +
  ggplot2::theme_void()

ggplot2::ggsave(
  file.path(output_dir, "busways_2015.png"),
  busway_map,
  width = 8,
  height = 8,
  dpi = 250,
  bg = "white"
)

print(buffer_diagnostics)
print(speed_factors)
print(global_factors)

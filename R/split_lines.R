# helper functions to sync stops and shapes

# split_lines ---------------------------------------------------------------------------------

split_lines <- function(lines, points, snap_tol = 10, tmp_crs = NULL) {

#' split a linestring 
  
#' @param lines A `data.frame` with class `sf` with the `LINESTRING`s to be splited
#' @param points A `data.frame` with class `sf` with the `POINT`s to split `lines`
#' @param snap_tol Snap tolerance in meters
#' @param temp_crs A temporary CRS to use for the operations
#' @description
#' `split_lines()` Splits linestrings between two or more points.
#' @details
#'  `snap_tol` guarantees that ponts are located along the lines. However, this operation requires
#'  a UTM coordinate reference system, hence `tmp_crs` allows a temporary conversion.
#' @returns a `data.frame` with class `sf`
#' @export

  if(!is.null(tmp_crs)) {
    old_crs <- sf::st_crs(lines)
    lines <- sf::st_transform(lines, crs = tmp_crs)
    points <- sf::st_transform(points, crs = tmp_crs)
  }
  
  lines <- sf::st_snap(lines, points, snap_tol) |> 
    sf::st_cast("MULTILINESTRING")
  lines <- lwgeom::st_split(lines, points)
  lines <- lines |> sf::st_collection_extract("LINESTRING") |> 
    sf::st_cast("LINESTRING")
  
  if(!is.null(tmp_crs)) {
    lines <- lines |> 
      sf::st_transform(crs = old_crs)
  }
  
  return(lines)
}



# find_endpoints ------------------------------------------------------------------------------

find_endpoints <- function(lines, points, by = NULL, reverse = FALSE) {
  
#' Identify endpoints in a line segment

#' @param lines A `data.frame` with class `sf` with the `LINESTRING`s to be splited
#' @param points A `data.frame` with class `sf` with the `POINT`s to split `lines`
#' @param point_name A character indicating the name column in `points` (optional)
#' @param other_name A character indicating another column in `points` (optional)
#' @param reverse Should start and endpoints be reversed? Defautls to FALSE.
#' @description
#' `find_endpoints()` determines the feature in `points` that is closest to each segment in `lines`.
#' @details
#'  Finding the nearest feature is a good, but not perfect proxy, since it not necessarily lies 
#'  in the line. Therefore, providing either `point_name` or `point_order` is highly recommended  
#'  both for usability and for identifying false positives downstream.
#' @returns a `data.frame` with class `sf`
#' @export

  # if(!is.null(other_name)) {point_order <- rlang::sym(other_name)}
  
  if(reverse) {
    lines <- lines |> 
      dplyr::mutate(from = lwgeom::st_endpoint(geometry), to = lwgeom::st_startpoint(geometry)) 
  } else {
    lines <- lines |> 
      dplyr::mutate(from = lwgeom::st_startpoint(geometry), to = lwgeom::st_endpoint(geometry)) 
  } 
  
  if(!is.null(by)) {
    for(i in 1:length(by)) {
      by_sym <- rlang::sym(by[i])
      df <- points |> dplyr::select(x = {{by_sym}})
      lines <- lines |>
        dplyr::mutate(
          !!paste0("from_", by[i]) := df$x[st_nearest_feature(from, df)],
          !!paste0("to_", by[i]) := df$x[st_nearest_feature(to, df)]
        )
      rm(df)
    }
  }
  
  return(lines)
}



# segment_lines -------------------------------------------------------------------------------

# library(sf)
# tar_load(c(lines_sf, stations_sf))

segment_lines <- function(lines, points, snap_tol) {
  if(is.character(lines)) {lines <- sf_from_parquet(lines)}
  if(is.character(points)) {points <- sf_from_parquet(points)}
  
  cols_lines <- setdiff(names(lines), "geometry") |> syms()
  
  lines <- lines |> 
    dplyr::group_by(!!!cols_lines) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
  
  lines_gp <- lines |> 
    dplyr::group_by(code_line) |> 
    dplyr::group_split()
  
  points_gp <- points |> 
    dplyr::group_by(code_line) |> 
    dplyr::group_split()
  
  lines_segmented <- purrr::map2(
    lines_gp, points_gp, function(x, y) {
      df <- split_lines(x, y, snap_tol = snap_tol) 
      df <- find_endpoints(df, y, by = c("yr_open", "infill", "name_station", "running"))
      return(df)
    }
  ) |> 
    dplyr::bind_rows()

  lines_segmented <- lines_segmented |> 
    mutate(infill = from_infill + to_infill) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      yr_open = if_else(infill > 0, min(from_yr_open, to_yr_open), max(from_yr_open, to_yr_open)),
      running = if_else(infill > 0, min(from_running, to_running), max(from_running, to_running)),
    ) |> 
    dplyr::select(-c(from, to, from_yr_open, to_yr_open, from_infill, to_infill, from_running, to_running)) |> 
    dplyr::rename(from = from_name_station, to = to_name_station) |> 
    dplyr::relocate(geometry, .after = everything())
  
  return(lines_segmented)
}

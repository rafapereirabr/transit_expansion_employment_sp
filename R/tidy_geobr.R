
# get_munis -----------------------------------------------------------------------------------

get_munis <- function(save_path = NULL, overwrite = FALSE) {
  munis_sf <- geobr::read_metro_area(2024, 35)
  munis_sf <- munis_sf |>
    filter(type == "RM de São Paulo (SP)")

  obj <- write_parquet_target(munis_sf, save_path, overwrite)
  return(obj)
}



# get_footprint -------------------------------------------------------------------------------

get_footprint <- function(save_path = NULL, overwrite = FALSE, munis_path = NULL) {
  footprint_sf <- geobr::read_urban_area(2019, code_muni = 35)

  if(!is.null(munis_path)) {
    munis_sf <- sf_from_parquet(munis_path)
    footprint_sf <- ddbs_filter(footprint_sf, munis_sf) |>
      st_as_sf()
  }

  obj <- write_parquet_target(footprint_sf, save_path, overwrite)
  return(obj)
}



# test ----------------------------------------------------------------------------------------

# library(dplyr)
# library(duckspatial)
# library(ggplot2)
# library(sf)

# munis_sf <- get_munis()

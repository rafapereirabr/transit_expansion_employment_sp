plot_lines <- function(footprint_sf, munis_sf, lines_sf, stations_sf, metro_palette, save_path) {
  ## read data --------------------------------------------------------------------------------
  
  footprint_sf <- sf_from_parquet(footprint_sf) |> 
    st_transform(crs = 31983)
  
  munis_sf <- sf_from_parquet(munis_sf) |> 
    filter(code_muni == 3550308) |> 
    st_transform(crs = 31983)
  
  cbd <- read_sf("data-raw/downtown.gpkg", crs = 31983)
  
  lines_sf <- sf_from_parquet(lines_sf)
  stations_sf <- sf_from_parquet(stations_sf)
  
  
  ## base map - city limits and stuff ---------------------------------------------------------
  metro_bbox <- stations_sf |> 
    filter(
      !(code_line %in% c(7,11)) | 
        name_station %in% c("CAIEIRAS")
    )
  
  pal <- metro_palette
  pal <- c("Pre-2013" = "gray75", pal)
  # pal <- factor(pal)
  
  base_map <- ggplot() + 
    geom_sf(data = footprint_sf, color = NA, fill = "#f2eddc") +
    geom_sf(
      data = munis_sf,
      aes(linetype = "Municipal borders"),
      color = "gray35",
      fill = NA,
      linewidth = 0.375
    ) +
    geom_sf(data = cbd, aes(linetype = "Expanded Downtown"), color = "red", fill = NA) +
    scale_linetype_manual(
      values = c("Municipal borders" = "dotted", "Expanded Downtown" = "dotdash", 
                 "Under Construction" = "dashed")
    )
  
  
  ## interventions
  transit_map <- base_map +
    geom_sf(
      data = filter(lines_sf, era == "Pre-2013"),
      aes(color = era),
      linewidth = 0.375,
      key_glyph = draw_key_point
    ) +
    geom_sf(
      data = filter(lines_sf, status == "Under Construction"),
      aes(linetype = status), color = "gray5", alpha = 0.75,
      linewidth = 0.5
    ) +
    geom_sf(
      data = filter(lines_sf, era == "2013-2025"),
      aes(color = label_line),
      linewidth = 0.75,
      key_glyph = draw_key_point
    ) +
    geom_sf(
      data = filter(stations_sf, era == "2013-2025"),
      aes(color = label_line),
      stroke = 0,
      size = 2,
      key_glyph = draw_key_point
    ) +
    geom_sf(
      data = filter(stations_sf, era == "2013-2025"),
      color = "white",
      stroke = 0,
      size = 1.5,
      key_glyph = draw_key_point
    )
  
  
  final_map <- transit_map +
    labs(color = "Intervention", linetype = "") + 
    spatialops::geom_bbox(metro_bbox) +
    scale_color_manual(values = pal) +
    guides(
      color = guide_legend(
        order = 1, nrow = 2,
        override.aes = list(size = 3, stroke = 0, linewidth = 0)
      ), 
      linetype = guide_legend(order = 2, nrow = 2)#,
    ) +
    spatialops::theme_abnq_map(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.title.position = "top",
      legend.box = "horizontal"
    )
  
  # final_map
  
  ggsave(save_path, plot = final_map, dpi = 600, width = 16, height = 13.5, un = "cm", bg = "white")
  
  return(save_path)
}


# test ----------------------------------------------------------------------------------------

# library(ggplot2)
# library(sf)
# 
# tar_load(c(footprint_sf, munis_sf, lines_sf, stations_sf, metro_palette))
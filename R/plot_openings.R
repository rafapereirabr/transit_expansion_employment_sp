plot_openings <- function(stations_sf, metro_palette, plot_type = c("acum", "stair", "bars"),
                          save_path) {
  plot_type <- rlang::arg_match(plot_type)
  dates <- sf_from_parquet(stations_sf, drop_geometry = T) |>
    filter(yr_open > 0) #|>
    # mutate(label_line = paste0(if_else(code_line %in% c(1:6,15:23), "Metro L", "Train L"), code_line))

  if(plot_type == "acum") {
    dates <- dates |>
      filter(name_line %in% c("AMARELA", "DIAMANTE", "JADE", "LILAS", "ESMERALDA", "PRATA", "RUBI")) |>
      group_by(label_line) |>
      count(yr_open) |>
      # mutate(code_line = factor(code_line, levels = as.character(1:24))) |>
      # mutate(acum = cumsum(n)) |> #, first = min(yr_open), last = max(yr_open)) |>
      rename(year = yr_open) |>
      tidyr::complete(year = seq(2006, 2025), fill = list(n = 0)) |>
      arrange(year) |>
      mutate(acum = cumsum(n))

    plot <- dates |>
      filter(between(year, 2009, 2025)) |>
      # View()
      ggplot(aes(x = year, y = acum, color = label_line)) +
      geom_line() +
      geom_point(size = 1.75, color = "black") +
      geom_point(aes(color = label_line), stroke = 0) +
      scale_color_manual(values = metro_palette) +
      theme_minimal()
  }

  if(plot_type == "stair") {
    plot <- dates |>
      filter(between(yr_open, 2009, 2025)) |>
      arrange(dt_full_svc) |>
      tibble::rowid_to_column("acum") |>
      ggplot() +
      geom_line(aes(x = dt_full_svc, y = acum)) +
      geom_point(aes(x = dt_full_svc, y = acum, color = factor(label_line))) +
      scale_color_manual(values = metro_palette) +
      theme_minimal()
  }


  if(plot_type == "bars") {
    plot <- dates |>
      filter(between(yr_open, 2000, 2026)) |>
      ggplot() +
      geom_bar(aes(x = yr_open, fill = label_line)) +
      scale_fill_manual(values = metro_palette) +
      scale_x_continuous(breaks = seq(2000, 2024, 2)) +
      labs(x = "Opening Year", y = "Number of Stations", fill = "Line") +
      theme_minimal()
  }

  ggsave(save_path, plot = plot, dpi = 600, width = 16, height = 10, un = "cm", bg = "white")

  return(save_path)
}

# library(ggplot2)
# tar_load(stations_sf)
# tar_load(metro_palette)
#
# # writexl::write_xlsx(sf_from_parquet(stations_sf, drop_geometry = T),
# #                     "data-raw/station_openings.xlsx")
# #
# # dates <- readxl::read_xlsx("data/station_openings.xlsx", sheet = "data")

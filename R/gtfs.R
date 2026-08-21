# unpack_feeds -------------------------------------------------------------------------------

# feeds
# feeds <- "data-raw/feeds_sptrans.zip"
# r5_dir <- "data/r5"

unpack_feeds <- function(feeds, r5_dir) {
  td <- tempdir()

  zip_files <- zip::zip_list(feeds)

  zip_files <- zip_files |>
    filter(stringr::str_detect(filename, "2012|2025")) |>
    pull(filename)

  zip::unzip(feeds, zip_files, exdir = td)

  temp_paths <- list.files(
    td,
    pattern = "google_transit",
    recursive = T,
    full.names = T
  )
  new_paths <- file.path(
    r5_dir,
    c("gtfs_sptrans_2012.zip", "gtfs_sptrans_2025.zip")
  )

  file.copy(temp_paths, new_paths)

  return(new_paths)
}

# validate_feeds -----------------------------------------------------------------------------

# feed_paths <- c(
#   "data/r5/gtfs_sptrans_2025.zip",
#   "data/r5/gtfs_sptrans_2012.zip"
# )
# validator_dir <- "data/gtfs_validator"

# validate_feeds <- function(feed_paths, validator_dir) {
#   if (!dir.exists(validator_dir)) {
#     dir.create(validator_dir)
#   }
#   validator_path <- list.files(validator_dir, pattern = "jar$", full.names = T)[
#     1
#   ]
#   if (length(validator_path) == 0) {
#     gtfstools::download_validator(validator_dir)
#     validator_path <- list.files(
#       validator_dir,
#       pattern = "jar$",
#       full.names = T
#     )[1]
#   }

#   reports <- purrr::map(
#     feed_paths,
#     function(x) {
#       gtfstools::validate_gtfs(
#         x,
#         output_path = validator_dir,
#         validator = validator_path
#       )
#       html_old <- file.path(validator_dir, "report.html")
#       html_new <- file.path(
#         validator_dir,
#         paste0("report_", stringr::str_remove(basename(x), "\\.zip$"), ".html")
#       )
#       file.copy(html_old, html_new)
#       return(html_new)
#     }
#   )

#   return(reports)
# }

# library(purrr)
# library(gtfstools)

# feeds <- map(feed_paths, read_gtfs)
# names(feeds) <- c("sptrans_2012", "sptrans_2025")
# routes <- map(feeds, ~ pluck(.x, "routes")) |>
#   bind_rows(.id = "feed") |>
#   select(feed, route_type, route_id)
# trips <- map(feeds, ~ pluck(.x, "trips")) |>
#   bind_rows(.id = "feed") |>
#   select(feed, route_id, trip_id)
# speeds <- map(feeds, get_trip_speed) |> bind_rows(.id = "feed")

# full <- full_join(speeds, trips) |>
#   full_join(routes)

# full |>
#   mutate(active = if_else(is.na(trip_id), 0, 1)) |>
#   summarise(
#     n_routes = n_distinct(route_id),
#     n_trips = n_distinct(trip_id),
#     n_trips_active = sum(!is.na(trip_id)),
#     avg_speed = mean(speed, na.rm = T),
#     speed_q1 = quantile(speed, 0.25, na.rm = T),
#     speed_q2 = quantile(speed, 0.5, na.rm = T),
#     speed_q3 = quantile(speed, 0.75, na.rm = T),
#     speed_q4 = quantile(speed, 1, na.rm = T),
#     .by = c(feed, route_type)
#   )

# library(ggplot2)
# full |>
#   filter(!is.infinite(speed)) |>
#   ggplot() +
#   geom_density(aes(x = speed, color = feed)) +
#   facet_wrap(vars(route_type)) +
#   theme_minimal()

# routes |> filter(stringr::str_detect(route_id, "6970"))
# trips |> filter(stringr::str_detect(route_id, "6970"))
# speeds |> filter(stringr::str_detect(trip_id, "6970"))

# # check_gtfs_density --------------------------------------------------------------------------

# # adapted from: https://higgicd.github.io/posts/accessibility_analysis/

# tar_load(feeds)
# check_gtfs_density <- function(feed_paths) {
#   # read in gtfs files
#   gtfs_list <- purrr::map(
#     feed_paths,
#     function(x) {
#       tidytransit::read_gtfs(x)
#     }
#   ) |>
#     purrr::set_names(feed_paths)

#   gtfs_list <- purrr::map(
#     gtfs_list,
#     function(x) {
#       x$calendar_long <- make_long_calendar(x$calendar)
#       return(x)
#     }
#   )

#   # get service period start and end dates from gtfs files
#   service_period <- gtfs_list |>
#     purrr::map(
#       function(x) purrr::pluck(x, "calendar_long")
#     ) |>
#     dplyr::bind_rows(.id = "feed_path")

#   # get count of services by day and identify overlaps
#   service_overlap <- service_period |>
#     dplyr::group_by(service_date) |>
#     dplyr::summarize(count = n(), feeds = n_distinct(feed_path)) |>
#     dplyr::mutate(
#       #overlap = case_when(count == length(gtfs_list) ~ 1, TRUE ~ 0)
#       # make more flexible - overlap as equal to max services
#       overlap = dplyr::case_when(count == max(count) ~ 1, TRUE ~ 0),
#       overlap_2 = dplyr::case_when(feeds == max(feeds) ~ 1, TRUE ~ 0)
#     )

#   # get a service peak around which to graph
#   service_density <- service_period$service_date |>
#     as.numeric() |>
#     stats::density()

#   service_density_peak <- service_density$x[which.max(service_density$y)] |>
#     as.integer() |>
#     lubridate::as_date()

#   # get start and end date of overlap period
#   service_overlap_start <- service_overlap |>
#     dplyr::filter(feeds == max(feeds)) |>
#     dplyr::summarize(min(service_date)) |>
#     dplyr::pull()

#   service_overlap_end <- service_overlap |>
#     dplyr::filter(feeds == max(feeds)) |>
#     dplyr::summarize(max(service_date)) |>
#     dplyr::pull()

#   service_overlap_start_month <- lubridate::floor_date(
#     service_overlap_start,
#     "month"
#   )

#   service_overlap_end_month <- lubridate::ceiling_date(
#     service_overlap_end,
#     "month"
#   ) -
#     days(1)

#   # 2. plot overlap gantt chart
#   ###66666666666
#   plot_data <- feeds_meta |>
#     mutate(
#       feed_name = stringr::str_remove(file_name, ".zip$"),
#     ) |>
#     select(feed_name, feed_alias, data_source, batch_id, feed_path) |>
#     right_join(service_period) |>
#     mutate(feed_name = forcats::fct_reorder(feed_name, service_date))

#   return(
#     list(
#       pretty_name = pop_unit$name_pop_unit,
#       label = pop_unit$label_pop_unit,
#       plot_data = plot_data,
#       service_density_peak = service_density_peak,
#       service_overlap_start = service_overlap_start,
#       service_overlap_end = service_overlap_end
#     )
#   )
# }

# # make_long_calendar -------------------------------------------------------------------------

# make_long_calendar <- function(
#   calendar,
#   min_start_year = NULL,
#   max_end_year = NULL
# ) {
#   loc <- if (.Platform$OS.type == "windows") {
#     "English_United States.1252"
#   } else {
#     "en_US.UTF-8"
#   }

#   calendar_list <- calendar |>
#     group_by(service_id) |>
#     group_split()

#   calendar_long <- calendar_list |>
#     purrr::map(
#       function(x) {
#         start <- lubridate::ymd(x$start_date)
#         end <- lubridate::ymd(x$end_date)
#         if (
#           !is.null(min_start_year) && lubridate::year(start) < min_start_year
#         ) {
#           start <- update(start, year = min_start_year)
#         }
#         if (!is.null(max_end_year) && lubridate::year(end) > max_end_year) {
#           end <- update(end, year = max_end_year)
#         }
#         allowed_days <- x |>
#           select(2:8) |>
#           tidyr::pivot_longer(
#             everything(),
#             names_to = "weekday",
#             values_to = "allowed"
#           )
#         df <- tibble(
#           service_id = x$service_id,
#           service_date = seq(start, end, by = "days"),
#           weekday = wday(service_date, label = T, abbr = F, locale = loc) |>
#             stringr::str_to_lower()
#         ) |>
#           left_join(allowed_days, by = "weekday") |>
#           filter(allowed == 1)
#       }
#     ) |>
#     bind_rows()
# }

# # treatment assignment

# library(ggplot2)
# library(sf)
# source("R/utils.R")

# tar_load(grid_sf)
# tar_load(ttm_transit)

# grid <- geobr::read_municipality(2025, 3550308)
# grid <- h3_from_sf(grid, "id")

# ttm <- ttm_transit

# acc <- accessibility::cumulative_cutoff(ttm, grid, "opp", "travel_time_p50", 20, "year")

# acc_sf <- acc |>
# 	mutate(
# 		acc = opp,
# 		period = factor(year, labels = c("Baseline (pre-2012)", "Post-2025")),
# 		.keep = "unused"
# 	) |>
# 	inner_join(grid) |>
# 	st_as_sf()

# acc_sf |>
# 	arrange(period) |>
# 	mutate(delta = travel_time_p50 - lag(travel_time_p50)) |>
# 	filter(period == "Post-2025") |>
# 	ggplot() +
# 	geom_sf(
# 		aes(fill = as.numeric(delta)),
# 		color = NA,
# 		stroke = 0
# 	) +
# 	scale_fill_viridis_c(direction = -1, option = "inferno") +
# 	labs(
# 		title = "Walking time to closest station - 2025 vs 2011",
# 		fill = "Time\n(minutes)"
# 	) +
# 	theme_void()

# ggsave("sidequsts/walk_delta.png", dpi = 300, bg = "white")

# acc_sf |>
# 	ggplot() +
# 	geom_sf(
# 		aes(fill = acc),
# 		color = NA,
# 		stroke = 0
# 	) +
# 	scale_fill_viridis_c(option = "inferno") +
# 	labs(
# 		title = "Transit mobility - cutoff (90 min)",
# 		fill = "Accessible\nhexagons"
# 	) +
# 	facet_wrap(vars(period)) +
# 	theme_void()

# ggsave("sidequests/acc_cutoff_90.png", dpi = 300, bg = "white")

# library(mapview)
# mapviewOptions(platform = "leafgl")

# ttm_min |>
# 	mapview(zcol = "travel_time_p50", alpha.regions = .5, lwd = 0)

# readxl::read_excel("data/station_openings.xlsx", sheet = "data")

# # 66666 test ---------------------------------------------------------------------------------

# tar_load(r5_network)
# library(ggplot2)
# library(sf)
# source("R/utils.R")
# network <- r5r::build_network(dirname(r5_network), overwrite = F)

# od <- tibble(
# 	id = c(
# 		"89a8100c263ffff",
# 		"89a8100c393ffff",
# 		"89a8100ec4bffff",
# 		"89a8100f1cfffff",
# 		"89a810050a7ffff",
# 		"89a81015237ffff",
# 		"89a81008813ffff",
# 		"89a81005a43ffff",
# 		"89a8100cdcbffff"
# 	)
# )

# od$geometry <- h3o::h3_from_strings(od$id) |> st_as_sfc()

# od <- st_as_sf(od) |>
# 	st_centroid()

# dep_datetime <- as.POSIXct("2012-04-09", tz = "America/Sao_Paulo")

# ttm <- r5r::travel_time_matrix(
# 	network,
# 	origins = od,
# 	destinations = od,
# 	mode = "TRANSIT",
# 	departure_datetime = dep_datetime,
# 	time_window = 15L,
# 	max_trip_duration = 90L,
# 	n_threads = 5,
# 	verbose = TRUE,
# 	percentiles = c(25L, 50L, 98L)
# )

# r5r::detailed_itineraries(
# 	network,
# 	origins = od,
# 	destinations = od,
# 	mode = "TRANSIT",
# 	departure_datetime = dep_datetime,
# 	time_window = 10L,
# 	max_trip_duration = 90L
# )

# ttm <- ttm |>
# 	mutate(year = !!year, dep_datetime = dep_datetime)

# r5r::transit_network_to_sf(network)

# treatment assignment

library(ggplot2)
library(sf)
source("R/utils.R")

tar_load(grid_sf)
tar_load(ttm_transit_all)

grid <- geobr::read_municipality(2025, 3550308)
grid <- h3_from_sf(grid, "id")
grid <- grid |>
	mutate(opp = 1)

grid <- aopdata::read_landuse("spo", geometry = TRUE)
grid <- grid |>
	rename(id = id_hex)

ttm <- ttm_transit_all
ttm <- ttm |>
	filter(from_id %in% grid$id & to_id %in% grid$id)

acc <- accessibility::cumulative_cutoff(
	travel_matrix = ttm,
	land_use_data = grid,
	opportunity = "T001",
	travel_cost = "travel_time_p50",
	cutoff = 60,
	"year"
)

acc_sf <- acc |>
	mutate(
acc = T001,
period = factor(year, labels = c("Baseline (pre-2012)", "Post-2025")),
.keep = "unused"
) |>
	inner_join(grid) |>
		st_as_sf()

	plot <- acc_sf |>
		ggplot() +
		geom_sf(
			aes(fill = acc), #, color = acc
			color = NA,
			stroke = 0
		) +
		scale_fill_viridis_c(option = "inferno",
	labels = scales::label_number(scale = 1e-6, suffix = "M")) +
		# scale_color_viridis_c(option = "inferno") +
		labs(
			title = "Accessibility - cutoff (60 min)",
			subtitle = "With 2019 land use data",
			fill = "Total jobs",
			# color = "Accessible\nhexagons"
		) +
		facet_wrap(vars(period)) +
		theme_void()

	ggsave(plot = plot, filename = "sidequests/acc_cutoff_60_v3.png", dpi = 1200, bg = "white",
	width = 18, height = 9, un = "cm")

acc_delta_sf <- acc_sf |>
	arrange(period) |>
	mutate(delta_acc = acc - lag(acc)) |>
	filter(period == "Post-2025")

plot_delta <- acc_delta_sf |>
	ggplot() +
	geom_sf(
		aes(fill = as.numeric(delta_acc)),
		color = NA,
		stroke = 0
	) +
	scale_fill_distiller(
		palette = "RdBu",
		direction = 1,
		labels = scales::label_number(scale = 1e-6, suffix = "M")
	) +
	labs(
		title = "Transit accessibility - 2025 vs 2012",
		fill = "Total jobs"
	) +
	theme_void()

ggsave(plot = plot_delta, filename = "sidequests/acc_delta_v2.png", dpi = 600, bg = "white")

acc_delta_sf |>
	sf::st_drop_geometry() |>
	ggplot() +
	geom_density(aes(x = delta_acc))

acc_sf |>
	sf::st_drop_geometry() |>
	ggplot() +
	geom_density(aes(x = acc, fill = period, color = period), alpha = 0.5) +
	theme(legend.position = "bottom")



library(mapview)
mapviewOptions(platform = "leafgl")

acc_gp <- split(acc_sf, acc_sf$period)

brk <- c(10, 50, 100, 250, 500, 1000, 2000, 3000, 4000, 5000)

map_gp <- acc_gp |>
	purrr::map(
		\(x) {
			nm = unique(x$period)
			mapview(x, zcol = "acc", alpha.regions = .75, lwd = 0, layer.name = nm, at = brk)
		}
	)

library(leafsync)
sync(map_gp, ncol = 1)
# at = c(10, 100, 500, 1000, 2000, 3000, 4000))

readxl::read_excel("data/station_openings.xlsx", sheet = "data")

# 66666 test ---------------------------------------------------------------------------------

tar_load(r5_network)
library(ggplot2)
library(sf)
source("R/utils.R")
network <- r5r::build_network(dirname(r5_network), overwrite = F)

od <- tibble(
	id = c(
		"89a8100c263ffff",
		"89a8100c393ffff",
		"89a8100ec4bffff",
		"89a8100f1cfffff",
		"89a810050a7ffff",
		"89a81015237ffff",
		"89a81008813ffff",
		"89a81005a43ffff",
		"89a8100cdcbffff"
	)
)

od$geometry <- h3o::h3_from_strings(od$id) |> st_as_sfc()

od <- st_as_sf(od) |>
	st_centroid()

dep_datetime <- as.POSIXct("2012-04-09", tz = "America/Sao_Paulo")

ttm <- r5r::travel_time_matrix(
	network,
	origins = od,
	destinations = od,
	mode = "TRANSIT",
	departure_datetime = dep_datetime,
	time_window = 15L,
	max_trip_duration = 90L,
	n_threads = 5,
	verbose = TRUE,
	percentiles = c(25L, 50L, 98L)
)

r5r::detailed_itineraries(
	network,
	origins = od,
	destinations = od,
	mode = "TRANSIT",
	departure_datetime = dep_datetime,
	time_window = 10L,
	max_trip_duration = 90L
)

ttm <- ttm |>
	mutate(year = !!year, dep_datetime = dep_datetime)

r5r::transit_network_to_sf(network)

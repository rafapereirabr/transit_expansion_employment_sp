# # treatment assignment

# library(ggplot2)
# library(sf)
# source("R/utils.R")

# tar_load(grid_sf)
# tar_load(ttm_transit_all)

# grid <- grid_sf
# ttm <- ttm_transit_all

# in_all <- ttm |>
# 	arrow::as_arrow_table() |>
# 	count(from_id, to_id) |>
# 	filter(n == 2) |>
# 	collect()

# ttm <- ttm |>
# 	filter(from_id %in% grid$id & to_id %in% grid$id) |>
# 	inner_join(in_all)

# acc <- accessibility::cumulative_cutoff(
# 	travel_matrix = ttm,
# 	land_use_data = grid,
# 	opportunity = "T001",
# 	travel_cost = "travel_time_p50",
# 	cutoff = 60,
# 	"year"
# )

# acc_sf <- acc |>
# 	mutate(
# 		acc = T001,
# 		period = factor(year, labels = c("Baseline (pre-2012)", "Post-2025")),
# 		.keep = "unused"
# 	) |>
# 	inner_join(grid) |>
# 	st_as_sf()

# plot <- acc_sf |>
# 	ggplot() +
# 	geom_sf(
# 		aes(fill = acc), #, color = acc
# 		color = NA,
# 		stroke = 0
# 	) +
# 	scale_fill_viridis_c(
# 		option = "inferno",
# 		labels = scales::label_number(scale = 1e-6, suffix = "M")
# 	) +
# 	# scale_color_viridis_c(option = "inferno") +
# 	labs(
# 		title = "Accessibility - cutoff (60 min)",
# 		subtitle = "With 2019 land use data",
# 		fill = "Total jobs",
# 		# color = "Accessible\nhexagons"
# 	) +
# 	facet_wrap(vars(period)) +
# 	theme_void()

# ggsave(
# 	plot = plot,
# 	filename = "sidequests/acc_cutoff_90_v2.png",
# 	dpi = 1200,
# 	bg = "white",
# 	width = 18,
# 	height = 9,
# 	un = "cm"
# )

# acc_delta_sf <- acc_sf |>
# 	arrange(period) |>
# 	mutate(delta_acc = acc - lag(acc), delta_acc_pct = acc / lag(acc) - 1, .by = "id") |>
# 	filter(period == "Post-2025" & !is.infinite(delta_acc_pct))

# # summary(acc_delta_sf$delta_acc)
# # quantile(acc_delta_sf$delta_acc, seq(0, 1, 0.01)) |>
# # abs() |>
# # sort()
# # sd(acc_delta_sf$delta_acc)
# # brk <- c(1700, 7500, 80000, 1e6)
# delta <- acc_delta_sf$delta_acc
# delta_nonzero <- abs(delta[is.finite(delta) & delta != 0])
# limit <- unname(quantile(abs(delta), 0.99, na.rm = TRUE))
# sigma <- unname(quantile(delta_nonzero, 0.25, na.rm = TRUE))

# contour <- st_union(grid)

# plot_delta <- acc_delta_sf |>
# 	ggplot() +
# 	geom_sf(
# 		aes(fill = delta_acc),
# 		color = NA,
# 		stroke = 0
# 	) +
# 	geom_sf(data = contour, fill = NA) +
# 	# scale_fill_steps2(
# 	# 	low = "#B2182B",
# 	# 	mid = "white",
# 	# 	high = "#2166AC",
# 	# 	midpoint = 0,
# 	# 	limits = c(-limit, limit),
# 	# 	transform = scales::transform_pseudo_log(sigma = sigma),
# 	# 	n.breaks = 9,
# 	# 	oob = scales::squish,
# 	# 	labels = scales::label_number(scale = 1e-3, suffix = "K")
# 	# ) +
# 	scale_fill_distiller(palette = "RdBu", direction = 1, label = scales::label_comma(big.mark = " ")) +
# 	labs(
# 		title = "Transit accessibility - 2025 vs 2012",
# 		fill = "Abs. change"
# 	) +
# 	theme_void()

# ggsave(plot = plot_delta, filename = "sidequests/acc_delta_v2.png", dpi = 600, bg = "white")

# acc_delta_sf |>
# 	# mutate(delta_t = case_when(delta_acc < -75e4 ~ -75e4,
# 	# 	delta_acc > 75e4 ~ 75e4, TRUE ~ delta_acc)) |>
# 	sf::st_drop_geometry() |>
# 	ggplot() +
# 	geom_density(aes(x = delta_acc))

# acc_sf |>
# 	sf::st_drop_geometry() |>
# 	ggplot() +
# 	geom_density(aes(x = acc, fill = period, color = period), alpha = 0.35) +
# 	theme(legend.position = "bottom")

# library(mapview)
# mapviewOptions(platform = "leafgl")

# acc_delta_sf |>
# 	filter(delta_acc < -1e5) |>
# 	mapview(zcol = "delta_acc", alpha.regions = 0.5, alpha.borders = 0)

# acc_gp <- split(acc_sf, acc_sf$period)

# brk <- c(10, 50, 100, 250, 500, 1000, 2000, 3000, 4000, 5000)

# map_gp <- acc_gp |>
# 	purrr::map(
# 		\(x) {
# 			nm = unique(x$period)
# 			mapview(x, zcol = "acc", alpha.regions = .75, lwd = 0, layer.name = nm, at = brk)
# 		}
# 	)

# library(leafsync)
# sync(map_gp, ncol = 1)
# # at = c(10, 100, 500, 1000, 2000, 3000, 4000))

# readxl::read_excel("data/station_openings.xlsx", sheet = "data")

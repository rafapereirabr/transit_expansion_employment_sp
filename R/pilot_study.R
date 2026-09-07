# set_study_area -----------------------------------------------------------------------------

# tar_load(grid_sf)
# tar_load(stations_sf)
# tar_load(access)
# source("R/utils.R")
# library(arrow)
# library(sf)
# stations <- stations_sf
# grid <- grid_sf

set_study_area <- function(stations, grid, access = NULL) {
	stations <- sf_from_parquet(stations)

	study_stations <- stations |>
		mutate(
			pilot_group = case_when(
				between(code_station, 125, 129) | between(code_station, 125, 129) ~ 4,
				code_line == 15 & is.na(yr_open) & code_station != 98 ~ 3,
				code_line == 15 ~ 2,
				code_line == 5 & code_station < 67 ~ 1
			),
			pilot_group_fct = factor(
				pilot_group,
				labels = c(
					"Additional treatment (Metro L5)",
					"Treatment (Metro L15)",
					"Control (Metro L15)",
					"Control (Metro L6)"
				)
			)
		) |>
		filter(!is.na(pilot_group)) |>
		st_transform(crs = 4326)

	rings <- c(1:6)

	tidy_ring <- function(ref, k) {
		tidyr::expand_grid(
			id_ref = ref,
			ring_order = k,
			id = h3o::grid_ring(ref, k = k) |> h3o::flatten_h3()
		)
	}

	study_stations$id_ref <- h3o::h3_from_points(study_stations$geometry, 9)
	stations_h3 <- study_stations$id_ref
	station_rings <- purrr::map(
		stations_h3,
		function(x) {
			purrr::map(rings, \(y) tidy_ring(x, y)) |>
				bind_rows()
		}
	) |>
		bind_rows() |>
		mutate(geometry = st_as_sfc(id)) |>
		mutate(id_ref = as.character(id_ref), id = as.character(id))

	study_stations_hex <- study_stations |>
		mutate(geometry = st_as_sfc(id_ref), id_ref = as.character(id_ref), ring_order = 0, id = id_ref) |>
		select(id, id_ref, yr_open, pilot_group, pilot_group_fct, ring_order, geometry)

	study_stations <- study_stations |>
		mutate(id_ref = as.character(id_ref)) |>
		select(id_ref, yr_open, pilot_group, pilot_group_fct, geometry)

	study_rings <- bind_rows(
		study_stations_hex,
		right_join(st_drop_geometry(study_stations), station_rings)
	) |>
		st_as_sf()

	allowed_h3 <- grid |>
		st_drop_geometry() |>
		pull(id)

	study_rings <- filter(study_rings, id %in% allowed_h3)

	if (!is.null(access)) {
		access <- access |>
			filter(period == "Post-2025") |>
			select(id, contains("CMATT"))
		study_rings <- inner_join(study_rings, access)
	}

	study_rings <- study_rings |>
		slice_min(ring_order, by = id) |>
		slice_min(yr_open, by = id, with_ties = FALSE)

	return(study_rings)
}

# ## dataviz
# ring_contours <- study_rings |>
# 	group_by(pilot_group, pilot_group_fct) |>
# 	summarise(geometry = st_union(geometry))

# transit_map +
# 	geom_sf(data = study_rings, aes(fill = factor(ring_order)), color = NA, alpha = 0.75) +
# 	geom_sf(data = ring_contours, fill = NA, aes(color = pilot_group_fct), linewidth = .75) +
# 	scale_fill_viridis_d() +
# 	scale_color_brewer(palette = "Dark2") +
# 	theme_void() +
# 	theme(legend.position = "bottom") +
# 	labs(fill = "Proximity (hex order)", color = "Group") +
# 	spatialops::geom_bbox(metro_bbox) +
# 	guides(
# 		color = guide_legend(
# 			order = 1,
# 			nrow = 2,
# 		),
# 		linetype = guide_legend(order = 2, nrow = 2),
# 		fill = guide_legend(order = 3, nrow = 2)
# 	) +
# 	spatialops::theme_abnq_map(base_size = 10) +
# 	theme(
# 		legend.position = "bottom",
# 		legend.title.position = "top",
# 		legend.box = "horizontal"
# 	)

# ggsave("figures/fig_study.png", dpi = 600, width = 16, height = 13.5, un = "cm", bg = "white")

# ggplot(study_rings) +
# 	geom_sf(aes(fill = delta_CMATT120), color = NA) +
# 	scale_fill_viridis_c(option = "inferno") +
# 	facet_wrap(vars(pilot_group_fct))

# ggplot(study_rings) +
# 	geom_boxplot(aes(color = factor(ring_order), y = delta_CMATT120, group = factor(ring_order))) +
# 	facet_wrap(vars(pilot_group_fct))

# assign_treatment ---------------------------------------------------------------------------
# library(arrow)
# library(sf)
# source("R/utils.R")

# tar_load(cadunico_fam)
# tar_load(pilot_cells)
# tar_load(pilot_years)
# families <- cadunico_fam
# study_area <- pilot_cells
# time_window <- pilot_years

assign_treatment <- function(families, study_area, time_window) {
	families <- open_dataset(families)
	study_area <- st_drop_geometry(study_area)
	families <- families |>
		rename(id = h3_09) |>
		inner_join(study_area, by = "id") |>
		filter(year %in% time_window)

	## hardcoded status - ok by now since time_window = c(2012, 2019, 2025) and 2019 is a "checkpoint"
	families <- families |>
		mutate(
			pilot_treated = ifelse(pilot_group < 3, 1, 0),
			pilot_post = ifelse(year == time_window[1], 1, 0)
		)

	families <- compute(families)
	return(families)
}

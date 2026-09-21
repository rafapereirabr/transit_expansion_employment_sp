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

	## remove interchange (dupli)
	duplicates <- bind_rows(
		stations |>
			st_drop_geometry() |>
			count(name_station, sort = T) |>
			filter(n > 1) |>
			select(name_station),
		stations |>
			st_drop_geometry() |>
			filter(stringr::str_detect(name_station, "\\(LINHA")) |>
			select(name_station)
	)

	keepers <- stations |>
		st_drop_geometry() |>
		select(code_station, name_station, yr_open) |>
		inner_join(duplicates) |>
		arrange(name_station, yr_open) |>
		slice_min(yr_open, by = name_station) |>
		slice_min(code_station, by = name_station)

	study_stations <- stations |>
		filter(
			(!(name_station %in% duplicates$name_station) |
				code_station %in% keepers$code_station) &
				!(code_station %in% c(3029, 3100))
		) |>
		mutate(
			pilot_group = case_when(
				code_line == 2 & yr_open %in% c(2027, 2028) & code_station > 108 ~ 3,
				code_line == 7 & yr_open == 2013 ~ 2,
				code_line == 4 & code_station %in% c(88, 94) ~ 2,
				code_line == 4 & code_station == 137 ~ 3,
				code_line == 5 & code_station < 69 ~ 1, #additional
				code_line == 5 & code_station < 90 ~ 2, #first
				code_station %in% c(125:129, 141:142) ~ 3, #u/c or planned,
				code_line == 15 & is.na(yr_open) & !(code_station %in% c(58, 98)) ~ 3,
				code_line == 17 & code_station %in% c(148, 157) ~ 3,
				code_line == 15 ~ 2,
				code_line == 19 & code_station %in% c(158, 162, 163, 169, 171) ~ 3,
				code_line == 20 & code_station %in% c(186:189) ~ 3,
				yr_open < 2012 ~ 0 #always
			),
			pilot_group_fct = factor(
				pilot_group,
				levels = c(0:3),
				labels = c("Always Treated", "Additional treatment", "First treatment", "Never treated")
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
			filter(year == 2025 & method == "CMATT60") |>
			select(id, accessibility)
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
# 	summarise(geometry = st_union(geometry)) |>
# 	nngeo::st_remove_holes()

# transit_map +
# 	geom_sf(data = study_rings, aes(fill = factor(ring_order)), color = NA, alpha = 0.75) +
# 	geom_sf(data = ring_contours, fill = NA, aes(color = pilot_group_fct), linewidth = 1.5) +
# 	scale_fill_viridis_d(direction = 1) +
# 	scale_color_viridis_d(option = "inferno") +
# 	# scale_color_brewer(palette = "Paired") +
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

assign_treatment <- function(families, study_area, time_window, save_dir = NULL) {
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

	## save
	file_name <- "pilot_families.parquet"
	file_path <- file.path(save_dir, file_name)
	write_parquet_target(families, file_path, overwrite = TRUE)
	return(file_path)
}

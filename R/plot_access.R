# library(sf)
# library(ggplot2)
# library(patchwork)
# tar_load(access)
# tar_load(grid_sf)
# grid <- grid_sf

plot_access <- function(access, grid, access_var = "CMATT90", use_delta = TRUE) {
	## convert to sf
	access_sf <- inner_join(access, grid) |>
		st_as_sf()

	access_sym <- rlang::sym(access_var)

	## initialize level plot
	if (use_delta) {
		plot_level <- access_sf |>
			filter(year == min(year)) |>
			ggplot()
	} else {
		plot_level <- ggplot(access_sf)
	}

	plot_level <- plot_level +
		geom_sf(
			aes(fill = {{ access_sym }}), #, color = acc
			color = NA,
			stroke = 0
		) +
		scale_fill_viridis_c(
			option = "inferno",
			labels = scales::label_number(scale = 1e-6, suffix = "M")
		) +
		labs(
			title = "Access to formal jobs by public transit",
			fill = "Total jobs"
		) +
		theme_void()

	if (!use_delta) {
		plot_level <- plot_level +
			facet_wrap(access_var)
	} else {
		plot_level <- plot_level +
			labs(subtitle = "Baseline")
	}

	if (!use_delta) {
		plot_path <- "figures/acc_cutoff_120.png"
		ggsave(
			plot = plot_level,
			filename = plot_path,
			dpi = 600,
			bg = "white",
			width = 18,
			height = 9,
			un = "cm"
		)

		return(plot_level)
	}

	contour_sf <- geobr::read_municipality(2025, 3550308)

	plot_delta <- access_sf |>
		filter(period == "Post-2025") |>
		ggplot() +
		geom_sf(aes(fill = delta_CMATT120), color = NA, stroke = 0) +
		geom_sf(data = contour_sf, fill = NA) +
		scale_fill_distiller(
			palette = "RdBu",
			direction = 1,
			labels = scales::label_number(scale = 1e-6, suffix = "M"),
			limits = c(-1e6, 1e6),
			oob = scales::squish
		) +
		labs(
			title = "Access to formal jobs by public transit",
			subtitle = "Absolute change (2025 vs. 2012)",
			fill = "Abs. change"
		) +
		theme_void()

	plot_combined <- wrap_plots(plot_level, plot_delta, guides = "collect") *
		labs(title = NULL) +
		plot_annotation(title = "Access to formal jobs by public transit")

	plot_path <- "figures/acc_cutoff_120.png"
	ggsave(
		plot = plot_combined,
		filename = plot_path,
		dpi = 300,
		bg = "white",
		width = 18,
		height = 9,
		un = "cm"
	)

	plot_list <- list(
		plot_level = plot_level,
		plot_delta = plot_delta,
		plot_combined = plot_combined
	)

	return(plot_list)
}

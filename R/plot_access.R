# library(sf)
# library(ggplot2)
# library(patchwork)
# source("R/utils.R")
# source("R/calc_access.R")

# tar_load(access)
# tar_load(grid_sf)
# grid <- grid_sf
# tar_load(access_spec)
# method <- access_spec[1, ]$method

plot_access <- function(access, grid, use_delta = TRUE, years = NULL, method = NULL) {
	if (is.null(years)) {
		years <- unique(access$year)
	}

	if (!is.null(method)) {
		method_abbr <- recode_values(
			method,
			"cumulative_cutoff" ~ "CMATT",
			"cumulative_interval" ~ "CIATT",
			"gravity" ~ "GRATT"
		)
		access <- filter(access, stringr::str_detect(method, method_abbr))
	}

	if (use_delta & length(grep("d_", names(access))) == 0) {
		access <- calc_access_delta(access)
	}

	## convert to sf
	access_sf <- inner_join(access, grid) |>
		st_as_sf()
	contour_sf <- geobr::read_municipality(2025, 3550308)

	access_syms <- rlang::syms(c("accessibility", "d_max_access"))
	stopifnot(access_syms %in% names(access))

	coreplotter <- function(yr) {
		if (yr == min(years)) {
			fill_var <- access_syms[[1]]
			scale_args <- list(
				option = "inferno",
				labels = scales::label_number(scale = 1e-6, suffix = "M"),
				name = "Total jobs"
			)
			scale_name <- "scale_fill_viridis_c"
		} else {
			fill_var <- access_syms[[2]]
			scale_args <- list(
				palette = "RdBu",
				direction = 1,
				limits = c(-45e4, 45e4),
				n.breaks = 9,
				# labels = scales::label_number(scale = 1e-6, suffix = "M"),
				labels = scales::label_comma(big.mark = " "),
				oob = scales::squish,
				name = "Abs. change"
			)
			scale_name <- "scale_fill_distiller"
		}

		p <- access_sf |>
			filter(year == yr) |>
			ggplot() +
			geom_sf(aes(fill = {{ fill_var }}), color = NA, stroke = 0) +
			geom_sf(data = contour_sf, fill = NA) +
			do.call(scale_name, scale_args) +
			labs(subtitle = yr) +
			theme_void()

		return(p)
	}

	plots <- purrr::map(years, coreplotter)

	plots_combined <- patchwork::wrap_plots(plots, guides = "collect")

	if (is.null(method)) {
		plot_path <- "figures/accessibility.png"
	} else {
		method_alias <- unique(access$method)
		plot_path <- paste0("figures/acc_", method_alias, ".png")
	}
	ggsave(
		plot = plots_combined,
		filename = plot_path,
		dpi = 300,
		bg = "white",
		width = 20,
		height = 20,
		un = "cm"
	)

	return(plots)
}

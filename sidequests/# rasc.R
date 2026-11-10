# rasc
options(scipen = 999)
tar_load(panel_eoy)
# tar_load(access)

panel <- arrow::read_parquet(panel_eoy, as_data_frame = F)

access <- access |>
	filter(year == 2025 & method == "GRATT60") |>
	select(id, d_max_access)

panel_g19 <- panel |>
	filter(code_line %in% c(5, 15, 6, 17)) |>
	left_join(access, by = c("h3_09_cad" = "id")) |>
	collect()

panel_g19 <- panel_g19 |>
	mutate(
		status = case_when(
			code_line == 5 & yr_open < 2014 ~ 0,
			code_line == 15 & yr_open < 2020 ~ 1,
			code_line %in% c(6, 17) ~ 2
		)
	) |>
	filter(!is.na(status)) |>
	mutate(
		status = factor(status, labels = c("Additional treatment", "First treatment", "Never treated"))
	)

quantile(panel_g19$d_max_access, seq(0, 1, 0.05), na.rm = T)

brk <- c(-260000, -10000, 0, 10000, 50000, 100000, 250000, 1e6)
panel_g19 <- panel_g19 |>
	mutate(
		isochrone = cut(walk_time, seq(0, 60, 15), include.lowest = T),
		delta_band = cut(d_max_access, brk, include.lowest = T)
	) |>
	filter(!is.na(delta_band))

panel_g19_summary <- panel_g19 |>
	summarise(
		across(c(employed_12mo_cad, emp_31dec_rais, sal_hr_rais), ~ mean(.x, na.rm = T)),
		.by = c(year, delta_band, status, gender_cad) #, isochrone
	)

library(ggplot2)
panel_g19 |>
	filter(year == 2011) |>
	ggplot(aes(x = d_max_access)) +
	stat_density()

panel_g19_summary |>
	ggplot(aes(x = year, y = emp_31dec_rais, color = delta_band)) + #isochrone
	geom_line() +
	geom_point() +
	geom_vline(xintercept = 2019, color = "blue", linetype = "dashed") +
	geom_hline(yintercept = 0) +
	facet_grid(status ~ gender_cad) +
	scale_y_continuous(label = scales::label_percent()) +
	scale_color_viridis_d(option = "inferno", end = 0.88) +
	# scale_color_brewer(palette = "BrBG") +
	theme_minimal()


years <- c(2011:2023)
vinc_eoy <- purrr::map(
	years,
	\(x) {
		ipeadatalake::ler_rais(x, "estab", c("codemun", "qt_vinc_ativos")) |>
			mutate(year = x) |>
			compute()
	}
)

vinc_eoy_summary <- purrr::reduce(vinc_eoy, arrow::concat_tables) |>
	summarise(n_vinc = sum(qt_vinc_ativos), .by = c(year, codemun)) |>
	compute()

vinc_eoy_summary |>
	mutate(is_sp = codemun == 355030) |>
	summarise(n_vinc = sum(n_vinc), .by = c(is_sp, year)) |>
	collect() |>
	ggplot(aes(x = year, y = n_vinc, color = is_sp)) +
	geom_line() +
	geom_point() +
	geom_vline(xintercept = 2019, color = "blue", linetype = "dashed") +
	geom_hline(yintercept = 0)


panel_g19_summary |>
	filter(year %in% c(2015, 2023)) |>
	summarise(
		across(employed_12mo_cad, ends_with("rais"), ~ last(.x) - first(.x)),
		.by = c(year, delta_band)
	)

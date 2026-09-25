tar_load(panel_pilot)
panel <- panel_pilot

panel <- arrow::open_dataset(panel)
panel <- collect(panel) |>
	mutate(ring_orig = ring_order[1], .by = cpf_mask)

library(ggplot2)

panel |>
	summarise(
		travel_time = mean(travel_time_p50, na.rm = T),
		.by = c(year, pilot_group_fct, ring_orig)
	) |>
	arrange(year) |>
	mutate(delta = travel_time - lag(travel_time), .by = c(pilot_group_fct, ring_orig)) |>
	filter(!is.na(delta)) |>
	ggplot() +
	geom_hline(yintercept = 0, linetype = "dashed", color = "gray75") +
	geom_line(aes(x = ring_orig, y = delta, color = pilot_group_fct)) +
	scale_fill_brewer(palette = "Dark2") +
	theme_minimal()

N


targets::tar_visnetwork()

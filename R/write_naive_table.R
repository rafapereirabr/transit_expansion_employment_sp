# write naive comparison table
# tar_load(panel_pilot)
# panel <- panel_pilot

write_naive_table <- function(panel, save_dir = NULL, balanced = FALSE, format = "docx") {
	if (inherits(panel, "character")) {
		panel <- arrow::open_dataset(panel)
	} else if (!inherits(panel, "ArrowTabular")) {
		panel <- arrow::as_arrow_table(panel)
	}

	tbl <- panel |>
		group_by(year, pilot_group_fct, cpf_mask) |>
		summarise(
			formal = if_else(sum(emp_31dez, na.rm = TRUE) > 0, 1, 0),
			travel_time = mean(travel_time_p50, na.rm = TRUE)
		) |>
		mutate(travel_time = ifelse(is.nan(travel_time), NA, travel_time)) |>
		summarise(
			n_adults = n_distinct(cpf_mask),
			n_formal = sum(formal),
			pct_formal_raw = n_formal / n_adults,
			mean_travel_time = mean(travel_time * formal, na.rm = TRUE)
		) |>
		collect()

	tbl_wide <- tbl |>
		mutate(across(starts_with("pct_"), ~ 100 * (.x))) |>
		mutate(across(starts_with(c("pct_", "mean_")), ~ round(.x))) |>
		tidyr::pivot_wider(
			names_from = year,
			values_from = starts_with(c("n_", "pct_", "mean_")),
			names_sep = "."
		)

	tiny_tbl <- tbl_wide |>
		tinytable::tt() |>
		tinytable::group_tt(j = ".")

	if (is.null(save_dir)) {
		return(tiny_tbl)
	}

	file_name <- if_else(balanced, paste0("tbl_naive_balanced.", format), paste0("tbl_naive.", format))
	file_path <- file.path(save_dir, file_name)
	tinytable::save_tt(tiny_tbl, file_path, overwrite = TRUE)
	return(file_path)
}

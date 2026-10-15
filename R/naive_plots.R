# naive_plots
library(ggplot2)
panel <- arrow::open_dataset("data/panel_eoy.parquet")

t_groups <- distinct(panel, yr_open) |> filter(yr_open >= 2012 & !is.na(yr_open) & !is.infinite(yr_open)) |> arrange(yr_open) |> pull(as_vector = T)
names(t_groups) <- t_groups

units <- t_groups |> 
  purrr::map(\(x) set_controls(panel, x))

panel_summary <- purrr::imap(
  units,
  \(x, idx) summarise_panel(panel, x, c(year, status)) #c(year, status, gender, race)
)

# set_controls -----------------------------------------------------------

## construct adequate counterfactuals
set_controls <- function(panel, time_window, always_treated = T, filter_panel = F) {
  if(always_treated) {
    mtw <- min(time_window)
    ctrl_ok <- panel |> 
    filter(is.na(yr_open) | is.infinite(yr_open) | yr_open < mtw)
  } else {
    ctrl_ok <- panel |> 
    filter(is.na(yr_open) | is.infinite(yr_open))
  }
  ctrl_ok <- ctrl_ok |>  
    filter(between(year, yr_open - 2, yr_open) & lives_in_sp == 1 & years_update <= 2) |> 
    distinct(cpf) |> 
    pull()

  if(length(time_window) == 2) {
    treat_ok <- panel |> 
      filter(between(yr_open, time_window[1], time_window[2])) |> 
      distinct(cpf) |> 
      pull()
  } else if(length(time_window) == 1) {
    treat_ok <- panel |> 
      filter(yr_open == time_window) |> 
      distinct(cpf) |> 
      pull()
  } 
  
  if(filter_panel) {
    panel <- panel |> 
    filter(cpf %in% ctrl_ok | between(yr_open, time_window[1], time_window[2]))
    return(panel)
  } else {
    return(c(treat_ok, ctrl_ok))
  } 
}


# summarise_panel --------------------------------------------------------

summarise_panel <- function(panel, units = NULL, group_vars) {
  panel_2 <- panel |> 
    filter(cpf %in% units) |> 
	  collect() |> 
    mutate(
		status = case_when(
			yr_open < 2012 ~ "Always Treated", 
			is.na(yr_open) | is.infinite(yr_open) ~ "Never Treated",
			TRUE ~ "First Treatment"#as.character(yr_open)
		) |> as.factor(),
		race = if_else(race_cad == 1, "White", "Nonwhite"),
		gender = if_else(gender_cad == 1, "Man", "Woman")
	)
  
  panel_2 <- panel_2 |> 
  filter(!is.na(race)) |> 
	summarise(
		across(
			c(age, emp_31dec_rais, salary_rais, hours_rais, sal_hr_rais, tenure_rais, job_in_sp, job_in_rmsp, new_job, walk_time), 
			~mean(.x, na.rm = T)
		), 
		.by = {{group_vars}}
	)

  return(panel_2)
}

# naive plots ------------------------------------------------------------

panel_summary_df <- panel_summary |> 
  bind_rows(.id = "cohort")

plot_1 <- panel_summary_df|> 
  ggplot(aes(x = year, y = sal_hr_rais, color = status)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(cohort)), inherit.aes = F, linetype = "dashed") +
  labs(title = "Average hourly wages by treatment cohort", subtitle = "Control units vary by cohort", color = "Treatment status", x = "Avg. hourly wage (current BRL)", 
  y = "Year") +
  scale_x_continuous(breaks = seq(2011, 2023, by = 2)) +
  scale_color_manual(values = c("Always Treated" = "darkred", "First Treatment" = "darkgreen", "Never Treated" = "gray35")) +
  facet_wrap(vars(cohort)) +
  theme_linedraw() +
  theme(legend.position = "bottom")

ggsave("figures/naive_plot_salary.png", plot = plot_1, dpi = 300, bg = "white", height = 18
, units = "cm")

plot_2 <- panel_summary_df|> 
  ggplot(aes(x = year, y = emp_31dec_rais, color = status)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(cohort)), inherit.aes = F, linetype = "dashed") +
  labs(title = "Average formal employment wages by treatment cohort", subtitle = "Control units vary by cohort", color = "Treatment status", x = "Avg. hourly wage (current BRL)", 
  y = "Year") +
  scale_x_continuous(breaks = seq(2011, 2023, by = 2)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = c("Always Treated" = "darkred", "First Treatment" = "darkgreen", "Never Treated" = "gray35")) +
  facet_wrap(vars(cohort)) +
  theme_linedraw() +
  theme(legend.position = "bottom")

ggsave("figures/naive_plot_emp31dec.png", plot = plot_2, dpi = 300, bg = "white", height = 18, units = "cm")

ggplot(panel_summary, aes(x = year, y = sal_hr_rais, color = status)) +
  geom_point() +
  geom_line() +
	facet_grid(gender ~ race)

ggplot(panel_summary, aes(x = year, y = emp_31dec_rais, color = status)) +
  geom_point() +
  geom_line() +
  facet_grid(gender ~ race)
  scale_y_continuous(labels = scales::percent)

panel_2 <- panel  |> 
	filter(year %in% c(2011, 2015, 2023)) |> 
  filter(!is.na(walk_time) & salary_rais < 5e3) |> 
	select(year, walk_time, salary_rais) |> 
	collect()

panel_2 |> 
	ggplot(aes(x = walk_time, y = salary_rais, color = factor(year))) +
	geom_point(stroke = 0) +
	facet_wrap(vars(year))

glimpse(panel)

panel |> filter(yr_open > 2012) |> distinct(yr_open, cpf) |> count(yr_open, sort = T) |> collect()


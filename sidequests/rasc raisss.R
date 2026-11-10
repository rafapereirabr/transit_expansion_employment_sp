# library(sf)
# source("R/utils.R")
# source("R/find_nearest_station.R")
# tar_load(c(person_records, rais_records, ttm_walk_stations, grid_sf, stations_sf))

panelize_rais <- function(rais_records, ) {
  
}
## tidy rais --------------------------------------------------------------
## temporary bypasse because coluna do ano ta zoada
rais_records <- rais_records |>
	purrr::map(
		\(x) {
			yr <- stringr::str_extract(x, "\\d{4}") |> as.integer()
			arrow::open_dataset(x) |>
				mutate(year = yr) |>
        distinct() |> 
				compute()
		}
	)
rais_records <- purrr::reduce(rais_records, arrow::concat_tables)
# rais_records <- arrow::open_dataset(rais_records)

## keep only end-of-year entries
rais_eoy <- rais_records |>
	filter(emp_31dez > 0) |>
	compute()

##' we'll drop people with multiple jobs when they:
##' (i) have "more jobs than unique wages" (possible error rather than actual mx jobs)
##' (ii) work above the allowed 44 weekly hours limit (same) 
##' (iii) have zero salary or work hours (bad data)
##' it's important to keep track of who these ppl are because we need to drop them altogether

bad_data_1 <- filter(rais_eoy, horas_contr < 1 | salario < 1) |> compute() |> pull(cpf)

multiple_jobs <- rais_eoy |>
	filter(!cpf %in% bad_data_1) |> 
	count(cpf, year, sort = T) |>
	filter(n > 1) |>
	select(-n) |>
	compute() |>
	inner_join(rais_eoy)

mjs <- multiple_jobs |>
	group_by(cpf, year) |>
	summarise(hours = sum(horas_contr), n_jobs = n(), n_sal = n_distinct(salario), js = n_jobs / n_sal) |> 
  compute()

bad_data_2 <- mjs |> filter(hours > 44 | js > 1) |> compute() |> pull(cpf)

mx_jobs_ok <- mjs |> pull(cpf) |> setdiff(bad_data_2)
# 151037/153814 # so basically we'll drop 98% of those with multiple jobs

## 6666666 TBD CALC ACTUAL WAGES
# wage_cols <- c(
# 	"cpf", "id_estab", "cei_vinc", "data_adm", "horas_contr",
# 	"vl_rem_sc_01", "vl_rem_sc_02", "vl_rem_sc_03", "vl_rem_sc_04","vl_rem_sc_05", "vl_rem_sc_06", "vl_rem_sc_07",
# 	"vl_rem_sc_08", "vl_rem_sc_09", "vl_rem_sc_10", "vl_rem_sc_11", "vl_rem_sc_12", "vl_rem_13_adiant", "vl_rem_13_final"
# )


## computations: is the job in (RM)SP? is it new?
allowed_cells <- st_drop_geometry(grid_sf) |> pull(id)
rmsp <- geobr::read_metro_area(2023, 35) |> 
  filter(type == "RM de São Paulo (SP)") |> 
  mutate(codemun = stringr::str_sub(code_muni, 1, 6) |> as.double()) |> 
	pull(codemun) |> 
	setdiff(355030)
rais_eoy <- rais_eoy |> 
  mutate(
	  job_in_sp = ifelse(work_h3_id %in% allowed_cells, 1, 0),
		job_in_rmsp = ifelse(codemun %in% rmsp, 1, 0),
	  new_job = ifelse(temp_empr >= 12, 0, 1), 
		employed = 1
	) |>
compute()

## sumarise rais to one line per person
rais_mx_jobs <- rais_eoy |>
  filter(cpf %in% mx_jobs_ok) |> 
 	collect() |> 
	summarise(
		across(c(salario, horas_contr), sum),
		temp_empr = max(temp_empr),
		across(c(job_in_sp, new_job, starts_with("work_")), ~.x[which.max(temp_empr)]),
		.by = c(cpf, year)
	) |>
	mutate(horas_contr = as.integer(horas_contr)) |> 
	arrow::as_arrow_table()

rais_mx_jobs <- rais_mx_jobs |> 
  mutate(
		horas_contr = arrow::cast(horas_contr, arrow::int8()),
		work_lat = arrow::cast(work_lat, arrow::decimal128(8, 6)),
		work_lon = arrow::cast(work_lon, arrow::decimal128(8, 6)),
	) |> 
	compute()

rais_eoy <- rais_eoy |> 
	filter(!(cpf %in% mx_jobs_ok)) |> 
	select(cpf, year, salario, horas_contr, temp_empr, job_in_sp, job_in_rmsp, new_job, starts_with("work")) |> 
	compute()

rais_consolidated <- arrow::concat_tables(rais_eoy, rais_mx_jobs)


## drop those whose geocoding isn't precise
bad_data_3 <- rais_consolidated |> 
  filter(work_desvio > 500) |>
	compute() |>
	pull(cpf) 

rais_consolidated <- rais_consolidated |> 
	filter(!(cpf %in% c(bad_data_3))) |> 
	compute()


## final rais adjustments
rais_consolidated <- rais_consolidated |> 
  transmute(
		year, cpf, emp_31dec_rais = 1, 
		salary_rais = salario, hours_rais = horas_contr, sal_hr_rais = salario/horas_contr,
		tenure_rais = temp_empr, job_in_sp, job_in_rmsp, new_job, 
		h3_09_rais = work_h3_id, lat_rais = work_lat, lon_rais = work_lon
	) |> 
	compute()


# join with cadunico -----------------------------------------------------


person_records <- arrow::open_dataset(person_records)
n0 <- nrow(person_records)
n0_u <- person_records |> distinct(cpf) |> compute() |> nrow()

bad_data_4 <- person_records |> 
  filter(desvio_metros > 500) |>
	compute() |>
	pull(cpf) 

## drop the bad apples from cadunico & arrumar colunas
person_records <- person_records |> 
	filter(!cpf %in% c(bad_data_1, bad_data_2, bad_data_3, bad_data_4)) |>
	mutate(year = arrow::cast(year, arrow::int32())) |> 
	rename(lat_cad = lat, lon_cad = lon) |> 
  select(-desvio_metros) |> 
	compute()	
n1 <- nrow(person_records)
n1_u <- person_records |> distinct(cpf) |> compute() |> nrow()

## join rais & cadunico
panel <- left_join(person_records, rais_consolidated) |> 
	mutate(emp_31dec_rais = dplyr::coalesce(emp_31dec_rais, 0)) |> 
	compute()

## log data

log <- tibble::tribble(
	~data, ~cond,
	n0, "raw records",
	n0_u, "initial roster",
	length(bad_data_1), "horas_contr < 1 | salario < 1",
	length(bad_data_2), "hours > 44 | n_jobs/n_salaries > 1",
	length(bad_data_3), "work_desvio > 500",
	length(bad_data_4), "desvio_metros > 500",
	n1, "final n*T",
	n1_u, "final roster"
)



# join with station opening dates & walking times ------------------------

min_times <- find_nearest_station(ttm_walk_stations, stations_sf, 2023)
min_times <- min_times |> 
  mutate(from_id = as.integer(from_id)) |> 
	arrow::as_arrow_table()

## TMPY WORKAROUND
origins <- arrow::open_dataset(tar_read(family_records))
origins <- filter(origins, h3_09 %in% allowed_cells) |>
	distinct(lat, lon) |>
  collect() |> 
	tibble::rowid_to_column("from_id") |> 
  arrow::as_arrow_table() |> 
  mutate(
		lat = arrow::cast(lat, arrow::decimal128(8, 6)),
		lon = arrow::cast(lon, arrow::decimal128(8, 6))
	) |> 
	compute()
min_times <- left_join(min_times, origins) |> rename(lat_cad = lat, lon_cad = lon) |> select(-from_id) |> compute()

panel <- left_join(panel, min_times) |> 
  compute()


panel_summary <- panel |> 
	collect() |> 
  mutate(
		status = case_when(
			yr_open < 2012 ~ "Always Treated", 
			is.na(yr_open) | is.infinite(yr_open) ~ "Never Treated",
			TRUE ~ as.character(yr_open)
		) |> as.factor()
	) |> 
	summarise(
		across(
			c(age, emp_31dec_rais, salary_rais, hours_rais, tenure_rais, job_in_sp, job_in_rmsp, new_job, walk_time), 
			~mean(.x, na.rm = T)
		), 
		.by = c(year, status)
	)

library(ggplot2)
	
ggplot(panel_summary, aes(x = year, y = salary_rais, color = status)) +
  geom_point() +
  geom_line()

ggplot(panel_summary, aes(x = year, y = emp_31dec_rais, color = status)) +
  geom_point() +
  geom_line() +
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

count(panel, yr_open, sort = T) |> collect()

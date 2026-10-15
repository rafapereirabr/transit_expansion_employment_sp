# library(sf)
# source("R/utils.R")
# source("R/find_nearest_station.R")
# tar_load(c(person_records, rais_records, ttm_walk_stations, grid_sf, stations_sf))

panelize_eoy <- function(rais_records, grid_sf, person_records, ttm_walk_stations, stations_sf, family_records, 
	save_dir = NULL, log = T) {
  
	rais_records <- open_dataset(rais_records)

	## keep only end-of-year entries
	rais_eoy <- rais_records |>
		filter(emp_31dez > 0) |>
		compute()

  ##' we'll drop people with multiple jobs when they:
	##' (i) have "more jobs than unique wages" (possible error rather than actual mx jobs)
	##' (ii) work above the allowed 44 weekly hours limit (same) 
	##' (iii) have zero salary or work hours (bad data)
	##' it's important to keep track of who these ppl are because we need to drop them altogether

	bad_data_1 <- filter(rais_eoy, horas_contr < 1 | salario < 1) |> distinct(cpf) |> pull(cpf)

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

	bad_data_2 <- mjs |> filter(hours > 44 | js > 1) |> distinct(cpf) |> pull()
	bad_data <- c(bad_data_1, bad_data_2)
	mx_jobs_ok <- mjs |> distinct(cpf) |> pull() |> setdiff(bad_data)
	# distinct(multiple_jobs, cpf) |> compute()
	# 114890/138355 # so basically we'll drop 99% of those with multiple jobs

	## 6666666 TBD CALC ACTUAL WAGES
	# wage_cols <- c(
	# 	"cpf", "id_estab", "cei_vinc", "data_adm", "horas_contr",
	# 	"vl_rem_sc_01", "vl_rem_sc_02", "vl_rem_sc_03", "vl_rem_sc_04","vl_rem_sc_05", 
	#   "vl_rem_sc_06", "vl_rem_sc_07", "vl_rem_sc_08", "vl_rem_sc_09", "vl_rem_sc_10", 
	# 	"vl_rem_sc_11", "vl_rem_sc_12", "vl_rem_13_adiant", "vl_rem_13_final"
	# )

  ## computations: is the job in (RM)SP? is it new?
	allowed_cells <- st_drop_geometry(grid_sf) |> pull(id)
	rmsp <- geobr::read_metro_area(2023, 35) |> 
	  filter(type == "RM de São Paulo (SP)") |> 
	  mutate(codemun = stringr::str_sub(code_muni, 1, 6) |> as.double()) 
  
  rmsp_ibge6 <- pull(rmsp, codemun) |> setdiff(355030)
	rais_eoy <- rais_eoy |> 
    filter(!(cpf %in% bad_data)) |> 
	  mutate(
		  job_in_sp = ifelse(work_h3_id %in% allowed_cells, 1, 0),
			job_in_rmsp = ifelse(codemun %in% rmsp_ibge6, 1, 0),
		  new_job = ifelse(temp_empr >= 12, 0, 1), 
			employed = 1
		) |>
	compute()
	
	
	# sumarise rais to one line per person -----------------------------------
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
		as_arrow_table() |> 
	  mutate(
			horas_contr = cast(horas_contr, int8()),
			work_lat = cast(work_lat, decimal128(8, 6)),
			work_lon = cast(work_lon, decimal128(8, 6)),
		) |> 
		compute()
  
	rais_eoy <- rais_eoy |> 
		filter(!(cpf %in% mx_jobs_ok)) |> 
		select(cpf, year, salario, horas_contr, temp_empr, job_in_sp, job_in_rmsp, new_job, starts_with("work")) |> 
		compute()
	
	rais_consolidated <- concat_tables(rais_eoy, rais_mx_jobs)
	
	## drop those whose geocoding is imprecise
	bad_data_3 <- rais_consolidated |> 
	  filter(work_desvio > 500) |>
		compute() |>
		pull(cpf) 
	bad_data <- c(bad_data, bad_data_3)
	
	rais_consolidated <- rais_consolidated |> 
		filter(!(cpf %in% bad_data)) |> 
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
	person_records <- open_dataset(person_records)
	n0 <- nrow(person_records)
	n0_u <- person_records |> distinct(cpf) |> compute() |> nrow()
  
  ## remove bad geocoding
	bad_data_4 <- person_records |> 
	  filter(desvio_metros > 500) |>
		distinct(cpf) |> 
		pull() |> 
    setdiff(bad_data)
  bad_data <- c(bad_data, bad_data_4)
	
  ## assign treatment by min walking time 
	min_times <- find_nearest_station(ttm_walk_stations, stations_sf, 2023)
	min_times <- min_times |> 
	mutate(from_id = as.integer(from_id)) |>
	as_arrow_table(schema = schema(from_id = int64(), code_line = double(), code_station = string(), name_station = string(), yr_open = double(), walk_time = int64()))

	### treatment is absorbing!!!
	pr1 <- person_records |>
		distinct(cpf, id_addr_cad) |>
		left_join(min_times, by = c("id_addr_cad" = "from_id")) |> 
    filter(!is.na(yr_open)) |> 
		collect()

	pr1 <- pr1 |> 
		slice_min(yr_open, by = cpf, with_ties = F)

	person_records <- left_join(person_records, min_times, by = c("id_addr_cad" = "from_id")) |> 
      compute()
 
  ## drop if, 2 yrs before treatment, (i) registry is outdated or (ii) home address was outside of SP capital.  
  bad_data_6 <- person_records |> 
    filter(between(year, yr_open - 2, yr_open) & years_update > 2) |> # ATTENTION: for ctrl groups we'na do case-by-case
		distinct(cpf) |> 
		pull() |> 
		setdiff(bad_data)
	bad_data <- c(bad_data, bad_data_6)
  
	rmsp_ibge7 <- pull(rmsp, code_muni) |> setdiff(3550308)
	person_records <- person_records |> 
		mutate(
			lives_in_sp = ifelse(h3_09_cad %in% allowed_cells, 1, 0),
			lives_in_rmsp = ifelse(code_muni_cad %in% rmsp_ibge7, 1, 0),
		) |> 
		select(-desvio_metros) |> 
		compute()
  bad_data_7 <- person_records |> 
    filter(between(year, yr_open - 2, yr_open) & lives_in_sp == 0) |> 
		distinct(cpf) |> 
		pull() |> 
		setdiff(bad_data)
	bad_data <- c(bad_data, bad_data_7)
	
  ## same cpf, different birth dates: bad data!! registration problem
  bad_data_8 <- person_records |> 
		distinct(cpf, birthdate_cad) |> 
			count(cpf, sort = T) |> 
				filter(n > 1) |> 
					pull(cpf) |> 
						setdiff(bad_data)
					bad_data <- c(bad_data, bad_data_8)
					
					
	## remove all the bad
	person_records <- person_records |> 
		filter(!(cpf %in% bad_data)) |> 
		compute()
	
	## final numbers
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
		length(bad_data_3), "multiple jobs - other",
		length(bad_data_4), "work_desvio > 500",
		length(bad_data_5), "desvio_metros > 500",
		length(bad_data_6), "outdated cadunico in t0",
		length(bad_data_7), "lives outside of sp in t0",
		length(bad_data_8), "multiple birth dates",
		n1, "final n*T",
		n1_u, "final roster"
	)


	# save panel and log -------------------------------------------------------
	file_path <- file.path(save_dir, "panel_eoy.parquet")
	log_path <- file.path(save_dir, "panel_eoy_log.parquet")
	write_parquet_target(panel, file_path, overwrite = TRUE)
	write_parquet_target(log, log_path, overwrite = TRUE)
	
	return(file_path)
}

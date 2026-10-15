# library(did)

# panels <- purrr::map2(
#   t_groups, units,
#   \(x, y) panel |> 
#   filter(cpf %in% y) |> 
#   mutate(
#     yr_open = coalesce(yr_open, Inf),
#     treated = if_else(yr_open == x, 1, 0), 
#     post = if_else(year >= x, 1, 0),
#     post_covid = if_else(year > 2019, 1, 0),
#     female = if_else(gender_cad == 2, 1, 0),
#     nonwhite = if_else(race_cad == 1, 0, 1),
#   ) |> 
#   filter(is.infinite(yr_open) | yr_open == x) |> 
#   select(year, cpf, female, nonwhite, yr_open, emp_31dec_rais, sal_hr_rais, treated, post) |> 
#   compute()
# )

# panels <- purrr::map(panels, \(x) collect(x) |> mutate(treated = max(treated), yr_open = yr_open)) 

# cpn <- panel |> 
#   distinct(cpf) |> 
#   collect() |> 
#   tibble::rowid_to_column("id_mask") |> 
#   arrow::as_arrow_table()

# panels <- purrr::map(panels, \(x) left_join(x, cpn) |> compute())

# did_sal <- purrr::map(
#   panels, \(p)
#   att_gt(
#     yname = "sal_hr_rais", tname = "year", idname = "id_mask", gname = "yr_open",
#     xformla = ~ female + nonwhite,
#     data = p,
#     control = "notyettreated",
#     pl = T, cores = 12
#   )
# )

# purrr::map(did_sal, summary)

# did_emp <- purrr::map(
#   panels, \(p)
#   att_gt(
#     yname = "emp_31dec_rais", tname = "year", idname = "cpf", gname = "yr_open",
#     xformla = ~ female + nonwhite,
#     data = p,
#     control = "notyettreated",
#     pl = T, cores = 12
#   )
# )

# purrr::map(did_emp, summary)
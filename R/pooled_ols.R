# # pooled ols
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

# ols_sal <- purrr::map(
#   panels, \(p)
#   lm(
#     sal_hr_rais ~ female + nonwhite + treated*post,
#     # emp_31dec_rais ~ female + nonwhite + treated*post,
#     data = collect(p)
#   )
# )

# purrr::map(ols_sal, summary)

# ols_emp <- purrr::map(
#   panels, \(p)
#   lm(
#     emp_31dec_rais ~ female + nonwhite + treated*post,
#     data = collect(p)
#   )
# )

# purrr::map(ols_emp, summary)

# time_window <- c(2011, 2015, 2023)
# names(time_window) <- time_window

# ols <- purrr::map(
#   time_window, \(t)
#   lm(
#     sal_hr_rais ~ female + nonwhite + treated*post,
#     # emp_31dec_rais ~ female + nonwhite + I(treated, post)
#     data = p1#filter(p1, year == t)
#   )
# )

# purrr::map(ols, summary)



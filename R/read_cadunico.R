# read_cad_fams -------------------------------------------------------------------------------

# tar_load(c(munis_sf, time_window))
# munis <- munis_sf
# years <- time_window

read_cad_families <- function(years, munis, save_dir = NULL) {
  cad_dates <- 100 * years + 12

  if (inherits(munis, "character")) {
    munis <- sf_from_parquet(munis, drop_geometry = T)
  }

  all_fams <- purrr::map2(
    cad_dates,
    years,
    function(x, y) {
      ipeadatalake::ler_cadunico(
        x,
        "familia",
        c("dt_atualizacao_fam", "cd_ibge_cadastro", "co_familiar_fam"),
        verboso = F
      ) |>
        mutate(year = y, .before = everything()) |>
        compute()
    }
  )

  unique_fams <- purrr::reduce(all_fams, arrow::concat_tables) |>
    filter(cd_ibge_cadastro %in% munis$code_muni) |>
    distinct(co_familiar_fam) |>
    pull(as_vector = T)

  fams <- all_fams |>
    purrr::map(\(x) filter(x, co_familiar_fam %in% unique_fams) |> compute())

  fams <- purrr::map2(
    fams,
    cad_dates,
    function(x, y) {
      ipeadatalake::adicionar_geoloc(x, "cadunico", y)
    }
  )

  fams <- purrr::reduce(fams, arrow::concat_tables)

  gc()

  fams <- fams |>
    # filter(desvio_metros < 1e3) |> we'll do this downstream!
    distinct(
      year,
      dt_atualizacao_fam,
      cd_ibge_cadastro,
      co_familiar_fam,
      lat,
      lon,
      desvio_metros,
      h3_09
    ) |>
    compute()

  write_parquet_target(fams, file.path(save_dir, "cad_fams.parquet"), T)
  # if (is.null(save_dir)) {
  #   return(fams)
  # } else {
  #   path <- file.path(save_dir, "cad_fams.parquet")
  #   arrow::write_parquet(fams, path)
  #   return(path)
  # }
}

# read_cad_individs ---------------------------------------------------------------------------

# tar_load(c(time_window, cadunico_fam))
# save_dir <- "data/temp"
# families <- cadunico_fam
# year <- time_window[1]
read_cad_individuals <- function(year, families, save_dir = NULL) {
  if (inherits(families, "character")) {
    families <- arrow::open_dataset(families)
  }

  families <- families |>
    filter(year == !!year) |>
    compute()

  individ <- ipeadatalake::ler_cadunico(
    100 * year + 12,
    "pessoa",
    harmonizado = T
  )

  cols_ind <- c(
    "co_familiar_fam",
    "cpf_recuperado",
    "co_sexo_pessoa",
    "dt_nasc_pessoa",
    "co_raca_cor_pessoa",
    # 6666666666 BLOCO DE VAR BOAS QUE FALTAM NA BASE
    # "co_principal_trab_memb",
    # "co_trabalhou_semana_memb",
    # "co_trabalho_12_meses_memb",
    # "qt_meses_12_meses_memb",
    # "vl_renda_bruta_12_meses_memb",
    "vl_remuner_emprego_memb",
    # "vl_renda_aposent_memb",
    # "vl_renda_seguro_desemp_memb",
    # "vl_renda_outras_memb"
    "fx_renda_individual_808"
  )

  individ <- individ |>
    select(any_of(cols_ind))

  end_of_year <- paste(year, 12, 31, sep = "-") |>
    as.Date()

  matched_ind <- inner_join(individ, families)

  matched_ind <- matched_ind |>
    mutate(
      age = (as.integer(end_of_year) - as.integer(dt_nasc_pessoa)) / 365.25
    ) |>
    filter(age >= 18) |>
    relocate(year, co_familiar_fam, cpf_recuperado, .before = everything()) |>
    compute()

  file_name <- paste0("cad_individ_", year, ".parquet")
  file_path <- file.path(save_dir, file_name)
  write_parquet_target(
    matched_ind,
    file_path,
    overwrite = T
  )

  return(file_path)
}

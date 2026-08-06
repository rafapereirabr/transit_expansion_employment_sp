# read_cad_fams -------------------------------------------------------------------------------

# tar_load(c(munis_sf, time_window))
# munis <- munis_sf
# years <- time_window

read_cad_families <- function(years, munis, save_dir = NULL) {
  cad_dates <- 100 * years + 12

  if (inherits(munis, "character")) {
    munis <- arrow::read_parquet(munis)
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

  fams <- fams |>
    filter(desvio_metros < 1e3) |>
    distinct(
      dt_atualizacao_fam,
      cd_ibge_cadastro,
      co_familiar_fam,
      lat,
      lon,
      h3_09
    ) |>
    compute()

  if (is.null(save_dir)) {
    return(fams)
  } else {
    path <- file.path(save_dir, "cad_fams.parquet")
    arrow::write_parquet(fams, path)
    return(path)
  }
}


# read_cad_individs ---------------------------------------------------------------------------

# tar_load(c(time_window, munis_sf, cadunico_fam))
# save_dir <- "data/temp"
# families <- "data/temp/cad_fams.parquet"
# munis <- munis_sf
# year <- time_window[1]
read_cad_individs <- function(year, munis, families, save_dir = NULL) {
  if (inherits(munis, "character")) {
    munis <- arrow::read_parquet(munis)
  }
  if (inherits(families, "character")) {
    families <- arrow::open_dataset(families)
  }

  individ <- ipeadatalake::ler_cadunico(
    100 * year + 12,
    "pessoa",
    harmonizado = T,
    colunas = c(
      "co_familiar_fam",
      "cpf_recuperado",
      "co_sexo_pessoa",
      "dt_nasc_pessoa",
      "co_raca_cor_pessoa",
      "vl_remuner_emprego_memb",
      "vl_renda_aposent_memb",
      "vl_renda_seguro_desemp_memb",
      "vl_renda_outras_memb"
    )
  )

  eoy <- paste(year, 12, 31, sep = "-") |>
    as.Date()

  fam_test <- families |>
    count(co_familiar_fam, sort = T) |>
    collect() |>
    slice(1e6L:2e6L) |>
    pull(co_familiar_fam)
  f2 <- families |>
    filter(co_familiar_fam %in% fam_test) |>
    compute()

  f2 |>
    mutate(delta = as.numeric(eoy) - as.numeric(dt_atualizacao_fam)) |>
    group_by(co_familiar_fam) |>
    filter(delta == min(delta))
  # individ <- individ |>
  #   mutate(year = !!year)

  head(families) |> collect()
}

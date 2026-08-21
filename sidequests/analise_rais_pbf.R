library(dplyr)

anos <- c(2015, 2025)

cad <- purrr::map(
  anos, function(x) {
    df <- ipeadatalake::ler_cadunico(
      data = 100*x + 12, base = "pessoa",
      colunas = c("co_uf", "cd_ibge_cadastro", "nu_cpf_pessoa", "marc_pbf", "co_sexo_pessoa",
                  "dt_nasc_pessoa")
    )
    df <- df |>
      mutate(code_state = as.double(co_uf), code_muni = cd_ibge_cadastro, cpf = nu_cpf_pessoa,
             femin_cad = as.double(co_sexo_pessoa - 1), ano_nasc = lubridate::year(dt_nasc_pessoa),
             .keep = "unused") |>
      mutate(ano = x, marc_pbf = as.double(marc_pbf), pea = if_else(ano_nasc > 1999, 0, 1)) |>
      filter(!is.na(cpf) & pea == 1) |>
      compute()
    return(df)
  }
)

rais <- purrr::map(
  anos, function(x) {
    df <- ipeadatalake::ler_rais(
      ano = x, base = "vinc", colunas = c("codemun", "cpf", "genero")
    )
    df <- df |>
      filter(!is.na(cpf)) |>
      mutate(code_muni_6 = codemun,
             code_state = as.double(stringr::str_sub(as.character(code_muni_6), 1, 2)),
             femin_rais = genero - 1, .keep = "unused") |>
      distinct(cpf, code_state, .keep_all = T) |>
      mutate(ano = x) |>
      compute()
    return(df)
  }
)

cad_2 <- purrr::map2(
  cad, rais, function(x, y) {
    df <- left_join(x, y, by = c("cpf", "code_state", "ano"))
    df <- df |>
      mutate(in_rais = if_else(is.na(femin_rais), 0, 1)) |>
      compute()
  }
)

cad_2 <- arrow::concat_tables(cad_2[[1]], cad_2[[2]])

gc()

summary_cad <- cad_2 |>
  summarise(
    n_cad = n_distinct(cpf),
    n_rais = sum(in_rais),
    n_pbf = sum(marc_pbf),
    n_rais_pbf = sum(in_rais*marc_pbf),
    n_fem_cad = sum(femin_cad),
    n_fem_rais = sum(femin_rais, na.rm = T),
    n_genero_match = sum(femin_cad == femin_rais, na.rm = T),
    .by = c("code_state", "ano")
  ) |>
  mutate(prop_rais = n_rais_pbf/n_pbf) |>
  collect()

uf <- geobr::read_state(year = 2025)
bbox <- uf |> filter(abbrev_state %in% c("AC", "RR", "RN", "RS")) |> sf::st_bbox()
uf <- sf::st_crop(uf, bbox)

tops <- bind_rows(
  summary_cad |> filter(ano == 2015) |> arrange(desc(prop_rais)) |> slice(1:5),
  summary_cad |> filter(ano == 2025) |> arrange(desc(prop_rais)) |> slice(1:5),
  summary_cad |> filter(ano == 2015) |> arrange(prop_rais) |> slice(1:5),
  summary_cad |> filter(ano == 2025) |> arrange(prop_rais) |> slice(1:5)
) |>
  left_join(uf) |>
  sf::st_as_sf() |>
  mutate(color_rev = if_else(code_state %in% c(41:43, 53, 35, 50), "1", "0"))

library(ggplot2)

summary_cad |>
  left_join(uf) |>
  sf::st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = prop_rais)) +
  geom_sf_text(data = tops, aes(label = paste0(round(100*prop_rais), "%"), color = color_rev)) +
  scale_color_manual(values = c(`1` = "black", `0` = "white")) +
  scale_fill_fermenter(palette = "BuPu", labels = scales::label_percent()) +
  facet_wrap(vars(ano)) +
  labs(title = "Bolsa Família x emprego formal",
       subtitle = "Proporção de inscritos no programa maiores de 18 anos com carteira assinada",
       fill = "") +
  guides(color = "none") +
  theme_void() +
  theme(strip.text = element_text(face = "bold"))

ggsave("pbf_rais.png", dpi = 600, bg = "white")

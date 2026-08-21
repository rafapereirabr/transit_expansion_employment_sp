# migra_cad

migra_cad <- cf |>
  distinct(cd_ibge_cadastro, co_familiar_fam) |>
  mutate(code_muni = as.character(cd_ibge_cadastro)) |>
  count(code_muni, sort = T) |>
  collect() |>
  mutate(code_state = stringr::str_sub(code_muni, 1, 2)) |>
  mutate(code_state = as.integer(code_state)) |>
  count(code_state, wt = n, sort = T) |>
  left_join(geobr::read_state(2025), by = "code_state") |>
  sf::st_as_sf()

summary(migra_cad$n)

migra_cad |>
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_fermenter(
    palette = "RdPu",
    breaks = c(1e3, 5e3, 15e3, 5e4, 5e5, 3e6),
    direction = 1
  )

ggsave("migra_cad.png", dpi = 300)

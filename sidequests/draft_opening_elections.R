plot_trial <- dates |>
  filter(!is.na(dt_trial_svc) & !is.na(dt_full_svc)) |>
  mutate(t = -lubridate::interval(dt_full_svc, dt_trial_svc)/lubridate::dmonths(1)) |>
  ggplot() +
  geom_histogram(aes(x = t, fill = label_line)) +
  scale_fill_manual(values = metro_palette) +
  labs(
    title = "Quanto tempo leva o período de testes?",
    subtitle = "Intervalo entre a inauguração oficial e a operação plena, SP",
    y = "Quantidade de estações",
    x = "Tempo (meses)",
    fill = "Linha",
    caption = paste0("Datas coletadas de diversas fontes.",
                     "\nLinha 17: data estimada de operação plena.",
                     "\nFeito por Arthur Bazolli (@baarthur0)")
  ) +
  theme_minimal(base_family = "Barlow")

ggsave(filename = "figures/plot_trial.png", plot = plot_trial, #width = 8, height = 8,
       dpi = 600, bg = "white")

plot_elections <- plot +
  labs(
    title = "Em ano eleitoral se inauguram mais estações?",
    subtitle = "2026, 2018 e 2014: eleição para o governo estadual; 2020: prefeitura",
    x = "Ano de inauguração",
    y = "Número de estações",
    fill = "Linha",
    caption = paste0("Datas coletadas de diversas fontes.",
                     "\nFeito por Arthur Bazolli (@baarthur0)")
  ) +
  theme_minimal(base_family = "Barlow")

ggsave(filename = "figures/plot_elections.png", plot = plot_elections, #width = 8, height = 6,
       dpi = 600, bg = "white")

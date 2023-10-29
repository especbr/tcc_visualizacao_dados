ggplot(cotas_df_ano) +
  geom_ribbon(aes(y = ano, xmin = 12, xmax = min), fill = "grey20", alpha = 0.5) +
  geom_dumbbell(
    aes(y = ano,
        x = min,
        xend = max),
    size = 3,
    size_x = 5,
    size_xend = 5,
    colour_x = "orange",
    colour_xend = "darkblue",
    color = "grey60"
  ) +
  gghighlight(ano %in% c(2010, 2023),
              label_key = type,
              unhighlighted_params = list(
                size = 1.5,
                size_x = 3,
                size_xend = 3,
                colour_x = "gold2",
                colour_xend = "dodgerblue",
                fill = "midnightblue"
              )
  ) +
  scale_y_continuous(breaks = seq(2000, 2023, 1), limits = c(2000, 2023)) +
  coord_flip() +
  labs(
    title = "Nível do Rio Negro (AM)",
    subtitle = "Máximos e Mínimos dos anos 2000 a 2023",
    y = "",
    x = "Nível (em m)",
    caption = "Fonte: Porto de Manaus (2023)"
  ) +
  geom_vline(
    aes(xintercept = metrica_geral$min), color = "red", linetype = "dashed", size = 1
  ) +
  theme_igray()

library(tidyverse)
library(ggalt)
library(ggthemes)
library(gghighlight)
library(echarts4r)
library(ggpattern)

# Dataviz

ggplot(cotas_df_ano) +
  geom_ribbon_pattern(aes(y = ano, xmin = 12, xmax = min),
                      fill = NA,
                      colour = NA,
                      pattern = "image",
                      pattern_type = "expand",
                      pattern_filename =  system.file('img', "textura.png", package = 'png')) +
  geom_dumbbell(
    aes(y = ano, x = min, xend = max),
    size = 1.5,
    size_x = 3,
    size_xend = 3,
    colour_x = "tomato",
    colour_xend = "darkblue",
    color = "grey60"
  ) +
  geom_dumbbell(
    data = subset(cotas_df_ano, ano %in% c(2010, 2023)),
    aes(y = ano, x = min, xend = max),
    size = 3,
    size_x = 5,
    size_xend = 5,
    colour_x = "tomato3",
    colour_xend = "darkblue",
    color = "darkgoldenrod2"
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
  theme_economist() +
  geom_label(aes(x = 12, y = 2020, label = paste0(
    "Mínima histórica (", metrica_geral$min, ")")), fill = "darkgoldenrod2")



cotas_df |>
  tidyr::fill(cota, .direction = "up") |>
  e_charts(data) |>
  e_calendar(range = "2010", left = "125") |>
  e_heatmap(cota, coord_system = "calendar", calendarIndex = 0, name = "2010") |>
  e_visual_map(min = 12, max = 30, top = "150", left = "15") |>
  e_calendar(range = "2023", top = "240", left = "125") |>
  e_heatmap(cota, coord_system = "calendar", calendarIndex = 1, name = "2023") |>
  e_title("Calendário", "Heatmap") |>
  e_tooltip()


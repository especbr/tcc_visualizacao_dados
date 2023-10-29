library(tidyverse)

cotas_df <- rio::import("dados/dados_rios.RDS") |>
  mutate(cota = str_replace(cota, ",", "."),
         cota = as.numeric(cota),
         cota = case_when(
           data == "2023-04-23" ~ 26.79,
           TRUE ~ cota)) |>
  distinct(data, cota) |>
  mutate(cota = if_else(cota == 0, NA, cota))

cotas_df_ano <- cotas_df |>
  drop_na() |>
  mutate(ano = year(data),
         mes = month(data)) |>
  group_by(ano) |>
  summarise(media = mean(cota),
            min = min(cota),
            max = max(cota))

metrica_geral <- cotas_df |>
  drop_na() |>
  summarise(
    media = mean(cota),
    max = max(cota),
    min = min(cota)
  )

# EDA ---------------------------------------------------------------------

gtExtras::gt_plt_summary(cotas_df_ano)

summarytools::dfSummary(cotas_df_ano) |> summarytools::stview()


# time series -------------------------------------------------------------

cotas_df_ts <- cotas_df |> drop_na() |> select(2)

ts_dados <- ts(cotas_df_ts, start = c(2000, 1), frequency = 365)
plot.ts(ts_dados)

dec <- decompose(ts_dados)
plot(dec)

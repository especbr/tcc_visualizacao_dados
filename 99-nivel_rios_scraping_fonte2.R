# Esta é outra fonte de dados, que complementa dados do outro site
# No entanto, há divergência entre os dados, de forma que mantive o código
# para o caso de haver necessidade de imputação para uma grande quantidade
# de dias

# função scrape -----------------------------------------------------------

criar_df <- function(mes, ano) {

  mes <- str_pad(mes, width = 2, pad = 0)
  # Realizar a requisição GET
  url <- paste0("https://proamanaus.com.br/indexregua.php?u=regua-dos-rios&mes=", mes, "&ano=", ano)
  res <- GET(url)

  # Fazer o parsing do conteúdo da página
  pagina <- content(res, as="parsed", encoding = "ISO-8859-1")

  # Verificar se a página contém uma tabela
  if(length(html_nodes(pagina, "table")) == 0) {
    warning(paste("Sem tabela encontrada para mês:", mes, " ano:", ano))
    return(data.frame()) # Retorna um data.frame vazio
  }

  # Encontrar a tabela e convertê-la em um dataframe
  tabela <- pagina |>
    html_nodes("table") |>
    pluck(1) |>
    html_table() |>
    select(X1, X2) |>
    rename(data = X1, manaus = X2) |>
    slice(-(1:3))

}


# coleta de dados ---------------------------------------------------------

grid_anos <- expand.grid(1:12, 2010:2023)

dados <- map2_df(grid_anos$Var1, grid_anos$Var2, criar_df)

write.csv(dados, "dados_rios.csv")

# tratamento de dados -----------------------------------------------------

dados_final <- dados |>
  distinct(data, manaus) |>
  mutate(manaus = as.numeric(manaus),
         data = as.Date(data, format = "%d/%m/%Y"),
         manaus = case_when(
           data == "2012-09-04" ~ ((24.41+24.05)/2),
           data == "2012-07-12" ~ 28.80,
           data == "2016-04-05" ~ 23.51,
           data == "2016-04-06" ~ 23.51,
           data == "2016-06-23" ~ 27.17,
           data == "2016-05-13" ~ 26.34,
           data == "2018-07-11" ~ 28.28,
           data == "2010-03-06" ~ 23.25,
           TRUE ~ manaus)
  ) |>
  fill(manaus, .direction = "down") |>
  mutate(anterior = lag(manaus, 1),
         dif = ifelse(is.na(anterior), NA, manaus - anterior))

dados_final <- dados_final |>
  select(manaus)

ts_dados <- ts(dados_final, start = c(2010, 1), frequency = 365)
plot.ts(ts_dados)
plot(decompose(ts_dados))

dados_final |>
  ggplot() +
  aes(data, manaus) +
  geom_line() +
  scale_x_date(date_breaks = "2 months") +
  theme_minimal()

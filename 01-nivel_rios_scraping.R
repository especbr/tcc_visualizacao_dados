library(httr)
library(rvest)
library(tidyverse)

# função scrape Site Porto de Manaus --------------------------------------

pegar_dados <- function(semestre, ano) {
# Definir a URL e os dados de payload
url_porto <- "https://www.portodemanaus.com.br/?pagina=nivel-do-rio-negro-hoje"
payload <- list(
  semestre = semestre,
  ano = ano,
  buscar = "Buscar"
)

# Fazer a requisição POST
res <- POST(url_porto, body = payload, encode = "form")

# Verificar se a requisição foi bem-sucedida
if (status_code(res) == 200) {
  # Fazer o parsing do conteúdo da página
  pagina <- content(res, as = "parsed")

  cols1 <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho")
  cols2 <- c("julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
  # Realizar o web scraping com rvest (por exemplo, extrair uma tabela)
  if (payload$semestre == 1) {
  tabela <- pagina %>%
    html_nodes("table") %>%  # Supondo que você esteja interessado em uma tag <table>
    html_table() |>
    pluck(8) |>
    slice(-(1:2)) |>
    select(dia = X1,
           janeiro = X2,
           fevereiro = X4,
           março = X6,
           abril = X8,
           maio = X10,
           junho = X12) |>
    pivot_longer(
      cols = cols1,
      names_to = "mes",
      values_to = "cota")|>
    mutate(
      data = paste0(payload$ano, "-", mes, "-", dia),
      data2 = ymd(data, locale = "pt_BR", quiet = TRUE)
    ) |>
    drop_na() |>
    select(data2, cota) |> rename(data = data2) |>
    arrange(data)
  } else {
    tabela <- pagina %>%
      html_nodes("table") %>%  # Supondo que você esteja interessado em uma tag <table>
      html_table() |>
      pluck(8) |>
      slice(-(1:2)) |>
      select(dia = X1,
             julho = X2,
             agosto = X4,
             setembro = X6,
             outubro = X8,
             novembro = X10,
             dezembro = X12) |>
      pivot_longer(
        cols = cols2,
        names_to = "mes",
        values_to = "cota")|>
      mutate(
        data = paste0(payload$ano, "-", mes, "-", dia),
        data2 = ymd(data, locale = "pt_BR", quiet = TRUE)
      ) |>
      drop_na() |>
      select(data2, cota) |> rename(data = data2) |>
      arrange(data)
  }

} else {
  warning("Requisição POST falhou com status ", status_code(res))
}
}


# tratamento de dados Porto -----------------------------------------------
grid_semestre_anos <- expand.grid(1:2, 2000:2023)

cotas <- purrr::map2_df(grid_semestre_anos$Var1, grid_semestre_anos$Var2, pegar_dados)

saveRDS(cotas, file = "dados/dados_rios.RDS")

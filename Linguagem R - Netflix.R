library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

netflix <- read_csv("netflix_titles.csv")
print(netflix)

netflix_limpo <- netflix %>%
  filter(!is.na(date_added)) %>%
  mutate(
    date_added = mdy(date_added),
    year_added = year(date_added)
  )

titulos_por_ano <- netflix_limpo %>%
  group_by(year_added,type) %>%
  summarise(qtd=n(),.groups='drop') %>%
  filter(!is.na(year_added))

ggplot(titulos_por_ano, aes(x=year_added, y= qtd,fill=type)) +
  geom_col()+
  labs(
    title="Número de Filmes e Séries Adicionados à Netflix por Ano",
    x="Ano de Adição",
    y="Quantidade de Títulos",
    fill="Tipo"
  )+
  theme_minimal()

generos <- netflix_limpo %>%
  separate_rows(listed_in, sep=', ') %>%
  group_by(year_added, listed_in) %>%
  summarise(qtd=n(),.groups='drop')

generos_top5 <- generos %>%
  group_by(listed_in) %>%
  summarise(total=sum(qtd),.groups='drop')%>%
  slice_max(total,n=5) %>%
  inner_join(generos, by='listed_in')

ggplot(generos_top5, aes(x = year_added, y=qtd,color = listed_in)) +
  geom_line(size=1) +
  labs(
    title = 'Evolução dos 5 Gêneros mais Populares na Netflix',
    x="Ano",
    y="Quantidade de Títulos",
    color = "Gênero"
  ) + 
  theme_minimal()
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

netflix <- read_csv("netflix_titles.csv")

filmes <- netflix %>%
  filter(type == "Movie", !is.na(listed_in))


generos <- filmes %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(listed_in, sort = TRUE) %>%
  top_n(10, n)

ggplot(generos, aes(x = reorder(listed_in, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Gêneros de Filmes na Netflix",
    x = "Gênero",
    y = "Quantidade"
  ) +
  theme_minimal()

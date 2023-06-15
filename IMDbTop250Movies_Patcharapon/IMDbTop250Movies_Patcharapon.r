# -*- coding: utf-8 -*-

# -- Sheet --
install.packages("tidyverse")
install.packages("rvest")

## IMDB project (web scraping)
library(tidyverse)
library(rvest)

url <- "https://www.imdb.com/chart/top/"

imdb <- read_html(url)
imdb

imdb_title <- imdb %>%
  html_nodes("td.titleColumn") %>%
  html_text() %>%
  str_remove_all("\n") %>%
  str_trim() %>%
  str_replace_all("\\s+", " ") %>%
  str_replace_all("^[0-9]{1,3}\\.\\s", "") %>%
  str_remove("[0-9]{4}") %>%
  str_remove("\\(") %>%
  str_remove("\\)")

imdb_year <- imdb %>%
  html_nodes("td.titleColumn") %>%
  html_text() %>%
  str_extract( "[0-9]{4}") 

imdb_rating <- imdb %>%
  html_nodes("td.imdbRating") %>%
  html_text() %>%
  str_remove_all("\n") %>%
  str_replace_all("\\s+"," ")

df <- data.frame(imdb_title, imdb_year, imdb_rating)
df




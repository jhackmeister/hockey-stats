library(tidyverse)
library(rvest)

# read in last season data from hockey reference
url <- "https://www.hockey-reference.com/leagues/NHL_2025_games.html"
page <- read_html(url)

games_table25 <- page %>%
  html_node("table#games") %>%
  html_table() %>%
  as_tibble(.name_repair = "unique") %>% 
  rename(
    home_goals = G...4,
    away_goals = G...6,
    game_length = ...7
  ) %>% 
  janitor::clean_names()

# read in this season data from hockey reference
url <- "https://www.hockey-reference.com/leagues/NHL_2026_games.html"
page <- read_html(url)

games_table26 <- page %>%
  html_node("table#games") %>%
  html_table() %>%
  as_tibble(.name_repair = "unique") %>% 
  rename(
    home_goals = G...4,
    away_goals = G...6,
    game_length = ...7
  ) %>% 
  janitor::clean_names()


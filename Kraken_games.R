library(tidyverse)
# read in data from https://moneypuck.com/moneypuck/playerData/careers/gameByGame/regular/teams/SEA.csv

sea <- read.csv("sea.csv")

sea <- sea %>%
  distinct(season, gameId) %>% # Keep only unique combinations of season and gameid
  group_by(season) %>%
  mutate(gamenum = row_number()) %>% # Create gamenum for unique games
  ungroup() %>%
  right_join(sea, by = c("season", "gameId")) # Join back to the original data
 


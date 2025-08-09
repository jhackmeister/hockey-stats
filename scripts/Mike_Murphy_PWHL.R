library(googlesheets4)
library(tidyverse)
library(ggpubr)
gs4_auth()

# Read in the 4 sheets
goals <- read_sheet("https://docs.google.com/spreadsheets/d/1FK4wdHXUhGc5R6p9Z0baGJVH8yxADZ34hVeLZUd4R78/edit?gid=0#gid=0",
                    sheet = "SCORING EVENTS")

games <- read_sheet("https://docs.google.com/spreadsheets/d/1FK4wdHXUhGc5R6p9Z0baGJVH8yxADZ34hVeLZUd4R78/edit?gid=0#gid=0",
                    sheet = "GAME LOG")
goalies <- read_sheet("https://docs.google.com/spreadsheets/d/1FK4wdHXUhGc5R6p9Z0baGJVH8yxADZ34hVeLZUd4R78/edit?gid=0#gid=0",
                    sheet = "Goalies")
standings <- read_sheet("https://docs.google.com/spreadsheets/d/1FK4wdHXUhGc5R6p9Z0baGJVH8yxADZ34hVeLZUd4R78/edit?gid=0#gid=0",
                    sheet = "Standings")
schedule <- read.csv("2024_2025_PWHL_schedule.csv")

# Clean up GAME LOG

games <- games[, 1:25] # drop unnecessary columns - could add back if needed
games <- games[, -13]

test_games <- games[-1, ] # remove double header names
colnames(test_games) <- test_games[1, ]
colnames(test_games) <- as.character(seq_along(test_games)) # unique column names

# create team dfs
bos <- test_games %>%
  select(1,2,13,14)
colnames(bos) <- c("GF", "GA", "SF", "SA")
bos <- bos %>% 
  mutate(team_game_num = row_number())

nyc <- test_games %>%
  select(3,4,15,16)
colnames(nyc) <- c("GF", "GA", "SF", "SA")
nyc <- nyc %>% 
  mutate(team_game_num = row_number())

min <- test_games %>%
  select(5,6,17,18)
colnames(min) <- c("GF", "GA", "SF", "SA")
min <- min %>% 
  mutate(team_game_num = row_number())

mtl <- test_games %>%
  select(7,8,19,20)
colnames(mtl) <- c("GF", "GA", "SF", "SA")
mtl <- mtl %>% 
  mutate(team_game_num = row_number())

tor <- test_games %>%
  select(9,10,21,22)
colnames(tor) <- c("GF", "GA", "SF", "SA")
tor <- tor %>% 
  mutate(team_game_num = row_number())

ott <- test_games %>%
  select(11,12,23,24)
colnames(ott) <- c("GF", "GA", "SF", "SA")

pwhl <- bind_rows(bos = bos,
                  mtl = mtl,
                  tor = tor,
                  ott = ott,
                  nyc = nyc,
                  min = min,
                  .id = "source")

pwhl <- drop_na(pwhl) %>%
  filter(team_game_num != 48)

# Standings chart
standings <- standings %>%
  mutate(
    RW = replace_na(RW, 0),
    `OT/SOW` = replace_na(`OT/SOW`, 0),
    `OT/SOL` = replace_na(`OT/SOL`, 0),
    points = RW * 3 + `OT/SOW` * 2 + `OT/SOL` * 1
  ) %>%
  mutate(pointsper = round(points / (GP * 3), 3))

pw_table <- standings %>%
  arrange(desc(pointsper)) %>%
  select(Team, GP, points, pointsper,  GF, GA) %>%
  rename(
    `Pts` = points,
    `Per` = pointsper)

ggtexttable(
  pw_table,
  rows = NULL, # Suppress row names
  theme = ttheme("lViolet")) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
  tab_add_hline(at.row = c(7), row.side = "bottom", linewidth = 3, linetype = 1) %>%
  tab_add_hline(at.row = c(6), row.side = "top", linewidth = 3, linetype = 2, linecolor = "red")

# goal leaders
top_10_names <- goals %>%
  drop_na(G) %>%
  count(G, name = "count") %>%  # Count occurrences of each name in column G
  arrange(desc(count)) %>%      # Arrange in descending order of count
  slice_head(n = 10)            # Select the top 10 rows


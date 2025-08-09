library(googlesheets4)
library(tidyverse)
library(janitor)

attendance <- read_sheet('https://docs.google.com/spreadsheets/d/1VhbATB_uBUQdg5AmjQbLN692Nc-k3KTvGMeiyLPu54A/edit?gid=0#gid=0',
                         sheet = '2024-2025 Regular Season Games')
attendance <- attendance [1:12]
attendance$GameNum <- 1:nrow(attendance)
attendance <- attendance %>%
  clean_names()

ggplot(attendance, aes(x = game_num, y = running_total, color = home_team, group = home_team)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Running Total Attendance by Home Team",
       x = "Game Number",
       y = "Running Total",
       color = "Home Team") +
facet_wrap(~home_team)

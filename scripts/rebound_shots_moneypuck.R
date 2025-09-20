library(tidyverse)

# Download and extract the file from MoneyPuck
temp_file <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file(
  url = "https://peter-tanner.com/moneypuck/downloads/shots_2024.zip",
  destfile = temp_file,
  mode = "wb"
)

unzip(temp_file, exdir = temp_dir)

# Read the CSV file
shots_data <- read_csv(file.path(temp_dir, "shots_2024.csv"))

# Clean names
shots <- janitor::clean_names(shots_data)

# Clean the data
rebound_shots <- shots %>% 
  select(game_id, is_playoff_game, home_team_code, away_team_code, team_code, event, time, shooting_team_forwards_on_ice, 
         shooting_team_defencemen_on_ice, defending_team_forwards_on_ice, defending_team_defencemen_on_ice,
         last_event_category, time_since_last_event, goal) %>% 
  filter(last_event_category %in% c("MISS", "SHOT", "GOAL")) %>% # shots after a shot
  mutate(
    shooting_team = shooting_team_forwards_on_ice + shooting_team_defencemen_on_ice,
    defending_team = defending_team_forwards_on_ice + defending_team_defencemen_on_ice
  ) %>% 
  mutate(situation = str_c(shooting_team, " v ", defending_team)) %>% # not included inte data 
  select(-shooting_team, -shooting_team_forwards_on_ice, -shooting_team_defencemen_on_ice,
         -defending_team, -defending_team_forwards_on_ice, -defending_team_defencemen_on_ice) %>% 
  filter(time_since_last_event < 2) # define a rebound shot as occuring less that 2 seconds after previous shot

rebound_shots_regsea_team <- rebound_shots %>% 
  filter(is_playoff_game == 0) %>% 
  ggplot(aes(x = team_code)) +
  geom_bar()

rebound_goals_regsea_team <- rebound_shots %>% 
  filter(
    is_playoff_game == 0,
    goal == 1) %>%
  ggplot(aes(x = team_code, fill = team_code == "SEA")) + 
  geom_bar() +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "#95d7d8")) +
  labs(
    title = "Regular Season Rebound Goals",
    subtitle = "Goals less than 2 seconds after previous shot",
    x = "Team",
    y = "Goal Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

rebound_goals_regsea_team

rebound_shots_regsea_team <- rebound_shots %>% 
  filter(is_playoff_game == 0) %>%
  ggplot(aes(x = team_code, fill = team_code == "SEA")) + 
  geom_bar() +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "#95d7d8")) +
  labs(
    title = "Regular Season Rebound Shot Attempts",
    subtitle = "Attempts less than 2 seconds after previous shot",
    x = "Team",
    y = "Goal Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

rebound_shots_regsea_team

rebound_shots %>% 
  filter(is_playoff_game == 0) %>%
  ggplot(aes(x = fct_infreq(team_code), fill = team_code == "SEA")) + 
  geom_bar() +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "#95d7d8")) +
  labs(
    title = "Regular Season Rebound Shot Attempts",
    subtitle = "Attempts less than 2 seconds after previous shot",
    x = "Team",
    y = "Shot Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

rebound_shots %>% 
  filter(
    is_playoff_game == 0,
    situation == '5 v 5') %>%
  ggplot(aes(x = fct_infreq(team_code), fill = team_code == "SEA")) + 
  geom_bar() +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "#95d7d8")) +
  labs(
    title = "2024-25 Regular Season 5v5 Rebound Shot Attempts",
    subtitle = "Attempts less than 2 seconds after previous shot.
    Data from MoneyPuck",
    x = "Team",
    y = "Shot Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

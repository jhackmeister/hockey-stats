library(tidyverse)
seakraken <- read.csv("https://moneypuck.com/moneypuck/playerData/careers/gameByGame/regular/teams/SEA.csv")

kraken <- seakraken %>% 
  select(1:8 ,situation, goalsFor, goalsAgainst, reboundsFor, reboundGoalsFor, reboundsAgainst, reboundGoalsAgainst) %>% 
  filter(situation == '5on5')

all_teams <- read.csv("https://moneypuck.com/moneypuck/playerData/careers/gameByGame/all_teams.csv")

all_teams <- all_teams %>% 
  filter(season > 2020)

all_teams_all <- all_teams %>% 
  filter(situation == 'all') %>% 
  select(1:8 ,situation, goalsFor, goalsAgainst, reboundsFor, 
         reboundGoalsFor, reboundsAgainst, reboundGoalsAgainst)

rebounds <- all_teams_all %>% 
  group_by(team, season) %>% 
  summarise(
    total_rebound_goals_for = sum(reboundGoalsFor, na.rm = TRUE),
    total_rebound_goals_against = sum(reboundGoalsAgainst, na.rm = TRUE)
  )

rebounds_24 <- rebounds %>% 
  filter(season == 2024)

ggplot(rebounds_24, aes(
  x = total_rebound_goals_for,
  y = total_rebound_goals_against,
  color = team,
)) +
  geom_point(size = 3) +
  geom_text(aes(label = team), vjust = -0.5, size = 3) +
  scale_color_manual(values = team_colors_main) +
  geom_vline()
  theme_minimal()

  
ggplot(rebounds_24, aes(
    x = total_rebound_goals_for,
    y = total_rebound_goals_against,
    color = team
  )) +
    geom_point(size = 3) +
    geom_text(aes(label = team), vjust = -0.5, size = 3) +
    geom_vline(xintercept = 32, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 32, linetype = "dashed", color = "black") +
    scale_color_manual(values = team_colors_main) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = "2024-25 Rebound Goals by Team",
      subtitle = "Data from MoneyPuck",
      x = "Rebound Goals For",
      y = "Rebond Goals Against"
    ) +
  annotate("text", x = 25, y = 25, label = "Need More Finishers", color = "gray", size = 5) +
  annotate("text", x = 25, y = 42, label = "Clear Your Crease", color = "gray", size = 5) +
  annotate("text", x = 42, y = 25, label = "Opportunistic", color = "gray", size = 5) +
  annotate("text", x = 42, y = 42, label = "Chaos Agents", color = "gray", size = 5) 

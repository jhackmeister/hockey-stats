library(tidyverse)
thisyear <- read.csv("https://moneypuck.com/moneypuck/playerData/seasonSummary/2025/regular/teams.csv")
kraken <- read.csv("https://moneypuck.com/moneypuck/playerData/careers/gameByGame/regular/teams/SEA.csv")

kraken_all <- kraken %>% 
  filter(situation == "all") %>% 
  select(team, season, gameId, reboundsFor, reboundGoalsFor, reboundsAgainst,
         reboundGoalsAgainst, goalsFor, goalsAgainst) %>% 
  group_by(season) %>%
  arrange(gameId) %>%  # Ensure games are in chronological order
  mutate(cumulative_GF = cumsum(goalsFor),
         cumulative_GA = cumsum(goalsAgainst),
         goal_dff = cumulative_GF - cumulative_GA,
         games_played = row_number(),
         cumulative_rebounds_for = cumsum(reboundsFor),
         cumulative_rebound_goals_for = cumsum(reboundGoalsFor),
         cumulative_rebounds_against = cumsum(reboundsAgainst),
         cumulative_rebound_goals_against = cumsum(reboundGoalsAgainst),
         rebound_diff = cumulative_rebounds_for - cumulative_rebounds_against,
         rebound_goal_diff = cumulative_rebound_goals_for - 
           cumulative_rebound_goals_against) %>%
  mutate(season = as.factor(season)) %>% 
  ungroup()

# Define colors
season_colors <- rep("#99d9d9", length(unique(kraken_all$season)))
names(season_colors) <- unique(kraken_all$season)
season_colors["2022"] <- "#001628"  # playoff season
season_colors["2025"] <- "#e9072b"  # current season

ggplot(kraken_all, aes(x = games_played, y = rebound_diff, color = season)) +
  geom_line(linewidth = 1.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = season_colors) +
  theme_minimal() +
  labs(
    title = "Kraken Running Rebound Differential by Season",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Rebound Differential"
  )


label_data <- kraken_all %>%
  group_by(season) %>%
  slice_tail(n = 1)  # last point for each season


ggplot(kraken_all, aes(x = games_played, y = rebound_goal_diff, color = season)) +
  geom_line(linewidth = 1.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(
    data = label_data,
    aes(label = season),
    hjust = -0.1,  # push slightly to the right
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = season_colors) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Kraken Running Rebound Goal Differential by Season",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Rebound Goal Differential"
  )

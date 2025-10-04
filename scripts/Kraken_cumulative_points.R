library(tidyverse)

# data from Hockey Reference 
kraken_games <- kraken_game_data %>%
  mutate(HomeAway = if_else(is.na(HomeAway) | HomeAway == "", "home", "away")) %>%
  mutate(
    Points = case_when(
      Result == "W" ~ 2,
      !is.na(OT) & OT != "" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(Season) %>%
  mutate(RunningPoints = cumsum(Points)) %>%
  ungroup() %>% 
  mutate(Season = as.factor(Season))

### Plot results by season ###

#set colors 
season_colors <- rep("#99d9d9", length(levels(kraken_games$Season)))
names(season_colors) <- levels(kraken_games$Season)
season_colors["23"] <- "#001628" # playoff season 
season_colors["26"] <- "#e9072b" # current season

# create the plot
ggplot(kraken_games, aes(x=GP, y=RunningPoints, color = Season)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = season_colors) +
  theme_minimal() + 
  labs(
    title = "Kraken Running Points by Season",
    subtitle = "Data from Hockey Reference",
    xlab = "Games Played",
    ylab = "Points Total"
  )


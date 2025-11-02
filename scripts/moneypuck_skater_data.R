library(tidyverse)

skaters25 <- read.csv("https://moneypuck.com/moneypuck/playerData/seasonSummary/2025/regular/skaters.csv") %>% 
  mutate(last_name = word(name, -1))

glimpse(skaters25)

kraken_pp <- skaters25 %>% 
  filter(
    situation == "5on4",
    team == "SEA"
  ) %>% 
  select(name, team, gameScore, icetime, onIce_xGoalsPercentage) %>% 
  arrange(desc(gameScore))
kraken_pp

kraken_rebounds <- skaters25 %>% 
  filter(
    situation == "5on5", 
    team == "SEA") %>% 
  select(name, icetime, OnIce_F_rebounds, I_F_rebounds, OnIce_A_xGoalsFromxReboundsOfShots) %>% 
  arrange(desc(OnIce_A_xGoalsFromxReboundsOfShots))
kraken_rebounds

##################
# Opponent Comps #
##################

# 5v5 Game Score
skaters25 %>% 
  filter(
    situation == "5on5",
    team %in% c("SEA", "MTL"),
    games_played > 4
  ) %>% 
  select(last_name, team, gameScore, icetime) %>% 
  ggplot(aes(x=gameScore, y = icetime, color = team)) +
  geom_point() +
  geom_text(aes(label = last_name), vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(color = "#001628"),
    plot.subtitle = element_text(color = "#355464"), 
    plot.caption = element_text(color = "#68a2b9")
  ) +
  labs(
    title = "Tonight's Game - Montreal at Seattle",
    subtitle = "5v5 Icetime and Game Score - Data from MoneyPuck",
    x = "YTD Game Score",
    y = "YTD Ice Time",
    caption = "@jhackmeister.bsky.social"
  )

# 5v5 xGoals
skaters25 %>% 
  filter(
    situation == "5on5",
    team %in% c("SEA", "MTL"),
    games_played > 4
  ) %>% 
  select(last_name, team, OnIce_F_xGoals, OnIce_A_xGoals, games_played) %>% 
  ggplot(aes(x=OnIce_F_xGoals, y = OnIce_A_xGoals, color = team)) +
  geom_point() +
  geom_text(aes(label = last_name), vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(color = "#001628"),
    plot.subtitle = element_text(color = "#355464"), 
    plot.caption = element_text(color = "#68a2b9")
  ) +
  labs(
    title = "Tonight's Game - Montreal at Seattle",
    subtitle = "5v5 On Ice xGoals - Data from MoneyPuck",
    x = "xGoals For",
    y = "xGoals Against",
    caption = "@jhackmeister.bsky.social"
  )

# PP xGoals and ice time
skaters25 %>% 
  filter(
    situation == "5on4",
    team %in% c("SEA", "MTL")
  ) %>% 
  select(last_name, team, OnIce_F_xGoals, icetime) %>% 
  ggplot(aes(x=OnIce_F_xGoals, y = icetime, color = team)) +
  geom_point() +
  geom_text(aes(label = last_name), vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(color = "#001628"),
    plot.subtitle = element_text(color = "#355464"), 
    plot.caption = element_text(color = "#68a2b9")
  ) +
  labs(
    title = "Tonight's Game - Edmonton at Seattle",
    subtitle = "Power Play xGoals - Data from MoneyPuck",
    y = "xGoals For",
    x = "Ice Time",
    caption = "@jhackmeister.bsky.social"
  )

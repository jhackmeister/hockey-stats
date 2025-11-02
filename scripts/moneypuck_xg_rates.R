library(tidyverse)
library(ggimage)

moneypuck2025 <- read.csv("https://moneypuck.com/moneypuck/playerData/seasonSummary/2025/regular/teams.csv")

logos <- read.csv("data/nhl_logos.csv") %>% 
  janitor::clean_names() %>% 
  filter(team != "ARI")


this_year <- moneypuck2025 %>% 
  select(team, situation,xGoalsPercentage, xGoalsFor, xGoalsAgainst, corsiPercentage, faceOffsWonFor,
         faceOffsWonAgainst, highDangerShotsFor, highDangerGoalsFor, highDangerShotsAgainst, 
         highDangerGoalsAgainst, reboundsFor, reboundsAgainst, reboundGoalsFor, reboundGoalsAgainst,
         goalsFor, goalsAgainst, iceTime, games_played, shotsOnGoalFor, shotsOnGoalAgainst) %>% 
  mutate(
    goals_for_p60 = round((goalsFor/iceTime) * 60, digits =3),
    goals_against_p60 = round((goalsAgainst/iceTime) * 60, digits = 3),
    xgoals_for_p60 = round((xGoalsFor/iceTime) * 60, digits = 3),
    xgoals_against_p60 = round((xGoalsAgainst/iceTime) * 60, digits = 3),
    shooting_perc = round(goalsFor/shotsOnGoalFor, digits = 4),
    save_perc = round(1-(goalsAgainst/shotsOnGoalAgainst), digits = 4),
    pdo = (save_perc + shooting_perc) * 100
  ) %>% 
  left_join(logos, by = "team") %>% 
  janitor::clean_names()

this_year %>% 
  filter(situation == "5on5") %>% 
  ggplot(aes(x = xgoals_for_p60, y =xgoals_against_p60)) +
           geom_image(aes(image = logo), size = 0.05) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
           theme(plot.title = element_text(color = "#68a2b9", face = 'bold'),
                         plot.caption = element_text(color = "#355464", face = 'bold')) +
  labs(
    title = "2025-26 5v5 Expected Goals per 60 Minutes",
    subtitle = "Data from MoneyPuck, Logos from SportsLogos.Net",
    x = "Expected Goals For per 60",
    y= "Expected Goals Against per 60", 
    caption = "@jhackmeister.bsky.social"
  )

this_year %>% 
  filter(situation == "5on5") %>% 
  ggplot(aes(x = shooting_perc, y =save_perc)) +
  geom_image(aes(image = logo), size = 0.05) +
  theme_minimal() +
  theme(plot.title = element_text(color = "#68a2b9", face = 'bold'),
        plot.caption = element_text(color = "#355464", face = 'bold')) +
  labs(
    title = "2025-26 PDO Plot",
    subtitle = "Data from MoneyPuck, Logos from SportsLogos.Net",
    x = "Shooting Percentage",
    y= "Save Percentage",
    caption = "@jhackmeister.bsky.social"
  )

pdo <- this_year %>%
  filter(situation == "5on5") %>% 
  select(team, pdo) %>% 
  arrange(desc(pdo))
pdo

xg <- this_year %>% 
  filter(situation == "5on5") %>% 
  select(team, xgoals_for_p60, xgoals_against_p60) %>% 
  arrange(desc(xgoals_against_p60))
xg

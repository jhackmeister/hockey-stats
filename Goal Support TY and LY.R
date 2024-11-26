library(tidyverse)

#data from Natural Stat Trick https://www.naturalstattrick.com/playerteams.php?fromseason=20242025&thruseason=20242025&stype=2&sit=all&score=all&stdoi=oi&rate=n&team=ALL&pos=G&loc=B&toi=0&gpfilt=none&fd=&td=&tgp=410&lines=multi&draftteam=ALL

# This seasons data
ty <- read.csv("NST_20242025.csv")

ty <- ty %>%
  filter(GP > 3) %>%  # Move the filter first
  mutate(
    GFperGP = round(GF / GP, 2),
    xGFperGP = round(xGF / GP, 2)
  )
hist(ty$GP)

GFplot <- ggplot(ty, aes(x = reorder(paste(Player, "(", Team, GP, ")", sep = " "), GFperGP), y = GFperGP, fill = ifelse(Team == "SEA", "SEA", "other"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("SEA" = "#68A2B9", "other" = "gray")) +
  coord_flip() +
  labs(title = "Goal Support", x = "Goalie (Team, Games Played)", y = "Goals Scored per Game", subtitle = "Minimum 4 GP") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate("text", x = 6, y = 4 , label = "Data from Natural Stat Trick 2024-11-16", color = "darkred", size = 3) 

GFplot


xGFplot <- ggplot(ty, aes(x = reorder(paste(Player, "(", Team, GP, ")", sep = " "), xGFperGP), y = xGFperGP, fill = ifelse(Team == "SEA", "SEA", "other"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("SEA" = "#68A2B9", "other" = "gray")) +
  coord_flip() +
  labs(title = "Expected Goals For per Game", x = "Goalie (Team, Games Played", y = " Expected Goals For per Game", subtitle = "Data from Natural Stat Trick") +
  theme_minimal() +
  theme(legend.position = "none")

xGFplot

#2023-2024 season
stats <- read.csv("NST_20232024.csv")

twentygames <- stats %>%
  filter(GP > 20)

#gf per game
twentygames <- twentygames %>%
  mutate(GFpergame = round(GF / GP, 2)) %>%
  arrange(desc(GFpergame))

# Last Season Charts
ggplot(twentygames, aes(x=Player, y=GFpergame)) +
  geom_bar(stat="identity")+
  coord_flip()

ggplot(twentygames, aes(x = reorder(Player, GFpergame), y = GFpergame)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "GF per Game by Player", x = "Player", y = "GF per Game")

ggplot(twentygames, aes(x = reorder(Player, GFpergame), y = GFpergame, fill = ifelse(Team == "SEA", "SEA", "Other"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("SEA" = "red", "Other" = "gray")) + # Colors for SEA and other teams
  coord_flip() +
  labs(title = "Goal Support by Goalie 2023-2024", x = "Goalie", y = "Goals Scored per Game", subtitle = "Minimum 20 Games Played") +
  theme_minimal() +  # Optional: cleaner theme
  theme(legend.position = "none")  # This removes the legend
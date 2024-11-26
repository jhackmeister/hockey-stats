# xGF analysis 

library(tidyverse)
library(teamcolors)
library(ggimage)

# Data from Natural Stat Trick https://www.naturalstattrick.com/teamtable.php?fromseason=20242025&thruseason=20242025&stype=2&sit=5v5&score=all&rate=n&team=all&loc=B&gpf=410&fd=&td=
nst_teams <- read.csv('2024.25_NST_teams.csv')

# Create nhl_teams df 
nhl_teams <- teamcolors %>%
  filter(league == "nhl")

# Fix Blues
nhl_teams <- nhl_teams %>%
  mutate(name = ifelse(name == "St. Louis Blues", "St Louis Blues", name))

# Get team logo URLs from the teamcolors dataset
nhl_logos <- nhl_teams %>%
  dplyr::select(name, primary, logo)

# Manually add SEA and UTA
new_rows <- data.frame(
  name = c("Seattle Kraken", "Utah Hockey Club"),
  primary = c("#001628", "#71AFE5"),  # Replace with the desired color codes
  logo = c("https://content.sportslogos.net/logos/1/6740/full/seattle_kraken_logo_primary_20226314.png", "https://content.sportslogos.net/logos/1/6902/full/utah_hockey_club_logo_primary_2025_sportslogosnet-8095.png")  # Replace with the actual file paths or URLs to logos
)

nhl_logos <- rbind(nhl_logos, new_rows)

# Add a column to indicate whether a team has a logo
nst_teams <- nst_teams %>%
  dplyr::left_join(nhl_logos, by = c("Team" = "name")) %>%
  dplyr::mutate(has_logo = !is.na(logo))

head(nst_teams)

# Add Logos or labels for teams
ggplot(nst_teams, aes(x = xGF., y = GF.)) +
  geom_image(data = subset(nst_teams, has_logo), aes(image = logo), size = 0.05) +  # Plot logos
  geom_text(data = subset(nst_teams, !has_logo), aes(label = Team), color = "black", size = 3, hjust = 0.5, vjust = -0.5) +
  geom_vline(xintercept = 50, color = "black", linetype = "solid") +
  geom_hline(yintercept = 50, color = "black", linetype = "solid") +
  labs(x = "Expected Goals For %",
       y = "Goals For %") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate("text", x = 60, y = 60, label = "Fun", color = "darkorange2", size = 5) +
  annotate("text", x = 58, y = 35, label = "Can't Finish", color = "darkorange2", size = 5) +
  annotate("text", x = 43, y = 60, label = "Snipers", color = "darkorange2", size = 5) +
  annotate("text", x = 43, y = 35, label = "Boring", color = "darkorange2", size = 5) +
  annotate("text", x = 45, y = 30, label = paste("Data from Natural Stat Trick as of:", Sys.Date()), color = "darkred", size = 3)

# Points % 
ggplot(nst_teams, aes(x = Point.., y = CF.)) +
  geom_image(data = subset(nst_teams, has_logo), aes(image = logo), size = 0.05) +  # Plot logos
  geom_text(data = subset(nst_teams, !has_logo), aes(label = Team), color = "black", size = 3, hjust = 0.5, vjust = -0.5) +
  geom_hline(yintercept = 50, color = "black", linetype = "solid") +
  geom_vline(xintercept = 0.5, color = 'black', linetype = "solid") +
  labs(x = "Points %",
       y = "Corsi For %") +
  theme_minimal() +
  theme(legend.position = "none")


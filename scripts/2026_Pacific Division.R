###########################
# Pacific Division Scrape #
###########################
library(rvest)
library(tidyverse)
library(ggimage)

# Function to scrape one team's season
scrape_season <- function(team) {
  url <- paste0("https://www.hockey-reference.com/teams/", team, "/2026_games.html") 
  page <- read_html(url)
  
  table <- page %>%
    html_node("table#games") %>%
    html_table() %>%
    as_tibble(.name_repair = "unique") %>%
    mutate(team = team)  # Add team column for reference
  
  return(table)
}

# Vector of teams
pac_teams <- c("ANA", "CGY", "EDM", "LAK", "SJS", "SEA", "VAN", "VEG")

# Combine all teams into one data frame
pacific_div <- map_df(pac_teams, scrape_season)

# Clean up
pac_div <- pacific_div %>% 
  rename(
    location = ...4,
    result = ...8,
    game_length = ...9
  ) %>% 
  filter(!is.na(GF)) %>% 
  mutate(team = ifelse(team == "VEG", "VGK", team)) %>% 
  mutate(
    location = ifelse(location == "@", "away", "home"),
    game_length = ifelse(game_length == "", "REG", game_length)) %>%
  mutate(
    Points = case_when(
      result == "W" ~ 2,
      !is.na(game_length) & game_length != "REG" ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  mutate(
    game_date = ymd(Date),  # Convert to Date format
    season = if_else(
      month(game_date) >= 10,
      paste0(year(game_date), "–", substr(year(game_date) + 1, 3, 4)),
      paste0(year(game_date) - 1, "–", substr(year(game_date), 3, 4))
    )
  ) %>% 
  group_by(team) %>%
  arrange(game_date) %>%  # Ensure games are in chronological order
  mutate(cumulative_points = cumsum(Points),
         cumulative_GF = cumsum(GF),
         cumulative_GA = cumsum(GA),
         goal_dff = cumulative_GF - cumulative_GA,
         points_perc = cumulative_points/(GP*2),
         ppg = cumulative_points - GP) %>%
  ungroup() %>% 
  filter(!is.na(GF)) %>% 
  janitor::clean_names()


# Prep team colors for plot
team_color_vector <- read.csv("data/team_primary_colors.csv")
team_colors <- setNames(team_color_vector$main_color, team_color_vector$team)
logos <- read.csv("data/nhl_logos.csv")
team_logos <- setNames(logos$logo, logos$Team)


label_data <- pac_div %>%
  group_by(team) %>%
  slice_tail(n = 1) %>%  # last row for each team
  mutate(image = team_logos[team])

### Plots ###

ggplot(pac_div, aes(x = gp, y = cumulative_points, color = team)) +
  geom_line(linewidth = 1.3) +
  geom_image(
    data = label_data,
    aes(x = gp, y = cumulative_points, image = image),
    size = 0.05,  # adjust logo size
    by = "width",
    asp = 1.0,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = team_colors) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "2025-26 Pacific Division Standings",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Points Total"
  )

ggplot(pac_div, aes(x = gp, y = points_perc, color = team)) +
  geom_line(linewidth = 1.3) +
  geom_image(
    data = label_data,
    aes(x = gp, y = points_perc, image = image),
    size = 0.05,  # adjust logo size
    by = "width",
    asp = 1.0,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  scale_color_manual(values = team_colors) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "2025-26 Pacific Division Standings by Points Percentage",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Points Total"
  )

ggplot(pac_div, aes(x = gp, y = ppg, color = team)) +
  geom_line(linewidth = 1.3) +
  geom_image(
    data = label_data,
    aes(x = gp, y = ppg, image = image),
    size = 0.05,  # adjust logo size
    by = "width",
    asp = 1.0,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  scale_color_manual(values = team_colors) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "2025-26 Pacific Division Standings",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Points Above Point Per Game Pace"
  )

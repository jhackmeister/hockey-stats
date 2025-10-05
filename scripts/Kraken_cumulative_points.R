library(rvest)
library(tidyverse)

##########################
# Multiple Season Scrape #
##########################

# Define the function to scrape one season
scrape_season <- function(year) {
  url <- paste0("https://www.hockey-reference.com/teams/SEA/", year, "_games.html")
  page <- read_html(url)
  
  table <- page %>%
    html_node("table#games") %>%
    html_table() %>%
    as_tibble(.name_repair = "unique") 
  return(table)
}

# Vector of seasons 
seasons <- 2022:2025

# Use purrr::map_df to apply the function and bind results into one data frame
all_kraken_games <- map_df(seasons, scrape_season)

##########################
## Clean the table data ##
##########################

all_kraken_games <- all_kraken_games %>% 
  rename(
    location = ...3,
    result = ...7,
    game_length = ...8
  ) %>% 
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
  group_by(season) %>%
  arrange(game_date) %>%  # Ensure games are in chronological order
  mutate(cumulative_points = cumsum(Points)) %>%
  ungroup() %>% 
  janitor::clean_names()

##########################
##### Plot the data ######
##########################

# Define colors
season_colors <- rep("#99d9d9", length(unique(all_kraken_games$season)))
names(season_colors) <- unique(all_kraken_games$season)
season_colors["2022–23"] <- "#001628"  # playoff season
season_colors["2025–26"] <- "#e9072b"  # current season

# Create the plot
ggplot(all_kraken_games, aes(x = gp, y = cumulative_points, color = season)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = season_colors) +
  theme_minimal() +
  labs(
    title = "Kraken Running Points by Season",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Points Total"
  )

###########################
# Pacific Division Scrape #
###########################

# Function to scrape one team's season
scrape_season <- function(team) {
  url <- paste0("https://www.hockey-reference.com/teams/", team, "/2025_games.html") 
  page <- read_html(url)
  
  table <- page %>%
    html_node("table#games") %>%
    html_table() %>%
    as_tibble(.name_repair = "unique") %>%
    mutate(team = team)  # Add team column for reference
  
  return(table)
}

# Vector of teams
teams <- c("ANA", "CGY", "EDM", "LAK", "SJS", "SEA", "VAN", "VEG")

# Combine all teams into one data frame
pacific_div <- map_df(teams, scrape_season)

# Clean up
pac_div <- pacific_div %>% 
  rename(
    location = ...3,
    result = ...7,
    game_length = ...8
  ) %>% 
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
  mutate(cumulative_points = cumsum(Points)) %>%
  ungroup() %>% 
  janitor::clean_names()


# Prepare data for labels: last game per team
label_data <- pac_div %>%
  group_by(team) %>%
  filter(gp == max(gp)) %>%
  ungroup()

# Create the plot
ggplot(pac_div, aes(x = gp, y = cumulative_points, color = team)) +
  geom_line(linewidth = 1.3) +
  geom_text(
    data = label_data,
    aes(label = team),
    hjust = -0.1,  # Push labels slightly to the right
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(values = team_color_vector) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "2024-25 Pacific Division Standings",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Points Total"
  ) +
  geom_hline(yintercept = 96, color = "red", linetype = "dashed")+
  annotate("text", x = 2, y = 94, label = "2024-25 Playoff Cutoff", color = "red", size = 3) +
  xlim(0, max(pac_div$gp) + 5)  # Add space for labels


library(rvest)
library(tidyverse)

# Scrape 2026 season
# URL of the page
url <- "https://www.hockey-reference.com/teams/SEA/2026_games.html"

# Read the HTML content
page <- read_html(url)

# Extract the table (it's the first table on the page)
kraken_table <- page %>%
  html_element("table#games") %>%  # The table has id="games"
  html_table()

names(kraken_table)[names(kraken_table) == ""] <- c("Location", "Result", "Length")

kraken_table <- kraken_table %>% 
  filter(!is.na(GF)) %>% 
  mutate(Season = 2026) %>% 
  select(-Time, -Notes) 

kraken_history <- read_excel("data/Kraken_seasons.xlsx", sheet = "allgames")

kraken_history <- kraken_history %>%
  mutate(
    GP = as.integer(GP),
    Date = as.character(Date),
    GF = as.integer(GF),
    GA = as.integer(GA),
    W = as.integer(W),
    L = as.integer(L),
    OL = as.integer(OL),
    Att. = as.character(Att.),
    LOG = as.character(LOG)
    )


# Combine safely
kraken_all <- bind_rows(kraken_history, kraken_table)


##########################
## Clean the table data ##
##########################

all_kraken_games <- kraken_all %>% 
  mutate(
    Points = case_when(
      Result == "W" ~ 2,
      !is.na(Length) & Length != "REG" ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  group_by(Season) %>%
  arrange(Date) %>%  # Ensure games are in chronological order
  mutate(cumulative_points = cumsum(Points),
         cumulative_GF = cumsum(GF),
         cumulative_GA = cumsum(GA),
         goal_dff = cumulative_GF - cumulative_GA,
         points_perc = cumulative_points/(GP*2)) %>%
  ungroup() %>% 
  filter(!is.na(GF)) %>% 
  mutate(Season = as.factor(Season)) %>% 
  janitor::clean_names()

##########################
##### Plot the data ######
##########################

# Define colors
season_colors <- rep("#99d9d9", length(unique(all_kraken_games$season)))
names(season_colors) <- unique(all_kraken_games$season)
season_colors["2023"] <- "#001628"  # playoff season
season_colors["2026"] <- "#e9072b"  # current season

# Create the plot for points
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

# Plot goal diff 
ggplot(all_kraken_games, aes(x = gp, y = goal_dff, color = season)) +
  geom_line(linewidth = 1.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = season_colors) +
  theme_minimal() +
  labs(
    title = "Kraken Running Goal Differential by Season",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Differential"
  )

# Points percentage
ggplot(all_kraken_games, aes(x = gp, y = points_perc, color = season)) +
  geom_line(linewidth = 1.3) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  scale_color_manual(values = season_colors) +
  theme_minimal() +
  labs(
    title = "Kraken Running Points Percentage by Season",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Points Percentage"
  )

###########################
# Pacific Division Scrape #
###########################

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
  mutate(cumulative_points = cumsum(Points)) %>%
  ungroup() %>% 
  janitor::clean_names()


# Prepare data for labels: last game per team
label_data <- pac_div %>%
  group_by(team) %>%
  filter(gp == max(gp)) %>%
  ungroup()

# Prep team colors for plot
team_color_vector <- read.csv("data/team_primary_colors.csv")
team_colors <- setNames(team_color_vector$main_color, team_color_vector$team)


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
  scale_color_manual(values = team_colors) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "2025-26 Pacific Division Standings",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Points Total"
  ) #+
  #geom_hline(yintercept = 96, color = "red", linetype = "dashed")+
  #annotate("text", x = 2, y = 94, label = "2024-25 Playoff Cutoff", color = "red", size = 3) +
  #xlim(0, max(pac_div$gp) + 5)  # Add space for labels

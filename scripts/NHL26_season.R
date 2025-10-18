library(rvest)
library(tidyverse)

#########################
###### Data Scrape ######
#########################

# Function to scrape one team's season
scrape_season <- function(team) {
  url <- paste0("https://www.hockey-reference.com/teams/", team, "/2026_games.html") 
  page <- read_html(url)
  
  table <- page %>%
    html_node("table#games") %>%
    html_table() %>%
    mutate(team = team)  # Add team column for reference
  
  return(table)
}

# All Teams
nhl_teams <- tibble::tribble(
  ~team_code, ~full_name, ~conference, ~division, ~div_rank, ~playoffs,
  "ANA", "Anaheim Ducks", "Western", "Pacific", 8, 0,
  "BOS", "Boston Bruins", "Eastern", "Atlantic", 8, 0,
  "BUF", "Buffalo Sabres", "Eastern", "Atlantic", 7, 0,
  "CAR", "Carolina Hurricanes", "Eastern", "Metro", 2, 1,
  "CGY", "Calgary Flames", "Western", "Pacific", 4, 0,
  "CHI", "Chicago Blackhawks", "Western", "Central", 8, 0,
  "COL", "Colorado Avalanche", "Western", "Central", 3, 1,
  "CBJ", "Columbus Blue Jackets", "Eastern", "Metro", 4, 1,
  "DAL", "Dallas Stars", "Western", "Central", 2, 1,
  "DET", "Detroit Red Wings", "Eastern", "Atlantic", 6, 0,
  "EDM", "Edmonton Oilers", "Western", "Pacific", 3, 1,
  "FLA", "Florida Panthers", "Eastern", "Atlantic", 3, 1,
  "LAK", "Los Angeles Kings", "Western", "Pacific", 2, 1,
  "MIN", "Minnesota Wild", "Western", "Central", 4, 0,
  "MTL", "Montreal Canadiens", "Eastern", "Atlantic", 5, 1,
  "NSH", "Nashville Predators", "Western", "Central", 7, 0,
  "NJD", "New Jersey Devils", "Eastern", "Metro", 3, 1,
  "NYI", "New York Islanders", "Eastern", "Metro", 6, 0,
  "NYR", "New York Rangers", "Eastern", "Metro", 5, 0,
  "OTT", "Ottawa Senators", "Eastern", "Atlantic", 4, 1,
  "PHI", "Philadelphia Flyers", "Eastern", "Metro", 8, 0,
  "PIT", "Pittsburgh Penguins", "Eastern", "Metro", 7, 0,
  "STL", "St. Louis Blues", "Western", "Central", 5, 1,
  "SJS", "San Jose Sharks", "Western", "Pacific", 8, 0,
  "SEA", "Seattle Kraken", "Western", "Pacific", 7, 0,
  "TBL", "Tampa Bay Lightning", "Eastern", "Atlantic", 2, 1,
  "TOR", "Toronto Maple Leafs", "Eastern", "Atlantic", 1, 1,
  "UTA", "Utah Mammoth", "Western", "Central", 6, 0,
  "VAN", "Vancouver Canucks", "Western", "Pacific", 5, 0,
  "VEG", "Vegas Golden Knights", "Western", "Pacific", 1, 1,
  "WAS", "Washington Capitals", "Eastern", "Metro", 1, 1,
  "WPG", "Winnipeg Jets", "Western", "Central", 1, 1,
)

# Divisions 
pac <- nhl_teams %>% 
  filter(division == "Pacific")

cen <- nhl_teams %>% 
  filter(division == "Central")

met<- nhl_teams %>% 
  filter(division == "Metro")

atl <- nhl_teams %>% 
  filter(division == "Atlantic")

pac_teams <- pac$team_code
cen_teams <- cen$team_code
met_teams <- met$team_code
atl_teams <- atl$team_code

# Combine all teams into one data frame
pac26 <- map_df(pac_teams, scrape_season)

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

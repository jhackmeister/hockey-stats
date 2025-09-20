
library(tidyverse)

nhl_teams <- tibble::tribble(
  ~team_code, ~full_name, ~conference, ~division, ~lydiv_rank, ~playoffs,
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
  "UHC", "Utah Hockey Club", "Western", "Central", 6, 0,
  "VAN", "Vancouver Canucks", "Western", "Pacific", 5, 0,
  "VGK", "Vegas Golden Knights", "Western", "Pacific", 1, 1,
  "WAS", "Washington Capitals", "Eastern", "Metro", 1, 1,
  "WPG", "Winnipeg Jets", "Western", "Central", 1, 1,
)

# Create named vectors for easy use in scale_fill_manual
pacific <- nhl_teams %>% 
  filter(division == "Pacific")

central <- nhl_teams %>% 
  filter(division == "Central")

metro <- nhl_teams %>% 
  filter(division == "Metro")

atlantic <- nhl_teams %>% 
  filter(division == "Atlantic")

west <- nhl_teams %>% 
  filter(conference == "Western")

east <- nhl_teams %>% 
  filter(conference == "Eastern")

playoff_teams <- nhl_teams %>% 
  filter(playoffs == 1)

# shoutout to beerett on github for the origial js script https://github.com/beerett/nhl-color/tree/master/src
library(tidyverse)

nhl_team_colors <- tibble::tribble(
  ~team, ~full_name, ~main_color, ~secondary_color, ~third_color, ~text_color,
  "ANA", "Anaheim Ducks", "#FC4C02", "#B6985A", "#CF45210", "white",
  "ARI", "Arizona Coyotes", "#98012E", "#e2d6b5", "#e2d6b5", "white",
  "BOS", "Boston Bruins", "#fcb514", "#000000", "#FFB81C", "black",
  "BUF", "Buffalo Sabres", "#003087", "#FFB81C", "#FFB81C", "white",
  "CAR", "Carolina Hurricanes", "#c8102e", "#333F48", "#A2AAAD", "white",
  "CGY", "Calgary Flames", "#C8102E", "#F1BE48", "#F1BE48", "white",
  "CHI", "Chicago Blackhawks", "#C8102E", "#CC8A00", "#FF6720", "white",
  "COL", "Colorado Avalanche", "#6F263D", "#236192", "#236192", "white",
  "CBJ", "Columbus Blue Jackets", "#041e42", "#C8102E", "#A2AAAD", "white",
  "DAL", "Dallas Stars", "#00843D", "#A2AAAD", "#A2AAAD", "white",
  "DET", "Detroit Red Wings", "#C8102E", "#FFFFFF", "#FFFFFF", "white",
  "EDM", "Edmonton Oilers", "#00205B", "#CF4520", "#CF4520", "white",
  "FLA", "Florida Panthers", "#C8102E", "#041E42", "#B9975B", "white",
  "LAK", "Los Angeles Kings", "#A2AAAD", "#000000", "#FFFFFF", "black",
  "MIN", "Minnesota Wild", "#154734", "#A6192E", "#EAAA00", "white",
  "MTL", "Montreal Canadiens", "#A6192E", "#001E62", "#001E62", "white",
  "NSH", "Nashville Predators", "#FFB81C", "#041E42", "#041E42", "black",
  "NJD", "New Jersey Devils", "#C8102E", "#000000", "#046A38", "white",
  "NYI", "New York Islanders", "#003087", "#FC4C02", "#FC4C02", "white",
  "NYR", "New York Rangers", "#0032A0", "#C8102E", "#C8102E", "white",
  "OTT", "Ottawa Senators", "#C8102E", "#B9975B", "#B9975B", "white",
  "PHI", "Philadelphia Flyers", "#CF4520", "#000000", "#FFFFFF", "white",
  "PIT", "Pittsburgh Penguins", "#FFB81C", "#000000", "#000000", "black",
  "STL", "St. Louis Blues", "#003087", "#FFB81C", "#FFB81C", "white",
  "SJS", "San Jose Sharks", "#006271", "#F4901E", "#F4901E", "white",
  "SEA", "Seattle Kraken", "#051C2C", "#9CDBD9", "#9CDBD9", "white",
  "TBL", "Tampa Bay Lightning", "#00205B", "#FFFFFF", "#FFFFFF", "white",
  "TOR", "Toronto Maple Leafs", "#00205B", "#FFFFFF", "#013E7F", "white",
  "UHC", "Utah Hockey Club", "#69b3e7", "#010101", "#69b3e7", "black",
  "UTA", "Utah Mammoth", "#69b3e7", "#010101", "#69b3e7", "black",
  "VAN", "Vancouver Canucks", "#00205B", "#00843D", "#00843D", "white",
  "VGK", "Vegas Golden Knights", "#B9975B", "#333F48", "#333F48", "black",
  "WAS", "Washington Capitals", "#C8102E", "#041E42", "#041E42", "white",
  "WPG", "Winnipeg Jets", "#041E42", "#004C97", "#004C97", "white"
)

# Create named vectors for easy use in scale_fill_manual
team_colors_main <- setNames(nhl_team_colors$main_color, nhl_team_colors$team)
team_colors_secondary <- setNames(nhl_team_colors$secondary_color, nhl_team_colors$team)
team_colors_accent <- setNames(nhl_team_colors$ice_blue, nhl_team_colors$team)

team_color_vector <- nhl_team_colors %>% 
  select(team, main_color)

write.csv2(team_color_vector, "data/team_primary_colors.csv")
write.csv2(nhl_team_colors, "data/team_colors.csv")

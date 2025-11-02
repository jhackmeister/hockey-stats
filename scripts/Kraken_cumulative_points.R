library(rvest)
library(tidyverse)
library(readxl)

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

kraken_colors <- c(
  "2022" = "#001628",
  "2023" = "#FF681D",
  "2024" = "#68a2b9",
  "2025" = "#99d9d9",
  "2026" = "#e9072b"
)

# Create the plot for points
ggplot(all_kraken_games, aes(x = gp, y = cumulative_points, color = season)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = kraken_colors) +
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
  scale_color_manual(values = kraken_colors) +
  theme_minimal() +
  labs(
    title = "Kraken Running Goal Differential by Season",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Differential",
    caption = "@jhackmeister.bsky.social"
  )


# Prepare data for labeling: get the last point of each season
label_data <- all_kraken_games %>%
  filter(gp < (nrow(kraken_table) + 2)) %>%
  group_by(season) %>%
  slice_max(order_by = gp, n = 1)

# Points percentage
ggplot(all_kraken_games, aes(x = gp, y = points_perc, color = season)) +
  geom_line(linewidth = 1.3) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  scale_color_manual(values = kraken_colors) +
  theme_minimal() +
  labs(
    title = "Kraken Running Points Percentage by Season",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Points Percentage",
    caption = "@jhackmeister.bsky.social"
  )

# Running Points TY + 5 games
all_kraken_games %>% 
  filter(gp < (nrow(kraken_table) + 5)) %>% 
ggplot(aes(x = gp, y = cumulative_points, color = season)) +
  geom_line(linewidth = 1.3) +
  geom_text(
    data = label_data,
    aes(label = season),
    hjust = -0.1,  # nudges label slightly to the right
    show.legend = FALSE
  ) +
  scale_color_manual(values = kraken_colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Kraken Point Totals by Season - TY GP +5",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Point Total",
    caption = "@jhackmeister.bsky.social"
  )

# full season 
ggplot(all_kraken_games, aes(x = gp, y = cumulative_points, color = season)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = kraken_colors) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal() +
  labs(
    title = "Kraken Point Totals by Season",
    subtitle = "Data from Hockey Reference",
    x = "Games Played",
    y = "Point Total",
    caption = "@jhackmeister.bsky.social"
  )

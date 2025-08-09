library(readxl)
library(lubridate)
library(ggplot2)
library(crosstable)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(shiny)

# start dashboard
dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

# view dashboard
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)

stats <- read_xlsx("HockeyStats.xlsx", sheet = 2)
str(stats)
attach(stats)
stats <- as.data.frame(stats)
names(stats)
names(stats) [1:15] <- c("date", "yr", "league", "season", "session", "team", "vs", 
                         "gf", "ga", "result", "pos", "goals", "assists", "points", "pim")

squidcolors <- c("#001628", "#99D9D9", "#68A2B9", "#E9072B")

stats <- stats %>%
  mutate(result = factor(result, levels = c("W", "L", "T")))
str(stats)

stats <- stats %>%
  mutate(month = month(date, label = TRUE))

# skahl 
skahl <- subset(stats, league == "SKAHL D")
str(skahl)

# squids 
squids <- subset(stats, team == "Squid Squad")
head(squids)

ggplot(skahl, aes(x=date, y=points, color=result)) +
  geom_point()


crosstable(skahl, c(gf,ga, result), by=team) %>%
  as_flextable()

squids <- subset(stats, team == "Squid Squad")
head(squids)

ggplot(squids, aes(x=result, fill=result)) + 
  geom_bar() +
  scale_fill_manual(values = c("#99D9D9", "#E9072B","#68A2B9")) +
  facet_wrap(~ session)

ggplot(squids, aes(x = gf, y = ga, shape = season, color = result)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # Add regression line
  scale_color_manual(values = c("#99D9D9", "#E9072B", "#68A2B9")) +
  labs(title = "Squid Squad Goals For vs. Goals Against", x = "Goals For", y = "Goals Against") +
  theme_minimal()

ggplot(squids, aes(x=goals, y=assists, shape=season, color=result)) +
  geom_point() + 
  scale_color_manual(values = c("#99D9D9", "#E9072B", "#68A2B9")) 

# Create the histogram with a smaller binwidth
ggplot(squids, aes(x = points)) +
  geom_histogram(binwidth = 1) +  # Smaller binwidth for closer bins
  labs(title = "Distribution of Points", x = "Points", y = "Count") +
  theme_minimal()

ggplot(stats, aes(x=month)) +
  geom_histogram(stat = "count", fill = "#99D9D9", color = "black") +
  labs(title = "Games Played by Month", x = "Month", y = "Count") +
  theme_minimal()

ggplot(skahl, aes(x=team)) +
  geom_histogram(stat = "count", fill = "#99D9D9", color = "black") +
  labs(title = "Games Played by Team", x = "Team", y = "games") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(skahl, aes(x=pos)) +
  geom_histogram(stat= "count", fill = "#99D9D9", color = "black") +
  labs(title = "Games Played by Position", x = "Position", y = "games") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

squids_sorted <- squids %>%
  mutate(vs = factor(vs, levels = unique(vs))) %>%
  group_by(vs) %>%
  summarise(total_points = sum(points)) %>%
  arrange(desc(total_points))

ggplot(squids_sorted, aes(x = vs, y = total_points)) +
  geom_bar(stat = "identity", fill = "#99D9D9", color = "black") +
  labs(title = "Points by Opponent", x = "Opponent", y = "Points") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


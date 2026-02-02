# file for myself to try out packages and functions - retained incase anyone is curious

library(DBI)
library(RPostgres)
library(dbplyr)
library(dplyr)
library(ggplot2)

# Secure credential handling (set these via in the .Renviron)
con <- dbConnect(
  Postgres(),
  host = Sys.getenv("PGHOST"),
  port = 5432,
  dbname = Sys.getenv("PGDATABASE"),
  user = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)

# Getting odds table
odds <- tbl(con, "odds1x2")

# Getting odds over time of one match from odds1x2 given id
event_id_target <- 1616854168 # Crystal Palace Vs Bournemouth

# Collect the match data into memory early
match_data <- odds |>
  filter(event_id == event_id_target) |>
  select(logged_time, league_name, home_team, away_team, 
         home_odds, draw_odds, away_odds, max_money_line) |>
  collect() # retrieves data into a local tibble

# Identify the closing favourite and get opening odds
closing_row <- match_data |>
  slice_max(logged_time, n = 1)

opening_row <- match_data |>
  slice_min(logged_time, n = 1)

# Determine which outcome closed as favourite (lowest odds = favourite)
fav_choice <- c("home_odds", "draw_odds", "away_odds")[
  which.min(c(closing_row$home_odds, closing_row$draw_odds, closing_row$away_odds))
]

# Get opening odds for the closing favourite
opening_fav_odds <- opening_row[[fav_choice]]

# Calculate percentage change over time
plot_data <- match_data |>
  mutate(
    fav_odds = case_when(
      fav_choice == "home_odds" ~ home_odds,
      fav_choice == "draw_odds" ~ draw_odds,
      fav_choice == "away_odds" ~ away_odds
    ),
    pct_change_fav_odds = (fav_odds - opening_fav_odds) / opening_fav_odds * 100
  )

# Create plot with conditional color based on max_money_line
ggplot(plot_data, aes(x = logged_time, y = pct_change_fav_odds)) +
  geom_line(aes(color = max_money_line), linewidth = 1) +
  scale_color_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(
    x = "Time",
    y = "Odds Change (%)",
    title = "Change over time of closing favourite odds",
    color = "Max Money Line"
  )

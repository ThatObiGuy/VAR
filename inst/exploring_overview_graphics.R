# Exploratory script: data coverage timeline and league distribution overview
library(DBI)
library(RPostgres)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

# Database connection
con <- dbConnect(
  Postgres(),
  host = Sys.getenv("PGHOST"),
  port = 5432,
  dbname = Sys.getenv("PGDATABASE"),
  user = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)

# Lazy table reference
results <- tbl(con, "results1")

# Fetch results data for overview analysis
results_data <- results |>
  select(event_id, league_name, starts, home_team, away_team) |>
  collect() |>
  mutate(starts = as.POSIXct(starts, tz = "UTC"))

### PLOT 1: DATA COVERAGE TIMELINE ###
# Histogram showing number of matches over time with adjustable binwidth
# Helps users understand temporal coverage and data freshness

# Create histogram of match dates
p_coverage <- ggplot(results_data, aes(x = starts)) +
  geom_histogram(
    binwidth = 7 * 24 * 60 * 60,  # Convert days to seconds for POSIXct
    fill = "steelblue", 
    alpha = 0.8,
    color = "white",
    linewidth = 0.3
  ) +
  theme_minimal() +
  labs(
    title = "Match Coverage Over Time",
    x = "Date",
    y = "Number of Matches"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Convert to interactive plotly
p_coverage_ly <- ggplotly(p_coverage) |>
  layout(
    hovermode = "closest"
  )

print(p_coverage_ly)

### PLOT 2: LEAGUE DISTRIBUTION ###
# Horizontal bar chart showing match count by league
# Gives quick sense of which leagues are most represented in the dataset

# Aggregate matches by league
league_distribution <- results_data |>
  group_by(league_name) |>
  summarise(
    match_count = n(),
    .groups = "drop"
  ) |>
  arrange(desc(match_count)) |>
  mutate(
    tooltip_text = paste0(
      league_name, "\n",
      "Matches: ", match_count, "\n",
      "Percentage: ", round(match_count / sum(match_count) * 100, 1), "%"
    )
  )

# Create horizontal bar chart (better than pie for comparing quantities)
p_leagues <- ggplot(league_distribution, aes(x = reorder(league_name, match_count), y = match_count, text = tooltip_text)) +
  geom_col(fill = "darkorange", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Match Distribution by League",
    subtitle = "Total number of matches per league",
    x = " ",
    y = "Number of Matches"
  )

# Convert to interactive plotly
p_leagues_ly <- ggplotly(p_leagues, tooltip = "text") |>
  layout(
    hovermode = "closest"
  )

print(p_leagues_ly)

try(dbDisconnect(con), silent = TRUE)

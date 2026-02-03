# Exploratory script: team-level performance vs implied probabilities from closing odds
library(DBI)
library(RPostgres)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(plotly)

# Database connection
con <- dbConnect(
  Postgres(),
  host = Sys.getenv("PGHOST"),
  port = 5432,
  dbname = Sys.getenv("PGDATABASE"),
  user = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)

# Lazy table references
odds <- tbl(con, "odds1x2")
results <- tbl(con, "results1")
entropy <- tbl(con, "entropy")

# Fetch closing odds snapshot per match with result and implied probabilities
closing_base <- odds |>
  group_by(event_id) |>
  filter(logged_time == max(logged_time)) |>
  ungroup() |>
  select(event_id, league_name, starts, home_team, away_team) |>
  inner_join(results |> select(event_id, result), by = "event_id") |>
  inner_join(entropy |> select(event_id, p_home, p_draw, p_away), by = "event_id") |>
  collect() |>
  mutate(starts = as.POSIXct(starts, tz = "UTC"))

# Map match result to points earned by each side
map_points <- function(result, side) { # 3 for a win, 1 for a draw and 0 for a loss
  case_when(
    result == "draw" ~ 1,
    side == "home" & result == "home_win" ~ 3,
    side == "away" & result == "away_win" ~ 3,
    TRUE ~ 0
  )
}

# Reshape to team-match level: each match becomes two rows (home + away)
team_long <- bind_rows(
  closing_base |>
    transmute(
      team = home_team,
      league_name,
      starts,
      side = "home",
      p_win = p_home,
      p_draw,
      actual_win = if_else(result == "home_win", 1, 0),
      actual_points = map_points(result, "home")
    ),
  closing_base |>
    transmute(
      team = away_team,
      league_name,
      starts,
      side = "away",
      p_win = p_away,
      p_draw,
      actual_win = if_else(result == "away_win", 1, 0),
      actual_points = map_points(result, "away")
    )
)

### CALIBRATION PLOT ###
# Assess whether market-implied win probabilities are well-calibrated
# at the team level. Each point is one team; x = avg implied P(win), y = actual win rate.
# Wilson score interval provides binomial confidence intervals.
calib_bins <- team_long |>
  group_by(team, league_name) |>
  summarise(
    n = n(),
    n_wins = sum(actual_win, na.rm = TRUE),
    avg_implied = mean(p_win, na.rm = TRUE),
    actual_rate = mean(actual_win, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    z = qnorm(0.975),
    p_hat = actual_rate,
    denominator = 1 + z^2 / n,
    center = (p_hat + z^2 / (2 * n)) / denominator,
    margin = z * sqrt((p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))) / denominator,
    ci_lower = pmax(0, center - margin),
    ci_upper = pmin(1, center + margin),
    tooltip_text = paste0(
      team, "\n",
      "Avg Implied: ", round(avg_implied, 2), "\n",
      "Actual: ", round(actual_rate, 2), "\n",
      "95% CI: [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]\n",
      "Matches: ", n
    )
  )

leagues <- unique(calib_bins$league_name)
color_map <- setNames(scales::hue_pal()(length(leagues)), leagues)

p_calibration <- plot_ly() |>
  add_segments(
    x = 0, xend = 1, y = 0, yend = 1,
    line = list(color = "grey50", dash = "dash", width = 1),
    showlegend = FALSE,
    hoverinfo = "skip"
  )

for (league in leagues) {
  league_data <- calib_bins |> filter(league_name == league)
  
  p_calibration <- p_calibration |>
    add_segments(
      data = league_data,
      x = ~avg_implied, xend = ~avg_implied,
      y = ~ci_lower, yend = ~ci_upper,
      line = list(color = color_map[league], width = 2),
      opacity = 0.4,
      showlegend = FALSE,
      legendgroup = league,
      hoverinfo = "skip"
    ) |>
    add_markers(
      data = league_data,
      x = ~avg_implied,
      y = ~actual_rate,
      marker = list(
        size = ~n * 2,
        color = color_map[league],
        sizemode = "diameter",
        sizeref = 0.5,
        line = list(width = 0)
      ),
      opacity = 0.9,
      name = league,
      legendgroup = league,
      text = ~tooltip_text,
      hoverinfo = "text"
    )
}

p_calibration <- p_calibration |>
  layout(
    title = list(text = "Calibration: Implied vs Actual Win Rate by Team"),
    xaxis = list(
      title = "Average Implied Win Probability",
      range = c(-0.05, 1.05),
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.1)"
    ),
    yaxis = list(
      title = "Actual Win Rate",
      range = c(-0.05, 1.05),
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.1)"
    ),
    legend = list(title = list(text = "League")),
    hovermode = "closest",
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p_calibration

### PERFORMANCE PLOT ###
# Cumulative residual points: actual points minus market-implied expected points.
# Random walk bounds: ±k*σ*√n where σ is std dev of single-match residuals.
perf_df <- team_long |>
  mutate(
    expected_points = 3 * p_win + 1 * p_draw,
    residual = actual_points - expected_points
  ) |>
  arrange(team, starts) |>
  group_by(team, league_name) |>
  mutate(
    match_number = row_number(),
    cum_residual = cumsum(ifelse(is.na(residual), 0, residual))
  ) |>
  ungroup() |>
  mutate(
    tooltip_text = paste0(
      team, "\n",
      "Date: ", format(starts, "%Y-%m-%d"), "\n",
      "Cumulative: ", round(cum_residual, 2)
    )
  )

residual_sd <- sd(perf_df$residual, na.rm = TRUE)
k <- 2 # How many sd's about the mean

match_range <- range(perf_df$match_number)
date_range <- range(perf_df$starts)
bounds_df <- data.frame(
  match_number = seq(match_range[1], match_range[2]),
  starts = seq(date_range[1], date_range[2], length.out = match_range[2] - match_range[1] + 1)
) |>
  mutate(
    upper_bound = k * residual_sd * sqrt(match_number),
    lower_bound = -k * residual_sd * sqrt(match_number)
  )

p_performance <- ggplot(perf_df, aes(x = starts, y = cum_residual, group = team, color = league_name, text = tooltip_text)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  geom_ribbon(
    data = bounds_df,
    aes(x = starts, ymin = lower_bound, ymax = upper_bound, group = 1),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.3
  ) +
  geom_line(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Cumulative Over/Underperformance vs Market",
    subtitle = "Grey ribbon: ±2σ√n random walk bounds",
    x = "Match Date",
    y = "Cumulative residual points",
    color = "League"
  )

ggplotly(p_performance, tooltip = "text")

try(dbDisconnect(con), silent = TRUE)

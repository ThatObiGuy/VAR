# Plotting helpers (pure; no Shiny dependencies). Some helpers accept a DB connection
# to retrieve metadata (e.g., leagues) using DBI safely when available.

# Deterministic color utilities -------------------------------------------------


# Internal: get the base R Set 1 qualitative palette
set1_palette <- function(n) {
  # grDevices::palette.colors provides ColorBrewer palettes, including Set 1
  grDevices::palette.colors(n, palette = "Set 1")
}

# Generate deterministic color for each key (e.g., league_id or name) using Set 1
# Mapping is deterministic by sorting unique keys and assigning the first n colors.
deterministic_palette <- function(keys) {
  u <- sort(unique(as.character(keys)), na.last = TRUE)
  n <- length(u)
  base_cols <- set1_palette(max(n, 1))
  if (n > length(base_cols)) {
    # Recycle if more groups than palette length
    cols <- rep(base_cols, length.out = n)
  } else {
    cols <- base_cols[seq_len(n)]
  }
  stats::setNames(cols, u)
}

# Build named vector of league colors using DB colors when available.
# Falls back to distinct leagues from results1 if a dedicated leagues table is absent.
get_league_colors <- function(db_con, prefer_db_colors = TRUE, key = c("league_id", "league_name")) {
  # Simple behavior: assign each league a color from base R Set 1, in order.
  # We ignore DB-provided colors and the key argument for simplicity.
  if (is.null(db_con)) {
    return(stats::setNames(character(0), character(0)))
  }
  
  leagues <- tryCatch({
    DBI::dbGetQuery(
      db_con,
      "SELECT DISTINCT league_name FROM results1 WHERE league_name IS NOT NULL ORDER BY league_name"
    )
  }, error = function(e) NULL)
  
  if (is.null(leagues) || nrow(leagues) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  
  lg <- as.character(leagues$league_name)
  n <- length(lg)
  pal <- set1_palette(max(n, 1))
  cols <- if (n <= length(pal)) pal[seq_len(n)] else rep(pal, length.out = n)
  stats::setNames(cols, lg)
}

# ggplot helper scales (do not drop levels to keep colors stable when subsetting)
scale_color_league <- function(values) ggplot2::scale_color_manual(values = values, drop = FALSE, name = "League")
scale_fill_league  <- function(values) ggplot2::scale_fill_manual(values = values, drop = FALSE, name = "League")

# Plotly helper to align colors to factor levels in data
league_plotly_colors <- function(levels, palette) {
  nm <- intersect(levels, names(palette))
  unname(palette[nm])
}

# Plot percentage change of closing favourite odds over time
plot_fav_odds_change <- function(df) {
  # Basic validation
  required_cols <- c("logged_time", "home_odds", "draw_odds", "away_odds")
  stopifnot(all(required_cols %in% names(df)))
  
  if (nrow(df) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_minimal() +
             ggplot2::labs(title = "No data"))
  }
  
  # Ensure logged_time is plottable
  if (!inherits(df$logged_time, "POSIXct")) {
    df$logged_time <- as.POSIXct(df$logged_time, tz = "UTC")
  }
  
  # Identify closing and opening rows
  closing_row <- dplyr::slice_max(df, logged_time, n = 1)
  opening_row <- dplyr::slice_min(df, logged_time, n = 1)
  
  # Determine favourite at close (min odds). If tie, break by lower opening odds.
  closing_vals <- c(
    home_odds = closing_row$home_odds[[1]],
    draw_odds = closing_row$draw_odds[[1]],
    away_odds = closing_row$away_odds[[1]]
  )
  min_close <- min(closing_vals, na.rm = TRUE)
  candidates <- names(closing_vals)[which(closing_vals == min_close)]
  
  if (length(candidates) > 1) {
    opening_vals <- c(
      home_odds = opening_row$home_odds[[1]],
      draw_odds = opening_row$draw_odds[[1]],
      away_odds = opening_row$away_odds[[1]]
    )[candidates]
    # Choose the one with the lowest opening odds among tied closers
    fav_choice <- names(opening_vals)[which.min(opening_vals)]
  } else {
    fav_choice <- candidates[[1]]
  }
  
  opening_fav_odds <- opening_row[[fav_choice]][[1]]
  
  plot_data <- dplyr::mutate(
    df,
    fav_odds = if (fav_choice == "home_odds") home_odds else if (fav_choice == "draw_odds") draw_odds else away_odds,
    pct_change_fav_odds = (fav_odds - opening_fav_odds) / opening_fav_odds * 100
  )
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = logged_time, y = pct_change_fav_odds)) +
    ggplot2::geom_line(ggplot2::aes(color = max_money_line), linewidth = 1) +
    ggplot2::scale_color_viridis_c(option = "viridis", na.value = "grey50") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Time", y = "Odds Change (%)",
      title = "Change over time of closing favourite odds",
      color = "Max Money Line"
    )
}

# Calibration plot for Teams view (plotly)
# - builds a plotly object showing implied vs actual win
plot_team_calibration <- function(calib_data, color_map = NULL) {
  # Validation
  req_cols <- c("league_name", "avg_implied", "actual_rate", "ci_lower", "ci_upper", "n", "tooltip_text")
  missing <- setdiff(req_cols, names(calib_data))
  if (length(missing) > 0) {
    stop(sprintf("calib_data is missing required columns: %s", paste(missing, collapse = ", ")))
  }
  if (!is.numeric(calib_data$avg_implied) || !is.numeric(calib_data$actual_rate)) {
    stop("avg_implied and actual_rate must be numeric.")
  }
  if (nrow(calib_data) == 0) {
    return(plotly::plot_ly())
  }
  # Unified legend color mapping
  leagues <- unique(calib_data$league_name)
  if (is.null(color_map)) {
    color_map <- stats::setNames(set1_palette(length(leagues)), leagues)
  } else {
    # Ensure all leagues have a color
    missing_keys <- setdiff(leagues, names(color_map))
    if (length(missing_keys) > 0) {
      stop(sprintf("color_map is missing colors for leagues: %s", paste(missing_keys, collapse = ", ")))
    }
  }
  p <- plotly::plot_ly() |>
    plotly::add_segments(
      x = 0, xend = 1, y = 0, yend = 1,
      line = list(color = "grey50", dash = "dash", width = 1),
      showlegend = FALSE,
      hoverinfo = "skip"
    )
  # Add one trace per league to control legend order/colors identically
  for (lg in leagues) {
    league_data <- dplyr::filter(calib_data, .data$league_name == lg)
    p <- p |>
      plotly::add_segments(
        data = league_data,
        x = ~avg_implied, xend = ~avg_implied,
        y = ~ci_lower, yend = ~ci_upper,
        line = list(color = color_map[[lg]], width = 2),
        opacity = 0.4,
        showlegend = FALSE,
        legendgroup = lg,
        hoverinfo = "skip"
      ) |>
      plotly::add_markers(
        data = league_data,
        x = ~avg_implied,
        y = ~actual_rate,
        marker = list(
          size = ~n * 2,
          color = color_map[[lg]],
          sizemode = "diameter",
          sizeref = 0.5,
          line = list(width = 0)
        ),
        opacity = 0.9,
        name = lg,
        legendgroup = lg,
        text = ~tooltip_text,
        hoverinfo = "text"
      )
  }
  p |>
    plotly::layout(
      title = list(text = "Calibration: Implied vs Actual Win Rate by Team"),
      xaxis = list(title = "Average Implied Win Probability", range = c(-0.05, 1.05), showgrid = TRUE, gridcolor = "rgba(0,0,0,0.1)"),
      yaxis = list(title = "Actual Win Rate", range = c(-0.05, 1.05), showgrid = TRUE, gridcolor = "rgba(0,0,0,0.1)"),
      legend = list(title = list(text = "League")),
      hovermode = "closest",
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
}

# Performance plot for Teams view (plotly via ggplotly)
# - builds a cumulative residual performance plot
# - each team's path is colored by league. A grey ribbon shows ±k·σ·√n bounds.
plot_team_performance <- function(perf_data, residual_sd, color_map = NULL, k = 2) {
  # Validation
  req_cols <- c("starts", "cum_residual", "league_name", "team", "match_number", "tooltip_text", "residual")
  missing <- setdiff(req_cols, names(perf_data))
  if (length(missing) > 0) {
    stop(sprintf("perf_data is missing required columns: %s", paste(missing, collapse = ", ")))
  }
  if (!is.numeric(residual_sd) || length(residual_sd) != 1 || !is.finite(residual_sd)) {
    stop("residual_sd must be a finite numeric scalar.")
  }
  if (nrow(perf_data) == 0) {
    return(plotly::plot_ly())
  }
  leagues <- unique(perf_data$league_name)
  if (is.null(color_map)) {
    color_map <- stats::setNames(set1_palette(length(leagues)), leagues)
  } else {
    missing_keys <- setdiff(leagues, names(color_map))
    if (length(missing_keys) > 0) {
      stop(sprintf("color_map is missing colors for leagues: %s", paste(missing_keys, collapse = ", ")))
    }
  }
  match_range <- range(perf_data$match_number)
  date_range <- range(perf_data$starts)
  bounds_df <- data.frame(
    match_number = seq(match_range[1], match_range[2]),
    starts = seq(date_range[1], date_range[2], length.out = match_range[2] - match_range[1] + 1)
  ) |>
    dplyr::mutate(
      upper_bound = k * residual_sd * sqrt(.data$match_number),
      lower_bound = -k * residual_sd * sqrt(.data$match_number)
    )
  gp <- ggplot2::ggplot(perf_data, ggplot2::aes(x = .data$starts, y = .data$cum_residual, group = .data$team, color = .data$league_name, text = .data$tooltip_text)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    ggplot2::geom_ribbon(
      data = bounds_df,
      ggplot2::aes(x = .data$starts, ymin = .data$lower_bound, ymax = .data$upper_bound, group = 1L),
      inherit.aes = FALSE,
      fill = "grey80",
      alpha = 0.3
    ) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::scale_color_manual(values = color_map, breaks = names(color_map), name = "League") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Cumulative Over/Underperformance vs Market",
      subtitle = "Grey ribbon: ±2σ√n random walk bounds",
      x = "Match Date",
      y = "Cumulative residual points"
    )
  plotly::ggplotly(gp, tooltip = "text")
}

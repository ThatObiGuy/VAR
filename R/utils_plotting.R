# Plotting helpers (pure; no DB or Shiny dependencies)

#' Plot percentage change of closing favourite odds over time
#'
#' @param df tibble with columns: logged_time (POSIXct), home_odds, draw_odds, away_odds, max_money_line
#' @return ggplot object
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

# Small pure helpers used by Teams plots
# Map match result to points earned by a side (3 for win, 1 for draw, 0 for loss)
map_points <- function(result, side) {
  dplyr::case_when(
    result == "draw" ~ 1,
    side == "home" & result == "home_win" ~ 3,
    side == "away" & result == "away_win" ~ 3,
    TRUE ~ 0
  )
}

# Wilson score interval for binomial proportion
# Returns a data.frame with ci_lower and ci_upper columns
calculate_wilson_ci <- function(n, n_wins, conf = 0.95) {
  z <- stats::qnorm(0.5 + conf / 2)
  p_hat <- ifelse(n > 0, n_wins / n, NA_real_)
  denominator <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denominator
  margin <- z * sqrt((p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))) / denominator
  data.frame(
    ci_lower = pmax(0, center - margin),
    ci_upper = pmin(1, center + margin)
  )
}
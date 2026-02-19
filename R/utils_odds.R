# Odds-related helpers (CSV/in-memory)

# Fetch odds time series for a single event from local DATA
# - Returns a tibble with plottable logged_time (POSIXct)
get_event_odds <- function(event_id) {
  stopifnot(!missing(event_id))
  odds <- tryCatch(get("DATA", envir = .GlobalEnv)$odds1x2, error = function(e) NULL)
  if (is.null(odds)) return(tibble::tibble())
  odds |>
    dplyr::filter(event_id == !!event_id) |>
    dplyr::select(
      logged_time, home_odds, draw_odds, away_odds, max_money_line
    ) |>
    dplyr::mutate(
      # ensure time column is plottable
      logged_time = as.POSIXct(logged_time, tz = "UTC")
    )
}

# Map match result to points earned by a side
# - mapping for standard 3/1/0 points system.
map_points <- function(result, side) {
  dplyr::case_when(
    result == "draw" ~ 1L,
    side == "home" & result == "home_win" ~ 3L,
    side == "away" & result == "away_win" ~ 3L,
    TRUE ~ 0L
  )
}

# Wilson score interval for a binomial proportion
# - Provides numerically stable confidence intervals
calculate_wilson_ci <- function(n, n_wins, conf = 0.95) {
  if (length(n) != length(n_wins)) {
    stop("n and n_wins must have the same length.")
  }
  if (!is.numeric(n) || !is.numeric(n_wins)) {
    stop("n and n_wins must be numeric vectors.")
  }
  if (any(n < 0, na.rm = TRUE) || any(n_wins < 0, na.rm = TRUE)) {
    stop("n and n_wins must be non-negative.")
  }
  z <- stats::qnorm(0.5 + conf / 2)
  # Avoid division by zero; where n == 0, return NA bounds
  p_hat <- ifelse(n > 0, n_wins / n, NA_real_)
  denominator <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denominator
  margin <- z * sqrt((p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))) / denominator
  data.frame(
    ci_lower = pmax(0, center - margin),
    ci_upper = pmin(1, center + margin)
  )
}

# Compute favourite pct-change time series for a single-event odds df (pure helper)
# - Ensures logged_time is POSIXct
# - Determines favourite based on closing odds, breaking ties by opening odds
# - Adds columns: fav_odds, pct_change_fav_odds
compute_fav_change <- function(df_single) {
  if (is.null(df_single) || nrow(df_single) == 0) return(df_single)
  # Ensure logged_time is plottable
  if (!inherits(df_single$logged_time, "POSIXct")) {
    df_single$logged_time <- as.POSIXct(df_single$logged_time, tz = "UTC")
  }
  closing_row <- dplyr::slice_max(df_single, logged_time, n = 1)
  opening_row <- dplyr::slice_min(df_single, logged_time, n = 1)
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
    fav_choice <- names(opening_vals)[which.min(opening_vals)]
  } else {
    fav_choice <- candidates[[1]]
  }
  opening_fav_odds <- opening_row[[fav_choice]][[1]]
  dplyr::mutate(
    df_single,
    fav_odds = if (fav_choice == "home_odds") home_odds else if (fav_choice == "draw_odds") draw_odds else away_odds,
    pct_change_fav_odds = (fav_odds - opening_fav_odds) / opening_fav_odds * 100
  )
}
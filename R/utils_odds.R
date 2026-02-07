# Odds-related DB helpers

# Fetch odds time series for a single event
# - Keeps DB work lazy until collect()
# - Returns a tibble with plottable logged_time (POSIXct)
get_event_odds <- function(con, event_id) {
  stopifnot(!missing(con), !missing(event_id))
  odds <- dplyr::tbl(con, "odds1x2")
  odds |>
    dplyr::filter(event_id == !!event_id) |>
    dplyr::select(
      logged_time, home_odds, draw_odds, away_odds, max_money_line
    ) |>
    dplyr::collect() |>
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
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

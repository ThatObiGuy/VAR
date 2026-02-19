# Teams child module for Grapher

mod_grapher_teams_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("TEAMS"),
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      h2("Market Calibration Analysis", style = "margin: 0;"),
      actionButton(
        ns("help_calib"),
        label = NULL,
        icon = icon("question-circle"),
        style = "font-size: 16px; color: #e67e22; background: transparent; border: none;",
        title = "Show help for calibration analysis"
      )
    ),
    plotlyOutput(ns("team_calibration_plot")),
    hr(),
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      h2("Cumulative Performance vs Market", style = "margin: 0;"),
      actionButton(
        ns("help_cumulative"),
        label = NULL,
        icon = icon("question-circle"),
        style = "font-size: 16px; color: #e67e22; background: transparent; border: none;",
        title = "Show help for cumulative residual"
      )
    ),
    plotlyOutput(ns("team_performance_plot"))
  )
}

# Server
# - league_palette: reactive() with named color vector
mod_grapher_teams_server <- function(id, league_palette) {
  moduleServer(id, function(input, output, session) {
    results <- req(get("DATA", envir = .GlobalEnv)$results1)
    odds    <- req(get("DATA", envir = .GlobalEnv)$odds1x2)
    entropy <- req(get("DATA", envir = .GlobalEnv)$entropy)
    
    observeEvent(input$help_calib, {
      showModal(modalDialog(
        title = "Market Calibration Analysis Help",
        tags$img(
          src = "calib_help.png",
          width = "100%",
          style = "max-width: 800px;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    observeEvent(input$help_cumulative, {
      showModal(modalDialog(
        title = "Cumulative Performance Help",
        tags$img(
          src = "cumulative_help.png",
          width = "100%",
          style = "max-width: 800px;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    closing_base <- reactive({
      shiny::withProgress(message = "Loading team-level closing odds...", value = 0, {
        odds |>
          dplyr::group_by(event_id) |>
          dplyr::filter(logged_time == max(logged_time)) |>
          dplyr::ungroup() |>
          dplyr::select(event_id, league_name, starts, home_team, away_team) |>
          dplyr::inner_join(results |> dplyr::select(event_id, result), by = "event_id") |>
          dplyr::inner_join(entropy |> dplyr::select(event_id, p_home, p_draw, p_away), by = "event_id") |>
          dplyr::mutate(starts = as.POSIXct(starts, tz = "UTC"))
      })
    })
    
    team_long <- reactive({
      base <- closing_base()
      req(nrow(base) > 0)
      dplyr::bind_rows(
        base |>
          dplyr::transmute(
            team = home_team,
            league_name,
            starts,
            side = "home",
            p_win = p_home,
            p_draw,
            actual_win = dplyr::if_else(result == "home_win", 1, 0),
            actual_points = map_points(result, "home")
          ),
        base |>
          dplyr::transmute(
            team = away_team,
            league_name,
            starts,
            side = "away",
            p_win = p_away,
            p_draw,
            actual_win = dplyr::if_else(result == "away_win", 1, 0),
            actual_points = map_points(result, "away")
          )
      )
    })
    
    team_color_map <- reactive({
      df <- team_long(); req(nrow(df) > 0)
      pal <- league_palette()
      leagues_present <- unique(df$league_name)
      pal_subset <- pal[names(pal) %in% leagues_present]
      missing <- setdiff(leagues_present, names(pal_subset))
      if (length(missing) > 0) pal_subset <- c(pal_subset, deterministic_palette(missing))
      pal_subset
    })
    
    output$team_calibration_plot <- plotly::renderPlotly({
      df <- team_long(); validate(need(nrow(df) > 0, "No team data available to plot."))
      calib_bins <- df |>
        dplyr::group_by(team, league_name) |>
        dplyr::summarise(
          n = dplyr::n(),
          n_wins = sum(actual_win, na.rm = TRUE),
          avg_implied = mean(p_win, na.rm = TRUE),
          actual_rate = mean(actual_win, na.rm = TRUE),
          .groups = "drop"
        )
      validate(need(nrow(calib_bins) > 0, "Not enough data for calibration."))
      ci <- calculate_wilson_ci(calib_bins$n, calib_bins$n_wins)
      calib_bins <- dplyr::bind_cols(calib_bins, ci) |>
        dplyr::mutate(
          tooltip_text = paste0(
            team, "\n",
            "Avg Implied: ", round(avg_implied, 2), "\n",
            "Actual: ", round(actual_rate, 2), "\n",
            "95% CI: [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]\n",
            "Matches: ", n
          )
        )
      plot_team_calibration(calib_bins, color_map = team_color_map()) |>
        plotly::config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })
    
    output$team_performance_plot <- plotly::renderPlotly({
      df <- team_long(); validate(need(nrow(df) > 0, "No team data available to plot."))
      perf_df <- df |>
        dplyr::mutate(expected_points = 3 * p_win + 1 * p_draw, residual = actual_points - expected_points) |>
        dplyr::arrange(team, starts) |>
        dplyr::group_by(team, league_name) |>
        dplyr::mutate(match_number = dplyr::row_number(), cum_residual = cumsum(ifelse(is.na(residual), 0, residual))) |>
        dplyr::ungroup() |>
        dplyr::mutate(tooltip_text = paste0(team, "\n", "Date: ", format(starts, "%Y-%m-%d"), "\n", "Cumulative: ", round(cum_residual, 2)))
      validate(need(nrow(perf_df) > 0, "Not enough data for performance plot."))
      residual_sd <- stats::sd(perf_df$residual, na.rm = TRUE)
      plot_team_performance(perf_df, residual_sd = residual_sd, color_map = team_color_map()) |>
        plotly::config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })
  })
}

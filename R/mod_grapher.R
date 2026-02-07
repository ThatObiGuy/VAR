# Grapher module: main interactive feature with DB access

# UI
mod_grapher_ui <- function(id) {
  ns <- NS(id) # https://shiny.posit.co/r/articles/improve/modules/
  sidebarLayout(
    sidebarPanel(
      width = 1,
      radioButtons(
        inputId = ns("menu"),
        label = "Select View",
        choices = c("Overview", "Single Match", "Compare Matches", "Teams", "Leagues"),
        selected = "Overview",
        inline = FALSE
      )
    ),
    mainPanel(
      width = 10,
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Overview'", ns("menu")),
        h1("OVERVIEW"),
        h2("Displaying data from odds markets on:"),
        h3(textOutput(ns("overview_text")))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Single Match'", ns("menu")),
        h1("SINGLE MATCH"),
        p("Click on a match to select it:"),
        DT::DTOutput(ns("matches_table")),
        hr(),
        plotOutput(ns("single_odds_plot"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Compare Matches'", ns("menu")),
        h1("COMPARE MATCHES")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Teams'", ns("menu")),
        h1("TEAMS"),
        h2("Market Calibration Analysis"),
        plotlyOutput(ns("team_calibration_plot")),
        hr(),
        h2("Cumulative Performance vs Market"),
        plotlyOutput(ns("team_performance_plot"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Leagues'", ns("menu")),
        h1("LEAGUES"),
        h2("Needs more data...")
      )
    )
  )
}

# Server
mod_grapher_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Per-session DB connection
    con <- connect_db()
    session$onSessionEnded(function() disconnect_db(con))
    
    # Lazily reference tables via dbplyr
    results <- dplyr::tbl(con, "results1")
    odds <- dplyr::tbl(con, "odds1x2")
    
    # Compute counts (collect to R for display)
    summary_stats <- results |>
      dplyr::summarise(
        unique_events = dplyr::n_distinct(event_id),
        unique_teams = dplyr::n_distinct(home_team),
        unique_leagues = dplyr::n_distinct(league_id)
      ) |>
      dplyr::collect()
    
    n_observations <- odds |> dplyr::tally() |> dplyr::pull(n)
    
    matches <- summary_stats$unique_events[[1]]
    teams <- summary_stats$unique_teams[[1]]
    leagues <- summary_stats$unique_leagues[[1]]
    
    output$overview_text <- renderText({
      paste(n_observations, "observations from", matches, "matches between", teams,
            "different teams, across", leagues, "different leagues")
    })
    
    # Prepare matches data for DT (explicit collect)
    matches_data <- reactive({
      results |>
        dplyr::select(event_id, home_team, away_team, league_name, starts) |>
        dplyr::collect() |>
        dplyr::mutate(Match_Date = format(as.Date(starts), "%Y-%m-%d")) |>
        dplyr::select(event_id, home_team, away_team, league_name, Match_Date)
    })
    
    output$matches_table <- DT::renderDT({
      DT::datatable(
        matches_data(),
        selection = 'single',
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 25, 50),
          scrollX = TRUE
        ),
        rownames = FALSE,
        colnames = c("Event ID", "Home Team", "Away Team", "League", "Match Date")
      )
    })
    
    # Reactive: selected event_id from DT single-row selection
    selected_event_id <- reactive({
      rows <- input$matches_table_rows_selected
      req(length(rows) == 1)
      matches_data()[["event_id"]][rows]
    })
    
    # Reactive: pull odds for the selected event
    event_df <- reactive({
      req(selected_event_id())
      get_event_odds(con, selected_event_id())
    })
    
    # Plot: favourite odds percentage change over time for selected match
    output$single_odds_plot <- renderPlot({
      # If nothing selected, return a blank plot (preserve previous UX)
      if (is.null(input$matches_table_rows_selected) || length(input$matches_table_rows_selected) == 0) {
        return(invisible(NULL))
      }
      df <- event_df()
      validate(need(nrow(df) > 0, "No odds available for this event."))
      plot_fav_odds_change(df)
    })
    
    ####
    # TEAMS VIEW REACTIVES - will move soon
    ####
    
    closing_base <- reactive({
      odds |>
        dplyr::group_by(event_id) |>
        dplyr::filter(logged_time == max(logged_time)) |>
        dplyr::ungroup() |>
        dplyr::select(event_id, league_name, starts, home_team, away_team) |>
        dplyr::inner_join(results |> dplyr::select(event_id, result), by = "event_id") |>
        dplyr::inner_join(dplyr::tbl(con, "entropy") |> dplyr::select(event_id, p_home, p_draw, p_away), by = "event_id") |>
        dplyr::collect() |>
        dplyr::mutate(starts = as.POSIXct(starts, tz = "UTC"))
    })
    
    # Reactive: Team-level long data (each match becomes two rows: home and away)
    # - Computes actual win indicator and actual points per side
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
    
    ####
    # TEAMS VIEW: PLOTS -  refactored to utilities with unified legend
    ####
    
    # Generate unified color map once per session for Teams view based on leagues present
    team_color_map <- reactive({
      df <- team_long()
      leagues <- unique(df$league_name)
      # We deliberately fix the mapping order to the unique() order to keep legend consistent
      setNames(scales::hue_pal()(length(leagues)), leagues)
    })
    
    # 1) Calibration Plot: Implied vs Actual Win Rate with Wilson CI
    output$team_calibration_plot <- plotly::renderPlotly({
      df <- team_long()
      validate(need(nrow(df) > 0, "No team data available to plot."))
      
      # Prepare calibration aggregates in the module (reactive data prep only)
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
      
      # Wilson CI computed via utility helper
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
      
      plot_team_calibration(calib_bins, color_map = team_color_map())
    })
    
    # 2) Performance Plot: Cumulative residual points vs market expectations
    output$team_performance_plot <- plotly::renderPlotly({
      df <- team_long()
      validate(need(nrow(df) > 0, "No team data available to plot."))
      
      # Prepare performance data in the module (reactive data prep only)
      perf_df <- df |>
        dplyr::mutate(
          # Expected points under 3/1/0 system from market implied probabilities
          expected_points = 3 * p_win + 1 * p_draw,
          residual = actual_points - expected_points
        ) |>
        dplyr::arrange(team, starts) |>
        dplyr::group_by(team, league_name) |>
        dplyr::mutate(
          match_number = dplyr::row_number(),
          # Treat NA residuals as 0 to keep cumulative path continuous
          cum_residual = cumsum(ifelse(is.na(residual), 0, residual))
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          tooltip_text = paste0(
            team, "\n",
            "Date: ", format(starts, "%Y-%m-%d"), "\n",
            "Cumulative: ", round(cum_residual, 2)
          )
        )
      
      validate(need(nrow(perf_df) > 0, "Not enough data for performance plot."))
      residual_sd <- stats::sd(perf_df$residual, na.rm = TRUE)
      
      # Pass unified color map so legend matches calibration plot
      plot_team_performance(perf_df, residual_sd = residual_sd, color_map = team_color_map())
    })
  })
}

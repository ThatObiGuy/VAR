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
    # TEAMS VIEW: PLOTS -  will move later
    ####
    
    # 1) Calibration Plot: Implied vs Actual Win Rate with Wilson CI
    output$team_calibration_plot <- plotly::renderPlotly({
      df <- team_long()
      validate(need(nrow(df) > 0, "No team data available to plot."))
      
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
      
      # Wilson score intervals for binomial proportion (why: better small-sample behavior than Wald)
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
      
      leagues <- unique(calib_bins$league_name)
      color_map <- setNames(scales::hue_pal()(length(leagues)), leagues)
      
      p <- plotly::plot_ly() |>
        plotly::add_segments(
          x = 0, xend = 1, y = 0, yend = 1,
          line = list(color = "grey50", dash = "dash", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        )
      
      for (lg in leagues) {
        league_data <- dplyr::filter(calib_bins, league_name == lg)
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
    })
    
    # 2) Performance Plot: Cumulative residual points vs market expectations
    output$team_performance_plot <- plotly::renderPlotly({
      df <- team_long()
      validate(need(nrow(df) > 0, "No team data available to plot."))
      
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
      k <- 2 # ±2σ as context under a simple random walk assumption
      
      match_range <- range(perf_df$match_number)
      date_range <- range(perf_df$starts)
      bounds_df <- data.frame(
        match_number = seq(match_range[1], match_range[2]),
        starts = seq(date_range[1], date_range[2], length.out = match_range[2] - match_range[1] + 1)
      ) |>
        dplyr::mutate(
          upper_bound = k * residual_sd * sqrt(match_number),
          lower_bound = -k * residual_sd * sqrt(match_number)
        )
      
      gp <- ggplot2::ggplot(perf_df, ggplot2::aes(x = starts, y = cum_residual, group = team, color = league_name, text = tooltip_text)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
        ggplot2::geom_ribbon(
          data = bounds_df,
          ggplot2::aes(x = starts, ymin = lower_bound, ymax = upper_bound, group = 1),
          inherit.aes = FALSE,
          fill = "grey80",
          alpha = 0.3
        ) +
        ggplot2::geom_line(alpha = 0.7) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Cumulative Over/Underperformance vs Market",
          subtitle = "Grey ribbon: ±2σ√n random walk bounds",
          x = "Match Date",
          y = "Cumulative residual points",
          color = "League"
        )
      
      plotly::ggplotly(gp, tooltip = "text")
    })
  })
}

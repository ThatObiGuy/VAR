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
        choices = c("Overview", "Matches", "Teams", "Leagues"),
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
        condition = sprintf("input['%s'] == 'Matches'", ns("menu")),
        h1("MATCHES"),
        p("Narrow by league to speed up loading and reduce data size (leave empty for all):"),
        selectInput(
          inputId = ns("league_filter"),
          label = "Leagues",
          choices = NULL, # populated from server based on DB
          multiple = TRUE,
          selected = NULL
        ),
        p("Then filter to one or more matches (leave empty to show all):"),
        selectInput(
          inputId = ns("match_filter"),
          label = "Filter Matches",
          choices = NULL, # populated from server based on DB
          multiple = TRUE,
          selected = NULL
        ),
        hr(),
        plotlyOutput(ns("matches_plot"))
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
    
    # League choices (populated lazily from DB)
    league_choices <- reactive({
      # Keep this lightweight: only distinct league_name
      results |>
        dplyr::distinct(league_name) |>
        dplyr::arrange(league_name) |>
        dplyr::collect() |>
        dplyr::pull(league_name)
    })
    
    observe({
      updateSelectInput(
        session,
        inputId = "league_filter",
        choices = league_choices(),
        selected = NULL # no default = all leagues
      )
    })
    
    # Prepare matches data for filter choices (explicit collect)
    matches_data <- reactive({
      # Start with base selection (lazy)
      tbl <- results |>
        dplyr::select(event_id, home_team, away_team, league_name, starts)
      # Apply league filter, if any (still lazy on DB)
      lf <- input$league_filter
      if (!is.null(lf) && length(lf) > 0) {
        tbl <- tbl |>
          dplyr::filter(.data$league_name %in% lf)
      }
      tbl |>
        dplyr::collect() |>
        dplyr::mutate(Match_Date = format(as.Date(starts), "%Y-%m-%d")) |>
        dplyr::select(event_id, home_team, away_team, league_name, Match_Date)
    })
    
    # Choices for selectInput: names shown as "Home vs. Away, YYYY-MM-DD" with event_id as value
    match_choices <- reactive({
      md <- matches_data()
      if (nrow(md) == 0) return(setNames(numeric(0), character(0)))
      labels <- paste0(md$home_team, " vs. ", md$away_team, ", ", md$Match_Date)
      stats::setNames(md$event_id, labels)
    })
    
    # Update the selectInput choices whenever matches_data changes
    observe({
      updateSelectInput(
        session,
        inputId = "match_filter",
        choices = match_choices(),
        selected = NULL # start with no selection = show all
      )
    })
    
    # Reactive: selected event_ids from the filter (NULL/empty means all)
    selected_event_ids <- reactive({
      ids <- input$match_filter
      all_ids <- matches_data()$event_id
      if (is.null(ids) || length(ids) == 0) {
        return(all_ids)
      }
      # Coerce to numeric to match DB type; drop NAs
      sel <- suppressWarnings(as.numeric(ids))
      sel <- sel[!is.na(sel)]
      if (length(sel) == 0) all_ids else sel
    })
    
    
    # Reactive: pull odds for all selected (or all) events and compute pct-change per event (single batch DB query)
    multi_events_df <- reactive({
      ids <- selected_event_ids()
      md <- matches_data()
      req(length(ids) >= 1, nrow(md) >= 1)
      
      # Build a named vector for labels to attach to each event's series
      label_map <- setNames(
        paste0(md$home_team, " vs. ", md$away_team, ", ", md$Match_Date),
        md$event_id
      )
      
      # SINGLE DB QUERY for all events, then process in-memory
      all_odds <- odds |>
        dplyr::filter(event_id %in% !!ids) |>
        dplyr::collect()
      
      if (nrow(all_odds) == 0) return(dplyr::tibble())
      
      dfs <- lapply(ids, function(eid) {
        df <- all_odds |> dplyr::filter(event_id == eid)
        if (nrow(df) == 0) return(NULL)
        df2 <- compute_fav_change(df)
        df2$event_id <- eid
        df2$event_label <- label_map[[as.character(eid)]]
        df2
      })
      
      dfs <- Filter(Negate(is.null), dfs)
      if (length(dfs) == 0) return(dplyr::tibble())
      dplyr::bind_rows(dfs)
    })
    
    # Plot: favourite odds percentage change over time for one or many matches (native Plotly for performance)
    output$matches_plot <- plotly::renderPlotly({
      df <- multi_events_df()
      if (is.null(df) || nrow(df) == 0) return(plotly::plot_ly())
      
      p <- plotly::plot_ly()
      
      for (event_label in unique(df$event_label)) {
        event_df <- df[df$event_label == event_label, ]
        p <- plotly::add_lines(
          p,
          data = event_df,
          x = ~logged_time,
          y = ~pct_change_fav_odds,
          name = event_label,
          line = list(width = 2),
          showlegend = FALSE,
          hovertemplate = paste0(
            "<b>%{fullData.name}</b><br>",
            "Time: %{x|%Y-%m-%d %H:%M}<br>",
            "Change: %{y:.2f}%<extra></extra>"
          )
        )
      }
      
      p |>
        plotly::layout(
          title = "Change over time of closing favourite odds",
          xaxis = list(title = "Time"),
          yaxis = list(title = "Odds Change (%)"),
          hovermode = "closest",
          showlegend = FALSE
        )
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

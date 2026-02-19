# Overview child module for Grapher

# UI
mod_grapher_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("OVERVIEW"),
    h2("Displaying data from odds markets on:"),
    h3(textOutput(ns("overview_text"))),
    hr(),
    fluidRow(
      column(
        width = 6,
        h2("Data Coverage Over Time"),
        plotlyOutput(ns("overview_coverage_plot"), height = "350px")
      ),
      column(
        width = 6,
        h2("Match Distribution by League"),
        plotlyOutput(ns("overview_league_dist_plot"), height = "350px")
      )
    )
  )
}

# Server
# - league_palette: reactive() provided by parent returning named vector of colors
mod_grapher_overview_server <- function(id, league_palette) {
  moduleServer(id, function(input, output, session) {
    # Local in-memory data
    results <- req(get("DATA", envir = .GlobalEnv)$results1)
    odds    <- req(get("DATA", envir = .GlobalEnv)$odds1x2)
    
    # Compute counts for header text
    {
      id <- shiny::showNotification("Loading overview metrics...", duration = NULL, type = "message")
      on.exit(shiny::removeNotification(id), add = TRUE)
      summary_stats <- results |>
        dplyr::summarise(
          unique_events = dplyr::n_distinct(event_id),
          unique_teams = dplyr::n_distinct(home_team),
          unique_leagues = dplyr::n_distinct(league_name)
        )
      n_observations <- odds |> dplyr::tally() |> dplyr::pull(n)
    }
    
    output$overview_text <- renderText({
      matches <- summary_stats$unique_events[[1]]
      teams   <- summary_stats$unique_teams[[1]]
      leagues <- summary_stats$unique_leagues[[1]]
      paste(n_observations, "observations from", matches, "matches between", teams,
            "different teams, across", leagues, "different leagues")
    })
    
    # Minimal results data for overview plots
    overview_results_data <- reactive({
      id <- shiny::showNotification("Loading overview data...", duration = NULL, type = "message")
      on.exit(shiny::removeNotification(id), add = TRUE)
      results |>
        dplyr::select(event_id, league_name, starts) |>
        dplyr::mutate(starts = as.POSIXct(starts, tz = "UTC"))
    })
    
    # Plot 1: coverage timeline
    output$overview_coverage_plot <- plotly::renderPlotly({
      df <- overview_results_data()
      validate(need(nrow(df) > 0, "No data available to plot coverage timeline."))
      
      pal <- league_palette()
      df$league_name <- factor_with_palette(df$league_name, pal)
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = starts, fill = league_name)) +
        ggplot2::geom_histogram(
          binwidth = 7 * 24 * 60 * 60,
          alpha = 0.9, color = "white", linewidth = 0.2,
          position = "stack"
        ) +
        scale_fill_league(values = pal) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Match Coverage Over Time (by League)",
          x = "Date", y = "Number of Matches", fill = "League"
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      plotly::ggplotly(p) |> plotly::layout(hovermode = "closest") |>
        plotly::config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })
    
    # Plot 2: league distribution
    output$overview_league_dist_plot <- plotly::renderPlotly({
      df <- overview_results_data()
      validate(need(nrow(df) > 0, "No data available to plot league distribution."))
      
      pal <- league_palette()
      league_distribution <- df |>
        dplyr::group_by(league_name) |>
        dplyr::summarise(match_count = dplyr::n(), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(match_count)) |>
        dplyr::mutate(
          pct = match_count / sum(match_count),
          tooltip_text = paste0(
            league_name, "\n",
            "Matches: ", match_count, "\n",
            "Percentage: ", round(pct * 100, 1), "%"
          )
        )
      
      league_distribution$league_name <- factor_with_palette(league_distribution$league_name, pal)
      
      p <- ggplot2::ggplot(
        league_distribution,
        ggplot2::aes(x = reorder(league_name, match_count), y = match_count, fill = league_name, text = tooltip_text)
      ) +
        ggplot2::geom_col(alpha = 0.9, color = "white", linewidth = 0.2) +
        ggplot2::coord_flip() +
        scale_fill_league(values = pal) +
        ggplot2::guides(fill = "none") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Match Distribution by League", subtitle = "Total number of matches per league", x = " ", y = "Number of Matches")
      
      plotly::ggplotly(p, tooltip = "text") |> plotly::layout(hovermode = "closest") |>
        plotly::config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })
  })
}

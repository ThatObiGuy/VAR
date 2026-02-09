# Leagues child module for Grapher

mod_grapher_leagues_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("LEAGUES"),
    h2("Market Entropy Distribution by League"),
    plotlyOutput(ns("league_entropy_plot"))
  )
}

# Server
# - con: DBI connection
# - league_palette: reactive() with named color vector
mod_grapher_leagues_server <- function(id, con, league_palette) {
  moduleServer(id, function(input, output, session) {
    results <- dplyr::tbl(con, "results1")
    entropy <- dplyr::tbl(con, "entropy")
    
    match_entropy_data <- reactive({
      id <- shiny::showNotification("Loading league entropy data...", duration = NULL, type = "message")
      on.exit(shiny::removeNotification(id), add = TRUE)
      entropy |>
        dplyr::select(event_id, entropy) |>
        dplyr::inner_join(
          results |> dplyr::select(event_id, league_name, starts, home_team, away_team, result),
          by = "event_id"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          starts = as.POSIXct(starts, tz = "UTC"),
          tooltip_text = paste0(
            league_name, "\n",
            format(starts, "%Y-%m-%d %H:%M UTC"), "\n",
            home_team, " vs ", away_team, "\n",
            "Outcome: ", result, "\n",
            "Entropy: ", round(entropy, 3), " bits\n",
            "event_id: ", event_id
          )
        )
    })
    
    output$league_entropy_plot <- plotly::renderPlotly({
      df <- match_entropy_data()
      validate(need(nrow(df) > 0, "No entropy data available to plot."))
      
      pal <- league_palette()
      df$league_name <- factor_with_palette(df$league_name, pal)
      
      p_violin <- ggplot2::ggplot(df, ggplot2::aes(x = league_name, y = entropy, fill = league_name)) +
        ggplot2::geom_violin(alpha = 0.7, color = "grey30", draw_quantiles = c(0.25, 0.5, 0.75)) +
        scale_fill_league(values = pal) +
        ggplot2::geom_jitter(ggplot2::aes(text = tooltip_text), width = 0.15, height = 0,
                             size = 1.5, alpha = 0.5, color = "#2c3e50") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = NULL , x = "League", y = "Shannon entropy (bits)") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      plotly::ggplotly(p_violin, tooltip = "text")
    })
  })
}

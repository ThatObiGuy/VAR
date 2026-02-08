# Matches child module for Grapher

mod_grapher_matches_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("MATCHES"),
    p("Narrow by league to speed up loading and reduce data size (leave empty for all):"),
    selectInput(ns("league_filter"), "Leagues", choices = NULL, multiple = TRUE, selected = NULL),
    p("Then filter to one or more matches (leave empty to show all):"),
    selectInput(ns("match_filter"), "Filter Matches", choices = NULL, multiple = TRUE, selected = NULL),
    hr(),
    plotlyOutput(ns("matches_plot"))
  )
}

# Server
# - con: DBI connection provided by parent
mod_grapher_matches_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    results <- dplyr::tbl(con, "results1")
    odds    <- dplyr::tbl(con, "odds1x2")
    
    league_choices <- reactive({
      results |>
        dplyr::distinct(league_name) |>
        dplyr::arrange(league_name) |>
        dplyr::collect() |>
        dplyr::pull(league_name)
    })
    
    observe({
      updateSelectInput(session, inputId = "league_filter", choices = league_choices(), selected = NULL)
    })
    
    matches_data <- reactive({
      tbl <- results |>
        dplyr::select(event_id, home_team, away_team, league_name, starts)
      lf <- input$league_filter
      if (!is.null(lf) && length(lf) > 0) {
        tbl <- tbl |> dplyr::filter(.data$league_name %in% lf)
      }
      tbl |>
        dplyr::collect() |>
        dplyr::mutate(Match_Date = format(as.Date(starts), "%Y-%m-%d")) |>
        dplyr::select(event_id, home_team, away_team, league_name, Match_Date)
    })
    
    match_choices <- reactive({
      md <- matches_data()
      if (nrow(md) == 0) return(stats::setNames(numeric(0), character(0)))
      labels <- paste0(md$home_team, " vs. ", md$away_team, ", ", md$Match_Date)
      stats::setNames(md$event_id, labels)
    })
    
    observe({
      updateSelectInput(session, inputId = "match_filter", choices = match_choices(), selected = NULL)
    })
    
    selected_event_ids <- reactive({
      ids <- input$match_filter
      all_ids <- matches_data()$event_id
      if (is.null(ids) || length(ids) == 0) return(all_ids)
      sel <- suppressWarnings(as.numeric(ids))
      sel <- sel[!is.na(sel)]
      if (length(sel) == 0) all_ids else sel
    })
    
    multi_events_df <- reactive({
      ids <- selected_event_ids()
      md  <- matches_data()
      req(length(ids) >= 1, nrow(md) >= 1)
      
      label_map <- stats::setNames(
        paste0(md$home_team, " vs. ", md$away_team, ", ", md$Match_Date),
        md$event_id
      )
      
      all_odds <- odds |>
        dplyr::filter(event_id %in% !!ids) |>
        dplyr::select(logged_time, home_odds, draw_odds, away_odds, max_money_line, event_id) |>
        dplyr::collect()
      
      if (nrow(all_odds) == 0) return(dplyr::tibble())
      
      dfs <- lapply(ids, function(eid) {
        df <- dplyr::filter(all_odds, .data$event_id == eid)
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
        plotly::layout(title = "Change over time of closing favourite odds",
                       xaxis = list(title = "Time"),
                       yaxis = list(title = "Odds Change (%)"),
                       hovermode = "closest",
                       showlegend = FALSE)
    })
  })
}

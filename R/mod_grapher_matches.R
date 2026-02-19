# Matches child module for Grapher

mod_grapher_matches_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; align-items: center; gap: 10px;",
      h1("MATCHES", style = "margin: 0;"),
      actionButton(
        ns("help_matches"),
        label = NULL,
        icon = icon("question-circle"),
        style = "font-size: 16px; color: #e67e22; background: transparent; border: none;",
        title = "Show help for matches"
      )
    ),
    p("Narrow by league (leave empty for all):"),
    selectInput(ns("league_filter"), "Leagues", choices = NULL, multiple = TRUE, selected = NULL),
    p("Filter to one or more matches (leave empty to show all):"),
    selectInput(ns("match_filter"), "Filter Matches", choices = NULL, multiple = TRUE, selected = NULL),
    hr(),
    plotlyOutput(ns("matches_plot"))
  )
}

# Server
# - ready: reactive logical to gate plotting (e.g., require leagues selected)
# - external_ids: reactive vector of event_ids from manual selection to combine (intersection)
mod_grapher_matches_server <- function(id, ready = reactive(TRUE), external_ids = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    results <- req(get("DATA", envir = .GlobalEnv)$results1)
    odds    <- req(get("DATA", envir = .GlobalEnv)$odds1x2)
    
    observeEvent(input$help_matches, {
      showModal(modalDialog(
        title = "Help for matches plot",
        tags$img(
          src = "matches_help.png",
          width = "100%",
          style = "max-width: 800px;"
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    league_choices <- reactive({
      id <- shiny::showNotification("Loading leagues...", duration = NULL, type = "message")
      on.exit(shiny::removeNotification(id), add = TRUE)
      results |>
        dplyr::distinct(league_name) |>
        dplyr::arrange(league_name) |>
        dplyr::pull(league_name)
    })
    
    observe({
      updateSelectInput(session, inputId = "league_filter", choices = league_choices(), selected = NULL)
    })
    
    matches_data <- reactive({
      id <- shiny::showNotification("Loading matches...", duration = NULL, type = "message")
      on.exit(shiny::removeNotification(id), add = TRUE)
      tbl <- results |>
        dplyr::select(event_id, home_team, away_team, league_name, starts)
      lf <- input$league_filter
      if (!is.null(lf) && length(lf) > 0) {
        tbl <- tbl |> dplyr::filter(.data$league_name %in% lf)
      }
      tbl |>
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
      if (is.null(ids) || length(ids) == 0) ids <- all_ids else {
        sel <- suppressWarnings(as.numeric(ids))
        sel <- sel[!is.na(sel)]
        ids <- if (length(sel) == 0) all_ids else sel
      }
      ext <- tryCatch(external_ids(), error = function(e) NULL)
      ext <- unique(as.numeric(ext))
      ext <- ext[!is.na(ext)]
      if (!is.null(ext) && length(ext) > 0) ids <- intersect(ids, ext)
      unique(ids)
    })
    
    multi_events_df <- reactive({
      # Gate heavy computation by readiness
      req(isTRUE(ready()))
      ids <- selected_event_ids()
      md  <- matches_data()
      req(length(ids) >= 1, nrow(md) >= 1)
      
      label_map <- stats::setNames(
        paste0(md$home_team, " vs. ", md$away_team, ", ", md$Match_Date),
        md$event_id
      )
      
      id <- shiny::showNotification("Loading odds for selected matches...", duration = NULL, type = "message")
      on.exit(shiny::removeNotification(id), add = TRUE)
      all_odds <- odds |>
        dplyr::filter(event_id %in% !!ids) |>
        dplyr::select(logged_time, home_odds, draw_odds, away_odds, max_money_line, event_id)
      
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
      req(isTRUE(ready()))
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
                       showlegend = FALSE) |>
        plotly::config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })
    
    # Expose existing filter IDs (from this feature) for parent intersection
    return(list(
      existing_filtered_ids = selected_event_ids
    ))
  })
}

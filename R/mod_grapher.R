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
        h1("TEAMS")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Leagues'", ns("menu")),
        h1("LEAGUES")
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
  })
}

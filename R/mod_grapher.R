# Grapher module: parent orchestrator

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
      conditionalPanel(sprintf("input['%s'] == 'Overview'", ns("menu")),  mod_grapher_overview_ui(ns("overview"))),
      conditionalPanel(sprintf("input['%s'] == 'Matches'",  ns("menu")),  mod_grapher_matches_ui(ns("matches"))),
      conditionalPanel(sprintf("input['%s'] == 'Teams'",    ns("menu")),  mod_grapher_teams_ui(ns("teams"))),
      conditionalPanel(sprintf("input['%s'] == 'Leagues'",  ns("menu")),  mod_grapher_leagues_ui(ns("leagues")))
    )
  )
}

# Server
mod_grapher_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Per-session DB connection
    con <- connect_db()
    session$onSessionEnded(function() disconnect_db(con))
    
    # DB connection status (poll)
    test_db_connection <- function(con) {
      tryCatch({ DBI::dbGetQuery(con, "SELECT 1 AS test"); TRUE }, error = function(e) FALSE)
    }
    db_status <- reactivePoll(
      intervalMillis = 30000, session = session,
      checkFunc = function() as.numeric(Sys.time()),
      valueFunc  = function() test_db_connection(con)
    )
    
    # Shared palette once per session
    league_palette <- reactive({
      pal <- get_league_colors(con, prefer_db_colors = TRUE, key = "league_id")
      validate(need(length(pal) > 0, "No leagues found to build palette"))
      pal
    })
    
    # Wire child modules
    mod_grapher_overview_server("overview", con = con, league_palette = league_palette)
    mod_grapher_matches_server("matches", con = con)
    mod_grapher_teams_server("teams", con = con, league_palette = league_palette)
    mod_grapher_leagues_server("leagues", con = con, league_palette = league_palette)
  })
}


# Grapher module: parent orchestrator

# UI
mod_grapher_ui <- function(id) {
  ns <- NS(id) # https://shiny.posit.co/r/articles/improve/modules/
  tagList(
    tabsetPanel(
      id = ns("subtab"),
      type = "tabs",
      tabPanel("Overview", value = "Overview", mod_grapher_overview_ui(ns("overview"))),
      tabPanel("Matches",  value = "Matches",  mod_grapher_matches_ui(ns("matches"))),
      tabPanel("Teams",    value = "Teams",    mod_grapher_teams_ui(ns("teams"))),
      tabPanel("Leagues",  value = "Leagues",  mod_grapher_leagues_ui(ns("leagues")))
    )
  )
}

# Server
mod_grapher_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Shared palette once per session from local DATA
    league_palette <- reactive({
      pal <- get_league_colors(if (exists("DATA", envir = .GlobalEnv)) get("DATA", envir = .GlobalEnv)$results1 else NULL)
      validate(need(length(pal) > 0, "No leagues found to build palette"))
      pal
    })
    
    # Wire child modules (no DB connection)
    mod_grapher_overview_server("overview", league_palette = league_palette)
    mod_grapher_matches_server("matches")
    mod_grapher_teams_server("teams", league_palette = league_palette)
    mod_grapher_leagues_server("leagues", league_palette = league_palette)
  })
}


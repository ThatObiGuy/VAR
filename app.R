# VAR
# author - Owen F. O'Connor - owen.oconnor.2024@mumail.ie

library(shiny)
library(shinythemes) # Adding themeing package, quick way to make app more appealing with minimal effort
library(DBI) # For DB interactions
library(RPostgres) # For DB interactions
library(dbplyr) # To allow us to use the language of dplyr rather than SQL on our tables
library(dplyr) # any server side adjustments
library(DT) # DataTable used to allow users to select matches

# Define UI for application
ui <- fluidPage(theme = shinytheme("united"), # Implementation of shinythemes library called in line 5 - I think the united theme suits the use case, found @ https://rstudio.github.io/shinythemes/
                
                navbarPage( # Allows us to use navigation bar at the top of the page, and structure in tabs
                  title = "VAR", # Text displayed in the top left corner
                  
                  tabPanel("About", # Some information about the app
                           
                           h2("About VAR"), 
                           p("A Shiny app to help analyse data collected for my final year project on betting odds."),
                           p("This app was developed as my application to the 2025/2026 Maynooth Data Science \"Shiny App Developement Competition\""),
                           p("Intention is to have an easy-to-use webapp that will help visulatise the odds movements captured and identify any unusual patterns."),
                           p("Users should be able to select subsets from the whole dataset and adjust using different filters/ modifiers."),
                           hr(), # Horizontal rule
                           
                           h3("About the Developer"),
                           p("My name is Owen F. O'Connor, I am a final year (MH207) data science student"),
                           p("you can email me @ owen.oconnor.2024@mumail.ie"),
                           tags$a(href="https://mulife.ie/society/data-science", "Data science society"),
                           br(),
                           tags$a(href="https://www.linkedin.com/in/owen-f-o-connor-7565001b3/", "LinkedIn")
                           
                  ), # tabPanel - About ends
                  
                  
                  tabPanel("How to use", # A description of how the app should be used
                           
                           h2("How to Use the App"),
                           p("1. tdb"),
                           p("2. tdb"),
                           p("3. tdb"),
                           hr(),
                           p("You can watch the video below as an example and guide."),
                           div(style = "text-align: center;", 
                               tags$iframe(width="560", height="315",
                                           src="https://www.youtube.com/embed/OErMB4WYkjE?si=0hzi8FV5ioTW2dlI",
                                           frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
                                           allowfullscreen=NA)) # Will replace with a current video when app is working. Happy to keep this here as a placeholder
                           
                  ), # tabPanel - How to use ends
                  
                  tabPanel("Grapher", # The 'real' app itself
                           sidebarLayout( # separates sidebarPanel and mainPanel
                             sidebarPanel( # select interest
                               width = 1, # doesn't look great, would love some better way to specify width
                               radioButtons(
                                 inputId = "menu",
                                 label = "Select View",
                                 choices = c("Overview", "Single Match", "Compare Matches", "Teams", "Leagues"),
                                 selected = "Overview",
                                 inline = FALSE
                               )
                             ),
                             mainPanel( # display info on selected interest
                               width = 10,
                               conditionalPanel(condition = "input.menu == 'Overview'",
                                                h1("OVERVIEW"),
                                                h2("Displaying data from odds markets on:"),
                                                h3(textOutput("overview_text"))
                               ),
                               conditionalPanel(condition = "input.menu == 'Single Match'",
                                                h1("SINGLE MATCH"),
                                                p("Click on a match to select it:"),
                                                DT::DTOutput("matches_table"),
                                                hr(),
                                                plotOutput("single_odds_plot")
                               ),
                               conditionalPanel(condition = "input.menu == 'Compare Matches'",
                                                h1("COMPARE MATCHES")),
                               conditionalPanel(condition = "input.menu == 'Teams'",
                                                h1("TEAMS")),
                               conditionalPanel(condition = "input.menu == 'Leagues'",
                                                h1("LEAGUES"))
                             )
                           )  # Closes sidebarLayout
                           
                  )  # tabPanel - Grapher ends
                  
                ) # navbarPage ends
) # fluidPage ends

# Define server logic
server <- function(input, output, session) {
  con <- dbConnect( # Secure DB connection once (use .Renviron for env vars)
    Postgres(),
    host = Sys.getenv("PGHOST"),
    port = 5432,
    dbname = Sys.getenv("PGDATABASE"),
    user = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )
  onStop(function() dbDisconnect(con))  # Clean up on exit
  
  # Reactive expression for summary_stats (dbplyr translates to SQL)
  results <- tbl(con, "results1")
  summary_stats <- results |>
    summarise(
      unique_events = n_distinct(event_id),
      unique_teams = n_distinct(home_team),
      unique_leagues = n_distinct(league_id)
    )
  
  odds <- tbl(con, "odds1x2")
  odds |> tally() |> pull() -> n_observations
  
  # Cast as regular df so we can perform operations
  summary_stats <- as.data.frame(summary_stats)
  
  # extract values
  matches <- summary_stats[1,1]
  teams <- summary_stats[1,2]
  leagues <- summary_stats[1,3]
  
  # Make these figures accessible to the frontend
  output$overview_text <- renderText({
    paste(n_observations, "observations from", matches, "matches between", teams, "different teams, across", leagues, "different leagues")
  })
  
  # Reactive expression to prepare matches data for DT
  matches_data <- reactive({
    results |>
      select(event_id, home_team, away_team, league_name, starts) |>
      collect() |>
      mutate(Match_Date = format(as.Date(starts), "%Y-%m-%d")) |>
      select(event_id, home_team, away_team, league_name, Match_Date)
  })
  
  # Render the DT table with single row selection
  output$matches_table <- DT::renderDT({
    datatable(
      matches_data(),
      selection = 'single',  # Enable single row selection
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        scrollX = TRUE
      ),
      rownames = FALSE,
      colnames = c("Event ID", "Home Team", "Away Team", "League", "Match Date")
    )
  })
  
  # Placeholder for now, eventually proper odds over time plot
  output$single_odds_plot <- renderPlot({
    ggplot() + theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

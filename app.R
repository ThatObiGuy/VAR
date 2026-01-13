# VAR
# author - Owen F. O'Connor - owen.oconnor.2024@mumail.ie

library(shiny)
library(shinythemes) # Adding themeing package, quick way to make app more appealing with minimal effort

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
                               width = 1,
                               radioButtons(
                                 inputId = "menu",
                                 label = "Select View",
                                 choices = c("Overview", "Matches", "Teams", "Leagues"),
                                 selected = "Overview",
                                 inline = FALSE
                               )
                             ),
                             mainPanel( # display info on selected interest
                               width = 10,
                               conditionalPanel(condition = "input.menu == 'Overview'", h3("Overview screen")),
                               conditionalPanel(condition = "input.menu == 'Matches'", h3("Matches screen")),
                               conditionalPanel(condition = "input.menu == 'Teams'", h3("Teams screen")),
                               conditionalPanel(condition = "input.menu == 'Leagues'", h3("Leagues screen"))
                             )
                           )  # Closes sidebarLayout
                           
                  )  # tabPanel - Grapher ends
                  
                ) # navbarPage ends
) # fluidPage ends

# Define server logic
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)

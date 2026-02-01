# About module: static content for the About tab

mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("About VAR"),
    p("A Shiny app to help analyse data collected for my final year project on betting odds."),
    p("This app was developed as my application to the 2025/2026 Maynooth Data Science \"Shiny App Developement Competition\""),
    p("Intention is to have an easy-to-use webapp that will help visulatise the odds movements captured and identify any unusual patterns."),
    p("Users should be able to select subsets from the whole dataset and adjust using different filters/ modifiers."),
    hr(),
    h3("About the Developer"),
    p("My name is Owen F. O'Connor, I am a final year (MH207) data science student"),
    p("you can email me @ owen.oconnor.2024@mumail.ie"),
    tags$a(href="https://mulife.ie/society/data-science", "Data science society"),
    br(),
    tags$a(href="https://www.linkedin.com/in/owen-f-o-connor-7565001b3/", "LinkedIn")
  )
}

mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic required for static content
  })
}

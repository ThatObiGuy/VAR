# VAR
# author - Owen F. O'Connor - owen.oconnor.2024@mumail.ie

# Entry point for the Shiny app:
# > Loads global config
# > Wires top-level UI and module servers

source("global.R")
source("R/mod_about.R")
source("R/mod_howto.R")
source("R/mod_grapher.R")

# Top-level UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  navbarPage(
    title = "VAR",
    tabPanel("About", mod_about_ui("about")),
    tabPanel("How to use", mod_howto_ui("howto")),
    tabPanel("Grapher", mod_grapher_ui("grapher"))
  )
)

# Server: wire modules
server <- function(input, output, session) {
  mod_about_server("about")
  mod_howto_server("howto")
  mod_grapher_server("grapher")
}

# Run the application
shinyApp(ui = ui, server = server)

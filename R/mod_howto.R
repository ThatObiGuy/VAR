# How-to module: static guidance and embedded video

mod_howto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("How to Use the App"),
    p("1. tdb"),
    p("2. tdb"),
    p("3. tdb"),
    hr(),
    p("You can watch the video below as an example and guide."),
    div(
      style = "text-align: center;",
      tags$iframe(
        width = "560", height = "315",
        src = "https://www.youtube.com/embed/OErMB4WYkjE?si=0hzi8FV5ioTW2dlI",
        frameborder = "0",
        allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
        allowfullscreen = NA
      )
    )
  )
}

mod_howto_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Static content only
  })
}

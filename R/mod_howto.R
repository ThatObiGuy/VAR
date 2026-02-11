# How-to module: static guidance and embedded video

mod_howto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("How to Use the App"),
    
    # What the App Does
    h3("What This App Does"),
    p(
      "This application displays sports betting market data from a range of soccer events.",
      "The visualizations are designed to help you identify and exploit betting opportunities",
      "or design and deploy data-driven strategies."
    ),
    
    # About Plotly
    h3(
      tags$img(
        src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR6UTPV9TTPThzYSFv8Ps9o4hdlr84SRn_f5g&s",
        height = "20px",
        style = "vertical-align: middle; margin-right: 8px;"
      ),
      "Interactive Plotly Charts"
    ),
    p(
      "All plots in this app have been made using",
      tags$a("Plotly", href = "https://plotly.com/r/", target = "_blank"),
      ", a powerful interactive visualization library.",
      "This was done to leverage the great built-in features, making exploring the data even easier:"
    ),
    tags$ul(
      tags$li(
        tags$strong("Zoom & Pan:"),
        "Click and drag on any plot to zoom into a region of interest.",
        "Double-click to reset to the full view.",
        "Drag near an axis edge to zoom along that axis only."
      ),
      tags$li(
        tags$strong("Accurate Tooltips:"),
        "Hover over any data point to see more information,",
        "including team names, dates, probabilities, and confidence intervals."
      ),
      tags$li(
        tags$strong("Legend Filtering:"),
        "Click a league name in the legend to toggle its visibility.",
        tags$em("Double-click"),
        "a league to hide all others.",
        "This is essential for busy plots with many overlapping series—narrow your focus to just the leagues or teams you care about."
      ),
      tags$li(
        tags$strong("Download:"),
        "Use the camera icon in the top-right toolbar to save any plot as a static PNG image."
      )
    ),
    
    # Interpreting the Plots
    h3("Interpreting the Plots"),
    p(
      "Each visualization in the Grapher tab includes a",
      tags$strong("'?' button"),
      "next to its title.",
      "Click this button for guidance on how to read and interpret that specific chart,",
      "including what patterns to look for and what they might indicate about betting value."
    ),
    
    # Getting Started
    h3("Getting Started"),
    tags$ol(
      tags$li(
        tags$strong("Navigate to the Grapher tab"),
        "using the menu at the top of the page."
      ),
      tags$li(
        tags$strong("Select a view"),
        "from the sidebar: Overview, Matches, Teams, or Leagues."
      ),
      tags$li(
        tags$strong("Filter your data:"),
        "Use the dropdowns, or toggle groups via the legends as discussed above to narrow your results.",
        "Filtering is especially important for the busier plots and as the database grows, trends may also only exsist within subsets."
      ),
      tags$li(
        tags$strong("Interact with the charts:"),
        "Zoom, hover, and toggle legend items to explore patterns and anomalies."
      ),
      tags$li(
        tags$strong("Look for opportunities!")
      )
    ),
    
    # Video Walkthrough
    hr(),
    h3("Video Walkthrough"),
    p(
      "Watch the video below for a guided tour of the app's features and example use cases.",
      "The walkthrough demonstrates how to filter data, interpret each chart type,",
      "and use Plotly's interactive tools to uncover insights."
    ),
    div(
      style = "text-align: center;",
      tags$iframe(
        width = "560", height = "315",
        src = "https://www.youtube.com/embed/Xh-JgMK3Pdk?si=-8WWavZbodaCgWIe",
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

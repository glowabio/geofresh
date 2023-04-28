# This module configures the content of the GeoFRESH app demo page
# demo page module UI function

demoPageUI <- function(id, label = "demo_page") {
  ns <- NS(id)

  # configure demo page tabPanel
  tabPanel(
    title = "Tutorial",
    value = "panel4",
    div(
      style = "margin: auto; padding:0px 11px; max-width: 1500px;",
      mainPanel(
        div(
          style = "border: 1px solid grey; margin: 8px; padding: 22px;",
          includeMarkdown("www/tutorial.md")
        ),
        width = 100
      )
    )
  )
}

# demo page module server function
demoPageServer <- function(input, output, session) {
  # TODO
}

# This module configures the content of the GeoFRESH app demo page

# demo page module UI function
demoPageUI <- function(id, label = "de,p_page") {
  ns <- NS(id)

  # configure demo page tabPanel
  tabPanel(
    title = "Demo",
    value = "panel4",
    fluidPage(

      # Page title
      titlePanel("Demo", windowTitle = "GeoFRESH"),

      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # General information on the demo workflow
        column(
          12,
          h3("Demo workflow", align = "center"),
          p("TODO: add some intro text to demo workflow here..."),
          p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua.")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("CSV upload", align = "center"),
          p("TODO: add some info on CSV upload here; demo CSV data is already uploaded, provide link to demo data here as template?")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Map", align = "center"),
          p("TODO: add the demo map here")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Table", align = "center"),
          p("TODO: add the demo table here")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Point info summary", align = "center"),
          p("TODO: add the demo point info summary here")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Select environmental variables", align = "center"),
          p("TODO: add the environmental variables demo here")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Get routing info", align = "center"),
          p("TODO: add the routing info demo here")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Download results as CSV", align = "center"),
          p("TODO: add download demo results here")
        )
      )
    )
  )
}

# demo page module server function
demoPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      # TODO

    }
  )
}

# This module configures the content of the GeoFRESH app analysis page

# analysis page module UI function
analysisPageUI <- function(id, label = "analysis_page") {
  ns <- NS(id)

  # configure analysis page tabPanel
  tabPanel(
    title = "Analysis",
    value = "panel3",
    fluidPage(

      # Page title
      titlePanel("Analysis", windowTitle = "GeoFRESH"),

      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # General information on the analysis workflow
        column(
          12,
          h3("Analysis workflow", align = "center"),
          p("TODO: add some intro to analysis workflow text here..."),
          p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
             sed diam nonumy eirmod tempor invidunt ut labore et dolore
             magna aliquyam erat, sed diam voluptua.")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Map", align = "center"),
          p("TODO: add the map here")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Select environmental variables", align = "center"),
          envVarAnalysisUI(ns("analysis"))
          ),
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Get routing info", align = "center"),
          p("TODO: add the routing info module here")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Download results as CSV", align = "center"),
          p("TODO: add the download results module here")
        )
      )
    )
  )
}

# analysis page module server function
analysisPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      envVarAnalysisServer("analysis")
    }
  )
}

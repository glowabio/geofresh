# This module configures the content of the GeoFRESH app upload page

# upload page module UI function
uploadPageUI <- function(id, label = "front_page") {
  ns <- NS(id)

  # configure upload page tabPanel
  tabPanel(
    title = "Upload",
    value = "panel2",
    fluidPage(

      # Page title
      titlePanel("Upload", windowTitle = "GeoFRESH"),

      # Grid layout for upload page
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # General information on the application
        column(
          12,
          h3("CSV Upload", align = "center"),
          p("TODO: add some intro to workflow text here..."),
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
          h3("Table", align = "center"),
          p("TODO: add the table here")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Point info summary", align = "center"),
          p("TODO: add the point info summary here")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          h3("Download CSV", align = "center"),
          p("TODO: add the snapped points CSV download here")
        )
      )
    )
  )
}

# upload page module server function
uploadPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

    # TODO

    }
  )
}

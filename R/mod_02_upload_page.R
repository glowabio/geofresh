# This module configures the content of the GeoFRESH app upload page

# upload page module UI function
uploadPageUI <- function(id, label = "upload_page") {
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
          p("Upload your data here."),
          p("Please provide the your point data as a .csv table with the first
          three columns being 'id', 'longitude', 'latitude' in the WGS84
          coordinate reference system. Column names are flexible."),
          # UI function of CSV upload module. Upload a CSV file with three columns:
          # point id, longitude, latitude
          sidebarLayout(
            sidebarPanel(
              csvFileUI(ns("datafile"), "User data (.csv format)")
            ),
            mainPanel(
              # UI to show the uploaded CSV as a table
              dataTableOutput(ns("table"))
            )
          )

        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        column(
          12,
          # UI function of the map module. Map with points uploaded by the user
          mapOutput(ns("mapuserpoints"))
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
    # Server function of the upload CSV module. Upload a CSV file with three columns:
    # id, longitude, latitude and return a data frame as a reactive object
      datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
    # Takes the data frame with coordinates as input and creates a table. the object
    # "datafile" must be called as datafile() because it is a reactive object
      output$table <- renderDataTable({
        datafile()
      })
    # Server function of the map module. Map with points uploaded by the user
      mapServer("mapuserpoints", datafile())

    }
  )
}

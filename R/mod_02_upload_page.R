
# This module configures the content of the GeoFRESH app upload page

# upload page module UI function
uploadPageUI <- function(id, label = "upload_page") {
  ns <- NS(id)

  # configure upload page tabPanel
  tabPanel(
    title = "Upload and snap",
    value = "panel2",
    fluidPage(

      # Page title
      titlePanel("Upload and snap", windowTitle = "GeoFRESH"),

      # Grid layout for upload page
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # General information on the application
        column(
          12,
          p("Please provide your point data as a .csv table with the first
          three columns being 'id', 'latitude', 'longitude' in the WGS84
          coordinate reference system. Column names are flexible.

          After the upload, click on 'Snap points' to assign the
          points to the corresponding sub-catchment."),
          csvFileUI(ns("datafile"))


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
    # id, latitude, longitude and return a list with two data frames. One data frame
    # has the coordinates uploaded by the user and the other one have the coordinates
    # after snapping

      point <- csvFileServer("datafile", stringsAsFactors = FALSE)

    # Server function of the map module. Map with points uploaded by the user and
    # points created after snapping
      mapServer("mapuserpoints", point)


    # Return user point coordinates and snap points coordinates. This is the
    # input for the analysis page
      return(point)

    }
  )
}


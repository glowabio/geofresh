
# analysis page module UI function
analysisPageUI <- function(id, label = "analysis_page") {
  ns <- NS(id)

  # this goes inside TabPanel
  boxes <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      # Show a plot of the generated distribution
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # General information on the application
        column(
          12,
          box(p("TODO: add some intro to analysis workflow text here..."),
              p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
                  sed diam nonumy eirmod tempor invidunt ut labore et dolore
                  magna aliquyam erat, sed diam voluptua."),
              solidHeader = T, collapsible = T, width = 12,
              title = "Analysis workflow", status = "primary")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # UI function of the upload CSV file module
        column(
          12,
          box(p("Please provide your point data as a .csv table with the first
                three columns being 'id', 'longitude', 'latitude' in the WGS84
                coordinate reference system. Column names are flexible."),
              csvFileUI(ns("datafile")),
              solidHeader = T, collapsible = T, width = 12,
              title = "Upload your data", status = "primary", collapsed = TRUE)
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # UI function of the map module
        column(
          12,
          box(
            mapOutput(ns("mapanalysis")),
            solidHeader = T, collapsible = T, width = 12,
            title = "Map", status = "primary")
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # add env_var_analysis module UI
        column(
          12,
          box(
            envVarAnalysisUI(ns("analysis")),
            solidHeader = T, collapsible = T, width = 12,
            title = "Select environmental variables", status = "primary", collapsed = TRUE)
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # Routing information
        column(
          12,
          box(p("TODO: add the routing info module here"),
              solidHeader = T, collapsible = T, width = 12,
              title = "Get routing info", status = "primary", collapsed = TRUE)
        )
      ),
      fluidRow(
        style = "border: 1px solid grey; margin: 8px; padding: 12px;",
        # General information on the application
        column(
          12,
          box(p("TODO: add the download results module here"),
              solidHeader = T, collapsible = T, width = 12,
              title = "Download results as CSV", status = "primary", collapsed = TRUE)
        )
      )
    )
  )

  # configure analysis page tabPanel
  tabPanel(
    title = "Analysis",
    value = "panel3",
    fluidPage(
      # Page title
      titlePanel("Analysis", windowTitle = "GeoFRESH"),
      boxes
    )
  )
}

# analysis page module server function
analysisPageServer <- function(id, point) {
  moduleServer(
    id,
    function(input, output, session) {

      # Server function of the upload CSV module. Upload a CSV file with three columns:
      # id, longitude, latitude and return a list with two data frames. One data frame
      # has the coordinates uploaded by the user and the other one have the coordinates
      # after snapping

      point <- csvFileServer("datafile", stringsAsFactors = FALSE)

      # Server function of the map module. "Point" is a list with two data frames,
      # one with user's coordinates and the other one with coordinates generated
      # after snapping
      mapServer("mapanalysis", point)

      # Server function of the environmental variable analysis module
      envVarAnalysisServer("analysis")
    }
  )
}

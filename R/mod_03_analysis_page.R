# analysis page module UI function
library(shinydashboard)

analysisPageUI <- function(id, label = "analysis_page") {
  ns <- NS(id)
  # this goes inside TabPanel
  boxes <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      fluidRow(
        # General information on the application
        column(
          12,
          box(
            HTML("<p>
            The GeoFRESH platform allows to upload point data, snap (move) the points
            to the stream network, query upstream environmental information, and
            download the resulting table.
            </br> </br>
            Currently the platform includes 48 variables related to
            <a href='https://hydrography.org/hydrography90m/hydrography90m_layers'><b>topography and hydrography</b></a>, 19 <a href='http://chelsa-climate.org/'><b>climate variables</b></a>,
            (i.e., current bioclimatic variables), 15 <a href='https://soilgrids.org/'><b>soil</b></a> variables and
            22 <a href='http://maps.elie.ucl.ac.be/CCI/viewer/index.php'><b>land cover</b></a>
            variables.
            </br> </br>
            For the uploaded points, you will obtain </br>
             &nbsp; &nbsp;i) one table per environmental variable, where each row corresponds to one point, followed by
             the ID of the sub-catchment where the point falls into, and the summary
            statistics of the variable within this sub-catchment</br>
            &nbsp; &nbsp;ii) a table including summary statistics for the upstream catchment of each point
            </br> </br>  In the case that variables were scaled in the raster layers,
            we have rescaled them back to their original values in the tables.
               </p>"),
            solidHeader = T, collapsible = T, width = 12,
            title = "Analysis workflow", status = "primary"
          )
        )
      ),
      fluidRow(
        # UI function of the upload CSV file module
        column(
          12,
          box(p("Please provide your point data as a .csv table with
                three columns: a unique 'id', 'longitude', 'latitude'."),
              p("Coordinates should be provided in the WGS84 coordinate reference system.
                Column names are flexible. The number of points in your .csv file
                is currently limited to 1000, and upload file size should not exceed 1MB."),
            csvFileUI(ns("datafile")),
            solidHeader = T, collapsible = T, width = 12,
            title = "Upload your data", status = "primary", collapsed = FALSE
          )
        )
      ),
      fluidRow(
        # UI function of the map module
        column(
          12,
          box(
            mapOutput(ns("mapanalysis")),
            solidHeader = T, collapsible = T, width = 12,
            title = "Map", status = "primary"
          )
        )
      ),
      fluidRow(
        # add env_var_analysis module UI
        column(
          12,
          box(
            envVarAnalysisUI(ns("analysis")),
            solidHeader = T, collapsible = T, width = 12,
            title = "Select environmental variables", status = "primary", collapsed = TRUE
          )
        )
      ),
      fluidRow(
        # Routing information
        column(
          12,
          box(p("TODO: add the routing info module here"),
            solidHeader = T, collapsible = T, width = 12,
            title = "Get routing info", status = "primary", collapsed = TRUE
          )
        )
      ),
      fluidRow(
        # General information on the application
        column(
          12,
          box(p("TODO: add the download results module here"),
            solidHeader = T, collapsible = T, width = 12,
            title = "Download results as CSV", status = "primary", collapsed = TRUE
          )
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

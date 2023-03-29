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

        sidebarPanel(
          icon("triangle-exclamation", "fa-3x",
                             lib = "font-awesome", style = "color: #CB1F98"),
            h4("This feature is not functional for the moment,
            please come back in a few days!")),

        # General information on the analysis workflow
        column(
          12,
          h3("Analysis workflow", align = "center"),
          HTML("<p>A common approach to model freshwater habitats and biodiversity at large
            scales is to use <b>sub-catchments</b> as the unit of analysis.
            In the recently published <a href='https://hydrography.org'>Hydrography90m</a> dataset,
            726 million sub-catchments have been delineated globally.
            We have calculated summary statistics (i.e, mean, standard deviation,
            minimum, maximum and range) for each sub-catchment for a total of
            104 environmental variables, including 47 variables related to
            <a href='https://hydrography.org/hydrography90m/hydrography90m_layers'>topography and hydrography</a>,
            19 <a href='http://chelsa-climate.org/'>climate variables</a>,
            (current bioclimatic variables), 16 <a href='https://soilgrids.org/'>soil</a> variables and
            22 <a href='http://maps.elie.ucl.ac.be/CCI/viewer/index.php'>land cover</a>
            categories.
            </br>
            For the uploaded points, you will obtain </br>
            i) one table per environmental variable, where each row corresponds to one point, followed by
            the ID of the sub-catchment where the point falls into, and the summary
            statistics of the variable within this sub-catchment</br>
            ii) a table including summary statistics for the upstream catchment of each point
               </p>"
               ),
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
        # add env_var_analysis module UI
        envVarAnalysisUI(ns("analysis"))
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

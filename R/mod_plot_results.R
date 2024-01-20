# This module allows the user to plot the results of the environmental
# variables queries as interactive histograms or boxplots

# Module UI function
plotResultsUI <- function(id, label = "Plot results") {
  ns <- NS(id)
  tagList(
    p("Plot the results of environmental variables queries as histograms or boxplots."),
    tabsetPanel(
      tabPanel(
        "Topography",
        sidebarLayout(
          sidebarPanel(
            br(),
            sliderInput(
              ns("topo_bins"),
              "Number of bins:",
              min = 1,
              max = 20,
              value = 10
            )
          ),
          mainPanel(
            plotOutput(ns("topo_plot_local"))
          )
        )
      ),
      tabPanel(
        "Climate",
        sidebarLayout(
          sidebarPanel(
            br(),
            sliderInput(
              ns("clim_bins"),
              "Number of bins:",
              min = 1,
              max = 20,
              value = 10
            )
          ),
          mainPanel(
            plotOutput(ns("clim_plot_local"))
          )
        )
      ),
      tabPanel(
        "Soil",
        sidebarLayout(
          sidebarPanel(
            br(),
            sliderInput(
              ns("soil_bins"),
              "Number of bins:",
              min = 1,
              max = 20,
              value = 10
            )
          ),
          mainPanel(
            plotOutput(ns("soil_plot_local"))
          )
        )
      ),
      tabPanel(
        "Land cover",
        sidebarLayout(
          sidebarPanel(
            br()
          ),
          mainPanel(
            plotOutput(ns("land_plot_local"))
          )
        )
      )
    )
  )
}
# Module server function
# query results passed as reactive list of datasets by envVarAnalysisServer()
plotResultsServer <- function(id, datasets) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns

      # check if datasets is reactiveValues
      stopifnot(is.reactivevalues(datasets))
    }
  )
}

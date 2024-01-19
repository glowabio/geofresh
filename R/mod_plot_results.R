# This module allows the user to plot the results of the environmental
# variables queries as interactive histograms or boxplots

# Module UI function
plotResultsUI <- function(id, label = "Plot results") {
  ns <- NS(id)
  tagList(
    plotOutput(ns("local"))
  )
}
# Module server function
plotResultsServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns

    }
  )
}

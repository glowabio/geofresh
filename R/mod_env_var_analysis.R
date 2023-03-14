# This module allows the selecting environmental variables
# and querying the GeoFRESH database

library(dplyr)
library(shinyBS)
library(data.table)


# Module UI function
envVarAnalysisUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  uiOutput(ns("envCheckbox"))

  # simpleNetworkOutput(ns("coolplot"), height = "800px")

  # textOutput(ns("txt"))
}


# Module server function
envVarAnalysisServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      output$envCheckbox <- renderUI({
        extendedCheckboxGroup(
          label = "Environmental variables",
          choiceNames = choiceNames,
          choiceValues = choiceNames,
          selected = c("check2"),
          extensions = checkboxExtensions,
          inputId = "envCheckbox"
        )
      })

      # output$txt <- renderText({
      #   rendered <- paste(input$rendered, collapse = ", ")
      #   paste("You chose", rendered)
      # })
    }
  )
}

# This module allows the selecting environmental variables
# and querying the GeoFRESH database


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
          label = "Land cover",
          choiceNames = data_list_inputData$`Land cover`$Variable,
          choiceValues = data_list_inputData$`Land cover`$Variable,
          selected = c("check2"),
          extensions = checkboxExtensions$`Land cover`,
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

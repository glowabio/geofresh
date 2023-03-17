# This module allows the selecting environmental variables
# and querying the GeoFRESH database


# Module UI function
envVarAnalysisUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  # add elements to fluidRow() in module analysis page
  column(
    12,
    h3("Select environmental variables", align = "center"),
    p("TODO: do we need some text on env var selection here?"),
    fluidRow(
      column(
        3,
        uiOutput(ns("envCheckboxTopography"))
      ),
      column(
        3,
        uiOutput(ns("envCheckboxClimate"))
      ),
      column(
        3,
        uiOutput(ns("envCheckboxSoil"))
      ),
      column(
        3,
        uiOutput(ns("envCheckboxLandcover"))
      )
    )
  )

  # simpleNetworkOutput(ns("coolplot"), height = "800px")

  # textOutput(ns("txt"))
}


# Module server function
envVarAnalysisServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # render topography checkboxes
      output$envCheckboxTopography <- renderUI({
        extendedCheckboxGroup(
          label = "Topography",
          choiceNames = data_list_inputData$`Topography`$Variable,
          choiceValues = data_list_inputData$`Topography`$Variable,
          # selected = c("check2"),
          extensions = checkboxExtensions$`Topography`,
          inputId = "envCheckboxTopography"
        )
      })

      # render climate checkboxes
      output$envCheckboxClimate <- renderUI({
        extendedCheckboxGroup(
          label = "Climate",
          choiceNames = data_list_inputData$`Climate`$Variable,
          choiceValues = data_list_inputData$`Climate`$Variable,
          # selected = c("check2"),
          extensions = checkboxExtensions$`Climate`,
          inputId = "envCheckboxClimate"
        )
      })

      # render soil checkboxes
      output$envCheckboxSoil <- renderUI({
        extendedCheckboxGroup(
          label = "Soil",
          choiceNames = data_list_inputData$`Soil`$Variable,
          choiceValues = data_list_inputData$`Soil`$Variable,
          # selected = c("check2"),
          extensions = checkboxExtensions$`Soil`,
          inputId = "envCheckboxSoil"
        )
      })

      # # render land cover checkboxes
      output$envCheckboxLandcover <- renderUI({
        extendedCheckboxGroup(
          label = "Land cover",
          choiceNames = data_list_inputData$`Land cover`$Variable,
          choiceValues = data_list_inputData$`Land cover`$Variable,
          # selected = c("check2"),
          extensions = checkboxExtensions$`Land cover`,
          inputId = "envCheckboxLandcover"
        )
      })


      # output$txt <- renderText({
      #   rendered <- paste(input$rendered, collapse = ", ")
      #   paste("You chose", rendered)
      # })
    }
  )
}

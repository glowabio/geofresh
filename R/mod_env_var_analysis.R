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

  fluidPage(
    tabPanel("",

             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 fluid = FALSE,
                 uiOutput(ns("rendered"))),

               mainPanel(
                 simpleNetworkOutput(ns("coolplot"), height = "800px"),
                 width = 10)
             ),

             textOutput(ns("txt"))

    )
  )



}


# Module server function
envVarAnalysisServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {


      output$rendered <- renderUI({

        extendedCheckboxGroup(
          label = "Environmental variables",
          choiceNames  = choiceNames,
          choiceValues = choiceNames,
          selected = c("check2"),
          extensions = checkboxExtensions,
          inputId = "rendered"  )

      })

      output$txt <- renderText({
        rendered <- paste(input$rendered, collapse = ", ")
        paste("You chose", rendered)
      })






    }
  )
}

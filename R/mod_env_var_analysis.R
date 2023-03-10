# This module allows the selecting environmental variables
# and querying the GeoFRESH database

library(dplyr)
library(shinyBS)
library(networkD3)
library(shiny)


extendedCheckboxGroup <- function(..., extensions = list()) {
  cbg <- checkboxGroupInput(...)
  nExtensions <- length(extensions)
  nChoices <- length(cbg$children[[2]]$children[[1]])

  if (nExtensions > 0 && nChoices > 0) {
    lapply(1:min(nExtensions, nChoices), function(i) {
      # For each Extension, add the element as a child (to one of the checkboxes)
      cbg$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
    })
  }
  cbg
}

# place the button on the right and not directly after the text
bsButtonRight <- function(...) {
  btn <- bsButton(...)
  # Directly inject the style into the shiny element.
  btn$attribs$style <- "float: right;"
  btn
}


data <- fread("~/Documents/PhD/geofresh/geofresh_environmental_variables.csv")[1:108,] %>%
  select("Category", "Variable type", "Variable", "Description")
choiceNames <- data$Variable

txt <- data$Description

ids <- paste0("pB", rep(1:length(data$Variable)))

inputData <-  data.frame(cbid = ids, helpInfoText = txt)

inputData$cbid <- sapply(inputData$cbid, as.character)

inputData$helpInfoText <- sapply(inputData$helpInfoText, as.character)

checkBoxHelpList <- function(id, Text){

  extensionsList <- tipify(bsButtonRight(id, "?", trigger = "hover",
                                         size = "extra-small"), Text)

  return(extensionsList)

}


helpList <- split(inputData, f = rownames(inputData))

checkboxExtensions <- lapply(helpList, function(x) checkBoxHelpList(x[1],
                                                                    as.character(x[2])))



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

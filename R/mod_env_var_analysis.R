# This module allows the selecting environmental variables
# and querying the GeoFRESH database

library(dplyr)
library(shinyBS)
library(networkD3)

# Module UI function
envVarAnalysisUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)








}


# Module server function
envVarAnalysisServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {







    }
  )
}

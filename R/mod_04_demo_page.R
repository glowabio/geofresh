# This module configures the content of the GeoFRESH app demo page
# demo page module UI function

demoPageUI <- function(id, label = "demo_page") {
  ns <- NS(id)

  # configure demo page tabPanel
  tabPanel(
    title = "Demo",
    value = "panel4",
        fluidPage(
          # uiOutput("mark"),
      # titlePanel("Included Content"),
      mainPanel(
        includeMarkdown("www/tutorial.md"), width = 100
      )
    )


    )



}





# demo page module server function
demoPageServer <- function(input, output, session) {

      # TODO

    }

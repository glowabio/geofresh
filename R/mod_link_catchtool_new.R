# link to point-and-click catchment delineation tool

#UI
linkCatchtoolUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("link"), "Catchment delineation tool")
}

# server logic
linkCatchtoolServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$link, {
      browseURL("https://aqua.igb-berlin.de/upstream")
    })
  })
}

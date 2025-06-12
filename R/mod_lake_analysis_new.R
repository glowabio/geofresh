# Module to snap points.

# User interface
lakeAnalysisUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Lake analysis")
}


# Server logic
lakeAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Lake analysis",
          "this is the content of lake analysis",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}


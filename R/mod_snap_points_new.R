# Module to snap points.

# User interface
snapPointsUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Snap points")
}


# Server logic
snapPointsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Snap points",
          "this is the content of snap point",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}


# Module topography variables.

# User interface
topographyUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Topography")
}


# Server logic
topographyServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Topography variables",
          "this is the content of topography",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}


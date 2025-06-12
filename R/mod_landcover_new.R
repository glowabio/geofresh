# Module landcover variables.

# User interface
landcoverUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Landcover")
}


# Server logic
landcoverServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Landcover variables",
          "this is the content of landcover",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}


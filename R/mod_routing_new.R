# Module routing information.

# User interface
routingUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Routing information")
}


# Server logic
routingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Routing information",
          "this is the content of routing",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}


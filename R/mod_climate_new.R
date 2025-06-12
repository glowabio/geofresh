# Module climate variables.

# User interface
climateUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Climate")
}


# Server logic
climateServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Climate variables",
          "this is the content of climate",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}


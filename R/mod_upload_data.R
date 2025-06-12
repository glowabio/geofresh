# Module to upload data.

# User interface
uploadDataUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Upload data")
}


# Server logic
uploadDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Upload data",
          "this is the content of Upload data",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}


# ============================================================
# Catchment delineation & routing
# ============================================================

# UI
catchmentRoutingUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("open"), "Open catchment delineation & routing tool")
}

# Server
catchmentRoutingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$open, {
      showModal(
        modalDialog(
          title = "Catchment delineation & routing",
          easyClose = TRUE,
          footer = modalButton("Close"),
          div(
            class = "alert alert-info",
            HTML(
              "Text describing general workflow.<br/>
              "
            )
          )
        )
      )
    })
  })
}

# ============================================================
# Routing to outlets
# ============================================================

# UI
routingUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("open"), "Open routing tool")
}

# Server
routingServer <- function(id, points_db, paths_to_outlet) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Variables:
    # Here we will store the paths to outlet
    #paths_to_outlet <- reactive(NULL)


    # Observe:
    # When opening the routing tool, this window appears:
    observeEvent(input$open, {
      showModal(
        modalDialog(
          title = "Routing",
          easyClose = TRUE,
          #footer = modalButton("Close"),
          footer = tagList(
            modalButton("Close"),
            actionButton(ns("compute_route"), "Compute Route")
          ),
          div(
            class = "alert alert-info",
            HTML(
              "Here you can compute the path of each point to the sea.<br/>
              "
            )
          )
        )
      )
    }) # end of: observeEvent(input$open, ...
  })
}

# ============================================================
# Catchment delineation
# ============================================================

# UI
catchmentUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("open"), "Open catchment delineation tool")
}

# Server
catchmentServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$open, {
      showModal(
        modalDialog(
          title = "Catchment delineation",
          easyClose = TRUE,
          footer = modalButton("Close"),
          div(
            class = "alert alert-info",
            HTML(
              "Here you can compute the points' upstream catchments, either ",
	      "as polygons (upstream subcatchments) or as lines (upstream ",
	      "stream segments).<br/>"
            )
          )
	  #uiOutput(ns("catch_btn_ui"))
        )
      )
    })
  })
}

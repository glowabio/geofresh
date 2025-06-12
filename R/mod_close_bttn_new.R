# Close button
# Module UI
modalCloseButtonUI <- function(id) {
  ns <- NS(id)
  tags$button(
    HTML("&times;"),
    class = "custom-close-btn",
    onclick = sprintf("Shiny.setInputValue('%s', true, {priority: 'event'});", ns("closeModal"))
  )
}

# Module Server
modalCloseButtonServer <- function(id, closeAction) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$closeModal, {
      closeAction()
    })
  })
}

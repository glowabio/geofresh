# Display a message that can be accepted only by clicking the
# accept button or outside the modal dialog window

# Modal dialog module UI function
modalDialogUI <- function(id, label = "Modal") {
  ns <- NS(id)
  tagList(
      actionLink(ns("show"), "Privacy Policy"
                 # a("Privacy Policy",
                 #               style = "font-size:0.8em;")
                 )
  )
}

# Modal dialog module server function
modalDialogServer <- function(id) {
  moduleServer(
    id,
      function(input, output, session) {
        observeEvent(input$show, {
          showModal(modalDialog(
            title = "Privacy Policy",
            includeMarkdown("privacy_policy.md"),
            footer = modalButton("Close"),
            easyClose = T)
          )
        }
      )
    }
  )
}





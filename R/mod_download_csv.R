# This module download a data frame as a CSV file

# Module UI function

downloadDataUI <- function(id, label = "Download") {
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadData"), label, icon = NULL)
  )
}

# Module server function

downloadDataServer <- function(id, data, file_name = "") {
  moduleServer(
    id,
    function(input, output, session) {
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("geofresh-", Sys.Date(), file_name, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(data, row.names = FALSE, file)
        }
      )
    }
  )
}

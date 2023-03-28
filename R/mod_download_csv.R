# This module download a data frame as a CSV file

# Module UI function

downloadDataUI <- function(id) {
    ns <- NS(id)
    tagList(
      downloadButton(ns("downloadData"), "Download", icon = NULL)
    )
  }

# Module server function

downloadDataServer <- function(id, data_download) {
    moduleServer(
      id,
      function(input, output, session) {
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("geofresh-", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(data_download, row.names = FALSE, file)
        }
      )
    }
  )
}


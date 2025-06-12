# This module creates a download button to download all output in a zip file

# module UI function
zipDownloadUI <- function(id, label = "zipdownload") {
  ns <- NS(id)
  tagList(
      # download button for zipped results
      uiOutput(ns("download_zipped"))
  )
}

# module server function. datasets: output from mod_env_analysis
zipDownloadServer <- function(id, datasets) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(datasets(), {
        # UI for download button zipped results
        output$download_zipped <- renderUI({
          column(
            12,
            wellPanel(
              fluidRow(
                column(
                  2,
                  # download button for zipped results
                  downloadDataUI(ns("download_zipped"), label = "Download ZIP")
                ),
                column(
                  10,
                  p("Download resulting CSVs in a ZIP file")
                )
              )
            )
          )
        })

        downloadDataServer("download_zipped",
                           dataset = datasets,
                           zipped = TRUE,
                           file_name = "-results"
        )
      })

    }
  )
}

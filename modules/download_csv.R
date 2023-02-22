## This is an example of a download link.
## We could customized it and transform it into a module


  ui <- fluidPage(
    downloadLink("downloadData", "Download")
  )

  server <- function(input, output) {
    # Our dataset
    data <- mtcars

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data, file)
      }
    )
  }

  shinyApp(ui, server)


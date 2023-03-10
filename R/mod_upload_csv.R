# This module allows the user to upload a CSV file and returns data frame
# with the date from the CSv file
library(dplyr)

# Module UI function
csvFileUI <- function(id, label = "CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label)
  )
}


# Module server function
csvFileServer <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })

      # The user's data, parsed into a data frame
      dataframe <- reactive({
        read.csv(userFile()$datapath,
                 header = TRUE,
                 stringsAsFactors = stringsAsFactors) %>%
          rename(id = 1, longitude = 2, latitude = 3)
      })

      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })

      # Return the reactive that yields the data frame
      return(dataframe)
    }
  )
}

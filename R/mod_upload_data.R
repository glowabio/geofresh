# Module to upload data.

# User interface
uploadDataUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Upload data")
}


# Server logic
uploadDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Upload data",
          easyClose = TRUE,
          footer = modalButton("Close"),

          #### This is the content of Upload data #####

          # Upload a CSV file with three columns:
          # point id, latitude, longitude.
          # Using uiOutput instead of fileInput to be able to reset form
          # when test data set is uploaded.
          uiOutput(ns("file")),

          # Button for loading test data csv
          actionButton(
            ns("test_data"),
            "Load test data",
            icon = icon("table"),
            class = "btn-primary"
          )
        )
      )
    })

    # create reactive value for either test or user data
    data <- reactiveVal()

    # Render file input
    output$file <- renderUI({
      fileInput(ns("file"), label = "Point data (.csv format)", accept = ".csv")
    })

    # The selected file, if any
    user_file <- reactive({
      # If no file is selected, don't do anything
      validate(need(input$file, message = FALSE))
      # Check if file extension is '.csv', otherwise display error message
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        input$file
      } else {
        clear_user_input(empty_df, map_proxy())
        validate(showModal(modalDialog(
          title = "Warning",
          "Invalid format: Please upload a .csv file",
          easyClose = TRUE
        )))
      }
    })

    # Read and check .csv file


    # If test_data action button is clicked load test data
    observeEvent(input$test_data, {
      test_data <- read.csv("./www/data/test_points.csv",
                            header = TRUE,
                            stringsAsFactors = FALSE
      ) %>% rename(id = 1, latitude = 2, longitude = 3)
      # test_data into reactive value
      data(test_data)

      # Reset file input (only if a file was already uploaded)
      req(input$file)
      output$file <- renderUI({
        fileInput(ns("file"), label = "Point data (.csv format)", accept = ".csv")
      })
    })

  # Module output. A data frame with either user data or test data
  return(data)

  })
}


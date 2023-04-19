# This module allows the user to upload a CSV file and returns data frame
# with the date from the CSv file
library(dplyr)
library(DT)
# Module UI function
csvFileUI <- function(id, label = "CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      # Upload a CSV file with three columns:
      # point id, longitude, latitude
      fileInput(ns("file"), label = "Point data (.csv format)", accept = ".csv"),
      # snapping points button
      snapPointUI(ns("snap"))
    ),
    mainPanel(
      # show the uploaded CSV as a table
      DTOutput(ns("table")),
      uiOutput(ns("download"))
    )
  )
}


# Module server function
csvFileServer <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns

      # output, non-reactive, to show an empty table
      df_non_ractive <- matrix(ncol = 3, nrow = 10) %>%
        as.data.frame()

      # Empty table, before uploading CSV file
      output$table <- renderDT({
        datatable(
          df_non_ractive,
          options = list(
            deferRender = TRUE,
            scrollX = TRUE,
            scrollY = "150px"
          ),
          rownames = FALSE,
          colnames = c("id", "longitude", "latitude")
        )
      })

      # The selected file, if any
      user_file <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })

      # The user's coordinates, parsed into a data frame
      coordinates_user <- reactive({
        # The user's coordinate, parsed into a data frame
        read.csv(user_file()$datapath,
                 header = TRUE,
                 stringsAsFactors = stringsAsFactors) %>%
          rename(id = 1, longitude = 2, latitude = 3)
      })

      # If a CSV file is uploaded, UI with a snap button. If click button, snap
      coordinates_snap <- snapPointServer("snap", coordinates_user())

      # The user data showed in a table after the CSV file is uploaded
      observeEvent(user_file(), {
        output$table <- renderDT({
          datatable(
            coordinates_user(),
            options = list(
              deferRender = TRUE,
              scrollX = TRUE,
              scrollY = "150px"
            ),
            rownames = FALSE
          )
        })
      })

      # User's coordinates and snapped point coordinates showed in a table
      observeEvent(coordinates_snap(), {
        output$table <- renderDT({
          datatable(
            coordinates_snap(),
            options = list(
              deferRender = TRUE,
              scrollX = TRUE,
              scrollY = "150px"
            ),
            rownames = FALSE,
            colnames = c("id", "longitude", "latitude",
                         "new longitude", "new latitude")
          )
        })
      })

      # If click snap button, give the option to download the table
      observeEvent(coordinates_snap(), {
        output$download <- renderUI({
          tagList(
            hr(),
            downloadDataUI(ns("download"))
          )
        })

        downloadDataServer("download", data = coordinates_snap())

      })

      # Module output. A list with two reactive expressions, one with the user's
      # coordinates and other with snapped point coordinates. Use this as input
      # for the module map and analysis page
      list(
        user_points = reactive(coordinates_user()),
        snap_points = reactive(coordinates_snap())
      )

    }
  )
}

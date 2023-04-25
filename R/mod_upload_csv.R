# This module allows the user to upload a CSV file and returns a data frame
# with the data from the CSV file
library(dplyr)
library(DT)
# Module UI function
csvFileUI <- function(id, label = "CSV file") {
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
csvFileServer <- function(id, map_proxy, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns

      # output, non-reactive, to show an empty table
      df_non_reactive <- matrix(ncol = 3, nrow = 10) %>%
        as.data.frame()

      # Empty table, before uploading CSV file
      empty_table <- renderDT({
        datatable(
          df_non_reactive,
          options = list(
            deferRender = TRUE,
            scrollX = TRUE,
            scrollY = "150px"
          ),
          rownames = FALSE,
          colnames = c("id", "longitude", "latitude")
        )
      })
      output$table <- empty_table

      # set number of expected columns and rows in user csv file
      num_columns <- 3
      num_rows <- 1000

      # function to clear table and map if user file is invalid
      clear_user_input <- function(empty_table, map_proxy) {
        output$table <- empty_table
        map_proxy %>%
          setView(0, 10, 2.5) %>%
          clearMarkers() %>%
          clearControls() %>%
          hideGroup("Input Points")
      }


      # The selected file, if any
      user_file <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        # check if file extension is '.csv', otherwise display error message
        ext <- tools::file_ext(input$file$name)
        if (ext == "csv") {
          input$file
        } else {
          clear_user_input(empty_table, map_proxy())
          validate(showModal(modalDialog(
            title = "Warning",
            "Invalid format: Please upload a .csv file",
            easyClose = TRUE
          )))
        }
      })

      # The user's coordinates, parsed into a data frame
      coordinates_user <- reactive({
        req(user_file())
        # upload file size limit is set to 1 MB in app.R
        input_csv <- read.csv(user_file()$datapath,
          header = TRUE,
          stringsAsFactors = stringsAsFactors
        )
        # check if uploaded csv contains three columns
        if (ncol(input_csv) == num_columns) {
          # check if input csv contains not more than 1000 points
          if (nrow(input_csv) <= num_rows) {
            # check if ID (first column) is unique
            if (any(duplicated(input_csv[, 1]))) {
              clear_user_input(empty_table, map_proxy())
              validate(showModal(modalDialog(
                title = "Warning",
                "Invalid format: ID is not unique.",
                easyClose = TRUE
              )))
            } else {
              # check if coordinates are valid using leaflet::validateCoords function
              tryCatch(
                expr = {
                  leaflet_warning <- leaflet::validateCoords(
                    lng = input_csv[, 2],
                    lat = input_csv[, 3],
                    funcName = "Snapping points",
                    mode = "point"
                  )
                  # user input point data csv to return, if no warnings or errors occur

                  # TODO: check if range of lat/lon is correct

                  input_csv <- rename(input_csv, id = 1, longitude = 2, latitude = 3)
                },
                warning = function(leaflet_warning) {
                  # if coordinates are invalid display warning from validateCoords function
                  clear_user_input(empty_table, map_proxy())
                  validate(showModal(modalDialog(
                    title = "Warning",
                    leaflet_warning[[1]],
                    easyClose = TRUE
                  )))
                },
                error = function(leaflet_warning) {
                  # if coordinates are invalid display error from validateCoords function
                  clear_user_input(empty_table, map_proxy())
                  validate(showModal(modalDialog(
                    title = "Error",
                    leaflet_warning[[1]],
                    easyClose = TRUE
                  )))
                }
              )
            }
          } else {
            clear_user_input(empty_table, map_proxy())
            validate(showModal(modalDialog(
              title = "Warning",
              "Invalid format: The number of points to analyse is currently limited to 1000.",
              easyClose = TRUE
            )))
          }
        } else {
          clear_user_input(empty_table, map_proxy())
          validate(showModal(modalDialog(
            title = "Warning",
            "Invalid format: Your .csv file must contain 3 columns ('id', 'longitude', 'latitude')",
            easyClose = TRUE
          )))
        }
      })

      # If a CSV file is uploaded, UI with a snap button. If click button, snap
      coordinates_snap <- snapPointServer("snap", coordinates_user())

      # The user data showed in a table after the CSV file is uploaded
      observeEvent(user_file(), {
        req(coordinates_user())
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
            colnames = c(
              "id", "longitude", "latitude",
              "new longitude", "new latitude"
            )
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

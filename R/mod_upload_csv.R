# This module allows the user to upload a CSV file and returns a data frame
# with the data from the CSV file
library(dplyr)
library(uuid)

# Module UI function
csvFileUI <- function(id, label = "CSV file") {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      # Upload a CSV file with three columns:
      # point id, longitude, latitude
      fileInput(ns("file"), label = "Point data (.csv format)", accept = ".csv"),
      # button for loading test data csv
      actionButton(
        ns("test_data"),
        "Load test data",
        icon = icon("table"),
        class = "btn-primary"
      ),
      # snapping points button
      snapPointUI(ns("snap"))
    ),
    mainPanel(
      # show the uploaded CSV as a table
      tableOutput(ns("csv_table")),
      uiOutput(ns("download_snapped"))
    )
  )
}


# Module server function
csvFileServer <- function(id, map_proxy, stringsAsFactors) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # non-reactive data frame for displaying an empty table
      empty_df <- matrix(ncol = 3, nrow = 10) %>% as.data.frame()
      # column names for empty table
      column_names <- c("ID", "longitude", "latitude")

      column_defs <- list(
        list(orderable = FALSE, targets = "_all")
      )

      # render empty table, before uploading CSV file
      tableServer("csv_table", empty_df, column_names, column_defs, searching = FALSE)

      # set number of expected columns and rows in user csv file
      num_columns <- 3
      num_rows <- 1000

      # function to reset progress bar if new csv is selected
      reset_progress_bar <- function(id) {
        updateProgressBar(
          session = session,
          id = id,
          value = 0,
          total = 6,
          title = ""
        )
      }

      # function to clear table, progress bar and map if user file is invalid
      clear_user_input <- function(empty_df, map_proxy) {
        tableServer("csv_table", empty_df, column_names)

        reset_progress_bar("panel3-datafile-snap-pb2")

        map_proxy %>%
          setView(0, 10, 2.5) %>%
          hideGroup(c("Input points", "Snapped points")) %>%
          clearMarkers() %>%
          clearControls()
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
          clear_user_input(empty_df, map_proxy())
          validate(showModal(modalDialog(
            title = "Warning",
            "Invalid format: Please upload a .csv file",
            easyClose = TRUE
          )))
        }
      })

      # create reactive value for either test or user data
      coordinates_user <- reactiveVal()

      # if test_data action button is clicked load test data
      observeEvent(input$test_data, {
        input_csv <- read.csv("./www/data/test_points.csv",
          header = TRUE,
          stringsAsFactors = stringsAsFactors
        ) %>% rename(id = 1, longitude = 2, latitude = 3)
        coordinates_user(input_csv)

        # reset progress bar
        reset_progress_bar("panel3-datafile-snap-pb2")
      })

      # The user's coordinates, parsed into a data frame
      observeEvent(user_file(), {
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
              clear_user_input(empty_df, map_proxy())
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

                  # TODO: check if range of lat/lon is correct

                  # user input point data csv to return, if no warnings or errors occur
                  input_csv <- rename(input_csv, id = 1, longitude = 2, latitude = 3)
                  # write to reactive value coordinates_user
                  coordinates_user(input_csv)
                  # reset progress bar
                  reset_progress_bar("panel3-datafile-snap-pb2")
                },
                warning = function(leaflet_warning) {
                  # if coordinates are invalid display warning from validateCoords function
                  clear_user_input(empty_df, map_proxy())
                  validate(showModal(modalDialog(
                    title = "Warning",
                    leaflet_warning[[1]],
                    easyClose = TRUE
                  )))
                },
                error = function(leaflet_warning) {
                  # if coordinates are invalid display error from validateCoords function
                  clear_user_input(empty_df, map_proxy())
                  validate(showModal(modalDialog(
                    title = "Error",
                    leaflet_warning[[1]],
                    easyClose = TRUE
                  )))
                }
              )
            }
          } else {
            clear_user_input(empty_df, map_proxy())
            validate(showModal(modalDialog(
              title = "Warning",
              "Invalid format: The number of points to analyse is currently limited to 1000.",
              easyClose = TRUE
            )))
          }
        } else {
          clear_user_input(empty_df, map_proxy())
          validate(showModal(modalDialog(
            title = "Warning",
            "Invalid format: Your .csv file must contain 3 columns ('ID', 'longitude', 'latitude')",
            easyClose = TRUE
          )))
        }
      })

      # create reactive value for input point table name
      input_point_table <- reactiveVal()

      # Create database table for user input points and render table
      # after CSV upload succeeded
      observeEvent(coordinates_user(), {
        # generate UUID for unique table name
        uuid <- UUIDgenerate(use.time = TRUE, output = "string")
        # set database table name
        table_name <- SQL(paste0("points_", uuid))
        # write to reactive value input_point_table
        input_point_table(table_name)

        # set user input points schema and table name
        table_id <- Id(schema = "shiny_user", table = table_name)

        tryCatch(
          expr = {
            # create table in schema "shiny_user" and upload data frame
            dbWriteTable_error <- dbWriteTable(pool, table_id, coordinates_user())

            # run ANALYZE to update database table statistics
            sql <- sqlInterpolate(pool,
              "ANALYZE ?point_table",
              point_table = dbQuoteIdentifier(pool, table_id)
            )
            dbExecute(pool, sql)

            # render table with user input points
            table_proxy <- tableServer("csv_table", coordinates_user(), column_names)
          },
          error = function(dbWriteTable_error) {
            message(dbWriteTable_error[[1]])
            clear_user_input(empty_df, map_proxy())
            validate(showModal(modalDialog(
              title = "Error",
              "Database error: Please restart the CSV upload.",
              easyClose = TRUE
            )))
          }
        )

        # register function to delete user input database table
        # when session for this user ends
        session$onSessionEnded(function() {
          dbRemoveTable(pool, table_id, fail_if_missing = FALSE)
        })
      })

      # call module snap_points
      # render UI with a snap button. If click button, snap
      # pass reactive value input_point_table with user database table name
      # gets updated when new file is uploaded
      coordinates_snap <- snapPointServer("snap", input_point_table)

      # User's coordinates and snapped point coordinates displayed in the table
      observeEvent(coordinates_snap(), {
        # set column names for snapping result table
        col_names_snap <- c(
          "ID", "longitude", "latitude",
          "longitude_snap", "latitude_snap", "sub-catchment_ID"
        )
        # call table module to render snapping result data
        tableServer("csv_table", coordinates_snap(), col_names_snap)
        # give warning if coordinate contain NA values (points fall into the          ocean)
        if (any(is.na(coordinates_snap()$subc_id))) {
          showNotification(
            tags$div(
              tags$b("Warning:"),
              tags$br(),
              sum(is.na(coordinates_snap()$subc_id)),
              " points are not located in a sub-catchment and are removed from further analysis."
            ),
            duration = NULL,
            type = "warning"
          )
        }
      })

      # If click snap button, give the option to download the table
      observeEvent(coordinates_snap(), {
        output$download_snapped <- renderUI({
          tagList(
            hr(),
            downloadDataUI(ns("download"))
          )
        })

        downloadDataServer("download",
          data = coordinates_snap(),
          file_name = "-snapped-method-sub-catchment"
        )
      })

      # Module output. A list with three reactive expressions:
      # - data frame with user's coordinates uploaded from CSV
      # - data frame with user coordinates and snapped coordinates
      # - user coordinates database table name
      # Use this as input for the modules map and env_var_analysis
      list(
        user_points = coordinates_user,
        snap_points = coordinates_snap,
        user_table = input_point_table
      )
    }
  )
}

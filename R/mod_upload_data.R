# Module to upload data.

# User interface
uploadDataUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "1. Upload data")
}


# Server logic
uploadDataServer <- function(id, ds) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------- Modal trigger ---------------------------------------
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Upload data",
          easyClose = TRUE,
          footer = modalButton("Close"),
          div(class = "alert alert-info",
              HTML("Please upload a CSV with <code>id</code>, <code>latitude</code>, <code>longitude</code> (WGS84), or load test data. Column names are flexible."),
              tags$small(class = "text-muted", "Limits: ≤ 1000 points, ≤ 1 MB.")),
          uiOutput(ns("file")),
          actionButton(
            ns("test_data"),
            "Load test data",
            icon = icon("table"),
            class = "btn-primary"
          )
        )
      )
    })

    # Active dataset
    data <- reactiveVal(NULL)

    # File input
    output$file <- renderUI({
      fileInput(ns("file"), label = "", accept = ".csv")
    })

    # ---- validator ------------------------------
    validate_points_strict <- function(df, land = getOption("myapp.land_mask")) {

      # 1) Columns: three columns
      if (ncol(df) != 3) {
        showNotification(
          "Invalid format: Your .csv file must contain 3 columns ('id', 'latitude', 'longitude')",
          type = "error", duration = 8
        )
        return(NULL)
      }

      # rename columns
      names(df)[1:3] <- c("id", "latitude", "longitude")

      # 2) Row limit
      if (nrow(df) > 1000) {
        showNotification(
          "Invalid format: The number of points to analyse is currently limited to 1000.",
          type = "error", duration = 8
        )
        return(NULL)
      }

      # 3) ID uniqueness (report per-ID rows; no coercion)
      tab <- table(df$id, useNA = "ifany")
      dup_ids <- names(tab[tab > 1])
      if (length(dup_ids) > 0) {
        MAX_IDS <- 15
        MAX_ROWS_PER_ID <- 12
        pieces <- character(0)
        for (idv in head(dup_ids, MAX_IDS)) {
          idx <- which(df$id %in% idv)  # 1-based rows where this id occurs
          pieces <- c(pieces, paste0(
            if (is.na(idv)) "NA" else as.character(idv),
            " at rows ",
            paste(head(idx, MAX_ROWS_PER_ID), collapse = ", "),
            if (length(idx) > MAX_ROWS_PER_ID) " …" else ""
          ))
        }
        showNotification(
          paste0(
            "Invalid format: IDs are not unique. Duplicated entries — ",
            paste(pieces, collapse = "; "),
            if (length(dup_ids) > MAX_IDS) "; …" else ""
          ),
          type = "error", duration = 12
        )
        return(NULL)
      }


      # 3b) ID must be integer-ish and within range
      id_raw <- df$id

      # try numeric coercion (keep original for reporting)
      id_num <- suppressWarnings(as.numeric(id_raw))

      bad_id <- which(is.na(id_num) | !is.finite(id_num) | (id_num %% 1 != 0))
      if (length(bad_id) > 0) {
        preview <- paste(head(bad_id, 50), collapse = ", ")
        showNotification(
          paste0(
            "Invalid format: 'id' must be an integer number (e.g., 1, 2, 3...). ",
            "Problematic row(s): ", preview,
            if (length(bad_id) > 50) " …" else ""
          ),
          type = "error", duration = 12
        )
        return(NULL)
      }

      # check ids are integer an not missing
      id_num <- suppressWarnings(as.numeric(df$id))
      bad_id <- which(is.na(id_num) | !is.finite(id_num) | (id_num %% 1 != 0))
      if (length(bad_id) > 0) {
        showNotification(
          paste0("Invalid format: 'id' must be an integer number. Bad row(s): ",
                 paste(head(bad_id, 50), collapse = ", "),
                 if (length(bad_id) > 50) " …" else ""),
          type = "error", duration = 12
        )
        return(NULL)
      }

      # store as numeric
      df$id <- id_num

      # 4) Coordinate validity via leaflet::validateCoords()
      #    Surface warnings/errors directly.
      valid_coords <- tryCatch(
        {
          leaflet::validateCoords(
            lat = df[,2],
            lng = df[,3],
            funcName = "Snapping points",
            mode = "point"
          )
          TRUE
        },
        warning = function(w) {
          showNotification(conditionMessage(w), type = "error", duration = 12)
          FALSE
        },
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 12)
          FALSE
        }
      )
      if (!isTRUE(valid_coords)) return(NULL)

      # 5) Out-of-range checks (list offending rows explicitly)
      lat <- suppressWarnings(as.numeric(df[,2]))
      lon <- suppressWarnings(as.numeric(df[,3]))

      lat_out <- which(lat < -90 | lat > 90 | is.na(lat))
      lon_out <- which(lon < -180 | lon > 180 | is.na(lon))
      bad_rows <- sort(unique(c(lat_out, lon_out)))

      if (length(bad_rows) > 0) {
        preview <- paste(utils::head(bad_rows, 50), collapse = ", ")
        showNotification(
          paste0(
            "Invalid coordinates: latitude/longitude must be numeric and within ranges ",
            "[-90, 90] and [-180, 180]. Problematic row(s): ",
            preview,
            if (length(bad_rows) > 50) " …" else ""
          ),
          type = "error", duration = 12
        )
        return(NULL)
      }

      # Keep id type as-is; set lat/lon to numeric for downstream modules
      df[,2]  <- lat
      df[,3] <- lon

      # Rename columns
      df <- rename(df, id = 1, latitude = 2, longitude = 3)

      df
    }

    # Selected user file
    user_file <- reactive({
      validate(need(input$file, message = FALSE))
      ext <- tools::file_ext(input$file$name)
      if (!identical(tolower(ext), "csv")) {
        showNotification("Invalid format: Please upload a .csv file", type = "error", duration = 5)
        return(NULL)
      }
      input$file
    })

    # Read + validate user CSV
    observeEvent(user_file(), ignoreInit = FALSE, {
      uf <- user_file(); req(uf)
      df <- tryCatch(
        read.csv(uf$datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) NULL
      )
      if (is.null(df)) {
        showNotification("Read error: could not read the CSV file.", type = "error", duration = 5)
        return(invisible(NULL))
      }

      df_valid <- validate_points_strict(df)
      if (is.null(df_valid)) return(invisible(NULL))

      data(df_valid)

    })

    # Load bundled test data (also must comply strictly)
    observeEvent(input$test_data, {
      df <- tryCatch(
        read.csv("./www/data/test_points.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) NULL
      )
      if (is.null(df)) {
        showNotification("Missing test data: ./www/data/test_points.csv not found.", type = "error", duration = 5)
        return(invisible(NULL))
      }

      df_valid <- validate_points_strict(df)
      if (is.null(df_valid)) return(invisible(NULL))

      data(df_valid)

      # Reset file input if one had been selected
      if (!is.null(input$file)) {
        output$file <- renderUI({
          fileInput(ns("file"), label = "Point data (.csv format)", accept = ".csv")
        })
      }
    })

    # Reactive to check upload was done
    upload_done <- reactiveVal(0L)

    # Write uploaded points into the EXISTING per-session table
    observeEvent(data(), {
      df <- data(); req(df)

      # base cols only
      df_base <- df[, c("id", "latitude", "longitude"), drop = FALSE]
      df_base$id <- as.numeric(df_base$id)

      # Ensure per-session table exists (safe even if already exists)
      ds$ensure()
      table_id <- ds$table_id()

      tryCatch(
        expr = {
          pool::poolWithTransaction(pool, function(conn) {
            write_points_base_db(conn, table_id, df_base)
          })

          # notify Shiny that DB changed
          ds$bump_version()

          # upload done
          upload_done(upload_done() + 1L)

          showNotification(
            "Upload successful. Please snap points before analysis.",
            type = "message", duration = 6
          )
        },
        error = function(e) {
          message(conditionMessage(e))
          showModal(modalDialog(
            title = "Error",
            paste("Database error:", conditionMessage(e)),
            easyClose = TRUE
          ))
        }
      )
    })



    # Output list with data.frame and input table name. Input table name will
    # be used by the snap point module and the environmental variable module
    list(upload_done = reactive(upload_done()))


  })
}

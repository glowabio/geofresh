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

    # ------------------- Modal trigger ---------------------------------------
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Upload data",
          easyClose = TRUE,
          footer = modalButton("Close"),
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
      fileInput(ns("file"), label = "Point data (.csv format)", accept = ".csv")
    })

    # ---- strict validator ------------------------------
    validate_points_strict <- function(df) {
      required <- c("id", "latitude", "longitude")

      # 1) Columns: exactly the required set (any order)
      if (ncol(df) != 3 || !setequal(names(df), required)) {
        showNotification(
          "Invalid format: Your .csv file must contain 3 columns ('id', 'latitude', 'longitude')",
          type = "error", duration = 8
        )
        return(NULL)
      }

      # reorder (no type changes to id)
      df <- df[, required, drop = FALSE]

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

      # 4) Coordinate validity via leaflet::validateCoords()
      #    Surface warnings/errors directly.
      valid_coords <- tryCatch(
        {
          leaflet::validateCoords(
            lat = df$latitude,
            lng = df$longitude,
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
      lat <- suppressWarnings(as.numeric(df$latitude))
      lon <- suppressWarnings(as.numeric(df$longitude))

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
      df$latitude  <- lat
      df$longitude <- lon
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
    observeEvent(user_file(), ignoreInit = TRUE, {
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

    # Output reactive data.frame
    return(reactive({ data() }))
  })
}

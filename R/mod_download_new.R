# --- download data module: accordion with nested "Local" / "Upstream" ----

# UI ----
downloadDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap;",
      actionLink(ns("select_all"), "Select all"),
      span("·"),
      actionLink(ns("select_none"), "Clear")
    ),
    # Top-level flat options
    checkboxGroupInput(
      inputId = ns("simple"),
      label   = "Basic datasets",
      choices = c("Snapped points" = "snapped",
                  "Lakes"          = "lakes"),
      selected = character(0)
    ),
    # Accordion with nested checkboxes
    bslib::accordion(
      id = ns("acc"),
      open = FALSE,
      bslib::accordion_panel(
        "Topography",
        checkboxGroupInput(
          ns("topography_opts"), label = NULL,
          choices = c("Local" = "local", "Upstream" = "upstream"),
          selected = character(0)
        )
      ),
      bslib::accordion_panel(
        "Climate",
        checkboxGroupInput(
          ns("climate_opts"), label = NULL,
          choices = c("Local" = "local", "Upstream" = "upstream"),
          selected = character(0)
        )
      ),
      bslib::accordion_panel(
        "Soil",
        checkboxGroupInput(
          ns("soil_opts"), label = NULL,
          choices = c("Local" = "local", "Upstream" = "upstream"),
          selected = character(0)
        )
      ),
      bslib::accordion_panel(
        "Land cover",
        checkboxGroupInput(
          ns("landcover_opts"), label = NULL,
          choices = c("Local" = "local", "Upstream" = "upstream"),
          selected = character(0)
        )
      )
    ),
    br(),
    downloadButton(ns("download"), "Download", style = "margin-left:8px;"),
    br(), br(),
    verbatimTextOutput(ns("log"))
  )
}

# Server (skeleton) ----
downloadDataServer <- function(id,
                               r_snapped = NULL,
                               r_lakes   = NULL
                               # Add more reactives later if needed
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helpers for "select all" / "clear"
    observeEvent(input$select_all, {
      updateCheckboxGroupInput(session, "simple", selected = c("snapped","lakes"))
      updateCheckboxGroupInput(session, "topography_opts", selected = c("local","upstream"))
      updateCheckboxGroupInput(session, "climate_opts",    selected = c("local","upstream"))
      updateCheckboxGroupInput(session, "soil_opts",       selected = c("local","upstream"))
      updateCheckboxGroupInput(session, "landcover_opts",  selected = c("local","upstream"))
    })
    observeEvent(input$select_none, {
      updateCheckboxGroupInput(session, "simple", selected = character(0))
      updateCheckboxGroupInput(session, "topography_opts", selected = character(0))
      updateCheckboxGroupInput(session, "climate_opts",    selected = character(0))
      updateCheckboxGroupInput(session, "soil_opts",       selected = character(0))
      updateCheckboxGroupInput(session, "landcover_opts",  selected = character(0))
    })

    # Collect selections into normalized keys like "topography_local"
    sel <- reactive({
      basic <- input$simple %||% character(0)

      make_keys <- function(prefix, v) {
        if (is.null(v) || !length(v)) character(0) else paste0(prefix, "_", v)
      }
      topo  <- make_keys("topography", input$topography_opts)
      clim  <- make_keys("climate",    input$climate_opts)
      soil  <- make_keys("soil",       input$soil_opts)
      land  <- make_keys("landcover",  input$landcover_opts)

      all <- c(basic, topo, clim, soil, land)
      validate(need(length(all) > 0, "Select at least one dataset."))
      all
    })

    prepared <- reactiveVal(NULL)
    log_txt  <- reactiveVal("Ready.")

    observeEvent(input$prepare, {
      req(sel())
      log_txt("Preparing data...")
      bundle <- list()

      # ---- BASIC ----
      if ("snapped" %in% sel()) {
        bundle$snapped <- if (is.null(r_snapped)) {
          data.frame(id = integer(), latitude_snap = numeric(), longitude_snap = numeric())
        } else r_snapped()
      }
      if ("lakes" %in% sel()) {
        bundle$lakes <- if (is.null(r_lakes)) {
          data.frame(id = integer(), hylak_id = integer(), hydrolake_name = character())
        } else r_lakes()
      }

      # ---- NESTED (placeholders) ----
      # Replace with your real export/prep for each scope
      if ("topography_local"   %in% sel()) bundle$topography_local   <- data.frame(dummy = 1)
      if ("topography_upstream"%in% sel()) bundle$topography_upstream<- data.frame(dummy = 1)

      if ("climate_local"      %in% sel()) bundle$climate_local      <- data.frame(dummy = 1)
      if ("climate_upstream"   %in% sel()) bundle$climate_upstream   <- data.frame(dummy = 1)

      if ("soil_local"         %in% sel()) bundle$soil_local         <- data.frame(dummy = 1)
      if ("soil_upstream"      %in% sel()) bundle$soil_upstream      <- data.frame(dummy = 1)

      if ("landcover_local"    %in% sel()) bundle$landcover_local    <- data.frame(dummy = 1)
      if ("landcover_upstream" %in% sel()) bundle$landcover_upstream <- data.frame(dummy = 1)

      prepared(bundle)
      log_txt(paste0("Prepared: ", paste(names(bundle), collapse = ", "), "."))
    })

    output$log <- renderText(log_txt())

    # Download: single CSV if 1 item, otherwise ZIP
    output$download <- downloadHandler(
      filename = function() {
        if (length(sel()) == 1) paste0(sel(), ".csv") else "datasets_bundle.zip"
      },
      content = function(file) {
        bundle <- prepared(); req(bundle)

        if (length(bundle) == 1) {
          nm  <- names(bundle)[1]
          tmp <- tempfile(fileext = ".csv")
          write.csv(bundle[[nm]], tmp, row.names = FALSE)
          file.copy(tmp, file, overwrite = TRUE)
        } else {
          owd <- setwd(tempdir()); on.exit(setwd(owd), add = TRUE)
          csv_files <- character(0)
          for (nm in names(bundle)) {
            path <- paste0(nm, ".csv")
            write.csv(bundle[[nm]], path, row.names = FALSE)
            csv_files <- c(csv_files, path)
          }
          zip(zipfile = file, files = csv_files, flags = "-r9Xq")
        }
      }
    )
  })
}




# --- download data module: accordion with nested "Local" / "Upstream" ----

downloadDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap;",
      actionLink(ns("select_all"), "Select all"),
      span("·"),
      actionLink(ns("select_none"), "Clear")
    ),
    hr(),
    # Accordion with nested checkboxes
    bslib::accordion(
      id = ns("acc"),
      open = FALSE,
      bslib::accordion_panel(
        "Points",
        checkboxGroupInput(
          ns("points_opts"), label = NULL,
          choices = c("Points" = "points"),
          selected = character(0)
        )
      ),
      bslib::accordion_panel(
        "Lakes",
        checkboxGroupInput(
          ns("lakes_opts"), label = NULL,
          choices = c("Lakes" = "lakes"),
          selected = character(0)
        )
      ),
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
    div(
      style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap; margin-left:8px;",
      downloadButton(ns("download"), "Download")
    ),
    br(), br(),
    verbatimTextOutput(ns("log"))
  )
}

downloadDataServer <- function(id,
                               r_points   = NULL,
                               r_lakes     = NULL,
                               r_topo_loc  = NULL,
                               r_topo_up   = NULL,
                               r_clim_loc  = NULL,
                               r_clim_up   = NULL,
                               r_soil_loc  = NULL,
                               r_soil_up   = NULL,
                               r_land_loc  = NULL,
                               r_land_up   = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%||%` <- function(x, y) if (is.null(x)) y else x

    # helper: does a reactive data frame have data?
    has_data <- function(r) {
      if (is.null(r)) return(FALSE)
      df <- r()
      if (is.null(df)) return(FALSE)
      is.data.frame(df) && nrow(df) > 0
    }

    # availability reactives
    has_points      <- reactive(has_data(r_points))
    has_lakes        <- reactive(has_data(r_lakes))
    has_topo_loc     <- reactive(has_data(r_topo_loc))
    has_topo_up      <- reactive(has_data(r_topo_up))
    has_clim_loc     <- reactive(has_data(r_clim_loc))
    has_clim_up      <- reactive(has_data(r_clim_up))
    has_soil_loc     <- reactive(has_data(r_soil_loc))
    has_soil_up      <- reactive(has_data(r_soil_up))
    has_land_loc     <- reactive(has_data(r_land_loc))
    has_land_up      <- reactive(has_data(r_land_up))

    # choice builders
    points_choices <- reactive({
      ch <- character(0)
      if (has_points()) {
        ch <- c("Points" = "points")
      }
      ch
    })

    lakes_choices <- reactive({
      ch <- character(0)
      if (has_lakes()) {
        ch <- c("Lakes" = "lakes")
      }
      ch
    })

    topography_choices <- reactive({
      ch <- character(0)
      if (has_topo_loc()) ch <- c(ch, "Local"    = "local")
      if (has_topo_up())  ch <- c(ch, "Upstream" = "upstream")
      ch
    })

    climate_choices <- reactive({
      ch <- character(0)
      if (has_clim_loc()) ch <- c(ch, "Local"    = "local")
      if (has_clim_up())  ch <- c(ch, "Upstream" = "upstream")
      ch
    })

    soil_choices <- reactive({
      ch <- character(0)
      if (has_soil_loc()) ch <- c(ch, "Local"    = "local")
      if (has_soil_up())  ch <- c(ch, "Upstream" = "upstream")
      ch
    })

    landcover_choices <- reactive({
      ch <- character(0)
      if (has_land_loc()) ch <- c(ch, "Local"    = "local")
      if (has_land_up())  ch <- c(ch, "Upstream" = "upstream")
      ch
    })

    # dynamic checkbox choices
    observe({
      ch <- points_choices()
      selected <- intersect(input$points_opts %||% character(0), unname(ch))
      updateCheckboxGroupInput(
        session, "points_opts",
        choices = ch,
        selected = selected
      )
    })

    observe({
      ch <- lakes_choices()
      selected <- intersect(input$lakes_opts %||% character(0), unname(ch))
      updateCheckboxGroupInput(
        session, "lakes_opts",
        choices = ch,
        selected = selected
      )
    })

    observe({
      ch <- topography_choices()
      selected <- intersect(input$topography_opts %||% character(0), unname(ch))
      updateCheckboxGroupInput(
        session, "topography_opts",
        choices = ch,
        selected = selected
      )
    })

    observe({
      ch <- climate_choices()
      selected <- intersect(input$climate_opts %||% character(0), unname(ch))
      updateCheckboxGroupInput(
        session, "climate_opts",
        choices = ch,
        selected = selected
      )
    })

    observe({
      ch <- soil_choices()
      selected <- intersect(input$soil_opts %||% character(0), unname(ch))
      updateCheckboxGroupInput(
        session, "soil_opts",
        choices = ch,
        selected = selected
      )
    })

    observe({
      ch <- landcover_choices()
      selected <- intersect(input$landcover_opts %||% character(0), unname(ch))
      updateCheckboxGroupInput(
        session, "landcover_opts",
        choices = ch,
        selected = selected
      )
    })

    # "select all" / "clear"
    observeEvent(input$select_all, {
      updateCheckboxGroupInput(
        session, "points_opts",
        selected = unname(points_choices())
      )
      updateCheckboxGroupInput(
        session, "lakes_opts",
        selected = unname(lakes_choices())
      )
      updateCheckboxGroupInput(
        session, "topography_opts",
        selected = unname(topography_choices())
      )
      updateCheckboxGroupInput(
        session, "climate_opts",
        selected = unname(climate_choices())
      )
      updateCheckboxGroupInput(
        session, "soil_opts",
        selected = unname(soil_choices())
      )
      updateCheckboxGroupInput(
        session, "landcover_opts",
        selected = unname(landcover_choices())
      )
    })

    observeEvent(input$select_none, {
      updateCheckboxGroupInput(session, "points_opts",    selected = character(0))
      updateCheckboxGroupInput(session, "lakes_opts",     selected = character(0))
      updateCheckboxGroupInput(session, "topography_opts",selected = character(0))
      updateCheckboxGroupInput(session, "climate_opts",   selected = character(0))
      updateCheckboxGroupInput(session, "soil_opts",      selected = character(0))
      updateCheckboxGroupInput(session, "landcover_opts", selected = character(0))
    })

    # selections → normalized keys
    sel <- reactive({
      pts   <- input$points_opts %||% character(0)   # "uploaded", "points"
      lakes <- input$lakes_opts  %||% character(0)   # "lakes"

      make_keys <- function(prefix, v) {
        if (is.null(v) || !length(v)) character(0) else paste0(prefix, "_", v)
      }
      topo <- make_keys("topography", input$topography_opts)
      clim <- make_keys("climate",    input$climate_opts)
      soil <- make_keys("soil",       input$soil_opts)
      land <- make_keys("landcover",  input$landcover_opts)

      c(pts, lakes, topo, clim, soil, land)
    })

    log_txt  <- reactiveVal("Ready.")
    output$log <- renderText(log_txt())

    # download handler
    output$download <- downloadHandler(
      filename = function() {
        s <- sel()
        if (length(s) == 1) paste0(s,"-geofresh-", Sys.Date(), ".csv")
        else paste0("datasets_bundle", "-geofresh-", Sys.Date(), ".zip")
      },
      content = function(file) {
        s <- sel()

        # if nothing selected, notify and abort
        if (!length(s)) {
          showNotification(
            "Please select at least one dataset before downloading.",
            type = "warning",
            duration = 4
          )
          stop("No dataset selected for download.")
        }

        log_txt("Preparing data for download...")
        bundle <- list()

        # ---- POINTS / LAKES ----

        # points
        if ("points" %in% s && !is.null(r_points)) {
          df <- r_points()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$points <- df
          }
        }


        # lakes
        if ("lakes" %in% s && !is.null(r_lakes)) {
          df <- r_lakes()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$lakes <- df
          }
        }

        # ---- TOPOGRAPHY ----
        if ("topography_local" %in% s && !is.null(r_topo_loc)) {
          df <- r_topo_loc()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$topography_local <- df
          }
        }

        if ("topography_upstream" %in% s && !is.null(r_topo_up)) {
          df <- r_topo_up()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$topography_upstream <- df
          }
        }

        # ---- CLIMATE ----
        if ("climate_local" %in% s && !is.null(r_clim_loc)) {
          df <- r_clim_loc()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$climate_local <- df
          }
        }

        if ("climate_upstream" %in% s && !is.null(r_clim_up)) {
          df <- r_clim_up()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$climate_upstream <- df
          }
        }

        # ---- SOIL ----
        if ("soil_local" %in% s && !is.null(r_soil_loc)) {
          df <- r_soil_loc()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$soil_local <- df
          }
        }

        if ("soil_upstream" %in% s && !is.null(r_soil_up)) {
          df <- r_soil_up()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$soil_upstream <- df
          }
        }

        # ---- LAND COVER ----
        if ("landcover_local" %in% s && !is.null(r_land_loc)) {
          df <- r_land_loc()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$landcover_local <- df
          }
        }

        if ("landcover_upstream" %in% s && !is.null(r_land_up)) {
          df <- r_land_up()
          if (is.data.frame(df) && nrow(df) > 0) {
            bundle$landcover_upstream <- df
          }
        }

        # if nothing actually collected, abort with error notification
        if (!length(bundle)) {
          showNotification("No data could be collected for the selected datasets.",
                           type = "error", duration = 5)
          stop("No data in bundle.")
        }

        # write file(s)
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

        log_txt(paste0("Prepared and downloaded: ", paste(names(bundle), collapse = ", "), "."))
      }
    )
  })
}

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)

snapPointsUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Snap points")
}

snapPointsServer <- function(
    id,
    input_point_table_name,     # reactive() -> string table name (e.g. ds$table_name)
    on_db_changed = NULL        # optional callback, e.g. ds$bump_version
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # simple state: "no_data", "ready", "snapping", "await_new_data"
    state <- reactiveVal("no_data")

    # -------------------- Progress helper --------------------
    custom_updateProgressBar <- function(perc, sleep = 0.05) {
      updateProgressBar(session = session, id = ns("progress_snap"), value = perc)
      Sys.sleep(sleep)
    }

    # -------------------- DB readiness (supports workflow 4) --------------------
    refresh_ready_state <- function() {
      tn <- input_point_table_name()
      if (is.null(tn) || !nzchar(tn)) {
        state("no_data")
        return(invisible())
      }

      table_id <- DBI::Id(schema = "shiny_user", table = tn)

      has_rows <- with_pool_connection(pool, function(conn) {
        tbl_q <- DBI::dbQuoteIdentifier(conn, table_id)
        DBI::dbGetQuery(conn, paste0(
          "SELECT EXISTS (SELECT 1 FROM ", tbl_q, " LIMIT 1) AS has"
        ))$has[[1]]
      })

      state(if (isTRUE(has_rows)) "ready" else "no_data")
      invisible()
    }

    # -------------------- Modal dialog --------------------
    observeEvent(input$show_modal, {
      refresh_ready_state()
      showModal(
        modalDialog(
          title = "Snap points",
          tagList(
            hr(),
            tags$b("Choose snapping method"),
            radioButtons(
              inputId = ns("snap_method"),
              label = NULL,
              choices = c("Sub-catchment (default)" = "subcatchment",
                          "Snap point to nearest stream segment" = "nearest"),
              selected = "subcatchment",
              inline = FALSE
            ),
            uiOutput(ns("method_help")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'nearest'", ns("snap_method")),
              br(),
              numericInput(
                inputId = ns("search_radius_m"),
                label   = "Maximum search distance (meters)",
                value   = 500,
                min     = 1,
                step    = 50
              ),
              helpText("Points farther than this distance from any stream segment will remain unsnapped.")
            ),
            br(),
            uiOutput(ns("snap_btn_ui")),
            br(),
            progressBar(
              id = ns("progress_snap"),
              value = 0,
              title = " ",
              display_pct = TRUE
            ),
            shinyjs::hidden(p(id = ns("text1"), "Processing..."))
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })

    # help text
    output$method_help <- renderUI({
      if (is.null(input$snap_method) || input$snap_method == "subcatchment") {
        div(
          class = "alert alert-info",
          tagList(
            tags$b("Snapping method: sub-catchment"),
            p("Points will be snapped to the nearest location on the river segment of the sub-catchment the point falls in.")
          )
        )
      } else {
        div(
          class = "alert alert-info",
          tagList(
            tags$b("Snapping method: nearest stream segment"),
            p("For each point, find the geographically nearest stream segment and project the point orthogonally onto that segment."),
            tags$ul(
              tags$li("Uses geometric proximity (not sub-catchment containment)."),
              tags$li("Can limit maximum distance; points beyond remain unsnapped.")
            )
          )
        )
      }
    })

    # button UI
    output$snap_btn_ui <- renderUI({
      s <- state()
      btn <- actionButton(
        ns("snap_button"),
        label = "Snap points",
        icon  = icon("arrow-right"),
        class = "btn btn-primary",
        disabled = !identical(s, "ready")
      )

      if (s == "no_data") {
        span(title = "Please upload or create points first.", btn)
      } else if (s == "snapping") {
        span(title = "Processing...", btn)
      } else if (s == "await_new_data") {
        span(title = "Upload/edit points before snapping again.", btn)
      } else {
        btn
      }
    })

    # refresh readiness when table name changes
    observeEvent(input_point_table_name(), {
      refresh_ready_state()
    }, ignoreInit = TRUE)

    # -------------------- Outputs --------------------
    snapped_data <- reactiveVal(NULL)
    lake_data    <- reactiveVal(NULL)

    # -------------------- Snapping --------------------
    observeEvent(input$snap_button, {
      req(identical(state(), "ready"))

      pt_name <- input_point_table_name()
      req(pt_name)

      state("snapping")
      shinyjs::show(ns("text1"))
      custom_updateProgressBar(0)

      pt_name <- input_point_table_name()
      points_table <- DBI::Id(schema = "shiny_user", table = pt_name)

      tryCatch(
        expr = {

          # Run snap in one DB transaction
          pool::poolWithTransaction(pool, function(conn) {
            snap_points_subcatchment_db(
              conn         = conn,
              points_table = points_table,
              progress     = function(p) custom_updateProgressBar(p)
            )
          })

          # tell app to re-read DB (points_db)
          if (is.function(on_db_changed)) on_db_changed()

          # Read results (optional outputs)
          snapped_data(with_pool_connection(pool, function(conn) {
            read_points_db(conn, points_table)
          }))

          # If you added read_lakes_for_points_db() helper, use it
          lake_data(with_pool_connection(pool, function(conn) {
            if (exists("read_lakes_for_points_db", mode = "function")) {
              read_lakes_for_points_db(conn, points_table)
            } else {
              # fallback: return NULL (or keep your old SQL here if you prefer)
              NULL
            }
          }))

          showNotification("Snapping finished.", type = "message", duration = 5)

          shinyjs::hide(ns("text1"))
          custom_updateProgressBar(0, sleep = 0.1)
          state("await_new_data")

        },
        error = function(e) {
          message(conditionMessage(e))
          shinyjs::hide(ns("text1"))
          custom_updateProgressBar(0, sleep = 0.1)
          showModal(modalDialog(
            title = "Snapping failed",
            paste("Database error:", conditionMessage(e)),
            easyClose = TRUE
          ))
          state("ready")
        }
      )
    })

    list(
      snapped_data  = snapped_data,
      snapped_lakes = lake_data
    )
  })
}

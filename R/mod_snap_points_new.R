library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)

snapPointsUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "2. Snap points")
}

snapPointsServer <- function(
    id,
    input_point_table_name,     # reactive() -> string table name
    db_version,
    on_db_changed = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    state <- reactiveVal("no_data")

    # -------------------- Progress helper --------------------
    custom_updateProgressBar <- function(perc, sleep = 0.05) {
      updateProgressBar(session = session, id = ns("progress_snap"), value = perc)
      Sys.sleep(sleep)
    }

    # -------------------- DB readiness --------------------
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
              choices = c("Sub-catchment (default)" = "subcatchment"),
              # c(
              #   "Sub-catchment (default)" = "subcatchment",
              #   "Closest stream of a chosen Strahler order" = "strahler"
              # ),
              selected = "subcatchment",
              inline = FALSE
            ),

            uiOutput(ns("method_help")),

            # Controls for the Strahler method
            conditionalPanel(
              condition = sprintf("input['%s'] == 'strahler'", ns("snap_method")),
              br(),
              numericInput(
                inputId = ns("target_strahler"),
                label   = "Target Strahler order",
                value   = 3,
                min     = 1,
                step    = 1
              ),
              numericInput(
                inputId = ns("search_radius_m"),
                label   = "Maximum search distance (meters)",
                value   = 500,
                min     = 1,
                step    = 50
              ),
              helpText("Each point will snap to the nearest stream segment with the selected Strahler order. Points farther than the maximum distance will remain unsnapped.")
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
            p("Points will be snapped to the nearest location on the river segment of the sub-catchment the point falls in (using the hint if provided).")
          )
        )
      } else {
        div(
          class = "alert alert-info",
          tagList(
            tags$b("Snapping method: closest stream by Strahler order"),
            p("For each point, find the geographically nearest stream segment with the selected Strahler order and project the point onto that segment."),
            tags$ul(
              tags$li("Uses geometric proximity, not sub-catchment containment."),
              tags$li("Only segments with the selected Strahler order are considered."),
              tags$li("A maximum search distance can be enforced; points beyond remain unsnapped.")
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

    observeEvent(input_point_table_name(), {
      refresh_ready_state()
    }, ignoreInit = TRUE)

    observeEvent(db_version(), {
      # DB changed due to upload/editor/snap => allow snapping again
      refresh_ready_state()

      # if we were blocking re-snap, unblock now
      if (identical(state(), "await_new_data")) state("ready")
    }, ignoreInit = TRUE)


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

      points_table <- DBI::Id(schema = "shiny_user", table = pt_name)
      method <- input$snap_method %||% "subcatchment"

      tryCatch(
        expr = {
          pool::poolWithTransaction(pool, function(conn) {

            if (identical(method, "subcatchment")) {
              snap_points_subcatchment_db(
                conn         = conn,
                points_table = points_table,
                progress     = function(p) custom_updateProgressBar(p)
              )

            } else if (identical(method, "strahler")) {
              target <- as.integer(input$target_strahler %||% 3L)
              radius <- as.numeric(input$search_radius_m %||% 500)

              snap_points_strahler_db(
                conn            = conn,
                points_table    = points_table,
                target_strahler = target,
                search_radius_m = radius,
                progress        = function(p) custom_updateProgressBar(p)
              )
            }
          })

          if (is.function(on_db_changed)) on_db_changed()

          snapped_data(with_pool_connection(pool, function(conn) {
            read_points_db(conn, points_table)
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
      snapped_data  = snapped_data
    )
  })
}

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
    steps <- 6
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
      req(input_point_table_name())

      state("snapping")
      shinyjs::show(ns("text1"))
      custom_updateProgressBar(0)

      pt_name <- input_point_table_name()
      points_table <- DBI::Id(schema = "shiny_user", table = pt_name)

      tryCatch(
        expr = {
          pool::poolWithTransaction(pool, function(conn) {

            # base columns (geom_orig/geom_hint/geom_snap/snap_state/...)
            ensure_points_schema(conn, points_table)
            pt_q <- DBI::dbQuoteIdentifier(conn, points_table)

            # 0) derived cols (idempotent)
            DBI::dbExecute(conn, paste0(
              "ALTER TABLE ", pt_q, "
                 ADD COLUMN IF NOT EXISTS subc_id integer,
                 ADD COLUMN IF NOT EXISTS basin_id integer,
                 ADD COLUMN IF NOT EXISTS strahler_order smallint,
                 ADD COLUMN IF NOT EXISTS reg_id smallint,
                 ADD COLUMN IF NOT EXISTS hylak_id integer,
                 ADD COLUMN IF NOT EXISTS upstream bigint[]"
            ))
            custom_updateProgressBar(100 / steps)

            # 1) geom_orig with SRID 4326
            DBI::dbExecute(conn, paste0(
              "UPDATE ", pt_q, "
                 SET geom_orig = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)"
            ))
            custom_updateProgressBar(100 / steps * 2)

            # 2) spatial index (idempotent)
            idx_name <- paste0(pt_name, "_geom_orig_idx")
            idx_q <- DBI::dbQuoteIdentifier(conn, idx_name)
            DBI::dbExecute(conn, paste0(
              "CREATE INDEX IF NOT EXISTS ", idx_q,
              " ON ", pt_q, " USING GIST (geom_orig)"
            ))
            custom_updateProgressBar(100 / steps * 3)

            # 3) reg_id
            reg_q <- DBI::dbQuoteIdentifier(conn, DBI::Id(schema = "hydro", table = "regional_units"))
            DBI::dbExecute(conn, paste0(
              "UPDATE ", pt_q, " poi
                  SET reg_id = reg.reg_id
                 FROM ", reg_q, " reg
                WHERE ST_Intersects(poi.geom_orig, reg.geom)"
            ))
            custom_updateProgressBar(100 / steps * 4)

            # 4) hylak_id
            lak_q <- DBI::dbQuoteIdentifier(conn, DBI::Id(schema = "hydro", table = "hydrolakes_poly"))
            DBI::dbExecute(conn, paste0(
              "UPDATE ", pt_q, " poi
                  SET hylak_id = lak.hylak_id
                 FROM ", lak_q, " lak
                WHERE ST_Intersects(poi.geom_orig, lak.geom)"
            ))

            # 5) subc_id + basin_id
            subc_q <- DBI::dbQuoteIdentifier(conn, DBI::Id(schema = "hydro", table = "sub_catchments"))
            DBI::dbExecute(conn, paste0(
              "UPDATE ", pt_q, " poi SET
                     subc_id  = sub.subc_id,
                     basin_id = sub.basin_id
                FROM ", subc_q, " sub
               WHERE ST_Intersects(poi.geom_orig, sub.geom)
                 AND poi.reg_id = sub.reg_id"
            ))
            custom_updateProgressBar(100 / steps * 5)

            # 6) snap to stream segment in subcatchment
            seg_q <- DBI::dbQuoteIdentifier(conn, DBI::Id(schema = "hydro", table = "stream_segments"))
            DBI::dbExecute(conn, paste0(
              "UPDATE ", pt_q, " poi SET
                   strahler_order = seg.strahler,
                   geom_snap = ST_LineInterpolatePoint(
                     seg.geom,
                     ST_LineLocatePoint(seg.geom, COALESCE(poi.geom_hint, poi.geom_orig))
                   ),
                   snap_state = 'snapped',
                   snap_fail_reason = NULL
              FROM ", seg_q, " seg
              WHERE seg.subc_id = poi.subc_id"
            ))

            # mark failures
            DBI::dbExecute(conn, paste0(
              "UPDATE ", pt_q, "
                  SET snap_state = 'failed',
                      snap_fail_reason = 'no segment in subcatchment'
                WHERE geom_snap IS NULL"
            ))

            custom_updateProgressBar(100 / steps * 6)
            DBI::dbExecute(conn, paste0("ANALYZE ", pt_q))
          })

          # tell app to re-read DB (points_db)
          if (is.function(on_db_changed)) on_db_changed()

          # Read results (optional outputs)
          snapped_data(with_pool_connection(pool, function(conn) {
            read_points_db(conn, points_table)
          }))

          lake_data(with_pool_connection(pool, function(conn) {
            pt_q  <- DBI::dbQuoteIdentifier(conn, points_table)
            hl_q  <- DBI::dbQuoteIdentifier(conn, DBI::Id(schema = "hydro", table = "hydrolakes_poly"))
            li_q  <- DBI::dbQuoteIdentifier(conn, DBI::Id(schema = "hydro", table = "lake_intersections"))

            DBI::dbGetQuery(conn, paste0(
              "SELECT poi.id, poi.hylak_id,
                      hylak.lake_name AS hydrolake_name, hylak.lake_area AS hydrolake_area,
                      lak.subc_id AS outlet_subc_id,
                      round(lak.latitude::numeric, 6) AS outlet_latitude,
                      round(lak.longitude::numeric, 6) AS outlet_longitude
                 FROM ", pt_q, " poi
                 JOIN ", hl_q, " hylak
                   ON poi.hylak_id = hylak.hylak_id
                 JOIN ", li_q, " lak
                   ON poi.hylak_id = lak.hylak_id
                  AND poi.reg_id = lak.reg_id
                WHERE lak.outlet_id = 1"
            ))
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

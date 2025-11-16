library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)

snapPointsUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Snap points")
}

snapPointsServer <- function(id, input_point_table) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # simple state: "no_data", "ready", "snapping", "await_new_data"
    state <- reactiveVal("no_data")

    # Modal dialog
    observeEvent(input$show_modal, {
      showModal(
        modalDialog(
          title = "Snap points",
          tagList(
            hr(),
            tags$b("Choose snapping method"),
            # --- method selector ---
            radioButtons(
              inputId = ns("snap_method"),
              label = NULL,
              choices = c("Sub-catchment (default)" = "subcatchment",
                          "Snap point to nearest stream segment" = "nearest"),
              selected = "subcatchment",
              inline = FALSE
            ),

            # --- dynamic method help text ---
            uiOutput(ns("method_help")),

            # --- optional settings for 'nearest' method (e.g., search distance) ---
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

            # Button is rendered here depending on state (you already have this)
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

    # Dynamic help text for the modal dialogue
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
        div( class = "alert alert-info",
             tagList(
               tags$b("Snapping method: nearest stream segment"),
               p("For each point, find the geographically nearest stream segment and project the point orthogonally onto that segment."),
               tags$ul(
                 tags$li("Does not require the point to fall inside the same sub-catchment; it uses pure geometric proximity."),
                 tags$li("Optionally constrain the search to a maximum distance to avoid snapping across valleys or to distant streams; if no segment lies within the distance, the point remains unsnapped.")
              )
              )
             )
      }
    })

    # Render the button (with or without native tooltip) based on state
    output$snap_btn_ui <- renderUI({
      s <- state()
      btn <- actionButton(
        ns("snap_button"),
        label = "Snap points",
        icon  = icon("arrow-right"),
        class = "btn btn-primary",
        # disabled when not "ready"
        disabled = !identical(s, "ready")
      )

      if (s == "no_data") {
        # show upload tooltip when no data
        span(title = "Please, upload your data first or load test data", btn)
      } else if (s == "snapping") {
        # show processing tooltip while snapping
        span(title = "Processing...", btn)
      } else if (s == "await_new_data") {
        # after snapping, disabled + upload tooltip
        span(title = "Please, upload your data first or load test data", btn)
      } else {
        # ready: enabled, no tooltip
        btn
      }
    })

    # When data arrives, become "ready" (enabled, no tooltip)
    observeEvent(input_point_table(), {
      req(input_point_table())
      state("ready")
    })


    # Function to create a custom update progress bar
    steps <- 6
    custom_updateProgressBar <- function(perc, sleep = 0.1) {
      updateProgressBar(
        session = session,
        id = ns("progress_snap"),
        value = perc
      )
      Sys.sleep(sleep)
    }

# ------------------------------------------------------------------------------
    # create reactive value for input point table name
    input_point_table_name <- reactiveVal()

    # Create database table for user input points
    observeEvent(input_point_table(), {
      # generate UUID for unique table name
      uuid <- UUIDgenerate(use.time = TRUE, output = "string")
      # set database table name
      table_name <- SQL(paste0("points_", uuid))
      # write to reactive value input_point_table_name
      input_point_table_name(table_name)

      # set user input points schema and table name
      table_id <- Id(schema = "shiny_user", table = table_name)

      tryCatch(
        expr = {
          # create table in schema "shiny_user" and upload data frame
          dbWriteTable_error <- dbWriteTable(pool, table_id, input_point_table())

          # run ANALYZE to update database table statistics
          sql <- sqlInterpolate(pool,
                                "ANALYZE ?point_table",
                                point_table = dbQuoteIdentifier(pool, table_id)
          )
          dbExecute(pool, sql)

          # render table with user input points
          #table_proxy <- tableServer("csv_table", coordinates_user(), column_names)
        },
        error = function(dbWriteTable_error) {
          message(dbWriteTable_error[[1]])
          #clear_user_input(empty_df, map_proxy())
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

#--------------------------------- snapping --------------------------------
    # Create empty reactive value objects for saving results after snapping
    snapped_data <- reactiveVal()
    lake_data <- reactiveVal()

    # If click snap button, snap points
    observeEvent(input$snap_button, {
      req(identical(state(), "ready"))

      # stop if a table with input points does not exist
      req(input_point_table())

      # Change state to snapping. Snapping button inactive
      state("snapping")
      shinyjs::show(ns("text1"))


      # set user input points table name
      points_table <- Id(schema = "shiny_user", table = input_point_table_name())
      # set regional units table name
      regional_units_table <- Id(schema = "hydro", table = "regional_units")
      # set sub_catchments table name
      sub_catchments_table <- Id(schema = "hydro", table = "sub_catchments")
      # set stream_segments table name
      stream_segments_table <- Id(schema = "hydro", table = "stream_segments")
      # set lakes table name
      lake_table <- Id(schema = "hydro", table = "hydrolakes_poly")

      # counter for progress bar
      custom_updateProgressBar(perc <- 0)


      # Add new columns to user input table
      # TODO: is target needed?
      sql <- sqlInterpolate(pool,
                            "ALTER TABLE ?point_table
           ADD COLUMN subc_id integer,
           ADD COLUMN basin_id integer,
           ADD COLUMN strahler_order smallint,
           ADD COLUMN reg_id smallint,
           ADD COLUMN hylak_id integer,
           ADD COLUMN upstream bigint[],
           ADD COLUMN geom_orig geometry(POINT, 4326),
           ADD COLUMN geom_snap geometry(POINT, 4326)
          ",
                            point_table = dbQuoteIdentifier(pool, points_table)
      )
      dbExecute(pool, sql)

      # counter for progress bar
      custom_updateProgressBar(perc <- 100 / steps)


      # update database table create point geometry from latitude and longitude
      sql <- sqlInterpolate(pool,
                            "UPDATE ?point_table SET geom_orig =
            ST_MakePoint(longitude, latitude)",
                            point_table = dbQuoteIdentifier(pool, points_table)
      )
      dbExecute(pool, sql)

      # counter for progress bar
      custom_updateProgressBar(perc <- 100 / steps * 2)

      # create spatial index on geom_orig
      sql <- sqlInterpolate(pool,
                            "CREATE INDEX ?idx ON ?point_table USING GIST (geom_orig)",
                            idx = dbQuoteIdentifier(pool, paste0(input_point_table_name(), "_geom_orig_idx")),
                            point_table = dbQuoteIdentifier(pool, points_table)
      )
      dbExecute(pool, sql)


      # counter for progress bar
      custom_updateProgressBar(perc <- 100 / steps * 3)


      # update database table with ID of the regional unit the point falls in
      sql <- sqlInterpolate(pool,
                            "UPDATE ?point_table poi SET reg_id =
            reg.reg_id
            FROM ?reg_table reg
            WHERE st_intersects(poi.geom_orig, reg.geom)",
                            reg_table = dbQuoteIdentifier(pool, regional_units_table),
                            point_table = dbQuoteIdentifier(pool, points_table)
      )
      dbExecute(pool, sql)


      # counter for progress bar
      custom_updateProgressBar(perc <- 100 / steps * 4)


      # update database table with ID of hydroLAKES the point falls in
      sql <- sqlInterpolate(pool,
                            "UPDATE ?point_table poi SET hylak_id =
          lak.hylak_id
          FROM ?lak_table lak
          WHERE st_intersects(poi.geom_orig, lak.geom)",
                            lak_table = dbQuoteIdentifier(pool, lake_table),
                            point_table = dbQuoteIdentifier(pool, points_table)
      )
      dbExecute(pool, sql)

      # counter for progress bar
      custom_updateProgressBar(perc <- 100 / steps * 4)

      # query sub_catchment table to get subc_id, basin_id and target
      sql <- sqlInterpolate(pool,
                            "UPDATE ?point_table poi SET
          subc_id = sub.subc_id,
          basin_id = sub.basin_id
          FROM ?subc_table sub
          WHERE st_intersects(poi.geom_orig, sub.geom)
          AND poi.reg_id = sub.reg_id",
                            subc_table = dbQuoteIdentifier(pool, sub_catchments_table),
                            point_table = dbQuoteIdentifier(pool, points_table)
      )
      dbExecute(pool, sql)


      # counter for progress bar
      custom_updateProgressBar(perc <- 100 / steps * 5)


      # snap points to line segment in sub-catchment
      sql <- sqlInterpolate(pool,
                            "UPDATE ?point_table poi SET
            strahler_order = seg.strahler,
            geom_snap = ST_LineInterpolatePoint(seg.geom,
              ST_LineLocatePoint(seg.geom, poi.geom_orig)
            )
            FROM ?segments_table seg
            WHERE seg.subc_id = poi.subc_id",
                            segments_table = dbQuoteIdentifier(pool, stream_segments_table),
                            point_table = dbQuoteIdentifier(pool, points_table)
      )
      dbExecute(pool, sql)


      # counter for progress bar
      custom_updateProgressBar(perc <- 100 / steps * 6)


      sql <- sqlInterpolate(pool,
                            "SELECT id, latitude, longitude,
          round(st_y(geom_snap)::numeric, 6) AS latitude_snap,
          round(st_x(geom_snap)::numeric, 6) AS longitude_snap,
          subc_id,
          hylak_id
          FROM ?point_table",
                            point_table = dbQuoteIdentifier(pool, points_table)
      )
      # set snapped_data reactive value to resulting data frame
      snapped_data(dbGetQuery(pool, sql))


      # join hydrolakes_poly and lake_intersections tables on Hydrolake ID
      sql <- sqlInterpolate(pool,
                            "SELECT poi.id, poi.hylak_id,
          hylak.lake_name AS hydrolake_name, hylak.lake_area AS hydrolake_area,
          lak.subc_id AS outlet_subc_id,
          round(lak.latitude::numeric, 6) AS outlet_latitude,
          round(lak.longitude::numeric, 6) AS outlet_longitude
          FROM ?point_table poi
          JOIN ?hydrolake_table hylak ON poi.hylak_id = hylak.hylak_id
          JOIN ?intersections_table lak ON poi.hylak_id = lak.hylak_id AND
          poi.reg_id = lak.reg_id WHERE outlet_id = 1 ",
                            point_table = dbQuoteIdentifier(pool, points_table),
                            hydrolake_table = dbQuoteIdentifier(
                              pool,
                              Id(schema = "hydro", table = "hydrolakes_poly")
                            ),
                            intersections_table = dbQuoteIdentifier(
                              pool,
                              Id(schema = "hydro", table = "lake_intersections")
                            )
      )
      # set lake data reactive value to dataframe resulting from lake query
      lake_data(dbGetQuery(pool, sql))


      # Option 2: snap point to nearest stream segment
      # using ST_LineLocatePoint and user input distance
      # TODO: replace 0.005 with user input distance
      # TODO: test query!
      # sql <- sqlInterpolate(pool,
      #   "UPDATE ?point_table poi SET geom_snap =
      #   ST_LineInterpolatePoint(seg.geom, point)
      #   FROM
      #     (SELECT ST_LineLocatePoint(seg.geom, poi.geom_orig) AS point
      #     FROM ?segments_table seg
      #     WHERE ST_DWithin(seg.geom, poi.geom_orig, 0.005)
      #     ORDER BY ST_Distance(
      #       ST_LineLocatePoint(seg.geom, poi.geom_orig),
      #       poi.geom_orig) ASC
      #     LIMIT 1)",
      #   segments_table = dbQuoteIdentifier(pool, stream_segments_table),
      #   point_table = dbQuoteIdentifier(pool, points_table)
      # )
      # dbExecute(pool, sql)

      # query result dataframe


      shinyjs::hide(ns("text1"))

      # reset progress bar
      custom_updateProgressBar(perc = 0, sleep = 0.8)

      # After snapping: disabled + upload tooltip,
      # will only enable again on NEW data
      state("await_new_data")
    })

    observe({
      cat(sprintf("[snapPoints %s] state = %s\n", session$ns(""), state()))
    })

    observe({
      print(snapped_data())
    })

    #return(snapped_data)

    list(user_table_name = input_point_table_name,
         snapped_data = snapped_data)


  })
}

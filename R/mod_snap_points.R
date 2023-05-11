# This module allows the user to snap points to a stream network
library("shinyWidgets")
# Module UI function
snapPointUI <- function(id, label = "Snap points") {
  ns <- NS(id)
  tagList(
    uiOutput(ns("snap_2_ntw_meth")),
  )
}


# Module server function
snapPointServer <- function(id, input_point_table) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input_point_table, {
        output$snap_2_ntw_meth <- renderUI({
          tagList(
            hr(),
            # textInput("distance",
            #   "Enter a snapping distance in meters or use default value",
            #   value = 500
            # ),
            tags$b("Snapping method: sub-catchment (default)"),
            p("Points will be snapped to the nearest location on the
              river segment of the sub-catchment the point falls in."),
            actionButton(ns("snap_button"),
              label = "Snap points"
            ),
            progressBar(
              id = ns("pb2"),
              value = 0,
              total = 6,
              title = "",
              display_pct = TRUE
            )
          )
        })
      })

      # progress bar
      custom_updateProgressBar <- function(step) {
        updateProgressBar(
          session = session,
          id = ns("pb2"),
          value = step,
          total = 6,
          title = paste("Step", step, "of 6 completed")
        )
        Sys.sleep(0.1)
      }


      # If click snap button, snap points, otherwise do nothing
      coordinates_snap <- eventReactive(input$snap_button, {
        # set user input points table name
        points_table <- Id(schema = "shiny_user", table = input_point_table())
        # set regional units table name
        regional_units_table <- Id(schema = "hydro", table = "regional_units")
        # set sub_catchments table name
        sub_catchments_table <- Id(schema = "hydro", table = "sub_catchments")
        # set stream_segments table name
        stream_segments_table <- Id(schema = "hydro", table = "stream_segments")

        # counter for progress bar
        custom_updateProgressBar(step <- 0)

        # Add new columns to user input table
        # TODO: is target needed?
        sql <- sqlInterpolate(pool,
          "ALTER TABLE ?point_table
           ADD COLUMN subc_id integer,
           ADD COLUMN basin_id integer,
           ADD COLUMN strahler_order smallint,
           ADD COLUMN reg_id smallint,
           ADD COLUMN upstream bigint[],
           ADD COLUMN geom_orig geometry(POINT, 4326),
           ADD COLUMN geom_snap geometry(POINT, 4326)
          ",
          point_table = dbQuoteIdentifier(pool, points_table)
        )
        dbExecute(pool, sql)

        custom_updateProgressBar(step <- step + 1)

        # update database table create point geometry from latitude and longitude
        sql <- sqlInterpolate(pool,
          "UPDATE ?point_table SET geom_orig =
            ST_MakePoint(longitude, latitude)",
          point_table = dbQuoteIdentifier(pool, points_table)
        )
        dbExecute(pool, sql)

        custom_updateProgressBar(step <- step + 1)

        # create spatial index on geom_orig
        sql <- sqlInterpolate(pool,
          "CREATE INDEX ?idx ON ?point_table USING GIST (geom_orig)",
          idx = dbQuoteIdentifier(pool, paste0(input_point_table(), "_geom_orig_idx")),
          point_table = dbQuoteIdentifier(pool, points_table)
        )
        dbExecute(pool, sql)

        custom_updateProgressBar(step <- step + 1)

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

        custom_updateProgressBar(step <- step + 1)

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

        custom_updateProgressBar(step <- step + 1)

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

        custom_updateProgressBar(step <- step + 1)

        sql <- sqlInterpolate(pool,
          "SELECT id, longitude, latitude,
          round(st_x(geom_snap)::numeric, 6) AS new_longitude,
          round(st_y(geom_snap)::numeric, 6) AS new_latitude,
          subc_id
          FROM ?point_table",
          point_table = dbQuoteIdentifier(pool, points_table)
        )
        # return result dataframe
        new_data <- dbGetQuery(pool, sql)
      })

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
    }
  )
}

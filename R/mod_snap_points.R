# This module allows the user to snap points to a stream network
library("shinyWidgets")
# Module UI function
snapPointUI <- function(id, label = "Snap points") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
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
              total = 5,
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
          total = 5,
          title = paste("Step", step, "of 5 completed")
        )
        Sys.sleep(0.1)
      }

      # If click snap button, snap points, otherwise do nothing
      snap_points <- eventReactive(input$snap_button, {
        # set regional units table name
        regional_units_table <- Id(schema = "hydro", table = "regional_units")
        # set sub_catchments table name
        sub_catchments_table <- Id(schema = "hydro", table = "sub_catchments")
        # set stream_segments table name
        stream_segments_table <- Id(schema = "hydro", table = "stream_edges")

        # counter for progress bar
        custom_updateProgressBar(step <- 0)

        # Add new columns to user input table
        sql <- sqlInterpolate(pool,
          "ALTER TABLE ?point_table
           ADD COLUMN subc_id integer,
           ADD COLUMN reg_id smallint,
           ADD COLUMN geom_orig geometry(POINT, 4326),
           ADD COLUMN geom_snap geometry(POINT, 4326)",
          point_table = dbQuoteIdentifier(pool, input_point_table)
        )
        dbExecute(pool, sql)

        custom_updateProgressBar(step <- step + 1)

        # update database table create point geometry from latitude and longitude
        sql <- sqlInterpolate(pool,
          "UPDATE ?point_table SET geom_orig =
            ST_MakePoint(longitude, latitude)",
          point_table = dbQuoteIdentifier(pool, input_point_table)
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
          point_table = dbQuoteIdentifier(pool, input_point_table)
        )
        dbExecute(pool, sql)

        custom_updateProgressBar(step <- step + 1)

        # query sub_catchment table to get subc_id
        sql <- sqlInterpolate(pool,
          "UPDATE ?point_table poi SET subc_id =
          sub.subc_id
          FROM ?subc_table sub
          WHERE st_intersects(poi.geom_orig, sub.geom)
          AND poi.reg_id = sub.reg_id",
          subc_table = dbQuoteIdentifier(pool, sub_catchments_table),
          point_table = dbQuoteIdentifier(pool, input_point_table)
        )
        dbExecute(pool, sql)

        custom_updateProgressBar(step <- step + 1)

        # snap points to line segment in sub-catchment
        sql <- sqlInterpolate(pool,
          "UPDATE ?point_table poi SET geom_snap =
            ST_LineInterpolatePoint(seg.geom,
              ST_LineLocatePoint(seg.geom, poi.geom_orig)
            ) FROM ?segments_table seg
            WHERE seg.subc_id = poi.subc_id",
          segments_table = dbQuoteIdentifier(pool, stream_segments_table),
          point_table = dbQuoteIdentifier(pool, input_point_table)
        )

        custom_updateProgressBar(step <- step + 1)

        # Option 2: snap point to nearest stream segment
        # using ST_LineLocatePoint and user input distance
        # sql <- sqlInterpolate(pool,
        #   "SELECT subc_id,
        #   round(ST_X(ST_LineInterpolatePoint(geom,
        #     ST_LineLocatePoint(geom, ST_Point(?x, ?y, 4326))
        #     ))::numeric, 6) AS lon,
        #   round(ST_Y(ST_LineInterpolatePoint(geom,
        #     ST_LineLocatePoint(geom, ST_Point(?x, ?y, 4326))
        #     ))::numeric, 6) AS lat,
        #   ST_LineInterpolatePoint(geom,
        #     ST_LineLocatePoint(geom, ST_Point(?x, ?y, 4326))
        #     ) AS geom
        #   FROM ?segments_table
        #   WHERE ST_DWithin(geom, ST_Point(?x, ?y, 4326), 0.005)
        #   ORDER BY ST_LineLocatePoint(geom, ST_Point(?x, ?y, 4326)) ASC
        #   LIMIT 1",
        #   segments_table = dbQuoteIdentifier(pool, stream_segments_table),
        #   x = lon,
        #   y = lat
        # )
        # snap_result <- dbGetQuery(pool, sql)
        # lon_vect[point] <- ifelse(is.null(snap_result), 0, snap_result$lon)
        # lat_vect[point] <- ifelse(is.null(snap_result), 0, snap_result$lat)
        # }

        # result dataframe
        sql <- sqlInterpolate(pool,
          "SELECT id, longitude, latitude,
          st_x(geom_snap) AS new_longitude,
          st_y(geom_snap) AS new_latitude,
          subc_id
          FROM ?point_table",
          point_table = dbQuoteIdentifier(pool, input_point_table)
        )
        new_data <- dbGetQuery(pool, sql)
      })
    }
  )
}

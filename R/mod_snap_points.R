# This module allows the user to snap points to a stream network
library("shinyWidgets")
library("shinyjs")
# Module UI function
snapPointUI <- function(id, label = "Snap points") {
  ns <- NS(id)
  tagList(
    hr(),
    tags$b("Snapping method: sub-catchment (default)"),
    p("Points will be snapped to the nearest location on the
              river segment of the sub-catchment the point falls in."),
    # Inactive action button. It is activated after point data is uploaded.
    # Point snapping starts after clicking
    shinyjs::disabled(actionButton(ns("snap_button"),
      label = "Snap points",
      icon = icon("arrow-right"),
      class = "btn-primary"
    )),
    # Tooltip to indicate point data must be uploaded before snapping.
    # It is hidden after point data is uploaded
    bsTooltip(ns("snap_button"),
      "Please, upload your data first or load test data",
      "right",
      options = list(container = "body")
    ),
    br(),
    # Progress bar, indicates the progress of snapping process
    progressBar(
      id = ns("progress_snap"),
      value = 0,
      title = " ",
      display_pct = TRUE
    ),
    # Text indicating snapping is taking place
    shinyjs::hidden(p(id = ns("text1"), "Processing..."))
  )
}


# Module server function
snapPointServer <- function(id, input_point_table) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns

      # progress bar
      steps <- 6

      custom_updateProgressBar <- function(perc, sleep = 0.1) {
        updateProgressBar(
          session = session,
          id = ns("progress_snap"),
          value = perc
        )
        Sys.sleep(sleep)
      }

      # activate snap button and hide tooltip after a table with the user's
      # points is loaded
      observeEvent(input_point_table(), {
        shinyjs::enable("snap_button")
        removeTooltip(session, ns("snap_button"))
      })

      # reactive value object to save results after snapping
      snapped_data <- reactiveVal()

      # If click snap button, snap points, otherwise do nothing
      observeEvent(input$snap_button, {
        # stop if a table with input points does not exist
        req(input_point_table())

        # disable snap button after clicking. Avoids that the user follows
        # clicking during snapping
        shinyjs::disable("snap_button")
        shinyjs::show("text1")

        # set user input points table name
        points_table <- Id(schema = "shiny_user", table = input_point_table())
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

        custom_updateProgressBar(perc <- 100 / steps)

        # update database table create point geometry from latitude and longitude
        sql <- sqlInterpolate(pool,
          "UPDATE ?point_table SET geom_orig =
            ST_MakePoint(longitude, latitude)",
          point_table = dbQuoteIdentifier(pool, points_table)
        )
        dbExecute(pool, sql)

        custom_updateProgressBar(perc <- 100 / steps * 2)

        # create spatial index on geom_orig
        sql <- sqlInterpolate(pool,
          "CREATE INDEX ?idx ON ?point_table USING GIST (geom_orig)",
          idx = dbQuoteIdentifier(pool, paste0(input_point_table(), "_geom_orig_idx")),
          point_table = dbQuoteIdentifier(pool, points_table)
        )
        dbExecute(pool, sql)

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

        # hide processing text
        shinyjs::hide("text1")

        # reset progress bar
        custom_updateProgressBar(perc = 0, sleep = 0.8)

        # show tooltip again
        addTooltip(session,
          ns("snap_button"),
          "Please, upload point data first",
          placement = "right"
        )

        # enable snap button
        # shinyjs::enable("snap_button")
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

      # return results of one of the snapping method after snapping
      return(snapped_data)
    }
  )
}

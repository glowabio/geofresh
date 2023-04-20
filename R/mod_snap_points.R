# This module allows the user to snap points to a stream network

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
snapPointServer <- function(id, input_points) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input_points, {
        output$snap_2_ntw_meth <- renderUI({
          tagList(
            hr(),
            textInput("distance",
              "Enter a snapping distance in meters or use default value",
              value = 500
            ),
            actionButton(ns("snap_button"),
              label = "Snap points"
            )
          )
        })
      })

      # If click snap button, snap points, otherwise do nothing
      coordinates_snap <- eventReactive(input$snap_button, {
        # set regional units table name
        regional_units_table <- Id(schema = "hydro", table = "regional_units")
        # set sub_catchments table name
        sub_catchments_table <- Id(schema = "hydro", table = "sub_catchments")
        # get user point coordinates
        input_point_df <- input_points
        # vectors to save the results of the loop
        reg_units_vect <- numeric(length = nrow(input_point_df))
        subc_id_vect <- numeric(length = nrow(input_point_df))
        lat_vect <- numeric(length = nrow(input_point_df))
        lon_vect <- numeric(length = nrow(input_point_df))

        for (point in 1:nrow(input_point_df)) {
          # get longitude and latitude of input points
          lon <- input_point_df[point, 2]
          lat <- input_point_df[point, 3]

          # query database to get ID of the regional unit the point is in
          sql <- sqlInterpolate(pool,
            "SELECT reg_id
            FROM ?reg_table
            WHERE st_intersects(st_makepoint(?x, ?y)::geometry(POINT, 4326), geom)",
            reg_table = dbQuoteIdentifier(pool, regional_units_table),
            x = lon,
            y = lat
          )
          reg_id_result <- dbGetQuery(pool, sql)
          reg_units_vect[point] <- reg_id_result[[1]][1]

          # query sub_catchment table to get subc_id and subcatchment centroid coordinates
          sql <- sqlInterpolate(pool,
            "SELECT subc_id, ST_X(ST_Centroid(geom)) AS lon, ST_Y(ST_Centroid(geom)) AS lat
            FROM ?subc_table
            WHERE reg_id = ?reg_id AND
             st_intersects(st_makepoint(?x, ?y)::geometry(POINT, 4326), geom)",
            subc_table = dbQuoteIdentifier(pool, sub_catchments_table),
            reg_id = reg_id_result[[1]][1],
            x = lon,
            y = lat
          )
          subc_id_result <- dbGetQuery(pool, sql)

          subc_id_vect[point] <- ifelse(is.null(subc_id_result), 0, subc_id_result$subc_id)
          lon_vect[point] <- ifelse(is.null(subc_id_result), 0, subc_id_result$lon)
          lat_vect[point] <- ifelse(is.null(subc_id_result), 0, subc_id_result$lat)
        }

        # result dataframe

        # centroid coordinates of sub_catchment to use as if they were
        # the result of snapping. (substitute these lines with the true
        # snapping script)
        snapped_coord <- data.frame(
          "new_longitude" = lon_vect,
          "new_latitude" = lat_vect,
          "regional_unit_id" = reg_units_vect,
          "subc_id" = subc_id_vect
        )

        # Append new columns with new coordinates that resulted from snapping
        new_data <- data.frame(input_points, snapped_coord)
      })
    }
  )
}

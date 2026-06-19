# ============================================================
# Catchment delineation
# ============================================================

# UI
catchmentUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("open"), "Open catchment delineation tool")
}

# Server
# * points_db: From this, we read the most recent input points,
#   as a data.frame. Check out "read_points_db()" in
#   "db_points_helpers.R" for the columns contained in it.
# * upstream_catchments: This is where we will store the upstream
#   catchments, for the map viewer module to display them.
catchmentServer <- function(id, points_db, upstream_catchments) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Observe:
    # When opening the catchment tool, this window appears:
    observeEvent(input$open, {
      showModal(
        modalDialog(
          title = "Catchment delineation",
          easyClose = TRUE,
          #footer = modalButton("Close"),
          footer = tagList(
            modalButton("Close"),
            actionButton(ns("compute_upstream"), "Compute upstream catchment")
          ),
          div(
            class = "alert alert-info",
            HTML(
              "Here you can compute the points' upstream catchments, either ",
	      "as polygons (upstream subcatchments) or as lines (upstream ",
	      "stream segments).<br/>"
            )
          )
        )
      )
    }) # end of: observeEvent(input$open, ...

    # define asynchronous extended task here, to be invoked below:
    # update: we are not using extended task anymore
    #outlet_task <- ExtendedTask$new(function(lon, lat) {
    #  showNotification("INVOKED EXTENDED TASK (outlet_task)")
    #  showNotification(paste("INVOKED upstream calculation for point: lon=", lon, ", lat=", lat, "..."))
    #  future_promise({
    #    # "run_upstream_computation()" is defined in "pygeoapi_helpers.R"!
    #    upstr_res <- run_upstream_computation(lon, lat)
    #    upstr_res
    #  })
    #})


    # Observe:
    # when the user clicked the action button to compute the upstream catchments
    observeEvent(input$compute_upstream, {
      # Code to run when button is clicked
      showNotification("Now calculating upstream catchments (asynchronously)")
      req(points_db())
      df <- points_db()

      # NOTE: If the points are snapped, we should use their subc_id, not just their coordinates! (faster!)
      # Check if data frame df contains both columns "latitude_snap" and "longitude_snap"
      has_subcid <- "subc_id" %in% names(df)
      has_cols <- all(c("latitude_snap", "longitude_snap") %in% names(df))
      # Check if at least one row has finite values in both columns.
      has_at_least_one_finite_row <- any(is.finite(df$latitude_snap) & is.finite(df$longitude_snap))
      # So:
      has_snapped <- has_cols && has_at_least_one_finite_row

      # how many points? - limit to hard-coded limit!
      num_points = nrow(df)
      max_points = 5
      if (num_points > max_points) {
        showNotification(paste0("Requesting upstream catchment. Input contains ", num_points, " points. Only computing for the first ", max_points, " points."))
      } else {
        showNotification(paste0("Requesting upstream catchment for ", num_points, " points: This may take a while, please be patient."))
      }
      n <- min(c(num_points, max_points))

      #showNotification("Paths will be shown only after you zoom or pan the map.")
      for (i in seq_len(n)) {
        #showNotification(paste("Now preparing promise, treating row:", i, "..."))
        promise <- future_promise({
          has_integer_subcid <- FALSE
          if (has_subcid) {
            subc_id <- df$subc_id[i]
            has_integer_subcid <- !is.na(suppressWarnings(as.numeric(subc_id))) && as.numeric(subc_id) %% 1 == 0
          }
          if (has_integer_subcid) {
            fetch_from_pygeoapi(subc_id=df$subc_id[i])
          } else if (has_snapped) {
            # TODO: Handle gracefully if a point could not be snapped and containes NULL (or so)!
            fetch_from_pygeoapi(lon=df$longitude_snap[i], lat=df$latitude_snap[i])
          } else {
            fetch_from_pygeoapi(lon=df$longitude[i], lat=df$latitude[i])
          }
        }, seed = TRUE)
        #showNotification(paste("Prepared promise no:", i, ", coordinates: ", df$longitude[i], df$latitude[i]))
        # Run promise and define callback for afterwards:
        promise %...>% (function(sf_result) {
          #showNotification(paste0("Callback ran for ", sf_result))
          bbox <- sf::st_bbox(sf_result)
          showNotification("Please zoom or pan to view upstream catchment...")
          showNotification(paste("Result has bbox:", paste(bbox, collapse="+")))

          # If we had access to the map from this module, we could
          # directly display on map:
          #leafletProxy("map") %>% # TODO which options for polygons
          #  addPolygons(
          #    data = sf_result,
          #    color = "blue",
          #    weight = 5
          #  )

          # Instead, we store them in a reactiveVal (upstream_catchments)

          # Store sf objects as list:
          # TODO is this async-safe? If two asynchronous callbacks access the list,
          # at the same time, some catchments may get lost?
          #current <- upstream_catchments()
          #current[[length(current) + 1]] <- sf_result
          #upstream_catchments(current)

          # Possibly cleaner, if we had a site_id here:
          #site_id <- sf_result$id[1]
          #current <- upstream_catchments()
          #current[[as.character(site_id)]] <- sf_result
          #upstream_catchments(current)

          # Now: Just store ONE sf object into upstream_catchments reactive:
          upstream_catchments(sf_result)


       }) %...!% (function(err) {
          showNotification(paste0("Error (during asynchronous task):", err$message))
        })
      }

      # Here, as we iterate over a dataframe, we don't use the ExtendedTask
      #upstr_task$invoke(click$lng, click$lat)
      #showNotification(paste("Calculating upstream catchments was requested for point lon=", click$lng, ", lat=", click$lat, "..." ))

      # Optional: close the modal
      removeModal()
    }) # end of: observeEvent(input$compute_upstream...



  }) # end of: moduleServer
} # end of: catchmentServer

# ============================================================
# Routing to outlets
# ============================================================

# UI
routingUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("open"), "Open routing tool")
}

# Server
# * points_db: From this, we read the most recent input points,
#   as a data.frame. Check out "read_points_db()" in
#   "db_points_helpers.R" for the columns contained in it.
# * paths_to_outlet: This is where we will store the paths
#   to outlet, for the map viewer module to display them.
routingServer <- function(id, points_db, paths_to_outlet) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Observe:
    # When opening the routing tool, this window appears:
    observeEvent(input$open, {
      showModal(
        modalDialog(
          title = "Routing",
          easyClose = TRUE,
          #footer = modalButton("Close"),
          footer = tagList(
            modalButton("Close"),
            actionButton(ns("compute_route"), "Compute Route")
          ),
          div(
            class = "alert alert-info",
            HTML(
              "Here you can compute the path of each point to the sea.<br/>
              "
            )
          )
        )
      )
    }) # end of: observeEvent(input$open, ...


    # Observe:
    # when the user clicked the action button to compute the paths to outlet
    observeEvent(input$compute_route, {
      # Code to run when button is clicked
      showNotification("Now calculating route to outlets (asynchronously)")
      req(points_db())
      df <- points_db()

      # NOTE: If the points are snapped, we should use their subc_id, not just their coordinates! (faster!)
      # Check if data frame df contains both columns "latitude_snap" and "longitude_snap"
      has_cols <- all(c("latitude_snap", "longitude_snap") %in% names(df))
      # Check if at least one row has finite values in both columns.
      has_at_least_one_finite_row <- any(is.finite(df$latitude_snap) & is.finite(df$longitude_snap))
      # So:
      has_snapped <- has_cols && has_at_least_one_finite_row

      showNotification(paste0("Requesting path to sea for ", nrow(df), " points: This may take a while, please be patient."))
      #showNotification("Paths will be shown only after you zoom or pan the map.")
      for (i in seq_len(nrow(df))) {
        #showNotification(paste("Now preparing promise, treating row:", i, "..."))
        promise <- future_promise({
          if (has_snapped) {
            # TODO: Handle gracefully if a point could not be snapped and containes NULL (or so)!
            fetch_from_pygeoapi_outlet(lon=df$longitude_snap[i], lat=df$latitude_snap[i])
          } else {
            fetch_from_pygeoapi_outlet(lon=df$longitude[i], lat=df$latitude[i])
          }
          #fetch_from_pygeoapi_outlet(subc_id=df$subc_id[i])
        }, seed = TRUE)
        #showNotification(paste("Prepared promise no:", i, ", coordinates: ", df$longitude[i], df$latitude[i]))
        # Run promise and define callback for afterwards:
        promise %...>% (function(sf_result) {
          #showNotification(paste0("Callback ran for ", sf_result))
          bbox <- sf::st_bbox(sf_result)
          showNotification("Please zoom or pan to view path to sea...")
          showNotification(paste("Result has bbox:", paste(bbox, collapse="+")))

          # If we had access to the map from this module, we could
          # directly display on map:
          #leafletProxy("map") %>%
          #  addPolylines(
          #    data = sf_result,
          #    color = "blue",
          #    weight = 5
          #  )

          # Instead, we store them in a reactiveVal (paths_to_outlet)

          # Store sf objects as list:
          # TODO is this async-safe? If two asynchronous callbacks access the list,
          # at the same time, some paths may get lost?
          #current <- paths_to_outlet()
          #current[[length(current) + 1]] <- sf_result
          #paths_to_outlet(current)

          # Possibly cleaner, if we had a site_id here:
          #site_id <- sf_result$id[1]
          #current <- paths_to_outlet()
          #current[[as.character(site_id)]] <- sf_result
          #paths_to_outlet(current)

          # Now: Just store ONE sf object into paths_to_outlet reactive:$
          paths_to_outlet(sf_result)


       }) %...!% (function(err) {
          showNotification(paste0("Error (during asynchronous task):", err$message))
        })
      }

      # Here, as we iterate over a dataframe, we don't use the ExtendedTask
      #upstr_task$invoke(click$lng, click$lat)
      #showNotification(paste("Calculating route to outlets was requested for point lon=", click$lng, ", lat=", click$lat, "..." ))

      # Optional: close the modal
      removeModal()
    }) # end of: observeEvent(input$compute_route...



  }) # end of: moduleServer
} # end of: routingServer

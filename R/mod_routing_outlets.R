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
# * last_path_to_outlet: This is where we will store the paths
#   to outlet, for the map viewer module to display them. It al-
#   ways just stores the last one that was returned from pygeoapi,
#   and we display them one by one.
routingServer <- function(id, points_db, last_path_to_outlet) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State variable for this module. Used to check wheter we have
    # ready to compute paths to outlet, i.e. whether we have point
    # data uploaded.
    # Possible states: "no_data", "ready", "finished_downstream"
    state <- reactiveVal("no_data")

    # Observe:
    # When opening the routing tool, this window appears:
    observeEvent(input$open, {
      showModal(
        modalDialog(
          title = "Routing to sea (outlet)",
          easyClose = TRUE,
          #footer = modalButton("Close"),
          footer = tagList(
            modalButton("Close"),
            # placeholders for buttons (content defined further below):
            uiOutput(ns("compute_route_btn_ui")),
            uiOutput(ns("download_btn_ui"))
          ),
          div(
            class = "alert alert-info",
            HTML(
              "Here you can compute the path of each point to the sea.<br/><br/>",
              "Once the paths are displayed, you can also download them ",
              "(a download button will appear in this window)."
            )
          )
        )
      )
    }) # end of: observeEvent(input$open, ...

    # Create the action button and format it depending on current state
    # of the point data...
    output$compute_route_btn_ui <- renderUI({
      current_state <- state()

      # Create action button, disabled whenever state is not ready
      btn <- actionButton(
        ns("compute_downstream_button"),
        label = "Compute paths to sea",
        icon  = icon("arrow-right"),
        class = "btn btn-primary",
        disabled = !identical(current_state, "ready")
      )

      # Set tooltip texts to the button, depending on state:
      if (current_state == "no_data") {
        span(title = "Please upload or create points first.", btn)
      } else if (current_state == "waiting_for_downstream") {
        span(title = "Processing...", btn)
      } else if (current_state == "finished_downstream") {
        span(title = "Upload/edit points before computing paths to sea again.", btn)
      } else {
        btn
      }
    })


    # Whenever the point table changes, update the state.
    observe({
      df <- points_db()
      # Whenever the point table changed in the database, we are ready to
      # recompute. We don't need snapped values necessarily.
      # TODO: Do use snapped coordinates when available.
      # TODO: Do use snapped subc_ids when available.
      # Let's also check if we have any rows...
      num_points = nrow(df)
      if (num_points == 0) {
        state("no_data")
      } else {
        state("ready")
      }
    })


    # When the user clicked the action button to compute the paths to outlet
    observeEvent(input$compute_downstream_button, {
      # Code to run when button is clicked

      # We need points:
      req(state() == "ready")
      req(points_db())
      df <- points_db()

      # We need non-zero points:
      # TODO: Better enforce this by greying out the button!
      num_points = nrow(df)
      if (num_points == 0) {
        showNotification("Cannot calculate route to outlets: No data. Please upload data first!", type="error")
      }
      req(num_points>0)

      # All conditions are met, continue:
      #showNotification("Now calculating route to outlets (asynchronously)", type="message")

      # NOTE: If the points are snapped, we should use their subc_id, not just their coordinates! (faster!)
      # Check if data frame df contains both columns "latitude_snap" and "longitude_snap"
      has_subcid <- "subc_id" %in% names(df)
      has_cols <- all(c("latitude_snap", "longitude_snap") %in% names(df))
      # Check if at least one row has finite values in both columns.
      has_at_least_one_finite_row <- any(is.finite(df$latitude_snap) & is.finite(df$longitude_snap))
      # So:
      has_snapped <- has_cols && has_at_least_one_finite_row

      # how many points? - limit to hard-coded limit!
      max_points = 10
      if (num_points > max_points) {
        showNotification(paste0("Requesting path to sea. Input contains ", num_points, " points. Only computing for the first ", max_points, " points."), type="message")
      } else {
        showNotification(paste0("Requesting path to sea for ", num_points, " points: This may take a while, please be patient."), type="message")
      }
      n <- min(c(num_points, max_points))

      # Starting asynchronous tasks in the for loop below

      # Beforehand, set the state to "waiting_for_upstream":
      state("waiting_for_downstream")

      # Make sure we don't flood the user with notifications:
      notifiedUserOnce <- reactiveVal(FALSE)

      for (i in seq_len(n)) {
        # Creating a local scope, to make sure that we always have the correct index,
        # even inside the promise!
        local({
          idx <- i

          #showNotification(paste("DEBUG: Now preparing promise, treating row:", idx, "..."))
          promise <- future_promise({
            has_integer_subcid <- FALSE
            if (has_subcid) {
              subc_id <- df$subc_id[idx]
              has_integer_subcid <- !is.na(suppressWarnings(as.numeric(subc_id))) && as.numeric(subc_id) %% 1 == 0
            }
            if (has_integer_subcid) {
              fetch_from_pygeoapi_outlet(subc_id=df$subc_id[idx])
            } else if (has_snapped) {
              # TODO: Handle gracefully if a point could not be snapped and containes NULL (or so)!
              fetch_from_pygeoapi_outlet(lon=df$longitude_snap[idx], lat=df$latitude_snap[idx])
            } else {
              fetch_from_pygeoapi_outlet(lon=df$longitude[idx], lat=df$latitude[idx])
            }
          }, seed = TRUE)
          #showNotification(paste("DEBUG: Prepared promise no:", idx, ", coordinates: ", df$longitude[idx], df$latitude[idx]))
          # Run promise and define callback for afterwards:
          promise %...>% (function(sf_result) {
            #showNotification(paste0("DEBUG: Callback ran for ", sf_result))
            bbox <- sf::st_bbox(sf_result)
            #showNotification("Please zoom or pan to view path to sea...", type="message")
            if (!notifiedUserOnce()) {
              showNotification("First path to outlet incoming...", type="message")
              notifiedUserOnce(TRUE)
            }
            #showNotification(paste("Result has bbox:", paste(bbox, collapse="+")), type="message")

            # If we had access to the map from this module, we could
            # directly display on map:
            #leafletProxy("map") %>%
            #  addPolylines(
            #    data = sf_result,
            #    color = "blue",
            #    weight = 5
            #  )

            # Instead, we store them in a reactiveVal (last_path_to_outlet)

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

            # Now: Just store ONE sf object into last_path_to_outlet reactive:
            last_path_to_outlet(sf_result)

            # Set the state to "finished_downstream", so we won't recompute the paths...
            # TODO: This is not entirely correct, as this callback runs for each point
            # separately. We would need to define a callback for when all promises finished...
            state("finished_downstream")


          }) %...!% (function(err) {
            showNotification(paste0("Error (during asynchronous task):", err$message), type="error")
          }) # end of callback
        }) # end of local({...})
      } # end of for-loop

      # Here, as we iterate over a dataframe, we don't use the ExtendedTask
      #upstr_task$invoke(click$lng, click$lat)
      #showNotification(paste("Calculating route to outlets was requested for point lon=", click$lng, ", lat=", click$lat, "..." ))

      # Optional: close the modal
      removeModal()
    }) # end of: observeEvent(input$compute_route...



    ############################
    ### Downloading the data ###
    ############################

    # Define the download button
    #
    # The button is only shown once data is there, because the browser will
    # generate a URL and open a tab when the button is clicked, no matter what,
    # and if no data is there yet, that will cause an error.
    # I tried disabling the button, but as the download button is no a regular
    # action button, that did not work.
    #
    # TODO (not urgent): We could try assigning the CSS class that shows the button
    # as disabled, or we could show a disabled action button in this place which
    # does nothing, and which gets replaced by the download button once data is
    # there. Just ideas to make it look more fancy.
    output$download_btn_ui <- renderUI({

      # Only show this once data is there:
      req(state() == "finished_downstream")

      # Generate a regular download button:
      downloadButton(
        ns("download_geojson"),
        "Download GeoJSON",
        class = "btn btn-primary"
      )
    })

    # Define behaviour when user clicked the download button
    # TODO: Currently we always just store the very last route in the reactive variable...
    output$download_geojson <- downloadHandler(
      filename = function() {
        return("geofresh_routing.geojson")
      },
      content = function(file) {

        # Require data to be there:
        # Note: When the button is clicked, the browser will generate a URL and
        # open a tab before this is run, so no matter what we req, there will be
        # some error if there is no data. That's why the button is only rendered
        # and displayed once the data is there.
        req(state() == "finished_downstream")
        last_path <- last_path_to_outlet()
        req(
          !is.null(last_path),
          nrow(last_path) > 0
        )

        # Write to GeoJSON:
        sf::st_write(
          last_path,
          file,
          driver = "GeoJSON",
          delete_dsn = TRUE,
          quiet = TRUE
        )
      }
    )


  }) # end of: moduleServer
} # end of: routingServer

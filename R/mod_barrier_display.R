# ============================================================
# Displaying barriers
# ============================================================

# UI
barrierUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("open"), "Open barrier tool")
}

# Server
# * points_db: From this, we read the most recent input points,
#   as a data.frame. Check out "read_points_db()" in
#   "db_points_helpers.R" for the columns contained in it.
# * barrier_points: This is where we will store the points
#   of the barriers, for the map viewer module to display them.
barrierServer <- function(id, points_db, barrier_points) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State variable for this module. Used to check wheter we have
    # ready to compute paths to outlet, i.e. whether we have point
    # data uploaded.
    # Possible states: "no_data", "not_snapped_yet", "ready"
    state <- reactiveVal("no_data")

    # When opening the routing tool, this window appears:
    observeEvent(input$open, {
      showModal(
        modalDialog(
          title = "Barriers",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close"),
            # placeholder for action button (content defined further below):
            uiOutput(ns("display_barriers_btn_ui"))
          ),
          div(
            class = "alert alert-info",
            HTML(
              "Here you can visualize the barriers.<br/>
              "
            )
          )
        )
      )
    }) # end of: observeEvent(input$open, ...


    # Create the action button and format it depending on current state
    # of the point data...
    output$display_barriers_btn_ui <- renderUI({
      current_state <- state()

      # Create action button, disabled whenever state is not ready
      btn <- actionButton(
        ns("display_barriers_button"),
        label = "Display barriers",
        icon  = icon("arrow-right"),
        class = "btn btn-primary",
        disabled = !identical(current_state, "ready")
      )

      # Set tooltip texts to the button, depending on state:
      if (current_state == "no_data") {
        span(title = "Please upload or create points first.", btn)
      } else if (current_state == "not_snapped_yet") {
        span(title = "Please snap the data first...", btn)
      } else {
        btn
      }
    })


    # Whenever the point table changes, update the state.
    observe({
      df <- points_db()
      # Whenever the point table changed in the database AND was (re-)snapped,
      # we are ready to refetch barriers.
      # TODO: Does this also check if the points were re-snapped? Because depending
      # on which barriers we load, re-snapping may lead to different barriers being
      # required.
      # TODO: We should also check if there are any new barriers to (re-)fetch.
      # For example, if we fetch all barriers in the whole basin, changing points
      # may not lead to a new basin.
      # Let's also check if we have any rows...
      num_points = nrow(df)
      has_snapped <- all(c("latitude_snap", "longitude_snap") %in% names(df)) &&
        any(is.finite(df$latitude_snap) & is.finite(df$longitude_snap))

      # Set the state
      if (num_points == 0) {
        state("no_data")
      } else if (has_snapped) {
        state("ready")
      } else {
        state("not_snapped_yet")
      }
    })

    # When the user clicked the action button to fetch the barriers
    observeEvent(input$display_barriers_button, {
      # Code to run when button is clicked

      # We need points:
      req(state() == "ready")
      req(points_db())
      df <- points_db()

      # Get basin_id from the points...
      basin_ids <- unique(df$basin_id)
      #showNotification(paste0('DEBUG: Basin ids: ', paste(basin_ids, collapse=" + ")))

      # Retrieve dataframe of barriers from database
      barriers_df <- with_pool_connection(pool, function(conn) {
        barrier_table_name <- "nearest_barriers"
        table_id <- DBI::Id(schema = "shiny_user", table = barrier_table_name)
        tbl_q <- DBI::dbQuoteIdentifier(conn, table_id)
        df <- DBI::dbGetQuery(conn, paste0(
          "SELECT
             geom_barrier
           FROM ", tbl_q
        ))
      })

      # Convert from dataframe to spatial:
      barriers_df$geom_barrier <- sf::st_as_sfc(barriers_df$geom_barrier, EWKB = TRUE)
      barriers_sf <- sf::st_sf(barriers_df)

      # Store in reactive value:
      barrier_points(barriers_sf)
    })


  }) # end of: moduleServer
} # end of: routingServer

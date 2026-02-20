# Module to snap points.

# User interface
lakeAnalysisUI <- function(id) {
  ns <- NS(id)
  actionLink(ns("show_modal"), "Lake analysis")
}


# Server logic
lakeAnalysisServer <- function(id, pool, points_table_name, db_version = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- fetch lake data from DB ---
    fetch_lake_data <- function() {
      tn <- points_table_name()
      if (is.null(tn) || !nzchar(tn)) return(NULL)

      points_table <- DBI::Id(schema = "shiny_user", table = tn)

      with_pool_connection(pool, function(conn) {
        if (exists("read_lakes_for_points_db", mode = "function")) {
          read_lakes_for_points_db(conn, points_table)
        } else {
          NULL
        }
      })
    }

    # Keep a cached copy to display
    lake_df <- reactiveVal(NULL)

    # Load on modal open
    observeEvent(input$show_modal, {
      lake_df(fetch_lake_data())

      showModal(
        modalDialog(
          title = "Lake analysis",
          size  = "l",
          tagList(
            div(
              class = "alert alert-info",
              HTML(paste0(
                "The lake analysis provides information about a lake outlet, name and area for all points falling into a lake of the ",
                "<a href='https://www.hydrosheds.org/hydrolakes' target='_blank' rel='noopener noreferrer'>HydroLAKES</a>",
                " dataset. The lake outlet is derived from the intersection point of the ",
                "<a href='https://hydrography.org/hydrography90m/hydrography90m_layers' target='_blank' rel='noopener noreferrer'>Hydrography90m</a>",
                " stream network and HydroLAKES polygon with the highest water discharge value. ",
                "The lake name and area are obtained from HydroLAKES."
              ))
            ),
            DT::DTOutput(ns("lake_table"))
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })

    # Optional: refresh cache when db_version changes (upload/edit/snap),
    # but only if the modal is currently open.
    if (!is.null(db_version)) {
      observeEvent(db_version(), {
        # input$`shiny-modal` isn't reliable; easiest is: refresh unconditionally,
        # it's cheap and keeps table current next time modal opens too.
        lake_df(fetch_lake_data())
      }, ignoreInit = TRUE)
    }

    # Render DT table from cached df
    output$lake_table <- DT::renderDT({
      df <- lake_df()

      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0,
                    "No lake data available yet. Snap points that fall into lakes first.")
      )

      # keep only expected columns if present (prevents errors if schema differs)
      wanted <- c("id", "hylak_id", "hydrolake_name", "hydrolake_area",
                  "outlet_subc_id", "outlet_latitude", "outlet_longitude")
      have <- intersect(wanted, names(df))
      df <- df[, have, drop = FALSE]

      DT::datatable(
        df,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
  })
}

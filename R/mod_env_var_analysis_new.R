# ========== Generic Module to choose variables and run analysis ==========
library(ggplot2)
library(plotly)

varsUI <- function(id, trigger_label = "Choose variables") {
  ns <- NS(id)
  actionLink(ns("show_modal"), trigger_label)
}

varsServer <- function(id,
                       title   = NULL,
                       choices,
                       desc    = NULL,
                       user_table_name,
                       snap_status,
                       var_class,
                       var_groups) {

  stopifnot(is.character(choices), !is.null(names(choices)))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- state ----
    picked <- reactiveVal(character(0))
    rv <- reactiveValues(
      local = NULL, upstream = NULL,
      have_local_tab = FALSE, have_upstream_tab = FALSE
    )

    # message shown under the progress bar
    upstream_msg <- reactiveVal("")

    output$upstream_msg <- renderText({
      upstream_msg()
    })

    # --- open modal  ---
    observeEvent(input$show_modal, {

      showModal(modalDialog(
        title = title, easyClose = TRUE, size = "l",
        footer = div(style = "text-align:right;",
                     actionButton(ns("close"), "Close", class = "btn btn-outline-secondary")),
        tabsetPanel(id = ns("tabs"), selected = "select_tab",
                    tabPanel("Select Variables", value = "select_tab",
                             div(style="max-height: 500px; overflow-y:auto; padding-right:5px;",
                                 # explainer (sticky)
                                 div(style="position:sticky;top:0;z-index:100;padding:10px 0;border-bottom:1px solid #ccc;",
                                     class = "alert alert-info",
                                     p("Select environmental variables and then run a query for either Local or Upstream scope.",
                                       br(), strong("Local:"), " Summarizes selected variables for the local sub-catchment (min, max, mean, sd).",
                                       br(), strong("Upstream:"), " Summarizes selected variables for the entire upstream catchment of each point (mean of sub-catchment means).",
                                       style="font-size:14px;margin:0;")
                                 ),
                                 # search
                                 textInput(ns("search"), NULL, placeholder = "Type to filter…", width = "100%"),
                                 # select/deselect
                                 fluidRow(
                                   column(6, actionButton(ns("select_all"), "Select all", class="btn btn-primary w-100")),
                                   column(6, actionButton(ns("deselect_all"), "Deselect all", class="btn btn-outline-secondary w-100"))
                                 ),
                                 br(),
                                 # two columns of checkboxes (built via renderUI)
                                 fluidRow(
                                   column(6, uiOutput(ns("left"))),
                                   column(6, uiOutput(ns("right")))
                                 ),
                                 hr(),
                                 # scope + query
                                 fluidRow(
                                   column(6,
                                          radioButtons(
                                            inputId = ns("scope"), label = NULL,
                                            choiceNames = list(
                                              tags$span("Local",    title="Query for local sub-catchment (min, max, mean, sd)",
                                                        style="white-space:normal;max-width:280px;display:inline-block;"),
                                              tags$span("Upstream", title="Query for upstream catchment (mean of sub-catchment means)",
                                                        style="white-space:normal;max-width:280px;display:inline-block;")
                                            ),
                                            choiceValues = c("local","upstream"),
                                            selected = "local", inline = TRUE
                                          )
                                   ),
                                   column(6, actionButton(ns("query"), "Start query", class="btn btn-primary w-100"))
                                 ),
                                 br(),
                                 progressBar(
                                   id = ns("progress_snap"),
                                   value = 0,
                                   title = " ",
                                   display_pct = TRUE
                                 ),
                                 textOutput(ns("upstream_msg")),
                                 hr()
                             )
                    )
        )
      ))

      # Reset flags/results each time the modal opens
      rv$have_local_tab    <- FALSE
      rv$have_upstream_tab <- FALSE
      rv$local             <- NULL
      rv$upstream          <- NULL
      picked(character(0))
      updateTextInput(session, "search", value = "")
      custom_updateProgressBar(0)
      upstream_msg("")
    })

    observeEvent(input$close, { removeModal() })

    # --- search + filtering ---
    term <- reactive({
      cur <- if (is.null(input$search)) "" else input$search
      tolower(trimws(cur))
    })

    filtered <- reactive({
      if (term() == "") return(choices)
      lbls <- names(choices)
      keep <- grepl(term(), tolower(lbls), fixed = TRUE) |
        grepl(term(), tolower(unname(choices)), fixed = TRUE)
      choices[keep]
    })

    # --- left/right checkbox columns ---
    output$left <- renderUI({
      ch <- filtered(); n <- length(ch); left_n <- ceiling(n/2L)
      left <- ch[seq_len(left_n)]
      vals <- unname(left); lbls <- names(left)
      cn <- mapply(function(label, code) {
        tip <- if (!is.null(desc) && !is.na(desc[[code]])) desc[[code]] else label
        tags$span(label, title = tip, style="white-space:normal;max-width:280px;display:inline-block;")
      }, lbls, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)
      checkboxGroupInput(ns("left_vals"), NULL,
                         choiceNames = cn, choiceValues = vals,
                         selected = intersect(picked(), vals), width = "100%")
    })

    output$right <- renderUI({
      ch <- filtered(); n <- length(ch); left_n <- ceiling(n/2L)
      right <- if (n > left_n) ch[seq.int(left_n + 1L, n)] else ch[0]
      vals <- unname(right); lbls <- names(right)
      cn <- mapply(function(label, code) {
        tip <- if (!is.null(desc) && !is.na(desc[[code]])) desc[[code]] else label
        tags$span(label, title = tip, style="white-space:normal;max-width:280px;display:inline-block;")
      }, lbls, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)
      checkboxGroupInput(ns("right_vals"), NULL,
                         choiceNames = cn, choiceValues = vals,
                         selected = intersect(picked(), vals), width = "100%")
    })

    observe({
      vals <- unique(c(input$left_vals, input$right_vals))
      picked(if (is.null(vals)) character(0) else vals)
    })

    observeEvent(input$select_all, {
      ch <- filtered(); n <- length(ch); left_n <- ceiling(n/2L)
      left  <- ch[seq_len(left_n)]
      right <- if (n > left_n) ch[seq.int(left_n + 1L, n)] else ch[0]
      updateCheckboxGroupInput(session, "left_vals",  selected = unname(left))
      updateCheckboxGroupInput(session, "right_vals", selected = unname(right))
      picked(unname(ch))
    })

    observeEvent(input$deselect_all, {
      updateCheckboxGroupInput(session, "left_vals",  selected = character(0))
      updateCheckboxGroupInput(session, "right_vals", selected = character(0))
      picked(character(0))
    })

    # --- tables ---
    output$local_results <- DT::renderDataTable({
      req(rv$local)
      DT::datatable(
        rv$local,
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
        filter = "top"
      )
    })

    output$upstream_results <- DT::renderDataTable({
      req(rv$upstream)
      DT::datatable(
        rv$upstream,
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
        filter = "top"
      )
    })

    outputOptions(output, "local_results",    suspendWhenHidden = FALSE)
    outputOptions(output, "upstream_results", suspendWhenHidden = FALSE)

    # --- dynamic result tabs ---
    add_local_tab <- function(select_after = TRUE) {
      if (rv$have_local_tab) {
        if (select_after) updateTabsetPanel(session, "tabs", selected = "local_tab")
        return(invisible())
      }
      appendTab(
        inputId = "tabs",
        tab = tabPanel(
          "Local Results", value = "local_tab",
          div(style = "max-height:500px; overflow-y:auto;",
              # header + histogram controls + download
              div(
                style = "display:flex; justify-content: space-between; align-items:flex-start; margin-bottom: 8px; gap: 16px;",
                div(
                  style = "flex: 1;",
                  h4("Local Summary Results", style="margin-top:0;"),
                  selectInput(
                    ns("local_hist_var"),
                    label = if (identical(var_class, "landcover"))
                      "Landcover classes"
                    else
                      "Variable for histogram",
                    choices  = NULL,   # will be filled when data arrives
                    width    = "100%",
                    multiple = identical(var_class, "landcover")  # multi-select for landcover
                  ),
                  # bins slider only for non-landcover
                  if (!identical(var_class, "landcover")) {
                    sliderInput(
                      ns("local_hist_bins"),
                      label = "Number of bins",
                      min   = 10, max = 100, value = 30, step = 5, width = "100%"
                    )
                  },
                  plotly::plotlyOutput(ns("local_hist"), height = "450px")
                ),
                div(
                  style = "flex: 0 0 auto; align-self:flex-start;",
                  downloadButton(ns("dl_local"), "Download CSV")
                )
              ),
              DT::dataTableOutput(ns("local_results"))
          )
        ),
        select = select_after
      )
      rv$have_local_tab <- TRUE
    }


    add_upstream_tab <- function(select_after = TRUE) {
      if (rv$have_upstream_tab) {
        if (select_after) updateTabsetPanel(session, "tabs", selected = "upstream_tab")
        return(invisible())
      }
      appendTab(
        inputId = "tabs",
        tab = tabPanel(
          "Upstream Results", value = "upstream_tab",
          div(style = "max-height:500px; overflow-y:auto;",
              div(
                style = "display:flex; justify-content: space-between; align-items:flex-start; margin-bottom: 8px; gap: 16px;",
                div(
                  style = "flex: 1;",
                  h4("Upstream Summary Results", style="margin-top:0;"),
                  selectInput(
                    ns("upstream_hist_var"),
                    label = if (identical(var_class, "landcover"))
                      "Landcover classes"
                    else
                      "Variable for histogram",
                    choices  = NULL,   # will be filled when data arrives
                    width    = "100%",
                    multiple = identical(var_class, "landcover")  # multi-select for landcover
                  ),
                  # bins slider only for non-landcover
                  if (!identical(var_class, "landcover")) {
                    sliderInput(
                      ns("upstream_hist_bins"),
                      label = "Number of bins",
                      min   = 10, max = 100, value = 30, step = 5, width = "100%"
                    )
                  },
                  plotly::plotlyOutput(ns("upstream_hist"), height = "450px")
                ),
                div(
                  style = "flex: 0 0 auto; align-self:flex-start;",
                  downloadButton(ns("dl_upstream"), "Download CSV")
                )
              ),
              DT::dataTableOutput(ns("upstream_results"))
          )
        ),
        select = select_after
      )
      rv$have_upstream_tab <- TRUE
    }


    # --- downloads ---
    output$dl_local <- downloadHandler(
      filename = function() paste0(var_class, "_local", "-geofresh-", Sys.Date(), ".csv"),
      content  = function(file) { req(rv$local); write.csv(rv$local, file, row.names = FALSE) }
    )

    output$dl_upstream <- downloadHandler(
      filename = paste0(var_class, "_upstream", "-geofresh-", Sys.Date(), ".csv"),
      content  = function(file) { req(rv$upstream); write.csv(rv$upstream, file, row.names = FALSE) }
    )


    # --- histogram variable choices & plots ---

    # helper: pick numeric columns and drop IDs
    numeric_plot_cols <- function(df) {
      if (is.null(df)) return(character(0))
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      # drop obvious ID columns if present
      setdiff(num_cols, c("id", "subc_id", "reg_id"))
    }

    # update choices for local histogram when rv$local changes
    observe({
      df <- rv$local
      if (is.null(df)) return()
      cols <- numeric_plot_cols(df)
      if (!length(cols)) return()
      updateSelectInput(
        session, "local_hist_var",
        choices  = cols,
        selected = cols[1]
      )
    })

    # update choices for upstream histogram when rv$upstream changes
    observe({
      df <- rv$upstream
      if (is.null(df)) return()
      cols <- numeric_plot_cols(df)
      if (!length(cols)) return()
      updateSelectInput(
        session, "upstream_hist_var",
        choices  = cols,
        selected = cols[1]
      )
    })

    # local plots
    output$local_hist <- plotly::renderPlotly({
      req(rv$local)
      df <- rv$local

      if (identical(var_class, "landcover")) {
        req(input$local_hist_var)
        vars <- input$local_hist_var
        vars <- vars[vars %in% names(df)]
        df_num <- df[, vars, drop = FALSE]
        df_num <- df_num[, vapply(df_num, is.numeric, logical(1)), drop = FALSE]
        req(ncol(df_num) > 0)

        df_long <- stack(as.data.frame(df_num))
        names(df_long) <- c("value", "class")

        p <- ggplot2::ggplot(df_long, ggplot2::aes(x = class, y = value)) +
          ggplot2::geom_boxplot(fill = "#69b3a2", color = "grey30") +
          ggplot2::labs(
            title = "Landcover summary by class",
            x = "Landcover class",
            y = "Value"
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          theme(
            plot.title  = element_text(margin = margin(b = 1)),
            axis.title.y = element_text(margin = margin(r = 5)),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2))
          )

        return(plotly::ggplotly(p))
      }

      req(input$local_hist_var)
      req(input$local_hist_bins)

      var <- input$local_hist_var
      x   <- df[[var]]
      if (!is.numeric(x)) return(NULL)

      p <- ggplot2::ggplot(df, ggplot2::aes_string(x = var)) +
        ggplot2::geom_histogram(
          bins = input$local_hist_bins,
          fill = "#3182bd", color = "white"
        ) +
        ggplot2::labs(
          title = paste("Histogram of", var, "(local)"),
          x = var,
          y = "Count"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        theme(
          plot.title  = element_text(margin = margin(b = 1)),
          axis.title.y = element_text(margin = margin(r = 5)),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2))
        )

      plotly::ggplotly(p)
    })



    # Upstream plots
    output$upstream_hist <- plotly::renderPlotly({
      req(rv$upstream)
      df <- rv$upstream

      if (identical(var_class, "landcover")) {
        req(input$upstream_hist_var)
        vars <- input$upstream_hist_var
        vars <- vars[vars %in% names(df)]
        df_num <- df[, vars, drop = FALSE]
        df_num <- df_num[, vapply(df_num, is.numeric, logical(1)), drop = FALSE]
        req(ncol(df_num) > 0)

        df_long <- stack(as.data.frame(df_num))
        names(df_long) <- c("value", "class")

        p <- ggplot2::ggplot(df_long, ggplot2::aes(x = class, y = value)) +
          ggplot2::geom_boxplot(fill = "#69b3a2", color = "grey30") +
          ggplot2::labs(
            title = "Landcover summary by class",
            x = "Landcover class",
            y = "Value"
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          theme(
            plot.title  = element_text(margin = margin(b = 1)),
            axis.title.y = element_text(margin = margin(r = 5)),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2))
          )

        return(plotly::ggplotly(p))
      }

      req(input$upstream_hist_var)
      req(input$upstream_hist_bins)

      var <- input$upstream_hist_var
      x   <- df[[var]]
      if (!is.numeric(x)) return(NULL)

      p <- ggplot2::ggplot(df, ggplot2::aes_string(x = var)) +
        ggplot2::geom_histogram(
          bins = input$upstream_hist_bins,
          fill = "#3182bd", color = "white"
        ) +
        ggplot2::labs(
          title = paste("Histogram of", var, "(upstream)"),
          x = var,
          y = "Count"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        theme(
          plot.title  = element_text(margin = margin(b = 1)),
          axis.title.y = element_text(margin = margin(r = 5)),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2))
        )

      plotly::ggplotly(p)
    })



    # Function to create a custom update progress bar
    custom_updateProgressBar <- function(perc, sleep = 0) {
      shinyWidgets::updateProgressBar(
        session = session,
        id      = "progress_snap",
        value   = perc
      )
      if (sleep > 0) Sys.sleep(sleep)
    }


    # create empty dplyr connection for user input points table
    points_table <- reactive({
      req(user_table_name())
      # set user input points database table name
      tbl(pool, in_schema("shiny_user", user_table_name()))
    })


    # Function to run local subcatchment query
    local_query <- function(x, vc) {
      stats <- c("_min", "_max", "_mean", "_sd")

      # Build names: either keep xx, or add stats suffixes
      names_list <- lapply(x, function(xx) {
        if (xx %in% var_groups$topo_without_stats || vc == "landcover") {
          xx
        } else {
          paste0(xx, stats)
        }
      })

      # Flatten list into a single character vector and add "id" and "subc_id"
      col_names <- c(
        unlist(names_list, use.names = FALSE),
        "id", "subc_id"
      )

      # Choose the appropriate table by class
      var_table <- switch(
        vc,
        "topography" = stats_topo_tbl,
        "climate"    = stats_clim_tbl,
        "soil"       = stats_soil_tbl,
        "landcover"  = stats_land_tbl,
        stop("Unknown var_class: ", vc)
      )

      # Query selected variables
      query_results <- points_table() %>%
        left_join(var_table, by = "subc_id") %>%
        select(all_of(col_names)) %>%
        collect()

      query_results
    }

    # flag: has upstream been computed for this user table?
    upstream_done <- reactiveVal(0)

    # if the user table changes (new upload / snap), allow upstream to be re-run
    observeEvent(user_table_name(), {
      upstream_done(0)
    }, ignoreInit = TRUE)

    # -----------------------------------------------------------------------------
    # calculate upstream catchment when user selects "Upstream"
    # runs only once per user_table_name(), unless reset above
    # -----------------------------------------------------------------------------
    observeEvent(input$scope, {
      # only proceed when the user actually selects "upstream"
      req(identical(input$scope, "upstream"))
      req(points_table())

      # only run if upstream area not calculated yet
      req(upstream_done() < 1)

      # set upstream_done so we don't run again for this dataset
      upstream_done(1)

      # UI feedback
      custom_updateProgressBar(10)
      upstream_msg("Calculating upstream catchment for your points. This may take some time...")

      # temporarily disable interactions while the heavy SQL runs
      shinyjs::disable("select_all")
      shinyjs::disable("deselect_all")
      shinyjs::disable("left_vals")
      shinyjs::disable("right_vals")
      shinyjs::disable("query")

      # ensure we re-enable controls even on error
      on.exit({
        shinyjs::enable("select_all")
        shinyjs::enable("deselect_all")
        shinyjs::enable("left_vals")
        shinyjs::enable("right_vals")
        shinyjs::enable("query")

        # upstream finished: tell user they can now run the query
        custom_updateProgressBar(60) # not 100; query observer will continue it
        upstream_msg("Upstream catchment calculation finished. You can now click 'Start query' to run the upstream analysis.")
      }, add = TRUE)

      message("calculating upstream catchment")

      # build SQL using user_table_name()
      sql <- sqlInterpolate(
        pool,
        "WITH sub AS (
       SELECT upstr.subc_id, upstr.nodes
       FROM ?point_table poi,
            hydro.pgr_upstreamcomponent(poi.subc_id, poi.reg_id, poi.basin_id) upstr
       WHERE poi.strahler_order > 1
     )
     UPDATE ?point_table poi SET
       upstream = sub.nodes
     FROM sub
     WHERE poi.subc_id = sub.subc_id",
        point_table = dbQuoteIdentifier(
          pool,
          Id(schema = 'shiny_user', table = user_table_name())
        )
      )

      custom_updateProgressBar(30)
      dbExecute(pool, sql)
      custom_updateProgressBar(50)

      message("calculating upstream catchment done")
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
    )

    # One function to aggregate upstream values for any variable class
    # Args:
    #   x               : character vector of selected variable base names (e.g., c("slope","elev"))
    #   vc              : variable class: "topography" | "climate" | "soil" | "landcover"
    # Returns: data.frame with columns id, subc_id, and aggregated variables
    upstream_query <- function(x, vc) {
      stopifnot(is.character(x), length(x) >= 1)
      stopifnot(is.character(vc), length(vc) == 1)

      # Map class -> stats table
      stats_table_name <- switch(
        vc,
        "topography" = "stats_topo",
        "climate"    = "stats_climate",
        "soil"       = "stats_soil",
        "landcover"  = "stats_landuse",
        stop("Unknown var_class: ", vc)
      )

      # Decide which *columns in the stats table* to average upstream
      # (These must match actual column names in the stats tables)
      to_avg <- switch(
        vc,
        "topography" = vapply(x, function(xx) {
          if (!is.null(var_groups$topo_without_stats) && xx %in% var_groups$topo_without_stats) {
            xx                     # base column (no suffix)
          } else {
            paste0(xx, "_mean")    # mean column in stats table
          }
        }, character(1)),
        "climate"   = paste0(x, "_mean"),
        "soil"      = paste0(x, "_mean"),
        "landcover" = x,           # land cover columns are already per sub-catchment (%/area shares)
        stop("Unknown var_class: ", vc)
      )

      # Safety: if no valid columns remain, return id + subc_id only
      if (!length(to_avg)) {
        warning("No variables to aggregate upstream; returning only id/subc_id.")
        # Still run a minimal query to return id/subc_id
        sql_min <- "
      SELECT poi.id,
             MIN(poi.subc_id) AS subc_id
      FROM ?stats_table stats
      JOIN ?point_table poi
        ON stats.subc_id = ANY (poi.upstream)
       AND stats.reg_id  = poi.reg_id
      GROUP BY poi.id"
        sql <- DBI::sqlInterpolate(
          pool, sql_min,
          point_table = DBI::dbQuoteIdentifier(pool, DBI::Id(schema = "shiny_user", table = user_table_name())),
          stats_table = DBI::dbQuoteIdentifier(pool, DBI::Id(schema = "hydro",      table = stats_table_name))
        )
        return(DBI::dbGetQuery(pool, sql))
      }

      # Build SELECT list for averages, aliasing to the *same name* as the stats column
      # e.g., round(avg(slope_mean)::numeric,4) AS slope_mean
      avg_exprs <- paste0("round(avg(", to_avg, ")::numeric, 4) AS ", to_avg)

      sql_string <- paste(
        "SELECT poi.id,",
        "       MIN(poi.subc_id) AS subc_id,",
        paste0(avg_exprs, collapse = ", "),
        "FROM ?stats_table stats",
        "JOIN ?point_table poi",
        "  ON stats.subc_id = ANY (poi.upstream)",
        " AND stats.reg_id  = poi.reg_id",
        "GROUP BY poi.id"
      )

      sql <- DBI::sqlInterpolate(
        pool, sql_string,
        point_table = DBI::dbQuoteIdentifier(pool, DBI::Id(schema = "shiny_user", table = user_table_name())),
        stats_table = DBI::dbQuoteIdentifier(pool, DBI::Id(schema = "hydro",      table = stats_table_name))
      )

      DBI::dbGetQuery(pool, sql)
    }


# ------------------------------------------------------------------------------

    # --- query click ---
    observeEvent(input$query, {

      # ----- lock UI: disable query button & reset progress -----
      shinyjs::disable("query")
      custom_updateProgressBar(5)

      # always re-enable the button and finish progress when we exit,
      # even if there's an error or early return
      on.exit({
        shinyjs::enable("query")
      }, add = TRUE)

      # ----- basic checks -----

      # check if any variable was picked
      vars <- picked()
      if (!length(vars)) {
        showNotification("Please select at least one variable before running the query.",
                         type = "warning", duration = 4)
        custom_updateProgressBar(0)
        return()
      }

      # ---- check user points table exists and is non-empty ----
      tbl_exists <- tryCatch({
        DBI::dbExistsTable(
        pool,
        DBI::Id(schema = "shiny_user", table = user_table_name())
      )}, error = function(e) FALSE)

      if (!tbl_exists) {
        showNotification("Please upload point data first.",
                         type = "warning", duration = 4)
        custom_updateProgressBar(0)
        return(invisible(NULL))
      }

      custom_updateProgressBar(15)

      # check that snapping took place
      s  <- snap_status()
      df_s <- tryCatch(s, error = function(e) NULL)

      snapped_ok <- with_pool_connection(pool, function(conn) {
        tbl_id <- DBI::Id(schema = "shiny_user", table = user_table_name())
        tbl_q  <- DBI::dbQuoteIdentifier(conn, tbl_id)

        DBI::dbGetQuery(conn, paste0(
          "SELECT EXISTS (
       SELECT 1
       FROM ", tbl_q, "
       WHERE geom_snap IS NOT NULL
         AND snap_state = 'snapped'
       LIMIT 1
     ) AS ok"
        ))$ok[[1]]
      })



      # print(paste0("snapped_ok: ", snapped_ok))

      if (!snapped_ok) {
        showNotification("Please snap your points first (no valid snapped coordinates found).",
                         type = "warning", duration = 4)
        custom_updateProgressBar(0)
        return(invisible(NULL))
      }

      custom_updateProgressBar(30)

      # ----- run the query -----
      if (identical(input$scope, "local")) {
        add_local_tab(select_after = TRUE)
        custom_updateProgressBar(50)

        rv$local <- local_query(x = vars, vc = var_class)

        custom_updateProgressBar(90)


      } else {
        add_upstream_tab(select_after = TRUE)
        custom_updateProgressBar(50)

        # upstream query, put it here
        rv$upstream <- upstream_query(x = vars, vc = var_class)

        custom_updateProgressBar(90)

      }

      # Only set to 100% on success
      custom_updateProgressBar(100)


    })

    local_data    <- reactive(rv$local)
    upstream_data <- reactive(rv$upstream)

    list(
      local    = local_data,
      upstream = upstream_data
    )

  })
}

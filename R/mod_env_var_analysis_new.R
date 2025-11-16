# ========== Generic Module to choose variables and run analysis ==========

varsUI <- function(id, trigger_label = "Choose variables") {
  ns <- NS(id)
  actionLink(ns("show_modal"), trigger_label)
}

varsServer <- function(id,
                       title   = NULL,
                       choices,
                       desc    = NULL,
                       user_table_name,
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

    # --- open modal (gated) ---
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
                    label = "Variable for histogram",
                    choices = NULL,   # will be filled when data arrives
                    width = "100%"
                  ),
                  plotOutput(ns("local_hist"), height = "300px")
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
                    label = "Variable for histogram",
                    choices = NULL,
                    width = "100%"
                  ),
                  plotOutput(ns("upstream_hist"), height = "300px")
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
      filename = function() sprintf("local_results_%s.csv", Sys.Date()),
      content  = function(file) { req(rv$local); write.csv(rv$local, file, row.names = FALSE) }
    )

    output$dl_upstream <- downloadHandler(
      filename = function() sprintf("upstream_results_%s.csv", Sys.Date()),
      content  = function(file) { req(rv$upstream); write.csv(rv$upstream, file, row.names = FALSE) }
    )

    # --- plot servers (register once) ---
    # If your plotServer expects a plain df, adapt it or pass a reactive that it dereferences.
    # plotServer("local_plot",
    #            df     = rv$local,
    #            column = "mean",
    #            title  = "Summary plot of environmental variables for the local sub-catchment of each point")
    #
    # plotServer("upstream_plot",
    #            df     = rv$upstream,
    #            column = "mean of sub-catchment means",
    #            title  = "Summary plot of environmental variables for the upstream catchment of each point")

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

    # local histogram
    output$local_hist <- renderPlot({
      req(rv$local)
      req(input$local_hist_var)
      df <- rv$local
      var <- input$local_hist_var
      x <- df[[var]]
      if (!is.numeric(x)) return()
      hist(
        x,
        main = paste("Histogram of", var, "(local)"),
        xlab = var,
        breaks = "FD"
      )
    })

    # upstream histogram
    output$upstream_hist <- renderPlot({
      req(rv$upstream)
      req(input$upstream_hist_var)
      df <- rv$upstream
      var <- input$upstream_hist_var
      x <- df[[var]]
      if (!is.numeric(x)) return()
      hist(
        x,
        main = paste("Histogram of", var, "(upstream)"),
        xlab = var,
        breaks = "FD"
      )
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


    # --- query click ---
    observeEvent(input$query, {

      # ----- lock UI: disable query button & reset progress -----
      shinyjs::disable("query")
      custom_updateProgressBar(5)

      # always re-enable the button and finish progress when we exit,
      # even if there's an error or early return
      on.exit({
        shinyjs::enable("query")
        custom_updateProgressBar(100)
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

      # check if database table with user input points exists
      req(points_table())
      custom_updateProgressBar(15)

      # check that snapping took place
      pts <- points_table()  # tbl_lazy

      # 1) check columns exist
      snap_cols_ok <- all(c("latitude_snap", "longitude_snap") %in% colnames(pts))

      # 2) check that there is at least one row where both snapped coords are non-NA
      if (snap_cols_ok) {
        snap_info <- pts %>%
          dplyr::filter(!is.na(latitude_snap), !is.na(longitude_snap)) %>%
          dplyr::tally(name = "n_non_na") %>%
          dplyr::collect()

        snap_vals_ok <- snap_info$n_non_na[1] > 0
      } else {
        snap_vals_ok <- FALSE
      }

      if (!snap_vals_ok) {
        showNotification(
          "Please snap your points first.",
          type     = "warning",
          duration = 4
        )
        custom_updateProgressBar(0)
        return()
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

        # when you implement upstream query, put it here
        # rv$upstream <- upstream_query(...)

        custom_updateProgressBar(90)
      }

      # on.exit() will set to 100 and re-enable the button
    })

  })
}

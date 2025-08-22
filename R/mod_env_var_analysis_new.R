# ========== Generic Module to choose variables and run analysis ==========

varsUI <- function(id, trigger_label = "Choose variables") {
  ns <- NS(id)
  actionLink(ns("show_modal"), trigger_label)
}

varsServer <- function(id, title = "Variables", choices, desc = NULL) {
  stopifnot(is.character(choices), !is.null(names(choices)))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    picked <- reactiveVal(character(0))

    # results + tab-existence flags
    rv <- reactiveValues(
      local = NULL, upstream = NULL,
      have_local_tab = FALSE, have_upstream_tab = FALSE
    )

    # -------- modal with ONLY the Select tab initially --------
    observeEvent(input$show_modal, {
      showModal(modalDialog(
        title = title,
        easyClose = TRUE,
        size = "l",
        footer = div(style = "text-align:right;",
                     actionButton(ns("close"), "Close", class = "btn btn-outline-secondary")),
        tabsetPanel(id = ns("tabs"), selected = "select_tab",
                    tabPanel("Select Variables", value = "select_tab",
                             div(style="max-height: 500px; overflow-y:auto; padding-right:5px;",
                                 # explainer (sticky)
                                 div(style="position:sticky;top:0;background:white;z-index:100;padding:10px 0;border-bottom:1px solid #ccc;",
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
                                 # two columns
                                 fluidRow(column(6, uiOutput(ns("left"))), column(6, uiOutput(ns("right")))),
                                 hr(),
                                 # scope + query
                                 fluidRow(
                                   column(6,
                                          radioButtons(
                                            inputId = ns("scope"), label = NULL,
                                            choiceNames = list(
                                              tags$span("Local",    title="Query selected environmental variables for the local sub-catchment (min, max, mean, sd)",
                                                        style="white-space:normal;max-width:280px;display:inline-block;"),
                                              tags$span("Upstream", title="Query selected environmental variables for the upstream catchment of each point (mean of sub-catchment means)",
                                                        style="white-space:normal;max-width:280px;display:inline-block;")
                                            ),
                                            choiceValues = c("local","upstream"),
                                            selected = "local", inline = TRUE
                                          )
                                   ),
                                   column(6, actionButton(ns("query"), "Start query", class="btn btn-primary w-100"))
                                 ),
                                 hr()
                             )
                    )
        )
      ))

      # reset flags/results each time the modal opens
      rv$have_local_tab <- FALSE
      rv$have_upstream_tab <- FALSE
      rv$local <- NULL
      rv$upstream <- NULL
    })

    observeEvent(input$close, removeModal())

    # ----------------- variable selection UI -----------------
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
    output$left <- renderUI({
      ch <- filtered(); n <- length(ch); left_n <- ceiling(n/2L)
      left <- ch[seq_len(left_n)]; vals <- unname(left); lbls <- names(left)
      cn <- mapply(function(label, code) {
        tip <- if (!is.null(desc) && !is.na(desc[[code]])) desc[[code]] else label
        tags$span(label, title = tip, style="white-space:normal;max-width:280px;display:inline-block;")
      }, lbls, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)
      checkboxGroupInput(ns("left_vals"), NULL, choiceNames = cn, choiceValues = vals,
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
      checkboxGroupInput(ns("right_vals"), NULL, choiceNames = cn, choiceValues = vals,
                         selected = intersect(picked(), vals), width = "100%")
    })
    observe({
      vals <- unique(c(input$left_vals, input$right_vals))
      picked(if (is.null(vals)) character(0) else vals)
    })
    observeEvent(input$select_all, {
      ch <- filtered(); n <- length(ch); left_n <- ceiling(n/2L)
      left <- ch[seq_len(left_n)]
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

    # ----------------- DT outputs (declared once) -----------------
    output$local_results <- DT::renderDataTable({
      req(rv$local)
      datatable(
        rv$local,
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
        filter = "top"
      )
    })
    output$upstream_results <- DT::renderDataTable({
      req(rv$upstream)
      datatable(
        rv$upstream,
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
        filter = "top"
      )
    })
    # Keep DT rendering even if tab is hidden (still helpful in modals)
    outputOptions(output, "local_results", suspendWhenHidden = FALSE)
    outputOptions(output, "upstream_results", suspendWhenHidden = FALSE)

    # --------- Download handlers (CSV stays the same) ---------
    output$dl_local <- downloadHandler(
      filename = function() sprintf("local_results_%s.csv", Sys.Date()),
      content  = function(file) { req(rv$local); write.csv(rv$local, file, row.names = FALSE) }
    )
    output$dl_upstream <- downloadHandler(
      filename = function() sprintf("upstream_results_%s.csv", Sys.Date()),
      content  = function(file) { req(rv$upstream); write.csv(rv$upstream, file, row.names = FALSE) }
    )

    # Helpers to create tabs on demand (only once) using appendTab()
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
              div(style = "display:flex; justify-content: space-between; align-items:center; margin-bottom: 8px;",
                  h4("Local Summary Results", style="margin:0;"),
                  downloadButton(ns("dl_local"), "Download CSV")
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
              div(style = "display:flex; justify-content: space-between; align-items:center; margin-bottom: 8px;",
                  h4("Upstream Summary Results", style="margin:0;"),
                  downloadButton(ns("dl_upstream"), "Download CSV")
              ),
              DT::dataTableOutput(ns("upstream_results"))
          )
        ),
        select = select_after
      )
      rv$have_upstream_tab <- TRUE
    }

    # ----------------- query click: create data, add tab, switch -----------------
    observeEvent(input$query, {
      vars <- picked()
      if (!length(vars)) {
        showNotification("Please select at least one variable before running the query.",
                         type = "warning", duration = 4)
        return()
      }
      labels <- setNames(names(choices), choices)[vars]

      # fake data for demo (replace with SQL results)
      set.seed(42)
      if (identical(input$scope, "local")) {
        add_local_tab(select_after = TRUE)   # create tab first
        session$onFlushed(function() {
          rv$local <- data.frame(
            Variable = labels,
            min  = round(runif(length(vars), 0, 10), 2),
            max  = round(runif(length(vars), 20, 30), 2),
            mean = round(runif(length(vars), 10, 20), 2),
            sd   = round(runif(length(vars),  1,  5), 2),
            check.names = FALSE
          )
        }, once = TRUE)
      } else {
        add_upstream_tab(select_after = TRUE)
        session$onFlushed(function() {
          rv$upstream <- data.frame(
            Variable = labels,
            `mean of sub-catchment means` = round(runif(length(vars), 5, 15), 2),
            check.names = FALSE
          )
        }, once = TRUE)
      }
    })

    # return whatever you need outside
    return(list(
      codes  = reactive(picked()),
      labels = reactive(names(choices)[match(picked(), choices)]),
      scope  = reactive(input$scope)
    ))
  })
}


# ========== Generic Module to choose variables and run analysis ==========

varsUI <- function(id, trigger_label = "Choose variables") {
  ns <- NS(id)
  actionLink(ns("show_modal"), trigger_label)
}

varsServer <- function(
    id,
    title   = "Variables",
    choices,
    desc = NULL,
    on_query = NULL
) {
  stopifnot(is.character(choices), !is.null(names(choices)))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    picked <- reactiveVal(character(0))

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

    observeEvent(input$show_modal, {
      showModal(modalDialog(
        title = title,
        easyClose = TRUE,
        size = "l",
        footer = div(style = "text-align: right;",
                     actionButton(ns("close"), "Close", class = "btn btn-outline-secondary")),
        # ---- Tabs ----
        tabsetPanel(id = ns("tabs"),
                    # ================== Tab 1: Variable Selection ==================
                    tabPanel("Select Variables",
                             div(style = "max-height: 500px; overflow-y: auto; padding-right: 5px;",

                                 # ---- Sticky explanation ----
                                 div(
                                   style = "position: sticky; top: 0; background: white; z-index: 100; padding: 10px 0; border-bottom: 1px solid #ccc;",
                                   p(
                                     "Select environmental variables and then run a query for either Local or Upstream scope.",
                                     br(),
                                     strong("Local:"), " Summarizes selected variables for the local sub-catchment (min, max, mean, sd).",
                                     br(),
                                     strong("Upstream:"), " Summarizes selected variables for the entire upstream catchment of each point (mean of sub-catchment means).",
                                     style = "font-size: 14px; margin: 0;"
                                   )
                                 ),

                                 # ---- Search bar ----
                                 textInput(ns("search"), NULL, placeholder = "Type to filter…", width = "100%"),

                                 # ---- Select/Deselect buttons ----
                                 fluidRow(
                                   column(6, actionButton(ns("select_all"), "Select all", class = "btn btn-primary w-100")),
                                   column(6, actionButton(ns("deselect_all"), "Deselect all", class = "btn btn-outline-secondary w-100"))
                                 ),
                                 br(),

                                 # ---- Variable lists ----
                                 fluidRow(column(6, uiOutput(ns("left"))), column(6, uiOutput(ns("right")))),

                                 # ---- Separator before Local/Upstream options ----
                                 hr(),
                                 fluidRow(
                                   column(6,
                                          radioButtons(
                                            inputId = ns("scope"),
                                            label   = NULL,
                                            choiceNames = list(
                                              tags$span(
                                                "Local",
                                                title = "Query selected environmental variables for the local sub-catchment (min, max, mean, sd)",
                                                style = "white-space: normal; max-width: 280px; display: inline-block;"
                                              ),
                                              tags$span(
                                                "Upstream",
                                                title = "Query selected environmental variables for the upstream catchment of each point (mean of sub-catchment means)",
                                                style = "white-space: normal; max-width: 280px; display: inline-block;"
                                              )
                                            ),
                                            choiceValues = c("local", "upstream"),
                                            selected = "local",
                                            inline = TRUE
                                          )
                                   ),
                                   column(6,
                                          actionButton(ns("query"), "Start query",  class = "btn btn-primary w-100")
                                   )
                                 ),
                                 hr()
                             )
                    ),

                    # ================== Tab 2: Local Results ==================
                    tabPanel("Local Results",
                             div(style = "max-height: 500px; overflow-y: auto;",
                                 h4("Local Summary Results"),
                                 tableOutput(ns("local_results")) # Placeholder
                             )
                    ),

                    # ================== Tab 3: Upstream Results ==================
                    tabPanel("Upstream Results",
                             div(style = "max-height: 500px; overflow-y: auto;",
                                 h4("Upstream Summary Results"),
                                 tableOutput(ns("upstream_results")) # Placeholder
                             )
                    )
        ) # end tabsetPanel
      ))
    })


    # Render left/right columns
    output$left <- renderUI({
      ch <- filtered(); n <- length(ch); left_n <- ceiling(n/2L)
      left <- ch[seq_len(left_n)]; vals <- unname(left); lbls <- names(left)

      cn <- mapply(function(label, code) {
        tip <- if (!is.null(desc) && !is.na(desc[[code]])) desc[[code]] else label
        tags$span(label, title = tip,
                  style = "white-space: normal; max-width: 280px; display: inline-block;")
      }, lbls, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)

      checkboxGroupInput(ns("left_vals"), NULL,
                         choiceNames = cn, choiceValues = vals, selected = intersect(picked(), vals), width = "100%")
    })

    output$right <- renderUI({
      ch <- filtered(); n <- length(ch); left_n <- ceiling(n/2L)
      right <- if (n > left_n) ch[seq.int(left_n + 1L, n)] else ch[0]
      vals <- unname(right); lbls <- names(right)

      cn <- mapply(function(label, code) {
        tip <- if (!is.null(desc) && !is.na(desc[[code]])) desc[[code]] else label
        tags$span(label, title = tip,
                  style = "white-space: normal; max-width: 280px; display: inline-block;")
      }, lbls, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)

      checkboxGroupInput(ns("right_vals"), NULL,
                         choiceNames = cn, choiceValues = vals, selected = intersect(picked(), vals), width = "100%")
    })

    # Sync picks
    observe({
      vals <- unique(c(input$left_vals, input$right_vals))
      if (!is.null(vals)) picked(vals) else picked(character(0))
    })

    # Select/Deselect all
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
    observeEvent(input$close, removeModal())


    # ---- Results outputs (placeholders) ----
    output$local_results <- renderTable({
      data.frame(Variable = character(), min = numeric(), max = numeric(),
                 mean = numeric(), sd = numeric(), check.names = FALSE)
    }, striped = TRUE, bordered = TRUE, hover = TRUE)

    output$upstream_results <- renderTable({
      data.frame(Variable = character(),
                 `mean of sub-catchment means` = numeric(),
                 check.names = FALSE)
    }, striped = TRUE, bordered = TRUE, hover = TRUE)

    # ---- Handle Start query ----
    observeEvent(input$query, {
      vars <- picked()
      if (length(vars) == 0) {
        showNotification("Please select at least one variable before running the query.",
                         type = "warning", duration = 4)
        return()
      }

      # Build nice labels for the selected codes
      sel_labels <- names(choices)[match(vars, choices)]

      # -- Local table: min, max, mean, sd (placeholders) --
      local_df <- data.frame(
        Variable = sel_labels,
        min  = NA_real_,
        max  = NA_real_,
        mean = NA_real_,
        sd   = NA_real_,
        check.names = FALSE
      )

      # -- Upstream table: mean of sub-catchment means (placeholder) --
      upstream_df <- data.frame(
        Variable = sel_labels,
        `mean of sub-catchment means` = NA_real_,
        check.names = FALSE
      )

      # Render to the outputs
      output$local_results <- renderTable(local_df, striped = TRUE, bordered = TRUE, hover = TRUE)
      output$upstream_results <- renderTable(upstream_df, striped = TRUE, bordered = TRUE, hover = TRUE)

      # Switch to the requested results tab
      if (identical(input$scope, "local")) {
        updateTabsetPanel(session, "tabs", selected = "Local Results")
      } else {
        updateTabsetPanel(session, "tabs", selected = "Upstream Results")
      }
    })


    # Return values
    return(list(
      codes    = reactive(picked()),
      labels   = reactive(names(choices)[match(picked(), choices)]),
      scope    = reactive(input$scope)
    ))
  })
}

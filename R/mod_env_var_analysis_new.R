# ========== Generic Module to choose variables and run analysis ==========

varsUI <- function(id, trigger_label = "Choose variables") {
  ns <- NS(id)
  actionLink(ns("show_modal"), trigger_label)
}

varsServer <- function(
    id,
    title = "Variables",
    choices,
    desc = NULL
) {
  stopifnot(is.character(choices), !is.null(names(choices)))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Start empty selection
    picked <- reactiveVal(character(0))

    # Current search term
    term <- reactive({
      cur <- if (is.null(input$search)) "" else input$search
      tolower(trimws(cur))
    })

    # Filter choices
    filtered <- reactive({
      if (term() == "") return(choices)
      lbls <- names(choices)
      keep <- grepl(term(), tolower(lbls), fixed = TRUE) |
        grepl(term(), tolower(unname(choices)), fixed = TRUE)
      choices[keep]
    })

    # Show modal
    observeEvent(input$show_modal, {
      showModal(modalDialog(
        title = title,
        easyClose = TRUE,
        size = "l",
        footer = div(style = "text-align: right;",
                     actionButton(ns("close"), "Close", class = "btn btn-outline-secondary")),
        textInput(ns("search"), NULL, placeholder = "Type to filter…", width = "100%"),
        fluidRow(
          column(6, actionButton(ns("select_all"), "Select all", class = "btn btn-primary w-100")),
          column(6, actionButton(ns("deselect_all"), "Deselect all", class = "btn btn-outline-secondary w-100"))
        ),
        br(),
        fluidRow(column(6, uiOutput(ns("left"))), column(6, uiOutput(ns("right")))),
        hr(),  # ---- separator before Local/Upstream ----
        fluidRow(column(6, radioButtons(
          inputId  = ns("scope"),
          label    = NULL,
          choices  = c("Local" = "local", "Upstream" = "upstream"),
          selected = "local",
          inline   = TRUE)
          ),
        column(6,
               actionButton(ns("query"), "Search",  class = "btn btn-primary w-100")
               )
        )
        ,
        hr()   # ---- separator before Close button ----
      ))
    })

    # Left column
    output$left <- renderUI({
      ch <- filtered(); n <- length(ch)
      left_n <- ceiling(n / 2L)
      left   <- ch[seq_len(left_n)]
      vals   <- unname(left)
      lbls   <- names(left)

      cn <- mapply(function(label, code) {
        tt <- if (!is.null(desc) && !is.na(desc[[code]])) desc[[code]] else label
        tags$span(label, title = tt,
                  style = "white-space: normal; max-width: 280px; display: inline-block;")
      }, lbls, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)

      checkboxGroupInput(ns("left_vals"), NULL,
                         choiceNames  = cn,
                         choiceValues = vals,
                         selected     = intersect(picked(), vals),
                         width        = "100%"
      )
    })

    # Right column
    output$right <- renderUI({
      ch <- filtered(); n <- length(ch)
      left_n <- ceiling(n / 2L)
      right  <- if (n > left_n) ch[(left_n+1):n] else ch[0]
      vals   <- unname(right)
      lbls   <- names(right)

      cn <- mapply(function(label, code) {
        tt <- if (!is.null(desc) && !is.na(desc[[code]])) desc[[code]] else label
        tags$span(label, title = tt,
                  style = "white-space: normal; max-width: 280px; display: inline-block;")
      }, lbls, vals, SIMPLIFY = FALSE, USE.NAMES = FALSE)

      checkboxGroupInput(ns("right_vals"), NULL,
                         choiceNames  = cn,
                         choiceValues = vals,
                         selected     = intersect(picked(), vals),
                         width        = "100%"
      )
    })

    # Keep picked values in sync
    observe({
      vals <- unique(c(input$left_vals, input$right_vals))
      if (!is.null(vals)) picked(vals) else picked(character(0))
    })

    # Select/Deselect
    observeEvent(input$select_all, {
      ch <- filtered(); n <- length(ch)
      left_n <- ceiling(n / 2L)
      left   <- ch[seq_len(left_n)]
      right  <- if (n > left_n) ch[(left_n+1):n] else ch[0]

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

    # Return selected variables + Local/Upstream flags
    return(list(
      codes     = reactive(picked()),
      labels    = reactive(names(choices)[match(picked(), choices)]),
      local     = reactive(input$local),
      upstream  = reactive(input$upstream)
    ))
  })
}

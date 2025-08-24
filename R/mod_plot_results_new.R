# This creates plots -------------------------------------------------------------

# UI function: button that opens a modal
plotUI <- function(id, label = "Plot results") {
  ns <- NS(id)
  actionButton(ns("open"), label)
}

# Server function: opens modal with a Plotly histogram
plotServer <- function(id, title = "Results", df, column, footer = NULL, size = "l", easyClose = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show modal when button is clicked
    observeEvent(input$open, {
      showModal(modalDialog(
        title = title,
        easyClose = easyClose,
        size = size,
        footer = footer %||% modalButton("Close"),
        tagList(
          h4(paste("Histogram of", column)),
          plotly::plotlyOutput(ns("histPlot"), height = "400px")
        )
      ))
    })

    # Render the Plotly histogram
    output$histPlot <- plotly::renderPlotly({
      req(df[[column]])  # Ensure column exists
      plotly::plot_ly(
        data = df,
        x = ~get(column),
        type = "histogram",
        marker = list(color = "#3182bd", line = list(color = "#fff", width = 1))
      ) %>%
        plotly::layout(
          xaxis = list(title = column),
          yaxis = list(title = "Count"),
          bargap = 0.1
        )
    })
  })
}

# Helper for null-coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b
